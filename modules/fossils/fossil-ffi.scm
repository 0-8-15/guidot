(c-declare "#include <zlib.h>")

(c-declare #<<c-declare-end

// #include <stdlib.h>
#include <zlib.h>

/*
See
https://blog.2of1.org/2011/03/03/decompressing-zlib-images/
https://tools.ietf.org/html/rfc6713
search for "magic number".
*/
static int zlib_compressed_p(___SCMOBJ u8v, size_t offset)
{
  unsigned char* memory = ___CAST(unsigned char*, ___BODY(u8v));
  ___S16 const header = (memory[offset] << 8) | memory[offset+1]; //|
  switch(header) {
    case 0x083c: case 0x087a: case 0x08b8: case 0x08f6:
    case 0x1838: case 0x1876: case 0x18b4: case 0x1872:
    case 0x2834: case 0x2872: case 0x28b0: case 0x28ee:
    case 0x3830: case 0x386e: case 0x38ac: case 0x38ea:
    case 0x482c: case 0x486a: case 0x48a8: case 0x48e6:
    case 0x5828: case 0x5866: case 0x58a4: case 0x58e2:
    case 0x6824: case 0x6862: case 0x68bf: case 0x68fd:
    case 0x7801: case 0x785e: case 0x789c: case 0x78da:
       return 1;
    default: return 0;
  }
}

static size_t zlib_compress(___SCMOBJ src_u8v, size_t src_start, size_t src_end, ___SCMOBJ dst_u8v, size_t dst_start, int compression_level)
{
  Bytef* ibuf = ___CAST(Bytef*,___BODY(src_u8v));
  Bytef* obuf = ___CAST(Bytef*,___BODY(dst_u8v));
  size_t d_len = ___INT(___U8VECTORLENGTH(dst_u8v));
  uLongf dstLen = (uLongf)(dst_start < d_len ? d_len-dst_start : 0);
  size_t s_len = ___INT(___U8VECTORLENGTH(src_u8v));
  uLongf srcLen = (uLongf)(src_end < s_len ? src_end : s_len) - (src_start < s_len ? src_start : s_len);
  int rc = compress2(obuf+dst_start, &dstLen, ibuf+src_start, srcLen, compression_level);
  if( rc == Z_OK) return (size_t)dstLen;
  return rc;
}

static size_t zlib_uncompress(___SCMOBJ src_u8v, size_t src_start, size_t src_end, ___SCMOBJ dst_u8v, size_t dst_start)
{
  Bytef* ibuf = ___CAST(Bytef*,___BODY(src_u8v));
  Bytef* obuf = ___CAST(Bytef*,___BODY(dst_u8v));
  uLongf dstLen = (uLongf)(___U8VECTORLENGTH(dst_u8v)-dst_start);
  size_t s_len = ___U8VECTORLENGTH(src_u8v);
  uLongf srcLen = (uLongf)(src_end < s_len ? src_end : s_len) - src_start;
  int rc = uncompress(obuf+dst_start, &dstLen, ibuf+src_start, srcLen);
  if(rc  == Z_OK) return (int)dstLen;
  else return rc;
}

c-declare-end
)

;; in module `beaver` is u8vector-copy-from-ptr! as:
;;
#;(define (u8vector-copy-from-ptr! u8 u8o ptr ptro len)
  ;; TBD: Add range checks
  ((c-lambda
    (scheme-object size_t void* size_t size_t) scheme-object
    "memcpy(___CAST(char *,___BODY(___arg1)) + ___arg2, ___CAST(char *,___arg3) + ___arg4, ___arg5);
    ___return(___arg1);")
   u8 u8o ptr ptro len))

(define (zlib-compressed? u8v #!optional (offset 0))
  (cond
   ((> (u8vector-length u8v) (+ offset 2))
    ((c-lambda (scheme-object size_t) bool "zlib_compressed_p") u8v offset))
   (else #f)))

(define zlib-compress-bound
  ;; yet another way to write (lambda (x) (+ x 13 (floor (/ (+ x 999) 1000))))
  (c-lambda (size_t) size_t "compressBound"))

(define (zlib-subu8vector-compress! src-u8vector src-start src-end dst-u8vector dst-start #!optional (compression-level -1))
  (when #f TBD-check-arguments)
  ((c-lambda (scheme-object size_t size_t scheme-object size_t int) int "zlib_compress")
   src-u8vector src-start src-end dst-u8vector dst-start compression-level))

(define (zlib-subu8vector-compress u8vec #!optional (start 0) (end (u8vector-length u8vec)))
  (let* ((dst-vector (make-u8vector (zlib-compress-bound (- end start))))
         (rc (zlib-subu8vector-compress! u8vec start end dst-vector 0)))
    (cond
     ((##fx< rc 0) (error "failed" zlib-subu8vector-compress rc))
     (else (subu8vector dst-vector 0 rc)))))

(define (zlib-subu8vector-uncompress! src-u8vector src-start src-end dst-u8vector dst-start)
  (when #f TBD-check-arguments)
  ((c-lambda (scheme-object size_t size_t scheme-object size_t) int "zlib_uncompress")
   src-u8vector src-start src-end dst-u8vector dst-start))

(define (zlib-subu8vector-uncompress u8vec #!optional (start 0) (end (u8vector-length u8vec)))
  (let loop ((room (* (- end start) 5)))
    (let* ((dst-vector (make-u8vector room))
           (rc (zlib-subu8vector-uncompress! u8vec start end dst-vector 0)))
      (cond
       ((##fx< rc 0) (error "failed" zlib-subu8vector-uncompress rc))
       ((eqv? rc room) dst-vector)
       ((##fx> rc room) (loop rc))
       (else (subu8vector dst-vector 0 rc))))))
;;

(define fossil-u8vector-uncompressed-size
  ;; 4 byte big endian uncompressed size art offset
  (c-lambda
   (scheme-object size_t) size_t
   "size_t mx = ___INT(___U8VECTORLENGTH(___arg1)), start = ___arg2;
    unsigned char* from = ___CAST(unsigned char*, ___BODY(___arg1)) + start;
    ___return(start+4<mx ? ((from[0]<<24) + (from[1]<<16) + (from[2]<<8) + from[3]) : 0);"))

(define (fossil-subu8vector-compress u8vec #!optional (start 0) (end (u8vector-length u8vec)))
  (let* ((uncompressed-size (- end start))
         (dst-vector (make-u8vector (+ (zlib-compress-bound uncompressed-size) 4)))
         (rc (zlib-subu8vector-compress! u8vec start end dst-vector 4)))
    (cond
     ((##fx< rc 0) (error "failed" zlib-subu8vector-compress rc))
     (else
      ((c-lambda
        (scheme-object size_t) void
        "unsigned char* lo = ___CAST(unsigned char*, ___BODY(___arg1));
        lo[0]=___arg2>>24 & 0xff;
        lo[1]=___arg2>>16 & 0xff;
        lo[2]=___arg2>>8  & 0xff;
        lo[3]=___arg2     & 0xff;")
       dst-vector uncompressed-size)
      (subu8vector dst-vector 0 (+ rc 4))))))

(define (fossil-subu8vector-uncompress u8vec #!optional (start 0) (end (u8vector-length u8vec)))
  (let* ((room (fossil-u8vector-uncompressed-size u8vec start))
         (dst-vector (make-u8vector room))
         (rc (zlib-subu8vector-uncompress! u8vec (+ start 4) end dst-vector 0)))
    (cond
     ((##fx< rc 0) (error "failed" zlib-subu8vector-uncompress rc))
     ((eqv? rc room) dst-vector)
     ((##fx> rc room) (loop rc))
     (else (subu8vector dst-vector 0 rc)))))

(c-declare
 ;; These are mostly simply stolen from fossil/libfossil
 #<<c-declare-end
/*
   Read bytes from *pz and convert them into a positive integer.  When
   finished, leave *pz pointing to the first character past the end of
   the integer.  The *pLen parameter holds the length of the string
   in *pz and is decremented once for each character in the integer.
*/
static size_t fsl_delta_int_get(unsigned char /*const*/ **pz, size_t *pLen){
  static const signed char zValue[] = {
    -1, -1, -1, -1, -1, -1, -1, -1,   -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1,   -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1,   -1, -1, -1, -1, -1, -1, -1, -1,
     0,  1,  2,  3,  4,  5,  6,  7,    8,  9, -1, -1, -1, -1, -1, -1,
    -1, 10, 11, 12, 13, 14, 15, 16,   17, 18, 19, 20, 21, 22, 23, 24,
    25, 26, 27, 28, 29, 30, 31, 32,   33, 34, 35, -1, -1, -1, -1, 36,
    -1, 37, 38, 39, 40, 41, 42, 43,   44, 45, 46, 47, 48, 49, 50, 51,
    52, 53, 54, 55, 56, 57, 58, 59,   60, 61, 62, -1, -1, -1, 63, -1,
  };
  size_t v = 0;
  int c;
  unsigned char /*const*/ *z = *pz;
  unsigned char const *zStart = z;
  while( (c = zValue[0x7f&*(z++)])>=0 ){
     v = (v<<6) + c;
  }
  z--;
  *pLen -= z - zStart;
  *pz = z;
  return v;
}
/*
   Calculates the size (in bytes) of the output from applying a
   delta. On success 0 is returned and *deltaSize will be updated with
   the amount of memory required for applying the delta.

   This routine is provided so that an procedure that is able
   to call fsl_delta_apply() can learn how much space is required
   for the output and hence allocate nor more space that is really
   needed.
*/
// from fossil/delta.c:
extern int delta_output_size(const char *zDelta, int lenDelta);
static int _delta_applied_size(___SCMOBJ delta, size_t offset)
{
  char *from = ___CAST(unsigned char*,___BODY(delta)) + offset;
  int s_len = ___INT(___U8VECTORLENGTH(delta)) - offset;
  // int result = s_len>0 ? fsl_delta_int_get(&from[offset], &s_len) : -1;
  int result = s_len>1 ? delta_output_size(from+offset, s_len) : -1;
/* also in delta_output_size
  if( result==-1 || *from!='\n' ) {
    /* ERROR: size integer not terminated by "\n" * /
    return -1;
  }
*/
  return result;
}

extern int delta_apply(
  const char *zSrc,      /* The source or pattern file */
  int lenSrc,            /* Length of the source file */
  const char *zDelta,    /* Delta to apply to the pattern */
  int lenDelta,          /* Length of the delta */
  char *zOut             /* Write the output into this preallocated buffer */
);

c-declare-end
)

(define (fossil-u8vector-delta-applied-size src #!optional (offset 0))
  (let ((x ((c-lambda (scheme-object size_t) int64 "_delta_applied_size") src offset)))
    (cond
     ((##fx< x 0) (error "no valid delta found" fossil-u8vector-delta-applied-size src offset))
     (else x))))

(c-declare
 #<<c-declare-end
static int _delta_apply(___U8* src, size_t src_len,
                        ___U8* delta, size_t delta_len,
                        ___U8* dst, size_t dst_len)
{
  return -1;
}
static int scm_delta_apply(___SCMOBJ src, size_t src_start, size_t src_end,
                        ___SCMOBJ delta, size_t delta_start, size_t delta_end,
                        ___SCMOBJ dst, size_t dst_start)
{
  if(src_end>___INT(___U8VECTORLENGTH(src)) || src_start>src_end) return -10;
  if(delta_end>___INT(___U8VECTORLENGTH(delta)) || delta_start>delta_end) return -11;
  if(dst_start>___INT(___U8VECTORLENGTH(dst))) return -12;
  return delta_apply(___CAST(___U8*, ___BODY(src)) + src_start, src_end-src_start,
                     ___CAST(___U8*, ___BODY(delta)) + delta_start, delta_end-delta_start ,
                     ___CAST(___U8*, ___BODY(dst)) + dst_start);
}
c-declare-end
)

(define (fossil-subu8vector-delta-apply! src src-start src-end delta delta-start delta-end dst dst-start)
  ((c-lambda (scheme-object size_t size_t scheme-object size_t size_t scheme-object size_t) int "scm_delta_apply")
   src src-start src-end delta delta-start delta-end dst dst-start))

(define (fossil-subu8vector-delta-apply src delta)
  (let* ((result (make-u8vector (fossil-u8vector-delta-applied-size delta))))
    (fossil-subu8vector-delta-apply! src 0 (u8vector-length src) delta 0 (u8vector-length delta) result 0)
    result))
