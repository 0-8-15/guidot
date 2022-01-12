(c-declare "#include <stdio.h>") ;; OH NOOO

(define %allocate-u8vector
  ;; allocate without initialization
  (c-lambda
   (size_t) scheme-object
   "___SCMOBJ result = ___EXT(___alloc_scmobj) (___PSTATE, ___sU8VECTOR, ___arg1);
if(___FIXNUMP(result)) { fprintf(stderr, \"OH NOOO\\n\"); ___return(___FAL); }
___EXT(___release_scmobj)(result);
___return(result);"))

(define (%allocate-still-u8vector size)
  ;; allocate still u8vector sans initialization
  (declare (not interrupts-enabled))
  (let ((o ((c-lambda
            (size_t) scheme-object
   "___SCMOBJ result = ___EXT(___alloc_scmobj) (___PSTATE, ___sU8VECTOR, ___arg1);
if(___FIXNUMP(result)) { ___return(___FAL); }
//___EXT(___release_scmobj)(result);
___still_obj_refcount_dec(result);
___return(result);")
            size)))
    (if (##fixnum? o)
        (begin
          (##raise-heap-overflow-exception)
          (%allocate-still-u8vector size))
        o)))

;;;*** copied from lwip/datastructures

(c-declare "#include <stdint.h>")

(define-macro (%u8vn-setter size conv)
  (let* ((size_str (number->string size))
         (result-c-type (string-append "uint" size_str "_t")))
    `(c-lambda
      (scheme-object size_t ,(string->symbol (string-append "unsigned-int" size_str))) void
      ,(string-append
       "const char *cptr = ___CAST(uint8_t*, ___BODY(___arg1));
" result-c-type " val = " conv "(___arg3); // TODO just inline the expression
*(" result-c-type "*)(cptr+___arg2) = val;"))))

(define %u8vector/32-set! (%u8vn-setter 32 ""))

(define-macro (gamhack-vector-range-assert proc vec offset size)
  `(unless (>= (u8vector-length ,vec) (+ ,offset ,(quotient size 8)))
           #;(error  "out of range" ',proc (u8vector-length ,vec) ,offset ,size)
     (##raise-range-exception 2 ',proc (u8vector-length ,vec) ,offset ,size)))

(define (u8vector/32-set! vec n v)
  (gamhack-vector-range-assert u8vector/32-set! vec n 32)
  (%u8vector/32-set! vec n v))

(define bitwise-reverse
  (c-lambda (unsigned-int) unsigned-int "
   unsigned int n = ___arg1;
#if (___INT_WIDTH == 32)
   n = ((n & 0xAAAAAAAA) >>  1) | ((n & 0x55555555) << 1);
   n = ((n & 0xCCCCCCCC) >>  2) | ((n & 0x33333333) << 2);
   n = ((n & 0xF0F0F0F0) >>  4) | ((n & 0x0F0F0F0F) << 4);
   n = ((n & 0xFF00FF00) >>  8) | ((n & 0x00FF00FF) << 8);
   ___return( (n >> 16) | (n << 16));
#else
   n = ((n & 0xAAAAAAAAAAAAAAAA) >>  1)  | ((n & 0x55555555055555555) << 1);
   n = ((n & 0xCCCCCCCCCCCCCCCC) >>  2)  | ((n & 0x33333333033333333) << 2);
   n = ((n & 0xF0F0F0F0F0F0F0F0) >>  4)  | ((n & 0x0F0F0F0F00F0F0F0F) << 4);
   n = ((n & 0xFF00FF00FF00FF00) >>  8)  | ((n & 0x00FF00FF000FF00FF) << 8);
   n = ((n & 0xFFFF0000FFFF0000) >>  16) | ((n & 0x0000FFFF0000FFFF0) << 16);
   ___return( (n >> 32) | (n << 32));
#endif
"))
