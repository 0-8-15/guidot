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
