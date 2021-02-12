(cond-expand
 (gambit
  (declare (fixnum))
  (define-macro (abs x) `(##fxabs ,x))
  (include "0100-repetition.scm")
  (include "0110-ggb.scm")
  (include "0120-ggb2d.scm")
  (cond-expand
   (test (include "2010-repetition.scm"))
   (else)))
 (else (error "just gambit so far")))
