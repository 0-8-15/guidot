(cond-expand
 (gambit
  (declare (fixnum))
  (define-macro (abs x) `(##fxabs ,x))
  (define-macro (min a b) `(##fxmin ,a ,b))
  (define-macro (max a b) `(##fxmax ,a ,b)))
 (else (error "just gambit so far")))

(include "0100-repetition.scm")
(include "0110-ggb.scm")
(include "0120-ggb2d.scm")
;; sorting does NOT belong here - just I don't want another modules
;; right now.
(include "0300-sort.scm")
(cond-expand
 (test (include "2010-repetition.scm"))
 (else))
