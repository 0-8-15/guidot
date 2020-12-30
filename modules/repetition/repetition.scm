(cond-expand
 (gambit
  (include "0100-repetition.scm")
  (cond-expand
   (test (include "2010-repetition.scm"))
   (else)))
 (else (error "just gambit so far")))
