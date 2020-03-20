(cond-expand
 (gambit
  (cond-expand
   (test (include "likely-test-gambit.scm"))
   (gambit (include "likely-gambit.scm"))))
 (else (error "just gambit so far")))
