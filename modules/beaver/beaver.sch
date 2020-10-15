(load "~~/lib/match.o1")
(define-macro (holladiholio)
  (let ((sym (with-exception-catcher
              (lambda (exn) 'noandroidapi)
              (lambda () (string->symbol (string-append "android-api" (getenv "SYS_ANDROIDAPI")))))))
    `(define-cond-expand-feature ,sym)))

(holladiholio)
