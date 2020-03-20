
(define test-hook '())
(define (add-test! thunk) ;; unsused
  (set! test-hook (cons thunk test-hook)))

(define (run-tests)
  (let ((tbd (reverse test-hook)))
    (set! test-hook '())
    (for-each (lambda (t) (t)) tbd)))

(define (add-test! thunk) (thunk))  ;; and overwritten

(define-macro (test-assert msg expr)
  `(eval
    (add-test!
     (lambda ()
       (display "Test ") (display ,msg) (display ": ")
       (with-exception-catcher
        (lambda (exn) (display exn) (display "\nFAIL\n"))
        (lambda () (if ,expr (display "\nPASS\n") (display "\nFAIL\n"))))))))

(define-macro (test-error msg expr)
  `(eval
    (add-test!
     (lambda ()
       (display "Test ") (display ,msg) (display ": ")
       (with-exception-catcher
        (lambda (exn) (display exn) (display "\nPASS\n"))
        (lambda () (display ,expr) (display "\nFAIL\n")))))))

(include "2001-basic.scm")
