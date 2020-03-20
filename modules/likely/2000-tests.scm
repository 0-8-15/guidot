
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
        (lambda (exn) (display exn) (display "\n  FAIL\n"))
        (lambda () (if ,expr (display "\n  PASS\n") (display "\n  FAIL\n"))))))))

(define-macro (test-error msg expr)
  `(eval
    (add-test!
     (lambda ()
       (display "Test ") (display ,msg) (display ": ")
       (with-exception-catcher
        (lambda (exn) (display exn) (display "\n  PASS\n"))
        (lambda () (display ,expr) (display "\n  FAIL\n")))))))

(include "2001-basic.scm")
