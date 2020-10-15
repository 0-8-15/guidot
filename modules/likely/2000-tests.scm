
(define test-hook '())
(define (add-test! thunk) ;; unused
  (set! test-hook (cons thunk test-hook)))

(define test-results '#(0 0 0))

(define (test-stats+! idx) (vector-set! test-results idx (add1 (vector-ref test-results idx))))

(define (tests-end)
  (define (X idx) (vector-ref test-results idx))
  (let ((tbd (reverse test-hook)))
    (set! test-hook '())
    (for-each (lambda (t) (t)) tbd))
  (for-each display (list "TOTAL " (X 0) " PASS " (X 1) " FAIL " (X 2) "\n")))

(define (add-test! thunk) (thunk))  ;; and overwritten

(define (test-report-begin msg)
  (test-stats+! 0)
  (display "Test ") (display msg) (display ": "))

(define (test-report-pass)
  (test-stats+! 1)
  (display "\n  PASS\n") )

(define (test-report-expected-condition result)
  (display result) (test-report-pass))

(define (test-report-fail result)
  (test-stats+! 2)
  (if (error-exception? result)
      (begin
        (display (error-exception-message result))
        (display " ")
        (display (error-exception-parameters result)))
      (display result))
  (display "\n  FAIL\n"))

(define-macro (test-assert msg expr)
  (let ((tmp (gensym 'expr)))
    `(eval
      (add-test!
       (lambda ()
         (test-report-begin ,msg)
         (with-exception-catcher
          test-report-fail
          (lambda ()
            (let ((,tmp ,expr))
              (if ,tmp (test-report-pass) (test-report-fail ,tmp))))))))))

(define-macro (test-error msg expr)
  (let ((tmp (gensym 'expr)))
    `(eval
      (add-test!
       (lambda ()
         (test-report-begin ,msg)
         (with-exception-catcher
          test-report-expected-condition
          (lambda () (test-report-fail ,expr))))))))

(define-macro (test-condition msg expr pred)
  (let ((tmp (gensym 'expr))
        (exn (gensym 'exn)))
    `(eval
      (add-test!
       (lambda ()
         (test-report-begin ,msg)
         (with-exception-catcher
          (lambda (,exn)
            (if (,pred ,exn) (test-report-pass) (test-report-fail ,exn)))
          (lambda () (test-report-fail ,expr))))))))

(include "2001-basic.scm")
