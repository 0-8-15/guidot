(define x (make-observable 23 #f #f 'X))

(test-error
 "no current transaction"
 (observable-set! x 42))

(test-assert
 "set! works"
 (begin
   (parameterize (($implicit-current-transactions #t)) (observable-set! x 42))
   (= (observable-deref x) 42)))

(test-assert
 "with-current-transaction"
 (with-current-transaction
  (lambda ()
    (let ((y (observable-deref (observable-reference x))))
      (observable-set! x (+ y 3))
      #t))))
(define y (make-observable 42))
(define op (make-observable *))
(define res (make-observable 'undefined))

(test-assert
 "observable-invoke!"
 (parameterize
  (($implicit-current-transactions #t))
  (observable-invoke!
   (lambda (res op a b) (observable-set! res ((observable-deref op) (observable-deref a) (observable-deref b))))
   (list res op x y))))

(test-assert "observable-alter!" (eq? (with-current-transaction (lambda () (observable-alter! res + 10))) 1900))

(define toplevel-observable-set!
  (dynamic
   (lambda (r v)
     (parameterize (($implicit-current-transactions #t)) (observable-set! r v)))))

(define (add-and-set-conflict ref overwrite)
  (lambda (a b) (toplevel-observable-set! ref overwrite) (+ a b)))

(test-assert
 "observable-alter! with conflict (retries)"
 (eq? (parameterize
       (($stm-retry-limit 2))
       (with-current-transaction
        (lambda ()
          (observable-alter! res (add-and-set-conflict res 5) 10))))
      15))

(test-error
 "$stm-retry-limit 0: retries become exceptions"
 (eq? (parameterize
       (($stm-retry-limit 0))
       (with-current-transaction
        (lambda ()
          (observable-alter! res (add-and-set-conflict res 0) 10))))
      10))

(test-assert
 "Outdated reference (currently only reported)"
 (eq? (parameterize
       (($stm-retry-limit 3))
       (toplevel-observable-set! res 42)
       (with-current-transaction
        (lambda ()
          (toplevel-observable-set! res 7)
          (observable-alter! res + #;(add-and-set-conflict res 7) 23))))
      30))

(test-error
 "Outdated reference and conflict with zero $stm-retry-limit fails."
 (eq? (parameterize
       (($stm-retry-limit 0))
       (toplevel-observable-set! res 42)
       (with-current-transaction
        (lambda ()
          (toplevel-observable-set! res 11)
          (observable-alter! res (add-and-set-conflict res 7) 23))))
      30))

(test-error
 "Retry limit effective"
 (eq? (parameterize
       (($stm-retry-limit 10))
       (toplevel-observable-set! res 42)
       (with-current-transaction
        (lambda ()
          (toplevel-observable-set! res 11)
          (observable-alter! res (add-and-set-conflict res 7) 23))))
      30))

(define (with-triggers-no-retry thunk)
  (parameterize
   ((current-trigger-handler observable-triggers)
    ($stm-retry-limit 0))
   ;; before ..??
   (with-current-transaction thunk)))

(test-assert
 "TBD: triggers AND set! works"
 (let ()
   (define (with-triggers-no-retry thunk)
     (parameterize
      ((current-trigger-handler observable-triggers)
       ($stm-retry-limit 0))
      ;; before ..??
      (with-current-transaction thunk)))
   (with-triggers-no-retry (lambda () (observable-set! x 42)))
   (= (observable-deref x) 42)))


;; ($implicit-current-transactions #f)

;;(run-tests)
