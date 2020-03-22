(define x (make-observable 23 #f #f 'X))

(define (error-exception-message-equal? x expected #!optional (= equal?))
  (and
   (error-exception? x)
   (let ((msg (error-exception-message x)))
     (= msg expected))))

(test-condition
 "no current transaction"
 (observable-set! x 42)
 (lambda (exn)
   (error-exception-message-equal? exn "no current transaction")))

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
 "observable-apply"
 (= (with-current-transaction (lambda () (observable-apply + (list x y)))) 87))

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
 "Outdated reference (might be reported)"
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

(test-condition
 "predicate works"
 (equal? (let ((a (make-observable 42 number? #f 'a)))
           (with-current-transaction
            (lambda ()
              (observable-alter! a number->string))))
         "42")
 (lambda (exn) (error-exception-message-equal? exn "value did not pass guard predicate")))

(test-condition
 "default predicate for observables"
 (equal? (let ((a (make-observable 42)))
           (with-current-transaction
            (lambda ()
              (observable-alter! a (lambda (v) a)))))
         "42")
 (lambda (exn) (equal? exn "observables may only be used with test predicate")))

(test-assert
 "filter"
 (equal? (let ((a (make-observable
                   42 number?
                   (lambda (o n) (inexact->exact (floor n))))))
           (with-current-transaction
            (lambda ()
              (observable-alter! a (lambda (o) 23.5)))))
         23))

(define (with-triggers-no-retry thunk)
  (parameterize
   ((current-trigger-handler observable-triggers)
    ($stm-retry-limit 0))
   ;; before ..??
   (with-current-transaction thunk)))

(test-assert
 "triggers AND set! works"
 (let ()
   (define (with-triggers-no-retry thunk)
     (parameterize
      ((current-trigger-handler observable-triggers)
       ($stm-retry-limit 0))
      ;; before ..??
      (with-current-transaction thunk)))
   (with-triggers-no-retry (lambda () (observable-set! x 42)))
   (= (observable-deref x) 42)))


(define reported (make-observable 23 #f #f 'reported))
(define report-counter (make-observable 0 #f #f 'reports))
(define (report-call)
  (debug 'report reported)
  (with-current-transaction
    (lambda () (observable-alter! report-counter + 1))))

(define (run-observed! thunk)
  (parameterize
   ((current-trigger-handler observable-triggers)
    #;($stm-retry-limit 0)
    #;($debug-trace-triggers #t)
    )
   ;; before ..??
   (with-current-transaction thunk)))

(test-assert
 "trail"
 (let ()
   (with-current-transaction
    (lambda ()
      (connect-dependent-value! #f (lambda () #f) report-call (list reported))))
   (debug 'expect 'report)
   (run-observed!
    (lambda () (observable-set! reported 42)))
   (thread-yield!)
   (= (observable-deref report-counter) 1)))

(test-assert
 "trail with critical procedure works (expect report)"
 (let ((ob (make-observable 23 #f #f 'reported))
       (success #f))
   (with-current-transaction
    (lambda ()
      (connect-dependent-value!
       (lambda thunk-results
         (box (lambda () (set! success (not (current-transaction))) (debug 'stm-critical (current-transaction)))))
       (lambda () #f)
       '() ;; sig
       (list ob))))
   (run-observed!
    (lambda () (observable-set! ob 42)))
   (thread-yield!)
   success))

(let ((this-is-the-error "post condition check failed"))
  (test-condition
   "trail with failing post condition aborts"
   (let ((ob (make-observable 23 #f #f 'ob))
         (success #f))
     (with-current-transaction
      (lambda ()
        (connect-dependent-value!
         ;; This pase is never reached.
         (lambda thunk-results
           (lambda () (set! success (not (current-transaction))) (debug 'stm-critical (current-transaction))))
         (lambda () (raise this-is-the-error))
         '() ;; sig
         (list ob))))
     (run-observed!
      (lambda () (observable-set! ob 42)))
     (unless (= (observable-deref ob) 23) (raise "FAIL FAIL FAIL"))
     (thread-yield!)
     success)
   (lambda (exn) (equal? exn this-is-the-error))))

#;(observable-connect!
       (list ob)
       critical:
       extern:
       check:
       post:)

(let ((this-is-the-error "post condition check failed"))
  (test-error
   "trail v2 failing post condition"
   (let ((ob (make-observable 23 #f #f 'ob))
         (success #f))
     (with-current-transaction
      (lambda ()
        (observable-connect!
         (list ob)
         critical:
         (lambda thunk-results
           ;; This pase is never reached.
           (set! success (not (current-transaction))) (debug 'stm-critical (current-transaction)))
         extern: #f
         ;; check: (lambda () (display "Check\n") #t)
         check: (lambda () (raise "post condition check failed"))
         post: (lambda () (display "Done\n")))))
     (run-observed!
      (lambda () (observable-set! ob 42)))
     (unless (= (observable-deref ob) 23) (raise "FAIL FAIL FAIL"))
     (thread-yield!)
     success))
  (lambda (exn) (equal? exn this-is-the-error)))

(test-assert
 "trail v2 passing (with traces)"
 (let ((ob (make-observable 23 #f #f 'ob))
       (success #f))
   (with-current-transaction
    (lambda ()
      (observable-connect!
       (list ob)
       critical:
       (lambda thunk-results
         (set! success (not (current-transaction))) (debug 'stm-critical (current-transaction)))
       extern: #f
       check: (lambda () (display "Check\n") #t)
       ;; check: (lambda () (raise "post condition check failed"))
       post: (lambda () (display "Done\n")))))
   (run-observed!
    (lambda () (observable-set! ob 42)))
   (unless (= (observable-deref ob) 42) (raise "FAIL FAIL FAIL"))
   (thread-yield!)
   success))

(test-assert
 "trail change signal"
 (let ((ob (make-observable 23 #f #f 'ob))
       (success #f))
   (with-current-transaction
    (lambda ()
      (observable-connect!
       (list ob)
       critical: #f
       extern: #f
       ;; check: (lambda () (display "Check\n") #t)
       ;; check: (lambda () (raise "post condition check failed"))
       ;;
       ;; post-changes: return a procedure taking as many arguments as
       ;; elements in the first argument to `observable-connect!` to
       ;; be bound to the old values returning a procedure taking the
       ;; same number to be bound to the new values return nothing
       ;; specific.  The latter is executed once the transaction
       ;; commenced.
       post-changes: (lambda (old) (lambda (new) (set! success (list old new)))))))
   (run-observed!
    (lambda () (observable-set! ob 42)))
   (thread-yield!)
   (equal? '(23 42) success)))

(test-assert
 "toggle connections ...\n"
 (let ((x (make-observable 23 #f #f 'x))
       (y (make-observable 42 #f #f 'y))
       (success #f))
   (let ((switch
          (with-current-transaction
           (lambda ()
             #f
             (observable-connect!
              (list x y)
              switchable: #t
              check-values: (lambda (x y) (= (+ x y) 65))
              )))))
     (test-error
      "invalid change rejected when check is switched on"
      (run-observed!
       (lambda () (observable-set! x 13) (observable-set! y 42) #t)))
     (test-assert
      "valid change passes"
      (run-observed!
       (lambda () (observable-set! x 42) (observable-set! y 23) #t)))
     (switch #f)
     (test-assert
      "invalid change passes while check switch is off"
      (run-observed!
       (lambda () (observable-set! x 42) (observable-set! y 13) #t)))
     (switch #t)
     (test-error
      "invalid change rejected when check is switched on"
      (run-observed!
       (lambda () (observable-set! x 25) (observable-set! y 42) #t)))
     )
   (equal? '(42 23) (with-current-transaction (lambda () (observable-apply list (list x y)))))
   #t))

;; ($implicit-current-transactions #f)

(test-assert
 "likely still working"
 (=
  (let ((x (make-observable 23 #f #f 'x1))
        (y (make-observable 42 #f #f 'y1)))
    (with-current-transaction (lambda () (observable-apply + (list x y)))))
  65))

(tests-end)
