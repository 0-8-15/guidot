(define-type observable
  macros: prefix: %
  (tag unprintable: equality-skip:)
  value
  (deps-tag unprintable: equality-skip:)
  (deps unprintable: equality-skip:)
  name
  pred
  filter
  )

(define-macro (%check-observable! obj where)
  `(or (%observable? ,obj) (error "not an observable" ,where ,obj)))

(define (make-observable v #!optional pred filter name)
  (let ((v (if filter (filter v v) v)))
    (if (and pred (not (pred v)))
        (error "value did not pass guard predicate" pred v))
    (%make-observable 0 v 0 '() name pred filter)))

(define (observable? var) (%observable? var))
(define (observable-deps var) (%observable-deps var))
(define (observable-name var) (%observable-name var))
(define (observable-pred var) (%observable-pred var))
(define (observable-filter var) (%observable-filter var))

(define (observable-deref var)
  (cond
   ((%stmref? var) (%cell-ref var))
   ((not (%observable? var)) (error "observable-deref unhandled" var))
   ((current-transaction) =>
    (lambda (transaction)
      (%cell-ref (make-tslot-ref transaction var 1))))
   (else (%observable-value var))))

(define (observable-reference var)
  (%check-observable! var 'observable-reference)
  (let ((transaction (current-transaction)))
    (if (%stmtnx? transaction)
        (make-tslot-ref transaction var 1)
        (error "observable-reference: not a transaction" transaction))))

(define (observable-references transaction vars where)
  (map
   (lambda (var)
     (%check-observable! var where)
     (make-tslot-ref transaction var 1))
   vars))

(define (%observable-invoke! transaction proc vars)
  (apply proc (observable-references transaction vars 'observable-invoke!)))

(define (observable-invoke! proc vars)
  (let ((transaction (current-transaction)))
    (if (%stmtnx? transaction)
        (%observable-invoke! transaction proc vars)
        (with-implied-current-transaction
         (lambda () (%observable-invoke! (%current-transaction) proc vars))
         'observable-invoke!))))

(define (%observable-apply transaction proc vars)
  (define (references transaction vars)
    (map
     (lambda (var)
       (%check-observable! var 'observable-apply)
       (%cell-ref (make-tslot-ref transaction var 1)))
     vars))
  (apply proc (references transaction vars)))

(define (observable-apply proc vars)
  (let ((transaction (current-transaction)))
    (if (%stmtnx? transaction)
        (%observable-apply transaction proc vars)
        (with-implied-current-transaction
         (lambda () (%observable-apply (%current-transaction) proc vars))
         'observable-apply))))

(define-macro (%filter target ref old val rcv)
  (let ((pred (gensym 'pred))
        (filter (gensym 'filter))
        (new (gensym 'new))
        (check-pred
         (lambda (target pred val)
           `(or (not (or ,pred
                         (and (%observable? ,val) ;; likely an error
                              (raise "observables may only be used with test predicate"))))
                (,pred ,val) ;; but pass if there is a predicate saying so
                (error "value did not pass guard predicate" ,target ,val)))))
    `(let ((,pred (%observable-pred ,target)))
       (let ((,filter (%observable-filter ,target)))
         (case ,filter
           ((#f) ,(check-pred target pred val) (,rcv ,ref ,val))
           ((#t)
            (unless (eqv? ,old ,val) ,(check-pred target pred val) (,rcv ,ref ,val)))
           (else (,rcv ,ref (let ((,new (,filter ,old ,val)))
                              ,(check-pred target pred new) ,new))))))))

(define (observable-set! var val)
  (cond
   ;; preconditions
   ((%stmref? val) (raise "observable-set! reference escaped"))
   ;; alternatives
   ((and (%stmref? var)
         (let ((source (%stmref-source var))) (and (%observable? source) source)))
    => (lambda (source)
         (%filter source var (%cell-ref var) val %alter!)))
   ((%observable? var)
    (%filter var var (%observable-value var) val (lambda (var val) (alter-current-slot! var 1 val))))
   (else (error "observable-set! unhandled reference" var))))

(define (observable-alter! ref f . args)
  (observable-invoke!
   (lambda ()
     (let* ((ref (observable-reference ref))
            (new (apply f (observable-deref ref) args)))
       (observable-set! ref new)
       ;; re-read as filters could have changed the value
       (%cell-ref ref)))
   '()))

(define (observable-deps-reference var transaction)
  (%check-observable! var 'observable-deps-reference)
  (make-tslot-ref transaction var 3))

(define (observable-deps-alter! var f . args)
  ;; (%check-observable! var 'observable-deps-alter!)
  (with-current-transaction
   (lambda ()
     (let* ((ref (make-tslot-ref (current-transaction) var 3))
            (old (%cell-ref ref))
            (new (apply f old args)))
       (or (eq? old new) (%alter! ref new))))))

(define (observable-regref! var trigger)
  (%check-observable! var 'observable-regref!)
  (or (procedure? trigger) (error "not a procedure" 'observable-regref! trigger))
  (or (memq trigger (%observable-deps var)) ;; short cut
      (let ((insert (lambda (deps trigger) (if (memq trigger deps) deps (cons trigger deps)))))
        (observable-deps-alter! var insert trigger))))

(define (observable-unref! var trigger)
  (define (del regs trigger)
    (if (memq trigger regs)
        (let loop ((regs regs))
          (if (eq? (car regs) trigger)
              (cdr regs)
              (cons (car regs) (loop (cdr regs)))))
        regs))
  (%check-observable! var 'observable-regref!)
  (observable-deps-alter! var del trigger))

(define $debug-trace-triggers (make-parameter #f))

(define observable-triggers
  (make-trigger-handler
   ;; Return initial value for fold operation.
   (lambda () '() #;(list (lambda () (lambda () '()))) )
   ;; folding function; returns list of procedures receiving a
   ;; transaction to call.  Those should prepare for a commit.  This
   ;; procedure as well as those thunks MUST NOT change additional STM
   ;; values let alone cause side effects.  They may fail, aborting
   ;; the commit.
   (lambda (t s n i)
     (if (and (%observable? s) (= n 1))
	 (let ((deps (observable-deps s)))
	   (if ($debug-trace-triggers)
	       (debug "Transaction name deps" (list t (observable-name s) deps)))
	   (lset-union eq? i deps))
	 i))
   ;; Sync function receives a list of thunks which MUST NOT fail to
   ;; complete the commit.  Still within the commit protocol: MUST NOT
   ;; change STM values or contain nested transactions but may
   ;; cause/sync other side effects.
   (lambda (l)
     (fold (lambda (thunk init)
	     (let ((next (thunk)))
	       (if ($debug-trace-triggers)
		   (debug "Phase I trigger returns" (list thunk next)))
	       (lset-union eq? init (if (or (pair? next) (null? next)) next (list next)))))
	   (list (lambda () "The elephant in Cairo for the sake of `lset-union`."))
	   l))
   ;; Post commit trigger.  Signal change.  MAY trigger additional
   ;; transactions.  SHOULD not block.  Note: those procedures should be
   ;; aware that they are called with the trigger setting in effect.
   ;; Better send asynchronous operations to pre-created threads than
   ;; forking threads from within.
   (lambda (l)
     (if ($debug-trace-triggers)
	 (debug "Post transaction triggers" l))
     (for-each (lambda (thunk) (thunk)) l)
     ;; (trail-complete! *default-trail*)
     )))

(define stm-critical-execute!
  (dynamic
   (lambda (thunk)
     ;; Assert that the stm critial mutex is held.
     (unless (thread? (mutex-state *hope*))
             (raise 'stm-critical-region-not-locked))
     (parameterize
      (($implicit-current-transactions #f))
      (with-exception-catcher
       (lambda (ex) (stm-consistency-error "toplevel-execute!" ex))
       thunk)))))

#|
(: connect-dependent-value!
   ((or string false)
    (procedure () (or string false)) (or :connsig: (list-of :connsig:))
    &rest (struct control-variable)
    -> *))
|#
(define (connect-dependent-value! value thunk sig params)
  (let ((action (cond
		 ((not value)
                  (if (procedure? thunk)
                      (lambda (tnx) (thunk) (lambda () sig))
                      (lambda (tnx) (lambda () sig))))
                 ((procedure? value)
                  (or (procedure? thunk) (error "value needs constructor"))
                  (lambda (tnx)
                    ;; exception will abort here
                    (let ((prepared (call-with-values thunk value)))
                      (lambda ()
                        ;; STM critical part
                        (if (procedure? prepared)
                            (let ((additional (stm-critical-execute! prepared)))
                              (cond
                               ((procedure? additional)
                                (cond
                                 ((pair? sig) (cons additional sig))
                                 ((procedure? sig) (list additional sig))
                                 (else additional)))
                               (else sig)))
                            sig)))))
                 ((string? value) (lambda (tnx) (call-with-overwrite value thunk sig)))
		 (else (error "connect-dependent-value! unhandled value" value)))))
    (with-current-transaction
     (lambda () (for-each (lambda (p) (observable-regref! p action)) params)))))

(define (observable-connect!
         params #!key
         (critical #f)
         (extern #f)
         (check #f)
         ;; (check-changes #f) ;; NO impossible
         (post-changes #f)
         (post #f))
  (let* ((all-checks (and check))
         (checks
          (lambda (extern)
            (if (procedure? extern)
                (if (procedure? all-checks)
                    (lambda () (all-checks) (extern))
                    extern)
                (if (procedure? all-checks)
                    (lambda () (all-checks) #f)
                    #f))))
         (thunk (checks
                 (if (procedure? extern)
                     extern
                     (if (procedure? critical)
                         (lambda () #f)
                         #f))))
         (post-changes
          (and (procedure? post-changes)
               (lambda () ;; save old values while we have both
                 (let ((ov (map (lambda (v) (%observable-value v)) params)) ;; out
                       (nv (map observable-deref params)))
                   (lambda ()
                     ;; defer over critical phase
                     (lambda ()
                       ;; defer to post phase
                       (apply (apply post-changes ov) nv)))))))
         (recv (cond
                ((procedure? post-changes)
                 (if (procedure? critical)
                     (lambda args
                       (let ((final (post-changes)))
                         ;; FIXME: don't forget 'critical's return, if any
                         (lambda () (apply critical args) final)))
                     (lambda args (post-changes))))
                ((procedure? critical)
                 (lambda args (lambda () (apply critical args))))
                (else #f))))
    (connect-dependent-value! recv thunk post params)))
