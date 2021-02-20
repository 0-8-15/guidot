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

(define-macro (promise? x) `(##promise? ,x))

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
   (else (error "observable-set! unhandled reference" var)))
  #!void)

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

(define $async-exceptions (make-parameter 'ignored))

(define (make-observable-triggers #!key (async #f))
  (define (handle-async-exception ex)
    (display "In " (current-error-port))
    (display (current-thread) (current-error-port))
    (display #\space (current-error-port))
    (##default-display-exception ex (current-error-port)))
  (define (check-not-in-transaction)
    (and #t ;; DEBUG
         (lambda ()
           (if (current-transaction)
               (stm-consistency-error 'make-observable-triggers thunk)))))
  (define (start-async! thunk name check)
    (thread-start!
     (make-thread
      (lambda ()
        (cond
         ((not check)
          (begin
            (MATURITY -2 "no check requested, checking anyway" loc: make-observable-triggers)
            (check-not-in-transaction)))
         ((procedure? check) (check))
         (else (check-not-in-transaction)))
        (if check (check))
        (case ($async-exceptions)
          ((ignored) (thunk))
          (else (with-exception-catcher handle-async-exception thunk))))
      name)))
  (define (async-consequence! x)
    (cond
     ((procedure? x) (start-async! (lambda () (kick! x)) 'consequence #f))
     ((promise? x)
      (start-async!
       (lambda () (kick! (lambda () (force x))))
       'consequence/promise #f))
     ((and (box? x) (let ((t (unbox x))) (and (procedure? t) t))) =>
      (lambda (thunk)
        ;; (stm-log 'atomic-with-consequence " starting new thread" thunk)
        (start-async!
         (lambda ()
           (let ((kicking (thunk)))
             (if (procedure? kicking) (kick! kicking))))
         'atomic-with-consequence
         (and #t ;; DEBUG
              (lambda ()
                (if (current-transaction) (stm-consistency-error 'atomic-with-consequence thunk)))))))
     (else
      (stm-log 'async-handler "USELESS handler MAYBE BETTER STM consistency error" x)
      (cond-expand
       (gambit
        (let ((port (current-error-port)))
          (continuation-capture
           (lambda (cont)
             ;; (display-exception-in-context e cont port)
             (display-continuation-backtrace cont port #t #t 10 10 -2)))))
       (else #f))
      #f)))
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
	       (stm-log 'stm-trace-triggers "Transaction name deps" (list t (observable-name s) deps)))
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
		   (stm-log 'stm-trace-triggers "Phase I trigger returns" (list thunk next)))
	       (lset-union eq? init (if (or (pair? next) (null? next)) next (list next)))))
	   (list (lambda () "The elephant in Cairo for the sake of `lset-union`."))
	   l))
   ;; Post commit trigger.  Signal change.  MAY trigger additional
   ;; transactions.  SHOULD not block.  Note: those procedures should be
   ;; aware that they are called with the trigger setting in effect.
   ;; Better send asynchronous operations to pre-created threads than
   ;; forking threads from within.
   (case async
    ((#t)
     (lambda (l)
       (if ($debug-trace-triggers)
           (stm-log 'stm-trace-triggers "Post transaction async triggers" l))
       (for-each async-consequence! l)))
    ((kick)
     (lambda (l)
       (if ($debug-trace-triggers)
           (stm-log 'stm-trace-triggers "Post transaction kick triggers" l))
       (for-each kick! l)))
    (else
     (lambda (l)
       (if ($debug-trace-triggers)
           (stm-log 'stm-trace-triggers "Post transaction triggers" l))
       (for-each (lambda (thunk) (thunk)) l))))))

(define observable-triggers (make-observable-triggers))

(define (check-observable-sequential*!)
  ;; Runtime assert (within the thread) that the stm critial mutex is held.
  (unless (eq? (mutex-state *hope*) (current-thread))
    (raise 'stm-critical-region-not-locked)))

(define check-observable-sequential!
  ;; Runtime assert (globally) that the stm critial mutex is held.
  (let ((error-out
         (dynamic (lambda (caller more) (stm-consistency-error "observable NOT sequential" caller more)))))
    (lambda (caller . more)
      (unless (eq? (mutex-state *hope*) (current-thread))
        (error-out caller more)))))

(define stm-critical-execute!
  (dynamic
   (lambda (thunk)
     ;; Assert that the stm critial mutex is held.
     (unless (thread? (mutex-state *hope*))
             (raise 'stm-critical-region-not-locked))
     (parameterize
      (($implicit-current-transactions #f))
      (with-exception-catcher
       (lambda (ex)
         (stm-consistency-error
          "toplevel-execute!" thunk
          (call-with-output-string (lambda (p) (##default-display-exception ex p)))))
       thunk)))))

(define %kick-style 'async)

(define $kick-style
  (case-lambda
   (() %kick-style)
   ((x) (set! %kick-style x))))

(define kick!
  (let ((triggers-async (make-observable-triggers async: #t))
        (triggers-kick (make-observable-triggers async: 'kick)))
    (define (%passive thunk)
      (let ((kicking (thunk)))
        (cond
         ((procedure? kicking) (kick! kicking)))))
    (define (%passive/check thunk)
      (if (current-transaction) (stm-consistency-error 'kick-passive-within-transaction thunk)) ;; DEBUG
      (%passive thunk))
    (define (%kick! thunk)
      (parameterize
       ((current-trigger-handler
         (case %kick-style
           ((async) triggers-async)
           ((sync) triggers-kick)
           (else #f)))
        #;($stm-retry-limit 0)
        #;($debug-trace-triggers #t)
        )
       (with-current-transaction thunk)))
    (lambda (thunk)
      (cond
       ((and (box? thunk) (let ((t (unbox thunk))) (and (procedure? t) t))) => %passive/check) ;; before
       ((current-transaction) (%passive thunk))
       (else (%kick! thunk))))))

(define (kick/sync! thunk)
  (if (eq? ($kick-style) 'sync)
      (kick! thunk)
      (parameterize
       (($kick-style 'sync))
       (kick! thunk))))

;; Short procedural interface.  NOT recommended for eventual use,
;; except as a drop in compatible to parameters. It just hides too
;; much.  Better deploy with observables.  Otherwise nice for scripts
;; as it saves typing.
(define (%make-lval x)
  (define conv (lambda (x) (if (procedure? x) (x #f #f) x)))
  ;; has only the output side
  (case-lambda
   (() (observable-deref x))
   ((v)
    ;; FIXME: $implicit-current-transactions was a bad idea
    ;;
    ;; TODO: await this check failing for a while and abstain from testing
    (if (not (current-transaction)) (error "make-rval: $implicit-current-transactions are a bad idea"))
    (observable-set! x v) #!void)
   ((k p . more) ;; INTERNAL API; TODO protect from uncontrolled access
    (cond
     ((not (or k p)) x)
     ;;; FIXME: sieh zu, daß das erstmal mit spezialisiertem
     ;;; `observable-connect!` funktioniert!
     ;;;
     ;;; `call-with-overwrite` kann/sollte danach transaktionsfest
     ;;; werden.
     ;;;
     ;;; Der eine Testfall wäre anzupassen und im Probiercode steht
     ;;; ein guter Fall von "schön", der so bleiben sollte.
     ;;;
     ;;; NB: `:=` ist gar nicht so schlecht.
     ((and (null? k) (string? p) (pair? more) (procedure? (car more)))
      (let* ((mk (car more))
             (more (cdr more))
             (sig (if (pair? more) (car more) #f))
             (params (map conv k)))
        (connect-dependent-value! p mk (or sig '()) (cons x params))))
     (else
      (let ((params (map conv k)))
        (apply observable-connect! (cons x params) p more)))))))

(define (%make-rval x)
  ;; authoritative input and top level, implicit transactional access
  (case-lambda
   (() (observable-deref x))
   ((v)
    (if (current-transaction)
        (observable-set! x v)
        (if #f ;; kick or not? TBD: for now kick
            (with-current-transaction (lambda () (observable-set! x v)))
            (kick! (lambda () (observable-set! x v))))))
   ((k p . more)
    (error "make-rval: may not connect to the input side of a wire"))))

(define (make-lval val . more) ;;
  (%make-lval (apply make-observable val more)))

(define (make-mval val . more) ;; intermediate
  (let ((x (apply make-observable val more)))
    (values
     (%make-rval x)
     ;; output side
     (%make-lval x))))

(define (wire! s . more)
  (define lval? procedure?)
  (let* ((s
          (cond
           ((pair? s) s)
           ((lval? s) (list s))
           (else (error "wire!: argument error" s))))
         (hooker (car s)))
    (unless (lval? hooker) (error "wire!: argument error" hooker))
    (apply hooker (cdr s) more)))

#|
(: connect-dependent-value!
   ((or string false)
    (procedure () (or string false)) (or :connsig: (list-of :connsig:))
    &rest (struct control-variable)
    -> *))
|#
(define (connect-dependent-value! value thunk sig params)
  (define (procedure-or-promise? x)
    (or (procedure? x) (promise? x)))
  (let ((action (cond
		 ((not value)
                  (cond
                   ((procedure? thunk)
                    (lambda (tnx) (thunk) (lambda () sig)))
                   ((promise? thunk)
                    (lambda (tnx) (force thunk) (lambda () sig)))
                   (else
                    (lambda (tnx) (lambda () sig)))))
                 ((procedure? value)
                  (or (procedure? thunk) (promise? thunk)
                      (error "value needs constructor"))
                  (lambda (tnx)
                    ;; exception will abort here
                    (let ((prepared
                           (cond
                            ((procedure? value) (call-with-values thunk value))
                            (else (thunk (force value))))))
                      (lambda ()
                        ;; STM critical part
                        (let ((immediate (or (procedure? prepared) (promise? prepared))))
                          (if (or immediate (box? prepared))
                              (let ((additional
                                     (if immediate
                                         (if (procedure? prepares) (prepared) (force prepared))
                                         (stm-critical-execute! (unbox prepared)))))
                                (cond
                                 ((procedure-or-promise? additional)
                                  (cond
                                   ((pair? sig) (cons additional sig))
                                   ((procedure-or-promise? sig) (list additional sig))
                                   (else additional)))
                                 (else sig)))
                              sig))))))
                 ((string? value) (lambda (tnx) (call-with-overwrite value thunk sig)))
		 (else (error "connect-dependent-value! unhandled value" value)))))
    (with-current-transaction
     (lambda () (for-each (lambda (p) (observable-regref! p action)) params)))
    action))

(define (exported-connect-dependent-value! value thunk sig params)
  (connect-dependent-value! value thunk sig params)
  #!void)

(define (observable-connect!
         params #!key
         ;; FIXME: sort these by phase/use
         (critical #f)
         (extern #f)
         (check #f)
         (check-values #f)
         (sequence #f)
         (post-changes #f)
         (post #f)
         (switchable #f))
  (when (and sequence post-changes)
        (error "observable-connect! : use either sequence: or post-changes:" params))
  (let* ((all-checks
          (cond
           ((procedure? check-values)
            (let ((vc (lambda ()
                        (or (apply check-values (map observable-deref params))
                            (error "check-values failed" check-values)))))
              (if (procedure? check) (lambda () (vc) (check)) vc)))
           (else check)))
         (checks
          (lambda (extern)
            (if (procedure? extern)
                (if (procedure? all-checks)
                    (lambda () (all-checks) (extern))
                    extern)
                (if (procedure? all-checks)
                    (lambda () (all-checks) #f)
                    #f))))
         (post-changes
          (cond
           ((procedure? post-changes)
            (lambda () ;; save old values while we have both
              (let ((ov (map (lambda (v) (%observable-value v)) params)) ;; out
                    (nv (map observable-deref params)))
                (lambda ()
                  ;; defer over critical phase - not boxed => in dyn scope
                  (lambda ()
                    ;; defer to post phase
                    (apply (apply post-changes ov) nv))))))
           ((procedure? sequence)
            (lambda () ;; save old values while we have both
              (let ((ov (map (lambda (v) (%observable-value v)) params)) ;; out
                    (nv (map observable-deref params)))
                (lambda ()
                  ;; defer over critical phase - not boxed => in dyn scope
                  (lambda ()
                    ;; defer to post phase
                    (let ((alt (let loop ((ov ov) (nv nv))
                                 (if (null? ov) '()
                                     (cons (car ov) (cons (car nv) (loop (cdr ov) (cdr nv))))))))
                      (apply sequence alt)))))))
           (else #f)))
         (thunk (checks
                 (if (procedure? extern)
                     extern
                     (if (or (procedure? critical) (procedure? post-changes))
                         (lambda () #f)
                         #f))))
         (recv (cond
                ((procedure? post-changes)
                 (if (procedure? critical)
                     (lambda args
                       (let ((final (post-changes)))
                         (box
                          (lambda ()
                            (let ((crit (apply critical args)))
                              (cond
                               ((procedure? crit) (list crit final))
                               ((pair? crit) (cons final crit))
                               (else final)))))))
                     (lambda args (post-changes))))
                ((procedure? critical)
                 (lambda args (box (lambda () (apply critical args)))))
                (else #f))))
    (let ((trigger (connect-dependent-value! recv thunk (or post '()) params)))
      ;; Hm.  Maybe this should be critical too.
      (and
       switchable ;; return a procedure to toggle or switch on/off
       (let ((on (lambda ()
                   (connect-dependent-value! recv thunk post params)))
             (off (lambda ()
                    (for-each
                     (lambda (var) (observable-unref! var trigger))
                     params))))
         (let ((alt off))
           (lambda opt
             (let ((then
                    (cond
                     ((null? opt) alt)
                     ((car opt) on)
                     (else off))))
               (if (eq? then alt)
                   (begin
                     (set! alt (if (eq? then off) on off))
                     (then)
                     #t)
                   #f)))))))))
