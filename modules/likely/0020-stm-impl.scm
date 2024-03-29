(define (stm-log k l v)
  (let ((p  (current-error-port)))
    (display
     (call-with-output-string
      (lambda (p)
        (display k p)
        (display " ")
        (display (current-thread) p)
        (display " " p)
        (display l p)
        (display ": " p)
        (write v p)
        (newline p)))
     p)
    ((cond-expand (gambit force-output) (chicken flush-output)) p)
    v))

(define stm-warning (dynamic stm-log))

(define-macro (stm-error msg . args)
  `(begin
     (stm-warning 'stm-error ,msg (list . ,args))
     (error ,msg . ,args)))

(define-macro (stm-consistency-error msg . args)
  `(begin
     (stm-warning 'stm-consistency-error ,msg (list . ,args))
     (error ,msg . ,args)))

;;;* implementation

;;; NOTE: The following changes where likely a good idea
;;; (but introduce complexity, hence left for later):
;;;
;;; 1. Do NOT load the content of a ref until first actual access.
;;; (Tag it with #f on create.)
;;;
;;; 2. Instead of breaking references and completly wiping the
;;; reference table from the transaction set their tags to #f (once
;;; that's handled, see before).
;;;
;;; 3. Avoid re-lookup of references.
;;;
;;; 4. Lift the phase 2 of trigger handlers into a new phase where
;;; they may add STM locations to the current transaction.

;;;** local envt

(define-macro (sub1 x) `(fx- ,x 1))
(define-macro (add1 x) `(fx+ ,x 1))
(define-macro (next-version x) `(fx+ ,x 2))

(define *hope* (make-mutex '*hope*))

;;;** algorithm

(define stm-current-global-clock-value (vector 1))

(define-macro (stm-current-global-clock) `(vector-ref stm-current-global-clock-value 0))

(define (exported-stm-current-global-clock) (stm-current-global-clock))

(define (stm-clock-tick-penealty!)
  ;; conflict penealty
  (thread-yield!))

(define (stm-clock-tick!)
  (declare (not interrupts-enabled))
  (let* ((old (stm-current-global-clock))
         (new (next-version old)))
    (if (eq? (vector-cas! stm-current-global-clock-value 0 new old) old)
        new
        (begin
          (stm-clock-tick-penealty!)
          (stm-clock-tick!)))))

(define-macro (%%outdated transaction source-tag) `(< (add1 (%stmtnx-id ,transaction)) ,source-tag))

(define-macro (new-transaction-identifier) `(stm-current-global-clock))

(define-type stmtnx
  macros: prefix: %
  id refs ht owner)

(define-type trigger-handler
  (new no-functional-setter:)
  (merge no-functional-setter:)
  (sync no-functional-setter:)
  (done no-functional-setter:))

(define %current-transaction
  (make-parameter #f))

(define (current-transaction)
  (let ((ct (%current-transaction)))
    (and ct (eq? (%stmtnx-owner ct) (current-thread)) ct)))

(define-macro (stm-pure?)
  ;; No STM context visible so far.
  `(let ((t (current-transaction)))
    (or (not t) (and (%stmtnx? t) (null? (%stmtnx-refs t))))))

(define (stm-atomic?) (not (current-transaction)))

(define current-trigger-handler (make-parameter #f))

(define (make-object-table) (make-table hash: eq?-hash))
(define (object-table-update! table key transform default)
  (let* ((unique (vector #f))
         (old (table-ref table key unique))
         (val (transform (if (eq? old unique) (default) old))))
    (table-set! table key val)
    val))

(define (new-transaction . x)
  (%make-stmtnx
   (new-transaction-identifier)
   '()
   (make-object-table)
   (current-thread)))

(cond-expand
 ((or gambit)
  (define-macro (transaction-extend! t r)
    `(begin
       (cond-expand
        (debug
         (if (even? (%stmtnx-id ,t))
             (stm-consistency-error "transaction already closed"))
         (if (not (eq? (%stmtnx-owner ,t) (current-thread)))
             (stm-consistency-error "transaction owned by thread" (%stmtnx-owner ,t))))
        (gambit)
        (else))
       (%stmtnx-refs-set! ,t (cons ,r (%stmtnx-refs ,t)))))
  )
 (else
  (define-inline (transaction-extend! t r)
    (cond-expand
     (debug
      (if (even? (%stmtnx-id t))
          (stm-consistency-error "transaction already closed"))
      (if (not (eq? (%stmtnx-owner t) (current-thread)))
          (stm-consistency-error "transaction owned by thread" (%stmtnx-owner t))))
     (else))
    (%stmtnx-refs-set! t (cons r (%stmtnx-refs t))))))

(define (transaction-reset! t)
  ;; (%stmtnx? t)
  (%stmtnx-refs-set! t '())
  (%stmtnx-ht-set! t (and (%stmtnx-ht t) #t)))
(define (transaction-close! t)
  (%stmtnx-id-set! t (next-version (%stmtnx-id t)))
  (transaction-reset! t))

(define (transaction-reopen! t)
;;  (assert (even? (%stmtnx-id t)))
  ;; (%stmtnx-id-set! t (sub1 (%stmtnx-id t))) ;; hopefully style
  (%stmtnx-id-set! t (new-transaction-identifier))
  (if (%stmtnx-ht t)
      (%stmtnx-ht-set! t (make-object-table))))

;; Named after the Clojure equivalent for atoms.
(define (compare-and-set-slot! obj n old new)
  (eqv? (##unchecked-structure-cas! obj new old n #f 'compare-and-set-slot!) old))

(define (compare-and-set-islot! obj n old new)
  (eqv? (##unchecked-structure-cas! obj new old n #f 'compare-and-set-islot!) old))

(define (obj+slot-table-update! t obj slot default)
  (let* ((st (object-table-update!
              t obj identity
              (lambda () (make-vector (fxquotient (sub1 (##structure-length obj)) 2) #f))))
         (i (fxquotient (sub1 slot) 2)))
    (or (##unchecked-structure-ref st i 'vector 'obj+slot-table-update!)
        (let ((v (default)))
          (##unchecked-structure-set! st v i 'vector 'obj+slot-table-update!)
          v))))

;;;;

(define-type stmref
  macros: prefix: %
  source slot tag val #;transaction)

(define %%report-outdated-referenced #f) ;; FIXME: handle this case

(define (make-tslot-ref transaction source slot) ;; oddly named internal unsafe
  ;; (ensure fixnum? slot)
  (define (make-cell)
    (declare (not interrupts-enabled))
    ;; FIRST read the tag, than the value.
    (let* ((tag (##unchecked-structure-ref source slot 'any 'make-cell))
           (cell (%make-stmref source slot tag (##unchecked-structure-ref source (add1 slot) 'any 'make-cell) #;transaction)))
      (transaction-extend! transaction cell)
      (if (%%outdated transaction tag) ;; FIXME: handle this case
          (if %%report-outdated-referenced
              (stm-warning "creating already outdated reference" source (list 'TID (%stmtnx-id transaction) 'TAG tag 'CELL cell))))
      cell))
  (ensure %stmtnx? transaction)
  (if (fx>= slot (##structure-length source)) (stm-error "slot not within structure" source slot))
  (if (even? slot) (stm-consistency-error "even slot numbers are reserved" source slot))
  (obj+slot-table-update! (%stmtnx-ht transaction) source slot make-cell))

(define-macro (%alter! cell val) `(%stmref-val-set! ,cell ,val))

(define (alter! cell val)
  (ensure %stmref? cell)
  (%alter! cell val))

;; %cell-ref : either fast reads with possible more work to be
;; re-done.  Or check every time.
;;
(define-macro (%cell-ref cell) `(%stmref-val ,cell))
#;(define-macro (%cell-ref cell)
`(if (eq? (%stmref-tag ,cell)
(##unchecked-structure-ref (%stmref-source ,cell) (%stmref-slot ,cell) 'any 'make-cell))
(%stmref-val ,cell)
nonono: (raise 'stm-conflict)))

(define (cell-ref cell)
  (ensure %stmref? cell)
  (%cell-ref cell))

(define-macro (the type val) val)

(define-macro (handle-exceptions exn handler . body)
  (let ((oh (gensym 'oh)))
    `(let ((,oh (current-exception-handler)))
       (with-exception-handler
        (lambda (,exn) (with-exception-handler ,oh (lambda () ,handler)))
        (lambda () . ,body)))))

(define-macro (%break-reference! x) `(%stmref-source-set! ,x #f))

(define-macro (%unhold! x i l) `(##unchecked-structure-set! ,x ,l ,i 'any '%unhold!))
(define-macro (%update! x i l n)
  `(begin
     ;; FIRST wite the value, then the tag.
     (##unchecked-structure-set! ,x ,n (add1 ,i) 'any '%update!)
     (%unhold! ,x ,i ,l)))

(define (transaction-commit! transaction)
  (define (undo-dirty-tagging! dirty)
    (for-each
     (lambda (x)
       (cond-expand
        (no-dirty-tagging #t)
        (else
         (##unchecked-structure-set!
          (%stmref-source x) (%stmref-tag x) (%stmref-slot x) 'any 'transaction-commit)))
       (%break-reference! x))
     dirty))
  (define (unlock-and-return result)
    (let ((mx *hope*))
      (if (eq? (mutex-state mx) (current-thread)) (mutex-unlock! mx)))
    result)
  (ensure %stmtnx? transaction)
  #;(if (txnclosed? (%stmtnx-id transaction))
  (error "transaction already closed"))
  #;(if (not (eq? (stmtnx-owner transaction) (current-thread)))
  (error "transaction owned by thread" (stmtnx-owner transaction)))
  (when (eq? (mutex-state *hope*) (current-thread))
        (stm-consistency-error "attempt to commit during commit" *hope* transaction))
  (let ((lock-tag (%stmtnx-id transaction)))
    (let loop ((refs (%stmtnx-refs transaction))
               (dirty '()))
      (if (null? refs)
          (if (null? dirty)
              (begin
                (transaction-close! transaction)
                (unlock-and-return transaction))
              (let ((trigger-handler (current-trigger-handler))
                    (found (the (or boolean pair) #f)))
                (if
                 (or (not trigger-handler)
                     ;; TBD lift this before a re-check of dirty references.
                     (with-exception-catcher
                      (lambda (ex)	;; Conflict. Undo dirty tagging.
                        (undo-dirty-tagging! dirty)
                        (transaction-close! transaction) ;; or should this be done elsewhere?
                        (raise (unlock-and-return ex))
                        #f)
                      (lambda ()
                        (set! found (map (lambda (proc) (proc transaction))
                                         (fold (let ((merge (trigger-handler-merge trigger-handler)))
                                                 (lambda (x i)
                                                   (merge lock-tag (%stmref-source x) (%stmref-slot x) i)))
                                               ((trigger-handler-new trigger-handler))
                                               dirty)))
                        #t)))
                 (begin
                   (mutex-lock! *hope*)
                   ;; once we took the global lock we re-check dirty for conflicts
                   (if (any (lambda (x)
                               (let ((source (%stmref-source x)) (slot (%stmref-slot x)))
                                 (let ((tag (##unchecked-structure-ref source slot 'any 're-check)))
                                   (%%outdated transaction (%stmref-tag x)))))
                             dirty)
                       (begin
                         ;; clean up and hope for restart
                         (undo-dirty-tagging! dirty)
                         (transaction-close! transaction)
                         (unlock-and-return #f))
                       (let ((clock (stm-clock-tick!)))
                         (for-each
                          (lambda (x)
                            (let ((source (%stmref-source x)) (slot (%stmref-slot x)))
                              ;; hopefully style
                              ;;
                              ;; (%update! source slot (next-version (%stmref-tag x)) (%stmref-val x))
                              (let ((slot-clock (add1 clock)))
                                (%update! source slot slot-clock (%stmref-val x)))
                              (%stmref-source-set! x #f)))
                          dirty)
                         ;; Apply STM critical sections with lock still held
                         (if found (set! found ((trigger-handler-sync trigger-handler) found)))
                         (transaction-close! transaction)
                         (unlock-and-return
                          (if found
                              (cons (trigger-handler-done trigger-handler) found)
                              transaction))))))))
          (let ((x (car refs)))
            (let ((source (%stmref-source x)) (slot (%stmref-slot x)))
              (let ((tag (and source (##unchecked-structure-ref source slot 'any 'transaction-commit!))))
                (cond
                 ((eqv? tag lock-tag) ;; eq?/eqv? ??
                  ;; Referenced in this transaction later (list is
                  ;; reverse access order).  Ignore prior ref.
                  (cond-expand
                   ((and (not debug) no-dirty-tagging) #t)
                   (else
                    (stm-consistency-error "Warning: 'hopefully' conflict free transaction ran into double reference\n" x transaction)))
                  (loop (cdr refs) dirty))
                 ((not (eq? tag (%stmref-tag x)))
                  ;; Conflict. Undo dirty tagging.
                  (cond-expand
                   ((and (not debug) no-dirty-tagging) #t)
                   (else
                    (undo-dirty-tagging! dirty)))
                  (transaction-close! transaction) ;; or should this be done elsewhere?
                  (unlock-and-return #f))
                 ((eqv? (##unchecked-structure-ref source (add1 slot) 'any 'transaction-commit!) (%stmref-val x))
                  ;; Unchanged, no dirty tagging, no updates.
                  (loop (cdr refs) dirty))
                 (else
                  (cond-expand
                   ((and (not debug) no-dirty-tagging)
                    (loop (cdr refs) (cons x dirty)))
                   (else
                    (if (compare-and-set-islot! source slot tag lock-tag)
                        (loop (cdr refs) (cons x dirty))
                        (begin	;; Conflict. Undo dirty tagging.
                          (undo-dirty-tagging! dirty)
                          (transaction-close! transaction) ;; or should this be done elsewhere?
                          (unlock-and-return #f))))))))))))))

(define-macro (call-post-triggers! commited)
  ;; Call post triggers if any.
  `(if (pair? ,commited) ((car ,commited) (cdr ,commited))))

(define $stm-retry-limit (make-parameter 100))

(define $stm-conflict-peanalty (make-parameter thread-yield!))

(define-macro (%conflict-loop retry . body)
  (let ((loop-counter (gensym 'loop-counter))
        (loop (gensym 'loop))
        (limit (gensym 'limit)))
    `(let ((,loop-counter 0))
       (letrec ((,retry
                 (lambda ()
                   ;; BEWARE: It's important to give CPU away for
                   ;; other means to possibly update
                   ;; ($stm-retry-limit) and only afterwards test
                   ;; against the loop counter.
                   (($stm-conflict-peanalty)) ;; conflict penealty
                   (set! ,loop-counter (add1 ,loop-counter))
                   (let ((,limit ($stm-retry-limit)))
                     (if (or (not ,limit) (< ,loop-counter ,limit))
                         (loop)
                         (stm-error "retry limit reached!" ,loop-counter ',retry)))))
                (loop (lambda () . ,body)))
         (loop)))))
#|
;; (: call-with-current-transaction/values ((procedure () . *) &rest boolean -> . *))
(define (call-with-transaction/values proc . heavy?)
  (let ((tnx (new-transaction (and (pair? heavy?) (car heavy?)))))
    (%conflict-loop
     loop
     (receive
      results (proc tnx)
      (let ((commited (transaction-commit! tnx)))
        (if commited
            (begin
              (call-post-triggers! commited)
              (apply values results))
            (begin
              (transaction-reopen! tnx)
              (loop))))))))

;; (: call-with-current-transaction ((procedure () . *) &rest boolean -> *))
(define (call-with-transaction proc . heavy?)
  (let ((tnx (new-transaction (and (pair? heavy?) (car heavy?)))))
    (%conflict-loop
     loop
     (let ((x (proc tnx)))
       (let ((commited (transaction-commit! tnx)))
         (if commited
             (begin
               (call-post-triggers! commited)
               x)
             (begin
               (transaction-reopen! tnx)
               (loop))))))))
;;|#
;; (: with-current-transaction ((procedure () . *) -> . *))
(define (with-current-transaction thunk)
  (if (current-transaction)
      (thunk)
      (let ((tnx (new-transaction #t)))
        (let ((x (parameterize
                  ((%current-transaction tnx))
                  (%conflict-loop
                   with-current-transaction-loop
                   (receive
                    results (thunk)
                    #;(if (transaction-commit! tnx)
                    results
                    (begin
                    (transaction-reopen! tnx)
                    (with-current-transaction-loop)))
                    (let ((post-triggers (transaction-commit! tnx)))
                      #;(when (eq? (mutex-state *hope*) (current-thread)) (stm-consistency-error "mutex left locked" *hope* tnx))
                      (if post-triggers
                          (cons results post-triggers)
                          (begin
                            (transaction-reopen! tnx)
                            (with-current-transaction-loop)))))))))
          (call-post-triggers! (cdr x))
          (apply values (car x))))))


(define $implicit-current-transactions (make-parameter #f)) ;; Not default on toplevel!

(define (with-implied-current-transaction thunk location)
  (if ($implicit-current-transactions)
      (with-current-transaction thunk)
      (stm-error "no current transaction" location)))

;; (: alter-current-slot! (* fixnum * -> *))
(define (alter-current-slot! source slot val)
  (let ((transaction (current-transaction)))
    (if transaction
        (%alter! (make-tslot-ref transaction source slot) val)
        (with-implied-current-transaction
         (lambda () (%alter! (make-tslot-ref (current-transaction) source slot) val))
         'alter-current-slot!))))
