(cond-expand
 (debug)
 (else
  (declare
   (standard-bindings)
   (extended-bindings) ;; no overwrites of standard bindings
   (not standard-bindings thread-start!) ;; except this
   (block)
   )))

(include "0001-dynamic-extent.scm")
(include "0002-srfi-1.scm")
(include "0003-tagged-proc-gambit.scm")
(include "0007-gambit-foreign.scm")
(cond-expand
 ((or test debug)
  (include "0010-debug.scm"))
 (else))

(define-macro (define-values names . body)
  (let ((arg1 (gensym 'arg1))
        (rest (gensym 'rest)))
    `(begin
       ,@(map (lambda (name) `(define ,name #f)) (cdr names))
       (define ,(car names)
         (call-with-values
             (lambda () unquote body)
           (lambda (,arg1 . ,rest)
             (begin
               ,@(map (lambda (name)
                        `(set! ,name
                               (let ((,name (car ,rest)))
                                 (set! ,rest (cdr ,rest))
                                 ,name)))
                      (cdr names))
               ,arg1)))))))

(define-macro (this-module-works name exports . body)
  (let ((extern (lambda (e) (if (pair? e) (car e) e)))
        (intern (lambda (e) (if (pair? e) (cadr e) e))))
    (let ((exported (map extern exports))
          (internal (map intern exports)))
      `(define-values ,exported ,@body (values . ,internal)))))

(define-macro (this-module name exports)
  (let ((extern (lambda (e) (if (pair? e) (car e) e)))
        (intern (lambda (e) (if (pair? e) (cadr e) e)))
        (module-name-end (string->symbol (string-append "module-" (symbol->string name) "-end"))))
    (let ((exported (map extern exports))
          (internal (map intern exports)))
      (let ((params (map (lambda (n) (string->symbol (string-append "p-" (number->string n)))) (iota (length internal))))
            (end-code `(,name . ,internal)))
        (eval `(define-macro (,module-name-end) ',end-code))
        `(begin
           (define-macro (,module-name-end) ',end-code)
           ,@(map (lambda (name) `(define ,name #f)) exported)
           (define ,name
             (lambda ,params . ,(map (lambda (e i) `(set! ,e ,i)) exported params))))))))

(this-module
 likely
 (with-current-transaction
  current-trigger-handler
  make-observable-triggers
  kick!
  kick/sync!
  make-lval make-mval wire!
  make-observable
  observable?
  observable-deps
  observable-name
  ;; observable-pred
  ;; observable-filter
  observable-deref
  observable-reference
  observable-set!
  observable-apply
  observable-alter! ;; like clojure API
  observable-invoke!
  observable-connect!
  (connect-dependent-value! exported-connect-dependent-value!) ;; BETTER NOT export these!_(?["now"])
  observable-regref!
  observable-unref!
  $stm-retry-limit ;; parameter
  $stm-conflict-peanalty
  $implicit-current-transactions
  (stm-current-global-clock exported-stm-current-global-clock)
  $async-exceptions
  $debug-trace-triggers
  ;; Higher level (kinds)
  make-pin pin? PIN
  (observable-filter-eq filter-eq) (observable-filter-eqv filter-eqv) (observable-filter-equal filter-equal)
  pin-filter!
  wire-persistent-file!
  ;; Debug & Deprecated
  ;;
  stm-atomic?
  check-observable-sequential!
  opportunistic-sequential check-not-observable-speculative!
  $kick-style
  kick-style
  ))

(let ()
  (include "0011-stm-imports.scm")
  (include "0020-stm-impl.scm")
  (include "0100-observable.scm")
  (include "0110-observable-kinds.scm")
  ;; NOTE: The module-end MUST be within the let form.
  (module-likely-end))

;;(include "1000-library.scm")
;; NO: (include "2000-tests.scm")
