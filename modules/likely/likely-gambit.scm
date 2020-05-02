(include "0001-dynamic-extent.scm")
(include "0002-srfi-1.scm")
(include "0003-tagged-proc-gambit.scm")
(cond-expand
 ((or test debug)
  (include "0010-debug.scm"))
 (else))

(define-macro (define-values names . body)
  (let ((vals (gensym 'vals)))
    `(begin
       ,@(map (lambda (name) `(define ,name #f)) names)
       (call-with-values (lambda () . ,body)
         (lambda ,vals
           . ,(map (lambda (name)
                     `(set! ,name (let ((,name (car ,vals))) (set! ,vals (cdr ,vals)) ,name)))
                   names))))))

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
  (connect-dependent-value! exported-connect-dependent-value!) ;; better not export these?
  observable-regref!
  observable-unref!
  $stm-retry-limit ;; parameter
  $stm-conflict-peanalty
  $implicit-current-transactions
  (stm-current-global-clock exported-stm-current-global-clock)
  $debug-trace-triggers
  ;; Debug & Deprecated
  ;;
  stm-atomic?
  $kick-style
  ))

(let ()
  (include "0011-stm-imports.scm")
  (include "0020-stm-impl.scm")
  (include "0100-observable.scm")
  ;; NOTE: The module-end MUST be within the let form.
  (module-likely-end))

;;(include "1000-library.scm")
;; NO: (include "2000-tests.scm")
