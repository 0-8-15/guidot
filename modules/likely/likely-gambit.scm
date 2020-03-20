(include "0001-dynamic-extent.scm")
(include "0010-debug.scm")

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
 (current-trigger-handler
  with-current-transaction
  make-observable
  observable-deps
  observable-name
  observable-pred
  observable-deref
  observable-reference
  observable-set!
  observable-alter! ;; like clojure API
  observable-invoke!
  observable-regref!
  observable-unref!
  $stm-retry-limit ;; parameter
  $implicit-current-transactions
  ))

(let ()
  (include "0011-stm-imports.scm")
  (include "0020-stm-impl.scm")
  (include "0100-observable.scm")
  ;; NOTE: The module-end MUST be within the let form.
  (module-likely-end))

;;(include "1000-library.scm")
;; NO: (include "2000-tests.scm")
