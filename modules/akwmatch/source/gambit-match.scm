(declare
 (standard-bindings)
 (extended-bindings) ;; no overwrites of standard bindings
 (not standard-bindings thread-start!) ;; except this
 (block)
 )

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

(define-macro (define-rt-macros fn)
  (let ()
    (define (macro/rt defmacro)
      (receive
       (kw expr nexpr) (apply values defmacro)
       `(begin
          (define-macro ,expr ,nexpr)
          (eval '(define-macro ,expr ,nexpr)))))
    (cons 'begin (map macro/rt (call-with-input-file fn read-all)))))



(this-module
 match-for-gambit
 (match:version
  ;;
  match:set-structure-control
  match:set-error
  match:set-error-control
  ;;
  match:error
  match:andmap
  match:syntax-err
  match:expanders
  genmatch genletrec gendefine pattern-var?
  ;;
  ))

(let ()
  (include "match-match-expanders.scm")
  ;; NOTE: The module-end MUST be within the let form.
  (module-match-for-gambit-end))
(define-rt-macros "source/match-match-syntax.scm")
