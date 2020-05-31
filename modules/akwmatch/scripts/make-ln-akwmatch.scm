(define (pp-macros/rt fn)
  ;; pretty-print macros for gambit to be available at runtime
  (for-each
   (lambda (defmacro)
     (receive
      (kw expr nexpr) (apply values defmacro)
      #;(begin (pp `(define-macro ,expr ,nexpr)) (newline))
      (pp `(eval '(define-macro ,expr ,nexpr)) (current-output-port))))
   (call-with-input-file fn read-all)))

(define (pp-define-values/rt)
  (define src
    '(define-macro (define-values names . body)
       (let ((vals (gensym 'vals)))
         `(begin
            ,@(map (lambda (name) `(define ,name #f)) names)
            (call-with-values (lambda () . ,body)
              (lambda ,vals
                . ,(map (lambda (name)
                          `(set! ,name (let ((,name (car ,vals))) (set! ,vals (cdr ,vals)) ,name)))
                        names)))))))
  (pp `(eval ',src) (current-output-port)))

(define (make-ln-akwmatch)
  (let ((prcdrs (call-with-input-file "source/match-match-expanders.scm"
                  (lambda (port) (read-line port #f))))
        (sntx (with-output-to-string
               (lambda ()
                 (pp-macros/rt "source/match-match-syntax.scm"))))
        (top-header ";;; Generated File - DO NOT EDIT!\n\n")
        (header #<<END-of-header
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
 match
 (match:version
  ;;
  match:set-structure-control
  match:set-error
  match:set-error-control
  ;;
  match:andmap
  match:syntax-err
  match:expanders
  ;;
  ))

(let () ;;; NOTE: This is NOT lexically matched!
;;;         The `let` is closed by the other `)` right after `(module-match-end)`.
;;;
;; end of the header
END-of-header
             ))
;;;; END OF HEADER
    (with-output-to-file
        "akwmatch.scm"
      (lambda ()
        (display top-header)
        ;; (pp-define-values/rt)
        (display header)
        (display prcdrs)
        (display "  (module-match-end)) ;; Closing the `let` at end of the header.\n")
        (display sntx)
        (pp-define-values/rt)
        (display "#eof\n")))))

(case 2
  ((2) (make-ln-akwmatch)))
