;;;* Notational Conventions (Observable Incubator)

(define make-pin
  (case-lambda
   (() (make-lval #f))
   ((initial) (make-lval initial))
   (more
    (define (make-lval* #!key (initial #!void) (pred #f) (filter #f) (name #f))
      (make-lval initial pred filter name))
    (apply make-lval* more))))

(define PIN make-pin)

(define-macro (define-pin name . more)
  `(define ,name (make-pin . ,more)))

(define (make-sensor . more)
  (receive
   (in out) (apply make-mval (if (null? more) (list #f) more))
   (case-lambda
    (() out)
    ((x) (in x))
    #;((k p . more) ;; UNDEFINED behavior: maybe this should be an error.
     (cond
      ((not (or k p)) (out #f #f)) ;; INTERNAL API `connect`
      (else (error "make-sensor: unexpected case"))))
    (else (error "UNDEFINED")))))

#;(define-macro (define-sense in out val . more)
  (let ((setter (gensym 'set))
        (v (gensym 'v)))
    `(begin
       (define ,in (make-mval ,val . ,more))
       (define ,out (call-with-values (lambda() ,in) (lambda (x y) y)))
       (let ((,setter (call-with-values (lambda() ,in) (lambda (x y) x))))
         (set! ,in (lambda (,v) (,setter ,v)))))))

(define-macro (define-sense name . more)
  (let ((in (string->symbol (string-append "." (symbol->string name)))))
    `(begin
       (define ,in ,(if (null? more) `(make-sensor #f) `(make-sensor . ,more)))
       (define ,name (,in)))))
;;
(define (make-sensor* . more)
  (define (make-mval* #!key (initial #!void) (pred #f) (filter #f) (name #f))
    (make-mval initial pred filter name))  (receive
   (in out) (apply make-mval* more)
   (case-lambda
    (() out)
    ((x) (in x))
    #;((k p . more) ;; UNDEFINED behavior: maybe this should be an error.
     (cond
      ((not (or k p)) (out #f #f)) ;; INTERNAL API `connect`
      (else (error "make-sensor*: unexpected case"))))
    (else (error "UNDEFINED")))))

(define SENSOR
  (case-lambda
   (() (make-sensor #f))
   ((x) (make-sensor x))
   (args (apply make-sensor* args))))

(define (wire-trivial-async-alias-connection! from to)
  (wire!
   from       ;; Any change in `from` (COULD be a list) will trigger:
   post:      ;; in next step (i.e., after commit)
   (lambda () ;; `to` to be set to the value of `from`
     (to (from)))))

(define-macro (define-sense* name val . more)
  (let ((in (string->symbol (string-append "." (symbol->string name)))))
    `(begin
       (define ,in (make-sensor* ,val . ,more))
       (define ,name (,in)))))
