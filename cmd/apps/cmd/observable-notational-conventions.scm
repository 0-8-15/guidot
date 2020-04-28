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

(define-macro (define-macro/rt expr nexpr)
  `(begin
     (define-macro ,expr ,nexpr)
     (eval '(define-macro ,expr ,nexpr))))

(define-macro/rt (define-pin name . more)
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

(define-macro/rt (define-sense name . more)
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

(define (wire-trivial-async-alias-connection! from to)
  (wire!
   from       ;; Any change in `from` (COULD be a list) will trigger:
   post:      ;; in next step (i.e., after commit)
   (lambda () ;; `to` to be set to the value of `from`
     (to (from)))))

(define SENSOR
  (case-lambda
   (() (make-sensor #f))
   ((x) (make-sensor x))
   (args (apply make-sensor* args))))

(define-macro/rt (define-sense* name val . more)
  (let ((in (string->symbol (string-append "." (symbol->string name)))))
    `(begin
       (define ,in (SENSOR ,val . ,more))
       (define ,name (,in)))))

(define-macro/rt (define-SENSOR name form)
  (if (not (eq? (car form) 'SENSOR))
      (error "define-SENSOR: missuse")
      (let* ((more (cdr form))
             (in (string->symbol (string-append "." (symbol->string name)))))
        `(begin
           (define ,in (SENSOR . ,more))
           (define ,name (,in))))))

;;; Not only gambit specific, but runtime-only as well.

(define (%DEF+leading-dot name generator more)
  (let ((name_1 (string->symbol (string-append "." (symbol->string name))))
        (ref1 (##make-global-var name_1)))
    (##global-var-set! ref1 (apply generator more))
    (##global-var-set! (##make-global-var name) ((##global-var-ref name_1)))))

(define (SENSOR! name . more)
  (%DEF+leading-dot name SENSOR more))
