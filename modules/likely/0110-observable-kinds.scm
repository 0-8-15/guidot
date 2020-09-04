(define pin?)

(define make-pin
  (let ((tag (string #\P #\I #\N)))
    (define (is-pin? x) (procedure-tagged? x tag))
    ;; Syntax
    (define make-pin
      (case-lambda
       (() (make-lval #f))
       ((initial) (make-lval initial))
       (more
        (define (make-lval* #!key (initial #!void) (pred #f) (filter #f) (name #f))
          (make-lval initial pred filter name))
        (apply make-lval* more))))
    (set! pin? is-pin?)
    (lambda args (tag-procedure (apply make-pin args) tag))
    #;make-pin))

(define PIN make-pin)
