;;** Debug Aid

(define (check-not-observable-speculative! caller . more)
  ;; Runtime assert (globally) that there is no current-transaction,
  ;; hence expecting no repeated invocation here.
  (unless (stm-atomic?) (stm-consistency-error "observable NOT atomic" caller more)))

(define (##opportunistic-sequential proc)
  (define (checked . args)
    (check-not-observable-speculative! proc args)
    (apply proc args))
  (tag-procedure checked 'sequence))

(define opportunistic-sequential
  ;; should be macro-defined away without debug, procedural fallback:
  (cond-expand
   (debug ##opportunistic-sequential)
   (else identity)))

;;** Utils

(define filter-eq #f) ;; use default

(define (filter-eqv old new)
  (if (eqv? old new) old new))

(define (filter-equal old new)
  (if (equal? old new) old new))

(define (pin-filter! pin pred)
  ;; like srfi-1 but: change arg order and cause side effect
  ;;
  ;; set `pin` - which must hold a list - removing all elements not
  ;; passing `pred`
  (kick!
   (lambda ()
     (let* ((init (pin))
            (change #f)
            (keep (lambda (element)
                    (if (pred element) #t (begin (set! change #t) #f))))
            (new (filter keep init)))
       (pin (if change new init))))))

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

(define kick-style
  (make-pin
   initial: ($kick-style)
   pred: (lambda (v) (member v '(async sync #f)))
   name: "$kick-style as PIN"))

(define (kick-style-pin-to-parameter) ($kick-style (kick-style)))
(wire! kick-style post: kick-style-pin-to-parameter)
