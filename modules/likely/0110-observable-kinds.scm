;;** Debug Aid

(define (check-not-observable-speculative! caller . more)
  ;; Runtime assert (globally) that there is no current-transaction,
  ;; hence expecting no repoeated invocation here.
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

(define (wire-persistent-file!
         pins ;; set of pins
         filename
         #!key
         ;; encoding
         (encode
          (lambda pin-list
            (object->u8vector (map (lambda (x) (x)) pin-list))))
         (decode
          (lambda (pin-list obj)
            (for-each (lambda (pin val) (pin val)) pin-list (u8vector->object obj))))
         ;; default assumes encoding as u8vector
         (save
          (lambda old-pin-values
            ;; default: fail on encode before damaging the file
            (let ((content (apply encode (if (pair? pins) pins (list pins)))))
              (unless (u8vector? content)
                (error "defaut `save` expects u8vector"
                       wire-persistent-file! filename pins content))
              (call-with-output-file filename
                (lambda (port)
                  (write-subu8vector content 0 (u8vector-length content) port))))))
         (load
          ;; NOTE: defined as varying for argument checks, see below
          (lambda pin-list
            (let ((obj (read-file-as-u8vector filename)))
              (when obj (decode pin-list obj)))))
         (switchable #f))
  ;;; TBD: maybe check filename and file access early here?
  ;;; Nevertheless the `load` parameter (procedure) however should take
  ;;; care anyway.
  ;;
  ;; NOTE: a trick: using `apply` here to enforce an argument count
  ;; check on the `load` procedure.
  (kick/sync! ;; a bit strict; just kick! ???
   (lambda () (if (pair? pins) (apply load pins) (load pins))))
  (wire!
   pins
   critical: save
   switchable: switchable))

;; ** Initialization

(wire! kick-style post: kick-style-pin-to-parameter)

;; eof
