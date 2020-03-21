;;; (C) 2020 JFW

;; Note: the default value for `multiple-value-return` being false was
;; choosen for compatibility with scheme versions which do not support
;; multiple value.  Implementations, which do not support multiple
;; value returns MUST report an error when called with a #t SHOULD
;; better only provide a base version which does not accept the
;; optional argument.
(define (current-dynamic-extent #!optional (multiple-value-return #f))
  (let ((yield (lambda (d) (lambda () d))))
    (if multiple-value-return ;; This might be faster, would it be?
        ((call-with-current-continuation
          (lambda (unit)
            (yield
             (lambda (thunk)
               (call-with-current-continuation
                (lambda (k)
                  (unit (lambda () (k (thunk)))))))))))
        ((call-with-current-continuation
          (lambda (unit)
            (yield
             (lambda (thunk)
               (call-with-current-continuation
                (lambda (k)
                  (unit (lambda () (call-with-values thunk k)))))))))))))

(define dynamic
  (let ((current-dynamic-extent current-dynamic-extent)
        (call-with-values call-with-values))
    (define (dynamic f #!optional (dynamic-extent #f) (single-value-return #f))
      (let ((fixed (if dynamic-extent dynamic-extent (current-dynamic-extent (not single-value-return)))))
        (lambda args (fixed (lambda () (apply f args))))))
    dynamic))

#|

(define (current-dynamic-extent #!optional (call-with-values call-with-values))
  (let ((yield (lambda (d) (lambda () d))))
    ((call-with-current-continuation
      (lambda (unit)
        (yield
         (lambda (thunk)
           (call-with-current-continuation
            (lambda (k)
              (unit (lambda () (call-with-values thunk k))))))))))))

(define (call-with-current-dynamic-extent proc #!optional (call-with-values call-with-values))
  (proc (current-dynamic-extent call-with-values)))

(define (fix f)
  (call-with-current-dynamic-extent
   (lambda (fixed) (lambda args (fixed (lambda () (apply f args)))))))

(define-macro (handle-exceptions exn handler . body)
  (let ((oh (gensym 'oh)))
    `(let ((,oh (current-exception-handler)))
       (with-exception-handler
        (lambda (,exn) (with-exception-handler ,oh (lambda () ,handler)))
        (lambda () . ,body)))))

(define-macro (exh exn handler . body)
  `(with-exception-handler (fix (lambda (,exn) ,handler)) (lambda () . ,body)))

(exh x (cons 'caught x) ((current-exception-handler) 42))

(define x (fix (lambda () (error "Fehler 42"))))
|#

(define make-protected-resource
  ;; returns a procedure with internal, mutex-protected state
  (lambda (initial)
    (let ((mutex (make-mutex 'mutex)))
      (mutex-specific-set! mutex initial)
      (lambda (#!optional get mutate)
        (mutex-lock! mutex)
        (call-with-values
            (lambda ()
              (if mutate
                  (let* ((before (mutex-specific mutex))
                         (after (mutate before)))
                    (mutex-specific-set! mutex after)
                    (if get (get before after) after))
                  (get (mutex-specific mutex))))
          (lambda results
            (mutex-unlock! mutex)
            (apply values results)))))))

#|
(define (store-set! store value) (store (lambda (old) value)))
(define (store-get store) (let ((now #f)) (store (lambda (old) (set! now old) old)) now))
(define (print-store store)
  (store (lambda (v) (display "Current value: ") (display v) (display "\n") v)))

;; make/set! are intensionally a two step process: first allocate the
;; symbol, then bind the value
(define store (make-protected-resource))
(store-set! store 0)

(define slow-up (lambda (old) (thread-sleep! 3) (display (current-thread) (current-error-port)) (+ old 1)))

(define slow-up (lambda (old)  (display (current-thread) (current-error-port)) (display "waits\n" (current-error-port)) (thread-sleep! 5) (display (current-thread) (current-error-port)) (display "continues\n" (current-error-port)) (+ old 1)))

(store slow-up)

(define (trace-report msg)
  (display (current-thread) (current-error-port)) (display msg (current-error-port)))

(define fast-up
  (dynamic
   (lambda ()
     (store (lambda (old) (trace-report "runs through\n") (* old 23))))))

(define (bg-up!)
  (thread-start! (make-thread (lambda () (store slow-up)) 'slow-up)))

(define (no-overtake)
  (bg-up!)
  (thread-sleep! 1)
  (fast-up))


|#
