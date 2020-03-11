#;(define call-with-current-dynamic-extent
  (let ()
    (define (current-dynamic-extent)
      (call-with-current-continuation
       (lambda (return)
         (call-with-values
             (lambda ()
               (call-with-current-continuation
                (lambda (c)
                  (return
                   (lambda (thunk)
                     (call-with-current-continuation
                      (lambda (k)
                        (c k thunk))))))))
           (lambda (k thunk)
             (call-with-values thunk k))))))
    (lambda (d) (d (current-dynamic-extent)))))

(define (call-with-current-dynamic-extent thunk)
  (thunk
   (call-with-current-continuation
    (lambda (return)
      (call-with-values
          (lambda ()
            (call-with-current-continuation
             (lambda (c)
               (return
                (lambda (thunk)
                  (call-with-current-continuation
                   (lambda (k)
                     (c k thunk))))))))
        (lambda (k thunk)
          (call-with-values thunk k)))))))

(define (dynamic f)
  (call-with-current-dynamic-extent
   (lambda (dynamic-extent)
     (lambda args
       (dynamic-extent
        (lambda () (apply f args)))))))

#|
(define-macro (exh exn handler . body)
  `(with-exception-handler (dynamic (lambda (,exn) ,handler)) (lambda () . ,body)))

(exh x (cons 'caught x) ((current-exception-handler) 42))

(define x (dynamic (lambda () (error "Fehler 42"))))
|#

(define horrible-internal-error-debugger
  (dynamic
   (lambda (mux before after)
     (let ((msg "Someone broke our synchronisation\n"))
       (display msg (current-error-port))
       (error msg))
     (exit 1))))

#;(define make-protected-resource
  ;; returns a procedure with internal, mutex-protexted state, which
  ;; takes a procedure to be applied to the state which shall return
  ;; the new state.
  (lambda ()
    (let ((mux (make-mutex 'mux)))
      (lambda (access)
        (dynamic-wind
            (lambda () (mutex-lock! mux))
            (lambda ()
              (let* ((before (mutex-specific mux))
                     (after (access before)))
                ;; sanity check, we know this section is protected by the mux.
                ;; Sure the following exception will never be raised:
                (unless (eq? before (mutex-specific mux)) (horrible-internal-error-debugger mux before after))
                (mutex-specific-set! mux after)
                after))
            (lambda () (mutex-unlock! mux)))))))

(define make-protected-resource
  ;; returns a procedure with internal, mutex-protexted state, which
  ;; takes a procedure to be applied to the state which shall return
  ;; the new state.
  (lambda ()
    (let ((mux (make-mutex 'mux)))
      (case-lambda
       (()
        (dynamic-wind
            (lambda () (mutex-lock! mux))
            (lambda ()
              (let* ((before (mutex-specific mux))
                     (after (access before)))
                ;; sanity check, we know this section is protected by the mux.
                ;; Sure the following exception will never be raised:
                (unless (eq? before (mutex-specific mux)) (horrible-internal-error-debugger mux before after))
                (mutex-specific-set! mux after)
                after))
            (lambda () (mutex-unlock! mux))))
       ((access)
        (dynamic-wind
            (lambda () (mutex-lock! mux))
            (lambda ()
              (let* ((before (mutex-specific mux))
                     (after (access before)))
                ;; sanity check, we know this section is protected by the mux.
                ;; Sure the following exception will never be raised:
                (unless (eq? before (mutex-specific mux)) (horrible-internal-error-debugger mux before after))
                (mutex-specific-set! mux after)
                after))
            (lambda () (mutex-unlock! mux))))))))
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
