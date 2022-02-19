;;; Protect Callbacks into threaded gambit

;;;* History

;; Original attempt was to suppress thead switches during C-to-Scheme
;; call sections.  This turned out to be tricky.  Since the Gambit
;; manual suggests to work the other way around: allow Scheme-to-C
;; calls which may call back from C to Scheme only in a single thread.

;;;* Second Approach

(define current-safe-thunk #f)

(define ##safe-thunk-exec!
  (let ((thread #f))
    (define (loop2)
      (let ((msg (thread-receive)))
        (cond
         ((procedure? msg)
          (set! current-safe-thunk msg)
          (msg)
          (set! current-safe-thunk #f))))
      (loop2))
    (define (loop1)
      (with-exception-catcher
       (lambda (exn)
         (stm-log 'safe-lambda-exec! current-safe-thunk exn)
         (set! current-safe-thunk #f))
       loop2)
      (loop1))
    #;(define (loop)
      (let ((msg (thread-receive)))
        (cond
         ((procedure? msg)
          (set! current-safe-thunk msg)
          (with-exception-catcher
           (lambda (exn) (stm-log 'safe-lambda-exec! msg exn))
           msg)
          (set! current-safe-thunk #f))))
      (loop))
    (set! thread (make-thread loop1 'safe-thunk-exec!))
    (thread-start! thread)
    (lambda (thunk) (thread-send thread thunk))))

(define (##safe-thunk-call thunk)
  (let ((mutex (make-mutex thunk)))
    (mutex-lock! mutex #f #f)
    (##safe-thunk-exec!
     (lambda ()
       (mutex-specific-set! mutex (thunk))
       (mutex-unlock! mutex)))
    (mutex-lock! mutex #f #f)
    (mutex-unlock! mutex) ;; may avoid space leak (?)
    (mutex-specific mutex)))

;;;* Old Approach

;;;** Internal

(define ##safe-lambda-mutex (make-mutex 'safe-lambda))

(c-declare "static ___mask_heartbeat_interrupts_state heartbeat_interrupts;")

(define (##raise-safe-lambda-exception location reason)
  (error reason location))

(define-macro (trace-lock* phase ref posted)
  `(display
    (with-output-to-string
      (lambda ()
        (display ,phase)
        (display #\space)
        (display (current-thread))
        (when (eq? ,phase 'O)
              (display " current: ")
              (display (mutex-state ##safe-lambda-mutex)))
        (when (or (eq? ,phase 'V) (eq? ,phase 'POST))
              (display " posted: ")
              (display ,posted))
        (display " ")
        (display (if (string? ,ref) (substring ,ref 0 (min (string-length ,ref) 40)) ,ref))
        (newline)))
    (current-error-port)))

(define ##safe-lambda-trace* #f)

(define ##safe-lambda-trace
  (case-lambda
   (() ##safe-lambda-trace*)
   ((x) (set! ##safe-lambda-trace* (and x #t)))))

(define-macro (trace-lock phase ref posted)
  `(if ##safe-lambda-trace* (trace-lock* ,phase ,ref ,posted)))

;(define-macro (trace-lock phase ref posted) #!void)

(define ##safe-lambda-busy #f) ;; more tracing - TBD: remove once done!

(define (##safe-lambda-lock! location)
  ;; Jump through loops to err out on dead locks,
  ;; report, trace etc.
  (trace-lock 'O location #f)
  (if (eq? (mutex-state ##safe-lambda-mutex) (current-thread))
      (##raise-safe-lambda-exception location (debug location "deadlock")))
  (mutex-lock! ##safe-lambda-mutex)
  (set! ##safe-lambda-busy (cons (current-thread) location))
  ((c-lambda () void "___mask_heartbeat_interrupts_begin(&heartbeat_interrupts);"))
  (trace-lock 'P location #f)
  ;; report trace
  )

(define ##safe-lambda-post!)

(define (##in-safe-callback?)
  (eq? (mutex-state ##safe-lambda-mutex) (current-thread)))

(define-macro (delay-until-after-return expr) `(lambda () ,expr))

(define-macro (delayed-until-after-return? expr) `(procedure? ,expr))

(define ##safe-lambda-unlock!
  (let ((post (list #f)) ;; dummy head
        (last '()))
    (define (run-safe-lambda-posted job)
      (cond
       ((delayed-until-after-return? job) (job))
       (else (debug 'run-safe-lambda-posted:unhandled job))))
    (set! last post) ;; initially
    (set! ##safe-lambda-post!
          (lambda (job)
            (trace-lock 'POST 'unknown job)
            (if (eq? (mutex-state ##safe-lambda-mutex) (current-thread))
                (let ((next (list job)))
                  (set-cdr! last next)
                  (set! last next))
                (##raise-safe-lambda-exception (debug "not locked" 'safe-lambda-post!) "not locked"))))
    (lambda (location)
      ;; report (not yet)
      (let ((tbd (cdr post)))
        (set-cdr! post '())
        (set! last post)
        ;; report trace
        (trace-lock 'V location tbd)
        (set! ##safe-lambda-busy #f)
        ((c-lambda () void "___mask_heartbeat_interrupts_end(&heartbeat_interrupts);"))
        (mutex-unlock! ##safe-lambda-mutex)
        ;; enforce delayed operations now
        (for-each run-safe-lambda-posted tbd)))))

;;;** Initialization

;;; BEWARE: Slippery slope: here MIGHT be an issue in gambit
;;; wrt. threads started from callbacks.

(define ##foreign-safe-thread-start!
  ;; Did suddenly not work as good anymore.
  (let ((thread-start! thread-start!))
    (lambda (thread)
      (let ((mux-state (mutex-state ##safe-lambda-mutex)))
        (if (thread? mux-state)
            (if (eq? mux-state (current-thread))
                (##safe-lambda-post! (delay-until-after-return (thread-start! thread)))
                (##raise-safe-lambda-exception '##foreign-safe-thread-start! (debug 'thread-start! "not locked")))
            (thread-start! thread))))))

#;(define ##foreign-safe-thread-start!
  (let ((thread-start! thread-start!))
    (define starter
      (thread-start!
       (make-thread
        (lambda () (do () (#f) (thread-start! (thread-receive))))
        'thread-starter)))
    (lambda (thread) (thread-send starter thread))))

;; This overwrite appears to fix the issue.
(define (##overwrite-thread-start!-for-safe-lambda!)
  (set! thread-start! ##foreign-safe-thread-start!))

;; FIXME: kill issue via overwrite (set! thread-start! ##foreign-safe-thread-start!)

;;;** Syntax Exports

;; API compatible to `c-lambda` but may safely call back to Scheme

(define-macro (c-safe-lambda formals return c-code)
  (let ((tmp (gensym 'c-safe-lambda-result))
        (argument-names
         (map
          (lambda (n) (string->symbol (string-append "arg" (number->string n))))
          (iota (length formals)))))
    `(lambda ,argument-names
       (##safe-lambda-lock! ,c-code)
       (let ((,tmp ((c-lambda ,formals ,return ,c-code) . ,argument-names)))
         (##safe-lambda-unlock! ,c-code)
         ,tmp))))

;; (define-foreign-safe ...) - likely nicer location trace info

(define-macro (define-foreign-safe formals return c-code)
  (let ((tmp (gensym 'c-safe-lambda-result))
        (argument-names
         (map
          (lambda (n) (string->symbol (string-append "arg" (number->string n))))
          (iota (length formals)))))
    `(define (,(car formals) . ,argument-names)
       (##safe-lambda-lock! ,(car formals))
       (let ((,tmp ((c-lambda ,formals ,return ,c-code) . ,argument-names)))
         (##safe-lambda-unlock! ,(car formals))
         ,tmp))))

(set! ##safe-lambda-post! ##safe-thunk-exec!)
