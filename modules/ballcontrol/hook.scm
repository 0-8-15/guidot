;; from srfi-173

    (define-record-type <hook>
      (%make-hook procs arity)
      hook?
      (procs hook-procs hook-procs!)
      (arity hook-arity hook-arity!))

    (define (make-hook arity)
      (%make-hook '() arity))

    (define (hook-add! hook proc)
      (let ((procs (hook-procs hook)))
        (hook-procs! hook (append procs (list proc)))))

    (define (hook-delete! hook proc)
      (let loop ((procs (hook-procs hook))
                 (out '()))
        (unless (null? procs)
          (if (eq? proc (car procs))
              (hook-procs! hook (append (cdr procs) out))
              (loop (cdr procs) (cons (car procs) out))))))

    (define (hook-reset! hook)
      (hook-procs! hook '()))

    (define (hook->list hook)
      (hook-procs hook))

    (define (hook-run hook . args)
      #;(unless (= (length args) (hook-arity hook))
              (error "hook-run args have wrong length" (length args) (hook-arity hook)))
      (for-each (lambda (proc) (apply proc args)) (hook-procs hook)))
