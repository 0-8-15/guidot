(define (maybe->string s)
  (cond ((string? s) s)
        ((or (symbol? s) (number? s) (char? s)) (object->string s))
        (else (error "Expected a string, symbol, character or number"))))

(define (port->string p)
  (and
   (input-port? p)
   (dynamic-wind
       (lambda () #t)
       (lambda ()
         (call-with-output-string
          (lambda (o)
            (let loop ()
              (let ((ln (read-line p)))
                (unless (eof-object? ln)
                        (display ln o)
                        (newline o)
                        (loop)))))))
       (lambda () (close-input-port p)))))

(define (run->status cmd . args)
  (process-status (open-process `(path: ,(maybe->string cmd) arguments: ,(map maybe->string args)))))

(define (run/boolean . args) (zero? (apply run->status args)))

(define (run->string cmd . args)
  (port->string (open-input-process `(path: ,(maybe->string cmd) arguments: ,(map maybe->string args)))))

(define (run->strings cmd . args)
  ;; patterned after scsh (i.e., scsh-process), but not a macro here.
  (let ((p #f))
    (dynamic-wind
        (lambda ()
          (set! p (open-input-process
                   `(path: ,(maybe->string cmd) arguments: ,(map maybe->string args)))))
        (lambda () (read-all p read-line))
        (lambda () (close-input-port p)))))

(define (which cmd)
  (let ((all (run->strings "which" cmd)))
    (and (pair? all) (car all))))

#;(define (open-input-sh-process cmd)
  (open-input-process `(path: "sh" arguments: ("-c" ,cmd))))

(define (system->string cmd) (run->string "sh" "-c" cmd))
(define (system->strings cmd) (run->strings "sh" "-c" cmd))
