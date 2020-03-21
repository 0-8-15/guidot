(define (debug l v)
  (let ((p  (current-error-port)))
    (display
     (call-with-output-string
      (lambda (p)
        (display (current-thread) p)
        (display " " p)
        (display l p)
        (display ": " p)
        (write v p)
        (newline p)))
     p)
    ((cond-expand (gambit force-output) (chicken flush-output)) p)
    v))
