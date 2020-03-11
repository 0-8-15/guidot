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
