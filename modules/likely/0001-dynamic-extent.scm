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
    (define (dynamic f #!optional (single-value-return #f))
      (let ((fixed (current-dynamic-extent (not single-value-return))))
        (lambda args (fixed (lambda () (apply f args))))))
    dynamic))
