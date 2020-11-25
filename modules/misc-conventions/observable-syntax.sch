;;; (include "~~tgt/lib/onetierzero/src/observable-notational-conventions.scm")
(define-macro (define-values names . body)
  (let ((vals (gensym 'vals)))
    `(begin
       ,@(map (lambda (name) `(define ,name #f)) names)
       (call-with-values (lambda () . ,body)
         (lambda ,vals
           . ,(map (lambda (name)
                     `(set! ,name (let ((,name (car ,vals))) (set! ,vals (cdr ,vals)) ,name)))
                   names))))))

(define-macro (kick expr . more)
  `(kick! (lambda () ,expr . ,more)))

(define-macro (kick/sync expr . more)
  `(kick/sync! (lambda () ,expr . ,more)))

(define-macro (define-pin name . more)
  `(define ,name (make-pin . ,more)))
