;;; (include "~~tgt/lib/onetierzero/src/observable-notational-conventions.scm")
(define-macro (define-values names . body)
  (let ((arg1 (gensym 'arg1))
        (rest (gensym 'rest)))
    `(begin
       ,@(map (lambda (name) `(define ,name #f)) (cdr names))
       (define ,(car names)
         (call-with-values
             (lambda () unquote body)
           (lambda (,arg1 . ,rest)
             (begin
               ,@(map (lambda (name)
                        `(set! ,name
                               (let ((,name (car ,rest)))
                                 (set! ,rest (cdr ,rest))
                                 ,name)))
                      (cdr names))
               ,arg1)))))))

(define-macro (kick expr . more)
  `(kick! (lambda () ,expr . ,more)))

(define-macro (kick/sync expr . more)
  `(kick/sync! (lambda () ,expr . ,more)))

(define-macro (define-pin name . more)
  `(define ,name (make-pin . ,more)))
