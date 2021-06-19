(include "../misc-conventions/observable-syntax.sch")

(cond-expand
 (debug
  (define-macro (assume obj msg . more)
    `(if ,obj ,obj (apply error ,msg ,@more))))
 (else
  (define-macro (assume obj msg . more) obj)))

;;;** memoize inline (gambit) TBD: include from somewhere (copied from
;;;guide.scm here)

(define-macro (macro-absent)
  ;; gambit specific
  '(##type-cast -6 (##type #f)))

(define-macro (macro-absent? x)
  ;; gambit specific
  `(eq? ,x (macro-absent)))

(define-macro (macro-alway-true-comparsion)
  ;; usually it's IMPORTANT to pass this as expression rather than a
  ;; free variable
  '(lambda (a b) #t))

(define-macro (macro-memoize:1->1 f cmp)
  (let ((last (gensym 'last))
        (current (gensym 'current))
        (value (gensym 'value)))
    `(let ((,last (macro-absent))
           (,value (macro-absent)))
       (lambda (,current)
         (cond
          ((and
            (,cmp ,current ,last)
            (not (macro-absent? ,value)))
           ,value)
          (else
           (set! ,value (,f ,current))
           (set! ,last ,current)
           ,value))))))

(define-macro (macro-memoize:2->1 f cmp1 cmp2)
  (let ((last-1 (gensym 'last-1))
        (current-1 (gensym 'current-1))
        (last-2 (gensym 'last-2))
        (current-2 (gensym 'current-2))
        (value (gensym 'value)))
    `(let ((,last-1 (macro-absent))
           (,last-2 (macro-absent))
           (,value (macro-absent)))
       (lambda (,current-1 ,current-2)
         (cond
          ((and
            (,cmp2 ,current-2 ,last-2)
            (,cmp1 ,current-1 ,last-1)
            (not (macro-absent? ,value)))
           ,value)
          (else
           (set! ,value (,f ,current-1  ,current-2))
           (set! ,last-1 ,current-1)
           (set! ,last-2 ,current-2)
           ,value))))))

(define-macro (macro-memoize:3->1 f cmp1 cmp2 cmp3)
  (let ((value (gensym 'value))
        (last-1 (gensym 'last-1))
        (current-1 (gensym 'current-1))
        (last-2 (gensym 'last-2))
        (current-2 (gensym 'current-2))
        (last-3 (gensym 'last-3))
        (current-3 (gensym 'current-3)))
    `(let ((,last-1 (macro-absent))
           (,last-2 (macro-absent))
           (,last-3 (macro-absent))
           (,value (macro-absent)))
       (lambda (,current-1 ,current-2 ,current-3)
         (cond
          ((and
            (,cmp3 ,current-3 ,last-3)
            (,cmp2 ,current-2 ,last-2)
            (,cmp1 ,current-1 ,last-1)
            (not (macro-absent? ,value)))
           ,value)
          (else
           (set! ,value (,f ,current-1 ,current-2 ,current-3))
           (set! ,last-1 ,current-1)
           (set! ,last-2 ,current-2)
           (set! ,last-3 ,current-3)
           ,value))))))

(define-macro (macro-memoize:4->1 f cmp1 cmp2 cmp3 cmp4)
  (let ((value (gensym 'value))
        (last-1 (gensym 'last-1))
        (current-1 (gensym 'current-1))
        (last-2 (gensym 'last-2))
        (current-2 (gensym 'current-2))
        (last-3 (gensym 'last-3))
        (current-3 (gensym 'current-3))
        (last-4 (gensym 'last-4))
        (current-4 (gensym 'current-4)))
    `(let ((,last-1 (macro-absent))
           (,last-2 (macro-absent))
           (,last-3 (macro-absent))
           (,last-4 (macro-absent))
           (,value (macro-absent)))
       (lambda (,current-1 ,current-2 ,current-3 ,current-4)
         (cond
          ((and
            (,cmp4 ,current-4 ,last-4)
            (,cmp3 ,current-3 ,last-3)
            (,cmp2 ,current-2 ,last-2)
            (,cmp1 ,current-1 ,last-1)
            (not (macro-absent? ,value)))
           ,value)
          (else
           (set! ,value (,f ,current-1 ,current-2 ,current-3 ,current-4))
           (set! ,last-1 ,current-1)
           (set! ,last-2 ,current-2)
           (set! ,last-3 ,current-3)
           (set! ,last-4 ,current-4)
           ,value))))))

;; FIXME: These should not be here!

(define-macro (%%guide-post-speculative expr)
  ;; either a thunk or a promise -- promise seems NOT to work under
  ;; gamit?
  `(lambda () ,expr))

(define-macro (macro-guide-execute-payload-result expr)
  ;; TBD: sanitize in debug mode only and then maybe use it always.
  (let ((results (gensym 'results))
        (obj (gensym 'obj)))
    `(receive ,results ,expr
       (let ((,obj (car ,results)))
         (cond
          ((procedure? ,obj) (,obj))
          ((promise? ,obj) (force ,obj)))))))

(define-macro (%%guide-post-speculative/async expr)
  ;; either a thunk or a promise -- promise seems NOT to work under
  ;; gamit?
  ;;
  ;; does not block, returns asap.
  `(%%guide-post-speculative
    (begin
      (thread-start! (make-thread (lambda () (macro-guide-execute-payload-result ,expr))))
      ;;(kick! (box (lambda () (macro-guide-execute-payload-result ,expr))))
      #t)))

(define-macro (macro-guide-sanitize-payload-result expr)
  ;; TBD: sanitize in debug mode only and then maybe use it always.
  (let ((results (gensym 'results))
        (obj (gensym 'obj)))
    `(receive ,results ,expr
       (cond
        ((null? ,results) #t)
        (else
         (let ((,obj (car ,results)))
           (cond
            ((procedure? ,obj) ,obj)
            ((promise? ,obj) ,obj)
            (else #t))))))))

(define-macro (macro-guidot-check-ggb/string-pred pred)
  ;; just don't make this global...
  (let ((ggb (gensym 'ggb))
        (str (gensym 'str))
        (i (gensym))
        (v (gensym)))
    `(lambda (,ggb)
       (let ((,str (make-string (ggb-length ,ggb))))
         (ggb-for-each ,ggb (lambda (,i ,v) (string-set! ,str ,i (integer->char ,v))))
         (,pred ,str)))))

(define-macro (%%macro-guidot-capture-guide-toplevel)
  '(guide-toplevel-capture-return))
