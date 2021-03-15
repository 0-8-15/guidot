(include "../misc-conventions/observable-syntax.sch")

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
