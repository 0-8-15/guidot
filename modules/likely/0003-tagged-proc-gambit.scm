;;; From: Marc Feeley <feeley@iro.umontreal.ca>
;;;
;;; Date: Fri, 27 Mar 2020 08:28:53 -0400
;;;
;;; Another solution that is a bit hackish is to use a wrapper closure
;;; that stores the information in the closure’s free variables.  The
;;; implementation is a bit more complicated because closures are
;;; represented differently by the compiler and interpreter:

#|
(define-type procinfo comment)

(define (attach comment proc)
    (let ((@procinfo (make-procinfo comment)))
      (lambda args
        (##first-argument @procinfo) ;; keep @procinfo in the free vars
        (apply proc args))))

(define (get-comment proc default)

  (define (extract x)
    (if (procinfo? x) (procinfo-comment x) default))

  (cond ((not (##closure? proc))
         default)
        ((##interp-procedure? proc)
         (let ((rte (##interp-procedure-rte proc)))
           (extract (and (vector? rte)
                         (= 2 (vector-length rte))
                         (vector-ref rte 1)))))
        (else
         (extract (##closure-ref proc 1)))))
;;;

(define dec (attach "decrement function" (lambda (x) (- x 1))))

(pp (get-comment dec "unknown")) ;; prints: "decrement function"
(pp (get-comment inc "unknown")) ;; prints: "unknown"
(pp (get-comment car "unknown")) ;; prints: "unknown"
|#

(define-type procinfo comment)

(define (tag-procedure* proc comment)
    (let ((@procinfo (make-procinfo comment)))
      (lambda args
        (##first-argument @procinfo) ;; keep @procinfo in the free vars
        (apply proc args))))

(define (tag-thunk* thunk comment)
    (let ((@procinfo (make-procinfo comment)))
      (lambda ()
        (##first-argument @procinfo) ;; keep @procinfo in the free vars
        (thunk))))

(define (extract-procedure-tags proc)
  (define (extract x)
    (if (procinfo? x) x #f))
  (cond ((not (##closure? proc))
         #f)
        ((##interp-procedure? proc)
         (let ((rte (##interp-procedure-rte proc)))
           (extract (and (vector? rte)
                         (= 2 (vector-length rte))
                         (vector-ref rte 1)))))
        (else
         (extract (##closure-ref proc 1)))))

(define (tag-procedure proc comment)
  (let ((procinfo (extract-procedure-tags proc)))
    (if procinfo
        (begin
          (procinfo-comment-set! procinfo (cons comment (procinfo-comment procinfo)))
          proc)
        (let ((@procinfo (make-procinfo (list comment))))
          (lambda args
            (##first-argument @procinfo) ;; keep @procinfo in the free vars
            (apply proc args))))))

(define (tag-thunk thunk comment)
  (let ((procinfo (extract-procedure-tags obj)))
    (if procinfo
        (begin
          (procinfo-comment-set! procinfo (cons comment (procinfo-comment procinfo)))
          proc)
        (let ((@procinfo (make-procinfo (list comment))))
          (lambda ()
            (##first-argument @procinfo) ;; keep @procinfo in the free vars
            (thunk))))))

(define (procedure-tagged? proc tag)
  (let ((tags (extract-procedure-tags proc))) (and tags (memq tag (procinfo-comment tags)) #t)))

(define dynamic
  (let ((current-dynamic-extent current-dynamic-extent)
        (call-with-values call-with-values)
        (@procinfo (make-procinfo (list 'dynamic))))
    (define (dynamic f #!optional (dynamic-extent #f) (single-value-return #f))
      (let ((fixed (if dynamic-extent dynamic-extent (current-dynamic-extent (not single-value-return)))))
        (lambda args
          (##first-argument @procinfo)
          (fixed (lambda () (apply f args))))))
    dynamic))

(define (procedure-tagged-dynamic? proc) (procedure-tagged? proc 'dynamic))

(define dynamic? procedure-tagged-dynamic?)
