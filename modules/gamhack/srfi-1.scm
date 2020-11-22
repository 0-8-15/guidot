;;; (C) 2020 JFW --- License: BSD

;;; SRFI-1 "cherries" used within lambdanative; if optimized then
;;; optimized for Gambit.

;; Make a list of length LEN.

#| ;; correct but might break lambdanative
(define (make-list n #!optional (elt #f))
  (do ((n (if (fixnum? n) n (inexact->exact (floor n)))
          (fx- n 1))
       (ans '() (cons elt ans)))
      ((fx<= n 0) ans)))
;;; |#

(define (##take x i) ;; from srfi-1
  ;; requirements:
  ;;
  ;; - x may be any value
  ;;
  ;; - (equal?    (append (take x n) (drop x n))   a)
  (if (and (pair? x) (fx> i 0))
      (let ((accumulator (list #f)))
        (do ((last accumulator (cdr last))
             (x x (cdr x))
             (i i (fx- i 1)))
            ((or (fx= i 0) (not (pair? x)))
             (cdr accumulator))
          (set-cdr! last (list (car x)))))
      x))

(set! take ##take)

(define (##drop x i)
  (do ((x x (cdr x))
       (i i (fx- i 1)))
      ((or (fx= i 0) (not (pair? x)))
       x)))

(set! drop ##drop)

#| ;;; Comment out this block comment (add a ';' before the '#') here
    ;;; include test in debug mode
    ;;;
    ;;; NOTE: this requires requires module tests, which at this time
    ;;; is not yet published.

(cond-expand
 (debug
  (define-macro (%%include-tests . body) `(begin ,@body)))
 ;; (else (define-macro (%%include-tests . body) #!void))
 (else (define-macro (%%include-tests . body) `(begin ,@body)))
 )

(%%include-tests
 (test-assert "take 0 from empty list" (eq? (take '() 0) '()))
 (test-assert "take car" (equal? (take '(1) 1) '(1)))
 (test-assert "take all" (equal? (take '(1 2) 3) '(1 2)))
 (test-assert
  "true inversion of partition"
  (let ((lst '(1 2 3 4 5)))
    (equal?
     (append (take lst 3) (drop lst 3))
     lst)))
 (test-assert
  "inversion of partition with impropper list"
  (let ((lst '(1 2 . tail)))
    (equal?
     (append (take lst 3) (drop lst 3))
     lst)))
 ) ;; end: %%include-tests
;;|#
