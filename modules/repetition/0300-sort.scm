;;; License: BSD 3 clause
;;;
;;; A simple, non-allocating[*], destructive sort.
;;;
;;; Stolen from Chicken Scheme, but uses SRFI-132 argument order.
;;;
;;; [*] Non-allocating for lists - sorting vectors allocates a
;;; intermediate list of the elements.

;;; (sort! less? sequence)
;;; sorts the list or vector sequence destructively.  It uses a version
;;; of merge-sort invented, to the best of my knowledge, by David H. D.
;;; Warren, and first used in the DEC-10 Prolog system.	 R. A. O'Keefe
;;; adapted it to work destructively in Scheme.

(define (sort! less? seq)
  ;; (merge! a b less?)
  ;; takes two sorted lists a and b and smashes their cdr fields to form a
  ;; single sorted list including the elements of both.
  ;; Note:  this does _not_ accept vectors.
  (define (merge! a b less?)
    (define (loop r a b)
      (if (less? (car b) (car a))
	  (begin
	    (set-cdr! r b)
	    (if (null? (cdr b))
	        (set-cdr! b a)
	        (loop b a (cdr b)) ))
	  ;; (car a) <= (car b)
	  (begin
	    (set-cdr! r a)
	    (if (null? (cdr a))
	        (set-cdr! a b)
	        (loop a (cdr a) b)) )) )
    (cond
     ((null? a) b)
     ((null? b) a)
     ((less? (car b) (car a))
      (if (null? (cdr b))
	  (set-cdr! b a)
	  (loop b a (cdr b)))
      b)
     (else ; (car a) <= (car b)
      (if (null? (cdr a))
	  (set-cdr! a b)
	  (loop a (cdr a) b))
      a)))  (define (step n)
              (cond
               ((> n 2)
                (let* ((j (quotient n 2))
	               (a (step j))
	               (k (- n j))
	               (b (step k)))
	          (merge! a b less?)))
               ((= n 2)
                (let ((x (car seq))
	              (y (cadr seq))
	              (p seq))
	          (set! seq (cddr seq))
	          (if (less? y x) (begin
			            (set-car! p y)
			            (set-car! (cdr p) x)))
	          (set-cdr! (cdr p) '())
	          p))
               ((= n 1)
                (let ((p seq))
	          (set! seq (cdr seq))
	          (set-cdr! p '())
	          p))
               (else
                '()) ))
      (if (vector? seq)
          (let ((n (vector-length seq))
	        (vec seq))
	    (set! seq (vector->list seq))
	    (do ((p (step n) (cdr p))
	         (i 0 (+ i 1)))
	        ((null? p) vec)
	      (vector-set! vec i (car p)) ))
          ;; otherwise, assume it is a list
          (step (length seq)) ))
