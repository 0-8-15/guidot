;; From SRFI-1

(define-macro (null-list? x) `(null? ,x))

(define (reduce f ridentity lis)
  ;;(check-arg procedure? f reduce)
  (if (null-list? lis) ridentity
      (fold f (car lis) (cdr lis))))

(define (any1 pred lis1)
  (and (not (null-list? lis1))
       (let lp ((head (car lis1)) (tail (cdr lis1)))
         (if (null-list? tail)
             (pred head)		; Last PRED app is tail call.
             (or (pred head) (lp (car tail) (cdr tail)))))))

(define any any1)

(define (lset-union = . lists)
  ;(check-arg procedure? = lset-union)
  (reduce (lambda (lis ans)		; Compute ANS + LIS.
	    (cond ((null? lis) ans)	; Don't copy any lists
		  ((null? ans) lis) 	; if we don't have to.
		  ((eq? lis ans) ans)
		  (else
		   (fold (lambda (elt ans) (if (any1 (lambda (x) (= x elt)) ans)
					       ans
					       (cons elt ans)))
			 ans lis))))
	  '() lists))
