;;; (C) 2020 JFW

;;; Tests for repetition.
;;;
;;; OVER -> to be hidden by macro expansion

(test-assert "OVER" (= ((OVER values identity values) 0) 0))

(test-assert
 "OVER eval"
 (=
  ((OVER
    (lambda (a b) (values '(lambda (a b) (+ a b)) (list a b)))
    (lambda (e v) (apply (eval e) v))
    values)
   2 3)
  5))

(test-assert
 "OVER #f/#f/2 eval" ;; NB: tests multiple intermediate values
 (=
  ((OVER
    (lambda (a b) (values '(lambda (a b) (+ a b)) (list a b)))
    (lambda (e v) (apply (eval e) v))
    values
    #f #f 2)
   2 3)
  ((OVER
    (lambda (a b) (values '(lambda (a b) (+ a b)) (list a b)))
    (lambda (e v) (apply (eval e) v))
    values)
   2 3)))

(test-assert
 "OVER list/vector"
 (equal?
  ((OVER
    (lambda (a b) (vector (+ a b) (* a b)))
    xcons
    list)
   2 3)
  '(#(5 6))))

(test-assert
 "OVER #f/#f/2 list/vector"
 (equal?
  ((OVER
    (lambda (a b) (vector (+ a b) (* a b)))
    xcons
    list
    #f #f 2)
   2 3)
  ((OVER
    (lambda (a b) (vector (+ a b) (* a b)))
    xcons
    list)
   2 3)))

(test-assert
 "OVER #f/1/2 list/vector"
 (equal?
  ((OVER
    (lambda (a b) (vector (+ a b) (* a b)))
    xcons
    list
    #f 1 2)
   2 3)
  ((OVER
    (lambda (a b) (vector (+ a b) (* a b)))
    xcons
    list)
   2 3)))

(test-assert
 "OVER with default R"
 (equal?
  ((OVER
    (lambda (a b) (* a b)))
   2 3)
  6))

(test-assert
 "OVER map/fold"
 (=
  ((OVER
    (lambda (a b) (map * a b))
    (lambda (i e) (apply + i e))
    (lambda () 11))
   '(2 3)
   '(5 7))
  42))

(test-assert
 "OVER #f/#f/2 map/fold"
 (=
  ((OVER
    (lambda (a b) (map * a b))
    (lambda (i e) (apply + i e))
    (lambda () 11)
    #f #f 2)
   '(2 3)
   '(5 7))
  ((OVER
    (lambda (a b) (map * a b))
    (lambda (i e) (apply + i e))
    (lambda () 11))
   '(2 3)
   '(5 7))))

(test-assert
 "OVER #f/1/2 map/fold"
 (=
  ((OVER
    (lambda (a b) (map * a b))
    (lambda (i e) (apply + i e))
    (lambda () 11)
    #f 1 2)
   '(2 3)
   '(5 7))
  ((OVER
    (lambda (a b) (map * a b))
    (lambda (i e) (apply + i e))
    (lambda () 11))
   '(2 3)
   '(5 7))))

(test-assert
 "OVER map/fold iterated"
 (=
  (let ((iter (OVER
               (lambda (a b) (map * a b))
               (lambda (i e) (apply + i e))
               (lambda () 11))))
    (iter
     '(2 3)
     '(5 7))
    (iter
     '(2 3)
     '(5 7)))
  73))

(test-assert
 "OVER #f/#f/2 map/fold iterated"
 (=
  (let ((iter (OVER
               (lambda (a b) (map * a b))
               (lambda (i e) (apply + i e))
               (lambda () 11)
               #f #f 2)))
    (iter
     '(2 3)
     '(5 7))
    (iter
     '(2 3)
     '(5 7)))
  (let ((iter (OVER
               (lambda (a b) (map * a b))
               (lambda (i e) (apply + i e))
               (lambda () 11))))
    (iter
     '(2 3)
     '(5 7))
    (iter
     '(2 3)
     '(5 7)))))

(test-assert
 "OVER #f/1/2 map/fold iterated"
 (=
  (let ((iter (OVER
               (lambda (a b) (map * a b))
               (lambda (i e) (apply + i e))
               (lambda () 11)
               #f 1 2)))
    (iter
     '(2 3)
     '(5 7))
    (iter
     '(2 3)
     '(5 7)))
  (let ((iter (OVER
               (lambda (a b) (map * a b))
               (lambda (i e) (apply + i e))
               (lambda () 11))))
    (iter
     '(2 3)
     '(5 7))
    (iter
     '(2 3)
     '(5 7)))))

(test-assert
 "OVER */+ iterated"
 (=
  (let ((iter (OVER
               *
               +
               (lambda () 11))))
    (iter 2 5)
    (iter 3 7)
    (iter 2 5)
    (iter 3 7))
  73))

(test-assert
 "OVER #f/#f/2  */+ iterated"
 (=
  (let ((iter (OVER
               *
               +
               (lambda () 11)
               #f #f 2)))
    (iter 2 5)
    (iter 3 7)
    (iter 2 5)
    (iter 3 7))
  (let ((iter (OVER
               *
               +
               (lambda () 11))))
    (iter 2 5)
    (iter 3 7)
    (iter 2 5)
    (iter 3 7))
  73))

(test-assert
 "OVER #f/1/2  */+ iterated"
 (=
  (let ((iter (OVER
               *
               +
               (lambda () 11)
               #f 1 2)))
    (iter 2 5)
    (iter 3 7)
    (iter 2 5)
    (iter 3 7))
  (let ((iter (OVER
               *
               +
               (lambda () 11))))
    (iter 2 5)
    (iter 3 7)
    (iter 2 5)
    (iter 3 7))
  73))

(test-assert "range? fixnum" (range? 2))
(test-assert "range? fixnum" (range? -2))
(test-assert "range? fixnum" (not (range? 0)))

(test-assert "range-rank fixnum positive" (= (range-rank 4) 1))
(test-assert "range-rank fixnum positive" (= (range-rank -3) 1))
(test-condition "range-rank fixnum zero -> range-exception?" (range-rank 0) range-exception?)

(test-assert "range-ascending?" (range-ascending? 2))

(test-condition "range volume not zero" (range-volume 0) range-exception?)
(test-assert
 ;; check off by one; status: never hit and little coverage
 "range volume not zero on -1"
 (= (range-volume -1) 1))


(test-assert "range volume positive fixnum" (= (range-volume 1) 1))
(test-assert "range volume negative fixnum" (= (range-volume -2) 2))

(test-assert "range? make-range" (range? (make-range 4)))
(test-assert "range? (range 4 2)" (range? (range 4 2)))
(test-assert "range? (range 4 2 3)" (range? (range 4 2 3)))
(test-assert "range? (range-step (range 4 2 3))" (= (range-step (range 4 2 3) 2) 8))
(test-assert "range? (range '#(4 2 3))" (range? (range '#(4 2 3))))
(test-assert "range? (range '#(2 2) 2)" (range? (range '#(2 2) 2)))

(define range-2x2 (range '#(2 2)))

(test-assert
 "vector->rangemask 3x2x2"
 (equal? (vector->rangemask '#(3 2 2)) '#(3 0 1 2 3 3 2 9 6)))

(test-assert
 "range-rank range-2x2"
 (= (range-rank range-2x2) 2))

(test-assert "range-volume" (= (range-volume range-2x2) 4))

(test-assert
 "for-range! positive fixnum"
 (let ((src '#(1 2 3))
       (dst (make-vector 3)))
   (and (= (for-range! (lambda (idx start len) (vector-set! dst idx (+ idx 1))) 3) 3)
        (equal? src dst))))

(test-assert
 "for-range! positive fixnum"
 (let ((src '#(1 2 3))
       (dst '()))
   (and (= (for-range! (lambda (idx start len) (set! dst (cons (+ idx 1) dst))) -3) 3)
        (equal? src (list->vector dst)))))

(test-condition
 "mdv-ref left index negative OOR exception on -1"
 (mdv-ref '#(11 12 21 22) range-2x2 -1 0)
 range-exception?)

(test-condition
 "mdv-ref left index OOR exception 1 dimensional"
 (mdv-ref '#(11 12 21 22) (range '#(2)) 2)
 range-exception?)

(test-assert
 "mdv-ref index in range 0,0"
 (=
  (mdv-ref '#(11 12 21 22) range-2x2 0 0)
  11))

(test-condition
 "mdv-ref left index (arg3==2) OOR exception"
 (mdv-ref '#(11 12 21 22) range-2x2 2 0)
 range-exception?)

(test-assert
 "mdv-ref index in range 0,1"
 (=
  (mdv-ref '#(11 12 21 22) range-2x2 0 1)
  12))

(test-assert
 "mdv-ref index in range 1,0"
 (=
  (mdv-ref '#(11 12 21 22) range-2x2 1 0)
  21))

(test-assert
 "mdv-ref index in range 1,1"
 (=
  (mdv-ref '#(11 12 21 22) range-2x2 1 1)
  22))

(test-condition
 "mdv-ref index OOR exception"
 (mdv-ref '#(11 12 21 22) (range-row range-2x2 0) 2)
 range-exception?)

(test-assert
 "for-range! range 1d ascending"
 (let ((pat '#(1 2 3))
       (dst '()))
   (and (=
         (for-range!
          (lambda (idx offset len) (set! dst `(,(fx+ idx 1) . ,dst))  1)
          (range '#(3)))
         3)
        (equal? pat (list->vector (reverse dst))))))

(test-assert
 "for-range! range 1d descending"
 (let ((pat '#(1 2 3))
       (dst '()))
   (and (=
         (for-range!
          (lambda (idx offset len) (set! dst `(,(fx+ idx 1) . ,dst))  1)
          (range '#(-3)))
         3)
        (equal? pat (list->vector dst)))))

(test-assert
 "for-range! range 2x3 ascending (rows)"
 (let ((pat '#(0 3))
       (dst '()))
   (and (=
         (for-range!
          (lambda (idx offset len)
            (set! dst (cons offset dst)) 1)
          (range '#(3 2)))
         2)
        (equal? pat (list->vector (reverse dst))))) )

(test-assert
 "for-range! range 2x3 descending"
 (let ((pat '#(3 0))
       (dst '()))
   (and (=
         (for-range!
          (lambda (idx offset len) (set! dst (cons offset dst)) 1)
          (range '#(3 -2)))
         2)
        (equal? pat (list->vector (reverse dst))))))

(test-assert
 "range-row dimension row 0"
 (=
  (range-rank (range-row range-2x2 0))
  1))

(test-assert
 "range-row volume row 0"
 (=
  (range-volume (range-row range-2x2 0))
  2))

(test-assert
 "range-row dimension row 1"
 (=
  (range-rank (range-row range-2x2 1))
  1))

(test-assert
 "range-row volume row 1"
 (=
  (range-volume (range-row range-2x2 0))
  2))

(test-assert
 "for-range! nested call sequence range 2x2"
 (let ((pat '#((0 0) (0 1) (1 0) (1 1)))
       (dst '()))
   (and
    (=
     (let ((full range-2x2))
       (for-range!
        (lambda (row start0 len)
          (let ((rr (range-row full row)))
            (for-range! rr (lambda (col col0 collen) (set! dst `((,row ,col) . ,dst)) 0))
            (range-volume rr)))
        full))
     4)
    (equal? pat (list->vector (reverse dst))))))

(test-assert
 "for-range! nested call sequence offset/length range 2x2"
 (let ((cpat '#((0 0) (0 1) (1 0) (1 1)))
       (roflpat '#((0 0 2) (1 2 2)))
       (rofl '())
       (dst '()))
   (and
    (=
     (let ((full range-2x2))
       (for-range!
        (lambda (row start0 len)
          (set! rofl `((,row ,start0 ,len) . ,rofl))
          (let ((rr (range-row full row)))
            (for-range! rr (lambda (col col0 collen) (set! dst `((,row ,col) . ,dst)) 0))
            (range-volume rr)))
        full))
     4)
    (equal? cpat (list->vector (reverse dst)))
    (equal? roflpat (list->vector (reverse rofl))))) )

(test-assert
 "for-range! nested call sequence offset/length range 2x2x3"
 (let ((cpat '#((0 0) (0 1) (1 0) (1 1) (2 0) (2 1)))
       (roflpat '#((0 0 4) (1 4 4) (2 8 4)))
       (rofl '())
       (dst '()))
   (and
    (=
     (let ((full (range 2 2 3)))
       (for-range!
        (lambda (row start0 len)
          (set! rofl `((,row ,start0 ,len) . ,rofl))
          (let ((rr (range-row full row)))
            (for-range! rr (lambda (col col0 collen) (set! dst `((,row ,col) . ,dst)) 1))))
        full))
     6)
    (equal? cpat (list->vector (reverse dst)))
    (equal? roflpat (list->vector (reverse rofl))))))


(test-assert
 "for-range! nested call sequence offset/length range 2x2x3"
 (let ((cpat '#((0 0) (0 1) (1 0) (1 1) (2 0) (2 1)))
       (roflpat '#((0 0 4) (1 4 4) (2 8 4)))
       (colpat '((row 0 col 0 offset 0 len 2)
                 (row 0 col 1 offset 2 len 2)
                 (row 1 col 0 offset 4 len 2)
                 (row 1 col 1 offset 6 len 2)
                 (row 2 col 0 offset 8 len 2)
                 (row 2 col 1 offset 10 len 2)))
       (coll '())
       (rofl '())
       (dst '()))
   (and
    (=
     (let ((full (range 2 2 3)))
       (for-range!
        (lambda (row start0 len)
          (set! rofl `((,row ,start0 ,len) . ,rofl))
          (let ((rr (range-row full row)))
            (for-range!
             rr
             (lambda (col col0 collen)
               (set! dst `((,row ,col) . ,dst))
               (set! coll `((row ,row col ,col offset ,(+ col0 start0) len ,collen) . ,coll))
               1))))
        full))
     6)
    (equal? cpat (list->vector (reverse dst)))
    (equal? roflpat (list->vector (reverse rofl)))
    (equal? colpat (reverse coll)))))

(test-assert
 "for-range! nested call sequence offset/length range 3x2x2"
 (let ((cpat '#((0 0) (0 1) (1 0) (1 1)))
       (roflpat '#((0 0 6) (1 6 6)))
       (colpat '((row 0 col 0 offset 0 len 3)
                 (row 0 col 1 offset 3 len 3)
                 (row 1 col 0 offset 6 len 3)
                 (row 1 col 1 offset 9 len 3)))
       (coll '())
       (rofl '())
       (dst '()))
   (and
    (=
     (let ((full (range 3 2 2)))
       (for-range!
        (lambda (row start0 len)
          (set! rofl `((,row ,start0 ,len) . ,rofl))
          (let ((rr (range-row full row)))
            (for-range!
             rr
             (lambda (col col0 collen)
               (set! dst `((,row ,col) . ,dst))
               (set! coll `((row ,row col ,col offset ,(+ col0 start0) len ,collen) . ,coll))
               1))))
        full))
     4)
    (equal? cpat (list->vector (reverse dst)))
    (equal? roflpat (list->vector (reverse rofl)))
    (equal? colpat (reverse coll))))

 )

(define (test-product-1/3x3/1 TESTNO)
  (let* ((r3x1 (range 3 1))
         (r1x3 (range 1 3))
         (r3x3 (range 3 3))
         (res (make-vector (range-volume r3x3) #f)))
    (receive (rl rr)
        (case TESTNO
          ((1) (values r1x3 r3x1))
          ((2) (values r3x1 r1x3))
          ((3) (values r1x3 r1x3))
          ((4) (values r3x1 r3x1))
          (else (error "out of test cases")))
      (let* ((call-number 0)
             (calls
              (for-range!
               (lambda (row start0 len)
                 (for-range!
                  rr
                  (lambda (col col0 collen)
                    (set! call-number (+ call-number 1))
                    (vector-set!
                     res
                     (mdv-idx r3x3 row col)
                     `(call ,call-number row ,row rowlen ,len col ,col collen ,collen))
                    1)))
               rl)))
        (when #f (debug 'calls call-number) (pp res))
        (values call-number res))))

  )

(test-assert
 "product-1/3x3/1 TESTNO 1: r1x3 r3x1"
 (let* ((TESTNO 1)
        (pat
         '#((call 1 row 0 rowlen 1 col 0 collen 3)
            #f
            #f
            (call 2 row 1 rowlen 1 col 0 collen 3)
            #f
            #f
            (call 3 row 2 rowlen 1 col 0 collen 3)
            #f
            #f)))
   (receive (calls res) (test-product-1/3x3/1 TESTNO)
     (and (= calls 3) (equal? pat res))))

 )

(test-assert
 "product-1/3x3/1 TESTNO 2: r3x1 r1x3"
 (let* ((TESTNO 2)
        (pat
         '#((call 1 row 0 rowlen 3 col 0 collen 1)
            (call 2 row 0 rowlen 3 col 1 collen 1)
            (call 3 row 0 rowlen 3 col 2 collen 1)
            #f
            #f
            #f
            #f
            #f
            #f)
         ))
   (receive (calls res) (test-product-1/3x3/1 TESTNO)
     (and (= calls 3) (equal? pat res))))

 )

(test-assert
 "product-1/3x3/1 TESTNO 3: r1x3 r1x3"
 (let* ((TESTNO 3)
        (pat
         '#((call 1 row 0 rowlen 1 col 0 collen 1)
            (call 2 row 0 rowlen 1 col 1 collen 1)
            (call 3 row 0 rowlen 1 col 2 collen 1)
            (call 4 row 1 rowlen 1 col 0 collen 1)
            (call 5 row 1 rowlen 1 col 1 collen 1)
            (call 6 row 1 rowlen 1 col 2 collen 1)
            (call 7 row 2 rowlen 1 col 0 collen 1)
            (call 8 row 2 rowlen 1 col 1 collen 1)
            (call 9 row 2 rowlen 1 col 2 collen 1))))
   (receive (calls res) (test-product-1/3x3/1 TESTNO)
     (and (= calls 9) (equal? pat res)))))


(test-assert
 "product-1/3x3/1 TESTNO 4; r3x1 r3x1"
 (let* ((TESTNO 4)
        (pat '#((call 1 row 0 rowlen 3 col 0 collen 3) #f #f #f #f #f #f #f #f)))
   (receive (calls res) (test-product-1/3x3/1 TESTNO)
     (and (= calls 1) (equal? pat res))))

 )

;|#
;;  eof
