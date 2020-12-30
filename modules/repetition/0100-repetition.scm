;;; (C) 2020 JFW

;;;* repetition

;;; OVER -> to be hidden by macro expansion

;;; range, for-range!
;;;
;;; experimental: mdv-ref mdv-ref/getter

;;;** STATUS

;; TBD: remove fixnum & vector-free use case of range structure! (dev
;; artifact)

;;;** OVER

;;; (define (OVER0-attic #!optional (R values) (M values))
;;;   ;; alternative: (lambda in (call-with-values (lambda () (apply M in)) R))
;;;   (lambda in (receive out (apply M in) (apply R out))))

(define-values
    (;; EXAMPLE:
     ;;
     ;; ((OVER
     ;;   (lambda (a b) (values '(lambda (a b) (+ a b)) (list a b)))
     ;;   (lambda (e v) (apply (eval e) v))
     ;;   values)
     ;;  2 3)
     ;; => 5
     OVER
     ;; generic iterator creator iterator, pure, no shortcut
     %%OVER/canonical
     ;; generic iterator creator iterator, shortcut on Z?
     %%OVER/canonical+?)

  (define (OVER/canonical M R N)
    ;; BEWARE: REFERENCE version DO NOT CHANGE!
    ;;
    ;; TBD: respect Z? ???  NOT HERE!
    (receive inits (N)
      (lambda in
        (call-with-values
            (lambda () (apply M in))
          (cond
           ((null? inits) R)
           (else
            (lambda out
              (receive accu (apply R (append inits out))
                (set! inits accu)
                (apply values accu)))))))))

  (define (aborted! proc) (error "already aborted" proc))

  (define (%%OVER/canonical+? M R N Z?)
    (receive inits (N)
      (lambda in
        (unless inits (aborted! %%OVER/canonical+?))
        (call-with-values
            (lambda () (apply M in))
          (cond
           ((null? inits) R)
           (else
            (lambda out
              (receive accu (apply R (append inits out))
                (cond
                 ((and Z? (apply Z? accu))
                  (set! inits #f))
                 (else (set! inits accu)))
                (apply values accu)))))))))

  (define (%%OVER/canonical*Nx1x1x1 M R N Z?)
    ;; ONE intermediate result, one final result, one init value
    (let ((init (N)) (aborted #f))
      (lambda in
        (when aborted (aborted! %%OVER/canonical*Nx1x1x1))
        (let ((accu (R init (apply M in))))
          (cond
           ((and Z? (Z? accu))
            (set! aborted #t))
           (else (set! init accu))))
        init)))

  (define (%%OVER/canonical*2x1x1x1 M R N Z?)
    ;; ONE intermediate result, one final result, one init value
    (let ((init (N)) (aborted #f))
      (lambda (a1 a2)
        (when aborted (aborted! %%OVER/canonical*2x1x1x1))
        (let ((accu (R init (M a1 a2))))
          (cond
           ((and Z? (Z? accu))
            (set! aborted #t))
           (else (set! init accu))))
        init)))

  (define (OVER/2 M R N Z? W)
    (cond
     ((equal? W 1) (%%OVER/canonical*2x1x1x1 M R N Z?))
     (else
      (receive inits (N)
        (lambda (a1 a2)
          (unless inits (aborted! OVER/2))
          (call-with-values
              (lambda () (M a1 a2))
            (cond
             ((null? inits) R)
             ((and (pair? (cdr inits)) (null? (cddr inits)))
              (let ((i1 (car inits))
                    (i2 (cadr inits)))
                (lambda out
                  (receive accu (apply R (cons* i1 i2 out))
                    (cond
                     ((and Z? (apply Z? accu))
                      (set! inits #f))
                     (else (set! inits accu)))
                    (apply values accu)))))
             (else
              (lambda out
                (receive accu (apply R (append inits out))
                  (cond
                   ((and Z? (apply Z? accu))
                    (set! inits #f))
                   (else (set! inits accu)))
                  (apply values accu)))))))) )))

  (define (%%OVER/canonical*3x1x1x1 M R N Z?)
    ;; ONE intermediate result, one final result, one init value
    (let ((init (N)) (aborted #f))
      (lambda (a1 a2 a3)
        (when aborted (aborted! %%OVER/canonical*2x1x1x1))
        (let ((accu (R init (M a1 a2 a3))))
          (cond
           ((and Z? (Z? accu))
            (set! aborted #t))
           (else (set! init accu))))
        init)))

  (define (OVER/3 M R N Z? W)
    (cond
     ((equal? W 1) (%%OVER/canonical*3x1x1x1 M R N Z?))
     (else
      (receive inits (N)
        (lambda (a1 a2 a3)
          (unless inits (aborted! OVER/2))
          (call-with-values
              (lambda () (M a1 a2 a3))
            (cond
             ((null? inits) R)
             ((and (pair? (cdr inits)) (null? (cddr inits)))
              (let ((i1 (car inits))
                    (i2 (cadr inits)))
                (lambda out
                  (receive accu (apply R (cons* i1 i2 out))
                    (cond
                     ((and Z? (apply Z? accu))
                      (set! inits #f))
                     (else (set! inits accu)))
                    (apply values accu)))))
             (else
              (lambda out
                (receive accu (apply R (append inits out))
                  (cond
                   ((and Z? (apply Z? accu))
                    (set! inits #f))
                   (else (set! inits accu)))
                  (apply values accu)))))))) )))

  (define (OVER #!optional (M values) (R values) (N R) (Z? #f) (W #f) (A #f))
    (unless ;; argument checks
        (and (procedure? M) (procedure? R) (procedure? N)
             (or (not Z?) (procedure? Z?)))
      (error "illegal argument" %%OVER/canonical+? M R N Z?))
    (cond
     ;;((or #t) (%%OVER/canonical M R N))
     ((equal? A 2) (OVER/2 M R N Z? W))
     ((equal? A 3) (OVER/3 M R N Z? W))
     ((equal? W 1) (%%OVER/canonical*Nx1x1x1 M R N Z?))
     (else (%%OVER/canonical+? M R N Z?))))

  (values OVER OVER/canonical %%OVER/canonical+?))

;;;* range, for-range!

(define-structure range ;; a.k.a. fixnum iota
  ;; data cases: a) inline b) at offset in 3vector
  dim ;; (experimental: negative? => backwards) a: number of elements
  ;; b: rank
  volume
  (offset unprintable:) ;; a: start b: offset into data
  (in unprintable:)     ;; a: step size b: 3vector
  )

(define debug#range-in range-in)

(define vector->rangemask)

(define range) ;; short for make-range (like vector etc.)
(define range-rank)
(define range-start) ;; maybe only internal?
(define range-step)  ;; maybe only internal?
(define range-size)  ;; try to make internal only
(define range-ascending?)
(define range-row) ;; shared subrange

(define mdv-ref)
(define mdv-ref/getter)
(define mdv-indexer) ;; range size offset -> lambda with rank arity

(let ((allocate-range make-range)
      (%%range-structure? range?)
      (%%structure-range-volume range-volume)
      (%%range-in range-in))
  (define (%%raise-range-exception arg-num proc val . more)
    (##raise-range-exception arg-num proc (cons val more)))
  (define (%%vector->rangemask vec)
    (let ((leni (vector-length vec)))
      (let* ((len (* 3 leni))
             ;; TBD: avoid useless initialization of `result`
             (result (make-vector len -1)))
        (do ((si 0 (fx+ si 1))
             (i 0 (fx+ i 3))
             (j 1 (fx+ j 3))
             (k 2 (fx+ k 3))
             (vol 1)
             (sum 0))
            ((fx>= si leni) result)
          (let ((count (vector-ref vec si)))
            (unless (and (number? count) (integer? count) (not (fx= count 0)))
              (##raise-range-exception si 'range count))
            (vector-set! result i count)
            (vector-set! result k vol)
            (set! vol (fx* (abs count) vol))
            (vector-set! result j sum)
            (set! sum (fx+ sum vol)))))))
  (define (%%fixnum-range? obj)
    (and (number? obj) (integer? obj) (not (fx= obj 0))))
  (define (%%fixnum-range/assert? obj)
    ;; for argument checking/dispatching: error out on illegal numeric ranges
    (or (%%fixnum-range? obj)
        (and (number? obj)  (%%raise-range-exception 1 %%fixnum-range? obj))))
  (define (%%range? obj)
    ;;(MATURITY +2 "draft" range?)
    (cond
     ((%%fixnum-range? obj))
     ((%%range-structure? obj))
     (else #f)))
  (define (%%make-range pat . more)
    (cond
     ((vector? pat)
      (let ((rank (vector-length pat))
            (interned (%%vector->rangemask pat)))
        (allocate-range
         rank
         (fx* ;; volume
          (vector-ref interned (fx- (vector-length interned) 1))
          (vector-ref pat (fx- (vector-length pat) 1)))
         0 interned)))
     ((number? pat) (%%make-range (apply vector pat more)))
     (else (error "%%make-range: invalid arguments" pat more))))
  (define (%%range-rank obj)
    (cond
     ((%%fixnum-range/assert? obj) 1)
     ((%%range-structure? obj) (abs (range-dim obj)))
     (else (error "not a range" 'range-rank obj))))
  (define (%%range-row obj n)
    (cond
     ((not (%%range-structure? obj))
      (error "not a multidimensional range" 'range-row obj))
     ((not (and (number? n) (integer? n)))
      (error "invalid index" 'range-row n)))
    (let* ((d0 (range-dim obj))
           (d0a (abs d0))
           (na (abs n))
           (z (range-offset obj))
           (d1i (- d0a 1)))
      (when (or (< na 0) (> na d0a))
        (%%raise-range-exception 1 'range-row d0 n))
      (let* ((d1 (%%range-size obj d1i))
             (step (%%range-step obj (- d1i 1)))
             (vol (/ (%%range-volume obj) d0a))
             (z (+ z (%%range-start obj na))))
        (allocate-range (if (< d0 0) (- d1i) d1i) vol z (range-in obj)))))
  (define (%%range-volume obj)
    (cond
     ((%%fixnum-range/assert? obj) (abs obj))
     ((%%range-structure? obj) (%%structure-range-volume obj))
     (else (error "not a range" 'range-volume obj))))
  (define (dimension rng step d vzs) ;; FIXME: runs too often in tight loops
    (let (;; (z0 0 #;(range-offset rng)) ;; usually zero
          (dn (abs (range-dim rng))))
      (when (or (< d 0) (>= d dn))
        (%%raise-range-exception 3 'range-dimension d dn rng))
      ;; (vector-ref step (fx+ (fx+ (fx* d 3) vzs) z0))
      (vector-ref step (fx+ (fx* d 3) vzs))))
  (define (%%range-size obj dim)  ;; [maybe stay only internal?]
    (cond
     ((number? obj) ;; disabled case
      (%%raise-range-exception 1 %%range-size obj)
      (unless (fx= dim 0) ;; error check
        (%%raise-range-exception 2 %%range-size dim))
      (%%range-volume obj))
     ((%%range-structure? obj)
      (let ((step (%%range-in obj)))
        (cond
         ((vector? step) (dimension obj step dim 0))
         (else step))))
     (else (error "argument error" 'range-size obj))))
  (define (%%range-start obj dim) ;; maybe only internal?
    (cond
     ((%%fixnum-range/assert? obj) 0)
     ((%%range-structure? obj)
      (let ((step (%%range-in obj)))
        (cond
         ((vector? step) (dimension obj step dim 1))
         (else step))))
     (else (error "argument error" 'range-start obj))))
  (define (%%range-step obj dim)  ;; maybe only internal?
    (cond
     ((%%fixnum-range/assert? obj) 1)
     ((%%range-structure? obj)
      (let ((step (%%range-in obj)))
        (cond
         ((vector? step) (dimension obj step dim 2))
         (else step))))
     (else (error "argument error" 'range-step obj))))
  (define (%%range-ascending? obj)
    (cond
     ((%%fixnum-range/assert? obj) (positive? obj))
     ((%%range-structure? obj) (positive? (range-dim obj)))
     (else (error "argument error" 'range-ascending? obj))))

  (define (%%MATURITY+4:mdv-indexer range #!optional (storage-size #f) (storage-offset 0))
    (when (and storage-size
               (fx< storage-size (fx+ storage-offset (range-volume range))))
      (%%raise-range-exception 2 'mdv-indexer-select range storage-size storage-offset))
    (case (range-rank range)
      ((1)
       (lambda (index)
         (let ((limit (%%range-volume range))
               (idx (+ (fx* index (%%range-step range 0))
                       (%%range-start range 0))))
           (when (or (fx>= index limit) (fx< index 0))
             (%%raise-range-exception 1 'mdv-indexer index limit))
           (fx+ storage-offset idx))))
      ((2)
       (lambda (i1 i2)
         (let ((element-size 1)
               (row0
                (or (and
                     (let ((obj i1)
                           (lower-bound 0)
                           (upper-bound (%%range-size range 1)))
                       (and (number? obj) (integer? obj)
                            (fx>= obj lower-bound)
                            (fx< obj upper-bound)
                            (fx* obj (%%range-start range 1)))))
                    (%%raise-range-exception 1 'mdv-indexer i1)))
               (offset
                (or (and
                     (let ((obj i2)
                           (lower-bound 0)
                           (upper-bound (%%range-size range 0)))
                       (and (number? obj) (integer? obj)
                            (fx>= obj lower-bound)
                            (fx< obj upper-bound)
                            obj)))
                    (%%raise-range-exception 2 'mdv-indexer i2))))
           (fx+ storage-offset (fx+ row0 (fx* offset element-size))))))
      (else (NYI "generic case" %%MATURITY+4:mdv-indexer range))))

  (define %%MATURITY+4:mdv-ref/getter
    (case-lambda
     ((ref vector range index)
      (unless (fx= (range-rank range) 1) ;; note: use safe/testing version
        (error "wrong number of indices: range requires" (range-rank range) 'mdv-ref))
      (let ((limit (%%range-volume range))
            (idx (+ (fx* index (%%range-step range 0))
                    (%%range-start range 0))))
        (when (or (fx>= index limit) (fx< index 0))
          (%%raise-range-exception 3 'mdv-ref/getter index limit))
        (ref vector idx)))
     ((ref vector range i1 i2)
      (unless (fx= (range-rank range) 2)
        (error "wrong number of indices: range requires" (range-rank range) 'mdv-ref))
      (let ((element-size 1)
            ;;(soso (dimension rng step d vzs))
            (row0
             (or (and
                  (let ((obj i1)
                        (lower-bound 0)
                        (upper-bound (%%range-size range 1)))
                    (and (number? obj) (integer? obj)
                         (fx>= obj lower-bound)
                         (fx< obj upper-bound)
                         (fx* obj (%%range-start range 1)))))
                 (%%raise-range-exception 3 'mdv-ref/getter i1)))
            (offset
             (or (and
                  (let ((obj i2)
                        (lower-bound 0)
                        (upper-bound (%%range-size range 0)))
                    (and (number? obj) (integer? obj)
                         (fx>= obj lower-bound)
                         (fx< obj upper-bound)
                         obj)))
                 (%%raise-range-exception 4 'mdv-ref/getter i2))))
        (ref vector (fx+ row0 (fx* offset element-size)))))
     ((ref vector range i1 i2 . more)
      (NYI "mdv-ref: too many arguments - there shall be at most two indices, natural and complex" more))))
  (define %%MATURITY+4:mdv-ref
    (case-lambda
     ((vector range index) (%%MATURITY+4:mdv-ref/getter vector-ref vector range index))
     ((vector range i1 i2) (%%MATURITY+4:mdv-ref/getter vector-ref vector range i1 i2))
     (otherwise (apply %%MATURITY+4:mdv-ref/getter vector-ref otherwise))))
  (set! vector->rangemask %%vector->rangemask)
  (set! range-rank %%range-rank)
  (set! range? %%range?)
  (set! make-range %%make-range)
  (set! range make-range)
  (set! range-volume %%range-volume)
  (set! range-size %%range-size)
  (set! range-start %%range-start) ;; maybe only internal?
  (set! range-step %%range-step)  ;; maybe only internal?
  (set! range-ascending? %%range-ascending?)
  (set! range-row %%range-row)
  (set! mdv-ref/getter %%MATURITY+4:mdv-ref/getter)
  (set! mdv-ref %%MATURITY+4:mdv-ref)
  (set! mdv-indexer %%MATURITY+4:mdv-indexer)
  ) ;; end range refinement

(define mdv-idx
  (let ((extract (lambda (v idx) idx)))
    (case-lambda
     ((range index)
      #;(values (mdv-ref/getter extract 'dummyvector range index) (range-step range 0))
      (mdv-ref/getter extract 'dummyvector range index))
     ((range i1 i2)
      #;(values (mdv-ref/getter extract 'dummyvector range i1 i2) (range-step range 0))
      (mdv-ref/getter extract 'dummyvector range i1 i2))
     (otherwise (apply mdv-ref/getter vector-ref otherwise)))))

;; for-range! proc! range ... call f with range numbers of indices in lecixographic order
;;
;; RETURN: number of calls to `f` considered and decided: ... or
;; maybe forget about the return value, it should maybe return the
;; however compute sum of all effects.  I.e., nothing here. ... at the
;; other hand multiplying the ranges length should be easy enough, so
;; better return the volume!

(define (MATURITY+2:for-range! range proc! . ranges)
  (cond
   ;; rearrange arguments ... :-/
   ((and (procedure? range) (range? proc!))
    (let ((t range)) (set! range proc!) (set! proc! t))))
  (cond
   ((pair? ranges)
    ;; DO NOT EVEN THINK about additional ranges and argument ordering
    ;; until there is a good working 2x2-test case.
    (let ((nyimsg "only one range so far"))
      (NYI nyimsg 'for-range! range) (error nyimsg 'for-range! range)))
   ((fixnum? range)
    (cond
     ((positive? range)
      (do ((i 0 (fx+ i 1)))
          ((fx= i range) range)
        (proc! i 0 1)))
     ((negative? range)
      (do ((i (fx- -1 range) (fx- i 1)))
          ((fx< i 0) (fx- range))
        (proc! i 0 1)))
     (else 0)))
   ((>= (range-rank range) 1)
    (let* ((rank (range-rank range))
           (rank-1 (fx- rank 1)))
      (let ((length (range-size range rank-1))
            (step (range-step range rank-1)))
        (let ((proc! (OVER proc! +)))
          (cond
           ((positive? length) ;;
            (do ((i 0 (fx+ i 1))
                 (j 0 (fx+ j step))
                 (r 0))
                ((fx= i length) r)
              (set! r (proc! i j step))))
           ((negative? length)
            (let* ((length (abs length))
                   (limit (fx- length 1)))
              (do ((i limit (fx- i 1))
                   (j (fx* limit step) (fx- j step))
                   (r 0))
                  ((fx< i 0) r)
                (set! r (proc! i j length)))))
           (error "for-range! INTERNAL ERROR invalid range argument" range))))))
   (else
    (apply NYI 'for-range! range ranges) (apply error 'for-range! range ranges))))

(define for-range! MATURITY+2:for-range!)
