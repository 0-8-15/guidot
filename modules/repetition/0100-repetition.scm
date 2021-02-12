;;; (C) 2020 JFW

;;;* repetition

;;; OVER -> to be hidden by macro expansion

;;; range, for-range!
;;;
;;; experimental: mdv-ref mdv-ref/getter

;;;** STATUS

;; TBD: remove fixnum & vector-free use case of range structure! (dev
;; artifact)

;;*** syntax import

(define-macro (define-values names . body)
  (let ((vals (gensym 'vals)))
    `(begin
       ,@(map (lambda (name) `(define ,name #f)) names)
       (call-with-values (lambda () . ,body)
         (lambda ,vals
           . ,(map (lambda (name)
                     `(set! ,name (let ((,name (car ,vals))) (set! ,vals (cdr ,vals)) ,name)))
                   names))))))

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

(define (rangemask? obj)
  (MATURITY +1 "planning, no damage, always failes" rangemask?)
  #f)

(define-structure range ;; a.k.a. fixnum iota
  opaque:
  macros:
  prefix: macro-
  ;; data cases: a) inline b) at offset in 3vector
  dim ;; (experimental: negative? => backwards) a: number of elements
  ;; b: rank
  volume
  (offset unprintable:) ;; a: start b: offset into data
  (in unprintable:)     ;; a: step size b: 3vector
  )

(define dev#make-range make-range)

(define (%%range? obj) (macro-range? obj))
(define (range-dim rng) (macro-range-dim rng))
(define (range-volume rng) (macro-range-volume rng))
(define (debug#range-offset rng) (macro-range-offset rng))
(define (debug#range-in rng) (macro-range-in rng))

(define vector->rangemask)

(define range?)
(define range) ;; short for make-range (like vector etc.)
(define call-in-range) ;; call proc with iota-style arguments (to be inlined in target code)
(define range-rank)
(define range-start) ;; maybe only internal?
(define range-step)  ;; maybe only internal?
(define range-size)  ;; try to make internal only
(define range-ascending?)
(define range-row) ;; shared subrange

(define mdv-ref)
(define mdv-ref/getter)
(define mdv-indexer) ;; range size offset -> lambda with rank arity

(define-macro (macro-%%range-start//checks obj dim)
  (let ((step (gensym 'step)))
    `(let ((,step (macro-range-in ,obj)))
       (cond
        ((vector? ,step)
         (##vector-ref ,step (fx+ (fx* ,dim 3) 1)))
        (else ,step)))))

(define-macro (macro-%%range-size//checks obj dim)
  (let ((step (gensym 'step)))
    `(let ((,step (macro-range-in ,obj)))
       (cond
        ((vector? ,step) (##vector-ref ,step (fx* ,dim 3)))
        (else ,step)))))

(let () ;; --- range refinement ---  DEFINE-VALUES style block
  ;;; comment this block out for debugging/profiling, required for production
  (define-macro (allocate-range a b c d) `(macro-make-range ,a ,b ,c ,d))
  (define-macro (%%range-structure? obj) `(macro-range? obj))
  (define-macro (%%structure-range-volume rng) `(macro-range-volume ,rng))
  (define-macro (%%range-in rng) `(macro-range-in ,rng))
  (define (%%raise-range-exception arg-num proc val . more)
    (##raise-range-exception arg-num proc (cons val more)))
  (define (%%vector->rangemask vec #!optional (offset 0))
    ;; FIXME: intented meaning: each entry is `(X . (,iota "(- X 1)"))
    ;; TBD: likely buggy, test and cleanup
    (let ((leni (vector-length vec)))
      (let* ((len (* 3 leni))
             ;; TBD: avoid useless initialization of `result`
             (result (make-vector len -1)))
        (do ((si 0 (fx+ si 1))
             (i 0 (fx+ i 3))
             (j 1 (fx+ j 3))
             (k 2 (fx+ k 3))
             (sum offset)
             (vol 1))
            ((fx>= si leni) result)
          (let ((count (vector-ref vec si)))
            (unless (and (number? count) (integer? count) (not (fx= count 0)))
              (##raise-range-exception si 'range count))
            (vector-set! result i count)
            (vector-set! result k vol)
            (vector-set! result j sum)
            (set! vol (fx* (abs count) vol))
            (set! sum (+ offset vol)))))))
  (define (%%fixnum-range? obj)
    (and (integer? obj) (exact? obj) (not (fx= obj 0))))
  (define (%%fixnum-range/assert? obj)
    ;; for argument checking/dispatching: error out on illegal numeric ranges
    (or (%%fixnum-range? obj)
        (and (number? obj)  (%%raise-range-exception 1 %%fixnum-range? obj))))
  (define (%%range? obj) ;; FIXME: bad idea, performance wise
    ;;
    ;;(MATURITY +2 "draft" range?)
    (cond
     ((%%fixnum-range? obj))
     ((%%range-structure? obj))
     (else #f)))
  (define (%%make-range pat . more)
    (cond
     ;; ((rangemask? pat) (NYI "make-range rangemask?" pat))
     ((vector? pat)
      (let ((rank (vector-length pat))
            (interned (%%vector->rangemask pat)))
        (allocate-range
         rank
         (fx* ;; volume
          (##vector-ref interned (fx- (vector-length interned) 1))
          (##vector-ref pat (fx- rank 1)))
         0 interned)))
     ((number? pat) (%%make-range (apply vector (reverse! (cons pat more)))))
     (else (error "make-range: invalid arguments" pat more))))
  (define (%%MATURITY+4:call-in-range range proc i1 . more)
    (unless (and (%%range-structure? range) (procedure? proc))
      (error "invalid arguments" 'call-in-range range proc))
    (case (range-rank range)
      ((1)
       (let ((storage-offset 0) ;; TBD
             (limit (%%range-volume range))
             (idx (+ (fx* i1 (%%range-step range 0))
                     (%%range-start range 0))))
         (when (or (fx>= i1 limit) (fx< i1 0))
           (%%raise-range-exception 3 'call-in-range i1 limit))
         (proc
          (fx+ storage-offset idx) ;; target index
          1 ;; single element
          ;; one running relative index
          i1)))
      ((2)
       (cond
        ((null? more) (NYI "call-in-range one index only"))
        ((pair? (cdr more))
         (error "wrong number of indices: range requires" (range-rank range) 'call-in-range))
        (else
         (let ((i2 (car more)))
           (let ((element-size 1)
                 (storage-offset 0) ;; TBD
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
             (proc
              (fx+ storage-offset (fx+ row0 (fx* offset element-size)))
              1 ;; single element
              ;; STILL TBD: just "scalar" result so far
               ;; two runnig relative indices
              i1 i2))))))
      (else (NYI "generic case" %%MATURITY+4:call-in-range range))))
  (define (%%range-rank obj)
    (cond
     ((%%range-structure? obj) (let ((x (macro-range-dim obj))) (if (fx>= x 0) x (fx- 0 x))))
     ((%%fixnum-range/assert? obj) 1)
     (else (error "not a range" 'range-rank obj))))
  (define (%%range-row obj n)
    (cond
     ((not (%%range-structure? obj))
      (error "not a multidimensional range" 'range-row obj))
     ((not (and (number? n) (integer? n)))
      (error "invalid index" 'range-row n)))
    (let* ((d0 (macro-range-dim obj))
           (d0a (abs d0))
           (na (abs n))
           (z (macro-range-offset obj))
           (d1i (- d0a 1)))
      (when (or (< na 0) (> na d0a))
        (%%raise-range-exception 1 'range-row d0 n))
      (let* ((d1 (%%range-size obj d1i))
             (step (%%range-step obj (- d1i 1)))
             (vol (/ (%%structure-range-volume obj) d1))
             (z (+ z (* na (%%range-start obj d1i)))))
        (allocate-range (if (< d0 0) (- d1i) d1i) vol z (macro-range-in obj)))))
  (define (%%range-volume obj)
    (cond
     ((%%range-structure? obj) (%%structure-range-volume obj))
     ((%%fixnum-range/assert? obj) (abs obj))
     (else (error "not a range" 'range-volume obj))))
  (define (dimension rng step d vzs) ;; FIXME: runs too often in tight loops
    (let (;; (z0 0 #;(macro-range-offset rng)) ;; usually zero
          (dn (abs (macro-range-dim rng))))
      (when (or (< d 0) (>= d dn))
        (%%raise-range-exception 3 'range-dimension d dn rng))
      ;; (vector-ref step (fx+ (fx+ (fx* d 3) vzs) z0))
      (vector-ref step (fx+ (fx* d 3) vzs))))
  (define (%%range-size obj dim)  ;; [maybe stay only internal?]
    (cond
     ((%%range-structure? obj)
      (let ((step (%%range-in obj)))
        (cond
         ((vector? step) (dimension obj step dim 0))
         (else step))))
     ((number? obj) ;; disabled case
      (%%raise-range-exception 1 %%range-size obj)
      (unless (fx= dim 0) ;; error check
        (%%raise-range-exception 2 %%range-size dim))
      (%%range-volume obj))
     (else (error "argument error" 'range-size obj))))
  (define (%%range-start obj dim) ;; maybe only internal?
    (cond
     ((%%range-structure? obj)
      (let ((step (%%range-in obj)))
        (cond
         ((vector? step) (dimension obj step dim 1))
         (else step))))
     ((%%fixnum-range/assert? obj) 0)
     (else (error "argument error" 'range-start obj))))
  (define (%%range-step obj dim)  ;; maybe only internal?
    (cond
     ((%%range-structure? obj)
      (let ((step (%%range-in obj)))
        (cond
         ((vector? step) (dimension obj step dim 2))
         (else step))))
     ((%%fixnum-range/assert? obj) 1)
     (else (error "argument error" 'range-step obj))))
  (define (%%range-ascending? obj)
    (cond
     ((%%range-structure? obj) (positive? (macro-range-dim obj)))
     ((%%fixnum-range/assert? obj) (positive? obj))
     (else (error "argument error" 'range-ascending? obj))))

  (define (%%MATURITY+4:mdv-indexer range #!optional (storage-size #f) (storage-offset 0))
    ;; (when (rangemask? range) (NYI "%%MATURITY+4:mdv-indexer rangemask?" range))
    (set! storage-offset (+ storage-offset (macro-range-offset range)))
    (when (and storage-size
               (fx< storage-size (fx+ storage-offset (range-volume range))))
      (%%raise-range-exception 2 'mdv-indexer-select range storage-size storage-offset))
    (case (range-rank range)
      ((1)
       (lambda (index)
         (let ((limit (%%range-volume range))
               (idx (+ (fx* index (%%range-step range 0))
                       (macro-%%range-start//checks range 0))))
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
                           (upper-bound (macro-%%range-size//checks range 1)))
                       (and (number? obj) (integer? obj)
                            (fx>= obj lower-bound)
                            (fx< obj upper-bound)
                            (fx* obj (macro-%%range-start//checks range 1)))))
                    (%%raise-range-exception 1 'mdv-indexer i1)))
               (offset
                (or (and
                     (let ((obj i2)
                           (lower-bound 0)
                           (upper-bound (macro-%%range-size//checks range 0)))
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
  (set! range? %%range?) ;; FIXME: bad idea, performance wise
  (set! make-range %%make-range)
  (set! range make-range)
  (set! call-in-range %%MATURITY+4:call-in-range)
  (set! range-volume %%range-volume)
  (set! range-size %%range-size)
  (set! range-start %%range-start) ;; maybe only internal?
  (set! range-step %%range-step)  ;; maybe only internal?
  (set! range-ascending? %%range-ascending?) ;; obsolete?
  (set! range-row %%range-row)
  (set! mdv-ref/getter %%MATURITY+4:mdv-ref/getter)
  (set! mdv-ref %%MATURITY+4:mdv-ref)
  (set! mdv-indexer %%MATURITY+4:mdv-indexer)
  ) ;; end --- range refinement ---

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

(define-structure mdvector
  body    ;; vector, homogenous vector or vector-alike object
  offset  ;; non negative fixnum - offset in body
  range   ;; dimension mask, iota vector
  special ;; indicator if body is not a generic Scheme vector
  ) ;; mdvector

(set! allocate-mdvector make-mdvector)

(set!
 make-mdvector
 (lambda (?range
          body
          #!optional
          (special #f)
          (storage-size
           (cond
            ((vector? body) (vector-length body))
            ((string? body) (string-length body))
            ((u8vector? body) (u8vector-length body))
            ((u16vector? body) (u16vector-length body))
            ((f32vector? body) (f32vector-length body))
            (else ("unhandled mdvector kind" make-mdvector body))))
          (storage-offset 0))
   (when (and storage-size ;; TBD: avoid calling `range-volume` - it is expensive!
              (fx< storage-size (fx+ storage-offset (range-volume ?range))))
     (##raise-range-exception 2 'make-mdvector ?range storage-size storage-offset))
   (when (number? ?range) (set! ?range (range ?range)))
   (allocate-mdvector body storage-offset ?range special)))

(define (mdvector-special? obj key)
  (and (mdvector? obj)
       (let ((s (mdvector-special obj)))
         (and (pair? s) (memq key s)))))

(define (mdvector-make-instance? key)
  (lambda (obj) (and (mdvector? obj) (eq? key (mdvector-special obj)))))

(define (mdvector/vector-ref mdv i1 . more) ;; experimental
  (let* ((rng (mdvector-range mdv))
         (idx (mdv-indexer rng)))
    (vector-ref
     (mdvector-body mdv)
     (cond
      ((null? more) (idx i1))
      (else (apply idx i1 more))))))

(define (mdvector-ref mdv i1 . more) ;; experimental
  (let* ((rng (mdvector-range mdv))
         (body (mdvector-body mdv))
         (idx (mdv-indexer rng)))
    (cond
     ((vector? body)
      (##vector-ref
       body
       (cond
        ((null? more) (idx i1))
        ((null? (cdr more)) (idx i1 (car more)))
        (else (apply idx i1 more)))))
     ((f32vector? body)
      (f32vector-ref
       body
       (cond
        ((null? more) (idx i1))
        ((null? (cdr more)) (idx i1 (car more)))
        (else (apply idx i1 more)))))
     ((u8vector? body)
      (u8vector-ref
       body
       (cond
        ((null? more) (idx i1))
        ((null? (cdr more)) (idx i1 (car more)))
        (else (apply idx i1 more)))))
     ((string? body)
      (string-ref
       body
       (cond
        ((null? more) (idx i1))
        ((null? (cdr more)) (idx i1 (car more)))
        (else (apply idx i1 more)))))
     ((u16vector? body)
      (u16vector-ref
       body
       (cond
        ((null? more) (idx i1))
        ((null? (cdr more)) (idx i1 (car more)))
        (else (apply idx i1 more)))))
     (else (error "unhandled mdvector kind" mdvector-ref)))))

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
      (do ((i (fx- -1 range) (##fx- i 1)))
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
                   (limit (##fx- length 1)))
              (do ((i limit (##fx- i 1))
                   (j (fx* limit step) (##fx- j step))
                   (r 0))
                  ((fx< i 0) r)
                (set! r (proc! i j length)))))
           (error "for-range! INTERNAL ERROR invalid range argument" range))))))
   (else
    (apply NYI 'for-range! range ranges) (apply error 'for-range! range ranges))))

(define for-range! MATURITY+2:for-range!)

(define (for-range2 rng proc)
  (let* ((limitr (range-size rng 1))
         (limitc (range-size rng 0))
         ;; optimizations
         (idx (mdv-indexer #|TBD: skip checks!|# rng)))
    (do ((r 0 (fx+ r 1)))
        ((eqv? r limitr))
      (do ((c 0 (fx+ c 1)))
          ((eqv? c limitc))
        (proc r c)))))
