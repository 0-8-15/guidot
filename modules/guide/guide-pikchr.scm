;;** Area Math

(define (pikchr-area area #!optional (select '()))
  (define (recurse area select) (pikchr-area area select))
  (define (area? x) (mdvector-interval? x))
  (define-values (xsw xno ysw yno)
    (match
     area
     ((? complex?) (values 0 (real-part area) 0 (imag-part area)))
     (#((? real? w) (? real? h)) (values 0 w 0 h))
     ;; Lacking a good reason for the sequence, we rather don't define
     ;; this yet.
     ;;; (#((? real? x0) (? real? y0) (? real? x1) (? real? y1)) (values x0 x1 y0 y1))
     (_ (guide-boundingbox->quadrupel area))))
  (define (return)
    (and
     (>= xsw 0) (> xno xsw)
     (>= ysw 0) (> yno ysw)
     (make-x0y0x1y1-interval/coerce xsw ysw xno yno)))
  (define (xof x)
    (cond
     ((complex? x) (real-part x))
     ((vector? x) (vector-ref x 0))
     (else (error "xof" x))))
  (define (yof x)
    (cond
     ((complex? x) (imag-part x))
     ((vector? x) (vector-ref x 1))
     (else (error "xof" x))))
  (define coordvec make-rectangular)
  (define (combine-expr op x y)
    (cond
     ((and (number? x) (number? y)) (op x y))
     ((and (vector? x) (vector? y))
      (coordvec
       (op (vector-ref x 0) (vector-ref y 0))
       (op (vector-ref x 1) (vector-ref y 1))))
     ((and (vector? x) (number? y)) (combine-expr op (coordvec (xof x) (yof x)) y))
     ((and (number? x) (vector? y)) (combine-expr op x (coordvec (xof y) (yof y))))
     (else (NYIE combine-expr op x y))))
  (define (numeric-key? x)
    (and (number? x) (<= 0 x 9)))
  (define (select-basic key)
    (case key
      ((0 top) yno)
      ((1 right) xno)
      ((2 bottom) ysw)
      ((3 left) xsw)
      ((4 ne) (coordvec xno yno))
      ((5 se) (coordvec xno ysw))
      ((6 sw) (coordvec xsw ysw))
      ((7 nw) (coordvec xsw yno))
      ((8 center) (coordvec (+ xsw (* 1/2 (- xno xsw))) (+ ysw (* 1/2 (- yno ysw)))))
      ((height) (- yno ysw))
      ((width) (- xno xsw))
      ((d1) (coordvec (- xno xsw) (- yno ysw)))
      ((9) (NYIE))))
  (define select-expr
    (match-lambda
     ((? numeric-key? key) (select-basic key))
     ((? symbol? key) (select-basic key))
     (otherwise (error "unhandled expression" recurse otherwise))))
  (define calc-expr
    (match-lambda
     ((? number? x) x)
     ((? area? x) x)
     (#(x y) (coordvec (calc-expr x) (calc-expr y)))
     ((? symbol? x) (select-basic x))
     (('@ key) (select-expr key))
     (('@ (? area? area) . key) (recurse area key))
     (('+ x y) (combine-expr + (calc-expr x) (calc-expr y)))
     (('- x y) (combine-expr - (calc-expr x) (calc-expr y)))
     (('fraction frac from to)
      (begin
        (unless (real? frac) (error "fraction must be a real number" pikchr-area frac))
        (let ((from (calc-expr from))
              (to (calc-expr to)))
          (+ from (* (- to from) frac)))))
     (((and (or '+ '-) operation) x y . more)
      (calc-expr `(,operation ,(combine-expr + (calc-expr x) (calc-expr y)) . ,more)))
     ((? procedure? proc) (proc))
     (((? procedure? proc) . args)
      (apply proc (map calc-expr args)))
     (otherwise (error "exception parsing calc expression" otherwise))))
  (define apply-expr!
    (match-lambda
     (('! key value)
      (let ((value (calc-expr value)))
        (case key
          ((0 top) (set! yno value))
          ((1 right) (set! xno value))
          ((2 bottom) (set! ysw value))
          ((3 left) (set! xsw value))
          ((4 ne) (set! xno (xof value)) (set! yno (yof value)))
          ((5 se) (set! xno (xof value)) (set! ysw (yof value)))
          ((6 sw) (set! xsw (xof value)) (set! ysw (yof value)))
          ((7 nw) (set! xsw (xof value)) (set! yno (yof value)))
          (else (NYIE)))))
     (('shift: delta) ;; NO calc-expr here! Absolute distance
      (let ((dx (xof delta))
            (dy (yof delta)))
        (set! yno (+ yno dy))
        (set! xno (+ xno dx))
        (set! ysw (+ ysw dy))
        (set! xsw (+ xsw dx))))
     (('move: key value)
      (let ((value (calc-expr value))
            (current (select-basic key)))
        (let ((shift
               (case key
                 ((top bottom 0 2) (coordvec 0 (- value current)))
                 (else
                  (coordvec (- (xof value) (xof current)) (- (yof value) (yof current)))))))
          (apply-expr! `(shift: ,shift)))))
     (('intersect: value)
      (receive (xsw1 xne1 ysw1 yne1) (guide-boundingbox->quadrupel (calc-expr value))
        (set! yno (min yno yne1))
        (set! xno (min xno xne1))
        (set! ysw (max ysw ysw1))
        (set! xsw (max xsw xsw1))))
     (('padding: value)
      (let ((padding (%%guide:parse-padding pikchr-area (return) value)))
        (set! yno (- yno (vector-ref padding 0)))
        (set! xno (- xno (vector-ref padding 1)))
        (set! ysw (+ ysw (vector-ref padding 2)))
        (set! xsw (+ xsw (vector-ref padding 3)))))
     (otherwise (error "unhandled expression" recurse otherwise))))
  (define continue
    (match-lambda
     (() (return))
     ((('@ expr)) (select-expr expr))
     (((? symbol? x)) (select-basic x))
     (((and (cmd . args) expr) . more) ;; fully parentized expression
      (begin
        (apply-expr! expr)
        (continue more)))
     ((? symbol? x) (select-basic x))
     (((and (or 'shift: 'intersect: 'padding:) cmd) ;; 1ari commands
       v . more)
      (let ((cmd cmd))
        (apply-expr! `(,cmd ,v))
        (continue more)))
     (((and (or 'set! 'move:) (not '!) cmd) ;; 2ari commands (excluding '! for fun)
       k v . more)
      (let ((cmd (if (eq? cmd 'set!) '! cmd))) ;; translating the fun
        (apply-expr! `(,cmd ,k ,v))
        (continue more)))
     (('@ expr) (calc-expr expr))
     (expr (calc-expr expr))
     #;(otherwise (error "invalid area spec" recurse otherwise))))
  (continue select))

#|
(test-assert
 "pikchr-area simple return"
 (let ((area (make-mdv-rect-interval 0 1 3 5)))
   (equal? (pikchr-area area) area)))

(test-assert
 "pikchr-area alternative initial size"
 (let ((area (make-mdv-rect-interval 0 1 3 5)))
   (equal? (pikchr-area 3+4i '(shift: 0+i)) area)))

;;; (test-assert
;;;  "pikchr-area alternative initial size is 4-vector"
;;;  (let ((area (make-mdv-rect-interval 0 1 3 5)))
;;;    (equal? (pikchr-area '#(0 1 3 5)) area)))

(test-assert
 "pikchr-area simple select"
 (let ((area (make-mdv-rect-interval 0 1 3 5)))
   (equal? (pikchr-area area '((@ 0))) 5)))

(test-assert
 "pikchr-area simple select symbolic"
 (let ((area (make-mdv-rect-interval 0 1 3 5)))
   (equal? (pikchr-area area 'top) 5)))

(test-assert
 "pikchr-area fraction"
 (let ((area (make-mdv-rect-interval 1 1 3 5)))
   (equal? (pikchr-area area '(set! ne (fraction 1/2 sw ne))) (make-mdv-rect-interval 1 1 2 3))))

(test-assert
 "pikchr-area calculated select"
 (let* ((area (make-mdv-rect-interval 0 1 70 500))
        (content-area (make-mdv-rect-interval 10 11 60 490))
        (expected (- (pikchr-area area '(top)) (pikchr-area content-area '(top)))))
   (equal? (pikchr-area area `(- top (@ ,content-area top))) expected)))

(test-assert
 "pikchr-area simple assignment"
 (let ((area (make-mdv-rect-interval 0 1 3 5))
       (expected (make-mdv-rect-interval 0 1 3 7)))
   (equal? (pikchr-area area '(set! top 7)) expected)))

(test-assert
 "pikchr-area calculated assignment"
 (let ((area (make-mdv-rect-interval 0 1 3 5))
       (expected (make-mdv-rect-interval 0 1 3 7)))
   (equal? (pikchr-area area '((! top (+ (@ 0) 2)))) expected)))

(test-assert
 "pikchr-area center move"
 (let ((area (make-mdv-rect-interval 0 1 3 5))
       (expected (make-mdv-rect-interval 2 2 5 6)))
   (equal? (pikchr-area area '((move: center (+ center #(2 1))))) expected)))

(test-assert
 "pikchr-area center move twice"
 (let ((area (make-mdv-rect-interval 0 1 3 5))
       (expected (make-mdv-rect-interval 3 2 6 6)))
   (equal? (pikchr-area area '(move: center (+ center #(2 1)) move: center (+ center #(1 0)))) expected)))

(test-assert
 "pikchr-area move to fixed"
 (let ((area (make-mdv-rect-interval 0 1 3 5))
       (a2 (make-mdv-rect-interval 0 1 3 4))
       (expected (make-mdv-rect-interval 0 0 3 4)))
   (equal? (pikchr-area area `(move: nw (@ ,a2 nw))) expected)))

(test-assert
 "pikchr-area padding"
 (let ((area (make-mdv-rect-interval 0 0 8 10))
       (expected (make-mdv-rect-interval 1 2 7 8)))
   (equal? (pikchr-area area `(padding: #(2 1))) expected)))

;;;|# ;; end of pikchr tests
