(cond-expand
 (debug (define-macro (MATURITY l m . a) `(maturity-note ,l ,m ,@a)))
 (gambit ;; production
  (define-macro (MATURITY l m . a) #!void))
 (else ;; interactive load
  (define-macro (MATURITY l m . a) `(maturity-note ,l ,m ,@a))))

(utf8string->unicode:on-encoding-error 'replace)

(define (utf8string->u32vector* str)
  ;; convert UTF8 to unicode u32vector
  (define (convert str)
    (let* ((o0 0)
           (sl (string-length str))
           (ie (+ o0 sl))
           (rng (range ie))
           (a0 (make-u32vector ie))
           (a0i (mdv-indexer rng ie)))
      ;; TBD: optimize initialization, for now we use the pre-tested
      ;; code as is, no matter how expesinve the overhead is.
      (do ((o o0 (fx+ o 1))
           (codepoints
            (begin
              (MATURITY -2 "overly expensive: utf8string->unicode begin" loc: utf8string->u32vector*)
              (let ((result (utf8string->unicode str)))
                (MATURITY -2 "overly expensive: utf8string->unicode end" loc: utf8string->u32vector*)
                result))
            (cdr codepoints)))
          ((null? codepoints)
           (if (fx= ie o) a0 (subu32vector a0 o0 o)))
        (u32vector-set! a0 (a0i o) (car codepoints)))))
  (if (fx> (string-length str) 0)
      (convert str)
      '#u32()))

(define utf8string->u32vector
  (let ((cache (make-table test: string=? weak-keys: #t)))
    (lambda (str)
      (or (table-ref cache str #f)
          (let ((result (utf8string->u32vector* str)))
            (table-set! cache str result)
            result)))))

;;;** srfi-179 compat

(define mdvector-interval?)
(define make-mdv-rect-interval)
(define make-mdvector-interval
  ;; FIXME: make it a typed pair of an offset vector and a range!
  (let ((tag '(exact positive))
        (d1 (range '#(2 1)))
        (d2 (range '#(2 2)))
        (d3 (range '#(2 3)))
        (d4 (range '#(2 4))))
    (set! mdvector-interval? (mdvector-make-instance? tag))
    (set! make-mdv-rect-interval
          (lambda (l1 l2 u1 u2)
            (define (valid? arg) (and (number? arg) (fixnum? arg) (not (negative? arg))))
            (unless (and (valid? l1) (valid? u1) (fx> u1 l1)
                         (valid? l2) (valid? u2) (fx> u2 l2))
              (error "invalid arguments" make-mdv-rect-interval l1 l2 u1 u2))
            (make-mdvector d2 (vector l1 l2 u1 u2) tag)))
    (lambda (dimensions . inits)
      ;; TBD: support alternative API compatible with srfi-179 for the sake of copatibility
      (define (valid? arg) (and (number? arg) (integer? arg) (not (negative? arg))))
      (unless (and (valid? dimensions) (positive? dimensions))
        (##raise-range-exception 1 'make-mdvector-interval dimensions))
      (make-mdvector
       (case dimensions
         ((4) d4)
         ((2) d2)
         ((3) d3)
         ((1) d1)
         (else (range (vector 2 dimensions))))
       (let* ((vec (apply vector inits))
              (len (vector-length vec)))
         (unless (even? len)
           (error "odd argument count" make-mdvector-interval inits))
         (unless (eqv? len (fx* 2 dimensions))
           (error "wrong argument count, requires"
                  make-mdvector-interval (fx+ (fx* dimensions 2) 1)))
         (do ((i 0 (fx+ i 2))
              (j dimensions (fx+ j 2)))
             ((eqv? j len) vec) ;; all checked
           (let ((lower (##vector-ref vec i))
                 (upper (##vector-ref vec (fx+ j 1))))
             (unless (valid? lower)
               (##raise-range-exception i 'make-mdvector-interval lower))
             (unless (valid? upper)
               (##raise-range-exception j 'make-mdvector-interval upper))
             (unless (fx> upper lower)
               (error "upper bound not larger than lower bound"
                      make-mdvector-interval lower upper)))))
       tag))))

(define (make-x0y0x1y1-interval/coerce x0 y0 x1 y1)
  (define (convl x)
    (cond
     ((integer? x) x)
     (else (inexact->exact (floor x)))))
  (define (convu x)
    (cond
     ((integer? x) x)
     (else (inexact->exact (ceiling x)))))
  (make-mdv-rect-interval (convl x0) (convl y0) (convu x1) (convu y1)))

(define (mdvector-interval-lower-bound interval dim)
  (unless (mdvector-interval? interval)
    (error "illegal argument" mdvector-interval-lower-bound interval))
  (mdv-ref (mdvector-body interval) (mdvector-range interval) 0 dim))

(define (mdvector-interval-upper-bound interval dim)
  (unless (mdvector-interval? interval)
    (error "illegal argument" mdvector-interval-upper-bound interval))
  (mdv-ref (mdvector-body interval) (mdvector-range interval) 1 dim))

(define (mdvector-interval-intersect left right)
  (unless (and (mdvector-interval? left) (mdvector-interval? right))
    (error "illegal argument" mdvector-interval-intersect left right))
  (let ((vl (mdvector-body left))
        (vr (mdvector-body right)))
    (let ((len (vector-length vl)))
      (unless (eqv? len (vector-length vr))
        (error "intervals differ in rank" mdvector-interval-intersect left right))
      (let ((intersection (make-vector len))
            (invalid #f)
            (limit (quotient len 2)))
        (do ((i 0 (fx+ i 1))
             (j limit (fx+ j 1)))
            ((or (eqv? i limit) invalid))
          (let ((ll (vector-ref vl i))
                (rl (vector-ref vr i))
                (lu (vector-ref vl j))
                (ru (vector-ref vr j)))
            (let ((il (max ll rl))
                  (iu (min lu ru)))
              (if (fx< il iu)
                  (begin
                    (vector-set! intersection i il)
                    (vector-set! intersection j iu))
                  (set! invalid #t)))))
        (cond
         (invalid #f)
         ((equal? intersection vl) left)
         ((equal? intersection vr) right)
         (else
          (make-mdvector (mdvector-range left) intersection (mdvector-special left))))))))

(define (mdvector-rect-interval-contains/xy? interval x y)
  (unless (mdvector-interval? interval)
    (error "illegal argument" mdvector-rect-interval-contains/xy? interval))
  (and (>= x (mdvector-ref interval 0 0))
       (< x (mdvector-ref interval 1 0))
       (>= y (mdvector-ref interval 0 1))
       (< y (mdvector-ref interval 1 1))))

;;;**

(define (for-range-index! fixnum-range proc!) ;; FIXME: belongs to repetition! (tests)
  (unless (and (number? fixnum-range) (integer? fixnum-range) (positive? fixnum-range))
    (error "wrong range" for-range-index fixnum-range))
  (unless (procedure? proc!) (error "not a procedure" for-range-index proc!))
  (do ((i 0 (fx+ i 1)))
      ((eqv? i fixnum-range))
    (proc! i)))

(define range-2x2 (range 2 2))

(define vertex-rect-range2d (range 4 2))
(define vertex-rect-range2d-indexer (mdv-indexer vertex-rect-range2d))

(define %%make-mdvector-rect-vertices
  (let ((tag '(f32)))
    ;; FIXME: offset not yet handled, remove it?
    (define (%%make-mdvector-rect-vertices #!optional (rng #f) (storage #f) (offset 0))
      (if rng
          (begin
            (unless (or (eq? rng vertex-rect-range2d)
                        (and (eqv? (range-rank rng) 2)
                             (eqv? (range-size rng 0) 2)))
              (error "range not 4x2" 'make-mdvector-rect-vertices rng))
            (when storage
              (unless (>= (f32vector-length storage) (+ (range-volume rng) offset))
                (error "storage too small to hande range" make-mdvector-rect-vertices))))
          (set! rng vertex-rect-range2d))
      (let ((vol (range-volume rng)))
        (unless (f32vector? storage)
          (set! storage (make-f32vector vol)))
      (make-mdvector rng storage tag vol)))
    %%make-mdvector-rect-vertices))

(define (guide-figure? obj)
  (and (mdvector-special? obj 'f32)
       (let ((rng (mdvector-range obj)))
         (and (eqv? (range-rank rng) 2) (eqv? (range-size rng 0) 2)))))

(define glC:mdv-vertex-area? guide-figure?)

(define (make-guide-area/x0y0x1y1 x0 y0 x1 y1)
  (make-mdvector
   range-2x2
   (f32vector
    (exact->inexact x0) (exact->inexact y0)
    (exact->inexact x1) (exact->inexact y1))
   '(f32)
   4))

(define (make-mdvector-rect-vertices/x0y0x1y1 x0 y0 x1 y1)
  ;;; y
  ;;; 1 +---+
  ;;;   |   |
  ;;; 0 +---+
  ;;; x 0   1
  (let ((x0 (exact->inexact x0)) (y0 (exact->inexact y0))
        (x1 (exact->inexact x1)) (y1 (exact->inexact y1)))
    (%%make-mdvector-rect-vertices
     #f
     (f32vector
      x0 y1
      x1 y1
      x0 y0
      x1 y0))))

(define (make-mdvector-rect-vertices/mdvector-interval interval) ;; x0y0x1y1
  (define (convert v t) (exact->inexact (mdvector-ref interval v t)))
  ;;; y
  ;;; 1 +---+
  ;;;   |   |
  ;;; 0 +---+
  ;;; x 0   1
  (unless (mdvector-interval? interval)
    (error "invalid argument" interval))
  (let ((x0 (convert 0 0)) (y0 (convert 0 1))
        (x1 (convert 1 0)) (y1 (convert 1 1)))
    (%%make-mdvector-rect-vertices
     #f
     (f32vector
      x0 y1
      x1 y1
      x0 y0
      x1 y0))))

(define %%make-mdvector-color
  (let ((tag '(color u8 #(4))))
    (define (%%make-mdvector-color rng storage)
      (make-mdvector rng storage tag (range-volume rng)))
    %%make-mdvector-color))

(define %%make-single-color-mdv/1
  (let ((range-rgba/1 (range 4))
        (range-rgba/2 (range 2 4))
        (range-rgba/3 (range 3 4))
        (range-rgba/4 (range 4 4)))
    (define (make-single-color-mdv/1 size color)
      (let* ((rng (case size
                    ((4) range-rgba/4)
                    ((2) range-rgba/2)
                    ((3) range-rgba/3)
                    ((1) range-rgba/1)
                    (else (range (vector 4 size)))))
             (storage (make-u8vector (range-volume rng))))
        ;; (for-range-index! size (lambda (i) (u8vector/32-set! storage (fx* i 4) color)))
        (do ((i (fx* 4 (fx- size 1)) (fx- i 4)))
            ((eqv? i -4) (%%make-mdvector-color rng storage))
          (u8vector/32-set! storage i color))))
    make-single-color-mdv/1))

(define (make-rect-single-color-array color) ;; 4 sides each a color
  (%%make-single-color-mdv/1 4 color))

(define guide-color-transparent+black-array
  (make-rect-single-color-array (color-rgba 0 0 0 255)))

;;;

(define guide-texcoords?)
(define %%make-mdvector-texcoords)
(let ((tag '(f32 texcoords)))
  (set! guide-texcoords?
        (lambda (obj) (and (mdvector? obj) (eq? (mdvector-special obj) tag))))
  (set! %%make-mdvector-texcoords
        (lambda (rng storage)
          ;; safe&slow: (make-mdvector rng storage tag (range-volume rng))
          (let ((storage-offset 0))
            (allocate-mdvector storage storage-offset rng tag)))))

(define (make-legacy-texcoords x0 y0 x1 y1)
  (%%make-mdvector-texcoords
   vertex-rect-range2d
   (f32vector
    x0 y1
    x1 y1
    x0 y0
    x1 y0)))

(define legacy-rect-full-texcoords (make-legacy-texcoords 0.1 0.1 0.9 0.9))

(define rect-full-texcoords (make-legacy-texcoords 0. 0. 1. 1.))

(define (cliptexcoords tex area clip)
  (define (clipproc full-volume texture-volume clip-offset texture-origin)
    (+ (if (= full-volume 0) ;; avoid device by zero
           0
           (*
            (/ clip-offset full-volume)
            texture-volume))
       texture-origin))
  (unless (guide-texcoords? tex) (error "illegal parameter" cliptexcoords 1 tex))
  (unless (mdvector-interval? area) (error "illegal parameter" cliptexcoords 2 area))
  (unless (mdvector-interval? clip) (error "illegal parameter" cliptexcoords 3 clip))
  ;; Canonical implementation. Rarely run, left as example.
  ;;
  ;; TBD: known to be 4 corners, maybe inline.
  (let* ((tsv (mdvector-body tex))
         (trv (make-f32vector (f32vector-length tsv)))
         (texrng (mdvector-range tex))
         (texidx (mdv-indexer texrng)))
    (do ((i (fx- (range-volume texrng) 1) (fx- i 2))
         (c (quotient (fx- (range-volume texrng) 1) 2) (fx- c 1)))
        ((fx< i 0) (%%make-mdvector-texcoords texrng trv))
      (let* ((v (modulo c 2))
             ;; outer
             (full-volume (fx- (mdvector-ref area v 1) (mdvector-ref area v 0)))
             (texture-volume (fl- (f32vector-ref tsv (texidx v 1)) (f32vector-ref tsv (texidx v 0)))))
        (let* ((bound 0)
               (i (fx- i 1))
               (clip-offset (fx- (mdvector-ref clip v bound) (mdvector-ref area v bound)))
               (texture-origin (f32vector-ref tsv i)))
          (f32vector-set! trv i (clipproc full-volume texture-volume clip-offset texture-origin)))
        (let* ((bound 1)
               (clip-offset (fx- (mdvector-ref clip v bound) (mdvector-ref area v bound)))
               (texture-origin (f32vector-ref tsv i)))
          (f32vector-set! trv i (clipproc full-volume texture-volume clip-offset texture-origin)))))))

;;;**

(define-structure guide-image
  texture  ;; fixnum, TBD cache-ref
  interval ;; the interval describing the texture source
  area     ;; the result vertex(?) array
  texcoord ;; texture coordinates
  colors)  ;; coloring

(define make-guide-image/checks
  (let ((orig make-guide-image))
    (define (texture? x) (or (glCore:texture? x) (fixnum? x)))
    (lambda (texture interval area texcoord colors)
      (cond
       ;; TBD: add all required checks here
       ((not (and (texture? texture)))
        (error "guide-image: invalid texture" texture))
       ((not (mdvector-interval? interval)))
       ((not (glC:mdv-vertex-area? area))
        (error "guide-image: invalid area" area))
       ((not (guide-texcoords? texcoord))
        (error "guide-image: invalid texcoords" texcoord))
       (else (orig texture interval area texcoord colors))))))

(set! make-guide-image make-guide-image/checks) ;; cond-expand on debug

(define make-guide-image* make-guide-image)

(define (guide-image-height img)
  ;; TBD: horrible overhead here
  (let ((interval (guide-image-interval img)))
    (fx- (mdvector-interval-upper-bound interval 1) (mdvector-interval-lower-bound interval 1))))

(define (guide-image-width img)
  ;; TBD: horrible overhead here
  (let ((interval (guide-image-interval img)))
    (fx- (mdvector-interval-upper-bound interval 0) (mdvector-interval-lower-bound interval 0))))

(define (with-guide-image-area img proc)
  (let* ((a1 (guide-image-area img))
         (a2 (proc a1)))
    (if (eq? a1 a2) ;; TBD: better predicate
        img
        (make-guide-image
         (guide-image-texture img) (guide-image-interval img)
         a2 (guide-image-texcoord img)
         (guide-image-colors img)))))

(define (with-guide-image-texcoord img proc)
  (let* ((a1 (guide-image-texcoord img))
         (a2 (proc a1)))
    (if (eq? a1 a2) ;; TBD better predicate
        img
        (make-guide-image
         (guide-image-texture img)
         (guide-image-interval img)
         (guide-image-area img)
         a2
         (guide-image-colors img)))))

(define (with-guide-image-colors img proc)
  (let* ((a1 (guide-image-texcoord img))
         (a2 (proc a1)))
    (if (eq? a1 a2) ;; TBD better predicate
        img
        (make-guide-image
         (guide-image-texture img)
         (guide-image-interval img)
         (guide-image-area img)
         (guide-image-texcoord img)
         a2))))

(define-structure glC:vertex-set v t c d23 volume)

(define glC:legacy-vertex-set-2d (make-glC:vertex-set glCore:varray glCore:tarray glCore:carray 2 glCore:MAX))
(define glC:legacy-vertex-set-3d (make-glC:vertex-set glCore:varray3D glCore:tarray glCore:carray 3 glCore:MAX))

(define (create-glC:vertex-set d23 size)
  (let ((v (make-f32vector (* d23 size)))
        (t (make-f32vector (* 2 size)))
        (c (make-u8vector (* 4 size))))
    (make-glC:vertex-set v t c d23 size)))

(define (dev#glC:vertex-set-vertex2d-set! target idx x y tx ty color) ;; deprecated
  ;;; NOTE: unused, deprecated, manual debug helper
  (let ((varr (glC:vertex-set-v target))
        (tarr (glC:vertex-set-t target))
        (cidx idx)
        (idx (fx* idx 2)))
    (f32vector-set! varr (fx+ idx 0) x)
    (f32vector-set! varr (fx+ idx 1) y)
    (f32vector-set! tarr (fx+ idx 0) tx)
    (f32vector-set! tarr (fx+ idx 1) ty)
    (let ((idx (fx* idx 2))
          (color
           (cond
            ((vector? color) (vector-ref color cidx))
            (else color)))
          (arr (glC:vertex-set-c target)))
      (u8vector/32-set! arr idx color))))

(define (glC:vertex-set-set! target idx vertices texcoords colors)
  ;; TBD: error check: vertices texcoords colors should supply the
  ;; be (range X 2) and have alls the same X.
  (let* ((varr (glC:vertex-set-v target))
         (tarr (glC:vertex-set-t target))
         (carr (glC:vertex-set-c target)))
    ;; colors
    (let ((colors-as-u8 (mdvector-body colors))
          (rng (mdvector-range colors))
          (dst carr))
      (unless (mdvector-special? colors 'color)
        (error "illegal argument" colors))
      (unless (eqv? (range-rank rng) 2)
        (error "colors must be n*4 u8vectors" colors))
      ;; FIXME: make sure this repeats colors over range!
      (let ((src-start 0)
            (src-end  ;; maybe we should have access to some offset here?
             (min (- (##u8vector-length dst) idx) (range-volume rng)))
            (dst-start (fx* idx 4)))
        (subu8vector-move! colors-as-u8 src-start src-end dst dst-start)))
    ;; texcoords
    (let ((src (mdvector-body texcoords))
          (rng (mdvector-range texcoords))
          (dst tarr))
      (let ((src-start 0)
            (src-end  ;; maybe we should have access to some offset here?
             (min (- (f32vector-length dst) idx) (range-volume rng)))
            (dst-start idx))
        (subf32vector-move! src src-start src-end dst dst-start)))
    ;; vertices
    (let ((src (mdvector-body vertices))
          (rng (mdvector-range vertices))
          (dst varr))
      (let ((src-start 0)
            (src-end  ;; maybe we should have access to some offset here?
             (min (- (f32vector-length dst) idx) (range-volume rng)))
            (dst-start idx))
        (subf32vector-move! src src-start src-end dst dst-start)))
    ;; finally
    ;; cone: drag that index
    (set! idx (fx+ (range-volume (mdvector-range vertices)) idx))
    (glC:vertex-set-volume-set! target idx) ;; sure?
    idx))

(define (glC:fill-rect-full-vertex-set! target idx interval color)
  (let* ((vertices (make-mdvector-rect-vertices/mdvector-interval interval))
         (texcoords rect-full-texcoords)
         (colors (make-rect-single-color-array color)))
    (glC:vertex-set-set! target idx vertices texcoords colors)
    target))

(define (glC:make-rect-full-vertex-set interval color)
  ;; arrange the rectangle with SW edge as (0, 0)
  #;(let ((target (create-glC:vertex-set 2 4)))
    (glC:fill-rect-full-vertex-set! target 0 interval color)
    target)
  (make-glC:vertex-set
   (mdvector-body (make-mdvector-rect-vertices/mdvector-interval interval))
   (mdvector-body rect-full-texcoords)
   (mdvector-body (make-rect-single-color-array color))
   2 4))

(define (glC:glCoreEnd glc-vertexset line-type)
  (glVertexPointer (glC:vertex-set-d23 glc-vertexset) GL_FLOAT 0 (glC:vertex-set-v glc-vertexset))
  (glColorPointer 4 GL_UNSIGNED_BYTE 0 (glC:vertex-set-c glc-vertexset))
  (cond
   ((or (eqv? line-type GL_LINES) (eqv? line-type GL_LINE_LOOP) (eqv? line-type GL_LINE_STRIP))
    (glDisable GL_TEXTURE_2D)
    (glDisableClientState GL_TEXTURE_COORD_ARRAY))
   (else
    (glEnable GL_TEXTURE_2D)
    (glEnableClientState GL_TEXTURE_COORD_ARRAY)
    (glTexCoordPointer 2 GL_FLOAT 0 (glC:vertex-set-t glc-vertexset))))
  (glDrawArrays line-type 0 (glC:vertex-set-volume glc-vertexset)))

(define (glC:TextureDrawGlArrays texture vertices scale shft rot)
  (when (or scale shft rot)
    (glPushMatrix))
  (cond
   ((f32vector? scale)
    (glScalef//checks (f32vector-ref scale 0) (f32vector-ref scale 1) (f32vector-ref scale 2)))
   ((procedure? shft) (shft)))
  (cond
   ((f32vector? shft)
    (glTranslatef//checks (f32vector-ref shft 0) (f32vector-ref shft 1) (f32vector-ref shft 2)))
   ((procedure? shft) (shft)))
  (cond
   ((f32vector? rot)
    (glRotatef//checks (f32vector-ref rot 0) (f32vector-ref rot 1)  (f32vector-ref rot 2)  (f32vector-ref rot 3)))
   ((procedure? rot) (rot)))
  (when texture (_glCoreTextureBind texture))
  ;; (glCoreBegin)
  (glC:glCoreEnd vertices GL_TRIANGLE_STRIP)
  (when (or scale shft rot)
    (glPopMatrix))
  #!void)

;;;** Clipping

(define glC:clip-int
  ;; TBD: for the time being, should look at lnglgui's clip too.
  (let ((i (delay
             ;; note: NOT inlinung (current-guide-gui-interval) as
             ;; this one is supposed to be pushed to lower level and
             ;; the other one expected to be changed.
             (make-mdv-rect-interval 0 0 (glgui-width-get) (glgui-height-get)))))
    (case-lambda
     (() (force i))
     ((v) (unless (or (mdvector-interval? v) (not v)) (error "not an mdvector-interval or #f" v)) (set! i v))
     ((x0 y0 x1 y1) (set! i (make-mdv-rect-interval x0 y0 x1 y1))))))

(define glC:clip-mode ;; deprecated, for debugging.
;; TBD: remove glC:clip-mode when done debugging
  (let ((v #t))
    (case-lambda
     (()
      ;; (MATURITY -1 "deprecated" loc: glC:clip-mode)
      v)
     ((x)
      ;; (MATURITY -1 "deprecated" loc: glC:clip-mode)
      (set! v (and x #t))))))

(define (glC:clip interval #!optional (clipping #f)) ;; experimental
  (cond
   (clipping (mdvector-interval-intersect interval clipping))
   (else
    (if (glC:clip-mode)
        (let ((clipping (glC:clip-int)))
          (if clipping (mdvector-interval-intersect interval clipping) interval))
        interval))))

;;;** Pixmaps

;;;*** Legacy Drop In

;;; "private" for parsing legacy code
(define-structure glC:image w h t xsw ysw xno yno)

(define make-glC:image/checks
  (let ((orig make-glC:image))
    ;; for debugging
    (lambda (w h t xsw ysw xno yno)
      (when (fixnum? t)
        (MATURITY -1 "outdated call, replacing texture lookup for now" loc: make-glC:image/checks t)
        (set! t (glCore:textures-ref t identity)))
      (when (maturity-tolerated? 1) ;; experimental checks
        (unless (fixnum? w) (error "glC:image: w" w))
        (unless (fixnum? h) (error "glC:image: h" h))
        (unless (or (fixnum? t) (glCore:texture? t)) (error "glC:image: t" t))
        (unless (inexact? xsw) (error "glC:image: xsw" xsw))
        (unless (inexact? ysw) (error "glC:image: ysw" ysw))
        (unless (inexact? xno) (error "glC:image: xno" xno))
        (unless (inexact? yno) (error "glC:image: yno" yno)))
      (orig w h t xsw ysw xno yno))))

;;(set! make-glC:image make-glC:image/checks)

(define (glC:image-texture-has-volume? img) ;; unused?
  (fx> (* (glC:image-w img) (glC:image-h img)) 0))

(define (glC:image-legacy-texcoords img)
  (let ((@x1 (glC:image-xsw img))
        (@y1 (glC:image-ysw img))
        (@x2 (glC:image-xno img))
        (@y2 (glC:image-yno img)))
    (make-legacy-texcoords @x1 @y1 @x2 @y2)))

(define (glC:image-clip-texcoords img clip)
  (let ((@x1 (glC:image-xsw img))
        (@y1 (glC:image-ysw img))
        (@x2 (glC:image-xno img))
        (@y2 (glC:image-yno img)))
    (cliptexcoords
     (make-legacy-texcoords @x1 @y1 @x2 @y2)
     (make-mdv-rect-interval 0 0 (glC:image-w img) (glC:image-h img))
     clip)))

(define (glC:image->guide-image img #!optional (position #f) (y0 0))
  (let* ((interval
          (cond
           ((not position)
            (make-mdv-rect-interval 0 0 (glC:image-w img) (glC:image-h img)))
           ((number? position)
            (let ((x (round (inexact->exact position)))
                  (y (round (inexact->exact y0))))
              (make-mdv-rect-interval x y (fx+ x (glC:image-w img)) (fx+ y (glC:image-h img)))))
           ((mdvector-interval? position) position)
           (else (error "glC:image->guide-image: illegal position" position))))
         (vertices (make-mdvector-rect-vertices/mdvector-interval interval))
         (colors #f)
         (texture
          (let ((t (glC:image-t img)))
            (if (glCore:texture? t) t (glCore:textures-ref t identity)))))
    (make-guide-image
     texture
     interval
     vertices
     (glC:image-legacy-texcoords img)
     colors)))

(define (MATURITY+2:glC:ImageTextureDrawInto! target idx img w h clip-int colors x y a) ;; outdating
  ;; TBD: update arglist
  (define (color-conv color)
    (cond ;; FIXME define & use consistent conversion
     ((not color) guide-color-transparent+black-array)
     ((integer? color) (make-rect-single-color-array color))
     (else colors)))
  (if (and (fx> w 0) (fx> h 0))
      (let* ((drawinterval0 (make-mdv-rect-interval 0 0 w h))
             #;(positioned
              (and clip-int
                   (let ((img-interval
                         (make-vector-dimension (vector (glC:image-w img) (glC:image-h img))))
                         (xfi (inexact->exact (round x)))
                         (yfi (inexact->exact (round y))))
                     (interval-translate img-interval (vector xfi yfi))
                     (make-mdvector-interval 2 xfi (fx+ xfi w) yfi (fx+ yfi h)))))
             (drawinterval (if clip-int (glC:clip drawinterval0 clip-int) drawinterval0)))
        (if drawinterval
            (let ((texture (glC:image-t img))
                  (scale #f)
                  (shift (f32vector (exact->inexact x) (exact->inexact y) 0.)))
              (unless (mdvector-interval? drawinterval)
                (MATURITY -10 "unless (mdvector-interval? drawinterval)" loc: 'glC:ImageTextureDrawInto!))
              (let ((end (let ((texcoords
                                (if (eq? drawinterval drawinterval0)
                                    (glC:image-legacy-texcoords img)
                                    (glC:image-clip-texcoords img drawinterval)))
                               (vertices (make-mdvector-rect-vertices/mdvector-interval drawinterval))
                               (colors (color-conv colors)))
                           (glC:vertex-set-set! target idx vertices texcoords colors)))
                    (rot (and (not (= a 0)) (f32vector (exact->inexact a) 0. 0. 1.))))
                (when (<= end idx) (error "no"))
                (values end texture target scale shift rot)))
            (values idx #f #f #f #f #f)))
      (values idx #f #f #f #f #f)))

(define glC:ImageTextureDrawInto! MATURITY+2:glC:ImageTextureDrawInto!)

(define (glcore:cliplist-top->interval)
  (define (conv x) (inexact->exact (round x)))
  (let ((lst glcore:cliplist))
    (and (pair? lst)
         (receive (x0 y0 x1 y1) (apply values (car lst))
           (let ((w (- x1 x0))
                 (h (- y1 y0)))
             (and (> w 0) (> h 0) (make-mdv-rect-interval 0 0 (conv w) (conv h))))))))

(define (MATURITY-2:glC:ImageTextureDraw! img w h colors x y a)
  (let ((target glC:legacy-vertex-set-2d)
        ;; (null? glcore:cliplist) ...
        (clip-int (or ;; (glcore:cliplist-top->interval)
                      (and (glC:clip-mode) (glC:clip-int))))
        (idx 0))
    (receive (end texture target scale shift rot)
        (glC:ImageTextureDrawInto! target idx img w h clip-int colors x y a)
      (if (> end idx) (glC:TextureDrawGlArrays texture target scale shift rot)))))

(define glC:ImageTextureDraw! MATURITY-2:glC:ImageTextureDraw!)

(define (%%glC:read-legacy-global-color-variable)
  (color-rgba glCore:red glCore:green glCore:blue glCore:alpha))

(define (glC:glCoreTextureDraw! x y w0 h0 t x1 y1 x2 y2 r #!rest colors)
  #; (apply glCoreTextureDraw x y w0 h0 t x1 y1 x2 y2 r colors)
  (define (color-conv color)
    (cond ;; FIXME define & use consistent conversion
     ((integer? color) (make-rect-single-color-array color))
     ((not color) guide-color-transparent+black-array)
     (else color)))
  (let ((entry (%%glCore:textures-ref t #f)))
    (if entry
        (let ((w (if (= w0 0) (glCore:texture-width entry) (round (inexact->exact w0))))
              (h (if (= h0 0) (glCore:texture-height entry) (round (inexact->exact h0)))))
          (let* ((interval0 (make-mdv-rect-interval 0 0 w h))
                 ;; TBD: remove glC:clip-mode when done debugging
                 (clipping (and (glC:clip-mode) (glcore:cliplist-top->interval)))
                 (interval (if clipping
                               (mdvector-interval-intersect interval0 clipping)
                               interval0)))
            (if interval
                (let* ((colors (color-conv
                                (if (pair? colors)
                                    (apply vector (debug 'COLORS!!! colors))
                                    (%%glC:read-legacy-global-color-variable))))
                       (vertices (make-mdvector-rect-vertices/mdvector-interval interval))
                       (texcoords
                        (let ((t0 (let ((x1 (exact->inexact x1))
                                        (y1 (exact->inexact y1))
                                        (x2 (exact->inexact x2))
                                        (y2 (exact->inexact y2)))
                                    (make-legacy-texcoords x1 y1 x2 y2))))
                          (if (eq? interval interval0)
                              t0
                              (let ((result (cliptexcoords t0 interval0 interval)))
                                result))))
                       (target (make-glC:vertex-set
                                (mdvector-body vertices)
                                (mdvector-body texcoords)
                                (mdvector-body colors)
                                2 4))
                       (texture t)
                       (shift (f32vector (exact->inexact x) (exact->inexact y) 0.))
                       (scale #f)
                       (rot #f))
                  (glC:TextureDrawGlArrays texture target scale shift rot)))))
        (log-error "glCoreTextureDraw: unbound index " t))))

(define $glC:overwrite-glcore
  (let ((original glCoreTextureDraw)
        (compiled glC:glCoreTextureDraw!)
        (active glCoreTextureDraw))
    (case-lambda
     (()
      (cond
       ((eq? active original) #f)
       ((eq? active compiled) 'compiled)
       (else active)))
     ((x)
      (cond
       ((procedure? x) (set! active x))
       ((eq? #t x) (set! active compiled))
       ((eq? #f x) (set! active original))
       (else (error "unhandled" x)))
      (set! glCoreTextureDraw active)))))

($glC:overwrite-glcore #t)

(define (glGui:pixmap-draw g wgt)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (img (glgui-widget-get g wgt 'image))
         (a (glgui-widget-get-dyn g wgt 'angle))
         (c (glgui-widget-get g wgt 'color)))
    (if (pair? img) (set! img (apply make-glC:image img)))
    (let* ((sw (glgui-widget-get-dyn g wgt 'sw))
           (sh (glgui-widget-get-dyn g wgt 'sh))
           (w (if sw sw (glC:image-w img)))
           (h (if sh sh (glC:image-h img))))
      ;; commented out: the cacheable version
      ;;
      ;; ((glC:ImageTexturePrepare i w h c) x y a)
      (cond
       ((procedure? img) (glC:ImageTextureDraw! (img) w h c x y a))
       (else (glC:ImageTextureDraw! img w h c x y a))))))

(set! glgui:pixmap-draw glGui:pixmap-draw)

;;;** Glyhps

(define glyphvector-columns
  ;; This is a good place to add additional columns for fancy things
  ;; like the cross product of ligatures, hyphonation etc.
  2)

(define-macro (%%glyphvector-tag) ''glyphvector)

(define glyphvector?
  (begin ;;mdvector-make-instance?
   (lambda (obj)
     (and (mdvector? obj) (eq? (%%glyphvector-tag) (mdvector-special obj))))))

(define-macro (macro-MATURITY+2:vector-fill-glyphs-from-unicodevector!
               result start end source source-start font)
  (let ((from (gensym 'from))
        (to (gensym 'to)))
    `(do ((,to ,start (##fx+ ,to 1))
          (,from ,source-start (##fx+ ,from 1)))
         ((eqv? ,to ,end))
       (vector-set! ,result ,to (MATURITY+1:ln-ttf:font-ref ,font (vector-ref ,source ,from))))))

(define (vector-fill-glyphs-from-unicodevector! result start end source source-start font)
  #;(macro-MATURITY+2:vector-fill-glyphs-from-unicodevector! result start end source source-start font)
  (do ((to start (fx+ to 1))
       (from source-start (fx+ from 1)))
      ((eqv? to end))
    (vector-set! result to (MATURITY+1:ln-ttf:font-ref font (vector-ref source from)))))

(define (MATURITY+0:utf8string->guide-glyphvector str font)
  (unless (ln-ttf:font? font) (error "not a font" 'utf8string->glyphvector))
  ;; convert UTF8 to unicode u32vector and glyph
  (let* ((sl (string-length str))
         (ie sl)
         (a0 (make-vector ie)))
    ;; TBD: optimize initialization, for now we use the pre-tested
    ;; code as is, no matter how expensive the overhead is.
    (do ((o 0 (fx+ o 1))
         (codepoints (utf8string->unicode str) (cdr codepoints)))
        ((null? codepoints)
         (and
          (not (eqv? o 0))
          (let* ((len o)
                 (rng (range (vector len glyphvector-columns)))
                 (vol (range-volume rng))
                 (result (make-vector vol)))
            (subvector-move! a0 0 o result 0) ;; row 0: u32 unicode
            (let ((end (+ len len)))
              (macro-MATURITY+2:vector-fill-glyphs-from-unicodevector!
               result
               len ;; row 1: glyphs
               end
               a0 0
               font))
            (make-mdvector rng result (%%glyphvector-tag)))))
      (vector-set! a0 o (car codepoints)))))

(define utf8string->guide-glyphvector MATURITY+0:utf8string->guide-glyphvector)

(define (guide-glyphvector-length vec)
  (unless (glyphvector? vec) (error "illegal argument" guide-glyphvector-length))
  (range-size (mdvector-range vec) 0))

(define (MATURITY+0:guide-glypvector-bounds vec #!optional (construct #f))
  (let* ((rng (mdvector-range vec))
         (row-glyph 1)
         (len (range-size rng 0)))
    (do ((i (fx- len 1) (fx- i 1))
         (above 0.)
         (below 0.))
        ((eqv? i -1) (if construct (construct below above) (values below above)))
      (let ((glyph (mdvector-ref vec row-glyph i)))
        (when glyph
          (let* ((goy (ttf:glyph-offsety glyph)))
            ;; FIXME: check/defaults into constructor!
            (unless goy
              (MATURITY +2 "glyph without vertical offset" loc: 'glypvectorheight)
              (set! goy 0.))
            (set! above (max above goy))
            (let ((gh (ttf:glyph-height glyph)))
              ;; FIXME: check/defaults into constructor!
              (set! below (min below (fx- goy gh))))))))))

(define guide-glypvector-bounds MATURITY+0:guide-glypvector-bounds)

(define (MATURITY+0:guide-glypvector-width vec)
  ;; return summary width as flownum
  (let* ((rng (mdvector-range vec))
         (row-glyph 1)
         (len (range-size rng 0)))
    (do ((i (fx- len 1) (fx- i 1))
         (x 0.))
        ((eqv? i -1) x)
      (let ((glyph (mdvector-ref vec row-glyph i)))
        (when glyph
          ;; Note: can't use fl+ here since `ttf:glyph-advancex`
          ;; rarely, but sometimes returns fixnums.
          (set! x (+ (ttf:glyph-advancex glyph) x)))))))

;;;** Strings

(define (%%glC:glyphvector-bounds glyphs font)
  (receive (below above) (guide-glypvector-bounds glyphs)
    (let ((override (MATURITY+1:ln-ttf:font-ref font (char->integer #\|))))
      ;; note: this path is usually always taken
      (when override
        (let ((goy (ttf:glyph-offsety override)))
          (set! above (max (max above goy) above))
          (set! below (min (fx- goy (ttf:glyph-height override)) below)))))
    (values below above)))

(define (glC:rederglyph/xy x y glyph color) ;; --> (advance-x rendering shift)
  (let ((gh (ttf:glyph-height glyph))
        (gax (ttf:glyph-advancex glyph)))
    (if (and (fx> gh 0) (fx> (ttf:glyph-width glyph) 0)) ;; anything to render?
        (let ((target
               (let ((vertices
                      (make-mdvector-rect-vertices/x0y0x1y1
                       0 0 (ttf:glyph-width glyph) (ttf:glyph-height glyph))))
                 (make-glC:vertex-set
                  (mdvector-body vertices)
                  (ttf:glyph-rect-texcoords glyph)
                  (mdvector-body color)
                  2 4)))
              (scale #f)
              (shift
               (let* ((gox (exact->inexact (ttf:glyph-offsetx glyph)))
                      (goy (exact->inexact (ttf:glyph-offsety glyph)))
                      (gax (exact->inexact (ttf:glyph-advancex glyph)))
                      (x (fl+ x gox))
                      (y (fl- (fl+ y goy) (exact->inexact gh))))
                 (f32vector x y 0.)))
              (rot #f))
          (values gax target shift))
        (values gax #f #f))))

(define (guide-linebreak-unicodevector!
;;; TBD: maybe switch to use glyphvectors here and in textarea
         into ;; receiving ggb2d
         data  ;; vector of codepoints
         font  ;; font to use
         width ;; width to fill
         #!key
         (rows 3) (cols 20)
         (indicator-space 32)
         (indicator-return 13)
         (indicator-newline 10)
         )
  ;; TBD: use ggb2d-procedures (which where late in the game).
  (MATURITY -2 "BEWARE DoS: overly expensive, especially for crafted input (e.g., sequences of zeros)"
            loc: guide-linebreak-unicodevector!)
  (let* ((lines (ggb2d-lines into))
         (current-line
          (and
           (> (ggb-length lines) 0)
           (ggb-ref lines (max 0 (- (ggb-point lines) 1)))))
         (current-line-width
          ;; TBD: look back in case the last char matches `indicator-space`
          0 #;(MATURITY+0:utf8string->guide-glyphvector current-line font))
         (current-word (make-ggb size: 10))
         (current-word-width 0))
    (define (push-line!)
      (set! current-line (make-ggb size: cols))
      (set! current-line-width 0)
      (ggb-insert! lines current-line))
    (define (push-word!)
      (ggb-for-each current-word (lambda (i c) (ggb-insert! current-line c)))
      (set! current-line-width (+ current-line-width current-word-width))
      (ggb-clear! current-word)
      (set! current-word-width 0))
    (push-line!)
    (let* ((getnext! #f)
           (total-length
            (cond
             ((or (ggb2d? data) (ggb? data))
              (let* ((lines (if (ggb2d? data) (ggb2d-lines data) data))
                     (len 0))
                (let ((i0 (ggb2d-current-row data)))
                  (when (and i0 (< i0 (ggb-length lines)))
                    (ggb-for-each
                     lines
                     (lambda (i line)
                       (set! len (+ len (ggb-length line))))
                     i0)))
                ;; NOT using ggb-goto! here to avoid leaving cow mode
                (let* ((lidx (max 0 (- (ggb-point lines) 1)))
                       (curlin (ggb-ref lines lidx))
                       (cidx 0)
                       (curlinlen (ggb-length curlin)))
                  (set!
                   getnext!
                   (lambda ()
                     (set! len (- len 1))
                     (when (>= cidx curlinlen)
                       (set! lidx (+ 1 lidx))
                       (set! curlin (ggb-ref lines lidx))
                       (set! curlinlen (ggb-length curlin))
                       (set! cidx 0))
                     (let ((c (ggb-ref curlin cidx)))
                       (set! cidx (+ cidx 1))
                       c))))
                len))
             ((u32vector? data) (u32vector-length data))
             (else (error "invalid argument" guide-linebreak-unicodevector! data)))))
      (do ((i 0 (fx+ i 1)))
          ((eqv? i total-length)
           (push-word!)
           into)
        (let ((c (cond
                  ((u32vector? data) (u32vector-ref data i))
                  (else (getnext!)))))
          (cond
           ((eqv? c indicator-return)) ;; we currently drop return characters
           ((eqv? c indicator-newline)
            (push-word!)
            (ggb-insert! current-line c)
            (push-line!))
           (else
            (let* ((glyph (MATURITY+1:ln-ttf:font-ref font c))
                   (gw (if glyph (ttf:glyph-advancex glyph) 0))
                   (next-word-width (+ current-word-width gw)))
              (cond
               ((eqv? c indicator-space) ;; space TBD: introduce set of word breaking chars
                (ggb-insert! current-word c)
                (set! current-word-width next-word-width)
                (when (>= (+ current-word-width current-line-width) width)
                  (push-line!))
                (push-word!))
               (else
                (cond
                 ((>= next-word-width width) ;; unbreakable case
                  (push-word!) ;; push what fits
                  (push-line!)
                  (ggb-insert! current-word c)
                  (set! current-word-width gw))
                 ((>= (+ next-word-width current-line-width) width) ;; soft line break
                  (unless (eqv? current-line-width 0)
                    (push-line!)
                    (ggb-insert! current-word c)
                    (set! current-word-width next-word-width)))
                 (else
                  (ggb-insert! current-word c)
                  (set! current-word-width next-word-width)))))))))))))

;;*** glyph vector rendering (draft)

(define glyphvector->render00-columns 3)

(define (MATURITY+1:glC:glyphvector->render00
         x y w h glyphs color #!optional (rtl #f) (offset 0) (limit #f))
  ;; TBD: get rid of this one!
  (define (color-conv color)
    (cond ;; FIXME define & use consistent conversion
     ((integer? color) (make-rect-single-color-array color))
     ((not color) guide-color-transparent+black-array)
     (else color)))
  (unless (mdvector? color) (set! color (color-conv color)))
  ;;(guide-glyphvector-length vec)
  (let* ((limit
          (let ((len (guide-glyphvector-length glyphs)))
            (if (number? limit) (min limit len) len)))
         (rng (range (vector glyphvector->render00-columns limit)))
         (resvec (make-vector (range-volume rng) #f))
         (result (make-mdvector rng resvec))
         (glyphvec (mdvector-body glyphs))
         (glyphidx (mdv-indexer (mdvector-range glyphs)))
         (glyph-idx 1)
         (yflo (exact->inexact y))
         (istep (if rtl -1 1))
         (idx (mdv-indexer rng)))
    (do ((i (if rtl (- limit 1) offset) (fx+ i istep))
         (to 0)
         (x0 0)) ;; maybe we better don't use flownums here?
        ((or (if rtl (< i 0) (>= i limit))
             (> x0 w))
         (cond
          ((eqv? to limit) result)
          ((eqv? to 0) #f)
          (else
           (make-mdvector
            (range (vector glyphvector->render00-columns to))
            resvec))))
      (let ((glyph ;;; (mdvector-ref glyphs glyph-idx i)
                   (vector-ref glyphvec (glyphidx glyph-idx i))))
        (when glyph
          (receive (gax target shift)
              (glC:rederglyph/xy
               (exact->inexact (+ x0 x)) yflo ;; TBD: make the other side accept propper fixnums!
               glyph color)
            (set! x0 (+ x0 gax))
            (if target
                (begin
                  (unless (> x0 w)
                    (vector-set! resvec (idx to 0) target)
                    (vector-set! resvec (idx to 1) shift)
                    (vector-set! resvec (idx to 2) (ttf:glyph-image glyph))
                    (set! to (fx+ to 1)))))))))))

#|
(define (GONE:glC:render-target-mdv! targets)
  (MATURITY -4 "outdated" loc: MATURITY-1:glC:render-target-mdv!)
  (let* ((rng (mdvector-range targets))
         (limit (range-size rng 1)))
    (do ((i 0 (fx+ i 1)))
        ((eqv? i limit))
      (let ((vertices (mdvector-ref targets i 0))
            (shift (mdvector-ref targets i 1))
            (texture (mdvector-ref targets i 2))
            (rot #f))
        (unless texture
          ;; ??? might be a regular case - when?
          (MATURITY -4 "no texture found after rendering" loc: 'glC:render-target-mdv!))
        ;;; (when texture (MATURITY +1 "texture found after rendering" loc: 'glC:render-target-mdv!))
        (and texture (glC:TextureDrawGlArrays texture vertices scale shift rot))))))

(define (MATURITY-1:glC:render-target-mdv! targets)
  (let* ((rng (mdvector-range targets))
         (limit (range-size rng 1))
         ;; optimizations
         (idx (mdv-indexer #|TBD: skip checks!|# rng))
         (targets (mdvector-body targets)))
    (do ((i 0 (fx+ i 1)))
        ((eqv? i limit))
      (let ((vertices (##vector-ref targets (idx i 0)))
            (shift (##vector-ref targets (idx i 1)))
            (texture (##vector-ref targets (idx i 2)))
            (rot #f))
        (unless texture
          ;; ??? might be a regular case - when?
          (MATURITY -2 "no texture found after rendering" loc: 'glC:render-target-mdv!))
        ;;; (when texture (MATURITY +1 "texture found after rendering" loc: 'glC:render-target-mdv!))
        (and texture (glC:TextureDrawGlArrays texture vertices scale shift rot))))))

|#

(define (MATURITY+1:glC:render-target-mdv! targets)
  (let* ((rng (mdvector-range targets))
         ;; FIXME: export some macros to support inlining instead of
         ;; absusing debug features.
         (vec (debug#range-in rng))
         (vol0 (##vector-ref vec 4))
         (o0 (debug#range-offset rng))
         (limit (range-size rng 1))
         (targets (mdvector-body targets)))
    (do ((i 0 (fx+ i 1)))
        ((eqv? i limit))
      (let ((j (fx+ o0 (fx* i vol0))))
        (let ((texture (##vector-ref targets (fx+ j 2))))
          (when texture
            (let ((vertices (##vector-ref targets (fx+ j 0)))
                  (shift (##vector-ref targets (fx+ j 1)))
                  (rot #f))
              ;; (when texture (MATURITY +1 "texture found after rendering" loc: 'glC:render-target-mdv!))
              ;;
              ;; TOP in gprof; the following line did it all -- inlining:
              ;;
              ;; (glC:TextureDrawGlArrays texture vertices scale shift rot)
              ;;
              ;; Note: inlining the above line gave 2021-02-02 a
              ;; pretty detailed profiling result essentially pointing
              ;; into glC:glCoreEnd lines glVertexPointer and
              ;; glTexCoordPointer above most except ___SCMOBJ_to_INT
              (let ((needs-matrix (or scale shift rot)))
                ;; inline of: (glC:TextureDrawGlArrays texture vertices scale shift rot)
                (when needs-matrix (glPushMatrix))
                (cond
                 ((f32vector? scale)
                  (glScalef//checks (##f32vector-ref scale 0) (##f32vector-ref scale 1) (##f32vector-ref scale 2)))
                 ((procedure? shift) (shift)))
                (cond
                 ((f32vector? shift)
                  (glTranslatef//checks (##f32vector-ref shift 0) (##f32vector-ref shift 1) (##f32vector-ref shift 2)))
                 ((procedure? shift) (shift)))
                #|
                (cond
                 ((f32vector? rot)
                  (glRotatef//checks (f32vector-ref rot 0) (f32vector-ref rot 1)  (f32vector-ref rot 2)  (f32vector-ref rot 3)))
                 ((procedure? rot) (rot)))
                |#
                (_glCoreTextureBind texture)
                ;; (glCoreBegin)
                (glC:glCoreEnd vertices GL_TRIANGLE_STRIP)
                (when needs-matrix (glPopMatrix))
                #!void))))))))
