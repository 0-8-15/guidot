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
           (codepoints (utf8string->unicode str) (cdr codepoints)))
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
(define make-mdvector-interval
  ;; FIXME: make it a typed pair of an offset vector and a range!
  (let ((tag '(exact positive))
        (d1 (range '#(2 1)))
        (d2 (range '#(2 2)))
        (d3 (range '#(2 3)))
        (d4 (range '#(2 4))))
    (set! mdvector-interval?
          (lambda (obj) (and (mdvector? obj) (eq? (mdvector-special obj) tag))))
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
  (define (conv x)
    (cond
     ((fixnum? x) x)
     (else (inexact->exact (floor x)))))
  (make-mdvector-interval 2 (conv x0) (conv y0) (conv x1) (conv y1)))

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
              (error "range not 4x2" make-mdvector-rect-vertices rng))
            (when storage
              (unless (> (f32vector-length storage) (+ (range-volume rng) offset))
                (error "stotrage too small to hande range" make-mdvector-rect-vertices))))
          (set! rng vertex-rect-range2d))
      (let ((vol (range-volume rng)))
        (unless (f32vector? storage)
          (set! storage (make-f32vector vol)))
      (make-mdvector rng storage tag vol)))
    %%make-mdvector-rect-vertices))

(define (glC:mdv-vertex-area? obj)
  (and (mdvector-special? obj 'f32)
       (let ((rng (mdvector-range obj)))
         (and (eqv? (range-rank rng) 2) (eqv? (range-size rng 0) 2)))))

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
  ;;; y
  ;;; 1 +---+
  ;;;   |   |
  ;;; 0 +---+
  ;;; x 0   1
  (unless (mdvector-interval? interval)
    (error "invalid argument" interval))
  (let ((idx (mdv-indexer (mdvector-range interval)))
        (body (mdvector-body interval)))
    (define (convert v t) (exact->inexact (vector-ref body (idx v t))))
    (let ((x0 (convert 0 0)) (y0 (convert 0 1))
          (x1 (convert 1 0)) (y1 (convert 1 1)))
      (%%make-mdvector-rect-vertices
       #f
       (f32vector
        x0 y1
        x1 y1
        x0 y0
        x1 y0)))))

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
        (for-range-index! size (lambda (i) (u8vector/32-set! storage (fx* i 4) color)))
        (%%make-mdvector-color rng storage)))
    make-single-color-mdv/1))

(define (make-rect-single-color-array color) ;; 4 sides each a color
  (%%make-single-color-mdv/1 4 color))

(define guide-color-black-array (make-rect-single-color-array Black))

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
            (allocate-make-mdvector storage storage-offset rng tag)))))

(define (make-legacy-texcoords x0 y0 x1 y1)
  (%%make-mdvector-texcoords
   vertex-rect-range2d
   (f32vector
    x0 y1
    x1 y1
    x0 y0
    x1 y0)))

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
  (MATURITY -1 "remove dependency on srfi 179" loc: glC:fill-rect-full-vertex-set!)
  (let* ((vertices (make-mdvector-rect-vertices/mdvector-interval interval))
         (texcoords rect-texcoords)
         (colors (make-rect-single-color-array color)))
    (glC:vertex-set-set! target idx vertices texcoords colors)
    target))

(define (%%glC:make-rect-full-vertex-set interval color)
  ;; TBD: check! arrange the rectangle with SW edge as (0, 0)
  (let ((target (create-glC:vertex-set 2 4)))
    (glC:fill-rect-full-vertex-set! target 0 interval color)
    target))

(define (glC:make-rect-full-vertex-set interval color)
  ;; arrange the rectangle with SW edge as (0, 0)
  (let ((target (create-glC:vertex-set 2 4)))
    (glC:fill-rect-full-vertex-set! target 0 interval color)
    target))

(define (glC:glCoreEnd glc-vertexset line-type)
  (glVertexPointer (glC:vertex-set-d23 glc-vertexset) GL_FLOAT 0 (glC:vertex-set-v glc-vertexset))
  (glColorPointer 4 GL_UNSIGNED_BYTE 0 (glC:vertex-set-c glc-vertexset))
  (cond
   ((or (fx= line-type GL_LINES) (fx= line-type GL_LINE_LOOP) (fx= line-type GL_LINE_STRIP))
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
    (glTranslatef//checks (f32vector-ref scale 0) (f32vector-ref scale 1) (f32vector-ref scale 2)))
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
  ;; TBD: for the time being, should look at lnglgui's clip to.
  (let ((i #f))
    (case-lambda
     (() i)
     ((v) (unless (or (mdvector-interval? v) (not v)) (error "not an mdvector-interval or #f" v)) (set! i v))
     ((x0 y0 x1 y1) (set! i (make-mdvector-interval 2 x0 y0 x1 y1))))))

(define glC:clip-mode (make-parameter #t)) ;; deprecated, for debugging.

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

(define (glC:image-clip-texcoords img clip-interval)
  (let ((@x1 (glC:image-xsw img))
        (@y1 (glC:image-ysw img))
        (@x2 (glC:image-xno img))
        (@y2 (glC:image-yno img)))
    (NYI "glC:image-clip-texcoords")
    (make-legacy-texcoords @x1 @y1 @x2 @y2)))

(define (glC:image->guide-image img #!optional (position #f) (y0 0))
  (let ((interval
         (cond
          ((not position)
           (make-mdvector-interval 2 0 0 (glC:image-w img) (glC:image-h img)))
          ((number? position)
           (make-mdvector-interval
            2
            (round (inexact->exact position)) (round (inexact->exact y0))
            (glC:image-w img) (glC:image-h img)))
          ((mdvector-interval? position) position)
          (else (error "glC:image->guide-image: illegal position" position))))
        (vertices
         (cond
          ((not position)
           (make-mdvector-rect-vertices/x0y0x1y1 0 0 (glC:image-w img) (glC:image-h img)))
          ((number? position)
           (make-mdvector-rect-vertices/x0y0x1y1
            (round (inexact->exact position)) (round (inexact->exact y0))
            (glC:image-w img) (glC:image-h img)))
          (else (error "glC:image->guide-image: illegal position" position))))
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
     ((or (equal? color 0) (not color)) guide-color-black-array)
     ((fixnum? color) (make-rect-single-color-array color))
     (else colors)))
  (if (and (fx> w 0) (fx> h 0))
      (let* ((drawinterval0 (make-mdvector-interval 2 0 0 w h))
             (positioned
              (and clip-int
                   (let (#;(img-interval
                         (make-vector-dimension (vector (glC:image-w img) (glC:image-h img))))
                         (xfi (inexact->exact (round x)))
                         (yfi (inexact->exact (round y))))
                     #;(interval-translate img-interval (vector xfi yfi))
                     (make-mdvector-interval 2 xfi (fx+ xfi w) yfi (fx+ yfi h)))))
             (drawinterval (if clip-int (glC:clip drawinterval0 clip-int) drawinterval0)))
        (if drawinterval
            (let ((texture (glC:image-t img))
                  (scale #f)
                  (shift (f32vector (exact->inexact x) (exact->inexact y) 0.)))
              (unless (mdvector-interval? drawinterval)
                (MATURITY -10 "unless (mdvector-interval? drawinterval)" loc: 'glC:ImageTextureDrawInto!))
              (let ((end (let ((texcoords
                                (if (eq? drawinterval 0)
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
  (let ((lst glcore:cliplist))
    (and (pair? lst) (apply make-x0y0x1y1-interval/coerce (car lst)))))

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

(define (MATURITY-1:glC:glCoreTextureDraw x y w0 h0 t x1 y1 x2 y2 r #!rest colors)
  #; (apply glCoreTextureDraw x y w0 h0 t x1 y1 x2 y2 r colors)
  (let ((entry (%%glCore:textures-ref t #f)))
    (if entry
        (let* ((w0 (if (fixnum? w0) w0 (inexact->exact (floor w0))))
               (h0 (if (fixnum? h0) h0 (inexact->exact (floor h0))))
               (w (if (fx= w0 0) (glCore:texture-width entry) w0))
               (h (if (fx= h0 0) (glCore:texture-height entry) h0))
               (x1 (exact->inexact x1))
               (y1 (exact->inexact y1))
               (x2 (exact->inexact x2))
               (y2 (exact->inexact y2))
               (colors (if (pair? colors)
                           (apply vector (debug 'COLORS!!! colors))
                           (%%glC:read-legacy-global-color-variable)))
               (img (make-glC:image w h entry x1 y1 x2 y2)))
          (glC:ImageTextureDraw! img w h (debug 'colors colors) x y r))
        (log-error "glCoreTextureDraw: unbound index " t))))

(define (MATURITY-3:glC:glCoreTextureDraw x y w0 h0 t x1 y1 x2 y2 r #!rest colors)
  (MATURITY -10 "Gone, KEEP FOR REFERENCE for a while!" loc: MATURITY-3:glC:glCoreTextureDraw)
  #; (apply glCoreTextureDraw x y w0 h0 t x1 y1 x2 y2 r colors)
  (let ((entry (%%glCore:textures-ref t #f)))
    (if entry
        (if (> (CCML) -2)
            (let* ((w0 (if (fixnum? w0) w0 (inexact->exact (floor w0))))
                   (h0 (if (fixnum? h0) h0 (inexact->exact (floor h0))))
                   (w (if (fx= w0 0) (glCore:texture-width entry) w0))
                   (h (if (fx= h0 0) (glCore:texture-height entry) h0))
                   (x1 (exact->inexact x1))
                   (y1 (exact->inexact y1))
                   (x2 (exact->inexact x2))
                   (y2 (exact->inexact y2))
                   (colors (if (pair? colors)
                               (apply vector (debug 'COLORS!!! colors))
                               (%%glC:read-legacy-global-color-variable)))
                   (img (make-glC:image w h entry x1 y1 x2 y2)))
              (if #t ;;
                  (glC:ImageTextureDraw! img w h (debug 'colors colors) x y r)))
            (let ((w (flo (if (fx= (fix w0) 0) (glCore:texture-width entry) w0)))
                  (h (flo (if (fx= (fix h0) 0) (glCore:texture-height entry) h0))))
              (if (null? glcore:cliplist)
                  (if (pair? colors)
                      (glCore:TextureDrawUnClipped
                       (flo x) (flo y) w h t (flo x1) (flo y1) (flo x2) (flo y2) (flo r)
                       (car colors))
                      (glCore:TextureDrawUnClipped
                       (flo x) (flo y) w h t (flo x1) (flo y1) (flo x2) (flo y2) (flo r)))
                  (if (pair? colors)
                      (glCore:TextureDrawClipped
                       (flo x) (flo y) w h t (flo x1) (flo y1) (flo x2) (flo y2) (flo r)
                       (car colors))
                      (glCore:TextureDrawClipped
                       (flo x) (flo y) w h t (flo x1) (flo y1) (flo x2) (flo y2) (flo r))))))
        (log-error "glCoreTextureDraw: unbound index " t))))

(define glC:glCoreTextureDraw MATURITY-1:glC:glCoreTextureDraw!)

(define $glC:overwrite-glcore
  (let ((original glCoreTextureDraw)
        (compiled glC:glCoreTextureDraw)
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
       (else (error "unhandled" x)))))))

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

;;;** Strings

(define (%glC:renderglyph1! target idx g img color)
  ;; TBD: how does "legacy latex" drawing work?
  (MATURITY 3 "experimental, SHALL factor shift into vertices" loc: %glC:renderglyph1!)
  (let ((vertices (guide-image-area img))
        (texcoords (guide-image-texcoord img)))
    (glC:vertex-set-set! target idx vertices texcoords colors)))

(define (glC:rederglyph-into!0 target idx x y glyph glCimage color)
  (define (glyph+x+y+gh->f32vector-shift x y gh glyph)
    ;; TODO: factor shift into the vertices!??
    ;;(MATURITY -1 "working, computes shift" loc: glC:legacy-glyph+x+y+gh->f32vector-shift)
    (let* ((gox (exact->inexact (glgui:glyph-offsetx glyph)))
           (goy (exact->inexact (glgui:glyph-offsety glyph)))
           (gax (exact->inexact (glgui:glyph-advancex glyph)))
           (x (fl+ x gox))
           (y (fl+ y goy (fl- (exact->inexact gh)))))
      (f32vector x y 0.)))
  (MATURITY 0 "working: renders glyp at correct position" loc: glC:rederglyph-into!0)
  (let* ((img glCimage)
         (gh (glC:image-h img))
         (gax (glgui:glyph-advancex glyph)))
    (if (and (fx> gh 0) (fx> (glC:image-w img) 0)) ;; anything to render?
        (let* ((img (glC:image->guide-image img)))
          (let ((end (let ((vertices (guide-image-area img))
                           (texcoords (guide-image-texcoord img)))
                       (glC:vertex-set-set! target idx vertices texcoords color)))
                (texture (guide-image-texture img))
                (scale #f)
                (shift (glyph+x+y+gh->f32vector-shift x y gh glyph))
                (rot #f))
            (values gax end shift)))
        (values gax idx #f))))

(define glC:rederglyph-into! glC:rederglyph-into!0)

(define glGui:renderstring
  (let ((target glC:legacy-vertex-set-2d)
        (idx 0))
    (define (color-conv color)
      (cond ;; FIXME define & use consistent conversion
       ((or (= color 0) (not color)) guide-color-black-array)
       ((fixnum? color) (make-rect-single-color-array color))
       (else color)))
    (define (renderglyph x y glyph img color) ;; NOTE: positions are inexact!
      ;; => x-delta
      (MATURITY 0 "stable; adapt when glyph has distinct data type" loc: 'renderglyph)
      (let ((img (if (pair? img) (apply make-glC:image img) img)))
        (receive (gax end shift) (glC:rederglyph-into!0 target idx x y glyph img color)
          (let ((texture (glC:image-t img))
                (scale #f)
                (rot #f))
            (when (fx> end idx) (glC:TextureDrawGlArrays texture target scale shift rot)))
          gax)))
    (define (renderstring x y txt fnt color)
      (set! glC:renderstring-is-active #t)
      (when (exact? y)
        (MATURITY -1 "exact y unexpected" renderstring)
        (set! y (exact->inexact y)))
      (set! color (color-conv color))
      (do ((i 0 (fx+ i 1))
           (x0 (exact->inexact x))
           (ccv (utf8string->u32vector txt)))
          ((fx= i (u32vector-length ccv)))
        (let* ((charcode (u32vector-ref ccv i))
               (g (assoc charcode fnt)))
          (if g
              (let ((img (glgui:glyph-image g)))
                (if img
                    (set! x0 (fl+ x0 (renderglyph x0 y g img color)))
                    (log-error "no image for glyph: " charcode g)))
              (log-error "no glyph for charcode: " charcode)))))
    renderstring))

(set! glgui:renderstring glGui:renderstring)
