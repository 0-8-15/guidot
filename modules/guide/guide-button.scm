
(define %%guide-view-default-dynamic
  ;; guide view are dynamic updated -- maybe useful for debugging
  ;;
  ;; default: #f
  (make-parameter #f))

(define (MATURITY+3:make-guide-button-view)
  (define (color-conv color)
    (cond ;; TBD: define consistent conversion here!
     ((integer? color) (make-rect-single-color-array color))
     ((not color) guide-color-transparent+black-array)
     (else color)))
  (let* ((font (find-font (guide-select-font size: 'small)))
         (label #f)
         (align 'left)
         (texture (%%glCore:textures-ref 0 #f))
         (texcoords (make-legacy-texcoords 0. 1. .7734375 .375)) ;; button style
         (foreground-color (guide-select-color-3))
         (background-color (guide-select-color-4))
         (x 0)
         (y 0)
         (w 100)
         (h 100)
         (scale #f)
         (shift #f)
         (rot #f)
         (visible #t)
         ;; derived
         (glyphs #f)
         (foreground-x-offset (lambda () 0.)) ;; example
         (foreground-y-offset
          (let* ((formula
                  (lambda (font label h)
                    (cond
                     ((not (and glyphs font)) 0) ;; default
                     (#t ;; verticaly centered - so far the only case
                      (receive (below above) (%%glyphvector-bounds glyphs font)
                        (let* ((heff (+ above below))
                               (hspace (- (if (> h 0) h heff) heff))
                               (centery (+ y (/ hspace 2))))
                          centery)))
                     (else (NYIE 'guide-button-view)))))
                 (calculation (memoize-last formula eq? equal? eqv?)))
            (lambda () (calculation font label h))))
         (fgcolora
          (let ((calc (memoize-last color-conv equal?)))
            (lambda () (calc foreground-color))))
         (bgcolora
          (let ((calc (memoize-last color-conv equal?)))
            (lambda () (calc  background-color))))
         (figure ;; rectangular
          (let* ((formula (lambda (w h) (make-mdvector-rect-vertices/x0y0x1y1 0 0 w h)))
                 (calc (memoize-last formula eqv? eqv?)))
            (lambda () (calc w h))))
         (bgvset
          (let* ((formula
                  (lambda (figure texcoords colors)
                    (make-glC:vertex-set
                     (mdvector-body figure)
                     (mdvector-body texcoords)
                     (mdvector-body colors)
                     2 4)))
                 (calc (memoize-last formula eq? eq? eq?)))
            (lambda () (calc (figure) texcoords (bgcolora)))))
         (fgvset #f))
    (define fixed-draw
      (lambda ()
        (if visible
            (let ((shift-0 (and shift (vector-ref shift 0)))
                  (shift-1 (and shift (vector-ref shift 1)))
                  (shift-2 (and shift (vector-ref shift 2)))
                  (scale-0 (and scale (vector-ref scale 0)))
                  (scale-1 (and scale (vector-ref scale 1)))
                  (scale-2 (and scale (vector-ref scale 2)))
                  (rot-0 (and rot (vector-ref rot 0)))
                  (rot-1 (and rot (vector-ref rot 1)))
                  (rot-2 (and rot (vector-ref rot 2)))
                  (rot-3 (and rot (vector-ref rot 3)))
                  (bgvset (bgvset))
                  (texture texture)
                  (foreground-x-offset (foreground-x-offset))
                  (foreground-y-offset (exact->inexact (foreground-y-offset)))
                  (fgvset fgvset))
              (lambda () ;; result … TBD: specialize up to nothing…
                ;; if not already in shift mode ... simplicity: a toplevel matrix push
                (glPushMatrix)
                (when shift-0
                  (glTranslatef//checks shift-0 shift-1 shift-2))
                (when scale-0
                  (glScalef//checks scale-0 scale-1 scale-2))
                (when rot-0
                  (glRotatef//checks rot-0 rot-1 rot-2 rot-3))
                (when bgvset
                  (glC:TextureDrawGlArrays texture bgvset #f #f #f))
                (cond
                 ((procedure? fgvset) (fgvset))
                 (fgvset
                  (glTranslatef//checks foreground-x-offset foreground-y-offset 0.)
                  (MATURITY+1:glC:render-target-mdv! fgvset)))
                ;; eventually pop out
                (glPopMatrix)))
            (lambda () #f))))
    (define (text-set! str)
      (set! label str)
      (set! glyphs (and label font (utf8string->guide-glyphvector label font)))
      (let ((targets
             (and glyphs
                  (case align
                    ((left) (MATURITY+1:glC:glyphvector->render00 0 0 w h glyphs (fgcolora)))
                    ((right)
                     (let ((shx (let* ((strw (MATURITY+0:guide-glypvector-width glyphs))
                                       (txo (- w strw)))
                                  (max 0 txo))))
                       (MATURITY+1:glC:glyphvector->render00 shx 0 w h glyphs (fgcolora))))
                    ((center)
                     (let ((shx (let* ((strw (MATURITY+0:guide-glypvector-width glyphs))
                                       (txo (- w strw)))
                                  (max 0 (/ txo 2)))))
                       (MATURITY+1:glC:glyphvector->render00 shx 0 w h glyphs (fgcolora))))))))
        (set! fgvset targets))
      #!void)
    (define (texture-set! t)
      (define tp t)
      (when (integer? t)
        (MATURITY -1 "outdated call, replacing texture lookup for now" loc: 'texture-set! t)
        (set! t (glCore:textures-ref t identity)))
      (unless (glCore:texture? t) (error "invalid argument" 'texture-set! t tp))
      (set! texture t))
    (define (ctrl! key more)
      (case key
        ((visible:) ;; maybe factor visibility out?
         (if (pair? more) (set! visible (car more)) visible))
        ((text:) (if (pair? more) (text-set! (car more)) label))
        ((align:)
         (if (pair? more)
             (begin
               (set! align
                     (case (car more)
                       ((left) 'left) ((right) 'right) ((center) 'center)
                       (else (error "alignment must be either left, right or center" (car more)))))
               (text-set! label))
             label))
        ((font:)
         (if (pair? more)
             (let ((v (car more)))
               (unless (ln-ttf:font? v) (error "not a font" v))
               (set! font v)
               (text-set! label))
             font))
        ((background-color:)
         (if (pair? more)
             (set! background-color (car more))
             background-color))
        ((foreground-color:)
         (if (pair? more)
             (begin
               (set! foreground-color (car more))
               (text-set! label))
             foreground-color))
        ((texcoords:)
         (if (pair? more)
             (let ((v (car more)))
               (unless (guide-texcoords? v) (error "not texcoords" v))
               (set! texcoords v))
             texcoords))
        ((texture:)
         (if (pair? more)
             (let ((v (car more)))
               (when (fixnum? v)
                 (MATURITY -2 "silently converting fixnum to texture" v)
                 (set! v (%%glCore:textures-ref v v)))
               (unless (glCore:texture? v) (error "not a texture" v))
               (set! texture v))
             texture))
        ((size:)
         (if (pair? more)
             (receive (pw ph) (apply values more)
               (set! w pw) (set! h ph)
               (text-set! label))
             (values w h)))
        ((position:)
         (if (pair? more)
             (receive (x y) (apply values more)
               (set! shift (vector (exact->inexact x) (exact->inexact y) 0.)))
             shift))
        ((rot:) ;; TBD: `rotate:`? use CSS term!
         ;;; HIER geht's weiter, da passiert 'was!:
         ;;;
         ;;; (magic! rot: 50 1 1 10)
         ;;; (magic! scale: 2 2)
         ;;; (magic! shift: 45 100)
         (if (pair? more)
             (set!
              rot
              (and
               (car more)
               (do ((given (map exact->inexact more) (cdr given))
                    (i 0 (fx+ i 1))
                    (result (make-vector 4 0.)))
                   ((null? given) result)
                 (vector-set! result i (car given)))))
             rot))
        ((scale:)
         (if (pair? more)
             (set!
              scale
              (and
               (car more)
               (do ((given (map exact->inexact more) (cdr given))
                    (i 0 (fx+ i 1))
                    (result (make-vector 3 0.)))
                   ((null? given) result)
                 (vector-set! result i (car given)))))
             scale))
        ((#t #f)   ;; reduce to draw thunk
         (if key   ;; either depending on observed state
             (lambda () (when visible ((fixed-draw))))
             ;; otherwise resolve all references
             (fixed-draw)))
        (else (error "unhandled" 'button-control key))))
    (MATURITY -2 "fresh button created" loc: 'make-guide-button-view)
    (case-lambda
     (() ;; draw thunk without dynamic dependencies
      (if (%%guide-view-default-dynamic)
          (fixed-draw) ;; optimized case, otherwise full exercise
          (ctrl! #t '())))
     ((key . more) (ctrl! key more)))))

(define make-guide-button-view MATURITY+3:make-guide-button-view)
