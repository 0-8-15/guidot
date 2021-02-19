
(define %%guide-view-default-dynamic
  ;; guide view are dynamic updated -- maybe useful for debugging
  ;;
  ;; default: #f
  (make-parameter #f))

(define (guide-figure-contains? figure x y)
  (let ((pos (figure position:))
        (scale (figure scale:))) ;; TBD add rot
    (receive (w h) (figure size:)
      (let ((x1 (if pos (- x (vector-ref pos 0)) x))
            (y1 (if pos (- y (vector-ref pos 1)) y)))
        (and ;; checking y first; likelier off in GUIs
         (positive? y1)
         (< y1 (if scale (* (vector-ref scale 1) h) h))
         (positive? x1)
         (< x1 (if scale (* (vector-ref scale 0) w) w)))))))

(define (MATURITY+2:make-guide-bg+fg-view) ;; a PAIR (BG . FG)
  ;; DEV: derivations; maybe useless
  (define foreground-x (lambda () 0.)) ;; example
  (define foreground-y (lambda () 0.)) ;; example
  (define foreground-w (lambda () 0.)) ;; example
  (define foreground-h (lambda () 0.)) ;; example
  (define figure ;; rectangular ??? required at all? ???
    (let* ((formula (lambda (w h) (make-mdvector-rect-vertices/x0y0x1y1 0 0 w h)))
           (calc (macro-memoize:2->1 formula eqv? eqv?) #;(memoize-last formula eqv? eqv?)))
      (lambda () (calc w h))))
  (let ((background #f) ;; entire area
        (foreground #f) ;; client area
        (x 0)
        (y 0)
        (w 100)
        (h 100)
        ;;; TBD add space for padding&border?
        ;;;
        ;;; NONONO Instead have a style!
        ;;;
        (scale #f)
        (shift #f)
        (rot #f)
        (visible #t))
    (define normalize
      ;; TBD: normalizer for x,y to scaled,shifted,rotated,[visible]
      ;; client area.
      NYI)
    (define fixed-draw
      (lambda ()
        (and visible (or background foreground)
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
                   ;;;
                   ;;; NONONO Instead have a style!
                   ;;;
                   #|
                   (background (if (procedure? background) (background) background))
                   (foreground (if (procedure? foreground) (foreground) foreground))
                   |#
                   (foreground-w (foreground-w))
                   (foreground-h (exact->inexact (foreground-h)))
                   (foreground-x (foreground-x))
                   (foreground-y (exact->inexact (foreground-y)))
                   ) ;; no more precalculations
               (lambda () ;; result … TBD: specialize up to nothing…
                 ;; if not already in shift mode ... simplicity: a toplevel matrix push
                 (glPushMatrix)
                 (when shift-0
                   (glTranslatef//checks shift-0 shift-1 shift-2))
                 (when scale-0
                   (glScalef//checks scale-0 scale-1 scale-2))
                 (when rot-0
                   (glRotatef//checks rot-0 rot-1 rot-2 rot-3))
                 (cond
                  ((procedure? background) (background))
                  (background
                   (MATURITY+1:glC:render-target-mdv! background)))
                 (cond
                  ((procedure? foreground)
                   (glTranslatef//checks foreground-x foreground-y 0.)
                   (foreground))
                  (foreground
                   (glTranslatef//checks foreground-x foreground-y 0.)
                   (MATURITY+1:glC:render-target-mdv! foreground)))
                 ;; eventually pop out
                 (glPopMatrix))))))
    (define (ctrl! key more)
      (case key
        ((visible:) ;; maybe factor visibility out?
         (if (pair? more) (set! visible (and (car more) #t)) visible))
        ((background:)
         (if (null? more) background
             (let ((thing (car more)))
               (cond
                ((or (procedure? thing) (not thing)) (set! background thing))
                (else (error "not a valid background" thing))))))
        ((foreground:)
         (if (null? more) foreground
             (let ((thing (car more)))
               (cond
                ((or (procedure? thing) (not thing)) (set! foreground thing))
                (else (error "not a valid foreground" thing))))))
        ((size:)
         (if (pair? more)
             (receive (pw ph) (apply values more)
               (set! w pw) (set! h ph))
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
         ;;; (m2! rot:  180 30 30 180)
         ;;; (magic! scale: 2 2)
         ;;; (magic! position: 45 100)
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
             (lambda () (when (and visible (or background foreground)) visible ((fixed-draw))))
             ;; otherwise resolve all references
             (fixed-draw)))
        (else (error "unhandled" 'drawing-pair-control key))))
    (MATURITY +2 "fresh drawing pair created"
              loc: "(make-guide-container-view make-guide-bg+fg-view)")
    (case-lambda
     (() ;; draw thunk without dynamic dependencies
      (if (%%guide-view-default-dynamic)
          (ctrl! #t '()) ;; full exercise, otherwise optimized:
          (and visible (or background foreground)
               (fixed-draw))))
     ((key . more)
      (cond
       ((procedure? key)
        ;; key is a procedure receiving the `ctrl!` procedure
        ;; return value is still experimental
        (let ((update! key)
              (location (if (null? more)
                            (error "dynamic drawing requires call location" key)
                            (car more))))
          (lambda ()
            (MATURITY -1 "dynamic update bg+fg" loc: location)
            (let* ((dead #f)
                   (restricted-ctrl!
                    (lambda (#!optional (key #f) . more)
                      (cond
                       (dead (error "too late to update" 'make-guide-bg+fg-view key more))
                       (else (ctrl! key more)))))
                   (drawing (update! restricted-ctrl!)))
              (set! dead #t)
              (cond
               ((procedure? drawing) (drawing))
               ((not drawing))
               (else (error "unexpected" drawing)))))))
       (else (ctrl! key more)))))))

(define (%%guide:complete-padding-css location arg . rest)
  ;; complete and check arguments according to CSS conventions
  (let ((k
         (cond
          ((pair? rest)
           (apply vector arg rest))
          ((vector? arg) arg)
          ((number? arg) (make-vector 4 arg))
          (else (error "invalid arguments" location (cons arg rest))))))
    (let ((len (vector-length k)))
      (do ((i 0 (fx+ i 1))) (eqv? i len)
        (let ((v (vector-ref k i)))
          (unless (and (number? v) (>= v 0))
            (error "invalid argument" location i v))))
      (case len
        ((4) k)
        ((2) ;; y,x
         (vector (vector-ref k 0) (vector-ref k 1) (vector-ref k 0) (vector-ref k 1)))
        ((3) ;; top,x,bottom
         (vector (vector-ref k 0) (vector-ref k 1) (vector-ref k 2) (vector-ref k 1)))))))

(define (MATURITY+2:make-guide-label-view)
  (define %%ttf:font-height glgui:fontheight)
  (define (color-conv color)
    (cond ;; TBD: define consistent conversion here!
     ((integer? color) (make-rect-single-color-array color))
     ((not color) guide-color-transparent+black-array)
     (else color)))
  (let* ((font (find-font (guide-select-font size: 'small)))
         (label #f)
         (horizontal-align 'left)
         (vertical-align 'bottom)
         (color (guide-select-color-2))
         (x 0)
         (y 0)
         (w 100)
         (h 100)
         (padding #f) ;; '#(0 0 0 0) top,right,bottom,left
         (scale #f)
         (shift #f)
         (rot #f)
         (visible #t) ;; TBD: deprecated! use fg+bg pair instead!
         ;; derived
         (glyphs #f) ;; rendered array or image
         (x-offset
          (let* ((formula
                  (lambda (glyphs w align padding)
                    (if glyphs
                        (case align
                          ((left) (if padding (vector-ref padding 3) 0))
                          ((right)
                           (let* ((strw
                                   (cond
                                    ((glC:image? glyphs) (glC:image-w glyphs))
                                    (else (MATURITY+0:guide-glypvector-width glyphs))))
                                  (strwp (if padding (+ strw (vector-ref padding 1)) strw))
                                  (txo (- w strw)))
                             (max 0 txo)))
                          ((center)
                           (let* ((strw
                                   (cond
                                    ((glC:image? glyphs) (glC:image-w glyphs))
                                    (else (MATURITY+0:guide-glypvector-width glyphs))))
                                  (txo (- w strw)))
                             (max 0 (/ txo 2)))))
                        0)))
                 (calculation (memoize-last formula eq? eqv? eq? eq?)))
            (lambda () (calculation glyphs w horizontal-align padding))))
         (y-offset
          (let* ((formula
                  (lambda (glyphs h align padding)
                    (cond
                     ((not glyphs) 0) ;; default
                     ((eq? align 'top)
                      (let ((s0 (- h (cond
                                      ((glC:image? glyphs) (glC:image-h glyphs))
                                      (else (%%ttf:font-height font))))))
                        (if padding (- s0 (vector-ref padding 0)) s0)))
                     ((eq? align 'center)
                      (receive (below above)
                          (cond
                           ((glC:image? glyphs) (values 0 (glC:image-h glyphs)))
                           (else (%%glC:glyphvector-bounds glyphs font)))
                        (let ((h (if (> h 0) h (- above below))))
                          (+ y (/ (- h above) 2)))))
                     ((eq? align 'bottom)
                      (if padding (vector-ref padding 2) 0))
                     (else (NYIE 'guide-label-view)))))
                 (calculation (memoize-last formula eq? eqv? eq? eq?)))
            (lambda () (calculation glyphs h vertical-align padding))))
         (fgcolora
          (let ((calc (macro-memoize:1->1 color-conv equal?) #;(memoize-last color-conv equal?)))
            (lambda () (calc color))))
         (foreground #f))
    (define fixed-draw
      (lambda ()
        (and
         foreground visible ;; TBD: remove visibility?
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
               (x-offset (exact->inexact (x-offset)))
               (y-offset (exact->inexact (y-offset)))
               (foreground foreground))
           (when (glC:image? glyphs) y-offset)
           (lambda () ;; result … TBD: specialize up to nothing…
             ;; if not already in shift mode ... simplicity: a toplevel matrix push
             (glPushMatrix)
             (when shift-0
               (glTranslatef//checks shift-0 shift-1 shift-2))
             (when scale-0
               (glScalef//checks scale-0 scale-1 scale-2))
             (when rot-0
               (glRotatef//checks rot-0 rot-1 rot-2 rot-3))
             (cond
              ((procedure? foreground)
               (unless (and (eqv? x-offset 0) (eqv? y-offset 0))
                 (glTranslatef//checks x-offset y-offset 0.))
               (foreground))
              (foreground
               (unless (and (eqv? x-offset 0) (eqv? y-offset 0))
                 (glTranslatef//checks x-offset y-offset 0.))
               (MATURITY+1:glC:render-target-mdv! foreground)))
             ;; eventually pop out
             (glPopMatrix))))))
    (define (text-set! str)
      (set! label str)
      (cond
       ((vector? str)
        (if (eqv? (vector-length str) 0) (set! label #f))
        (set! glyphs (and label font (MATURITY-1:vector->guide-glyphvector str font)))
        (let ((targets
               (and glyphs
                    (let ((w (if padding (- w (+ (vector-ref padding 1) (vector-ref padding 3))) w))
                          (h (if padding (- h (+ (vector-ref padding 0) (vector-ref padding 2))) h)))
                      (MATURITY+1:glC:glyphvector->render00 0 0 w h glyphs (fgcolora))))))
          (set! foreground targets))
        #!void)
       ((or (string? str) (not str))
        (set! glyphs (and label font (utf8string->guide-glyphvector label font)))
        (let ((targets
               (and glyphs
                    (let ((w (if padding (- w (+ (vector-ref padding 1) (vector-ref padding 3))) w))
                          (h (if padding (- h (+ (vector-ref padding 0) (vector-ref padding 2))) h)))
                      (MATURITY+1:glC:glyphvector->render00 0 0 w h glyphs (fgcolora))))))
          (set! foreground targets))
        #!void)
       ((glC:image? str)
        (set! glyphs str)
        (let ((target
               (MACRO-make-glC:vertex-set
                (let ((w (glC:image-w glyphs)) (h (glC:image-h glyphs)))
                  (mdvector-body (make-mdvector-rect-vertices/x0y0x1y1 0 0 w h)))
                (mdvector-body (glC:image-legacy-texcoords glyphs))
                (mdvector-body (fgcolora))
                2 4))
              (texture (glC:image-t glyphs)))
          (set! foreground (lambda () (glC:TextureDrawGlArrays texture target #f #f #f)))))
       (else (error "invalid label" str))))
    (define (ctrl! key more)
      (case key
        ((visible:) ;; TBD: deprecated!
         ;;; factor visibility out! into fg+bg pair instead
         (if (pair? more) (set! visible (car more)) visible))
        ((text: foreground:)
         (if (null? more) label
             (let ((thing (car more)))
               (cond
                ((or (vector? thing) (glC:image? thing) (string? thing))
                 (text-set! thing))
                ((or (procedure? thing) (not thing))
                 (set! label #f)
                 (set! foreground thing))
                (else (error "not a valid foreground" thing))))))
        ((align: horizontal-align:)
         (if (pair? more)
             (set! horizontal-align
                   (case (car more)
                     ((left) 'left) ((right) 'right) ((center) 'center)
                     (else (error "alignment must be either left, right or center" (car more)))))
             horizontal-align))
        ((vertical-align:)
         (if (pair? more)
             (set! vertical-align
                   (case (car more)
                     ((top) 'top) ((bottom) 'bottom) ((center) 'center)
                     (else (error "alignment must be either top, bottom or center" (car more)))))
             vertical-align))
        ((font:)
         (if (pair? more)
             (let ((v (car more)))
               (unless (ln-ttf:font? v) (error "not a font" v))
               (set! font v)
               (text-set! label))
             font))
        ((color:)
         (if (pair? more)
             (begin
               (set! color (car more))
               (text-set! label))
             color))
        ((padding:) ;; TBD: currently only effective for textual labels
         (if (pair? more)
             (begin
               (set! padding (and (car more) (apply %%guide:complete-padding-css 'guide-label-view more)))
               (text-set! label))
             padding))
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
             (lambda ()
               (when visible (let ((draw (fixed-draw))) (if draw (draw)))))
             ;; otherwise resolve all references
             (fixed-draw)))
        (else (error "unhandled" 'label-control key))))
    (MATURITY 2 "fresh label created" loc: 'make-guide-label-view)
    (case-lambda
     (() ;; draw thunk without dynamic dependencies
      (if (%%guide-view-default-dynamic)
          (ctrl! #t '()) ;; full exercise, otherwise optimized case
          (fixed-draw)))
     ((key . more)
      (cond
       ((procedure? key)
        ;; key is a procedure receiving the `ctrl!` procedure
        ;; return value is still experimental
        (let ((update! key)
              (location (if (null? more)
                            (error "dynamic drawing requires call location" key)
                            (car more))))
          (lambda ()
            (MATURITY -1 "dynamic update in label" loc: location)
            (let* ((dead #f)
                   (restricted-ctrl!
                    (lambda (#!optional (key #f) . more)
                      (cond
                       (dead (error "too late to update" 'make-guide-label-view key more))
                       (else (ctrl! key more)))))
                   (drawing (update! restricted-ctrl!)))
              (set! dead #t)
              (cond
               ((procedure? drawing) (drawing))
               ((not drawing))
               (else (error "unexpected" drawing)))))))
       (else (ctrl! key more)))))))

(define make-guide-label-view MATURITY+2:make-guide-label-view)

(define (MATURITY+1:make-guide-figure-view)
  (define (color-conv color)
    (cond ;; TBD: define consistent conversion here!
     ((integer? color) (make-rect-single-color-array color))
     ((not color) guide-color-transparent+black-array)
     (else color)))
  (let* ((foreground #f)
         (texture (%%glCore:textures-ref 0 #f))
         (texcoords (make-legacy-texcoords 0. 1. .7734375 .375)) ;; button style
         (color (guide-select-color-4))
         (x 0)
         (y 0)
         (w 100)
         (h 100)
         (scale #f)
         (shift #f)
         (rot #f)
         (visible #t) ;; TBD: deprecated! use fg+bg pair instead!
         (bgcolora
          (let ((calc (macro-memoize:1->1 color-conv equal?) #;(memoize-last color-conv equal?)))
            (lambda () (calc color))))
         (figure ;; rectangular
          (let* ((formula (lambda (w h) (make-mdvector-rect-vertices/x0y0x1y1 0 0 w h)))
                 (calc (macro-memoize:2->1 formula eqv? eqv?) #;(memoize-last formula eqv? eqv?)))
            (lambda () (calc w h))))
         (bgvset
          (let* ((formula
                  (lambda (figure texcoords colors)
                    (MACRO-make-glC:vertex-set
                     (mdvector-body figure)
                     (mdvector-body texcoords)
                     (mdvector-body colors)
                     2 4)))
                 (calc (memoize-last formula eq? eq? eq?)))
            (lambda () (and texture (calc (figure) texcoords (bgcolora)))))))
    (define fixed-draw
      (lambda ()
        (if visible ;; TBD: deprecated! use fg+bg pair instead!
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
                  )
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
                (when (procedure? foreground) (foreground))
                ;; eventually pop out
                (glPopMatrix)))
            (lambda () #f))))
    (define (texture-set! t)
      (define tp t)
      (when (integer? t)
        (MATURITY -1 "outdated call, replacing texture lookup for now" loc: 'texture-set! t)
        (set! t (glCore:textures-ref t t)))
      (unless (glCore:texture? t) (error "invalid argument" 'texture-set! t tp))
      (set! texture t))
    (define (ctrl! key more)
      (case key
        ((visible:) ;; TBD: deprecated!
         ;;; factor visibility out! into fg+bg pair instead
         (if (pair? more) (set! visible (car more)) visible))
        ((foreground:)
         (if (null? more) foreground
             (let ((thing (car more)))
               (cond
                ((or (procedure? thing) (not thing))
                 (set! foreground thing))
                (else (error "not a valid foreground" thing))))))
        ((color:)
         (if (pair? more)
             (set! color (car more))
             color))
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
        ((background:)
         (if (null? more) background
             (let ((thing (car more)))
               (cond
                ((not thing) (set! texture #f))
                ((glC:image? thing)
                 (set! texture (glC:image-t thing))
                 (set! texcoords (glC:image-legacy-texcoords thing)))
                ((glCore:texture? thing) (set! texture thing))
                (else (error "not a valid background" thing))))))
        ((size:)
         (if (pair? more)
             (receive (pw ph) (apply values more)
               (set! w pw) (set! h ph))
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
    (MATURITY 2 "fresh button created" loc: 'make-guide-button-view)
    (case-lambda
     (() ;; draw thunk without dynamic dependencies
      (if (%%guide-view-default-dynamic)
          (ctrl! #t '()) ;; full exercise, otherwise optimized:
          (fixed-draw)))
     ((key . more)
      (cond
       ((procedure? key)
        ;; key is a procedure receiving the `ctrl!` procedure
        ;; return value is still experimental
        (let ((update! key)
              (location (if (null? more)
                            (error "dynamic drawing requires call location" key)
                            (car more))))
          (lambda ()
            (MATURITY -1 "dynamic update in button" loc: location)
            (let* ((dead #f)
                   (restricted-ctrl!
                    (lambda (#!optional (key #f) . more)
                      (cond
                       (dead (error "too late to update" 'make-guide-button-view key more))
                       (else (ctrl! key more)))))
                   (drawing (update! restricted-ctrl!)))
              (set! dead #t)
              (cond
               ((procedure? drawing) (drawing))
               ((not drawing))
               (else (error "unexpected" drawing)))))))
       (else (ctrl! key more)))))))

(define make-guide-figure-view MATURITY+1:make-guide-figure-view)

(define make-guide-button-view MATURITY+1:make-guide-figure-view)
