(define guide-drawing->guide-payload
  (let ((area (make-mdv-rect-interval 0 0 1 1))
        (events (lambda (rect payload event x y) #f)))
    (lambda (thunk)
      (make-guide-payload name: 'guide-drawing in: area on-redraw: thunk))))

(define (guide-line-drawing
         vertices lines
         #!key in color (width 1.) (mode GL_LINES))
  (let ((size (exact->inexact width))
        (w (exact->inexact (pikchr-area in 'width)))
        (h (exact->inexact (pikchr-area in 'height)))
        (color-kind
         (cond
          ((u8vector? color) GL_UNSIGNED_BYTE)
          ((u32vector? color) GL_UNSIGNED_BYTE)
          (else #f))))
    (define-values (line-kind nlines)
      (cond
       ((u16vector? lines) (values GL_UNSIGNED_INT (u16vector-length lines)))
       (else (values GL_UNSIGNED_BYTE (u8vector-length lines)))))
    ;; TBD: add checks that no array is out of bounds
    (lambda ()
      (glVertexPointer
       2 ;; #coord per vertex
       GL_FLOAT ;; kind of vertices
       0 ;; stride, no gap
       vertices)
      (cond
       ((not color-kind)
        (glDisableClientState GL_COLOR_ARRAY)
        (glColor color))
       (else (glColorPointer 4 GL_UNSIGNED_BYTE 0 color)))
      (cond
       ((eqv? mode GL_POINTS) (glPointSize width))
       (else (glLineWidth size)))
      (glDisable GL_TEXTURE_2D)
      (glTranslatef//checks (* 0.5 w) (* 0.5 h) 1.) ;;(guide-glCenter in)
      (glScalef//checks w h 1.) ;; (guide-glScale in)
      (glDrawElements mode nlines line-kind lines)
      (glEnableClientState GL_COLOR_ARRAY))))

(define (guide-frame-drawing
         area content-area #!key
         (color ;; maybe a procedure
          (let* ((color (guide-select-color-3))
                 (r (color-red color))
                 (g (color-green color))
                 (b (color-blue color))
                 (a 140))
            (color-rgba r g b a)))
         (background (guide-background default: in: #f)))
  (let* ((top! (make-guide-figure-view))
         (right! (make-guide-figure-view))
         (bottom! (make-guide-figure-view))
         (left! (make-guide-figure-view))
         (all (list top! right! bottom! left!)))
    (for-each
     (lambda (view!)
       (view! color: (cond ((procedure? color) (color)) (else color)))
       (view! background: background))
     all)
    (top! size: (pikchr-area area 'width) (- (pikchr-area area `(- top (@ ,content-area top))) 1))
    (right! size: (- (pikchr-area area `(- right (@ ,content-area right))) 1) (pikchr-area content-area '(+ height 2)))
    (bottom! size: (pikchr-area area 'width) (- (pikchr-area content-area `(- bottom (@ ,area bottom))) 1))
    (left! size: (- (pikchr-area content-area `(- left (@ ,area left))) 1) (pikchr-area content-area '(+ height 2)))
    (top! position: (pikchr-area area 'left) (pikchr-area content-area '(+ top 1)))
    (right! position: (pikchr-area content-area '(+ right 1)) (pikchr-area content-area '(- bottom 1)))
    (bottom! position: (pikchr-area area 'left) (pikchr-area area 'bottom))
    (left! position: (pikchr-area area 'left) (pikchr-area content-area '(- bottom 1)))
    (cond
     ((procedure? color)
      (let ((cached
             (macro-memoize:1->1 ;; memoize-last
              (lambda (value)
                (let ((av (map
                           (lambda (view!)
                             (view! color: value)
                             (view!))
                           all)))
                  (lambda () (for-each (lambda (view!) (view!)) av))))
              eqv?)))
        (lambda () ((cached (color))))))
     (else
      (let ((frozen (map (lambda (view!) (view!)) all)))
        (lambda () (for-each (lambda (view!) (view!)) frozen)))))))
