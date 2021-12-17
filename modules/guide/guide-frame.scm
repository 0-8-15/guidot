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
