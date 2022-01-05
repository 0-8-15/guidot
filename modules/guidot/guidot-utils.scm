(define (MATURITY+1:guidot-frame-payload
         area
         content-area #!key
         (color ;; maybe a procedure
          (let* ((color (guide-select-color-3))
                 (r (color-red color))
                 (g (color-green color))
                 (b (color-blue color))
                 (a 140))
            (color-rgba r g b a)))
         (background (guide-background default: in: #f))
         (events (lambda _ #t))
         (name 'guidot-frame-payload))
  (make-guide-payload
   in: area name: name
   on-redraw:
   (guide-frame-drawing area content-area color: color background: background)
   on-any-event:
   (cond
    ((procedure? events) events)
    (else
     (case events
       ;;; Not yet working
       ;;; ((swallow #t) (lambda _ #t))
       ;;; ((opaque pass #f) (lambda _ #f))
       (else (error "invalid value for events:" 'guidot-frame-payload events)))))
   lifespan: 'ephemeral widget: #f))

(define guidot-frame-payload MATURITY+1:guidot-frame-payload)

#;(define (guide-dispatch/graphics-shift handler x0 y0)
  (lambda (rect payload event x y)
    (cond
     ((guide-event-graphics? event)
      (handler rect payload event (- x x0) (- y y0)))
     (else (handler rect payload event x y)))))

(define (guidot-background-frame
         content
         #!key
         (in (current-guide-gui-interval))
         (padding #f)
         (border-ratio 1/20) ;; deprecated, outdated
         (color ;; maybe a procedure
          (let* ((color (guide-select-color-3))
                 (r (color-red color))
                 (g (color-green color))
                 (b (color-blue color))
                 (a 140))
            (color-rgba r g b a)))
         (background (guide-background default: in: in))
         (clip #f)
         (name 'guidot-frame))
  (define-values (xsw xno ysw yno) (guide-boundingbox->quadrupel in))
  (let* ((view! (make-guide-figure-view))
         (pad (%%guide:parse-padding guidot-frame in (or padding border-ratio)))
         (width (- xno xsw))
         (height (- yno ysw))
         (inner
          (content
           (make-x0y0x1y1-interval/coerce
            (+ xsw (vector-ref pad 3))
            (+ ysw (vector-ref pad 2))
            (- xno (vector-ref pad 1))
            (- yno (vector-ref pad 0))))))
    (view! background: background)
    (cond
     ((procedure? color))
     (else (view! color: color)))
    (view! size: width height)
    (view! position: xsw ysw)
    (view! clip: clip)
    (make-guide-payload
     in: in name: name
     on-redraw:
     (let ((bg
            (cond
             ((procedure? color)
              (let ((cached
                     (macro-memoize:1->1 ;; memoize-last
                      (lambda (value)
                        (view! color: value)
                        (view!))
                      eqv?)))
                (lambda () ((cached (color))))))
             (else (view!)))))
       (lambda () (bg) (guide-event-dispatch-to-payload/redraw inner)))
     on-any-event:
     (guide-payload-on-any-event inner)
     #;(lambda (rect payload event x y)
       (cond
        ((and (guide-event-graphics? event)
              (guide-payload-contains/xy? inner x y))
         (guide-payload-on-any-event inner))
        (else #f)))
     lifespan: 'ephemeral widget: #f)))

(define (guidot-frame
         content
         #!key
         (in (current-guide-gui-interval))
         (padding #f)
         (border-ratio 1/20) ;; deprecated, outdated
         (color ;; maybe a procedure
          (let* ((color (guide-select-color-3))
                 (r (color-red color))
                 (g (color-green color))
                 (b (color-blue color))
                 (a 140))
            (color-rgba r g b a)))
         (background (guide-background default: in: in))
         (name 'guidot-frame))
  (let* ((content-area (pikchr-area in `(padding: ,(or padding border-ratio))))
         (inner (content content-area)))
    (make-guide-payload
     in: in name: name
     on-redraw:
     (let ((frame (guide-frame-drawing in content-area color: color background: background)))
       (lambda () (frame) (guide-event-dispatch-to-payload/redraw inner) #;(frame)))
     on-any-event:
     (guide-payload-on-any-event inner)
     #;(lambda (rect payload event x y)
       (cond
        ((and (guide-event-graphics? event)
              (guide-payload-contains/xy? inner x y))
         (guide-payload-on-any-event inner))
        (else #f)))
     lifespan: 'ephemeral widget: #f)))

(define (guidot-layers
         area #!key
         (dialog (make-ggb size: 2))
         (results values)
         (name "Guidot Layers"))
  ;; Note: as long as we have a plain ggb as INPUT, there is no way to
  ;; properly lock (using mutices at least).
  (define (push! payload #!key (notify #f))
    (assume (guide-payload? payload) "invalid layer payload" name payload)
    (check-not-observable-speculative! name key more)
    (ggb-goto! dialog (ggb-length dialog))
    (ggb-insert! dialog payload)
    (cond
     ((box? notify) (set-box! notify payload) payload))
    payload)
  (define (close! obj)
    (cond-expand
     (debug (define before (ggb->vector dialog)))
     (else))
    (check-not-observable-speculative! name key more)
    (cond
     ((guide-payload? obj)
      (ggb-delete-first-match! dialog (lambda (x) (eq? x obj)))
      (cond-expand
       (debug
        (when (eqv? (vector-length before) (ggb-length dialog))
          (MATURITY -1 "layer payload close failed" name obj))
        #t)
       (else #t)))
     ((and (box? obj) (guide-payload? (unbox obj)))
      (let ((payload (unbox obj)))
        (ggb-delete-first-match! dialog (lambda (x) (eq? x payload))))
      (cond-expand
       (debug
        (when (eqv? (vector-length before) (ggb-length dialog))
          (MATURITY -1 "layer payload close failed" name obj))
        #t)
       (else #t)))
     (else (error "invalid layer payload" name key more))))
  (results
   (guide-ggb-layout area dialog direction: 'layer fixed: #t name: name)
   (lambda (key . more)
     (case key
       ((top:)
        (cond
         ((stm-atomic?) (apply push! more))
         (else (guide-critical-add! (lambda () (apply push! more))))))
       ((close:)
        (cond
         ((stm-atomic?) (apply close! more))
         (else (guide-critical-add! (lambda () (apply close! more))))))
       ((content) (ggb->vector dialog))
       (else (error "invalid layer control key" name key more))))))

(define-values (guidot-load-svg-image guidot-make-svg-image)
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
  (define (prepare in img position scale)
    (let* ((iw (nsvgimage-width img))
           (w (if in (pikchr-area in 'width) iw))
           (ih (nsvgimage-height img))
           (h (if in (pikchr-area in 'height) ih))
           (scale
            (cond
             (scale scale)
             ((and (eqv? iw w) (eqv? ih h)) 1.0)
             (else (exact->inexact (min (/ w iw) (/ h ih))))))
           (result (nanosvg-rasterize
                    img w h
                    scale
                    (cond
                     (position (exact->inexact (xof position)))
                     (else (* (- w (* scale iw)) 0.5)))
                    (cond
                     (position (exact->inexact (yof position)))
                     (else (* (- h (* scale ih)) 0.5))))))
      (nanosvg-finalize-image img)
      ;;(values w h result)
      (make-glC:image w h (glCoreTextureCreate w h result) 0. 1. 1. 0.)))
  (define (load-svg-image
           filename #!key
           (units 'px)
           (dpi 96.)
           (in #f)
           (position #f)
           (scale #f))
    (let ((img (nanosvg-load-file filename (symbol->string units) dpi)))
      (cond
       (img (prepare in img position scale))
       (else (error "guidot-load-svg-image failed to load" filename)))))
  (define (make-svg-image
           source #!key
           (units 'px)
           (dpi 96.)
           (in #f)
           (position #f)
           (scale #f))
    (let ((img (make-nanosvg-image source (symbol->string units) dpi)))
      (cond
       (img (prepare in img position scale))
       (else (error "guidot-make-svg-image failed parse" source)))))
  (values load-svg-image make-svg-image))
