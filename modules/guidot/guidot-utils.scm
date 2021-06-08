(define (guide-event-graphics? event)
  (or
   (eqv? EVENT_MOTION event)
   (eqv? EVENT_BUTTON1UP event)
   (eqv? EVENT_BUTTON1DOWN event)
   (eqv? EVENT_BUTTON2UP event)
   (eqv? EVENT_BUTTON2DOWN event)
   (eqv? EVENT_BUTTON3UP event)
   (eqv? EVENT_BUTTON3DOWN event)
   ;; (eqv? EVENT_MULTITOUCH event) ;; ???
   ))

(define (guide-dispatch/graphics-shift handler x0 y0)
  (lambda (rect payload event x y)
    (cond
     ((guide-event-graphics? event)
      (handler rect payload event (- x x0) (- y y0)))
     (else (handler rect payload event x y)))))

(define (guidot-frame1
;;; FIXME: figure out why (view! position:) does not works as
;;; expected.
         content
         #!key
         (in (current-guide-gui-interval))
         (border-ratio 1/20)
         (color
          (let* ((color (guide-select-color-3))
                 (r (color-red color))
                 (g (color-green color))
                 (b (color-blue color))
                 (a 140))
            (color-rgba r g b a)))
         (background (guide-background default: in: in))
         (name 'guidot-frame))
  (define-values (xsw xno ysw yno) (guide-boundingbox->quadrupel in))
  ;; TBD: Would be better if we could avoid position shifting alltogether!
  (let* ((view! (make-guide-figure-view))
         (width (- xno xsw))
         (height (- yno ysw))
         (table
          (make-guide-table
           (make-mdvector
            (range '#(1 1))
            (vector (lambda (area row col) (content area))))
           in: (make-mdv-rect-interval 0 0 width height)
           border-ratio: border-ratio
           name: name)))
    (view! background: background)
    (view! color: color)
    (view! size: width height)
    (view! position: xsw ysw)
    (view! foreground: (guide-payload-on-redraw table))
    (make-guide-payload
     in: in name: name
     on-redraw: (view!)
     ;; on-any-event: (guide-dispatch/graphics-shift (guide-payload-on-any-event table) xsw ysw)
     on-any-event: (guide-payload-on-any-event table)
     lifespan: 'ephemeral widget: #f)))

(define (guidot-frame
         content
         #!key
         (in (current-guide-gui-interval))
         (border-ratio 1/20)
         (color
          (let* ((color (guide-select-color-3))
                 (r (color-red color))
                 (g (color-green color))
                 (b (color-blue color))
                 (a 140))
            (color-rgba r g b a)))
         (background (guide-background default: in: in))
         (name 'guidot-frame))
  (define-values (xsw xno ysw yno) (guide-boundingbox->quadrupel in))
  (let ((view! (make-guide-figure-view))
        (width (- xno xsw))
        (height (- yno ysw))
        (table
         (make-guide-table
          (make-mdvector
           (range '#(1 1))
           (vector (lambda (area row col) (content area))))
          in: in
          border-ratio: border-ratio
          name: name)))
    (view! background: background)
    (view! color: color)
    (view! size: width height)
    (view! position: xsw ysw)
    (make-guide-payload
     in: in name: name
     on-redraw:
     (let ((bg (view!)) (fg (guide-payload-on-redraw table)))
       (lambda () (bg) (fg)))
     on-any-event: (guide-payload-on-any-event table)
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
