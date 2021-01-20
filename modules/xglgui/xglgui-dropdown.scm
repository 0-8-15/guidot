DEPRECATED . . . BASICALY GONE

(define (make-selection-payload/dropdown
         rect current-payload selection-interval content-interval font)
  (define (switch-selected-tool! old new)
    (let ((new (guide-payload-ref new #f)))
      (when new (current-payload (new rect content-interval)))))
  (define (tool-selection-draw-choice str)
    (lambda (lg lw x y w h s)
      (if s (glgui:draw-box x y w h (guide-select-color-1)))
      (glgui:draw-text-left (+ x 5) y (- w 10) h str font (guide-select-color-4))))
  (lambda (selected-display visible-tl-options current-colorscheme)
    (define (cb-tool-selection-change gui wgt type x y)
      (kick (selected-display (list-ref (visible-tl-options) (glgui-widget-get gui wgt 'current)))))
    (let* ((xsw (mdvector-interval-lower-bound selection-interval 0))
           (ysw (mdvector-interval-lower-bound selection-interval 1))
           (xno (mdvector-interval-upper-bound selection-interval 0))
           (yno (mdvector-interval-upper-bound selection-interval 1))
           (w (- xno xsw))
           (h (- yno ysw))
           (gui (guide-rectangle-glgui rect))
           (build-content
            (lambda () (map tool-selection-draw-choice (visible-tl-options))))
           (build-current
            (lambda ()
              (let* ((options (visible-tl-options))
                     (n (length options)))
                (if (positive? n)
                    (- n (length (member (selected-display) (visible-tl-options))))
                    -1))))
           (dd-events
            (lambda (rect payload event x y)
              (guide-default-event-dispatch/fallback-to-glgui rect payload (debug 'DD event) x y)))
           (build-dd
            (lambda ()
              (let ((c1 (guide-select-color-1))
                    (c2 (guide-select-color-2))
                    (c3 (guide-select-color-3)))
                (let ((dd (glgui-dropdownbox gui xsw ysw w h (build-content) c1 c2 c3)))
                  (glgui-widget-set! gui dd 'current (build-current))
                  (glgui-widget-set! gui dd 'callback cb-tool-selection-change)
                  (make-guide-payload
                   on-any-event: dd-events
                   in: selection-interval widget: dd lifespan: 'ephemeral)))))
           (dd (guide-make-payload rect "dd"))
           (events
            (lambda (rect payload event x y)
              (debug 'event event)
              (cond
               (#t
                (guide-event-dispatch-to-payload rect (dd) event x y))
               #;((interval-contains-multi-index? content-interval x y)
                (guide-default-event-dispatch rect (current-payload) (debug 'content event) x y))
               (else #f))))
           (update-dd
            (lambda ()
              (let ((dd (guide-payload-widget (dd))))
                (glgui-widget-set! gui dd 'list (build-content))
                (glgui-widget-set! gui dd 'current (build-current)))))
           (replace-dd
            (lambda () (dd (build-dd)))))
      (replace-dd)
      (wire! (list selected-display visible-tl-options) post: update-dd)
      (wire! current-colorscheme post: replace-dd)
      (wire! selected-display sequence: switch-selected-tool!)
      (let ((result (make-guide-payload
                     name: 'legacy-dd
                     on-any-event: events
                     in: selection-interval
                     widget: gui
                     lifespan: 'once)))
        (if #t
            result ;; return payload
            (case-lambda ;; return payload selector
             (() (debug 'PL result))
             ((pl) (debug 'ingnored pl))))))))

(define make-tl-selection-payload make-selection-payload/dropdown)
