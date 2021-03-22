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
    (view! size: (- xno xsw) (- yno ysw))
    (view! position: xsw ysw)
    (view! foreground: (guide-payload-on-redraw table))
    (make-guide-payload
     in: in name: name
     on-redraw: (view!) on-any-event: (guide-payload-on-any-event table)
     lifespan: 'ephemeral widget: #f)))

(define (guidot-layers
         area #!key
         (dialog (make-ggb size: 2))
         (results values)
         (name "Guidot Layers"))
  ;; Note: as long as we have a plain ggb as INPUT, there is no way to
  ;; properly lock (using mutices at least).
  (define (push! payload #!key (notify #f))
    (assume (guide-payload? payload) "invalid" name payload)
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

(define (guidot-texteditor-menu
         edit-control
         #!key
         (in (current-guide-gui-interval))
         (font (guide-select-font size: 'medium))
         (color (guide-select-color-4))
         (background-color (guide-select-color-3))
         (border-ratio 1/10)
         (save-label "save")
         (save-callback NYI)
         (reload-label "reload")
         (reload-callback NYI)
         (close-label "X")
         (close-callback NYI)
         (name "text editor menu"))
  (let* ((w (mdv-rect-interval-width in))
         (area (make-mdv-rect-interval 0 0 w (round (* 8/5 (guide-font-height font))))))
    (make-guide-table
     (make-mdvector
      (range '#(5 1))
      (vector
       ;; copy
       (lambda (in row col)
         (guide-button
          in: in
          label: "copy"
          font: font
          color: color
          background-color: background-color
          guide-callback:
          (lambda _
            (%%guide-post-speculative;/async
             (begin
               (clipboard-copy ((edit-control) 'string))
               #t)))))
       ;; paste
       (let ((action
              (lambda _
                (%%guide-post-speculative;/async
                 ((edit-control) insert: (clipboard-paste))))))
         (lambda (in row col)
           (guide-button
            in: in
            label: "paste"
            font: font
            color: color
            background-color: background-color
            guide-callback: action)))
       (lambda (in row col)
         (guide-button
          in: in
          label: save-label
          font: font
          color: color
          background-color: background-color
          guide-callback: save-callback))
       ;; reload
       (lambda (in row col)
         (guide-button
          in: in
          label: reload-label
          font: font
          color: color
          background-color: background-color
          guide-callback: reload-callback))
       ;; close
       (lambda (in row col)
         (guide-button
          in: in
          label: close-label
          font: font
          color: color
          background-color: background-color
          guide-callback: close-callback))))
     in: area
     name: name
     border-ratio: border-ratio)))
