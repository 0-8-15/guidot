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
         (background (guide-background 'default))
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
