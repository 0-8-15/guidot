(define (guidot-fossil-help-browser area)
  (define xsw (mdvector-interval-lower-bound area 0))
  (define ysw (mdvector-interval-lower-bound area 1))
  (define xno (mdvector-interval-upper-bound area 0))
  (define yno (mdvector-interval-upper-bound area 1))
  (define total-height (mdv-rect-interval-height area))
  (define menu-height 20)
  (define label-width 1/4)
  (define basic (make-pin #t))
  (define menu
    (make-guide-table
     (make-mdvector
      (range '#(1 1))
      (let ((size 'small))
        (vector
         (lambda (area row col)
           (guide-valuelabel
            in: area size: size label: "mode"
            label-width: label-width
            value: basic
            value-display: (lambda (x) (if x "basic" "extended"))
            input:
            (lambda (rect payload event xsw ysw)
              (cond
               ((eqv? event EVENT_BUTTON1DOWN)
                (basic (not (basic)))))
              #t))))))
     in: (make-mdv-rect-interval xsw 0 xno menu-height)))
  (define output-control!)
  (define work-view
    (make-guide-table
     (make-mdvector
      (range '#(3 1))
      (vector
       (let ((basic-options
              (apply
               vector
               (fossil-help-basic-parse-output-to-commands (fossil-command "help"))))
             (all-options
              (apply
               vector
               (fossil-help-all-parse-output-to-commands (fossil-command "help" "-a"))))
             (layers (make-ggb size: 1)))
         (define (options) (if (basic) basic-options all-options))
         (ggb-insert! layers #f)
         (lambda (area row col)
           (define (update-options)
             (ggb-set!
              layers 0
              (guide-list-select-payload
               area options
               line-height: 21
               action:
               (lambda (n x)
                 (%%guide-post-speculative/async
                  (begin
                    (output-control! text: #f)
                    (let ((result (fossil-command "help" (vector-ref (options) n))))
                      (output-control! insert: result))))))))
           (wire! basic post: update-options)
           (update-options)
           (guide-list-select-payload
            area
            options
            action:
            (lambda (n x)
              (%%guide-post-speculative/async
               (begin
                 (output-control! text: #f)
                 (let ((result (fossil-command "help" (vector-ref (options) n))))
                   (output-control! insert: result))))))
           (guide-ggb-layout area layers fixed: #t direction: 'layer name: "fossil help command selection")))
       (let ((label "1-2"))
         (lambda (area row col)
           (guide-textarea-payload
            in: area
            data: (lambda _ #f)
            rows: 120
            horizontal-align: 'left
            vertical-align: 'bottom
            readonly: #t
            wrap: #t
            results: (lambda (pl ctrl) (set! output-control! ctrl) pl)
            name: "fossil output")))
       #f))
     in: (make-mdv-rect-interval xsw ysw xno (- yno menu-height))
     border-ratio: 1/20 name: "Fossil Browser"))
  (define vbuf (make-ggb size: 2))
  (ggb-insert! vbuf menu)
  (ggb-insert! vbuf work-view)
  (guide-ggb-layout area vbuf direction: 'topdown fixed: #t name: "fossil help browser"))
