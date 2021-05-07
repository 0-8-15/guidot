(json-set-options! keys: #t)

(include "guidot-macros.scm")

(include "guidot-utils.scm")

(include "debugger.scm")

(define (guidot-insert-scheme-interpreter!
         plane ;; GGB (expected to be used as `direction: 'layer`) or control proc
         #!key
         (in (error "required keyword parameter" 'guidot-insert-scheme-interpreter! 'in))
         (done (%%macro-guidot-capture-guide-toplevel))
         (eval eval)
         (read-all read-all)
         (backtrace-on-exception #t)
         ) ;; end of keyword list -- for diffability on a line itself
  (define (add-at-end! pl #!optional (ref #f))
    (cond
     ((ggb? plane)
      (ggb-goto! plane (ggb-length plane))
      (ggb-insert! plane pl))
     ((procedure? plane) (plane top: pl notify: ref))))
  (let*
      ((area in)
       (dialog-area
        (let* ((xsw (mdvector-interval-lower-bound in 0))
               (ysw (mdvector-interval-lower-bound in 1))
               (xno (mdvector-interval-upper-bound in 0))
               (yno (mdvector-interval-upper-bound in 1))
               (w (- xno xsw))
               (h (- yno ysw))
               (border-ratio 3/10))
          (make-x0y0x1y1-interval/coerce
           (+ xsw (* border-ratio w))
           (+ ysw (* border-ratio h))
           (- xno (* border-ratio w))
           (- yno (* border-ratio h)))))
       (timeout
        (make-pin
         initial: 2 ;; +inf.0
         filter:
         (lambda (o n)
           (cond
            ((number? n) n)
            ((string->number n) => (lambda (n) (if (positive? n) n +inf.0)))
            (else o)))))
       (chat-control! #f)
       (output-control! #f)
       (error-control! #f)
       (copy-port-to-textarea!
        (lambda (port area-control!)
          (thread-start!
           (make-thread
            (lambda ()
              (let ((p0 (thread-base-priority (current-thread))))
                (thread-base-priority-set! (current-thread) (+ p0 10)))
              (do ((c (read-char port) (read-char port)))
                  ((eof-object? c))
                (area-control! insert: c)))
            'copy-port-to-textarea))))
       (eval-and-dispatch
        (lambda (input-control! input)
          (let* ((error-port
                  (receive (i o) (open-string-pipe '(direction: input buffering: #f))
                    (error-control! text: #f)
                    (copy-port-to-textarea! i error-control!)
                    o))
                 (output-done #f)
                 (output-port
                  (receive (i o) (open-string-pipe '(direction: input buffering: #f))
                    (output-control! text: #f)
                    (set! output-done (copy-port-to-textarea! i output-control!))
                    o)))
            (continuation-capture
             (lambda (cont)
               (with-exception-catcher
                (lambda (exn)
                  (display-exception-in-context exn cont error-port)
                  (display-continuation-backtrace cont error-port #t #t 10 10 -2))
                (lambda ()
                  (let ((exprs (call-with-input-string input read-all))
                        (vals '()))
                    (let* ((evaluating
                            (make-thread
                             (lambda ()
                               (let ((p0 (thread-base-priority (current-thread))))
                                 (thread-base-priority-set! (current-thread) (- p0 10))
                                 (thread-quantum-set! (current-thread) 0))
                               (parameterize ((current-error-port error-port)
                                              (current-output-port output-port))
                                 (with-exception-catcher
                                  (lambda (exn)
                                    (cond
                                     (backtrace-on-exception
                                      (display-exception-in-context exn cont error-port)
                                      (display-continuation-backtrace cont error-port #t #t 10 10 0))
                                     (else (display-exception exn error-port)))
                                    #f)
                                  (lambda ()
                                    (for-each
                                     (lambda (expr)
                                       (receive res (eval expr) (set! vals res)))
                                     exprs)
                                    #t))))
                             exprs))
                           (aborting
                            (thread-start!
                             (make-thread
                              (lambda ()
                                (let ((p0 (thread-base-priority (current-thread))))
                                  (thread-base-priority-set! (current-thread) (+ p0 10)))
                                (thread-start! evaluating)
                                (thread-sleep! (timeout))
                                (thread-terminate! evaluating))
                              (timeout))))
                           ;; note: I tried to thread-join! with
                           ;; timeout, which would be more logical,
                           ;; but that never kicked in.
                           (success (thread-join! evaluating)))
                      (thread-terminate! aborting)
                      (close-output-port output-port)
                      (thread-join! output-done)
                      (close-output-port error-port)
                      (when success
                        (input-control! text: #f)
                        (chat-control! sent: input)
                        (let ((output (output-control! 'string)))
                          ;; final newline is implicit in GUI and visually disturbing
                          (cond
                           ((and (string-empty? output) (null? vals)))
                           ((or (null? vals) (and (null? (cdr vals)) (eq? (car vals) #!void)))
                            (chat-control!
                             msg:
                             (let ((output (get-output-string results-port)))
                               (if (eqv? (string-ref output (- (string-length output) 1)) #\newline)
                                   (substring output 0 (- (string-length output) 1))
                                   output))))
                           (else
                            (let ((results-port (open-output-string)))
                              (display "results: " results-port)
                              (for-each (lambda (r) (pretty-print r results-port)) vals)
                              (chat-control!
                               msg:
                               (cond
                                ((string-empty? output)
                                 (let ((output (get-output-string results-port)))
                                   (substring output 0 (- (string-length output) 1))))
                                (else
                                 (string-append
                                  output
                                  (if (eqv? (string-ref output (- (string-length output) 1)) #\newline)
                                      "" "\n")
                                  (let ((output (get-output-string results-port)))
                                    (substring output 0 (- (string-length output) 1))))))))))))))))))
            (close-output-port error-port)
            (close-output-port output-port)
            #t)))
       (payload-eval-action
        (lambda (ctrl)
          (let ((input (ctrl 'string)))
            ;; (ctrl text: #f)
            (cond
             ((string-empty? input) #t)
             (else
              (%%guide-post-speculative/async (eval-and-dispatch ctrl input)))))))
       (mkchat
        (lambda (area row col)
          (let* ((font (guide-select-font size: 'small))
                 (menu-height (round (* 8/5 (guide-font-height font))))
                 (area-width (mdv-rect-interval-width area))
                 (menu
                  (let* ((area (make-x0y0x1y1-interval/coerce 0 0 area-width menu-height))
                         (color (guide-select-color-4))
                         (background-color (guide-select-color-3)))
                    (make-guide-table
                     (make-mdvector
                      (range '#(1 1))
                      (vector
                       (let ((label-width 2/3)
                             (label "timeout")
                             (action
                              (lambda _
                                (%%guide-post-speculative;/async
                                 (edit-control! insert: (clipboard-paste))))))
                         (lambda (in row col)
                           (guide-valuelabel
                            name: label
                            in: in size: 'medium label-width: label-width
                            label: label value: timeout
                            value-display: number->string
                            input:
                            (lambda (rect payload event x y)
                              (cond
                               ((eqv? event EVENT_BUTTON1UP)
                                (%%guide-post-speculative
                                 (let ((this (box #f)))
                                   (add-at-end!
                                    (guide-value-edit-dialog
                                     name: label
                                     in: dialog-area label: label
                                     keypad: guide-keypad/numeric
                                     validate:
                                     (macro-guidot-check-ggb/string-pred
                                      (lambda (str)
                                        (let ((n (string->number str)))
                                          (and n (or (eqv? n 0) (positive? n))))))
                                     data:
                                     (case-lambda
                                      (() (timeout))
                                      ((val)
                                       (timeout val)
                                       (cond
                                        ((ggb? plane)
                                         (ggb-delete-first-match!
                                          plane
                                          (lambda (x) (equal? (guide-payload-name x) label))))
                                        ((procedure? plane) (plane close: this)))
                                       #t)))
                                    this))))
                               (else #t))))))))
                     in: area
                     name: 'eval-options
                     border-ratio: 1/10)))
                 (buffer (make-ggb size: 2)))
            (ggb-insert! buffer menu)
            (ggb-insert!
             buffer
             (make-chat
              in: (make-x0y0x1y1-interval/coerce
                   0 0 area-width (- (mdv-rect-interval-height area) menu-height))
              mode: #t
              ;; font: fnt line-height: line-height
              rows: 20
              timestamp: #t
              ;; keypad: keypad
              ;; action: callback
              action: payload-eval-action
              results:
              (lambda (pl ctrl) (set! chat-control! ctrl) pl)))
            (guide-ggb-layout
             area buffer direction: 'topdown
             name: "interpreter input"
             ;; background: (make-glC:image 40 40 0 0. 1. .7734375 .375)
             fixed: #t))))
       (co (lambda (in row col)
             (guide-textarea-payload
              in: in
              data: (lambda _ #f)
              data-char-encoding: #f
              rows: 40
              horizontal-align: 'left
              vertical-align: 'bottom
              readonly: #t
              wrap: #t
              results: (lambda (pl ctrl) (set! output-control! ctrl) pl)
              name: 'expression-output)))
       (ce (lambda (in row col)
             (guide-textarea-payload
              in: in
              data: (lambda _ #f)
              data-char-encoding: #f
              rows: 40
              horizontal-align: 'left
              vertical-align: 'bottom
              readonly: #t
              wrap: #t
              results:
              (lambda (pl ctrl)
                (set! error-control! ctrl)
                pl)
              name: 'expression-errors))))
    (add-at-end!
     (guide-table-layout
      area cols: 2 rows: 2
      name: 'interpreter
      border-ratio: 1/20
      mkchat co
      #t     ce))
    (add-at-end!
     (let ((size 20)
           (xno (mdvector-interval-upper-bound area 0))
           (yno (mdvector-interval-upper-bound area 1)))
       (guide-button
        name: 'close
        in: (make-x0y0x1y1-interval/coerce (- xno size) (- yno size) xno yno)
        label: "x"
        guide-callback: done)))))

(include "fossil-guidot.scm")
