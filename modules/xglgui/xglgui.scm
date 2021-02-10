;;; glgui overrides and refinements

;;;* glcore

;;;** Strings

(define (%%xglgui:glyphvector-height glyphs font) ;; unused?
  (receive (below above) (guide-glypvector-bounds glyphs)
    (let ((override (MATURITY+1:ln-ttf:font-ref font (char->integer #\|))))
      ;; note: this path is usually always taken
      (when override
        (let ((goy (ttf:glyph-offsety override)))
          (set! above (max (max above goy) above))
          (set! below (min (fx- goy (ttf:glyph-height override)) below)))))
    (+ above below)))

(define (MATURITY+0:glC:draw-text-left x y w h label fnt color) ;; -> #!void
  (MATURITY -2 "WASTEFUL, working, looks correct; mimics behavior" loc: 'glC:draw-text-left)
  (let* ((font (find-font fnt))
         (glyphs (utf8string->guide-glyphvector label font)))
    (and
     glyphs
     (receive (below above) (%%glC:glyphvector-bounds glyphs font)
       (let* ((heff (+ above below))
              (hspace (- (if (> h 0) h heff) heff))
              (centery (+ y (/ hspace 2))))
         (let ((targets (MATURITY+1:glC:glyphvector->render00 x centery w h glyphs color)))
           (if targets
               (MATURITY+1:glC:render-target-mdv! targets)
               (MATURITY -5 "DEV +4: no targets to render" loc: 'draw-text-left))))))))

(define glC:draw-text-left MATURITY+0:glC:draw-text-left)

(set! glgui:draw-text-left glC:draw-text-left) ;; !!! BEWARE! appears to be good enough

(define (MATURITY+0:glC:draw-text-right x y w h label fnt color) ;; -> #!void
  (MATURITY -2 "WASTEFUL, working, looks correct; mimics behavior" loc: 'glC:draw-text-right)
  (let* ((font (find-font fnt))
         (glyphs (utf8string->guide-glyphvector label font)))
    (and
     glyphs
     (receive (below above) (%%glC:glyphvector-bounds glyphs font)
       (let* ((shx (let* ((strw (MATURITY+0:guide-glypvector-width glyphs))
                          (txo (- w strw)))
                     (if (> txo 0) (+ x txo) 0)))
              (heff (+ below above))
              (hspace (- (if (> h 0) h heff) heff))
              (centery (+ (+ y (/ hspace 2)) (* 1/2 below))))
         (let ((targets (MATURITY+1:glC:glyphvector->render00 shx centery w h glyphs color)))
           (if targets
               (MATURITY+1:glC:render-target-mdv! targets)
               (MATURITY -5 "DEV +4: no targets to render" loc: 'draw-text-right))))))))

(define glC:draw-text-right MATURITY+0:glC:draw-text-right)

(set! glgui:draw-text-right glC:draw-text-right) ;; !!! BEWARE! appears to be good enough

(define (MATURITY+0:glC:draw-text-center x y w h label fnt color
                                         #!optional (clipright #f)) ;; -> #!void
  (MATURITY -2 "WASTEFUL, working, looks correct; mimics behavior" loc: 'glC:draw-text-center)
  (let* ((font (find-font fnt))
         (glyphs (utf8string->guide-glyphvector label font)))
    (and
     glyphs
     (receive (below above) (%%glC:glyphvector-bounds glyphs font)
       (let* ((strw (MATURITY+0:guide-glypvector-width glyphs))
              (shx (let ((txo (- w strw)))
                     (if (> txo 0) (+ x (/ txo 2)) 0)))
              (heff (- above below))
              (hspace (- (if (> h 0) h heff) above))
              (centery (+ y (/ hspace 2))))
         (let ((targets (MATURITY+1:glC:glyphvector->render00
                         shx centery w h glyphs color clipright)))
           (if targets
               (MATURITY+1:glC:render-target-mdv! targets)
               (MATURITY -5 "DEV +4: no targets to render" loc: 'draw-text-center))))))))

(define glC:draw-text-center MATURITY+0:glC:draw-text-center)

(set! glgui:draw-text-center glC:draw-text-center) ;; !!! BEWARE! appears to be good enough

;;;*** Strings 1st draft

(define glGui:renderstring
  (let ((target glC:legacy-vertex-set-2d)
        (idx 0))
    (define (color-conv color)
      (cond ;; FIXME define & use consistent conversion
       ((integer? color) (make-rect-single-color-array color))
       ((not color) guide-color-transparent+black-array)
       (else color)))
    (define (renderglyph x y glyph color) ;; NOTE: positions are inexact!
      ;; => x-delta
      (receive (gax target shift) (glC:rederglyph/xy x y glyph color)
        (when target
          (let ((rot #f))
            (glC:TextureDrawGlArrays (ttf:glyph-image glyph) target scale shift rot)))
        gax))
    (define (renderstring x y txt fnt color)
      (set! glC:renderstring-is-active #t)
      (when (exact? y)
        (MATURITY -1 "exact y unexpected" renderstring)
        (set! y (exact->inexact y)))
      (set! color (color-conv color))
      (let ((fnt (find-font fnt)))
        (do ((i 0 (fx+ i 1))
             (x0 (exact->inexact x))
             (ccv (utf8string->u32vector txt)))
            ((fx= i (u32vector-length ccv)))
          (let* ((charcode (u32vector-ref ccv i))
                 (g (MATURITY+1:ln-ttf:font-ref fnt charcode)))
            (if g
                (let ((img (ttf:glyph-image g)))
                  (if img
                      (set! x0 (fl+ x0 (renderglyph x0 y g color)))
                      (log-error "no image for glyph: " charcode g)))
                (log-error "no glyph for charcode: " charcode))))))
    renderstring))

(define $glC:overwrite-renderstring
  (let ((original glgui:renderstring)
        (compiled glGui:renderstring)
        (active glgui:renderstring))
    (case-lambda
     (()
      (cond
       ((eq? active original) #f)
       ((eq? active compiled) 'compiled)
       (else active)))
     ((x)
      (cond
       ((procedure? x) (set! active x))
       ((eq? #t x) (set! active compiled))
       ((eq? #f x) (set! active original))
       (else (error "unhandled" x)))
      (set! glgui:renderstring active)))))

($glC:overwrite-renderstring #t)

;;;* Guide Incubator

(define (guide-textarea-payload
;;; TBD: maybe switch to use glyphvectors
;;;
;;; This is a bit of grown up code; maybe rewrite.
         #!key
         (in (current-guide-gui-interval))
         (font (guide-select-font size: 'medium))
         (horizontal-align 'center)
         (vertical-align 'center)
         (line-height 20)
         (rows 3)
         (cols #f)
         (data (let ((state "n/a")) (case-lambda (() state) ((val) (set! state val)))))
         (size 'small)
         (color (guide-select-color-2))
         (hightlight-color (guide-select-color-4)))
  (unless (and (eqv? (mdvector-interval-lower-bound in 0) 0)
               (eqv? (mdvector-interval-lower-bound in 1) 0))
    (error "area not zero based" guide-textarea-payload))
  (let*
      ((newline-indicator 10)
       (xsw (mdvector-interval-lower-bound in 0))
       (ysw (mdvector-interval-lower-bound in 1))
       (xno (mdvector-interval-upper-bound in 0))
       (yno (mdvector-interval-upper-bound in 1))
       (text-width (- xno xsw))
       ;; (text-height (- yno ysw))
       (cols (or cols
                 ;; FIXME: should use font-width
                 (floor (/ text-width line-height))))

       (row-display-offset 0)

       (value-buffer
        (let ((buffer (make-ggb2d size: rows))
              (source (data)))
          (when (string? source) (set! source (utf8string->u32vector source)))
          (cond
           ((or (ggb2d? source) (u32vector? source) (ggb? source))
            (guide-linebreak-unicodevector! buffer source font text-width))
           (else ;; backward compatible: insert empty row
            (ggb2d-insert-row! buffer)))
          (ggb2d-goto! buffer position: 'absolute row: 1 col: 0)
          buffer))
       (linebreak-again!
        (lambda ()
          (let* ((source0 value-buffer)
                 (source (ggb2d-copy source0))
                 (next-buffer (ggb2d-copy source 0 'point reserve: (ggb2d-length value-buffer)))
                 (row (ggb2d-current-row next-buffer)))
            (when row
              (let ((lalipo (ggb-point (%%ggb2d-row-ref source row))))
                (ggb2d-delete-row! next-buffer -1)
                (ggb2d-goto! source position: 'relative col: 0)
                (guide-linebreak-unicodevector! next-buffer source font text-width)
                (let* ((rlen (ggb2d-length next-buffer))
                       (row (min row (- rlen 1)))
                       (lalipo (min lalipo (ggb-length (%%ggb2d-row-ref next-buffer row)))))
                  (ggb2d-goto! next-buffer position: 'absolute row: (+ row 1) col: lalipo))))
            (set! value-buffer next-buffer))))

       (value-views
        (let ((vec (make-vector rows #f)))
          (do ((i 0 (+ i 1)))
              ((eqv? i rows) vec)
            (let ((label! (make-guide-label-view)))
              (label! horizontal-align: horizontal-align)
              (label! vertical-align: vertical-align)
              (label! font: font)
              (label! color: color)
              (label! size: text-width line-height)
              (label! position: xsw (round (- yno (* (+ i 1) line-height))))
              (label! text: "")
              (vector-set! vec i label!)))))
       (update-value-views!
        (lambda ()
          (let ((vec value-views)
                (used-rows (ggb2d-length value-buffer)))
            (do ((i 0 (+ i 1))
                 (j row-display-offset (fx+ j 1)))
                ((eqv? i rows))
              (let ((label! (vector-ref vec i)))
                (label!
                 text:
                 (if (< j used-rows)
                     (ggb->vector (%%ggb2d-row-ref value-buffer j))
                     "")))))))
       (value-draw #f)
       (fix-value-draw!
        (lambda ()
          (let ((vec (make-vector rows #f)))
            (do ((i 0 (+ i 1)) (j 0))
                ((eqv? i rows)
                 (set! value-draw (%%guide-make-redraw (subvector vec 0 j))))
              (let ((draw ((vector-ref value-views i))))
                (when draw
                  (vector-set! vec j draw)
                  (set! j (+ j 1))))))))

       (current-line-number #f)
       (current-line #f)
       (current-line-length #f)
       (current-line-ends-in-newline #f)
       (update-current-line!
        (lambda ()
          (define row (ggb2d-current-row value-buffer))
          (when (and (not row) (> (ggb2d-length value-buffer) 0))
            (MATURITY -1 "ggb2d before first line" loc: 'guide-textarea-payload)
            (ggb2d-goto! value-buffer position: 'absolute row: 1)
            (set! row (ggb2d-current-row value-buffer)))
          (when row
            (set! current-line-number row)
            (set! current-line (%%ggb2d-row-ref value-buffer current-line-number))
            (set! current-line-length (ggb-length current-line))
            (set! current-line-ends-in-newline
                  (and (> current-line-length 0)
                       (eqv? (ggb-ref current-line (- current-line-length 1))
                             newline-indicator)))
            (when (and current-line-ends-in-newline
                       (eqv? current-line-length (ggb-point current-line)))
              (ggb-goto-left! current-line))
            (let ((line-point (+ current-line-number 1)))
              (cond
               ((or (> line-point rows) (<= line-point row-display-offset))
                (set! row-display-offset (max 0 (- line-point rows)))))))))

       (cursor-draw #f)
       (this-payload #f)
       (update-cursor!
        (lambda ()
          (guide-wakeup!) ;; ensure timely visual response
          (update-current-line!)
          (let* ((line-buffer current-line)
                 (line-point (ggb-point line-buffer))
                 (width-before 0)
                 (width-total
                  (let loop ((w 0) (count 0))
                    (ggb-for-each
                     line-buffer
                     (lambda (i c)
                       (if (eqv? i line-point) (set! width-before w))
                       (let ((glyph (MATURITY+1:ln-ttf:font-ref font c)))
                         (if glyph (set! w (+ w (ttf:glyph-advancex glyph)))))))
                    (when (and (>= w (- text-width 3))
                               ;; The -3 is rather arbitrary - helps
                               ;; to see the cursor but might break
                               ;; the logic, hence we break the loop.
                               (< count 3))
                      (debug 'Libreak (list current-line-number: current-line-number w: w))
                      (linebreak-again!)
                      (update-current-line!)
                      (let ((new-curlinlen (ggb-length current-line)))
                        (when (> line-point new-curlinlen) ;; current word was wrapped
                          (ggb2d-goto! value-buffer position: 'relative row: 1)
                          (ggb2d-goto!
                           value-buffer position: 'absolute
                           col: (- line-point new-curlinlen))
                          (update-current-line!)))
                      (set! line-buffer current-line)
                      (set! line-point (ggb-point line-buffer))
                      (set! width-before 0)
                      (loop 0 (+ count 1)))
                    w))
                 (label! (make-guide-label-view)))
            (begin ;; must come after linebreak-again!
              (update-value-views!)
              (fix-value-draw!))
            (if (eqv? current-line-length line-point)
                (set! width-before width-total))
            (label! horizontal-align: horizontal-align)
            (label! vertical-align: vertical-align)
            (label! font: font)
            (label! color: hightlight-color)
            (label! size: text-width line-height)
            (label!
             position:
             (round (case horizontal-align
                      ((left) (+ xsw width-before))
                      ((center) (+ xsw (* 1/2 (- text-width width-total))))
                      (else (- width-total width-before))))
             (round (+ ysw (* (+ (- (- rows 1) current-line-number) row-display-offset) line-height))))
            (label! text: "|")
            (let ((draw
                   (if #t ;; blink
                       (let ((on (label!)))
                         (lambda ()
                           (let* ((n0 ##now)
                                  (n0f (floor n0)))
                             (when (and (eq? (guide-focus) this-payload)
                                        (< (- n0 n0f) 1/2))
                               (on)))))
                       (label!))))
              (set! cursor-draw draw)))))

       (on-key
        (lambda (p/r key)
          (or
           (eq? press: p/r) ;; ignore press - maybe more
           (let ((value! (vector-ref value-views (- current-line-number row-display-offset))))
             (cond
              ((eqv? key EVENT_KEYRIGHT) (ggb-goto-right! current-line) (update-cursor!))
              ((eqv? key EVENT_KEYLEFT) (ggb-goto-left! current-line) (update-cursor!))
              ((or (eqv? key EVENT_KEYUP) (eqv? key EVENT_KEYDOWN))
               (ggb2d-goto! value-buffer position: 'relative row: (if (eqv? key EVENT_KEYUP) -1 1))
               (update-cursor!))
              ((eqv? key EVENT_KEYHOME) (ggb-goto! current-line 0) (update-cursor!))
              ((eqv? key EVENT_KEYEND) (ggb-goto! current-line current-line-length) (update-cursor!))
              ((eqv? key EVENT_KEYDELETE)
               (let ((line-point (ggb-point current-line)))
                 (cond
                  ((or (eqv? line-point current-line-length)
                       (and current-line-ends-in-newline
                            (eqv? (+ line-point 1) current-line-length)))
                   (cond
                    ;; can't remove beyond last line
                    ((eqv? (ggb2d-current-row value-buffer) (- (ggb2d-length value-buffer) 1)))
                    (else
                     (let ((next (%%ggb2d-row-ref value-buffer (+ current-line-number 1))))
                       (cond
                        (current-line-ends-in-newline
                         (ggb-delete! current-line 1))
                        (else
                         (ggb-goto! next 0)
                         (ggb-delete! next 1)))
                       (ggb-insert-sequence! current-line (ggb->vector next))
                       (ggb-goto! current-line line-point)
                       (ggb2d-delete-row! value-buffer 1)
                       (linebreak-again!)
                       (update-cursor!)))))
                  (else
                   (ggb-delete! current-line 1)
                   (value! text: (ggb->vector current-line))
                   (update-cursor!)))))
              ((eqv? key EVENT_KEYBACKSPACE)
               (cond
                ((eqv? (ggb-point current-line) 0)
                 (cond
                  ((eqv? (ggb2d-current-row value-buffer) 0)) ;; can't remove 1st line
                  (else
                   (let ((before (%%ggb2d-row-ref value-buffer (- current-line-number 1))))
                     (ggb-goto! before (ggb-length before))
                     (ggb-delete! before -1)
                     (let ((col (ggb-point before)))
                       (ggb-insert-sequence! before (ggb->vector current-line))
                       (ggb-goto! before col))
                     (ggb2d-delete-row! value-buffer -1)
                     (update-cursor!)))))
                (else
                 (ggb-delete! current-line -1)
                 (value! text: (ggb->vector current-line))
                 (update-cursor!))))
              ((eqv? key EVENT_KEYENTER)
               (let ((point (ggb-point current-line)))
                 (let ((nextline (make-ggb size: cols)))
                   (ggb-insert-sequence! nextline (ggb->vector current-line point current-line-length))
                   (ggb-goto! nextline 0)
                   (ggb-delete! current-line (- current-line-length point))
                   (ggb-insert! current-line newline-indicator)
                   (ggb->vector current-line)
                   (ggb->vector nextline)
                   (ggb2d-insert-row! value-buffer nextline)))
               (update-cursor!))
              ((char? key)
               (ggb-insert! current-line (char->integer key))
               (when (and (not current-line-ends-in-newline)
                          (eqv? (ggb-point current-line) (ggb-length current-line)))
                 (linebreak-again!))
               (value! text: (ggb->vector current-line))
               (update-cursor!))
              (else (debug "ignored key" (list 'guide-line-input key))))))))
       (redraw! (lambda () (value-draw) (cursor-draw)))
       (events
        (lambda (rect payload event x y)
          (cond
           ((eqv? event EVENT_KEYPRESS)
            (on-key press: (%%guide:legacy-keycode->guide-keycode x)))
           ((eqv? event EVENT_KEYRELEASE)
            (on-key release: (%%guide:legacy-keycode->guide-keycode x)))
           (else (mdvector-rect-interval-contains/xy? in x y))))))
    (update-value-views!)
    (update-current-line!)
    (fix-value-draw!)
    (update-cursor!)
    (values
     (let ((result
            (make-guide-payload
             name: 'guide-textarea-payload in: in widget: #f
             on-redraw: redraw! on-any-event: events lifespan: 'ephemeral)))
       (set! this-payload result)
       result)
     ;; control procedure
     (lambda (key . more)
       (case key
         ((display)
          (let ((port (if (pair? more) (car more) (open-output-string))))
            (unless (output-port? port)
              (error "invalid arguments" 'guide-textarea-payload key more))
            (ggb2d-display-value-on-port
             value-buffer port
             display:
             (lambda (c p)
               (cond ((##fx< c 128)
                      (display (##integer->char c) p))
                     ((##fx< c 2048)
                      (display (##integer->char (##fxior (##fxarithmetic-shift-right c 6) 192)) p)
                      (display (##integer->char (##fxior (##fxand c 63) 128)) p))
                     (else
                      (display (##integer->char (##fxior (##fxarithmetic-shift-right c 12) 224)) p)
                      (display (##integer->char (##fxior (##fxand (##fxarithmetic-shift-right c 6) 63) 128)) p)
                      (display (##integer->char (##fxior (##fxand c 63) 128)) p)))))
            (if (null? more) (get-output-string port))))
         ((text) (ggb2d-copy value-buffer))
         ((text:)
          (let ((value
                 (if (null? more)
                     (error "argument required to keyword 'text:'" 'textarea-payload)
                     (car more))))
            (cond
             ((or (ggb2d? value) (u32vector? value))
              (cond
               ((ggb2d? value) (set! value-buffer value))
               ((u32vector? value)
                (let ((buffer (make-ggb2d size: rows)))
                  (guide-linebreak-unicodevector! buffer value font text-width)
                  (set! value-buffer buffer))))
              (update-value-views!)
              (update-current-line!)
              (fix-value-draw!)
              (update-cursor!))
             (else (error "" 'textarea-payload text: value)))))
         (else (error "invalid command key" 'guide-textarea-payload key)))))))

(define (make-figure-list-payload
         in content
         #!key
         (font #f)
         (action #f) (guide-callback #f)
         (background (%%glCore:textures-ref (glC:image-t %%guide-default-background) #f))
         ;; non-functional; for debugging:
         (name make-figure-list-payload))
  ;; TBD: Option to catch/display errors in handling content events.
  (let* ((content (let ((content (content)))
                    (if (vector? content) content (apply vector content))))
         (len (vector-length content))
         (all (make-vector len #f))
         (sely (mdvector-interval-lower-bound in 1))
         (selh (- (mdvector-interval-upper-bound in 1) sely))
         (selcb
          (lambda (rect payload event x y)
            (when action
              (action
               (floor (/ (- sely y) selh)) ;; selected item
               (let ((xsw (mdvector-interval-lower-bound in 0))
                     (xno (mdvector-interval-upper-bound in 0)))
                 ;; relative width
                 (floor (/ (- x xsw) (- xno xsw))))))
            (when guide-callback (guide-callback rect payload event x y))))
         (redraw
          (lambda ()
            (do ((i 0 (fx+ i 1)))
                ((eqv? i len) #t)
              (let ((payload (vector-ref all i)))
                (and payload
                     (let ((draw (guide-payload-on-redraw payload)))
                       (and draw (draw))))))))
         (events
          (lambda (rect payload event x y)
            ;; TBD: we can't deliver all events everywhere!!!
            (and (let ((n (floor (/ (- sely y) selh))))
                   (and (>= n 0) (< n len)))
                 (do ((i 0 (fx+ i 1)))
                     ((eqv? i len) #t)
                   (let ((payload (vector-ref all i)))
                     (guide-event-dispatch-to-payload rect payload event x y)))))))
    (unless font (set! font (guide-select-font height: selh)))
    (do ((i 0 (fx+ i 1)))
        ((eqv? i len)
         (make-guide-payload
          in: in on-redraw: redraw on-any-event: events
          name: name lifespan: 'ephemeral widget: #f))
      (let* ((label (vector-ref content i))
             (payload
              (guide-button
               in: in label: label font: font background: background
               padding: '#(0 1 0 1)
               position: (vector 0 (- sely (* (+ i 1) selh)))
               vertical-align: 'center
               horizontal-align: 'left
               guide-callback: selcb)))
        (vector-set! all i payload)))))

(define (make-tool-switch-payload/dropdown
         selection options content
         #!key
         (in (guide-payload-measures (if (procedure? content) (content) content)))
         (height 26)
         (dynamic #f)
         (selection-area #|deprecated|# #f)
         ;; non-functional; for debugging:
         (name 'tool-switch-payload/dropdown))
  (define (in-pl? pl x y) ;; TBD: independent, move elsewhere once stable
    (let ((in (guide-payload-measures pl)))
      (and (> x (mdvector-interval-lower-bound in 0))
           (< x (mdvector-interval-upper-bound in 0))
           (> y (mdvector-interval-lower-bound in 1))
           (< y (mdvector-interval-upper-bound in 1)))))
  (let ((selection-area
         (or selection-area ;; garbage in garbage out
             (let* ((x0 (mdvector-interval-lower-bound in 0))
                    (x1 (mdvector-interval-upper-bound in 0))
                    (y0 (mdvector-interval-upper-bound in 1)))
               (make-mdv-rect-interval x0 y0 x1 (+ y0 height)))))
        (selbg (glC:image-t %%guide-default-background))
        (selfnt (guide-select-font height: height)))
    ;; volatile:
    (let* ((active #f) ;; modal: dropdown
           (b1c
            (lambda (rect payload event x y)
              (cond
               (active (set! active #f))
               (else
                (set!
                 active
                 (let ((action (lambda (sel x) (set! active #f) (selection sel))))
                   (make-figure-list-payload selection-area options font: selfnt action: action)))))))
           (b1 (let* ((sel
                       (lambda ()
                         (let ((kind (options)))
                           (cond
                            ((vector? kind) (vector-ref kind (selection)))
                            ((list? kind) (list-ref kind (selection)))
                            (else "error")))))
                       ;; current selected label
                      (curla (if dynamic sel (sel))))
                 (guide-button
                  in: selection-area
                  label: curla
                  font: selfnt
                  background: (%%glCore:textures-ref selbg #f) ;;??? are texture volatile
                  vertical-align: 'center
                  horizontal-align: 'left
                  guide-callback: b1c)))
           (redraw
            (lambda ()
              (guide-event-dispatch-to-payload/redraw (if (procedure? content) (content) content))
              (guide-event-dispatch-to-payload/redraw b1)
              (when active (guide-event-dispatch-to-payload/redraw active))))
           (events
            (let ((d1 (guide-payload-on-any-event b1)))
              (lambda (rect payload event x y)
                ;; TBD: we can't deliver all events everywhere!!!
                (cond
                 (active
                  (unless ((guide-payload-on-any-event active) rect active event x y)
                    (d1 rect payload event x y)
                    (let ((content (if (procedure? content) (content) content)))
                      (when (in-pl? content x y)
                        (set! active #f)
                        (guide-event-dispatch-to-payload rect content event x y)))))
                 (else
                  (d1 rect payload event x y)
                  (guide-event-dispatch-to-payload rect (if (procedure? content) (content) content) event x y)))))))
      (make-guide-payload
       in:
       (make-mdv-rect-interval
        (mdvector-interval-lower-bound in 0)
        (mdvector-interval-lower-bound in 1)
        (mdvector-interval-upper-bound selection-area 0)
        (mdvector-interval-upper-bound selection-area 1))
       name: name widget: #f on-redraw: redraw on-any-event: events lifespan: 'ephemeral))))

;;;* Xglgui

(define Xglgui-font guide-select-font)

(define Xglgui-label
  (let ((orig.glgui-label glgui-label)
        (select-font Xglgui-font))
    (define (label gui x y w h #!key text (size 'small) (color (guide-select-color-4)) (bgcolor #f))
      (let ((font (select-font size: size height: h)))
        (if bgcolor
            (glgui-label gui x y w h text font color bgcolor)
            (glgui-label gui x y w h text font color))))
    label))

(define Xglgui-valuelabel
  (let ((orig.glgui-label glgui-label)
        (select-font Xglgui-font)
        (glgui-label Xglgui-label)
        (label-string (lambda (value) (if (string? value) value (object->string value)))))
    (define (change! gui wgt #!key (value #f))
      (if value (glgui-widget-set! gui wgt 'label (label-string value))))
    (define (label+value
             gui x y w h #!key
             (label "") (label-width 1/2)
             (value "") (input #f)
             (size 'small) (color (guide-select-color-4)) (bgcolor #f))
      (define (delegate-input gui wgt type x y)
        (glgui-widget-set! gui wgt 'focus #f)
        (input gui wgt type x y))
      (let* ((w2 (* w label-width))
             (r (+ x w2)))
        (let ((lbl (glgui-label gui x y w2 h text: (label-string label) size: size color: color bgcolor: bgcolor))
              (wgt (glgui-label gui r y w2 h text: (label-string value) size: size color: color bgcolor: bgcolor)))
          (when (procedure? input)
            (glgui-widget-set! gui wgt 'enableinput #t)
            (glgui-widget-set! gui wgt 'onfocuscb delegate-input)
            (glgui-widget-set! gui lbl 'enableinput #t)
            (glgui-widget-set! gui lbl 'onfocuscb delegate-input))
          (lambda args
            (match
             args
             ((value) (glgui-widget-set! gui wgt 'label (label-string value)))
             ((arg1 arg2 . more) (apply change! gui wgt arg1 arg2 more))
             (X (debug 'Komisch X)))
            (glgui-wakeup!)))))
    label+value))

(define (guide-keypad/numeric #!key (in (current-guide-gui-interval)) (action #f))
  (let ((rng (range '#(3 4)))
        (spec
         (vector
          #\1 #\2 #\3
          #\4 #\5 #\6
          #\7 #\8 #\9
          (list EVENT_KEYBACKSPACE label: (apply make-glC:image glgui_keypad_delete.img))
          #\0
          (list EVENT_KEYENTER label: (apply make-glC:image glgui_keypad_return.img)))))
    (guide-make-keypad in (make-mdvector rng spec) on-key: action)))

(define (guide-keypad/ipv6 #!key (in (current-guide-gui-interval)) (action #f))
  (guide-make-keypad
   in
   (make-mdvector
    (range '#(5 5))
    (vector
     #\0 #\1 #\2 #\3 #\[
     #\4 #\5 #\6 #\7 #\]
     #\8 #\9 #\a #\b #\:
     #\c #\d #\e #\f (list EVENT_KEYBACK #|FIXME: EVENT_KEYBACK is a bad choice|#
                           label: "^G")
     ;; last line
     (list EVENT_KEYLEFT label: "<-")
     (list EVENT_KEYRIGHT label: "->")
     (list EVENT_KEYBACKSPACE label: (apply make-glC:image glgui_keypad_delete.img))
     (list EVENT_KEYENTER label: (apply make-glC:image glgui_keypad_return.img))
     #f
     ))
   on-key: action))

(define (guide-keypad/simplified #!key (in (current-guide-gui-interval)) (action #f))
  (guide-make-keypad
   in
   (make-mdvector
    (range '#(10 4 4))
    (let ((k-shift (list 'shift label: (apply make-glC:image glgui_keypad_shift.img)))
          (k-shift-on (list 'shift label: (apply make-glC:image glgui_keypad_shift_on.img)))
          (k-shift-3 (list 'shift label: "*-+"))
          (k-shift-4 (list 'shift label: "@#$"))
          (k-del (list EVENT_KEYBACKSPACE label: (apply make-glC:image glgui_keypad_delete.img)))
          (k-toggle (list 'toggle label: (apply make-glC:image glgui_keypad_toggle.img)))
          (k-toggle-3 (list 'toggle label: (apply make-glC:image glgui_keypad_toggleChar.img)))
          (k-space (list #\space background: %%guide-default-background background-color: (guide-select-color-2)))
          (k-ret (list EVENT_KEYENTER label: (apply make-glC:image glgui_keypad_return.img))))
      (vector
       ;; Pane I
       #\q #\w #\e #\r #\t #\y #\u #\i #\o #\p
       #\a #\s #\d #\f #\g #\h #\j #\k #\l #\#
       k-shift #f #\z #\x #\c #\v #\b #\n #\m k-del
       k-toggle #f #\,  k-space #f #f #f  #\. k-ret #f
       ;; Pane II
       #\Q #\W #\E #\R #\T #\Y #\U #\I #\O #\P
       #f #\A #\S #\D #\F #\G #\H #\J #\K #\L
       k-shift-on #f  #\Z #\X #\C #\V #\B #\N #\M k-del
       k-toggle #f #\, k-space #f #f #f  #\. k-ret #f
       ;; Pane III
       #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0
       #\@ #\# #\$ #\% #\& #\( #\) #\- #\\       k-del
       k-shift-3 #f #\! #\; #\: #\' #\" #\? #\/  #t
       k-toggle-3 #f  #\, k-space #f #f #f #\. k-ret #f
       ;; Pane IV
       #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0
       #f #f #f #\^ #\[ #\] #\{ #\} #\< #\>
       k-shift-4 #f  #\* #\- #\+ #\= #\_ #\~ #\| k-del
       k-toggle-3 #f #\, k-space #f #f #f  #\. k-ret #t
       )))
   on-key: action))

(define (guide-keypad/default #!key (in (current-guide-gui-interval)) (action #f))
  (guide-make-keypad
   in
   (make-mdvector
    (range '#(10 4 4))
    (let ((k-shift (list 'shift label: (apply make-glC:image glgui_keypad_shift.img)))
          (k-shift-on (list 'shift label: (apply make-glC:image glgui_keypad_shift_on.img)))
          (k-shift-3 (list 'shift label: "*-+"))
          (k-shift-4 (list 'shift label: "@#$"))
          (k-del (list EVENT_KEYBACKSPACE label: (apply make-glC:image glgui_keypad_delete.img)))
          (k-toggle (list 'toggle label: (apply make-glC:image glgui_keypad_toggle.img)))
          (k-toggle-3 (list 'toggle label: (apply make-glC:image glgui_keypad_toggleChar.img)))
          (k-space (list #\space background: %%guide-default-background background-color: (guide-select-color-2)))
          (k-ret (list EVENT_KEYENTER label: (apply make-glC:image glgui_keypad_return.img))))
      (vector
       ;; I
       #\q #\w #\e #\r #\t #\y #\u #\i #\o #\p
       #\a #\s #\d #\f #\g #\h #\j #\k #\l     k-del
       k-shift #f #\z #\x #\c #\v #\b #\n #\m  #t
       k-toggle #f  #\, k-space #f #f #f #\. k-ret #f
       ;; II
       #\Q #\W #\E #\R #\T #\Y #\U #\I #\O #\P
       #\A #\S #\D #\F #\G #\H #\J #\K #\L       k-del
       k-shift-on #f #\Z #\X #\C #\V #\B #\N #\M #t
       k-toggle #f  #\, k-space #f #f #f #\. k-ret #f
       ;; III
       #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0
       #\@ #\# #\$ #\% #\& #\( #\) #\- #\\      k-del
       k-shift-3 #f #\! #\; #\: #\' #\" #\? #\/ #t
       k-toggle-3 #f  #\, k-space #f #f #f #\. k-ret #f
       ;; IV
       #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0
       #\' #\" #\^ #\[ #\] #\{ #\} #\< #\>         k-del
       k-shift-4 #f  #\* #\- #\+ #\= #\_ #\~ #\| #t
       k-toggle-3 #f  #\, k-space #f #f #f #\. k-ret #f
       )))
   on-key: action))

(define (guide-value-edit-dialog
         #!key
         (in (current-guide-gui-interval))
         (font (guide-select-font size: 'medium))
         (label #f) (label-string (lambda (value) (if (string? value) value (object->string value))))
         (line-height 20)
         (keypad guide-keypad/numeric)
         (on-key
          (lambda (p/r key) ;; fit's to numeric keyboard
            (if (eq? press: p/r)
                #f ;; ignore press - maybe more
                (case key
                  ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) key)
                  (else
                   (cond
                    ((eqv? key EVENT_KEYRIGHT) EVENT_KEYRIGHT)
                    ((eqv? key EVENT_KEYLEFT) EVENT_KEYLEFT)
                    ((eqv? key EVENT_KEYBACKSPACE) EVENT_KEYBACKSPACE)
                    ((eqv? key EVENT_KEYENTER) EVENT_KEYENTER)
                    (else (debug 'ignored key) #f)))))))
         (data (let ((state "n/a")) (case-lambda (() state) ((val) (set! state val)))))
         (size 'small)
         (horizontal-align 'center)
         (vertical-align 'center)
         (background %%guide-default-background)
         (background-color
          (let* ((color (guide-select-color-1))
                 (r (color-red color))
                 (g (color-green color))
                 (b (color-blue color))
                 (a 210))
            (color-rgba r g b a)))
         (color (guide-select-color-2))
         (hightlight-color (guide-select-color-4)))
  (let*
      ((xsw (mdvector-interval-lower-bound in 0))
       (ysw (mdvector-interval-lower-bound in 1))
       (xno (mdvector-interval-upper-bound in 0))
       (yno (mdvector-interval-upper-bound in 1))
       (w (- xno xsw))
       (h (- yno ysw))
       (border-width (* h 1/20))
       (line-height+border (+ border-width line-height))
       (background-view
        (let ((bg! (make-guide-figure-view)))
          (bg! background: background)
          (bg! color: background-color)
          (bg! size: w h)
          (bg! position: xsw ysw)
          (bg!)))
       (title
        (and
         label
         (let ((label! (make-guide-label-view)))
           (label! horizontal-align: horizontal-align)
           (label! vertical-align: vertical-align)
           (label! font: font)
           (label! color: color)
           (label! size: w line-height)
           (label! position: xsw (- yno line-height+border))
           (label! text: (label-string label))
           (label!))))
       (title-height (if title line-height+border 0))
       (line (guide-line-input
              in: (make-mdv-rect-interval
                   xsw
                   (round (- yno title-height line-height+border)) xno yno)
              horizontal-align: horizontal-align vertical-align: vertical-align
              font: font size: size line-height: line-height
              color: color hightlight-color: hightlight-color
              data: data))
       (kpd (keypad
             in: (make-x0y0x1y1-interval/coerce xsw ysw xno (round (- yno title-height line-height+border)))
             action:
             (lambda (p/r key)
               (let ((key (if on-key (on-key p/r key) key)))
                 (if key
                     (let ((plx (cond
                                 ((char? key) (char->integer key))
                                 (else key))))
                       (guide-event-dispatch-to-payload in line EVENT_KEYRELEASE plx 0)))))))
       (redraw! ;; FIXME: nested vector drawing handlers should be supported too
        (vector-append
         (if title (vector background-view title) (vector background-view))
         (guide-payload-on-redraw line)
         (vector (guide-payload-on-redraw kpd))))
       (events
        (lambda (rect payload event x y)
          (cond
           ((or (eqv? event EVENT_BUTTON1DOWN) (eqv? event EVENT_BUTTON1UP))
            (cond
             ((guide-payload-contains/xy? kpd x y) (guide-event-dispatch-to-payload rect kpd event x y))))
           ((eqv? event EVENT_KEYPRESS)
            (let ((v (on-key press: (%%guide:legacy-keycode->guide-keycode x))))
              (if v (guide-event-dispatch-to-payload rect line event x y))))
           ((eqv? event EVENT_KEYRELEASE)
            (let ((v (on-key release: (%%guide:legacy-keycode->guide-keycode x))))
              (if v (guide-event-dispatch-to-payload rect line event x y))))
           (else (mdvector-rect-interval-contains/xy? in x y))))))
    (make-guide-payload
     name: 'guide-value-edit-dialog in: in widget: #f
     on-redraw: redraw! on-any-event: events lifespan: 'ephemeral)))

(define (guide-textarea-edit
         #!key
         (in (current-guide-gui-interval))
         (font (guide-select-font size: 'medium))
         (label #f) (label-string (lambda (value) (if (string? value) value (object->string value))))
         label-properties
         (line-height 20)
         (keypad guide-keypad/default)
         (on-key
          (lambda (p/r key)
            (if (eq? press: p/r)
                #f ;; ignore press - maybe more
                key)))
         (data (let ((state "n/a")) (case-lambda (() state) ((val) (set! state val)))))
         (control-receiver #f)
         (rows 3)
         (cols #f)
         (size 'small)
         (horizontal-align 'left)
         (vertical-align 'bottom)
         (background %%guide-default-background)
         (background-color
          (let* ((color (guide-select-color-1))
                 (r (color-red color))
                 (g (color-green color))
                 (b (color-blue color))
                 (a 210))
            (color-rgba r g b a)))
         (color (guide-select-color-2))
         (hightlight-color (guide-select-color-4)))
  (let*
      ((xsw (mdvector-interval-lower-bound in 0))
       (ysw (mdvector-interval-lower-bound in 1))
       (xno (mdvector-interval-upper-bound in 0))
       (yno (mdvector-interval-upper-bound in 1))
       (width (- xno xsw))
       (height (- yno ysw))
       (border-width (* height 1/20))
       (line-height+border (+ border-width line-height))
       (background-view
        (let ((bg! (make-guide-figure-view)))
          (bg! background: background)
          (bg! color: background-color)
          (bg! size: width height)
          (bg! position: xsw ysw)
          (bg!)))
       (title-height (if label line-height+border 0))
       (title
        (and
         label
         (let ((label! (make-guide-label-view)))
           (label! horizontal-align: horizontal-align)
           (label! vertical-align: vertical-align)
           (label! font: font)
           (label! color: color)
           (label! text: (label-string label))
           (when (pair? label-properties)
             (for-each
              (lambda (setting) (apply label! setting))
              label-properties))
           (label! size: width line-height)
           (label! position: xsw (- yno line-height+border))
           (label!))))
       (edit-position
        (let ((yno (ceiling (- yno title-height))))
          (vector xsw (floor (- yno (* rows line-height))))))
       (edit-area (make-mdv-rect-interval 0 0 width (* rows line-height)))
       (lines-control! #f)
       (lines
        (receive
            (pl ctrl)
            (guide-textarea-payload
             in: edit-area
             rows: rows cols: cols
             horizontal-align: horizontal-align vertical-align: vertical-align
             font: font size: size line-height: line-height
             color: color hightlight-color: hightlight-color
             data: data)
          (if control-receiver
              (set! lines-control! (control-receiver ctrl))
              (set! lines-control!
                    (lambda (event x y)
                      (let ((in (data)))
                        (cond
                         ((string? in) (data (ctrl 'display)))
                         (else (data (ctrl 'text))))))))
          pl))
       (edit-area-positioned-draw #f)
       (edit-area-positioned-view
        (let ((view! (MATURITY+2:make-guide-bg+fg-view)))
          (view! foreground: (guide-payload-on-redraw lines))
          (view! size: width (* rows line-height))
          (view! position: (vector-ref edit-position 0) (vector-ref edit-position 1))
          (set! edit-area-positioned-draw (view!))
          view!))
       (kpd (keypad
             ;; FIXME: normalize x/y to be zero based
             in: (make-x0y0x1y1-interval/coerce
                  xsw ysw
                  xno (round (- yno title-height border-width (* rows line-height))))
             action:
             (lambda (p/r key)
               (let ((key (if on-key (on-key p/r key) key)))
                 (if key
                     (let ((plx (cond
                                 ((char? key) (char->integer key))
                                 (else key))))
                       (guide-event-dispatch-to-payload in lines EVENT_KEYRELEASE plx 0)))))))
       (redraw! ;; TBD: nested vector drawing handlers should be supported too - aren't they
        (vector-append
         (if title (vector background-view title) (vector background-view))
         (vector edit-area-positioned-draw (guide-payload-on-redraw kpd))))
       (events
        (lambda (rect payload event x y)
          (cond
           ((or (eqv? event EVENT_BUTTON1DOWN) (eqv? event EVENT_BUTTON1UP))
            ;; FIXME:
            (cond
             ((guide-payload-contains/xy? kpd x y)
              (when (eqv? event EVENT_BUTTON1UP) (guide-focus lines))
              (guide-event-dispatch-to-payload rect kpd event x y))
             ((guide-figure-contains? edit-area-positioned-view x y)
              (when (eqv? event EVENT_BUTTON1UP) (guide-focus lines)))
             ((and lines-control! title (> y (- yno line-height+border)) (eqv? event EVENT_BUTTON1DOWN))
              (lines-control! event x y))))
           ((eqv? event EVENT_KEYPRESS)
            (guide-focus lines) ;; questionable!
            (let ((v (on-key press: (%%guide:legacy-keycode->guide-keycode x))))
              (if v (guide-event-dispatch-to-payload rect lines event x y))))
           ((eqv? event EVENT_KEYRELEASE)
            (let ((v (on-key release: (%%guide:legacy-keycode->guide-keycode x))))
              (if v (guide-event-dispatch-to-payload rect lines event x y))))
           (else (mdvector-rect-interval-contains/xy? in x y))))))
    (make-guide-payload
     name: 'guide-textarea-edit in: in widget: #f
     on-redraw: redraw! on-any-event: events lifespan: 'ephemeral)))

(define (guide-list-select-payload
         in content
         #!key
         (font #f)
         (action #f) (guide-callback #f)
         (background %%guide-default-background)
         (background-color (guide-select-color-1))
         (color (guide-select-color-2))
         (horizontal-align 'center)
         (vertical-align 'center)
         (line-height 35)
         ;; non-functional; for debugging:
         (name make-list-select-payload))
  (unless font (set! font (guide-select-font height: line-height)))
  (let*
      ((content (let ((content (content)))
                  (if (vector? content) content (apply vector content))))
       (content-offset 0)
       (len (vector-length content))
       (xno (mdvector-interval-upper-bound in 0))
       (xsw (mdvector-interval-lower-bound in 0))
       (yno (mdvector-interval-upper-bound in 1))
       (ysw (mdvector-interval-lower-bound in 1))
       (w (- xno xsw))
       (h (- yno ysw))
       (selh line-height)
       (num-max-visible (floor (/ h line-height)))
       (num-visible (min len num-max-visible))
       (area-visible-width (- xno ysw))
       (area-visible-height (- yno (* num-visible line-height)))
       (area-visible
        (if (> len num-max-visible) in
            (make-x0y0x1y1-interval/coerce xsw (- yno (* num-visible line-height)) xno yno)))
       (background-color-even
        (let* ((color background-color)
               (r (color-red color))
               (g (color-green color))
               (b (color-blue color))
               (a (color-alpha color)))
          (define (conv x)
            (round (if (> x 127) (/ x 2) (/ (- 255 x) 2))))
          (color-rgba (conv r) (conv g) (conv b) a)))
       (visible-labels-bg (make-vector num-visible #f))
       (draw-labels
        (lambda ()
          (do ((i 0 (fx+ i 1)))
              ((eqv? i num-visible) #t)
            (let ((payload (vector-ref visible-labels-bg i)))
              (and payload (payload))))))
       (visible-labels
        (let ((vec (make-vector (* 2 num-visible) #f)))
          (do ((i 0 (fx+ i 1)))
              ((eqv? i num-visible) vec)
            (let ((label! (make-guide-label-view))
                  (view! (make-guide-figure-view))
                  (y (- area-visible-height (* (fx+ i 1) line-height))))
              (view! background: background)
              (view! size: w line-height)
              (view! position: 0 y)
              (view! color: (if (even? i) background-color-even background-color))
              (label! size: w line-height)
              (label! font: font)
              (label! horizontal-align: horizontal-align)
              (label! vertical-align: vertical-align)
              (label! color: color)
              (view! foreground: (label!))
              (vector-set! vec (+ i num-visible) view!)
              (vector-set! vec i label!)))))
       (update-drawing!
        (lambda ()
          (do ((i 0 (fx+ i 1)))
              ((eqv? i num-visible) #t)
            (let ((label! (vector-ref visible-labels i))
                  (view! (vector-ref visible-labels (+ i num-visible))))
              (when label!
                (view! foreground: (label!))
                (vector-set! visible-labels-bg i (view!)))))))
       (update-content!
        (lambda ()
          (do ((i 0 (fx+ i 1)))
              ((eqv? i num-visible) (update-drawing!))
            ((vector-ref visible-labels i) text: (vector-ref content (+ i content-offset))))
          content-offset))
       (labels-container! (MATURITY+2:make-guide-label-view))
       (visible-frame!
        (let ((frame! (MATURITY+2:make-guide-bg+fg-view))
              (bg! (MATURITY+2:make-guide-label-view))
              (fg! labels-container!))
          (bg! size: area-visible-width area-visible-height)
          (bg! color: background-color)
          (bg! foreground: background)
          (frame! background: (bg!))
          (fg! size: area-visible-width area-visible-height)
          (fg! color: color)
          (fg! foreground: draw-labels)
          (frame! foreground: (fg!))
          (frame! size: area-visible-width area-visible-height)
          (frame! position: 0 (- yno area-visible-height))
          frame!))
       (draw-frame (visible-frame!))
       (y-shift 0)
       (shift!
        (lambda ()
          (define (update-shift!)
            (labels-container! position: 0 y-shift)
            (visible-frame! foreground: (labels-container!))
            (set! draw-frame (visible-frame!)))
          (cond
           ((or (and (eqv? content-offset 0) (< y-shift 0))
                (and (>= (+ num-visible content-offset) len) (> y-shift 0)))
            (set! y-shift 0))
           ((> y-shift line-height)
            (set! y-shift (remainder y-shift line-height))
            (set! content-offset (min (- len num-visible) (fx+ content-offset 2)))
            (update-content!)
            (update-shift!))
           ((< y-shift line-height)
            (set! y-shift (remainder y-shift line-height))
            (set! content-offset (max 0 (fx- content-offset 2)))
            (update-content!)
            (update-shift!)))))
       (redraw (lambda () (draw-frame)))
       (events
        (let ((armed #f) (armed-at #f) (motion-hyst 15))
          (lambda (rect payload event x y)
            (cond
             ((eqv? event EVENT_BUTTON1DOWN)
              (set! armed (vector x y))
              (set! armed-at armed)
              #t)
             ((eqv? event EVENT_BUTTON1UP)
              (cond
               ((eq? armed armed-at)
                (let* ((dx (- x (vector-ref armed 0)))
                       (dy (- y (vector-ref armed 1)))
                       (in-sel (floor (/ (- yno y) selh))))
                  (when action
                    (action
                     (+ content-offset in-sel) ;; selected item
                     (/ (- x xsw) (- xno xsw)))) ;; relative width
                  (when guide-callback (guide-callback rect payload event x y)))))
              (set! armed #f)
              (set! armed-at #f)
              #t)
             ((eqv? event EVENT_MOTION)
              (cond
               (armed
                (let* ((dx (- x (vector-ref armed-at 0)))
                       (dy (- y (vector-ref armed-at 1))))
                  (when (and (eq? armed armed-at)
                             (> (sqrt (+ (* dx dx) (* dy dy))) ;; distance
                                motion-hyst))
                    (set! armed y-shift))
                  (when (number? armed)
                    (set! y-shift (+ armed dy))
                    (shift!))))
               (else #t)))
             (else (debug 'guide-list-select-payload (list event x y)))))))
       (events-here
        (lambda (rect payload event x y)
          (cond
           ((guide-payload-contains/xy? payload x y)
            (events rect payload event x y))
           (else
            #;(MATURITY
            -1 "NOT handling event outside, TBD: don't pass here!"
            loc: 'make-figure-list-payload2)
            #f)))))
    (update-content!)
    (make-guide-payload
     in: area-visible on-redraw: redraw on-any-event: events-here
     name: name lifespan: 'ephemeral widget: #f)))

(define (guide-ggb-layout area buffer #!key (direction 0) (on-key #f))
  (unless (ggb? buffer) (error "arg1 ggb expected" 'guide-ggb-layout buffer))
  (let ((direction ;; direction: 0: z, 1: x, y: z...
         (case direction
           ((0 1 2) direction)
           ((layer) 0)
           ((horizontal) 1)
           ((vertical) 2)
           (else (error "unknown direction" 'guide-ggb-layout direction))))
        (lower-left-x (mdvector-interval-lower-bound area 0))
        (lower-left-y (mdvector-interval-lower-bound area 1)))
    (define (make-drawing)
      (let ((offset 0)
            (result (make-vector (ggb-length buffer) #f)))
        (ggb-for-each
         buffer
         (lambda (i v)
           (when (guide-payload? v)
             (let* ((interval (guide-payload-measures v))
                    (xsw (mdvector-interval-lower-bound interval 0))
                    (ysw (mdvector-interval-lower-bound interval 1))
                    (xno (mdvector-interval-upper-bound interval 0))
                    (yno (mdvector-interval-upper-bound interval 1))
                    (width (fx- xno xsw))
                    (height (fx- yno ysw))
                    (view! (make-guide-label-view)))
               ;; (view! size: width height) ;; TBD: Maybe we need this?
               (let ((draw (guide-payload-on-redraw v)))
                 (cond
                  ((procedure? draw) (view! foreground: draw))
                  ((vector? draw) (view! foreground: (%%guide-make-redraw draw)))
                  (else
                   #;(MATURITY -1 "payload has no drawing" log: guide-ggb-layout)
                   #t)))
               (case direction
                 ((0) #f)
                 ((2)
                  (view! position: 0 (+ lower-left-y offset))
                  ;; update running
                  (set! offset (+ offset height)))
                 (else
                  (view! position: (+ lower-left-x offset) lower-left-y)
                  ;; update running
                  (set! offset (+ offset width))))
               (vector-set! result i (view!))))))
        (%%guide-make-redraw result)))
    (define redraw!
      (let ((last-content (ggb->vector buffer))
            (last (make-drawing)))
        (lambda ()
          (let ((changed (not (eqv? (vector-length last-content) (ggb-length buffer)))))
            (unless changed
              (ggb-for-each
               buffer
               (lambda (i v)
                 (unless (eqv? v (vector-ref last-content i))
                   (set! changed #t)))))
            (when changed
              (set! last-content (ggb->vector buffer))
              (set! last (make-drawing))))
          (last))))
    (define (events rect payload event x y)
      (let ((area (guide-payload-measures payload)))
        (cond
         ((not (mdvector-rect-interval-contains/xy? area x y)) #f)
         (else
          (let ((offset 0)
                (hit #f))
            (ggb-for-each
             buffer ;; TBD: this pass is almost cacheable
             (lambda (i v)
               (when (guide-payload? v)
                 (let* ((interval (guide-payload-measures v))
                        (xsw (mdvector-interval-lower-bound interval 0))
                        (ysw (mdvector-interval-lower-bound interval 1))
                        (xno (mdvector-interval-upper-bound interval 0))
                        (yno (mdvector-interval-upper-bound interval 1))
                        (width (fx- xno xsw))
                        (height (fx- yno ysw)))
                   ;; update running
                   (case direction
                     ((2) (set! offset (+ offset height)))
                     ((1) (set! offset (+ offset width))))))))
            (ggb-for-each-rtl
             buffer
             (lambda (i v)
               (when (and (not hit) (guide-payload? v))
                 (cond
                  ((or (eqv? event EVENT_KEYPRESS) (eqv? event EVENT_KEYRELEASE))
                   (and on-key (on-key press: (%%guide:legacy-keycode->guide-keycode x))))
                  (else
                   (let* ((interval (guide-payload-measures v))
                          (xsw (mdvector-interval-lower-bound interval 0))
                          (ysw (mdvector-interval-lower-bound interval 1))
                          (xno (mdvector-interval-upper-bound interval 0))
                          (yno (mdvector-interval-upper-bound interval 1))
                          (width (fx- xno xsw))
                          (height (fx- yno ysw))
                          (x (case direction
                               ((1) (+ lower-left-x (+ width (- x offset))))
                               (else x)))
                          (y (case direction
                               ((2) (- y lower-left-y (- offset height)))
                               (else (- y lower-left-y)))))
                     (when (mdvector-rect-interval-contains/xy? interval x y)
                       (set! hit #t)
                       (guide-event-dispatch-to-payload rect v event x y))
                     ;; update running
                     (case direction
                       ((2) (set! offset (- offset height)))
                       ((1) (set! offset (- offset width))))))))))
            #t)))))
    (make-guide-payload
     in: area name: (vector 'guide-ggb-layout direction)
     widget: #f lifespan: 'ephemeral ;; TBD: change defaults here!
     on-redraw: redraw!
     on-any-event: events)))

(define (Xglgui-select gui x y w h #!key (line-height 20) (color (guide-select-color-1)))
  (let ((font (guide-select-font height: line-height)))
    (lambda (lst continue #!key (permanent #f) #;(longpress #f))
      (let* ((n (length lst))
             (border-width 2)
             (line-height line-height)
             (border-left (/ border-width 2))
             (border-right border-left)
             (element-height (ceiling (+ (* 1.1 line-height) (* 2 border-width))))
             (usable-n 0))
        (define (list-element obj)
          (let ((display-string (if (string? obj) obj (object->string obj))))
            (lambda (g wgt x y w h selected)
              (glgui:draw-text-left
               (+ x border-left) (- y border-width)
               (- w border-right) (- h 1 (* 2 border-width))
               display-string font color))))
        (let ((wgt (glgui-list gui x y w h element-height '() #f))
              (offset 0))
          (define (del) (glgui-widget-delete gui wgt))
          (define (set-content . new)
            (if (pair? new)
                (let ((new (car new)))
                  (set! lst new)
                  (set! n (length lst))
                  (set! offset 0)
                  (set! usable-n (min (inexact->exact (floor (/ h element-height))) n))))
            (let* ((up (> offset 0))
                   (down (> (- n offset) (- usable-n (if up 1 0))))
                   (visible (- usable-n (if up 1 0) (if down 1 0)))
                   (part (take (list-tail lst offset) visible))
                   (lst (map list-element
                             (let ((tl (if down (append part (list "down")) part)))
                               (if up (cons "up" tl) tl)))))
              (glgui-widget-set! gui wgt 'list lst)))
          (define (callback g wgt t x y)
            (let* ((idx (glgui-widget-get g wgt 'current))
                   (up (> offset 0))
                   (down (> (- n offset) (- usable-n (if up 1 0))))
                   (visible (- usable-n (if up 1 0) (if down 1 0)))
                   (key (if up (- idx 1) idx)))
              (cond
               ((and down (eqv? idx (- usable-n 1)))
                (set! offset (min (+ offset visible) (- n visible)))
                (set-content))
               ((and up (eqv? idx 0))
                (set! offset (max (- offset visible) 0))
                (if (eqv? offset 1) (set! offset 0))
                (set-content))
               (else
                (unless permanent (del))
                (continue (fx+ key offset) (/ x w))))))
          (glgui-widget-set! gui wgt 'callback callback)
          #;(when longpress
            (glgui-widget-set! gui wgt 'longpress-callback longpress))
          (glgui-widget-set! gui wgt 'bordercolor (guide-select-color-3))
          (glgui-widget-set! gui wgt 'scrollw 0)
          (let ((cb (lambda (g wgt t x y) (unless permanent (del)))))
            (glgui-widget-set! gui wgt 'callbackbeyond cb))
          (set-content lst)
          (lambda args
            (match
             args
             (('close) (del))
             (('hidden: v) (glgui-widget-set! gui wgt 'hidden (and v #t)) (glgui-wakeup!))
             (('set: new) (set-content new))
             (_ (begin (del) (error "Xglgui-select unhandled arguments" args))))
            (glgui-wakeup!)))))))

(define (Xglgui-button
         ctx x y w h
         #!key
         (font (guide-select-font size: 'medium))
         (label "exit")
         (glgui-callback (lambda (gui wgt type x y) (terminate))))
  (cond
   (#t ;; experimental
    (let ((view! (make-guide-button-view))
          (label! (make-guide-label-view)))
      (label! text: label)
      (label! horizontal-align: 'center)
      (label! vertical-align: 'center)
      (label! font: (find-font font))
      (label! size: w h)
      (view! size: w h)
      (view! position: x y)
      (view! foreground: (label!))
      ;; drop in old scheme for now
      (let ((wgt (glgui-button-string ctx x y w h #f #f glgui-callback))
            (drawing (let ((actually! (view!))) (lambda (g wgt) (actually!)))))
        (glgui-widget-set! ctx wgt 'draw-handle drawing))))
   (else ;; frozen: old version
    (glgui-button-string ctx x y w h label font glgui-callback))))

;; (include "xglgui-dropdown.scm")

;;; END Xglgui
