;;; glgui overrides and refinements

(define-macro (bind-exit x) `(call-with-current-continuation ,x))

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
             (ccv (ggb->vector (utf8string->ggb txt))))
            ((eqv? i (vector-length ccv)))
          (let* ((charcode (vector-ref ccv i))
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

(define %%guide-textarea-keyfilter
  (let ((setting (sre->cset '(or printing (/ #\x7f #\xfffe)))))
    (lambda (p/r key mod)
      (and (eq? release: p/r) ;; ignore press - maybe more
           (or (number? key) ;; legacy special keys
               (cset-contains? setting key))
           key))))

(define-macro (%%guide-post-speculative expr)
  ;; either a thunk or a promise -- promise seems NOT to work under
  ;; gamit?
  `(lambda () ,expr))

(define-macro (macro-guide-sanitize-payload-result expr)
  ;; TBD: sanitize in debug mode only and then maybe use it always.
  (let ((results (gensym 'results))
        (obj (gensym 'obj)))
    `(receive ,results ,expr
       (cond
        ((null? ,results) #t)
        (else
         (let ((,obj (car ,results)))
           (cond
            ((procedure? ,obj) ,obj)
            ((promise? ,obj) ,obj)
            (else #t))))))))

;;** Textarea Payload

(define (guide-textarea-payload
;;; TBD: maybe switch to use glyphvectors
;;;
;;; This is a bit of grown up code; maybe rewrite.
         #!key
         (in (current-guide-gui-interval))
         (horizontal-align 'center)
         (vertical-align 'center)
         (line-height 16)
         (font (guide-select-font height: line-height))
         (rows 3)
         (cols #f)
         (data (let ((state "n/a")) (case-lambda (() state) ((val) (set! state val)))))
         (on-key %%guide-textarea-keyfilter);; filter characters to handle
         (size 'small)
         (color (guide-select-color-2))
         (hightlight-color (guide-select-color-4))
         (results values))
  (define (ggb-delete-word! ggb dir)
    (do ()
        ((let ((point (ggb-point ggb)))
           (or (eqv? point (if dir (ggb-length ggb) 0))
               (not (char-alphabetic? (integer->char (ggb-ref ggb (- point 1))))))))
      (ggb-delete! ggb (if dir 1 -1))))
  (define (ggb-goto-word! ggb dir)
    (do () ;; skip spaces
        ((let ((point (ggb-point ggb)))
           (or (eqv? point 0)
               (char-alphabetic? (integer->char (ggb-ref ggb (- point 1)))))))
      (if dir (ggb-goto-right! ggb) (ggb-goto-left! ggb)))
    (do () ;; skip alphabetics
        ((let ((point (ggb-point ggb)))
           (or (eqv? point 0)
               (not (char-alphabetic? (integer->char (ggb-ref ggb (- point 1))))))))
      (if dir (ggb-goto-right! ggb) (ggb-goto-left! ggb))))
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
          (when (string? source)
            (let ((ggb (utf8string->ggb source)))
              (set! source (make-ggb2d))
              (ggb2d-insert-row! source ggb)
              (ggb2d-goto! source position: 'absolute row: 1 col: 0)))
          (cond
           ((or (ggb2d? source) (u32vector? source))
            (guide-linebreak! buffer source font text-width))
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
                (guide-linebreak! next-buffer source font text-width)
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
              (label! text: '#())
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
            ;; not really bad (MATURITY -1 "ggb2d before first line" loc: 'guide-textarea-payload)
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
               ((<= line-point row-display-offset)
                (set! row-display-offset (max 0 current-line-number)))
               ((> line-point (+ rows row-display-offset))
                (set! row-display-offset (max 0 (- line-point rows)))))))))

       (cursor-draw #f)
       (this-payload #f)
       (cursor-label!
        (let ((label! (make-guide-label-view)))
          (label! horizontal-align: horizontal-align)
          (label! vertical-align: vertical-align)
          (label! font: font)
          (label! color: hightlight-color)
          (label! size: text-width line-height)
          (label! text: '#(124)) ;; "|"
          label!))
       (update-cursor!
        (lambda ()
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
                    (when (and (>= w text-width) (eqv? count 0))
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
                 (label! cursor-label!))
            (begin ;; must come after linebreak-again! and update-current-line!
              (update-value-views!)
              (fix-value-draw!))
            (if (eqv? current-line-length line-point)
                (set! width-before width-total))
            (label!
             position:
             (round (case horizontal-align
                      ((left) (+ xsw width-before))
                      ((center) (+ xsw (* 1/2 (- text-width width-total))))
                      (else (- width-total width-before))))
             (round (+ ysw (* (+ (- (- rows 1) current-line-number) row-display-offset) line-height))))
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
              (set! cursor-draw draw)))
          (guide-wakeup!)
          ;; ensure timely visual response used here in tail position
          ;; of event handlers, signal successfully handled event.
          #t))

       (set-cursor-position!
        (lambda (x y)
          (let* ((in-line (floor (/ (- yno y) line-height)))
                 (line-count (ggb2d-length value-buffer))
                 (target-line-number (max 0 (min (- line-count 1) (+ in-line row-display-offset))))
                 (target-line (%%ggb2d-row-ref value-buffer target-line-number))
                 (target-column
                  (bind-exit
                   (lambda (return)
                     (let ((w 0))
                       (ggb-for-each
                        target-line
                        (lambda (i c)
                          (let ((glyph (MATURITY+1:ln-ttf:font-ref font c)))
                            (if glyph (set! w (+ w (ttf:glyph-advancex glyph)))))
                          (if (< x w) (return i))))
                       (ggb-length target-line))))))
            (ggb2d-goto!
             value-buffer position: 'absolute
             row: (+ target-line-number 1)
             col: target-column)
            (update-cursor!))))

       (handle-key
        (lambda (p/r key mod)
          (or
           (eq? press: p/r) ;; ignore press - maybe more
           (let ((value! (vector-ref value-views (- current-line-number row-display-offset))))
             ;; better we check not to incure side effects during
             ;; speculative execution:
             (check-not-observable-speculative! 'guide-textarea-payload:handle-key) ;; debug
             (cond
              ((eqv? key EVENT_KEYRIGHT)
               (cond
                ((eqv? (bitwise-and MODIFIER_ALT mod) MODIFIER_ALT)
                 (ggb-goto-word! current-line #t))
                ((eqv? (ggb-point current-line)
                       (if current-line-ends-in-newline
                           (- current-line-length 1)
                           current-line-length))
                 (ggb2d-goto! value-buffer position: 'relative row: 1)
                 (ggb2d-goto! value-buffer position: 'absolute col: 0))
                (else (ggb-goto-right! current-line)))
               (update-cursor!))
              ((eqv? key EVENT_KEYLEFT)
               (cond
                ((eqv? (bitwise-and MODIFIER_ALT mod) MODIFIER_ALT)
                 (ggb-goto-word! current-line #f))
                ((eqv? (ggb-point current-line) 0)
                 (ggb2d-goto! value-buffer position: 'relative row: -1 col: 1000))
                (else (ggb-goto-left! current-line)))
               (update-cursor!))
              ((or (eqv? key EVENT_KEYUP) (eqv? key EVENT_KEYDOWN))
               (ggb2d-goto! value-buffer position: 'relative row: (if (eqv? key EVENT_KEYUP) -1 1))
               (update-cursor!))
              ((eqv? key EVENT_KEYHOME) (ggb-goto! current-line 0) (update-cursor!))
              ((eqv? key EVENT_KEYEND) (ggb-goto! current-line current-line-length) (update-cursor!))
              ((or (eq? key 'PageDown) (eq? key 'PageUp))
               (let ((delta
                      (case key
                        ((PageDown) rows)
                        ((PageUp) (- rows))
                        (else 0))))
                 (ggb2d-goto! value-buffer position: 'relative row: delta)
                 (ggb2d-goto! value-buffer position: 'absolute col: 0))
               (update-cursor!))
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
                   (cond
                    ((eqv? (bitwise-and MODIFIER_ALT mod) MODIFIER_ALT)
                     (ggb-delete! current-line 1)
                     (ggb-delete-word! current-line #t))
                    (else (ggb-delete! current-line 1)))
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
                     (cond
                      ((eqv? (bitwise-and MODIFIER_ALT mod) MODIFIER_ALT)
                       (ggb-delete! before -1)
                       (ggb-delete-word! before #f))
                      (else (ggb-delete! before -1)))
                     (let ((col (ggb-point before)))
                       (ggb-insert-sequence! before (ggb->vector current-line))
                       (ggb-goto! before col))
                     (ggb2d-delete-row! value-buffer -1)
                     (update-cursor!)))))
                (else
                 (cond
                  ((eqv? (bitwise-and MODIFIER_ALT mod) MODIFIER_ALT)
                   (ggb-delete! current-line -1)
                   (ggb-delete-word! current-line #f))
                  (else (ggb-delete! current-line -1)))
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
               (value! text: (ggb->vector current-line))
               (update-cursor!))
              (else (debug "ignored key" (list 'guide-textarea-payload key)) #t))))))
       (redraw! (lambda () (value-draw) (cursor-draw)))
       (events
        ;; TBD: factor motion/shift handling out (3rd copy here from
        ;; `select` already).
        (let ((armed #f) (armed-at #f) (motion-hyst 15))
          (lambda (rect payload event x y)
            (cond
             ((or (eqv? press: event) (eqv? release: event))
              (let ((x (on-key event x y)))
                (if x (%%guide-post-speculative (handle-key event x y)) #t)))
             ((eqv? event EVENT_BUTTON1DOWN)
              (set! armed (vector x y))
              (set! armed-at armed)
              #t)
             ((eqv? event EVENT_BUTTON1UP)
              (when (eq? armed armed-at) ;; set cursor position
                (set-cursor-position! x y))
              (set! armed #f)
              (set! armed-at #f)
              (guide-focus this-payload)
              #t)
             ((and armed (eqv? event EVENT_MOTION))
              (let* ((dx (- x (vector-ref armed-at 0)))
                     (dy (- y (vector-ref armed-at 1))))
                (when (and (eq? armed armed-at)
                           (> (sqrt (+ (* dx dx) (* dy dy))) ;; distance
                              motion-hyst))
                  (set! armed row-display-offset))
                (when (number? armed)
                  (let* ((delta (floor (/ dy line-height)))
                         (cr (ggb2d-current-row value-buffer))
                         (new-position
                          (min (max 0 (+ row-display-offset delta))
                               (- (ggb2d-length value-buffer) rows))))
                    (set! row-display-offset new-position)
                    (cond
                     ((< current-line-number row-display-offset)
                      (ggb2d-goto! value-buffer position: 'absolute row: row-display-offset))
                     ((> current-line-number (+ row-display-offset rows))
                      (ggb2d-goto! value-buffer position: 'absolute row: (+ row-display-offset rows)))))
                  (update-cursor!))
                #t))
             (else (mdvector-rect-interval-contains/xy? in x y)))))))
    (update-value-views!)
    (update-current-line!)
    (fix-value-draw!)
    (update-cursor!)
    (results
     (let ((result
            (make-guide-payload
             name: 'guide-textarea-payload in: in widget: #f
             on-redraw: redraw! on-any-event: events lifespan: 'ephemeral)))
       (set! this-payload result)
       result)
     ;; control procedure
     (lambda (key . more)
       (case key
         ((string) (ggb2d->string/encoding-utf8 value-buffer))
         ((text) (ggb2d-copy value-buffer))
         ((text:)
          (check-not-observable-speculative! 'textarea-mutation key more)
          (apply
           (lambda (value #!key (wrap #t) (cols 20))
             (cond
              ((or (ggb2d? value) (u32vector? value))
               (set!
                value-buffer
                (if wrap
                    (let ((buffer (make-ggb2d size: rows)))
                      (when (ggb2d? value) (ggb2d-goto! value position: 'absolute row: 1 col: 0))
                      (guide-linebreak! buffer value font text-width cols: cols)
                      (ggb2d-goto! buffer position: 'absolute row: 1 col: 0)
                      buffer)
                    value))
               (update-cursor!))
              (else (error "unhandled text representation" 'textarea-payload text: value))))
           more))
         ((insert:)
          (check-not-observable-speculative! 'textarea-mutation key more)
          (apply
           (lambda (data)
             (cond
              ((string? data)
               (guide-focus this-payload)
               (continuation-capture
                (lambda (cont)
                  (with-exception-catcher
                   (lambda (exn)
                     (log-error
                      "textarea insert failed:\n"
                      (debug
                       "textarea insert failed\n"
                       (call-with-output-string
                        (lambda (port)
                          (display-exception-in-context exn cont port)
                          (display-continuation-backtrace cont port))))))
                   (lambda ()
                     (call-with-input-string
                      data
                      (lambda (port) (ggb2d-insert-port! value-buffer port)))
                     (update-cursor!))))))
              (else #f)))
           more))
         (else (error "invalid command key" 'guide-textarea-payload key)))))))

;;** Line Input Payload

;;; FIXME: (special case of textarea?)

(define (guide-line-input
         #!key
         (in (current-guide-gui-interval))
         (font (guide-select-font size: 'medium))
         (horizontal-align 'center)
         (vertical-align 'center)
         (line-height 20)
         (data (let ((state "n/a")) (case-lambda (() state) ((val) (set! state val)))))
         (validate #f)
         (size 'small)
         (color (guide-select-color-2))
         (hightlight-color (guide-select-color-4))
         (name 'guide-line-input))
  (define (%%value-buffer->string ggb #!optional (start 0) (end (ggb-length ggb)))
    (let ((result (make-string (- end start))))
      (ggb-for-each ggb (lambda (i v) (string-set! result i (integer->char v))) start end)
      result))
  (define (input->buffer data)
    (utf8string->ggb
     (let ((x (data)))
       (if (string? x) x (object->string x)))))
  (let*
      ((xsw (mdvector-interval-lower-bound in 0))
       (ysw (mdvector-interval-lower-bound in 1))
       (xno (mdvector-interval-upper-bound in 0))
       (yno (mdvector-interval-upper-bound in 1))
       (w (- xno xsw))
       (h (- yno ysw))
       (border-width (* h 1/20))
       (line-height+border (+ border-width line-height))
       (value-buffer (input->buffer data))
       (value!
        (let ((label! (make-guide-label-view)))
          (label! horizontal-align: horizontal-align)
          (label! vertical-align: vertical-align)
          (label! font: font)
          (label! color: color)
          (label! size: w line-height)
          (label! position: xsw (round (+ ysw border-width)))
          (label! text: (%%value-buffer->string value-buffer))
          label!))
       (value-draw (value!))

       (cursor-draw #f)
       (this-payload #f)
       (update-cursor!
        ;; TBD: this is a bit overly simple and needlessly expensive
        ;; at runtime
        (lambda ()
          (let* ((total (%%value-buffer->string value-buffer))
                 (before (%%value-buffer->string value-buffer 0 (ggb-point value-buffer)))
                 (glv-before (MATURITY-1:utf8string->guide-glyphvector before font))
                 (glv-total (MATURITY-1:utf8string->guide-glyphvector total font))
                 (width-before (if glv-before (MATURITY+0:guide-glypvector-width glv-before) 0))
                 (width-total (if glv-total (MATURITY+0:guide-glypvector-width glv-total) 0))
                 (label! (make-guide-label-view)))
            (label! horizontal-align: horizontal-align)
            (label! vertical-align: vertical-align)
            (label! font: font)
            (label! color: hightlight-color)
            (label! size: w line-height)
            (label!
             position:
             (round (+ xsw
                       (case horizontal-align
                         ((left) width-total)
                         ((center) (* 1/2 width-total))
                         (else 0))
                       (- width-before width-total)))
             (round (+ ysw border-width)))
            (label! text: '#(124)) ;; a vertical bar ("|")
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
              (set! cursor-draw draw)))
          (guide-wakeup!)
          ;; no longer: tail position of key handler - return "event handled"
          #t))

       (on-key
        (lambda (p/r key mod)
          (or
           (eq? press: p/r) ;; ignore press - maybe more
           (cond
            ((eqv? key EVENT_KEYRIGHT) (ggb-goto-right! value-buffer) (update-cursor!))
            ((eqv? key EVENT_KEYLEFT) (ggb-goto-left! value-buffer) (update-cursor!))
            ((eqv? key EVENT_KEYBACKSPACE)
             (ggb-delete! value-buffer -1)
             (when (procedure? validate) (validate value-buffer))
             (value! text: (%%value-buffer->string value-buffer))
             (set! value-draw (value!))
             (update-cursor!))
            ((eqv? key EVENT_KEYENTER)
             ;; (data (%%value-buffer->string value-buffer))
             (set! value-buffer (input->buffer data))
             (value! text: (ggb->vector value-buffer))
             (set! value-draw (value!))
             (update-cursor!))
            ((char? key)
             (ggb-insert! value-buffer (char->integer key))
             (when (procedure? validate) (validate value-buffer))
             (value! text: (%%value-buffer->string value-buffer))
             (set! value-draw (value!))
             (update-cursor!))
            (else (debug "ignored key" (list 'guide-line-input key)))))))
       (redraw! (vector
                 (lambda () (and value-draw (value-draw)))
                 (lambda () (cursor-draw))))
       (events
        (lambda (rect payload event x y)
          (case event
            ((press: release:)
             (cond
              ((eqv? x EVENT_KEYENTER)
               (data (%%value-buffer->string value-buffer)) ;; speculative
               (%%guide-post-speculative (on-key event x y)))
              (else (%%guide-post-speculative (on-key event x y)))))
            (else (mdvector-rect-interval-contains/xy? in x y))))))
    (when (procedure? validate) (validate value-buffer))
    (update-cursor!)
    (let ((result ;; a letrec* on `this-payload`
           (make-guide-payload
            name: name in: in widget: #f
            on-redraw: redraw! on-any-event: events lifespan: 'ephemeral)))
      (set! this-payload result)
      result)))

;;*** Figure List Payload

(define (make-figure-list-payload
         in content
         #!key
         (font #f)
         (action #f)
         (guide-callback #f)
         (background (%%glCore:textures-ref (glC:image-t %%guide-default-background) #f))
         ;; non-functional; for debugging:
         (name 'figure-list-payload))
  ;; TBD: merge into successor `list-selection` - anything here we loose
  ;;
  ;; TBD: Option to catch/display errors in handling content events.
  (MATURITY -1 "make-figure-list-payload: was precursor of list-selection"
            loc: make-figure-list-payload)
  (when guide-callback
    (MATURITY -1 "parameter: guide-callback" loc: 'figure-list-payload)
    (when action
      (MATURITY -10 "BOTH GIVEN guide-callback AND action" loc: 'figure-list-payload)))
  (let* ((content (let ((content (content)))
                    (if (vector? content) content (apply vector content))))
         (len (vector-length content))
         (all (make-vector len #f))
         (sely (mdvector-interval-lower-bound in 1))
         (selh (- (mdvector-interval-upper-bound in 1) sely))
         (selcb
          (lambda (rect payload event x y)
            (cond
             (action
              (macro-guide-sanitize-payload-result
               (action
                (floor (/ (- sely y) selh)) ;; selected item
                (let ((xsw (mdvector-interval-lower-bound in 0))
                      (xno (mdvector-interval-upper-bound in 0)))
                  ;; relative width
                  (floor (/ (- x xsw) (- xno xsw)))))))
             (guide-callback
              (macro-guide-sanitize-payload-result
               (guide-callback rect payload event x y)))
             (else #t))))
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
            (cond
             ((or (eqv? press: event) (eqv? release: event)) #t)
             (else
              (and (let ((n (floor (/ (- sely y) selh))))
                     (and (>= n 0) (< n len)))
                   (do ((i 0 (fx+ i 1))
                        (result #f))
                       ((or result (eqv? i len)) result)
                     (let ((payload (vector-ref all i)))
                       (set! result (guide-event-dispatch-to-payload rect payload event x y))))))))))
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

;;*** Drop Down Payload

;; TBD: rename?

(define %%guide-dropdownbox_downarrow.img
  (glCoreTextureCreate
   16 16
   '#u8(
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 63 0 0 0 0 0 0 0 0 0 0 0 0 66 0
        51 210 170 0 0 0 0 0 0 0 0 0 0 168 206 49
        201 214 213 172 3 0 0 0 0 0 0 0 174 215 214 201
        83 218 223 217 179 0 0 0 0 0 0 180 220 219 220 82
        0 68 212 225 226 184 0 0 0 0 187 219 227 210 69 0
        0 0 42 214 232 230 216 44 40 217 230 232 217 42 0 0
        0 0 0 45 219 238 237 218 224 237 234 223 41 0 0 0
        0 0 0 0 32 195 242 240 239 245 195 29 0 0 0 0
        0 0 0 0 0 5 195 252 244 198 0 0 0 0 0 0
        0 0 0 0 0 0 0 203 209 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        )))

(define (make-tool-switch-payload/dropdown
         selection options content
         #!key
         (in (guide-payload-measures (if (procedure? content) (content) content)))
         (height 26)
         (dynamic #f)
         (selection-area #|deprecated|# #f)
         ;; non-functional; for debugging:
         (name 'tool-switch-payload/dropdown))
  (define dropdown.img
    (make-glC:image 16 16 %%guide-dropdownbox_downarrow.img 0. 1. 1.00000000000000000000 0.))
  (define (in-pl? pl x y) ;; TBD: independent, move elsewhere once stable
    (let ((in (guide-payload-measures pl)))
      (and (> x (mdvector-interval-lower-bound in 0))
           (< x (mdvector-interval-upper-bound in 0))
           (> y (mdvector-interval-lower-bound in 1))
           (< y (mdvector-interval-upper-bound in 1)))))
  (unless (and (procedure? selection) (procedure? options))
    (error "invalid arguments" make-tool-switch-payload/dropdown selection options))
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
                 (let ((action (lambda (sel x) (set! active #f) (selection sel) #t)))
                   (make-figure-list-payload selection-area options font: selfnt action: action)))))
              #t))
           (b1 (let* ((sel
                       (lambda ()
                         (let ((kind (options)))
                           (cond
                            ((vector? kind) (vector-ref kind (selection)))
                            ((list? kind) (list-ref kind (selection)))
                            (else "error")))))
                      ;; current selected label
                      (curla (if dynamic sel (sel)))
                      (line-area (make-mdv-rect-interval
                                  0 0
                                  (mdv-rect-interval-width selection-area)
                                  (mdv-rect-interval-height selection-area)))
                      (line (make-ggb size: 2)))
                 (ggb-insert!
                  line
                  (guide-button
                   in: line-area
                   label: curla
                   font: selfnt
                   color: (guide-select-color-1) ;; invert colors
                   background-color: (guide-select-color-2)
                   background: (guide-background 'default in: line-area)
                   vertical-align: 'center
                   horizontal-align: 'left
                   guide-callback: b1c))
                 (ggb-insert!
                  line
                  (guide-button
                   in: line-area
                   label: dropdown.img
                   color: (guide-select-color-1) ;; invert colors
                   background-color: #f
                   background: 'none
                   vertical-align: 'center
                   horizontal-align: 'right
                   guide-callback: b1c))
                 (guide-button
                  in: selection-area
                  label: (guide-ggb-layout selection-area line direction: 'layer)
                  font: selfnt
                  color: (guide-select-color-1) ;; invert colors
                  background-color: (guide-select-color-2)
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
                  (let ((activity-result
                         ((guide-payload-on-any-event active) rect active event x y)))
                    (cond
                     (activity-result activity-result)
                     (else
                      (d1 rect payload event x y)
                      (let ((content (if (procedure? content) (content) content)))
                        (cond
                         ((and (eqv? event EVENT_MOTION) (in-pl? content x y))
                          (set! active #f)
                          (guide-event-dispatch-to-payload rect content event x y))
                         (else #t)))))))
                 ((d1 rect payload event x y) #t) ;; dropdown button hit?
                 (else
                  (guide-event-dispatch-to-payload rect (if (procedure? content) (content) content) event x y)))))))
      (make-guide-payload
       in:
       (make-mdv-rect-interval
        (mdvector-interval-lower-bound in 0)
        (mdvector-interval-lower-bound in 1)
        (mdvector-interval-upper-bound selection-area 0)
        (mdvector-interval-upper-bound selection-area 1))
       name: name widget: #f on-redraw: redraw on-any-event: events lifespan: 'ephemeral))))

;;** Keypad Definitions

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

(define (guide-keypad/de #!key (in (current-guide-gui-interval)) (action #f))
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
          (k-ret (list EVENT_KEYENTER label: (apply make-glC:image glgui_keypad_return.img)))
          (k-eu (list (integer->char 8364) label: '#(8364))) ;; €
          (k-ae (list (integer->char 228) label: '#(228)))
          (k-oe (list (integer->char 246) label: '#(246)))
          (k-ue (list (integer->char 252) label: '#(252)))
          (k-Ae (list (integer->char 196) label: '#(196)))
          (k-Oe (list (integer->char 214) label: '#(214)))
          (k-Ue (list (integer->char 220) label: '#(220)))
          (k-sz (list (integer->char 223) label: '#(223)))
          (k-pg (list (integer->char 167) label: '#(167))))
      (vector
       ;; I   -- lower case characters
       #\q        #\w #\e #\r #\t #\y #\u  #\i  #\o   #\p
       #\a        #\s #\d #\f #\g #\h #\j  #\k  #\l   k-del
       k-shift    #f  #\z #\x #\c #\v #\b  #\n  #\m   #t
       k-toggle   #f  #\, k-space #f #f #f #\.  k-ret #f
       ;; II  -- upper case characters
       #\Q        #\W #\E #\R #\T #\Y #\U  #\I  #\O   #\P
       #\A        #\S #\D #\F #\G #\H #\J  #\K  #\L   k-del
       k-shift-on #f  #\Z #\X #\C #\V #\B  #\N  #\M   #t
       k-toggle   #f  #\, k-space #f #f #f #\.  k-ret #f
       ;; III -- numeric & lower case graphics
       #\1        #\2 #\3 #\4 #\5 #\6 #\7  #\8  #\9   #\0
       #\@        #\# #\$ #\% #\& #\( #\)  k-ue k-sz  k-del
       k-shift-3  #f  #\~ #\null #\" #\; #\:  k-oe k-ae  #t
       k-toggle-3 #f  #\< k-space #f #f #f #\> k-ret #f
       ;; IV  -- upper case other graphics
       #\!        #\" k-pg #\$ #\% #\& #\/ #\(  #\)   #\?
       #\^        #\' k-eu #\[ #\] #\{ #\}  k-Ue #\\  k-del
       k-shift-4  #f  #\* #\- #\+ #\= #\_  k-Oe k-Ae  #t
       k-toggle-3 #f  #\| k-space #f #f #f #\.  k-ret #f
       )))
   on-key: action))

;;** Dialogs

;;*** Value Edit

(define (guide-value-edit-dialog
         #!key
         (in (current-guide-gui-interval))
         (font (guide-select-font size: 'medium))
         (label #f) (label-string (lambda (value) (if (string? value) value (object->string value))))
         (line-height 20)
         (keypad guide-keypad/numeric)
         (on-key
          (lambda (p/r key mod) ;; fit's to numeric keyboard
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
         (validate #f)
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
       (title!
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
           label!)))
       (title (and title! (title!)))
       (title-height (if title line-height+border 0))
       (check-valid
        (and (procedure? validate)
             (lambda (ggb)
               (let ((valid (validate ggb)))
                 (when title
                   (title! color: (if valid hightlight-color color))
                   (set! title (title!)))))))
       (line (guide-line-input
              in: (make-mdv-rect-interval
                   xsw
                   (round (- yno title-height line-height+border)) xno yno)
              horizontal-align: horizontal-align vertical-align: vertical-align
              font: font size: size line-height: line-height
              color: color hightlight-color: hightlight-color
              data: data
              validate: check-valid))
       (kpd (keypad
             in: (make-x0y0x1y1-interval/coerce xsw ysw xno (round (- yno title-height line-height+border)))
             action:
             (lambda (p/r key mod)
               (let ((key (if on-key (on-key p/r key mod) key)))
                 (if key (guide-event-dispatch-to-payload in line p/r key 0) #t)))))
       (redraw! ;; FIXME: nested vector drawing handlers should be supported too
        (vector-append
         (if title
             (vector background-view (if check-valid (lambda () (title)) title))
             (vector background-view))
         (guide-payload-on-redraw line)
         (vector (guide-payload-on-redraw kpd))))
       (this #f)
       (events
        (lambda (rect payload event x y)
          (when (eq? (guide-focus) this) (guide-focus line))
          (cond
           ((or (eqv? event EVENT_BUTTON1DOWN) (eqv? event EVENT_BUTTON1UP))
            (cond
             ((guide-payload-contains/xy? kpd x y)
              (guide-event-dispatch-to-payload rect kpd event x y))
             (else #f)))
           ((or (eqv? press: event) (eqv? release: event))
            (guide-focus line) ;; questionable?
            (let ((v (on-key event x y)))
              (if v
                  (guide-event-dispatch-to-payload rect line event x y)
                  (begin
                    (debug 'value-edit-ignored-key x)
                    #f))))
           (else (mdvector-rect-interval-contains/xy? in x y))))))
    (let ((result
           (make-guide-payload
            name: 'guide-value-edit-dialog in: in widget: #f
            on-redraw: redraw! on-any-event: events lifespan: 'ephemeral)))
      (set! this result)
      result)))

;;*** Text Area

(define (guide-textarea-edit
         #!key
         (in (current-guide-gui-interval))
         (menu #f)
         (label #f) (label-string (lambda (value) (if (string? value) value (object->string value))))
         label-properties
         (line-height 16)
         (font (guide-select-font height: line-height))
         (keypad guide-keypad/default)
         (on-key %%guide-textarea-keyfilter)
         (data (let ((state "n/a")) (case-lambda (() state) ((val) (set! state val)))))
         (results values)
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
       (title-height (if (or menu label) line-height+border 0))
       (title
        (cond
         (label
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
            (label!)))
         (menu
          (let ((label! (make-guide-label-view)))
            (label! position: xsw (- yno line-height+border))
            (when (pair? label-properties)
              (for-each
               (lambda (setting) (apply label! setting))
               label-properties))
            (label! foreground: (guide-payload-on-redraw menu))
            (label!)))
         (else #f)))
       (edit-area-height (* rows line-height))
       (edit-position
        (let ((yno (ceiling (- yno title-height))))
          (vector xsw (floor (- yno (* rows line-height))))))
       (edit-area (make-mdv-rect-interval 0 0 width edit-area-height))
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
             data: data
             on-key: on-key)
          (set! lines-control! ctrl)
          pl))
       (edit-area-positioned-draw #f)
       (edit-area-positioned-view
        (let ((view! (MATURITY+2:make-guide-bg+fg-view)))
          (view! foreground: (guide-payload-on-redraw lines))
          (view! size: width edit-area-height)
          (view! position: (vector-ref edit-position 0) (vector-ref edit-position 1))
          (set! edit-area-positioned-draw (view!))
          view!))
       (kpd (keypad
             ;; FIXME: normalize x/y to be zero based
             in: (make-x0y0x1y1-interval/coerce
                  xsw ysw
                  xno
                  (let ((intented (vector-ref edit-position 1)))
                    (if (> intented line-height) intented (+ ysw line-height))))
             action:
             (lambda (p/r key mod)
               (let ((key (if on-key (on-key p/r key mod) key)))
                 (if key (guide-event-dispatch-to-payload in lines p/r key mod) #t)))))
       (redraw! ;; TBD: nested vector drawing handlers should be supported too - aren't they
        (vector-append
         (if title (vector background-view title) (vector background-view))
         (vector edit-area-positioned-draw (guide-payload-on-redraw kpd))))
       (events
        (lambda (rect payload event x y)
          (cond
           ((or (eqv? event EVENT_BUTTON1DOWN) (eqv? event EVENT_BUTTON1UP)
                (eqv? event EVENT_MOTION))
            ;; FIXME:
            (cond
             ((guide-payload-contains/xy? kpd x y)
              (when (eqv? event EVENT_BUTTON1UP) (guide-focus lines))
              (guide-event-dispatch-to-payload rect kpd event x y))
             ((guide-figure-contains? edit-area-positioned-view x y)
              (let ((x (- x (vector-ref edit-position 0)))
                    (y (- y (vector-ref edit-position 1))))
                (guide-event-dispatch-to-payload rect lines event x y)))
             ((and menu (> y (- yno line-height+border)))
              (guide-event-dispatch-to-payload rect menu event x (- y (- yno line-height+border))))
             ((and lines-control! title (> y (- yno line-height+border)) (eqv? event EVENT_BUTTON1DOWN))
              (lines-control! event x y))
             (else (mdvector-rect-interval-contains/xy? in x y))))
           ((or (eqv? press: event) (eqv? release: event))
            (guide-focus lines) ;; questionable?
            (let ((v (on-key event x y)))
              (if v (guide-event-dispatch-to-payload rect lines event x y) v)))
           (else (mdvector-rect-interval-contains/xy? in x y))))))
    (if (eq? results values)
        (set! lines-control!
              (let ((ctrl lines-control!))
                (MATURITY -1 "obsolete code path taken" loc: guide-textarea-edit)
                (lambda (event x y)
                  (let ((in (data)))
                    (cond
                     ((string? in) (data (ctrl 'string)))
                     (else (data (ctrl 'text)))))))))
    (results
     (make-guide-payload
      name: 'guide-textarea-edit in: in widget: #f
      on-redraw: redraw! on-any-event: events lifespan: 'ephemeral)
     lines-control!)))

;;*** Selection List

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
         (name 'list-select-payload))
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
            (update-shift!)))
          ;; tail in event handler
          #t))
       (redraw (lambda () (draw-frame)))
       (this-payload #f) ;; letrec
       (pointer-event
        (let ((armed #f) (armed-at #f) (motion-hyst 15))
          (lambda (rect payload event x y)
            (cond
             ((eqv? event EVENT_BUTTON1DOWN)
              (set! armed (vector x y))
              (set! armed-at armed)
              (guide-focus this-payload)
              #t)
             ((eqv? event EVENT_BUTTON1UP)
              (cond
               ((eq? armed armed-at)
                (let* ((dx (- x (vector-ref armed 0)))
                       (dy (- y (vector-ref armed 1)))
                       (in-sel (floor (/ (- yno y) selh))))
                  (set! armed #f)
                  (set! armed-at #f)
                  (cond
                   (action
                    (let ((selected-item (+ content-offset in-sel))
                          (relative-width (/ (- x xsw) (- xno xsw))))
                      (macro-guide-sanitize-payload-result
                       (action selected-item relative-width))))
                   (guide-callback
                    (macro-guide-sanitize-payload-result
                     (guide-callback rect payload event x y)))
                   (else #t))))
               (else #f)))
             ((eqv? event EVENT_MOTION)
              (cond
               (armed
                (let* ((dx (- x (vector-ref armed-at 0)))
                       (dy (- y (vector-ref armed-at 1))))
                  (when (and (eq? armed armed-at)
                             (> (sqrt (+ (* dx dx) (* dy dy))) ;; distance
                                motion-hyst))
                    (set! armed y-shift))
                  (cond
                   ((number? armed)
                    (set! y-shift (+ armed dy))
                    (shift!))
                   (else #t))))
               (else #t)))
             (else (debug 'guide-list-select-payload (list event x y)))))))
       (key-event
        (lambda (event key modifier)
          (case key
            ((PageUp PageDown)
             (set! y-shift ((if (eq? key 'PageDown) + -) y-shift (* num-visible line-height)))
             (shift!))
            (else #f))))
       (events-here
        (lambda (rect payload event x y)
          (case event
            ((press: release:)
             (and (eq? (guide-focus) this-payload) (key-event event x y)))
            (else
             (cond
              ((guide-payload-contains/xy? payload x y)
               (pointer-event rect payload event x y))
              (else
               #;(MATURITY
               -1 "NOT handling event outside, TBD: don't pass here!"
               loc: 'make-figure-list-payload2)
               #f)))))))
    (update-content!)
    (let ((result
           (make-guide-payload
            in: area-visible on-redraw: redraw on-any-event: events-here
            name: name lifespan: 'ephemeral widget: #f)))
      (set! this-payload result)
      result)))

(define %%guide-critical-call
  ;; Guide *Global* Critical Section (GGCS)
  ;;
  ;; suspended computation (currently thunk or promise)
  (let ((receiver
         (make-pin
          initial: #f
          pred: (lambda (obj)
                  (or (not obj) (procedure? obj) (promise? obj)))
          ;; FIXME: in LIKELY !: illegal values make it hang!
          filter: (lambda (old new) (and (procedure? new) new))
          name: "GGCS (critical section): a suspended computation")))
    (wire!
     receiver
     extern: receiver
     critical:
     (lambda (new)
       (cond-expand
        (debug
         (check-not-observable-speculative! %%guide-critical-call)
         (check-observable-sequential! %%guide-critical-call))
        (else))
       (cond
        ((not new))
        ((procedure? new) (new))
        (else (force new)))
       ;; important: whatever the operation returns MUST NOT not leak
       #f)
     post: (lambda () (when (receiver) (receiver #f)) #f))
    receiver))

(define (make-chat
         #!key
         (in (current-guide-gui-interval))
         (line-height 20)
         (size 'small)
         (rows 3)
         (font (guide-select-font size: 'small))
         (keypad guide-keypad/default)
         (mode #t)
         (right-side-offset line-height)
         (action #f #;(lambda (msg-ggb2d) #f))
         (results values)
         ) ;; make-chat: end of keyword parameters
  (set! mode
        (case mode
          ((server #t) #t)
          ((client #f) #f)
          (else (error "invalid mode" 'guide-chat mode))))
  (let* ((area in)
         (xno (mdvector-interval-upper-bound area 0))
         (chat-message
          (lambda (data l/r)
            (receive
                (pl ctrl)
                (let ((color-2 (guide-select-color-2))
                      (color-4 (guide-select-color-4))
                      (horizontal-align 'left)
                      (line-count
                       (cond
                        ((ggb2d? data) (ggb2d-length data))
                        ((string? data)
                         ;; BAD: length!
                         (length (string-split data #\newline)))
                        (else 5))))
                  (when l/r
                    (let ((t color-2))
                      (set! color-2 color-4)
                      (set! color-4 t)))
                  (guide-textarea-payload
                   in: (make-mdv-rect-interval 0 0 xno (* line-count line-height))
                   horizontal-align: horizontal-align
                   vertical-align: 'bottom
                   font: font
                   size: size
                   line-height: (* 2/3 line-height)
                   color: color-2 hightlight-color: color-4
                   ;; background: %%guide-default-background
                   data: (lambda _
                           (when (ggb2d? data)
                             (ggb2d-goto! data 'position: 'absolute row: 1 col: 0))
                           data)))
              (guide-button
               in: (guide-payload-measures pl)
               position: (and (not mode) (vector right-side-offset 0))
               guide-callback:
               (lambda _
                 (unless
                     (clipboard-copy
                      (cond
                       ((ggb2d? data) (ggb2d->string data))
                       ((string? data) data)
                       (else (string-append "UNHANDLED: " (object->string data)))))
                   (MATURITY -1 "copying to clipboard failed" loc: 'chat))
                 ;; gui: signal done anyway
                 #t)
               label: pl))))
         ;; -- model
         (messages (make-ggb size: 0))
         (msg
          (let ((state ""))
            (case-lambda
             (() state)
             ((val) #!void))))
         (input-edit #f) ;; catch editor here to enable focus handling
         ;; CONTRUCTOR (lambda (contructor INTERVAL COL ROW . rest) . rest)
         ;; CONTRUCTOR+ARGS: (or CONTRUCTOR (CONTRUCTOR . ARGS))
         (cm (lambda (in col row) (guide-ggb-layout in messages direction: 'vertical)))
         (ce0
          (lambda (in col row)
            (define edit-control!)
            (define (send! . _)
              (cond
               ((procedure? action)
                (let ((data (edit-control! 'text)))
                  (edit-control! text: '#u32())
                  (macro-guide-sanitize-payload-result (action data))))
               (else
                (ggb-goto! messages 0)
                (let ((msg (edit-control! 'text)))
                  (ggb-insert! messages (chat-message msg mode)))
                (edit-control! text: '#u32())
                #t)))
            (define menu
              (let* ((w (mdv-rect-interval-width in))
                     (menu-area (make-mdv-rect-interval 0 0 w line-height))
                     (bw (/ w 4))
                     (menu-color (guide-select-color-4))
                     (button-area (make-mdv-rect-interval 0 0 bw line-height))
                     (menu (make-ggb)))
                (ggb-insert!
                 menu
                 (guide-button
                  in: button-area
                  label: "paste"
                  color: menu-color
                  guide-callback:
                  (lambda _ (%%guide-post-speculative (edit-control! insert: (clipboard-paste))))))
                (ggb-insert!
                 menu
                 (guide-button
                  in: button-area
                  label: "send!"
                  color: menu-color
                  guide-callback: (lambda _ send!)))
                (guide-ggb-layout menu-area menu direction: 'horizontal fixed: #t)))
            (guide-textarea-edit
             in: in
             menu: menu
             keypad: keypad
             data: msg
             horizontal-align: 'left
             label-properties:
             `((color: ,(guide-select-color-4))
               (horizontal-align: right))
             ;; is default: on-key: %%guide-textarea-keyfilter
             rows: rows
             results:
             (lambda (payload ctrl)
               (set! edit-control! ctrl)
               payload))))
         (ce (lambda (in row col)
               (let ((value (ce0 in row col)))
                 (set! input-edit value)
                 value))))
    (results
     (case 1
       ((1)
        (let ((panel (make-ggb size: 2))
              (xno (mdvector-interval-upper-bound area 0))
              (yno (mdvector-interval-upper-bound area 1))
              (ysw (mdvector-interval-lower-bound area 1))
              (kph (* (+ rows 4) line-height)))
          (ggb-insert! panel (ce (make-mdv-rect-interval 0 0 xno kph) 0 0))
          (ggb-insert! panel (cm (make-mdv-rect-interval 0 0 xno (+ (- yno ysw) kph)) 0 0))
          (guide-ggb-layout area panel direction: 'vertical fixed: #t)))
       (else
        (make-guide-table
         (make-mdvector
          (range '#(1 2))
          (vector cm ce))
         in: area name: 'chat
         border-ratio: 0)))
     (lambda (key msg)
       (case key
         ((msg:) ;; add remote message
          (check-not-observable-speculative! 'chat key msg)
          (unless (equal? msg "")
            (ggb-goto! messages 0)
            (ggb-insert! messages (chat-message msg (not mode)))))
         ((focus:)
          (guide-focus (and msg input-edit)))
         ((load:)
          (check-not-observable-speculative! 'chat key msg)
          (ggb-clear! messages)
          (for-each
           (lambda (msg)
             (receive (reference from msg kind) (apply values msg)
               (ggb-insert! messages (chat-message msg (eqv? kind 0)))))
           msg)))))))

;;;* Xglgui

;;; END Xglgui
