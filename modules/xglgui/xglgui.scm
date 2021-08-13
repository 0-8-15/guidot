;;; glgui overrides and refinements

(define-macro (bind-exit x) `(call-with-current-continuation ,x))

(define-macro (define-values names . body)
  (let ((arg1 (gensym 'arg1))
        (rest (gensym 'rest)))
    `(begin
       ,@(map (lambda (name) `(define ,name #f)) (cdr names))
       (define ,(car names)
         (call-with-values
             (lambda () unquote body)
           (lambda (,arg1 . ,rest)
             (begin
               ,@(map (lambda (name)
                        `(set! ,name
                               (let ((,name (car ,rest)))
                                 (set! ,rest (cdr ,rest))
                                 ,name)))
                      (cdr names))
               ,arg1)))))))

;;;* glcore

;;;** Strings

(define (%%xglgui:glyphvector-height glyphs font) ;; unused?
  (receive (below above) (guide-glypvector-bounds glyphs)
    (let ((override (ln-ttf:font-ref font (char->integer #\|))))
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
                 (g (ln-ttf:font-ref fnt charcode)))
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
  ;;
  ;; MUST NOT block, SHOULD RETURN ASAP!
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

(define-macro (macro-guide-execute-payload-result expr)
  ;; TBD: sanitize in debug mode only and then maybe use it always.
  (let ((results (gensym 'results))
        (obj (gensym 'obj)))
    `(receive ,results ,expr
       (let ((,obj (car ,results)))
         (cond
          ((procedure? ,obj) (,obj))
          ((promise? ,obj) (force ,obj)))))))

(define-macro (%%guide-post-speculative/async expr)
  ;; either a thunk or a promise -- promise seems NOT to work under
  ;; gamit?
  ;;
  ;; does not block, returns asap.
  `(%%guide-post-speculative
    (begin
      (thread-start! (make-thread (lambda () (macro-guide-execute-payload-result ,expr))))
      ;; (kick! (box (lambda () (macro-guide-execute-payload-result ,expr))))
      #t)))

;;** guide-transformed-payload

;; Probably better under "composition".

(define (Xguide-transformed-payload
         ;; TBD: may do clipping???
         #!key
         (in (current-guide-gui-interval))
         (content #f)
         (active #t)
         (position
          (vector
           (mdvector-interval-lower-bound in 0)
           (mdvector-interval-lower-bound in 1)))
         (results values)
         (name 'transformed))
  (define (check-zero-based! payload)
    (let ((in (guide-payload-measures payload)))
      (unless (and (eqv? (mdvector-interval-lower-bound in 0) 0)
                   (eqv? (mdvector-interval-lower-bound in 1) 0))
        (error "area not zero based" 'guide-transformed-payload payload))))
  (let ((view! (MATURITY+2:make-guide-label-view))
        (redraw! #f))
    (define (fix-redraw!)
      (view! foreground: (and active content (guide-payload-on-redraw content)))
      (set! redraw! (view!))
      #t)
    (define events
      (lambda (rect payload event x y)
        (cond
         ((not active) #f)
         ((not content) #f)
         ((not (mdvector-rect-interval-contains/xy? in x y))
          ;; clip active region
          #f)
         ((case event ((press: release:) #t) (else #f))
          (guide-event-dispatch-to-payload rect content event x y))
         (else
          (let ((x (- x (vector-ref position 0)))
                (y (- y (vector-ref position 1))))
            (cond
             ((guide-payload-contains/xy? content x y)
              (guide-event-dispatch-to-payload rect content event x y))
             (else #f)))))))
    (define (set-content! obj)
      (when obj (check-zero-based! obj))
      (set! content obj))
    (define (set-position! x y)
      ;; TBD: error checks
      (set! position (vector x y))
      (view! position: x y)
      (fix-redraw!))
    (results
     (let ((payload
            (make-guide-payload
             in: in name: name
             widget: #f
             on-redraw: (lambda () (and redraw! (redraw!)))
             on-any-event: events)))
       (set-content! content)
       (set-position! (vector-ref position 0) (vector-ref position 1))
       (fix-redraw!)
       payload)
     (lambda (key val . more)
       (case key
         ((content:)
          (set-content! val)
          (fix-redraw!))
         ((position:)
          (cond
           ((mdvector-interval? val)
            (set-position!
             (mdvector-interval-lower-bound in 0)
             (mdvector-interval-lower-bound in 1)))
           (else (apply set-position! val more))))
         ((padding: size: rot: scale:)
          (apply view! key val more)
          (fix-redraw!))
         (else (error "unhandled" 'Xguide-transformed-payload key more)))))))

;;** Textarea Payload

(define (guide-textarea-payload
;;; TBD: maybe switch to use glyphvectors
;;;
;;; This is a bit of grown up code; maybe rewrite.
         #!key
         (in (current-guide-gui-interval))
         (horizontal-align 'center)
         (vertical-align 'center)
         (style (guide-current-style))
         (line-height #f)
         (font #f)
         (rows 3)
         (cols #f)
         (wrap #t)
         (data (let ((state #f)) (case-lambda (() state) ((val) (set! state val)))))
         (data-char-encoding 'UTF-8) ;; only applicable if data is string or u8vector
         (readonly #f)
         (on-key %%guide-textarea-keyfilter);; filter characters to handle
         (color (or (guide-style-ref style color:) (guide-select-color-2)))
         (highlight-color (or (guide-style-ref style highlight-color:) (guide-select-color-4)))
         (results values)
         (name 'guide-textarea-payload))
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
  ;; complete missing defaults
  (cond
   ((and line-height font)) ;; yield to use case
   (line-height (set! font (guide-select-font height: line-height)))
   (font (set! line-height (guide-font-height font)))
   (else
    (MATURITY -1 "at least one key out of (line-height font) is required" loc: (list guide-textarea-payload name))
    (set! font (or (guide-style-ref style font:) (guide-select-font size: 'medium)))
    (set! line-height (guide-font-height font))))
  ;;
  (let ((fh line-height))
    (set! rows (min rows (floor (/ (mdv-rect-interval-height in) fh)))))
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
          (cond
           ((string? source)
            (case data-char-encoding
              ((UTF-8 utf-8)
               (let ((ggb (utf8string->ggb source)))
                 (set! source (make-ggb2d))
                 (ggb2d-insert-row! source ggb)
                 (ggb2d-goto! source position: 'absolute row: 1 col: 0)) )
              (else
               (let ((ggb2d (make-ggb2d)))
                 (ggb2d-insert-port! ggb2d (open-input-string source))
                 (set! source ggb2d)
                 (ggb2d-goto! source position: 'absolute row: 1 col: 0)))))
           ((u8vector? source)
            (set! source (u8vector->ggb2d source 0 (u8vector-length source) encoding: data-char-encoding))
            (ggb2d-goto! source position: 'absolute row: 1 col: 0)))
          (cond
           ((ggb2d? source)
            (cond
             (wrap (guide-linebreak! buffer source font text-width))
             (else (set! buffer (ggb2d-copy source)))))
           ((u32vector? source) (guide-linebreak! buffer source font text-width))
           (else ;; clear buffer, one empty row
            (ggb2d-insert-row! buffer)))
          (ggb2d-goto! buffer position: 'absolute row: 1 col: 0)
          buffer))
       (linebreak-again!
        (lambda ()
          (when wrap
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
              (set! value-buffer next-buffer)))))

       (value-views
        (let ((vec (make-vector rows #f))
              (label-height (guide-font-height font)))
          (do ((i 0 (+ i 1)))
              ((eqv? i rows) vec)
            (let ((label! (make-guide-label-view)))
              (label! horizontal-align: horizontal-align)
              (label! vertical-align: vertical-align)
              (label! font: font)
              (label! color: color)
              (label! size: text-width label-height)
              (label! position: xsw (floor (- yno (* (+ i 1) line-height))))
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

       (check-linebreak!
        (lambda ()
          (update-current-line!)
          (let* ((line-buffer current-line)
                 (line-point (ggb-point line-buffer)))
            (let loop ((w 0) (count 0))
              (ggb-for-each
               line-buffer
               (lambda (i c)
                 (let ((glyph (ln-ttf:font-ref font c)))
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
                (loop 0 (+ count 1)))))))

       (cursor-draw #f)
       (this-payload #f)
       (cursor-label!
        (let ((label! (make-guide-label-view)))
          (label! horizontal-align: horizontal-align)
          (label! vertical-align: vertical-align)
          (label! font: font)
          (label! color: highlight-color)
          (label! size: text-width line-height)
          (label! text: '#(124)) ;; "|"
          label!))

       (cursor-update-required #t)
       (update-cursor!
        (lambda ()
          (check-linebreak!)
          (set! cursor-update-required #t)
          (guide-wakeup!)
          #t))

       (really-update-cursor!
        (lambda ()
          (update-current-line!)
          (let* ((line-buffer current-line)
                 (line-point (ggb-point line-buffer))
                 (width-before 0)
                 (width-total
                  (let ((w 0))
                    (ggb-for-each
                     line-buffer
                     (lambda (i c)
                       (if (eqv? i line-point) (set! width-before w))
                       (let ((glyph (ln-ttf:font-ref font c)))
                         (if glyph (set! w (+ w (ttf:glyph-advancex glyph)))))))
                    w))
                 (label! cursor-label!))
            (begin ;; must come after linebreak-again! and update-current-line!
              (update-value-views!)
              (fix-value-draw!))
            (if (eqv? current-line-length line-point)
                (set! width-before width-total))
            (cond
             (readonly (set! cursor-draw (lambda () #t)))
             (else
              (label!
               position:
               (round (case horizontal-align
                        ((left) (+ xsw width-before))
                        ((center) (+ xsw (* 1/2 (- text-width width-total))))
                        (else (- width-total width-before))))
               (floor (- yno (* (+ (- current-line-number row-display-offset) 1) line-height))))
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
          (set! cursor-update-required #f)
          ;;(guide-wakeup!)
          ;; ensure timely visual response used here in tail position
          ;; of event handlers, signal successfully handled event.
          #t))

       (delayed-update-cursor!
        (lambda ()
          (or (not cursor-update-required)
              (really-update-cursor!))))
#|
       (no-update-cursor!
        (lambda ()
          (cond
           (readonly
            (update-current-line!)
            (when wrap
              (linebreak-again!)
              (update-current-line!))
            (update-value-views!)
            (fix-value-draw!)
            (guide-wakeup!)
            #t)
           (else (really-update-cursor!)))))
|#
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
                          (let ((glyph (ln-ttf:font-ref font c)))
                            (if glyph (set! w (+ w (ttf:glyph-advancex glyph)))))
                          (if (< x w) (return i))))
                       (ggb-length target-line))))))
            (ggb2d-goto!
             value-buffer position: 'absolute
             row: (+ target-line-number 1)
             col: target-column)
            (update-cursor!))))

       (insert-newline!
        (lambda ()
          (let ((point (ggb-point current-line)))
            (let ((nextline (make-ggb size: cols)))
              (cond
               ((< point current-line-length)
                (ggb-insert-sequence! nextline (ggb->vector current-line point current-line-length))
                (ggb-goto! nextline 0)
                (ggb-delete! current-line (- current-line-length point))))
              (ggb-insert! current-line newline-indicator)
              (ggb2d-insert-row! value-buffer nextline)))))

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
              ((eqv? key EVENT_KEYHOME)
               (cond
                ((eqv? (bitwise-and MODIFIER_CTRL mod) MODIFIER_CTRL)
                 (ggb2d-goto! value-buffer position: 'absolute row: 0 col: 0))
                (else (ggb-goto! current-line 0)))
               (update-cursor!))
              ((eqv? key EVENT_KEYEND)
               (cond
                ((eqv? (bitwise-and MODIFIER_CTRL mod) MODIFIER_CTRL)
                 (ggb2d-goto!
                  value-buffer position: 'absolute
                  row: (ggb2d-length value-buffer) col: 1000)
                 (ggb2d-goto! value-buffer position: 'relative col: 1000))
                (else (ggb-goto! current-line current-line-length)))
               (update-cursor!))
              ((or (eq? key 'PageDown) (eq? key 'PageUp))
               (let ((delta
                      (case key
                        ((PageDown) rows)
                        ((PageUp) (- rows))
                        (else 0))))
                 (ggb2d-goto! value-buffer position: 'relative row: delta)
                 (ggb2d-goto! value-buffer position: 'absolute col: 0))
               (update-cursor!))
              (readonly #t) ;; rest is mutation
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
               (insert-newline!)
               (update-cursor!))
              ((char? key)
               (ggb-insert! current-line (char->integer key))
               (value! text: (ggb->vector current-line))
               (update-cursor!))
              (else (debug "ignored key" (list 'guide-textarea-payload key)) #t))))))
       (redraw! (lambda ()
                  (delayed-update-cursor!)
                  (value-draw) (cursor-draw)))
       (events
        ;; TBD: factor motion/shift handling out (3rd copy here from
        ;; `select` already).
        (let ((armed #f) (armed-at #f) (motion-hyst 15))
          (lambda (rect payload event x y)
            (cond
             ((or (eqv? press: event) (eqv? release: event))
              (cond
               (readonly #f) ;; throws the baby out with the bathtube: no navigation
               (else
                (let ((x (on-key event x y)))
                  (if x (%%guide-post-speculative/async (handle-key event x y)) #t)))))
             ((eqv? event EVENT_BUTTON1DOWN)
              (set! armed (vector x y))
              (set! armed-at armed)
              #t)
             ((eqv? event EVENT_BUTTON1UP)
              (when (and (not readonly)
                         (eq? armed armed-at)) ;; set cursor position
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
                          (max 0 (min (+ row-display-offset delta)
                                      (- (ggb2d-length value-buffer) rows)))))
                    (set! row-display-offset new-position)
                    (cond
                     ((< current-line-number row-display-offset)
                      (ggb2d-goto! value-buffer position: 'absolute row: row-display-offset))
                     ((>= current-line-number (+ row-display-offset rows))
                      (ggb2d-goto! value-buffer position: 'absolute row: (- (+ row-display-offset rows) 1)))))
                  ;; TBD: update only if things have changed
                  (update-cursor!))
                #t))
             (else (mdvector-rect-interval-contains/xy? in x y)))))))
    (really-update-cursor!)
    (results
     (let ((result
            (make-guide-payload
             name: name in: in widget: #f
             on-redraw: redraw! on-any-event: events lifespan: 'ephemeral)))
       (set! this-payload result)
       result)
     ;; control procedure
     (lambda (key . more)
       (case key
         ((string) (ggb2d->string value-buffer data-char-encoding)) ;; SHOULD stay UTF-8 encoded
         ((utf8) (ggb2d->u8vector value-buffer))
         ((text) (ggb2d-copy value-buffer))
         ((text:)
          (check-not-observable-speculative! 'textarea-mutation key more)
          (apply
           (lambda (value #!key (wrap wrap) (cols 20))
             (cond
              ((or (ggb2d? value) (u32vector? value))
               (set!
                value-buffer
                (if (or wrap (u32vector? value)) ;; FIXME
                    (let ((buffer (make-ggb2d size: rows)))
                      (when (ggb2d? value) (ggb2d-goto! value position: 'absolute row: 1 col: 0))
                      (guide-linebreak! buffer value font text-width cols: cols)
                      (ggb2d-goto! buffer position: 'absolute row: 1 col: 0)
                      buffer)
                    value))
               (update-cursor!)
               (guide-wakeup!))
              ((not value)
               (let ((buffer (make-ggb2d)))
                 (ggb2d-insert-row! buffer)
                 (set! value-buffer buffer))
               (update-cursor!)
               (guide-wakeup!))
              (else (error "unhandled text representation" 'textarea-payload text: value)))
             #t)
           more))
         ((goto:) (apply ggb2d-goto! value-buffer more))
         ((insert:)
          (check-not-observable-speculative! 'textarea-mutation key more)
          (apply
           (lambda (data #!key (wrap wrap) (char-encoding data-char-encoding))
             (define (finalize-insert)
               (when wrap
                 (ggb2d-goto! value-buffer position: 'absolute row: 1 col: 0)
                 (linebreak-again!)
                 (update-cursor!)
                 (guide-wakeup!)
                 #f))
             (cond
              ((char? data)
               (guide-focus this-payload)
               (cond
                ((char=? data #\newline) (insert-newline!))
                (else (ggb-insert! current-line (char->integer data))))
               (update-cursor!))
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
                     (case char-encoding
                       ((UTF-8)
                        (ggb-insert-sequence! current-line (ggb->vector (utf8string->ggb data))))
                       (else
                        (call-with-input-string
                         data
                         (lambda (port) (ggb2d-insert-port! value-buffer port)))))
                     (finalize-insert))))))
              ((input-port? data)
               ;; maybe better char-by-char?
               (ggb2d-insert-port! value-buffer data)
               (finalize-insert))
              ((u8vector? data)
               (call-with-input-u8vector
                (list init: data char-encoding: char-encoding)
                (lambda (port) (ggb2d-insert-port! value-buffer port)))
               (finalize-insert))
              (else #f)))
           more))
         (else (error "invalid command key" 'guide-textarea-payload key)))))))

;;** Line Input Payload

;;; FIXME: (special case of textarea?)

(define (guide-line-input
         #!key
         (in (current-guide-gui-interval))
         (data (let ((state "n/a")) (case-lambda (() state) ((val) (set! state val)))))
         (data-char-encoding 'UNICODE) ;; TBD: remove; only applicable if data is string or u8vector
         (validate #f)
         (on-key %%guide-textarea-keyfilter);; filter characters to handle
         (style (guide-current-style))
         (size 'medium)
         (font (or (guide-style-ref style font:) (guide-select-font size: size)))
         (horizontal-align (guide-style-ref style horizontal-align:))
         (vertical-align (guide-style-ref style vertical-align:))
         (line-height (guide-font-height font))
         (color (or (guide-style-ref style color:) (guide-select-color-2)))
         (highlight-color (or (guide-style-ref style highlight-color:) (guide-select-color-4)))
         (name 'guide-line-input))
  (define (%%value-buffer->string ggb #!optional (start 0) (end (ggb-length ggb)))
    (ggb->string ggb start end encoding: #f))
  (define (input->buffer data)
    (let loop ((x (data)))
      (cond
       ((string? x)
        (case data-char-encoding
          ((UTF-8 utf-8) (utf8string->ggb x))
          (else
           (do ((i 0 (+ i 1)) (result (make-ggb size: (##string-length x))))
               ((eqv? i (##string-length x)) result)
             (ggb-insert! result (char->integer (string-ref x i)))))))
       ((u8vector? x)
        (u8vector->ggb x 0 (u8vector-length x) encoding: data-char-encoding))
       (else (loop (object->string x))))))
  (let*
      ((xsw (mdvector-interval-lower-bound in 0))
       (ysw (mdvector-interval-lower-bound in 1))
       (xno (mdvector-interval-upper-bound in 0))
       (yno (mdvector-interval-upper-bound in 1))
       (w (- xno xsw))
       (h (- yno ysw))
       (border-width (* h 1/20))
       (line-height+border (+ border-width line-height))
       (value-display-max-width (- w (* 2 border-width)))
       (font-size-treshold (guide-font-height font))
       (value-buffer (input->buffer data)) ;; central value
       (value-display-offset 0)
       (value-buffer-as-string "")
       (update-value-string!
        (lambda ()
          (set! value-buffer-as-string (%%value-buffer->string value-buffer value-display-offset))))
       (value!
        (let ((label! (make-guide-label-view)))
          (label! horizontal-align: horizontal-align)
          (label! vertical-align: vertical-align)
          (label! font: font)
          (label! color: color)
          (label! size: w h)
          (label! position: xsw (round (+ ysw border-width)))
          (label! text: value-buffer-as-string)
          label!))
       (value-draw (value!))

       (cursor-draw #f)
       (this-payload #f)
       (update-cursor!
        ;; TBD: this is a bit overly simple and needlessly expensive
        ;; at runtime
        (lambda ()
          (let loop ()
            (let ((point (ggb-point value-buffer)))
              (cond
               ((> value-display-offset point) (set! value-display-offset point))))
            (update-value-string!)
            (let* ((total value-buffer-as-string)
                   (before (%%value-buffer->string value-buffer value-display-offset (ggb-point value-buffer)))
                   (glv-before (string->guide-glyphvector before font))
                   (glv-total (string->guide-glyphvector total font))
                   (width-before (if glv-before (MATURITY+0:guide-glypvector-width glv-before) 0))
                   (width-total (if glv-total (MATURITY+0:guide-glypvector-width glv-total) 0)))
              (cond
               ((> width-before value-display-max-width)
                (set! value-display-offset (+ value-display-offset 1))
                (loop))
               ((and (> value-display-offset 0)
                     (< width-total (- value-display-max-width font-size-treshold)))
                (set! value-display-offset (- value-display-offset 1))
                (loop))
               (else
                (value! text: value-buffer-as-string)
                (set! value-draw (value!))
                (let ((label! (make-guide-label-view)))
                  (label! horizontal-align: horizontal-align)
                  (label! vertical-align: vertical-align)
                  (label! font: font)
                  (label! color: highlight-color)
                  (label! size: w h)
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
                #t))))))

       (set-cursor-position!
        (lambda (x y) ;; TBD: move as introspection feature into guide-figure
          (let ((target-column
                 (bind-exit
                  (lambda (return)
                    (let ((w (+ xsw (vector-ref (value! 'aligned-position) 0))))
                      (ggb-for-each
                       value-buffer
                       (lambda (i c)
                         (let ((glyph (ln-ttf:font-ref font c)))
                           (if glyph (set! w (+ w (ttf:glyph-advancex glyph)))))
                         (if (< x w) (return i)))
                       value-display-offset)
                      (- (ggb-length value-buffer) value-display-offset))))))
            (ggb-goto! value-buffer (+ value-display-offset target-column))
            (update-cursor!))))

       (handle-key
        (lambda (p/r key mod)
          (cond
           ((eqv? key EVENT_KEYRIGHT) (ggb-goto-right! value-buffer) (update-cursor!))
           ((eqv? key EVENT_KEYLEFT) (ggb-goto-left! value-buffer) (update-cursor!))
           ((eqv? key EVENT_KEYHOME) (ggb-goto! value-buffer 0) (update-cursor!))
           ((eqv? key EVENT_KEYEND)
            (ggb-goto! value-buffer (ggb-length value-buffer)) (update-cursor!))
           ((eqv? key EVENT_KEYBACKSPACE)
            (ggb-delete! value-buffer -1)
            (when (procedure? validate) (validate value-buffer))
            (update-cursor!))
           ((eqv? key EVENT_KEYDELETE)
            (ggb-delete! value-buffer 1)
            (when (procedure? validate) (validate value-buffer))
            (update-cursor!))
           ((eqv? key EVENT_KEYENTER)
            ;; (data (%%value-buffer->string value-buffer))
            (set! value-display-offset 0)
            (set! value-buffer (input->buffer data))
            (when (procedure? validate) (validate value-buffer))
            (update-cursor!))
           ((eqv? key #\null)) ;; should have been ignored, more harm than good
           ((char? key)
            (ggb-insert! value-buffer (char->integer key))
            (when (procedure? validate) (validate value-buffer))
            (update-cursor!))
           (else (debug "ignored key" (list 'guide-line-input key)) #t))))

       (local-on-key ;; rename for clarity
        (lambda (p/r key mod)
          (or
           (eq? press: p/r) ;; ignore press - maybe more
           (let ((key (on-key p/r key mod)))
             (cond
              ((eqv? key EVENT_KEYENTER)
               (cond ;; speculative
                ;; TBD: should we use value-buffer-as-string here?
                ((procedure? validate)
                 (when (validate value-buffer)
                   (data (ggb->string value-buffer 0 (ggb-length value-buffer) encoding: data-char-encoding)) ))
                (else (data (ggb->string value-buffer 0 (ggb-length value-buffer) encoding: data-char-encoding))))
               (%%guide-post-speculative (handle-key p/r key mod)))
              (key (%%guide-post-speculative (handle-key p/r key mod)))
              (else #t))))))

       (redraw! (vector
                 (lambda () (and value-draw (value-draw)))
                 (lambda () (cursor-draw))))
       (events
        (let ((armed #f))
          (lambda (rect payload event x y)
            (case event
              ((press: release:) (local-on-key event x y))
              ((reload:) ;;  Bad style!
               ;;
               ;; NOTE: This is to document possible style as bad.  TBD:
               ;; detail why it is bad.
               (MATURITY -1 "control operation embedded GUI event" loc: guide-line-input)
               (set! value-display-offset 0)
               (set! value-buffer (input->buffer data))
               (guide-critical-add!
                (lambda ()
                  (when (procedure? validate) (validate value-buffer))
                  (value! text: (%%value-buffer->string value-buffer))
                  (set! value-draw (value!))
                  (update-cursor!))
                async: #f)
               #t)
              (else
               (cond
                ((eqv? event EVENT_BUTTON1DOWN)
                 (set! armed (vector x value-display-offset))
                 #t)
                ((eqv? event EVENT_BUTTON1UP)
                 (set-cursor-position! x y)
                 (set! armed #f)
                 (guide-focus this-payload)
                 #t)
                ((and (eqv? event EVENT_MOTION) armed)
                 (let ((x0 (vector-ref armed 0))
                       (o0 (vector-ref armed 1)))
                   (set! value-display-offset
                         (max 0
                              (min (ggb-length value-buffer)
                                   (+ o0 (round (/ (- x0 x) font-size-treshold)))))))
                 #t)
                (else (mdvector-rect-interval-contains/xy? in x y)))))))))
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
         (receive (xsw xno ysw yno) (guide-boundingbox->quadrupel in)
           (make-guide-payload
            in: (make-x0y0x1y1-interval/coerce xsw (- ysw (* len selh)) xno yno)
            on-redraw: redraw on-any-event: events
            name: name lifespan: 'ephemeral widget: #f)))
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
               (make-mdv-rect-interval x0 y0 x1 (ceiling (+ y0 height))))))
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
                         ((and (eqv? event EVENT_MOTION) (not (in-pl? active x y)))
                          (set! active #f)
                          (guide-event-dispatch-to-payload rect content event x y))
                         (else #t)))))))
                 ((d1 rect payload event x y) #t) ;; dropdown button hit?
                 (else
                  (let ((res (guide-event-dispatch-to-payload rect (if (procedure? content) (content) content) event x y)))
                    (when (eq? res #!void)
                      (debug event (if (procedure? content) (content) content)))
                    (macro-guide-sanitize-payload-result res))))))))
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

(define (guide-keypad/english #!key (in (current-guide-gui-interval)) (action #f))
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
       #\q        #\w #\e #\r #\t #\z #\u  #\i  #\o   #\p
       #\a        #\s #\d #\f #\g #\h #\j  #\k  #\l   k-del
       k-shift    #f  #\y #\x #\c #\v #\b  #\n  #\m   #t
       k-toggle   #f  #\, k-space #f #f #f #\.  k-ret #f
       ;; II  -- upper case characters
       #\Q        #\W #\E #\R #\T #\Z #\U  #\I  #\O   #\P
       #\A        #\S #\D #\F #\G #\H #\J  #\K  #\L   k-del
       k-shift-on #f  #\Y #\X #\C #\V #\B  #\N  #\M   #t
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

(define (guide-keypad/default
         #!key
         (in (current-guide-gui-interval))
         (style (guide-current-style))
         (locale (guide-style-ref style locale:))
         (action #f))
  (case locale
    ((de) (guide-keypad/de in: in action: action))
    (else (guide-keypad/english in: in action: action))))

;;** Dialogs

;;*** Value Edit

(define (guide-value-edit-dialog
         #!key
         (in (current-guide-gui-interval))
         (done (lambda _ #f))
         (style (guide-current-style))
         (font (or (guide-style-ref style font:) (guide-select-font size: 'medium)))
         (color (or (guide-style-ref style color:) (guide-select-color-2)))
         (highlight-color (or (guide-style-ref style highlight-color:) (guide-select-color-4)))
         (background (or (guide-style-ref style background:) %%guide-default-background))
         (background-color
          (or (guide-style-ref style background-color:)
              (let* ((color (guide-select-color-1))
                     (r (color-red color))
                     (g (color-green color))
                     (b (color-blue color))
                     (a 210))
                (color-rgba r g b a))))
         (label #f) (label-string (lambda (value) (if (string? value) value (object->string value))))
         (line-height (guide-font-height font))
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
         (data-char-encoding 'UTF-8) ;; only applicable if data is string or u8vector
         (validate #f)
         (size 'small)
         (horizontal-align 'center)
         (vertical-align 'center)
         (name 'guide-value-edit-dialog))
  (let*
      ((xsw (mdvector-interval-lower-bound in 0))
       (ysw (mdvector-interval-lower-bound in 1))
       (xno (mdvector-interval-upper-bound in 0))
       (yno (mdvector-interval-upper-bound in 1))
       (border-ratio 1/20)
       (w (- xno xsw))
       (h (- yno ysw))
       (border-horizontal (* w border-ratio))
       (border-vertical (* h border-ratio))
       (wi (- w (* 2 border-horizontal)))
       (hi (- w (* 2 border-vertical)))
       (xsw-inner (+ xsw border-horizontal))
       (ysw-inner (+ ysw border-vertical))
       (xno-inner (- xno border-horizontal))
       (yno-inner (- yno border-vertical))
       (line-height+border (+ border-vertical line-height))
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
           (label! size: wi line-height)
           (label! position: xsw-inner (- yno-inner line-height))
           (label! text: (label-string label))
           label!)))
       (title (and title! (title!)))
       (title-height (if title line-height+border 0))
       (check-valid
        (and (procedure? validate)
             (lambda (ggb)
               (let ((valid (validate ggb)))
                 (when title
                   (title! color: (if valid highlight-color color))
                   (set! title (title!)))))))
       (line (guide-line-input
              in: (let ((ysw-line (max 0 (- yno-inner title-height line-height))))
                    (make-x0y0x1y1-interval/coerce
                     xsw-inner ysw-line
                     xno-inner (- yno-inner title-height)))
              horizontal-align: horizontal-align vertical-align: vertical-align
              font: font size: size line-height: line-height
              color: color highlight-color: highlight-color
              data: data data-char-encoding: data-char-encoding
              validate: check-valid))
       (refresh-line! (lambda (rect)
                        ;; FIXME: this begs for trouble!
                        ((guide-payload-on-any-event line) rect line reload: 0 0)))
       (control-panel
        (and title
             (let ((buttons 4)
                   (color (guide-select-color-4))
                   (background-color (guide-select-color-3))
                   (button-size (* 10/16 line-height)))
               (make-guide-table
                (make-mdvector
                 (range (vector buttons 1))
                 (vector
                  (lambda (area row col)
                    (guide-button
                     name: 'clear
                     in: area
                     label: "C" background-color: background-color color: color
                     guide-callback:
                     (lambda (rect payload event x y) (data "") (refresh-line! rect))))
                  (lambda (area row col)
                    (guide-button
                     name: 'snarf
                     in: area
                     label: "M" background-color: background-color color: color
                     guide-callback:
                     (lambda (rect payload event x y)
                       (clipboard-copy (data)) #t)))
                  (lambda (area row col)
                    (guide-button
                     name: 'recall
                     in: area
                     label: "R" background-color: background-color color: color
                     guide-callback:
                     (lambda (rect payload event x y)
                       (guide-critical-add!
                        (lambda ()
                          (let ((value (clipboard-paste)))
                            (thread-start! (make-thread (lambda () (kick! (lambda () (data value) (refresh-line! rect))))))))
                        ;; clipboard-paste can not be run
                        ;; asynchroneous under X11 but should be
                        ;; asynchroneous under Android
                        async:
                        (cond-expand
                         ((or android) #t)
                         (else #f)))
                       #t)))
                  (lambda (area row col)
                    (guide-button
                     name: 'close
                     in: area
                     label: "X" background-color: background-color color: color
                     guide-callback: (lambda (rect payload event x y) (done))))))
                in: (make-x0y0x1y1-interval/coerce
                     (- xno-inner (* buttons button-size)) (- yno-inner button-size)
                     xno-inner yno-inner)
                border-ratio: 1/20))))
       (kpd (keypad
             in: (let ((ynokpd
                        (max (- yno-inner title-height (* 16/10 line-height)) ;; best
                             (+ ysw-inner title-height))))
                   (make-x0y0x1y1-interval/coerce xsw-inner ysw-inner xno-inner ynokpd))
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
         (vector (guide-payload-on-redraw kpd) (guide-payload-on-redraw control-panel))))
       (this #f)
       (events
        (lambda (rect payload event x y)
          (when (eq? (guide-focus) this) (guide-focus line))
          (cond
           ((or (eqv? event EVENT_BUTTON1DOWN) (eqv? event EVENT_BUTTON1UP))
            (cond
             ((guide-payload-contains/xy? kpd x y)
              (guide-event-dispatch-to-payload rect kpd event x y))
             ((guide-payload-contains/xy? control-panel x y)
              (guide-event-dispatch-to-payload rect control-panel event x y))
             ((guide-payload-contains/xy? line x y)
              (guide-event-dispatch-to-payload rect line event x y))
             (else #t)))
           ((eqv? event EVENT_MOTION)
            (cond
             ((guide-payload-contains/xy? line x y)
              (guide-event-dispatch-to-payload rect line event x y))
             (else #t)))
           ((or (eqv? press: event) (eqv? release: event))
            (guide-focus line) ;; questionable?
            (let ((v (on-key event x y)))
              (if v
                  (guide-event-dispatch-to-payload rect line event x y)
                  (begin
                    (debug 'value-edit-ignored-key x)
                    #f))))
           (else (mdvector-rect-interval-contains/xy? in x y))))))
    (guide-focus line)
    (let ((result
           (make-guide-payload
            name: name in: in widget: #f
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
         (style (guide-current-style))
         (font (or (guide-style-ref style font:) (guide-select-font size: 'small)))
         (line-height (guide-font-height font))
         (keypad guide-keypad/default)
         (on-key %%guide-textarea-keyfilter)
         (data (let ((state "n/a")) (case-lambda (() state) ((val) (set! state val)))))
         (data-char-encoding 'UTF-8) ;; only applicable if data is string or u8vector
         (results values)
         (rows 3)
         (cols #f)
         (size 'small)
         (horizontal-align 'left)
         (vertical-align 'bottom)
         (color (or (guide-style-ref style color:) (guide-select-color-2)))
         (highlight-color (or (guide-style-ref style highlight-color:) (guide-select-color-4)))
         (background (or (guide-style-ref style background:) %%guide-default-background))
         (background-color
          (or (guide-style-ref style background-color:)
              (let* ((color (guide-select-color-1))
                     (r (color-red color))
                     (g (color-green color))
                     (b (color-blue color))
                     (a 210))
                (color-rgba r g b a))))
         (name 'guide-textarea-edit))
  (let*
      ((min-rows 3)
       (kpd-lines 4)
       (xsw (mdvector-interval-lower-bound in 0))
       (ysw (mdvector-interval-lower-bound in 1))
       (xno (mdvector-interval-upper-bound in 0))
       (yno (mdvector-interval-upper-bound in 1))
       (width (- xno xsw))
       (height (- yno ysw))
       (background-view
        (let ((bg! (make-guide-figure-view)))
          (bg! background: background)
          (bg! color: background-color)
          (bg! size: width height)
          (bg! position: xsw ysw)
          (bg!)))
       (title-height
        (cond
         ((guide-payload? menu) (guide-payload-height menu))
         (label line-height)
         (else 0)))
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
            (label! position: xsw (- yno title-height))
            (label!)))
         (menu
          (let ((label! (make-guide-label-view))
                (figure! (MATURITY+2:make-guide-bg+fg-view)))
            (label! size: width line-height)
            (figure! size: width line-height)
            (figure! position: xsw (- yno title-height))
            (when (pair? label-properties)
              (for-each
               (lambda (setting) (apply label! setting))
               label-properties))
            (figure! background: #f)
            (figure! foreground: (guide-payload-on-redraw menu))
            (case 2
              ((1)
               (let ((fig! (make-guide-figure-view)))
                 (fig! size: width title-height)
                 (fig! foreground: (label!))
                 (fig! position: xsw (- yno title-height))
                 (fig! color: Yellow)
                 (fig! background:
                       (guide-background
                        button:
                        in: (guide-payload-measures menu)))
                 (fig!)))
              ((2) (figure!))
              (else (label!)))))
         (else #f)))
       (edit-area-height
        (max (* min-rows line-height)
             (min
              (* rows line-height)
              (- height title-height (* kpd-lines line-height)))))
       (edit-position
        (let ((yno (ceiling (- yno title-height))))
          (vector xsw (floor (- yno edit-area-height)))))
       (edit-area (make-mdv-rect-interval 0 0 width edit-area-height))
       (lines-control! #f)
       (lines
        (receive
            (pl ctrl)
            (guide-textarea-payload
             in: edit-area
             rows: rows cols: cols
             horizontal-align: horizontal-align vertical-align: vertical-align
             font: font line-height: line-height
             color: color highlight-color: highlight-color
             data: data data-char-encoding: data-char-encoding
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
             in: (let ((intented (vector-ref edit-position 1)))
                   (let ((min-kpd-height (* kpd-lines line-height)))
                     (make-x0y0x1y1-interval/coerce xsw ysw xno (+ ysw (max min-kpd-height intented)))))
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
             ((and menu (> y (- yno title-height)))
              (guide-event-dispatch-to-payload rect menu event x (- y (- yno title-height))))
             ((and lines-control! title (> y (- yno line-height)) (eqv? event EVENT_BUTTON1DOWN))
              (lines-control! event x (- y (- yno title-height))) #t)
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
      name: name in: in widget: #f
      on-redraw: redraw! on-any-event: events lifespan: 'ephemeral)
     lines-control!)))

;;*** Selection List

(define (guide-list-select-payload
         in content
         #!key
         (done #f)
         (action #f) (guide-callback #f)
         (style (guide-current-style))
         (line-height #f)
         (font
          (cond
           (line-height (guide-select-font height: line-height))
           ((guide-style-ref style font:) => identity)
           ;; deprecated legacy default:
           (else (guide-select-font height: 35))))
         (background %%guide-default-background)
         (background-color (guide-style-ref style background-color:))
         (color (guide-style-ref style color:))
         (horizontal-align (or (guide-style-ref style horizontal-align:) 'center))
         (vertical-align (or (guide-style-ref style vertical-align:) 'center))
         ;; non-functional; for debugging:
         (name 'list-select-payload))
  ;; legacy parameter cleanup
  (unless line-height (set! line-height (guide-font-height font)))
  ;;
  (let* ((content (let ((content (content)))
                    (if (vector? content) content (apply vector content))))
         (len (vector-length content)))
    (cond
     ((eqv? len 0)
      (guide-button
       in: in
       label: "nothing"
       guide-callback:
       (cond
        ((procedure? action)
         (lambda (rect payload event x y)
           (with-exception-catcher
            (lambda (lmb) #t)
            (lambda () (macro-guide-sanitize-payload-result (action -1 0))))))
        ((procedure? done)
         (lambda (rect payload event x y) (done)))
        ((procedure? guide-callback)
         (macro-guide-sanitize-payload-result
          (guide-callback rect payload event x y)))
        (else #t))
       font: font
       background: background background-color: background-color
       color: color
       horizontal-align: horizontal-align vertical-align: vertical-align
       name: name))
     (else
      (let*
          ((content-offset 0)
           (xno (mdvector-interval-upper-bound in 0))
           (xsw (mdvector-interval-lower-bound in 0))
           (yno (mdvector-interval-upper-bound in 1))
           (ysw (mdvector-interval-lower-bound in 1))
           (area-width (- xno xsw))
           (h (- yno ysw))
           (selh line-height)
           (num-max-visible (floor (/ h line-height)))
           (num-visible (min len num-max-visible))
           (set-content-offset!
            (lambda (to)
              (set! content-offset (max 0 (min (- len num-visible) to)))))
           (area-visible-height (* num-visible line-height))
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
                  (view! size: area-width line-height)
                  (view! position: 0 y)
                  (view! color: (if (even? i) background-color-even background-color))
                  (label! size: area-width line-height)
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
              (bg! size: area-width area-visible-height)
              (bg! color: background-color)
              (bg! foreground: background)
              (frame! background: (bg!))
              (fg! size: area-width area-visible-height)
              (fg! color: color)
              (fg! foreground: draw-labels)
              (frame! foreground: (fg!))
              (frame! size: area-width area-visible-height)
              (frame! position: xsw (- yno area-visible-height))
              frame!))
           (draw-frame (visible-frame!))
           (y-shift 0)
           (shift!
            (let ((rebuild-height (* 2 line-height)))
              (lambda (base target)
                (define (update-shift!)
                  (labels-container! position: 0 y-shift)
                  (visible-frame! foreground: (labels-container!))
                  (set! draw-frame (visible-frame!)))
                (cond
                 ((< target rebuild-height) ;; upwards
                  (let ((delta-lines (* 2 (quotient target rebuild-height))))
                    ;; (set! y-shift (remainder target rebuild-height))
                    (set-content-offset! (+ base delta-lines))
                    (update-content!)
                    (update-shift!)
                    (- (* delta-lines line-height))))
                 ((> target (- rebuild-height))
                  (let ((delta-lines (* 2 (quotient target rebuild-height))))
                    ;; (set! y-shift (remainder target rebuild-height))
                    (set-content-offset! (+ base delta-lines))
                    (update-shift!))
                  (update-content!))
                 (else #f)))))
           (redraw (lambda () (draw-frame)))
           (this-payload #f) ;; letrec
           (pointer-event
            (let ((armed #f) (armed-content-offset #f) (motion-hyst 15))
              (lambda (rect payload event x y)
                (cond
                 ((eqv? event EVENT_BUTTON1DOWN)
                  (set! armed (vector x y))
                  (guide-focus payload)
                  #t)
                 ((eqv? event EVENT_BUTTON1UP)
                  (cond
                   ((vector? armed)
                    (let* ((dx (- x (vector-ref armed 0)))
                           (dy (- y (vector-ref armed 1)))
                           (in-sel (floor (/ (- yno y) selh))))
                      (set! armed #f)
                      (set! armed-content-offset #f)
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
                   (armed
                    (set! armed #f)
                    (set! armed-content-offset #f)
                    #t)
                   (else #f)))
                 ((eqv? event EVENT_MOTION)
                  ;; strange: observing wild jumps in `y`
                  (cond
                   ((number? armed)
                    (shift! armed-content-offset (- y armed))
                    #t)
                   (armed
                    (let* ((dx (- x (vector-ref armed 0)))
                           (dy (- y (vector-ref armed 1))))
                      (when (> (sqrt (+ (* dx dx) (* dy dy))) ;; distance
                               motion-hyst)
                        (set! armed-content-offset content-offset)
                        (set! armed (vector-ref armed 1))
                        (shift! armed-content-offset (- y armed)))
                      #t))
                   (else #t)))
                 (else (debug 'guide-list-select-payload (list event x y)))))))
           (key-event
            (lambda (event key modifier)
              (case key
                ((PageUp PageDown)
                 (set-content-offset! ((if (eq? key 'PageDown) + -) content-offset (* num-visible line-height)))
                 (update-content!))
                (else #f))))
           (events-here
            (lambda (rect payload event x y)
              (case event
                ((press: release:)
                 (and (eq? (guide-focus) this-payload) (key-event event x y)))
                (else
                 (let ((y (- y y-shift)))
                   (cond
                    ((guide-payload-contains/xy? payload x y)
                     (pointer-event rect payload event x y))
                    (else
                     #;(MATURITY
                     -1 "NOT handling event outside, TBD: don't pass here!"
                     loc: 'make-figure-list-payload2)
                     #f))))))))
        (update-content!)
        (let ((result
               (make-guide-payload
                in: area-visible on-redraw: redraw on-any-event: events-here
                name: name lifespan: 'ephemeral widget: #f)))
          (set! this-payload result)
          result))))))

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
       (MATURITY -1 "obsolete, use `guide-critical-add!`instead" loc: %%guide-critical-call)
       (cond
        ((not new))
        ((procedure? new) (new))
        (else (force new)))
       ;; important: whatever the operation returns MUST NOT not leak
       #f)
     post: (lambda () (when (receiver) (receiver #f)) #f))
    receiver))

(define (guide-path-select
         #!key
         (in (current-guide-gui-interval))
         (done (lambda _ #f))
         (menu #f)
         (label #f) (label-string (lambda (value) (if (string? value) value (object->string value))))
         label-properties
         (size 'small)
         (font (guide-select-font size: 'medium))
         (keypad guide-keypad/default)
         (on-key %%guide-textarea-keyfilter)
         (directory (let ((state ".")) (case-lambda (() state) ((val) (set! state val)))))
         (selected (let ((state #f)) (case-lambda (() state) ((val) (set! state val)))))
         (filter-pred (lambda (x) #t))
         (ignore-hidden #t)
         (results values)
         (background (guide-background default: in: in))
         (background-color
          (let* ((color (guide-select-color-1))
                 (r (color-red color))
                 (g (color-green color))
                 (b (color-blue color))
                 (a 210))
            (color-rgba r g b a)))
         (color (guide-select-color-2))
         (highlight-color (guide-select-color-4))
         (name 'guide-path-select))
  (define-values (xsw xno ysw yno) (guide-boundingbox->quadrupel in))
  (define line-height (guide-font-height font))
  (define button-size (* 14/10 line-height))
  (define listing
    (let* ((directory (or (directory) (current-directory)))
           (port (open-directory (list path: directory ignore-hidden: ignore-hidden)))
           (files (list->vector (sort! string<? (filter filter-pred (read-all port))))))
      (guide-list-select-payload
       (make-x0y0x1y1-interval/coerce
        xsw ysw xno (- yno (* 11/10 button-size)))
       (lambda () files)
       action:
       (lambda (n x)
         (when (and (fixnum? n) (>= n 0) (< n (vector-length files)))
           (selected (vector-ref files n)))
         (%%guide-post-speculative (done))))))
  (define menu
    (let ((buttons 2))
      (guide-table-layout
       (make-x0y0x1y1-interval/coerce
        (- xno (* 11/10 buttons button-size)) (- yno (* 11/10 button-size))
        xno yno)
       rows: 1 cols: buttons
       name: "menu"
       (lambda (area row col)
         (guide-button
          name: 'clear in: area
          label: "C" background-color: background-color color: color
          guide-callback:
          (lambda (rect payload event x y)
            (selected #f)
            (cond
             ((procedure? done) (macro-guide-sanitize-payload-result (done)))
             (else #f)))))
       (lambda (area row col)
         (guide-button
          name: 'close in: area
          label: "x" background-color: background-color color: color
          guide-callback:
          (lambda (rect payload event x y)
            (cond
             ((procedure? done) (macro-guide-sanitize-payload-result (done)))
             (else #f))))))))
  (let ((buffer (make-ggb size: 2)))
    (ggb-insert! buffer listing)
    (ggb-insert! buffer menu)
    (guide-ggb-layout
     in buffer name: name direction: 'layer
     background: background background-color: background-color
     fixed: #t)))

;;** GUIDOT

;;*** Text Edit Menu

(define (guidot-texteditor-menu
         edit-control
         #!key
         (in (current-guide-gui-interval))
         (font (guide-select-font size: 'medium))
         (color (guide-select-color-4))
         (background-color (guide-select-color-3))
         (border-ratio 1/10)
         (action-save-label "S")
         (action-save-callback NYI)
         (action-reload-label "R")
         (action-reload-callback NYI)
         (action-close-label "X")
         (action-close-callback NYI)
         (action-label-copy "M")
         (action-paste-label "MR")
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
          label: action-label-copy
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
                (guide-critical-add!
                 (lambda ()
                   ((edit-control) insert: (clipboard-paste) char-encoding: 'UTF-8))
                 ;; clipboard-paste can not be run
                 ;; asynchroneous under X11 but should be
                 ;; asynchroneous under Android
                 async:
                 (cond-expand
                  ((or android) #t)
                  (else #f))))))
         (lambda (in row col)
           (guide-button
            in: in
            label: action-paste-label
            font: font
            color: color
            background-color: background-color
            guide-callback: action)))
       (lambda (in row col)
         (guide-button
          in: in
          label: action-save-label
          font: font
          color: color
          background-color: background-color
          guide-callback: action-save-callback))
       ;; reload
       (lambda (in row col)
         (guide-button
          in: in
          label: action-reload-label
          font: font
          color: color
          background-color: background-color
          guide-callback: action-reload-callback))
       ;; close
       (lambda (in row col)
         (guide-button
          in: in
          label: action-close-label
          font: font
          color: color
          background-color: background-color
          guide-callback: action-close-callback))))
     in: area
     name: name
     border-ratio: border-ratio)))

;;** Chat

(define (make-chat
         #!key
         (in (current-guide-gui-interval))
         (font (guide-select-font size: 'small))
         (line-height (guide-font-height font))
         (rows 3)
         (keypad guide-keypad/default)
         (mode #t)
         (right-side-offset line-height)
         (timestamp #f)
         (action #f #;(lambda (ctrl) #f))
         (results values)
         (data-char-encoding 'UTF-8) ;; FIXME: get rid of that
         (name 'chat)
         ) ;; make-chat: end of keyword parameters
  (define (chat-message data timestamp l/r)
    ;; re-format message
    (let*
        ((area in)
         (width
          (cond
           (l/r (mdv-rect-interval-width area))
           (else (- (mdv-rect-interval-width area) right-side-offset))))
         ;; I. general
         (color-2 (guide-select-color-2))
         (color-4 (guide-select-color-4))
         (horizontal-align
          (begin
            (when l/r
              (let ((t color-2))
                (set! color-2 color-4)
                (set! color-4 t)))
            'left))
         (line-count
          (cond
           ((ggb2d? data) (ggb2d-length data))
           ((string? data)
            ;; BAD: length!
            (length (string-split data #\newline)))
           (else 5)))
         ;; II. reformat text
         ;;
         ;; TBD: this is quite slow.
         (data-len (cond
                    ((ggb2d? data) (ggb2d-total-length data))
                    ((string? data) (string-length data))
                    (else 2)))
         (linebroken #f)
         (formatted-payload
          (guide-textarea-payload
           readonly: #t
           in: (make-mdv-rect-interval 0 0 width (* data-len line-height))
           rows: data-len
           horizontal-align: horizontal-align
           vertical-align: 'bottom
           font: font
           line-height: line-height
           color: color-2 highlight-color: color-4
           ;; background: #f
           data: (lambda _
                   (when (ggb2d? data)
                     (ggb2d-goto! data 'position: 'absolute row: 1 col: 0))
                   data)
           data-char-encoding: data-char-encoding
           results: (lambda (pl ctrl)
                      (set! linebroken (ctrl 'text))
                      ;;(ggb2d-goto! linebroken 'position: 'absolute row: 1 col: 0)
                      pl)))
         ;; III. re-size
         (resized-payload
          (let ((rows (ggb2d-length linebroken)))
            (guide-textarea-payload
             readonly: #t
             in: (make-mdv-rect-interval 0 0 width (* rows line-height))
             rows: rows
             horizontal-align: horizontal-align
             vertical-align: 'bottom
             font: font
             line-height: line-height
             color: color-2 highlight-color: color-4
             ;; background: %%guide-default-background
             wrap: #f
             data: (lambda _ linebroken) data-char-encoding: data-char-encoding
             results: (lambda (pl ctrl) pl)))))
      (let* ((msg-area (guide-payload-measures resized-payload))
             (msg-bg (guide-background 'default in: msg-area))
             (datentime
              (and
               timestamp
               (guide-button
                in: (make-mdv-rect-interval 0 0 width line-height)
                font: font
                label: (date->string
                        (cond
                         ((date? timestamp) timestamp)
                         ((number? timestamp) (time-utc->date (make-srfi19:time 'time-utc 0 timestamp)))
                         (else (current-date))))
                background: msg-bg
                color: (guide-select-color-4)
                guide-callback: #f)))
             (buffer (and timestamp (make-ggb size: 2))))
        (when timestamp
          (ggb-insert! buffer datentime)
          (ggb-insert! buffer resized-payload))
        (let ((decorated-area
               (let ((headroom
                      (cond
                       (timestamp 3/2)
                       (else 0/2))))
                 (make-x0y0x1y1-interval/coerce
                  0 0
                  width (+ (mdvector-interval-upper-bound msg-area 1)
                           (* headroom line-height))))))
          (guide-button
           in: decorated-area
           position: (and (not l/r) (vector right-side-offset 0))
           background: msg-bg
           guide-callback:
           (let ((url? (rx '($ http-url))))
             (lambda (rect payload event x y)
               (cond
                ((or (eq? event press:) (eq? event release:))
                 #t)
                ((eqv? event EVENT_BUTTON1UP)
                 (let ((str
                        (cond
                         ((ggb2d? data) (ggb2d->string data))
                         ((string? data) data)
                         (else (string-append "UNHANDLED: " (object->string data))))))
                   (cond
                    ((and (< (* 3 x) (mdv-rect-interval-width area))
                          (rx~ url? str))
                     =>
                     (lambda (m)
                       (let ((url (rxm-ref m 1)))
                         (%%guide-post-speculative/async (webview-launch! url via: 'webview)))))
                    (else
                     (%%guide-post-speculative
                      (begin
                        (unless (clipboard-copy str)
                          (MATURITY -1 "copying to clipboard failed" loc: (list 'chat name)))
                        ;; gui: signal done anyway
                        #t))))))
                (else #f))))
           label:
           (cond
            (timestamp
             (guide-ggb-layout
              decorated-area
              buffer
              background: #t
              direction: 'topdown
              results:
              (lambda (pl ctrl)
                ;; payload is supposed to be garbage, just does
                ;; not yet work, why?
                (if #t pl (ctrl 'fix #t)))))
            (else resized-payload)))))))
  (set! line-height (inexact->exact (ceiling line-height))) ;; just to be sure
  (let* ((mode
             (case mode
               ((server #t) #t)
               ((client #f) #f)
               (else (error "invalid mode" 'guide-chat mode))))
         (area in)
         ;; -- model
         (messages (make-ggb size: 0))
         (msg
          (let ((state ""))
            (case-lambda
             (() state)
             ((val) #!void))))
         (input-edit #f) ;; catch editor here to enable focus handling
         (message-display-control! #f)
         ;; CONTRUCTOR (lambda (contructor INTERVAL COL ROW . rest) . rest)
         ;; CONTRUCTOR+ARGS: (or CONTRUCTOR (CONTRUCTOR . ARGS))
         (cm (lambda (in col row)
               (guide-ggb-layout
                in messages direction: 'vertical ;; FIXME: clip: #t
                results: (lambda (pl ctrl) (set! message-display-control! ctrl) pl))))
         (ce0
          (lambda (in col row)
            (define edit-control!)
            (define (send! . _)
              (guide-critical-add!
               (lambda ()
                 (cond
                  ((procedure? action)
                   (action edit-control!))
                  (else
                   (ggb-goto! messages 0)
                   (let ((msg (edit-control! 'text)))
                     (ggb-insert! messages (chat-message msg timestamp mode)))
                   (edit-control! text: #f))) )
               async: #t))
            (define menu
              (let ((font (guide-select-font size: 'medium))
                    (w (mdv-rect-interval-width in)))
                (guidot-texteditor-menu
                 (lambda _ edit-control!)
                 name: (cons name "menu")
                 in: (make-mdv-rect-interval 0 0 w (round (* 8/5 (guide-font-height font))))
                 font: font
                 color: (guide-select-color-4)
                 background-color: (guide-select-color-3)
                 action-save-label: "Send!"
                 action-save-callback: send!
                 action-reload-label: "n/a"
                 action-reload-callback: NYI
                 action-close-label: "n/a"
                 action-close-callback: NYI
                 action-label-copy: "M"
                 action-paste-label: "MR")))
            (guide-textarea-edit
             in: in
             menu: menu
             keypad: keypad
             data: msg data-char-encoding: data-char-encoding
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
              (kph (* (+ rows 8) (guide-font-height font))))
          (ggb-insert! panel (ce (make-mdv-rect-interval 0 0 xno kph) 0 0))
          (ggb-insert! panel (cm (make-mdv-rect-interval 0 0 xno (+ (- yno ysw) kph)) 0 0))
          (guide-ggb-layout area panel direction: 'vertical fixed: #t)))
       ((2) ;; Table, messages at bottom - bug workaround.
        (make-guide-table
         (make-mdvector
          (range '#(1 2))
          (vector ce cm))
         in: area name: name
         border-ratio: 0))
       (else
        (make-guide-table
         (make-mdvector
          (range '#(1 2))
          (vector cm ce))
         in: area name: name
         border-ratio: 0)))
     (lambda (key msg #!optional (timestamp timestamp))
       (case key
         ((msg:) ;; add remote message
          (check-not-observable-speculative! name key msg)
          (unless (equal? msg "")
            (ggb-goto! messages 0)
            (ggb-insert! messages (chat-message msg timestamp (not mode)))))
         ((sent:) ;; add local message
          (check-not-observable-speculative! name key msg)
          (unless (equal? msg "")
            (ggb-goto! messages 0)
            (ggb-insert! messages (chat-message msg timestamp mode))))
         ((focus:)
          (guide-focus (and msg input-edit)))
         ((load:)
          (check-not-observable-speculative! name key msg)
          (message-display-control! position: 0)
          (ggb-clear! messages)
          (for-each
           (lambda (msg)
             (receive (reference from msg kind) (apply values msg)
               (ggb-insert! messages (chat-message msg (and timestamp reference) (eqv? kind 0)))))
           msg)))))))

;;;* Xglgui

;;; END Xglgui
