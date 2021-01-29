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

(define (make-figure-list-payload
         in content
         #!key
         (font (error "font is required so far" 'make-figure-list-payload))
         (action #f) (guide-callback #f)
         ;; non-functional; for debugging:
         (name make-figure-list-payload))
  ;; TBD: Option to catch/display errors in handling content events.
  (let* ((content (let ((content (content)))
                    (if (vector? content) content (apply vector content))))
         (len (vector-length content))
         (all (make-vector len #f))
         (bg (%%glCore:textures-ref (glC:image-t %%guide-default-background) #f))
         (sely (mdvector-interval-lower-bound in 1))
         (selh (- (mdvector-interval-upper-bound in 1) sely))
         (selcb
          (lambda (rect payload event x y)
            (when action (action (floor (/ (- sely y) selh))))
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
    (do ((i 0 (fx+ i 1)))
        ((eqv? i len)
         (make-guide-payload
          in: in on-redraw: redraw on-any-event: events
          name: name lifespan: 'ephemeral widget: #f))
      (let* ((label (vector-ref content i))
             (payload
              (guide-button
               in: in label: label font: font background: bg
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
                 (let ((action (lambda (sel) (set! active #f) (selection sel))))
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
  (let ((rng (range '#(5 5)))
        (spec
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
          )))
    (guide-make-keypad in (make-mdvector rng spec) on-key: action)))

(define (guide-keypad/simplified #!key (in (current-guide-gui-interval)) (action #f))
  (let ((rng (range '#(10 4)))
        (spec
         (vector
          #\q #\w #\e #\r #\t #\y #\u #\i #\o #\p
          #\a #\s #\d #\f #\g #\h #\j #\k #\l #\#
          (list 'shift label: (apply make-glC:image glgui_keypad_shift.img))
          #f
          #\z #\x #\c #\v #\b #\n #\m
          (list delchar label: (apply make-glC:image glgui_keypad_delete.img) 1.5)
          ;;
          (list 'toggle label: (apply make-glC:image glgui_keypad_toggle.img)) #f
          #\, #\space #f #f #f #f #\. (list retchar label: (apply glgui_keypad_return.img)) #f
          )))
    (guide-make-keypad in (make-mdvector rng spec) on-key: action)))

(define (guide-value-edit-dialog
         #!key
         (in (current-guide-gui-interval))
         (font (guide-select-font size: 'medium))
         (label "") (label-string (lambda (value) (if (string? value) value (object->string value))))
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
        (let ((label! (make-guide-label-view)))
          (label! horizontal-align: horizontal-align)
          (label! vertical-align: vertical-align)
          (label! font: font)
          (label! color: color)
          (label! size: w line-height)
          (label! position: xsw (- yno line-height+border))
          (label! text: (label-string label))
          (label!)))
       (line (guide-line-input
              in: (make-mdv-rect-interval
                   xsw
                   (round (- yno (* 2 line-height+border))) xno yno)
              horizontal-align: horizontal-align vertical-align: vertical-align
              font: font size: size line-height: line-height
              color: color hightlight-color: hightlight-color
              data: data))
       (kpd (keypad
             in: (make-x0y0x1y1-interval/coerce xsw ysw xno (- yno (* 2 line-height+border)))))
       (redraw! ;; FIXME: nested vector drawind handlers should be supported too
        (vector-append
         (vector background-view title)
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
