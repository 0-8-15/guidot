;;** Missplaced Code

(include "../../front/apps/glgui/DejaVuSans-14,24,32.scm")

;;;** X11

(cond-expand
 (linux
  (c-declare "#include <stdint.h>\nextern int64_t microgl_getWindow();")
  (define microgl-getWindow (c-lambda () int64 "microgl_getWindow")))
 (else
  (define (microgl-getWindow) -1)))

(define (guide-floating-window!)
  ;; i3 specific
  (let ((i3cmd (string-append "[id=" (number->string (microgl-getWindow)) "] floating enable")))
    (with-exception-catcher
     (lambda (exn) #f)
     (lambda () (open-process `(path: "i3-msg" arguments: (,i3cmd)))))))

;;;** GLC a fixes and improvements to glcore

(include "glc.scm")

;;;** Basic Generic Drawing.

(include "guide-button.scm")

;;** Imports

(include "../misc-conventions/observable-syntax.sch")

;;; END INSTEAD OF (include "~~tgt/lib/onetierzero/src/observable-notational-conventions.scm")

(define-structure guide-widget redraw) ;; temporary helper for type safety

(define-structure guide-rectangle glgui measures)
(define-structure guide-payload
  measures     ;; interval
  widget       ;; glgui or extended
  on-any-event ;; generic event dispatch (replace by r/w specific versions
  gui-before   ;; when exposed
  gui-after    ;; when hidden
  )

(define (guide-rectangle-width obj)
  ;; FIXME: OVERHEAD
  (let ((interval (guide-rectangle-measures obj)))
    (- (mdvector-interval-upper-bound interval 0) (mdvector-interval-lower-bound interval 0))))

(define (guide-rectangle-height obj)
  ;; FIXME: OVERHEAD
  (let ((interval (guide-rectangle-measures obj)))
    (- (mdvector-interval-upper-bound interval 1) (mdvector-interval-lower-bound interval 1))))

(define (guide-default-event-dispatch/fallback-to-glgui rect payload event x y)
  ;; fallback to glgui
  (let ((glgui (guide-rectangle-glgui rect)))
    (cond
     ((not glgui)) ;; TBD
     #;((eq? event EVENT_REDRAW)
      (glgui-event glgui event x y)
      (guide-meta-menu-draw!))
     (else (glgui-event glgui event x y)))))

(define guide-default-event-dispatch guide-default-event-dispatch/fallback-to-glgui)

(define guide-terminate-on-key (make-parameter EVENT_KEYESCAPE))

(define guide-meta-menu (make-parameter #f))
(define MATURITY+4:%%guide-overlay ;; note: keeping %%-prefix as this
                                   ;; might be debug only!
  (let ((c #f))
    (case-lambda
     (() c)
     ((n)
      (unless (or (not n) (procedure? n))
        (error "illegal argument" 'guide-overlay n))
      ;; swap
      (let ((o c)) (set! c n) o)))))
(define %%guide-overlay MATURITY+4:%%guide-overlay)

(define (guide-meta-menu-draw!)
  (let ((gmm (guide-meta-menu))
        (overlay (MATURITY+4:%%guide-overlay)))
    ;; For debug/meta/advanced controls; TBD: react to input!
    (if (procedure? gmm) (gmm))
    ;; For monitoring, e.g. during debug.  Do NOT make this active!
    (if (procedure? overlay) (overlay))))

(define (guide-default-event-dispatch/toplevel rect payload event x y)
  (let ((event-handler (guide-payload-on-any-event payload)))
    (cond
     ((eq? event EVENT_REDRAW)
      ;; (log-status "REDRAW")
      (event-handler rect payload event x y)
      (guide-meta-menu-draw!))
     ((eq? event EVENT_IDLE)
      (thread-yield!)
      #t)
     ;; (check-magic-keys gui t x y)
     ((and (fx= event EVENT_KEYPRESS)
           (let ((termkey (guide-terminate-on-key)))
             (and termkey (fx= x termkey))))
      (terminate))
     (else (kick! (lambda () (event-handler rect payload event x y)))))))

(define (guide-event-dispatch-to-payload rect payload event x y)
  ((guide-payload-on-any-event payload) gui payload event x y))

(set!
 make-guide-payload
 (let ((make-guide-payload make-guide-payload)
       (exposed
        (lambda (payload rect)
          (let ((wgt (guide-payload-widget payload)))
            (cond
             ((not wgt)) ;; TBD
             (else (glgui-widget-set! (guide-rectangle-glgui rect) wgt 'hidden #f))))))
       (hidden
        (lambda (payload rect)
          (let ((wgt (guide-payload-widget payload)))
            (cond
             ((not wgt)) ;; TBD
             (else (glgui-widget-set! (guide-rectangle-glgui rect) wgt 'hidden #t))))))
       (remove
        (lambda (payload rect)
          (let ((wgt (guide-payload-widget payload)))
            (cond
             ((not wgt)) ;; TBD
             (else (glgui-widget-delete (guide-rectangle-glgui rect) wgt))))))
       (events guide-default-event-dispatch))
   (lambda (#!key
            (in #f)
            (widget (make-gtable))
            (lifespan #t)
            (on-any-event events)
            (gui-before exposed)
            (gui-after hidden))
     (unless (mdvector-interval? in)
       (error "make-guide-payload: 'in:' must be an interval" in))
     (cond
      ((or (eq? lifespan #f) (eq? lifespan 'ephemeral))
       (make-guide-payload in widget on-any-event gui-before remove))
      (else (make-guide-payload in widget on-any-event gui-before gui-after))))))

(define guide-open-rectangle
  (let ((empty (make-guide-payload
                in: (make-mdvector-interval 2 0 0 1 1)
                gui-before: #f
                gui-after: #f
                )))
    (define switch-over
      (identity #;opportunistic-sequential ;; upon debug: check no transaction active
       (lambda (rect current new)
         (let ((after (guide-payload-gui-after current)))
           (when after (after current rect)))
         (set! current new)
         (let ((before (guide-payload-gui-before current)))
           (when before (before current rect)))
         (glgui-wakeup!)
         current)))
    (define (guide-open-rectangle rect #!optional (name #f))
      (unless (guide-rectangle? rect) (error "guide-open-rectangle: rectangle required" rect))
      (let ((current empty))
        (case-lambda
         (() current)
         ((new)
          (cond
           ((eq? current new))
           (else
            (unless (guide-payload? new) (error "illegal payload" new))
            (set! current (switch-over rect current new))))))))
    guide-open-rectangle))

(define (current-guide-gui-interval)
  (make-mdv-rect-interval 0 0 (glgui-width-get) (glgui-height-get)))

(define (guide-make-gui #!optional (content (make-glgui)))
  (make-guide-rectangle content (current-guide-gui-interval)))

(define (guide-make-payload gui #!optional name)
  (guide-open-rectangle gui name))

(define-values
    ;; optional sugar: mapping from names to payload constructors
    (guide-define-payload guide-payload-ref)
  (let ((defined (make-table))
        ;; payload-contructor? should become a specialized/tagged procedure
        (payload-contructor? procedure?))
    (define (guide-make-payload-definition-once constructor)
      ;; This locking is supposed to be useless overhead; just to be
      ;; safe we run into exceptions when missued.
      (let ((once (make-mutex constructor)))
        (lambda (rect interval)
          (if (mutex? once)
              (let ((seen once))
                (mutex-lock! seen)
                (when (mutex? once)
                  (set! once (constructor rect interval)))
                (mutex-unlock! seen)
                once)
              once))))
    (define (guide-define-payload name . lifespan+payload)
      (if (pair? lifespan+payload)
          (let ((lifespan (car lifespan+payload))
                (payload (cdr lifespan+payload)))
            (when (null? payload) (error "guide-define-payload: name lifespan payload"))
            (let ((constructor
                   (let ((obj (car payload)))
                     (unless (payload-contructor? obj)
                       (error "not a payload contructor" guide-define-payload obj))
                     (case lifespan
                       ((#f ephemeral) obj)
                       (else (guide-make-payload-definition-once obj))))))
              (table-set! defined name constructor)))
          (table-set! defined name)))
    (define (guide-payload-ref name . default)
      (apply table-ref defined name default))
    (values
     guide-define-payload
     guide-payload-ref)))

;;** Colors

(define-structure glColor color-rgba)

(define (guide-make-color-rgba r g b a)
  (make-glColor (color-rgba r g b a)))

(define-structure guide-colorscheme colors)

(define (guide-color->glColor obj)
  (cond
   ((glColor? obj) (glColor-color-rgba obj))
   ((##fixnum? obj) obj)
   ((##flonum? obj)
    (if (##fl< obj fix:fixnum-max-as-flonum)
        (##flonum->fixnum obj) (##flonum->exact-int obj)))
   ((##bignum? obj) obj)
   ((##ratnum? obj) (##floor obj))
   (else (error "guide-color->glColor: unhandled color" obj))))

(set!
 make-guide-colorscheme
 (let ((make-guide-colorscheme make-guide-colorscheme)
       (min-colors 4))
   (lambda (#!key
            colors)
     (cond
      ((and (vector? colors)
            (fx>= (vector-length colors) min-colors))
       ;; FIXME: create u64vector instead
       (make-guide-colorscheme (vector-map guide-color->glColor colors)))
      (else (error "not a valid color scheme" colors))))))

(define-values
    (guide-current-colorscheme guide-select-color)
  (let* ((convert
          (lambda (old new)
            (cond
             ((guide-colorscheme? new) new)
             ((vector? new) (make-guide-colorscheme colors: new))
             (else new))))
         (current-colorscheme
          (make-pin
           initial: (convert #f (apply vector (map make-glColor (list Black Orange Black White))))
           pred: guide-colorscheme?
           filter: convert
           name: "The current color scheme"))
        (colors #f)
        (%%colors-ref #f))
    (define (guide-select-color pref . more)
      (cond
       ((and (pair? more) %%colors-ref)
        (apply %%colors-ref pref more))
       ((number? pref)
        (if (negative? pref)
            (let ((index (- -1 pref)))
              (cond
               (%%colors-ref (%%colors-ref index))
               ((vector? colors)
                (vector-ref colors (modulo index (vector-length colors))))
               (else (error "guide-select-color: bad spec:" pref more))))
            pref))
       (else (error "guide-select-color: bad spec:" pref more))))
    (define (update-cache!)
      (set! colors (guide-colorscheme-colors (current-colorscheme)))
      ;; (set! %%colors-ref (and (array? colors) (array-getter colors)))
      #f)
    (update-cache!)
    (wire! current-colorscheme post: update-cache!)
    (wire! current-colorscheme post: glgui-wakeup!)
    (values current-colorscheme guide-select-color)))

(define (guide-select-color-1)
  (guide-select-color -1))

(define (guide-select-color-2)
  (guide-select-color -2))

(define (guide-select-color-3)
  (guide-select-color -3))

(define (guide-select-color-4)
  (guide-select-color -4))

#| ;;; Color Usage

;;; lowlevel, deprecated (kick (guide-current-colorscheme (make-guide-colorscheme colors: (vector Blue Brown Red Blue))))

(kick (guide-current-colorscheme (vector Blue Brown Red Blue)))

(kick (guide-current-colorscheme (guide-make-color-array (vector White Black Yellow Green))))

;;|#

;;** Fonts

(define Xglgui-font
  (let ((small.fnt DejaVuSans_14.fnt)
        (medium.fnt DejaVuSans_24.fnt)
        (large.fnt DejaVuSans_32.fnt))
    (define (select-font #!key (size 'small) (height #f))
      (case (cond
              ((eq? height #f) size)
              ((<= height 16) 'small)
              ((<= height 26) 'medium)
              ((<= height 35) 'large)
              (else 'small))
        ((small small:) small.fnt)
        ((medium small:) medium.fnt)
        ((large small:) large.fnt)
        (else small.fnt)))
    select-font))

(define guide-select-font Xglgui-font)

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

(define keypad:hexnum
  `(#;(
     (#\0 #\1 #\2 #\3)
     (#\4 #\5 #\6 #\7)
     (#\8 #\9 #\a #\b)
     (#\c #\d #\e #\f)
     ( (,delchar ,glgui_keypad_delete.img) #\[ #\: #\] (,retchar ,glgui_keypad_return.img))
     )
    (
     (#\1 #\2 #\3 #\[)
     (#\4 #\5 #\6 #\])
     (#\7 #\8 #\9 #\:)
     (#\a #\b #\c #\.)
     (#\d #\e #\f #\space)
     ( (,delchar ,glgui_keypad_delete.img) #\0 (,retchar ,glgui_keypad_return.img))
     )
    ))

(define Xglgui-value-edit-dialog
  (let ((orig.glgui-label glgui-label)
        (select-font Xglgui-font)
        (glgui-label Xglgui-label)
        (label-string (lambda (value) (if (string? value) value (object->string value)))))
    (define (change! gui wgt #!key (value #f))
      (if value (glgui-widget-set! gui wgt 'label (label-string value))))
    (define (value-edit-dialog
             gui x0 y0 w0 h0 #!key
             (label "") (label-height 20)
             (value "") (value-string #f)
             (input (lambda (val) #f)) (keypad keypad:numeric)
             (size 'small) (color (guide-select-color-4)) (bgcolor (guide-select-color-1)))
      (let* ((x (glgui-get gui 'xofs))
             (y (glgui-get gui 'yofs))
             (w (glgui-get gui 'w) #;(glgui-width-get))
             (h (glgui-get gui 'h) #;(glgui-height-get))
             (dlg (glgui-container gui x y w h))
             (bg (glgui-box dlg 0 0 w h bgcolor))
             (lbl (glgui-label
                   dlg 0 (- h (* 3/2 label-height)) w label-height
                   text: (label-string label) size: size color: color bgcolor: bgcolor))
             (wgt (glgui-label
                   dlg x (- h (* 32/10 label-height)) w label-height
                   text: ((or value-string label-string) value) size: size color: color bgcolor: bgcolor))
             (kbd (glgui-keypad dlg 0 0 w (/ h 2) (select-font size: 'medium) keypad)))
        (define (edit-cb cb-gui cb-wgt type x y)
          (let ((val (glgui-get wgt 'label)))
            (glgui-widget-delete gui dlg)
            (input val)))
        (glgui-widget-set! dlg lbl 'align GUI_ALIGNCENTER)
        (glgui-widget-set! dlg wgt 'align GUI_ALIGNCENTER)
        (when (procedure? input)
          (glgui-widget-set! dlg wgt 'enableinput #t)
          (glgui-widget-set! dlg wgt 'callback edit-cb)
          (glgui-widget-set! dlg wgt 'focus #t))
        (lambda args
          (match
           args
           ((value) (glgui-widget-set! dlg wgt 'label (label-string value)))
           ((arg1 arg2 . more) (apply change! dlg wgt arg1 arg2 more))
           (X (debug 'Komisch X)))
          (glgui-wakeup!))))
    value-edit-dialog))

(define (Xglgui-pin-editable bag x y w h tag pin kbd #!key (value-string #f))
  (define (conv v)
    (case v
      ((#f) "no")
      ((#t) "yes")
      (else
       (cond
        ((and (number? v) (exact? v)) (number->string v))
        (else v)))))
  (define (set-pin! val)
    (pin val))
  (define (pin-edit gui wgt type x1 y1)
    (Xglgui-value-edit-dialog
     bag x y w h
     label: tag
     value: (pin) value-string: value-string input: set-pin! keypad: kbd))
  (let ((setter (Xglgui-valuelabel
                 bag x y w h
                 label: tag label-width: 3/4
                 value: (conv (pin))
                 ;; TBD: on input create modal dialog with appropriate
                 ;; (for now numeric) keyboard
                 ;;
                 ;; input: #f
                 ;;
                 input: pin-edit
                 )))
    (wire! pin post: (lambda () (setter (conv (pin)))))))

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

;;; end Xglgui

(include "calculator.scm")

(include "guide-dropdown.scm")

(define make-tl-selection-payload make-selection-payload/dropdown)

;; LambdaNative glgui frame -- it's a bit tricky to work around that one.

(define-values
    (guide-main guide-exit)
  (let ((once main))
    (cond-expand
     (linux (define i3hook (delay (guide-floating-window!))))
     (else))
    (define (guide-main
             init #!key
             ;; (events #f)
             (suspend glgui-suspend)
             (resume (lambda ()
                       (glgui-wakeup!)
                       (glgui-resume)))
             (terminate (lambda () #t)))
      (let ((gui #f)
            (payload (lambda _ (error "wrong payload")))
            (todo once))
        (set! once (lambda _ (exit 23)))
        (todo
         ;; initialization
         (lambda (w h)
           (with-exception-catcher
            (lambda (exn)
              (handle-replloop-exception exn)
              (exit 32))
            (lambda ()
              (receive (a b) (init w h)
                (unless (and (guide-rectangle? a)
                             (or (guide-payload? b)
                                 (and (procedure? b) (guide-payload? (b)))))
                  (error "initialization must return a rectangle and a payload or payload accessor" a b))
                (set! gui a)
                (set! payload b)))))
         ;; events
         (lambda (event x y)
           (cond-expand
            (linux (force i3hook))
            (android
             (##thread-heartbeat!)
             (thread-sleep! 0.01))
            (else))
           (let ((cpl (if (procedure? payload) (payload) payload)))
             (guide-default-event-dispatch/toplevel gui cpl event x y)))
         ;; termination
         terminate
         ;; suspend
         suspend
         ;; resume
         resume
         )))
    (define (guide-exit code)
      (if (eq? once main) (exit code)))
    (values guide-main guide-exit)))


;; #eof
