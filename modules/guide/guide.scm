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

;;;** memoize inline (gambit)

(define-macro (macro-absent)
  ;; gambit specific
  '(##type-cast -6 (##type #f)))

(define-macro (macro-absent? x)
  ;; gambit specific
  `(eq? ,x (macro-absent)))

(define-macro (macro-alway-true-comparsion)
  ;; usually it's IMPORTANT to pass this as expression rather than a
  ;; free variable
  '(lambda (a b) #t))

(define-macro (macro-memoize:1->1 f cmp)
  (let ((last (gensym 'last))
        (current (gensym 'current))
        (value (gensym 'value)))
    `(let ((,last (macro-absent))
           (,value (macro-absent)))
       (lambda (,current)
         (cond
          ((and
            (,cmp ,current ,last)
            (not (macro-absent? ,value)))
           ,value)
          (else
           (set! ,value (,f ,current))
           (set! ,last ,current)
           ,value))))))

(define-macro (macro-memoize:2->1 f cmp1 cmp2)
  (let ((last-1 (gensym 'last-1))
        (current-1 (gensym 'current-1))
        (last-2 (gensym 'last-2))
        (current-2 (gensym 'current-2))
        (value (gensym 'value)))
    `(let ((,last-1 (macro-absent))
           (,last-2 (macro-absent))
           (,value (macro-absent)))
       (lambda (,current-1 ,current-2)
         (cond
          ((and
            (,cmp2 ,current-2 ,last-2)
            (,cmp1 ,current-1 ,last-1)
            (not (macro-absent? ,value)))
           ,value)
          (else
           (set! ,value (,f ,current-1  ,current-2))
           (set! ,last-1 ,current-1)
           (set! ,last-2 ,current-2)
           ,value))))))

;;;** Basic Generic Drawing.

(include "guide-figure.scm")

;;** Imports

(include "../misc-conventions/observable-syntax.sch")

;;* ACTUAL CODE

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

(define (MATURITY-2:guide-default-event-dispatch/fallback-to-glgui rect payload event x y)
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

(define guide-wakeup!)
(define %%guide-timings-set!) ;; deprecated

(define guide-default-event-dispatch/toplevel
  (let ((frame-period-max-value 0.5) ;; How long to sleep at most in redraw.
        (step 0.05) ;; delay increase
        (consecutive-redraw-count 1)
        (customized-moment #f) ;; may be a procedure returning the wait time/moment
        (wakeup-seen #f)
        (wait-mutex (make-mutex 'glgui-event))
        (wait-cv (make-condition-variable 'glgui-event)))
    (define (timings-set! #!key (frame-period-max #f) (frame-period-min #f) (frame-period-custom #f))
      (define (legal? x) (and (number? x) (positive? x)))
      (if (legal? frame-period-max) (set! frame-period-max-value frame-period-max))
      (if (legal? frame-period-min) (set! step frame-period-min))
      (if (or (not frame-period-custom) (procedure? frame-period-custom))
          (set! customized-moment frame-period-custom)))
    (define (wakeup!)
      (set! wakeup-seen #t)
      (condition-variable-signal! wait-cv))
    (define (reset-wait!)
      (set! wakeup-seen #f)
      (set! consecutive-redraw-count 1))
    (define (wait-for-time-or-signal!)
      ;; wait for delay or signal from other thread
      (if (if wakeup-seen
              (begin
                (mutex-unlock! wait-mutex)
                #f)
              (let ((moment (if customized-moment
                                (customized-moment consecutive-redraw-count)
                                (min frame-period-max-value (* consecutive-redraw-count step)))))
                (if (and (number? moment) (> moment 0))
                    (cond-expand
                     (win32
                      ;; Work around bug in gambit
                      (begin (mutex-unlock! wait-mutex wait-cv moment) wakeup-seen))
                     (else (mutex-unlock! wait-mutex wait-cv moment)))
                    (begin (mutex-unlock! wait-mutex) #t))))
          (reset-wait!)
          (set! consecutive-redraw-count (fx+ consecutive-redraw-count 1))))
    (define (guide-default-event-dispatch/toplevel rect payload event x y)
      (let ((event-handler (guide-payload-on-any-event payload)))
        (cond
         ((not (and glgui:active app:width app:height))
          (if (fx= t EVENT_REDRAW)
              (wait-for-time-or-signal!)
              (if customized-moment
                  (let ((moment (customized-moment 1)))
                    (when (and (number? moment) (> moment 0))
                      (thread-sleep! moment)))
                  (begin
                    (thread-sleep! step)
                    (reset-wait!)))))
         ((eq? event EVENT_REDRAW)
          ;; (log-status "REDRAW")
          (if (mutex-lock! wait-mutex 0)
              (begin
                (set! wakeup-seen #f)
                (glCoreInit)
                (event-handler rect payload event x y)
                (guide-meta-menu-draw!)
                (wait-for-time-or-signal!))
              (begin
                (log-warning "ignoring EVENT_REDRAW while handling EVENT_REDRAW")
                (set! wakeup-seen #t))))
         ((eq? event EVENT_IDLE)
          (thread-yield!)
          #t)
         ;; (check-magic-keys gui t x y)
         ((and (fx= event EVENT_KEYPRESS)
               (let ((termkey (guide-terminate-on-key)))
                 (and termkey (fx= x termkey))))
          (terminate))
         (else
          (reset-wait!)
          (kick! (lambda () (event-handler rect payload event x y)))))))
    (set! guide-wakeup! wakeup!)
    (set! %%guide-timings-set! timings-set!)
    (set! glgui-wakeup! wakeup!)
    (glgui-timings-set! frame-period-custom: (lambda (x) #f))
    guide-default-event-dispatch/toplevel))

(define $guide-frame-period)
(define %%guide-use-frame-period!
  (let ((last-frame-sec (time->seconds (current-time)))
        (frame-period 0.1))
    (define (wait x) ;; ignoring `x`
      (let ((next (+ last-frame-sec frame-period)))
        (set! last-frame-time next)
        (seconds->time next)))
    (set!
     $guide-frame-period
     (case-lambda
      (() frame-period)
      ((x)
       (unless (and (number? x) (not (negative? x)))
         (error "illegal argument" 'guide-frame-period x))
       (set! frame-period x))))
    (lambda ()
      (%%guide-timings-set! frame-period-custom: wait))))

;;;** Payload

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
             (else ;; legacy glgui
              (let ((glgui (guide-rectangle-glgui rect)))
                (when (table? glgui)
                  (glgui-widget-set! glgui wgt 'hidden #f))))))))
       (hidden
        (lambda (payload rect)
          (let ((wgt (guide-payload-widget payload)))
            (cond
             ((not wgt)) ;; TBD
             (else ;; legacy glgui
              (let ((glgui (guide-rectangle-glgui rect)))
                (when (table? glgui) (glgui-widget-set! glgui wgt 'hidden #t))))))))
       (remove
        (lambda (payload rect)
          (let ((wgt (guide-payload-widget payload)))
            (cond
             ((not wgt)) ;; TBD
             (else ;; legacy glgui
              (let ((glgui (guide-rectangle-glgui rect)))
                (when (table? glgui) (glgui-widget-delete glgui wgt))))))))
       (events (lambda (rect payload event x y) #f)))
   (lambda (#!key
            (in #f)
            (widget (make-gtable)) ;; TBD: change default to new style
            (lifespan #f) ;; BEWARE: default changed 20210117
            (on-any-event events)
            (gui-before exposed)
            (gui-after hidden))
     (unless (mdvector-interval? in)
       (error "make-guide-payload: 'in:' must be an interval" in))
     (unless (or (procedure? on-any-event) (not on-any-event))
       (error "illegal event handler" 'make-guide-payload on-any-event))
     (cond
      ((or (eq? lifespan #f) (eq? lifespan 'ephemeral))
       (make-guide-payload in widget on-any-event gui-before remove))
      (else (make-guide-payload in widget on-any-event gui-before gui-after))))))

(define guide-open-rectangle
  ;; A procedure of zero or one argument.  Without returns the current
  ;; payload, if given and a valid payload switch over calling
  ;; before/after thunks.
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
  (let ((w (glgui-width-get))
        (h (glgui-height-get)))
    (unless (and w h) (error "width/height invalid" current-guide-gui-interval w h))
    (make-mdv-rect-interval 0 0 w h)))

(define (guide-make-gui #!optional (content (make-glgui)))
  (MATURITY -2 "replaced by: guide-legacy-make-rect" loc: guide-make-gui)
  (make-guide-rectangle content (current-guide-gui-interval)))

(define (guide-legacy-make-rect #!optional (interval (current-guide-gui-interval)))
  (make-guide-rectangle (make-glgui) interval))

(define (guide-default-event-dispatch/fallback-to-glgui rect payload event x y)
  ;; fallback to glgui
  (let ((glgui (guide-payload-widget payload)))
    (cond
     ((not glgui)) ;; TBD
     #;((eq? event EVENT_REDRAW)
      (glgui-event glgui event x y)
      (guide-meta-menu-draw!))
     (else (glgui-event glgui event x y)))))

(define (guide-legacy-make-payload #!key (area (guide-legacy-make-rect)))
  ;; fresh glgui container to hold legacy glgui widgets
  ;;
  ;; Note: use for backward compatibility only!  Legacy is rather
  ;; expensive.
  (let* ((gui (guide-rectangle-glgui area))
         (interval (guide-rectangle-measures area))
         (xsw (mdvector-interval-lower-bound interval 0))
         (ysw (mdvector-interval-lower-bound interval 1))
         (xno (mdvector-interval-upper-bound interval 0))
         (yno (mdvector-interval-upper-bound interval 1))
         (w (- xno xsw))
         (h (- yno ysw))
         (widget (glgui-container gui xsw ysw w h)))
    (make-guide-payload
     in: interval widget: widget lifespan: 'ephemeral
     on-any-event: guide-default-event-dispatch/fallback-to-glgui)))

(define (guide-make-payload gui #!optional name) ;; ???
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

(define (guide-button
         #!key
         (in (current-guide-gui-interval))
         (font (guide-select-font size: 'medium))
         (label "exit")
         ;; TBD: better interface&name
         (guide-callback (lambda (rect payload event x y) (terminate))))
  (let* ((view! (make-guide-button-view))
         (label! (make-guide-label-view))
        ;;; TBD: inline these!
         (x (mdvector-interval-lower-bound in 0))
         (y (mdvector-interval-lower-bound in 1))
         (w (- (mdvector-interval-upper-bound in 0) x))
         (h (- (mdvector-interval-upper-bound in 1) y)))
    (unless (procedure? label) (label! text: label))
    (label! horizontal-align: 'center)
    (label! vertical-align: 'center)
    (label! font: (find-font font))
    (label! size: w h)
    (view! size: w h)
    (cond
     ((string? label) (view! foreground: (label!)))
     ((procedure? label)
      (let* ((cached
              (macro-memoize:2->1
               (lambda (update! str)
                 (update! text: str)
                 (update!))
               (macro-alway-true-comparsion) equal?))
             (check! (lambda (update!) (cached update! (label)))))
        (new! foreground: (label! check! (list "guide-button" label)))))
     (else (error "illegal label" guide-button label)))
    (view! position: x y)
    (let ((events
           (let ((armed #f) (redraw! (view!)))
             (lambda (rect payload event x y)
               (cond
                ((eqv? event EVENT_REDRAW) (redraw!))
                ((eqv? event EVENT_BUTTON1DOWN)
                 (if (guide-figure-contains? view! x y) (set! armed #t)))
                ((eqv? event EVENT_BUTTON1UP)
                 (when (and (guide-figure-contains? view! x y) armed)
                   (guide-callback rect payload event x y))
                 (set! armed #f))
                (else #f))))))
      (make-guide-payload in: in widget: #f on-any-event: events lifespan: 'ephemeral))))

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
    (guide-toplevel-payload guide-main guide-exit)
  (let ((once main) ;; lambdanative `main` called at most once
        (attributes #f)
        (payload #f))
    (cond-expand ;; funny stuff
     (linux (define i3hook (delay (guide-floating-window!))))
     (else))
    (define guide-toplevel-payload
      (case-lambda
       (() payload)
       ((obj)
        (unless (guide-payload? obj) (error "illegal payload" 'guide-toplevel-payload obj))
        (set! payload obj))))
    (define (guide-main
             init #!key
             ;; (events #f)
             (suspend glgui-suspend)
             (resume (lambda ()
                       (glgui-wakeup!)
                       (glgui-resume)))
             (terminate (lambda () #t)))
      (let ((gui #f)
            (todo once))
        (set! once (lambda _ (exit 23)))
        (todo
         ;; initialization
         (lambda (w h) ;; initial width&hight (from lambdanative event loop `ln-main`)
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
      (cond
       ((not (eq? once main))) ;; fall through to `ln-main` - dev compatible
       (payload ;; experimental: simple payload
        (guide-main
         (lambda (w h)
           ;; error checking callarguments (likely useless)
           (unless (and (number? w) (positive? w) (number? h) (positive? h))
             (error "useless arguments to guide-init" w h))
           (receive (w h orientation)
               (cond
                ((not attributes)
                 (let* ((interval (guide-payload-measures payload))
                        (w (mdvector-interval-upper-bound interval 0))
                        (h (mdvector-interval-upper-bound interval 1)))
                   (values w h GUI_PORTRAIT)))
                (else (NYIE)))
             (let ()
               ;; TBD: replace default operations:
               (make-window w h)
               (glgui-orientation-set! orientation)
               ;; finally
               (values
                (make-guide-rectangle
                 #f
                 (make-mdv-rect-interval ;; FIXME: get rid of that lowlevel stuff
                  0 0 w h))
                payload)))))
        ;; recurse
        (guide-exit code))
       (else (exit code))))
    (values guide-toplevel-payload guide-main guide-exit)))


;; #eof
