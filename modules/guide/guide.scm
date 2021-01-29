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
  name         ;; identifier for debugging
  measures     ;; interval
  widget       ;; glgui or extended
  on-redraw    ;; redraw event handler
  on-any-event ;; generic event dispatch (replace by r/w specific versions
  gui-before   ;; when exposed
  gui-after    ;; when hidden
  )


(define (guide-payload-contains/xy? obj x y)
  (unless (guide-payload? obj) (error "invalid payload" guide-payload-contains/xy? obj))
  (let ((measures (guide-payload-measures obj)))
    (and ;; chicking vertial first, as this is more likely to be off
     (>= y (mdvector-ref measures 0 1))
     (< y (mdvector-ref measures 1 1))
     (>= x (mdvector-ref measures 0 0))
     (< x (mdvector-ref measures 1 0)))))

(define (guide-rectangle-width obj)
  ;; FIXME: OVERHEAD
  (let ((interval (guide-rectangle-measures obj)))
    (- (mdvector-interval-upper-bound interval 0) (mdvector-interval-lower-bound interval 0))))

(define (guide-rectangle-height obj)
  ;; FIXME: OVERHEAD
  (let ((interval (guide-rectangle-measures obj)))
    (- (mdvector-interval-upper-bound interval 1) (mdvector-interval-lower-bound interval 1))))

(define guide-terminate-on-key (make-parameter EVENT_KEYESCAPE))

(define guide-meta-menu (make-parameter #f))
(define MATURITY+4:%%guide-overlay ;; note: keeping %%-prefix as this
                                   ;; might be debug only!
  (let ((c #f))
    (case-lambda
     (() c)
     ((n)
      (unless (or (not n) (procedure? n))
        (error "invalid argument" 'guide-overlay n))
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
                (if (or (time? moment) (and (number? moment) (> moment 0)))
                    (cond-expand
                     (win32
                      ;; Work around bug in gambit
                      (begin (mutex-unlock! wait-mutex wait-cv moment) wakeup-seen))
                     (else (mutex-unlock! wait-mutex wait-cv moment)))
                    (begin (mutex-unlock! wait-mutex) #t))))
          (reset-wait!)
          (set! consecutive-redraw-count (fx+ consecutive-redraw-count 1))))
    (define (guide-default-event-dispatch/toplevel rect payload event x y)
      (cond
       ((not (and glgui:active app:width app:height))
        (if (fx= t EVENT_REDRAW)
            (wait-for-time-or-signal!)
            (if customized-moment
                (let ((moment (customized-moment 1)))
                  (when (or (time? moment) (and (number? moment) (> moment 0)))
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
              (guide-event-dispatch-to-payload rect payload event x y)
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
        (kick! (lambda () (guide-event-dispatch-to-payload rect payload event x y))))))
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
        (set! last-frame-sec next)
        (seconds->time next)))
    (set!
     $guide-frame-period
     (case-lambda
      (() frame-period)
      ((x)
       (unless (and (number? x) (not (negative? x)))
         (error "invalid argument" 'guide-frame-period x))
       (set! frame-period x))))
    (lambda ()
      (%%guide-timings-set! frame-period-custom: wait))))

;;;** Payload

(define (guide-event-dispatch-to-payload/redraw payload)
  (let ((redraw (guide-payload-on-redraw payload)))
    (cond
     ;; TBD: check that payload is visible before
     (redraw
      (cond
       ((procedure? redraw) (redraw))
       ((vector? redraw)
        (do ((i 0 (fx+ i 1))) ;; draw background first
            ((eqv? i (##vector-length redraw)))
          ((##vector-ref redraw i))))))
     (else
      (let ((handler (guide-payload-on-any-event payload)))
        (cond
         ((procedure? handler) (handler 'rect payload EVENT_REDRAW 0 0))
         ((vector? handler)
          (do ((i 0 (fx+ i 1))) ;; draw background first
              ((eqv? i (##vector-length redraw)))
            ((##vector-ref handler i) 'rect payload EVENT_REDRAW 0 0)))
         ((not handler) (MATURITY -1 "no event handler for redraw" loc: guide-event-dispatch-to-payload/redraw))
         (else (MATURITY -10 "invalid event handler" loc: guide-event-dispatch-to-payload/redraw))))))))

(define (guide-event-dispatch-to-payload rect payload event x y)
  (cond
   ((eqv? event EVENT_REDRAW)
    (let ((redraw (guide-payload-on-redraw payload)))
      (cond
       ;; TBD: check that payload is visible before
       (redraw
        (cond
         ((procedure? redraw) (redraw))
         ((vector? redraw)
          (do ((i 0 (fx+ i 1))) ;; draw background first
              ((eqv? i (##vector-length redraw)))
            ((##vector-ref redraw i))))
         (else (MATURITY -10 "invalid redraw" loc: guide-event-dispatch-to-payload))))
       (else
        (let ((handler (guide-payload-on-any-event payload)))
          (cond
           ((procedure? handler) (handler rect payload event x y))
           ((vector? handler)
            (do ((i 0 (fx+ i 1))) ;; draw background first
                ((eqv? i (##vector-length redraw)))
              ((##vector-ref handler i) rect payload event x y)))
           ((not handler) (MATURITY -1 "no event handler for redraw" loc: guide-event-dispatch-to-payload))
           (else (MATURITY -10 "invalid event handler" loc: guide-event-dispatch-to-payload))))))))
   (else
    (let ((handler (guide-payload-on-any-event payload)))
      (cond
       ((procedure? handler) (handler rect payload event x y))
       ((vector? handler) ;; try handling in foreground first
        (do ((i (fx- (##vector-length handler) 1) (fx- i 1))
             (hit #f (and hit ((##vector-ref handler i) rect payload event x y))))
            ((or hit (eqv? i -1)) hit)))
       ((not handler) (MATURITY -1 "no event handler " loc: guide-event-dispatch-to-payload))
       (else (MATURITY -10 "invalid event handler" loc: guide-event-dispatch-to-payload)))))))

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
       (events (lambda (rect payload event x y)
                 (MATURITY -1 "default event hander not handling anything" loc: make-guid-payload)
                 #f)))
   (lambda (#!key
            (name #f)
            (in #f)
            (widget (make-gtable)) ;; TBD: change default to new style
            (lifespan #f) ;; BEWARE: default changed 20210117
            (on-redraw #f)
            (on-any-event events)
            (gui-before exposed)
            (gui-after hidden))
     (unless (mdvector-interval? in)
       (error "make-guide-payload: 'in:' must be an interval" in))
     (unless (or (procedure? on-redraw) (not on-redraw)
                 (and (vector? on-redraw)
                      (do ((i (fx- (vector-length on-redraw) 1) (fx- i 1)))
                          ((or (eqv? i -1)
                               (not (procedure? (vector-ref on-redraw i))))
                           (eqv? i -1)))))
       (error "invalid drawing handler" 'make-guide-payload on-redraw))
     (unless (or (procedure? on-any-event) (not on-any-event)
                 (and (vector? on-any-event)
                      (do ((i (fx- (vector-length on-any-event) -1) (fx- i 1)))
                          ((or (eqv? i -1)
                               (not (procedure? (vector-ref any-event i))))
                           (eqv? i -1)))))
       (error "invalid event handler" 'make-guide-payload on-any-event))
     (cond
      ((or (eq? lifespan #f) (eq? lifespan 'ephemeral))
       (make-guide-payload name in widget on-redraw on-any-event gui-before remove))
      (else (make-guide-payload name in widget on-redraw on-any-event gui-before gui-after))))))

(define guide-open-rectangle
  ;; A procedure of zero or one argument.  Without returns the current
  ;; payload, if given and a valid payload switch over calling
  ;; before/after thunks.
  (let ((empty (make-guide-payload
                name: 'empty
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
            (unless (guide-payload? new) (error "invalid payload" new))
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
     (else
      (MATURITY -3 "legacy rendering is very expensive"
                loc: guide-default-event-dispatch/fallback-to-glgui)
      (glgui-event glgui event x y)))))

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

(define guide-select-font
  (let ((small.fnt DejaVuSans_14.fnt)
        (medium.fnt DejaVuSans_24.fnt)
        (large.fnt DejaVuSans_32.fnt))
    (define (select-font #!key (size 'small) (height #f))
      (find-font
       (case (cond
              ((eq? height #f) size)
              ((<= height 16) 'small)
              ((<= height 26) 'medium)
              ((<= height 35) 'large)
              (else 'small))
         ((small small:) small.fnt)
         ((medium small:) medium.fnt)
         ((large small:) large.fnt)
         (else small.fnt))))
    select-font))

;;** Keys

(define (%%guide:legacy-special-key? x) ;; special keys
  (or
   (eqv? x EVENT_KEYENTER)
   (eqv? x EVENT_KEYTAB)
   (eqv? x EVENT_KEYBACKSPACE)
   (eqv? x EVENT_KEYRIGHT)
   (eqv? x EVENT_KEYLEFT)
   (eqv? x EVENT_KEYUP)
   (eqv? x EVENT_KEYDOWN)
   (eqv? x EVENT_KEYESCAPE)
   (eqv? x EVENT_KEYMENU)
   (eqv? x EVENT_KEYBACK)
   (eqv? x EVENT_KEYDELETE)
   (eqv? x EVENT_KEYHOME)
   (eqv? x EVENT_KEYEND)))

(define (%%guide:legacy-keycode->guide-keycode x)
  (if (%%guide:legacy-special-key? x) x (integer->char x)))

;;** GUI Widgets (payloads)

(define %%guide-default-background
  (make-glC:image 4 4 (glCoreTextureCreate 4 4 (make-u8vector 16 #xff)) 0.1 0.1 .9 .9))

(define-macro (macro-guide-default-background) `((lambda () %%guide-default-background)))

(define (guide-button
         #!key
         (in (current-guide-gui-interval))
         (font (guide-select-font size: 'medium))
         (label "exit")
         (color #f)
         (padding '#(1 1 1 1))
         (background #f)
         (background-color #f)
         (position #f)
         (horizontal-align 'center)
         (vertical-align 'center)
         (location guide-button)
         ;; TBD: better interface&name
         (guide-callback (lambda (rect payload event x y) (terminate))))
  (let* ((view! (make-guide-figure-view))
         (label! (make-guide-label-view))
        ;;; TBD: inline these!
         (x (mdvector-interval-lower-bound in 0))
         (y (mdvector-interval-lower-bound in 1))
         (w (- (mdvector-interval-upper-bound in 0) x))
         (h (- (mdvector-interval-upper-bound in 1) y)))
    (label! horizontal-align: horizontal-align)
    (label! vertical-align: vertical-align)
    (label! font: (find-font font))
    (when color (label! color: color))
    (label! size: w h)
    (label! padding: padding)
    ;; finally - in order to ot trigger useless recalculations
    (unless (procedure? label) (label! text: label))
    (view! size: w h)
    (cond
     ((or (string? label) (glC:image? label)) (view! foreground: (label!)))
     ((procedure? label)
      (let* ((cached
              (macro-memoize:2->1
               (lambda (update! str)
                 (update! text: str)
                 (update!))
               (macro-alway-true-comparsion) equal?))
             (check! (lambda (update!) (cached update! (label)))))
        (view! foreground: (label! check! (list "guide-button" label)))))
     (else (error "invalid label" guide-button label)))
    (when background
      (view!
       texture:
       (if (fixnum? background)
           (begin
             (MATURITY -1 "background: converting fixnum to texture" loc: location)
             (%%glCore:textures-ref background #f))
           background)))
    (when background-color (view! color: background-color))
    (if position
        (view! position: (vector-ref position 0) (vector-ref position 1))
        (view! position: x y))
    (let ((events
           (let ((armed #f))
             (lambda (rect payload event x y)
               (cond
                ((eqv? event EVENT_BUTTON1DOWN)
                 (if (guide-figure-contains? view! x y) (begin (set! armed #t) #t) #f))
                ((eqv? event EVENT_BUTTON1UP)
                 (if (and (guide-figure-contains? view! x y) armed)
                     (begin
                       (set! armed #f)
                       (guide-callback rect payload event x y))
                     (begin
                       (set! armed #f)
                       #f)))
                (else (guide-figure-contains? view! x y)))))))
      (make-guide-payload in: in widget: #f on-redraw: (view!) on-any-event: events lifespan: 'ephemeral))))

(define (make-guide-table
         ;; CONTRUCTOR (lambda (contructor INTERVAL COL ROW . rest) . rest)
         ;; CONTRUCTOR+ARGS: (or CONTRUCTOR (CONTRUCTOR . ARGS))
         contructors ;; mdvector of CONTRUCTOR+ARGS
         #!key
         (in (current-guide-gui-interval))
         (font #f)
         (border-ratio 1/20)
         (background-image (macro-guide-default-background))
         (on-key #f)
         )
  (let* ((error-location (lambda (col row) (list 'make-guide-table-payload col row)))
         (rng (mdvector-range contructors))
         (idx (mdv-indexer rng))
         (content (make-vector (range-volume rng) #f))
         (columns (range-size rng 0))
         (rows (range-size rng 1))
         (yu (mdvector-interval-upper-bound in 1))
         (yl (mdvector-interval-lower-bound in 1))
         (total-height (- yu yl))
         (xu (mdvector-interval-upper-bound in 0))
         (xl (mdvector-interval-lower-bound in 0))
         (total-width (- xu xl))
         (colw (floor (/ total-width columns)))
         (min-hgap (ceiling (* border-ratio colw)))
         (cell-width (- colw (* 2 min-hgap)))
         (used-width (* columns (+ cell-width (* 2 min-hgap))))
         (hgap (* (/ total-width used-width) min-hgap))
         (left-offset (* 1/2 (+ (* 2 hgap) (/ (- total-width used-width) 2))))
         ;;
         (rowh (floor (/ total-height rows)))
         (min-vgap (ceiling (* border-ratio rowh)))
         (cell-height (- rowh (* 2 min-vgap)))
         (used-height (* rows (+ cell-height (* 2 min-vgap))))
         (vgap (* (/ total-height used-height) min-vgap))
         (vertical-offset (- yu (/ (- total-height used-height) 2) (* 2 vgap)))
         (font (or font (guide-select-font height: cell-height))))
    (for-range2
     rng
     (lambda (row col)
       (let* ((idx (idx row col))
              (pattern-vector (mdvector-body contructors))
              (pattern (vector-ref pattern-vector idx))
              (payload
               (and
                pattern
                (let* ((colspan (do ((i (fx+ col 1) (fx+ i 1))
                                     (span 0 (fx+ span 1)))
                                    ((or (eqv? i columns)
                                         (mdvector-ref contructors row i))
                                     span)))
                       (rowspan (do ((i (fx+ row 1) (fx+ i 1))
                                     (span 0 (fx+ span 1)))
                                    ((or (eqv? i rows)
                                         (mdvector-ref contructors i col))
                                     span)))
                       (x0 (+ left-offset (* col (+ hgap cell-width))))
                       (y0 (- vertical-offset
                              (+ cell-height (* (+ row rowspan) (+ vgap cell-height)))))
                       (x1 (+ x0 cell-width (* colspan (+ hgap cell-width))))
                       (y1 (+ cell-height y0 (* rowspan (+ vgap cell-height))))
                       (area (make-x0y0x1y1-interval/coerce x0 y0 x1 y1)))
                  (cond
                   ((procedure? pattern) (pattern area col row))
                   ((pair? pattern) (apply (car pattern) area col row (cdr pattern)))
                   (else (error "invalid constructor spec" (error-location col row) pattern)))))))
         (when payload
           (unless (or (guide-payload? payload))
             (error "invalid payload from constructor" (error-location col row) pattern payload))
         (vector-set! content idx payload)))))
    ;; TBD: maybe reduce event handlers to those actually having a payload.
    (make-guide-payload
     in: in
     on-redraw:
     (lambda () ;; better return customized vector; draw sequence does not matter
       (do ((i (fx- (vector-length content) 1) (fx- i 1)))
           ((eqv? i -1))
         (let ((payload (vector-ref content i)))
           (and
            payload
            (let ((draw (guide-payload-on-redraw payload)))
              (and draw (draw)))))))
     on-any-event:
     (lambda (rect payload event x y)
       (cond
        ((eqv? event EVENT_KEYPRESS)
         (and on-key (on-key press: (%%guide:legacy-keycode->guide-keycode x))))
        ((eqv? event EVENT_KEYRELEASE)
         (and on-key (on-key release: (%%guide:legacy-keycode->guide-keycode x))))
        ((guide-payload-contains/xy? payload x y)
         (do ((i (fx- (vector-length content) 1) (fx- i 1))
              (hit #f))
             ((or hit (eqv? i -1)) #|signal event handled in any case:|# #t)
           (let ((payload (vector-ref content i)))
             (and payload
                  (and
                   (guide-payload-contains/xy? payload x y)
                   (begin
                     (set! hit #t)
                     (guide-event-dispatch-to-payload rect payload event x y)))))))
        (else (debug 'guide-table:ignored-event event))))
     ;; backward compatibility
     widget: #f lifespan: 'ephemeral)))

(define (guide-valuelabel
         #!key
         (in (current-guide-gui-interval))
         (label "") (label-width 1/2)
         (value #f) (input #f)
         (size 'small) (color (guide-select-color-4))
         (success values))
  (unless (or (string? value) (procedure? value))
    (error "invalid value argument" 'guide-valuelabel value))
  (let* ((x (mdvector-interval-lower-bound in 0))
         (y (mdvector-interval-lower-bound in 1))
         (xno (mdvector-interval-upper-bound in 0))
         (yno (mdvector-interval-upper-bound in 1))
         (w (- xno x))
         (h (- yno y))
         (tag! (make-guide-label-view))
         (label! (make-guide-label-view))
         (box! (MATURITY+2:make-guide-bg+fg-view))
         (check!
          (and
           (procedure? value)
           (let ((src value)
                 (cached
                  (macro-memoize:2->1 ;;memoize-last
                   (lambda (update! str)
                     (update! text: str)
                     (update!))
                   (lambda (a b) #t) equal?)))
             (lambda (update!) (cached update! (src)))))))
    (let ((lw (* w (- 1 label-width)))
          (gap (/ h 5))
          (font (guide-select-font size: size)))
      (tag! size: (- (* w label-width) gap) h)
      (label! size: lw h)
      (tag! font: font)
      (label! font: font)
      (tag! vertical-align: 'center)
      (label! vertical-align: 'center)
      (tag! horizontal-align: 'right)
      (label! horizontal-align: 'left)
      (tag! color: color)
      (label! color: color))
    (tag! text: label)
    (label! text: (if (procedure? value) (value) value))
    (label! position: (* w (- 1 label-width)) 0)
    (box! size: w h)
    (box! background: (tag!))
    (box! foreground: (label! check! label))
    (box! position: x y)
    (let ((interval (make-mdv-rect-interval x y (+ x w) (+ y h)))
          (events
           (lambda (rect payload event x y)
             (cond
              ((guide-figure-contains? box! x y)
               (or (not input) (input rect payload event x y)))
              (else #f)))))
      (let ((payload (make-guide-payload in: interval widget: #f on-redraw: (box!) on-any-event: events)))
        (cond
         ((procedure? value) payload)
         (else
          (success
           payload
           (case-lambda
            (() value)
            ((x)
             (unless (string? x)
               (set! x (call-with-output-string (lambda (p) (display x)))))
             (set! value x)
             (label! text: x)
             (box! foreground: (label!))
             #!void)))))))))

(define (guide-make-keypad
         area spec
         #!key
         (font (guide-select-font size: 'medium))
         (color (guide-select-color-2))
         (background-color (guide-select-color-1))
         (on-key #f))
  (define (%%guide-post-key-event key)
    (cond
     ((fixnum? key)
      (if (procedure? on-key) (on-key release: key) (event-push EVENT_KEYRELEASE key 0)))
     ((char? key)
      (if (procedure? on-key) (on-key release: key) (event-push EVENT_KEYRELEASE (char->integer key) 0)))
     (else (error "invalid key" %%guide-post-key-event key)))
    #t)
  (define (post-key pat)
    (lambda (rect payload event x y)
      (%%guide-post-key-event pat)))
  (define (keybutton in c #!key (label (string c)) (color color) (background-color background-color))
    (guide-button
     in: in label: label color: color font: font
     background-color: background-color guide-callback: (post-key c)))
  (let* ((rng (mdvector-range spec))
         (constructors
          (make-vector
           (range-volume rng)
           (lambda (in col row)
             (let ((pat (mdvector-ref spec row col)))
               (cond
                ((char? pat) (keybutton in pat))
                ((and (fixnum? pat) (positive? pat)) (keybutton in (integer->char pat)))
                ((pair? pat) (apply keybutton in pat))
                ((not pat)
                 (MATURITY -2 "FIXME: something is broken wrt. span i keypads" loc: 'guide-keypad)
                 (guide-button
                  in: in label: "#f" color: Red font: font
                  background-color: background-color guide-callback: (lambda _ #t)))
                (else (error "invalid key spec" guide-make-keypad pat))))))))
    (make-guide-table (make-mdvector rng constructors) in: area)))

(define (guide-line-input
         #!key
         (in (current-guide-gui-interval))
         (font (guide-select-font size: 'medium))
         (horizontal-align 'center)
         (vertical-align 'center)
         (line-height 20)
         (data (let ((state "n/a")) (case-lambda (() state) ((val) (set! state val)))))
         (size 'small)
         (color (guide-select-color-2))
         (hightlight-color (guide-select-color-4)))
  (define (%%value-buffer->string ggb #!optional (start 0) (end (ggb-length ggb)))
    (let ((result (make-string (- end start))))
      (ggb-for-each ggb (lambda (i v) (string-set! result i (integer->char v))) start end)
      result))
  (define (input->buffer data)
    (let* ((initial (utf8string->u32vector (data)))
           (buffer (make-ggb size: (u32vector-length initial))))
      (do ((i 0 (fx+ i 1)))
          ((eqv? i (u32vector-length initial)) buffer)
        (ggb-insert! buffer (u32vector-ref initial i)))))
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
       (update-cursor!
        ;; TBD: this is a bit overly simple and needlessly expensive
        ;; at runtime
        (lambda ()
          (let* ((total (%%value-buffer->string value-buffer))
                 (before (%%value-buffer->string value-buffer 0 (ggb-point value-buffer)))
                 (glv-before (MATURITY+0:utf8string->guide-glyphvector before font))
                 (glv-total (MATURITY+0:utf8string->guide-glyphvector total font))
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
            (label! text: "|")
            (let ((draw
                   (if #t ;; blink
                       (let ((on (label!)))
                         (lambda ()
                           (let* ((n0 ##now)
                                  (n0f (floor n0)))
                             (when (< (- n0 n0f) 1/2) (on)))))
                       (label!))))
              (set! cursor-draw draw)))))

       (on-key
        (lambda (p/r key)
          (or
           (eq? press: p/r) ;; ignore press - maybe more
           (cond
            ((eqv? key EVENT_KEYRIGHT) (ggb-goto-right! value-buffer) (update-cursor!))
            ((eqv? key EVENT_KEYLEFT) (ggb-goto-left! value-buffer) (update-cursor!))
            ((eqv? key EVENT_KEYBACKSPACE)
             (ggb-delete! value-buffer -1)
             (value! text: (%%value-buffer->string value-buffer))
             (set! value-draw (value!))
             (update-cursor!))
            ((eqv? key EVENT_KEYENTER)
             (data (%%value-buffer->string value-buffer))
             (set! value-buffer (input->buffer data))
             (value! text: (%%value-buffer->string value-buffer))
             (set! value-draw (value!))
             (update-cursor!))
            (else
             (ggb-insert! value-buffer (char->integer key))
             (value! text: (%%value-buffer->string value-buffer))
             (set! value-draw (value!))
             (update-cursor!))))))
       (redraw! (vector
                 (lambda () (and value-draw (value-draw)))
                 (lambda () (cursor-draw))))
       (events
        (lambda (rect payload event x y)
          (cond
           ((eqv? event EVENT_KEYPRESS)
            (on-key press: (%%guide:legacy-keycode->guide-keycode x)))
           ((eqv? event EVENT_KEYRELEASE)
            (on-key release: (%%guide:legacy-keycode->guide-keycode x)))
           (else (mdvector-rect-interval-contains/xy? in x y))))))
    (update-cursor!)
    (make-guide-payload
     name: 'guide-line-input in: in widget: #f
     on-redraw: redraw! on-any-event: events lifespan: 'ephemeral)))

(include "calculator.scm")

;; LambdaNative glgui frame -- it's a bit tricky to work around that one.

;#| deprecated dependency `make-window`
(define (%%MATURITY+2:guide:make-window w h) ;; deprecated
  (MATURITY 1 "experimental, simplified" loc: %%MATURITY+2:guide:make-window)
  (let* ((xscale (/ w app:screenwidth))
         (yscale (/ h app:screenheight)))
    ;; register width&height to be used after initialization to
    ;; actually create the window.
    (set! app:width w)
    (set! app:height h)
    (if (or app:forcefullscreen ;; fullscreen unsuded here (broken)
            (string=? (system-platform) "ios")
            (string=? (system-platform) "bb10")
            (string=? (system-platform) "playbook")
            (string=? (system-platform) "android"))
        (begin
          (set! app:xscale xscale)
          (set! app:yscale yscale)
          (set! app:scale? #t)))))

(define %%guide:make-window-initial ;; internal
  ;; not inlined for easier debugging and called once only
  (let ((once %%MATURITY+2:guide:make-window))
    (lambda args
      (unless once (error "once only call" %%guide:make-window-initial args))
      (let ((todo once))
        (set! once #f)
        (apply todo args)))))

(set! make-window (lambda args (error "guide: NO LONGER SUPPORTED: make-window" args)))
;;|# end deprecated dependency `make-window`

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
        (unless (guide-payload? obj) (error "invalid payload" 'guide-toplevel-payload obj))
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
               (%%guide:make-window-initial w h)
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
