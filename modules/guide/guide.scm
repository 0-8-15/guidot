;;** Missplaced Code

(include "../../apps/guide/DejaVuSans-14,24,32.scm")

(cond-expand
 (debug
  (define-macro (assume obj msg . more)
    `(if ,obj ,obj (apply error ,msg ,more))))
 (else
  (define-macro (assume obj msg . more) obj)))

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

(define (current-guide-gui-interval)
  (let ((w (glgui-width-get))
        (h (glgui-height-get)))
    (unless (and w h) (error "width/height invalid" current-guide-gui-interval w h))
    (make-mdv-rect-interval 0 0 w h)))

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
            (not (macro-absent? ,value))
            (,cmp ,current ,last))
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
            (not (macro-absent? ,value))
            (,cmp2 ,current-2 ,last-2)
            (,cmp1 ,current-1 ,last-1))
           ,value)
          (else
           (set! ,value (,f ,current-1  ,current-2))
           (set! ,last-1 ,current-1)
           (set! ,last-2 ,current-2)
           ,value))))))

(define-macro (macro-memoize:3->1 f cmp1 cmp2 cmp3)
  (let ((value (gensym 'value))
        (last-1 (gensym 'last-1))
        (current-1 (gensym 'current-1))
        (last-2 (gensym 'last-2))
        (current-2 (gensym 'current-2))
        (last-3 (gensym 'last-3))
        (current-3 (gensym 'current-3)))
    `(let ((,last-1 (macro-absent))
           (,last-2 (macro-absent))
           (,last-3 (macro-absent))
           (,value (macro-absent)))
       (lambda (,current-1 ,current-2 ,current-3)
         (cond
          ((and
            (not (macro-absent? ,value))
            (,cmp3 ,current-3 ,last-3)
            (,cmp2 ,current-2 ,last-2)
            (,cmp1 ,current-1 ,last-1))
           ,value)
          (else
           (set! ,value (,f ,current-1 ,current-2 ,current-3))
           (set! ,last-1 ,current-1)
           (set! ,last-2 ,current-2)
           (set! ,last-3 ,current-3)
           ,value))))))

(define-macro (macro-memoize:4->1 f cmp1 cmp2 cmp3 cmp4)
  (let ((value (gensym 'value))
        (last-1 (gensym 'last-1))
        (current-1 (gensym 'current-1))
        (last-2 (gensym 'last-2))
        (current-2 (gensym 'current-2))
        (last-3 (gensym 'last-3))
        (current-3 (gensym 'current-3))
        (last-4 (gensym 'last-4))
        (current-4 (gensym 'current-4)))
    `(let ((,last-1 (macro-absent))
           (,last-2 (macro-absent))
           (,last-3 (macro-absent))
           (,last-4 (macro-absent))
           (,value (macro-absent)))
       (lambda (,current-1 ,current-2 ,current-3 ,current-4)
         (cond
          ((and
            (not (macro-absent? ,value))
            (,cmp4 ,current-4 ,last-4)
            (,cmp3 ,current-3 ,last-3)
            (,cmp2 ,current-2 ,last-2)
            (,cmp1 ,current-1 ,last-1))
           ,value)
          (else
           (set! ,value (,f ,current-1 ,current-2 ,current-3 ,current-4))
           (set! ,last-1 ,current-1)
           (set! ,last-2 ,current-2)
           (set! ,last-3 ,current-3)
           (set! ,last-4 ,current-4)
           ,value))))))

;;;** Basic Generic Drawing.

(include "guide-figure.scm")

;;** Imports

(include "../misc-conventions/observable-syntax.sch")

(include "guide-pikchr.scm")

(include "guide-frame.scm")

;;* ACTUAL CODE

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
  (case x
    ((#xff55) 'PageUp)
    ((#xff56) 'PageDown)
    (else
     (cond
      ((symbol? x) x)
      ((%%guide:legacy-special-key? x) x)
      (else (integer->char x))))))

(define (guide-event-graphics? event)
  (or
   (eqv? EVENT_MOTION event)
   (eqv? EVENT_BUTTON1UP event)
   (eqv? EVENT_BUTTON1DOWN event)
   (eqv? EVENT_BUTTON2UP event)
   (eqv? EVENT_BUTTON2DOWN event)
   (eqv? EVENT_BUTTON3UP event)
   (eqv? EVENT_BUTTON3DOWN event)
   ;; (eqv? EVENT_MULTITOUCH event) ;; ???
   ))

;;** Payload Model

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

(define (guide-boundingbox->quadrupel obj)
  (cond
   ((mdvector-interval? obj) (mdv-rect-interval->quadrupel obj))
   ((guide-payload? obj) (mdv-rect-interval->quadrupel (guide-payload-measures obj)))
   ((guide-rectangle? obj) (mdv-rect-interval->quadrupel (guide-rectangle-measures obj)))
   (else (error "invalid argument" guide-boundingbox->quadrupel obj))))

(define (guide-payload-width obj)
  (mdv-rect-interval-width (guide-payload-measures obj)))

(define (guide-payload-height obj)
  (mdv-rect-interval-height (guide-payload-measures obj)))

(define (guide-rectangle-width obj)
  (mdv-rect-interval-width (guide-rectangle-measures obj)))

(define (guide-rectangle-height obj)
  (mdv-rect-interval-height (guide-rectangle-measures obj)))

(define guide-terminate-on-key (make-parameter EVENT_KEYESCAPE))

(define guide-focus
  (make-pin
   initial: #f
   pred: (lambda (x) (or (not x) (guide-payload? x)))
   name: 'guide-focus))

(wire!
 guide-focus sequence:
 (lambda (before value)
   (kick
     (when before
       (guide-event-dispatch-to-payload
        (guide-payload-measures before) before focus: #f 0))
     (when value
       (guide-event-dispatch-to-payload
        (guide-payload-measures value) value focus: #t 0)))))

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

(define %%guide-wait-mutex
  (let ((mux (make-mutex 'guide-wait-mutex)))
    (mutex-specific-set! mux (make-condition-variable 'guide-wakeup))
    mux))
(define (guide-wakeup!)
  (condition-variable-signal! (mutex-specific %%guide-wait-mutex)))

(define guide-default-event-dispatch/toplevel
  (let ()
    (define (redraw! rect payload event x y)
      (glCoreInit)
      (guide-event-dispatch-to-payload rect payload event x y)
      (guide-meta-menu-draw!)
      (microgl-swapbuffers))
    (define (activate! rect payload event x y)
      (define (activate! rect payload event x y)
        (let loop
            ((finally
              (kick! (lambda () (guide-event-dispatch-to-payload rect payload event x y)))))
          (cond
           ((##promise? finally) (loop (force finally)))
           ((procedure? finally) (loop (finally)))
           ((box? finally)
            (MATURITY -1 "TBD: avoid boxed returns" loc: guide-default-event-dispatch/toplevel:activate!)
            (loop (unbox (debug 'finally finally))))
           (else ;; debugging
            (cond
             ((boolean? finally)) ;; OK, expected
             (else (debug 'ignored-payload-result (list finally payload)))))))
        (and #f ;; $guide-wakeup-on-any-event
             (guide-wakeup!))
        ;; not sure what needs to be returned, we are done handling results
        #t)
      (cond-expand
       (debug
        (check-not-observable-speculative! guide-default-event-dispatch/toplevel 'activate!)
        (with-debug-exception-catcher ;; debugging
         ;; (lambda (exn) (raise (debug 'FAIL-SO-BADLY exn)))
         (lambda () (activate! rect payload event x y))))
       (else (activate! rect payload event x y))))
    (define (guide-default-event-dispatch/toplevel rect payload event x y)
      (cond
       ((not (and glgui:active app:width app:height)))
       ((eq? event EVENT_REDRAW)
        (redraw! rect payload event x y))
       ((eq? event EVENT_IDLE))
       ((or (eq? event EVENT_KEYPRESS) (eq? event EVENT_KEYRELEASE))
        (set! x (%%guide:legacy-keycode->guide-keycode x))
        (unless (keyword? event)
          (set! event (if (eq? event EVENT_KEYPRESS) press: release:)))
        (cond
         ((and ;; (check-magic-keys gui t x y)
           (let ((termkey (guide-terminate-on-key)))
             (and termkey (eqv? x termkey))))
          (terminate))
         ((guide-focus) =>
          (lambda (payload) ;; redirecting to focus payload here
            (activate! rect payload event x y)))
         (else (activate! rect payload event x y))))
       (else (activate! rect payload event x y))))
    guide-default-event-dispatch/toplevel))

;;;** Payload (De)Construction

(define (%%guide-make-redraw spec)
  (cond
   ((vector? spec)
    (lambda ()
      (do ((i 0 (fx+ i 1))) ;; draw background first
          ((eqv? i (##vector-length spec)))
        ((##vector-ref spec i)))))))

(define (%%guide-make-redraw/check spec)
  (cond
   ((vector? spec)
    (lambda ()
      (do ((i 0 (fx+ i 1))) ;; draw background first
          ((eqv? i (##vector-length spec)))
        (let ((draw (##vector-ref spec i)))
          (and (procedure? draw) (draw))))))))

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

(cond-expand
 (debug
  (define $guide-event-dispatch-log (make-parameter #f)))
 (else
  (define ($guide-event-dispatch-log . args) 'unsupported)))

(define (guide-event-dispatch-to-payload rect payload event x y)
  (cond-expand
   (debug
    (let ((logger ($guide-event-dispatch-log)))
      (when (procedure? logger)
        (logger rect payload event x y))))
   (else))
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
       ((not handler) (MATURITY -1 "no event handler " loc: guide-event-dispatch-to-payload) #f)
       (else (MATURITY -10 "invalid event handler" loc: guide-event-dispatch-to-payload)))))))

(cond-expand
 (debug
  (set! guide-event-dispatch-to-payload
        (let ((guide-event-dispatch-to-payload guide-event-dispatch-to-payload))
          (lambda (rect payload event x y)
            (let ((result (guide-event-dispatch-to-payload rect payload event x y)))
              (cond
               ((eqv? event EVENT_REDRAW)) ;; returns void
               ((eqv? result #!void) (error "void returned" payload event x y)))
              result)))))
 (else))

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
            (widget #f) ;; default: not legacy glgui widget
            (lifespan #f) ;; BEWARE: default changed 20210117
            (on-redraw #f)
            (on-any-event events)
            (gui-before #f)
            (gui-after #f))
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
                      (do ((i (fx- (vector-length on-any-event) 1) (fx- i 1)))
                          ((or (eqv? i -1)
                               (not (procedure? (vector-ref any-event i))))
                           (eqv? i -1)))))
       (error "invalid event handler" 'make-guide-payload on-any-event))
     (cond
      ((or (eq? lifespan #f) (eq? lifespan 'ephemeral))
       (make-guide-payload name in widget on-redraw on-any-event gui-before gui-after))
      (else
       (when widget ;; legacy glgui backward compatibility
         (set! gui-before exposed)
         (set! gui-after hidden))
       (make-guide-payload name in widget on-redraw on-any-event gui-before gui-after))))))

(define guide-critical-add!
  ;; During speculative execution phase: register operation to be
  ;; executed in critical phase (with STM lock held).  Optionally
  ;; starting a thread for it when the async: option is specified as
  ;; #t.
  (let ()
    (define critical-calls
      ;; Guide *Global* Critical Section (GGCS)
      ;;
      ;; suspended computation (currently thunk or promise)
      (let ((receiver
             (make-pin
              initial: '()
              name: "GGCS (critical sections): a list of suspended computations")))
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
           (for-each
            (lambda (new)
              (cond
               ((procedure? new) (new))
               (else (force new))))
            ;; This is better moved into `likely` - we MUST clear the
            ;; list within the critical section.
            (let* ((tbd (reverse new)))
              (set-cdr! new '())
              (set-car! new #f)
              tbd))
           #f))
        receiver))
    (lambda (obj #!key (async #f) (once #f))
      (assume (or (procedure? obj) (promise? obj))
              "invalid" guide-critical-add! obj)
      (cond
       (async
        (when once (error "invalid async and once are exclusive"
                          guide-critical-add! async once))
        (let ((wrapped
               (lambda ()
                 (thread-start!
                  (make-thread
                   (cond
                    ((procedure? obj) obj)
                    (else (lambda () (force obj))))
                   obj)))))
          (critical-calls (cons wrapped (critical-calls)))))
       (else
        (cond
         (once (let ((registered (critical-calls)))
                 (unless (memq obj registered)
                   (critical-calls (cons obj (critical-calls))))))
         (else (critical-calls (cons obj (critical-calls)))))))
      ;; for call site convinience, return #t, not unspecified #!void
      #t)))

(define guide-open-rectangle ;; onsolete?
  ;;
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

;;** Legacy Compatibility

(define (guide-make-gui #!optional (content (make-glgui)))
  (MATURITY -2 "replaced by: guide-legacy-make-rect" loc: guide-make-gui)
  (make-guide-rectangle content (current-guide-gui-interval)))

(define (guide-legacy-make-rect #!optional (interval (current-guide-gui-interval)))
  (make-guide-rectangle (make-glgui) interval))

(define (guide-default-event-dispatch/fallback-to-glgui rect payload event x y)
  ;; fallback to glgui
  (MATURITY -10 "conversion to legacy event representation NYI"
            loc: guide-default-event-dispatch/fallback-to-glgui)
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
    (guide-define-payload guide-payload-ref guide-payload-names)
  (let ((defined (make-table))
        ;; payload-contructor? should become a specialized/tagged procedure
        (payload-contructor? procedure?))
    (define (guide-make-payload-definition-once constructor)
      ;; This locking is supposed to be useless overhead; just to be
      ;; safe we run into exceptions when missued.
      (let ((once (make-mutex constructor)))
        (lambda (interval)
          (if (mutex? once)
              (let ((seen once))
                (mutex-lock! seen)
                (when (mutex? once)
                  (set! once (constructor interval)))
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
    (define (guide-payload-names)
      (list->vector (sort! string<? (map car (table->list defined)))))
    (values
     guide-define-payload
     guide-payload-ref
     guide-payload-names)))

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
           initial: (convert #f (apply vector (map make-glColor (list Black Orange SlateGray White))))
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
              ((<= height 20) 'small)
              ((<= height 26) 'medium)
              (else 'large))
         ((small small:) small.fnt)
         ((medium small:) medium.fnt)
         ((large small:) large.fnt)
         (else small.fnt))))
    select-font))

(define (guide-font-height font)
  ;;(unless (ln-ttf:font? font) (error "invalid font argument" guide-font-height font))
  ;;(assume (ln-ttf:font? font) "invalid font argument" guide-font-height font)
  (let ((override (ln-ttf:font-ref font (char->integer #\|))))
    (cond
     (override (+ 2 (ttf:glyph-height override))) ;; TBD: +2 is garbage
     (else
      (MATURITY -2 "failed to find font height" loc: guide-font-height)
      40))))

;;** Style

(include "guide-style.scm")

;;** GUI Widgets (payloads)

;;*** Widget Utilities

(define %%guide-default-background
  (let* ((fuzz 0.2) (fuzz- (fl- 1. fuzz)))
    ;; Without the fuzz factor at least win32 renders gradient
    ;; transparency at the edges.
    (make-glC:image 4 4 (glCoreTextureCreate 4 4 (make-u8vector 16 #xff)) fuzz fuzz fuzz- fuzz-)))

(define (guide-background
         key
         #!key
         (in (NYIE "in: key missing" guide-background))) ;; (current-guide-gui-interval)
  (case key
    ((button:)
     (make-glC:image
      (mdv-rect-interval-width in)
      (mdv-rect-interval-height in)
      0 0. 1. .7734375 .375))
    ((default: default)
     (cond
      ((not in) %%guide-default-background)
      (else
       (let* ((fuzz 0.2) (fuzz- (fl- 1. fuzz)))
         (make-glC:image
          (mdv-rect-interval-width in)
          (mdv-rect-interval-height in)
          (glC:image-t %%guide-default-background) fuzz fuzz fuzz- fuzz-)))))
    (else (error "invalid argument" guide-background key))))

(define-macro (macro-guide-default-background) `((lambda () %%guide-default-background)))

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
  `(begin
     (guide-critical-add!
      (lambda () (macro-guide-execute-payload-result ,expr))
      async: #t)
     #t))

;;*** Widget Composition

(define guide-scissor-layout
  (let ((current-scissor-area #f))
    (define (set-current! area)
      (set! current-scissor-area area)
      (cond
       (current-scissor-area
        (receive (xsw xne ysw yne) (guide-boundingbox->quadrupel area)
          (let ((w (- xne xsw))
                (h (- yne ysw)))
            (glEnable GL_SCISSOR_TEST)
            (glScissor xsw ysw w h))))
       (else (glDisable GL_SCISSOR_TEST))))
    (define (guide-scissor-layout
             content
             #!key
             (in (guide-payload-measures content))
             (name `(guide-scissor-layout ,(guide-payload-name content))))
  ;;; NOTE: `glScissor` expects "window coordinates"
  ;;  BEWARE: this likely does not nest after re-positioning
  ;;; either!
      (MATURITY -1 "GL_SCISSOR_TEST handling requires window coordinates (and might not properly nest)" loc: guide-scissor-layout)
      (make-guide-payload
       in: (guide-payload-measures content) name: name
       on-redraw:
       (let ((foreground (guide-payload-on-redraw content)))
         (lambda ()
           ;; NOTE: a correct implementation might require dynamic-wind,
           ;; which is expensive, and a parameter for
           ;; current-scissor-area, trying to get away with a simpler
           ;; solution, since we do NOT expect non-local escapes while
           ;; drawing.
           (let* ((old current-scissor-area)
                  (in (if old (pikchr-area in `(intersect: ,old)) in)))
             (when in
               (set-current! in)
               (foreground)
               (set-current! old)))))
       on-any-event:
       (guide-payload-on-any-event content)
       lifespan: 'ephemeral widget: #f))
    guide-scissor-layout))

;;**** GGB Composition

;; Arranges content (a generic gap buffer) in a direction (x, y, z).

(define (guide-ggb-layout
         area buffer
         #!key
         (direction 0)
         (fixed #f)
         (x-motion-opaque #t) ;; hide motion events from content
         (x-volatile #t)
         (on-key #f)
         (shrink-to-content #t)
         (clip #f) ;; clip content display to area
         (background #f) ;; note: special case, see below
         (background-color #f)
         (results (lambda (payload control) payload))
         (name (vector 'guide-ggb-layout direction))
         )
  (unless (ggb? buffer) (error "arg1 ggb expected" 'guide-ggb-layout buffer))
  (let* ((direction ;; direction: 0: z, 1: x, y: z...
          (case direction
            ((0 1 2 -2) direction)
            ((layer) 0)
            ((horizontal) 1)
            ((vertical) 2)
            ((topdown) -2)
            (else (error "unknown direction" 'guide-ggb-layout direction))))
         (upper-bound-x (mdvector-interval-upper-bound area 0))
         (lower-bound-x0 (mdvector-interval-lower-bound area 0))
         (upper-bound-y (mdvector-interval-upper-bound area 1))
         (lower-bound-y0 (mdvector-interval-lower-bound area 1))
         (lower-bound-x lower-bound-x0)
         (lower-bound-y lower-bound-y0))
    (define (lower-bound-x-set! v)
      (unless (eqv? v lower-bound-x)
        (set! lower-bound-x v)
        (guide-wakeup!)))
    (define (lower-bound-y-set! v)
      (unless (eqv? v lower-bound-y)
        (set! lower-bound-y v)
        (guide-wakeup!)))
    (define (make-drawing)
      (let ((offset
             (case direction
               ((1) lower-bound-x)
               ((2) lower-bound-y)
               ((-2) (+ upper-bound-y (- lower-bound-y lower-bound-y0)))
               (else lower-bound-x0)))
            (result (make-vector (ggb-length buffer) #f)))
        (ggb-for-each
         buffer
         (lambda (i v)
           (when (guide-payload? v)
             (let* ((interval (guide-payload-measures v))
                    (width (mdv-rect-interval-width interval))
                    (height (mdv-rect-interval-height interval))
                    (view! (make-guide-label-view)))
               (case direction
                 ((1)
                  (unless (eqv? (mdvector-interval-lower-bound interval 0) 0)
                    (MATURITY -1 "horizontal ggb payload with horizontal offset" loc: 'ggb-layout v)))
                 ((2 -2)
                  (unless (eqv? (mdvector-interval-lower-bound interval 1) 0)
                    (MATURITY -1 "vertical ggb payload with vertical offset" loc: 'ggb-layout v))))
               (view! size: width height)
               (let ((draw (guide-payload-on-redraw v)))
                 (cond
                  ((procedure? draw) (view! foreground: draw))
                  ((vector? draw) (view! foreground: (%%guide-make-redraw draw)))
                  (else
                   #;(MATURITY -1 "payload has no drawing" log: guide-ggb-layout)
                   #t)))
               (case direction
                 ((0) #f #;(view! position: lower-bound-x lower-bound-y))
                 ((2)
                  (let ((y offset))
                    (if (or fixed
                            (and (>= (+ y height) lower-bound-y0)
                                 (<= y upper-bound-y)))
                        (begin (view! visible: #t) (view! position: lower-bound-x y))
                        (view! visible: #f)))
                  ;; update running
                  (set! offset (+ offset height)))
                 ((-2)
                  (set! offset (- offset height))
                  (let ((y offset))
                    (if (or fixed
                            (and (>= (+ y height) lower-bound-y0)
                                 (<= y upper-bound-y)))
                        (begin (view! visible: #t) (view! position: lower-bound-x y))
                        (view! visible: #f))))
                 ((1)
                  (let ((x offset))
                    (if (or fixed
                            (and (<= x upper-bound-x) (>= (+ x width) lower-bound-x)))
                        (begin (view! visible: #t) (view! position: x lower-bound-y))
                        (view! visible: #f)))
                  ;; update running
                  (set! offset (+ offset width)))
                 (else (NYI "ggb draw" direction)))
               (vector-set! result i (view!))))))
        (cond
         #|
         ;; Does NOT work that way!
         ;;
         ((and #f (or clip background))
          (let ((bg! (MATURITY+1:make-guide-figure-view)))
            (bg! size: total-width total-height)
            (bg! position: lower-bound-x0 lower-bound-y0)
            ;; (bg! foreground: (guide-background button: in: area))
            ;; (bg! clip: clip)
            (cond
             ((eq? background #t) ;; FIXME outdated: enforces background+positioning
              ;; only but otherwise like #f
              (bg! background: #f))
             (else (bg! background: background)))
            (bg! color: background-color)
            (bg! foreground:
                 (%%guide-make-redraw/check
                  ;; NOT nice but works
                  (case direction
                    ((2) (apply vector (reverse! (vector->list result))))
                    (else result))))
            (bg!)))
         |#
         ((and #t (or clip background))
          (let ((pc! (MATURITY+2:make-guide-bg+fg-view))
                (bg! (MATURITY+2:make-guide-label-view)))
            (bg! size: (- upper-bound-x lower-bound-x0) (- upper-bound-y lower-bound-y0))
            (bg! position: lower-bound-x0 lower-bound-y0)
            ;; (bg! foreground: (guide-background button: in: area))
            ;; (bg! clip: clip) ;; no clipping implemented here yet
            (cond
             ((eq? background #t) ;; FIXME outdated: enforces background+positioning
              ;; only but otherwise like #f
              (bg! foreground: (guide-background default: in: area)))
             (else (bg! foreground: background)))
            (bg! color: background-color)
            (pc! background: (bg!))
            (pc! foreground:
                 (%%guide-make-redraw/check
                  ;; NOT nice but works
                  (case direction
                    ((2) (apply vector (reverse! (vector->list result))))
                    (else result))))
            (pc!)))
         (else (%%guide-make-redraw/check
                ;; NOT nice but works
                (case direction
                  ((2) (apply vector (reverse! (vector->list result))))
                  (else result)))))))
    (define redraw!
      (let ((last-content (ggb->vector buffer))
            (last-lower-bound-y lower-bound-y)
            (last-lower-bound-x lower-bound-x)
            (last (make-drawing)))
        (lambda ()
          (let ((changed
                 (or (case direction
                       ((2 -2) (not (eqv? last-lower-bound-y lower-bound-y)))
                       ((1) (not (eqv? last-lower-bound-x lower-bound-x)))
                       (else #f))
                     (not (eqv? (vector-length last-content) (ggb-length buffer))))))
            (unless changed
              (ggb-for-each
               buffer
               (lambda (i v)
                 (unless (eqv? v (vector-ref last-content i))
                   (set! changed #t)))))
            (when changed
              (set! last-content (ggb->vector buffer))
              (set! last-lower-bound-y lower-bound-y)
              (set! last-lower-bound-x lower-bound-x)
              (set! last (make-drawing))))
          (last))))
    (define (pass-event! rect payload event x y)
      (let ((x-offset
             (case direction
               ((0) 0 #;lower-bound-x0)
               (else lower-bound-x)))
            (y-offset
             (case direction
               ((0) 0)
               ((-2) (+ upper-bound-y (- lower-bound-y lower-bound-y0)))
               (else lower-bound-y))))
        (ggb-for-each
         buffer ;; TBD: this pass is almost cacheable
         (lambda (i v)
           (when (guide-payload? v)
             (let* ((interval (guide-payload-measures v))
                    (width (mdv-rect-interval-width interval))
                    (height (mdv-rect-interval-height interval)))
               ;; update running
               (case direction
                 ((2) (set! y-offset (+ y-offset height)))
                 ((-2) (set! y-offset (- y-offset height)))
                 ((1) (set! x-offset (+ x-offset width))))))))
        (bind-exit
         (lambda (return)
           (ggb-for-each-rtl
            buffer
            (lambda (i v)
              (when (guide-payload? v)
                (let* ((interval (guide-payload-measures v))
                       (width (mdv-rect-interval-width interval))
                       (height (mdv-rect-interval-height interval))
                       (y0 y)
                       (x
                        (case direction
                          ((0) x #;(- x x-offset))
                          ((1) (+ (- x x-offset) width))
                          (else (- x lower-bound-x))))
                       (y (case direction
                            ((0) y)
                            ((2) (- y (+ y-offset (- height))))
                            ((-2) (- y y-offset))
                            (else (- y lower-bound-y)))))
                  (when (mdvector-rect-interval-contains/xy? interval x y)
                    (case direction
                      ((0)
                       (let ((hit (guide-event-dispatch-to-payload rect v event x y)))
                         (when hit (return hit))))
                      (else (return (guide-event-dispatch-to-payload rect v event x y)))))
                  ;; update running
                  (case direction
                    ((2) (set! y-offset (- y-offset height)))
                    ((-2) (set! y-offset (+ y-offset height)))
                    ((1) (set! x-offset (- x-offset width))))))))
           (cond ;; no hit
            (shrink-to-content #f)
            (else #t))))))
    (define events
      ;; TBD: factor motion/shift handling out (copied here from
      ;; `select` already.
      (let ((armed #f) (armed-at #f) (motion-hyst 15))
        (lambda (rect payload event x y)
          (let ((area (guide-payload-measures payload)))
            (cond
             ((or (eqv? press: event) (eqv? release: event))
              (and on-key (on-key event x y)))
             (fixed (pass-event! rect payload event x y))
             ((and (guide-event-graphics? event)
                   (not (mdvector-rect-interval-contains/xy? area x y)))
              (MATURITY -2 "pointer event outside of payload" loc: name)
              #f)
             ((and (eqv? event EVENT_BUTTON1DOWN))
              (set! armed (vector x y))
              (set! armed-at armed)
              (or x-motion-opaque (pass-event! rect payload event x y)))
             ((and (eqv? event EVENT_BUTTON1UP))
              (cond
               ((eq? armed armed-at)
                (set! armed #f)
                (set! armed-at #f)
                (begin ;; not %%guide-post-speculative
                 (let ((handle
                        (lambda (x)
                          (cond
                           ((boolean? x) x)
                           ((procedure? x) (x))
                           ((promise? x) (force x))
                           (else (error "invalid return" x))))))
                   (cond
                    ((not x-motion-opaque)
                     (pass-event! rect payload event x y))
                    (else
                     (let* ((r1 (pass-event! rect payload EVENT_BUTTON1DOWN x y))
                            (r2 (pass-event! rect payload event x y)))
                       (%%guide-post-speculative (begin (handle r1) (handle r2)))))))))
               (else
                (set! armed #f)
                (set! armed-at #f)
                (or x-motion-opaque (pass-event! rect payload event x y)))))
             ((and armed (eqv? event EVENT_MOTION))
              (cond
               (armed
                (let* ((dx (- x (vector-ref armed-at 0)))
                       (dy (- y (vector-ref armed-at 1))))
                  (when (and (eq? armed armed-at)
                             (> (sqrt (+ (* dx dx) (* dy dy))) ;; distance
                                motion-hyst))
                    (case direction
                      ((1) (set! armed lower-bound-x))
                      ((2 -2) (set! armed lower-bound-y))))
                  (when (number? armed)
                    (case direction
                      ((1) (lower-bound-x-set! (+ armed dx)))
                      ((2 -2) (lower-bound-y-set! (+ armed dy)))))
                  (or x-motion-opaque (pass-event! rect payload event x y))))
               (else #t)))
             (else (pass-event! rect payload event x y)))))))
    (results
     (make-guide-payload
      in: area name: name
      widget: #f lifespan: 'ephemeral ;; TBD: change defaults here!
      on-redraw: (if x-volatile redraw! (make-drawing))
      on-any-event: events)
     (lambda (key val . more)
       (case key
        ((position:)
         (cond
          ((null? more) (lower-bound-x-set! val) (lower-bound-y-set! val))
          (else #f)))
        ((fix) (make-drawing))
        (else (error "unhandled" name key)))))))

;;**** Table Composition

(define (make-guide-table
         ;; CONTRUCTOR (lambda (contructor INTERVAL COL ROW . rest) . rest)
         ;; CONTRUCTOR+ARGS: (or CONTRUCTOR (CONTRUCTOR . ARGS))
         contructors ;; mdvector of CONTRUCTOR+ARGS
         #!key
         (in (current-guide-gui-interval))
         (font #f)
         (border-ratio 1/20)
         (on-key #f)
         (name 'table)
         )
  (let* ((error-location (lambda (col row) (list 'make-guide-table-payload col row)))
         (rng (mdvector-range contructors))
         (idx (mdv-indexer rng))
         (content (make-vector (range-volume rng) #f))
         (columns (range-size rng 0))
         (rows (range-size rng 1))
         (num-hgaps (+ columns 1))
         (num-vgaps (+ rows 2))
         (yu (mdvector-interval-upper-bound in 1))
         (yl (mdvector-interval-lower-bound in 1))
         (total-height (- yu yl))
         (xu (mdvector-interval-upper-bound in 0))
         (xl (mdvector-interval-lower-bound in 0))
         (total-width (- xu xl))
         (hgap (* border-ratio (/ total-width num-hgaps)))
         (hstep (/ (- total-width hgap) columns))
         (cell-width (/ (- total-width (* num-hgaps hgap)) columns))
         (left-offset (+ xl hgap))
         ;;
         (total-height (- yu yl))
         (vgap (* border-ratio (/ total-height num-vgaps)))
         (vstep (/ (- total-height vgap) rows))
         (cell-height (/ (- total-height (* num-vgaps vgap)) rows))
         (top-offset (- yu vgap))
         (font (or font (guide-select-font height: cell-height))))
    (for-range2
     rng
     (lambda (row col)
       (let* ((idx (idx row col))
              (pattern-vector (mdvector-body contructors))
              (pattern (vector-ref pattern-vector idx))
              (payload
               (and
                (not (boolean? pattern)) ;; booleans -> col/row span
                (let* ((colspan ;; columns spec #f
                        (do ((i (fx+ col 1) (fx+ i 1))
                             (span 0 (fx+ span 1)))
                            ((or (eqv? i columns)
                                 (let ((next (mdvector-ref contructors row i)))
                                   (not (eq? next #f))))
                             span)))
                       (rowspan ;; following row is #t
                        (do ((i (fx+ row 1) (fx+ i 1))
                             (span 0 (fx+ span 1)))
                            ((or (eqv? i rows)
                                 (not (eq? #t (mdvector-ref contructors i col))))
                             span)))
                       (x0 (+ left-offset (* col hstep)))
                       (x1 (+ x0 cell-width (* colspan hstep)))
                       (y1 (- top-offset (* row vstep)))
                       (y0 (max -1 (- y1 cell-height (* rowspan vstep))))
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
     in: in name: name
     on-redraw:
     (lambda () ;; better return customized vector; draw sequence does not matter
       (do ((i (fx- (vector-length content) 1) (fx- i 1)))
           ((eqv? i -1))
         (let ((payload (vector-ref content i)))
           (and
            payload
            (let ((draw (guide-payload-on-redraw payload)))
              (cond
               ((procedure? draw) (draw))
               ((vector? draw)
                (do ((i 0 (fx+ i 1))) ;; draw background first
                    ((eqv? i (##vector-length draw)))
                  ((##vector-ref draw i))))
               (else (MATURITY -1 "unhandled drawing" loc: 'guide-table))))))))
     on-any-event:
     (lambda (rect payload event x y)
       (cond
        ((or (eqv? press: event) (eqv? release: event))
         (if on-key (on-key event x y) #t))
        ((and (guide-event-graphics? event) (guide-payload-contains/xy? payload x y))
         (do ((i (fx- (vector-length content) 1) (fx- i 1))
              (result #f)
              (hit #f))
             ((or hit (eqv? i -1)) result)
           (let ((payload (vector-ref content i)))
             (and payload
                  (and
                   (guide-payload-contains/xy? payload x y)
                   (begin
                     (set! hit #t)
                     (set! result (guide-event-dispatch-to-payload rect payload event x y))))))))
        (else (debug 'guide-table:ignored-event event))))
     ;; backward compatibility
     widget: #f lifespan: 'ephemeral)))

(define (guide-table-layout
         area #!key
         (cols 1)
         (rows 1)
         ;; inherited from make-guide-table
         font
         (border-ratio 1/20)
         on-key
         (name 'guide-table-layout)
         (map-over-constructors #f) ;; TBD: backward compatible TRASH!
         #!rest
         constructors)
  (assume (mdvector-interval? area) "invalid area" name area)
  (let ((constructors
         (cond
          ((and (pair? constructors) (null? (cdr constructors))
                (vector? (car constructors))) ;; may be easier to write
           (set! constructors (car constructors)))
          (map-over-constructors
           (list->vector (map map-over-constructors constructors)))
          (else (list->vector constructors)))))
    (assume (positive? (vector-length constructors))
            "at least one constructor required" guide-table-layout)
    (let ((given (vector-length constructors))
          (volume (* rows cols)))
      (cond
       ((< given volume)
        (MATURITY -1 "too few constructurs" loc: name given volume)
        (let ((expanded (make-vector volume #f)))
          (subvector-move! constructors 0 volume expanded volume)
          (set! constructors expanded)))
       ((> volume given)
        (MATURITY -1 "additional constructurs" loc: name given volume))))
    (make-guide-table
     (make-mdvector
      (range (vector cols rows))
      constructors)
     in: area
     font: font
     border-ratio: border-ratio
     on-key: on-key
     name: name)))

;;** Basic Payloads

;;*** Button Payload

(define (guide-button
         #!key
         (in (current-guide-gui-interval))
         (done #f)
         (style (guide-current-style))
         (font (guide-style-ref ($current-guide-style) font:))
         (accesskey #f)
         (label "exit")
         (label-equal equal?)
         (label-display
          (lambda (value)
            (cond
             ((or (string? value) (glC:image? value) (vector? value))
              value)
             (else (object->string value)))))
         (color (or (guide-style-ref style color:) (guide-select-color-2)))
         (padding '#(1 1 1 1))
         (background (guide-background button: in: in))
         (background-color (or (guide-style-ref style background-color:) (guide-select-color-3)))
         (position #f)
         (horizontal-align (guide-style-ref style horizontal-align:))
         (vertical-align (guide-style-ref style vertical-align:))
         (location guide-button)
         ;; TBD: better interface&name
         (guide-callback
          (lambda (rect payload event x y) (if (procedure? done) (done) (terminate))))
         (name 'button))
  ;; I. find additional default values
  (unless font
    (set! font (guide-select-font height: (mdv-rect-interval-height in))))
  ;; II. go for it
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
    (view! size: w h)
    ;; finally - in order to not trigger useless recalculations
    (cond
     ((or (string? label) (glC:image? label) (vector? label))
      (label! foreground: label)
      (view! foreground: (label!)))
     ((procedure? label)
      (let* ((cached
              (macro-memoize:2->1
               (lambda (update! value)
                 (update! foreground: (label-display value))
                 (update!))
               (macro-alway-true-comparsion) label-equal))
             (check! (lambda (update!) (cached update! (label)))))
        (view! foreground: (label! check! (list "guide-button" label)))))
     ((guide-payload? label) (view! foreground: (guide-payload-on-redraw label)))
     (else (error "invalid label" guide-button label)))
    (when background
      (view!
       background:
       (cond
        ((fixnum? background)
         (MATURITY -1 "background: converting fixnum to texture" loc: location)
         (%%glCore:textures-ref background #f))
        ((eq? background 'none) #f)
        (else background))))
    (when background-color (view! color: background-color))
    (if position
        (view! position: (vector-ref position 0) (vector-ref position 1))
        (view! position: x y))
    (let ((events
           (let ((armed #f))
             (lambda (rect payload event x y)
               (case event
                 ((press: release:)
                  (and accesskey (eqv? x accesskey)
                       (or (not (procedure? guide-callback))
                           (macro-guide-sanitize-payload-result
                            (guide-callback rect payload event x y)))))
                 (else
                  (cond
                   ((eqv? event EVENT_BUTTON1DOWN)
                    (if (guide-figure-contains? view! x y) (begin (set! armed #t) #t) #f))
                   ((eqv? event EVENT_BUTTON1UP)
                    (if (and (guide-figure-contains? view! x y) armed)
                        (begin
                          (set! armed #f)
                          (or (not (procedure? guide-callback))
                              (macro-guide-sanitize-payload-result
                               (guide-callback rect payload event x y))))
                        (begin
                          (set! armed #f)
                          #f)))
                   ((guide-event-graphics? event) (guide-figure-contains? view! x y))
                   (else #f))))))))
      (make-guide-payload
       in: in name: name widget: #f
       on-redraw: (view!) on-any-event: events lifespan: 'ephemeral))))

;;*** Labeled Value Payload

(define (guide-valuelabel
         #!key
         (in (current-guide-gui-interval))
         (label "") (label-width 1/2)
         (value #f)
         (value-equal equal?)
         (value-display
          (lambda (value)
            (cond
             ((or (string? value) (glC:image? label) (vector? label))
              value)
             (else (object->string value)))))
         (input #f)
         (size 'small)
         (style (guide-current-style))
         (font (or (guide-style-ref style font:) (guide-select-font size: size)))
         (color #f)
         (label-color #f)
         (value-color #f)
         (success values)
         (name 'valuelabel))
  (unless (or (string? value) (procedure? value))
    (error "invalid value argument" 'guide-valuelabel value))
  ;; complete defaults
  (cond
   ((and color (not label-color) (set! label-color color)))
   ((and color (not value-color) (set! value-color color))))
  (unless label-color (set! label-color (or (guide-style-ref style color:) (guide-select-color-2))))
  (unless value-color (set! value-color (or (guide-style-ref style highlight-color:) (guide-select-color-4))))
  ;; derived
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
                   (lambda (update! value)
                     (update! foreground: (value-display value))
                     (update!))
                   (lambda (a b) #t) value-equal)))
             (lambda (update!) (cached update! (src)))))))
    (let ((lw (* w (- 1 label-width)))
          (gap (/ h 5)))
      (tag! size: (- w lw gap) h)
      (label! size: lw h)
      (tag! font: font)
      (label! font: font)
      (tag! vertical-align: 'center)
      (label! vertical-align: 'center)
      (tag! horizontal-align: 'right)
      (label! horizontal-align: 'left)
      (tag! color: label-color)
      (label! color: value-color)
      (label! position: (- w lw) 0))
    (tag! text: label)
    (label! text: (value-display (if (procedure? value) (value) value)))
    (box! size: w h)
    (box! background: (tag!))
    (box! foreground: (label! check! label))
    (box! position: x y)
    (let ((interval (make-mdv-rect-interval x y (+ x w) (+ y h)))
          (events
           (lambda (rect payload event x y)
             (cond
              ((or (eqv? press: event) (eqv? release: event)))
              ((and (guide-event-graphics? event)
                    (guide-figure-contains? box! x y))
               (or (not input) (input rect payload event x y)))
              (else #f)))))
      (let ((payload (make-guide-payload
                      name: name
                      in: interval widget: #f on-redraw: (box!) on-any-event: events)))
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

;;*** Keypad Payload

(define (guide-make-keypad
         area spec
         #!key
         (style (guide-current-style))
         (font (or (guide-style-ref style font:) (guide-select-font size: 'medium)))
         (color (or (guide-style-ref style color:) (guide-select-color-2)))
         (background-color (or (guide-style-ref style background-color:) (guide-select-color-1)))
         (background #f)
         (key-background #!void) ;; FIXME: macro-absent ... ??
         (on-key #f)
         (name 'keypad))
  (define (%%guide-post-key-event key switch)
    ;; note: still using legacy events encoding
    (define modifiers 0)
    (and switch (switch #f)) ;; unset switch
    (cond
     ((fixnum? key)
      (if (procedure? on-key)
          (on-key release: key modifiers)
          (%%guide-post-speculative (event-push EVENT_KEYRELEASE key modifiers))))
     ((char? key)
      (if (procedure? on-key)
          (on-key release: key modifiers)
          (%%guide-post-speculative (event-push EVENT_KEYRELEASE (char->integer key) modifiers))))
     (else (error "invalid key" %%guide-post-key-event key))))
  (define (post-key pat switch)
    (lambda (rect payload event x y)
      (%%guide-post-key-event pat switch)))
  (define (keybutton
           in ;; area
           switch ;; #f or keypane switch control procedure
           c ;; character to produce
           #!key
           (label (string c)) (color color)
           (background key-background)
           (background-color background-color)
           (vertical-align 'center)
           (padding '#(1 1 1 1)))
    (guide-button
     in: in label: label color: color font: font
     padding: padding
     background: background background-color: background-color guide-callback: (post-key c switch)))
  (define (keypane rng spec-vector switch)
    (let* ((len (range-volume rng))
           (start ((mdv-indexer rng) 0 0))
           (spec-vector (mdvector-body spec))
           (constructors (make-vector len #f)))
      (do ((i 0 (fx+ i 1)))
          ((eqv? i len))
        (let ((pat (vector-ref spec-vector (+ i start))))
          (vector-set!
           constructors i
           (if (boolean? pat) pat
               (lambda (in col row)
                 (cond
                  ((char? pat) (keybutton in switch pat))
                  ((and (fixnum? pat) (positive? pat)) (keybutton in switch pat))
                  ((pair? pat)
                   (cond
                    ((symbol? (car pat))
                     (and switch
                          (apply
                           guide-button in: in
                           guide-callback:
                           (let ((key (car pat)))
                             (lambda (rect payload event x y)
                               (%%guide-post-speculative (switch key))))
                           color: color font: font
                           background: background background-color: background-color
                           (cdr pat))))
                    (else (apply keybutton in switch pat))))
                  (else (error "invalid key spec" guide-make-keypad pat))))))))
      (make-guide-table
       (make-mdvector (range (vector (range-size rng 0) (range-size rng 1))) constructors)
       in: area)))
  ;; adjust defaults
  (when (eqv? key-background #!void)
    (set! key-background (guide-background button: in: area)))
  ;;
  (let ((rng (mdvector-range spec)))
    (case (range-rank rng)
      ((2) (keypane rng (mdvector-body spec) #f))
      ((3)
       (let* ((len (range-size rng 2))
              (panes (make-vector len #f)))
         (let* ((sticky #f)
                (shift 0)
                (toggle 0)
                (current-pane 0)
                (switch
                 (lambda (key)
                   (case key
                     ((#f) ;; unset modifiers if not sticky
                      (unless sticky
                        (set! shift 0)
                        (set! toggle 0)
                        (set! current-pane (+ shift toggle))))
                     ((shift)
                      (cond
                       ((not (or sticky (eqv? shift 0))) ;; (and (eqv? shift 1) (not sticky))
                        (set! sticky #t))
                       (else (set! shift (if (eqv? shift 1) 0 1))))
                      (set! current-pane (+ shift toggle)))
                     ((toggle)
                      (cond
                       ((not (or sticky (eqv? toggle 0))) ;;(and (eqv? toggle 2) (not sticky))
                        (set! sticky #t))
                       (else (set! toggle (if (eqv? toggle 2) 0 2))))
                      (set! current-pane (+ shift toggle)))
                     (else
                      (set! current-pane 0)))
                   (when (eqv? current-pane 0) (set! sticky #f)))))
           (do ((i 0 (fx+ i 1)))
               ((eqv? i len)
                (make-guide-payload
                 in: area name: name widget: #f
                 on-redraw:
                 (lambda ()
                   (guide-event-dispatch-to-payload/redraw (vector-ref panes current-pane)))
                 on-any-event:
                 (lambda (rect payload event x y)
                   (guide-event-dispatch-to-payload rect (vector-ref panes current-pane) event x y))))
             (vector-set! panes i (keypane (range-row rng i) (mdvector-body spec) switch))))))
      (else "unsupported rank for keypad" rng))))

;;** Calculator Payload (Example)

(include "calculator.scm")

;;** Lambdanative Driver

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

(define $guide-frame-period
  (case-lambda
   (() (/ (microgl-redraw-period) 1000000))
   ((x)
    (let ((fix (lambda (n)
                 (cond
                  ((##ratnum? n) (##floor n))
                  ((##flonum? n) (##flonum->fixnum n))
                  (else n)))))
      (microgl-redraw-period (fix (* x 1000000)))))))

(define-values
    (guide-toplevel-payload guide-main guide-exit $guide-frame-period-minimum)
  (let ((once main) ;; lambdanative `main` called at most once
        (attributes #f)
        (payload #f))
    (cond-expand ;; funny stuff
     (linux (define i3hook (delay (guide-floating-window!))))
     (else))
    (define guide-toplevel-payload
      (case-lambda
       (() payload)
       ((new)
        (cond
         ((not (guide-payload? new)) (error "invalid payload" 'guide-toplevel-payload new))
         ((eq? payload new) payload)
         (else
          (when payload
            (let ((after (guide-payload-gui-after payload)))
              (when after (after payload (guide-payload-measures payload)))))
          (set! payload new)
          ;; enforcing a garbace collection as the before/after
          ;; mechanism is often too weak and may be worth to be removed
          (##gc)
          (let ((before (guide-payload-gui-before payload)))
            (when before (before payload (guide-payload-measures payload))))
          (guide-wakeup!)
          payload)))))
    (define (guide-main
             init #!key
             ;; (events #f)
             (suspend (lambda ()
                        (log-status "suspended")
                        (set! app:suspended #t)
                        #f))
             (resume (lambda ()
                       (set! glCore:needsinit #t) ;; reinitialize OpenGL pipeline
                       (set! app:suspended #f)
                       (guide-wakeup!)
                       ;; this helps android to ALMOST SURE sit in
                       ;;
                       ;; W GLSurfaceView: Warning, !readyToDraw() but waiting for draw finished!
                       ;; Early reporting draw finished.
                       ;;
                       ;; (thread-sleep! 0.2)
                       (log-status "resumed")))
             (terminate (lambda () #t))
             (wait-mutex %%guide-wait-mutex)
             (wait-cv (mutex-specific %%guide-wait-mutex)))
      (let ((gui #f)
            (next-frame-time (current-time))
            (draw-thread #f)
            (todo once))
        (define (draw-once)
          (when (and (not app:suspended)
                     (not app:mustinit)
                     glgui:active app:width app:height) ;; ???
            (set! ##now (current-time-seconds))
            (set! next-frame-time (seconds->time (+ (time->seconds (current-time)) ($guide-frame-period-minimum))))
            (let ((cpl (if (procedure? payload) (payload) payload)))
              (guide-default-event-dispatch/toplevel gui cpl EVENT_REDRAW 0 0))))
        (define (wait-for-frame-period)
          (mutex-lock! wait-mutex 0)
          (when (mutex-unlock! wait-mutex wait-cv ($guide-frame-period))
            (thread-sleep! next-frame-time)))
        (define (draw-loop) ;; TBD: add optional "no draw required"
          (draw-once)
          (wait-for-frame-period)
          (draw-loop))
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
                (set! payload b)
                (cond-expand
                 ((or android linux)
                  (set! draw-thread (thread-start! (make-thread draw-loop 'draw-thread))))
                 (else #!void))))))
         ;; events
         (lambda (event x y)
           (cond-expand
            (linux (force i3hook))
            (else))
           (cond
            ((eqv? event EVENT_REDRAW)
             (guide-wakeup!)
             (unless draw-thread
               (draw-once)
               (wait-for-frame-period)))
            (else
             (let ((cpl (if (procedure? payload) (payload) payload)))
               (guide-default-event-dispatch/toplevel gui cpl event x y))
             #t)))
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
    (values guide-toplevel-payload guide-main guide-exit (make-parameter 1/60))))

(define (guide-toplevel-capture-return)
  (let ((before (guide-toplevel-payload)))
    (lambda _
      (cond
       (before (guide-toplevel-payload before))
       (else
        (let ((return (guide-toplevel-capture-return)))
          (guide-toplevel-payload
           (guide-table-layout
            (guide-payload-measures (guide-toplevel-payload))
            name: "Sure to exit?"
            cols: 1 rows: 3
            (lambda (area row col)
              (guide-button
               in: area label: "Sure to exit?"
               background: (guide-background default: in: area)
               guide-callback: (lambda _ #t)))
            (lambda (area row col)
              (guide-button in: area label: "No, Go Back." guide-callback: return))
            (lambda (area row col)
              (guide-button in: area label: "Exit"))))))))))

;; #eof
