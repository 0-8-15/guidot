
;;** Utilities

(define guidot-event-trace
  (let*
      ((trace-log-port
        (lambda () (current-error-port))))
    (define (fail val) (error "unhandled" guidot-event-trace val))
    (define (log rect payload event x y)
      (define port (trace-log-port))
      (println port: port "GUIDOT event: " event)
      (println port: port "    rect: " rect)
      (println port: port "    payload: " payload)
      (println port: port "    x: " x " y: " y)
      ((cond-expand (gambit force-output) (chicken flush-output)) port))
    (define (set!! proc) ($guide-event-dispatch-log proc))
    (define-macro (define-iflog spec pred)
      ;; BEWARE: half backed - expects careful local use - only for
      ;; guidot-event-trace; After all: this is gambit, which has
      ;; lexical scope for macros, doesn't it?
      `(define ,spec (when ,pred (log rect payload event x y))))
    (define-iflog (log-button1 rect payload event x y)
      (or (eqv? event EVENT_BUTTON1DOWN) (eqv? event EVENT_BUTTON1UP)))
    (define-iflog (log-draw rect payload event x y) (eqv? event EVENT_REDRAW))
    (define-iflog (log-key rect payload event x y) (or (eq? press: event) (eq? release: event)))
    (define-iflog (log-motion rect payload event x y) (eqv? event EVENT_MOTION))
    (define (add! pred)
      (let ((rest (guidot-event-trace)))
        (set!!
         (lambda (rect payload event x y)
           (cond
            ((pred rect payload event x y)
             (log rect payload event x y)))
           (when (procedure? rest)
             (rest rect payload event x y))))))
    (define (add-filter! pred)
      (let ((rest (guidot-event-trace)))
        (set!!
         (lambda (rect payload event x y)
           (when (and (procedure? rest)
                      (pred rect payload event x y))
             (rest rect payload event x y))))))
    (case-lambda
     (()
      (let ((val ($guide-event-dispatch-log)))
        (cond
         ((not val) (guidot-display/boolean val))
         ((eq? val log-draw) 'draw)
         ((eq? val log-key) 'key)
         ((eq? val log-motion) 'motion)
         ((eq? val log-button1) 'button1)
         (else (object->string val)))))
     ((val)
      (cond
       ((not val) (set!! #f))
       ((eqv? val #t) ($guide-event-dispatch-log))
       ((symbol? val)
        (set!!
         (case val
           ((draw) log-draw)
           ((key) log-key)
           ((motion) log-motion)
           ((button1) log-button1)
           (else (fail val)))))
       ((procedure? val) (set!! val))
       (else (fail))))
     ((key val)
      (case key
        ((filter:)
         ((procedure? val) (add-filter! val))
         (else (fail val)))
        (else (fail key))))
     (args (error "unhandled" guidot-event-trace args)))))

;;** GUI

(define (guide-div/lineheight-payload
         #!key
         (in (current-guide-gui-interval))
         (line-height 16)
         (line-height-selectable 60)
         ;;
         ;; ;; not yet: (results values)
         (direction 'topdown)
         (name 'page)
         #!rest content-constructors
         )
  (let*
      ((interval in)
       (xsw 0) (ysw 0) ;; whatever zero is... ;-)
       #|
       (xsw (mdvector-interval-lower-bound interval 0))
       (ysw (mdvector-interval-lower-bound interval 1))
       |#
       (w (mdv-rect-interval-width interval))
       (h (mdv-rect-interval-height interval))
       ;; derived values (context may need to re-define what `+` means)
       (xno (+ xsw h))
       (active
        (let ((active #f))
          (case-lambda
           (() active)
           ((key . more)
            (case key
              #;((top:)
               (cond
                ((stm-atomic?) (apply push! more))
                (else (guide-critical-add! (lambda () (apply push! more))))))
              ((next:)
               (apply (lambda (next) (set! active next)) more))
              (else (error "invalid key" name key more)))))))
       (redraw! (lambda ()
                  (let ((active (active)))
                    (and (guide-payload? active) ((guide-payload-on-redraw active))))))
       (events
        (lambda (rect payload event x y)
          (cond
           ((active) =>
            (lambda (payload)
              (guide-payload-contains/xy? payload x y)
              (guide-event-dispatch-to-payload rect payload event x y)))
           (else (mdvector-rect-interval-contains/xy? interval x y))))))
    (define result
      (cond
       ((null? content-constructors)
        (guide-button
         in: (make-x0y0x1y1-interval/coerce xsw ysw (+ xsw (* w 19/20)) (+ ysw (/ h 2)))
         label: "(C)... fallback version"
         guide-callback: (lambda (rect payload event xsw ysw) (active next: #f) #t)))
        (else
         (guide-ggb-layout
          interval
          (let* ((in
                  (let ((w w)
                        (h (min (/ h 2) line-height)))
                    (make-x0y0x1y1-interval/coerce 0 0 w h))))
            ;; summarize `args` in result GGB
            (let ((buffer (make-ggb)))
              (for-each
               (lambda (obj) (ggb-insert! buffer (obj in buffer active)))
               content-constructors)
              buffer)) ;; MUST return a GGB for `guide-ggb-layout` at this position
          background: #t
          fixed: #f ;; better #t if known that no scrolling required
          direction: direction
          ))))
    ;; finally
    (active next: result) ;; ...don't touch the "(C)"... line
    (make-guide-payload
     name: name in: interval
     on-redraw: redraw!
     on-any-event: events
     widget: #f lifespan: 'ephemeral)))

(define (guidot-display/boolean x) (if x "yes" "no"))

;;** Debuggger

(define (guidot-debugger-about-page-content-constructors)
  (define (select-registered area)
    (let* ((options (guide-payload-names))
           (select
            (lambda (n x)
              (guide-toplevel-payload ((guide-payload-ref (vector-ref options n)) area)))))
      (guide-list-select-payload
       area (lambda _ options)
       action: select)))
  (define (beaver-number-display x) (if x (beaver-number->unicode-vector x) '#()))
  (define conv
    (lambda (v)
      (cond
       ((string? v) v)
       ((boolean? v) (guidot-display/boolean v))
       (else (object->string v)))))
  (define val1 (lambda (a1 . more) a1))
  (define size 'medium)
  (define content
    (list
     (lambda (area buffer active)
       (guide-button
        in: area
        label: "(C) JFW [Corona edition: 2020-2021]"
        guide-callback: (lambda (rect payload event xsw ysw) (active next: (select-registered (guide-rectangle-measures rect))) #t)))
     (lambda (area buffer active)
       (guide-valuelabel in: area label: "Version" value: (system-appversion)))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "kick-style"
        value: kick-style
        value-equal: eq?
        value-display: conv
        input:
        (lambda (rect payload event xsw ysw)
          (cond
           ((eqv? event EVENT_BUTTON1DOWN)
            (kick-style
             (case (kick-style)
               ((async) 'sync)
               ((sync) 'async)
               (else #f)))))
          #t)))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "async exceptions"
        value: $async-exceptions
        value-equal: eq? value-display: object->string))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "guidot trace"
        value: guidot-event-trace
        value-equal: eq?))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "vpn"
        value: ot0cli-server
        value-equal: eq? value-display: guidot-display/boolean))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "context directory"
        value-display: conv
        value: ot0-context))
     ;; ot0cli-origin
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "onetierzero"
        value: ot0-online
        value-equal: eq? value-display: guidot-display/boolean))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "lwIP"
        value: lwIP
        value-equal: eq? value-display: guidot-display/boolean))
     #;(lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "beaver id"
        value: beaver-local-unit-id
        value-equal: eq? value-display: (lambda (x) (number->string x 16))))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "beaver number"
        value: beaver-local-unit-id
        value-equal: eq? value-display: (lambda (v) (and v (beaver-unit-id->unicode-vector v)))))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "use deamonize"
        value: beaver-use-daemonize
        value-equal: eq?
        value-display: guidot-display/boolean
        input:
        (lambda (rect payload event xsw ysw)
          (cond
           ((eqv? event EVENT_BUTTON1DOWN)
            (%%guide-post-speculative (beaver-use-daemonize (not (beaver-use-daemonize)))))
           (else #t)))))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "beaver domain"
        value: beaver-captured-domain
        value-equal: eq? value-display: (lambda (x) (or x "n/a"))))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "fossils directory"
        value: fossils-directory
        value-equal: eq? value-display: (lambda (x) (or x "n/a"))))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "code maturity level"
        value: current-maturity-level
        value-equal: eq? value-display: object->string))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "memoize active"
        value: $memoize-active
        value-equal: eqv? value-display: guidot-display/boolean
        input:
        (lambda (rect payload event xsw ysw)
          (cond
           ((eqv? event EVENT_BUTTON1DOWN)
            (%%guide-post-speculative ($memoize-active (not ($memoize-active)))))
           (else #t)))))
     ;; end of content
     ))
  content)

(define (debugger-about-payload #!key in)
  (apply
   guide-div/lineheight-payload name: "Godot Debugger About" in: in
   line-height: 32
   (guidot-debugger-about-page-content-constructors)))
