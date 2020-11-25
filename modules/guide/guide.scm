(include "../../front/apps/glgui/DejaVuSans-14,24,32.scm")

(utf8string->unicode:on-encoding-error 'replace)

;;; BEGIN INSTEAD OF (include "~~tgt/lib/onetierzero/src/observable-notational-conventions.scm")
(define-macro (define-values names . body)
  (let ((vals (gensym 'vals)))
    `(begin
       ,@(map (lambda (name) `(define ,name #f)) names)
       (call-with-values (lambda () . ,body)
         (lambda ,vals
           . ,(map (lambda (name)
                     `(set! ,name (let ((,name (car ,vals))) (set! ,vals (cdr ,vals)) ,name)))
                   names))))))

(define-macro (kick expr . more)
  `(kick! (lambda () ,expr . ,more)))

(define-macro (kick/sync expr . more)
  `(kick/sync! (lambda () ,expr . ,more)))

;;; END INSTEAD OF (include "~~tgt/lib/onetierzero/src/observable-notational-conventions.scm")

(define-structure guide-rectangle glgui measures)
(define-structure guide-payload
  widget       ;; glgui or extended
  on-any-event ;; generic event dispatch (replace by r/w specific versions
  gui-before   ;; when exposed
  gui-after    ;; when hidden
  )

(define (guide-rectangle-width obj)
  (let ((interval (guide-rectangle-measures obj)))
    (- (interval-upper-bound interval 0) (interval-lower-bound interval 0))))

(define (guide-rectangle-height obj)
  (let ((interval (guide-rectangle-measures obj)))
    (- (interval-upper-bound interval 1) (interval-lower-bound interval 1))))

(define guide-terminate-on-key (make-parameter EVENT_KEYESCAPE))

(define (guide-default-event-dispatch rect payload event x y)
  ;; (check-magic-keys gui t x y)
  (let ((glgui (guide-rectangle-glgui rect)))
    (cond
     ((eq? event EVENT_REDRAW)
      ;; (log-status "REDRAW")
      (glgui-event glgui event x y))
     ((eq? event EVENT_IDLE)
      #t)
     ((and (fx= event EVENT_KEYPRESS)
           (let ((termkey (guide-terminate-on-key)))
             (and termkey (fx= x termkey))))
      (terminate))
     (else (kick! (lambda () (glgui-event glgui event x y)))))))

(set!
 make-guide-payload
 (let ((make-guide-payload make-guide-payload)
       (exposed
        (lambda (payload rect)
          (glgui-widget-set! (guide-rectangle-glgui rect) (guide-payload-widget payload) 'hidden #f)))
       (hidden
        (lambda (payload rect)
          (glgui-widget-set! (guide-rectangle-glgui rect) (guide-payload-widget payload) 'hidden #t)))
       (remove
        (lambda (payload rect)
          (glgui-widget-delete (guide-rectangle-glgui rect) (guide-payload-widget payload))))
       (events guide-default-event-dispatch))
   (lambda (#!key
            (widget (make-gtable))
            (lifespan #t)
            (on-any-event events)
            (gui-before exposed)
            (gui-after hidden))
     (cond
      ((or (eq? lifespan #f) (eq? lifespan 'ephemeral))
       (make-guide-payload widget on-any-event gui-before remove))
      (else (make-guide-payload widget on-any-event gui-before gui-after))))))

(define guide-open-rectangle
  (let ((empty (make-guide-payload
                gui-before: #f
                gui-after: #f
                )))
    (define (guide-open-rectangle rect #!optional (name #f))
      (unless (guide-rectangle? rect) (error "guide-open-rectangle: rectangle required" rect))
      #;(make-pin
       initial: empty
       pred: guide-payload?
       name: name)
      (let ((current empty))
        (case-lambda
         (() current)
         ((new)
          (unless (guide-payload? new) (error "illegal payload" new))
          (cond
           ((eq? current new))
           (else
            (let ((after (guide-payload-gui-after current)))
              (when after (after current rect)))
            (set! current new)
            (let ((before (guide-payload-gui-before current)))
              (when before (before current rect)))
            (glgui-wakeup!)))))))
    guide-open-rectangle))

(define (guide-make-gui)
  (make-guide-rectangle
   (make-glgui)
   (make-interval '#(0 0) (vector (glgui-width-get) (glgui-height-get)))))

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

;;;

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
    (define (label gui x y w h #!key text (size 'small) (color White) (bgcolor #f))
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
             (size 'small) (color White) (bgcolor #f))
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
             (size 'small) (color White) (bgcolor Black))
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

(define (Xglgui-select gui x y w h #!key (line-height 20) (color White))
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
          (glgui-widget-set! gui wgt 'bordercolor Black)
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

(define (Xglgui-button1
         ctx x y w h
         #!key
         (font (guide-select-font size: 'medium))
         (label "exit")
         (glgui-callback (lambda (gui wgt type x y) (terminate))))
  (glgui-button-string ctx x y w h label font glgui-callback))

(define Xglgui-button Xglgui-button1)

;;; end Xglgui

(include "calculator.scm")

;; LambdaNative glgui frame -- it's a bit tricky to work around that one.

(define-values
    (guide-main guide-exit)
  (let ((once main))
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
                (set! gui a)
                (set! payload b)))))
         ;; events
         (lambda (event x y)
           (thread-yield!) ;; At least for Android
           (let ((cpl (payload)))
             ((guide-payload-on-any-event cpl) gui cpl event x y)))
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
