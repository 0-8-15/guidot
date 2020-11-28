
(log-status "initializing chat")

(define select-font guide-select-font)

(declare
 (not standard-bindings
      take drop ;; actually they need to be standard
      ))

(include "../misc-conventions/observable-syntax.sch")

(define (init-beaverchat! dir #!key (use-origin #f))
  (unless (file-exists? dir)
    (ot0-init-context! dir)
    (when use-origin
      (call-with-output-file (make-pathname dir "origin")
        (let ((content use-origin))
          (lambda (port)
            (write-subu8vector content 0 (u8vector-length content) port)))))
    (log-status "done. Initialized chat in "  dir)))

;;; These are exported as input/output:

;; Other pins

(define-pin audible-beep
  initial: (lambda () #f)
  pred: procedure?
  name: "hook to beep")

(define select-font guide-select-font)

(define (local-server-port-filter old new)
  (match new
         (#f 0)
         ((? fixnum?) (if (and (fx>= new 0) (fx<= new #xffff)) new 0))
         ((? string?) (local-server-port-filter old (string->number new)))))

(define (local-server-port-change service)
  (lambda (old new)
    (if (positive? old) (tcp-service-unregister! old))
    (if (positive? new) (tcp-service-register! new service))))

(define-pin beaver-proxy-port-number
  initial: 0
  filter: local-server-port-filter
  name: "Port number fot HTTP/S proxy to listen on.  If 0: no proxy.")

(define-pin beaver-socks-port-number
  initial: 0
  filter: local-server-port-filter
  name: "Port number fot HTTP/S proxy to listen on.  If 0: no socks.")

(kick
  (wire!
   beaver-proxy-port-number post:
   (lambda ()
     (let ((v (beaver-proxy-port-number))
           (names '("http_proxy" "https_proxy")))
       (cond-expand
        (android
         (lnjscheme-eval
          (if (= v 0)
              `(begin
                 (webview-set-proxy! 'http #f #f)
                 (webview-set-proxy! 'https #f #f))
              `(begin
                 (webview-set-proxy! 'http "127.0.0.1" ,(number->string v))
                 (webview-set-proxy! 'https "127.0.0.1" ,(number->string v))))))
        (else #f))
       (if (= v 0) (for-each setenv names)
           (let ((v (string-append "http://127.0.0.1:" (number->string v))))
             (for-each (lambda (n) (setenv n v)) names))))))
  (wire! beaver-proxy-port-number sequence: (local-server-port-change http-proxy))
  (wire! beaver-socks-port-number sequence: (local-server-port-change socks-server)))

(define string-empty?
  (let ((m (rx "^[[:space:]]*$")))
    (lambda (obj) (and (string? obj) (rx~ m obj) #t))))

(define (gui-forward-address-filter old new)
  (cond
   ((string-empty? new) #f)
   ((string-chat-address->unit-id new) new)
   (else new)))

(define-pin beaver-socks-forward-addr
  initial: (or (socks-forward-addr) "")
  filter: gui-forward-address-filter
  name: "SOCKS forward address")

(wire! (list beaver-socks-forward-addr ot0cli-ot0-networks)
       extern: beaver-socks-forward-addr
       critical:
       (lambda (in)
         (let* ((m (and (string? in) (rx~ (rx "^([0-9. -]+):([0-9]+)$") in)))
                (nw (ot0cli-ot0-networks))
                (n (cond
                    ((not m) in)
                    ((string-chat-address->unit-id (rxm-ref m 1)) =>
                     (lambda (ndid)
                       (and (pair? nw)
                            (string-append
                             "["
                             (ip6addr->string (make-6plane-addr (car nw) ndid))
                             "]:"
                             (rxm-ref m 2)))))
                    (else in))))
           (socks-forward-addr (and (pair? nw) n)))))


(define glgui-chat-messages-are-valid?
  ;; pre-check messages before they break the glgui widget
  (cond-expand
   ;; TBD: check only if feature `debug` is enabled.
   (else
    (let ((each (match-lambda
                 (((? number? timestamp) (? string? sender) (? string? message) (? fixnum? kind)) #t)
                 (_ #f))))
      (lambda (lst)
        (let loop ((lst lst))
          (or (null? lst) (and (each (car lst)) (loop (cdr lst))))))))))

;; Mode/Tool Switch (TBD: move)

(define-pin visible-tl-options
  initial: '()
  pred: (lambda (lst) (every string? lst))
  name: "Active Toplevel Switch Options")

(define-pin selected-display
  initial: "about"
  filter: (lambda (o n) (if (member n (visible-tl-options)) n o))
  name: "Toplevel Display (a registered option)")

;; Modes

(define (cb-tool-selection-change gui wgt type x y)
  (kick (selected-display (list-ref (visible-tl-options) (glgui-widget-get gui wgt 'current)))))

(define (tool-selection-draw-choice font)
  (lambda (str)
    (lambda (lg lw x y w h s)
      (if s (glgui:draw-box x y w h Black))
      (glgui:draw-text-left (+ x 5) y (- w 10) h str font White))))

(define-pin chat-messages
  initial: '()
  pred: glgui-chat-messages-are-valid?
  name: "Chat messages")


(define-pin chat-messages-limit 30)

(define chat-sender-list-valid?
  ;; pre-check senders before they break things
  (cond-expand
   ;; TBD: check only if feature `debug` is enabled.
   (else
    (let ((each (lambda (x) (and (number? x) (positive? x) (exact? x)))))
      (lambda (lst)
        (let loop ((lst lst))
          (or (null? lst) (and (each (car lst)) (loop (cdr lst))))))))))

(define-pin chat-inbox-senders
  initial: '()
  pred: chat-sender-list-valid?
  name: "List of senders having messages not yet seen.")

(define (chat-add-message! from reference msg kind #!key (store chat-messages))
  (let* ((from
          (cond
           ((equal? from (chat-own-address)) "me")
           ((string? from) from)
           (else (object->string from))))
         (new (list reference from msg kind)))
    (kick
     (let ((seen (store)))
       (unless (member new seen)
         (store (cons new (take seen (chat-messages-limit)))))))))

(define decode-ot0-message-payload u8vector->object)
(define encode-ot0-message-payload object->u8vector)

(define (chat-post-message! to reference payload)
  ((ot0cli-send!) 'post to reference payload))

(let ((orig (unbox ot0cli-on-ot0-received))
      (payload-seen (encode-ot0-message-payload '(chat seen))))
  (define (on-incomming-message type from reference data)
    (case type
      ((request)
       (let ((payload (decode-ot0-message-payload data)))
         (match
          payload
          (('chat (? string? msg))
           (begin
             (chat-partner-add-message! from (chat-own-address) reference msg 0)
             (chat-post-message! from reference payload-seen)))
          (('chat 'seen)
           (pin-filter!
            chat-pending-messages
            (lambda (msg)
              (not (and (equal? (car msg) from)
                        (equal? (cadr msg) reference))))))
          (_ (orig type from reference data)))))
      (else (orig type from reference data))))
  (set-box! ot0cli-on-ot0-received on-incomming-message))

(define-pin chat-pending-messages '())

(define (chat-send-pending-message! msg)
  (apply chat-post-message! msg))

(thread-start!
 (make-thread
  (lambda ()
    (do () (#f)
      (thread-sleep! 10)
      (if (ot0cli-server)
          (for-each chat-send-pending-message! (chat-pending-messages)))))
  'send-pending))

(define-pin chat-address
  initial: #f
  pred: (lambda (x) (or (not x) (exact? x)))
  name: "Address for chat messages.")

(define (remove-chat-address-from-inbox-senders)
  ;; FIXME: has no effect!
  (pin-filter! chat-inbox-senders (lambda (x) (not (equal? (chat-address) x)))))

(wire! chat-address post: remove-chat-address-from-inbox-senders)

(define-pin selected-display
  initial: "about"
  filter: (lambda (o n) (if (member n (visible-tl-options)) n o))
  name: "Toplevel Display (a registered option)")

(define chat-own-address beaver-local-unit-id)

;;(pin-attach-log! calculator-result 'calculator-result)
;;(pin-attach-log! calculator-mem1 'calculator-mem1)

(define id2phone beaver-unit-id->beaver-number)
(define phone2id beaver-number->beaver-unit-id)
(define (chat-number->neatstring num #!optional (gap "·"))
  (or (beaver-unit-id->string num gap)
      (string-append "ERROR: illegal beaver number " (number->string num))))
(define string-chat-address->unit-id unit-id-string->unit-id)

;; Persistent data

(define chat-partners (make-table))
(define (chat-partner-known num) (table-ref chat-partners num #f))
(define (chat-partner->neatstring num)
  (match
   (chat-partner-known num)
   (((? string? name) . more) name)
   (_ (if (number? num)
          (if (equal? num (chat-own-address))
              "me"
              (chat-number->neatstring num))
          ""))))

(define (persistant-file-name)
  (make-pathname (ot0-context) "chat.data"))

(define (current-persistant-data)
  (vector
   chat-partners (chat-pending-messages) (chat-inbox-senders) #f #f
   (beaver-proxy-port-number) (beaver-socks-port-number) (beaver-socks-forward-addr)
   ))

(define-pin persistent-data
  initial: (current-persistant-data)
  pred: vector?
  ;; NO! filter: (lambda (o n) (if (equal? o n) o n))
  name: "persistent data")

(define (write-persistent-data)
  (let ((content (object->u8vector (persistent-data))))
    (call-with-output-file (persistant-file-name) ;; (make-pathname dir "origin")
      (lambda (port)
        (write-subu8vector content 0 (u8vector-length content) port)))))

(define (set-data-persistent!)
  (let ((current (let ((old (read-file-as-u8vector (persistant-file-name))))
                   (if old (u8vector->object old) (current-persistant-data)))))
    (kick/sync
     (do ((i 0 (fx+ i 1)))
         ((fx= i (vector-length current)))
       (case i
         ((0) (set! chat-partners (vector-ref current 0)))
         ((1) (chat-pending-messages (vector-ref current 1)))
         ((2) (chat-inbox-senders (vector-ref current 2)))
         ((3 4) #f)
         ((5) (beaver-proxy-port-number (vector-ref current 5)))
         ((6) (beaver-socks-port-number (vector-ref current 6)))
         ((7) (beaver-socks-forward-addr (vector-ref current 7)))))
     (wire! persistent-data post: write-persistent-data)
     (wire! (list
             #;chat-partners
             chat-inbox-senders
             chat-pending-messages
             beaver-proxy-port-number
             beaver-socks-port-number
             beaver-socks-forward-addr
             )
            post: (lambda () (persistent-data (current-persistant-data)))))))

(wire! ot0-context post: set-data-persistent!)

(define (%chat-partner-set2! num name more)
  (let ((ne `(,name . ,more))
        (oe (table-ref chat-partners num #f)))
    (unless (equal? ne oe)
      (cond
       ((and (null? more) oe)
        (set-cdr! ne (cdr oe)))
       (else (set-cdr! ne (list '()))))
      (table-set! chat-partners num ne)
      (kick (persistent-data (current-persistant-data))))))

(define (chat-partner-set! num . more)
  (if (pair? more)
      (%chat-partner-set2! num (car more) (cdr more))
      (when (table-ref chat-partners num #f)
        (table-set! chat-partners num)
        (kick (persistent-data (current-persistant-data))))))

(define (chat-partner-add-message! from to reference msg kind)
  (let* ((ref (if (equal? to (chat-own-address)) from to))
         (e (table-ref chat-partners ref #f))
         (new (list reference from msg kind)))
    (unless (and e (member new (cadr e)))
      (when (equal? ref from) ((audible-beep)))
      (table-set!
       chat-partners ref
       (if e
           (let ((name (car e))
                 (msgs (cadr e)))
             `(,name ,(cons new (take msgs (chat-messages-limit))) . ,(cddr e)))
           (list #f (list new)))))
    (kick
     (unless (or (equal? from (chat-own-address))
                 (equal? from (chat-address)))
       (let ((seen (chat-inbox-senders)))
         (unless (member from seen)
           (chat-inbox-senders (cons from seen)))))
     (cond
      ((equal? (chat-address) from)
       (chat-add-message! from reference msg kind store: chat-messages))
      ((and (chat-address) (equal? (chat-own-address) from))
       (chat-add-message! from reference msg kind store: chat-messages)))
     (persistent-data (current-persistant-data)))))

(define (chat-set-current-partner!)
  (define (conv e)
    `(,(car e) ,(chat-partner->neatstring (cadr e)) . ,(cddr e)))
  (kick
   (let ((e (table-ref chat-partners (chat-address) '(0 ()))))
     (chat-messages (map conv (cadr e))))))

(wire! chat-address post: chat-set-current-partner!)

(define (beaverchat-payload-config launch-url)
  (define (make-beaverchat-payload rect interval)
    (let* ((xsw (interval-lower-bound interval 0))
           (ysw (interval-lower-bound interval 1))
           (xno (interval-upper-bound interval 0))
           (yno (interval-upper-bound interval 1))
           (w (- xno xsw))
           (h (- yno ysw))
           (gui (guide-rectangle-glgui rect))
           (bag (glgui-container gui 0 0 w h))
           (fnt (select-font size: 'small))
           (cllx 0)
           (clly (/ h 2))
           (cw w)
           (ch (/ h 2))
           (line-height 20)
           (line-height-selectable 28)
           (text-keypad keypad:simplified)
           (dial-keypad keypad:numeric)
           (keypad (cond-expand
                    ((and exclude-this-code android)
                     (glgui-native-keypad bag 0 0 w (- clly line-height line-height)))
                    (else (glgui-keypad bag 0 0 w (- clly line-height line-height) fnt)))))
      (define (callback gui wgt type x y)
        (debug 'chat-callback y)
        #f)
      (define (update-keypad)
        (glgui-widget-set! bag keypad 'keypad (if (number? (chat-address)) text-keypad dial-keypad)))
      (update-keypad)
      (glgui-widget-set! gui bag 'hidden #t)
      (let* ((illy (- clly line-height))
             (illx cllx)
             (iw w)
             (cwgt (glgui-chat bag cllx clly cw ch line-height (chat-messages) fnt callback))
             (todisplay (glgui-label bag illx illy iw line-height "" fnt White))
             (input (glgui-label bag illx (- illy line-height) iw line-height "" fnt White))
             (nick-dialog #f)
             (nick-dialog-keypad keypad:simplified)
             (ponebook-dialog #f))
        (define (follow-input) (glgui-widget-set! gui cwgt 'list (chat-messages)) (Xtrigger-redraw!))
        (define (follow-to) (glgui-widget-set! gui todisplay 'label (chat-partner->neatstring (chat-address))) (Xtrigger-redraw!))
        (define (ask-for-nick to)
          (set! nick-dialog
                (Xglgui-value-edit-dialog
                 bag 0 0 w h
                 label: (chat-number->neatstring to)
                 value: ""
                 input: (lambda (val)
                          (set! nick-dialog #f)
                          (unless (string-empty? val) (chat-partner-set! to val))
                          (chat-address to))
                 keypad: nick-dialog-keypad)))
        (define (select-from-phonebook)
          (let ((all
                 (sort
                  (map
                   (lambda (p)
                     (cons
                      (if (pair? (cdr p))
                          (or (cadr p) (chat-number->neatstring (car p)))
                          (chat-number->neatstring (car p)))
                      p))
                   (table->list chat-partners))
                  (lambda (a b) (string<? (car a) (car b)))))
                (border (* 0.1 line-height)))
            (set! ponebook-dialog
                  ((Xglgui-select
                    bag border border (- w (* 2 border)) (- h (* 2 border))
                    line-height: line-height-selectable color: White)
                   (map car all)
                   (lambda (sel x)
                     (if (>= sel 0)
                         (let ((e (cdr (list-ref all sel))))
                           (if (cadr e)
                               (cond
                                ((< x 1/3)
                                 (when ponebook-dialog (ponebook-dialog 'close) (set! ponebook-dialog #f))
                                 (let ((pn (chat-number->neatstring (car e) "-")))
                                   (launch-url
                                    (string-append "http://" pn "." (beaver-domain) "/index")
                                    via: (if (< x 1/4) 'webview 'extern))))
                                ((> x 2/3)
                                 (set! nick-dialog
                                       (Xglgui-value-edit-dialog
                                        bag 0 0 w h
                                        label: (chat-number->neatstring (car e))
                                        value: (cadr e)
                                        input: (lambda (val)
                                                 (set! nick-dialog #f)
                                                 (if (string-empty? val)
                                                     (chat-partner-set! (car e)) ;; remove
                                                     (begin
                                                       (chat-address (car e))
                                                       (chat-partner-set! (car e) val))))
                                        keypad: nick-dialog-keypad)))
                                (else (kick (chat-address (car e)))))
                               (ask-for-nick (car e))))
                         (when ponebook-dialog (ponebook-dialog 'close) (set! ponebook-dialog #f))))))))
        (define (close-chat!)
          (when ponebook-dialog (ponebook-dialog 'close) (set! ponebook-dialog #f))
          (kick (chat-address #f)
                (selected-display (list-ref (visible-tl-options) 0))))
        (define (on-event rect pl event x y)
          (define skip #f)
          (define gui (guide-rectangle-glgui rect))
          (cond
           ((= event EVENT_KEYRELEASE)
            (cond
             ((= x EVENT_KEYESCAPE) (set! skip #t) (close-chat!))
             ((= x EVENT_KEYENTER)
              (set! skip #t)
              (Xtrigger-redraw!)
              (let ((msg (glgui-widget-get bag input 'label)))
                (glgui-widget-set! bag input 'label "")
                (glgui-widget-set! gui input 'bgcolor Black)
                (cond
                 ((and (chat-address) (equal? msg ""))
                  (close-chat!))
                 ((number? (chat-address))
                  (let ((timestamp (inexact->exact (floor (time->seconds (current-time)))))
                        (to (chat-address))
                        (payload (encode-ot0-message-payload `(chat ,msg))))
                    (kick
                     (chat-pending-messages (cons (list to timestamp payload) (chat-pending-messages)))
                     (chat-partner-add-message! (chat-own-address) to timestamp msg 1))
                    (if (ot0cli-server)
                        (chat-post-message! to timestamp payload)
                        (glgui-widget-set! bag input 'label "Error: not connected!"))))
                 (nick-dialog (guide-default-event-dispatch rect pl event x y))
                 ((not (chat-address))
                  (let ((to (string-chat-address->unit-id msg)))
                    (cond
                     (to
                      (if (let ((x (chat-partner-known to)))
                            (and x (car x)))
                          (chat-address to)
                          (ask-for-nick to)))
                     ((string-empty? msg) (select-from-phonebook))
                     (else
                      (glgui-widget-set! gui input 'bgcolor Red)
                      (glgui-widget-set! bag input 'label msg))))))
                #f))))
           ((and (= event EVENT_KEYPRESS) (= x EVENT_KEYESCAPE)) (set! skip #t)))
          (or skip (guide-default-event-dispatch rect pl event x y)))
        (glgui-widget-set! gui input 'focus #t)
        (glgui-widget-set! gui input 'enableinput #t)
        ;;        (glgui-widget-set! gui input 'draw-handle Xglgui:label-draw2)
        (wire! chat-messages post: follow-input)
        (wire! chat-address post: update-keypad)
        (wire! chat-address post: follow-to)
        ;; Wired to globals, MUST be instanciated once only.
        (make-guide-payload
         in: interval widget: bag lifespan: 'once
         on-any-event: on-event))))
  make-beaverchat-payload)

(define (define-payload-beaverchat! name launch-url)
  ;; Wired to globals, MUST be instanciated once only.
  (guide-define-payload name 'once (beaverchat-payload-config launch-url)))

(define (glgui-beaverchat launch-url beaver-domain) ;; Note: this is currently a once-at-most call!

;;;  (define field_gradient (list (color:shuffle #xe8e9eaff) (color:shuffle #xe8e9eaff) (color:shuffle #xfefefeff) (color:shuffle #xfefefeff)))

  (define make-about-payload
    (let ((lifespan 'once)) ;; wired to globals - once only!
      (define (make-about-payload rect interval)
        (let* ((xsw (interval-lower-bound interval 0))
               (ysw (interval-lower-bound interval 1))
               (xno (interval-upper-bound interval 0))
               (yno (interval-upper-bound interval 1))
               (w (- xno xsw))
               (h (- yno ysw))
               (gui (guide-rectangle-glgui rect))
               (bag (glgui-container gui 0 0 w h))
               (str "(C) JFW")
               (fnt (select-font size: 'small))
               (line-height 16)
               (line-height-selectable 28))
          (define (callback gui wgt type x y)
            (glgui-widget-delete gui wgt)
            #f)
          (define (pin-display-chat-number n tag pin)
            (define (conv v)
              (case v
                ((#f) "no")
                ((#t) "yes")
                (else
                 (cond
                  ((and (number? v) (exact? v)) (chat-number->neatstring v))
                  (else v)))))
            (let ((setter (Xglgui-valuelabel
                           bag 10 (- h 10 (* n line-height)) (- w 10) line-height
                           label: tag
                           value: (conv (pin))
                           ;; input: #f
                           )))
              (wire! pin post: (lambda () (setter (conv (pin)))))))
          (define (pin-display n tag pin)
            (define (conv v)
              (case v
                ((#f) "no")
                ((#t) "yes")
                (else
                 (cond
                  ((and (number? v) (exact? v)) (number->string v))
                  (else v)))))
            (let ((setter (Xglgui-valuelabel
                           bag 10 (- h 10 (* n line-height)) (- w 10) line-height
                           label: tag
                           value: (conv (pin))
                           ;; input: #f
                           )))
              (wire! pin post: (lambda () (setter (conv (pin)))))))
          (define (pin-edit base n tag pin kbd value-string)
            (Xglgui-pin-editable
             bag  10 (- h base (* n line-height-selectable)) (- w 10) line-height-selectable
             tag pin kbd value-string: value-string))
          (pin-display 1 "kick-style" kick-style)
          (pin-display 2 "vpn" ot0cli-server)
          (pin-display-chat-number 3 "Address" chat-own-address)
          (let ((base (* 4 line-height)))
            (pin-edit base 1 "proxy port" beaver-proxy-port-number keypad:numeric #f)
            (pin-edit base 2 "socks port" beaver-socks-port-number keypad:numeric #f)
            (pin-edit base 3 "forward" beaver-socks-forward-addr keypad:hexnum
                      (lambda (x) (if (not x) "" x)))
            )
          (glgui-button-string
           bag (/ w 10) 100 (* w 8/10) line-height-selectable "Browse Homepage (needs proxy)" (select-font size: 'small)
           (lambda (gui wgt type x y)
             (if (positive? (beaver-proxy-port-number))
                 (launch-url
                  (string-append "http://127.0.0.1:" (number->string (beaver-proxy-port-number)))
                  via: (if (< x (/ w 2)) 'webview 'extern)))))
          (let* ((w2 (/ w 2))
                 (border 10)
                 (w2w (- w2 border))
                 (y (* 4 line-height))
                 (fnt (select-font size: 'small)))
            (Xglgui-label bag border y w2w line-height text: "Pending" size: 'small)
            (let ((wgt (glgui-button-string
                        bag w2 y w2w line-height "" fnt
                        (lambda (gui wgt type x y)
                          (kick (chat-pending-messages '()))))))
              (define (setter)
                (glgui-widget-set! bag wgt 'image (list (number->string (length (chat-pending-messages))))))
              (setter)
              (wire! chat-pending-messages post: setter)))
          (let* ((border (/ line-height 2))
                 (line-height-selectable 28)
                 (content (chat-inbox-senders))
                 (ctrl
                  ((Xglgui-select
                    bag border border (- w (* 2 border)) (- h (* 2 border))
                    line-height: line-height-selectable color: White)
                   '()
                   (lambda (sel x)
                     (if (>= sel 0)
                         (let ((sel2 (list-ref content sel)))
                           (kick
                            (chat-address sel2)
                            (selected-display "chat")))))
                   permanent: #t)))
            (define (new-content)
              (set! content (chat-inbox-senders))
              (let ((new (map chat-partner->neatstring content)))
                (ctrl set: new)
                (ctrl hidden: (null? new))))
            (new-content)
            (wire! chat-inbox-senders post: new-content))
          (glgui-button-string bag (/ w 4) (/ h 4) (/ w 2) (/ h 2) str fnt callback)
          (let* ((ctx bag)
                 (wb (/ w 5))
                 (x 0 #;(- w wb))
                 (y (* h 0))
                 (h (/ h 15))
                 (fnt fnt)
                 (label "exit")
                 (callback (lambda (gui wgt type x y) (terminate))))
            (Xglgui-button ctx x y wb h font: fnt label: label glgui-callback: callback))
          (make-guide-payload in: interval widget: bag lifespan: lifespan)))
      (guide-define-payload "about" lifespan make-about-payload)
      make-about-payload))

  (define (guide-start!)

    (define (init! w h)
      (make-window 320 502 #;480)
      (glgui-orientation-set! GUI_PORTRAIT)
      (let* ((rect (guide-make-gui))
             (payload (guide-make-payload rect "guide"))
             (selector #f)
             (w (guide-rectangle-width rect))
             (h (guide-rectangle-height rect))
             (gui (guide-rectangle-glgui rect))
             (mh 28)
             (menu-area (make-interval (vector 0 (- h mh)) (vector w h)))
             (payload-area (make-interval '#(0 0) (vector w (- h mh))))
             (menu-font (select-font size: 'medium))
             (k 'dd))
        (when (> mh 0)
          (case k
            ((men)
             (glgui-menubar gui 0 (- h (* mh 2)) w (* mh 2))
             ;; (glgui-button-string bag (/ w 4) (/ h 4) (/ w 2) (/ h 2) str fnt callback)
             (glgui-label gui 0 (- h mh) #;(- h (/ mh 2)) 80 mh "Menu" menu-font White))
            ((dd)
             (set!
              selector
              ((make-tl-selection-payload
                rect payload menu-area payload-area menu-font)
               selected-display visible-tl-options guide-current-colorscheme)))))
        (kick
         (visible-tl-options '("calculator" "chat" "about"))
         (selected-display "calculator"))
        (values rect payload)))

    (guide-define-payload-calculator! "calculator")
    (define-payload-beaverchat! "chat" launch-url)
    (guide-main
     ;; initialization
     init!
     ;; events: event-dispatcher
     ;; suspend: terminate
     )
    )
  (guide-start!))

(log-status "initializing chat completed")
;; eof
