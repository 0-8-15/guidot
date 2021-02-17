;; (C) 2020, 2021 Jörg F. Wittenberger, GPL

(log-status "initializing chat")

(declare
 (not standard-bindings
      take drop ;; actually they need to be standard
      ))

(include "../misc-conventions/observable-syntax.sch")

;; Mode/Tool Switch (TBD: move!)

(define-pin visible-tl-options
  initial: '#("")
  pred:
  (lambda (vec)
    (define predicate? string?)
    (and (vector? vec) ;; check all elements for `predicate?`
         (do ((i (fx- (vector-length vec) 1) (fx- i 1))
              (result #t (and result (predicate? (vector-ref vec i)))))
             ((eqv? i -1) result))))
  name: "Active Toplevel Switch Options")

(define-pin PIN:toplevel-selection
  initial: 0
  pred: (lambda (n) (and (number? n) (< -1 n (vector-length (visible-tl-options)))))
  name: "guide toplevel selection")

;;** Init (TBD: move!)

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
  name: "Port number for HTTP/S proxy to listen on.  If 0: no proxy.")

(define-pin beaver-socks-port-number
  initial: 0
  filter: local-server-port-filter
  name: "Port number for SOCKS proxy to listen on.  If 0: no socks.")

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

(define string-match-ipv6+port?
  (let ((m (rx '(seq "[" ipv6-address "]:" (+ numeric)))))
    (lambda (str) (rx~= m str))))

(define string-match-unit-id+port?
  (let ((pattern
         (rx '(seq (submatch (** 13 13 (seq numeric (? (or #\. #\- #\space)))))
                   (? (seq ":" (+ numeric)))))))
    (lambda (str)
      (let ((match (rx~= pattern str)))
        (and match (string-chat-address->unit-id (rxm-ref match 1)))))))

(define (gui-check-ggb/string-pred pred)
  (lambda (ggb)
    (let ((str (make-string (ggb-length ggb))))
      (ggb-for-each ggb (lambda (i v) (string-set! str i (integer->char v))))
      (pred str))))

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

;;** Beaver Chat

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

(define (make-beaverchat-payload
         launch-url beaver-domain
         #!key
         (in (current-guide-gui-interval))
         (keypad guide-keypad/default)
         )
  (let* ((area in)
         (xsw (mdvector-interval-lower-bound area 0))
         (ysw (mdvector-interval-lower-bound area 1))
         (xno (mdvector-interval-upper-bound area 0))
         (yno (mdvector-interval-upper-bound area 1))
         (w (- xno xsw))
         (h (- yno ysw))
         (fnt (guide-select-font size: 'small))
         (line-height 20)
         (foreground-color (guide-select-color-2))
         (line-height-selectable 28))
    (define (close-chat!)
      (kick (chat-address #f)
            (PIN:toplevel-selection 2)))
    (define (callback msg)
      (set! msg (ggb2d->string msg))
      (cond
       ((equal? msg "")) ;; ingore empty messages
       ((number? (chat-address))
        (let ((timestamp (inexact->exact (floor (time->seconds (current-time)))))
              (to (chat-address))
              (payload (encode-ot0-message-payload `(chat ,msg))))
          (kick
           (chat-pending-messages (cons (list to timestamp payload) (chat-pending-messages)))
           (chat-partner-add-message! (chat-own-address) to timestamp msg 1))
          (if (not (ot0cli-server))
              (PIN:toplevel-selection 2) ;; error out into about dialog
              (chat-post-message! to timestamp payload))))
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
            (debug 'Missing 'PartnerSelectionError)))))))
    (let* ((title-area (make-mdv-rect-interval xsw (- yno line-height) xno yno))
           (to-display #f)
           (update-to-display!
            (lambda ()
              (let* ((addr (chat-address))
                     (btn (guide-button
                           in: title-area font: fnt
                           label: (if addr (chat-partner->neatstring addr) "")
                           guide-callback: (lambda _ (chat-address #f)  #f))))
                (set! to-display btn))))
           (chat-ctrl! #f)
           (chat-payload
            (receive (pl ctrl)
                (make-chat
                 in: (make-mdv-rect-interval xsw ysw xno (- yno line-height))
                 font: fnt line-height: line-height
                 keypad: keypad
                 action: callback)
              (set! chat-ctrl! ctrl)
              (chat-ctrl! load: (chat-messages))
              pl))
           (dialog #f)
           (nick-dialog-keypad guide-keypad/simplified))
      (define (dialog-set! payload)
        (guide-focus payload)
        (set! dialog payload))
      (define (activate-chat-partner! to)
        (dialog-set! #f)
        (chat-address to))
      (define (nick-dialog to remove-when-empty)
        (guide-value-edit-dialog
         in: area label: (chat-number->neatstring to)
         keypad: nick-dialog-keypad
         data:
         (case-lambda
          (() "")
          ((val)
           (with-exception-catcher
            (lambda (exn) #f)
            (if remove-when-empty
                (lambda ()
                  (dialog-set! #f)
                  (if (string-empty? val)
                      (chat-partner-set! to) ;; remove
                      (begin
                        (chat-partner-set! to val)
                        (activate-chat-partner! to))))
                (lambda ()
                  (unless (string-empty? val) (chat-partner-set! to val))
                  (activate-chat-partner! to))))))))
      (define chat-address/conversion
        (case-lambda
         (() (let ((val (chat-address)))
               (if val (chat-number->neatstring val) "")))
         ((val)
          (let ((to (string-chat-address->unit-id val)))
            (cond
             (to
              (if (let ((x (chat-partner-known to)))
                    (and x (car x)))
                  (activate-chat-partner! to)
                  (dialog-set! (nick-dialog to #f))))
             ((string-empty? val) (select-from-phonebook))
             (else
              (debug 'Missing 'PartnerSelectionError)
              "invalid phone number"))))))
      (define (select-from-phonebook)
        (let ((all
               (sort!
                (lambda (a b) (string<? (car a) (car b)))
                (map
                 (lambda (p)
                   (cons
                    (if (pair? (cdr p))
                        (or (cadr p) (chat-number->neatstring (car p)))
                        (chat-number->neatstring (car p)))
                    p))
                 (table->list chat-partners))))
              (border (* 0.1 line-height)))
          (dialog-set!
           (guide-list-select-payload
            area
            (let ((data (map car all))) (lambda () data))
            action:
            (lambda (sel x)
              (if (>= sel 0)
                  (let ((e (cdr (list-ref all sel))))
                    (if (cadr e)
                        (cond
                         ((< x 1/3)
                          (dialog-set! #f)
                          (let ((pn (chat-number->neatstring (car e) "-")))
                            (launch-url
                             (string-append "http://" pn "." (beaver-domain) "/index")
                             via: (if (< x 1/4) 'webview 'extern))))
                         ((> x 2/3)
                          (dialog-set! (nick-dialog (car e) #t)))
                         (else
                          (activate-chat-partner! (car e))))
                        (dialog-set! (nick-dialog (car e) #f))))
                  (dialog-set! #f)))
            line-height: line-height-selectable
            font: (guide-select-font size: 'medium)
            horizontal-align: 'left))))
      (define (dial-dialog!)
        (dialog-set!
         (guide-value-edit-dialog
          in: area label: "dial" data: chat-address/conversion
          font: fnt line-height: line-height-selectable
          validate: (gui-check-ggb/string-pred string-chat-address->unit-id))))
      (define (redraw!)
        (cond
         (dialog (guide-event-dispatch-to-payload/redraw dialog))
         (else
          (guide-event-dispatch-to-payload/redraw chat-payload)
          (guide-event-dispatch-to-payload/redraw to-display))))
      (define (on-event rect pl event x y)
        (cond
         (dialog (guide-event-dispatch-to-payload rect dialog event x y))
         ((case event ((press: release:) #t) (else #f))
          (guide-event-dispatch-to-payload rect chat-payload event x y))
         ((guide-payload-contains/xy? to-display x y)
          (guide-event-dispatch-to-payload rect to-display event x y))
         (else (guide-event-dispatch-to-payload rect chat-payload event x y))))
      (update-to-display!)
      (unless (chat-address) (dial-dialog!))
      (let* ((when-wired
              (lambda ()
                (chat-ctrl! load: (chat-messages))))
             (toggle! (wire! chat-messages switchable: #t post: when-wired))
             (to-toggle! (wire! chat-address switchable: #t post:
                                (lambda ()
                                  (unless (chat-address) (dial-dialog!))
                                  (update-to-display!))))
             (result
              (make-guide-payload
               name: 'beaverchat
               in: area lifespan: 'ephemeral
               on-redraw: redraw!
               on-any-event: on-event)))
        (make-will result (lambda (pl) (to-toggle!) (toggle!)))
        result))))

(define (beaverchat-keypad/ipv6-or-unit+port #!key (in (current-guide-gui-interval)) (action #f))
  (guide-make-keypad
   in
   (make-mdvector
    (range '#(5 5))
    (vector
     #\0 #\1 #\2 #\3 #\[
     #\4 #\5 #\6 #\7 #\]
     #\8 #\9 #\a #\b #\:
     #\c #\d #\e #\f #\.
     ;; last line
     (list EVENT_KEYLEFT label: "<-")
     (list EVENT_KEYRIGHT label: "->")
     (list EVENT_KEYBACKSPACE label: (apply make-glC:image glgui_keypad_delete.img))
     (list EVENT_KEYENTER label: (apply make-glC:image glgui_keypad_return.img))
     #f
     ))
   on-key: action))

(log-status "initializing chat completed")
;; eof
