;; (C) 2020, 2021 Jörg F. Wittenberger, GPL

;; (log-status "initializing chat")

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

(define (local-server-port-change-possible? new)
  ;; Issue: when the service can't be registered, the STM core aborts
  ;; with an asynchronous exception we can not catch.  That's a good
  ;; thing in principle and MUST NOT be changed.
  (and (positive? new)
       (begin
         ;; FIXME: This workaround still contains a race exception.
         ;; We register the service, which may fail -> great, we
         ;; avoid the global fail, unregister it and register it
         ;; eventually in the global context.  The RACE condition: if
         ;; any other programm registered the service between thise
         ;; two moments, we still fail.
         (tcp-service-register! new (lambda () #t))
         ;; FIXME: in principle, we could keep the service registered
         ;; here, but then we would need an "undo of side effects
         ;; from value checks" -- which is conceptuially worse.
         (tcp-service-unregister! new)
         #t)))

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

(cond-expand
 (android
  (define (on-android-set-webview-proxy v)
    (cond
     ((or (not v) (eqv? v 0))
      (lnjscheme-eval
       `(begin
          (webview-set-proxy! 'http #f #f)
          (webview-set-proxy! 'https #f #f))))
     (else
      (log-status "requesting proxy setting to port " v)
      (lnjscheme-eval
       `(begin
          (webview-set-proxy! 'http "127.0.0.1" ,(number->string v))
          (webview-set-proxy! 'https "127.0.0.1" ,(number->string v))))
      (log-status "proxy setting completed")))))
 (else (define-macro (on-android-set-webview-proxy . x) '(begin))))

(define-pin beaverchat-webview-proxy-automatic
  initial: #f
  pred: boolean?
  name: "Automatically set webview proxy when proxy is enabled.")

(kick
  (wire!
   beaver-proxy-port-number post:
   (lambda ()
     (cond-expand
      (android
       (when (beaverchat-webview-proxy-automatic)
         (on-android-set-webview-proxy v)))
      (else #f))
     (let ((v (beaver-proxy-port-number))
           (names '("http_proxy" "https_proxy")))
       (if (= v 0) (for-each setenv names)
           (let ((v (string-append "http://127.0.0.1:" (number->string v))))
             (for-each (lambda (n) (setenv n v)) names))))))
  (wire! beaver-proxy-port-number
         sequence: (local-server-port-change http-proxy))
  (wire! beaver-socks-port-number
         sequence: (local-server-port-change socks-server)))

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

(define beaverchat-messages-all-valid?
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
  pred: beaverchat-messages-all-valid?
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

(let ((orig (on-ot0-received))
      (payload-seen (encode-ot0-message-payload '(chat seen))))
  (define (on-incomming-message type from reference data)
    (define (sanitized-message msg)
      (let* ((errors 0)
             (checked
              (utf8string->ggb
               msg
               (lambda (i)
                 (set! errors (+ errors 1))
                 #xfffd))))
        (if (eqv? errors 0)
            msg
            (let ((conversion-buffer (make-ggb2d)))
              (ggb2d-insert-row! conversion-buffer checked)
              (string-append
               "WARNING: The message contained "
               (number->string errors)
               "UTF8 encoding errors, sanitized message:\n"
               (ggb2d->string/encoding-utf8 conversion-buffer))))))
    (case type
      ((request)
       (let ((payload (decode-ot0-message-payload data)))
         (match
          payload
          (('chat (? string? msg))
           (begin
             (chat-partner-add-message!
              from (chat-own-address) reference (sanitized-message msg) 0)
             (chat-post-message! from reference payload-seen)))
          (('chat 'seen)
           (pin-filter!
            chat-pending-messages
            (lambda (msg)
              (not (and (equal? (car msg) from)
                        (equal? (cadr msg) reference))))))
          (_ (orig type from reference data)))))
      (else (orig type from reference data))))
  (on-ot0-received on-incomming-message))

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
(define (chat-number->neatstring num #!optional (gap (string (integer->char 183)))) ;; "·"
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
  (let ((current (let ((old #;(read-file-as-u8vector (persistant-file-name))
                        (with-exception-catcher
                         ;;; TBD: remove this ASAP, for backward
                         ;;; compatibility we ignore deserialization
                         ;;; issues.
                         (lambda (exn) #f)
                         (lambda () (read-file-as-u8vector (persistant-file-name))))))
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
      (table-set!
       chat-partners ref
       (if e
           (let ((name (car e))
                 (msgs (cadr e)))
             `(,name ,(sort!
                       (lambda (a b) (> (car a) (car b)))
                       (cons new (take msgs (chat-messages-limit)))) . ,(cddr e)))
           (list #f (list new)))))
    (kick
     (when (equal? ref from) (guide-critical-add! (audible-beep) async: #t))
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

;; GUI

(define-macro (%%guide-post-speculative expr)
  ;; either a thunk or a promise -- promise seems NOT to work under
  ;; gamit?
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

(define-macro (%%macro-guidot-capture-guide-toplevel)
  (let ((before (gensym 'before)))
    `(let ((,before (guide-toplevel-payload)))
       (lambda _ (guide-toplevel-payload ,before)))))

(define (beaverchat-required-key-parameter key location)
  (error "required key parameter" key location))

(define (beaverchat-port-edit
         #!key
         (in (beaverchat-required-key-parameter in: beaverchat-port-edit))
         (label (beaverchat-required-key-parameter label: beaverchat-port-edit))
         (size 'medium)
         (input (beaverchat-required-key-parameter data: beaverchat-port-edit))
         (output input)
         (success (beaverchat-required-key-parameter done: beaverchat-port-edit))
         (fail (lambda (exn)
                 ;; (MATURITY -2 "failed" loc: beaverchat-port-edit exn)
                 #f))
         )
  (define (port-free? n)
    (with-exception-catcher
     (lambda (exn) #f)
     (lambda () (local-server-port-change-possible? n))))
  (guide-value-edit-dialog
   in: in label: label
   keypad: guide-keypad/numeric
   validate:
   (gui-check-ggb/string-pred
    (lambda (str)
      (or (string-empty? str)
          (let ((n (string->number str)))
            (and n (or (eqv? n 0)
                       (and
                        (< 1000 n #xffff)
                        (port-free? n))))))))
   data-char-encoding: #f
   data:
   (case-lambda
    (() (input))
    ((val)
     (with-exception-catcher
      fail
      (lambda ()
        (let ((n (string->number val)))
          (and n (local-server-port-change-possible? n)))
        (when output (output val))
        (success val)))))))

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

(define (beaverchat-service-address-edit
         #!key
         (in (beaverchat-required-key-parameter in: beaverchat-service-address-edit))
         (done (%%macro-guidot-capture-guide-toplevel))
         (label (beaverchat-required-key-parameter label: beaverchat-service-address-edit))
         (size 'medium)
         (input (beaverchat-required-key-parameter data: beaverchat-service-address-edit))
         (output input)
         (success (beaverchat-required-key-parameter success: beaverchat-service-address-edit))
         (validate
          (lambda (str)
            (or (string-empty? str)
                (string-match-ipv6+port? str)
                (string-match-unit-id+port? str))))
         (fail (lambda (exn)
                 ;; (MATURITY -2 "failed" loc: beaverchat-service-address-edit exn)
                 #f))
         )
  (guide-value-edit-dialog
   in: in label: label
   keypad: beaverchat-keypad/ipv6-or-unit+port
   on-key:
   (lambda (p/r key mod)
     (if (eq? press: p/r)
         #f ;; ignore press - maybe more
         (case key
           ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f
             #\[ #\] #\: #\.) key)
           (else
            (cond
             ((eqv? key EVENT_KEYRIGHT) EVENT_KEYRIGHT)
             ((eqv? key EVENT_KEYLEFT) EVENT_KEYLEFT)
             ((eqv? key EVENT_KEYBACKSPACE) EVENT_KEYBACKSPACE)
             ((eqv? key EVENT_KEYENTER) EVENT_KEYENTER)
             (else (debug 'ignored key) #f))))))
   validate: (and (procedure? validate) (gui-check-ggb/string-pred validate))
   data-char-encoding: #f
   data:
   (case-lambda
    (() (or (input) ""))
    ((val)
     (if (or (not (procedure? validate)) (validate val))
         (begin
           (when output (output val))
           (success val))
         (fail val))))
   done: done))

(let
    ;; TBD: This might be better promoted to the fossil module --
    ;; EXCEPT that the fossils module does not depend on lwIP!!!
    ((later (make-pin initial: #f filter: (lambda (o n) (or o n))))
     (fossil-server-lwip-port 80))
  (define (at-phone-decoder str)
    (let* ((e0 (string-contains (substring str 1 (string-length str)) "/"))
           (e (if e0 (+ e0 1)  (string-length str))))
      (if (and (> (string-length str) 1) (or (eqv? (string-ref str 0) #\/) (eqv? (string-ref str 0) #\@)))
          (unit-id-string->unit-id (substring str 1 e))
          (unit-id-string->unit-id (if (eqv? e (string-length str)) str (substring str 0 e))))))
  (wire!
   later post:
   (lambda ()
     (httpproxy-atphone-set! at-phone-decoder)
     (capture-domain!
      (beaver-captured-domain)
      handler: fossils-directory-handler
      at-phone-decoder: at-phone-decoder
      network-id: (car (ot0cli-ot0-networks)))
     (lwip-tcp-service-register! fossil-server-lwip-port fossils-directory-service)))
  (wire!
   (list lwIP ot0cli-ot0-networks fossils-directory beaver-captured-domain)
   post:
   (lambda ()
     (cond
      ((and (lwIP) (pair? (ot0cli-ot0-networks)) (fossils-directory) (beaver-captured-domain))
       (later #t))
      ((later)
       (MATURITY -2 "there is NO WAY to switch services off yet" loc: 'beaverchat))))))

(define (guidot-beaver-select-path-payload
         area #!optional
         (done #f) #!key
         (network-id 18374687579166474240))
  ;; TBD: find better check that this is a valid directory
  (unless done
    (let ((before (guide-toplevel-payload)))
      (set! done (lambda _ (guide-toplevel-payload before)))))
  (cond
   ((ot0-context) (done))
   (else
    (let ((val (current-directory)))
      (guide-path-select
       in: area name: "Beaver Directory Selection"
       ;; directory: current-directory
       selected:
       (case-lambda
        (() val)
        ((dir)
         (define control-port
           (cond-expand
            (win32 1313)
            (else (make-pathname dir "control"))))
         (when (equal? dir "..")
           (set! dir (path-directory (fossils-directory))))
         (when (and (file-exists? dir)
                    (eq? (file-type dir) 'directory))
           (set! val dir) ;; beware intensional side effect while speculative!
           (guide-critical-add!
            (let ((args
                   `(
                     "-B" ,dir
                     ip: on
                     -S control ,control-port :
                     -service ot0 start "\"*:0\"" join: ,network-id
                     ;; join: ,(debug-adhoc-network-id) -S vpn tcp register ,(debug-adhoc-network-port) beaver-cmd3 :
                     ;; -S tcp register (debug-adhoc-network-port) replloop2 :
                     -wait
                     )))
              (lambda () (beaver-process-commands args)))
            async: #t)
           done)))
       ignore-hidden: #f
       filter-pred:
       (lambda (x)
         (cond
          ((or (equal? x ".") (equal? x "..")))
          (else (and (file-exists? x) (eq? (file-type x) 'directory)))))
       done: done)))))

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
         (line-height (ceiling (* 11/10 (guide-font-height fnt))))
         (foreground-color (guide-select-color-2))
         (line-height-selectable 28))
    (define (close-chat!)
      (kick (chat-address #f)
            (PIN:toplevel-selection 2)))
    (define (callback ctrl)
      (define msg (ctrl 'string))
      (ctrl text: #f)
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
                 timestamp: #t
                 mode: #f
                 keypad: keypad
                 action: callback)
              (set! chat-ctrl! ctrl)
              pl))
           (dialog #f)
           (nick-dialog-keypad guide-keypad/simplified))
      (define (dialog-set! payload)
        (guide-focus payload)
        (set! dialog payload)
        ;; tail in handler
        #t)
      (define (activate-chat-partner! to)
        (chat-address to)
        (dialog-set! #f))
      (define (nick-dialog to remove-when-empty)
        (guide-value-edit-dialog
         in: area label: (chat-number->neatstring to)
         keypad: nick-dialog-keypad
         on-key: %%guide-textarea-keyfilter
         data-char-encoding: #f
         data:
         (case-lambda
          (() (if (chat-partner-known to) (chat-partner->neatstring to) ""))
          ((val)
           (with-exception-catcher
            (lambda (exn) #f)
            (if remove-when-empty
                (lambda ()
                  (dialog-set! #f)
                  (if (string-empty? val)
                      (begin
                        (chat-partner-set! to) ;; remove
                        #t)
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
              (cond
               ((>= sel 0)
                (let ((e (cdr (list-ref all sel))))
                  (cond
                   ((cadr e)
                    (cond
                     ((< x 1/3)
                      (dial-dialog!)
                      (let* ((pn (chat-number->neatstring (car e) "-"))
                             (url (string-append "http://" pn "." (beaver-domain) "/index"))
                             (via (if (< x 1/4) 'webview 'extern)))
                        ;; %%guide-post-speculative should be enough!
                        (guide-critical-add!
                         (lambda () (launch-url url via: via))
                         async: #t)
                        #t))
                     ((> x 2/3) (dialog-set! (nick-dialog (car e) #t)) #t)
                     (else (activate-chat-partner! (car e)))))
                   (else (dialog-set! (nick-dialog (car e) #f))))))
               (else (dial-dialog!) #t)))
            line-height: line-height-selectable
            font: (guide-select-font size: 'medium)
            horizontal-align: 'left))))
      (define (dial-dialog!)
        (dialog-set!
         (guide-value-edit-dialog
          in: area label: "dial"
          data-char-encoding: #f data: chat-address/conversion
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
         ((and (guide-event-graphics? event) (guide-payload-contains/xy? to-display x y))
          (guide-event-dispatch-to-payload rect to-display event x y))
         (else (guide-event-dispatch-to-payload rect chat-payload event x y))))
      (update-to-display!)
      (unless (chat-address) (dial-dialog!))
      (unless (ot0-context)
        (dialog-set!
         (let ((val (current-directory))
               (done (lambda _ (dial-dialog!))))
           (guide-path-select
            in: area
            ;; directory: current-directory
            selected:
            (case-lambda
             (() val)
             ((dir)
              (when (equal? dir "..")
                (set! dir (path-directory (fossils-directory))))
              (when (and (file-exists? dir)
                         (eq? (file-type dir) 'directory))
                (set! val dir) ;; beware intensional side effect while speculative!
                (ot0-global-context-set! 0 dir #t)
                dial-dialog!)))
            ignore-hidden: #f
            filter-pred:
            (lambda (x)
              (cond
               ((or (equal? x ".") (equal? x "..")))
               (else (and (file-exists? x) (eq? (file-type x) 'directory)))))
            done: done))))
      (unless (null? (chat-messages))
        (if (stm-atomic?) ;; not speculative
            (chat-ctrl! load: (chat-messages))
            (and
             (%%guide-critical-call (lambda () (chat-ctrl! load: (chat-messages))))
             #t)))
      (let* ((when-wired
              (lambda () (chat-ctrl! load: (chat-messages))))
             (toggle! (wire! chat-messages
                             switchable: #t
                             ;; boxed -> starts non-speculative
                             post: (box when-wired)))
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

;; About

(define (beaverchat-about-payload interval)
  (let*
      ((xsw (mdvector-interval-lower-bound interval 0))
       (ysw (mdvector-interval-lower-bound interval 1))
       (xno (mdvector-interval-upper-bound interval 0))
       (yno (mdvector-interval-upper-bound interval 1))
       (w (- xno xsw))
       (h (- yno ysw))
       (line-height 16)
       (line-height-selectable 60)
       (b1active #t)
       (b1
        (let* ((x (/ w 4))
               (y (/ h 4)))
          (guide-button
           in: (make-x0y0x1y1-interval/coerce x y (+ x (/ w 2)) (+ y (/ h 2)))
           label: "(C) JFW"
           guide-callback: (lambda (rect payload event x y) (set! b1active #f) #t))))
       (b2 (let* ((wb (/ w 5))
                  (x (- w wb))
                  (y (* h 0))
                  (h (/ h 15)))
             (guide-button
              in: (make-x0y0x1y1-interval/coerce x y (+ x wb) (+ y h))
              label: "exit"
              guide-callback:
              (lambda (rect payload event x y) (terminate)))))
       (conv (lambda (v)
               (case v
                 ((#f) "no")
                 ((#t) "yes")
                 (else
                  (cond
                   ((and (number? v) (exact? v)) (beaver-unit-id->unicode-vector v))
                   ((string? v) v)
                   (else (object->string v)))))))
       (info
        (let* ((rng (range '#(1 3)))
               (yoff 60)
               (border-ratio 1/10)
               (area (make-x0y0x1y1-interval/coerce
                      xsw (- h yoff (* 5 (+ 1 border-ratio) line-height))
                      xno (- h yoff)))
               (constructors
                (let ((val1 (lambda (p a) p)))
                  (vector
                   (lambda (in col row)
                     (guide-valuelabel
                      in: in label: "Address"
                      value: chat-own-address
                      value-display: conv))
                   (lambda (in col row)
                     (guide-valuelabel in: in label: "Version" value: system-appversion))
                   #;(lambda (in col row)
                     (let* ((last (memoize-last conv eqv?))
                            (check (lambda () (last (kick-style)))))
                       (guide-valuelabel in: in label: "kick-style" value: check success: val1)))
                   (lambda (in col row)
                     (let* ((last (memoize-last conv eq?))
                            (check (lambda () (last (ot0-online)))))
                       (guide-valuelabel in: in label: "vpn" value: check success: val1)))
                   ))))
          (make-guide-table (make-mdvector rng constructors) in: area border-ratio: border-ratio)))
       (edit-active #f)
       (dialog-set!
        (lambda (x)
          (guide-focus x)
          (set! edit-active x)
          #t))
       (edits
        (let* ((base (mdvector-interval-lower-bound (guide-payload-measures info) 1))
               (border-ratio 1/20)
               (rng (range '#(1 5)))
               (area (make-x0y0x1y1-interval/coerce xsw (- base (* 3 line-height-selectable)) xno base))
               (label-width 6/10)
               (constructors
                (let ((val1 (lambda (p a) p)))
                  (vector
                   (lambda (in col row)
                     (let* ((source beaver-proxy-port-number)
                            (last (memoize-last object->string eqv?))
                            (check (lambda () (last (source))))
                            (label "proxy port"))
                       (guide-valuelabel
                        in: in size: 'medium label-width: label-width
                        label: label value: check success: val1
                        input:
                        (lambda (rect payload event x y)
                          (dialog-set!
                           (beaverchat-port-edit
                            in: interval label: label size: size
                            input: check
                            output: source
                            success: (lambda _ (dialog-set! #f))))))))
                   (lambda (in col row)
                     (let* ((source beaver-socks-port-number)
                            (last (memoize-last object->string eqv?))
                            (check (lambda () (last (source))))
                            (label "socks port"))
                       (guide-valuelabel
                        in: in size: 'medium label-width: label-width
                        label: label value: check success: val1
                        input:
                        (lambda (rect payload event x y)
                          (dialog-set!
                           (beaverchat-port-edit
                            in: interval label: label size: size
                            input: check
                            output: source
                            success: (lambda _ (dialog-set! #f))))))))
                   (lambda (in col row)
                     (let ((label "forward")
                           (source beaver-socks-forward-addr))
                       (guide-valuelabel
                        in: in size: 'medium label-width: label-width
                        label: label
                        value: source
                        value-display:
                        (lambda (v)
                          (cond
                           ((not v) '#())
                           ((string? v) v)
                           (else (object->string v))))
                        input:
                        (lambda (rect payload event x y)
                          (dialog-set!
                           (beaverchat-service-address-edit
                            in: interval label: label size: size
                            input: source
                            success: (lambda _ (dialog-set! #f))))))))
                   (lambda (area col row)
                     (guide-button
                      in: area
                      label: "Browse Homepage (needs proxy)"
                      font: (guide-select-font size: 'small)
                      background-color: (guide-select-color-3)
                      background: (guide-background button: in: area)
                      guide-callback:
                      (lambda (rect payload event x y)
                        (cond
                         ((positive? (beaver-proxy-port-number))
                          (let ((url (string-append "http://127.0.0.1:" (number->string (beaver-proxy-port-number))))
                                (via (if (< x (/ w 2)) 'webview 'extern)))
                            (guide-critical-add!
                             (lambda () (webview-launch! url via: via))
                             async: #t)
                            #t))
                         (else #t)))))
                   (lambda (in col row)
                     (let* ((last (memoize-last (lambda (v) (number->string (length v))) eq?))
                            (check (lambda () (last (chat-pending-messages)))))
                       (guide-valuelabel
                        in: in size: 'medium label: "Pending" value: check success: val1
                        input: (lambda (rect payload event x y)
                                 (chat-pending-messages '())
                                 #t))))
                   ))))
          (make-guide-table (make-mdvector rng constructors) in: area border-ratio: border-ratio)))
       (inbox
        (let* ((wb (* w 2/3))
               (x (+ xsw 15))
               (y (* h 0))
               (h (/ h 15))
               (in (make-x0y0x1y1-interval/coerce x y (+ x wb) (+ y h)))
               (last (memoize-last (lambda (v) (number->string (length v))) eq?))
               (check (lambda () (last (chat-inbox-senders)))))
          (guide-valuelabel
           in: in label: "Ungesehen:" value: check
           input:
           (lambda _
             (let ((all (chat-inbox-senders)))
               (or (null? all)
                   (let* ((data (lambda _ (map chat-partner->neatstring all)))
                          (sel (lambda (n x)
                                 (chat-address (list-ref (chat-inbox-senders) n))
                                 (PIN:toplevel-selection 1) ;; FIXME: must match menu!
                                 (dialog-set! #f)))
                          (pl (guide-list-select-payload interval data action: sel)))
                     (dialog-set! pl))))))))
       (redraw! (vector
                 (guide-payload-on-redraw info)
                 (guide-payload-on-redraw edits)
                 (let ((d (guide-payload-on-redraw b1))) (lambda () (and b1active (d))))
                 (cond-expand
                  (android (lambda () #f))
                  (else (guide-payload-on-redraw b2)))
                 (guide-payload-on-redraw inbox)
                 (lambda () (when edit-active (guide-event-dispatch-to-payload/redraw edit-active)))))
       (events
        (lambda (rect payload event x y)
          (define (otherwise interval x y)
            (mdvector-rect-interval-contains/xy? interval x y))
          (cond
           (edit-active (guide-event-dispatch-to-payload rect edit-active event x y))
           ((or (eqv? event EVENT_BUTTON1DOWN) (eqv? event EVENT_BUTTON1UP))
            (cond
             ((and b1active (guide-payload-contains/xy? b1 x y))
              (guide-event-dispatch-to-payload rect b1 event x y))
             ((guide-payload-contains/xy? edits x y) (guide-event-dispatch-to-payload rect edits event x y))
             ((guide-payload-contains/xy? inbox x y) (guide-event-dispatch-to-payload rect inbox event x y))
             ((guide-payload-contains/xy? b2 x y)
              (cond-expand
               (android #t)
               (else (guide-event-dispatch-to-payload rect b2 event x y))))
             (else (otherwise interval x y))))
           ((or (eqv? press: event) (eqv? release: event))
            #t)
           ((guide-event-graphics? event) (otherwise interval x y))
           (else #f)))))
    (make-guide-payload
     name: 'beaverchat-about
     in: interval widget: #f on-redraw: redraw! on-any-event: events lifespan: 'ephemeral)))

;; (log-status "initializing chat completed")
;; eof
