
(log-status "initializing chat")

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

(define-macro (define-pin name . more)
  `(define ,name (make-pin . ,more)))

;;; END INSTEAD OF (include "~~tgt/lib/onetierzero/src/observable-notational-conventions.scm")

(define (init-beaverchat! dir #!key (use-origin #f))
  (unless (file-exists? dir)
    (ot0-init-context! dir)
    (when use-origin
      (call-with-output-file (make-pathname dir "origin")
        (let ((content use-origin))
          (lambda (port)
            (write-subu8vector content 0 (u8vector-length content) port)))))
    (log-status "done. Initialized chat in "  dir)))

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

(define select-font Xglgui-font)

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
            (Xtrigger-redraw!)))))
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
          (Xtrigger-redraw!))))
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
  (let ((font (select-font height: line-height)))
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
                  (set! usable-n (min (floor (/ h element-height)) n))))
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
             (('hidden: v) (glgui-widget-set! gui wgt 'hidden (and v #t)) (Xtrigger-redraw!))
             (('set: new) (set-content new))
             (_ (begin (del) (error "Xglgui-select unhandled arguments" args))))
            (Xtrigger-redraw!)))))))

;;; These are exported as input/output:

(define-pin calculator-input
  initial: #f
  name: "Calculator input value")

(define-pin calculator-result
  initial: #f
  name: "Calculator result value")

(define-pin calculator-mem1
  initial: 0
  name: "Calculator \"Mem\" value")

(define-pin calculator-operations
  initial: '()
  name: "Calculator stack")

(define-pin calculator-main-display
  initial: #f
  name: "Calculator value in display")

;; Other pins

(define-pin audible-beep
  initial: (lambda () #f)
  pred: procedure?
  name: "hot to beep")

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
  (wire! beaver-proxy-port-number sequence: (local-server-port-change http-proxy))
  (wire! beaver-socks-port-number sequence: (local-server-port-change socks-server)))

(define string-empty?
  (let ((m (rx "^[[:space:]]*$")))
    (lambda (obj) (and (string? obj) (rx~ m obj) #t))))

(define string-chat-address->unit-id
  (let ((ignore (rx "[ .-]")))
    (lambda (str)
      (and (string? str)
           (let ((despace (rx-replace/all ignore str)))
             (phone2id (string->number despace)))))))

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

(define-pin chat-messages
  initial: '()
  pred: glgui-chat-messages-are-valid?
  name: "Chat messages")

(wire! chat-messages post: update-pages!)

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

(define (take lst n) ;; from srfi-1
  (let rec ((lst lst) (n n))
    (if (or (eqv? n 0) (null? lst)) '()
        (cons (car lst) (rec (cdr lst) (- n 1))))))
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
  (pin-filter! chat-inbox-senders (lambda (x) (not (equal? (chat-address) x)))))

(wire! chat-address post: remove-chat-address-from-inbox-senders)

(define known-tools '("calculator" "chat" "about"))

(define-pin selected-display
  initial: "about"
  filter: (lambda (o n) (if (member n known-tools) n o))
  name: "The tool to display")

(define-pin chat-own-address #f)
(wire! ot0cli-server post: (box (lambda () (thread-sleep! 5) (kick (chat-own-address (ot0-address))))))

;;(pin-attach-log! calculator-result 'calculator-result)
;;(pin-attach-log! calculator-mem1 'calculator-mem1)

(define (id2phone id)
  (and (number? id)
       (exact? id)
       (positive? id)
       (< id 1099511627776)
       (let ((n (if (even? (bit-count id)) 4000000000000 2000000000000)))
         (+ n id))))

(define (phone2id id)
  (cond
   ((not (and (number? id) (exact? id) (positive? id))) #f)
   ((> id 4000000000000)
    (let ((x (- id 4000000000000)))
      (and (even? (bit-count x)) x)))
   (else
    (let ((x (- id 2000000000000)))
      (and (positive? x) (odd? (bit-count x)) x)))))

(define (chat-number->neatstring num #!optional (gap "·"))
  (let ((str (number->string (id2phone num))))
    (string-append
     (substring str 0 3)
     gap
     (substring str 3 5)
     gap
     (substring str 5 8)
     gap
     (substring str 8 10)
     gap
     (substring str 10 13))))

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

(define (glgui-beaverchat) ;; Note: this is currently a once-at-most call!

  ;; This simple calculator app uses the gambit in-fix interpreter
  ;; It handles fractions, complex numbers, and large fixnums!

  (define gui #f)
  (define calculator-subdisplay #f)
  (define calculator-display #f)

  (define keypad
    `((
       ( (#\A "AC") (#\M "MC") #\C (,delchar ,glgui_keypad_delete.img) )
       ( (#\m "MR")  (#\p "M+") (#\q "M-") #\/)
       ( #\7 #\8 #\9 #\* )
       ( #\4 #\5 #\6 #\- )
       ( #\1 #\2 #\3 #\+ )
       ( (#\0 "0" 2.)  #\. (#\= "=" 1. ,DarkOrange))
       )))

  ;; suppress floating point inaccuracies
  (define (number->neatstring n)
    (if (not (##flonum? n)) (number->string n)
        (let* ((s (number->string (fl/ (flfloor (fl+ (fl* (flo n) 1.0e10) 0.5)) 1.0e10)))
               (sl (string-length s))
               (b (substring s 0 1))
               (e (substring s (- sl 1) sl)))
          (string-append (if (string=? b ".") "0" "")
                         (if (string=? e ".") (substring s 0 (- sl 1)) s)))))

  (define (value->neatstring n)
    (cond
     ((boolean? n) "")
     ((number? n) (number->neatstring n))
     (else (object->string n))))

  (define (calculator-updatesub)
    (let ((Ans (calculator-result))
          (Mem (calculator-mem1)))
      (glgui-widget-set!
       gui calculator-subdisplay 'label
       (string-append "In=" (value->neatstring (calculator-input)) "Mem=" (number->neatstring Mem) " Ans=" (value->neatstring Ans)  " "))
      (Xtrigger-redraw!)))

  (define (calculator-update-main-display)
    (Xtrigger-redraw!)
    (let ((Ans (calculator-main-display)))
      (glgui-widget-set! gui calculator-display 'label (if (number? Ans) (number->neatstring Ans) (if (boolean? Ans) "" (object->string Ans))))
      (glgui-widget-set! gui calculator-display 'bgcolor (if Ans #f Red))))

  (define (calculator-evaluate)
    (let ((input (string->number (glgui-widget-get gui calculator-display 'label))))
      (kick
       (calculator-result
        (let ((input (or input (and (number? (calculator-input)) (calculator-input)))))
          (and input
               (if (pair? (calculator-operations))
                   (let ((top (car (calculator-operations))))
                     (calculator-operations (cdr (calculator-operations)))
                     (with-exception-catcher
                      (lambda (exn) exn)
                      (lambda () (top input))))
                   input))))
       (calculator-input #f)
       (calculator-main-display (calculator-result)))))

  (define (calculator-C)  (kick (calculator-input #t) (calculator-main-display (calculator-input))))
  ;;(define (calculator-C) (glgui-widget-set! gui calculator-display 'label ""))
  (define (calculator-MC) (kick (calculator-mem1 0)))
  (define (calculator-AC) (kick (calculator-result #t) (calculator-C) (calculator-MC) (calculator-operations '())))

  (define (calculator-MR) (kick (calculator-input (calculator-mem1))))

  (define (calculator-M+)
    (let ((input (take-display-value)))
      (kick
       (calculator-input #f)
       (if (number? input) (kick (calculator-mem1 (+ (calculator-mem1) input)))))))
  (define (calculator-M-)
    (let ((input (take-display-value)))
      (kick
       (calculator-input #f)
       (if (number? input) (kick (calculator-mem1 (- (calculator-mem1) input)))))))

  (define (take-display-value)
    (let ((input (string->number (glgui-widget-get gui calculator-display 'label))))
      (and input
           (begin
             (glgui-widget-set! gui calculator-display 'label "")
             input))))

  (define (calculator-push-label-to-pin! pin)
    (let ((input (take-display-value)))
      (and input (kick (pin input)))))

  (define (calculator-sqrt)
    (let ((input (take-display-value)))
      (kick
       (calculator-input input)
       (calculator-result
        (cond
         ((number? (calculator-input)) (sqrt (calculator-input)))
         ((number? (calculator-result)) (sqrt (calculator-result)))
         (else #f)))
       (calculator-main-display (calculator-result)))))

  (define (calculator-push-operation op)
    (let ((input (string->number (glgui-widget-get gui calculator-display 'label))))
      (if input
          (begin
            (glgui-widget-set! gui calculator-display 'label "")
            (kick
             (calculator-result input)
             (calculator-operations (cons (lambda (x) (op (calculator-result) x)) (calculator-operations))))))))

  (define (calculator-on-event gui op event x y)
    (let ((skipevent #t))
      (if (= event EVENT_KEYRELEASE)
          (cond
           ((= x EVENT_KEYESCAPE)     (terminate))
           ((= x (char->integer #\=)) (calculator-evaluate))
           ((= x (char->integer #\+)) (calculator-push-operation +))
           ((= x (char->integer #\-)) (calculator-push-operation -))
           ((= x (char->integer #\*)) (calculator-push-operation *))
           ((= x (char->integer #\/)) (calculator-push-operation /))
           ((= x (char->integer #\A)) (calculator-AC))
           ((= x (char->integer #\C)) (calculator-C))
           ((= x (char->integer #\M)) (calculator-MC))
           ((= x (char->integer #\p)) (calculator-M+))
           ((= x (char->integer #\q)) (calculator-M-))
           ((= x (char->integer #\m)) (calculator-MR))
           ((= x (char->integer #\S)) (calculator-sqrt))
           (else (set! skipevent #f)))
          (set! skipevent #f))
      (if (not skipevent) (glgui-dispatch-event gui op event x y))))

  (define (calculator-update-display!) #t)

  (define (calculatorcontainer-init! gui w h)
    (set! calculator-subdisplay (glgui-label gui 0 (- h 20) w 20 "" (select-font size: 'small) White))
    (glgui-widget-set! gui calculator-subdisplay 'align GUI_ALIGNRIGHT)
    (set! calculator-display (glgui-label gui 5 (- h 80) (- w 10) 60 "" (select-font size: 'large) White))
    (glgui-widget-set! gui calculator-display 'align GUI_ALIGNRIGHT)
    (glgui-widget-set! gui calculator-display 'focus #t)
    (let ((wgt (glgui-keypad gui 5 5 (- w 10) (- h 80 5) (select-font size: 'medium) keypad)))
      (glgui-widget-set! gui wgt 'rounded #f)
      (glgui-widget-set! gui wgt 'floatinghighlight #f))
    (calculator-updatesub)
    (values gui calculator-update-display!))

  (define field_gradient (list (color:shuffle #xe8e9eaff) (color:shuffle #xe8e9eaff) (color:shuffle #xfefefeff) (color:shuffle #xfefefeff)))

  (define calculator-container #f)

  (define (make-about-container gui w h)
    (let ((bag (glgui-container gui 0 0 w h))
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
             (launch-url (string-append "http://127.0.0.1:" (number->string (beaver-proxy-port-number)))))
         (kick (chat-pending-messages '()))))
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
             (ctrl
              ((Xglgui-select
                bag border border (- w (* 2 border)) (- h (* 2 border))
                line-height: line-height-selectable color: White)
               '()
               (lambda (sel x)
                 (if (>= sel 0)
                     (let ((sel2 (list-ref (chat-inbox-senders) sel)))
                       (kick
                        (chat-address sel2)
                        (selected-display "chat")))))
               permanent: #t)))
        (define (new-content)
          (let ((new (map chat-partner->neatstring (chat-inbox-senders))))
            (ctrl set: new)
            (ctrl hidden: (null? new))))
        (new-content)
        (wire! chat-inbox-senders post: new-content))
      (glgui-button-string bag (/ w 4) (/ h 4) (/ w 2) (/ h 2) str fnt callback)
      bag))

  (define chat-container #f)

  (define chat-on-event #f)

  (define (make-chat-container gui w h)
    (let* ((bag (glgui-container gui 0 0 w h))
           (fnt (select-font size: 'small))
           (cllx 0)
           (clly (/ h 2))
           (cw w)
           (ch (/ h 2))
           (line-height 20)
           (line-height-selectable 28)
           (text-keypad keypad:simplified)
           (dial-keypad keypad:numeric)
           (keypad (glgui-keypad bag 0 0 w (- clly line-height line-height) fnt)))
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
                               (if (> x 2/3)
                                   (begin
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
                                   (kick (chat-address (car e))))
                               (ask-for-nick (car e))))
                         (when ponebook-dialog (ponebook-dialog 'close) (set! ponebook-dialog #f))))))))
        (define (close-chat!)
          (when ponebook-dialog (ponebook-dialog 'close) (set! ponebook-dialog #f))
          (kick (chat-address #f)
                (selected-display (list-ref known-tools 0))))
        (define (on-event gui op event x y)
          (define skip #f)
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
                 (nick-dialog (glgui-dispatch-event gui op event x y))
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
          (or skip (glgui-dispatch-event gui op event x y)))
        (set! chat-on-event on-event)
        (glgui-widget-set! gui input 'focus #t)
        (glgui-widget-set! gui input 'enableinput #t)
;;        (glgui-widget-set! gui input 'draw-handle Xglgui:label-draw2)
        (wire! chat-messages post: follow-input)
        (wire! chat-address post: update-keypad)
        (wire! chat-address post: follow-to)
        bag)))

  (define about-container #f)

  (define tool-switcher #f)

  (define glgui-event-dispatcher
    (let ((current glgui-dispatch-event))
      (case-lambda
       ((gui op event x y) (current gui op event x y))
       (() current)
       ((val) (set! current val)))))

  (define glgui-dispatch-event-default glgui-dispatch-event)

  (define (switch-event-dispatcher)
    (match
     (selected-display)
     ("calculator" (glgui-event-dispatcher calculator-on-event))
     ("chat" (glgui-event-dispatcher chat-on-event))
     (_ (glgui-event-dispatcher glgui-dispatch-event-default))))

  (define switch-selected-tool
    (let ((current #f))
      (lambda ()
        (let* ((to (selected-display))
               (new (cond
                     ((equal? to "calculator") calculator-container)
                     ((equal? to "chat") chat-container)
                     ((equal? to "about") about-container)
                     (else #f))))
          (when new
            (when tool-switcher
              (glgui-widget-set!
               gui tool-switcher
               'current (- (length known-tools) (length (member to known-tools)))))
            (when (and current (not (eq? current new)))
              (glgui-widget-set! gui current 'hidden #t))
            (when (not (eq? current new))
              (set! current new)
              (glgui-widget-set! gui current 'hidden #f))
            (Xtrigger-redraw!))))))

  (define (cb-tool-selection-change gui wgt type x y)
    (kick (selected-display (list-ref known-tools (glgui-widget-get gui wgt 'current)))))

  (define (tool-selection-draw-choice font)
    (lambda (str)
      (lambda (lg lw x y w h s)
        (if s (glgui:draw-box x y w h Black))
        (glgui:draw-text-left (+ x 5) y (- w 10) h str font White))))

  (define (example-init! w h)
    (make-window 320 502 #;480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))
    (let* ((w (glgui-width-get))
           (h (glgui-height-get))
           (mh 28)
           (menu-font (select-font size: 'medium))
           (k 'dd))
      (set! calculator-container (glgui-container gui 0 0 w (- h mh)))
      (when (> mh 0)
        (case k
          ((men)
           (glgui-menubar gui 0 (- h (* mh 2)) w (* mh 2))
           ;; (glgui-button-string bag (/ w 4) (/ h 4) (/ w 2) (/ h 2) str fnt callback)
           (glgui-label gui 0 (- h mh) #;(- h (/ mh 2)) 80 mh "Menu" menu-font White))
          ((dd)
           (let ((dd (glgui-dropdownbox
                      gui 0 (- h mh) w mh
                      (map (tool-selection-draw-choice menu-font) known-tools)
                      Black Orange Black)))
             (glgui-widget-set! gui dd 'callback cb-tool-selection-change)
             (set! tool-switcher dd)))))
      (set! about-container (make-about-container gui w (- h mh)))
      (set! chat-container (make-chat-container gui w (- h mh)))
      (glgui-widget-set! gui about-container 'hidden #t)
      (receive (container update!) (calculatorcontainer-init! calculator-container w (- h mh))
        (kick (selected-display "calculator"))
        (values gui update!))))

  (wire! calculator-mem1 post: calculator-updatesub)
  (wire! calculator-result post: calculator-updatesub)
  (wire! calculator-input post: calculator-updatesub)
  (wire! calculator-main-display post: calculator-update-main-display)

  (wire! selected-display post: switch-selected-tool)
  (wire! selected-display post: switch-event-dispatcher)
  (wire! selected-display post: update-pages!)

  (glgui-run
   ;; initialization
   example-init!
   events: glgui-event-dispatcher
   ;;suspend: terminate
   )
  )

(log-status "initializing chat completed")
;; eof
