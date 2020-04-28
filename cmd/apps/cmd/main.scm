(define-macro (XXX-lwip-initial) #t)

(define-macro (define-cond-expand-feature-value v)
  (let ((v (eval v)))
    `(define-cond-expand-feature ,v)))


;#|;;;* FEATURE SWITCH lwip-requires-pthread-locks
(define-cond-expand-feature lwip-requires-pthread-locks)
;;;|#

(cond-expand
 (lwip-requires-pthread-locks
  (when
   (eqv? lwip-NO_SYS 0)
   (debug 'lwip-requires-pthread-locks lwip-NO_SYS)
   (exit 1)))
 (else
  (unless
   (eqv? lwip-NO_SYS 1)
   (debug 'lwip-requires-check-timeouts lwip-NO_SYS)
   (exit 1))))

;;;* Notational Conventions
;;;** Consistency :Notational Conventions:
;; ($kick-style 'sync)

(include "observable-notational-conventions.scm")

(define-macro (mustbe-async-when-lwip-requires-pthread-locks expr)
    `(thread-start! (make-thread (lambda () ,expr #;(debug ',expr ,expr)) ',expr)))

(define-macro (maybe-async-when-lwip-requires-pthread-locks expr) `(mustbe-async-when-lwip-requires-pthread-locks ,expr))
#;(define-macro (maybe-async-when-lwip-requires-pthread-locks expr) expr)

(define-macro (begin-after-return! expr)
  ;; Schedule EXPR for execution (one way or another) and return nonsense.
  `(begin (mustbe-async-when-lwip-requires-pthread-locks ,expr) #!void))

;;;** Values :Notational Conventions:

(define (or-false pred?) (lambda (x) (or (eq? x #f) (pred? x))))

(define (ground) #f)
(define (ground? x) (eq? x (ground)))
(define (or-ground pred?) (lambda (x) (or (eq? x #f) (pred? x))))

(define (async! thunk)
  (thread-start! (make-thread thunk 'async)))

(define (async thunk) (lambda () (async! thunk)))

;;;** Utilitarian Garbage :Notational Conventions:

(define (sa->u8 caller sa)
  (cond
   ((internet-socket-address? sa) (socket-address->internet-address sa))
   ((internet6-socket-address? sa)
    (receive
      (host port flowinfo scope-id) (socket-address->internet6-address sa)
      (values host port)))
   (else (error "not-a-valid-socket-address" (debug 'EXIT caller) sa))))

(define (is-ip4-local? ip)
  (and (eqv? (u8vector-length ip) 4)
       (eqv? (u8vector-ref ip 0) 192)
       (eqv? (u8vector-ref ip 1) 168)
       (eqv? (u8vector-ref ip 2) 43)))

;;;* Test Environment

(define difaddr "192.168.43.96")
(define eifaddr "192.168.43.86")

;;(define dmeine (internet-address->socket-address '#u8(192 168 43 95) 0))
;;(define emeine (internet-address->socket-address '#u8(192 168 43 86) 0))

;;(define dmeine '#u8(192 168 43 95))
;;(define emeine '#u8(192 168 43 86))

(define dmeine ip-address/broadcast)
(define emeine ip-address/broadcast)

;; (zt-add-local-interface-address! dmeine)

;; (zt-node-status)

(define testers
  '((Dave #x6f318c4783)
    (Earline #xf3de84af82)))

(define (tester-ndid t) (cadr (assq t testers)))

(define (other-party)
  (case (here)
    ((Dave) 'Earline)
    ((Earline) 'Dave)
    (else #f)))

(define (earline-server) #xf3de84af82)


;; FIXME: ZT is not reentrant!  When a second thread tries to
;; zt-contact-peer the whole thing dies.

(define (contact-whom?)
  (zt-contact-peer
   "c72fe7ef99:0:d5986544158efb7c668a53c1a9d42c37f93cb8ad71be49b22cd54e1bf3cf533681ed86b18b36e908d31983db126e45f439d9bb2a975015a2d98757f12d9dfc1d"
   (internet-address->socket-address '#u8(192 168 43 86) 9993)))

(define (contact-dave)
  (zt-contact-peer
   "6f318c4783:0:5d46ec5136b6a184c39d7c34acc749d495921baa944cc6ef945d843a588ca601b40b09b8849e404b2506c26aa654364f1ecc8562d3ec4f924efc78705f29f6ea"
   (internet-address->socket-address '#u8(192 168 43 96) 9994)))

(define (contact-earline)
  (debug 'contact 'earl)
  (debug 'contacted (zt-contact-peer
   "f3de84af82:0:d6eea42c09e3f91845dd8fdf9e94c048ea5d63bde1108e8369a7cb79a98ca76951c6b80b80c9d596e0d32d83d94ee73de724412a10ae1cda21888b1206bb1461"
   (internet-address->socket-address '#u8(192 168 43 86) 9994))))

(define (contact-dave-vm)
  (zt-contact-peer
   "183ae34a4c:0:fb013629f7ad7c47b10356ab06e29f0244b8e3ddf47dac9fb6392c04d9b26d0c307c05fae6162d97b2c338f4bcfe7abe397c0fcbba6f806f78f11992a7230442"
   (internet-address->socket-address '#u8(192 168 43 96) 9993)))

;; (zt-peers-info)

;; (zt-send! #x183ae34a4c 1 (object->u8vector "abcde"))
;; (zt-send! #xc72fe7ef99 1 (object->u8vector "abcde"))

;; earline: (zt-send! #xf3de84af82 1 (object->u8vector "abcde"))

;; (zt-orbit #x183ae34a4c)
;; (zt-orbit #xc72fe7ef99)
;; (number->string (zt-address) 16)

;; (zt-peers-info)

;; (.enable-external #t)

(define (adhoc-port) 7443)

(define nng-nw-o #xa09acf02337b057b)
(define nng-nw-c #x17d709436ce162a3)
(define adhoc-nw (zt-adhoc-network-id (adhoc-port)))
(define (nws x)
  (case x
    ((0) (ground))
    ((1) adhoc-nw)
    ((2) nng-nw-o)
    ((3) nng-nw-c)
    (else x)))

(define (eth-ip6-header from to ethertype)
  (let ((dst (zt-network+node->mac (ctnw) to))
        (src (zt-network+node->mac (ctnw) from))
        (p (make-u8vector SIZEOF_ETH_HDR)))
    (u8vector/n48h-set! p 0 dst)
    (u8vector/n48h-set! p 6 src)
    (u8vector/n16h-set! p 12 ethertype)
    p))

(define (icmp6-echo-request from to)
  ;; dave to earline: (icmp6-echo-request #x6f318c4783 #xf3de84af82)
  (let ((p (make-u8vector (+ SIZEOF_IP6_HDR SIZEOF_ICMP6_HDR))))
    (let ((o 0)
          (ip6src (make-6plane-addr (ctnw) from (adhoc-port)))
          (ip6dst (make-6plane-addr (ctnw) to (adhoc-port))))
      (u8vector-set! p (+ o 0) #x60)    ;; IPv6 class 0 upper bit
      ;; left as zero
      (u8vector/n16h-set! p (+ o 4) SIZEOF_ICMP6_HDR) ;; Payload
      (u8vector-set! p (+ o 6) 58)      ;; IPv6-ICMP
      (u8vector-set! p (+ o 7) 255)     ;; hop limit
      (receive
       (addr port) (sa->u8 'icmp-echo-request ip6src)
       (subu8vector-move! addr 0 (u8vector-length addr) p (+ o 8)))
      (receive
       (addr port) (sa->u8 'icmp-echo-request ip6dst)
       (subu8vector-move! addr 0 (u8vector-length addr) p (+ o 24))))
    (let ((o SIZEOF_IP6_HDR))
      ;; ICMP6 Ping
      (u8vector-set! p (+ o 0) 128) ;; type EREQ
      )
    p))

(define-SENSOR tnw
  (SENSOR
   initial: (ground)
   pred: (or-ground integer?)
   filter: (lambda (o n) (nws n))
   name: 'tnw))

(.tnw 1)

(define (ctnw) (tnw))

;;; lwIP

(define-SENSOR lwip
  (SENSOR
   initial: #f
   pred: boolean?
   filter:
   (lambda (o n)
     (if (and #t o)
         (error "lwIP initialization can not be undone")
         n))
   name: "lwIP enabled"))

(.lwip (XXX-lwip-initial)) ;; do not get cought in the on-time-check

#|
(wire!
 ;; NOTE: This could be (more easily) achieved using a filter against
 ;; the old value.  But it's a nice example when it comes to multiple
 ;; input values
 (list lwip #| other dependencies go here |# )
 check-values: ;;; still aborts the transaction
 (let ((would-disable-lwip-if-that-where-possible (dynamic lwip)))
   (lambda (enable)
     (if (and (not enable) (would-disable-lwip-if-that-where-possible))
         (error "would-disable-lwip-if-that-where-possible")))))
;|#

(define-SENSOR zt-started (SENSOR))

(wire!
 (list lwip zt-started)
 post:
 (lambda ()
   (if (and (lwip) (zt-started))
       (.*nwif* (make-zt-ad-hoc-network-interface (ctnw) (zt-address))))))

(wire!
 zt-started
 post:
 (let ()
   (define (iam nwid port)
     (let* ((ndid (zt-address))
            (mac (zt-network+node->mac nwid ndid)))
       (receive
        (ip6-addr port flowinfo scope-id) (socket-address->internet6-address (make-6plane-addr nwid ndid port))
        (string-append
         "I am: \npublic:\n "
         (zt-node-status)
         "\n #x" (hexstr ndid 10) " in nw #x" (hexstr nwid 16) " mac " (hexstr mac 12)
         " at addr " (ip-address->string ip6-addr) " port " (number->string port)
         "\n"))))
   (lambda ()
     (s)
     (display (iam (ctnw) (adhoc-port)) (current-error-port)))))

(define .external-enabled
  (SENSOR
   initial: #f
   filter: (lambda (o n) n)
   name: 'external-enabled))
(define .ea
  .external-enabled) ;; shortcut
(define external-enabled (.external-enabled))

(define should-use-external (PIN))

(define use-external (PIN))

(wire!
 (list external-enabled should-use-external)
 post: (lambda () (use-external (and (external-enabled) (should-use-external)))))

(define .here
  (SENSOR
   initial: #f
   pred: (or-ground (lambda (x) (assq x testers)))))
(define here (.here))

(define (hexstr id digits)
  (let ((effective (number->string id 16)))
    (string-append (make-string (- digits (string-length effective)) #\0) effective)))

(define (dbgmac l v)
  (debug l (hexstr v 12))
  v)

(define-SENSOR *nwif*
  (SENSOR
   initial: (ground)
   pred: (or-ground netif?)
   filter: #f
   name: '*nwif-example*))

(define (find-nif mac)
  (let ((nif (*nwif*)))
    (and nif (eqv? (lwip-netif-mac nif) mac) nif)))

(define (make-zt-ad-hoc-network-interface nwid ndid)
  (let ((nif (lwip-make-netif (lwip-mac:host->network (zt-network+node->mac nwid ndid)))))
    (dbgmac 'MAC0 (zt-network+node->mac nwid ndid))
    ;; WRONG (debug 'lwip-mac-integer->string//ll (lwip-mac-integer->string (lwip-htonll (zt-network+node->mac nwid ndid))))
    (debug 'lwip-mac-integer->string (lwip-mac-integer->string (lwip-mac:host->network (zt-network+node->mac nwid ndid))))
    (debug 'lwip-mac/host-integer->string (lwip-mac-integer->string (zt-network+node->mac nwid ndid)))
    (debug 'lwip-netif-mac->string (lwip-netif-mac->string nif))
    #;(let ((addr (make-6plane-addr nwid ndid (adhoc-port))))
      (lwip_init_interface_IPv6 nif addr)
      (zt-multicast-subscribe nwid (dbgmac 'MCMAC (gamsock-socket-address->nd6-multicast-mac addr))))
    (begin
      (lwip_init_interface_IPv6 nif (make-6plane-addr nwid ndid (adhoc-port)))
      ;; this goes south under valgrind (only)
      (do ((n (- (debug 'NMacs (lwip-netif-ip6addr-count nif)) 1) (- n 1)))
          ((= n -1))
        (zt-multicast-subscribe nwid (dbgmac 'MCMAC (lwip-netif-ip6broadcast-mach nif n)))))
    nif))

(define (eth-send! u8vec)
  (define netif (*nwif*))
  (unless netif (error "no network"))
  (let* ((len (u8vector-length u8vec))
         (pbuf (or (make-pbuf-raw+ram len) (error "pbuf allocation failed"))))
    (pbuf-copy-from-u8vector! pbuf 0 u8vec 0 len)
    (debug 'eth-send!passing-to 'lwip)
    ;; copy it back once more until this works
    (display-eth-packet/offset (pbuf->u8vector pbuf) 0 (current-error-port))
    (lwip-send-ethernet-input/pbuf! netif pbuf)))

(define (etx-send! src dst u8vec)
  ;; Dave to Earline: (etx-send! (icmp6-echo-request #x6f318c4783 #xf3de84af82))
  (display-ip6-packet/offset u8vec 0 (current-error-port))
  (zt-virtual-send (ctnw) src dst ETHTYPE_IPV6 0 u8vec))

(define (etx-ping! to)
  ;; Dave to Earline: (etx-ping! #xf3de84af82)
  ;; Earline to Dave: (etx-ping! #x6f318c4783)
  (let* ((from (cadr (assq (here) testers)))
         (dst (zt-network+node->mac (ctnw) to))
         (src (zt-network+node->mac (ctnw) from)))
    (etx-send! src dst (icmp6-echo-request from to))))

(define (find-nwid-for-nif nif) (ctnw))

(define (ds)
  (.here 'Dave)
  (zt-start! "/home/u/build/ball/ball/zerotier-server" 9994 background-period: 0.5)
  (zt-add-local-interface-address! (internet-address->socket-address dmeine 0 #;9994))
  (zt-join (ctnw))
  (.zt-started #t))

(define (dc)
  (.here 'DaveC)
  (zt-start! "/home/u/build/ball/ball/zerotier-client" 9995 background-period: 0.5)
  (zt-add-local-interface-address! (internet-address->socket-address dmeine 9995))
  (zt-join (ctnw))
  (.zt-started #t))

(define (es)
  (.here 'Earline)
  (zt-start! "/home/u/zerotier-server" 9994 background-period: 0.5)
  (zt-add-local-interface-address! (internet-address->socket-address emeine 0 #;9994))
  (zt-join (ctnw))
  (.zt-started #t))

(define (ec)
  (zt-start! "/home/u/zerotier-client" 9995 background-period: 0.5)
  (zt-add-local-interface-address! (internet-address->socket-address emeine 9995))
  (zt-join (ctnw))
  (.zt-started #t))

(define (zt-start! path port #!key (background-period 0.5)) ;; EXPORT
  (if (zt-up?) (error "zt-start!: already running"))
  (receive
   (get put) (zt-make-default-state-handlers (zt-state-file-generator 0 path))
   (zt-state-get get)
   (zt-state-put put))
  (if (let ((udp (let* ((addr (internet-address->socket-address ip-address/any port))
                        (sock (create-socket address-family/internet socket-type/datagram)))
                   (bind-socket sock addr)
                   sock)))
        (zt-node-init! udp background-period: background-period))
      (begin
        (kick!
         (lambda ()
           (zt-up (debug 'zt-up #t))
           (should-use-external #t)))
        ;; maybe manually orbit?
        (for-each zt-orbit '(#x183ae34a4c #xc72fe7ef99))
        #t)
      #f))

;;* Locking

(define (zt-locks-no) ;; should die in zt_contact_peer
  (zt-locking-set! (lambda () #f) (lambda () #f)))

;; (zt-locks-no)

;;* EVENTS

(zt-recv
 (lambda (from type data)
   (case type
     ((2) ;; tcp test should connect back now
      (debug 'RECV-from:test (list from type))
      (lwc0 from))
     (else
      (debug 'RECV-from-type-data (list from type data))
      #f))))

#;(define-sense
  zt-online
  #f boolean?
  (lambda (o n) (if (boolean? n) n (eq? 'ONLINE n)))
  'zt-online)

(define zt-online
  (PIN
   initial: #f
   pred: boolean?
   filter: (lambda (o n) (if (boolean? n) n (eq? 'ONLINE n)))
   name: 'zt-online))

(define (.zt-online x) (kick! (lambda () (zt-online x))))

(define zt-up (PIN))

(zt-event
 (lambda (node userptr thr event payload)
   (case event
     ((UP) ;; UP comes BEFORE the initialization is completed! Don't use it.
      (debug 'ZT-EVENT event) )
     ((ONLINE OFFLINE)
      (debug 'ZT-EVENT event)
      (kick!
       (lambda ()
         #;(should-use-external #f)
         (zt-online event))))
     ((TRACE)
      (debug 'ZT-TRACE ((c-lambda ((pointer void)) char-string "___return(___arg1);") payload))
      (debug 'ZT-CFG (zt-query-network-config-base->vector (ctnw))))
     (else (debug 'ZT-EVENT event)))))

#;(zt-wire-packet-send
 (lambda (udp socket remaddr data ttl)
   ;;(debug 'wire-send-via socket)
   (cond
    ((internet-socket-address? remaddr)
     (receive
      (ip-addr port) (socket-address->internet-address remaddr)
      (udp-destination-set! ip-addr port udp)
      (udp-write-u8vector data udp)
      #t))
    ((and #f (internet6-socket-address? remaddr)) ;; FIXME: Why does udp-destination-set! fail here?
     (receive
      (host port flowinfo scope-id) (socket-address->internet6-address remaddr)
      (udp-destination-set! host port udp))
     (udp-write-u8vector data udp)
     #t)
    (else #f))))

(zt-wire-packet-send
 (lambda (udp socket remaddr data len ttl)
   (thread-yield!) ;; KILLER!
   ;; (debug 'wire-send-via socket)
   ;; FIXME: allocate IPv6 too!
   (let ((remaddr (and remaddr (zt->gamsock-socket-address remaddr))))
     (if (internet-socket-address? remaddr)
         (receive
          (addr port) (sa->u8 'zt-wire-packet-send remaddr)
          (thread-yield!) ;; KILLER!
          (##gc)
          (cond
           ((or (use-external) (is-ip4-local? addr))
            (debug 'wire-send-via (socket-address->string remaddr))
            ;;  FIXME: need to copy remaddr too? -- seems not be be the culprit
            (let ((u8 (make-u8vector len))
                  (remaddr (internet-address->socket-address addr port))
                  #;(remaddr
                   (receive
                    (addr port) (sa->u8 'zt-wire-packet-send2 remaddr)
                  (internet-address->socket-address addr port))))
              (unless (eqv? (lwip-gambit-locked?) 1) (error "locking issue"))
              ;; This COULD happen to copy from another threads stack!
              (u8vector-copy-from-ptr! u8 0 data 0 len)
              (thread-yield!) ;; KILLER!
              #;(debug 'remaddr-is-still-ipv4? (internet-socket-address? remaddr))
              #;(debug 'wire-send-via (socket-address->string remaddr))
              #;(eqv? (send-message udp u8 0 #f 0 remaddr) len)
              (maybe-async-when-lwip-requires-pthread-locks (send-message udp u8 0 #f 0 remaddr))
              #t))
           (else #;(debug 'wire-send-via/blocked (socket-address->string remaddr)) #f)))))))

(zt-virtual-receive
 ;; API issue: looks like zerotier may just have disassembled a memory
 ;; segment which we now must copy bytewise.  If that's the case we
 ;; better had an interface to pass the underlying pointer.  NOTE:
 ;; It's (currently) important that LWIP_TCPIP_CORE_LOCKING_INPUT is
 ;; not set.
 (lambda (node userptr thr nwid netptr srcmac dstmac ethertype vlanid payload len)
   (define ethtp (ethertype/host-decor ethertype))
   (dbgmac 'VRECV-SRCMAC srcmac)
   (dbgmac 'VRECV-DSTMAC dstmac)
   (debug 'VRECV-ethertype ethtp)
   (debug 'VRECV len)
   (debug 'netptr netptr)
   (if (eq? ethtp 'ETHTYPE_IPV6)
       (let ((u8p (make-u8vector len)))
         (u8vector-copy-from-ptr! u8p 0 payload 0 len)
         (display-ip6-packet/offset u8p 0 (current-error-port))))
   (let ((nif (find-nif dstmac)))
     (if nif
         (lwip-send-ethernet-input! nif (lwip-mac:host->network srcmac) (lwip-mac:host->network dstmac) ethertype payload len)
         (begin
           (debug 'DROP:VRECV-DSTMAC dstmac))))))

(define (ethertype/host-decor etht)
  (cond
   ((<= etht 1500) (list 'length etht))
   ((and (> etht 1500) (< etht 1536)) (list 'illegal etht))
   (else
    (let ((x (ethertype/host->symbol etht)))
      (if (eq? x 'lwip-UNKNOWN) (list x etht (ethertype/network->symbol etht)) x)))))

(lwip-nd6-get-gateway (lambda (netif dst) dst)) ;; ZT serves a single switch

(lwip-ethernet-send
 (lambda (netif src dst ethertype #;0 pbuf)
   (thread-yield!) ;; KILLER? - No, Yes.
   (let ((src (lwip-mac:network->host src))
         (dst (lwip-mac:network->host dst))
         (ethertype ethertype))
     (define ethtp (ethertype/host-decor ethertype))
     (debug 'lwip-ethernet-send-to-zt pbuf)
     (dbgmac 'src src)
     (dbgmac 'dst dst)
     (debug 'EtherType ethtp)
     #;(display-eth-packet/offset pbuf 0 (current-error-port))
     (let ((vlanid 0)
           (bp (pbuf->u8vector pbuf SIZEOF_ETH_HDR))) ;; FIXME: avoid the copy
       (debug 'Packt-len (u8vector-length bp))
       (cond
        ((eq? ethtp 'ETHTYPE_IPV6) (display-ip6-packet/offset bp 0 (current-error-port))))
       (maybe-async-when-lwip-requires-pthread-locks (debug 'DONE:zt-virtual-send (zt-virtual-send (find-nwid-for-nif netif) src dst ethertype vlanid bp)))
       (debug 'lwip-ethernet-send 'return-ok)
       ERR_OK))))

(lwip-ip6-send
 (lambda (netif pbuf ip6addr)
   (let ((addr (make-u8vector 16)))
     (u8vector-copy-from-ptr! addr 0 ip6addr 0 16)
     (if (eqv? (u8vector-ref addr 0) #xfc)
         (let ((ndid (quotient (%u8vector/n48h-ref addr 5) 256))
               (nwid (find-nwid-for-nif netif))
               (src (lwip-netif-mac netif))
               (bp (pbuf->u8vector pbuf 0)))
           (debug 'lwip-ip6-send-to (hexstr ndid 10))
           (display-ip6-packet/offset bp 0 (current-error-port))
           (mustbe-async-when-lwip-requires-pthread-locks (debug 'DONE:zt-vsend (zt-virtual-send nwid src (zt-network+node->mac nwid ndid) ETHTYPE_IPV6 0 bp)))
           ERR_OK)
         ERR_RTE))))

;; Config

(define config-helper
  (thread-start!
   (make-thread
    (lambda ()
      (let loop ()
        (thread-receive)
        (zt-set-config-item! (ctnw) 2 16)
        (loop))))))

(zt-virtual-config
 (lambda (node userptr nwid netptr op config)
   #;(thread-yield!) ;; KILLER? - No
   (debug 'CONFIG op)
   (debug 'CFG (zt-virtual-config-base->vector config))
   ;; set multicast limit
   ;;(thread-send config-helper #t)
   ;;(if (eqv? nwid (ctnw)) (maybe-async-when-lwip-requires-pthread-locks (debug 'set-mc-limit (zt-set-config-item! nwid 2 16))))
   #t))

;; Optional

(zt-path-check
 (lambda (node userptr thr nodeid socket sa)
   (debug 'PATHCHECK (number->string nodeid 16))
   ;; (debug 'PATHCHECK:gamit-locked? (lwip-gambit-locked?))
   (unless (eqv? (lwip-gambit-locked?) 1) (error "locking issue, zt-path-check"))
   (receive
    (addr port) (sa->u8 'zt-path-check (zt->gamsock-socket-address sa))
    (debug 'PATHCHECK (cons addr port))
    (or (use-external) (is-ip4-local? addr)))))


;; FIXME, CRAZY: Just intercepting here causes havoc under valgrind!

(zt-path-lookup
 (lambda (node uptr thr nodeid family sa)
   ;; (debug 'LOOKUP (number->string nodeid 16))
   (debug 'LOOKUP (hexstr nodeid 12))
   (debug 'LookupFamily family)
   #;(debug 'LookupSA (and sa (sa->u8 'zt-path-lookup sa)))
   #f))

(zt-maintainance
 (lambda (prm thunk)
   #; (debug 'zt-maintainance (lwip-gambit-locked?))
   thunk))

;; this will provide an interactive prompt

(define (debug l v)
  (let ((p (current-error-port)))
    (display l p)
    (display ": " p)
    (write v p)
    (newline p)
    v))

;; lwIP TCP

(define (lwip-ok? x) (eqv? x 0))

(define (on-tcp-accept ctx connection err)
  (debug 'on-tcp-accept ctx)
  ERR_ABRT)

(define (on-tcp-sent ctx connection len)
  ;; should signal conn free
  ERR_OK)

(define (on-tcp-receive ctx connection pbuf err)
  ERR_IF)

(define (on-tcp-connect ctx connection err)
  (case err
    ((0)
     (let ()
       (debug 'CONNECTED connection)
       (ctx connection)
       ERR_OK))
    (else (debug 'on-tcp-connect err)))) ;; FIXME

(define (on-tcp-poll ctx connection)
  (debug 'UpsPOLL connection)
  (thread-yield!)
  (thread-yield!)
  ;; (debug 'lwip-tcp-flush! (lwip-err (lwip-tcp-flush! connection)))
  ERR_OK)

(define (on-tcp-error ctx err)
  (debug 'ERROR ctx)
  (debug 'err (lwip-err err))
  ERR_OK)

(on-tcp-event
 (lambda (ctx pcb e pbuf size err)
   ;; NOTE: This passes along what the lwIP callback API did.
   (cond
    ((eq? LWIP_EVENT_ACCEPT e) (on-tcp-accept ctx pcb err))
    ((eq? LWIP_EVENT_SENT e) (on-tcp-sent ctx pcb len))
    ((eq? LWIP_EVENT_RECV e) (on-tcp-receive ctx pcb pbuf err))
    ((eq? LWIP_EVENT_CONNECTED e) (on-tcp-connect ctx pcb err))
    ((eq? LWIP_EVENT_POLL e) (on-tcp-poll ctx pcb))
    ((eq? LWIP_EVENT_ERR e) (on-tcp-error ctx err))
    (else (debug "on-tcp-event: unknown event" e) ERR_ARG))))

(define-sense connecting #t) ;; enabled

(define-sense lws-contact-is-listening)

(define (lwc0 from) (.lws-contact-is-listening from))

(wire!
 (list *nwif* connecting lws-contact-is-listening)
 post: (lambda () (if (and (*nwif*) (connecting) (lws-contact-is-listening)) (lwc))))

(define (lwc)
  (let* ((pcb (tcp-new-ip-type lwip-IPADDR_TYPE_V6))
         (sa (make-6plane-addr (ctnw) (lws-contact-is-listening) #;(tester-ndid (other-party)) (adhoc-port)))
         (addr (receive (a p) (sa->u8 'lwc sa) a)))
    (debug "






"  'PLATZ)
     (let ((client (tcp-new6)))
        (debug 'client-bound (lwip-tcp-bind/netif client (*nwif*)))
        (tcp-context-set!
         client
         (lambda (connection)
           (debug 'ClientConnected connection)
           (lwip-tcp-close connection)))
        (unless (lwip-ok? (debug 'conn (lwip-tcp-connect client addr (debug 'connecting (adhoc-port)))))
                (error "lwip-tcp-connect failed"))
        (unless (lwip-ok? (lwip-tcp-flush! (debug 'lwip-tcp-flush! client)))
                (error "lwip-tcp-flush! failed"))
        (debug 'lwc 'waiting-for-callback)
        (thread-sleep! 30)
        #;(lambda () (debug 'lws 'post-post) #f))))

(define lws-listener (PIN))

(define (lws-listening!)
  (if (lws-listener)
      (begin-after-return! (zt-send! (tester-ndid (other-party)) 2 (object->u8vector "listening")))))

(wire! lws-listener post: lws-listening!)

(define (lws)
  (let* ((srv (tcp-new-ip-type lwip-IPADDR_TYPE_V6))
         (sa (make-6plane-addr (ctnw) (zt-address) (adhoc-port)))
         (addr (if #f
                   (receive (a p) (sa->u8 'lws sa) a) ;; Bind local/loopback only
                   '#u8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
    (debug "






"  'BEDARF)
    (debug 'lwIP-bind (ip-address->string addr))
    (unless
     (lwip-ok? (lwip-tcp-bind (debug 'pcb srv) addr (adhoc-port)))
     (error "lwip-tcp-bind failed"))
    (let ((srv (debug 'listening (lwip-tcp-listen (debug 'listenon srv)))))
      (unless srv (error "lwip-listen failed"))
      (tcp-context-set! srv #f)
      (lws-listener srv))))

(define tried-to-contact (PIN #f))

(define (report-contact-attempt-later)
  (thread-sleep! 10)
  (kick! (lambda () (tried-to-contact #t))))

(define (contact-other-party)
  (case (here)
    ((Dave) (contact-earline))
    ((Earline) (contact-dave)))
  (if #t
      (async! report-contact-attempt-later)
      (tried-to-contact #t))
  #!void)

(define-SENSOR contacting (SENSOR initial: #f pred: boolean?))

(wire!
 (list here zt-online zt-started contacting)
 post:
 (lambda ()
   (if (and (contacting) (here) (or (zt-online) (zt-started)))
       (begin
         (debug 'CONTACTING (list (here) (zt-online) (zt-started)))
         (begin-after-return! (contact-other-party)))
       (tried-to-contact #f))))

(wire!
 (list *nwif* tried-to-contact) ;; list of dependencies
 post:                   ;; connect post condition
 ;; TBD DEBUG: lws here just does not cut it.
 (lambda () (if (and (*nwif*) (tried-to-contact) (not (lws-listener))) (lws))))

;;; NIXDA |#

(define (s)
  (for-each
   display
   `(,(here) " started " ,(zt-started) " up " ,(zt-up) " online " ,(zt-online)
     " nw " ,(number->string (ctnw) 16)
     ,(if (external-enabled) " external (enabled) using " " external (disabled) using ")
     ,(use-external)
     " Contacting " ,(contacting)
     " Connecting " ,(connecting)
     " Kick style " ,($kick-style)
     ,(if (= lwip-NO_SYS 1) " TCP synchroneous" " TCP in pthread")
     "\n")))

(define (system-command-line)
  (let loop ((n (system-cmdargc)) (r '()))
    (if (< n 1) r
	(let ((i (- n 1)))
	  (loop i (cons (system-cmdargv i) r))))))

(define (replloop)
  (with-exception-catcher
   (lambda (e)
     (##default-display-exception e (current-error-port))
     #;(for-each display (list (exception->string e) "\n")) #f)
   (lambda () (##repl-debug #f #t)))
  (replloop))

(replloop)

;; eof
