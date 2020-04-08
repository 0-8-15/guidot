;; LambdaNative console template

(include "observable-notational-conventions.scm")

(define (or-false pred?) (lambda (x) (or (eq? x #f) (pred? x))))

(define (ground) #f)
(define (ground? x) (eq? x (ground)))
(define (or-ground pred?) (lambda (x) (or (eq? x #f) (pred? x))))

;; utilitarian garbage

(define (sa->u8 sa)
  (cond
   ((internet-socket-address? sa) (socket-address->internet-address sa))
   ((internet6-socket-address? sa)
    (receive
      (host port flowinfo scope-id) (socket-address->internet6-address sa)
      (values host port)))
   (else 'invalid #;(raise 'not-a-valid-socket-address))))

(define (is-ip4-local? ip)
  (and (eqv? (u8vector-length ip) 4)
       (eqv? (u8vector-ref ip 0) 192)
       (eqv? (u8vector-ref ip 1) 168)
       (eqv? (u8vector-ref ip 2) 43)))

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
  '((Dave)
    (Earline)))

(define (contact-whom?)
  (zt-contact-peer
   "c72fe7ef99:0:d5986544158efb7c668a53c1a9d42c37f93cb8ad71be49b22cd54e1bf3cf533681ed86b18b36e908d31983db126e45f439d9bb2a975015a2d98757f12d9dfc1d"
   (internet-address->socket-address '#u8(192 168 43 86) 9993)))

(define (contact-dave)
  (zt-contact-peer
   "6f318c4783:0:5d46ec5136b6a184c39d7c34acc749d495921baa944cc6ef945d843a588ca601b40b09b8849e404b2506c26aa654364f1ecc8562d3ec4f924efc78705f29f6ea"
   (internet-address->socket-address '#u8(192 168 43 96) 9994)))

(define (contact-earline)
  (zt-contact-peer
   "f3de84af82:0:d6eea42c09e3f91845dd8fdf9e94c048ea5d63bde1108e8369a7cb79a98ca76951c6b80b80c9d596e0d32d83d94ee73de724412a10ae1cda21888b1206bb1461"
   (internet-address->socket-address '#u8(192 168 43 86) 9994)))

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

;; (set-use-external! #t)

(define (dave-server) #x6f318c4783)

(define (earline-server) #xf3de84af82)

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

#;(define-sense*
  tnw
  initial: (ground)
  pred: (or-ground integer?)
  filter: (lambda (o n) (nws n))
  name: 'tnw)

(define .tnw
  (SENSOR
   initial: (ground)
   pred: (or-ground integer?)
   filter: (lambda (o n) (nws n))
   name: 'tnw))

(define tnw (.tnw))

(.tnw 1)

(define (ctnw) (tnw))

;;; lwIP

(define .lwip
  (SENSOR
   initial: #f
   pred: boolean?
   filter:
   (lambda (o n)
     ;; remove #f to not need the check-values below
     (if (and #t o)
         (error "lwIP initialization can not be undone")
         n))
   name: "lwIP enabled"))

(define lwip (.lwip))

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

(define .zt-started (SENSOR)) (define zt-started (.zt-started))

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
     (display (iam (ctnw) (adhoc-port)) (current-error-port)))))

(define-sense use-external #f)

(define-sense
  here
  #f
  (or-ground (lambda (x) (assq x testers))))

(define (hexstr id digits)
  (let ((effective (number->string id 16)))
    (string-append (make-string (- digits (string-length effective)) #\0) effective)))

(define (dbgmac l v)
  (debug l (hexstr v 12))
  v)

(define-sense*
  *nwif*
  initial: (ground)
  pred: (or-ground netif?)
  filter: #f
  name: '*nwif-example*)

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
    (lwip_init_interface_IPv6 nif (make-6plane-addr nwid ndid (adhoc-port)))
    nif))

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
  (zt-add-local-interface-address! (internet-address->socket-address emeine 9994))
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
        ;; maybe manually orbit?
        (for-each zt-orbit '(#x183ae34a4c #xc72fe7ef99))
        #t)
      #f))

;;* EVENTS

(zt-recv
 (lambda (from type data)
   (debug 'RECV-from-type-data (list from type data))
   (case type
     ((2) (lwc)) ;; tcp test should connect back now
     (else #f))))

#;(define-sense
  zt-online
  #f boolean?
  (lambda (o n) (if (boolean? n) n (eq? 'ONLINE n)))
  'zt-online)

(define-pin
  zt-online
  initial: #f
  pred: boolean?
  filter: (lambda (o n) (if (boolean? n) n (eq? 'ONLINE n)))
  name: 'zt-online)

(zt-event
 (lambda (node userptr thr event payload)
   (debug 'soso event)
   (case event
     ((UP) (.use-external #t))
     ((ONLINE OFFLINE)
      (kick!
       (lambda ()
         (use-external #f)
         (zt-online event)))))))

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
 (lambda (udp socket remaddr data ttl)
   ;; (debug 'wire-send-via socket)
   ;; FIXME: allocate IPv6 too!
   (if (internet-socket-address? remaddr)
       (cond
        ((or (use-external) (receive (a p) (sa->u8 remaddr) (is-ip4-local? a)))
         (debug 'wire-send-via (socket-address->string remaddr))
         (send-message udp data 0 #f 0 remaddr))
        (else #;(debug 'wire-send-via/blocked (socket-address->string remaddr)) -1)))))

(zt-virtual-receive
 ;; API issue: looks like zerotier may just have disassembled a memory
 ;; segment which we now must copy bytewise.  If that's the case we
 ;; better had an interface to pass the underlying pointer.  NOTE:
 ;; It's (currently) important that LWIP_TCPIP_CORE_LOCKING_INPUT is
 ;; not set.
 (lambda (node userptr thr nwid netptr srcmac dstmac ethertype vlanid payload len)
   (dbgmac 'VRECV-SRCMAC srcmac)
   (dbgmac 'VRECV-DSTMAC dstmac)
   (debug 'VRECV-ethertype (ethertype/host-decor ethertype))
   (debug 'VRECV len)
   (debug 'netptr netptr)
   (let ((nif (find-nif dstmac)))
     (if nif
         (lwip-send-ethernet-input! nif (lwip-mac:host->network srcmac) (lwip-mac:host->network dstmac) ethertype payload len)
         (begin
           (debug 'DROP:VRECV-DSTMAC dstmac)
           -1)))))

(define (ethertype/host-decor etht)
  (cond
   ((<= etht 1500) (list 'length etht))
   ((and (> etht 1500) (< etht 1536)) (list 'illegal etht))
   (else
    (let ((x (ethertype/host->symbol etht)))
      (if (eq? x 'lwip-UNKNOWN) (list x etht (ethertype/network->symbol etht)) x)))))

(lwip-nd6-get-gateway (lambda (netif dst) dst)) ;; ZT serves a single switch

(lwip-ethernet-send
 (lambda (netif src dst ethertype #;0 bp len)
   (let ((src (lwip-mac:network->host src))
         (dst (lwip-mac:network->host dst))
         (ethertype ethertype))
     (debug 'lwip-ethernet-send-to-zt len)
     (dbgmac 'src src)
     (dbgmac 'dst dst)
     (debug 'EtherType (ethertype/host-decor ethertype))
     (let ((vlanid 0))
       (if (debug 'DONE:zt-virtual-send/ptr (zt-virtual-send/ptr (find-nwid-for-nif netif) src dst ethertype vlanid bp len))
           0
           -12)))))

;; Config

(zt-virtual-config
 (lambda (node userptr nwid netptr op config)
   (debug 'CONFIG op)
   (debug 'CFG (zt-virtual-config-base->vector config))
   #t))

;; Optional

(zt-path-check
 (lambda (node userptr thr nodeid socket sa)
   (debug 'PATHCHECK (number->string nodeid 16))
   (receive
    (addr port) (sa->u8 sa)
    (debug 'PATHCHECK (cons addr port))
    (or (use-external) (is-ip4-local? addr)))))

(zt-path-lookup
 (lambda (node uptr thr nodeid family sa)
   ;; (debug 'LOOKUP (number->string nodeid 16))
   (debug 'LOOKUP (hexstr nodeid 12))
   (debug 'LooupFamily family)
   (debug 'LookupSA (and sa (sa->u8 sa)))
   0))

;; this will provide an interactive prompt

(define (debug l v)
  (let ((p (current-error-port)))
    (display l p)
    (display ": " p)
    (write v p)
    (newline p)
    v))

;; lwIP TCP

#;(zt-pre-maintainance
 (lambda (prm)
   (debug 'zt-maintainance 'now)
   #t))

(define-type tcp-conn
  pcb
  thread)

(define (lwip-ok? x) (eqv? x 0))

(on-tcp-accept
 (lambda (ctx connection err)
   (case err
     ((0)
      (let ((tcp (make-tcp-conn connection (make-thread (ctx connection) 'tcp-service))))
        (tcp-context-set! connection tcp)
        (thread-start! (tcp-conn-thread tcp))
        0)) ;; ERR_OK
     (else -12)))) ;; FIXME

(on-tcp-receive
 (lambda (ctx connection pbuf err)
   ;; ERR_IF
   -12))

(on-tcp-sent
 (lambda (ctx connection len)
   ;; should signal conn free
   ;; ERR_OK
   0))

(define (c-tcp-poll-interval) 255)

(on-tcp-poll
 (lambda (ctx connection)
   (debug 'UpsPOLL connection)
   (debug 'lwip-tcp-flush! (lwip-err (lwip-tcp-flush! connection)))
    ERR_OK))

(on-tcp-error ;; void
 (lambda (ctx err)
   (debug 'ERROR ctx)
   (debug 'err (lwip-err err))
   #f))

(on-tcp-connect
 (lambda (ctx connection err)
   (case err
     ((0)
      (let ()
        (debug 'CONNECTED connection)
        (ctx connection)
        0)) ;; ERR_OK
     (else (debug 'on-tcp-connect err))))) ;; FIXME

(define (tcp-set-all! pcb) ;; debug
  (for-each
   (lambda (x) (x pcb))
   (list
    tcp-set-receive!
    tcp-set-sent!
    tcp-set-err!
    tcp-set-accept!
    (lambda (pcb) (tcp-set-poll! pcb (c-tcp-poll-interval)))
    )))

(define (lwc)
  (let* ((pcb (tcp-new-ip-type lwip-IPADDR_TYPE_V6))
         (sa (make-6plane-addr (ctnw) (dave-server) (adhoc-port)))
         (addr (receive (a p) (sa->u8 sa) a)))
    (debug "






"  'PLATZ)
     (let ((client (tcp-new6)))
        (debug 'client-bound (lwip-tcp-bind/netif client (*nwif*)))
        (tcp-context-set!
         client
         (lambda (connection)
           (debug 'ClientConnected connection)
           (lwip-tcp-close connection)))
        (tcp-set-err! client)
        (tcp-set-all! client)
        (unless (lwip-ok? (debug 'conn (lwip-tcp-connect client addr (debug 'connecting (adhoc-port)))))
                (error "lwip-tcp-connect failed"))
        (unless (lwip-ok? (lwip-tcp-flush! (debug 'lwip-tcp-flush! client)))
                (error "lwip-tcp-flush! failed"))
        #;(debug 'lws 'waiting-for-callback)
        #;(lambda () (debug 'lws 'post-post) #f))))

(define lws-listener (PIN))

(define (lws-listening!)
  (and (lws-listener) (zt-send! (earline-server) 2 (object->u8vector "listening"))))

(wire! lws-listener post: lws-listening!)

(define (lws)
  (let* ((pcb (tcp-new-ip-type lwip-IPADDR_TYPE_V6))
         (sa (make-6plane-addr (ctnw) (zt-address) (adhoc-port)))
         (addr (receive (a p) (sa->u8 sa) a)))
    (debug "






"  'BEDARF)
    (debug 'lwIP-bind (ip-address->string addr))
    (unless
     (lwip-ok? (lwip-tcp-bind (debug 'pcb pcb) addr (adhoc-port)))
     (error "lwip-tcp-bind failed"))
    (let ((srv (debug 'listening (lwip-tcp-listen (debug 'listenon pcb)))))
      (unless srv (error "lwip-listen failed"))
      (tcp-context-set!
       srv
       (lambda (pcb)
         (lambda ()
           (debug 'Connected pcb))))
      (tcp-set-err! srv)
      (tcp-set-accept! srv)
      (tcp-set-all! srv)
      (lws-listener srv))))

(define tried-to-contact (PIN #f))

(wire!
 (list here zt-online)
 post:
 (lambda ()
   (if (zt-online)
       (begin
         (case (here)
           ((Dave) (contact-earline))
           ((Earline) (contact-dave)))
         (thread-start! (make-thread (lambda () (thread-sleep! 10) (kick! (lambda () (tried-to-contact #t)))))))
       (tried-to-contact #f))))

(wire!
 (list *nwif* tried-to-contact) ;; list of dependencies
 post:                   ;; connect post condition
 ;; TBD DEBUG: lws here just does not cut it.
 (lambda () (if (and (*nwif*) (tried-to-contact) (not (lws-listener))) (lws))))

(define (system-command-line)
  (let loop ((n (system-cmdargc)) (r '()))
    (if (< n 1) r
	(let ((i (- n 1)))
	  (loop i (cons (system-cmdargv i) r))))))

(define (replloop)
  (with-exception-catcher
   (lambda (e)
     (for-each display (list (exception->string e) "\n")) #f)
   (lambda ()
     (##repl-debug)))
  (replloop))

(replloop)

;; eof
