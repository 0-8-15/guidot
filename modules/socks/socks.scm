(declare
 ;; currently overwriting port operations:
 (not standard-bindings
      close-port close-output-port close-input-port
      read-u8 read-subu8vector write-subu8vector write-substring
      force-output
      port?
      ))

;;;** Utilitarian Garbage

(define socks-data-timeout (make-parameter 660))

(define socks-connect-timeout (lambda args (apply lwip-connect-timeout args)))

(define debug-trace-request-forwarding (make-parameter #f))

;; (debug-trace-request-forwarding (lambda (label . more) (debug label more)))

(define-macro (debug-handle-exceptions exn handler expr . body)
  `(with-exception-catcher
    (lambda (,exn) ,handler)
    (lambda () ,expr ,@body)))

(define-macro (debug-trace-request label . more)
  (let ((handler (gensym)))
    `(let ((,handler (debug-trace-request-forwarding)))
       (if ,handler (,handler ,label ,@more)))))

;;;** SOCKS Protocol

(define (socks4a-request cmd dstport dstip #!optional (id ""))
  (let* ((dstip (if (string? dstip) dstip (ipaddr->string dstip)))
         (iplen (string-length dstip))
         (idlen (string-length id))
         (packet (%allocate-u8vector (+ 8 idlen 1 iplen 1))))
    (u8vector-set! packet 0 4)
    (let ((cmd (case cmd
                 ((#t) 2)
                 (else 1))))
      (u8vector-set! packet 1 cmd))
    (%u8vector/n16h-set! packet 2 dstport)
    (%u8vector/n32h-set! packet 4 #xff)
    (u8vector-copy-from-string! packet 8 id)
    (u8vector-set! packet (+ 8 idlen) 0)
    #;((if (string? dstip)
        u8vector-copy-from-string!
        u8vector-copy-from-u8vector!)
    packet (+ 9 idlen) dstip)
    (u8vector-copy-from-string! packet (+ 9 idlen) dstip)
    (u8vector-set! packet (+ 9 idlen iplen) 0)
    packet))

(define socks4-reply
  (let ((granted '#u8(0 #x5a 0 0  0 0 0 0))
        (failed '#u8(0 #x5b 0 0  0 0 0 0)))
    (lambda (key)
      (case key
        ((#t) granted)
        (else failed)))))

(define (socks4-reply! port key)
  (send-packet-now! (socks4-reply key) port))

(define (socks5-reply key addr port)
  (call-with-output-u8vector
   (lambda (out)
     (write-u8 5 out)
     (write-u8
      (case key
        ((#t) 0) ;; granted
        ((#f) 1) ;; general failure
        ((denied 2) 2) ;; connection not allowed by ruleset
        ((3) 3) ;; network unreachable
        ((4) 4) ;; host unreachable
        ((5) 5) ;; connection refused by destination host
        ((6) 6) ;; TTL expired
        ((7) 7) ;; command not supported / protocol error
        ((8) 8) ;; address type not supported
        (else 1))
      out)
     (write-u8 0 out) ;; reserved, fixed zero
     (write-u8
      (cond
       ((string? addr) 3) ;; domain name
       ((fx= (u8vector-length addr) 16) 4) ;; IPv6 address
       (else 1))
      out)
     (if (string? addr)
         (begin
           (write-u8 (string-length addr) out)
           (display addr out))
         (write-subu8vector addr 0 (u8vector-length addr) out))
     (write-u8 (fxarithmetic-shift port -8) out)
     (write-u8 (fxand port 255) out))))

(define (socks5-reply! out key addr port)
  (send-packet-now! (socks5-reply key addr port) out))

(define-macro (socks-success? reply) `(eqv? (u8vector-ref ,reply 1) #x5a))

(define (%socks-bind! version conn-in conn-to kind addr port)
  (let ((request (socks4a-request kind port addr)))
    (send-packet-now! request conn-to)
    (let ((reply (%allocate-u8vector 8)))
      (unless (and (fx= (read-subu8vector reply 0 8 conn-in 8) 8)
                   (socks-success? reply))
        (error "failed")))))

(define (socks4-connect-via proxy socks-spec addr port)
  (let ((request (socks4a-request #f port addr)))
    (send-packet-now! request proxy)
    (let ((reply (%allocate-u8vector 8)))
      (if (and (fx= (read-subu8vector reply 0 8 proxy 8) 8)
               (socks-success? reply))
          proxy
          (begin (close-port proxy) proxy)))))

(define (socks5-connect-via proxy socks-spec addr port)
  (error "NYI socks 5"))

;;;** SOCKS Client

(define (open-socks-tcp-client socks-spec addr port #!key (protocol '4a))
  ;; defaults to protocol 4a since this saves at least one round trip
  (let ((proxy (if (port? socks-spec) socks-spec (open-tcp-client socks-spec))))
    (and proxy
         (case protocol
           ((4 4a) (socks4-connect-via proxy socks-spec addr port))
           ((5) (socks5-connect-via proxy socks-spec addr port))
           (else #f)))))

;;;** SOCKS Service

(define on-socks-connect
  ;; TODO: Add parameters and code for authorization
  ;;
  ;; returns dispatch info.  #f -> denied
  (let ((proc #f))
    (define (dflt key dstip dstport)
      (debug-trace-request key dstip dstport)
      ;; TODO find a usable predicate for the job
      (cond
       ((and (> (string-length dstip) 2)
             (member (substring dstip 0 2) '("FC" "fc")))
        'lwip)
       (else 'host)))
    (set! proc dflt)
    (case-lambda
     (() proc)
     ((x) (if (procedure? x) (set! proc x) (error "on-socks-connect" x))))))

(define (socks-dispatch-connection version name in out dstip dstport)
  (define (socks4-dispatch-connection/common in out addr port conn)
    (cond
     ((port? conn)
      (socks4-reply! out #t)  ;; SOCKS granted
      (ports-connect! conn conn in out))
     (else
      ;; SOCKS reply "rejected or failed"
      (socks4-reply! out #f))))
  (define (socks5-dispatch-connection/common in out addr port conn)
    (cond
     ((port? conn)
      (socks5-reply! out #t addr port)  ;; SOCKS granted
      (ports-connect! conn conn in out))
     (else
      ;; SOCKS reply "rejected or failed"
      (socks5-reply! out #f addr port))))
  (define local-address-parameter
    (if (u16vector? dstip) '(local-address: #u16(0 0 0 0 0 0 0 0)) '()))
  ((case version
     ((4 4a) socks4-dispatch-connection/common)
     ((5) socks5-dispatch-connection/common)
     (else (error "socks-dispatch-connection: illegal version parameter" version)))
   in out dstip dstport
   (with-exception-catcher
    (lambda (ex) (handle-debug-exception ex) #f)
    (lambda ()
      (debug-trace-request name dstip dstport)
      (match
       ((on-socks-connect) name dstip dstport)
       ((? port? x) x)
       ((or 'lwip 'vpn) (open-lwip-tcp-client-connection (lwip-string->ip6-address dstip) dstport))
       ((or 'host #t) (open-tcp-client `(address: ,dstip port-number: ,dstport ,@local-address-parameter)))
       (('vpn addr port) (open-lwip-tcp-client-connection addr port))
       (('host addr port) (open-tcp-client `(address: ,addr port-number: ,port)))
       (#f #f))))))

(define socks-service-register!)
(define socks-service-unregister!)
(define socks-listener
  (let ((listeners (make-table hash: eq?-hash)))
    (define (local-register! port local-port-number local-address)
      (table-set!
       listeners port
       (open-tcp-server
        (list
         local-port-number: local-port-number
         local-address: local-address))))
    (define (lwip-register! port local-port-number local-address)
      (table-set!
       listeners port
       (open-lwip-tcp-server*/ipv6
        local-port-number
        (if (equal? local-address "*") ip6addr-any local-address))))
    (define (register! port #!key (local-port-number port) (local-address "*") (kind 'local))
      (case kind
        ((lwip) (lwip-register! port local-port-number local-address))
        (else (local-register! port local-port-number local-address))))
    (define (unregister! port)
      (let ((listener (table-ref listeners port #f)))
        (when listener
              (table-set! listeners port)
              (close-port/no-exception listener))))
    (set! socks-service-register! register!)
    (set! socks-service-unregister! unregister!)
    (lambda (port) (table-ref listeners port #f))))

(define (socks-dispatch-bind version in out dstip dstport)
  (let ((listener (socks-listener dstport)))
    (if listener
        (let ((conn (read listener)))
          (case version
            ((5) (error "NYI socks-bind version 5"))
            (else (socks4-reply! out #t)))
          (ports-connect! conn conn in out))
        (case version
          ((5) (error "NYI socks-bind version 5"))
          (else (socks4-reply! out #f))))))

(define (socks-dispatch-udp version in out dstip dstport)
  (error "NYI socks-dispatch-udp"))

(define (make-socks-client #!key (socks-address '#u8(127 0 0 1)) (socks-port (socks-port)))
  (open-tcp-client `(address: ,socks-address port-number: ,socks-port)))

(define (socks-bind-cut!
         version port #!optional
         (conn-in (make-socks-client))
         (conn-to conn-in)
         (in (current-input-port))
         (to (current-output-port)))
  (%socks-bind! version conn-in conn-to #t "addr ignored" port)
  (ports-connect! conn-in conn-to in to))

(define (socks-test-local! #!optional (port 1234))
  (socks-service-register! port)
  (socks-bind-cut! 4 port))

(define (socks4a-server/req+in+out name req in out)
  (let ((cmd (u8vector-ref req 1))
        (dstport (u8vector/n16h-ref req 2))
        (dstip (u8vector/n32h-ref req 4)))
    (let* ((user-id (read-line/null-terminated in))
           (dstip
            (if (and (<= dstip 255)
                     ;; (> dstip 0) ;; Buggy firefox versions set it to zero!
                     )
                (read-line/null-terminated in)
                (subu8vector req 4 8))))
      ;; FIXME pass user-id along and fix read-line/null-terminated
      (case cmd
        ((1) (socks-dispatch-connection '4a name in out dstip dstport))
        ((2) (socks-dispatch-bind '4a in out dstip dstport))
        (else (socks4-reply! out #f))))))

(define (socks5-server/req+in+out name req in out)
  (let ((cmd (u8vector-ref req 1))
        (dst-type (u8vector-ref req 3)))
    (let* ((dst-size
            (case dst-type
              ((4) 16)
              ((1) 4)
              ((3) (read-u8 in))
              (else (error "SOCKS5 protocol violation"))))
           (dstip
            (case dst-type
              ((4 1)
               (let ((addr (%allocate-u8vector dst-size)))
                 (if (fx= (read-subu8vector addr 0 dst-size in dst-size) dst-size)
                     addr
                     (error "SOCKS5 could not read address bytes" dst-size))))
              ((3)
               (let ((buffer (%allocate-u8vector dst-size))
                     (addr (make-string dst-size)))
                 (if (fx= (read-subu8vector buffer 0 dst-size in dst-size) dst-size)
                     (do ((i 0 (fx+ i 1)))
                         ((fx= i dst-size) addr)
                       (string-set! addr i (integer->char (u8vector-ref buffer i))))
                     (error "SOCKS5 could not read address string" dst-size))))))
           (dstport
            (let ((buffer (make-u8vector 2)))
              (and
               (fx= (read-subu8vector buffer 0 2 in 2) 2)
               (u8vector/n16h-ref buffer 0)))))
      (case (and dstport cmd)
        ((1) (socks-dispatch-connection 5 name in out dstip dstport))
        ((2) (socks-dispatch-bind 5 in out dstip dstport))
        ((3) (socks-dispatch-udp 5 in out dstip dstport))
        (else (socks5-reply! out #f))))))

(define (socks4a-server/in+out name in out)
  (let ((req (make-u8vector 8)))
    (when
     (fx= (read-subu8vector req 0 8 in 8) 8)
     (case (u8vector-ref req 0)
       ((4) (socks4a-server/req+in+out name req in out))
       (else (socks4-reply! out #f))))))

(define (socks4a-server #!optional (name 'host))
  (socks4a-server/in+out name (current-input-port) (current-output-port)))

(define (socks5-select-auth auth-offered)
  (let loop ((i (fx- (u8vector-length auth-offered) 1)))
    (if (fx= i -1) #f
        (let ((code (u8vector-ref auth-offered i)))
          (case code
            ((0) 0) ;; no authentication required
            (else (loop (fx- i 1))))))))

(define (socks5-client-authenticated name in out method-code)
  ;; handle client authentication
  (case method-code
    ((0) #t) ;; none - succeeds immediately
    ;; Unsupported codes fail
    (else #f)))

(define (socks5-server/in+out name in out)
  (let ((req (make-u8vector 4))) ;; fix client request prefix
    (when
     (fx= (read-subu8vector req 0 4 in 4) 4)
     (case (u8vector-ref req 0)
       ((5) (socks5-server/req+in+out name req in out))
       ;; send protocol error
       (else (socks5-reply! out 7 '#u8(0 0 0 0) 0))))))

(define (socks-server/in+out name in out)
  (let ((version (read-u8 in)))
    (case version
      ((5)
       (let* ((nauth (read-u8 in))
              (auth-offered (make-u8vector nauth)))
         (when (fx= (read-subu8vector auth-offered 0 nauth in nauth) nauth)
               (let ((auth (socks5-select-auth auth-offered)))
                 (if auth
                     (begin
                       (send-packet-now! (u8vector 5 auth) out)
                       (when (socks5-client-authenticated name in out auth)
                             (socks5-server/in+out name in out)))
                     (send-packet-now! (u8vector 5 #xff) out))))))
      ((4)
       (let ((req (make-u8vector 8)))
         ;; (u8vector-set! req 0 version) ;; actually unsued
         (when (fx= (read-subu8vector req 1 8 in 7) 7)
           (socks4a-server/req+in+out name req in out)))))))

(define (socks-server #!optional (name 'host))
  (debug-handle-exceptions
   exn (handle-debug-exception exn 'socks-server)
   (socks-server/in+out name (current-input-port) (current-output-port))))

#|

(define-sense socks-port #f)

(wire!
 socks-port
 sequence:
 (lambda (old new)
   (if old (tcp-service-unregister! old))
   (if new (tcp-service-register! new socks-server))))

(wire!
;; Boah - what a dangerous thing!
 socks-port
 sequence:
 (lambda (old new)
   (if old (lwip-tcp-service-unregister! old))
   (if new (lwip-tcp-service-register! new socks-server))))


(.socks-port 9051)

;;;|#
