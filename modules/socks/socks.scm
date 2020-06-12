;;;** Utilitarian Garbage

(define (handle-debug-exception e)
  (##default-display-exception e (current-error-port))
  #!void)

(define (read-line/null-terminated port #!optional (mxlen 64))
  ;;(define (read-line/null-terminated port) (read-line port #\nul))
  (let ((in (make-string mxlen)))
    (do ((off 0 (+ off 1))
         (n (read-u8 port) (read-u8 port)))
        ((or (eqv? n 0) (eof-object? n))
         (substring in 0 off))
      ;; overflows at 64 - is just a PoC
      (string-set! in off (integer->char n)))))

(define (port-copy-through in out #!optional (MTU 3000))
  ;; Copy `in` to `out` and close the input side of `in` and the
  ;; output side of `out` when EOF is reached on `in`.
  ;;
  ;; TODO: re-write using get-output-u8vector
  ;; ##wait-input-port
  (let ((buffer (make-u8vector MTU)))
    (let loop ()
      (let ((n (read-subu8vector buffer 0 MTU in 1)))
        (cond
         ((eqv? n 0)
          ;; done TBD: which one to close first?  Sequence seems to
          ;; depend on implementation.
          (close-input-port in)
          (close-output-port out))
         (else
          (write-subu8vector buffer 0 n out)
          (force-output out)
          (loop)))))))

(define (port-pipe+close! in out #!optional (MTU 3000))
  (with-exception-catcher
   (lambda (exn)
     (close-input-port in)
     (close-output-port out)
     (raise (debug 'port-pipe+close! exn)))
   (lambda () (port-copy-through in out MTU))))

(define (ports-connect! r0 w0 r1 w1)
  ;; terminates when r1 is at EOF - TBD: maybe should also terminate when w0 is closed!
  (let ((thr (thread-start! (make-thread (lambda () (port-pipe+close! r0 w1)) 'port-copy))))
    (port-pipe+close! r1 w0)
    (thread-join! thr)))

(define (send-packet-now! packet conn)
  (write-subu8vector packet 0 (u8vector-length packet) conn)
  (force-output conn))

;;;** SOCKS Protocol

(define (socks4a-request cmd dstport dstip #!optional (id ""))
  (let* ((iplen (if (string? dstip) (string-length dstip) (u8vector-length dstip)))
         (idlen (string-length id))
         (packet (make-u8vector (+ 8 idlen 1 iplen 1))))
    (u8vector-set! packet 0 4)
    (let ((cmd (case cmd
                 ((#t) 2)
                 (else 1))))
      (u8vector-set! packet 1 cmd))
    (%u8vector/n16h-set! packet 2 dstport)
    (%u8vector/n32h-set! packet 4 #xff)
    (u8vector-copy-from-string! packet 8 id)
    (u8vector-set! packet (+ 8 idlen) 0)
    ((if (string? dstip)
        u8vector-copy-from-string!
        u8vector-copy-from-u8vector!)
     packet (+ 9 idlen) dstip)
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

(define-macro (socks-success? reply) `(eqv? (u8vector-ref ,reply 1) #x5a))

(define (%socks-bind! conn-in conn-to kind addr port)
  (let ((request (socks4a-request kind port addr)))
    (send-packet-now! request conn-to)
    (let ((reply (make-u8vector 8)))
      (when
       (read-subu8vector reply 0 8 conn-in 8)
       (unless (socks-success? reply) (error "failed"))))))

(define (open-socks-tcp-client socks-spec addr port)
  (let ((proxy (if (port? socks-spec) socks-spec (open-tcp-client socks-spec))))
    (and proxy
         (let ((request (socks4a-request #f port addr)))
           (send-packet-now! request proxy)
           (let ((reply (make-u8vector 8)))
             (if (read-subu8vector reply 0 8 proxy 8)
                 (if (socks-success? reply) proxy
                     (begin (close-port proxy) proxy))
                 (begin (close-port proxy) proxy)))))))

;;;** SOCKS Service

(define on-socks-connect
  ;; returns dipatch info.  #f -> denied
  (let ((proc #f))
    (define (dflt dstip dstport)
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

(define (socks-dispatch-connection name in out dstip dstport)
  (define (socks-dispatch-connection/common in out conn)
    (cond
     ((port? conn)
      (socks4-reply! out #t)  ;; SOCKS granted
      (with-exception-catcher
       handle-debug-exception
       (lambda () (ports-connect! conn conn in out)))
      (close-port conn))
     (else
      ;; SOCKS reply "rejected or failed"
      (socks4-reply! out #f))))
  (socks-dispatch-connection/common
   in out
   (with-exception-catcher
    (lambda (ex) (handle-debug-exception ex) #f)
    (lambda ()
      (match
       ((on-socks-connect) name dstip dstport)
       ((? port? x) x)
       ((or 'lwip 'vpn) (open-lwip-tcp-client-connection (lwip-string->ip6-address dstip) dstport))
       ((or 'host #t) (open-tcp-client `(address: ,dstip port-number: ,dstport)))
       (('vpn addr port) (open-lwip-tcp-client-connection addr port))
       (('host addr port) (open-tcp-client addr port))
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
              (close-port listener))))
    (set! socks-service-register! register!)
    (set! socks-service-unregister! unregister!)
    (lambda (port) (table-ref listeners port #f))))

(define (socks-dispatch-bind in out dstip dstport)
  (let ((listener (socks-listener dstport)))
    (if listener
        (let ((conn (read listener)))
          (socks4-reply! out #t)
          (with-exception-catcher
           handle-debug-exception
           (lambda () (ports-connect! conn conn in out)))
          (close-port conn))
        (socks4-reply! out #f))))

(define (make-socks-client #!key (socks-address '#u8(127 0 0 1)) (socks-port (socks-port)))
  (open-tcp-client `(address: ,socks-address port-number: ,socks-port)))

(define (socks-bind-cut!
         port #!optional
         (conn-in (make-socks-client))
         (conn-to conn-in)
         (in (current-input-port))
         (to (current-output-port)))
  (%socks-bind! conn-in conn-to #t "addr ignored" port)
  (ports-connect! conn-in conn-to in to))

(define (socks-test-local! #!optional (port 1234))
  (socks-service-register! port)
  (socks-bind-cut! port))

(define (sock4a-server/req+in+out name req in out)
  (let ((cmd (u8vector-ref req 1))
        (dstport (u8vector/n16h-ref req 2))
        (dstip (u8vector/n32h-ref req 4)))
    (let* ((user-id (read-line/null-terminated in))
           (dstip
            (if (and (<= dstip 255) (> dstip 0))
                (read-line/null-terminated in)
                dstip)))
      ;; FIXME pass user-id along and fix read-line/null-terminated
      (case cmd
        ((1) (socks-dispatch-connection name in out dstip dstport))
        ((2) (socks-dispatch-bind in out dstip dstport))
        (else (socks4-reply! out #f))))))

(define (socks-server/in+out name in out)
  (let ((req (make-u8vector 8)))
    (when
     (read-subu8vector req 0 8 in 8)
     (case (u8vector-ref req 0)
       ((4) (sock4a-server/req+in+out name req in out))
       (else (socks4-reply! out #f))))))

(define (socks-server #!optional (name 'host))
  (socks-server/in+out name (current-input-port) (current-output-port)))

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
