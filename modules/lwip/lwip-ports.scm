(define-macro (lwip/after-safe-return expr) `(lambda () ,expr))

;; (C) 2020 JFW

;;** lwIP TCP

(define-macro (lwip-timeout outstanding) 60) ;; TODO: calculate better timeout

(define lwip-connect-timeout (make-parameter 60))

;;;*** lwIP TCP data structures and basics
(define (%%lwip-make-pipe) (open-u8vector-pipe '(buffering: #f) '(buffering: #f)))

#;(define (%%tcp-context-set! pcb ctx)
  (tcp-context-set! pcb ctx))

;#|;; BEWARE: it appeares to be illegal to store the context directly in
;;;; the pcb, even if we kept another reference to it around.
(define-type tcp-connection id srv-port status pcb next)

(define tcp-conn-nr
  (let ((n (vector 1)))
    (lambda ()
      (let* ((old (vector-ref n 0)) (new (+ old 1)))
        (if (eq? (vector-cas! n 0 new old) old)
            old
            (tcp-conn-nr))))))

(define tcp-contexts (make-table hash: equal?-hash weak-keys: #f))

(define (%%lwip-close-tcp-connection/dir conn dir)
  (define table-remove! table-set!)
  (define (closeit pcb status dir)
    (cond
     ((zero? (bitwise-and status 3)) (lwip-tcp-close pcb))
     (else
      (lwip-tcp_shutdown
       pcb
       (and (not (zero? (bitwise-and status 1))) (eq? dir 'input))
       (and (not (zero? (bitwise-and status 2))) (eq? dir 'output))))))
  (let ((pcb (tcp-connection-pcb conn))
        (status (tcp-connection-status conn)))
    (case dir
      ((input) (tcp-connection-status-set! conn (bitwise-and status 2)))
      ((output) (tcp-connection-status-set! conn (bitwise-and status 1)))
      ((input+output) (tcp-connection-status-set! conn 0) (set! status 0))
      (else (error "lwIP close: illegal direction" dir)))
    (let retry ((rc (and pcb (closeit pcb status dir))))
      (cond
       ((eq? rc ERR_MEM)
        (thread-receive)
        (retry (closeit pcb status dir))))
      (when (eqv? (bitwise-and (tcp-connection-status conn) 3) 0)
            (tcp-connection-pcb-set! conn #f)
            ;; eventually only
            (table-remove! tcp-contexts (tcp-connection-id conn))
            (close-port (tcp-connection-srv-port conn))))))

(define (%%close-tcp-connection loc conn dir)
  (%%lwip-close-tcp-connection/dir conn dir))

(define-macro (close-tcp-connection location conn)
  `(%%close-tcp-connection ,location ,conn 'input+output))

(define (%%tcp-context-set! pcb ctx)
  #;(tcp-context-set! pcb ctx)
  (let ((n (tcp-conn-nr)))
    (tcp-connection-id-set! ctx n)
    (tcp-context-set! pcb n)
    (table-set! tcp-contexts n ctx)))
;;|#

;;;*** lwIP TCP API

(define (open-lwip-tcp-client-connection addr port) ;; EXPORT
  (receive
   (c s) (%%lwip-make-pipe)
   (input-port-timeout-set! c (lwip-connect-timeout))
   (let* ((client (tcp-new6))
          (conn (make-tcp-connection #f s 4 client #f)))
     (%%tcp-context-set! client conn) ;; FIRST register for on-tcp-connect to find it
     (unless (eqv? (lwip-tcp-connect client addr port) ERR_OK)
       (%%lwip-close-tcp-connection/dir conn 'input+output)
       (close-port s)
       #;(error (debug 'open-lwip-tcp-client-connection "open-lwip-tcp-client-connection failed") addr port))
     c)))

(define (port-copy-to-lwip/synchronized in conn mtu)
  ;; TBD: How to use: get-output-u8vector ?  How to wait for it?
  (let ((buffer (%allocate-u8vector mtu)))
    (let loop ()
      (let ((n (read-subu8vector buffer 0 mtu in 1)))
        (cond
         ((eqv? n 0) ;; done
          (%%close-tcp-connection 'port-copy-to-lwip/synchronized conn 'input+output))
         (else
          (let retry ((pcb (tcp-connection-pcb conn)))
            (cond
             (pcb
              (let ((rc (lwip-tcp-write-subu8vector* buffer 0 n pcb)))
                (cond
                 ((eq? rc ERR_OK)
                  (lwip-tcp-force-output pcb)
                  ;; wait for it to be out -- a bit dangerous - if buffer was moved around by GC
                  (let wait ((ack (thread-receive)))
                    (if (= ack n) (loop) (wait (+ ack (thread-receive))))))
                 ((eq? rc ERR_MEM)
                  (thread-receive)
                  (retry (tcp-connection-pcb conn)))
                 (else (debug 'port-copy-to-lwip:fail (lwip-err rc))))))))))))))

(define (port-copy-to-lwip/always-copy in conn mtu)
  (let ((buffer (%allocate-u8vector mtu))
        (outstanding 0))
    (define (await)
      (and (fx> outstanding 0)
           (let ((aknowledged (thread-receive (lwip-timeout outstanding) #f)))
             (and aknowledged
                  (begin
                    (set! outstanding (fx- outstanding aknowledged))
                    #t)))))
    (let loop ()
      (let ((n (read-subu8vector buffer 0 mtu in 1)))
        (cond
         ((eqv? n 0)
          #;(%%lwip-close-tcp-connection/dir conn 'output)
          (%%close-tcp-connection 'port-copy-to-lwip/always-copy conn 'input+output)
          (do ((r (await) (await))) ((not r)))
          #;(%%lwip-close-tcp-connection/dir conn 'input))
         (else
          (let retry ((pcb (tcp-connection-pcb conn)))
            (cond
             (pcb
              (let ((rc (lwip-tcp-write-subu8vector/copy* buffer 0 n pcb)))
                (cond
                 ((eq? rc ERR_OK)
                  (set! outstanding (fx+ outstanding n))
                  (lwip-tcp-force-output pcb)
                  (loop))
                 ((eq? rc ERR_MEM) (and (await) (retry (tcp-connection-pcb conn))))
                 (else (debug 'port-copy-to-lwip:fail (lwip-err rc))))))))))))))

(define port-copy-to-lwip/default port-copy-to-lwip/always-copy)

(define lwip-MTU (make-parameter 2880))

(define port-copy-to-lwip-set!)

(define port-copy-to-lwip
  ;; TBD: experiment with strategies wrt. performance
  (let ((proc port-copy-to-lwip/default))
    (define (setter arg)
      (if (procedure? arg) (set! proc arg) (error "setting port-copy-to-lwip: illegal argument" arg)))
    (set! port-copy-to-lwip-set! setter)
    (lambda (in conn #!optional (MTU (lwip-MTU))) (proc in conn MTU))))

(define (open-lwip-tcp-server*/ipv6 port #!key (local-addr lwip-ip6addr-any)) ;; half EXPORT
  (let* ((srv (tcp-new-ip-type lwip-IPADDR_TYPE_V6)))
    (unless
     (eqv? (lwip-tcp-bind srv local-addr port) ERR_OK)
     (lwip-tcp-close srv)
     (error "lwip-tcp-bind failed"))
    (let ((srv (let ((listening (lwip-tcp-listen srv)))
                 (or listening
                     (begin
                       (lwip-tcp-close srv)
                       (error "lwip-listen failed"))))))
      (receive
       (c s) (open-vector-pipe)
       (let ((lws-conn (make-tcp-connection #f s 1 srv #f)))
         (%%tcp-context-set! srv lws-conn)
         c)))))

(define (%lwip-service-starter handler srv name)
  (let loop ()
    (let ((conn (read srv)))
      (if (port? conn)
          (begin
            (thread-start!
             (make-thread
              (lambda ()
                (parameterize
                 ((current-input-port conn)
                  (current-output-port conn))
                 (with-exception-catcher lwip-exception-handler handler)) ;; FIXME prints "Connection refused"
                (close-port conn))
              name))
            (loop))))))

(define lwip-tcp-service-register!)   ;; EXPORT
(define lwip-tcp-service-unregister!) ;; EXPORT
(define lwip-tcp-service
  (let ((services (make-table hash: eqv?-hash weak-keys: #f)))
    (define (register! port handler)
      (if (table-ref services port #f)
          (error "lwip-tcp-service-register! port already bound" port))
      (let ((srv (open-lwip-tcp-server*/ipv6 port)))
        (thread-start!
         (make-thread
          (lambda () (%lwip-service-starter handler srv 'lwip-tcp-server))
          'lwip-tcp-service))
        (table-set! services port srv)))
    (define (unregister! port)
      (let ((srv (table-ref services port #f)))
        (when srv
              (table-set! services port)
              (close-port srv))))
    (set! lwip-tcp-service-register! register!)
    (set! lwip-tcp-service-unregister! unregister!)
    (lambda (port) (table-ref services port #f))))

(let ()

  (define (on-tcp-accept ctx connection err)
    (cond
     ((and (eqv? err ERR_OK) ctx)
      (receive
       (c s) (%%lwip-make-pipe)
       (input-port-timeout-set! c (lwip-connect-timeout))
       (let ((conn (make-tcp-connection #f s 3 connection #f)))
         (tcp-connection-next-set!
          conn
          (make-thread
           (lambda ()
             (port-copy-to-lwip s conn)
             ;; (close-port s) done in close-tcp-connection
             ;; lwip-tcp-close done in close-tcp-connection
             (close-tcp-connection "client closed connection" conn))
           'tcp-server))
         (%%tcp-context-set! connection conn)
         (let ((rcv (tcp-connection-srv-port ctx)))
           (write c rcv)
           (force-output rcv))
         (lwip/after-safe-return (thread-start! (tcp-connection-next conn))))))
     (else (lwip/after-safe-return (lwip-tcp-close connection)))))

  (define (on-tcp-sent ctx connection len)
    ;; signal conn free
    (let ((t (if ctx (tcp-connection-next ctx) (begin #;(debug 'on-tcp-sent:conn-already-closed len) #f))))
      (if t (thread-send t len)))
    ERR_OK)

  (define (on-tcp-receive ctx connection pbuf err)
    (cond
     ((not pbuf) ;; closed
      (if ctx (lwip/after-safe-return (%%close-tcp-connection on-tcp-receive ctx 'input+output)) ERR_OK))
     ((and (eqv? err ERR_OK) ctx)
      (%%while-in-callback-tcp-received! connection (pbuf-length pbuf))
      (lwip/after-safe-return
       (let ((u8 (pbuf->u8vector pbuf))
             (s (tcp-connection-srv-port ctx)))
         (pbuf-release! pbuf)
         (let ((n (u8vector-length u8)))
           ;; (tcp-received! connection n) ;; too late, could have been frred already
           (write-subu8vector u8 0 n s)
           (force-output s)))))
     ((and (not ctx) connection) (lwip/after-safe-return (lwip-tcp-close connection)))
     (else ERR_IF)))

  (define (on-tcp-connect ctx connection err)
    (cond
     ((and (eqv? err ERR_OK) ctx)
      (let ()
        (tcp-connection-pcb-set! ctx connection)
        (tcp-connection-status-set! ctx 3)
        (tcp-connection-next-set!
         ctx
         (make-thread
          (lambda ()
            (port-copy-to-lwip (tcp-connection-srv-port ctx) ctx)
            ;; (close-port s) done in close-tcp-connection
            ;; lwip-tcp-close done in close-tcp-connection
            ;; (close-tcp-connection "server closed connection" ctx)
            #;(thread-send (debug 'tcp-connect-input-closed (tcp-connection-next ctx)) 'close-input)
            (tcp-connection-next-set! ctx #f))
          'lwip-tcp-client))
        (lwip/after-safe-return (thread-start! (tcp-connection-next ctx)))))
     ((and (not ctx) connection) (lwip/after-safe-return (lwip-tcp-close connection)))
     (else (debug 'on-tcp-connect err)))) ;; FIXME

  (define (on-tcp-poll ctx connection)
    (if (tcp-connection? ctx)
        (let ((t (tcp-connection-next ctx)))
          (if t (thread-send t 0))))
    ;; (debug 'lwip-tcp-force-output (lwip-err (lwip-tcp-force-output connection)))
    ERR_OK)

  (define (on-tcp-error ctx err)
    (cond
     ((tcp-connection? ctx)
      ;; This is likely either a timeout or an indicator of other errors.
      (if ($lwip-debug)
          (debug "lwIP: TCP error, closing connection: " ctx))
      ;; The pcb is already freed when this callback is called!
      (tcp-connection-pcb-set! ctx #f)
      (tcp-connection-status-set! ctx 0)
      (close-tcp-connection "closing connection on error" ctx)))
    ERR_OK)

  (on-tcp-event
   (lambda (ctx pcb e pbuf len err)
     (set! ctx (table-ref tcp-contexts ctx #f)) ;;; ENABLE if conns are counted above
     ;; NOTE: This passes along what the lwIP callback API did.
     (cond
      ;; ((not ctx) (debug 'already-closed e) ERR_OK)
      ((eq? LWIP_EVENT_ACCEPT e) (on-tcp-accept ctx pcb err))
      ((eq? LWIP_EVENT_SENT e) (on-tcp-sent ctx pcb len))
      ((eq? LWIP_EVENT_RECV e) (on-tcp-receive ctx pcb pbuf err))
      ((eq? LWIP_EVENT_CONNECTED e) (on-tcp-connect ctx pcb err))
      ((eq? LWIP_EVENT_POLL e) (on-tcp-poll ctx pcb))
      ((eq? LWIP_EVENT_ERR e) (on-tcp-error ctx err))
      (else (debug "on-tcp-event: unknown event" e) ERR_ARG))))
  ) ;; lwIP TCP API
