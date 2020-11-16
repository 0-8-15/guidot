;; (C) 2020 JFW

;;**** TBD: Imports

(define-macro (lwip/after-safe-return expr) `(lambda () ,expr))

(declare
 ;; currently depends on gamhack overwriting port operations:
 (not standard-bindings
      close-port close-output-port close-input-port
      read-u8 read-subu8vector write-subu8vector write-substring
      force-output
      port?
      ))

;;** lwIP TCP

(define-macro (lwip-timeout outstanding) 60) ;; TODO: calculate better timeout

(define lwip-connect-timeout (make-parameter 60))

(define lwip-buffer-factor (make-parameter 50)) ;; as many MTU in memory

;;;*** lwIP TCP data structures and basics

#;(define (%%tcp-context-set! pcb ctx)
  (tcp-context-set! pcb ctx))

;#|;; BEWARE: it appeares to be illegal to store the context directly in
;;;; the pcb, even if we kept another reference to it around.
(define-type tcp-connection id client-port srv-port status pcb next)

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
        (status (tcp-connection-status conn))
        (srv-port (tcp-connection-srv-port conn))
        (client-port (tcp-connection-client-port conn)))
    (case dir
      ((input)
       (close-input-port srv-port)
       (close-output-port client-port)
       (tcp-connection-status-set! conn (bitwise-and status 2)))
      ((output)
       (close-output-port srv-port)
       (close-input-port client-port)
       (tcp-connection-status-set! conn (bitwise-and status 1)))
      ((input+output)
       (close-port srv-port)
       (close-port client-port)
       (tcp-connection-status-set! conn 0) (set! status 0))
      (else (error "lwIP close: illegal direction" dir)))
    (let retry ((rc (if pcb
                        (closeit pcb status dir)
                        (and ($lwip-debug) (debug (list conn dir) 'already-closed) #f))))
      (cond
       ((eq? rc ERR_MEM)
        (thread-receive)
        (retry (let ((pcb (tcp-connection-pcb conn))) (and pcb (closeit pcb status dir))))))
      (when (eqv? (bitwise-and (tcp-connection-status conn) 3) 0)
        (tcp-connection-pcb-set! conn #f)
        ;; eventually only
        (table-remove! tcp-contexts (tcp-connection-id conn))))))

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

(define (make-lwip-quasi-port conn #!optional (MTU 3000))
  ;; return pseudo-port (currently a procedure)
  (let ((outstanding 0))
    (define (await)
      (and (fx> outstanding 0)
           (let ((aknowledged (thread-receive (lwip-timeout outstanding) #f)))
             (and aknowledged
                  (begin
                    (set! outstanding (fx- outstanding aknowledged))
                    #t)))))
    (define (write-buffer buffer offset n mx continue)
      (cond
       ((eqv? n 0)
        (cond
         ((not outstanding)
           (if ($lwip-debug) (debug 'closed-again conn))
          #f)
         (else
          (do ((r (await) (await))) ((not r)))
          (set! outstanding #f)
          (%%close-tcp-connection 'port-copy-to-lwip/always-copy conn 'output)
          (continue buffer 0 0 mx (lambda _ #f)))))
       (else
        (let retry ((pcb (tcp-connection-pcb conn)))
          (cond
           (pcb
            (let ((rc (lwip-tcp-write-subu8vector/copy* buffer offset (fx+ offset n) pcb)))
              (cond
               ((eq? rc ERR_OK)
                (set! outstanding (fx+ outstanding n))
                (lwip-tcp-force-output pcb)
                (continue buffer 0 0 mx write-buffer))
               ((eq? rc ERR_MEM) (and (await) (retry (tcp-connection-pcb conn))))
               (else (debug 'port-copy-to-lwip:fail (lwip-err rc)))))))))))
    (receive
        (read-port s) (open-u8vector-pipe '(buffering: #f) '(buffering: #f))
      (tcp-connection-client-port-set! conn read-port)
      (tcp-connection-srv-port-set! conn s)
      (let ((buflim (* MTU (lwip-buffer-factor))))
        (macro-u8vector-port-buffering-limit-set! s buflim)
        (macro-u8vector-port-buffering-limit-set! read-port buflim))
#|
      (macro-u8vector-port-rbuf-fill-set!
       (lambda (port want block?)
         (let loop ()

           ;; shift bytes between rlo and rhi to beginning of buffer

           (let ((byte-rlo (macro-u8vector-port-rlo port))
                 (byte-rhi (macro-u8vector-port-rhi port)))
             (if (##fx< byte-rlo byte-rhi)
                 (let ((byte-rbuf (macro-u8vector-port-rbuf port)))
                   (##subu8vector-move! byte-rbuf byte-rlo byte-rhi byte-rbuf 0)))
             (macro-u8vector-port-rlo-set! port 0)
             (macro-u8vector-port-rhi-set! port (##fx- byte-rhi byte-rlo)))

           ;; read into byte buffer at rhi

           (let* ((byte-rbuf
                   (macro-u8vector-port-rbuf port))
                  (byte-rhi
                   (macro-u8vector-port-rhi port))
                  (n
                   .... lwip here
                   (##os-device-stream-read
                    (macro-device-port-rdevice-condvar port)
                    byte-rbuf
                    byte-rhi
                    (let ((rbuf-len (##u8vector-length byte-rbuf)))
                      (if (and want (macro-unbuffered? (macro-port-roptions port)))
                          (##fxmin (##fx+ byte-rhi want) rbuf-len)
                          rbuf-len)))))

             (if (##fx< n 0)

                 ;; the read caused an error

                 (cond ((##fx= n ##err-code-EINTR)

                        ;; the read was interrupted, so try again

                        (loop))

                       ((and block?
                             (##fx= n ##err-code-EAGAIN))

                        ;; the read would block and it is OK to block so wait
                        ;; and then try again

                        (macro-port-mutex-unlock! port)
                        (let ((continue?
                               (or (##wait-for-io!
                                    (macro-device-port-rdevice-condvar port)
                                    (macro-port-rtimeout port))
                                   ((macro-port-rtimeout-thunk port)))))
                          (macro-port-mutex-lock! port) ;; regain access to port
                          (if continue?
                              (loop)
                              n)))

                       (else

                        ;; return the error code to the caller

                        n))

                 ;; the read completed successfully

                 (if (##fx= n 0) ;; was end-of-file reached?
                     #f
                     (begin
                       (macro-u8vector-port-rhi-set! port
                                                 (##fx+ (macro-u8vector-port-rhi port) n))
                       #t)))))))
|#
      (make-quasi-port read-port write-buffer))))

;;;*** lwIP TCP API

(define (open-lwip-tcp-client-connection addr port) ;; EXPORT
  (let* ((client (tcp-new6))
         (conn (make-tcp-connection #f #f #f 4 client #f))
         (result (make-lwip-quasi-port conn)))
    (%%tcp-context-set! client conn) ;; FIRST register for on-tcp-connect to find it
    (input-port-timeout-set! (quasi-port-read-port result) (lwip-connect-timeout))
    (unless (eqv? (lwip-tcp-connect client addr port) ERR_OK)
      (%%lwip-close-tcp-connection/dir conn 'input+output)
      (close-output-quasi-port result)
      #;(error (debug 'open-lwip-tcp-client-connection "open-lwip-tcp-client-connection failed") addr port))
    (quasi-port-read-port result)))

(define (port-copy-to-lwip/synchronized in conn mtu)
  ;; TBD: How to use: get-output-u8vector ?  How to wait for it?
  (let ((buffer (%allocate-u8vector mtu)))
    (let loop ()
      (let ((n (read-subu8vector buffer 0 mtu in 1)))
        (cond
         ((eqv? n 0) ;; done
          (%%close-tcp-connection 'port-copy-to-lwip/synchronized conn 'output))
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
          (do ((r (await) (await))) ((not r)))
          (close-input-port in)
          (%%close-tcp-connection 'port-copy-to-lwip/always-copy conn 'output))
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

(define (port-copy-lwip/flow-control in out #!optional (MTU 3000))
  (if  (not (quasi-port? out))
       (port-copy-to-lwip/always-copy in out MTU)
       ;; Copy `in` to `out` and close the input side of `in` and the
       ;; output side of `out` when EOF is reached on `in`.
       (let ((corout (quasi-port-writer out))
             (buffer (%allocate-u8vector MTU)))
         (input-port-timeout-set! in (lwip-connect-timeout))
         (let loop ((buffer buffer)
                    (offset 0)
                    (n 0)
                    (mtu MTU)
                    (corout corout))
           (let ((n (read-subu8vector buffer offset mtu in 1)
                  #;(begin
                    (##wait-input-port in)
                    (set! buffer (get-output-u8vector in))
                    (u8vector-length buffer))))
             (input-port-timeout-set! in (lwip-timeout mtu))
             (cond
              ((eqv? n 0)
               (close-input-port in)
               ;; ??? FIXME: really close out???
               (close-output-port out))
              (else (corout buffer offset n mtu loop))))))))

(define port-copy-to-lwip/default port-copy-lwip/flow-control)

(define lwip-MTU (make-parameter 2880))

(define port-copy-to-lwip-set!)

(define port-copy-to-lwip
  ;; TBD: experiment with strategies wrt. performance
  (let ((proc port-copy-to-lwip/default))
    (define (setter arg)
      (if (procedure? arg) (set! proc arg) (error "setting port-copy-to-lwip: illegal argument" arg)))
    #;(set! port-copy-to-lwip-set! setter)
    (set! port-copy-to-lwip-set! (lambda _ #f))
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
       (c s) (open-vector-pipe '(buffering: #f) '(buffering: #f))
       (let ((lws-conn (make-tcp-connection #f #f s 1 srv #f)))
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
                 ((current-input-port (cond ((quasi-port? conn) (quasi-port-read-port conn)) (else conn)))
                  (current-output-port (cond ((quasi-port? conn) (quasi-port-read-port conn)) (else conn))))
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
      (let* ((conn (make-tcp-connection #f #f #f 3 connection #f))
             (result (make-lwip-quasi-port conn)))
        (input-port-timeout-set! (quasi-port-read-port result) (lwip-connect-timeout))
        (tcp-connection-next-set!
         conn
         (make-thread
          (lambda ()
            (port-copy-to-lwip (tcp-connection-srv-port conn) result))
          'lwip-tcp-server-connection))
        (%%tcp-context-set! connection conn)
        (let ((rcv (tcp-connection-srv-port ctx)))
          (write result rcv)
          (force-output rcv))
        (lwip/after-safe-return (thread-start! (tcp-connection-next conn)))))
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
           ;; (tcp-received! connection n) ;; too late, could have been freed already
           (if (fx= (write-subu8vector u8 0 n s) n)
               (force-output s)
               ;; lost client connection
               (lwip-tcp-close connection))))))
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
      (close-tcp-connection "closing connection on error" ctx)
      ERR_OK)
     (else (debug 'on-tcp-error/missing-context err) ERR_OK)))

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
