;; (C) 2020 JFW

;;** lwIP TCP

;;;*** lwIP TCP data structures and basics
(define (%%lwip-make-pipe) (open-u8vector-pipe '(buffering: #f) '(buffering: #f)))

#;(define (%%tcp-context-set! pcb ctx)
  (tcp-context-set! pcb ctx))

;#|;; BEWARE: it appeares to be illegal to store the context directly in
;;;; the pcb, even if we kept another reference to it around.
(define-type tcp-connection id srv-port pcb next)

(define tcp-conn-nr
  (let ((n (vector 1)))
    (lambda ()
      (let* ((old (vector-ref n 0)) (new (+ old 1)))
        (if (eq? (vector-cas! n 0 new old) old)
            old
            (tcp-conn-nr))))))

(define tcp-contexts (make-table hash: equal?-hash weak-keys: #f))

(define (close-tcp-connection conn)
  (define table-remove! table-set!)
  (let ((pcb (tcp-connection-pcb conn)))
    (close-port (tcp-connection-srv-port conn))
    (when pcb
          (tcp-connection-pcb-set! conn #f)
          (let retry ((rc (lwip-tcp-close pcb)))
            (cond
             ((eq? rc ERR_MEM)
              (thread-receive)
              (retry (lwip-tcp-close pcb))))))
    ;; eventually only
    (table-remove! tcp-contexts (tcp-connection-id conn))))

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
   (let ((client (tcp-new6)))
     (unless (eqv? (lwip-tcp-connect client addr port) ERR_OK)
             (lwip-tcp-close client)
             (error "open-lwip-tcp-client-connection failed"))
     (let ((conn (make-tcp-connection #f s client #f)))
       (%%tcp-context-set! client conn)
       #;(unless (lwip-ok? (lwip-tcp-flush! (debug 'lwip-tcp-flush! client))) (error "lwip-tcp-flush! failed"))
       c))))

(define (port-copy-to-lwip in conn #!optional (MTU 3000))
  ;; FIXME: use get-output-u8vector !!!
  ;;
  ;; FIXME: likely we need to do bookkeeping wrt outstanding writes
  (let ((buffer (make-u8vector MTU)))
    (let loop ()
      (let ((n (read-subu8vector buffer 0 MTU in 1)))
        (cond
         ((eqv? n 0)) ;; done
         (else
          (let retry ((pcb (tcp-connection-pcb conn)))
            (cond
             (pcb
              (let ((rc (lwip-tcp-write-subu8vector* buffer 0 n pcb)))
                (cond
                 ((eq? rc ERR_OK)
                  (lwip-tcp-flush! pcb)
                  (loop))
                 ((eq? rc ERR_MEM)
                  (thread-receive)
                  (retry (tcp-connection-pcb conn)))
                 (else (debug 'port-copy-to-lwip:fail (lwip-err rc))))))))))))))

(define (open-lwip-tcp-server*/ipv6 port #!key (local-addr ip6addr-any)) ;; half EXPORT
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
       (let ((lws-conn (make-tcp-connection #f s srv #f)))
         (%%tcp-context-set! srv lws-conn)
         c)))))

(define (service-starter handler srv name)
  (let loop ()
    (let ((conn (read srv)))
      (if (port? conn)
          (thread-start!
           (make-thread
            (lambda ()
              (current-input-port conn)
              (current-output-port conn)
              (with-exception-catcher handle-debug-exception handler)
              (close-port conn))
            name))))))

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
          (lambda () (service-starter handler srv 'lwip-tcp-server))
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
     ((eqv? err ERR_OK)
      (receive
       (c s) (%%lwip-make-pipe)
       (let ((conn (make-tcp-connection #f s connection #f)))
         (tcp-connection-next-set!
          conn
          (make-thread
           (lambda ()
             (port-copy-to-lwip s conn)
             ;; (close-port s) done in close-tcp-connection
             ;; lwip-tcp-close done in close-tcp-connection
             (close-tcp-connection conn))
           'tcp-server))
         (%%tcp-context-set! connection conn)
         (write c (tcp-connection-srv-port ctx))
         (lwip/after-safe-return (thread-start! (tcp-connection-next conn))))))
     (else (lwip/after-safe-return (lwip-tcp-close connection)))))

  (define (on-tcp-sent ctx connection len)
    ;; signal conn free
    (let ((t (tcp-connection-next ctx)))
      (if t (thread-send t len)))
    ERR_OK)

  (define (on-tcp-receive ctx connection pbuf err)
    (cond
     ((not pbuf) ;; closed
      (lwip/after-safe-return (close-tcp-connection ctx)))
     ((eqv? err ERR_OK)
      (pbuf-add-reference! pbuf)
      (lwip/after-safe-return
       (let ((u8 (pbuf->u8vector pbuf))
             (s (tcp-connection-srv-port ctx)))
         (pbuf-release! pbuf)
         (let ((n (u8vector-length u8)))
           (tcp-received! connection n)
           (write-subu8vector u8 0 n s)
         (force-output s)))))
     (else ERR_IF)))

  (define (on-tcp-connect ctx connection err)
    (case err
      ((0)
       (let ()
         (tcp-connection-pcb-set! ctx connection)
         (tcp-connection-next-set!
          ctx
          (make-thread
           (lambda ()
             (port-copy-to-lwip (tcp-connection-srv-port ctx) ctx)
             ;; (close-port s) done in close-tcp-connection
             ;; lwip-tcp-close done in close-tcp-connection
             (close-tcp-connection ctx)
             (tcp-connection-next-set! ctx #f))
           'lwip-tcp-client))
         (lwip/after-safe-return (thread-start! (tcp-connection-next ctx)))))
      (else (debug 'on-tcp-connect err)))) ;; FIXME

  (define (on-tcp-poll ctx connection)
    (if (tcp-connection? ctx)
        (let ((t (tcp-connection-next ctx)))
          (if t (thread-send t 0))))
    ;; (debug 'lwip-tcp-flush! (lwip-err (lwip-tcp-flush! connection)))
    ERR_OK)

  (define (on-tcp-error ctx err)
    (cond
     ((tcp-connection? ctx)
      ;; The pcb is already freed when this callback is called!
      (tcp-connection-pcb-set! ctx #f)
      (close-tcp-connection ctx)))
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
