;; (C) 2020 JFW; copy and use at your own risk

;;** Debug Helpers

(define (handle-debug-exception e #!optional location)
  (let ((port (current-error-port)))
    (debug (or location 'EXN) e)
    (display-exception e port))
  #!void)

;;** Once Only Continuations

;; Avoid use of general `call-with-current-continuation` for once
;; continuations called once only.  Some compilers may have a better
;; idea than creating a full blown continuation.
;;
;; Then name is inherited from `bigloo`.

(define bind-exit call-with-current-continuation)

;;** Delayed Evaluation

(define (promise? obj) ;; widespread available, easy to support
  (##promise? obj))

;;** Lowlevel data access

(include "datastructures.scm")

;;** Lowlevel Port Operations

(include "~~lib/_gambit#.scm")

(define-macro (macro-output-port-closed? port)
  `(macro-closed? (macro-port-woptions ,port)))

(define-macro (macro-input-port-closed? port)
  `(macro-closed? (macro-port-roptions ,port)))

;;** Quasi Ports

(define-type quasi-port
  macros: prefix: macro-
  opaque:
  client-port
  server-port
  (writer unprintable:))

(define make-quasi-port
  (case-lambda
   ((read-port writer) ;; backward compatible; TBD: remove
    (macro-make-quasi-port read-port #f writer))
   ((client server writer) ;; TBD
    (macro-make-quasi-port client server writer))))

(define (quasi-port? obj) (macro-quasi-port? obj))

;;#| Backward compatible
(define (quasi-port-read-port port) (macro-quasi-port-client-port port))
(define (quasi-port-writer port) (macro-quasi-port-writer port))
;;#|

(define-macro (REDEFINE! formals . body)
  `(set! ,(car formals) (let ((,(car formals) ,(car formals))) (lambda ,(cdr formals) ,@body))))

(declare
 ;; currently overwriting port operations:
 (not standard-bindings
      close-port close-output-port close-input-port
      read-u8 read-subu8vector write-subu8vector write-substring
      force-output
      port?
      ))

(define (close-output-quasi-port port)
  ((quasi-port-writer port) #f 0 0 0 (lambda (buffer offset n mx continue) #t)))

(REDEFINE! (close-output-port port)
  (cond
   ((quasi-port? port) (close-output-quasi-port port))
   (else (close-output-port port))))

(define (close-input-quasi-port port)
  (close-port (quasi-port-read-port port)))

(REDEFINE! (close-input-port port)
  (cond
   ((quasi-port? port) (close-input-quasi-port port))
   (else (close-input-port port))))

(REDEFINE! (read-u8 #!optional (port (current-input-port)))
  (read-u8
   (cond
    ((quasi-port? port) (quasi-port-read-port port))
    (else port))))

(REDEFINE! (read-subu8vector vec start end #!optional (port (current-input-port)) (need #f))
  (let ((port (cond
               ((quasi-port? port) (quasi-port-read-port port))
               (else port))))
    (if need
        (read-subu8vector vec start end port need)
        (read-subu8vector vec start end port))))

(REDEFINE! (write-subu8subvector vec start end #!optional (port (current-output-port)))
  (cond
   ((quasi-port? port)
    ((quasi-port-writer out) port vec start (fx- end start) 0
     (lambda (buffer offset n mx continue) (fx- end start))))
   (else
    (if (macro-output-port-closed? port)
        -1
        (write-subu8vector vec start end port)))))

(REDEFINE! (write-substring str start end #!optional (port (current-output-port)))
  (cond
   ((quasi-port? port)
    (let* ((n (fx- end start))
           (vec (make-u8vector n)))
      (do ((i start (fx+ i 1))
           (j 0 (fx+ j 1)))
          ((fx= j n))
        (u8vector-set! vec j (char->integer (string-ref str i))))
      ((quasi-port-writer out) port vec 0 n 0
       (lambda (buffer offset n mx continue) (fx- end start)))))
   (else (write-substring str start end port))))

(define (close-quasi-port port)
  (close-output-quasi-port port)
  (close-input-quasi-port port))

(REDEFINE! (close-port port)
  (close-output-port port)
  (close-input-port port))

(REDEFINE! (force-output #!optional (port (current-output-port)) (level 0))
  (cond
   ((quasi-port? port))
   (else (force-output port level))))

(REDEFINE! (port? obj) (or (port? obj) (quasi-port? obj)))

;;** Utilitarian Garbage --  EXPORTed

;; u8-read-line2 IMPORTED BY httpproxy,[?] ; deprecated

(define (u8-read-line2 port separator #!optional (mxlen 128))
  ;; avoid character buffer which disturbs when using bulk io.
  (let ((in (make-string mxlen)))
    (do ((off 0 (+ off 1))
         (n (read-u8 port) (read-u8 port)))
        ((or (eqv? n separator) (eof-object? n))
         (substring in 0 off))
      ;; overflows at mxlength - is just a PoC
      (string-set! in off (integer->char n)))))

;; read-line/null-terminated IMPORTED BY socks

(define (read-line/null-terminated port #!optional (mxlen 128))
  (u8-read-line2 port 0))

;; send-packet-now! IMPORTED BY socks

(define (send-packet-now! packet conn)
  (if (let ((n (u8vector-length packet)))
        (fx= (write-subu8vector packet 0 n conn) n))
      (force-output conn)
      (error "lost on output connection" conn)))

;;** Connecting Ports

(define (port-copy-through in out #!optional (MTU 3000))
  ;; Copy `in` to `out` and close the input side of `in` and the
  ;; output side of `out` when EOF is reached on `in`.
  (let ((buffer (%allocate-u8vector MTU)))
    (input-port-timeout-set! in (socks-connect-timeout))
    (let loop ((buffer buffer)
               (offset 0)
               (n 0)
               (mtu MTU)
               (corout (and (quasi-port? out) (quasi-port-writer out))))
      (let ((n (read-subu8vector buffer offset mtu in 1)))
        (cond
         ((fx<= n 0)
          (close-input-port in)
          ;; (force-output out) ;; part of close??
          (close-output-port out))
         ((procedure? corout) (corout buffer offset n mtu loop))
         (else
          (if (fx= (write-subu8vector buffer 0 n out) n)
              (begin
                (force-output out)
                (unless (macro-output-port-closed? out)
                  (input-port-timeout-set! in (socks-data-timeout))
                  (loop buffer 0 0 mtu corout)))
              (error "lost on output connection" out))))))))

(define (close-input-port/no-exception port)
  (with-exception-catcher
   (lambda (exn)
     (cond-expand
      (debug
       (debug 'closing-input port)
       (handle-debug-exception exn 'close-input-port/no-exception))
      (else))
     #f)
   (lambda () (close-input-port port))))

(define (close-output-port/no-exception port)
  (with-exception-catcher
   (lambda (exn)
     (cond-expand
      (debug
       (debug 'closing-output port)
       (handle-debug-exception exn 'close-output-port/no-exception))
      (else))
     #f)
   (lambda () (close-output-port port))))

(define (port-pipe+close! in out #!optional (MTU 3000))
  (with-exception-catcher
   (lambda (exn)
     (cond-expand
      (debug (handle-debug-exception exn))
      (else))
     (close-input-port/no-exception in)
     (close-output-port/no-exception out)
     exn)
   (lambda () (port-copy-through in out MTU))))

(define (ports-connect! r0 w0 r1 w1 . ignored)
  ;; TBD (unless (null? ignored) (debug 'ports-connect!--additional: ignored))
  (let* ((job (lambda () (port-pipe+close! r0 w1)))
         (thr (thread-start! (make-thread job 'port-copy))))
    (port-pipe+close! r1 w0)
    (thread-join! thr)))

;;** Fix gambit lists

(include "srfi-1.scm")

;;#| eof
