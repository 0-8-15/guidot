;; Server connection

;; This file is included within a let environment.
;;
;; Bound in the let are:
;;
;; tmo: a local distinct constant to tag timeout values
;;
;; wait: #f or number: timeout to wait for next message
;;
;; conn: (TBD) the connection to use with the kernel

(define (get-kernel-connection)
  (let ((addr (string->socket-address (control-socket-path)))
        (sock (create-socket protocol-family/unix socket-type/stream)))
    (with-exception-catcher
     (lambda (ex)
       #;(debug 'Check-EX (exception-->printable ex))
       #;(start-kernel-server!)
       ;; (log-debug "connecting to control socket failed with " 1 (exception-->printable ex))
       (close-socket sock)
       #f)
     (lambda ()
       ;; (log-debug "trying to connect to control socket " 1)
       (connect-socket sock addr)
       (log-debug "connected to control socket " 1)
       (socket-port sock)))))

(define with-ball-kernel
  (let ((catching
         (lambda (ex)
           ;; (close-kernel-connection!)
           #;(raise ex)
           (list #!eof)))
        (catch-clean
         (lambda (ex)
           (log-error "kernel control connection change failed.\n Setting to #f." ex)
           (conn-set! (debug 'Clean #f))
           #;(raise ex)
           #f)))
    (lambda (proc #!optional (values values) (mode 'wait))
      (let ((v conn))
        (case mode
          ((set)
           (let ((nv (with-exception-catcher
                      catch-clean
                      (lambda () (proc v)))))
             (conn-set! nv)
             (values nv)))
          (else
           (apply
            values
            (with-exception-catcher
             catching
             (lambda ()
               (receive
                results (proc v)
                results))))))))))

(define (kernel-send-idle0 restart)
  (if (eq? (with-exception-catcher
            (lambda (ex) (log-error "exn in idle talk " (exception-->printable ex))  #f)
            (lambda () (call-kernel 'begin '(handle-idle-event!))))
           #t)
      #t
      (begin
        (log-error "Kernel failed to answer idle notification.")
        (close-kernel-connection!)
        (if restart (restart))
        #f)))

(define (%write-kernel! p msg)
  (write msg p)
  (force-output p))

(define (%%write-kernel! msg)
  (with-ball-kernel
   (lambda (p)
     (unless (port? p) (error "no connection"))
     (%write-kernel! p msg))))

(define (call-kernel . msg)
  (with-ball-kernel
   (lambda (p)
     (unless (port? p) (error "no connection"))
     (%write-kernel! p msg)
     (read p))
   (lambda (ans)
     (if (eof-object? ans)
	 (begin
	   (close-kernel-connection!)
	   ans)
	 (case (car ans)
	   ((E) (error (cdr ans)))
	   ((D) (apply values (cdr ans)))
	   (else (error "protocol error" msg)))))))

(define (call-kernel2 msg)
  (with-ball-kernel
   (lambda (p)
     (unless (port? p) (error "no connection"))
     (%write-kernel! p msg)
     (read p))
   (lambda (ans)
     (if (eof-object? ans)
	 (begin
	   (close-kernel-connection!)
	   (lambda () ans))
	 (lambda ()
           (case (car ans)
             ((E) (error (cdr ans)))
             ((D) (apply values (cdr ans)))
             (else (error "protocol error" msg))))))))

