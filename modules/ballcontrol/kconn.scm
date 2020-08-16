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
  (with-exception-catcher
   (lambda (exn)
     ;; (log-debug "FAILED to connected to control socket " 1 exn)
     #f)
   (lambda ()
     (let ((port (open-unix-client (ball-string->socket-address (control-socket-path)))))
       (if (port? port)
           (log-debug "connected to control socket " 1))
       port))))

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
            (lambda () (call-kernel 'begin `(handle-idle-event! ,(log-ballcontrol)))))
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

(define (kc-connection-lost-reply)
  (error "lost connection"))

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
	   kc-connection-lost-reply)
	 (lambda ()
           (case (car ans)
             ((E) (error (cdr ans)))
             ((D) (apply values (cdr ans)))
             (else (error "protocol error" msg))))))))

