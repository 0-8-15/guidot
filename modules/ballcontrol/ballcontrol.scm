;; Helpers (maybe to be moved elsewhere)

(define getpid (c-lambda () int "getpid"))

(define this-pid (getpid))

(define (debug l v)
  (let ((p  (current-error-port)))
    (display this-pid p)
    (display " " p)
    (display l p)
    (display ": " p)
    (write v p)
    (newline p)
    (force-output p)
    v))

(define (exception-->printable exc)
  (if (os-exception? exc)
      (list 'OS-EXCEPTION (os-exception-procedure exc)
	    (os-exception-arguments exc)
	    (os-exception-code exc)
	    (err-code->string (os-exception-code exc))
	    (os-exception-message exc))
      exc))

#|
(let ((o mutex-lock!))
  (set! mutex-lock!
	(lambda (m . args)
	  (debug 'ct (current-thread))
	  (debug 'locking m)
	  (apply o m args))))

(let ((o mutex-unlock!))
  (set! mutex-unlock!
	(lambda (m . args)
	  (debug 'ct (current-thread))
	  (debug 'unlocking m)
	  (apply o m args))))
|#

;; Server connection

(define with-ball-kernel
  (let ((conn (make-mutex 'kernel)))
    (mutex-specific-set! conn #f)
    (let ((catching
	   (lambda (ex)
	     (mutex-unlock! conn)
	     #;(raise ex)
             #!eof))
	  (catch-clean
	   (lambda (ex)
	     (log-error "kernel control connection change failed.\n Setting to #f." ex)
	     (mutex-specific-set! conn (debug 'Clean #f))
	     (mutex-unlock! conn)
	     #;(raise ex)
             #!eof)))
      (lambda (proc #!optional (values values) (mode 'wait))
	(mutex-lock! conn)
	(let ((v (mutex-specific conn)))
	  (case mode
	    ((set)
	     (let ((nv (with-exception-catcher
			catch-clean
			(lambda () (proc v)))))
	       (mutex-specific-set! conn nv)
	       (mutex-unlock! conn)
	       (values (and nv #t))))
	    (else
	     (apply
	      values
	      (with-exception-catcher
	       catching
	       (lambda ()
		 (receive
		  results (proc v)
		  (mutex-unlock! conn)
		  results)))))))))))

(define (close-kernel-connection!)
  (with-ball-kernel
   (lambda (p) (if p (close-port p)) #f)
   (lambda (x) x)
   'set))

(define string->socket-address abstract-address->socket-address)

(define kernel-data-directory
  (cond
   (app:android?
    (make-pathname (system-directory) "data"))
   (else "data")))

(define (rep-exists?)
  (and
   (file-exists? kernel-data-directory)
   (eq? (file-info-type (file-info kernel-data-directory)) 'directory)))

(define (control-socket-path)
  (make-pathname kernel-data-directory "control"))

(define enable-removal-of-control-socket #f) ; NOT a good idea!

(define (possibly-remove-control-socket! #!optional (enforce enable-removal-of-control-socket))
  (if enforce
      (let ((f (control-socket-path)))
	(if (file-exists? f) (delete-file (debug 'removing f))))))

(define kernel-server
  (let ((process #f)
	(mux (make-mutex 'ks)))
    (lambda args
      (dynamic-wind
	  (lambda () (mutex-lock! mux))
	  (if (pair? args)
	      (lambda ()
		(if (and (null? (cdr args)) (not (car args)))
		    (let ((srv process))
		      (set! process #f)
		      ;; FIXME: always wait for it!
		      (if (port? srv) (process-status srv))
		      (possibly-remove-control-socket! #;(and (not-using-fork-alike) srv))
		      srv)
		    (begin
		      (when process (error "kernel already running"))
		      (set! process (apply (car args) (cdr args)))
		      process)))
	      (lambda () process))
	  (lambda () (mutex-unlock! mux))))))

(define (start-kernel-server!)
  (cond
   ((not (rep-exists?))
    (log-error "data directory does not exist" kernel-data-directory))
   ((not (kernel-server))
    (kernel-server fork-process "ball" "-start" kernel-data-directory))
   (else (log-error "Not starting kernel server, still " (kernel-server)))))

(define (restart-kernel-server!)
  (unless
   (eq? (subprocess-style) 'pthread)
   (when (debug 'current-server ((debug 'retrieving-current-server-via kernel-server))) (debug 'cleanedUp (kernel-server #f)))
   (debug 'closedConnection (close-kernel-connection!))
   (debug 'started (start-kernel-server!))
   (or (wait-for-kernel-server 20)
       (log-error "Kernel server did not restart!"))))

(define (%write-kernel! p msg)
  (write msg p)
  (force-output p))

(define (write-kernel! msg)
  (with-ball-kernel
   (lambda (p)
     (unless (port? p) (error "no connection"))
     (%write-kernel! p msg))))

#;(define (expect-from-kernel p)
  (let ((t (make-thread (lambda () (read-from-kernel p)) 'kernel-read)))
    (thread-start! t)
    (thread-yield!)
    t))

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
	   (else (error "protocol error")))))))

(define (stop-kernel-server!0)
  (with-ball-kernel
   (lambda (p)
     (when
      p
      (%write-kernel! p '("stop" "-f"))
      (close-port p)
      #f))
   values 'set)
  (let ((srv (kernel-server)))
    (when srv (kernel-server #f))
    #;(when (and (not-using-fork-alike) (debug 'kernel-server srv))
	  (thread-sleep! 5)
	  (terminate))))

(set! stop-kernel-server! stop-kernel-server!0)

(define (check-kernel-server!)
  (with-ball-kernel
   (lambda (p)
     (if p p
	 (and
	  (rep-exists?)
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
	       ;; (log-debug "connect to control socket " 1)
	       (socket-port sock)))))))
   values
   'set))

(define (wait-for-kernel-server limit)
  (let loop ((w 0))
    (cond
     ((not (kernel-server)) #f)
     ((check-kernel-server!) #t)
     ((>= w limit) #f)
     (else
      (thread-sleep! 0.5)
      (loop (+ w 1))))))

;; forking and replacements

(define *subprocess-style*
  (let ((pf (system-platform)))
    (cond
     ((or
       (member pf '(#;"linux" #;"android" "ios" "win32"))
       )
      'pthread)
     ((member pf '("linux" "android"))
      'fork)
     (else 'semi-fork))))

(define (subprocess-style) *subprocess-style*)

(define (not-using-fork-alike)
  (case (subprocess-style)
    ((pthread) #t)
    (else #f)))

(define (fork-process cmd . args)
  (case (subprocess-style)
    ((semi-fork) (semi-fork cmd args))
    ((pthread)
     (unless (equal? cmd "ball") (error "no pthread support for command" cmd))
     (log-debug "Starting pthread for " 1 cmd " on " args)
     (ballroll-pthread args))
    ((fork) (fork-and-call cmd args))
    (else (error "internal error, unknown subprocess style"))))

(define (semi-fork cmd args)
  ;; (debug 'semi-fork `(,cmd . ,args))
  (log-debug "semi-fork " 1 cmd " on " args)
  (open-process `(path: ,(system-cmdargv 0) arguments: ("-s" ,cmd . ,args)))
  )

(define *registered-commands* '())

(define (register-command! cmd procedure)
  (set! *registered-commands* `((,cmd . ,procedure) . ,*registered-commands*)))

(c-declare
 #<<end-of-c-declare
 #include <unistd.h>
 static int fork_and_close_fds(int from)
 {
#ifndef _WIN32
  pid_t pid = fork();
  int fd ;

  if(pid == -1) return -1;
  if(pid == 0) {
    fd = sysconf(_SC_OPEN_MAX) - 1;
    while(fd >= from) close(fd--); /* ignoring errors */
     return 0;
  }
  return pid;
#else
  return -1;
#endif
}

 #include <sys/types.h>
 #include <sys/stat.h>
 #include <fcntl.h>
 #include <stdio.h>
 static int log_to_file(char *fn)
 {
  if(freopen(fn, "a", stdout) == NULL) return 1;
  if(freopen(fn, "a", stderr) == NULL) return 2;
  // if(fprintf(stderr, "%s", "\r\nredirected output into this file\r\n")<0) return 3;
  // if(fflush(stderr)!=0) return 4;
  return 0;
 }
end-of-c-declare
)

#;(define (redirect-failed! fn)
  (with-output-to-file (list path: (make-pathname (system-directory) "errors")
			     append: #t)
    (lambda ()
      (display (time->seconds (current-time)))
      (display " redirect failed for file ")
      (display fn)
      (newline)))
  (exit 23))

(define (redirect-standard-ports-for-logging)
  (if log:on
      (let* ((clfn (if #t log:file
                       (string-append log:file ".C.txt")))
             (rc ((c-lambda (char-string) int "log_to_file") clfn)))
        (if (eq? rc 0)
            (begin
              (current-error-port (##open-predefined 2 clfn 2))
              (current-output-port (##open-predefined 2 clfn 1)))
            (log-error "redirect failed rc " rc " to file " clfn)))
      #;(if (member (system-platform) '("android"))
          (unless
           ((c-lambda (char-string) bool "log_to_file") "/dev/null")
           (log-error "redirect failed to file " "/dev/null")))))

(define (fork-and-call cmd args)
  (let ((e (assoc cmd *registered-commands*)))
    (if e
	(let ((pid ((c-lambda (int) int "fork_and_close_fds") 3)))
	  (case pid
	    ((-1) (log-error "fork failed") #f) ;; TODO: include errno
	    ((0)
	     ;; ((c-lambda () void "microgl_close"))
	     ;; (redirect-standard-ports-for-logging)
	     (exit ((cdr e) args)))
	    (else
	     (log-status "Kernel running as PID " pid)
	     pid)))
	(error "not procedure registered for command " cmd))))

(define (system-command-line* offset)
  (let loop ((n (system-cmdargc)) (r '()))
    (if (< n offset) r
	(let ((i (- n 1)))
	  (loop i (cons (system-cmdargv i) r))))))

(define (execute-registered-command)
  (let ((exe (system-cmdargv 0))
	(cmd (system-cmdargv 2)))
    (let ((e (assoc cmd *registered-commands*)))
      (if e ;; (debug 'CmdEntry e)
	  (exit ((cdr e) (system-command-line* 4)))
	  (exit 1)))))

;; eof
