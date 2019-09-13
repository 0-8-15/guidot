;; Helpers (maybe to be moved elsewhere)

(include "hook.scm")

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
	  ;(debug 'ct (current-thread))
	  (debug (list (current-thread) 'unlocking) m)
	  (apply o m args))))
;|#

(include "scsh-utils.scm")

(define (backup-file-name)
  (make-pathname
   (cond-expand
    (android (system-directory))
    (else "."))
   "backup.tar.gz"))

(define (kernel-backup!
         #!key
         (to (backup-file-name))
         (from kernel-data-directory))
  (define excludes-file
    (make-pathname
     (cond-expand
      (android (system-appdirectory))
      (else "."))
     "excluding"))
  (define excludes "spool.db
spool.db-journal
")
  (and (not (file-exists? to))
       (file-exists? from)
       (dynamic-wind
           (lambda ()
             (call-with-output-file excludes-file
               (lambda (p) (display excludes p))))
           (lambda ()
             (run/boolean "tar" "-c" "-z" "-f" to "-X" excludes-file "-C" from "."))
           (lambda ()
             (delete-file excludes-file)))))

(define (kernel-restore!
         #!key
         (from (backup-file-name))
         (to kernel-data-directory))
  (and (not (file-exists? to))
       (file-exists? from)
       (begin
         (create-directory to)
         (run/boolean "tar" "-x" "-z" "-f" from "-C" to))))

(define (kernel-remove!
         #!key
         (from kernel-data-directory))
  (and (file-exists? from)
       (begin
         (log-status "Will remove kernel data from " from)
         (when (kernel-server)
               (log-status "Stopping kernel")
               (stop-kernel-server!))
         (run/boolean "rm" "-rf" from))))

;; Kernel Control

(define kernel-on-start (make-hook 0))

;; Async send with no reply expected
(define (kernel-control-send! msg . args)
  (let ((to ballcontrol#kernel-control-thread))
    (if (null? args)
        (thread-send to msg)
        (thread-send to (cons msg args)))))

(define (kernel-control-call immediately key payload)
  (let ((m (make-mutex key)))
    (mutex-lock! m #f #f)
    (mutex-specific-set! m payload)
    (thread-send ballcontrol#kernel-control-thread m)
    (if immediately
        (begin
          (mutex-lock! m #f #f)
          ((mutex-specific m)))
        (let ((r #f) (m m))
          (lambda args
            (if r (r)
                (let ((tmo (if (pair? args) (car args) #f)))
                  (if (mutex-lock! m tmo #f)
                      (begin
                        (set! r (mutex-specific m))
                        (set! m #f)
                        (r))
                      (let ((default-provided? (and (pair? args) (pair? (cdr args)))))
                        (if default-provided? (cadr args)
                            (error "timeout waiting for reply from kernel control thread")))))))))))

(define (call-kernel/promise . msg)
  (kernel-control-call #f 'call-kernel msg))

(define (call-kernel . msg)
  (kernel-control-call #t 'call-kernel msg))

(define (kernel-send-stop!) (kernel-control-send! 'stop!))

(define (close-kernel-connection!) (kernel-control-send! 'close!))

#;(define (write-kernel! msg)
(kernel-send! (cons 'send! msg)))

(namespace
 ("ballcontrol#"
  kernel-control-thread
  get-kernel-connection
  %call-kernel))

(define string->socket-address abstract-address->socket-address)

(define (control-socket-path)
  (make-pathname kernel-data-directory "control"))

(define enable-removal-of-control-socket #f) ; NOT a good idea!

(define (possibly-remove-control-socket! #!optional (enforce enable-removal-of-control-socket))
  (if enforce
      (let ((f (control-socket-path)))
	(if (file-exists? f) (delete-file (debug 'removing f))))))

(define kernel-control-thread)
(let ((tmo (vector #!eof))
      (check-period 60)
      (retry-period 1)
      (wait #f)
      (conn #f))
  (define (bailout msg)
    (error "kernel-control-thread: unhandled message" msg))
  (define (conn-set! v)
    (set! conn v)
    (set! wait (if conn check-period retry-period)))
  (define (close-kernel-connection!)
    (if conn (close-port conn))
    (conn-set! #f))
  (define (idle)
    (when conn (kernel-send-idle0)))
  (define (return-connected)
    (if conn (lambda () #t) (lambda () #f)))
  (include "kconn.scm")
  (define (dispatch msg)
    (cond
     ((eq? msg 'idle) (idle))
     ((eq? msg 'close!) (close-kernel-connection!))
     ((eq? msg 'stop!)
      (when (debug 'conn conn)
            (%write-kernel! conn '("stop" "-f"))
            (thread-sleep! 1))
      (dispatch 'close!))
     ((pair? msg)
      (cond
       ((procedure? (car msg))
        )
       (else
        (case (car msg)
          ((call-kernel) (call-kernel2 (cdr msg)))
          ((send!) (%%write-kernel! (cdr msg)))
          ((connect)
           (if (rep-exists?)
               (if (not conn)
                   (conn-set! (get-kernel-connection))))
           (return-connected))
          (else (bailout msg))))))
     (else (bailout msg))))
  (define (dispatch/enx-handled msg)
    (with-exception-catcher
     (lambda (exn) (lambda () (raise exn)))
     (lambda () (dispatch msg))))
  (define (handle-one)
    (let ((msg (if wait (thread-receive wait tmo) (thread-receive))))
      ;; (debug 'MSG msg)
      (cond
       #;((not conn)
        (conn-set! (get-kernel-connection))
        )
       ((mutex? msg)
        (mutex-specific-set! msg (dispatch/enx-handled (cons (mutex-name msg) (mutex-specific msg))))
        (mutex-unlock! msg))
       ((eq? msg tmo) (dispatch 'idle))
       (else (dispatch msg)))))
  (define (exn-handler ex)
    (log-error (thread-name (current-thread)) ": " (debug 'EXN (exception->string ex))))
  (define (loop)
    (with-exception-catcher exn-handler handle-one)
    (loop))
  (set! kernel-control-thread (thread-start! (make-thread loop 'kernel-control-job))))

(define (kernel-start-idle-handler!)
  (call-kernel
   'begin
   '(define handle-idle-event!
      (let ((sig #f))
        (define (keepalive)
          (thread-sleep! 180)
          (let ((pid (current-process-id)))
            (logerr "Keepalive timeout in ~a\n" pid)
            (process-signal pid 15)))
        (lambda ()
          ;; (logerr "EVENT_IDLE\n")
          (if sig (thread-terminate! sig))
          #;(set! sig (thread-start! keepalive))
          #t)))
   '(logerr "I: installed idle-event-handler\n")
   '(handle-idle-event!)))

(hook-add! kernel-on-start kernel-start-idle-handler!)

(define kernel-data-directory
  (cond
   (app:android?
    (make-pathname (system-appdirectory) "data"))
   (else "data")))

(define (rep-exists?)
  (and
   (file-exists? kernel-data-directory)
   (eq? (file-info-type (file-info kernel-data-directory)) 'directory)))

(define kernel-server
  (let ((process #f)
	(mux (make-mutex 'ks)))
    (define (cleanup!)
      (set! process #f)
      (possibly-remove-control-socket! #;(and (not-using-fork-alike) srv)))
    (lambda args
      (dynamic-wind
	  (lambda () (mutex-lock! mux))
	  (if (pair? args)
	      (lambda ()
		(if (and (null? (cdr args)) (boolean? (car args)))
		    (let ((check-only (car args))
                          (srv process))
                      (cond
                       ((port? srv)
                        (process-status srv) (cleanup!) srv)
                       ((and srv (eq? (subprocess-style) 'fork))
                        (receive
                         (c n p) (process-wait srv)
                         (if (or (= p srv)
                                 (= p -1))
                             (begin
                               (if (and (= p -1) (= c errno/child))
                                   (log-error "failed to wait for process " srv " ECHILD: gambit stole the value" ))
                               (cleanup!)
                               (list c n p))
                             #f)))
                       (else
                        ;; FIXME: always wait for it!
                        (log-error "kernel-server unhandled waiting case (not forking)")
                        #f)))
		    (begin
		      (if process
                          (log-error "kernel already running")
                          (set! process (apply (car args) (cdr args))))
		      process)))
	      (lambda () process))
	  (lambda () (mutex-unlock! mux))))))

(define (start-kernel-server!)
  (cond
   ((not (rep-exists?))
    (log-error "data directory does not exist" kernel-data-directory))
   ((and (not (kernel-server))
         (not (check-kernel-server!)))
    (foreground-service! #t)
    (kernel-server fork-process "ball" "-start" kernel-data-directory))
   ((and (kernel-server #t)
         (not (check-kernel-server!)))
    (log-error "Kernel died at some time, retrying")
    (start-kernel-server!))
   (else (log-error "Not starting kernel server, still " (kernel-server)))))

(define restart-kernel-server!
  (let ((in-restart #f))
    (lambda ()
      (if (eq? (subprocess-style) 'pthread)
          (begin
            (log-error "can not (yet) restart kernel when running as pthread")
            #f)
          (if (and (not in-restart) (rep-exists?))
              (begin
                (set! in-restart #t)
                (when (debug 'current-server (kernel-server))
                      (unless (kernel-server-kill! "-USR2") ;; report alive state
                              (log-error "failed to send signal to kernel"))
                      (let loop ((n 200))
                        (unless (kernel-server #f)
                                (thread-sleep! 0.1)
                                (loop (- n 1)))))
                (unless (kernel-server) (start-kernel-server!))
                (let ((r (wait-for-kernel-server 60)))
                  (set! in-restart #f))
                (or r
                    (begin
                      (log-error "Kernel server did not restart!")
                      #f))))))))

(define (restart-kernel-and-custom-services!)
  (define (restart-kernel-and-custom-services-task)
    (and (restart-kernel-server!)
         (hook-run kernel-on-start)))
  (thread-start! (make-thread restart-kernel-and-custom-services-task 'restart))
  (thread-yield!)
  #t)

(define (kernel-server-kill! #!optional (sig "-TERM"))
  (and (eq? (subprocess-style) 'fork)
       (let ((pid (kernel-server)))
         (and (number? pid) (run/boolean "kill" sig pid)))))

(define (stop-kernel-server!0)
  (foreground-service! #f)
  (kernel-send-stop!)
  #;(let loop ((srv (kernel-server)) (n 20))
    (when
     srv
     (unless
      (kernel-server #f)
      (thread-sleep! 0.1)
      (kernel-server-kill! "-USR1")
      (when (< n 0)
            (kernel-server-kill! (if (< n -10) "-KILL" "-TERM")))
      (loop (kernel-server) (- n 1))))
    #;(when (and (not-using-fork-alike) (debug 'kernel-server srv))
	  (thread-sleep! 5)
	  (terminate))))

(set! stop-kernel-server! stop-kernel-server!0)

(define (check-kernel-server!)
  (kernel-control-call #t 'connect #f))

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
