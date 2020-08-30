;; Helpers (maybe to be moved elsewhere)

(cond-expand
 (android
  (define (handle-replloop-exception e)
    (cond
     ((unbound-global-exception? e)
      (println port: (current-error-port) "Unbound variable " (unbound-global-exception-variable e)))
     (else (##default-display-exception e (current-error-port))))
    #!void))
 (else #f))

(include "~~lib/onetierzero/src/observable-notational-conventions.scm")

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
  (cond
   ((os-exception? exc)
    (list 'OS-EXCEPTION (os-exception-procedure exc)
	  (os-exception-arguments exc)
	  (os-exception-code exc)
	  (err-code->string (os-exception-code exc))
	  (os-exception-message exc)))
   ((unbound-global-exception? exc)
    (println port: port "Unbound variable " (unbound-global-exception-variable exc)))
   (else exc)))

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
  (if (file-exists? to)
      (error "backup file exists already" to))
  (and (not (file-exists? to))
       (file-exists? from)
       (dynamic-wind
           (lambda ()
             (call-with-output-file excludes-file
               (lambda (p) (display excludes p))))
           (lambda ()
             (log-status "Creating backup in " to " from " from)
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

(define kernel-on-init (make-hook 0))

(define (kernel-on-start-add! key expression)
  (define (update-config-db-script! key expression)
    `(let ((db (sqlite3-open (make-pathname (spool-directory) "config" "db"))))
       (sqlite3-exec db "create table if not exists ExtensionCode (name text primary key, code text)")
       (sqlite3-exec db "create view if not exists Extensions as select code from ExtensionCode")
       (sqlite3-exec db "insert or replace into ExtensionCode values(?1, ?2)"
                     ,key ,(with-output-to-string (lambda () (pretty-print expression))))
       (sqlite3-close db)
       ))
  (call-kernel 'begin (update-config-db-script! key expression)))

(define (kernel-start-custom-services!)
  (call-kernel 'begin '(reload-virtual-hosts! eval (spool-directory))))

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

(define (kernel-control-set-running! flag) (kernel-control-send! 'set-running flag))

(define (close-kernel-connection!) (kernel-control-send! 'close!))

#;(define (write-kernel! msg)
(kernel-send! (cons 'send! msg)))

(namespace
 ("ballcontrol#"
  kernel-control-thread
  get-kernel-connection
  %call-kernel))

(c-declare "
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#ifdef _WIN32
# include <winsock2.h>
# include <ws2tcpip.h>
// # include <afunix.h>

/*
 * MinGW does not have sockaddr_un (yet)
 */

# ifndef UNIX_PATH_MAX
#  define UNIX_PATH_MAX 108
struct sockaddr_un {
  ADDRESS_FAMILY sun_family;
  char sun_path[UNIX_PATH_MAX];
};
# endif
#else
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/un.h>
#endif
")

(c-declare
#<<end-of-c-declare
#if defined(__ANDROID__) || defined(_WIN32) || defined(__linux__)
#include <stddef.h>
static socklen_t set_socket_name(struct sockaddr_un *socket_name, const char *filename)
{
  size_t fn_length = strlen(filename), off=1;
  const char prefix[] = "FIXchange";
  const char suffix[] = "FIXMESHOULDbechanged";
  size_t total = sizeof(socket_name->sun_path)-1;
  memset(socket_name->sun_path, (42 + 23), sizeof(socket_name->sun_path));
  socket_name->sun_path[0] = '\0';
  strncpy(socket_name->sun_path+off, prefix, total);
  total -= sizeof(prefix);
  off += sizeof(prefix);
  strncpy(socket_name->sun_path+off, filename, total);
  total -= fn_length;
  off += fn_length;
  strncpy(socket_name->sun_path+off, suffix, total);
  off += sizeof(suffix);
  off += offsetof(struct sockaddr_un, sun_path);
  // return off < sizeof(socket_name->sun_path) ? off : sizeof(socket_name->sun_path);
  return sizeof(struct sockaddr_un);
}
#else
static socklen_t set_socket_name(struct sockaddr_un *socket_name, const char *filename)
{
  strncpy (socket_name->sun_path, filename, sizeof (socket_name->sun_path));
  socket_name->sun_path[sizeof (socket_name->sun_path) - 1] = '\0';
  return SUN_LEN(socket_name);
}
#endif
end-of-c-declare
)

(define (ball-abstract-address->socket-address fn)
  (if (>= (string-length fn) unix-path-max) ;; FIXME this MUST exclude prefix and suffix and leading '\0'
      (error "abstract-address->socket-address: path too long" fn))
  (let ((addr (make-u8vector SOCKADDR_UN_SIZE)))
    ((c-lambda (scheme-object nonnull-char-string) void "
struct sockaddr_un *sa_un = ___BODY(___arg1);
sa_un->sun_family = AF_UNIX;
set_socket_name(sa_un, ___arg2);
") addr fn)
    addr))

(define ball-string->socket-address ball-abstract-address->socket-address)

(define (control-socket-path)
  (make-pathname kernel-data-directory "control"))

(define enable-removal-of-control-socket #f) ; NOT a good idea!

(define (possibly-remove-control-socket! #!optional (enforce enable-removal-of-control-socket))
  (if enforce
      (let ((f (control-socket-path)))
	(if (file-exists? f) (delete-file (debug 'removing f))))))

(define log-ballcontrol
  (let ((on #f))
    (lambda args
      (cond
       ((and (pair? args) (boolean? (car args)) (null? (cdr args)))
        (set! on (car args)))
       ((null? args) on)
       (on
        (apply log:submit (cons "ballcontrol" args)))))))

(log-ballcontrol #t)

(define ball-kernel-enabled:=
  (make-pin
   initial: #f
   pred: boolean?
   filter: (lambda (o n) (and n (rep-exists?))) ;; enable restricted
   name: "enable ball kernel"))

(define ball-kernel-up??
  (make-pin initial: #f name: "ball kernel seen up"))

#;(define ball-kernel-up??
  (make-pin initial: #f filter: (lambda (o n) #f)))

(define (ball-kernel-up:= v)
  (kick! (lambda () (ball-kernel-up?? (and v #t)))))

(define kernel-control-thread)
(let ((tmo (vector #!eof))
      (kernel-supposed-to-run #f)
      (check-period 30)
      (retry-period 1)
      (wait #f)
      (conn #f))
  (define (bailout msg)
    (error "kernel-control-thread: unhandled message" msg))
  (define (conn-set! v)
    (set! conn v)
    (ball-kernel-up:= v)
    (set! wait (if conn check-period retry-period)))
  (define (close-kernel-connection!)
    (if conn (close-port conn))
    (conn-set! #f))
  (define (retry-to-connect times #!key (sleep 0.5))
    (let loop ((n times))
      (if (> n 0)
          (unless conn
            (conn-set! (get-kernel-connection))
            (unless conn
                    (let ((slptm sleep))
                      ;;(log-ballcontrol "try-restart: sleeping " slptm)
                      (thread-sleep! slptm)
                      ;;(log-ballcontrol "try-restart: woke up from retry #" n)
                      (loop (- n 1)))))))
    conn)
  (define (try-restart)
    (log-ballcontrol "try-restart: connection " conn)
    ;;(run/boolean "ps" "-a")
    (unless conn (retry-to-connect 10)
            (log-ballcontrol "re-establisch connection?: " conn)
            (unless conn (restart-kernel-and-custom-services!))))
  (define (idle)
    (if conn
        (kernel-send-idle0 (and kernel-supposed-to-run try-restart))
        (if kernel-supposed-to-run (try-restart))))
  (define (return-connected)
    (if conn (lambda () #t) (lambda () #f)))
  (include "kconn.scm")
  (define (dispatch msg)
    (cond
     ((eq? msg 'idle) (idle))
     ((eq? msg 'close!) (close-kernel-connection!))
     ((eq? msg 'stop!)
      (set! kernel-supposed-to-run #f)
      (when conn
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
          ((set-running) (set! kernel-supposed-to-run (cadr msg)))
          (else (bailout msg))))))
     (else (bailout msg))))
  (define (dispatch/enx-handled msg)
    (with-exception-catcher
     (lambda (exn) (lambda () (raise exn)))
     (lambda () (dispatch msg))))
  (define (handle-one)
    ;; (log-ballcontrol "handle-one: " (current-thread) " waiting " wait " for message")
    (define t0 (current-second))
    (let ((msg (if wait (thread-receive wait tmo) (thread-receive))))
      (define t1 (current-second))
      (if (and wait (> (- t1 t0) (* 1.2 wait)))
          (log-error "thread-receive timeout " wait " missed, waited for " (- t1 t0)))
      ;; (log-ballcontrol "handle-one: got " msg)
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
    (log-error (thread-name (current-thread)) ": " (debug 'EXN (exception-->printable ex))))
  (define (loop)
    (with-exception-catcher exn-handler handle-one)
    (loop))
  (set! kernel-control-thread (thread-start! (make-thread loop 'kernel-control-job))))

(define (kernel-start-idle-handler!)
  (kernel-on-start-add!
   "idle-handler"
   `(define handle-idle-event!
      (let ((sig #f))
        (define (keepalive)
          (thread-sleep! 180)
          (let ((pid (current-process-id)))
            (logerr "Keepalive timeout in ~a\n" pid)
            (process-signal pid 15)))
        (lambda (verbose)
          (if verbose (logerr "EVENT_IDLE\n"))
          (if sig (thread-terminate! sig))
          #;(set! sig (thread-start! keepalive))
          #t)))))

(hook-add! kernel-on-init kernel-start-idle-handler!)

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
      (ball-kernel-up:= #f)
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
                               (if (and (= p -1))
                                   (log-error "failed to wait for process " srv (if  (= c ((c-lambda () int "___return(ECHILD);"))) " ECHILD: gambit stole the value" "") ))
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
                          (begin
                            (set! process (apply (car args) (cdr args)))
                            #;(ball-kernel-up:= #t)))
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
    ;; (apply run/boolean `("rm" . ,(map (lambda (x) (make-pathname kernel-data-directory x)) '("control"))))
    (kernel-server fork-process (and #t 'daemon) "ball" #;"-:r" "-start" kernel-data-directory)
    (kernel-control-set-running! #t))
   ((and (kernel-server #t)
         (not (check-kernel-server!)))
    (log-error "Kernel died at some time, retrying")
    (restart-kernel-and-custom-services!))
   (else (log-error "Not starting kernel server, still " (kernel-server)))))

(define restart-kernel-server!
  (let ((in-restart #f))
    (lambda ()
      (log-status "In " (current-thread) " in-restart: " in-restart)
      (with-exception-catcher
       (lambda (ex)
         (log-error (thread-name (current-thread)) ": " (exception-->printable ex)))
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
                                 (log-error "failed to send USR2 signal to kernel"))
                         (let loop ((n 200))
                           (unless (kernel-server #f)
                                   (thread-sleep! 0.1)
                                   (loop (- n 1)))))
                   (unless (debug 'Stillrunning??? (kernel-server)) (start-kernel-server!))
                   (or (let ((r (wait-for-kernel-server 60)))
                         (set! in-restart #f)
                         r)
                       (begin
                         (log-error "Kernel server did not restart!")
                         #f)))
                 #t)))))))

(define (restart-kernel-and-custom-services!)
  (thread-start! (make-thread restart-kernel-server! 'restart))
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

(define ball-kernel-found-up-on-startup (check-kernel-server!))

(if ball-kernel-found-up-on-startup
    (kick! (lambda () (ball-kernel-enabled:= #t) (ball-kernel-up?? #t))))

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

(define (fork-process watchdog cmd . args)
  (case (subprocess-style)
    ((semi-fork) (semi-fork cmd args))
    ((pthread)
     (unless (equal? cmd "ball") (error "no pthread support for command" cmd))
     (log-debug "Starting pthread for " 1 cmd " on " args)
     (ballroll-pthread args))
    ((fork) (fork-and-call watchdog cmd args))
    (else (error "internal error, unknown subprocess style"))))

(define (semi-fork cmd args)
  ;; (debug 'semi-fork `(,cmd . ,args))
  (log-debug "semi-fork " 1 cmd " on " args)
  (cond-expand
   (android
    (let ((datadir
           (jscheme-eval
            `(let* ((app ,(android-app-class))
                    (this ((method "me" app)))
                    )
               (let (
                     (getApplicationContext (method "getApplicationContext" app))
                     (getDataDir (method "getDataDir" "android.content.Context"))
                     )
                 (getDataDir (getApplicationContext this)))))))
      (if datadir
          (let ((exe (make-pathname (list (object->string datadir) "lib") (string-append "lib" cmd ".so"))))
            (with-exception-catcher
             (lambda (exn) (log-debug "open-process failed " 1 (debug 'fail (exception-->printable exn))) #f)
             (lambda () (open-process `(path: ,exe arguments: ,args stdout-redirection: #f))))))))
   (linux
    (open-process `(path: ,(system-cmdargv 0) arguments: ("-s" ,cmd . ,args) stdout-redirection: #f)))
   (else
    (with-exception-catcher
     (lambda (exn) (log-debug "open-process failed " 1 (debug 'fail (exception-->printable exn))) #f)
     (lambda () (debug 'Got (open-process `(path: #;"/proc/self/exe" ,(system-cmdargv 0) arguments: ("-s" ,cmd . ,args) stdout-redirection: #f))))))))

(define (semi-run cmd args)
  (let ((port (semi-fork cmd args)))
    (and (port? port)
         (begin
           (close-port port)
           (eqv? (debug 'ProcessStatus (process-status port)) 0)))))

(define (semi-fork& cmd args)
  (let ((port (semi-fork cmd args)))
    (and (port? port)
         (begin
           (close-port port)
           port))))

(define *registered-commands* '())

(define (register-command! cmd procedure)
  (set! *registered-commands* `((,cmd . ,procedure) . ,*registered-commands*)))

(c-declare
 #<<end-of-c-declare
 #include <unistd.h>
 #include <sys/types.h>
 #include <sys/stat.h>
 #include <stdio.h>
static void cutoff_unwind(int rc, void *ignored)
{
  _exit(rc);
}
static int deliver_payload_events = 1;
void lambdanative_cutoff_unwind()
{
 if(deliver_payload_events) {
  deliver_payload_events = 0;
/* unifdef only for old android!
  if(on_exit(cutoff_unwind, NULL)) {
    fprintf(stderr, "lambdanative_cutoff_unwind failed to register cleanup procedure, exiting\n");
    _exit(1);
  }
//*/
 }
}

// extern void lambdanative_cutoff_unwind();
 static int fork_and_close_fds(int from)
 {
#ifndef _WIN32
  pid_t pid = fork();
  int i;

  if(pid != 0) return pid;
  lambdanative_cutoff_unwind();
  for(i=1;i<NSIG;++i) signal(i,SIG_DFL);
  i = sysconf(_SC_OPEN_MAX) - 1;
  while(i >= from) close(i--); /* ignoring errors */
  umask(0);
  return 0;
#else
  return -1;
#endif
}

 #include <fcntl.h>
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

(c-declare
#<<end-of-c-declare
#if defined(ANDROID) || defined(LINUX)
#include <sys/prctl.h>
#include <errno.h>
#define proc_name_buf_size 15
static void set_proc_name(const char* name) {
  static char buf[proc_name_buf_size+1];
  strncpy(buf, name, proc_name_buf_size);
  if(prctl(PR_SET_NAME, (unsigned long) buf, 0, 0, 0))
    fprintf(stderr, "ERROR: PR_SET_NAME %s\n", strerror(errno));
  // else fprintf(stderr, "PR_SET_NAME success: %s\n", buf);
}
#endif

static void ballcontrol_daemonize()
{
 unsigned int i=0;
 if(fork() != 0) exit(0);
 lambdanative_cutoff_unwind();
 // https://chaoticlab.io/c/c++/unix/2018/10/01/daemonize.html
 for(i=1;i<NSIG;++i) signal(i,SIG_DFL);
 i = sysconf(_SC_OPEN_MAX) - 1;
 while(i >= 3) close(i--); /* ignoring errors */
 umask(0);
 signal(SIGHUP,SIG_IGN);
 signal(SIGTERM,SIG_IGN);
 signal(SIGCHLD,SIG_IGN);
 signal(SIGINT,SIG_IGN);
 // for(i=3;i<FD_SETSIZE;++i) close(i);
 if(setsid() == -1) fprintf(stderr, "ERROR: setsid: %s\n", strerror(errno));
}
static int ln_fork()
{
 unsigned int i=0;
 pid_t pid = fork();
 if(pid != 0) return pid;
 lambdanative_cutoff_unwind();
 for(i=1;i<NSIG;++i) signal(i,SIG_DFL);
 umask(0);
 for(i=3;i<FD_SETSIZE;++i) close(i);
 return 0;
}
end-of-c-declare
)

(define (fork-and-call watchdog-style cmd args)
  (define set-process-name!
    (cond-expand
     ((or android #;linux) (c-lambda (char-string) void "set_proc_name"))
     (else (lambda (n) (debug 'set-process-name! 'ignored) #f))))
  (define _exit (c-lambda (int) void "_exit"))
  (define (watchdog kind name proc args)
    (set-process-name! (string-append "watchdog:" name))
    (cond
     ((eq? kind 'daemon)
      ((c-lambda () void "ballcontrol_daemonize"))
      (log-status "Watchdog daemon running as PID " (getpid))))
    (let ((pid ((c-lambda () int "ln_fork"))))
      (case pid
        ((-1) (log-error "fork failed") (_exit 1)) ;; TODO: include errno
        ;; FIXME: establish signal handlers as with the original watchdog.
        ;; maybe simply use the latter here.
        ((0) (set! this-pid (getpid)) (set-process-name! name) (_exit (proc args)))
        (else
         (log-status cmd "running as PID " pid)
         (receive
          (sig success pid2) (process-wait pid #f)
          (cond ;; be sure NOT to run at_exit hooks trying to take down lambdanative
           ((equal? pid pid2)
            (cond
             ((and (equal? sig 0) success)
              (log-status "Kernel PID " pid " terminated normally")
              (_exit #;(if (not kind) exit _exit) 0)) ;; still seeing that SEGV - just to be sure.
             ((and success (equal? sig 42))
              (log-status "Kernel PID " pid " terminated normally with code " sig)
              (_exit #;(if (not kind) exit _exit) sig))
             (else
              (debug 'watchdog-restart-on-sig sig)
              (log-status "Kernel PID " pid " terminated " (if success "normally" "abnormal") " code " sig " restarting")
              ;; FIXME: should this *really* use `watchdog-style` here or just #t in order not daemonize again ?
              (watchdog watchdog-style name proc args))))
           (else
            (log-error "Kernel PID " pid " process-wait returned signal " sig
                       " terminated " (if success "normally" "abnormal") " pid returned is " pid2)
            (_exit 1))))))))
  (cond-expand
   ((or #;linux #;android)
    (if (eq? watchdog-style #t) (set! watchdog-style 'daemon)))
   (else))
  (let ((e (assoc cmd *registered-commands*)))
    (if e
	(let ((pid ((c-lambda (int) int "fork_and_close_fds") 3)))
	  (case pid
	    ((-1) (log-error "fork failed") #f) ;; TODO: include errno
	    ((0)
	     ;; ((c-lambda () void "microgl_close"))
	     ;; (redirect-standard-ports-for-logging)
	     ;; was : (exit ((cdr e) args))
             (set! this-pid (getpid))
	     (if watchdog-style
                 (watchdog watchdog-style (car e) (cdr e) args)
                 (_exit ((cdr e) args))))
	    (else
             (cond
              ((not watchdog-style)
               (log-status "Kernel running as PID " pid)
               pid)
              ((eq? watchdog-style #t)
               (log-status "Watchdog running as PID " pid)
               pid)
              (else
               (log-status "Forked watchdog deamon for " cmd)
               ;; FIXME: what should we return here? #t, #f, 0, 1 all result in garbage
               pid)))))
	(error "no procedure registered for command " cmd))))

(define (system-command-line* offset)
  (let loop ((n (system-cmdargc)) (r '()))
    (if (< n offset) r
	(let ((i (- n 1)))
	  (loop i (cons (system-cmdargv i) r))))))

(define (execute-registered-command)
  ;; (##clear-exit-jobs!)
  (set! exit (lambda x ((c-lambda (int) void "exit") (if (pair? x) (car x) 0)))) ;; dunno yet where this segfaults
  ;; ((c-lambda () void "lambdanative_cutoff_unwind"))
  (let ((exe (system-cmdargv 0))
	(cmd (system-cmdargv 2)))
    (let ((e (assoc cmd *registered-commands*)))
      (if e ;; (debug 'CmdEntry e)
	  (if #t
              (exit ((cdr e) (system-command-line* 4)))
              (with-exception-catcher
               handle-replloop-exception
               (lambda ()
                (debug 'CP (fork-and-call 'daemon cmd (system-command-line* 4)))
                (exit 0))))
	  (exit 1)))))

;; eof
