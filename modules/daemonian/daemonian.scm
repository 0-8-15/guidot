(c-declare "
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>

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
#if defined(ANDROID) || defined(LINUX) || defined(__linux__)
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

static int redirect_to_file(int n, char *fn)
{
 if(freopen(fn, "a", n==1 ? stdout : stderr) == NULL) return 1;
 return 0;
}

#if !WIN32
static unsigned int daemonian_daemonize()
{
 unsigned int i=0;
 if(fork() != 0) return(0);
 // lambdanative_cutoff_unwind();
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
 if(setsid() == -1) {
   fprintf(stderr, "ERROR: setsid: %s\n", strerror(errno));
   exit(1);
 }
 if(fork() != 0) return(0);
 return(1);
}
#endif

extern ___SCMOBJ ___setup_child_interrupt_handling();

end-of-c-declare
)

;;

(define redirect-standard-fd! (c-lambda (int char-string) bool "redirect_to_file"))

(define (redirect-standard-port! n fn)
  (case n
    ((1 stdout)
     (and (redirect-standard-fd! 1 fn)
          (current-output-port (open-fd-port 2 fn 1))))
    ((2 stderr)
     (and (redirect-standard-fd! 1 fn)
          (current-output-port (open-fd-port 2 fn 2))))
    (else (error "not a standard port" n))))

(define set-process-name!
  (cond-expand
   ((or android linux Linux) (c-lambda (char-string) void "set_proc_name"))
   (else (lambda (n) (debug 'set-process-name! 'ignored) #f))))

(define (_exit #!optional (code 0))
  ((c-lambda (int) void "_exit") code))

;;; semi-fork

(define daemonian-semifork-key (make-parameter "-s"))

(define *registered-commands* '())

(define (register-command! cmd procedure)
  (set! *registered-commands* `((,cmd . ,procedure) . ,*registered-commands*)))

(define (android-directory-files)
  ((c-lambda () char-string "
#ifdef ANDROID
extern char *android_getFilesDir();
#endif
___result=
#ifdef ANDROID
android_getFilesDir();
#else
NULL;
#endif
")))

(register-command!
 "daemonian-captured-run"
 (lambda (args)
   (let ((conn (open-process
                `(path: ,(system-cmdargv 0) arguments: (,(daemonian-semifork-key) . ,(cdr args))
                        eol-encoding: lf
                        stdin-redirection: #f stdout-redirection: #f
                        stderr-redirection: #t show-console: #f))))
     (cond
      ((port? conn)
       (let* ((error-output (read-line conn #f))
              (rc (process-status conn)))
         ;; TBD: how to pass `error-output`?
         (cond
          ((and (eqv? rc 0) (eof-object? error-output))
           (exit 0))
          (else
           (when (string? error-output)
             (display error-output (current-output-port)))
           (close-output-port (current-output-port))
           (display error-output (current-error-port))
           (exit (if (eqv? rc 0) 1 (/ (process-status conn) 256)))))))
      (else (exit 1))))))

(cond-expand
 (android
  (define daemonian-directory-files (delay (android-directory-files)))
  (define (android-ls-lR dir)
    (let ((port (open-input-process `(path: "ls" arguments: ("-lR" ,dir) stderr-redirection: #t))))
      (if (input-port? port) (read-line port #f) (string-append "failed to run ls -lR " dir))))
  (define (android-file fn)
    (let ((port (open-input-process `(path: "file" arguments: (,fn) stderr-redirection: #t))))
      (if (input-port? port) (read-line port #f) (string-append "failed to run file " fn))))
  (define (android-run-in/boolean directory cmd  . args)
    (log-error "running in " directory " " cmd " " (object->string args))
    (let* ((port (open-input-process `(path: ,cmd arguments: ,args stderr-redirection: #f directory: ,directory)))
           (ret (and (port? port) (zero? (process-status port)))))
      (unless ret (log-error "FAILED in " directory " : " cmd " " (object->string args)))
      ret))
  (define (android-run->string cmd  . args)
    (let ((port (open-input-process `(path: ,cmd arguments: ,args stderr-redirection: #t))))
      (if (input-port? port) (read-line port #f) (apply string-append "failed to run " cmd args))))
  (define (android-run-in->string directory cmd  . args)
    (let ((port (open-input-process `(path: ,cmd arguments: ,args stderr-redirection: #t directory: ,directory))))
      (if (input-port? port) (read-line port #f) (apply string-append "failed to run " cmd args))))
  )
 (else))

(define (semi-fork cmd args #!optional (stderr #f) #!key (directory #f) (char-encoding 'UTF-8))
  ;; (debug 'semi-fork `(,cmd . ,args))
  (log-debug "semi-fork " 1 cmd " on " args)
  (cond-expand
   (android
    (let ((datadir
           (or (path-directory (force daemonian-directory-files))
               (error "data directory not found"))
           #;(jscheme-eval
            `(let* ((app ,(android-app-class))
                    (this ((method "me" app)))
                    )
               (let (
                     (getApplicationContext (method "getApplicationContext" app))
                     ;; getDataDir is new in API24 and deprecated
                     ;; (getDataDir (method "getDataDir" "android.content.Context"))
                     (getDataDir
                      (let ((getFilesDir (method "getFilesDir" "android.content.Context"))
                            (getParent (method "getParent" "java.io.File")))
                        (lambda (ctx) (getParent (getFilesDir ctx)))))
                     )
                 (getDataDir (getApplicationContext this)))))))
      (define (file-exists-and-executable? fn)
        (and (file-exists? fn)
             (not (eqv? (bitwise-and (file-mode fn) #b001000000) 0))))
      (define (apk-executable-path cmd)
        (string-append "lib/armeabi-v7a/lib" cmd ".so"))
      (define (cache-path) (string-append datadir "cache"))
      (define (cached-executable-path cmd)
        (string-append datadir "cache/lib/armeabi-v7a/lib" cmd ".so"))
      (define (cached-executable cmd)
        (let ((cachedpath (cached-executable-path cmd)))
          (and (file-exists-and-executable? cachedpath) cachedpath)))
      (let loop ((exe (string-append datadir "lib/lib" cmd ".so") #;(make-pathname (list (object->string datadir) "lib") (string-append "lib" cmd) ".so"))
                 (envt (list
                        (string-append "HOME=" (force daemonian-directory-files)))))
        (let* ((exists? (file-exists? exe))
               (executable? (and exists? (not (eqv? (bitwise-and (file-mode exe) #b001000000) 0)))))
          (cond
           ((and exists? executable?)
            (with-exception-catcher
             (lambda (exn) (log-debug "open-process failed " 1 (debug 'fail (exception-->printable exn))) #f)
             (lambda ()
               (cond-expand
                (debug
                 (log-debug
                  "semi-fork " 1
                  `(path: ,exe arguments: ,args environment: ,envt directory: ,directory
                          stdout-redirection: #t stdin-redirection: #t stderr-redirection: ,(and stderr #t))))
                (else))
               (open-process
                `(path: ,exe arguments: ,args
                        environment: ,envt
                        directory: ,directory
                        char-encoding: ,char-encoding
                        stdout-redirection: #t stdin-redirection: #t
                        stderr-redirection: ,(and stderr #t))))))
           ((cached-executable cmd) => (lambda (cached) (loop cached envt)))
           (else
            (log-error "executable: " exe " exists: " exists? " executable: " executable?)
            (let ((exe (cached-executable-path cmd)))
              (log-error "cached executable: " exe " exists: " (file-exists? exe) " executable: " (file-exists-and-executable? exe)))
            ;;(log-error "data dir: " datadir "\n" (android-ls-lR datadir))
            (let ((p (android-PackageCodePath))
                  (cachedir (cache-path)))
              (log-error "package code path: " p "\n" (android-ls-lR p) (android-file p))
              (with-exception-catcher
               (lambda (exn) (log-error "unzip ran into: " exn))
               (lambda () (log-error "content: " (android-run->string "unzip" "-lq" p))))
              (if (and
                   (or (android-run-in/boolean
                        cachedir "unzip" "-d" cachedir p (apk-executable-path cmd))
                       (begin
                         (log-error
                          "Error: "
                          (android-run-in->string
                           cachedir "unzip" "-d" cachedir p (apk-executable-path cmd)))
                         #f))
                   (or (file-exists-and-executable? (cached-executable-path cmd))
                       (android-run-in/boolean cachedir "chmod" "+x" (cached-executable-path cmd))
                       (begin
                         (log-error
                          "Error: " (android-run-in->string cachedir "chmod" "+x" (cached-executable-path cmd)))
                         (log-error " Dir " (android-ls-lR (string-append datadir "cache")))
                         #f))
                   (cached-executable cmd))
                  (loop (cached-executable cmd) envt)
                  (begin
                    (log-error "FAILED to extract cmd " cmd " from archive " p)
                    #f)))))))))
   (else
    (let ((arguments `(,(daemonian-semifork-key) ,cmd . ,args)))
      ;; FIXME: is this really true? Note: do NOT use `show-console: #f` - it interfers with redirection.
      (case stderr
        ((raise)
         (open-process
          `(path: ,(system-cmdargv 0) arguments: (,(daemonian-semifork-key) "daemonian-captured-run" ,@arguments)
                  directory: ,directory
                  show-console: #f
                  eol-encoding: lf
                  char-encoding: ,char-encoding
                  stdout-redirection: #t stdin-redirection: #t
                  stderr-redirection: ,(and stderr #t))))
        (else
         (open-process
          `(path: ,(system-cmdargv 0) arguments: ,arguments
                  directory: ,directory
                  show-console: #f
                  eol-encoding: lf
                  ;; input-eol-encoding: cr-lf output-eol-encoding: lf
                  ;; char-encoding: ,char-encoding
                  buffering: #f
                  stdout-redirection: #t stdin-redirection: #t
                  stderr-redirection: ,(and stderr #t)))))))))

(define (semi-run cmd args)
  (let ((port (semi-fork cmd args)))
    (and (port? port)
         (begin
           (close-port port)
           (eqv? (process-status port) 0)))))

(define (semi-fork& cmd args)
  (let ((port (semi-fork cmd args)))
    (and (port? port)
         (begin
           (close-port port)
           port))))

;; from scsh-utils
(define (maybe->string s)
  (cond ((string? s) s)
        ((or (symbol? s) (number? s) (char? s)) (object->string s))
        (else
         (log-error "Expected a string, symbol, character or number" s)
         (object->string s))))

(define (port->string p)
  #;(and
   (input-port? p)
   (dynamic-wind
       (lambda () #t)
       (lambda ()
         (call-with-output-string
          (lambda (o)
            (let loop ()
              (let ((ln (read-line p)))
                (unless (eof-object? ln)
                        (display ln o)
                        (newline o)
                        (loop)))))))
       (lambda () (close-input-port p))))
  (and (input-port? p) (read-line p #f)))

(define (run->string cmd . args)
  (port->string (semi-fork cmd (map maybe->string args))))

(define (run->error-string cmd . args)
  (port->string (semi-fork cmd (map maybe->string args) #t)))

(define (run->status cmd . args)
  (process-status (semi-fork cmd (map maybe->string args))))

(define (run/boolean cmd . args)
  (let ((port (semi-fork cmd (map maybe->string args))))
    (and (port? port) (zero? (process-status port)))))

(define (run-logging/boolean cmd . args)
  (log-status "running " cmd " " (object->string args))
  (let ((result (apply run/boolean cmd args)))
    (log-status cmd (if result " success" " FAILED"))
    result))

;; end scsh-utils

(define (system-command-line* offset) ;; FIXME: depends on lambdanative!
  (let loop ((n (system-cmdargc)) (r '()))
    (if (< n offset) r
	(let ((i (- n 1)))
	  (loop i (cons (system-cmdargv i) r))))))

(define (daemonian-execute-registered-command cmd args)
  ;; (##clear-exit-jobs!)
  ;; (println "execute registered command")
  ;; (set! exit (lambda x ((c-lambda (int) void "exit") (if (pair? x) (car x) 0)))) ;; dunno yet where this segfaults
  ;; ((c-lambda () void "lambdanative_cutoff_unwind"))
  (let ((exe (system-cmdargv 0)))
    (let ((e (assoc cmd *registered-commands*)))
      (if e ;; (debug 'CmdEntry e)
	  (begin
            (with-exception-catcher
             handle-replloop-exception
             (lambda ()
               ((cdr e) (if (fixnum? args) (system-command-line* args) args))
               (exit 0)))
            (exit 42))
	  (begin
            (println port: (current-error-port) "Command not found: " cmd)
            (exit 1))))))
;;;

(cond-expand
 (win32
  (define (daemonize thunk #!optional (child-exit exit))
    (cond
     ((procedure? thunk) (error "Not implemented: `daemonize thunk` on windows"))
     ((pair? thunk) (semi-fork&  (car thunk) (cdr thunk)))
     #;((pair? thunk)
      (let ((port (semi-fork (car thunk) (cdr thunk))))
        (when (port? port)
          (close-port port)
          (thread-sleep! 2)  ;; otherwise forked process may die during startup
          (println port: (current-error-port)
                   (car thunk) " watching '" (cadr thunk) "' as process " (process-pid port))
          (exit 0))
        (error "failed on" thunk))
      (exit 0))
     (else (error "daemonize: illegal argument" thunk)))))
 (else
  (define (daemonize thunk #!optional (post-fork-exit (c-lambda (int) void "_exit")))
    ;; (if exit-procedure (set! exit exit-procedure))
    (cond
     ((procedure? thunk)
      (if ((c-lambda () bool "daemonian_daemonize"))
          (thunk)
          (post-fork-exit 0)))
     ;; ((pair? thunk) (exit (if (semi-run (car thunk) (cdr thunk)) 0 1))) ;; debug only
     ((pair? thunk)
      (if ((c-lambda () bool "daemonian_daemonize"))
          (begin
            (setup-child-interrupt-handling!)
            (exit (if (semi-run (car thunk) (cdr thunk)) 0 1)))
          (post-fork-exit 0)))
     ((pair? thunk)
      (let ((port (semi-fork (car thunk) (cdr thunk))))
        (when (port? port)
          (close-port port)
          (thread-sleep! 2) ;; otherwise forked process will die during startup
          (if (or #t (cerberus-verbose))
              (println port: (current-error-port)
                       (car thunk) " watching '" (cadr thunk) "' as process " (process-pid port)))
          (post-fork-exit 0))
        (error "failed on" thunk))
      ;; does not work (open-process '(path: "stty" arguments: ("sane") stdout-redirection: #f stdin-redirection: #f))
      (exit 0))
     (else (error "daemonize: illegal argument" thunk))))))

(define $cerberus-report-exit (make-parameter #f))

(define (cerberus-watch
         once #!key
         (input #f) (post-exit-delay 1) (max-fast-restarts 5) (restart-time-limit 10))
  (let loop ((start (current-time)))
    (let fast ((i 1))
      (or (once)
          (let* ((end (current-time))
                 (duration (- (time->seconds end) (time->seconds start))))
            (let ((report ($cerberus-report-exit)))
              (when report (report once duration)))
            (if (< duration restart-time-limit)
                (if (< i max-fast-restarts)
                    (begin
                      (and post-exit-delay (thread-sleep! post-exit-delay))
                      (fast (fx+ i 1)))
                    #f)
                (begin
                  (and post-exit-delay (thread-sleep! post-exit-delay))
                  (loop end))))))))

(define (cerberus
         cmd args #!key
         (input #f) (startup-delay 1) (post-exit-delay 1) (max-fast-restarts 5) (restart-time-limit 10)
         (stdout-redirection #t) (stderr-redirection #f)
         (exit _exit))
  (define (once!)
    (let ((port (open-process
                 `(path: ,cmd arguments: ,args
                         stdout-redirection: ,stdout-redirection stderr-redirection: ,stderr-redirection))))
      (and (port? port)
           (begin
             (thread-sleep! startup-delay) ;; give it time to start up
             (let ((terminated (process-status port 0 #f)))
               (if terminated
                   (begin
                     (close-port port)
                     ;; should at least run for startup-delay
                     #f)
                   (begin
                     (if input (write input port))
                     (close-port port)
                     ;; BEWARE: FIXME: HACK: A println was required (on
                     ;; Linux) avoid hanging gambit in a endless loop eating
                     ;; all memory!
                     (if (cerberus-verbose)
                         (eqv? (process-status (debug 'waiting-for port)) 0)
                         (eqv? (process-status port) 0)))))))))
  (define (run)
    (cerberus-watch
     once!
     input: input
     post-exit-delay: post-exit-delay
     max-fast-restarts: max-fast-restarts restart-time-limit: restart-time-limit))
  ;; (set-process-name! (string-append "cerberus " (car args)))
  ;; (debug 'cerberus ((c-lambda () int "getpid")))
  (setup-child-interrupt-handling!)
  (exit (if (with-exception-catcher (lambda (exn) #f) run) 0 1)))

(define setup-child-interrupt-handling! (c-lambda () scheme-object "___setup_child_interrupt_handling"))

(define readlink
  (c-lambda (char-string) char-string
#<<EOF
#if WIN32
fprintf(stderr, "NYI readlink on windows");
#else
  char buf[1024];
  int len=readlink(___arg1, buf, 1024);
  if(len>0) ___return(buf);
#endif
  ___return(NULL);
EOF
))

(define cerberus-verbose (make-pin initial: #t name: "Trace Cerberus"))

(define (tag-thunk-as-service thunk . more)
  (let ((thunk (tag-thunk thunk 'service)))
    (for-each (lambda (tag) (tag-thunk thunk tag)) more)
    thunk))

(define (service? obj) (and (procedure? obj) (procedure-tagged? obj 'service)))

(define (daemonian-parse-null-port-alias old new)
  (if (member new '("/dev/null" "NULL" "NUL" "")) #f new))

(define daemonian-stdout-file
  (make-pin
   initial: #t
   pred: (lambda (fn) (or (boolean? fn) (string? fn)))
   filter: daemonian-parse-null-port-alias
   name: 'daemonian-stdout-file))

(define daemonian-stderr-file
  (make-pin
   initial: #t
   pred: (lambda (fn) (or (boolean? fn) (string? fn)))
   filter: daemonian-parse-null-port-alias
   name: 'daemonian-stderr-file))

(define daemonian-stdout-port
  (make-pin
   initial: (current-output-port)
   pred: output-port?
   name: 'daemonian-stdout-port))

(define daemonian-stderr-port
  (make-pin
   initial: (current-error-port)
   pred: output-port?
   name: 'daemonian-stderr-port))

(define (daemonian-redirect-standard-file-descriptors) #f)

(define null-output-port
  (delay
    (let ((port #f))
      (define (reset!)
        (set! port (open-string))
        (set! null-output-port port))
      (thread-start!
       (make-thread
        (lambda ()
          (do () (#f)
            (let ((x (read-char port)))
              (if (eof-object? x)
                  (reset!)
                  (get-output-string port)))))
        'null-drain))
      (reset!)
      port)))

(namespace ("daemonian#" standard-file-change! standard-port-change!))

(define (standard-file-change! old1 new1 old2 new2)
  (define (file-change! out-pin param key old new)
    (if (not (equal? old new))
        (let ((port (if (boolean? new)
                        (force null-output-port)
                        (open-output-file `(path: ,new append: #t buffering: line)))))
          (and
           (port? port)
           (begin
             (when (daemonian-redirect-standard-file-descriptors)
               (unless (redirect-standard-fd! key new)
                 (println port: (current-error-port) "Error: failed to redirect fd " key " to file: " new)))
             (param port)
             (lambda () (out-pin port)))))
        #f))
  (let ((o (file-change! daemonian-stdout-port current-output-port 1 old1 new1))
        (e (file-change! daemonian-stderr-port current-error-port 2 old2 new2)))
    (and (or o e) (kick! (lambda () (if o (o)) (if e (e)) #f)))))

(wire! (list daemonian-stdout-file daemonian-stderr-file)
       sequence: standard-file-change!)

(define standard-port-change!
  (let ((dont (list (current-output-port) (current-error-port))))
    (define (standard-port-change! old1 new1 old2 new2)
      (if (and old1 (not (memq old1 dont))) (close-output-port old1))
      (if (and old2 (not (memq old2 dont))) (close-output-port old2))
      #f)
    standard-port-change!))

(wire! (list daemonian-stdout-port daemonian-stderr-port)
       sequence: standard-port-change!)

;; HACK, FIXME: make it work on lambdanative/android
(define (ln-system-command-line* offset) ;; FIXME: depends on lambdanative!
  (cond-expand
   (debug (MATURITY -1 "fake system command line" loc: 'daemonian:ln-system-command-line))
   (else))
#|
  (log-status "faking command line from offset " offset)
  (log-status "system-cmdargc: " (system-cmdargc))
|#
  (let loop ((n (system-cmdargc)) (r '()))
    (if (< n offset) r
	(let ((i (- n 1)))
          #|
          (log-status "n: " i)
          (log-status "system-cmdargv: " system-cmdargv)
          (log-status "system-cmdargv[i]: " i (system-cmdargv i))
          |#
	  (loop i (cons (system-cmdargv i) r))))))

(cond-expand
 (android
  (define (command-line) (cons "unknown-on-adroid" (ln-system-command-line* 1))))
 (else
  (define (command-line) (ln-system-command-line* 1))))
