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
static void daemonian_daemonize()
{
 unsigned int i=0;
 if(fork() != 0) exit(0);
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
 if(setsid() == -1) fprintf(stderr, "ERROR: setsid: %s\n", strerror(errno));
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

(cond-expand
 (win32
  (define (daemonize thunk #!optional (exit-procedure _exit))
    (cond
     ((procedure? thunk) (error "Not implemented: `daemonize thunk` on windows"))
     ((pair? thunk)
      (let ((port (semi-fork (car thunk) (cdr thunk))))
        (close-port port)
        (thread-sleep! 2)  ;; otherwise forked process will die during startup
        (exit 0)
        (debug 'STATUS (process-status port))))
     (else (error "daemonize: illegal argument" thunk)))))
 (else
  (define (daemonize thunk #!optional (exit-procedure _exit))
    (if exit-procedure (set! exit exit-procedure))
    ((c-lambda () void "daemonian_daemonize"))
    (cond
     ((procedure? thunk)
      ((c-lambda () void "daemonian_daemonize"))
      (thunk))
     ((pair? thunk)
      (let ((port (semi-fork (car thunk) (cdr thunk))))
        (close-port port)
        (thread-sleep! 2)  ;; otherwise forked process will die during startup
        (exit 0)
        (debug 'STATUS (process-status port)))
      ;; does not work (open-process '(path: "stty" arguments: ("sane") stdout-redirection: #f stdin-redirection: #f))
      (_exit 0))
     (else (error "daemonize: illegal argument" thunk))))))

(define (cerberus cmd args #!key (input #f) (max-fast-restarts 5) (restart-time-limit 10))
  (define (once!)
    (let ((port (open-process `(path: ,cmd arguments: ,args stdout-redirection: #f))))
      (and (port? port)
           (begin
             (if input (write input port))
             (close-port port)
             (eqv? (process-status port) 0)))))
  (set-process-name! (string-append "cerberus " cmd))
  ((c-lambda () scheme-object "___setup_child_interrupt_handling"))
  (let loop ((start (current-time)))
    (let fast ((i 1))
      (or (once!)
          (let ((end (current-time)))
            (if (< (- (time->seconds end) (time->seconds start))
                   restart-time-limit)
                (if (< i max-fast-restarts)
                    (fast (fx+ i 1))
                    #f)
                (loop end)))))))

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

(include "~~tgtlib/onetierzero/src/observable-notational-conventions.scm")

(define daemonian-stdout-file
  (make-pin
   initial: #f
   pred: (lambda (fn) (or (boolean? fn) (string? fn)))
   name: 'daemonian-stdout-file))

(define daemonian-stderr-file
  (make-pin
   initial: #f
   pred: (lambda (fn) (or (boolean? fn) (string? fn)))
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

(namespace ("daemonian#" standard-file-change! standard-port-change!))

(define (standard-file-change! old1 new1 old2 new2)
  (define (file-change! out-pin param key old new)
    (if (not (equal? old new))
        (let ((port (open-output-file `(path: ,new append: #t))))
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
