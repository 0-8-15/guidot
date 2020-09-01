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

(define (daemonize thunk #!optional (exit-procedure _exit))
  (if exit-procedure (set! exit exit-procedure))
  ((c-lambda () void "daemonian_daemonize"))
  (thunk))

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
  char buf[1024];
  int len=readlink(___arg1, buf, 1024);
  if(len>0) ___return(buf);
  ___return(NULL);
EOF
))

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

(namespace ("daemonian#" standard-file-change!))

(define (standard-file-change! old1 new1 old2 new2)
  (define (file-change! out-pin param key old new)
    (if (and (not (equal? old new))
             (redirect-standard-fd! key new))
        ;; FIXME: ##open-predefined leaks memory, open-fd-port does not handle ioctrl
        (let ((port (#;open-fd-port ##open-predefined 2 new key)))
          (param port)
          (lambda () (out-pin port)))
        #f))
  (let ((o (file-change! daemonian-stdout-port current-output-port 1 old1 new1))
        (e (file-change! daemonian-stderr-port current-error-port 2 old2 new2)))
    (and (or o e) (kick! (lambda () (if o (o)) (if e (e)))))))

(wire! (list daemonian-stdout-file daemonian-stderr-file)
       sequence: standard-file-change!)

(define (standard-port-change! old1 new1 old2 new2)
  (if old1 (close-output-port old1))
  (if old2 (close-output-port old2)))

(wire! (list daemonian-stdout-port daemonian-stderr-port)
       post-changes: standard-port-change!)
