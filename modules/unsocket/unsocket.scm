;; Unix Domain sockets in gambit API

;; BEWARE: This will need to be rewritten for gambit > 4.9.2.  Depends
;; on this patch to gambit:
;;
;; --- lib/os_io.c~	2019-01-18 20:10:55.000000000 +0100
;; +++ lib/os_io.c	2020-07-29 20:29:51.525331737 +0200
;; @@ -1561,6 +1561,14 @@
;;    self->close_direction = self->direction;
;;  }

;; +void ___os_device_transfer_close_responsibility
;; +   ___P((___device *self),
;; +        (self)
;; +___device *self;)
;; +{
;; +  device_transfer_close_responsibility(self);
;; +}
;; +
;;  ___HIDDEN void device_add_ref
;;     ___P((___device *self),
;;          (self)

(c-declare "extern void ___os_device_transfer_close_responsibility(void*);")

(define ##os-device-transfer-close-responsibility!
  (c-lambda ((pointer (struct "___device")))  ;; device
            void
   "___os_device_transfer_close_responsibility"))

(define (open-fd-port direction name index #!optional (settings '()))
  (##make-path-psettings
   direction
   (append settings (if (eqv? (bitwise-and direction 2) 0) '() '(output-width: 80)) (##list 'readtable: ##main-readtable))
   ##exit-abnormally
   (lambda (psettings)
     (let ((device
            (##os-device-stream-open-predefined
             index
             (##psettings->device-flags psettings))))
       (if (##fixnum? device)
           (##exit-with-err-code device)
           (and device
                (begin
                  (##os-device-transfer-close-responsibility! device)
                  (##make-device-port-from-single-device
                   name
                   device
                   psettings))))))))

;;;----------------------------------------------------------------------------

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

(c-declare "
#ifndef UNIX_PATH_MAX
#define UNIX_PATH_MAX 108
#endif

#if defined(ANDROID) && !defined(SO_REUSEPORT)
#define SO_REUSEPORT SO_REUSEADDR
#endif
#include <string.h>

static ___SCMOBJ release_memory(void *p)
{
  free(p);
  return ___FIX(___NO_ERR);
}

")

(define SOCKADDR_STORAGE_SIZE ((c-lambda () size_t "___return(sizeof(struct sockaddr_storage));")))
(define SOCKADDR_INET_SIZE ((c-lambda () size_t "___return(sizeof(struct sockaddr_in));")))
(define SOCKADDR_INET6_SIZE ((c-lambda () size_t "___return(sizeof(struct sockaddr_in6));")))
(define SOCKADDR_UN_SIZE ((c-lambda () size_t "___return(sizeof(struct sockaddr_un));")))

(define unix-path-max ((c-lambda () int "___return(UNIX_PATH_MAX);")))

(define (make-unspecified-socket-address)
  (let ((result (make-u8vector SOCKADDR_STORAGE_SIZE)))
    ((c-lambda (scheme-object) void "
struct sockaddr_storage* sa_st = ___CAST(struct sockaddr_storage*, ___BODY(___arg1));
sa_st->ss_family = AF_UNSPEC;
") result)
    result))

(define (unix-address->socket-address fn)
  (let ((result (make-u8vector SOCKADDR_UN_SIZE))
        (l (string-length fn)))
    (if (>= l unix-path-max) (error "unix-address->socket-address: path too long" fn))
    (if (>= (fx- l 2) SOCKADDR_UN_SIZE) (error "unix-address->socket-address: exceeds SOCKADDR_UN_SIZE" fn))
    ((c-lambda (scheme-object char-string) void "
 struct sockaddr_un* sa_un = ___CAST(struct sockaddr_un*, ___BODY(___arg1));
 sa_un->sun_family = AF_UNIX;
 strncpy(sa_un->sun_path, ___arg2, UNIX_PATH_MAX);
 sa_un->sun_path[UNIX_PATH_MAX - 1] = '\\0';
") result fn)
    result))

(define (abstract-address->socket-address pattern)
  (let ((result (make-u8vector SOCKADDR_UN_SIZE)))
    ((c-lambda (scheme-object scheme-object size_t) void "
 struct sockaddr_un* sa_un = ___CAST(struct sockaddr_un*, ___BODY(___arg1));
 ___U8* pattern = ___CAST(___U8*, ___BODY(___arg2));
 ___U8* to = ___CAST(___U8*, &sa_un->sun_path) + 1;
 size_t n = ___arg3 < UNIX_PATH_MAX ? ___arg3 : UNIX_PATH_MAX;
 size_t i=1;
 while(i<UNIX_PATH_MAX) {
   memcpy(to, ___BODY(___arg2), n);
   to += n; i += n;
 }
 sa_un->sun_family = AF_UNIX;
 sa_un->sun_path[0] = '\\0';
") result pattern (u8vector-length pattern))
    result))

(namespace ("unsocket#"
            c-errno c-close c-socket c-make-socket-nonblocking!
            create-socket
            c-bind c-listen c-accept
            c-connect
            ))

(define c-errno (c-lambda () int "___return(errno);"))

(define-macro (raise-socket-exception-if-error expr proc . args)
  (let ((b (gensym 'b))
        (e (gensym 'e))
        (loop (gensym 'loop)))
    `(let ()
       (declare (not interrupts-enabled))
       (let ,loop
         ((,b ,expr))
         (if (< ,b 0)
             (let* ((,e (c-errno)))
               (if (or
                    (= ,e ##err-code-EAGAIN)
                    (= ,e ##err-code-EINTR))
                   (begin
                     ;; (thread-yield!) ; to avoid tying up the CPU
                     (,loop ,expr))
                   (##raise-os-exception
                    #f
                    ,e
                    ,proc
                    (err-code->string ,e)
                    ,@args
                    )))
             ,b)))))

(define c-close (c-lambda (int) int "___return(close(___arg1));"))
(define strerror (c-lambda (int) char-string "strerror"))

(define-macro (unsocket-os-exception proc . args)
  (let ((errno (gensym 'errno)))
    `(let ((,errno (c-errno)))
       (##raise-os-exception #f ,errno ,proc (strerror ,errno) ,@args))))

(define
  c-socket
  (c-lambda (int int int) int #<<C-END
int s = socket(___arg1,___arg2,___arg3);
___result = s;
C-END
))

(define
  c-make-socket-nonblocking!
  (c-lambda (int) int #<<C-END
int s = ___arg1;
#ifdef _WIN32
___return(ioctlsocket(s, FIONBIO, (void *)&s) != SOCKET_ERROR ? 0 : -1);
#else
int fl = fcntl(s,F_GETFL);
___return(fcntl(s,F_SETFL,fl | O_NONBLOCK)); // | keep emacs highlight happy
#endif
C-END
))

(define (create-socket domain type protocol)
  (let ((fd (raise-socket-exception-if-error
             (c-socket domain type protocol)
             create-socket)))
    (with-exception-catcher
     (lambda (x) (c-close fd) (error x))
     (lambda ()
       (raise-socket-exception-if-error
        (c-make-socket-nonblocking! fd)
        'make-socket-nonblocking!)))
    fd))

(define c-bind
  (c-lambda
   (int scheme-object int) int
   "___return(bind(___arg1, ___CAST(struct sockaddr *, ___BODY(___arg2)), ___arg3));"
   ))

(define c-listen (c-lambda (int int) int "___return(listen(___arg1, ___arg2));"))

(define c-accept
  (c-lambda
   (int scheme-object size_t) int
   "int len = ___arg3; ___return(accept(___arg1, ___CAST(struct sockaddr*, ___BODY(___arg2)), &len));"))

(define (open-unix-server spec #!key (backlog 4) (reuse-address #f) (wait-for-close #t))
  (define fs-address? string?)
  (define (accept-connection fd port)
    (let* ((client (make-unspecified-socket-address))
           (fd (raise-socket-exception-if-error
                (begin
                  (##wait-input-port port)
                  (c-accept fd client SOCKADDR_STORAGE_SIZE))
                'open-unix-server:accept-connection)))
      (values
       (begin
         (raise-socket-exception-if-error
          (c-make-socket-nonblocking! fd)
          'open-unix-server:make-socket-nonblocking!)
         fd)
       client)))
  (define (forward-connections to from-fd from-port)
    (with-exception-catcher
     (lambda (exn)
       (close-port from-port) ;; hope this actually closes the `from-fd` too!!!
       (close-output-port to)
       (if (fs-address? spec) (delete-file spec)))
     (lambda ()
       (let loop ()
         (receive
          (fd client) (accept-connection from-fd from-port)
          (write (open-fd-port 3 open-unix-server fd) to))
         (loop)))))
  (define (success fd)
    (define fd-port (open-fd-port 1 open-unix-server fd))
    (c-make-socket-nonblocking! fd)
    (receive
     (c s) (open-vector-pipe '(buffering: #f) '(buffering: #f))
     (thread-start!
      (make-thread
       (lambda () (forward-connections s fd fd-port))
       spec))
     (when wait-for-close ;; watch the client port to be closed or forgotten
       (thread-start!
        (make-thread
         (lambda ()
           (read s)
           (close-port fd-port)
           (close-port c))
         `(unix-servant ,spec)))
       (make-will c close-port))
     c))
  (let* ((addr (cond
                ((string? spec) (unix-address->socket-address spec))
                ((u8vector? spec) (abstract-address->socket-address spec))
                (else (error "open-unix-server: illegal argument" spec))))
         (fd (create-socket
              ((c-lambda () int "___return(PF_UNIX);"))
              ((c-lambda () int "___return(SOCK_STREAM);"))
              0))
         (rc (begin
               (when
                (and (fs-address? spec) reuse-address (file-exists? spec)
                     (eq? (file-info-type (file-info spec)) 'socket))
                (delete-file spec))
               (c-bind fd addr SOCKADDR_UN_SIZE))))
    (cond
     ((eq? rc 0)
      (let ((rc (c-listen fd backlog)))
        (if (eq? rc 0)
            (success fd)
            (begin
              (c-close fd)
              (unsocket-os-exception open-unix-server)))))
     (else
      (c-close fd)
      (unsocket-os-exception open-unix-server)))))

(define (unsocket-service-register spec handler #!key (service-name 'unsocket-service)
                                   (backlog 4) (reuse-address #f) (wait-for-close #t))
  ;; registers a service MOSTLY like tcp-service-register! -- however
  ;; it returns a port to be close to unregister the service.  It does
  ;; NOT implement closing the service via the address (spec).
  (define (handle-connection conn)
    (lambda ()
      (parameterize
       ((current-input-port conn) (current-output-port conn))
       (handler))
      ;; (force-output conn)
      (with-exception-catcher
       (lambda (exn) #f) ;; close-port raises an exception if the client is gone.
       (lambda () (close-port conn)))))
  (let ((incomming
         (open-unix-server
          spec backlog: backlog
          reuse-address: reuse-address wait-for-close: wait-for-close)))
    (thread-start!
     (make-thread
      (lambda ()
        (let loop ()
          (let ((conn (read incomming)))
            (when
             (port? conn)
             (thread-start! (make-thread (handle-connection conn) service-name))
             (loop)))))
      service-name))
    incomming))

(define c-connect
  (c-lambda
   (int scheme-object size_t) int
   "___return(connect(___arg1, ___CAST(struct sockaddr *, ___BODY(___arg2)), ___arg3));"))

(define (open-unix-client* spec #!optional (fail (lambda () (unsocket-os-exception open-unix-client))))
  (let* ((addr (cond
                ((and (u8vector? spec) (eqv? (u8vector-length spec) SOCKADDR_UN_SIZE)) spec)
                ((string? spec) (unix-address->socket-address spec))
                ((u8vector? spec) (abstract-address->socket-address spec))
                (else (error "open-unix-client: illegal argument" spec))))
         (fd (create-socket
              ((c-lambda () int "___return(PF_UNIX);"))
              ((c-lambda () int "___return(SOCK_STREAM);"))
              0))
         (rc (c-connect fd addr SOCKADDR_UN_SIZE)))
    (cond
     ((eq? rc 0) (open-fd-port 3 open-unix-client fd))
     (else
      (c-close fd)
      (fail)))))

(define (open-unix-client spec #!optional (retry #f) (scale 0.01) (base 2) (limit 1.0))
  (define (wait-for-connection sockaddr)
      (define (wait-for-socket n)
        (or (with-exception-catcher
             (lambda (ex)
               ;; (log-error "open-unix-client: " spec (debug 'EXN (exception-->printable ex)))
               #f)
             (lambda () (open-unix-client* sockaddr)))
            (let ((sleep-time (* scale (expt base n))))
              (if (> sleep-time limit)
                  (if (procedure? retry) (retry) (error "failed to connect to beaver" sockaddr))
                  (begin
                    (thread-sleep! sleep-time)
                    (wait-for-socket (+ n 1)))))))
      (wait-for-socket 0))
  (if retry (wait-for-connection spec) (open-unix-client* spec)))
