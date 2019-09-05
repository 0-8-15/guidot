; Gamsock -- an enhanced socket library for Gambit built around the Scsh
; socket API.

; Copyright (c) 2006-2007 by Jeffrey T. Read.
; Scsh constant files copyright (c) 1993-1994 by Olin Shivers and Brian D.
; Carlstrom.

; For redistribution conditions, please see the file COPYING.
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
;(include "gamsock-headers.scm")
(c-declare "
#ifndef UNIX_PATH_MAX
#define UNIX_PATH_MAX 108
#endif

#if defined(ANDROID) && !defined(SO_REUSEPORT)
#define SO_REUSEPORT SO_REUSEADDR
#endif
#include <string.h>
")
(define-macro (define-c-constant var type . const)
  (let* ((const (if (not (null? const)) (car const) (symbol->string var)))
	 (str (string-append "___result = " const ";")))
    `(define ,var ((c-lambda () ,type ,str)))))

(define-macro (define-int-c-constants prefix . constants)
  (let* ((base (cond
		((string? prefix) prefix)
		((symbol? prefix) (symbol->string prefix))
		(else (error "Symbol or string required for define-enum-constants prefix")))))
    `(begin
       ,@(map (lambda (x)
		`(define-c-constant
		   ,(string->symbol
		     (string-append base "/" (symbol->string (car x))))
		   int ,(cadr x))) constants))))

(define-macro (define-enum-constants prefix . constants)
  (let* ((base (cond
		  ((string? prefix) prefix)
		  ((symbol? prefix) (symbol->string prefix))
		  (else (error "Symbol or string required for define-enum-constants prefix")))))
    `(begin
       ,@(map (lambda (x)
	       `(define ,(string->symbol
			  (string-append base "/" (symbol->string (car x))))
		  ,(cadr x)))
	     constants))))


(include "gamsock-constants.scm")

; This is the definition of the socket type. It should be treated as opaque.

(define-type socket
  id: 98e94265-558a-d985-b3fe-e67f32458c35
  type-exhibitor: macro-type-socket
  constructor: macro-make-socket
  implementer: implement-type-socket
  opaque:
  macros:
  prefix: macro-
  predicate: macro-socket?
  (fd unprintable:)
  port
  (will unprintable:))

(implement-type-socket)

;; FIXME: This depends on lambdanative!
(define gamsock:exception-handler
  (lambda (ex)
    (log-error (thread-name (current-thread)) ": " (exception->string e))))

(define (socket-port sock)
  (define (start! name proc cleanup!)
    (thread-start!
     (make-thread
      (lambda ()
        (with-exception-catcher gamsock:exception-handler proc)
        (cleanup!))
      name)))
  (receive
   (c s) (open-u8vector-pipe '(buffering: #t) '(buffering: #t))
   (define (cleanup!)
     (log-debug "gamsock cleaning up" 1)
     (close-socket sock)
     (close-port s)
     ;; (close-port c)
     )
   (start!
    'sender
    (lambda ()
      (define d (make-u8vector 100))
      (let loop ()
        (let ((n (read-subu8vector d 0 (u8vector-length d) s 1)))
          (if (not (= n 0))
              (let ((d (if (< n (u8vector-length d)) (subu8vector d 0 n) d)))
                (if (= (send-message sock d) n)
                    (loop)))))))
    cleanup!)
   (start!
    'receiver
    (lambda ()
      (let loop ()
        (receive
         (d from) (recv-message sock 1000)
         (if d
             (let ((n (u8vector-length d)))
               (if (not (= n 0))
                   (begin
                     (write-subu8vector d 0 n s)
                     (force-output s)
                     (loop))))))))
    cleanup!)
   ;; (thread-yield!) ;; Otherwise might lock on SOME versions of Android at least.
   ;; (log-debug "returning socket port " c)
   c))

; This is the definition of the socket address type.

(c-define-type socket-address (pointer (struct "sockaddr_storage") socket-address))

(c-declare "
size_t
c_sockaddr_size(struct sockaddr_storage *sa_st)
{
    switch(sa_st->ss_family)
    {
        case AF_INET:
          return sizeof(struct sockaddr_in);
	case AF_UNIX:
	  return sizeof(struct sockaddr_un);
	case AF_INET6:
	  return sizeof(struct sockaddr_in6);
	default:
	  return sizeof(struct sockaddr);
    }
}
"
)

; This is the definition of an internal type which holds
; all of the data for an IPv6 address.

(define-type sockaddr-inet6-info
  id: 74065378-a567-ba71-0047-22b413ad9797
  type-exhibitor: macro-type-sockaddr-inet6-info
  constructor: macro-make-sockaddr-inet6-info
  implementer: implement-type-sockaddr-inet6-info
  opaque:
  macros:
  prefix: macro-
  predicate: macro-sockaddr-inet6-info?
  (host unprintable:)
  (port unprintable:)
  (flowinfo unprintable:)
  (scope-id unprintable:))

(implement-type-sockaddr-inet6-info)

(define socket-address-family
  (c-lambda (socket-address) int
	    "
___result = ___arg1->ss_family;
"))

; An exception that is raised when you try to access address information
; from a socket address of the wrong family (e.g., trying to get the IP
; address of a UNIX socket).

(define-record-type invalid-socket-address-family-exception
  (make-invalid-sockaddr-exception n expected-family proc args)
  invalid-sockaddr-exception?
  (n invalid-sockaddr-argument-number)
  (expected-family invalid-sockaddr-exception-expected-family)
  (proc invalid-sockaddr-exception-procedure)
  (args invalid-sockaddr-exception-arguments))

; Socket and socket-address type predicates.

(define (socket-address? obj) (let ((f (foreign-tags obj))) (and f (eq? (car f) 'socket-address))))
(define (socket? obj) (macro-socket? obj))


(define (check-socket-address obj fam n proc args)
  (if (not (socket-address? obj))
      (##raise-type-exception n 'socket-address proc args))
  (if (not (= (socket-address-family obj) fam))
      (raise (make-invalid-sockaddr-exception n fam proc args))))

(define-macro (define-sockaddr-family-pred name family)
  `(define (,name a)
     (and (socket-address? a)
	  (= (socket-address-family a) ,family))))

; Converts a "UNIX address" or file name (Shivers named the procedure,
; not me!) to a socket address for a UNIX socket.

; This does not yet work with character encodings except ASCII/Latin-1.

(define unix-path-max ((c-lambda () int "___result = UNIX_PATH_MAX;")))

(define (unix-address->socket-address fn)
  (let* (
	 (l (string-length fn))
	 )
    (if (>= l unix-path-max)
	(error "unix-address->socket-address: path too long" fn)
	((c-lambda (nonnull-char-string) socket-address "
struct sockaddr_un *sa_un = (struct sockaddr_un *)malloc(sizeof(struct sockaddr_un));
if(sa_un != NULL) {
    sa_un->sun_family = AF_UNIX;
    strncpy(sa_un->sun_path,___arg1,UNIX_PATH_MAX);
    sa_un->sun_path[UNIX_PATH_MAX - 1] = '\\0';
}
___result_voidstar = (void *)sa_un;
") fn))))

(c-declare
#<<end-of-c-declare
#if defined(__ANDROID__) || defined(_WIN32) || defined(__linux__)
#include <stddef.h>
static socklen_t set_socket_name(struct sockaddr_un *socket_name, const char *filename)
{
  size_t fn_length = strlen(filename), off=1;
  const char prefix[] = "FIXMEaskemosSHOULDbechanged";
  const char suffix[] = "FIXMEaskemosSHOULDbechanged";
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

(define (abstract-address->socket-address fn)
  (let* (
	 (l (string-length fn))
	 )
    (if (>= l unix-path-max) ;; FIXME this MUST exclude prefix and suffix and leading '\0'
	(error "abstract-address->socket-address: path too long" fn)
	((c-lambda (nonnull-char-string) socket-address "
struct sockaddr_un *sa_un = (struct sockaddr_un *)malloc(sizeof(struct sockaddr_un));
if(sa_un != NULL) {
    sa_un->sun_family = AF_UNIX;
    set_socket_name(sa_un, ___arg1);
}
___result_voidstar = (void *)sa_un;
") fn))))

; Predicate for UNIX-socket-address-ness.

(define-sockaddr-family-pred unix-socket-address? address-family/unix)

; Given a UNIX socket address, returns the "address" (file name) for the
; socket.

(define (socket-address->unix-address a)
  ((c-lambda (socket-address) nonnull-char-string "
struct sockaddr_un *sa_un = (struct sockaddr_un *)(___arg1);
___result = sa_un->sun_path;
") a))

(define (integer->network-order-vector-16 n)
  (u8vector
   (bitwise-and (arithmetic-shift n -8) 255)
   (bitwise-and n 255)))

(define (integer->network-order-vector-32 n)
  (u8vector
   (bitwise-and (arithmetic-shift n -24) 255)
   (bitwise-and (arithmetic-shift n -16) 255)
   (bitwise-and (arithmetic-shift n -8) 255)
   (bitwise-and n 255)))

(define (network-order-vector->integer-16 v)
  (bitwise-ior
   (arithmetic-shift (u8vector-ref v 0) 8)
   (u8vector-ref v 1)))

(define (network-order-vector->integer-32 v)
  (bitwise-ior
   (arithmetic-shift (u8vector-ref v 0) 24)
   (arithmetic-shift (u8vector-ref v 1) 16)
   (arithmetic-shift (u8vector-ref v 2) 8)
   (u8vector-ref v 3)))

(define (raise-not-an-ip-address)
  (error "not an ip address"))

(define (check-ip4-address v)
  (let* ((e raise-not-an-ip-address))
    (if (not (and (u8vector? v) (= (u8vector-length v) 4))) (e))))

(define (check-ip6-address v)
  (let* ((e raise-not-an-ip-address))
    (if (not (and (u8vector? v) (= (u8vector-length v) 16))) (e))))

; Some important IPv4 address and port constants.

(define ip-address/any (u8vector 0 0 0 0))
(define ip-address/loopback (u8vector 127 0 0 1))
(define ip-address/broadcast (u8vector 255 255 255 255))

(define port/any 0)

; Creates a new IPv4 socket address from a host IP address
; and port number.

(define (internet-address->socket-address host port)
  (check-ip4-address host)
  ((c-lambda (scheme-object int) socket-address
"
struct sockaddr_storage *sa_st = (struct sockaddr_storage *)malloc(sizeof(struct sockaddr_storage));
struct sockaddr_in *sa_in = (struct sockaddr_in *)sa_st;
if(sa_in != NULL) {
    sa_in->sin_family = AF_INET;
    sa_in->sin_port = htons(___arg2);
    memcpy((void *)(&(sa_in->sin_addr)),(const void *)___BODY_AS(___arg1,___tSUBTYPED),sizeof(struct in_addr));
}
___result_voidstar = sa_st;
") host port))

; IPv4 socket-address predicate.

(define-sockaddr-family-pred internet-socket-address? address-family/internet)

; Returns the address (host and port number as 2 values) of
; an IPv4 socket address.

(define (socket-address->internet-address a)
  (check-socket-address a address-family/internet 0 socket-address->internet-address (list a))
  (let ((portno ((c-lambda (socket-address) int "
struct sockaddr_in *sa_in = (struct sockaddr_in *)(___arg1);
___result = ntohs(sa_in->sin_port);
") a))
	(ip-addr (make-u8vector 4)))
    ((c-lambda (socket-address scheme-object) void "
struct sockaddr_in *sa_in = (struct sockaddr_in *)(___arg1);
memcpy((void *)___BODY_AS(___arg2,___tSUBTYPED),(const void *)(&(sa_in->sin_addr)),4);
") a ip-addr)
    (values ip-addr portno)))

; Creates a new IPv6 socket address from a host IP address,
; port number, flow info and scope ID.

(define (internet6-address->socket-address host port flowinfo scope-id)
  (check-ip6-address host)
  ((c-lambda (scheme-object int int int) socket-address
"
struct sockaddr_storage *sa_st = (struct sockaddr_storage *)malloc(sizeof(struct sockaddr_storage));
struct sockaddr_in6 *sa_in6 = (struct sockaddr_in6 *)sa_st;
if(sa_in6 != NULL) {
    sa_in6->sin6_family = AF_INET;
    sa_in6->sin6_port = htons(___arg2);
    sa_in6->sin6_flowinfo = htonl(___arg3);
    sa_in6->sin6_scope_id = htonl(___arg4);
    memcpy((void *)(&(sa_in6->sin6_addr)),(const void *)___BODY_AS(___arg1,___tSUBTYPED),sizeof(struct in6_addr));
}
___result_voidstar = sa_st;
") host port flowinfo scope-id))

; IPv6 socket-address predicate.

(define-sockaddr-family-pred internet6-socket-address? address-family/internet6)

; Returns the IPv6 address info associated with an IPv6
; socket address.

(define (socket-address->internet6-address a)
  (check-socket-address a address-family/internet6 0 socket-address->internet-address (list a))
  (let ((port ((c-lambda (socket-address) int
"___result = ((struct sockaddr_in6 *)___arg1)->sin6_port;") a))
	(flowinfo ((c-lambda (socket-address) int
"___result = ((struct sockaddr_in6 *)___arg1)->sin6_flowinfo;") a))
	(scope-id ((c-lambda (socket-address) int
"___result = ((struct sockaddr_in6 *)___arg1)->sin6_scope_id;") a))
	(ip6-addr (make-u8vector 16)))
    ((c-lambda (socket-address scheme-object) void "
struct sockaddr_in6 *sa_in6 = (struct sockaddr_in6 *)(___arg1);
memcpy((void *)___BODY_AS(___arg2,___tSUBTYPED),(const void *)(&(sa_in6->sin6_addr)),16);
") a ip-addr)
    (values ip-addr portno flowinfo scope-id)))

; Creates a new unspecified socket address.

(define (make-unspecified-socket-address)
  ((c-lambda () socket-address "
struct sockaddr_storage *sa_st = (struct sockaddr_storage *)malloc(sizeof(struct sockaddr_storage));
if(sa_st != NULL) {
sa_st->ss_family = AF_UNSPEC;
}
___result_voidstar = (void *)sa_st;
")))

;; ; Predicate to test for an unspecified socket address.

;; (define-sockaddr-family-pred unspecified-socket-address? address-family/unspecified)

;; ; All socket related procedures propagate errors from the operating system
;; ; by raising a Gambit os-exception with the errno as the exception code.
;; ; The exceptions are EAGAIN, EWOULDBLOCK, and EINTR; all of which
;; ; simply retry the operation until it's successful or raises another error.

(define errno (c-lambda () int "___result = errno;"))

(define (raise-socket-exception-if-error thunk proc . args)
  (let loop
      ((b (thunk)))
    (if (< b 0)
	(let* ((e (errno)))
	  (if (or
	       (= e errno/again)
	       (= e errno/wouldblock)
	       (= e errno/intr))
	      (begin
		;; (thread-yield!) ; to avoid tying up the CPU
		(loop (thunk)))
	      (apply
	       ##raise-os-exception
	       (append
		(list
		 #f
		 e
		 proc
		 ((c-lambda (int) char-string "strerror") e)
		 )
		args
		))))
	  b)))

(define-macro (macro-really-make-socket fd)
  `(let* ((port (##open-predefined 3 'socket ,fd))
	  (sockobj (macro-make-socket ,fd port #f)))
    (macro-socket-will-set! sockobj
			    (make-will sockobj (lambda (s) (close-socket s))))
    sockobj))

; These are C wrappers for the socket-related system calls exposed by gamsock.
; They are kept in a private namespace "gamsock-c#". MESSING WITH THEM IS BAD
; JUJU as their exact interfaces are likely to change. Gamsock's external
; interface is likely to be stable, leaving our internals to be AS MESSY AS
; WE WANNA BE.

(namespace ("gamsock-c#" c-socket c-bind c-connect c-send c-sendto c-recvfrom c-recv
	    c-listen c-accept c-do-boolean-socket-option
	    c-do-integer-socket-option c-do-timeout-socket-option
	    c-do-boolean-set-socket-option c-do-integer-set-socket-option
	    c-do-timeout-set-socket-option c-close
            c-make-socket-nonblocking!))
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
___result = ioctlsocket(s, FIONBIO, (void *)&s) != SOCKET_ERROR ? 0 : -1;
#else
int fl = fcntl(s,F_GETFL);
___result = fcntl(s,F_SETFL,fl | O_NONBLOCK);
#endif
C-END
))

(define c-bind
  (c-lambda (int socket-address) int #<<C-END
int mysize;
mysize = c_sockaddr_size((struct sockaddr_storage *)(___arg2));
___result = bind(___arg1,(struct sockaddr *)(___arg2),mysize);
C-END
))

(define c-connect (c-lambda (int socket-address) int #<<C-END
int mysize;
mysize = c_sockaddr_size((struct sockaddr_storage *)(___arg2));
___result = connect(___arg1,
                    (struct sockaddr *)___arg2,mysize);
#ifdef _WIN32
 ioctlsocket(___result, FIONBIO, (void *)&___result); // != SOCKET_ERROR)
#else
if(___result == 0) {
   int fl = fcntl(___arg1,F_GETFL);
   ___result = fcntl(___arg1,F_SETFL,fl | O_NONBLOCK);
}
#endif
C-END
))
(define c-send
  (c-lambda (int scheme-object int) int #<<C-END
int soc = ___arg1;
void *buf = ___CAST(void *,___BODY_AS(___arg2,___tSUBTYPED));
size_t bufsiz = ___CAST(size_t,___INT(___U8VECTORLENGTH(___arg2)));
int fl = ___CAST(int,___INT(___arg3));
___result = send(soc,buf,bufsiz,fl);
C-END
))
(define c-sendto
  (c-lambda (int scheme-object int socket-address) int #<<C-END
struct sockaddr_storage *sa = ___arg4;
int sa_size;
int soc = ___arg1;
void *buf = ___CAST(void *,___BODY_AS(___arg2,___tSUBTYPED));
size_t bufsiz = ___CAST(size_t,___INT(___U8VECTORLENGTH(___arg2)));
int fl = ___CAST(int,___INT(___arg3));
sa_size = c_sockaddr_size((struct sockaddr_storage *)sa);
___result = sendto(soc,buf,bufsiz,fl,(struct sockaddr *)sa,sa_size);
C-END
))

(define c-recvfrom
  (c-lambda (int scheme-object int socket-address) int #<<C-END
struct sockaddr_storage *sa = ___arg4;
socklen_t sa_size;
int soc = ___arg1;
void *buf = ___CAST(void *,___BODY_AS(___arg2,___tSUBTYPED));
size_t bufsiz = ___CAST(size_t,___INT(___U8VECTORLENGTH(___arg2)));
int fl = ___CAST(int,___INT(___arg3));
___result = recvfrom(soc,buf,bufsiz,fl,(struct sockaddr *)sa,&sa_size);
C-END
))
(define c-recv
  (c-lambda (int scheme-object int) int #<<C-END
int soc = ___arg1;
void *buf = ___CAST(void *,___BODY_AS(___arg2,___tSUBTYPED));
size_t bufsiz = ___CAST(size_t,___INT(___U8VECTORLENGTH(___arg2)));
int fl = ___CAST(int,___INT(___arg3));
___result = recv(soc,buf,bufsiz,fl);
C-END
))
(define c-listen
  (c-lambda (int int) int #<<C-END
int soc = ___arg1;
___result = listen(soc,___arg2);
C-END
))

(define c-accept
  (c-lambda (int socket-address) int #<<C-END
struct sockaddr_storage *ss = ___arg2;
socklen_t sslen = sizeof(struct sockaddr_storage);
int soc = ___arg1;
int r = accept(soc,(struct sockaddr *)ss,&sslen);
if(r < 0) {
   ___result = r;
}
else {
#ifndef _WIN32
   int fl = fcntl(r,F_GETFL);
   fcntl(r,F_SETFL,fl | O_NONBLOCK);
#endif
   ___result = r;
}
C-END
))


(define c-do-boolean-socket-option
  (c-lambda (int int int scheme-object) int #<<C-END
int optval = 0;
socklen_t optlen = sizeof(optval);
int r;
int soc = ___arg1;
r = getsockopt(soc,___arg2,___arg3,&optval,&optlen);
___VECTORSET(___arg4,___FIX(0L),___FIX(optval));
___result = r;
C-END
))
(define c-do-integer-socket-option
  (c-lambda (int int int scheme-object) int #<<C-END
int optval = 0;
socklen_t optlen = sizeof(optval);
int r;
int soc = ___arg1;
r = getsockopt(soc,___arg2,___arg3,&optval,&optlen);
___VECTORSET(___arg4,___FIX(0L),___FIX(optval));
___result = r;
C-END
))

(define c-do-timeout-socket-option
 (c-lambda (int int int scheme-object) int #<<C-END
struct timeval optval;
socklen_t optlen = sizeof(optval);
int r;
int soc = ___arg1;
r = getsockopt(soc,___arg2,___arg3,&optval,&optlen);
___VECTORSET(___arg4,___FIX(0L),___FIX(optval.tv_sec));
___VECTORSET(___arg4,___FIX(1L),___FIX(optval.tv_usec));
___result = r;
C-END
))

(define c-do-boolean-set-socket-option
 (c-lambda (int int int scheme-object) int #<<C-END
int optval = 0;
socklen_t optlen = sizeof(optval);
int r;
int soc = ___arg1;
if(___arg4 != ___FAL)
{
  optval = 1;
}
else
{
 optval = 0;
}
r = setsockopt(soc,___arg2,___arg3,&optval,optlen);
___result = r;
C-END
))

(define c-do-integer-set-socket-option
  (c-lambda (int int int int) int #<<C-END
int optval = ___arg4;
socklen_t optlen = sizeof(optval);
int r;
int soc = ___arg1;
r = setsockopt(soc,___arg2,___arg3,&optval,optlen);
___result = r;
C-END
))

(define c-do-timeout-set-socket-option
  (c-lambda (int int int int int) int #<<C-END
struct timeval optval;
socklen_t optlen = sizeof(optval);
int r;
int soc = ___arg1;
optval.tv_sec = ___arg4;
optval.tv_usec = ___arg5;
r = setsockopt(soc,___arg2,___arg3,&optval,optlen);
___result = r;
C-END
))

(define c-close
  (c-lambda (int) int "___result = close(___arg1);"))


; GAMSOCK API begins here.
; Closes an open socket.

(define (close-socket sock)
  (let ((p (macro-socket-port sock))
	(fd (macro-socket-fd sock)))
    (if (>= fd 0) (macro-socket-fd-set! sock -1))
    #;(when p
	  (macro-socket-port-set! sock #f)
	  (close-port p))
    (if (>= fd 0) (c-close fd))))

(define (socket-closed? sock)
  (if (not (socket? sock))
      (##raise-type-exception 0 'socket socket-closed? (list sock))
      (< (macro-socket-fd sock) 0)))

; Creates a new socket of the specified domain (protocol family),
; type (e.g., stream, datagram), and optional protocol.

(define (make-socket-nonblocking! fd)
  (raise-socket-exception-if-error
   (lambda () (c-make-socket-nonblocking! fd))
   make-socket-nonblocking!))

(define (create-socket domain type #!optional (protocol 0))
  (let ((fd (raise-socket-exception-if-error
             (lambda () (c-socket domain type protocol))
             create-socket)))
    (with-exception-catcher
     (lambda (x) (c-close fd) (error x))
     (lambda () (make-socket-nonblocking! fd)))
    (macro-really-make-socket fd)))

; Binds a socket to a local address.

(define (bind-socket sock addr)
    (if (not (socket? sock))
	(##raise-type-exception
	 0 'socket bind-socket (list sock addr))
	(if (not (socket-address? addr))
	    (##raise-type-exception
	     1 'socket-address bind-socket (list sock addr))
	    (raise-socket-exception-if-error
	     (lambda () (c-bind (macro-socket-fd sock) addr)) bind-socket)))
      (macro-socket-port-set! sock (##open-predefined 1 addr (macro-socket-fd sock)))
    (if #f #f))

; Connects a socket to a remote address.

(define (connect-socket sock addr)

    (if (not (socket? sock))
	(##raise-type-exception 0 'socket connect-socket (list sock addr))
	(if (not (socket-address? addr))
	    (##raise-type-exception 1 'socket-address connect-socket (list sock addr))
	    (raise-socket-exception-if-error (lambda () (c-connect (macro-socket-fd sock) addr)) connect-socket)))
    (if #f #f))

; Sends a message on a socket. The message must be a u8vector or, if
; start and end parameters are given, a slice of the u8vector bound by
; the start and end params.

; Optional flags and a destination address may also be specified; the latter
; is only useful for connectionless sockets (e.g., UDP/IP).

(define (send-message sock vec #!optional (start 0) (end #f) (flags 0)
		      (addr #f))
  (let ((svec (if (and (= start 0) (not end)) vec
		   (subu8vector vec
				start
				(if (not end) (u8vector-length vec) end)))))

    (if (not (socket? sock))
	(##raise-type-exception 0 'socket send-message (list sock vec start end flags addr)))
    (if (not (u8vector? vec))
	(##raise-type-exception 1 'u8vector send-message (list sock vec start end flags addr)))
    (if (< (macro-socket-fd sock) 0) -1
        (if (not addr)
            (raise-socket-exception-if-error
             (lambda ()
               (##wait-output-port (macro-socket-port sock))
               (c-send (macro-socket-fd sock) svec flags))
             send-message)
            (if (not (socket-address? addr))
                (##raise-type-exception
                 3 'socket-address send-message
                 (list sock vec start end flags addr))
                (raise-socket-exception-if-error
                 (lambda ()
                   (##wait-output-port (macro-socket-port sock))
                   (c-sendto (macro-socket-fd sock) svec flags addr))
                 send-message))))))

; Receives a message from a socket of a given length and returns it as a
; u8vector. Optional flags may be specified. This procedure actually returns
; two values: the received message and the source address.

(define (receive-message sock len #!optional (flags 0))
  (let ((addr (make-unspecified-socket-address))
	(vec (make-u8vector len 0)))
    (if (not (socket? sock))
	(##raise-type-exception
	 0 'socket receive-message (list sock len flags)))
    ;; (log-debug "waiting for message" 1)
    (let* ((size-actually-recvd
	    (raise-socket-exception-if-error
	     (lambda ()
	       (##wait-input-port (macro-socket-port sock))
	       (thread-sleep! 0.1) ;; Argh
	       (c-recvfrom (macro-socket-fd sock) vec flags addr))
	     receive-message)))
      ;; (log-debug "received bytes:" size-actually-recvd)
      (values
       (subu8vector vec 0 size-actually-recvd)
       addr))))

;; Same as receive-message, but returns #f as second value and does
;; not ask for address.
(define (recv-message sock len #!optional (flags 0))
  (if (not (socket? sock))
      (##raise-type-exception
       0 'socket receive-message (list sock len flags)))
  (let ((fd (macro-socket-fd sock)))
    (if (< fd 0)
        (values #f #f)
        (let ((vec (make-u8vector len 0)))
          ;; (log-debug "waiting for message" 1)
          (let* ((size-actually-recvd
                  (raise-socket-exception-if-error
                   (lambda ()
                     (##wait-input-port (macro-socket-port sock))
                     (thread-sleep! 0.1) ;; Argh
                     (c-recv fd vec flags))
                   recv-message)))
            ;; (log-debug "received bytes:" size-actually-recvd)
            (values
             (subu8vector vec 0 size-actually-recvd)
             #f))))))

; Sets up a socket to listen for incoming connections, with the specified number
; of backlogged connections allowed.

(define (listen-socket sock backlog)
  (if (not (socket? sock))
      (##raise-type-exception
       0 'socket listen-socket (list sock backlog)))
  (raise-socket-exception-if-error
   (lambda () (c-listen (macro-socket-fd sock) backlog)) listen-socket)
  (if #f #f)
  )

; Returns the local socket address of the socket.

(define (socket-local-address sock)
  (let* (
	 (dummy-sockaddr (make-unspecified-socket-address))
	 (c-getsockname
	  (c-lambda (int socket-address) int
		    "
struct sockaddr_storage* ss = ___arg2;
socklen_t sslen = sizeof(struct sockaddr_storage);
int soc = ___arg1;
int r = getsockname(soc,(struct sockaddr *)&ss,&sslen);
___result = r;
")))
    (if (not (socket? sock))
	(##raise-type-exception 0 'socket socket-local-address (list sock)))
    (raise-socket-exception-if-error (lambda ()
				       (c-getsockname (macro-socket-fd sock) dummy-sockaddr)) socket-local-address)
    dummy-sockaddr))

; Returns the remote socket address of a socket.

(define (socket-remote-address sock)
  (let* (
	 (dummy-sockaddr (make-unspecified-socket-address))
	 (c-getpeername
	  (c-lambda (int socket-address) int
		    "
struct sockaddr_storage ss;
socklen_t sslen = sizeof(struct sockaddr_storage);
int soc = ___arg1;
int r = getpeername(soc,(struct sockaddr *)&ss,&sslen);
___result = r;
")))
    (if (not (socket? sock))
	(##raise-type-exception 0 'socket socket-remote-address (list sock)))
    (raise-socket-exception-if-error (lambda ()
				       (c-getpeername (macro-socket-fd sock) dummy-sockaddr)) socket-remote-address)
    dummy-sockaddr))

; Accepts a connection on a socket. Returns two values: a new socket corresponding to
; the connection, and the address of the other side of the connection.

(define (accept-connection sock)
  (let ((dummy-sockaddr (make-unspecified-socket-address)))
    (if (not (socket? sock))
	(##raise-type-exception
	 0 'socket accept-connection (list sock)))
    (let ((s2
	   (raise-socket-exception-if-error
	    (lambda ()
	      (##wait-input-port (macro-socket-port sock))
	      (c-accept (macro-socket-fd sock) dummy-sockaddr))
	    accept-connection)))
      (values (macro-really-make-socket s2) dummy-sockaddr))))

(define (boolean-socket-option? optname)
  (member optname boolean-socket-options))
(define (integer-socket-option? optname)
  (member optname integer-socket-options))
(define (timeout-socket-option? optname)
  (member optname timeout-socket-options))

; ### Socket Option Getters ###

(define (do-boolean-socket-option socket level optname)
  (let ((v (make-vector 1)))
    (if (not (socket? socket))
	(##raise-type-exception 0
				'socket
				socket-option
				(list socket level optname)))
    (if (not (integer? level))
	(##raise-type-exception 1
				'integer
				socket-option
				(list socket level optname)))
    (if (not (integer? optname))
	(##raise-type-exception 2
				'integer
				socket-option
				(list socket level optname)))
    (raise-socket-exception-if-error
     (lambda () (c-do-boolean-socket-option (macro-socket-fd socket)
					    level
					    optname
					    v)) socket-option)
    (not (zero? (vector-ref v 0)))))

(define (do-integer-socket-option socket level optname)
  (let ((v (make-vector 1)))
    (if (not (socket? socket))
	(##raise-type-exception 0
				'socket
				socket-option
				(list socket level optname)))
    (if (not (integer? level))
	(##raise-type-exception 1
				'integer
				socket-option
				(list socket level optname)))
    (if (not (integer? optname))
	(##raise-type-exception 2
				'integer
				socket-option
				(list socket level optname)))
    (raise-socket-exception-if-error
     (lambda () (c-do-integer-socket-option (macro-socket-fd socket)
					    level
					    optname
					    v)) socket-option)
    (vector-ref v 0)))

(define (do-timeout-socket-option socket level optname)
  (let ((v (make-vector 2)))
    (if (not (socket? socket))
	(##raise-type-exception 0
			        'socket
				socket-option
				(list socket level optname)))
    (if (not (integer? level))
	(##raise-type-exception 1
				'integer
				socket-option
				(list socket level optname)))
    (if (not (integer? optname))
	(##raise-type-exception 2
				'integer
				socket-option
				(list socket level optname)))
    (raise-socket-exception-if-error
     (lambda () (c-do-timeout-socket-option (macro-socket-fd socket)
					    level
					    optname
					    v)) socket-option)
    (+
     (vector-ref v 0)
     (/ (vector-ref v 1) 1000000.0))))

(define (socket-option socket level optname)
  (cond
   ((boolean-socket-option? optname)
    (do-boolean-socket-option socket level optname))
   ((integer-socket-option? optname)
    (do-integer-socket-option socket level optname))
   ((timeout-socket-option? optname)
    (do-timeout-socket-option socket level optname))
   (else
    (error "unsupported socket option"))))

; ### Socket option setters ###
(define (do-boolean-set-socket-option socket level optname optval)
    (if (not (socket? socket))
	(##raise-type-exception 0
				'socket
				socket-option
				(list socket level optname optval)))
    (if (not (integer? level))
	(##raise-type-exception 1
				'integer
				socket-option
				(list socket level optname optval)))
    (if (not (integer? optname))
	(##raise-type-exception 2
				'integer
				socket-option
				(list socket level optname optval)))
    (raise-socket-exception-if-error
     (lambda () (c-do-boolean-set-socket-option (macro-socket-fd socket)
					    level
					    optname
					    optval)) socket-option)
    #!void)

(define (do-integer-set-socket-option socket level optname optval)
    (if (not (socket? socket))
	(##raise-type-exception 0
				'socket
				socket-option
				(list socket level optname optval)))
    (if (not (integer? level))
	(##raise-type-exception 1
				'integer
				socket-option
				(list socket level optname optval)))
    (if (not (integer? optname))
	(##raise-type-exception 2
				'integer
				socket-option
				(list socket level optname optval)))
    (if (not (integer? optval))
	(##raise-type-exception 3
				'integer
				socket-option
				(list socket level optname optval)))
    (raise-socket-exception-if-error
     (lambda () (c-do-integer-set-socket-option (macro-socket-fd socket)
					    level
					    optname
					    optval)) socket-option)
    #!void)

(define (do-timeout-set-socket-option socket level optname optval)
    (if (not (socket? socket))
	(##raise-type-exception 0
				'socket
				socket-option
				(list socket level optname optval)))
    (if (not (integer? level))
	(##raise-type-exception 1
				'integer
				socket-option
				(list socket level optname optval)))
    (if (not (integer? optname))
	(##raise-type-exception 2
				'integer
				socket-option
				(list socket level optname optval)))
    (if (not (real? optval))
	(##raise-type-exception 3
				'integer
				socket-option
				(list socket level optname optval)))
    (let* ((sec (inexact->exact (truncate optval)))
	   (usec (inexact->exact (truncate (* (- optval sec) 1000000.)))))
      (raise-socket-exception-if-error
       (lambda () (c-do-timeout-set-socket-option (macro-socket-fd socket)
						  level
						  optname
						  sec
					    usec)) socket-option))
    #!void)

(define (set-socket-option socket level optname optval)
  (cond
   ((boolean-socket-option? optname)
    (do-boolean-set-socket-option socket level optname optval))
   ((integer-socket-option? optname)
    (do-integer-set-socket-option socket level optname optval))
   ((timeout-socket-option? optname)
    (do-timeout-set-socket-option socket level optname optval))
   (else
    (error "unsupported socket option"))))

(namespace (""))

(set! ##type-exception-names (cons '(socket . "SOCKET object")
				   (cons '(socket-address . "SOCKET ADDRESS")
					 ##type-exception-names)))
