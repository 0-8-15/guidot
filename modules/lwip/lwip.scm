#|;;;* FEATURE SWITCH lwip-requires-pthread-locks
(define-cond-expand-feature lwip-requires-pthread-locks)
;;;|#

(define $lwip-debug (make-parameter #f))

(define (lwip-exception-handler exn)
  (when
   ($lwip-debug)
   (display "lwIP Exception: " (current-error-port))
   (##default-display-exception exn (current-error-port)))
  #f)

;;;* Global Syntax Imports

(define-macro (c-safe-lambda formals return c-code)
  (let ((tmp (gensym 'c-safe-lambda-result))
        (argument-names
         (map
          (lambda (n) (string->symbol (string-append "arg" (number->string n))))
          (iota (length formals)))))
    `(lambda ,argument-names
       (##safe-lambda-lock! ,c-code)
       (let ((,tmp ((c-lambda ,formals ,return ,c-code) . ,argument-names)))
         (##safe-lambda-unlock! ,c-code)
         ,tmp))))

(define-macro (delay-until-after-return expr) `(lambda () ,expr))
(define-macro (after-safe-return expr)
  ;; Schedule EXPR for execution (one way or another) and return nonsense.
  `(if (##in-safe-callback?)
       (delay-until-after-return ,expr)
       (begin (debug 'lwip-after-safe-return:not-in-callback ',expr) ,expr)))

(declare
 (standard-bindings)
 (extended-bindings) ;; no overwrites of standard bindings
 (not standard-bindings thread-start!) ;; except this
 (block)
 )

#;(declare (debug))

;;;* Local Syntax

(define-macro (lwip/after-safe-return expr)
  `(after-safe-return ,expr))

(define-macro (define-c-constant var type . const)
  (let* ((const (if (not (null? const)) (car const) (symbol->string var)))
	 (str (string-append "___result = " const ";")))
    `(define ,var ((c-lambda () ,type ,str)))))

(define-macro (define-custom name initial)
  `(define ,name
     (let ((val ,initial))
       (case-lambda
        (() val)
        ((x) (set! val x))))))

(define-macro (start! expr)
  (thread-start! (make-thread (lambda () ,expr)) 'start!))

(define lwip-ip6addr-any '#u8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
(define lwip-ip6addr-loopback '#u8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
(define lwip-ip4addr-any '#u8(0 0 0 0))
(define lwip-ip4addr-loopback '#u8(127 0 0 1))
(define lwip-ip4addr-broadcast '#u8(255 255 255 255))

(define lwip-host-is-network-endian
  ;; big endian needs no conversion => #t
  ;;
  ;; this is just informal; intented for debugging
  (c-lambda () bool "___result = htons(1)==1;"))

;;; Constants

(c-declare #<<c-declare-end

typedef enum {false=0, true=1} bool;

#include "netif/ethernet.h"
#include "lwip/netif.h"
#include "lwip/etharp.h"
#include "lwip/tcpip.h"
#include "lwip/mem.h"
#include "lwip/memp.h"
#include "lwip/sys.h"
#include "lwip/tcp.h"
#include "lwip/timeouts.h"
#include "lwip/stats.h"
#include "lwip/ethip6.h"
#include "lwip/ip_addr.h"
#include "lwip/nd6.h"
#include "lwip/netifapi.h"
#include "lwip/stats.h"
c-declare-end
)

(define-c-constant ERR_OK int8)
(define-c-constant ERR_MEM int8)
(define-c-constant ERR_BUF int8)
(define-c-constant ERR_TIMEOUT int8)
(define-c-constant ERR_RTE int8)
(define-c-constant ERR_INPROGRESS int8)
(define-c-constant ERR_VAL int8)
(define-c-constant ERR_WOULDBLOCK int8)
(define-c-constant ERR_USE int8)
(define-c-constant ERR_ALREADY int8)
(define-c-constant ERR_ISCONN int8)
(define-c-constant ERR_CONN int8)
(define-c-constant ERR_IF int8)
(define-c-constant ERR_ABRT int8)
(define-c-constant ERR_RST int8)
(define-c-constant ERR_CLSD int8)
(define-c-constant ERR_ARG int8)

(define lwip-err
  (let ((all
         '#(#f ;;ERR_OK
            ERR_MEM
            ERR_BUF
            ERR_TIMEOUT
            ERR_RTE
            ERR_INPROGRESS
            ERR_VAL
            ERR_WOULDBLOCK
            ERR_USE
            ERR_ALREADY
            ERR_ISCONN
            ERR_CONN
            ERR_IF
            ERR_ABRT
            ERR_RST
            ERR_CLSD
            ERR_ARG)))
    (lambda (nr)
      (vector-ref all (* nr -1)))))

(include "datastructures.scm")

(define (lwip-string->ip6-address str)
  (let ((result (make-u8vector 16)))
    (and
     ((c-lambda
       (char-string scheme-object) bool
       "___return(ip6addr_aton(___arg1,___CAST(ip6_addr_t*, ___BODY(___arg2))));")
      str result)
     result)))

(define (lwip-string->ip4-address str)
  (let ((result (make-u8vector 4)))
    (and
     ((c-lambda
       (char-string scheme-object) bool
       "___return(ip4addr_aton(___arg1,___CAST(ip4_addr_t*, ___BODY(___arg2))));")
      str result)
     result)))

;;; C Types

(c-define-type void* (pointer "void"))

(c-define-type nonnull-void* (nonnull-pointer "void"))

(c-define-type err_t int8)

(c-declare "static ___SCMOBJ release_netif(void *p);")
(c-define-type netif* (pointer (struct "netif") netif #;"release_netif")) ;; trying make-will

;;; LWIP Hooks

(define-custom lwip-nd6-get-gateway #f) ;; EXPORT HOOK - return netif for destination
(c-define
 (gambit-lwip-nd6-get-gw netif dest)
 (netif* void*) void*
 "scm_lwip_nd6_get_gw" "static"
 (let ((hook (lwip-nd6-get-gateway)))
   (and hook (hook netif dest))))

(c-declare #<<c-declare-end

struct lwip_nd6_get_gw__args { struct netif *netif; const ip6_addr_t *dest; const ip6_addr_t *result; };

static int call_scm_lwip_nd6_get_gw(void *in, struct lwip_nd6_get_gw__args *args)
{
 // unused(in);
 args->result = scm_lwip_nd6_get_gw(args->netif, (void*) args->dest);
 return 0; //unused
}

// just for gambit_lwip_nd6_get_gw debugging
#include <netinet/in.h>
#include <arpa/inet.h>

const ip6_addr_t *gambit_lwip_nd6_get_gw(struct netif *netif, const ip6_addr_t *dest)
{
 // return scm_lwip_nd6_get_gw(netif, dest);
/*
 struct lwip_nd6_get_gw__args r = {netif, dest, NULL};
 lwip_calling_back(NULL, call_scm_lwip_nd6_get_gw, &r);
 return r.result;
 */
 char buf[46];
ip6_addr_t dummy;
ip6_addr_set(&dummy, dest);
inet_ntop(AF_INET6, dest->addr, buf, INET6_ADDRSTRLEN);
dest=NULL;
fprintf(stderr, "RETURN %p %s\n", dest, buf);
return dest;
}

c-declare-end
)

;;; PBUF

(c-declare
 "static ___SCMOBJ g_release_pbuf(struct pbuf *p) {pbuf_free(p); return(___FIX(___NO_ERR));}")

(c-define-type pbuf (pointer (struct "pbuf") pbuf #;"g_release_pbuf"))  ;; MUST NOT have a release function.

(define pbuf-ref (c-lambda (pbuf size_t) unsigned-int8 "pbuf_get_at"))
(define pbuf-length (c-lambda (pbuf) unsigned-int16 "___return(___arg1->tot_len);"))
(define pbuf-references (c-lambda (pbuf) unsigned-int "___return(___arg1->ref);"))

;;;** pbuf->u8vector
;;;
;;; return fresh u8vector with a COPY of the pbuf section

(define %lwip-allocate-u8vector-for-pbuf
  ;; FIXME: Performance: How to allocate without useless initialization?
  (c-lambda
   (pbuf size_t int) scheme-object
   "___SCMOBJ result = ___EXT(___alloc_scmobj) (___PSTATE, ___sU8VECTOR, (___arg3 < 0 ? ___arg1->tot_len : ___arg3) - ___arg2);
if(___FIXNUMP(result)) { fprintf(stderr, \"OH NOOO\\n\"); ___return(___FAL); }
___EXT(___release_scmobj)(result);
___return(result);"))

(c-declare #<<end-pbuf-cdeclare
#include <string.h>
static inline void copy_pbuf_to_ptr(char *bp, struct pbuf *from, size_t offset, size_t limit)
// precondition: arguments are valid
{
 struct pbuf *q = from;
 if(offset > from->tot_len) return; // ok, avoid very bad cases

 /* skip offset and copy first pbuf */
 for(q=from; q != NULL && offset > 0; q = q->next) {
   if(offset < q->len) {
    size_t using = q->len-offset;
    memcpy(bp, q->payload+offset, using);
    q = q->next;
    bp+=using;
    limit-=using;
    break;
   } else {
    offset -= q->len;
  }
 }
 // fprintf(stderr, "copy_pbuf_to_ptr: collect chained buffer into single one\n");
 for (; q != NULL && limit > 0; q = q->next) {
    size_t using = q->len;
    using = using > limit ? limit : using;
    memcpy(bp, q->payload, q->len);
    limit-=using;
    bp += q->len;
  }
}
static inline void copy_pbuf_from_ptr(struct pbuf *to, const void* src, size_t offset, size_t limit)
// precondition: arguments are valid
{
 struct pbuf *q = to;
 const char *bp=src;
 if(offset > to->tot_len) return; // ok, avoid very bad cases
 if(offset + limit > to->tot_len) limit = to->tot_len - offset; // should better throw error?

 /* skip offset and copy first pbuf */
 for(q=to; q != NULL && offset > 0; q = q->next) {
   if(offset < q->len) {
    size_t using = q->len-offset;
    memcpy(q->payload+offset, bp, using);
    q = q->next;
    bp+=using;
    limit-=using;
    break;
   } else {
    offset -= q->len;
  }
 }
 // fprintf(stderr, "copy_pbuf_to_ptr: collect chained buffer into single one\n");
 for (; q != NULL && limit > 0; q = q->next) {
    size_t using = q->len;
    using = using > limit ? limit : using;
    memcpy(q->payload, bp, q->len);
    limit-=using;
    bp += q->len;
  }
}
static inline ___SCMOBJ pbuf_to_u8v_copied(___SCMOBJ to, struct pbuf *from, size_t offset, size_t limit)
{
 struct pbuf *q = from;
 char *bp;
 if(offset > from->tot_len) return ___FAL;
 /* nonono, release_scmobj!!! to = ___EXT(___alloc_scmobj) (___PSTATE, ___sU8VECTOR, p->tot_len - sizeof(ethhdr));
 if(___FIXNUMP(to)) {
   return ___FAL;
 }
 //*/
 bp = ___CAST(char*, ___BODY(to));
 pbuf_copy_partial(from, bp, limit, offset);
 return to;
}
end-pbuf-cdeclare
)

(define (pbuf->u8vector pbuf #!optional (offset 0) (limit #f))
  (let ((result (%lwip-allocate-u8vector-for-pbuf pbuf offset (or limit -1))))
    (unless result (error (debug result "allocation failed"))) ;;; ???
    ((c-lambda (scheme-object pbuf size_t size_t) scheme-object "pbuf_to_u8v_copied")
     result pbuf offset (u8vector-length result))))

(define pbuf-copy-to-ptr!
  (c-lambda (void* pbuf size_t size_t) void "copy_pbuf_to_ptr"))

(define make-pbuf-raw+ram
  (c-lambda (size_t) pbuf "___return(pbuf_alloc(PBUF_RAW, ___arg1, PBUF_RAM));"))

(define pbuf-release! (c-lambda (pbuf) scheme-object "g_release_pbuf"))

(define pbuf-add-reference! (c-lambda (pbuf) void "pbuf_ref"))

(define (make-pbuf-raw+ram/will size)
  (let ((result (make-pbuf-raw+ram size)))
    (make-will result pbuf-release!)
    result))

(define pbuf-copy-from-ptr!
  (c-lambda (pbuf void* size_t size_t) void "copy_pbuf_from_ptr"))

(define pbuf-copy-from-u8vector!
  (c-lambda
   (pbuf size_t scheme-object size_t size_t) bool
   "size_t pbuf_offset=___arg2, input_offset = ___arg4, len = ___arg5;
___return(pbuf_take_at(___arg1, ___BODY(___arg3) + input_offset, len, pbuf_offset) == ERR_OK);"))

;;; LOCKING

(c-declare
;;; This section is the only one, which MAY use {UN}LOCK_TCPIP_CORE
#<<lwip-core-lock-end
#if NO_SYS
#define gambit_lwipcore_lock(x) {}
#define gambit_lwipcore_unlock() {}
#else
//*
#define LG_TRACELOCK(x) // {fprintf x;}
#include <pthread.h>

static pthread_t the_tcp_pthread = 0;

static /*inline*/ void gambit_lwipcore_lock(const char *src)
{
 if(src) LG_TRACELOCK((stderr, "gambit_lwipcore O %x %s\n", pthread_self(), src));
 if( the_tcp_pthread != pthread_self()) {
   LOCK_TCPIP_CORE();
 }
 if(src) LG_TRACELOCK((stderr, "gambit_lwipcore P %x %s\n", pthread_self(), src));
}
static inline void gambit_lwipcore_unlock()
{
 LG_TRACELOCK((stderr, "gambit_lwipcore V %x\n", pthread_self()));
 if( the_tcp_pthread != pthread_self()) {
   UNLOCK_TCPIP_CORE();
 }
}
#endif

#define gambit_lwipcore_lock_bg(x) gambit_lwipcore_lock(NULL)
#if NO_SYS
#define gambit_lwipcore_unlock_bg()
#else
#define gambit_lwipcore_unlock_bg() UNLOCK_TCPIP_CORE()
#endif
//*/
#ifndef LG_TRACELOCK
#define LG_TRACELOCK(x) {}
#endif
lwip-core-lock-end
)

(define-macro (delayed-until-after-return? expr) `(procedure? ,expr))

(cond-expand
 (lwip-requires-pthread-locks
  (define-macro (c-lambda-with-lwip-locked formals types ret code)
    ;; BEWARE: MUST NOT use the ___return macro!  But assign to
    ;; ___result
    `(c-lambda
      ,types ,ret
      ,(string-append
        "\ngambit_lwipcore_lock(" (with-output-to-string (lambda () (write code))) ");\n"
        code
        "\ngambit_lwipcore_unlock();\n"))))
 (define (%lwip-post! promise)
   (error "%lwip-post! NYI for lwip-requires-pthread-locks"))
 (quasi-template
  (define *lwip-mutex* (make-mutex '*lwip*))
  (mutex-specific-set! *lwip-mutex* '())
  (define (%lwip-lock!)
    (if (eq? (debug (list 'lwIP-O (current-thread)) (mutex-state *lwip-mutex*)) (current-thread))
        (error (debug 'DEAD "lwIP: DEADLOCK") (current-thread)))
    (mutex-lock! *lwip-mutex*)
    (debug (list 'lwIP-P (current-thread)) (mutex-state *lwip-mutex*)))
  (define (%lwip-unlock!)
    (let ((post (mutex-specific *lwip-mutex*)))
      (mutex-specific-set! *lwip-mutex* '())
      (debug (list 'lwIP-V (current-thread)) (mutex-state *lwip-mutex*))
      (mutex-unlock! *lwip-mutex*)
      (for-each force post)))
  (define (%lwip-post! result)
    (if (delayed-until-after-return? result)
        (begin
          (mutex-specific-set! *lwip-mutex* (cons result (mutex-specific *lwip-mutex*)))
          ERR_OK)
        result))
  (define-macro (c-lambda-with-lwip-locked formals types ret code)
    ;; BEWARE: MUST NOT use the ___return macro!  But assign to
    ;; ___result
    (let ((result (gensym 'result)))
      `(lambda ,formals
         (%lwip-lock!)
         (let ((,result ((c-lambda ,types ,ret ,code) . ,formals)))
           (%lwip-unlock!)
           ,result))))
  )
 (else
  (define-macro (%lwip-lock!) '(##safe-lambda-lock! '%lwip-lock!))
  (define-macro (%lwip-unlock!) '(##safe-lambda-unlock! '%lwip-unlock!))
  (define-macro (%lwip-post! expr)
    (let ((result (gensym 'result)))
      `(let ((,result ,expr))
         (if (delayed-until-after-return? ,result)
             (begin
               (##safe-lambda-post! ,result)
               ERR_OK)
             ,result))))
  (define-macro (c-lambda-with-lwip-locked unused-backward-compatible-formals types ret code)
    `(c-safe-lambda ,types ,ret ,code))
  )) ;; cond-expand

(c-declare
;;; Basic Data Types, Initialization, lwip->gambit calling/locking
 #<<c-declare-end

#include "lwip/init.h" // do we REALLY need that?

#include <sys/socket.h>
#include <netinet/in.h>

/* Define those to better describe your network interface. */
#define IFNAME0 'e'
#define IFNAME1 'n'

#if NO_SYS
#define INPUT_HANDLER netif_input
#else
#define INPUT_HANDLER tcpip_input
#endif

#include <string.h>
/*
#include <stdlib.h>

// snprintf
#include <stdio.h>
//*/

#define LOCAL_MTU LWIP_MTU

typedef struct eth_addr MACREF;
typedef union {
  uint64_t n;
  char c[8];
  struct eth_addr addr;
} u64w;

#define MACDEREF(x) (x).addr
#define UI64_TO_MACREF(x) ( ((u64w*)&(x))->addr )

struct ethernetif {
  struct netif net;
  // int (*send_packet)(struct ethernetif *nif, uint64_t src, uint64_t dst, int proto, void *buf, size_t len);
};

#if NO_SYS

#define lwip_gambit_lock(x) {}
#define lwip_gambit_unlock() {}

#else

static void tcpip_init_done(void *arg)
{
 sys_sem_signal((sys_sem_t *)arg);
}

#include <pthread.h>
//*
static sys_mutex_t gambit_lock;
static pthread_t the_gambit_owner = 0, the_gambit_pthread = 0;
static int current_gambit_count = 0;

static void lwip_gambit_lock(const char *msg)
{
 LG_TRACELOCK((stderr, "Gambit REQ %x - %x %d %s\n", pthread_self(), the_gambit_owner, current_gambit_count, msg));
 if( the_tcp_pthread == 0 && the_gambit_pthread != pthread_self() ) {
  // NONO: not only the gambit thread can do that: local_lwip_init();
  the_tcp_pthread = pthread_self();
  fprintf(stderr, "Guessing TCP thread is %x\n", the_tcp_pthread);
 }

 if(the_gambit_owner != pthread_self() ) {
  sys_mutex_lock(&gambit_lock);
  the_gambit_owner = pthread_self();
 }
 current_gambit_count++;
 LG_TRACELOCK((stderr, "Gambit ENTER %x %d\n", the_gambit_owner, current_gambit_count));
 if(current_gambit_count > 1) {
  fprintf(stderr, "WARNING: Gambit ENTERed AGAIN %x %d\n", the_gambit_owner, current_gambit_count);
 }
}

static void lwip_gambit_unlock()
{
 LG_TRACELOCK((stderr, "Gambit EXIT  %x %d\n", the_gambit_owner, current_gambit_count));
 if(the_gambit_owner == pthread_self() ) {
  if(--current_gambit_count == 0) {
    the_gambit_owner = 0;
    sys_mutex_unlock(&gambit_lock);
  }
 }
}

static int lwip_gambit_init()
{
 if(sys_mutex_new(&gambit_lock) != ERR_OK) return -1;
 the_gambit_pthread = pthread_self();
 lwip_gambit_lock("lwip_gambit_init");
 return ERR_OK;
}

//*/
//#define lwip_gambit_lock ln_gambit_lock
//#define lwip_gambit_unlock ln_gambit_unlock
static int lwip_init_once()
{
  static bool done = false;
  if(!done) {
    sys_sem_t sem;
    if(lwip_gambit_init() != ERR_OK) return 0;
    done=true;
    // fprintf(stderr, "Init NO_SYS: %d LWIP_CALLBACK_API %d\n", NO_SYS, LWIP_CALLBACK_API); // DEBUG
    lwip_init();
    if(sys_sem_new(&sem, 0) != ERR_OK) return 0;
    tcpip_init(tcpip_init_done, &sem);
    sys_sem_wait(&sem);
    sys_sem_free(&sem);
    // disable callbacks until being asked for in
    return 1;
  }
  return 0;
}

#endif

#if NO_SYS
static int lwip_init_once()
{
  static bool done = false;
  if(!done) {
    done=true;
    // fprintf(stderr, "Init NO_SYS: %d LWIP_CALLBACK_API %d\n", NO_SYS, LWIP_CALLBACK_API); // DEBUG
    lwip_init();
    // tcpip_init(NULL, NULL);
    return 1;
  }
  return 0;
}
#endif

static inline u32_t local_sys_timeouts_sleeptime()
{
  u32_t result = 0;
  gambit_lwipcore_lock_bg("sys_timeouts_sleeptime");
  result = sys_timeouts_sleeptime();
  gambit_lwipcore_unlock_bg();
  return result;
}

c-declare-end
)

(define lwip-gambit-lock (c-lambda (char-string) void "lwip_gambit_lock"))
(define lwip-gambit-unlock (c-lambda () void "lwip_gambit_unlock"))

(define lwip-gambit-locked?
  (c-lambda
   () int
   "
#if NO_SYS
___return(1);
#else
___return( the_gambit_owner==0 ? 0 : the_gambit_owner==pthread_self() ? 1 : -1 );
#endif"))

;;; Calling lwIP

(define-macro (c-define-with-gambit-locked.0 def type result-type c-name scope TBD-proto TBD-proto-result TBD-exn body)
  ;; This does not even try to be pretty. Just work for this file; for now.
  (let* ((const (car def))
	 (locked-c-part (string-append "gambit_lwip_" c-name))
         (isvoid (cond
                  ((eq? result-type 'void))
                  (else #f)))
         (result-type-str (if isvoid "void" TBD-proto-result))
         (paramlist
          (call-with-output-string
           (lambda (p)
             (display #\( p)
             (do ((params (cdr def)))
                 ((null? params)
                  (display #\) p))
               (display (car params) p)
               (let ((next (cdr params)))
                 (set! params next)
                 (unless (null? next) (display ", " p)))))))
	 (within-gambit
          (let ((decl (string-append "\nstatic inline " result-type-str " " c-name TBD-proto "\n{\n"))
                (local (if isvoid
                           (string-append " int got_throw = 0;\n")
                           (string-append " int got_throw = 0; " TBD-proto-result " result;\n")))
                (p (string-append " lwip_gambit_lock(\"" c-name "\");\n"))
                (v " lwip_gambit_unlock();\n")
                (body (string-append
                       " ___ON_THROW("
                       (if isvoid "" "result = ")
                       locked-c-part
                       paramlist
                       ", got_throw=1);\n"))
                (final (if isvoid
                           " if (got_throw) {
   /*FIXME: This does not make any sense.*/ return;
 }
 return;\n}\n"
                           (string-append
                            " if (got_throw) {
   return " TBD-exn ";
 }
 return result;\n}\n"))))
            (string-append decl local p body v final))))
    `(begin
       (c-define ,def ,type ,result-type ,locked-c-part ,scope (%lwip-post! ,body))
       (c-declare ,within-gambit))))

;;; Init Part II

#;(define-macro (define-cond-expand-feature-value v)
  (let ((v (eval v)))
    `(define-cond-expand-feature ,v)))

#;(define (lwip-gambit-state)
  (list
   'Caller: ((c-lambda () unsigned-int64 "___result = pthread_self();"))
   'Onwer: ((c-lambda () unsigned-int64 "___result = the_gambit_owner;"))
   'Count: ((c-lambda () unsigned-int64 "___result = current_gambit_count;"))
   ))

(define lwip-sys-timeouts-sleeptime
  (c-lambda () unsigned-int32 "___result = local_sys_timeouts_sleeptime();"))

(define-c-constant lwip-mtu size_t "LWIP_MTU")

(define-c-constant lwip-NO_SYS size_t "NO_SYS")

(cond-expand
 (lwip-requires-pthread-locks
  (when
   (eqv? lwip-NO_SYS 0)
   (debug 'lwip-requires-pthread-locks lwip-NO_SYS)
   (exit 1)))
 (else
  (unless
   (eqv? lwip-NO_SYS 1)
   (debug 'lwip-requires-check-timeouts lwip-NO_SYS)
   (exit 1))))

(define-c-constant LWIP_TCPIP_CORE_LOCKING_INPUT int)

(define-c-constant LWIP_TCPIP_CORE_LOCKING int)

(define-c-constant LWIP_NETIF_HWADDRHINT int)

(define (lwip-tcp-min-sleep) 0.05)

(c-define
 (lwip-init!)
 () void  "local_lwip_init" "static"
 (let ()
   (define (lwip-tcp-loop tmo)
     (thread-sleep! (max (lwip-tcp-min-sleep) (/ tmo 1000.0)))
     (lwip-tcp-loop (lwip-check-timeouts 100)))
   (and ((c-lambda () bool "lwip_init_once"))
        (begin
          (thread-start!
           (make-thread
            (lambda ()
              (with-exception-catcher
               lwip-exception-handler
               (lambda () (lwip-tcp-loop 1))))
            'tcp))
          #t))))

;;; MAC and Host/Network Byte Order

(c-declare #<<c-declare-end

static inline uint64_t mac_from_u8v6(const void *src) // maybe better hwaddr instead of void
{
 const unsigned char *b = (const unsigned char *)src;
 uint64_t result;
 result = ((uint64_t)b[0] << 40)
  | ((uint64_t)b[1] << 32)
  | ((uint64_t)b[2] << 24)
  | ((uint64_t)b[3] << 16)
  | ((uint64_t)b[4] << 8)
  | (uint64_t)b[5];
 return result; // | ;-( highlithing confused with odd number of vertical bars here
}

static inline void g_lwip_set_mac_ui64h(void *buf, uint64_t mac)
{
 unsigned char *b = (unsigned char *)buf;
 b[0] = (unsigned char)((mac >> 40) & 0xff);
 b[1] = (unsigned char)((mac >> 32) & 0xff);
 b[2] = (unsigned char)((mac >> 24) & 0xff);
 b[3] = (unsigned char)((mac >> 16) & 0xff);
 b[4] = (unsigned char)((mac >> 8) & 0xff);
 b[5] = (unsigned char)(mac & 0xff);
}

static inline void lwip_fill_mac_string(char *result, const struct eth_addr*src)
{
 static char cnv[] = "0123456789ABCDEF";
 unsigned int i, j, v;
 for(i=0, j=0; i<ETH_HWADDR_LEN;) {
  v = src->addr[i++];
  result[j++] = cnv[v >> 4];
  result[j++] = cnv[v & 0xf];
  result[j++] = ':';
 }
 result[j-1] = '\0';
}

c-declare-end
)

(define lwip-make-mac/bytes
  ;; six bytes to one mac
  (c-lambda
   (unsigned-int8 unsigned-int8 unsigned-int8 unsigned-int8 unsigned-int8 unsigned-int8) unsigned-int64
   "u64w r; r.n=0; r.addr.addr[0] = ___arg1; r.addr.addr[1] = ___arg2; r.addr.addr[2] = ___arg3; r.addr.addr[3] = ___arg4; r.addr.addr[4] = ___arg5; r.addr.addr[5] = ___arg6; ___result = r.n;"))

(define (lwip-mac:network->host x) ;; return a uint64_t in host byte order
  (cond
   ((fixnum? x)
    ((c-lambda
      (unsigned-int64) unsigned-int64
      "___return(mac_from_u8v6(&___arg1));")
     x))
   (else (error "->zt-mac illegal argument" x))))

(define (lwip-mac:host->network x) ;; return a uint64_t in network byte order
  (cond
   ((and (u8vector? x) (eqv? (u8vector-length x) 6))
    ((c-lambda
      (scheme-object) unsigned-int64
      "u64w r; r.n=0; memcpy(r.addr.addr, ___CAST(void *,___BODY_AS(___arg1,___tSUBTYPED)), ETH_HWADDR_LEN); ___return(r.n);")
     x))
   ((fixnum? x)
    ((c-lambda
      (unsigned-int64) unsigned-int64
      "uint64_t result = 0; g_lwip_set_mac_ui64h(&result, ___arg1); ___return(result);")
     x))
   (else (error "lwip-mac->network illegal argument" x))))

(define lwip-mac-multicast? (c-lambda (unsigned-int64) bool "___return((___arg1 & 0x010000000000ULL) != 0);"))
(define lwip-mac-broadcast? (c-lambda (unsigned-int64) bool "___return(___arg1 == 0xffffffffffffULL );"))
(define lwip-mac-locally-administered? (c-lambda (unsigned-int64) bool "___return((___arg1 & 0x020000000000ULL) != 0);"))

(define lwip-htons (c-lambda (unsigned-int16) unsigned-int16 "lwip_htons"))
(define lwip-htonl (c-lambda (unsigned-int32) unsigned-int32 "lwip_htonl"))
(define lwip-ntohs (c-lambda (unsigned-int16) unsigned-int16 "lwip_ntohs"))
(define lwip-ntohl (c-lambda (unsigned-int32) unsigned-int32 "lwip_ntohl"))

(define lwip-htonll
  (c-lambda
   (unsigned-int64) unsigned-int64
   "___return( (((uint64_t)(lwip_ntohl((uint32_t)((___arg1 << 32) >> 32))) << 32) | (uint32_t)lwip_ntohl(((uint32_t)(___arg1 >> 32)))) );"))
(define lwip-ntohll lwip-htonll)

FIXME: figure out how to properly use char-string
(define lwip-mac-pointer->string
  (c-lambda
   (void*) char-string #<<END
   char result[ETH_HWADDR_LEN*3];
   lwip_fill_mac_string(result,(struct eth_addr*) ___arg1);
   ___return(result);
END
))

(define lwip-mac-integer->string
  (c-lambda
   (unsigned-int64) char-string #<<END
   char result[ETH_HWADDR_LEN*3];
   lwip_fill_mac_string(result,(struct eth_addr*) &___arg1);
   ___return(result);
END
))

(define (lwip-u8-mac->string u8 #!optional (offset 0))
  (and (>= (+ (u8vector-length u8) offset) 6)
       ((c-lambda
         (scheme-object size_t) char-string #<<END
         char result[ETH_HWADDR_LEN*3];
         char *body = ___CAST(char *, ___BODY(___arg1));
         lwip_fill_mac_string(result, ___CAST(struct eth_addr*, body+___arg2));
         ___return(result);
END
)
        u8 offset)))

#|
(define lwip-mac-pointer->string
  (c-lambda
   (void*) scheme-object #<<END
   char buf[ETH_HWADDR_LEN*3];
   ___SCMOBJ err, result;
   lwip_fill_mac_string(buf, (struct eth_addr*) ___arg1);
   err = ___EXT(___CHARSTRING_to_SCMOBJ)(___PSTATE, buf, &result, ___RETURN_POS);
   ___EXT(___release_scmobj)(result);
   if(___FIXNUMP(result)) ___return(___FAL);
   ___return(result);
END
))

(define lwip-mac-integer->string
  (c-lambda
   (unsigned-int64) scheme-object #<<END
   char buf[ETH_HWADDR_LEN*3];
   ___SCMOBJ err, result;
   lwip_fill_mac_string(buf, (struct eth_addr*) &___arg1);
   err = ___EXT(___CHARSTRING_to_SCMOBJ)(___PSTATE, buf, &result, ___RETURN_POS);
   ___EXT(___release_scmobj)(result);
   if(___FIXNUMP(result)) ___return(___FAL);
   ___return(result);
END
))
|#
;;; Timeout Handling

(define %%lwip-check-timeouts
  (c-lambda (int32) int32 #<<END
#if NO_SYS
  /* handle timers (already done in tcpip.c when NO_SYS=0) */
  sys_check_timeouts();
  ___return(sys_timeouts_sleeptime());
#else
  uint32_t result = 10;
  if( the_gambit_owner == the_gambit_pthread && current_gambit_count == 1) {
    // fprintf(stderr, "enabling callbacks into gambit\n");
    lwip_gambit_unlock();
    if(___arg1 < 0) ___arg1 = local_sys_timeouts_sleeptime();
    // fprintf(stderr, "gambit sleep %d microseconds\n", ___arg1);
    usleep(___arg1);
    result = local_sys_timeouts_sleeptime();
    lwip_gambit_lock("lwip-check-timeouts");
  }
  ___return(result);
#endif
END
))

(define (lwip-check-timeouts #!optional (timeout -1))
  (cond-expand
   (lwip-requires-pthread-locks (%%lwip-check-timeouts timeout))
   (else
    (%lwip-lock!)
    (let ((result (%%lwip-check-timeouts timeout)))
      (%lwip-unlock!)
      result))))

;;; Network Interfaces

(c-define-type socket-address (nonnull-pointer (struct "sockaddr_storage") socket-address))

(define (netif? obj) (and (foreign? obj) (let ((f (foreign-tags obj))) (and f (eq? (car f) 'netif)))))

(define-custom lwip-ethernet-send #f) ;; EXPORT HOOK - ethernet output to send

(c-define
 (lwip-ethernet-send! ethif src dst proto #;0 pbuf)
 ((nonnull-pointer (struct "ethernetif")) unsigned-int64 unsigned-int64 unsigned-int16 pbuf)
 int "Xscm_ether_send" "static"
 (let ((handler (lwip-ethernet-send)))
   (cond
    ((procedure? handler) (%lwip-post! (handler netif src dst proto #;0 pbuf)))
    (else ERR_IF))))

#|
(c-define
 (lwip-ethernet-send/raw! ethif src dst proto #;0 bp len)
 ((nonnull-pointer (struct "ethernetif")) unsigned-int64 unsigned-int64 unsigned-int16 void* size_t)
 int "Xscm_ether_send_raw" "static"
 (let ((handler (lwip-ethernet-send)))
   (cond
    ((procedure? handler) (handler netif src dst proto #;0 bp len))
    (else -12))))
|#

(c-declare
;;; FIXME: This is only useful now, if we know that the other side is
;;; ZT and does NOT want the ethernet header.
#<<c-declare-end

// PASS_PBUF_ALONG: keep a pbuf reference, seems needed, maybe not
#define PASS_PBUF_ALONG 0

static err_t scm_ether_send(struct ethernetif *nif, struct pbuf *p)
{
 int got_throw = 0;
 err_t result = ERR_ARG;
 struct pbuf *q = q;
 char *bp;
 struct eth_hdr *ethhdr = p->payload; // caller is responsible!
 //fprintf(stderr, "scm_ether_send\n");
 {
   uint64_t src = 0, dst = 0;  // FIXME: Better use lwip types!!!
   uint64_t nwid = 0;  // FIXME
   // fprintf(stderr, "lwip_netif_linkoutput: prepare ethernet frame\n");
   src = mac_from_u8v6(ethhdr->src.addr);
   dst = mac_from_u8v6(ethhdr->dest.addr);
//*
   memcpy(&src, ethhdr->src.addr, ETH_HWADDR_LEN);
   memcpy(&dst, ethhdr->dest.addr, ETH_HWADDR_LEN);
//*/
 //pbuf_ref(p);
 lwip_gambit_lock("scm_ether_send");
 //fprintf(stderr, "scm_ether_send gambit locked\n");
   // fprintf(stderr, "lwip_netif_linkoutput: hand over gambit me %d\n", pthread_self());
  ___ON_THROW(result = Xscm_ether_send(nif, src, dst, lwip_ntohs(ethhdr->type), p), got_throw=1);
  /*
  { size_t len=p->tot_len;
    ___SCMOBJ u8buf = ___EXT(___alloc_scmobj) (___PSTATE, ___sU8VECTOR, len);
    if(!___FIXNUMP(u8buf)) {
     pbuf_to_u8v_copied(u8buf, p, 0, len);
     ___ON_THROW(result = Xscm_ether_send(nif, src, dst, lwip_ntohs(ethhdr->type), u8buf), got_throw=1);
     ___EXT(___release_scmobj)(u8buf);
    }
   }//*/

 }
 // fprintf(stderr, "scm_ether_send gambit ran\n");
 lwip_gambit_unlock();
#if PASS_PBUF_ALONG
 pbuf_free(p); // ref from ethip6_output (below)
#endif
 if (got_throw) {
   fprintf(stderr, "scm_ether_send: threw an exception, result: %d\n", result);
   return ERR_IF;
 }
 return result;
}
c-declare-end
)

(c-declare #<<END
static ___SCMOBJ release_netif(void *p)
{
 struct netif *nif=p;
 gambit_lwipcore_lock("release_netif");
 netif_remove(nif);
 netif_set_down(nif);
 netif_set_link_down(nif);
 free(nif);
 gambit_lwipcore_unlock();
 return ___FIX(___NO_ERR);
}
END
)

(define lwip-make-netif
  (let ()
    (define lwip-make-netif
  (c-lambda
   (unsigned-int64) netif* #<<END
   struct ethernetif *nif = (struct ethernetif *) malloc(sizeof(struct ethernetif));
   local_lwip_init();
   // struct netif *net = (struct netif *)malloc(sizeof(struct netif));
   // memset(net, sizeof(struct netif), 0);
   memset(nif, sizeof(struct ethernetif), 0);
   memcpy(&nif->net.hwaddr, &___arg1, ETH_HWADDR_LEN); // argument is in network order
   nif->net.hwaddr_len = ETH_HWADDR_LEN;
   nif->net.state = nif; // ??? should we better return the `etherneitif` type?
   // nif->send_packet = scm_ether_send;
   ___return(nif);
END
))
    (lambda (mac)
      (%lwip-lock!)
      (let ((nif (lwip-make-netif mac)))
        (make-will nif (c-lambda #|-with-lwip-locked (nif)|# (void*) scheme-object "release_netif"))
        (%lwip-unlock!)
        nif))))

(define lwip-netif-find (c-lambda (char-string) netif* "netif_find"))

(define lwip-netif-mac
  (c-lambda
   (netif*) unsigned-int64
   "___result = mac_from_u8v6(___arg1->hwaddr);"))

(define lwip-netif-mac->string
  (c-lambda
   (netif*) char-string #<<END
   char result[18];
   lwip_fill_mac_string(result,___CAST(struct eth_addr* ,___arg1->hwaddr));
   ___result = result;
END
))

(c-declare #<<END
static inline int lwip_netif_ip6addr_count(struct netif* netif)
{
 int i, result = 0;
 for (i = 0; i < LWIP_IPV6_NUM_ADDRESSES; i++) {
  if(!ip6_addr_isinvalid(netif_ip6_addr_state(netif, i))) result++;
 }
 return result;
}

static inline uint64_t lwip_netif_ip6bc_mach(struct netif *netif, unsigned int idx)
{
 uint64_t result;
 if(idx >= LWIP_IPV6_NUM_ADDRESSES) return 0;
 u32_t *na = ___CAST(u32_t*, netif_ip6_addr(netif, idx));
 u8_t *a = (u8_t*)na;
 result = (0x33ll<<40|0x33ll<<32|0xffll<<24|(uint64_t)(a[13])<<16|(uint64_t)(a[14])<<8|a[15]); //|
 return result;
}

END
)

(define lwip-netif-ip6addr-count (c-lambda (netif*) unsigned-int "lwip_netif_ip6addr_count"))

(define lwip-netif-ip6broadcast-mach
  (c-lambda (netif* unsigned-int) unsigned-int64 "lwip_netif_ip6bc_mach"))

;;; Network Interface IO

(define-custom lwip-ip6-send #f) ;; EXPORT HOOK - ethernet output to send

(c-define
 (lwip-ip6-send! netif pbuf addr)
 (netif* pbuf void* #;(nonnull-pointer (struct "ip6_addr_t")))
 int "scm_ip6_send" "static"
 (let ((handler (lwip-ip6-send)))
   (cond
    ((procedure? handler) (%lwip-post! (handler netif pbuf addr)))
    (else ERR_RTE))))

(c-declare #<<c-declare-end

static err_t
lwip_netif_linkoutput(struct netif *netif, struct pbuf *p)
{
  struct ethernetif *ether = netif->state;
  err_t result;
 // fprintf(stderr, "linkoutput pre\n");  // DEBUG
  if(netif == NULL) return -1;
  if(p == NULL) return -1;
  // fprintf(stderr, "linkoutput\n");  // DEBUG
  result = scm_ether_send(ether, p);

  /* // From ethernetif.c example of lwip
  MIB2_STATS_NETIF_ADD(netif, ifoutoctets, p->tot_len);
  if (((u8_t *)p->payload)[0] & 1) {
    MIB2_STATS_NETIF_INC(netif, ifoutnucastpkts); // broadcast or multicast packet
  } else {
    MIB2_STATS_NETIF_INC(netif, ifoutucastpkts); // unicast packet
  }
  //*/

  return ERR_OK;
  // return result;
}

static err_t
lwip_send_ethernet_input(struct netif *netif, struct pbuf* p)
{
  int result;
  // fprintf(stderr, "lwip_send_ethernet_input: interface %p\n", netif);
#ifdef LWIP_STATS
  stats_display();
#endif
  //**/fprintf(stderr, "lwip_send_ethernet_input: feed packet into netif->input %p\n", netif->input);
  if(netif->input == NULL) {
    fprintf(stderr, "lwip_send_ethernet_input: INTERNAL ERROR feeding packet into netif->input \n");
    pbuf_free(p);
    return ERR_IF;
  }
  if ((result = netif->input(p, netif)) != ERR_OK) {
    fprintf(stderr, "lwip_send_ethernet_input: input failed with %d on %p\n", result, p);
  } else {
    //**/ fprintf(stderr, "lwip_send_ethernet_input: packet has been read into netif\n", p);
    /*
    MIB2_STATS_NETIF_ADD(netif, ifinoctets, p->tot_len);
    if (((u8_t *)p->payload)[0] & 1) {
      // broadcast or multicast packet
      MIB2_STATS_NETIF_INC(netif, ifinnucastpkts);
    } else {
      // unicast packet
      MIB2_STATS_NETIF_INC(netif, ifinucastpkts);
    }
    LINK_STATS_INC(link.recv);
    //*/
  }
  return result;
}

static err_t
dbg_ethip6_output(struct netif *netif, struct pbuf *q, const ip6_addr_t *ip6addr)
{
 err_t r;
 lwip_gambit_lock("scm_ip6_send");
 r = scm_ip6_send(netif, q, (ip6_addr_t *) ip6addr); // no warn const discarded
 lwip_gambit_unlock();
 if(r != ERR_OK) fprintf(stderr, "scm_ip6_output failed for %p %d\n", netif, r);
 if(r == ERR_RTE) {
#if PASS_PBUF_ALONG
   pbuf_ref(q); // seems needed - freed in scm_ether_send
#endif
   r = ethip6_output(netif, q, ip6addr);
   if(r != ERR_OK) fprintf(stderr, "ethip6_output failed for %p %d\n", netif, r);
 }
 return r;
}

static err_t netif_init6(struct netif *nif)
{
  // Called from netif code
  nif->hwaddr_len = 6;
  nif->name[0]    = IFNAME0;
  nif->name[1]    = IFNAME1;
  nif->linkoutput = lwip_netif_linkoutput;
  nif->output     = NULL /*etharp_output*/;
  nif->output_ip6 = dbg_ethip6_output;
  nif->mtu        = LOCAL_MTU;
  nif->flags      = NETIF_FLAG_BROADCAST
    | NETIF_FLAG_ETHARP  // ??
    | NETIF_FLAG_ETHERNET
    | NETIF_FLAG_IGMP  // ??
    | NETIF_FLAG_MLD6
    | NETIF_FLAG_LINK_UP  // ??
    | NETIF_FLAG_UP;  // ??
  return ERR_OK;
}

static int lwip_calling_back(void*(*s)(void *), int(*f)(void *, void *), void *e)
{
 void *in; int result;
 gambit_lwipcore_unlock();
 in = s ? (s)(e) : NULL;
 lwip_gambit_unlock();
 result = (f)(in, e);
 lwip_gambit_lock("lwip_calling_back");
 gambit_lwipcore_lock("lwip_calling_back");
 return result;
}

static inline void cp_sockaddr_to_ip6_addr(ip6_addr_t *ip6addr, struct sockaddr_in6 *sa_in6)
{
  memcpy(&ip6addr->addr, &sa_in6->sin6_addr, sizeof(ip6addr->addr));
}

static void
lwip_init_interface_IPv6(struct netif *nif, uint8_t* sin6addr)
{
  ip6_addr_t ip6addr;
  local_lwip_init(); // be sure that's done
  // fprintf(stderr, "lwip_init_interface_IPv6\n");
  memcpy(ip6addr.addr, sin6addr, sizeof(ip6addr.addr));
  ip6_addr_set_zone(&ip6addr, IP6_NO_ZONE);
  // nif->ip6_autoconfig_enabled = 1; // too early

  // now in macro: gambit_lwipcore_lock("lwip_init_interface_IPv6");
  // questionable: passing the state as the old value.  Better don't initialize it?
  if(!netif_add_noaddr(nif, nif /*->state*/, netif_init6, INPUT_HANDLER)) {
    fprintf(stderr, "lwip: netif_add_noaddr failed\n");
  }
  // fprintf(stderr, "lwip: netif_create_ip6_linklocal_address\n");
  netif_create_ip6_linklocal_address(nif, 1);
  // using default now: netif_ip6_addr_set_state(nif, 0, IP6_ADDR_VALID); // was IP6_ADDR_TENTATIVE
  nif->ip6_autoconfig_enabled = 0; // ??
  netif_set_default(nif);  // ??
  // fprintf(stderr, "lwip: netif_set_up\n");
  netif_set_up(nif);  // ??
  // fprintf(stderr, "init addr %s\n", ip6addr_ntoa(&ip6addr));  // DEBUG
  netif_ip6_addr_set(nif, 1, &ip6addr);
  netif_ip6_addr_set_state(nif, 1, nif->ip6_autoconfig_enabled ? IP6_ADDR_TENTATIVE : IP6_ADDR_VALID /*IP6_ADDR_PREFERRED*/); // was IP6_ADDR_TENTATIVE
  // now in macro: gambit_lwipcore_unlock();
  // fprintf(stderr, "lwip_init_interface_IPv6 DONE\n");
}

c-declare-end
)

(define lwip_init_interface_IPv6
  (c-lambda-with-lwip-locked
   (nif addr) (netif* scheme-object) void "lwip_init_interface_IPv6(___arg1, ___BODY(___arg2));"))

(define pbuf-fill-ethernet-header!
  (c-lambda
   (pbuf unsigned-int64 unsigned-int64 unsigned-int) void #<<END
   struct eth_hdr *ethhdr = (struct eth_hdr*) ___arg1->payload;
   u64w from, to;
   from.n= ___arg2;
   to.n = ___arg3;
   memcpy(&ethhdr->src.addr, &MACDEREF(from), ETH_HWADDR_LEN);
   memcpy(&ethhdr->dest.addr, &MACDEREF(to), ETH_HWADDR_LEN);
   ethhdr->type = htons(___arg4);
END
))

(define lwip-send-ethernet-input!
  (c-lambda-with-lwip-locked (nif pbuf) (netif* pbuf) err_t "___result=lwip_send_ethernet_input(___arg1, ___arg2);"))

;;(define lwip-default-netif-poll! (c-lambda () void "default_netif_poll"))

;;; ND6, Routing

(define lwip-nd6-find-route
  (c-lambda-with-lwip-locked
   ;; TODO: check for IPv6 address.
   (addr) (socket-address) netif*
   "ip6_addr_t a; cp_sockaddr_to_ip6_addr(&a,(struct sockaddr_in6*) ___arg1); ___result = nd6_find_route(&a);"))

;;;* TCP API

;;(c-define-type CONTEXT (pointer "void")) ;; we may want to redefine that.
(c-define-type CONTEXT scheme-object) ;; we may want to redefine that.
(c-declare "\n#define GAME_CONTEXT ___SCMOBJ\n")

(define-c-constant lwip-IPADDR_TYPE_V4 int "IPADDR_TYPE_V4")
(define-c-constant lwip-IPADDR_TYPE_V6 int "IPADDR_TYPE_V6")
(define-c-constant lwip-IPADDR_TYPE_ANY int "IPADDR_TYPE_ANY")

(c-define-type tcp_pcb (nonnull-pointer (struct "tcp_pcb") tcp-pcb)) ;; MUST NOT have a release function.
(c-define-type new_tcp_pcb (pointer (struct "tcp_pcb") tcp-pcb))

(define (tcp-pcb? obj) (let ((f (foreign-tags obj))) (and f (eq? (car f) 'tcp-pcb))))

(define-macro (custom-handler name default . args)
  ;; FIXME: We better check from c-define-with-gambit-locked.0 for the
  ;; handler BEFORE we lock the core and provide defaults in plain C!
  (let ((handler (gensym 'handler)))
    `(let ((,handler (,name)))
       (if ,handler
           (,handler . ,args)
           ,default))))

;; err_t tcp_bind (struct tcp_pcb *pcb, const ip_addr_t *ipaddr, u16_t port);

#;(define lwip-tcp-bind/socket-address
  (c-lambda-with-lwip-locked
   ;; FIXME: works only with IPv6 for now., does not at all, misses locking
   (tcp_pcb socket-address) err_t #<<END
   ip_addr_t ipaddr = IPADDR6_INIT(0,0,0,0);
   cp_sockaddr_to_ip6_addr(&ipaddr.u_addr.ip6, (struct sockaddr_in6 *) ___arg2);
   ___result = tcp_bind(___arg1, &ipaddr, ((struct sockaddr_in6 *) ___arg2)->sin6_port);
END
))

(define (lwip-tcp-bind pcb u8 port)
 ;; FIXME: works only with IPv6 for now.
 (unless (eqv? (u8vector-length u8) 16) (error "lwip-tcp-bind: currently only IPv6"))
 ((c-lambda-with-lwip-locked
   (pcb addr port)
   (tcp_pcb scheme-object unsigned-int) err_t #<<END
   ip_addr_t ipaddr;
   ipaddr.type = IPADDR_TYPE_V6;
   memcpy(&ipaddr.u_addr.ip6.addr, ___CAST(void *,___BODY_AS(___arg2,___tSUBTYPED)), sizeof(ip6_addr_t));
   ipaddr.u_addr.ip6.zone = IP6_NO_ZONE;
   lwip_gambit_unlock();
   gambit_lwipcore_lock("lwip-tcp-bind");
   ___result = tcp_bind(___arg1, &ipaddr, ___arg3);
   gambit_lwipcore_unlock();
   lwip_gambit_lock("lwip-tcp-bind");
END
) pcb u8 port))

(define (lwip-tcp-bind/netif pcb netif)
  ((c-lambda-with-lwip-locked
   (pcb netif)
   (tcp_pcb netif*) void #<<END
   gambit_lwipcore_lock("lwip-tcp-bind/netif");
   tcp_bind_netif(___arg1, ___arg2);
   gambit_lwipcore_unlock();
END
) pcb netif))

(define tcp-new (c-lambda () new_tcp_pcb "local_lwip_init(); ___return(tcp_new());"))

(define tcp-new-ip-type
  (c-lambda-with-lwip-locked
   (type) (unsigned-int8) new_tcp_pcb
   "local_lwip_init();
 struct tcp_pcb *result=tcp_new_ip_type(___arg1);
 ___return(result);"))

(define (tcp-new6) (tcp-new-ip-type lwip-IPADDR_TYPE_V6))

#;(define tcp-context-set!
  (c-lambda-with-lwip-locked
   (pcb ctx) (tcp_pcb CONTEXT) void "tcp_arg(___arg1, (void*)___arg2);"))

(define tcp-context-set!
  (c-lambda
   (tcp_pcb CONTEXT) void "tcp_arg(___arg1, (void*)___arg2);"))

(define tcp-context
  (c-lambda
   (tcp_pcb) CONTEXT "___return(___CAST(GAME_CONTEXT ,___arg1->callback_arg));"))

#|
(define-type tcp-connection macros: prefix: % ctx pcb)

(define (make-tcpip-connection type context)
  (let ((pcb
         (tcp-new-ip-type
          (case type
            ((IPv6) lwip-IPADDR_TYPE_V6)
            ((IPv4) lwip-IPADDR_TYPE_V4)
            ((*) lwip-IPADDR_TYPE_ANY)
            (else type)))))
    (tcp-context-set! pcb context)
    (%make-tcp-connection pcb context)))
|#

;;;** TCP Event API

(define-c-constant LWIP_EVENT_ACCEPT unsigned-int8)
(define-c-constant LWIP_EVENT_SENT unsigned-int8)
(define-c-constant LWIP_EVENT_RECV unsigned-int8)
(define-c-constant LWIP_EVENT_CONNECTED unsigned-int8)
(define-c-constant LWIP_EVENT_POLL unsigned-int8)
(define-c-constant LWIP_EVENT_ERR unsigned-int8)

(define-custom on-tcp-event #f) ;; TBD

(c-define-with-gambit-locked.0
 (%%on-tcp-event ctx pcb event pbuf size err)
 (CONTEXT new_tcp_pcb unsigned-int8 pbuf unsigned-int16 err_t)
 err_t "scm_lwip_tcp_event" "static inline"
 "(GAME_CONTEXT ctx, struct tcp_pcb* pcb, enum lwip_event event, struct pbuf* pbuf, u16_t size, err_t err)"
 "err_t" "ERR_IF"
 (custom-handler on-tcp-event ERR_ABRT ctx pcb event pbuf size err))

(c-declare #<<end-lwip-tcp-event
err_t lwip_tcp_event(void *arg, struct tcp_pcb *pcb, enum lwip_event event, struct pbuf *p, u16_t size, err_t err)
{
#if NO_SYS
// if(event == LWIP_EVENT_POLL) return ERR_OK;
#endif
 return scm_lwip_tcp_event((GAME_CONTEXT) arg, pcb, event, p, size, err);
}
end-lwip-tcp-event
)

(define lwip-tcp-connect/event
  ;; EVENT API version, NULL connect function (unused)
  (c-lambda-with-lwip-locked
   ;; FIXME: works only with IPv6 for now.
   (pcb u8addr port)
   (tcp_pcb scheme-object unsigned-int) err_t #<<END
   ip_addr_t ipaddr;
   ipaddr.type = IPADDR_TYPE_V6;
   ipaddr.u_addr.ip6.zone = IP6_NO_ZONE;
   memcpy(&ipaddr.u_addr.ip6.addr, ___CAST(void *,___BODY_AS(___arg2,___tSUBTYPED)), 16);
   gambit_lwipcore_lock("lwip-tcp-connect");
   ___result = tcp_connect(___arg1, &ipaddr, ___arg3, NULL);
   gambit_lwipcore_unlock();
END
))

(define lwip-tcp-connect lwip-tcp-connect/event)

(define (tcp-set-poll-interval! pcb interval)
  ((c-lambda-with-lwip-locked
    (pcb interval) (tcp_pcb unsigned-int8) void "tcp_poll(___arg1, NULL, ___arg2);")
   pcb interval))

(define tcp-flags-set! (c-lambda (tcp_pcb unsigned-int) void "tcp_set_flags"))

(define tcp-flags-clear! (c-lambda (tcp_pcb unsigned-int) void "tcp_clear_flags"))

(define tcp-flags-set? (c-lambda (tcp_pcb unsigned-int) bool "tcp_is_flag_set"))

;; TBD Some are missing here

;; To be called by the application to aknowledge that it has read that
;; much incoming data.
(define tcp-received!
  (c-lambda-with-lwip-locked (pcb amount) (tcp_pcb unsigned-int16) void "tcp_recved(___arg1, ___arg2);"))

(define %%while-in-callback-tcp-received!
  (c-lambda (tcp_pcb unsigned-int16) void "tcp_recved(___arg1, ___arg2);"))

(define lwip-tcp-listen
  ;; NOTE: this deallocates the argument, except when it returns #f.
  ;;
  ;; BEWARE: MUST NOT use the ___return macro!
  (c-lambda-with-lwip-locked (listen-pcb) (tcp_pcb) new_tcp_pcb "___result = tcp_listen(___arg1);"))

(define tcp_abort (c-lambda-with-lwip-locked (abort-pcb) (tcp_pcb) void "tcp_abort(___arg1);"))

(define (lwip-tcp-close pcb)
  (let ((r ((c-lambda-with-lwip-locked (close-pcb) (tcp_pcb) err_t "___result=tcp_close(___arg1);") pcb)))
    (if (eqv? r ERR_OK) r
        (begin
          (debug 'lwip-tcp-close r)
          (tcp_abort pcb)))))

(define lwip-tcp_shutdown
  (c-lambda-with-lwip-locked
   (pcb shtin shtou) (tcp_pcb bool bool) err_t "___result=tcp_shutdown(___arg1, ___arg2, ___arg3);"))

(define lwip-tcp_write
  (c-lambda-with-lwip-locked
   (pcb data sz x) (tcp_pcb void* unsigned-int16 unsigned-int8) err_t
   "___result = tcp_write(___arg1, ___arg2, ___arg3, ___arg4);"))

(define lwip-tcp-write-subu8vector*
  (c-lambda-with-lwip-locked
   (data off sz pcb) (scheme-object size_t size_t tcp_pcb) err_t
   "___result = tcp_write(___arg4, ___CAST(uint8_t*, ___BODY(___arg1)) + ___arg2, ___arg3 - ___arg2, 0);"))

(define lwip-tcp-write-subu8vector/copy*
  (c-lambda-with-lwip-locked
   (data off sz pcb) (scheme-object size_t size_t tcp_pcb) err_t
   "___result = tcp_write(___arg4, ___CAST(uint8_t*, ___BODY(___arg1)) + ___arg2, ___arg3 - ___arg2, TCP_WRITE_FLAG_COPY);"))

(define (lwip-tcp-write-subu8vector vec start end pcb)
  (when (>= (+ start end) (u8vector-length vec))
        (##raise-range-exception 1 'lwip-tcp-write-subu8vector (u8vector-length vec) start end))
  (lwip-tcp-write-subu8vector* vec start end pcb))

(define lwip-tcp-prio-set!
  (c-lambda-with-lwip-locked (pcb prio) (tcp_pcb unsigned-int8) void "tcp_setprio(___arg1, ___arg2);"))

(define lwip-tcp-force-output (c-lambda-with-lwip-locked (flush-pcb) (tcp_pcb) err_t "___result = tcp_output(___arg1);"))
(define lwip-tcp-force-output2
  ;; FIXME: so far it did not call back.  Could it?
  (c-lambda-with-lwip-locked
   (flush-pcb) (tcp_pcb) err_t #<<END
   lwip_gambit_unlock();
   gambit_lwipcore_lock("lwip-tcp-flush!");
   ___result = tcp_output(___arg1);
   gambit_lwipcore_unlock();
   lwip_gambit_lock("lwip-tcp-flush!");
END
))

;; (define lwip- (c-lambda ()  ""))

;; (define lwip-tcp_tcp_get_tcp_addrinfo (c-lambda ()  ""))

(include "lwip-ports.scm")
