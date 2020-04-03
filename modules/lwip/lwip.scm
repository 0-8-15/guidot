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

(define lwip-host-is-network-endian
  ;; big endian needs no conversion => #t
  ;;
  ;; this is just informal; intented for debugging
  (c-lambda () bool "___result = htons(1)==1;"))

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

(define-c-constant ERR_OK unsigned-int8)
(define-c-constant ERR_MEM unsigned-int8)
(define-c-constant ERR_BUF unsigned-int8)
(define-c-constant ERR_TIMEOUT unsigned-int8)
(define-c-constant ERR_RTE unsigned-int8)
(define-c-constant ERR_INPROGRESS unsigned-int8)
(define-c-constant ERR_VAL unsigned-int8)
(define-c-constant ERR_WOULDBLOCK unsigned-int8)
(define-c-constant ERR_USE unsigned-int8)
(define-c-constant ERR_ALREADY unsigned-int8)
(define-c-constant ERR_ISCONN unsigned-int8)
(define-c-constant ERR_CONN unsigned-int8)
(define-c-constant ERR_IF unsigned-int8)
(define-c-constant ERR_ABRT unsigned-int8)
(define-c-constant ERR_RST unsigned-int8)
(define-c-constant ERR_CLSD unsigned-int8)
(define-c-constant ERR_ARG unsigned-int8)

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

(define-c-constant ETHTYPE_IP unsigned-int)
(define-c-constant ETHTYPE_ARP unsigned-int)
(define-c-constant ETHTYPE_WOL unsigned-int)
(define-c-constant ETHTYPE_RARP unsigned-int)
(define-c-constant ETHTYPE_VLAN unsigned-int)
(define-c-constant ETHTYPE_IPV6 unsigned-int)
(define-c-constant ETHTYPE_PPPOEDISC unsigned-int)
(define-c-constant ETHTYPE_PPPOE unsigned-int)
(define-c-constant ETHTYPE_JUMBO unsigned-int)
(define-c-constant ETHTYPE_PROFINET unsigned-int)
(define-c-constant ETHTYPE_ETHERCAT unsigned-int) ;; Ethernet for control automation technology
(define-c-constant ETHTYPE_LLDP unsigned-int)
(define-c-constant ETHTYPE_SERCOS unsigned-int) ;; Serial real-time communication system
(define-c-constant ETHTYPE_MRP unsigned-int) ;; Media redundancy protocol
(define-c-constant ETHTYPE_PTP unsigned-int) ;; Precision time protocol
(define-c-constant ETHTYPE_QINQ unsigned-int) ;; Q-in-Q, 802.1ad

(define (ethertype/host->symbol x)
  (cond
   ((eqv? x ETHTYPE_IP) 'ETHTYPE_IP)
   ((eqv? x ETHTYPE_ARP) 'ETHTYPE_ARP)
   ((eqv? x ETHTYPE_WOL) 'ETHTYPE_WOL)
   ((eqv? x ETHTYPE_RARP) 'ETHTYPE_RARP)
   ((eqv? x ETHTYPE_VLAN) 'ETHTYPE_VLAN)
   ((eqv? x ETHTYPE_IPV6) 'ETHTYPE_IPV6)
   ((eqv? x ETHTYPE_PPPOEDISC) 'ETHTYPE_PPPOEDISC)
   ((eqv? x ETHTYPE_PPPOE) 'ETHTYPE_PPPOE)
   ((eqv? x ETHTYPE_JUMBO) 'ETHTYPE_JUMBO)
   ((eqv? x ETHTYPE_PROFINET) 'ETHTYPE_PROFINET)
   ((eqv? x ETHTYPE_ETHERCAT) 'ETHTYPE_ETHERCAT)
   ((eqv? x ETHTYPE_LLDP) 'ETHTYPE_LLDP)
   ((eqv? x ETHTYPE_SERCOS) 'ETHTYPE_SERCOS)
   ((eqv? x ETHTYPE_MRP) 'ETHTYPE_MRP)
   ((eqv? x ETHTYPE_PTP) 'ETHTYPE_PTP)
   ((eqv? x ETHTYPE_QINQ) 'ETHTYPE_QINQ)
   (else 'lwip-UNKNOWN)))

(define (ethertype/network->symbol x) (ethertype/host->symbol (lwip-htons x)))

(c-define-type err_t int)

(c-declare
;;; This section is the only one, which MAY use {UN}LOCK_TCPIP_CORE
#<<lwip-core-lock-end
//*
#include <pthread.h>

static /*inline*/ void gambit_lwipcore_lock()
{
 fprintf(stderr, "gambit_lwipcore O %x\n", pthread_self());
 LOCK_TCPIP_CORE();
 fprintf(stderr, "gambit_lwipcore P %x\n", pthread_self());
}
static inline void gambit_lwipcore_unlock()
{
 fprintf(stderr, "gambit_lwipcore V %x\n", pthread_self());
 UNLOCK_TCPIP_CORE();
}

#define gambit_lwipcore_lock_bg() LOCK_TCPIP_CORE()
#define gambit_lwipcore_unlock_bg() UNLOCK_TCPIP_CORE()
//*/
lwip-core-lock-end
)

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

#if ! NO_SYS
static void tcpip_init_done(void *arg)
{
 sys_sem_signal((sys_sem_t *)arg);
}
#endif

#include <pthread.h>

//*
static sys_mutex_t gambit_lock;
static pthread_t the_gambit_owner = 0;
static int current_gambit_count = 0;

static void lwip_gambit_lock()
{
 // fprintf(stderr, "Gambit REQ %x - %x %d\n", pthread_self(), the_gambit_owner, current_gambit_count);
 if(the_gambit_owner != pthread_self() ) {
  sys_mutex_lock(&gambit_lock);
  the_gambit_owner = pthread_self();
 }
 current_gambit_count++;
 // fprintf(stderr, "Gambit ENTER %x %d\n", the_gambit_owner, current_gambit_count);
 if(current_gambit_count > 1) {
  fprintf(stderr, "WARNING: Gambit ENTERed AGAIN %x %d\n", the_gambit_owner, current_gambit_count);
 }
}

static void lwip_gambit_unlock()
{
 // fprintf(stderr, "Gambit EXIT  %d %d\n", the_gambit_owner, current_gambit_count);
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
 lwip_gambit_lock();
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
    fprintf(stderr, "Init NO_SYS: %d LWIP_CALLBACK_API %d\n", NO_SYS, LWIP_CALLBACK_API); // DEBUG
    // lwip_init();
    if(sys_sem_new(&sem, 0) != ERR_OK) return 0;
    tcpip_init(tcpip_init_done, &sem);
    sys_sem_wait(&sem);
    tcp_init();
    // disable callbacks until being asked for in
    return 1;
  }
  return 0;
}

static inline u32_t local_sys_timeouts_sleeptime()
{
  u32_t result = 0;
  gambit_lwipcore_lock_bg();
  result = sys_timeouts_sleeptime();
  gambit_lwipcore_unlock_bg();
  return result;
}

c-declare-end
)

;;; Calling lwIP

(define-macro (c-define-with-gambit-locked.0 def type result-type c-name scope TBD-proto TBD-proto-result TBD-exn body)
  ;; This does not even try to be pretty. Just work for this file; for now.
  (let* ((const (car def))
	 (locked-c-part (string-append "gambit_lwip_" c-name))
         (isvoid (cond
                  ((eq? result-type 'void))
                  (else #f)))
         (result-type (if isvoid "void" TBD-proto-result))
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
          (let ((decl (string-append "\nstatic inline " result-type " " c-name TBD-proto "\n{\n"))
                (local (if isvoid
                           (string-append " int got_throw = 0;\n")
                           (string-append " int got_throw = 0; " TBD-proto-result " result;\n")))
                (p " lwip_gambit_lock();\n")
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
       (c-define ,def ,type ,result-type ,locked-c-part ,scope ,body)
       (c-declare ,within-gambit))))

(define (lwip-gambit-state)
  (list
   'Caller: ((c-lambda () unsigned-int64 "___result = pthread_self();"))
   'Onwer: ((c-lambda () unsigned-int64 "___result = the_gambit_owner;"))
   'Count: ((c-lambda () unsigned-int64 "___result = current_gambit_count;"))
   ))

(define lwip-sys-timeouts-sleeptime
  (c-lambda () unsigned-int32 "___result = local_sys_timeouts_sleeptime();"))

(define-c-constant lwip-mtu size_t "LWIP_MTU")

(define-c-constant lwip-NO_SYS size_t "NO_SYS")

(define-c-constant LWIP_TCPIP_CORE_LOCKING_INPUT int)

(define-c-constant LWIP_NETIF_HWADDRHINT int)

(define-c-constant SIZEOF_ETH_HDR size_t "SIZEOF_ETH_HDR")

(define (lwip-tcp-min-sleep) 10)

(c-define
 (lwip-init!)
 () void  "local_lwip_init" "static"
 (let ()
   (define (lwip-tcp-loop tmo)
     (thread-sleep! (min (lwip-tcp-min-sleep) (/ tmo 1000.0)))
     (lwip-tcp-loop (lwip-check-timeouts 100)))
   (and ((c-lambda () bool "lwip_init_once"))
        (begin
          (thread-start! (make-thread (lambda () (lwip-tcp-loop 1)) 'tcp))
          #t))))

(c-define-type void* (pointer "void"))

(c-define-type nonnull-void* (nonnull-pointer "void"))

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

static inline void lwip_fill_mac_string(char *result, struct eth_addr*src)
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
      "___result = mac_from_u8v6(&___arg1);")
     x))
   (else (error "->zt-mac illegal argument" x))))

(define (lwip-mac:host->network x) ;; return a uint64_t in network byte order
  (cond
   ((and (u8vector? x) (eqv? (u8vector-length x) 6))
    ((c-lambda
      (scheme-object) unsigned-int64
      "u64w r; r.n=0; memcpy(r.addr.addr, ___CAST(void *,___BODY_AS(___arg1,___tSUBTYPED)), ETH_HWADDR_LEN); ___result = r.n;")
     x))
   ((fixnum? x)
    ((c-lambda
      (unsigned-int64) unsigned-int64
      "___result = 0; g_lwip_set_mac_ui64h(&___result, ___arg1);")
     x))
   (else (error "lwip-mac->network illegal argument" x))))

(define lwip-htons (c-lambda (unsigned-int16) unsigned-int16 "lwip_htons"))
(define lwip-htonl (c-lambda (unsigned-int32) unsigned-int32 "lwip_htonl"))
(define lwip-ntohs (c-lambda (unsigned-int16) unsigned-int16 "lwip_ntohs"))
(define lwip-ntohl (c-lambda (unsigned-int32) unsigned-int32 "lwip_ntohl"))

(define lwip-htonll
  (c-lambda
   (unsigned-int64) unsigned-int64
   "___result = (((uint64_t)(lwip_ntohl((uint32_t)((___arg1 << 32) >> 32))) << 32) | (uint32_t)lwip_ntohl(((uint32_t)(___arg1 >> 32))));"))
(define lwip-ntohll lwip-htonll)

(define lwip-mac-pointer->string
  (c-lambda
   (void*) char-string #<<END
   char result[ETH_HWADDR_LEN*3];
   lwip_fill_mac_string(result,(struct eth_addr*) ___arg1);
   ___result = result;
END
))

(define lwip-mac-integer->string
  (c-lambda
   (unsigned-int64) char-string #<<END
   char result[ETH_HWADDR_LEN*3];
   lwip_fill_mac_string(result,(struct eth_addr*) &___arg1);
   ___result = result;
END
))

(c-define-type socket-address (pointer (struct "sockaddr_storage") socket-address))

#|

(c-define-type eth-addr* (pointer (struct "eth_addr")))

(define (lwip-make-eth-addr)
  ((c-lambda (scheme-object) eth-addr* "___result = ___arg1;") (make-u8vector 6 0)))

(define lwip-eth-addr-equal? (c-lambda (eth-addr* eth-addr*) bool "___result = eth_addr_cmp(___arg1, ___arg2);"))

(define eth-addr->uint64
  (c-lambda
   (eth-addr*) unsigned-int64
   "___result = 0; memcpy(&((struct eth_addr*)&___result)->addr, &___arg1->addr, ETH_HWADDR_LEN);"))

(define (lwip-eth-addr-set! addr u8)
  (cond
   ((integer? u8)
    ((c-lambda
      (eth-addr* unsigned-int64) void
      "memcpy(&___arg1->addr, &___arg2, ETH_HWADDR_LEN);")
     addr u8))
   ((u8vector? u8)
    ((c-lambda
      (eth-addr* scheme-object) void
      "memcpy(&___arg1->addr, ___CAST(void *,___BODY_AS(___arg2,___tSUBTYPED)), ETH_HWADDR_LEN);")
     addr u8))
   (else (error "lwip-eth-addr-set! illegal argument" u8))))

(c-define-type eth-hdr (struct "eth_hdr"))
|#

(c-define-type netif* (pointer (struct "netif")))

(define (netif? obj) (and (foreign? obj) (let ((f (foreign-tags obj))) (and f (eq? (car f) '|struct netif*|)))))

(define-custom lwip-ethernet-send #f) ;; EXPORT HOOK - ethernet output to send

(define cebug
  (let ((t (make-thread
            (lambda ()
              (do ()
                  (#f)
                (call-with-values thread-receive debug)))
            'cebug)))
    (thread-start! t)
    (lambda (l v)
      (declare (not interrupts-enabled))
      (let ((x (values l v)))
        (thread-send t x)
        x)
      v)))

(c-define
 (lwip-ethernet-send! ethif src dst proto #;0 bp len)
 ((pointer (struct "ethernetif")) unsigned-int64 unsigned-int64 unsigned-int16 void* size_t)
 int "Xscm_ether_send" "static"
 (let ((handler (lwip-ethernet-send)))
   (cond
    ((procedure? handler) (handler netif src dst proto #;0 bp len))
    (else -12))))

(c-declare #<<c-declare-end
static int scm_ether_send(struct ethernetif *nif, uint64_t src, uint64_t dst, uint16_t proto, void *buf, size_t len)
{
 int got_throw = 0;
 int result;
 //fprintf(stderr, "scm_ether_send\n");
 lwip_gambit_lock();
 //fprintf(stderr, "scm_ether_send gambit locked\n");
  ___ON_THROW(result = Xscm_ether_send(nif, src, dst, proto, buf, len), got_throw=1);
 // fprintf(stderr, "scm_ether_send gambit ran\n");
 lwip_gambit_unlock();
 if (got_throw) {
   fprintf(stderr, "scm_ether_send: threw an exception\n");
   return ERR_IF;
 }
 return result;
}
c-declare-end
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
   ___result_voidstar = nif;
END
))
lwip-make-netif))

(define lwip-netif-mac
  (c-lambda
   (netif*) unsigned-int64
   "___result = mac_from_u8v6(___arg1->hwaddr);"))

(define lwip-netif-mac->string
  (c-lambda
   (netif*) char-string #<<END
   char result[18];
   lwip_fill_mac_string(result,___arg1->hwaddr);
   ___result = result;
END
))

(define (lwip-check-timeouts #!optional (timeout -1))
 ((c-lambda (int32) int32 #<<END
#if NO_SYS
  /* handle timers (already done in tcpip.c when NO_SYS=0) */
  sys_check_timeouts();
  ___result = sys_timeouts_sleeptime();
#else
  // fprintf(stderr, "enabling callbacks into gambit\n");
  lwip_gambit_unlock();
  if(___arg1 < 0) ___arg1 = local_sys_timeouts_sleeptime();
  // fprintf(stderr, "gambit sleep %d microseconds\n", ___arg1);
  usleep(___arg1);
  ___result = local_sys_timeouts_sleeptime();
  lwip_gambit_lock();
#endif
END
)
  timeout))

(c-declare #<<c-declare-end

static err_t
lwip_netif_linkoutput(struct netif *netif, struct pbuf *p)
{
  struct ethernetif *ether = netif->state;
  err_t result;
  struct pbuf *q = q;
  char buf[LOCAL_MTU+32];
  char *bp=buf;
  size_t bl=0;
  // fprintf(stderr, "linkoutput pre\n");  // DEBUG
  if(netif == NULL) return -1;
  if(p == NULL) return -1;
  // fprintf(stderr, "linkoutput\n");  // DEBUG

  if(q->next != NULL) {
    // fprintf(stderr, "lwip_netif_linkoutput: collect chained buffer into single one\n");
    for (; q != NULL; q = q->next) {
      /* Send the data from the pbuf to the interface, one pbuf at a
         time. The size of the data in each pbuf is kept in the ->len
         variable. */
      memcpy(bp, q->payload, q->len);
      bp += q->len;
      bl += q->len;
    }
    bp = buf;
  } else {
    bp=q->payload;
    bl=q->len;
  }

  if(1){
    struct eth_hdr *ethhdr = (struct eth_hdr *)bp;
    uint64_t src = 0, dst = 0;  // FIXME: Better use lwip types!!!
    uint64_t nwid = 0;  // FIXME
    // fprintf(stderr, "lwip_netif_linkoutput: prepare ethernet frame\n");
    size_t len = bl - sizeof(struct eth_hdr);
    bp += sizeof(struct eth_hdr);
    src = mac_from_u8v6(ethhdr->src.addr);
    dst = mac_from_u8v6(ethhdr->dest.addr);
//*
    memcpy(&src, ethhdr->src.addr, ETH_HWADDR_LEN);
    memcpy(&dst, ethhdr->dest.addr, ETH_HWADDR_LEN);
//*/
    // fprintf(stderr, "lwip_netif_linkoutput: hand over gambit me %d\n", pthread_self());
    result = scm_ether_send(ether, /*NULL, NULL, nwid,*/ src, dst, lwip_ntohs(ethhdr->type), /* 0,*/ bp, len);
    // fprintf(stderr, "lwip_netif_linkoutput: sent %d\n", result);
  } else {
    fprintf(stderr, "lwip_netif_linkoutput: no send function registered\n");
    return ERR_IF;
  }

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
lwip_send_ethernet_input(struct netif *netif, MACREF from, MACREF to, unsigned int ethertype,
                 const void *data, unsigned int len)
{
  struct pbuf *p,*q;
  struct eth_hdr ethhdr;
  int result;
  // fprintf(stderr, "lwip_send_ethernet_input: interface %p\n", netif);
#ifdef LWIP_STATS
  // fprintf(stderr, "lwip_send_ethernet_input: stats\n");
  stats_display();
#endif
  if(netif == NULL) {
    return ERR_ARG;
  }
  // input mac addresses are in network order (big-endian)
  memcpy(&ethhdr.src.addr, &MACDEREF(from), ETH_HWADDR_LEN);
  memcpy(&ethhdr.dest.addr, &MACDEREF(to), ETH_HWADDR_LEN);
  ethhdr.type = htons(ethertype);
  p = pbuf_alloc(PBUF_RAW, len+sizeof(struct eth_hdr), PBUF_RAM);
  // fprintf(stderr, "lwip_send_ethernet_input: pbuf %p\n", p);
  if (!p) {
    // unable to allocate memory
    return ERR_MEM;
  }
  // First pbuf gets ethernet header at start
  q = p;
  if (q->len < sizeof(ethhdr)) {
    pbuf_free(p);
    // first pbuf smaller than ethernet header
    return ERR_BUF;
  }
  { // fprintf(stderr, "lwip_send_ethernet_input: copy data into pbuf(s)\n");
    const char *src = (const char*) data;
    int rest = q->len - sizeof(ethhdr);
    memcpy(q->payload,&ethhdr,sizeof(ethhdr));
    memcpy((char*)q->payload + sizeof(ethhdr),src,rest);
    src += rest;
    while ((q = q->next)) {
      memcpy(q->payload,src,q->len);
      src += q->len;
    }
  }
  // fprintf(stderr, "lwip_send_ethernet_input: feed packet into netif->input %p\n", netif->input);
  if(netif->input == NULL) {
    fprintf(stderr, "lwip_send_ethernet_input: INTERNAL ERROR feed packet into netif->input \n");
    return ERR_IF;
  }
  if ((result = netif->input(p, netif)) != ERR_OK) {
    fprintf(stderr, "lwip_send_ethernet_input: input failed\n", p);
    pbuf_free(p);
  } else {
    // fprintf(stderr, "lwip_send_ethernet_input: packet has been read into netif\n", p);
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
 r = ethip6_output(netif, q, ip6addr);
 if(r != ERR_OK) fprintf(stderr, "ethip6_output failed for %p %d\n", netif, r);
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
 lwip_gambit_lock();
 gambit_lwipcore_lock();
 return result;
}

static inline void cp_sockaddr_to_ip6_addr(ip6_addr_t *ip6addr, struct sockaddr_in6 *sa_in6)
{
  memcpy(&ip6addr->addr, &sa_in6->sin6_addr, sizeof(ip6addr->addr));
}

static void
lwip_init_interface_IPv6(struct netif *nif, struct sockaddr_storage *ip)
{
  struct sockaddr_in6 *sa_in6 = (struct sockaddr_in6 *) ip;

  ip6_addr_t ip6addr;
  local_lwip_init(); // be sure that's done
  // fprintf(stderr, "lwip_init_interface_IPv6\n");
  // ip6_addr_copy_from_packed(ip6addr, (sa_in6->sin6_addr));
  cp_sockaddr_to_ip6_addr(&ip6addr, sa_in6);
  // nif->ip6_autoconfig_enabled = 1; // too early

  gambit_lwipcore_lock();
  // questionable: passing the state as the old value.  Better don't initialize it?
  if(!netif_add_noaddr(nif, nif->state, netif_init6, INPUT_HANDLER)) {
    fprintf(stderr, "lwip: netif_add_noaddr failed\n");
  }
  // fprintf(stderr, "lwip: netif_create_ip6_linklocal_address\n");
  netif_create_ip6_linklocal_address(nif, 1);
  netif_ip6_addr_set_state(nif, 0, IP6_ADDR_VALID); // was IP6_ADDR_TENTATIVE
  nif->ip6_autoconfig_enabled = 1; // ??
  netif_set_default(nif);  // ??
  // fprintf(stderr, "lwip: netif_set_up\n");
  netif_set_up(nif);  // ??
  // fprintf(stderr, "init addr %s\n", ip6addr_ntoa(&ip6addr));  // DEBUG
  netif_ip6_addr_set(nif, 1, &ip6addr);
  netif_ip6_addr_set_state(nif, 1, IP6_ADDR_TENTATIVE); // was IP6_ADDR_TENTATIVE
  gambit_lwipcore_unlock();
  // fprintf(stderr, "lwip_init_interface_IPv6 DONE\n");
}

c-declare-end
)

(c-define-type eth-addr (struct "eth_addr"))

(define lwip_init_interface_IPv6
 (c-lambda (netif* socket-address) void "lwip_init_interface_IPv6"))

(define lwip-send-ethernet-input!
  ;; Compose ethernet frame and send it to @param netif
  ;;
  ;; mac addresses are in network order (big-endian)
  ;;
  ;; (lwip-send-input! nif srcmac dstmac ethertype payload len)
  (c-lambda
   (netif* unsigned-int64 unsigned-int64 unsigned-int void* size_t) int
   "___result = lwip_send_ethernet_input(___arg1, UI64_TO_MACREF(___arg2), UI64_TO_MACREF(___arg3), ___arg4,___arg5, ___arg6);"))

;;(define lwip-default-netif-poll! (c-lambda () void "default_netif_poll"))

(define lwip-nd6-find-route
  (c-lambda
   ;; TODO: check for IPv6 address.
   (socket-address) netif*
   "ip6_addr_t a; cp_sockaddr_to_ip6_addr(&a,(struct sockaddr_in6*) ___arg1); ___result = nd6_find_route(&a);"))

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

const ip6_addr_t *gambit_lwip_nd6_get_gw(struct netif *netif, const ip6_addr_t *dest)
{
 // return scm_lwip_nd6_get_gw(netif, dest);
/*
 struct lwip_nd6_get_gw__args r = {netif, dest, NULL};
 lwip_calling_back(NULL, call_scm_lwip_nd6_get_gw, &r);
 return r.result;
*/
ip6_addr_t dummy;
ip6_addr_set(&dummy, dest);
fprintf(stderr, "RETURN %p\n", dest);
return dest;
}
c-declare-end
)

;;;* TCP callback API

(define-c-constant lwip-IPADDR_TYPE_V4 int "IPADDR_TYPE_V4")
(define-c-constant lwip-IPADDR_TYPE_V6 int "IPADDR_TYPE_V6")
(define-c-constant lwip-IPADDR_TYPE_ANY int "IPADDR_TYPE_ANY")

(c-define-type tcp_pcb (pointer (struct "tcp_pcb")))

(c-define-type pbuf* (pointer (struct "pbuf")))

;;(c-define-type CONTEXT (pointer "void")) ;; we may want to redefine that.
(c-define-type CONTEXT scheme-object) ;; we may want to redefine that.

;;; Function prototype for tcp accept callback functions. Called when
;;; a new connection can be accepted on a listening pcb.
;;;
;;; @param ctx Additional argument to pass to the callback function
;;; @see tcp-pcb-context-set!
;;;
;;; @param newpcb The new connection pcb
;;;
;;; @param err An error code if there has been an error accepting.
;;; Only return ERR_ABRT if you have called tcp_abort from within the
;;; callback function!
;;;
;;; typedef err_t (*tcp_accept_fn)(void *ctx, struct tcp_pcb *newpcb, err_t err);

(c-define-type tcp_accept_fn (function (CONTEXT tcp_pcb err_t) err_t))

(define-custom on-tcp-accept #f) ;; TBD

(define-macro (custom-handler name default . args)
  ;; FIXME: We better check from c-define-with-gambit-locked.0 for the
  ;; handler BEFORE we lock the core and provide defaults in plain C!
  (let ((handler (gensym 'handler)))
    `(let ((,handler (,name)))
       (if ,handler
           (,handler . ,args)
           ,default))))

(c-define-with-gambit-locked.0
 (%%on-tcp-accept ctx connection err)
 (CONTEXT tcp_pcb err_t)
 err_t "scm_tcp_accept" "static inline"
 "(void *ctx, struct tcp_pcb* connection, err_t err)" "err_t" "ERR_IF"
 (custom-handler
  on-tcp-accept
  (begin
    ;; default as if nothing had been registered
    (tcp_abort (debug 'TCP-accept-aborting connection))
    ERR_ABRT)
  ctx connection err))

;; typedef err_t (*tcp_recv_fn)(void *arg, struct tcp_pcb *tpcb, struct pbuf *p, err_t err);

(c-define-type tcp_recv_fn (function (CONTEXT tcp_pcb pbuf* err_t) err_t))

(define-custom on-tcp-receive #f)

(c-define-with-gambit-locked.0
 (%%on-tcp-receive ctx connection pbuf err)
 (CONTEXT tcp_pcb pbuf* err_t)
 err_t "scm_tcp_recv" "static inline"
 "(void *ctx, struct tcp_pcb *connection, struct pbuf *pbuf, err_t err)" "err_t" "ERR_IF"
 (custom-handler
  on-tcp-recv
  (begin
    ;; TODO: compare to default as if nothing had been registered
    (tcp_abort (debug 'TCP-receive-aborting connection))
    ERR_ABRT)
  ctx connection pbuf err))

;; typedef err_t (*tcp_sent_fn)(void *arg, struct tcp_pcb *tpcb, u16_t len);
;;
;; Function prototype for tcp sent callback functions. Called when sent data has
;; been acknowledged by the remote side. Use it to free corresponding resources.
;; This also means that the pcb has now space available to send new data.

(c-define-type tcp_sent_fn (function (CONTEXT tcp_pcb unsigned-int16) err_t))

(define-custom on-tcp-sent #f)

(c-define-with-gambit-locked.0
 (%%on-tcp-sent ctx connection len)
 (CONTEXT tcp_pcb unsigned-int16)
 err_t "scm_tcp_sent" "static inline"
 "(void *ctx, struct tcp_pcb* connection, u16_t len)" "err_t" "ERR_IF"
 (begin
   (debug 'NYI '%%on-tcp-sent)
 (custom-handler
  on-tcp-sent
  ;; TBD: free ressources
  (begin
    ;; FIXME: compare to default as if nothing had been registered
    ERR_OK)
  ctx connection len)))

;; Function prototype for tcp poll callback functions. Called
;; periodically as specified by @see tcp_poll.
;;
;; typedef err_t (*tcp_poll_fn)(void *arg, struct tcp_pcb *tpcb);

(c-define-type tcp_poll_fn (function (CONTEXT tcp_pcb) err_t))

(define-custom on-tcp-poll #f)

(c-define-with-gambit-locked.0
 (%%on-tcp-poll ctx connection)
 (CONTEXT tcp_pcb)
 err_t "scm_tcp_poll" "static inline"
 "(void *ctx, struct tcp_pcb* connection)" "err_t" "ERR_IF"
 (custom-handler
  on-tcp-poll
  ;; TBD: NYI
  0 ;; ERR_OK
  ctx connection))

;; Function prototype for tcp error callback functions. Called when
;; the pcb receives a RST or is unexpectedly closed for any other
;; reason.
;;
;; typedef void  (*tcp_err_fn)(void *arg, err_t err);

(c-define-type tcp_err_fn (function (CONTEXT err_t) void))

(define-custom on-tcp-error #f)

(c-define-with-gambit-locked.0
 (%%on-tcp-error ctx err)
 (CONTEXT err_t)
 void "scm_tcp_error" "static inline"
 "(void *ctx, err_t err)" 'ignored 'ignored
 (custom-handler
  on-tcp-error
  (begin
    ;; FIXME TODO: compare to default as if nothing had been registered
    (lwip-tcp-close (debug 'lwip-TCP-error-close ctx))
    #!void)
  ctx err))

;; Function prototype for tcp connected callback functions. Called
;; when a pcb is connected to the remote side after initiating a
;; connection attempt by calling tcp_connect().
;;
;; typedef err_t (*tcp_connected_fn)(void *arg, struct tcp_pcb *tpcb, err_t err);

(c-define-type tcp_connected_fn (function (CONTEXT tcp_pcb err_t) err_t))

(define-custom on-tcp-connect #f)

(c-define-with-gambit-locked.0
 (%%on-tcp-connect ctx connection err)
 (CONTEXT tcp_pcb err_t)
 err_t "scm_tcp_connect" "static inline"
 "(void *ctx, struct tcp_pcb* connection, err_t err)" "err_t" "ERR_IF"
 (custom-handler
  on-tcp-connect
  (begin
    ;; FIXME TODO: compare to default as if nothing had been registered
    (lwip-tcp-close (debug 'lwip-TCP-connect-close ctx))
    err)
  ctx connection err))

#|
/** TCP connected callback (active connection), send data now */
static err_t
lwiperf_tcp_client_connected(void *arg, struct tcp_pcb *tpcb, err_t err)
{
  lwiperf_state_tcp_t *conn = (lwiperf_state_tcp_t *)arg;
  LWIP_ASSERT("invalid conn", conn->conn_pcb == tpcb);
  LWIP_UNUSED_ARG(tpcb);
  if (err != ERR_OK) {
    lwiperf_tcp_close(conn, LWIPERF_TCP_ABORTED_REMOTE);
    return ERR_OK;
  }
  conn->poll_count = 0;
  conn->time_started = sys_now();
  return lwiperf_tcp_client_send_more(conn);
}
|#

(define tcp-new (c-lambda () tcp_pcb "tcp_new"))

(define tcp-new-ip-type (c-lambda (unsigned-int8) tcp_pcb "tcp_new_ip_type"))

(define (tcp-new6) (tcp-new-ip-type lwip-IPADDR_TYPE_V6))

(define tcp-context-set! (c-lambda (tcp_pcb CONTEXT) void "tcp_arg"))

(define (tcp-set-receive! pcb)
  ((c-lambda (tcp_pcb tcp_recv_fn) void "tcp_recv") pcb %%on-tcp-receive))

(define (tcp-set-sent! pcb)
  ((c-lambda (tcp_pcb tcp_sent_fn) void "tcp_sent") pcb %%on-tcp-sent))

(define (tcp-set-err! pcb)
  ((c-lambda (tcp_pcb tcp_err_fn) void "tcp_err") pcb %%on-tcp-error))

(define (tcp-set-accept! pcb)
  ((c-lambda (tcp_pcb tcp_accept_fn) void "tcp_accept") pcb %%on-tcp-accept))

(define (tcp-set-poll! pcb)
  ((c-lambda (tcp_pcb tcp_poll_fn unsigned-int8) void "tcp_poll") pcb %%on-tcp-poll))

(define tcp-flags-set! (c-lambda (tcp_pcb unsigned-int) void "tcp_set_flags"))

(define tcp-flags-clear! (c-lambda (tcp_pcb unsigned-int) void "tcp_clear_flags"))

(define tcp-flags-set? (c-lambda (tcp_pcb unsigned-int) bool "tcp_is_flag_set"))

;; TBD Some are missing here

;; To be called by the application to aknowledge that it has read that
;; much incoming data.
(define tcp-received! (c-lambda (tcp_pcb unsigned-int16) void "tcp_recved"))

;; err_t tcp_bind (struct tcp_pcb *pcb, const ip_addr_t *ipaddr, u16_t port);

(define lwip-tcp-bind/socket-address
  (c-lambda
   ;; FIXME: works only with IPv6 for now.
   (tcp_pcb socket-address) err_t #<<END
   ip6_addr_t ip6addr;
   cp_sockaddr_to_ip6_addr(&ip6addr, (struct sockaddr_in6 *) ___arg2);
   ___result = tcp_bind(___arg1, &ip6addr, ((struct sockaddr_in6 *) ___arg2)->sin6_port);
END
))

(define (lwip-tcp-bind pcb u8 port)
  ;; FIXME: works only with IPv6 for now.
  (unless (eqv? (u8vector-length u8) 16) (error "lwip-tcp-bind: currently only IPv6"))
  ((c-lambda
   (tcp_pcb scheme-object unsigned-int) err_t #<<END
   ip_addr_t ipaddr;
   ipaddr.type = IPADDR_TYPE_V6;
   ipaddr.u_addr.ip6.zone = 0;
   memcpy(&ipaddr.u_addr.ip6.addr, ___CAST(void *,___BODY_AS(___arg2,___tSUBTYPED)), sizeof(ip6_addr_t));
   gambit_lwipcore_lock();
   ___result = tcp_bind(___arg1, &ipaddr, ___arg3);
   gambit_lwipcore_unlock();
END
) pcb u8 port))

(define (lwip-tcp-bind/netif pcb netif)
  ((c-lambda
   (tcp_pcb netif*) void #<<END
   gambit_lwipcore_lock();
   tcp_bind_netif(___arg1, ___arg2);
   gambit_lwipcore_unlock();
END
) pcb netif))

;; err_t tcp_connect (struct tcp_pcb *pcb, const ip_addr_t *ipaddr, u16_t port, tcp_connected_fn connected);

#;(define lwip-tcp-connect/socket-address
  (c-lambda
   ;; FIXME: works only with IPv6 for now.
   (tcp_pcb socket-address tcp_connected_fn) err_t #<<END
   ip6_addr_t ip6addr;
   cp_sockaddr_to_ip6_addr(&ip6addr, (struct sockaddr_in6 *) ___arg2);
   ___result = tcp_connect(___arg1, &ip6addr, ((struct sockaddr_in6 *) ___arg2)->sin6_port, ___arg3);
END
))

(define (lwip-tcp-connect pcb u8 port)
  ((c-lambda
   ;; FIXME: works only with IPv6 for now.
   (tcp_pcb scheme-object unsigned-int tcp_connected_fn) err_t #<<END
   ip_addr_t ipaddr;
   ipaddr.type = IPADDR_TYPE_V6;
   ipaddr.u_addr.ip6.zone = 0;
   memcpy(&ipaddr.u_addr.ip6.addr, ___CAST(void *,___BODY_AS(___arg2,___tSUBTYPED)), sizeof(ip6_addr_t));
   gambit_lwipcore_lock();
   ___result = tcp_connect(___arg1, &ipaddr, ___arg3, ___arg4);
   gambit_lwipcore_unlock();
END
) pcb u8 port %%on-tcp-connect))

(define lwip-tcp-listen (c-lambda (tcp_pcb) tcp_pcb "tcp_listen"))

(define tcp_abort (c-lambda (tcp_pcb) void "tcp_abort"))

(define (lwip-tcp-close pcb)
  (let ((r ((c-lambda (tcp_pcb) err_t  "tcp_close") pcb)))
    (if (eqv? r ERR_OK) r
        (begin
          (tcp_abort pcb)
          (debug 'lwip-tcp-close r)))))

(define lwip-tcp_shutdown (c-lambda (tcp_pcb bool bool) err_t "tcp_shutdown"))

(define lwip-tcp_write (c-lambda (tcp_pcb void* unsigned-int16 unsigned-int8) err_t  "tcp_write"))

(define lwip-tcp-prio-set! (c-lambda (tcp_pcb unsigned-int8) void "tcp_setprio"))

(define lwip-tcp-flush! (c-lambda (tcp_pcb) err_t "tcp_output"))

;; (define lwip- (c-lambda ()  ""))

;; (define lwip-tcp_tcp_get_tcp_addrinfo (c-lambda ()  ""))
