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

typedef uint64_t MAC;
typedef uint64_t MACREF;
#define MACDEREF(x) (x)

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
 // fprintf(stderr, "Gambit REQ %d - %d %d\n", pthread_self(), the_gambit_owner, current_gambit_count);
 if(the_gambit_owner != pthread_self() ) {
  sys_mutex_lock(&gambit_lock);
  the_gambit_owner = pthread_self();
 }
 current_gambit_count++;
 // fprintf(stderr, "Gambit ENTER %d %d\n", the_gambit_owner, current_gambit_count);
 if(current_gambit_count > 2) {
  fprintf(stderr, "WARNING: Gambit ENTERed AGAIN and AGAIN %d %d\n", the_gambit_owner, current_gambit_count);
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
    fprintf(stderr, "Init NO_SYS: %d\n", NO_SYS); // DEBUG
    // lwip_init();
    if(sys_sem_new(&sem, 0) != ERR_OK) return 0;
    tcpip_init(tcpip_init_done, &sem);
    sys_sem_wait(&sem);
    // disable callbacks until being asked for in
    return 1;
  }
  return 0;
}

static inline u32_t local_sys_timeouts_sleeptime()
{
  u32_t result = 0;
  LOCK_TCPIP_CORE();
  result = sys_timeouts_sleeptime();
  UNLOCK_TCPIP_CORE();
  return result;
}

c-declare-end
)

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

static inline uint64_t mac_from_vector(const void *src) // maybe better hwaddr instead of void
{
 const unsigned char *b = (const unsigned char *)src;
 uint64_t result;
 result = ((((uint64_t)*b) & 0xff) << 40); ++b;
 result |= ((((uint64_t)*b) & 0xff) << 32); ++b;
 result |= ((((uint64_t)*b) & 0xff) << 24); ++b;
 result |= ((((uint64_t)*b) & 0xff) << 16); ++b;
 result |= ((((uint64_t)*b) & 0xff) << 8); ++b;
 result |= (((uint64_t)*b) & 0xff);
 return result; // | ;-( highlithing confused with wrong numbers of vertical bars here
}

static inline void g_lwip_set_mac_ui64(void *buf, uint64_t mac)
{
 unsigned char *b = (unsigned char *)buf;
 *(b++) = (unsigned char)((mac >> 40) & 0xff);
 *(b++) = (unsigned char)((mac >> 32) & 0xff);
 *(b++) = (unsigned char)((mac >> 24) & 0xff);
 *(b++) = (unsigned char)((mac >> 16) & 0xff);
 *(b++) = (unsigned char)((mac >> 8) & 0xff);
 *b = (unsigned char)(mac & 0xff);
}


c-declare-end
)

(define (->lwip-mac x) ;; return a uint64_t in network byte order
  (cond
   ((u8-vector? x)
    ((c-lambda
      (scheme-object) unsigned-int64
      "___result = mac_from_vector(___CAST(void *,___BODY_AS(___arg1,___tSUBTYPED)));")
     ) x)
   (else (error "->zt-mac illegal argument" x))))


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
 ((pointer (struct "ethernetif")) unsigned-int64 unsigned-int64 int void* size_t)
 int "Xscm_ether_send" "static"
 (let ((handler (lwip-ethernet-send)))
   (cond
    ((procedure? handler) (handler netif src dst proto #;0 bp len))
    (else -12))))

(c-declare #<<c-declare-end
static int scm_ether_send(struct ethernetif *nif, uint64_t src, uint64_t dst, int proto, void *buf, size_t len)
{
 int got_throw = 0;
 int result;
 // fprintf(stderr, "scm_ether_send\n");
 lwip_gambit_lock();
 // fprintf(stderr, "scm_ether_send gambit locked\n");
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
   g_lwip_set_mac_ui64(&nif->net.hwaddr, ___arg1);
   // memcpy(&nif->net.hwaddr, &hwaddr.c, ETH_HWADDR_LEN);
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
   "___result = mac_from_vector(___arg1->hwaddr);"))

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
    uint64_t src = 0, dst = 0;
    uint64_t nwid = 0;  // FIXME
    // fprintf(stderr, "lwip_netif_linkoutput: prepare ethernet frame\n");
    int proto = ntohs((uint16_t)ethhdr->type);
    size_t len = bl - sizeof(struct eth_hdr);
    bp += sizeof(struct eth_hdr);
    src = mac_from_vector(ethhdr->src.addr);
    dst = mac_from_vector(ethhdr->dest.addr);
    // fprintf(stderr, "lwip_netif_linkoutput: hand over gambit me %d\n", pthread_self());
    result = scm_ether_send(ether, /*NULL, NULL, nwid,*/ src, dst, proto, /* 0,*/ bp, len);
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
    fprintf(stderr, "lwip_send_ethernet_input: packet has been read into netif\n", p);
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

static err_t netif_init6(struct netif *nif)
{
  // Called from netif code
  nif->hwaddr_len = 6;
  nif->name[0]    = IFNAME0;
  nif->name[1]    = IFNAME1;
  nif->linkoutput = lwip_netif_linkoutput;
  nif->output     = NULL /*etharp_output*/;
  nif->output_ip6 = ethip6_output;
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
 UNLOCK_TCPIP_CORE();
 in = s ? (s)(e) : NULL;
 lwip_gambit_unlock();
 result = (f)(in, e);
 lwip_gambit_lock();
 LOCK_TCPIP_CORE();
 return result;
}

static void
lwip_init_interface_IPv6(struct netif *nif, struct sockaddr_storage *ip)
{
  struct sockaddr_in6 *sa_in6 = (struct sockaddr_in6 *) ip;

  ip6_addr_t ip6addr;
  local_lwip_init(); // be sure that's done
  // fprintf(stderr, "lwip_init_interface_IPv6\n");
  // ip6_addr_copy_from_packed(ip6addr, (sa_in6->sin6_addr));
  memcpy(&(ip6addr.addr), &(sa_in6->sin6_addr), sizeof(ip6addr.addr));
  // nif->ip6_autoconfig_enabled = 1; // too early

  LOCK_TCPIP_CORE();
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
  UNLOCK_TCPIP_CORE();
  // fprintf(stderr, "lwip_init_interface_IPv6 DONE\n");
}

c-declare-end
)

(define lwip_init_interface_IPv6
 (c-lambda (netif* socket-address) void "lwip_init_interface_IPv6"))

(define lwip-send-ethernet-input!
  ;; Compose ethernet frame and send it to @param netif
  ;;
  ;; (lwip-send-input! nif srcmac dstmac ethertype payload len)
  (c-lambda
   (netif* unsigned-int64 unsigned-int64 unsigned-int void* size_t) int
   "lwip_send_ethernet_input"))

;;(define lwip-default-netif-poll! (c-lambda () void "default_netif_poll"))
