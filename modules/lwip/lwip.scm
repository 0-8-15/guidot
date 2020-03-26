(define-macro (define-c-constant var type . const)
  (let* ((const (if (not (null? const)) (car const) (symbol->string var)))
	 (str (string-append "___result = " const ";")))
    `(define ,var ((c-lambda () ,type ,str)))))

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

struct ethernetif {
  struct netif net;
  int (*send_packet)(uint64_t src, uint64_t dst, int proto, char *buf, size_t len);
};

static void lwip_init_once()
{
  static bool done = false;
  if(!done) {
    done=true;
    fprintf(stderr, "Init NO_SYS: %d\n", NO_SYS); // DEBUG
    lwip_init();
  }
}

static err_t
lwip_netif_linkoutput(struct netif *netif, struct pbuf *p)
{
  struct ethernetif *ether = netif->state;
  struct pbuf *q = q;
  char buf[LOCAL_MTU+32];
  char *bp=buf;
  size_t bl=0;
  fprintf(stderr, "linkoutput pre\n");  // DEBUG
  if(netif == NULL) return -1;
  if(p == NULL) return -1;
  fprintf(stderr, "linkoutput\n");  // DEBUG

  if(q->next != NULL) {
    // collect chained buffer into single one
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

  if(ether->send_packet){
    struct eth_hdr *ethhdr = (struct eth_hdr *)bp;
    uint64_t src = 0, dst = 0;
    uint64_t nwid = 0;  // FIXME
    int proto = ntohs((uint16_t)ethhdr->type);
    size_t len = bl - sizeof(struct eth_hdr);
    bp += sizeof(struct eth_hdr);
    memcpy(&src, ethhdr->src.addr, ETH_HWADDR_LEN);
    memcpy(&dst, ethhdr->dest.addr, ETH_HWADDR_LEN);
    ether->send_packet(/*NULL, NULL, nwid,*/ src, dst, proto, /* 0,*/ bp, len);
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

void lwip_init_interface_IPv6(struct netif *nif, struct sockaddr_storage *ip)
{
  struct sockaddr_in6 *sa_in6 = (struct sockaddr_in6 *) ip;

  ip6_addr_t ip6addr;
  lwip_init_once(); // be sure that's done
  // ip6_addr_copy_from_packed(ip6addr, (sa_in6->sin6_addr));
  memcpy(&(ip6addr.addr), &(sa_in6->sin6_addr), sizeof(ip6addr.addr));
  // nif->ip6_autoconfig_enabled = 1; // too early

  LOCK_TCPIP_CORE();
  // questionable: pasing the state as the old value.  Better don't initialize it?
  if(!netif_add_noaddr(nif, nif->state, netif_init6, INPUT_HANDLER)) {
    fprintf(stderr, "lwip: netif_add_noaddr failed\n");
  }
  netif_create_ip6_linklocal_address(nif, 1);
  netif_ip6_addr_set_state(nif, 0, IP6_ADDR_VALID); // was IP6_ADDR_TENTATIVE
  nif->ip6_autoconfig_enabled = 1; // ??
  netif_set_default(nif);  // ??
  netif_set_up(nif);  // ??
  fprintf(stderr, "init addr %s\n", ip6addr_ntoa(&ip6addr));  // DEBUG
  netif_ip6_addr_set(nif, 1, &ip6addr);
  netif_ip6_addr_set_state(nif, 1, IP6_ADDR_TENTATIVE); // was IP6_ADDR_TENTATIVE
  UNLOCK_TCPIP_CORE();
}

c-declare-end
)

(define lwip-init! (c-lambda () void "lwip_init_once"))

(c-define-type void* (pointer "void"))

(c-define-type socket-address (pointer (struct "sockaddr_storage") socket-address))

(c-define-type netif* (pointer (struct "netif") netif))

(define lwip-make-netif
  (c-lambda
   (unsigned-int64 void*) netif* #<<END
   struct ethernetif *nif = (struct ethernetif *) malloc(sizeof(struct ethernetif));
   // struct netif *net = (struct netif *)malloc(sizeof(struct netif));
   // memset(net, sizeof(struct netif), 0);
   memset(nif, sizeof(struct ethernetif), 0);
   memcpy(&nif->net.hwaddr, &___arg1, ETH_HWADDR_LEN);
   nif->net.hwaddr_len = ETH_HWADDR_LEN;
   nif->net.state = nif; // ??? should we better return the `etherneitif` type?
   nif->send_packet = ___arg2;
   ___result_voidstar = nif;
END
))

(define lwip-check-timeouts!
 (c-lambda () void #<<END
#if NO_SYS
  /* handle timers (already done in tcpip.c when NO_SYS=0) */
  sys_check_timeouts();
#endif
END
))

(define lwip_init_interface_IPv6
 (c-lambda (netif* socket-address) void "lwip_init_interface_IPv6"))

;;(define lwip-default-netif-poll! (c-lambda () void "default_netif_poll"))
