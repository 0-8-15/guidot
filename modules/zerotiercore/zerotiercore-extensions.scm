;;* Extensions

;; These require changes to zerotier core code.

(c-declare #<<c-declare-end
extern enum ZT_ResultCode
 ZT_Node_contact_peer(ZT_Node *node, void *tptr, unsigned int port,
                      const char* id, const struct sockaddr_storage *addr, int64_t now);

static int zt_contact_peer(ZT_Node *zt_node, int lsock, const char* id, const struct sockaddr_storage *addr)
{
 return ZT_Node_contact_peer(zt_node, NULL, lsock, id, addr, zt_now()) == ZT_RESULT_OK;
}
c-declare-end
)

(define (zt-contact-peer id addr #!optional (lsock 0))
  (assert-zt-up! zt-contact-peer)
  ((c-lambda (zt-node int char-string socket-address) bool "zt_contact_peer")
   (zt-prm-zt %%zt-prm) lsock id addr))
