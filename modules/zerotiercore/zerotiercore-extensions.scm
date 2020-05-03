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
  (begin-zt-exclusive
   ((c-lambda (zt-node int char-string gamsock-socket-address) bool "zt_contact_peer")
    (zt-prm-zt %%zt-prm) lsock id addr))
  addr)

#|
;; Might be usedful.
bool ZT_Node_setConfigItem(ZT_Node* node, uint64_t nwid, int item, uint64_t value)
{
  try {
    ZeroTier::SharedPtr<ZeroTier::Network> net = reinterpret_cast<ZeroTier::Node *>(node)->network(nwid);
    if(!net->hasConfig()) {
      fprintf(stderr, "ZT_Node_setConfigItem: network %xll has no config\n", nwid);
      return false;
    }
    ZeroTier::NetworkConfig &cfg = (ZeroTier::NetworkConfig &) net->config();
    // Hm. FIXME.  Maybe this would need locking.  Function first, it's an experiment.
    switch(item) {
    case 1: cfg.mtu = value; return true;
    case 2: cfg.multicastLimit = value; return true;
    }
    return false;
  } catch ( ... ) {
    return false;
  }
}
;;; ------

(c-declare "extern bool ZT_Node_setConfigItem(ZT_Node* node, uint64_t nwid, int item, uint64_t value);")

(define (zt-set-config-item! nwid item value)
  (assert-zt-up! zt-set-config-item)
  (begin-zt-exclusive
   ((c-lambda (zt-node unsigned-int64 int unsigned-int64) bool "ZT_Node_setConfigItem")
    (zt-prm-zt %%zt-prm) nwid item value)))
|#
