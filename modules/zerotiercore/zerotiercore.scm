#|

* Thread Model

- (zt-send! to type data)  :: ;; EXPORT - send user message (u8vector)

  Cause: Any peer knowing /to/ as our MAC *MAY* possible *DoS/SPAM* us.
    (Would cause O(2) traffic increase: /to/ sending multiple packets our way
     we too. )

  Mitigation:
    - change frequently
    - avoid ad-hoc networks

  Defence:
    - use network management
      -> 1. NO P2P until JOIN succeeded.
      -> 2. (since: "change frequently" OR "may have leaked"):
         consider "take down" measurements.
      -> ...
|#


(define-macro (define-c-constant var type . const)
  (let* ((const (if (not (null? const)) (car const) (symbol->string var)))
	 (str (string-append "___return(" const ");")))
    `(define ,var ((c-lambda () ,type ,str)))))

;; There's an issue with gambit when using `set!` - it does sometimes,
;; but not always set what I expect.

(define-macro (define-custom name initial)
  `(define ,name
     (let ((val ,initial))
       (case-lambda
        (() val)
        ((x) (set! val x))))))

(c-declare #<<c-declare-end

typedef enum {false=0, true=1} bool;
#include <zerotiercore/ZeroTierOne.h>
#include <stdlib.h>
#include <string.h>

// snprintf
#include <stdio.h>

c-declare-end
)

;; There should be only one ZT per process.  ZT is tricky enough.

(define-macro (assert-zt-up! caller) `(or (zt-up?) (error "ZT not running" ',caller)))

(c-declare #<<END
static uint64_t zt_now()
{
#ifdef __WINDOWS__
 FILETIME ft;
 SYSTEMTIME st;
 ULARGE_INTEGER tmp;
 GetSystemTime(&st);
 SystemTimeToFileTime(&st,&ft);
 tmp.LowPart = ft.dwLowDateTime;
 tmp.HighPart = ft.dwHighDateTime;
 return (int64_t)( ((tmp.QuadPart - 116444736000000000LL) / 10000L) + st.wMilliseconds );
#else
 #include <sys/time.h>
 struct timeval tv;
//#ifdef __LINUX__
// syscall(SYS_gettimeofday,&tv,0); /* fix for musl libc broken gettimeofday bug */
//#else
 gettimeofday(&tv,(struct timezone *)0);
//#endif
 return ( (1000LL * (int64_t)tv.tv_sec) + (int64_t)(tv.tv_usec / 1000) );
#endif
}
END
)

(define zt-now (c-lambda () unsigned-int64 "zt_now"))

(define-c-constant ZT_RESULT_OK int "ZT_RESULT_OK")

(c-define-type void* (pointer "void"))

(c-define-type void** (pointer (pointer "void")))

(c-define-type zt-node (pointer "ZT_Node"))

(c-define-type socket-address (pointer (struct "sockaddr_storage") socket-address))

(c-define-type zt-message (pointer "ZT_UserMessage" zt-message))

(c-define-type ZT_NodeStatus (pointer "ZT_NodeStatus"))

(c-define-type zt-virtual-config* (pointer "ZT_VirtualNetworkConfig" zt-virtual-config))

(c-define-type zt-peers (pointer "ZT_PeerList" zt-peers))

(c-define-type zt-peer (pointer "ZT_Peer" zt-peer))

(c-define-type ZT_PeerPhysicalPath (pointer "ZT_PeerPhysicalPath" ZT_PeerPhysicalPath))

(define-macro (%%checked location expr fail)
  `(with-exception-catcher
    (lambda (ex)
      (debug ',location ex)
      ,fail)
    (lambda () ,expr)))

;; ZT event callback (zt-event node userptr thr event payload)

(define-c-constant ZT_EVENT_UP int "ZT_EVENT_UP")
(define-c-constant ZT_EVENT_OFFLINE int "ZT_EVENT_OFFLINE")
(define-c-constant ZT_EVENT_ONLINE int "ZT_EVENT_ONLINE")
(define-c-constant ZT_EVENT_DOWN int "ZT_EVENT_DOWN")
(define-c-constant ZT_EVENT_FATAL_ERROR_IDENTITY_COLLISION int "ZT_EVENT_FATAL_ERROR_IDENTITY_COLLISION")
(define-c-constant ZT_EVENT_TRACE int "ZT_EVENT_TRACE")
(define-c-constant ZT_EVENT_USER_MESSAGE int "ZT_EVENT_USER_MESSAGE")
(define-c-constant ZT_EVENT_REMOTE_TRACE int "ZT_EVENT_REMOTE_TRACE")

(define-custom zt-event #f) ;; EXPORT HOOK - network events

(c-define
 (zt-event-cb node userptr thr event payload)
 (zt-node void* void* int void*)
 void "scm_zt_event_cb" "static"
 (let ()
   (define (onevt e)
     (cond
      ((eqv? ZT_EVENT_UP e) 'UP)
      ((eqv? ZT_EVENT_OFFLINE e) 'OFFLINE)
      ((eqv? ZT_EVENT_ONLINE e) 'ONLINE)
      ((eqv? ZT_EVENT_DOWN e) 'DOWN)
      ((eqv? ZT_EVENT_FATAL_ERROR_IDENTITY_COLLISION e) 'FATAL_ERROR_IDENTITY_COLLISION)
      ((eqv? ZT_EVENT_TRACE e) 'TRACE)
      ((eqv? ZT_EVENT_REMOTE_TRACE e) 'REMOTE_TRACE)
      (else 'ZT_EVENT_UNKNOWN)))
   (cond
    ((procedure? (zt-event)) (%%checked zt-event ((zt-event) node userptr thr (onevt event) payload) #f)))))

(define-custom zt-recv #f) ;; EXPORT HOOK - user messages

(c-define
 (zt_recv node userptr thr payload)
 (zt-node void* void* zt-message)
 void "scm_zt_recv" "static"
 (cond
  ((procedure? (zt-recv))
   (let ((size ((c-lambda (zt-message) size_t "___return(___arg1->length);") payload))
         (from ((c-lambda (zt-message) size_t "___return(___arg1->origin);") payload))
         (type ((c-lambda (zt-message) size_t "___return(___arg1->typeId);") payload)))
     (let ((data (make-u8vector size)))
       ((c-lambda
         (scheme-object zt-message) void
         "memcpy(___CAST(void *,___BODY_AS(___arg1,___tSUBTYPED)), ___arg2->data, ___arg2->length);")
        data payload)
       (%%checked zt-recv ((zt-recv) from type data) #t))))))

(c-declare #<<c-declare-end
static void
zt_event_cb(ZT_Node *node, void *userptr, void *thr, enum ZT_Event event, const void *payload)
{
 switch(event) {
  case ZT_EVENT_USER_MESSAGE: scm_zt_recv(node, userptr, thr, (ZT_UserMessage*)payload); break;
  default: scm_zt_event_cb(node, userptr, thr, event, (void *) payload);
 }
}
c-declare-end
)

;; ZT State

(define-c-constant ZT_STATE_OBJECT_NULL int "ZT_STATE_OBJECT_NULL")
(define-c-constant ZT_STATE_OBJECT_IDENTITY_PUBLIC int "ZT_STATE_OBJECT_IDENTITY_PUBLIC") ;; required
(define-c-constant ZT_STATE_OBJECT_IDENTITY_SECRET int "ZT_STATE_OBJECT_IDENTITY_SECRET") ;; required
(define-c-constant ZT_STATE_OBJECT_PLANET int "ZT_STATE_OBJECT_PLANET") ;; recommended
(define-c-constant ZT_STATE_OBJECT_MOON int "ZT_STATE_OBJECT_MOON") ;; required
(define-c-constant ZT_STATE_OBJECT_PEER int "ZT_STATE_OBJECT_PEER") ;; optional
(define-c-constant ZT_STATE_OBJECT_NETWORK_CONFIG int "ZT_STATE_OBJECT_NETWORK_CONFIG") ;; required

(define-custom zt-state-get #f) ;; EXPORT HOOK - read state

(define (zt-state-id->symbol e) ;; EXPORT
  (cond
   ((eqv? ZT_STATE_OBJECT_IDENTITY_PUBLIC e) 'IDENTITY_PUBLIC)
   ((eqv? ZT_STATE_OBJECT_IDENTITY_SECRET e) 'IDENTITY_SECRET)
   ((eqv? ZT_STATE_OBJECT_PLANET e) 'PLANET)
   ((eqv? ZT_STATE_OBJECT_MOON e) 'MOON)
   ((eqv? ZT_STATE_OBJECT_PEER e) 'PEER)
   ((eqv? ZT_STATE_OBJECT_NETWORK_CONFIG e) 'NETWORK_CONFIG)
   (else 'NULL)))

(c-define
 (zt_state_get node userptr thr objtype objid into len)
 (zt-node void* void* int unsigned-int64 void* size_t)
 int "scm_zt_state_get" "static"
 (if (procedure? (zt-state-get))
     (let ((v ((zt-state-get) objtype objid)))
       (if (u8vector? v)
           (let ((n (min (u8vector-length v) len)))
             ((c-lambda
               (void* scheme-object size_t) void
               "memcpy(___arg1, ___CAST(void *,___BODY_AS(___arg2,___tSUBTYPED)), ___arg3);")
              into v n)
             n)
           -1))
     -1))

(define-custom zt-state-put #f) ;; EXPORT HOOK - set state

(c-define
 (zt_state_put node userptr thr objtype objid from len)
 (zt-node void* void* int unsigned-int64 void* size_t)
 void "scm_zt_state_put" "static"
 (if (procedure? (zt-state-put))
     (let ((data (make-u8vector len)))
       ((c-lambda
         (scheme-object void* size_t) void
         "memcpy(___CAST(void *,___BODY_AS(___arg1,___tSUBTYPED)), ___arg2, ___arg3);")
        data from len)
       ((zt-state-put) objtype objid data))))

(c-declare #<<c-declare-end

/*
 * This function should return the number of bytes actually stored to the
 * buffer or -1 if the state object was not found or the buffer was too
 * small to store it.
 */

static int
zt_state_get(ZT_Node *node, void *userptr, void *thr,
    enum ZT_StateObjectType objtype, const uint64_t objid[2], void *data,
    unsigned int len)
{
 //;; objid is a vector -- BUT WHY ??? !!!
 //if(objid) fprintf(stderr, "GET ID %lx - %lx\n", objid[0], objid[1]);
 //if(objid[1] != 0) fprintf(stderr, "GET ID %lx - %lx\n", objid[0], objid[1]);
 return scm_zt_state_get(node, userptr, thr, objtype, objid ? objid[0] : 0, data, len);
}

static void
zt_state_put(ZT_Node *node, void *userptr, void *thr,
    enum ZT_StateObjectType objtype, const uint64_t objid[2], const void *data,
    int len)
{
 //if(objid) fprintf(stderr, "PUT ID %lx - %lx\n", objid[0], objid[1]);
 //if(objid[1] != 0) fprintf(stderr, "PUT ID %lx - %lx\n", objid[0], objid[1]);
 scm_zt_state_put(node, userptr, thr, objtype, objid ? objid[0] : 0, (void*)data, len);
}

// ;; TZ nextBackgroundTaskDeadline
static volatile int64_t nextBackgroundTaskDeadline;

c-declare-end
)

(define zt-bg-deadline (c-lambda () unsigned-int64 "___return(nextBackgroundTaskDeadline);"))

;;* ZT Network

;;** ZT Network Wire
;;*** ZT Network Wire Incoming

;; Maybe we should not make this reachable outside this module.
;; Standard use is from the packet receiving thread.
(define (zt-wire-packet-process packet from)
  (define doit
    (c-lambda
     ;; 1     lsock addr           data   7
     (zt-node int64 socket-address scheme-object size_t) bool #<<END
     int rc = -1;
     rc = ZT_Node_processWirePacket(___arg1, NULL, zt_now(), ___arg2, (void *) ___arg3,
             ___CAST(void *,___BODY_AS(___arg4,___tSUBTYPED)), ___arg5, &nextBackgroundTaskDeadline);
     ___return(rc == ZT_RESULT_OK);
END
))
  (and
   (zt-up?)
   (or (doit (zt-prm-zt %%zt-prm) 0 from packet (u8vector-length packet))
       (error "zt-wire-packet-process: failed"))))

;;*** ZT Network Wire Outgoing

;; There is only so much reason to be able to overwrite this.  (Except
;; for debugging.)  Maybe we should at least have a decent default
;; here.

(define-custom zt-wire-packet-send #f) ;; EXPORT?? HOOK - send via UDP
(define-custom zt-wire-packet-send-complex #f) ;; EXPORT?? - send via UDP - MUST NOT raise exceptions

(c-define
 (zt_wire_packet_send node userptr thr socket remaddr data len ttl)
 (zt-node scheme-object void* int socket-address void* size_t unsigned-int)
 int "scm_zt_wire_packet_send" "static"
 ;; BEWARE:
 (cond
  ((not (zt-up?)) -1)
  ((procedure? (zt-wire-packet-send-complex))
   ((zt-wire-packet-send-complex) node userptr thr socket remaddr data len ttl))
  ((procedure? (zt-wire-packet-send))
   (%%checked
    zt_wire_packet_send
    (let ((u8 (make-u8vector len)))
      ((c-lambda
        (scheme-object void* size_t) void
        "memcpy(___CAST(void *,___BODY_AS(___arg1,___tSUBTYPED)), ___arg2, ___arg3);")
       u8 data len)
      (let ((udp (zt-prm-udp %%zt-prm)))
        (if ((zt-wire-packet-send) udp socket remaddr u8 ttl) 0 -1)))
    -1))
  (else -1)))

(c-declare #<<c-declare-end

// This function is called when ZeroTier desires to send a
// physical frame. The data is a UDP payload, the rest of the
// payload should be set over vanilla UDP.
static int
zt_wire_packet_send(ZT_Node *node, void *userptr, void *thr, int64_t socket,
    const struct sockaddr_storage *remaddr, const void *data, unsigned int len,
    unsigned int ttl)
{
 return scm_zt_wire_packet_send(node, (___SCMOBJ) userptr, thr, socket, (struct sockaddr_storage*)remaddr, (void*)data, len, ttl);
}
c-declare-end
)

;;** ZT Network Virtual

;;*** ZT Network Virtual Incoming
(define-custom zt-virtual-receive #f) ;; EXPORT HOOK

(c-define
 (zt_virtual_receive node userptr thr nwid netptr srcmac dstmac ethertype vlanid payload len)
 (zt-node void* void* unsigned-int64 void** unsigned-int64 unsigned-int64 unsigned-int unsigned-int void* size_t)
 void "scm_zt_virtual_recv" "static"
 (if (procedure? (zt-virtual-receive))
     (%%checked
      zt_virtual_receive
      ((zt-virtual-receive) node userptr thr nwid netptr srcmac dstmac ethertype vlanid payload len)
      #f)))

(c-declare #<<c-declare-end

static void
zt_virtual_recv(ZT_Node *node, void *userptr, void *thr, uint64_t nwid,
    void **netptr, uint64_t srcmac, uint64_t dstmac, unsigned int ethertype,
    unsigned int vlanid, const void *payload, unsigned int len)
{
 scm_zt_virtual_recv(node, userptr, thr, nwid, netptr, srcmac, dstmac, ethertype, vlanid, (void*)payload, len);
}

c-declare-end
)

;;*** ZT Network Virtual outgoing

;;
(define (zt-virtual-send nwid srcmac dstmac ethertype vlanid payload) ;; EXPORT
  (assert-zt-up! zt-virtual-send)
  ((c-lambda
    ;; 1     2 nwid         3 src          4 dst          5 ethertype  6 vlan       7
    (zt-node unsigned-int64 unsigned-int64 unsigned-int64 unsigned-int unsigned-int scheme-object size_t)
    bool #<<END
    ___return(ZT_Node_processVirtualNetworkFrame(___arg1, NULL, zt_now(),
                ___arg2, ___arg3, ___arg4, ___arg5, ___arg6,
                ___CAST(void *, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8,
                &nextBackgroundTaskDeadline)
              == ZT_RESULT_OK);
END
)
   (zt-prm-zt %%zt-prm) nwid srcmac dstmac ethertype vlanid payload (u8vector-length payload)))

(define (zt-virtual-send/ptr nwid srcmac dstmac ethertype vlanid data len) ;; EXPORT
  (assert-zt-up! zt-virtual-send)
  ((c-lambda
    ;; 1     2 nwid         3 src          4 dst          5 ethertype  6 vlan       7
    (zt-node unsigned-int64 unsigned-int64 unsigned-int64 unsigned-int unsigned-int void* size_t)
    bool #<<END
    ___return(ZT_Node_processVirtualNetworkFrame(___arg1, NULL, zt_now(),
                 ___arg2, ___arg3, ___arg4, ___arg5, ___arg6,
                 ___arg7, ___arg8,
                 &nextBackgroundTaskDeadline)
              == ZT_RESULT_OK);
END
)
   (zt-prm-zt %%zt-prm) nwid srcmac dstmac ethertype vlanid data len))

;;* ZT Config

(define-custom zt-virtual-config #f) ;; EXPORT

(c-define
 (zt_virtual_config0 node userptr thr nwid netptr op config)
 (zt-node void* void* unsigned-int64 void** int zt-virtual-config*)
 int "scm_zt_virtual_config" "static"
 (let ((opsym (let ((config-operations '#(#f up update down destroy)))
                (lambda (op) (vector-ref config-operations op)))))
   ;; TODO: Make sure multicastSubscribe() or other network-modifying
   ;; methods re disabled while handling is running as it may
   ;; deadlock.  Would it actually?
   (if (procedure? (zt-virtual-config))
       (%%checked
        zt_virtual_config
        (if ((zt-virtual-config) node userptr nwid netptr (opsym op) config) 0 -1)
        0)
       0)))

(c-declare #<<c-declare-end
static int
zt_virtual_config(ZT_Node *node, void *userptr, void *thr, uint64_t nwid,
                  void **netptr, enum ZT_VirtualNetworkConfigOperation op,
                  const ZT_VirtualNetworkConfig *config)
{
 return scm_zt_virtual_config(node, userptr, thr, nwid, netptr, op, (ZT_VirtualNetworkConfig *)config);
}
c-declare-end
)

;; ZT Path

(define-custom zt-path-check #f)

(c-define
 (zt_path_check node userptr thr nodeid socket sa)
 (zt-node void* void* unsigned-int64 int socket-address)
 bool "scm_zt_path_check" "static"
 (if (procedure? (zt-path-check))
     (%%checked
      zt_path_check
      ((zt-path-check) node userptr thr nodeid socket sa)
      #f)
     ;; otherwise use it
     #t))

(define-custom zt-path-lookup #f)

(c-define
 (zt_path_lookup node uptr thr nodeid family sa)
 (zt-node void* void* unsigned-int64 int socket-address)
 int "scm_zt_path_lookup" "static"
 (if (procedure? (zt-path-lookup))
     (%%checked zt_path_lookup ((zt-path-lookup) node uptr thr nodeid family sa) 0)
     ;; otherwise nothing returned
     0))

(c-declare #<<c-declare-end
static int
zt_path_check(ZT_Node * node, void *uptr, void *tptr,
              uint64_t nodeid, int64_t socket, const struct sockaddr_storage * sa)
{
 return scm_zt_path_check(node, uptr, tptr, nodeid, socket, (struct sockaddr_storage *)sa);
}

static int
zt_path_lookup(ZT_Node *node, void *uptr, void * tptr, uint64_t nodeid, int family, struct sockaddr_storage * sa)
{
 return scm_zt_path_lookup(node, uptr, tptr, nodeid, family, sa);
}

static struct ZT_Node_Callbacks zt_callbacks = {
	.version                      = 0,
	.statePutFunction             = zt_state_put,
	.stateGetFunction             = zt_state_get,
	.wirePacketSendFunction       = zt_wire_packet_send,
	.virtualNetworkFrameFunction  = zt_virtual_recv,
	.virtualNetworkConfigFunction = zt_virtual_config,
	.eventCallback                = zt_event_cb,
	.pathCheckFunction            = zt_path_check,
	.pathLookupFunction           = zt_path_lookup
};

c-declare-end
)

(define zt-background-period/lower-limit (make-parameter 0.60))

(define-structure zt-prm zt udp incoming-thread)
(define %%zt-prm #f) ;; keep a scheme pointer to auxillary stuff
(define (zt-node-init! udp #!key (now (zt-now)) (background-period 5))
  (define (init zt now)
    ((c-lambda
      (scheme-object (pointer void) int64) zt-node #<<END
nextBackgroundTaskDeadline = ___arg3;
static ZT_Node *zt_node = NULL;
int rc=ZT_Node_new(&zt_node, (void*) ___arg1, ___arg2, &zt_callbacks, nextBackgroundTaskDeadline);
___return(rc == ZT_RESULT_OK ? zt_node : NULL);
END
)
     zt #f now))
  (define (exn-catcher ex) (debug (thread-name (current-thread)) ex))
  (define (recv-loop)
    (let ((prm %%zt-prm))
      (when prm
            (with-exception-catcher
             exn-catcher
             (lambda ()
               (call-with-values (lambda () (receive-message (zt-prm-udp prm) 3000)) zt-wire-packet-process)))
            (recv-loop))))
  (define (maintainance)
    ((c-lambda
      (zt-node) bool #<<END
      uint64_t now = zt_now();
      int rc = nextBackgroundTaskDeadline <= now ?
      ZT_Node_processBackgroundTasks(___arg1, NULL, now, &nextBackgroundTaskDeadline) : ZT_RESULT_OK;
      ___return(rc == ZT_RESULT_OK);
END
) (zt-prm-zt %%zt-prm)))
  (define (maintainance-loop)
    (thread-sleep! (max background-period (zt-background-period/lower-limit)))
    (when (zt-up?) (%%checked maintainance (((zt-maintainance) %%zt-prm maintainance)) #f) (maintainance-loop)))
  ;; Should we lock?  No: Better document single-threadyness!
  (if (zt-up?) (error "ZT already running"))
  (let ((prm (make-zt-prm #f udp (make-thread recv-loop 'zt-receiver)))
        (nd (init #f now)))
    (if nd
        (begin
          (zt-prm-zt-set! prm nd) ;; let rec
          (set! %%zt-prm prm) ;; zt-up? => true
          ;; start handlers now
          (thread-start! (zt-prm-incoming-thread prm))
          (thread-start! (make-thread maintainance-loop 'zt-maintainance))
          #t)
        (begin
          (close-socket (zt-prm-udp %%zt-prm))
          #f))))

(define (zt-node-destroy!)
  (if %%zt-prm
      (let ((udp (zt-prm-udp %%zt-prm))
            (zt (zt-prm-zt %%zt-prm)))
        (set! %%zt-prm #f)
        ((c-lambda (zt-node) void "ZT_Node_delete(___arg1);") zt)
        (close-socket udp))))

(define (zt-up?) (and %%zt-prm #t))

(define (zt-address) ;; EXPORT
  (and (zt-up?) ((c-lambda (zt-node) unsigned-int64 "___return(ZT_Node_address(___arg1));") (zt-prm-zt %%zt-prm))))

;; zt-pre-maintainance is a hook/predicate.  Should be used to add to mainainace.
;; RETURN: #t to run or #f to suppress running the ZT background tasks.

(define-custom zt-maintainance (lambda (prm thunk) thunk)) ;; EXPORT

(define (zt-add-local-interface-address! sa) ;; EXPORT
  (assert-zt-up! zt-add-local-interface-address)
  ((c-lambda
    (zt-node socket-address) bool
    "___return(ZT_Node_addLocalInterfaceAddress(___arg1, ___arg2));")
   (zt-prm-zt %%zt-prm) sa))

(define (zt-clear-local-interface-address!) ;; EXPORT
  (assert-zt-up! zt-clear-local-interface-address!)
  ((c-lambda (zt-node) void "ZT_Node_clearLocalInterfaceAddresses(___arg1);") (zt-prm-zt %%zt-prm)))

(define (zt-send! to type data) ;; EXPORT - send user message (u8vector)
  (assert-zt-up! zt-send!)
  ((c-lambda
    (zt-node unsigned-int64 unsigned-int64 scheme-object size_t) bool #<<END
    void *buf = ___CAST(void *,___BODY_AS(___arg4,___tSUBTYPED));
    ___return(ZT_Node_sendUserMessage(___arg1, NULL, ___arg2, ___arg3, buf, ___arg5));
END
) (zt-prm-zt %%zt-prm) to type data (u8vector-length data)))

(define (zt-orbit moon #!optional (seed 0)) ;; EXPORT
  (assert-zt-up! zt-orbit)
  ((c-lambda
   (zt-node unsigned-int64 unsigned-int64) bool
   "___return(ZT_Node_orbit(___arg1, NULL, ___arg2, ___arg3) == ZT_RESULT_OK);")
   (zt-prm-zt %%zt-prm) moon seed))

(define (zt-deorbit moon) ;; EXPORT
  (assert-zt-up! zt-deorbit)
  ((c-lambda
   (zt-node unsigned-int64) bool
   "___return(ZT_Node_deorbit(___arg1, NULL, ___arg2));")
   (zt-prm-zt %%zt-prm) moon))

(define (zt-join network) ;; EXPORT
  (define dojoin (c-lambda (zt-node unsigned-int64) int "___return(ZT_Node_join(___arg1, ___arg2, NULL, NULL));"))
  (assert-zt-up! zt-join)
  (let ((rc (dojoin (zt-prm-zt %%zt-prm) network)))
    (or (eqv? rc 0) (error "zt-join: failed for with rc" network rc))))

(define (zt-leave network) ;; EXPORT
  (define doit (c-lambda (zt-node unsigned-int64) int "___return(ZT_Node_leave(___arg1, ___arg2, NULL, NULL));"))
  (assert-zt-up! zt-leave)
  (let ((rc (doit network)))
    (or (eqv? rc 0) (error "zt-leave: failed for with rc" network rc))))

(define (zt-multicast-subscribe network group #!optional (adi 0)) ;; EXPORT
  (define doit
    (c-lambda
     (zt-node unsigned-int64 unsigned-int64 unsigned-int64) int
     "___return(ZT_Node_multicastSubscribe(___arg1, NULL, ___arg2, ___arg3, ___arg4));"))
  (assert-zt-up! zt-multicast-subscribe)
  (let ((rc (doit (zt-prm-zt %%zt-prm) network group adi)))
    (or (eqv? rc 0) (error "zt-multicast-subscribe: failed for with rc" network rc))))

(define (zt-multicast-unsubscribe network group #!optional (adi 0)) ;; EXPORT
  (define doit
    (c-lambda
     (zt-node unsigned-int64 unsigned-int64 unsigned-int64) int
     "___return(ZT_Node_multicastUnsubscribe(___arg1, ___arg2, ___arg3, ___arg4));"))
  (assert-zt-up! zt-multicast-unsubscribe)
  (let ((rc (doit (zt-prm-zt %%zt-prm) network group adi)))
    (or (eqv? rc 0) (error "zt-multicast-unsubscribe: failed for with rc" network rc))))

;;* Inspection

(define zt-node-status
  (let ((bufsiz ((c-lambda () size_t "___return(sizeof(ZT_NodeStatus));")))
        (address (c-lambda (scheme-object) unsigned-int64
                           "___return(___CAST(ZT_NodeStatus *,___BODY_AS(___arg1,___tSUBTYPED))->address);"))
        (public (c-lambda (scheme-object) char-string
                          "___return((char*) ___CAST(ZT_NodeStatus *,___BODY_AS(___arg1,___tSUBTYPED))->publicIdentity);"))
        (private (c-lambda (scheme-object) char-string
                           "___return((char*) ___CAST(ZT_NodeStatus *,___BODY_AS(___arg1,___tSUBTYPED))->secretIdentity);"))
        (online (c-lambda (scheme-object) bool
                           "___return(___CAST(ZT_NodeStatus *,___BODY_AS(___arg1,___tSUBTYPED))->online);")))
    (lambda (#!optional k)
      (assert-zt-up! zt-node-status)
      (let ((buf (make-u8vector bufsiz)))
        ((c-lambda
          (zt-node scheme-object) void
          "ZT_Node_status(___arg1, ___CAST(ZT_NodeStatus *,___BODY_AS(___arg2,___tSUBTYPED)));")
         (zt-prm-zt %%zt-prm) buf)
        (case k
          ((address) (address buf))
          ((public) (public buf))
          ((private) (private buf))
          ((online) (online buf))
          (else (public buf)))))))

(define zt-peer-address (c-lambda (zt-peer) unsigned-int64 "___return(___arg1->address);"))
(define zt-peer-version
  (c-lambda
   (zt-peer) char-string #<<END
   char buf[20] = "-";
   if(___arg1->versionMajor != -1) {
     snprintf(buf,20, "%d.%d.%d",___arg1->versionMajor, ___arg1->versionMinor, ___arg1->versionRev);
   }
   ___return(buf);
END
))
(define zt-peer-latency (c-lambda (zt-peer) int "___return(___arg1->latency);"))
(define zt-peer-role
  (let ((numeric (c-lambda (zt-peer) int "___return(___arg1->role);"))
        (roles '#(leaf moon planet)))
    (lambda (peer) (vector-ref roles (numeric peer)))))
(define zt-peer-path-count (c-lambda (zt-peer) size_t "___return(___arg1->pathCount);"))
(define zt-peer-had-aggregate-link (c-lambda (zt-peer) bool "___return(___arg1->hadAggregateLink);"))
;; TODO accessors for `ZT_PeerPhysicalPath`
(define zt-peer-n-path (c-lambda (zt-peer size_t) ZT_PeerPhysicalPath  "___return(&___arg1->paths[___arg2]);"))
(define (zt-peer-paths peer)
  (let ((len (zt-peer-path-count peer)) (result '()))
    (do ((i 0 (+ i 1)))
        ((= i len) result)
      (set! result (cons (zt-peer-n-path peer i) result)))))

(define (zt-peerpath-address ppp)
  (let ((sockaddr (make-unspecified-socket-address)))
    ((c-lambda
      (socket-address ZT_PeerPhysicalPath) void #<<END
      struct sockaddr_storage *sa_st = ___arg1;
      sa_st->ss_family = AF_INET6;
      memcpy(sa_st, &___arg2->address, (sizeof(struct sockaddr_storage)));
END
) sockaddr ppp)
    sockaddr))

(define zt-peer-info->vector
  (let ((all (vector
              zt-peer-address
              zt-peer-version
              zt-peer-latency
              zt-peer-role
              zt-peer-path-count
              zt-peer-had-aggregate-link
              ;;zt-peer-paths
              ;; (lambda (peer) (map zt-peerpath-address (zt-peer-paths peer)))
              (lambda (peer) (map (lambda (p) (socket-address->string (zt-peerpath-address p)))  (zt-peer-paths peer)))
              )))
    (lambda (obj)
      (let* ((len (vector-length all)) (result (make-vector len)))
        (do ((i 0 (+ i 1)))
            ((= i len) result)
          (vector-set! result i ((vector-ref all i) obj)))))))

(define zt-peers-map
  (let ((get (c-lambda (zt-node) zt-peers "___return(ZT_Node_peers(___arg1));"))
        (free (c-lambda (zt-node zt-peers) void "ZT_Node_freeQueryResult(___arg1, (void*)___arg2);"))
        (peer-n (c-lambda (zt-peers size_t) zt-peer "___return(&___arg1->peers[___arg2]);")))
    (lambda (proc)
      (assert-zt-up! zt-peers-map)
      (let* ((node (zt-prm-zt %%zt-prm))
             (all (get node)))
        (with-exception-catcher
         (lambda (ex) (free node all) (raise ex))
         (lambda ()
           (let ((n ((c-lambda (zt-peers) size_t "___return(___arg1->peerCount);") all))
                 (result '()))
             (do ((i 0 (+ i 1)))
                 ((= i n)
                  (free node all)
                  result)
               (set! result (cons (proc (peer-n all i)) result))))))))))

(define (zt-peers-info) (zt-peers-map zt-peer-info->vector))

;;** Config Accessors
(define zt-virtual-config-nwid (c-lambda (zt-virtual-config*) unsigned-int64 "___return(___arg1->nwid);"))
(define zt-virtual-config-mac (c-lambda (zt-virtual-config*) unsigned-int64 "___return(___arg1->mac);"))
(define zt-virtual-config-name (c-lambda (zt-virtual-config*) char-string "___return(___arg1->name);"))
(define (zt-virtual-config-status cfg) ;; EXPORT
  (vector-ref
   '#(REQUESTING_CONFIGURATION OK ACCESS_DENIED NOT_FOUND PORT_ERROR CLIENT_TOO_OLD)
   ((c-lambda (zt-virtual-config*) int "___return(___arg1->status);") cfg)))
(define zt-virtual-config-public (c-lambda (zt-virtual-config*) bool "___return(___arg1->type);"))
(define zt-virtual-config-mtu (c-lambda (zt-virtual-config*) size_t "___return(___arg1->mtu);"))
(define zt-virtual-config-dhcp (c-lambda (zt-virtual-config*) bool "___return(___arg1->dhcp);"))
(define zt-virtual-config-bridge (c-lambda (zt-virtual-config*) bool "___return(___arg1->bridge);"))
(define zt-virtual-config-broadcast (c-lambda (zt-virtual-config*) bool "___return(___arg1->broadcastEnabled);"))
(define zt-virtual-config-porterror (c-lambda (zt-virtual-config*) int "___return(___arg1->portError);"))
(define zt-virtual-config-assigned-address-count
  (c-lambda (zt-virtual-config*) size_t "___return(___arg1->assignedAddressCount);"))
(define zt-virtual-config-route-count
  (c-lambda (zt-virtual-config*) size_t "___return(___arg1->routeCount);"))
(define zt-virtual-config-multicast-subscription-count
  (c-lambda (zt-virtual-config*) size_t "___return(___arg1->multicastSubscriptionCount);"))

(define zt-virtual-config-base->vector
  (let ((all (vector
              zt-virtual-config-nwid
              zt-virtual-config-mac
              zt-virtual-config-name
              zt-virtual-config-status
              zt-virtual-config-public
              zt-virtual-config-mtu
              zt-virtual-config-dhcp
              zt-virtual-config-bridge
              zt-virtual-config-broadcast
              zt-virtual-config-porterror
              zt-virtual-config-assigned-address-count
              zt-virtual-config-route-count
              zt-virtual-config-multicast-subscription-count
              )))
    (lambda (obj)
      (let* ((len (vector-length all)) (result (make-vector len)))
        (do ((i 0 (+ i 1)))
            ((= i len) result)
          (vector-set! result i ((vector-ref all i) obj)))))))

;;** Query
(define (ZT_Node_freeQueryResult cfg) ;; Don't forget to free results!
  ((c-lambda (zt-node void*) void "ZT_Node_freeQueryResult(___arg1, ___arg2);")
   (zt-prm-zt %%zt-prm) cfg))

(define (zt-virtual-config*_release cfg) ;; INTERN Don't forget to free results!
  ((c-lambda (zt-node zt-virtual-config*) void "ZT_Node_freeQueryResult(___arg1, ___arg2);")
   (zt-prm-zt %%zt-prm) cfg))

(define (zt-network-virtual-config* network) ;; INTERN
  (assert-zt-up! zt-VirtualNetworkConfig)
  ((c-lambda (zt-node unsigned-int64) zt-virtual-config* "___return(ZT_Node_networkConfig(___arg1, ___arg2));")
   (zt-prm-zt %%zt-prm) network))

(define (make-zt-network-config-query accessor) ;; INTERN
  (lambda (network)
    (let ((cfg (zt-network-virtual-config* network)))
      (and cfg
           (let ((result (accessor cfg)))
             (zt-virtual-config*_release cfg)
             result)))))

(define zt-query-network-mac (make-zt-network-config-query zt-virtual-config-mac)) ;; EXPORT
(define zt-query-network-name (make-zt-network-config-query zt-virtual-config-name)) ;; EXPORT
(define zt-query-network-status (make-zt-network-config-query zt-virtual-config-status)) ;; EXPORT
(define zt-query-network-public (make-zt-network-config-query zt-virtual-config-public)) ;; EXPORT
(define zt-query-network-mtu (make-zt-network-config-query zt-virtual-config-mtu)) ;; EXPORT
(define zt-query-network-dhcp (make-zt-network-config-query zt-virtual-config-dhcp)) ;; EXPORT
(define zt-query-network-bridge (make-zt-network-config-query zt-virtual-config-bridge)) ;; EXPORT
(define zt-query-network-broadcast (make-zt-network-config-query zt-virtual-config-broadcast)) ;; EXPORT
(define zt-query-network-porterror (make-zt-network-config-query zt-virtual-config-porterror)) ;; EXPORT
(define zt-query-network-assigned-address-count (make-zt-network-config-query zt-virtual-config-assigned-address-count)) ;; EXPORT
(define zt-query-network-route-count (make-zt-network-config-query zt-virtual-config-route-count)) ;; EXPORT
(define zt-query-network-multicast-subscription-count (make-zt-network-config-query zt-virtual-config-multicast-subscription-count)) ;; EXPORT

(define zt-query-network-config-base->vector (make-zt-network-config-query zt-virtual-config-base->vector)) ;; EXPORT

;;* Utilities

(define zt-network-mac->node ;; EXPORT
  (c-lambda
   (unsigned-int64 unsigned-int64) unsigned-int64 #<<END
   uint64_t node, nwid=___arg1, mac=___arg2;
   // This extracts a node address from a mac address.
   node = mac & 0xffffffffffull;
   node ^= ((nwid >> 8) & 0xff) << 32;
   node ^= ((nwid >> 16) & 0xff) << 24;
   node ^= ((nwid >> 24) & 0xff) << 16;
   node ^= ((nwid >> 32) & 0xff) << 8;
   node ^= (nwid >> 40) & 0xff;
   ___return(node);
END
))

(define zt-network+node->mac ;; EXPORT
  (c-lambda
   (unsigned-int64 unsigned-int64) unsigned-int64 #<<END
 uint64_t mac, nwid=___arg1, node=___arg2;
 // We use LSB of network ID, and make sure that we clear
 // multicast and set local administration -- this is the first
 // octet of the 48 bit mac address.  We also avoid 0x52, which
 // is known to be used in KVM, libvirt, etc.
 mac = ((uint8_t)(nwid & 0xfe) | 0x02);
 if (mac == 0x52) {
  mac = 0x32;
 }
 mac <<= 40;
 mac |= node;
 // The rest of the network ID is XOR'd in, in reverse byte
 // order.
 mac ^= ((nwid >> 8) & 0xff) << 32;
 mac ^= ((nwid >> 16) & 0xff) << 24;
 mac ^= ((nwid >> 24) & 0xff) << 16;
 mac ^= ((nwid >> 32) & 0xff) << 8;
 mac ^= (nwid >> 40) & 0xff;
 ___return(mac);
END
))

(c-declare #<<c-declare-end

static inline uint64_t mac_from_vector(const void *src) // maybe better hwaddr instead of void
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

static inline uint64_t g_zt_mac_hton(uint64_t mac)
{
 uint64_t result = 0;
 unsigned char *b = (unsigned char *)&result;
 b[0] = (unsigned char)((mac >> 40) & 0xff);
 b[1] = (unsigned char)((mac >> 32) & 0xff);
 b[2] = (unsigned char)((mac >> 24) & 0xff);
 b[3] = (unsigned char)((mac >> 16) & 0xff);
 b[4] = (unsigned char)((mac >> 8) & 0xff);
 b[5] = (unsigned char)(mac & 0xff);
 return result;
}

c-declare-end
)

(define (->zt-mac x) ;; return a ZT uint64_t MAC encoding
  (cond
   ((u8vector? x)
    ((c-lambda
      (scheme-object) unsigned-int64
      "___return(mac_from_vector(___CAST(void *,___BODY_AS(___arg1,___tSUBTYPED))));")
     x))
   (else (error "->zt-mac illegal argument" x))))

(define (zt-mac->network x) ;; MAC encoding in network byte order (big endian)
  (cond
   ((fixnum? x)
    ((c-lambda
      (unsigned-int64) unsigned-int64
      "___return(g_zt_mac_hton(___arg1));")
     x))
   (else (error "zt-mac->network illegal argument" x))))

(c-declare #<<c-declare-end

void set_6plane_addr(struct sockaddr_in6 *sin6, uint64_t nwid, uint64_t zeroTierAddress, uint16_t port)
{
  nwid ^= (nwid >> 32);
  struct in6_addr *buf=&sin6->sin6_addr;
  //memset(buf, 0, sizeof(struct in6_addr));
  buf->s6_addr[0] = 0xfc;
  buf->s6_addr[1] = (uint8_t)(0xff&(nwid >> 24));
  buf->s6_addr[2] = (uint8_t)(0xff&(nwid >> 16));
  buf->s6_addr[3] = (uint8_t)(0xff&(nwid >> 8));
  buf->s6_addr[4] = (uint8_t)(0xff&nwid);
  buf->s6_addr[5] = (uint8_t)(0xff&(zeroTierAddress >> 32));
  buf->s6_addr[6] = (uint8_t)(0xff&(zeroTierAddress >> 24));
  buf->s6_addr[7] = (uint8_t)(0xff&(zeroTierAddress >> 16));
  buf->s6_addr[8] = (uint8_t)(0xff&(zeroTierAddress >> 8));
  buf->s6_addr[9] = (uint8_t)(0xff&zeroTierAddress);
  buf->s6_addr[10] = 0;
  buf->s6_addr[11] = 0;
  buf->s6_addr[12] = 0;
  buf->s6_addr[13] = 0;
  buf->s6_addr[14] = 0;
  buf->s6_addr[15] = 0x01;
  //sin6->sin6_len = sizeof(struct zts_sockaddr_in6);
  sin6->sin6_family = AF_INET6;
  sin6->sin6_port = htons(port);
  /*
  { int i;
    printf("Addr: ");
    for(i=0;i<14;i+=2) {
      printf("%02x%02x:", buf->s6_addr[i], buf->s6_addr[i+1]);
    }
    printf("%02x%02x\n", buf->s6_addr[i], buf->s6_addr[i+1]);
  }
  //*/
}
c-declare-end
)

;; (define p6 (make-6plane-addr #xff1d131d13000000 #x57707f31b6 7443))
;; (socket-address->internet6-address p6)

(define make-6plane-addr
  (let ((set-6plane-addr!
         (c-lambda
          (socket-address unsigned-int64 unsigned-int64 unsigned-int)
          void
          "set_6plane_addr((struct sockaddr_in6 *) ___arg1, ___arg2, ___arg3, ___arg4);")))
    (lambda (nwid node port)
      (let ((sa (internet6-address->socket-address
                 '#u8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                 port 0 0)))
        (set-6plane-addr! sa nwid node port)
        sa))))

(define (zt-adhoc-network-id start #!optional (end start))
  ((c-lambda
    (unsigned-int unsigned-int) unsigned-int64
    "uint64_t r = 0xff00000000000000, s=(uint16_t)___arg1, e=(uint16_t)___arg2; ___return(r | (s << 40) | (e << 24));")
   start end))

(define (zt-state-file-generator file-pattern base)
  (define (state-file objtype objid)
    (and base ((vector-ref file-pattern objtype) base objid)))
  (let loop ((p base) (sl (string-length base)))
    (if (or (eqv? (string-ref p (- sl 1)) #\/) (eqv? sl 0))
        (if (and (file-exists? p) (eq? (file-info-type (file-info p)) 'directory))
            (set! base p)
            (set base #f))
        (loop (string-append base "/") (+ sl 1))))
  state-file)

(let ((orig zt-state-file-generator))
  (define (hexstr id digits)
    (let ((effective (number->string id 16)))
      (string-append (make-string (- digits (string-length effective)) #\0) effective)))

  (define file-pattern-orig
    (vector
     #f ;; _NULL
     (lambda (home id) (string-append home "identity.public"))
     (lambda (home id) (string-append home "identity.secret"))
     (lambda (home id) (string-append home "planet"))
     (lambda (home id) (string-append home "moons.d/" (hexstr id 16) ".moon"))
     (lambda (home id) (string-append home "peers.d/" (hexstr id 10) ".peer"))
     (lambda (home id) (string-append home "networks.d/" (hexstr id 16) ".conf"))
     (lambda (home id) (string-append home "networks.d/" (hexstr id 16)".local.conf"))
     ))

  (define file-pattern-nng  ;; FIXME, wrong!
    (vector
     #f ;; _NULL
     (lambda (home id) (string-append home "identity.public"))
     (lambda (home id) (string-append home "identity.secret"))
     (lambda (home id) (string-append home "planet"))
     (lambda (home id) (string-append home "moon." (hexstr id 16) ))
     (lambda (home id) (string-append home "peers." (hexstr id 10)))
     (lambda (home id) (string-append home "networks.d/" (hexstr id 16) ".conf"))
     (lambda (home id) (string-append home "networks.d/" (hexstr id 16)".local.conf"))
     ))

  (define (state-file-generator file-pattern base)
    (cond
     ((eqv? file-pattern 0) (orig file-pattern-orig base))
     ((eqv? file-pattern 1) (orig file-pattern-nng base))
     (else (orig file-pattern base))))

  (set! zt-state-file-generator state-file-generator))

(define (zt-make-default-state-handlers zt-state-file)
  (define (pickup from)
    (and (file-exists? from)
         (let* ((size (file-size from))
                (data (make-u8vector size)))
           (call-with-input-file from (lambda (p) (read-subu8vector data 0 size p)))
           data)))
  (define (get objtype objid)
    (let ((from (zt-state-file objtype objid)))
      (and from (pickup from))))
  (define (put objtype objid data)
    (let* ((into (zt-state-file objtype objid))
           (was (and into (pickup into))))
      (if (and into (not (equal? was data)))
          (call-with-output-file into (lambda (p) #f (write-subu8vector data 0 (u8vector-length data) p))))))
  (values get put))

(include "zerotiercore-extensions.scm")
