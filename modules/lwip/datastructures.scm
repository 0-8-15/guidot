;; https://www.codeproject.com/articles/4804/basic-concepts-on-endianness

;;;* U8Vector Network Encoded

;;;** Local Syntax

(define-macro (nw-vector-range-assert proc vec offset size)
  `(unless (>= (u8vector-length ,vec) (+ ,offset ,(quotient size 8)))
           #;(error  "out of range" ',proc (u8vector-length ,vec) ,offset ,size)
           (##raise-range-exception 2 ',proc (u8vector-length ,vec) ,offset ,size)))

(define-macro (nw-vector-range-assert-1 proc vec offset size)
  `(unless (>= (u8vector-length ,vec) (+ ,offset ,size))
           #;(error  "out of range" ',proc (u8vector-length ,vec) ,offset ,size)
           (##raise-range-exception 2 ',proc (u8vector-length ,vec) ,offset ,size)))

(define-macro (nw-vector-range+pos-assert proc pos vec offset size)
  `(unless (>= (u8vector-length ,vec) (+ ,offset ,size))
           #;(error  "out of range" ',proc ,pos (u8vector-length ,vec) ,offset ,size)
           (##raise-range-exception ,pos ',proc (u8vector-length ,vec) ,offset ,size)))

;;;*** Compile Time Options

(cond-expand
 ((and gambit unsafe)

  (define-macro (%u8v-ref-code vec off size conv)
    (let ((big (if (>= size 32) "BIG" ""))
          (result-c-type (string-append "uint" (number->string size) "_t")))
      `(##c-code
        ,(string-append
          result-c-type " tmp = * ___CAST("
          result-c-type "*, ___CAST(uint8_t*, ___BODY_AS(___ARG1,___tSUBTYPED)) + ___ARG2);"
          "___RESULT=___" big "FIX(" conv "(tmp));")
        ,vec ,off)))

  (define-macro (%u8v-ref size conv)
    ;; ##c-code has no conversion overhead
    `(lambda (vec off)
       (%%u8v-ref-code vec off ,size ,conv)))

  (define-macro (define-u8v-ref name size conv)
    `(define (,name vec off)
       (nw-vector-range-assert ,name vec off ,size)
       (%%u8v-ref-code vec off ,size ,conf)))

  ) (gambit ;; gambit NOT unsafe

     (define-macro (%u8v-ref size conv)
       (let ((size_str (number->string size)))
         `(c-lambda
           (scheme-object size_t) ,(string->symbol (string-append "unsigned-int" size_str))
           ,(string-append
             "const char *cptr = ___CAST(uint8_t*, ___BODY(___arg1));
uint" size_str "_t result, val = *(uint" size_str "_t*)(cptr+___arg2);
result = " conv "(val); // TODO just inline the expression
___return(result);"))))

     (define-macro (define-u8v-ref name size conv)
       `(define (,name vec off)
          (nw-vector-range-assert ,name vec off ,size)
          ((%u8v-ref ,size ,conv) vec off)))

     )
    (else NYI -- anything but gambit not yet implemented))

(define-macro (%u8vn-setter size conv)
  (let* ((size_str (number->string size))
         (result-c-type (string-append "uint" size_str "_t")))
    `(c-lambda
      (scheme-object size_t ,(string->symbol (string-append "unsigned-int" size_str))) void
      ,(string-append
       "const char *cptr = ___CAST(uint8_t*, ___BODY(___arg1));
" result-c-type " val = " conv "(___arg3); // TODO just inline the expression
*(" result-c-type "*)(cptr+___arg2) = val;"))))

#;(define-macro (%%u8vn-setter-code vec off val size conv)
  ;;; WRONG
  (if (>= size 32)
      `(##c-code
        ,(string-append
          "* ___CAST(uint"
          (number->string size)
          "_t*, ___CAST(uint8_t*, ___BODY_AS(___ARG1,___tSUBTYPED)) + ___ARG2) = "
          conv "(___BIGNUMP(___ARG3) ?  ___BIGAFETCH(___ARG3, 1)<<32|___BIGAFETCH(0, ___ARG3) : ___INT(___ARG3));"
          "___RESULT=___VOID;")
        ,vec ,off ,val)
      `(##c-code
        ,(string-append
          "* ___CAST(uint"
          (number->string size)
          "_t*, ___CAST(uint8_t*, ___BODY_AS(___ARG1,___tSUBTYPED)) + ___ARG2) = "
          conv "(___BIGNUMP(___ARG3) ? ___BIGAFETCH(___ARG3, 0) : ___INT(___ARG3)); ___RESULT=___VOID;")
        ,vec ,off ,val)))

(define-macro (%%u8vn-setter size conv)
  `(lambda (vec off val) (%%u8vn-setter-code vec off val ,size ,conv)))

;;;* Generic Packets (u8vector)

(define %u8vector/n16h-ref (%u8v-ref 16 "PP_NTOHS"))

(define-u8v-ref u8vector/n16h-ref 16 "PP_NTOHS")

(define %u8vector/n16h-set! (%u8vn-setter 16 "PP_HTONS"))

(define (u8vector/n16h-set! vec n v)
  (nw-vector-range-assert u8vector-set/n16! vec n 16)
  (%u8vector/n16h-set! vec n v))

(define %u8vector/n32h-ref (%u8v-ref 32 "PP_NTOHL"))

(define-u8v-ref u8vector/n32h-ref 32 "PP_NTOHL")

(define %u8vector/n32h-set! (%u8vn-setter 32 "PP_HTONL"))

(define (u8vector/n32h-set! vec n v)
  (nw-vector-range-assert u8vector/n32h-set! vec n 32)
  (%u8vector/n32h-set! vec n v))

(c-declare "static inline uint64_t lwip_ntohll(uint64_t x)
{ return (((uint64_t)(PP_NTOHL((uint32_t)((x << 32) >> 32))) << 32)
   | (uint32_t)PP_NTOHL(((uint32_t)(x >> 32)))) ;}")

(define %u8vector/n64h-ref (%u8v-ref 64 "lwip_ntohll"))

(define-u8v-ref u8vector/n64h-ref 64 "lwip_ntohll")

(define %u8vector/n64h-set! (%u8vn-setter 64 "lwip_ntohll"))

(define (u8vector/n64h-set! vec n v)
  (nw-vector-range-assert u8vector/n64h-set! vec n 64)
  (%u8vector/n64h-set! vec n v))

(define %u8vector/n48h-ref
  (c-lambda
   (scheme-object size_t) unsigned-int64
   "___return(mac_from_u8v6( ___CAST(const uint8_t*, ___BODY(___arg1)) + ___arg2 ));"))

(define (u8vector/n48h-ref vec n)
  (nw-vector-range-assert u8vector/n48h-ref vec n 48)
  (%u8vector/n48h-ref vec n))

(define %u8vector/n48h-set!
  (c-lambda
   (scheme-object size_t unsigned-int64) void
   "g_lwip_set_mac_ui64h( ___CAST(uint8_t*, ___BODY(___arg1)) + ___arg2, ___arg3 );"))

(define (u8vector/n48h-set! vec n mac)
  (nw-vector-range-assert u8vector/n48h-set! vec n 48)
  (%u8vector/n48h-set! vec n mac))

(define (u8vector-copy-from-string! to off from #!optional (start 0) (end (string-length from)))
  (let ((len (- end start)))
    (nw-vector-range-assert-1 u8vector-copy-from-string! to off len)
    (do ((off off (+ off 1)) (n start (+ n 1)))
        ((= n len))
      (u8vector-set! to off (char->integer (string-ref from n))))))

(define (u8vector-copy-from-u8vector! to off from #!optional (start 0) (end (u8vector-length from)))
  (subu8vector-move! from start end to off))

;;;* Ethernet
(define-c-constant SIZEOF_ETH_HDR size_t "SIZEOF_ETH_HDR")

(define ethernet-header?type/get ;; Horror: this naming convention hints to "dunno"!
  (c-lambda
   (scheme-object size_t) unsigned-int16
   "___return( ___CAST(struct eth_hdr *, ___CAST(char*, ___BODY(___arg1)) + ___arg2) -> type);"))

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
(define ETHTYPE_NNG #x0901) ;; nanomsg

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

(define (ethertype/host-decor etht)
  (cond
   ((<= etht 1500) (list 'length etht))
   ((and (> etht 1500) (< etht 1536)) (list 'illegal-ethertype etht))
   (else
    (let ((x (ethertype/host->symbol etht)))
      (if (eq? x 'lwip-UNKNOWN) (list x etht (ethertype/network->symbol etht)) x)))))

(define (display-eth-packet/offset u8 offset #!optional (port (current-output-port)))
  #;(define type (ethertype/network->symbol (ethernet-header?type/get u8 offset)))
  (define type (ethertype/host-decor (lwip-ntohs (ethernet-header?type/get u8 offset))))
  (display type port)
  (display " from: " port)
  (display (lwip-u8-mac->string u8 (+ 6 offset)) port)
  (display " to: " port)
  (display (lwip-u8-mac->string u8 offset) port)
  (newline port)
  (case type
    ((ETHTYPE_IPV6) (display-ip6-packet/offset u8 (+ offset SIZEOF_ETH_HDR) port)))
  u8)

;;;* IP

;;;** IPv6
(define-c-constant SIZEOF_IP6_HDR size_t "sizeof(struct ip6_hdr);")

(define-macro (define-u8-ip6header-get var type access size)
  (let ((u8 (gensym 'u8))
        (c-struct "struct ip6_hdr")
        (offset (gensym 'offset)))
    `(define ,var
       (lambda (,u8 ,offset)
         (if (>= (+ ,offset ,size) (u8vector-length ,u8)) (error "out of range" ',var)) ;; FIXME: offset+off_in_struct
         ((c-lambda
           (scheme-object size_t) ,type
           ,(string-append
             "___return(" access "(___CAST( " c-struct " *, ___CAST(char*, ___BODY(___arg1)) + ___arg2) ));"))
          ,u8 ,offset)))))

(define-u8-ip6header-get IP6H_V unsigned-int8 "IP6H_V" 4)
(define-u8-ip6header-get IP6H_TC unsigned-int8 "IP6H_TC" 4)
(define-u8-ip6header-get IP6H_FL unsigned-int8 "IP6H_FL" 4)
(define-u8-ip6header-get IP6H_PLEN unsigned-int16 "IP6H_PLEN" 6)
(define-u8-ip6header-get IP6H_NEXTH unsigned-int8 "IP6H_NEXTH" 7)
(define-u8-ip6header-get IP6H_HOPLIM unsigned-int8 "IP6H_HOPLIM" 8)

(c-declare "static inline char * local_ip6addr_ntoa(const ip6_addr_p_t *addr) {
 static char buf[80];
 ip6_addr_t ua; ip6_addr_copy_from_packed(ua, *addr);
 ip6addr_ntoa_r(&ua, buf, 80);
 return buf;
}")

(define (ip6addr->string u8 #!optional (offset 0))
  (nw-vector-range+pos-assert ip6addr->string 1 u8 offset 16)
  ((c-lambda (scheme-object) char-string "___return(local_ip6addr_ntoa(___BODY(___arg1)));") u8))

(c-declare "static inline char * local_IP6H_SRC_s(const struct ip6_hdr *hdr)
{return local_ip6addr_ntoa(&hdr->src);}")

(c-declare "static inline char * local_IP6H_DST_s(const struct ip6_hdr *hdr)
{return local_ip6addr_ntoa(&hdr->dest);}")

(define-u8-ip6header-get IP6H_SRC->string char-string "local_IP6H_SRC_s" 16)
(define-u8-ip6header-get IP6H_DST->string char-string "local_IP6H_DST_s" 16)

(define (ip-protocol-number->symbol nr)
  ;; TODO: get it from /etc/protocols source
  (case nr
    ((0) 'IP)
    ((1) 'ICMP)
    ((2) 'IGMP)
    ((3) 'GGP)
    ((4) 'IP-ENCAP)
    ((5) 'ST)
    ((6) 'TCP)
    ((17) 'UDP)
    ((41) 'IPv6)
    ((43) 'IPv6-Route)
    ((44) 'IPv6-Frag)
    ((58) 'IPv6-ICMP)
    ((59) 'IPv6-NoNxt)
    ((60) 'IPv6-Opts)
    ((139) 'HIP)
    ((140) 'Shim6)
    (else (list 'ip-protocol-number-UNKNOWN nr))))

(define (ip6-extension->symbol nr)
  ;; TODO: get it from /etc/protocols source
  (case nr
    ((0) 'IPv6e-HopByHop)
    ((59) 'IPv6-NoNxt)
    ((60) 'IPv6e-DestinationOptions)
    (else (ip-protocol-number->symbol nr))))

(define (IP6H_NEXTH->sym u8 o)
  (ip6-extension->symbol (IP6H_NEXTH u8 o)))

(define (display-ip6-packet/offset u8 offset #!optional (port (current-output-port)))
  (define (show l a)
    (display l port)
    (display (a u8 offset) port))
  (show "IPv6 V: " IP6H_V)
  (show " class: " IP6H_TC)
  (show " flow: " IP6H_FL)
  (show " len: " IP6H_PLEN)
  (show " next header: " (if #t IP6H_NEXTH->sym IP6H_NEXTH))
  (show " hop limit: " IP6H_HOPLIM)
  (show " src: " IP6H_SRC->string)
  (show " dst: " IP6H_DST->string)
  (newline port)
  (case (IP6H_NEXTH->sym u8 offset)
    ((IPv6-ICMP) (display-icmp6-packet/offset u8 (+ offset SIZEOF_IP6_HDR) port))
    ((TCP) (display-tcp-heaper/offset u8 (+ offset SIZEOF_IP6_HDR) port)))
  u8)

#;(define ip6-header?type/get ;; Horror: this naming convention hints to "dunno"!
  (c-lambda
   (scheme-object size_t) unsigned-int16
   "___return( ___CAST(struct ip6_hdr *, ___CAST(char*, ___BODY(___arg1)) + ___arg2) -> type);"))

;;;** ICMP6

(define (icmp6-type->string x)
  (case x
    ((1) "DUR destination unreachable")
    ((2) "PTB packet too big")
    ((3) "TE time exceeded")
    ((4) "PP parameter problem")
    ((100) "PE1 Private experimentation 100")
    ((101) "PE2 Private experimentation 101")
    ((127) "RSV_ERR reserved")
    ((128) "EREQ Echo request")
    ((129) "EREP Echo reply")
    ((130) "MLQ Multicast listener query")
    ((131) "MLR Multicast listener report")
    ((132) "MLD Multicast listener done")
    ((133) "RS Router solicitation")
    ((134) "RA Router advertisement")
    ((135) "NS Neighbor solicitation")
    ((136) "NA Neighbor advertisement")
    ((137) "RD Redirect")
    ((151) "MRA Multicast router advertisement")
    ((152) "MRS Multicast router solicitation")
    ((153) "MRT Multicast router termination")
    ((200) "PE3 Private experimentation")
    ((201) "PE4 Private experimentation")
    ((255) "RSV_INF Reserved for expansion of informational messages")
    (else "ICMP6 UNKNOWN value")))

(c-define-type imcp6_hdr (nonnull-pointer (struct "icmp6_echo_hdr")))

(define-c-constant SIZEOF_ICMP6_HDR size_t "sizeof(struct icmp6_hdr);")
(define-c-constant SIZEOF_ICMP6ECHO_HDR size_t "sizeof(struct icmp6_echo_hdr);")

(define icmp6-type* (c-lambda (imcp6_hdr) unsigned-int8 "___return(___arg1->type);"))

(define (icmp6-type x #!optional (offset 0))
  ;; FIXME: incomplete AND wrong
  (cond
   ((u8vector? x) (u8vector-ref x offset))
   (else (icmp6-type* x))))

(define (display-icmp6-packet/offset u8 offset #!optional (port (current-output-port)))
  (display (icmp6-type->string (icmp6-type u8 offset)) port)
  (newline port)
  u8)

(define (make-icmp6-echo-request from to)
  (let ((proc icmp6-echo-request)
        (ip6addr-len 16)
        (p (make-u8vector (+ SIZEOF_IP6_HDR SIZEOF_ICMP6_HDR)))
        (o 0))
    (nw-vector-range+pos-assert proc 1 from 0 ip6addr-len)
    (nw-vector-range+pos-assert proc 2 to 0 ip6addr-len)
    (u8vector-set! p (+ o 0) #x60)    ;; IPv6 class 0 upper bit
    ;; left as zeros
    (u8vector/n16h-set! p (+ o 4) SIZEOF_ICMP6_HDR) ;; Payload
    (u8vector-set! p (+ o 6) 58)      ;; IPv6-ICMP
    (u8vector-set! p (+ o 7) 255)     ;; hop limit
    (subu8vector-move! from 0 ip6addr-len p (+ o 8))
    (subu8vector-move! to 0 ip6addr-len p (+ o 24))
    ;; ICMP6 Ping
    (u8vector-set! p (+ o SIZEOF_IP6_HDR) 128) ;; type EREQ
    p))

;;;** TCP

(c-declare "#include \"lwip/prot/tcp.h\"")

(define-c-constant SIZEOF_TCP_HDR size_t "sizeof(struct tcp_hdr);")

(define-macro (define-u8-header-get var type hdr access size)
  (let ((u8 (gensym 'u8))
        (offset (gensym 'offset)))
    `(define ,var
       (lambda (,u8 ,offset)
         (if (>= (+ ,offset ,size) (u8vector-length ,u8)) (error "out of range" ',var)) ;; FIXME: offset+off_in_struct
         ((c-lambda
           (scheme-object size_t) ,type
           ,(string-append
             "___return(" access "(___CAST( struct " hdr " *, ___CAST(char*, ___BODY(___arg1)) + ___arg2) ));"))
          ,u8 ,offset)))))

(define-u8-header-get TCPH_HDRLEN unsigned-int16 "tcp_hdr" "TCPH_HDRLEN" 2)
(define-u8-header-get TCPH_FLAGS unsigned-int8 "tcp_hdr" "TCPH_FLAGS" 1)

(define (tcp-flag->symbol idx)
  (case idx
    ((#x01) 'FIN)
    ((#x02) 'SYN)
    ((#x04) 'RST)
    ((#x08) 'PSH)
    ((#x10) 'ACK)
    ((#x20) 'URG)
    ((#x40) 'ECE)
    ((#x80) 'CWR)))

(define (display-tcp-flags flags #!optional (port (current-output-port)))
  (let ((following #f))
    (define (df n nm)
      (when (eq? (bitwise-and flags n) n)
            (if following (display "|" port) (set! following #t))
            (display (tcp-flag->symbol n) port)))
    (df #x01 "FIN")
    (df #x02 "SYN")
    (df #x04 "RST")
    (df #x08 "PSH")
    (df #x10 "ACK")
    (df #x20 "URG")
    (df #x40 "ECE")
    (df #x80 "CWR")))

(define (display-tcp-heaper/offset u8 offset #!optional (port (current-output-port)))
  (display "TCP src " port)
  (display (u8vector/n16h-ref u8 offset) port)
  (display " dst " port)
  (display (u8vector/n16h-ref u8 (+ offset 2)) port)
  (display " seqno " port)
  (display (u8vector/n32h-ref u8 (+ offset 4)) port)
  (display " ackno " port)
  (display (u8vector/n32h-ref u8 (+ offset 8)) port)
  (display " " port)
  (display-tcp-flags (TCPH_FLAGS u8 offset) port)
  (display " header length: " port)
  (display (TCPH_HDRLEN u8 offset) port)
  (newline port))
