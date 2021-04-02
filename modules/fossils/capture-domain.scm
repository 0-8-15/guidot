;; (C) 2020 JFW
;;
;; FIXME: This is a really bad hack

(define beaver-captured-domain (make-pin initial: #f pred: (lambda (x) (or (not x) (string? x)))))

(http-proxy-on-illegal-proxy-request
 (lambda (line)
   (let ((domain (or (beaver-captured-domain) "")))
     (display #<<EOF
HTTP/1.0 200 OK
Content-type: text/html; charset=utf-8

<html>
 <head>
  <title>ERROR: This is a proxy.</title>
 </head>
 <body>
  <h1>Error</h1>
  <p>This is a HTTP/HTTPS proxy.</p>
  <p>This page may one day redirect to <a href=
EOF
)
     (write (string-append "http://" domain))
     (display "\">")
     (display domain)
     (display #<<EOF
</a> .</p>
  <p>Note that his link will <strong>NOT</strong> work if your browser does not use the proxy.</p>
 </body>
</html>

EOF
))))

(define (capture-domain!
         domain-name #!key
         (handler #f)
         (at-phone-decoder (lambda (x) #f))
         (network-id #f))

  ;; Connect to this domain and get the page below back from any port.
  (define domain-rx
    #;(convert-domain-name-to-regex domain-name)
    (begin
      (beaver-captured-domain domain-name)
      ;; Bad KLUDGE, better use SRE
      (rx (string-append "(?:([^.]+)\\.)?" (rx-replace/all (rx "\\.") domain-name "\\.")))))

  (define (intercept? addr)
    (and (string? addr)
         (let ((m (rx~/anchored domain-rx addr)))
           (and m (or (rxm-ref m 1) #t)))))

  (define (display-page)
    (display page-header)
    (display page-body)
    (display
     (string-append
      " <ul>
 <li><a href=\"http://beaver-dam." domain-name "/ot0/uv\">Download</a></li>
 <li><a href=\"http://beaver-dam." domain-name "/jfw/rptview?rn=1\">Issues</a></li>
 </ul>
"))
    (display page-footer))

  (define page-header
;;; The reply we echo.  HTTP+HTML -- just in case.
#<<end-of-page-header
HTTP/1.0 200 OK
Content-type: text/html; charset=utf-8

<html>
 <head>
  <title>Biberburg@beaver.dam</title>
 </head>
 <body>
end-of-page-header
)
  (define page-footer
;;; The reply we echo.  A HTTP header just in case.
#<<end-of-page-footer
 </body>
</html>

end-of-page-footer
)

  (define page-body
;;; Content Here
#<<end-of-page-body
<h1>Biberburg</h1>
<p><small>Note that links above require proxy support as well.</small></p>
end-of-page-body
)

  ;; Boilerplate: Setup, hook in and test.

  (define (producer->pipe producer)
    (receive (servant client) (open-u8vector-pipe)
      (parameterize ((current-output-port servant))
        (producer)
        (close-port servant))
      client))

  (define (replacement-connect-procedure name original)
    (define (replacement key addr port)
      (define auth (eq? key 'host))
      (cond
       ((number? addr)
        (if (equal? addr (beaver-local-unit-id))
            (if handler ((handler) auth) (producer->pipe display-page))
            (let ((p6 (and network-id (make-6plane-addr network-id addr))))
              (and p6 (open-lwip-tcp-client-connection p6 port)))))
       ((equal? addr '#u8(127 0 0 1))
        (open-tcp-client `(address: ,addr port-number: ,port)))
       ((not (string? addr)) (original key addr port))
       ((at-phone-decoder addr) => (lambda (num) (replacement key num port)))
       ((intercept? addr) =>
        (lambda (subdom)
          (cond
           ((and (string? subdom) (at-phone-decoder subdom))
            => (lambda (id) (replacement key id port)))
           ((member subdom '("beaver" "download" "beaver-dam"))
            (replacement key 281406011893 port))
           (else
            (if handler ((handler) auth) (producer->pipe display-page))))))
       ((looks-like-ot0-ad-hoc? addr)
        (let ((ipaddr (lwip-string->ip6-address addr)))
          (and ipaddr (open-lwip-tcp-client-connection ipaddr port))))
       (else (original key addr port))))
    replacement)

  (on-socks-connect (replacement-connect-procedure 'socks-connect-capture (on-socks-connect)))

  (on-ot0cli-connect (replacement-connect-procedure 'cli-connect-capture (on-ot0cli-connect)))

  (%%fossils%at-phone-decoder at-phone-decoder)

  #f
  ;;
  )
