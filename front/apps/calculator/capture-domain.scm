(http-proxy-on-illegal-proxy-request
 (lambda (line)
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
  <p>Look at <a href="http://beaver.dam">beaver.dam</a> instead!</p>
  <p></p>
 </body>
</html>

EOF
)))

(define (capture-domain! domain-name)

  ;; Connect to this domain and get the page below back from any port.
  (define domain-rx
    #;(convert-domain-name-to-regex domain-name)
    (rx "(?:[^.]+\\.)?beaver\\.dam"))

  (define (intercept? addr) (and (string? addr) (rx~/anchored domain-rx addr)))

  (define (display-page)
    (display page-header)
    (display page-body)
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
 <ul>
 <li><a href="http://[fc00:0:ff41:851a:f9f5::1]/ot0/uv">Download</a></li>
 <li><a href="http://[fc00:0:ff41:851a:f9f5::1]/jfw/rptview?rn=1">Issues</a></li>
 </ul>
end-of-page-body
)

  ;; Boilerplate: Setup, hook in and test.

  (define (proceducer->pipe producer)
    (receive (servant client) (open-u8vector-pipe)
      (parameterize ((current-output-port servant))
        (producer)
        (close-port servant))
      client))

  (define (replacement-connect-procedure original)
    (define (replacement key addr port)
      (cond
       ((not (string? addr)) (original key addr port))
       ((intercept? addr)
        ;; displays the portal only
        (proceducer->pipe display-page))
       ((looks-like-ot0-ad-hoc? addr)
        (let ((ipaddr (lwip-string->ip6-address addr)))
          (and ipaddr (open-lwip-tcp-client-connection ipaddr port))))
       (else (original key addr port))))
    replacement)

  (on-socks-connect (replacement-connect-procedure (on-socks-connect)))

  (on-ot0cli-connect (replacement-connect-procedure (on-ot0cli-connect)))

  #f
  ;;
  )
