(include "~~tgt/lib/onetierzero/src/observable-notational-conventions.scm")

(define http-proxy-on-illegal-proxy-request
  (let ((handler (lambda (line)
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
 </body>
</html>

EOF
)
                 )))
    (case-lambda
     (() handler)
     ((proc) (if (procedure? proc) (set! handler proc))))))

(define httpproxy-connect-set!)

(define make-httpproxy
  (let ((max-line-length 1024)
        (http-proxy-connect-line #f)
        (http-proxy-request-line #f)
        (https-regex (rx "^https"))
        (connect-handler (lambda (tag host port) (display "NOT Initialized: httproxy connect"))))
    (define (init!)
      (set! http-proxy-connect-line
            (rx "^CONNECT (\\[(?:[^]]+])|(?:[^:/]+))(?:(?:[:])([0-9]+))? (HTTP/[0-9]\\.[0-9])\r?$"))
      (set! http-proxy-request-line
            (rx "^([^ ]+) (http://)?(\\[(?:[^]]+])|(?:[^:/]+))(?:(?:[:])([^/]+))?([^ ]+) (HTTP/[0-9]\\.[0-9])\r?$")))
    (define (clean-ip6addr host)
      (if (eqv? (string-ref host 0) #\[) (substring host 1 (fx- (string-length host) 1)) host))
    (define (ingore-headers! port)
      (let ((line (u8-read-line2 port 10 max-line-length)))
        (or (equal? line "") (equal? line "\r") (ingore-headers! port))))
    (define (connect host port proto)
      (println port: (current-error-port) "CONNECT " host " " port)
      (ingore-headers! (current-input-port))
      (let ((conn (with-exception-catcher
                   (lambda (exn) #f)
                   (lambda () (connect-handler "HTTP" (clean-ip6addr host) port)))))
        (if (port? conn)
            (begin
              (display proto)
              (display " 200 OK\r\n\r\n")
              (force-output)
              (ports-connect! conn conn (current-input-port) (current-output-port) 3))
            (begin
              (display proto)
              (display " 502 Bad Gateway\r\n\r\n")))))
    (define (forward host port cmd scheme path proto)
      (let ((nl1 (string-append cmd " " path " " proto "\r\n"))
            (port (or (and port (string->number port)) (if (rx~ https-regex scheme) 443 80))))
        (println port: (current-error-port) "HTTP FORWARD "
                 host " " port " "  scheme " : " nl1)
        (let ((conn (with-exception-catcher
                     handle-replloop-exception
                     (lambda () (connect-handler "HTTP" (clean-ip6addr host) port)))))
          (when (port? conn)
            (display nl1 conn)
            (force-output conn)
            (ports-connect! conn conn (current-input-port) (current-output-port) 3)))))
    (set! httpproxy-connect-set! (lambda (v) (set! connect-handler v)))
    (lambda (#!optional (illegal-proxy-request #f))
      (lambda ()
        (unless http-proxy-connect-line (init!))
        (let* ((ln1 (u8-read-line2 (current-input-port) 10 max-line-length))
               (m (with-exception-catcher
                   handle-replloop-exception
                   (lambda () (rx~ http-proxy-connect-line ln1)))))
          (if m
              (connect (rxm-ref m 1) (string->number (rxm-ref m 2)) (rxm-ref m 3))
              (let ((m (rx~ http-proxy-request-line ln1)))
                (if m
                    (forward (rxm-ref m 3) (rxm-ref m 4)
                             (rxm-ref m 1) (rxm-ref m 2) (rxm-ref m 5) (rxm-ref m 6))
                    ((or illegal-proxy-request (http-proxy-on-illegal-proxy-request))
                     ln1)))))))))

(define http-proxy (make-httpproxy (lambda (line) ((http-proxy-on-illegal-proxy-request) line))))

