
(define (fossils-networks-present? networks)
  (pair? networks))

(define (fossils-fallback-name)
  (chat-number->neatstring (chat-own-address) "-"))

(define-pin fossils-directory
  initial: #f
  pred: (lambda (v) (or (not v) (string? v)))
  filter: (lambda (old new) (if old old new)) ;; once only
  name: "projects directory")

(define (fossils-directory-location dir)
  (cond-expand
   (android
    (make-pathname (system-directory) dir))
   (else dir)))

(define (fossils-project-filename project)
  (let ((dir (fossils-directory)))
    (and dir (make-pathname dir project "fossil"))))

(wire!
 (list fossils-directory chat-own-address)
 sequence:
 (lambda (oldd newd oa na)
   (when new
     (unless (file-exists? newd)
       (create-directory newd))
     (let* ((fallback-name (and (chat-own-address) (fossils-fallback-name)))
            (fallback (and fallback-name (fossils-project-filename fallback-name))))
       (when (and fallback (not (file-exists? fallback)))
         ;; clone default
         (log-status "cloning default fossil")
         (let ((template (make-pathname (system-directory) "templates/template" "fossil")))
           (log-status "cloning default fossil" (object->string (list run/boolean "fossil" "clone" template fallback "--once" "-A" fallback-name)))
           (unless (run-logging/boolean "fossil" "clone" template fallback "--once" "-A" fallback-name)
             (log-error "fossil is: " (run->string "fossil" "version"))
             (log-error "Again: " (run->error-string "fossil" "clone" template fallback "--once" "-A" fallback-name)))))))))

(define fossils-directory-handler
  (let ((v #f))
    (case-lambda
     (() (and v (v)))
     ((n) (set! v n)))))

#|
(define (copy-http-headers-except in out . args)
  (let loop ()
    (let ((line (u8-read-line2 in 10 1024)))
      (cond
       ((or (equal? line "") (equal? line "\r"))
        (display "\r\n" out))
       ((member line args string-prefix?) (loop))
       (else (display line out) (newline out) (loop))))))
|#


(define (fossils-copy-http-headers-catching-host in)
  (define host #f)
  (define (gather out)
    (let loop ()
      (let ((line (u8-read-line2 in 10 1024)))
        (cond
         ((or (equal? line "") (equal? line "\r"))
          (display "\r\n" out))
         (else
          (when (string-prefix? "Host: " line)
            (let* ((cr (string-ref line (fx- (string-length line) 1)))
                   (len (if (eqv? cr #\return) (- (string-length line) 1) (string-length line))))
              (set! host (substring line 6 len))))
          (display line out) (newline out) (loop))))))
  (let ((headers (call-with-output-string gather)))
    (values host headers)))

(wire!
 (list fossils-directory chat-own-address ot0cli-ot0-networks)
 sequence:
 (let ((once #t)
       (brk #f)
       (max-line-length 1024))
   (lambda _
     (when (and (fossils-directory) (chat-own-address) (fossils-networks-present? (ot0cli-ot0-networks)))
       (set! once #f)
       (set! brk
             (rx "^([^ ]+) (?:/([^/]+))([^ ]+) (HTTP/[0-9]\\.[0-9])\r?$"))
       (let* ((fossil "fossil")
              (dir (fossils-directory))
              (fallback
               (cond
                (#t `("-notfound" ,(fossils-fallback-name)))
                (else '())))
              (cmdln `("http" ,dir ,@fallback)))
         (fossils-directory-handler (lambda () (semi-fork fossil cmdln)))
         #;(fossils-directory-handler
          (lambda ()
            (let* ((line (u8-read-line2 (current-input-port) 10 max-line-length))
                   (m (rx~ brk line))
                   (m2 (and m (uri-parse (rxm-ref m 2)))))
              (debug 'FossilLine line)
              (cond
               ((and m2 (equal? (at-phone-decoder m2) (chat-own-address)))
                (let* ((baseurl (debug 'baseurl (string-append "/" (rxm-ref m 2))))
                       (proc (semi-fork fossil `(,@cmdln "-baseurl" ,baseurl))))
                  (and (port? proc)
                       (begin
                         (display line proc) (newline proc)
                         proc))))
               (else
                (let ((proc (semi-fork fossil cmdln)))
                  (and (port? proc)
                       (begin
                         (display line proc) (newline proc)
                         proc))))))))
         (http-proxy-on-illegal-proxy-request
          (lambda (line)
            (let* ((m (rx~ brk line))
                   (m2 (and m (uri-parse (rxm-ref m 2)))))
              (cond
               ((and m2 (at-phone-decoder m2)) =>
                (lambda (id)
                  (let ((conn (ot0cli-connect "local" id 80)))
                    (when (port? conn)
                      (display
                       (string-append (rxm-ref m 1) " " (rxm-ref m 3) " " (rxm-ref m 4) "\r\n")
                       conn)
                      #;(begin
                        (display (string-append "Host: " m2 "\r\n") conn)
                        (copy-http-headers-except (current-input-port) conn "Host:"))
                      #;(begin (display line conn) (newline conn))
                      (ports-connect! conn conn (current-input-port) (current-output-port) 3)))))
               ((and (fossils-directory) (chat-own-address))
                (let* ((cmdln `(,@cmdln "-repolist" "-nocompress" "-ipaddr" "127.0.0.1" "-localauth"))
                       (conn (semi-fork fossil cmdln)))
                  (display line conn)
                  (display "\r\n" conn)
                  (ports-connect! conn conn (current-input-port) (current-output-port) 3)))))))
         (lwip-tcp-service-register!
          80
          ;;; (ot0cli-make-process-service fossil cmdln)
          (lambda ()
            (with-exception-catcher
             handle-replloop-exception
             (lambda ()
               (let* ((line (u8-read-line2 (current-input-port) 10 max-line-length))
                      (m (rx~ brk line))
                      (m2 (and m (uri-parse (rxm-ref m 2))))
                      (srv #f))
                 (cond
                  ((and m2 (equal? (at-phone-decoder m2) (chat-own-address)))
                   (receive (host headers) (fossils-copy-http-headers-catching-host (current-input-port))
                     (let* ((baseurl (string-append "http://"  host "/" (rxm-ref m 2)))
                            (proc (semi-fork fossil `(,@cmdln "-baseurl" ,baseurl))))
                       (when (port? proc)
                         #;(let ((p (current-error-port)))
                           (display line p) (newline p) (display headers p) (display "---\n" p))
                         (display line proc) (newline proc)
                         (display headers proc)
                         (set! srv proc)))))
                  (else
                   (let ((proc (semi-fork fossil cmdln)))
                     (when (port? proc)
                       (display line proc) (newline proc)
                       (set! srv proc)))))
                 (when srv
                   (force-output srv)
                   (ports-connect! (current-input-port) (current-output-port) srv srv)
                   (process-status srv))))))))))))
