(include "capture-domain.scm")

(register-command!
 "fossil"
 (lambda (args)
   (process-status
    (open-process
     `(path: "fossil" arguments: ,args
             stdin-redirection: #f stdout-redirection: #f show-console: #f)))))

(define (fossils-fallback-name unit-id)
  (beaver-unit-id->string unit-id "-"))

(define fossils-directory
  (make-pin
   initial: #f
   pred: (lambda (v) (or (not v) (string? v)))
   filter: (lambda (old new) (if old old new)) ;; once only
   name: "projects directory"))

(define (fossils-directory-location dir)
  (cond-expand
   (android
    (make-pathname (system-directory) dir))
   (else dir)))

(define (fossils-project-filename project)
  (let ((dir (fossils-directory)))
    (and dir (make-pathname dir project "fossil"))))

(wire!
 (list fossils-directory beaver-local-unit-id)
 sequence:
 (lambda (oldd newd oa na)
   (when newd
     (unless (file-exists? newd)
       (create-directory newd))
     (let* ((unit-id (beaver-local-unit-id))
            (fallback-name (and (beaver-local-unit-id) (fossils-fallback-name unit-id)))
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

(define-macro (fossils-http-service-default/value name)
  (let ((option (string-append "-" (symbol->string name))))
    `(if ,name (list ,option ,name) '())))

(define-macro (fossils-http-service-default name)
  (let ((option (string-append "-" (symbol->string name))))
    `(if ,name (list ,option) '())))

(define (fossils-make-http-command-line-options
         repository #!key
         (notfound #f)
         (repolist #f)
         (https #f)
         (nossl #t)
         (nocompress #f)
         (localauth #f)
         (baseurl #f)
         (host #f)
         (ipaddr #f)
         (skin #f))
  (let ((notfound (fossils-http-service-default/value notfound))
        (repolist (fossils-http-service-default repolist))
        (https (fossils-http-service-default https))
        (nossl (fossils-http-service-default nossl))
        (nocompress (fossils-http-service-default nocompress))
        (localauth (fossils-http-service-default localauth))
        (baseurl (fossils-http-service-default/value baseurl))
        (host (fossils-http-service-default/value host))
        (ipaddr (fossils-http-service-default/value ipaddr))
        (skin (fossils-http-service-default/value skin))
        )
    `("http" ,repository
      ,@notfound
      ,@repolist
      ,@https
      ,@nossl
      ,@localauth
      ,@baseurl
      ,@host
      ,@ipaddr
      ,@skin
      ,@nocompress
      )))

(define fossils-http-serve
  (let ((brk (delay (rx "^([^ ]+) (?:/([^/]+))([^ ]+) (HTTP/[0-9]\\.[0-9])\r?$")))
        (fossil "fossil")
        (max-line-length 1024))
    (define (fossils-http-serve
             local repository line0
             #!key
             (scheme "http://"))
      (let* ((line (or line0
                       (u8-read-line2 (current-input-port) 10 max-line-length)))
             (m (rx~ (force brk) line))
             (unit-id (beaver-local-unit-id))
             (m2 (and m (uri-parse (rxm-ref m 2)))))
        (cond
         ((not repository) #f)
         ((and line0 m2 (equal? (at-phone-decoder m2) unit-id))
          (receive (host headers) (fossils-copy-http-headers-catching-host (current-input-port))
            (let* ((baseurl (string-append scheme  host "/" (rxm-ref m 2)))
                   (notfound (fossils-fallback-name unit-id))
                   (cmdln (if local
                              (fossils-make-http-command-line-options
                               repository
                               notfound: notfound
                               baseurl: baseurl
                               repolist: #t nocompress: #t
                               ipaddr: "127.0.0.1" localauth: #t)
                              (fossils-make-http-command-line-options
                               repository
                               notfound: notfound
                               baseurl: baseurl)))
                   (proc (semi-fork fossil cmdln)))
              (when (port? proc)
                #;(let ((p (current-error-port)))
                (display line p) (newline p) (display headers p) (display "---\n" p))
                (display
                 (string-append (rxm-ref m 1) " " (rxm-ref m 3) " " (rxm-ref m 4) "\r\n")
                 proc)
                (display headers proc)
                (force-output proc)
                proc))))
         (else
          (let* ((cmdln (if local
                            (fossils-make-http-command-line-options
                             repository
                             notfound: (fossils-fallback-name unit-id)
                             repolist: #t nocompress: #t
                             ipaddr: "127.0.0.1" localauth: #t)
                            (fossils-make-http-command-line-options
                             repository
                             notfound: (fossils-fallback-name unit-id))))
                 (proc (semi-fork fossil cmdln)))
            (and (port? proc)
                 (begin
                   (display line proc) (newline proc)
                   (force-output proc)
                   proc)))))))
    fossils-http-serve))

(define (fossils-directory-service)
  (let ((dir (fossils-directory)))
    (when dir
      (let ((conn (fossils-http-serve #f dir #f)))
        (when (port? conn)
          (ports-connect! conn conn (current-input-port) (current-output-port) 3))))))

(wire!
 (list fossils-directory beaver-local-unit-id)
 sequence:
 (let ((once #t)
       (brk #f)
       (max-line-length 1024))
   (lambda _
     (when (and once (fossils-directory) (beaver-local-unit-id))
       (set! once #f)
       (set! brk (rx "^([^ ]+) (?:/([^/]+))([^ ]+) (HTTP/[0-9]\\.[0-9])\r?$"))
       (let ((unit-id (beaver-local-unit-id)))
         (fossils-directory-handler
          (lambda () (fossils-http-serve #f (fossils-directory) #f)))
         (http-proxy-on-illegal-proxy-request
          (lambda (line)
            (let* ((m (rx~ brk line))
                   (m2 (and m (uri-parse (rxm-ref m 2))))
                   (id (and m2 (at-phone-decoder m2))))
              (cond
               ((or (not id) (equal? id unit-id))
                (let ((conn (fossils-http-serve #t (fossils-directory) line)))
                  (when (port? conn)
                    (ports-connect! conn conn (current-input-port) (current-output-port) 3))))
               (else
                (let ((conn (ot0cli-connect "local" id 80)))
                  (when (port? conn)
                    (display line conn) (newline conn)
                    (force-output conn)
                    (ports-connect! conn conn (current-input-port) (current-output-port) 3)))))))))))))
