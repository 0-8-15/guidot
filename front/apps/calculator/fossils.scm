
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

(wire!
 (list fossils-directory chat-own-address ot0cli-ot0-networks)
 sequence:
 (let ((once #t)
       (brk #f))
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
         (http-proxy-on-illegal-proxy-request
          (lambda (line)
            (let ((m (rx~ brk line)))
              (cond
               ((at-phone-decoder (uri-parse (rxm-ref m 2))) =>
                (lambda (id)
                  (let ((conn (ot0cli-connect "local" id 80)))
                    (when (port? conn)
                      (display
                       (string-append (rxm-ref m 1) " " (rxm-ref m 3) " " (rxm-ref m 4) "\r\n")
                       conn)
                      (ports-connect! conn conn (current-input-port) (current-output-port) 3)))))
               ((and (fossils-directory) (chat-own-address))
                (let* ((cmdln `(,@cmdln "-repolist" "-nocompress" "-ipaddr" "127.0.0.1" "-localauth"))
                       (conn (semi-fork fossil cmdln)))
                  (display line conn)
                  (display "\r\n" conn)
                  (ports-connect! conn conn (current-input-port) (current-output-port) 3)))))))
         (lwip-tcp-service-register! 80 (ot0cli-make-process-service fossil cmdln)))))))
