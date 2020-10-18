
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
         (let ((template (make-pathname (system-directory) "templates/template" "fossil")))
           (open-process (debug 'DO `(path: "fossil" arguments: ("clone" ,template ,fallback "--once" "-A" ,fallback-name))))))))))

(wire!
 (list fossils-directory chat-own-address ot0cli-ot0-networks)
 sequence:
 (let ((once #t))
   (lambda _
     (when (and (fossils-directory) (chat-own-address) (fossils-networks-present? (ot0cli-ot0-networks)))
       (set! once #f)
       (let* ((fossil "fossil")
              (dir (fossils-directory))
              (fallback
               (cond
                (#t `("-notfound" ,(fossils-fallback-name)))
                (else '())))
              (cmdln `("http" ,dir ,@fallback)))
         (http-proxy-on-illegal-proxy-request
          (lambda (line)
            (when (and (fossils-directory) (chat-own-address))
              (let* ((cmdln `(,@cmdln "-repolist" "-nocompress" "-ipaddr" "127.0.0.1" "-localauth"))
                     (conn (open-process `(path: ,fossil arguments: ,cmdln))))
                (display line conn)
                (display "\r\n" conn)
                (ports-connect! conn conn (current-input-port) (current-output-port) 3)))))
         (beaver-process-commands `(-service vpn tcp register 80 command: ,fossil . ,cmdln)))))))
