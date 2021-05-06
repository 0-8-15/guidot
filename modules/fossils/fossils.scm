(define %%fossils%at-phone-decoder (make-parameter (lambda (x) #f)))

(cond-expand
 (debug
  (define-macro (assume obj msg . more)
    `(if ,obj ,obj (apply error ,msg ,more))))
 (else
  (define-macro (assume obj msg . more) obj)))

(include "capture-domain.scm")

(cond-expand
 ((or android fossil-not-embedded)
  (register-command!
   "fossil"
   (lambda (args)
     #|
     (exit
     (debug 'fossil-exit (process-status
     (open-process
     `(path: "fossil" arguments: ,args
     stdin-redirection: #f stdout-redirection: #f show-console: #f)))))
     |#
     (let ((conn (open-process
                  `(path: "fossil" arguments: ,args
                          stdin-redirection: #t stdout-redirection: #t show-console: #f))))
       (when (port? conn)
         (parameterize
             ((port-copy-initial-timeout 2)
              (port-copy-data-timeout 1))
           (ports-connect! conn conn (current-input-port) (current-output-port))
           (exit (/ (process-status conn) 256))))))))
 (else
  (include "../gamhack/gambit-embedded-chararray.scm")
  (c-declare "extern int fossil_main(int argc, char **argv);")
  (register-command!
   "fossil"
   (lambda (args)
     (let* ((args (cons "-s fossil" args))
            (n (length args)))
       ((c-lambda (int char**) int "fossil_main") n args))))))

(define (fossil-command
         #!key
         (log (and #f (lambda (args) (debug 'fossil-command args))))
         (directory (fossils-directory))
         (input #f)
         (repository #t)
         #!rest args)
  (let ((working-directory (or directory (current-directory)))
        (stderr-redirection #f)
        (arguments
         (cond
          ((not repository) args)
          ((eq? repository #t)
           (let ((fn (current-fossil-pathname)))
             (cond
              (fn (append args (list "-R" fn)))
              (else args))))
          ((string? repository) ;; TBD: file,exists,etc...
           (append args (list "-R" repository)))
          (else args))))
    (assume
     (begin
       (when (procedure? log)
         (log `(cwd: ,working-directory arguments: ,@arguments)))
       #t)
     "unreachable")
    (let ((port (semi-fork "fossil" arguments stderr-redirection #|directory: directory|#)))
      (cond
       ((not input) (close-output-port port))
       ((string? input)
        (display input port)
        (close-output-port port)))
      port)))

(define (fossil-command-port/json
         #!key
         (log (and #f (lambda (args) (debug 'fossil-command/json args))))
         (directory #f)
         (repository #t)
         (user (getenv "USER" "u")))
  (let ((working-directory (or directory (current-directory)))
        (stderr-redirection #f)
        (arguments
         (let ((args `("json" "-json-input" "-" "-user" ,user)))
           (cond
            ((not repository) args)
            ((eq? repository #t)
             (let ((fn (current-fossil-pathname)))
               (cond
                (fn (append args (list "-R" fn)))
                (else args))))
            ((string? repository) ;; TBD: file,exists,etc...
             (append args (list "-R" repository)))
            (else args)))))
    (assume
     (begin
       (when (procedure? log)
         (log `(cwd: ,working-directory arguments: ,@arguments)))
       #t)
     "unreachable")
    (semi-fork "fossil" arguments stderr-redirection #|directory: working-directory|#)))

(define (fossil-command/json
         json-sexpr
         #!key
         (log (and #f (lambda (args) (debug 'fossil-command/json args))))
         (directory (fossils-directory))
         (repository #t)
         (user (getenv "USER" "u")))
  (let ((port (fossil-command-port/json repository: repository)))
    (json-write json-sexpr port)
    (close-output-port port)
    (let ((json (json-read port)))
      (cond
       ((eof-object? json) json)
       (else
        (let ((pl (assq 'payload json)))
          (cond
           (pl (cdr pl))
           (else
            (let ((msg (assq 'resultText json)))
              (cond
               ((pair? msg) (error (cdr msg)))
               (else (error "fossil-command/json" json))))))))))))

(define (fossil-project-title repository)
  (let ((json (fossil-command/json
               `((command . "config/get/project"))
               repository: repository)))
    (cond
     ((eof-object? json) #f)
     (else (cdr (assq 'project-name json))))))

(define (fossil-command/sql
         sql-string
         #!key
         (log (and #f (lambda (args) (debug 'fossil-command/json args))))
         (directory (fossils-directory))
         (repository #f)
         (user (getenv "USER" "u")))
  (let ((port 
         (let ((working-directory (or directory (current-directory)))
               (stderr-redirection #f)
               (arguments
                (let ((args `("sql" "-user" ,user)))
                  (cond
                   ((not repository) args)
                   ((string? repository) ;; TBD: file,exists,etc...
                    (append args (list "-R" repository)))
                   (else args)))))
           (assume
            (begin
              (when (procedure? log)
                (log `(cwd: ,working-directory arguments: ,@arguments)))
              #t)
            "unreachable")
           (semi-fork "fossil" arguments stderr-redirection #|directory: working-directory|#))))
    (display sql-string port)
    (close-output-port port)
    port))

(define (sql-quote str)
  (define (string-index s char/char-set/pred i)
    (let loop ((i i))
      (and (< i (string-length s))
           (if (eqv? (string-ref s i) char/char-set/pred)
               i
               (loop (+ i 1))))))
  (if (string-index str #\' 0)
      (let loop ((off 0) (res '()))
        (let ((s (string-index str #\' off)))
          (if s
              (let ((e (string-index str #\' (+ s 1))))
                (loop (or e (string-length str))
                      `(,(case (string-ref str s)
                                   ((#\\) "\\\\")
                                   ((#\') "''")
                                   ;; ((#\")   "\\"" )
                                   ((#\x0) "\\0" )
                                   (else (error "internal error sql-quote broken")))
                                ,(substring str off s)
                                . ,res)))
              (apply
               string-append
               (reverse!      ; res is a fresh list
                (cons (substring str off (string-length str))
                      res))))))
      str))

(define (fossil-project-title-set! repository title)
  (let ((port
         (fossil-command/sql
          (string-append
           "update repository.config set value='"
           (sql-quote title)
           "' where name='project-name'")
          repository: repository)))
    (close-port port)
    (eqv? (process-status port) 0)))

;;** fossils directory and service

(define (fossils-fallback-name unit-id)
  (beaver-unit-id->string unit-id "-"))

(define fossils-directory
  (make-pin
   initial: #f
   pred:
   (lambda (v)
     (or (not v)
         (and (string? v)
              (or (not (file-exists? v))
                  (eq? (file-type v) 'directory)))))
;;   filter: (lambda (old new) (if old old new)) ;; once only
   name: "projects directory"))

(define fossils-enable-http-hijacking
  ;; NOTE: This is experimental.  Switching this on may expose the
  ;; browser to cross site scripting attacks.
  (make-pin
   initial: #f
   name: "fossils-enable-http-hijacking dangerous feature"))

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
             (fossil-project-title-set! fallback fallback-name)
             (log-error "fossil is: " (run->string "fossil" "version"))
             (log-error "Again: " (run->error-string "fossil" "clone" template fallback "--once" "-A" fallback-name)))))))))

(define fossils-directory-handler
  (let ((v #f))
    (case-lambda
     (() v)
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
        (max-line-length 1024))
    (define (fossil args)
      (semi-fork "fossil" args #t))
    (define (producer->pipe producer)
      (receive (servant client) (open-u8vector-pipe)
        (parameterize ((current-output-port servant))
          (producer)
          (close-port servant))
        client))
    (define (fossils-http-serve*
             local repository line0 scheme)
      (let* ((line (or line0
                       (u8-read-line2 (current-input-port) 10 max-line-length)))
             (m (rx~ (force brk) line))
             (unit-id (beaver-local-unit-id))
             (m2 (and m (uri-parse (rxm-ref m 2)))))
        (cond
         ((and m2 (equal? ((%%fossils%at-phone-decoder) m2) unit-id))
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
                   (proc (fossil cmdln)))
              (when (port? proc)
                #;(let ((p (current-error-port)))
                (display line p) (newline p) (display headers p) (display "---\n" p))
                (display
                 (string-append (rxm-ref m 1) " " (rxm-ref m 3) " " (rxm-ref m 4) "\r\n")
                 proc)
                (display headers proc)
                (force-output proc)
                proc))))
         ((equal? line "")
          (producer->pipe
           (lambda ()
             (display
           #<<EOF
HTTP/1.0 500 Error
Content-Type: text/plain

fossils server could not read HTTP request.

EOF
           ))))
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
                 (proc (fossil cmdln)))
            (and (port? proc)
                 (begin
                   (display line proc) (newline proc)
                   (force-output proc)
                   proc)))))))
    (define (fossils-http-serve
             local repository line
             #!key
             (scheme "http://")
             (wait #t))
      (and repository
           (if (or line wait)
               (fossils-http-serve* local repository line scheme)
               (receive (port srv) (open-u8vector-pipe '(buffering: #t) '(buffering: #t))
                 (thread-start!
                  (make-thread
                   (lambda ()
                     (with-debug-exception-catcher
                      (lambda ()
                        (parameterize
                            ;; There should be a simpler way!
                            ((current-input-port srv) (current-output-port srv))
                          (let ((conn (fossils-http-serve* local repository #f scheme)))
                            (ports-connect! conn conn srv srv)
                            (process-status conn))))
                      `(fossils-http-serve ,repository)))
                   repository))
                 port))))
    fossils-http-serve))

(define fossils-directory-service
  (let ()
    (define (fossils-directory-service)
      (let ((dir (fossils-directory)))
        (when dir
          (let ((conn (fossils-http-serve #f dir #f)))
            (when (port? conn)
              (ports-connect! conn conn (current-input-port) (current-output-port)))))))
    (tag-thunk-as-service fossils-directory-service)))

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
       (let ((unit-id (beaver-local-unit-id))
             (previous-handler (http-proxy-on-illegal-proxy-request)))
         (fossils-directory-handler
          (lambda (auth) (fossils-http-serve auth (fossils-directory) #f wait: #f)))
         (http-proxy-on-illegal-proxy-request
          (lambda (line)
            (let* ((m (rx~ brk line))
                   (m2 (and m (uri-parse (rxm-ref m 2))))
                   (id (and m2 ((%%fossils%at-phone-decoder) m2))))
              (cond
               ((or (not id) (equal? id unit-id)
                    (file-exists? (fossils-project-filename (fossils-fallback-name id))))
                (let ((conn (fossils-http-serve #t (fossils-directory) line)))
                  (when (port? conn)
                    (ports-connect! conn conn (current-input-port) (current-output-port)))))
               ((fossils-enable-http-hijacking)
                (let ((conn (ot0cli-connect "local" id 80)))
                  (when (port? conn)
                    (display line conn) (newline conn)
                    (force-output conn)
                    (ports-connect! conn conn (current-input-port) (current-output-port)))))
               (else (previous-handler line)))))))))))
