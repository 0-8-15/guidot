;; frontend (to be changed to mimic the Calculator demo)

(register-command! "ball" ballroll)

(register-command! "beaver" beaver-start!)

(cond
 ((and (>= (system-cmdargc) 3) (equal? (system-cmdargv 1) "-s"))
  (execute-registered-command))
 (else #!void))

(cond-expand
 (debug
  ($debug-trace-triggers #t)
  ($async-exceptions 'catch)
  ) (else #f))

(set!
 thread-sleep!
 (let ((thread-sleep! thread-sleep!)
       (short-time (cond-expand
                    (android 0.3)
                    (else 0.01))))
   (lambda (t)
     (let ((t0 (current-second)))
       (thread-sleep! t)
       (let* ((t1 (current-second))
              (delta (- t1 t0)))
         (if (> (abs (- t delta)) (max short-time (* 0.3 t)))
             (log-error "Error in thread sleep, sleeping for " t " took " delta)))))))

(cond-expand
 (android
  (define (setup-heartbeat!)
    #;((c-lambda () scheme-object "___setup_heartbeat_interrupt_handling"))
    (##set-heartbeat-interval! (exact->inexact 1/100)))
  (setup-heartbeat!)
  )
 (else))

(define (android-directory-files)
  ((c-lambda () char-string "
#ifdef ANDROID
extern char *android_getFilesDir();
#endif
___result=
#ifdef ANDROID
android_getFilesDir();
#else
NULL;
#endif
")))

(include "jscheme.scm")

(cond-expand
 (android
  (let ((ot terminate)) (set! terminate (lambda () (log-error "No terminate on Android!") #f)))
  )
 (else
  ))

(define (migrate-data-to-protected-space!)
  (let ((target kernel-data-directory)
        (source (make-pathname (system-directory) "data")))
    (if (and (file-exists? source)
             (not (file-exists? target))
             (not (equal? source target))
             (kernel-backup! from: source))
        (run/boolean 'mv source target)
        #t)))

(define (portnumber-or-false? obj)
  (or (not obj) (and (fixnum? obj) (>= obj 0) (<= obj #xffff))))

(define (front-pin-attach-log! pin msg)
  (wire! pin post: (lambda () (log-debug msg 1 (debug msg (pin))))))

(define (front-beaver-directory-default)
  (make-pathname
   (cond-expand
    (android (system-appdirectory))
    (else '()))
   "beaver"))
(define front-beaver-query (lambda (expr) (error "beaver-call/~query not yet started")))
(define front-beaver-call front-beaver-query)
(define front-test-beaver-active (lambda () (error "beaver not yet started")))
(define front-beaver-directory
  (let ((pin
         (make-pin
          initial: #f
          filter: (lambda (o n) (or o n))
          name: "The directory for beaver VPN data")))
    (define (export old new)
      (receive (test query call) (make-beaver-api new)
        (set! front-test-beaver-active test)
        (set! front-beaver-query call)
        (set! front-beaver-call call)))
    (wire! pin sequence: export)
    pin))
(kick/sync! (lambda () (front-beaver-directory (front-beaver-directory-default))))

(define front-beaver-enabled:=
  (make-pin
   initial: (query-beaver-ot0-status)
   pred: boolean?
   name: "enable beaver VPN"))

(define front-beaver-up??
  (make-pin initial: (front-beaver-enabled:=) name: "beaver VPN seen up"))
(front-pin-attach-log! front-beaver-up?? "beaver up")
(let ((msg "ball kernel up upon startup"))
  (if ball-kernel-found-up-on-startup (log-debug msg 1 (debug msg ball-kernel-found-up-on-startup))))
(front-pin-attach-log! ball-kernel-up?? "ball kernel up")

(define front-beaver-network (make-pin initial: 18374687579166474240))

(define (front-beaver-origin:= u8v)
  (front-beaver-call `(kick (ot0cli-origin ',u8v))))

(define (front-beaver-start-script)
  `(ot0cli-process-commands
    '("-adm" "ip:" "on"
      "-service" "ot0" "start" "\"*:19993\"" "join:" ,(object->string (front-beaver-network)))))

(define beaver-socks-port (make-pin initial: 9052))

(define (front-beaver-setup-script-2)
  `(ot0cli-process-commands
    '("-service" "tcp" "register" ,(object->string (beaver-socks-port)) "socks" ":")))

(define front-beaver-ip-address
  (make-pin
   initial: #f
   ;;pred: (lambda (x) (or (not x) (string? x)))
   name: "beaver IP address"))
(front-pin-attach-log! front-beaver-ip-address "Beaver IP address ")

(define-macro (macro-file-as-u8 fn)
  (and (file-exists? fn)
       (let* ((size (file-size fn))
              (data (make-u8vector size))
              (got (call-with-input-file fn (lambda (p) (read-subu8vector data 0 size p)))))
         (if (eqv? got size)
             `',data
             (error "read-file-as-u8vector failed. following file size read and size expected"
                    fn got size)))))

(define front-beaver-default-origin (macro-file-as-u8 "/home/u/.ot0/origin"))

(define (query-beaver-ip-address)
  (log-status "Waiting for beaver address")
  (front-beaver-ip-address
   (front-beaver-query
    `(and (ot0-up?)
          (ip6addr->string (make-6plane-addr ,(front-beaver-network) (ot0-address))))))
  (front-beaver-up?? (and (front-beaver-ip-address) #t)))

(if (front-beaver-up??) (kick! query-beaver-ip-address))

(define (query-beaver-ot0-status)
  (with-exception-catcher
   (lambda (exn) (handle-replloop-exception exn) #f)
   (lambda () (and (front-test-beaver-active) (front-beaver-query '(ot0-up?))))))

(define (front-beaver-start!*)
  (define (control-socket fn) (make-pathname fn "control"))
  (define (runit! fn)
    (log-status "Running beaver from " fn)
    (semi-fork "beaver" `(#;"-:r" "-B" ,fn "-S" "control" ,(control-socket fn) "-e" ":" "-wait")))
  (define (beaver-init! fn)
    (log-status "Initialize beaver home " fn)
    (when (debug 'BInit (semi-run "beaver" `("-A" ,fn)))
      (let ((beaver (semi-fork& "beaver" `("-B" ,fn "-S" "control" ,(control-socket fn) "-e" ":" "-wait"))))
        (when beaver
          (front-beaver-call `(begin (kick/sync (ot0cli-origin ',front-beaver-default-origin)) (exit 0)))
          (when (eqv? (process-status beaver) 0)
            (runit! fn))))))
  (define (startup!)
    (log-status "Starting beaver")
    (when (front-beaver-call (front-beaver-start-script))
      (kick/sync! query-beaver-ip-address))
    (log-debug "Beaver setup returned: " 1 (front-beaver-call (front-beaver-setup-script-2))))
  (let ((fn (front-beaver-directory)))
    (cond
     ((not (file-exists? fn)) (beaver-init! fn) (startup!))
     ((front-test-beaver-active) (kick! query-beaver-ip-address))
     (else (runit! fn) (startup!)))))

(define (front-beaver-start!)
  (with-exception-catcher
   (lambda (exn)
     ;; (kick! (lambda () (front-beaver-ip-address (exception-->printable exn))))
     (log-error "front-beaver-start! : " (exception-->printable exn)))
   front-beaver-start!*))

(define (front-beaver-stop!)
  (with-exception-catcher
   (lambda (exn) #;(debug 'query-beaver-ot0-status exn) #f)
   (lambda () (front-beaver-query '(exit 0))))
  ;; FIXME: unset up?? when it dies unexpectedly.
  (kick! (lambda () (front-beaver-up?? #f))))

(define (front-trigger-beaver-start-stop!)
  (let ((e (front-beaver-enabled:=))
        (u (front-beaver-up??)))
    (cond
     ((and e (not u)) (front-beaver-start!))
     ((and u (not e)) (front-beaver-stop!)))))

(define (front-beaver-reset-logging! #!key (on log:on) (to log:file))
  (if (front-beaver-up??)
      (front-beaver-call `(ot0cli-process-commands '("-change" "daemonian-stderr-file" ,(if on to "/dev/null") ":")))))

(wire!
 (list front-beaver-enabled:= front-beaver-up??)
 post: front-trigger-beaver-start-stop!)

(thread-start!
 ;; NOTE: we might not need this thread at all.
 (make-thread
  (lambda ()
    (do () (#f)
      (thread-sleep! 60)
      (let ((active (front-test-beaver-active)))
        (unless (eq? active (front-beaver-up??))
          (kick! (lambda () (front-beaver-up?? active)))))))
  'watch-beaver))

(define (kernel-send-connect to)
  (unless (call-kernel "connect" to)
	  (error "failed to connect to" to)))

(define (kernel-host-lookup host)
  (call-kernel 'begin `(askemos-host-lookup ,host)))

(define (kernel-send-support from name remote)
  (unless (call-kernel "support" from name remote)
	  (error "failed to support" from name remote)))

(define (is-garlic? name) (and name (string-suffix? ".i2p" name)))
(define (is-onion? name) (string-suffix? ".onion" name))
(define (is-beavered? name) #t) ;; FIXME

(define (is-allioideae? name) (or (is-onion? name) (is-garlic? name)))

;; `documented-external-https-port` is actually a constant and maybe
;; should be(come) a macro.
;;
;; We need this because so far we can't make Android I2P use a
;; different external port than the one we forward to.
(define (documented-external-https-port) 7443)

(define (front-trigger-ball-kernel-start-stop!)
  (let ((e (ball-kernel-enabled:=))
        (u (ball-kernel-up??)))
    (cond
     ((and e (not u)) (start-kernel-server!))
     ((and u (not e) (not in-initialization)) (stop-kernel-server!)))))

(wire!
 (list ball-kernel-enabled:= ball-kernel-up??)
 post: front-trigger-ball-kernel-start-stop!)

#;(wire!  ;; TODO: remove after a while
 ball-kernel-up?? post:
 (lambda ()
   (call-kernel
    'begin
    '(for-each
      http-host-remove!
      (filter
       (let ((mat (dsssl-pcre "onion|i2p")))
         (lambda (h) (mat (http-host-lookup h))))
       (quorum-others (http-all-hosts))))
    ;; '(ball-trigger-save-config!)
    '(ball-save-config))))

(define (front-trigger-transfer-of-beaver-ip-to-ball!)
  (when (and (ball-kernel-up??) (front-beaver-ip-address))
    (log-status "configuring ball to use beaver at: " (front-beaver-ip-address))
    (call-kernel
      'begin
      `($external-address ,(front-beaver-ip-address))
      '($https-use-socks4a #t)
      `($https-socks4a-server ,(string-append "127.0.0.1:" (number->string (beaver-socks-port))))
      '($external-port 443)
      ;;'(ball-trigger-save-config!) ;; TODO: remove
      '(ball-save-config)
      #t)))

(wire!
 (list ball-kernel-up?? front-beaver-ip-address)
 post: front-trigger-transfer-of-beaver-ip-to-ball!)

(define (kernel-config-socks-script cn)
  (define socks-port (beaver-socks-port))
  (cond
   ((beaver-socks-port)
    `(begin
       ($https-use-socks4a #t)
       ($https-socks4a-server ,(string-append "127.0.0.1:" (number->string (beaver-socks-port))))
       ($external-port 443)
       ($external-address ,(front-beaver-ip-address))))
   ((is-allioideae? cn)
    `(begin
       ($https-use-socks4a #t)
       ($https-socks4a-server ,(if (is-garlic? cn) "127.0.0.1:9051" "127.0.0.1"))
       ($external-port 443)
       ($external-address ,cn)))
   (else
    `(begin
       ($external-port ,(documented-external-https-port))
       ($external-address ,(if (ipv6-address/port? cn) cn #f))
       ($https-use-socks4a 'maybe)))))

(define (kernel-send-set-auth kind user password cn)
  (unless
      (call-kernel
       'begin
       (add-guard-fail
        "signing failed"
        `(and (install-x509-default ,user ,password ,cn)
              (begin
                ;; not exported ($tc-tofu ,(equal? kind 'tofu))
                (local-id ,cn)
                ,(kernel-config-socks-script cn)
                (ball-save-config)
                #t))))
    (error "failed to set auth" kind user cn)))

(define (get-channels-command #!optional (source #f) (oid? #f))
  (define from
    (cond
     ((not source) '(my-oid))
     ((string? source)
      '(or `(string->oid ,source) `(entry-name->oid ,source)))
     (else source)))
  (define ff
    (if source
	'`((,k ,v) . ,i)
	'(if (equal? k "public") i `((,k ,v) . ,i))))
  `(and-let* ((oid ,from)
              (f (find-local-frame-by-id ,from 'gui-client))
              (links (fget f 'mind-links)))
             (fold-links-sorted (lambda (k v i) ,ff) '() links)))

(define (kernel-initial-configuration cn logname)
  (call-kernel
   'begin
   `(guard
     (ex (else
          (log-condition "Failed to complete initial configuration"  ex)
          #f))
     ($http-server-port 0)
     ($lmtp-server-port 0)
     ($control-port 0)
     (ball-client-mode #t)
     ($http-client-connections-maximum 10)
     (respond-timeout-interval 20)
     ($broadcast-timeout 8)
     ,(kernel-config-socks-script cn)
     (ball-save-config)
     #; (logerr "Fixing protection on \"system\" to be usable by \"~a\"\n" ,logname)
     (let* ((sid (entry-name->oid "system"))
            (sys (document sid)))
       (fset! sys 'protection (list (entry-name->oid ,logname)))
       (frame-commit! sid))
     #t)))

(define (add-guard-fail msg code)
  `(guard
    (ex (else (log-condition ,msg ex) #f))
    ,code))

#;(define (kernel-add-reflexive-entry! entry)
  ;;(call-kernel "channel" "-link" entry filename)
  (call-kernel
   'begin
   (debug 'Trying `(and ,(add-guard-fail "creating reflexive entry" `(install-new-private-name!
          ,entry
          ,(make-new-element
           `(message-body (query-via-entry! ,(main-entry-name) to: '("system" "user.code")))
           action: "system/bail-reflexive"
           protection: (main-entry))))
         #t))))

#;(define (kernel-add-reflexive-entry! entry)
  #;(call-kernel "channel" "-link" entry filename)
  (define sys
    (let ((e (assoc "system" (entry-points))))
      (if e (cadr e) (error "system missing"))))
  (define to (with-output-to-string
               (lambda ()
                 (display "/")
                 (display sys)
                 (display "/user.code"))))
  (call-kernel
   'begin
   (debug 'Trying `(and ,(add-guard-fail "creating reflexive entry" `(install-new-private-name!
          ,entry
          ,(make-new-element
            `(let ((x (message-body (query-via-entry! ,(main-entry-name) (empty-node-list) to: ,to))))
               (logerr "Found ~a\n" (xml-format x))
               x)
           action: "system/bail-reflexive"
           protection: (main-entry))))
         #t))))

(define (kernel-add-reflexive-entry! entry)
  #;(call-kernel "channel" "-link" entry filename)
  (define sys
    (let ((e (assoc "system" (entry-points))))
      (if e (cadr e) (error "system missing"))))
  (define to (with-output-to-string
               (lambda ()
                 (display "/")
                 (display sys)
                 (display "/user.code"))))
  (call-kernel
   'begin
   (debug 'Trying `(and ,(add-guard-fail "creating reflexive entry" `(install-new-private-name!
          ,entry
          ,(make-new-element
            `(xml-parse (filedata ,(embedded-file '("lib") "xslt-user.xml")))
           action: "system/bail-reflexive"
           protection: (main-entry))))
         #t)))
  (refresh-entry-points-and-dependencies!)
  #t)

(define (make-new-element nl #!key (action "public") (protection (public-oid)))
  (let ((protection (if (symbol? protection) (symbol->string protection)  protection)))
    `(make-new-element ,nl '(action . ,action) '(protection . ,protection))))

(define (make-new-element-from-file fn)
  ;; FIXME: A horrible hack to simplify coding for right now.
  (if (string-suffix? ".ico" fn)
      (make-new-element
       `(sxml `(output (@ (method "text") (media-type "image/x-icon"))
                       ($$ ,filedata ,,fn))))
      (make-new-element `(xml-parse (filedata ,fn)))))

(define (make-link-form-from-file nm fn)
  (list 'make-link-form nm (make-new-element-from-file fn)))

(define (code-system-link-via-metactrl entry as filename)
  (let ((in '(entry-name->oid "system")))
    `(send-via-entry!
      ,entry
      ,(make-link-form-from-file as filename)
      to: ,in)))

(define (kernel-install-in-system0 entry as filename)
  (let* ((from-path '("lib"))
         (fn (embedded-file from-path filename)))
    (and
     fn
     (or
      (call-kernel
       'begin
       `(and-let*
         ((result (guard
                   (ex (else (log-condition "Linking into system failed" ex) #f))
                   ,(debug 'calling (code-system-link-via-metactrl entry as fn))))
          ((frame? result)))
         ;; FIXME: look at 'status
         #t))
      (begin
        (log-error "failed to install '" as "' from \"" fn "\"")
        #f)))))

(define (kernel-install-in-system entry as filename)
  (debug as (kernel-install-in-system0 entry as filename)))

(define (kernel-additional-setup entry)
  (and
   (kernel-install-in-system entry "user.code" "xslt-user.xml")
   (kernel-install-in-system entry "wallet.code" "wallet.xml")
   (kernel-install-in-system entry "wallet-skin-simple" "wallet-skin-simple.xml")
   (kernel-install-in-system entry "wallet.icon" "wallet-icon.ico")
   (kernel-install-in-system entry "sms.code" "sms.xml")
   (kernel-install-in-system entry "davtree.code" "xslt-davtree.xml")
   (kernel-install-in-system entry "Kalender.xml" "Kalender.xml")
   (kernel-install-in-system entry "hoist.xml" "hoist.xml")
   (kernel-install-in-system entry "Edit15.xml" "Edit15.xml")
   (kernel-add-reflexive-entry! (string-append (main-entry-name) "-private"))))

(define (kernel-additional-configuration logname large?)
  (or (not large?)
      (kernel-additional-setup logname)))

(define (embedded-file path name)
  (let ((fn (make-pathname (cons (system-directory) path) name)))
    (if (file-exists? fn)
	fn
	(begin
	  (log-error "missing file" fn)
	  #f))))

(define (*init-rep! CN logname passwd large?)
  (define data-file-path
    (cond
     ((string=? (system-platform) "android")
      (lambda (fn) (make-pathname (list (system-directory) "lib") fn)))
     (else (lambda (fn) (make-pathname "." fn)))))
  (let ((path '("lib")))
    (let
     ((rules (embedded-file path "Rules.md"))
      (refltr (embedded-file path "reflexive-xdslt.xml"))
      (sysinf (embedded-file path "sysinf.xml"))
      (syssetup (data-file-path "setup-system.setup"))
      (sysctrl (data-file-path "system.core"))
      (user-app (if large?
                    (let ((fn1 (embedded-file path "xslt-user.xml"))
                          (fn2 (data-file-path "user.core")))
                      (and fn1
                           (let ((data (make-string (file-size fn1))))
                             (call-with-input-file fn1
                               (lambda (p) (read-substring data 0 (string-length data) p)))
                             (with-output-to-file fn2
                               (lambda ()
                                 (display "<new action=\"system/bail-reflexive\" secret=\"none\">\n")
                                 (display data)
                                 (display "\n</new>\n")))
                             fn2)))
                    (embedded-file path "wallet.core"))))
     (and
      (not (rep-exists?))
      rules sysctrl user-app
      (begin
	(with-output-to-file syssetup
	  (lambda ()
	    (pretty-print
	     `(sxml
	       `(letseq
		 (bindings
		  (bind (@ (name "public")) (id ($$ ,literal ,(public-oid))))
		  (bind
		   (@ (name "reflexive"))
		   (new
		    (@ (action "public"))
		    (output
		     ($$ ,xml-parse ,(filedata ,refltr)))))
		  )
		 (new
		  (@ (action "reflexive") (secret "none"))
		  (link (@ (name "xslt-method")) (ref "reflexive"))
		  (link (@ (name "bail-reflexive")) (ref "reflexive"))
		  (link (@ (name "public")) (ref "public"))
		  (output
		   ($$ ,xml-parse ,(filedata ,sysinf))))
		 )))))
	(kernel-server
	 fork-process #f ;; no watchdog
	 "ball"
	 "-init" kernel-data-directory rules
	 "load" "-o" sysctrl syssetup
	 "channel" "-link" "system" sysctrl
	 "channel" "-link" logname user-app
	 "tofu" CN logname passwd
	 ;; "channel" "secret" "set" logname passwd
	 "-start" kernel-data-directory)
	(and (wait-for-kernel-server 10000)
             (begin (if large? (delete-file user-app)) #t)
             (kernel-initial-configuration CN logname)
             (kernel-additional-configuration logname large?)
             (begin
               (hook-run kernel-on-init)
               ;; Stop to be restarted as usual.
               (kernel-server-kill!)
               (kernel-server #t) ;; wait for it
               #t)))))))

(define in-initialization #f)

(define (expecting-progress)
  (thread-yield!)
  (or in-initialization))

(define (init-rep! CN logname passwd large?)
  (define steps
    `((0.2 ,(lambda ()
	      (file-exists? kernel-data-directory)))
      (0.5 ,(lambda ()
	      (file-exists? (make-pathname (list kernel-data-directory "ssl") "ca-cert" "pem"))))
      (0.9 ,(lambda ()
	      (file-exists? (make-pathname (list kernel-data-directory "ssl") "private-cert" "pem"))))))
  (let ((t1 (make-thread
	     (lambda ()
	       (set! in-initialization 0.05)
	       (if (*init-rep! CN logname passwd large?)
		   (begin
                     (kick/sync! (lambda () (front-beaver-enabled:= #t)))
                     (log-status "Initialization done."))
		   ;; TBD: handle the error situation.  Just how?
		   (log-error "Initialization failed.\n"))
	       (set! in-initialization #f)
	       #;(update-pages!))
	     'initializing))
	(t2 (make-thread
	     (lambda ()
	       (let loop ()
		 (thread-sleep! 0.2)
		 (when in-initialization
		       (do ((steps steps (cdr steps)))
			   ((null? steps))
			 (if (and (< in-initialization (caar steps))
				  ((cadar steps)))
			     (set! in-initialization (+ (caar steps) 0.01))))
		       (loop))))
	     'init-watching)))
    (set! in-initialization 0.01)
    (thread-start! t1)
    (thread-start! t2)
    (thread-sleep! 0.2)))

;; GUI

(define (modal-if message action #!key
		       (left `("OK" ,action))
		       (right #t)
		       (abort #f))
  (let ((needs-processing #f))
    (let ((left
	   (cond
	    ((string? left) `(,left ,action))
	    ((pair? left) left)
	    (else (error "modal syntax for left action not handled" left))))
	  (right
	   (cond
	    ((or (eq? right #t) abort) `(("NO" ,abort)))
	    ((string? right) `((,right ,abort)))
	    ((not right) '())
	    (else (error "modal syntax for right action not handled" right))))
	  (message
	   (cond
	    ((procedure? message) (message))
	    ((string? message) message)
	    ((pair? message) (set! needs-processing #t) message)
	    (else "load string error, was soll ich tun?"))))
      (if needs-processing
	  `(,(with-output-to-string
               (lambda ()
                 (for-each
                  (lambda (m)
                    (cond
                     ((symbol? m)
                      (display (dbget m '#f)))
                     (else (display m))))
                  message)))
            ,left ,@right)
	  `(,message ,left ,@right)))))

(define myip #f)  ;; deprecated

(define %local-void-value (list #!eof))
(define (%is-local-void? value)
  (eq? value %local-void-value))
(define %cached-kernel-values-hook (make-hook 1))
(define (chached-kernel-values-flush!) (hook-run %cached-kernel-values-hook #t) #f)
(define-macro (define-once/or name missing . body)
  (let ((refresh (gensym 'refresh))
        (value (gensym name))
        (proc (gensym name)))
    `(define ,name
       (let* ((,value %local-void-value)
              (,proc
               (lambda (#!optional (,refresh #f))
                 (if ,refresh (set! ,value %local-void-value))
                 (if (and (or (not ,value) (%is-local-void? ,value)) (not (eq? ,refresh 'void)))
                     (set! ,value (begin . ,body)))
                 ;; FIXME: That's a bit strange: these values can NOT be #f!!!
                 (if (or (%is-local-void? ,value) (not ,value)) ,missing ,value))))
         (hook-add! %cached-kernel-values-hook ,proc)
	 ,proc))))

(define *entry-missing* "<Entry Missing>")

(define-once/or local-connect-string *entry-missing*
  (and (check-kernel-server!)
       (let ((ip (allioideae-address)))
         (if ip (set! ip (ipconnect-string ip (if (or (is-beavered? ip) (is-allioideae? ip)) 443 (external-https-port)))))
	 (if ip
	     (set! myip ip)
	     (set! ip myip))
	 ip)))

(define (have-local-connect?)
  (not (eq? (local-connect-string) *entry-missing*)))

(define (my-ip-address-display #!optional (postproc #f))
  (define (v0)
    `(button
      h 75 size normal ;; indent 0.05 rounded #t
      text
      ,(if (have-local-connect?)
	   (local-connect-string)
	   "Find Address")
      action
      ,(lambda ()
         (cond
          ((have-local-connect?) (chached-kernel-values-flush!))
          (else
           (when (and (check-kernel-server!)
                      (eq? (local-connect-string #t) *entry-missing*))
             (set! myip (let ((myip (host-ipaddr)))
                          (and myip (ipconnect-string myip (external-https-port))))))))
         (if (procedure? postproc) (postproc))
         #f)))
  (define (v1)
    `(label text ,(lambda () (string-append "Address: " (or (front-beaver-ip-address) "local address not available")))))
  (v1))

(define-macro (define-cached-kernel-value name valid? convert . query)
  (let ((cache (gensym name))
        (proc (gensym name))
	(reload (gensym))
	(tmp (gensym)))
    `(define ,name
       (let* ((,cache %local-void-value)
              (,proc (lambda (#!optional (,reload #f))
                       (if (or ,reload (%is-local-void? ,cache))
                           (if (check-kernel-server!)
                               (let ((,tmp (call-kernel . ,query)))
                                 (if (procedure? ,convert) (set! ,tmp (,convert ,tmp)))
                                 (if (or (not ,valid?) (,valid? ,tmp))
                                     (set! ,cache ,tmp)))))
                       (if (%is-local-void? ,cache)
                           (if (procedure? ,convert) (,convert) ,convert)
                           ,cache))))
         (hook-add! %cached-kernel-values-hook ,proc)
         ,proc))))

(define-cached-kernel-value allioideae-address
  #f (lambda (#!optional (v #f)) (if (equal? v "") #f v))
  'begin '($external-address))

(define-cached-kernel-value public-oid #f #f 'begin '(public-oid) #;("print" "public"))

(define-cached-kernel-value entry-points #f '() 'begin (get-channels-command))

(define-cached-kernel-value main-entry #f #f 'begin '(mesh-cert-o (tc-private-cert)))

(define-cached-kernel-value https-server-port #f (documented-external-https-port) 'begin '($https-server-port))

(define https-server-port2
  (make-pin
   initial: #f
   pred: portnumber-or-false?
   name: "actual https port ball kernel listens on"))

(define (front-trigger-install-vpn-forward!)
  (cond
   ;; FIXME: should handle change of server port too, i.e. take the old forwarding down! -> sequence:
   ((and (front-beaver-up??) (https-server-port2))
    (let ((addr (string-append "127.0.0.1:" (number->string (https-server-port2)))))
      (log-status "forwarding beaver vpn from " 443 " to " addr)
      (front-beaver-call `(ot0cli-process-commands '("-service" "vpn" "tcp" "forward" "443" ,addr)))))))
(define (front-trigger-get-http-server!)
  (when (ball-kernel-up??)
    (kick! (lambda () (https-server-port2 (call-kernel 'begin '($https-server-port)))))))
(wire! ball-kernel-up?? post: front-trigger-get-http-server!)
(if ball-kernel-found-up-on-startup (front-trigger-get-http-server!))
(wire! (list front-beaver-up?? https-server-port2)
       post: front-trigger-install-vpn-forward!)

(define-cached-kernel-value external-https-port #f (documented-external-https-port) 'begin '($external-port))

(define (satellite-port) 8443)

(define (start-satellite-script0 port name ssl)
  `(thread-start!
    (make-thread
     (lambda ()
       (logerr "I: satellite in ~a starting on ~a\n" (current-process-id) ,port)
       (and-let*
        ((user (mesh-cert-o (tc-private-cert))))
        (parameterize
         (($authenticate-www-user
           (let ((o ($authenticate-www-user)))
             (lambda (request peer-certificate)
               (or (o request peer-certificate)
                   (set-slot! request 'web-user ""))
               #t))))
         (if ,ssl
             (https-server
              "127.0.0.1" ,port
              `((require . ,($https-server-require-cert))
                (ca . ,(tc-ca-cert-file))
                (cert . ,(tc-private-cert-file))
                (key . ,(tc-private-key-file)))
              (lambda args #f) 'http-location-format
              (lambda (request)
                (enter-front-court
                 (lambda (request)
                   (set-slot! request 'caller user)
                   (web-handle-authenticated-service request user (document user) #f))
                 request)))
             (http-server
              "127.0.0.1" ,port (lambda args #f) 'http-location-format
              (lambda (request)
                (enter-front-court
                 (lambda (request)
                   (set-slot! request 'caller user)
                   (web-handle-authenticated-service request user (document user) #f))
                 request)))))
        ,name)))))

(define (override-meta-interface-script)
  `(let ((old ($meta-lookup-method))
         (ctrl (xml-parse (filedata ,(embedded-file '("lib") "metactrl.xml")))))
     ($meta-lookup-method
      (lambda (type)
        (case type
          ((read) (old type))
          ((write) ctrl)
          (else (error "$meta-lookup-method: pardon?")))))))

(define (start-satellite-script port name ssl)
  `(begin
     (if ln-satellite
         (logerr "ln-satellite in ~a is already ~a\n" (current-process-id) ln-satellite)
         (set! ln-satellite ,(start-satellite-script0 port name ssl)))
     ,(add-guard-fail
       "overiding meta-interface failed"
       (override-meta-interface-script))
     #t))

(define (kernel-satellite-variable-exits)
  (call-kernel
   'begin
   '(guard
     (ex (else #f))
     ln-satellite ;; raises exception if not existing
     (logerr "ln-satellite already defined as ~a in process ~a\n" ln-satellite (current-process-id))
     #t)))

(define satellite-protocol #;'https 'http)

(define (kernel-start-satellite!) ;; FIXME obsolete, remove
  (if (kernel-satellite-variable-exits)
      (log-error "kernel-start-satellite!: ln-satellite already defined")
      (call-kernel 'begin '(begin (define ln-satellite #f) #t)))
  (call-kernel 'begin (start-satellite-script (satellite-port) "satellite" (eq? satellite-protocol 'https)))
   ;; unconditionally returning success here, is this corect?
   #t)

(hook-add!
 kernel-on-init
 (lambda ()
   (kernel-on-start-add!
    "satellite"
    `(begin
       (if (not (guard
                 (ex (else #f))
                 ln-satellite ;; raises exception if not existing
                 (logerr "ln-satellite already defined as ~a in process ~a\n" ln-satellite (current-process-id))
                 #t))
           (set! ln-satellite #f))
       ,(start-satellite-script (satellite-port) "satellite" (eq? satellite-protocol 'https))))))

(define (reset-kernel-logging! #!key (on log:on) (to log:file))
  (call-kernel
   'begin
   `(begin
      (spool-db-exec "create table if not exists appstates (name text unique not null, val text)")
      (spool-db-exec/prepared "insert or replace into appstates (name, val) values ('logging', ?1)" ,(if on "yes" "no"))
      (spool-db-exec/prepared "insert or replace into appstates (name, val) values ('error log', ?1)" ,to)
      (spool-db-exec/prepared "insert or replace into appstates (name, val) values ('default log', ?1)" ,to))))

(define (reset-logging!)
  (log-reset!)
  (reset-kernel-logging!)
  (front-beaver-reset-logging!)
  (log-status "Log of " (public-oid) " " (allioideae-address))
  (call-kernel 'begin '(log-reset!)))

(hook-add!
 kernel-on-init
 (lambda ()
   (reset-kernel-logging!)
   (kernel-on-start-add!
    "define (log-reset!)"
    '(define (log-reset!)
       (if (equal? (sql-ref (spool-db-select "select val from appstates where name = 'logging'") 0 0) "yes")
           (let ((le (sql-ref (spool-db-select "select val from appstates where name = 'error log'") 0 0))
                 (lo (sql-ref (spool-db-select "select val from appstates where name = 'default log'") 0 0)))
             (when (file-exists? le)
               (set-log-output! 'error le)
               (set-log-output! 'output lo))))
       #t))))

(hook-add! kernel-on-init (lambda () (kernel-on-start-add! "redirect log" '(log-reset!))))

(define (public-oid-string)
  (let ((v (public-oid)))
    (if (symbol? v) (symbol->string v) "Public OID not found.\n
Is the service not yet running?")))

(define (main-entry-string) (if (main-entry) (symbol->string (main-entry)) *entry-missing*))

(define-once/or main-entry-name *entry-missing*
  (let ((e (assoc (main-entry) (map reverse (entry-points)))))
    (and e (cadr e))))

(define (refresh-entry-points-and-dependencies!)
  ;; FIXME: This calls for automatic deps mngmt.
  (entry-points #t)
  ;;(interesting-pages #t) ist actually dependent.
  #;(update-pages!)
  )

(define drop-selected-entry-point!
  (lambda ()
    (define name (car (dbget 'selected-channel #f)))
    (modal-if
     `("Really drop \"" ,name "\"? There is no way to recover!")
     (lambda ()
       (when (equal? name (main-entry-name))
	     (error "refusing to remove owner entry"))
       (when (member name '("public" "system"))
	     (error "refusing to remove essential entry" name))
       (let ((v (call-kernel 'begin `(drop-entry-point! ,name))
                #;(call-kernel "channel" "drop" name)))
         (refresh-entry-points-and-dependencies!))
       (dbclear 'selected-channel)
       #f))))

(define-cached-kernel-value kernel-connections #f '("n/a")
  'begin
  '(let ((v (fold
             (lambda (x i)
               (if (eq? x (public-oid)) i
                   (cons (if (oid? x) (oid->string x) x) i)))
             '()
             (quorum-others (http-all-hosts)))))
     (if (null? v) '("none") v)))

#;(define-once/or kernel-connections0 '("n/a")
  (with-output-to-string
    (lambda ()
      (display "Connections\n")
      (display "TBD: Format this in a useful way.\n")
      (display (call-kernel 'begin '(xml-format (sxml (display-http-channels))))))))

(define (ipconnect-string ip #!optional (port (external-https-port)) #!key (https #t))
  (with-output-to-string
    (lambda ()
      (display (if https "https://" "http://"))
      (let ((ps (if (if https
                        (= port 443)
                        (= port 80))
                    #f
                    (lambda () (display ":") (display port)))))
        (cond
         ((u8vector? ip)
          (display (ipaddr->string ip))
          #;(begin  ;; This works only for IPv4
	  (display (u8vector-ref ip 0))
	  (do ((i 1 (+ 1 i)))
          ((= i 4))
          (display ".")
          (display (u8vector-ref ip i)))))
         ((string? ip)
          (if (and ps (ipv6-address? ip))
              (begin
                (display #\[)
                (display ip)
                (display #\]))
              (display ip))))
        (if ps (ps))))))

(define (local-map-entry-name->oid name default)
  (let ((e (assoc name (entry-points))))
    (if e (symbol->string (cadr e)) default)))

(include "webview.scm")

(define (local-launchurl #!optional (path #f) #!key (https (eq? satellite-protocol 'https)))
  (let ((path (or path
                  (let ((p0 (uiget 'browse-path)))
                    (and p0 (local-map-entry-name->oid p0 p0)))
                  "")))
    (launch-url
     (string-append
      (ipconnect-string '#u8(127 0 0 1) (satellite-port) https: https)
       "/" path)))
  (case (subprocess-style)
    ((semi-fork fork) (terminate))
    (else
     ;; (glgui-suspend) ;; FIXME: minimize!
     #f)))

#;(define-once/or interesting-pages '()
  (append
   (list "")
   (map car (entry-points))))

(define interesting-pages
  (let ((value #f)
	(eps '()))
    (lambda (#!optional (refresh #f))
      (if (or refresh
	      (not (eq? eps (entry-points))))
	  (set! value #f))
      (unless (or value (eq? refresh 'void))
	      (set! eps (entry-points))
	      (set! value
		    (append
		     (list "")
		     (map car eps))))
      (or value '()))))

(define (help-url page)
  ;; FIXME: does NOT work on Android (anymore) as it replaces the
  ;; `file://` scheme with `file//` and prepends `http://`.
  (string-append "file://" (embedded-file '("lib" "help") page)))

(define (copy-allioideae-address-to-db)
  (when (and (allioideae-address)
             (not (member (dbget (dbget 'CN) #f) '(#f "localhost"))))
        (dbset 'CN (allioideae-address))))

(define (CN-entry-form-section)
  `((button text "Help on CN setup" action
            ,(lambda ()
               (launch-url (help-url "allioideae-setup.html"))
               #f))
    (textentry id CN text "CN:")
    ,(lambda ()
       (copy-allioideae-address-to-db)
       (if (clipboard-hascontent)
           `(button text "Paste From Clipboard" action
                    ,(lambda () (dbset 'CN (clipboard-paste)) #f))
           '(label text "nothing to paste in clipboard")))
    ))

(define pages-again-hook #f)

(define (update-pages!)
  (front-trigger-update-of-pages!)
  #f)

(define *front-pages-update-mux* (make-mutex '*front-pages-update-required*))
(define *front-pages-update-cv* (make-condition-variable '*front-pages-update-required*))

(define (front-trigger-update-of-pages!)
  ;;(mutex-lock! *front-pages-update-mux*)
  (mutex-specific-set! *front-pages-update-mux* #t)
  ;;(mutex-unlock! *front-pages-update-mux*)
  (condition-variable-signal! *front-pages-update-cv*))

(wire! (list ball-kernel-up?? front-beaver-up??) post: front-trigger-update-of-pages!)

(define *unset-code* "<no code>")

(define (is-unset? str)
  (member str (list #f "" *unset-code*)))

(define label-not-running '(label "Kernel Not Running"))

(define (check-settings-for-new-support)
  (define (assocdflt key lst dflt)
    (let ((e (and (pair? lst) (assoc key lst))))
      (or (and e (cadr e)) dflt)))
  (let* ((scanned (let ((v (dbget 'to-be-supported-code #f)))
		    (if (is-unset? v) #f
			(call-with-input-string v read))))
	 (from (if scanned
		   (let ((v (assocdflt 'public scanned #f)))
		     (if (symbol? v) (symbol->string v) v))
		   (dbget 'to-be-supported-from)))
	 (whom (if scanned
		   (assocdflt 'channel scanned #f)
		   (dbget 'to-be-supported-whom)))
	 (here (dbget 'to-be-supported-here whom))
	 (address (if scanned
		      (let ((e (assq 'ip-address scanned)))
			(and e (cadr e)))
		      #f)))
    (log-debug "Found support from: " 1 from " whom: " whom " here: " here)
    (when (is-unset? from) (error "host not specified"))
    (when (is-unset? whom) (error "party not specified"))
    (log-debug "Setting values back into db" 1)
    (dbset 'to-be-supported-code *unset-code*)
    (dbset 'to-be-supported-from from)
    (dbset 'to-be-supported-whom whom)
    (dbset 'to-be-supported-here here)
    (unless (kernel-host-lookup from)
	    (if address (kernel-send-connect address)
		(error "Could not find host" from " at " address))))
  'channels-support-verification)

(define pop-page
  (let ((stack '()))
    (case-lambda
     (() (if (pair? stack)
	     (let ((last (car stack))) (set! stack (cdr stack)) last)
	     'main))
     ((x) (set! stack (cons (uiget 'page) stack)) x)
     ((here go) (set! stack (cons here stack)) go))))

(define push-page
  (case-lambda
   ((x) (lambda () (pop-page x)))
   ((here go) (lambda () (pop-page here go)))))

(define *other-pages*
  '(("Connections" connections)
    ("Status" status)
    ("Allioideae" address)
    ("Manage" manage)
    ("Debug" debug)
    ))

(define (goto-other-page)
  (let ((e (assoc (uiget 'goto-location #f) *other-pages*)))
    (uiset 'goto-location #f)
    (and e (cadr e))))

(define (show-start-stop-buttons?)
  (or (not app:android?) (not-using-fork-alike)))

(define (uiform:pages again)
  (define upnrunning (and (ball-kernel-up??) (front-beaver-up??)))
  (define (again1 lst) (again (list->table lst)))
  (again1
   `(
     (main
      "Askemos"
      ("About" ,(push-page 'about))
      ("Ident" identification)
      (spacer height 50)
      (label text "Hello from Askemos" size header)
      (spacer)
#|
      (button h 75 size header indent 0.05 rounded #t text "???" action
	      ("I said choose yes!" ("OK" ("Again: are your sure" ("Yes" #f) ("Not really" #f))) ("NO" #f)))
      (button h 75 size header indent 0.05 rounded #t text "??? 1" action
	      ,(modal-if "Ich sagte nimm Ja!" #f))
      (button h 75 size header indent 0.05 rounded #t text "??? 2" action
	      ,(modal-if "I said choose yes!" #f left: "Jaja"))
      (button h 75 size header indent 0.05 rounded #t text "??? 3" action
	      ,(modal-if "I said choose yes!" #f left: "Naja" abort: 'about))
      (button h 75 size header indent 0.05 rounded #t text "??? 3" action
	      ,(modal-if '("Sure if " x " is invalid?") #f left: "Naja" abort: 'about))
|#
      ,@(cond
	 ((rep-exists?)
	  (if upnrunning
	      `(,@(if (show-start-stop-buttons?)
		      `((button h 75 size header #;(indent 0.05) rounded #f text "Stop" action
				,(lambda ()
                                   (kick!
                                    (lambda ()
                                      (front-beaver-enabled:= #f)
                                      (ball-kernel-enabled:= #f)))
				   (uiform:pages again)
				   'main))
			(spacer))
                      '())
		(dropdown indent 0.05 text "Go to" id browse-path location ui entries ,(interesting-pages))
		(button h 75 size header #;(indent 0.05) rounded #f text "Start Browser" action ,local-launchurl)
		(spacer)
		(button h 75 size header #;(indent 0.05) rounded #f text "Channels" action channels)
		(spacer)
		(dropdown indent 0.05 text "Advanced" id goto-location location ui entries ,(map car *other-pages*))
		(button h 75 size header #;(indent 0.05) rounded #f text "Go" action ,goto-other-page)

		)
              (let ((kick-start
                     (lambda ()
                       (kick!
                        (lambda ()
                          (front-beaver-enabled:= #t)
                          (ball-kernel-enabled:= #t)))
                       'starting)))
                (if (show-start-stop-buttons?)
                    `((button h 75 size header #;(indent 0.05) rounded #f text "Start" action ,kick-start)
                      (spacer)
                      (button h 75 size header #;(indent 0.05) rounded #f text "Manage" action manage))
                    `((redirect action ,kick-start))))))
	 (else
	  `((button h 75 size header indent 0.05 rounded #t text "Initialize" action init)
            (spacer)
            (button h 75 size header indent 0.05 rounded #t text "Manage" action manage))))
      ;; Show an image (file listed in EMBED)
      ;; (image file "LN_logo.png")
      )
     (starting
      "Starting"
      #f
      ("Status" ,(push-page 'status))
      (spacer height 50)
      (label text "Starting... stay tuned!")
      (spacer)
      (label text ,(lambda () (or (front-beaver-ip-address) "local address not available")))
      (redirect action
		,(lambda ()
                   #;(and (wait-for-kernel-server 1)
                        (begin
                          ;; (thread-sleep! 0.1)
                          (hook-run kernel-on-init) ;; FIXME: disable/remove once old installes are updated.
                          (public-oid #t)
                          (uiform:pages again)
                          'main))
                   (and (ball-kernel-up??) (front-beaver-up??)
                        (begin
                          (hook-run kernel-on-init) ;; FIXME: disable/remove once old installes are updated.
                          (public-oid #t)
                          (uiform:pages again)
                          'main))))
      ;; end of "starting" page
      )
     (init
      "Initialize"
      ("About" ,(push-page 'about))
      #f
      #;(label text ,(string-append "Initialize in: " kernel-data-directory) wrap #t)
      (spacer)
      (textentry id username text "User Name:")
      (spacer)
      ;; Don't store the password in the database table, just store it in the UI
      (textentry id password text "Password:" password #t location ui)
      (spacer)
      ,@(CN-entry-form-section)
      (spacer)
      (checkbox id setup-large text "Large Setup")
      (spacer)
      (button h 75 size header indent 0.05 rounded #t text "Initialize" action
	      ,(lambda ()
		 (let ((logname (dbget 'username "me"))
		       (passwd (uiget 'password))
		       (cn (dbget 'CN)))
		   (if passwd
		       (begin
			 (set! in-initialization 0.0)
			 (init-rep! cn logname passwd (dbget 'setup-large #f))
			 'initializing)
		       (begin
			 (error "password must not be empty")
			 #f)))))
      ;; end of "init" page
      )
     (initializing
      "Initializing"
      #f
      #f
      (spacer height 50)
      (label text "Initializing.  This may take several minutes to gather enough entropy to create good enough private keys.")
      (spacer)
      ,(lambda () (thread-sleep! 0.2) `(progress value ,(if (number? in-initialization) in-initialization 1)))
      (spacer)
      (redirect action ,(lambda () (if in-initialization #f (begin (uiform:pages again) 'main))))
      #;(button h 75 size header indent 0.05 rounded #t text "OK" action
	      ,(lambda ()
		 (uiform:pages again)
		 (if in-initialization 'initializing 'main)))
      ;; end of "initializing" page
      )
     (identification
      "Identify"
      ("Back" main)
      #f
      (spacer)
      (label text ,(string-append "Host: " (public-oid-string)))
      (spacer)
      (label text ,(string-append "Party: " (main-entry-name)))
      (spacer)
      (label text ,(string-append "Party ID: " (main-entry-string)))
      (spacer)
      ,(my-ip-address-display update-pages!)
      (spacer)
      ,@(if (and (public-oid) (have-local-connect?))
	    (let ((idstr (with-output-to-string
		       (lambda ()
			 (pretty-print
			  `((public ,(public-oid))
			    (channel ,(main-entry-name))
			    (ip-address ,(local-connect-string))
			    ;; TBD: add certificate hash and set auth to fixed!
			    ))))))
	      `((dmencode text ,idstr)))
	    '())
      (spacer)
      ;; end of "identification" page
      )
     (connections
      "Connections"
      ("Back" main)
      #f
      (spacer)
      (label text ,(if (symbol? (public-oid)) (public-oid-string) "Error retrieving public OID."))
      (spacer)
      ,@(if (and (have-local-connect?) (allioideae-address))
            `((dmencode text ,(local-connect-string)))
            `(,(my-ip-address-display update-pages!)
              (dmencode text ,(local-connect-string))))
      #;,@(let ((connstr (and (have-local-connect?) (local-connect-string))))
	  `((label text ,(if connstr (string-append "my IP: " connstr) "No IP address known!"))
	    ,(if connstr
		 (my-ip-address-display update-pages!)
		 `(dmencode text ,connstr))
	    (spacer)))
      (button h 75 size header indent 0.05 rounded #t text "Connect" action
	      ,(lambda ()
		 (let ((to (dbget 'connecturl "")))
		   (if (not (is-unset? to))
		       (kernel-send-connect to)
		       (kernel-connections 'void)))
		 #f))
      (spacer)
      (textentry id connecturl text "Connect To")
      (spacer)
      (label text "Touch button to take photo and\nscan it for DM barcode to connect to:" wrap #t)
      (dmdecode id connecturl default ,*unset-code*)
      ;; end of "connections" page
      )
     (channels
      "Channels"
      ("Back" ,(lambda () (dbclear 'selected-channel) 'main))
      #f
      (spacer)
      (button h 50 #;(size header indent 0.05 rounded #t) text "Identify" action identification)
      (spacer)
      (button h 50 #;(size header indent 0.05 rounded #t) text "Support" action ,(push-page 'channels-support))
      (spacer)
      ,(lambda ()
	 (if upnrunning
	     `(checklist id selected-channel default ,(map car (entry-points)) radio #t)
	     label-not-running))
      (spacer)
      ,(lambda ()
	 (let ((sel (dbget 'selected-channel #f)))
	   (if sel
	       `(button h 50 text "Drop" action ,drop-selected-entry-point!)
	       '(label text "No channel selected."))))
      ;; end of "channels" page
      )
     (channels-support
      "Channel Support"
      ("Back" ,pop-page)
      #f #;("Verfiy" channels-support-verification)
      (spacer)
      (textentry id to-be-supported-from text "From Host")
      (spacer)
      (textentry id to-be-supported-whom text "Support Party")
      (spacer)
      (textentry id to-be-supported-here text "Here As")
      (spacer)
      (button h 50 size header #;(indent 0.05 rounded #t) text "Support" action
	      ,check-settings-for-new-support)
      (spacer)
      (button h 50 size header #;(indent 0.05 rounded #t) text "Clear" action
	      ,(lambda ()
		 (dbset 'to-be-supported-code *unset-code*)
		 (dbset 'to-be-supported-from "")
		 (dbset 'to-be-supported-whom "")
		 (dbset 'to-be-supported-here "")
		 'channels-support))
      (spacer)
      (label text "Touch button to take photo and\nscan it for DM barcode:" wrap #t)
      (spacer)
      ;; Note: dmdecode is currently buggy, all input underneath is vertially off!
      ;; Better leave it the last thing on the page.
      (dmdecode id to-be-supported-code default ,*unset-code*)
      ;; end of "channels-connect" page
      )
     (channels-support-verification
      "Verify New Account"
      ("Back" channels-support)
      #f
      (spacer)
      ,(lambda () `(label text ,(string-append "From: " (dbget 'to-be-supported-from ""))))
      (spacer)
      ,(lambda () `(label text ,(string-append "Name: " (dbget 'to-be-supported-whom ""))))
      (spacer)
      ,(lambda () `(label text ,(string-append "Local Alias: " (dbget 'to-be-supported-here ""))))
      (spacer)
      (button h 75 size header #;(indent 0.05 rounded #t) text "Confirm" action
	      ,(lambda ()
		 (kernel-send-support
		  (dbget 'to-be-supported-from)
		  (dbget 'to-be-supported-here)
		  (dbget 'to-be-supported-whom))
		 (entry-points #t)
		 'channels))
      ;; end of "channels-support-verification" page
      )
     (address
      "Address"
      ("Back" ,pop-page)
      #f
      (spacer)
      ,@(CN-entry-form-section)
      (spacer)
      (textentry id password text "Password:" password #t location ui)
      (spacer)
      (button text "Sign New Cert" action
              ,(lambda ()
                 ;; ERROR "failed to add subject component" with .i2p:<portnumber>
                 (kernel-send-set-auth 'tofu (main-entry-name) (uiget 'password) (dbget 'CN))
                 ;; ERROR: Does not refresh.
                 (chached-kernel-values-flush!)
                 'identification))
      ;; end of "address" page
      )
     (status
      "Status"
      ("Back" ,pop-page)
      #f
      (redirect action ,(lambda () (if upnrunning #f 'main)))
      (spacer)
      (label text ,(string-append "Data directory: " kernel-data-directory) wrap #t)
      (spacer)
      (label text ,(string-append "Kernel running as: " (symbol->string (subprocess-style))))
      ;;; (spacer) (label text ,(lambda () (string-append "Address: " (or (front-beaver-ip-address) "local address not available"))))
      (spacer)
      ,(my-ip-address-display update-pages!)
      (spacer)
      (button text "Network Connections (Refresh List)" action
	      ,(lambda ()
		 (local-connect-string #t)
		 (kernel-connections #t)
                 (update-pages!)
		 #f))
      (spacer)
      (list id remote-host entries ,(kernel-connections))
      (spacer)
      #;,(lambda ()
         (cond
          ((not app:android?) '(label text "No Orbot control on this platform"))
          ((not (orbot-running?))
           `(button text "Orbot not detected" action ,(lambda () (orbot-running!) 'main)))
          (else '(label text "Orbot should be running"))))
      #;,(lambda () `(label align left text ,kernel-connections))
      ;; end of "status" page
      )
     (manage
      "Manage"
      ("Back" ,pop-page)
      #f
      (spacer)
      ,@(let ((comeback (lambda (job) (lambda () (job) (update-pages!)))))
          (if (rep-exists?)
              `((button text "Backup" action ,(comeback kernel-backup!))
                (spacer)
                (button text "Remove" action
                        ,(modal-if "Sure? Got a backup?" (comeback kernel-remove!))))
              `((button text "Initialize" action init)
                (spacer)
                (button text "Restore" action
                        ,(lambda () (and (kernel-restore!) (begin (update-pages!) 'main)))))))
      (spacer)
      (button text "Reset Logging" action ,(lambda () (reset-logging!) 'main))
      (spacer)
      ,(lambda ()
         (if (not upnrunning)
             `(button text "Remove SpoolDB" action
                      ,(lambda ()
                         (for-each
                          (lambda (x)
                            (let ((fn (make-pathname kernel-data-directory x)))
                              (if (file-exists? fn) (delete-file fn))))
                          '("spool.db" "spool.db-journal"))
                         'main))
             `(button text "Force Spool GC" action
                      ,(lambda ()
                         (call-kernel 'begin '(and (future (let ((store (spool-directory))) (fsm-enforce-restore! store) (fsm-run-now store))) #t))
                         'main))))
      ;; end of "manage" page
      )
     (about
      "About"
      ("Back" ,pop-page)
      #f
      (spacer height 50)
      (label text ,(string-append "Version: " (system-appversion)))
      (spacer)
      (label text "This is a second draft of the control app for Askemos/BALL. See also:")
      (button h 50 size normal indent 0.05 rounded #t text "askemos.org" action
              ,(lambda () (launch-url "http://ball.askemos.org") #f))
      (spacer)
      (label text "Copyright (C) 2000-2019\nJörg F. Wittenberger")
      (spacer)
      (label text ,(string-append "Data directory: " kernel-data-directory) wrap #t)
      (spacer)
      ,@(if (and app:android?) '()
            `(,(if (not-using-fork-alike)
                   '(label text "Exiting here will terminate the service!")
                   '(spacer))
              (spacer)
              (button h 75 size header indent 0.05 rounded #t text "Exit" action
                      ,terminate #;(lambda ()
                      (if (not-using-fork-alike)
                      (begin
                      (stop-kernel-server!)
                      (thread-sleep! 1)
                      (terminate))
                      (terminate))))))
      (spacer)
      ,@(if (eq? (subprocess-style) 'fork)
            `((button text "Kill Kernel Server" action ,(lambda () (kernel-server-kill!) (front-beaver-stop!) (update-pages!)))
              (spacer)
              (button text "Restart Kernel Server" action ,(lambda () (call-kernel 'begin '(exit 1)) (front-beaver-stop!) (chached-kernel-values-flush!) (update-pages!))))
            '((spacer)))
      #;,(lambda ()
	 (if upnrunning
	     `(button h 50 size header indent 0.05 rounded #t text "Debug" action ,(lambda () 'debug))
	     '(spacer)))
      (spacer)
      #;(button h 75 size header indent 0.05 rounded #t text "Load Overwrite" action
      ,(lambda ()
      (load "overwrite.scm")
      (uiform:pages again)
      'main))
      ;; end of "about" page
      )
     (debug
      "Debug"
      ("Main" main)
      #f
      ,@(if upnrunning
	    `(
              (button text "Create Test Account" action ,(lambda () (kernel-add-reflexive-entry! "test") 'main))
              (spacer)
              (label text "Input")
	      (multilinetextentry id testinput lines 5)
	      (spacer)
	      (button
	       h 40 text "Send" action
	       ,(lambda ()
		  (let ((in (call-with-input-string (dbget 'testinput "#t") read-all)))
		    (receive
		     results (debug 'GotBack (apply call-kernel 'begin in))
		     (dbset 'testresults
			    (with-output-to-string
			      (lambda ()
				(for-each
				 (lambda (r) (pretty-print r) (newline))
				 results))))))
		  (uiform:pages again)
		  'debug))
	      (label text "Results")
	      #;(multilinetextentry id testoutput location ui lines 5)
	      (label align left text ,(dbget 'testresults "") wrap #t)
	      )
	    '())
      ;; end of "debug page"
      )
     )))

(define (update-pages2!)
  (if (procedure? pages-again-hook)
  (pages-again-hook))
  #f)

(define dispatch-events
  (let ((check-magic-keys
	 (lambda (gui t x y)
	   ;; (debug 'event t)
	   (when (= t EVENT_KEYPRESS)
		 (if (= x EVENT_KEYESCAPE)
		     (terminate))))))
    (cond
     #;(app:android? ;;(member (system-platform) '("android"))
      (lambda (gui t x y)
        (##thread-heartbeat!)
        (thread-yield!)
	(cond
         ((eq? t EVENT_IDLE)
          ;; (log-debug "idle" 1)
          #t)
         (else
          ;; (check-magic-keys gui t x y)
          (glgui-event gui t x y)))))
     (else
      (let ((frame-period 0.7)
            (step 0.07)
            (count 0))
        (lambda (gui t x y)
          (thread-yield!)
	  (check-magic-keys gui t x y)
	  (cond
           ((eq? t EVENT_IDLE)
            #t)
	   ((= t EVENT_REDRAW)
            (let loop ()
              (when (mutex-lock! *front-pages-update-mux*)
                (cond
                 ((mutex-specific *front-pages-update-mux*)
                  (let loop2 ()
                    (mutex-specific-set! *front-pages-update-mux* #f)
                    (update-pages2!)
                    (thread-yield!)
                    (if (mutex-specific *front-pages-update-mux*)
                        (loop2)
                        (begin
                          (glgui-event gui t x y)
                          (set! count 0)
                          (mutex-unlock! *front-pages-update-mux*)))))
                 (else
                  (glgui-event gui t x y)
                  (cond
                   ((expecting-progress) (mutex-unlock! *front-pages-update-mux*))
                   ((mutex-unlock! *front-pages-update-mux* *front-pages-update-cv* (min frame-period (* count step)))
                    (set! count 0)
                    (loop))
                   (else (set! count (fx+ count 1)))))))))
           ((eq? t 126) (LNjScheme-result))
	   (else
            (set! count 0)
	    (unless
	        (= t EVENT_MOTION)
	      (glgui-event gui t x y))))))))))

(define (run-gui!)
  (define gui #f)
  (define form #f)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;; code ;;;;;;;;;;;;;;;;;
  (main
   ;; initialization
   (lambda (w h)

     (redirect-standard-ports-for-logging)
     (log-status
      "compiled for "
      (cond-expand
       (android 'android)
       (linux 'linux)
       (win32 'win32)
       (else 'unknown))
      " subprocess-style: " (subprocess-style))
     (unless (migrate-data-to-protected-space!)
             (display "ERROR data migration failed.  Exiting.\n" (current-error-port))
             (exit 1))

;; initialize gui here
     (make-window 480 800)
     (glgui-orientation-set! GUI_PORTRAIT)
     (set! gui (make-glgui))

     (let ((aw (glgui-width-get))
	   (ah (glgui-height-get)))
       (glgui-box gui 0 0 aw ah DarkGreen)
       (set! form (glgui-uiform gui 0 0 aw ah)))

     ;; Set the sandbox up to be the current directory and use the above example as the script
     (glgui-widget-set! gui form 'sandbox (system-directory))
     #;(uiform:pages (lambda (pages) (glgui-widget-set! gui form 'uiform pages)))
     (set! pages-again-hook (lambda () (uiform:pages (lambda (pages) (mutex-specific-set! *front-pages-update-mux* #f) (glgui-widget-set! gui form 'uiform pages)))))
     (update-pages2!)

     ;; Set the fonts
     (glgui-widget-set! gui form 'fnt ascii_18.fnt)
     (glgui-widget-set! gui form 'smlfnt ascii_14.fnt)
     (glgui-widget-set! gui form 'hdfnt ascii_24.fnt)
     (glgui-widget-set! gui form 'bigfnt ascii_40.fnt)

     ;; Create the table to store data (default location for widget values)
     (glgui-widget-set! gui form 'database (make-table))

     (unless (dbget 'username) (dbset 'username "me"))
     (unless (dbget 'CN) (dbset 'CN "localhost"))
     (cond-expand
      (android
       (let ((appdir (object->string
                      (jscheme-eval
                       `(let* ((app ,(android-app-class))
                               (this ((method "me" app)))
                               (String (lambda (x) (new "java.lang.String" x)))
                               )
                          (let (
                                (getApplicationContext (method "getApplicationContext" app))
                                ;; getDataDir is new in API24 and deprecated
                                ;; (getDataDir (method "getDataDir" "android.content.Context"))
                                (getDataDir
                                 (let ((getFilesDir (method "getFilesDir" "android.content.Context"))
                                       (getParent (method "getParent" "java.io.File")))
                                   (lambda (ctx) (getParent (getFilesDir ctx)))))
                                )
                            (getDataDir (getApplicationContext this))))))))
         (log-status "appdir is " appdir)
         (log-status
          "content: "
          (call-with-input-process
           `(path: "/system/bin/ls" arguments: ("-l" ,(string-append appdir "/files/")) #;("-lR" ,(string-append appdir "/lib/")))
           (lambda (port) (read-line port #f)))))
       )
      (else #f)))
   ;; events
   (lambda (t x y) (dispatch-events gui t x y))
   ;; termination
   (lambda () #t)
   ;; suspend
   (lambda ()
     (glgui-suspend) #;(terminate)
     )
   ;; resume
   (lambda ()
     (update-pages2!)
     (glgui-resume))
   ))

#;(register-command! "tsrv" (lambda (args) (load "overwrite.scm") (set! kernel-server #t) (pseudo-server-task) ((debug 'exiting-via exit) 0)))

#|
(debug 'argc (system-cmdargc))
(debug 'commands *registered-commands*)
|#

(cond-expand
 (debug
  (if (file-exists? "overwrite.scm") (load "overwrite.scm")))
 (else #f))

(cond
 ((and (>= (system-cmdargc) 3) (equal? (system-cmdargv 1) "-s"))
  (execute-registered-command))
 (else (run-gui!)))

;; eof
