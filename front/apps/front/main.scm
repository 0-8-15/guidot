;; frontend (to be changed to mimic the Calculator demo)

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
  #;(setup-heartbeat!)
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

(define (kernel-send-idle)
  #;(cond-expand
   (android
    (unless (orbot-running?)
            (log-error "Orbot not running")
            (orbot-running!)))
   (else))
  (kernel-control-send! 'idle))

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

(define (is-allioideae? name) (or (is-onion? name) (is-garlic? name)))

;; `documented-external-https-port` is actually a constant and maybe
;; should be(come) a macro.
;;
;; We need this because so far we can't make Android I2P use a
;; different external port than the one we forward to.
(define (documented-external-https-port) 7443)

(define (kernel-config-socks-script cn)
  `(if ,(is-allioideae? cn)
       (begin
         ($https-use-socks4a #t)
         ($https-socks4a-server ,(if (is-garlic? cn) "127.0.0.1:9051" "127.0.0.1"))
         ($external-port 443)
         ($external-address ,cn))
       ($https-use-socks4a #f)))

(define (kernel-send-set-auth kind user password cn)
  (unless (call-kernel
           'begin
           `(and (install-x509-default ,user ,password ,cn)
                 (begin
                   ;; not exported ($tc-tofu ,(equal? kind 'tofu))
                   (local-id ,cn)
                   ,(kernel-config-socks-script cn)
                   (ball-save-config)
                   #t)))
	  (error "failed to set auth" kind user cn)))

(define (get-channels-command #!optional (source #f) (oid? #f))
  (define from
    (cond
     ((not source) '(my-oid))
     ((string? source)
      (if oid? `(string->oid ,source) `(entry-name->oid ,source)))
     (else source)))
  (define ff
    (if source
	'`((,k ,v) . ,i)
	'(if (equal? k "public") i `((,k ,v) . ,i))))
  `(let ((links (fget (find-local-frame-by-id ,from 'gui-client) 'mind-links)))
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
	 fork-process
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
             (kernel-start-custom-services!)))))))

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
		   (log-status "Initialization done.")
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
(define-macro (define-once/or name missing . body)
  (let ((refresh (gensym 'refresh))
	(value (gensym name)))
    `(define ,name
       (let ((,value %local-void-value))
	 (lambda (#!optional (,refresh #f))
	   (if ,refresh (set! ,value %local-void-value))
	   (if (and (or (not ,value) (%is-local-void? ,value)) (not (eq? ,refresh 'void)))
               (set! ,value (begin . ,body)))
           ;; FIXME: That's a bit strange: these values can NOT be #f!!!
	   (if (or (%is-local-void? ,value) (not ,value)) ,missing ,value))))))

(define *entry-missing* "<Entry Missing>")

(define-once/or local-connect-string *entry-missing*
  (and (check-kernel-server!)
       (let ((ip (allioideae-address)))
         (if ip (set! ip (ipconnect-string ip 443)))
	 (if ip
	     (set! myip ip)
	     (set! ip myip))
	 ip)))

(define (have-local-connect?)
  (not (eq? (local-connect-string) *entry-missing*)))

(define (my-ip-address-display #!optional (postproc #f))
  `(button
    h 75 size normal ;; indent 0.05 rounded #t
    text
    ,(if (have-local-connect?)
	 (local-connect-string)
	 "Find Address")
    action
    ,(lambda ()
       (unless (have-local-connect?)
	       (when (and (check-kernel-server!)
			  (eq? (local-connect-string #t) *entry-missing*))
		     (set! myip (let ((myip (host-ipaddr)))
                                  (and myip (ipconnect-string myip (external-https-port))))))
	     (if (procedure? postproc) (postproc)))
       #f)))

(define-macro (define-cached-kernel-value name valid? convert . query)
  (let ((cache (gensym name))
	(reload (gensym))
	(tmp (gensym)))
    `(define ,name
       (let ((,cache %local-void-value))
	 (lambda (#!optional (,reload #f))
	   (if (or ,reload (%is-local-void? ,cache))
	       (if (check-kernel-server!)
		   (let ((,tmp (call-kernel . ,query)))
		     (if (procedure? ,convert) (set! ,tmp (,convert ,tmp)))
		     (if (or (not ,valid?) (,valid? ,tmp))
			 (set! ,cache ,tmp)))))
	   (if (%is-local-void? ,cache)
	       (if (procedure? ,convert) (,convert) ,convert)
	       ,cache))))))

(define-cached-kernel-value allioideae-address
  #f (lambda (#!optional (v #f)) (if (equal? v "") #f v))
  'begin '($external-address))

(define-cached-kernel-value public-oid #f #f 'begin '(public-oid) #;("print" "public"))

(define-cached-kernel-value entry-points #f '() 'begin (get-channels-command))

(define-cached-kernel-value main-entry #f #f 'begin '(mesh-cert-o (tc-private-cert)))

(define-cached-kernel-value https-server-port #f 7443 'begin '($https-server-port))

(define-cached-kernel-value external-https-port #f 7443 'begin '($external-port))

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
         (begin
           (set! ln-satellite ,(start-satellite-script0 port name ssl))
           (debug 'SatelliteIsNow ln-satellite)))
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

(define (kernel-start-satellite!)
  (if (kernel-satellite-variable-exits)
      (log-error "kernel-start-satellite!: ln-satellite already defined")
      (call-kernel 'begin '(begin (define ln-satellite #f) #t)))
  (call-kernel 'begin (start-satellite-script (satellite-port) "satellite" (eq? satellite-protocol 'https)))
   ;; unconditionally returning success here, is this corect?
   #t)

(hook-add! kernel-on-start kernel-start-satellite!)

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
      (cond
       ((u8vector? ip)
	(display (ipaddr->string ip))
	#;(begin  ;; This works only for IPv4
	  (display (u8vector-ref ip 0))
	  (do ((i 1 (+ 1 i)))
	      ((= i 4))
	    (display ".")
	    (display (u8vector-ref ip i)))))
       ((string? ip) (display ip)))
      (unless (if https
                  (= port 443)
                  (= port 80))
              (display ":")
              (display port)))))

(define (local-map-entry-name->oid name default)
  (let ((e (assoc name (entry-points))))
    (if e (symbol->string (cadr e)) default)))

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
  (if (procedure? pages-again-hook)
      (pages-again-hook))
  #f)

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

(define (uiform:pages again)
  (define upnrunning (check-kernel-server!))
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
	      `(,@(if (or app:android? (not-using-fork-alike)) '()
		      `((button h 75 size header #;(indent 0.05) rounded #f text "Stop" action
				,(lambda ()
				   (stop-kernel-server!)
				   (uiform:pages again)
				   'main))
			(spacer)))
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
                       (thread-start! (make-thread start-kernel-server! 'starting))
                       'starting)))
                (if app:android?
                    `((redirect action ,kick-start))
                    `((button h 75 size header #;(indent 0.05) rounded #f text "Start" action ,kick-start)
                      (spacer)
                      (button h 75 size header #;(indent 0.05) rounded #f text "Manage" action manage))))))
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
      (redirect action
		,(lambda ()
                   (and (wait-for-kernel-server 1)
                        (begin
                          ;; (thread-sleep! 0.1)
                          (kernel-start-custom-services!)
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
      "Idendify"
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
      ("Back" main)
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
                 (kernel-send-set-auth 'tofu (main-entry-name) (uiget 'password) (dbget 'CN))
                 (allioideae-address #t)
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
      ,(my-ip-address-display update-pages!)
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
      ,@(if (or app:android?) '()
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
      ,(if (eq? (subprocess-style) 'fork)
           `(button text "Kill Kernel Server" action ,(lambda () (kernel-server-kill!) (update-pages!)))
           '(spacer))
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

(define dispatch-events
  (let ((check-magic-keys
	 (lambda (gui t x y)
	   ;; (debug 'event t)
	   (when (= t EVENT_KEYPRESS)
		 (if (= x EVENT_KEYESCAPE)
		     (terminate))))))
    (cond
     (app:android? ;;(member (system-platform) '("android"))
      (lambda (gui t x y)
        (##thread-heartbeat!)
        (thread-yield!)
	(cond
         ((eq? t EVENT_IDLE)
          ;; (log-debug "idle" 1)
          #;(kernel-send-idle)
          #t)
         (else
          ;; (check-magic-keys gui t x y)
          (glgui-event gui t x y)))))
     (else
      (let ((last-redraw 0)
	    (wait-step 0.2)
	    (max-count 10))
	(lambda (gui t x y)
          (thread-yield!)
	  (check-magic-keys gui t x y)
	  (cond
           ((eq? t EVENT_IDLE)
            ;; (log-debug "idle" 1)
            (kernel-send-idle)
            #t)
	   ((and (= t EVENT_REDRAW) (not (expecting-progress)))
	    (glgui-event gui t x y)
	    (when
	     (> last-redraw 0)
	     (thread-sleep! (* wait-step last-redraw)))
	    (when (< last-redraw max-count) (set! last-redraw (+ 1 last-redraw))))
	   (else
	    (set! last-redraw -2)
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
     (set! pages-again-hook (lambda () (uiform:pages (lambda (pages) (glgui-widget-set! gui form 'uiform pages)))))
     (update-pages!)

     ;; Set the fonts
     (glgui-widget-set! gui form 'fnt ascii_18.fnt)
     (glgui-widget-set! gui form 'smlfnt ascii_14.fnt)
     (glgui-widget-set! gui form 'hdfnt ascii_24.fnt)
     (glgui-widget-set! gui form 'bigfnt ascii_40.fnt)

     ;; Create the table to store data (default location for widget values)
     (glgui-widget-set! gui form 'database (make-table))

     (unless (dbget 'username) (dbset 'username "me"))
     (unless (dbget 'CN) (dbset 'CN "localhost"))

     )
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
     (update-pages!)
     (glgui-resume))
   ))

(register-command! "ball" ballroll)

#;(register-command! "tsrv" (lambda (args) (load "overwrite.scm") (set! kernel-server #t) (pseudo-server-task) ((debug 'exiting-via exit) 0)))

#|
(debug 'argc (system-cmdargc))
(debug 'commands *registered-commands*)
|#

(cond
 ((and (>= (system-cmdargc) 3) (equal? (system-cmdargv 1) "-s"))
  (execute-registered-command))
 (else (run-gui!)))

;; eof
