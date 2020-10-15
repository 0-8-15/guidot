(log-status "Startup")

(include "../glgui/DejaVuSans-14,24,32.scm")

(utf8string->unicode:on-encoding-error 'replace)

(define (debug l v)
  (let ((p  (current-error-port)))
    (display l p)
    (display ": " p)
    (write v p)
    (newline p)
    (force-output p)
    v))

(cond-expand
 ((or #;android debug)
  ($debug-trace-triggers #t)
  ($async-exceptions 'catch)
  ) (else #f))

(cond-expand
 (android
  (define (setup-heartbeat!)
    #;((c-lambda () scheme-object "___setup_heartbeat_interrupt_handling"))
    (##set-heartbeat-interval! (exact->inexact 1/100)))
  (setup-heartbeat!)
  ($kick-style 'sync) (kick/sync! (lambda () (kick-style 'sync)))
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

(cond-expand
 (android
  (let ((ot terminate)) (set! terminate (lambda () (log-error "No terminate on Android!") #f)))
  )
 (else
  ))

(define origin-pluto
  '#u8(1 0 0 0 0 0 2 12 196 0 0 1 114 132 86 202 149 41 135 178 127 21 12 185 196 179 100 108 19 209
         252 189 247 161 197 44 83 32 42 52 220 81 184 143 44 119 13 83 53 29 98 90 219 141 177 68 120
         155 57 144 189 86 15 185 70 201 245 43 18 226 161 84 23 246 109 48 209 68 204 202 47 97 163 15
         199 96 71 104 219 89 241 167 59 249 235 113 226 79 197 209 29 234 84 208 109 22 36 14 192 5
         179 229 69 179 11 3 12 7 144 182 48 57 100 255 181 251 244 0 111 59 129 82 50 42 105 144 49 64
         186 235 94 39 80 219 4 226 121 18 225 11 191 189 21 44 86 166 195 215 219 105 174 187 164 139
         242 194 73 73 205 77 15 130 205 121 219 26 169 1 65 133 26 249 245 0 140 254 80 210 116 111 15
         96 174 78 204 217 89 144 36 110 150 247 41 145 144 206 55 223 193 137 246 27 143 65 195 50 13
         206 75 16 31 65 239 227 174 114 47 51 19 96 194 13 235 227 213 240 90 36 166 243 223 170 143
         214 155 17 227 249 0 1 4 185 163 119 119 39 9))

;;(define use-origin #f)
(define use-origin origin-pluto)

(define (pin-attach-log! pin msg)
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

(define (forward-logging-to-daemonian!)
  (let ((to (and log:on log:file)))
    (daemonian-stdout-file to)
    (daemonian-stderr-file to)))

(thread-start!
 (make-thread
  (lambda ()
    (do () (#f)
      (thread-sleep! (* 24 36 60)) ;; 1d
      (log-reset!)
      (kick! forward-logging-to-daemonian!)))
  'reset-log))

;; GUI helpers

(define (update-pages!)
  (glgui-wakeup!)
  #f)

(cond-expand
 (android
  (define-macro (please-do-me-the-favor-and-make-progress! n)
    `(begin
       (##thread-heartbeat!)
       (thread-sleep! 0.01)))
  (define-macro (DENIED:please-do-me-the-favor-and-make-progress! n)
    (if (= n 0)
        `(begin
           (##thread-heartbeat!)
           (thread-sleep! 0.01))
        `(do ((i 0 (fx+ i 1)))
             ((fx= i ,n) (thread-sleep! ,(* n 0.01)))
           (##thread-heartbeat!)
           (thread-yield!))))
  (define %%Xredraw #f)
  (define (Xtrigger-redraw!)
    (glgui-wakeup!)
    (set! %%Xredraw #t))
  (define (Xconditional-redraw)
    (if %%Xredraw
        (begin
          (set! %%Xredraw #f)
          #;(glgui-suspend)
          (set! glCore:needsinit #t)
          #t)
        #f))
  )
 (else
  (define-macro (please-do-me-the-favor-and-make-progress! n) '#!void)
  (define Xtrigger-redraw! glgui-wakeup!)
  (define (Xconditional-redraw) #f)
  ))

(define glgui-dispatch-event
  (let ((check-magic-keys
	 (lambda (gui op t x y)
	   ;; (debug 'event t)
	   (when (= t EVENT_KEYPRESS)
		 (if (= x EVENT_KEYESCAPE)
		     (terminate))))))
    (cond
     #;(app:android? ;;(member (system-platform) '("android"))
      (lambda (gui op t x y)
        (##thread-heartbeat!)
        (thread-yield!)
	(cond
         ((eq? t EVENT_IDLE)
          ;; (log-debug "idle" 1)
          #t)
         (else
          ;; (check-magic-keys gui op t x y)
          (glgui-event gui t x y)))))
     (else
      (lambda (gui update-pages-now! t x y)
        (please-do-me-the-favor-and-make-progress! 0)
	(check-magic-keys gui update-pages-now! t x y)
        (Xconditional-redraw)
	(cond
         ((eq? t EVENT_REDRAW)
          ;; (log-status "REDRAW")
          (glgui-event gui t x y))
         ((eq? t EVENT_IDLE)
          (please-do-me-the-favor-and-make-progress! 2)
          #; (log-status "IDLE")
          )
         ((eq? t 126) (kick! (lambda () (LNjScheme-result))))
	 (else
          (kick! (lambda () (glgui-event gui t x y)))
          #;(please-do-me-the-favor-and-make-progress! 1)
          #;(Xconditional-redraw)
          #;(if glCore:needsinit (please-do-me-the-favor-and-make-progress! 1))
          ;;(if (Xconditional-redraw) (glgui-event gui EVENT_REDRAW 0 0))
          #;(begin (glgui-suspend) (glgui-resume))
          )))))))

(define (handle-replloop-exception e)
  (cond
   ((unbound-global-exception? e)
    (println port: (current-error-port) "Unbound variable " (unbound-global-exception-variable e)))
   (else (display-exception e (current-error-port))))
  #!void)

(define (replloop) ;; interactive read-evaluate-print-loop
  (with-exception-catcher handle-replloop-exception (lambda () (##repl-debug #f #t)))
  (replloop))

;; LambdaNative glgui frame -- it's a bit tricky to work around that one.

(define *glgui-main* main)

(define (glgui-run init #!key
                   (events glgui-dispatch-event)
                   (suspend glgui-suspend)
                   (resume glgui-resume)
                   (terminate (lambda () #t)))
  (foreground-service! #t)
  (let ((gui #f)
        (update! (lambda _ #f))
        (once *glgui-main*))
    (set! *glgui-main* (lambda _ (exit 42)))
    (once
     ;; initialization
     (lambda (w h)
       (with-exception-catcher
        (lambda (exn)
          (handle-replloop-exception exn)
          (exit 32))
        (lambda ()
          (receive (g u) (init w h)
            (set! gui g)
            (set! update! (or u (lambda () #f)))))))
     ;; events
     (lambda (t x y) (events gui update! t x y))
     ;; termination
     terminate
     ;; suspend
     suspend
     ;; resume
     resume
     )))

#|
(glgui-run
 (lambda (w h)
     (make-window 320 480)
     (glgui-orientation-set! GUI_PORTRAIT)
     (let ((gui (make-glgui)))
       ;; initialize gui here
       (values gui #f)))
 suspend: terminate)
|#


(define beaver-stdout-redirection #t)

(register-command!
 "cerberus"
 (lambda (args)
   (cerberus (system-cmdargv 0) (cdr args) startup-delay: 2 max-fast-restarts: 2
             stdout-redirection: beaver-stdout-redirection)))

(register-command! "beaver" beaver-process-commands)

(include "~~tgt/lib/onetierzero/src/observable-notational-conventions.scm")

(include "chat.scm")

(define chat-dir "beaver")

(cond-expand
 (android
  (set! chat-dir (make-pathname (system-appdirectory) chat-dir)))
 (else #f))

(define (debug-adhoc-network-port) 3333)

(define (debug-adhoc-network-id)
  (let* ((a (debug-adhoc-network-port))
         (o a))
    (ot0-adhoc-network-id a o)))

(define (remreplloop)
  (define out (current-output-port))
  (display "\n;>> " out)
  (force-output out)
  (let ((expr (read (current-input-port))))
    (unless (eof-object? expr)
      (write (eval expr) out)
      (newline out)
      (force-output out)
      (remreplloop))))

(define (replloop2) ;; interactive read-evaluate-print-loop
  (with-exception-catcher handle-replloop-exception remreplloop))

(define (beaver-cmd2) ;; interactive read-evaluate-print-loop
  (define eval/ro eval)
  (define (eval/rw expr) (eval expr))
  (define location beaver-cmd2) ;; source code tag without use
  (define (dispatch-condition e port)
    (cond
     ((unbound-global-exception? e)
      (println port: port "Unbound variable " (unbound-global-exception-variable e)))
     (else (display-exception e port))))
  (define (beaver-handle-cmd-exception e) (debug location e)
    (write `(ref E . ,(call-with-output-string (lambda (s) (dispatch-condition e s)))))
    #t)
  (define (handle-cmd)
    (match
     (read)
     ((ref 'Q . expr) (begin (write `(,ref D ,(eval/ro expr))) #t))
     ((ref 'P . expr) (begin (write `(,ref D ,(eval/rw expr))) #t) )
     ((? eof-object?) #f)
     (X (error "unhandled request" location X))))
  (with-exception-catcher beaver-handle-cmd-exception handle-cmd))

(define (beaver-cmd3) (and (beaver-cmd2) (beaver-cmd3)))

(define make-beaver-api2
  (let ((handle-error error))
    (define (print-error err)
      (println port: (current-error-port) "Remote Error: " err)
      (values))
    (define (send-request! conn expr)
      (write expr conn)
      (force-output conn))
    (define (dispatch-result conn)
      (let ((result (read conn)))
        ;; (beaver-debug 'Beaver-Result result)
        (match
         result
         ((ref 'E . err) (handle-error err))
         ((ref 'D . vals) (apply values vals))
         ((? eof-object?) #!void)
         (X (error "beaver call: bad protocol reply" X)))))
    (define (with-conn conn kind expr)
      #;(beaver-debug (list 'beaver kind) expr)
      (send-request! conn `(0 ,kind . ,expr))
      (dispatch-result conn))
    (set! handle-error print-error)
    (lambda (conn)
      (values
       (lambda (expr) (with-conn conn 'Q expr))
       ;; make-beaver-caller
       (lambda (expr) (with-conn conn 'P expr))))))

(define sounds
  (delay
    (begin
      (audiofile-init)
      (vector
       (audiofile-load "win")))))

(define (audible-beep!)
  (if (< (audiofile-getvolume) 0.3)
      (log-warning "Beep with low volume."))
  (audiofile-forceplay (vector-ref (force sounds) 0)))

(define (calculator dir)
  (define control-port
    (cond-expand
     (win32 1313)
     (else (make-pathname dir "control"))))
  (define (args)
    `(
      "-B" ,dir
      ip: on
      -S control ,control-port :
      -service ot0 start "\"*:0\""
      ;; Don't do this here!
      ;;
      ;; join: ,(debug-adhoc-network-id) -S vpn tcp register ,(debug-adhoc-network-port) beaver-cmd3 :
      ;; -S tcp register (debug-adhoc-network-port) replloop2 :
      -wait
      ))
  (log-status "Starting from " dir (object->string (args)))
  (glgui-example)
  (init-chat! dir)
  (let ((job (lambda () (beaver-process-commands (args)))))
    (kick/sync! forward-logging-to-daemonian!)
    (thread-start! (make-thread job 'beaver))))

(let ()
  (define (load-file-with-arguments file args)
    (load file))
  (define parse
    (match-lambda
     ((CMD) (calculator chat-dir))
     ((CMD (? (lambda (key) (equal? (daemonian-semifork-key) key))) loadkey . more)
      (daemonian-execute-registered-command loadkey more))
     ((CMD "-l" FILE . more)
      (load-file-with-arguments FILE more))
     ((CMD "-version" . more)
      (begin
        (println (system-appversion))
        (exit 0)))
     ((CMD (? file-exists? FILE) . more) (calculator FILE))
     ((CMD . more)
      (println port: (current-error-port) "Warning: " CMD " did not parse: " (object->string more))
      (println port: (current-error-port) "Assuming: " (object->string `(,(daemonian-semifork-key) "beaver" ,@more)))
      (ot0cli-process-commands more)
      (exit 42))
     (otherwise
      (println port: (current-error-port) "Error: " (system-cmdargv 0) " did not parse: " (object->string otherwise))
      (exit 23))))
  (parse (command-line)))

(if (eq? *glgui-main* main) (exit 0))
