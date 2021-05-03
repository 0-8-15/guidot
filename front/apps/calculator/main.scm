(match
 (command-line)
 ((CMD (? (lambda (key) (equal? (daemonian-semifork-key) key))) loadkey . more)
  (daemonian-execute-registered-command loadkey more))
 (otherwise #t))

(log-status "Startup")

(define (debug l v)
  (let ((p  (current-error-port)))
    (display l p)
    (display ": " p)
    (write v p)
    (newline p)
    (force-output p)
    v))

(set!
 ##exit
 (let ((old ##exit))
   (lambda args
     (##print-maturity-warnings!)
     (apply old args))))

(current-maturity-level 0)

($maturity-tolerance!
 ;; lower bound
 2
 ;; upper bound Any chance +2 code came here?
 1
 ;; FIXME: (##print-maturity-warnings!) not yet seen upon exit, why?
 1 ;; normally 0
 )

(cond-expand
 ((or #;android debug)
  ($debug-trace-triggers #t)
  ($async-exceptions 'catch)
  ($kick-style 'sync)
  (on-ot0-path-lookup
   (lambda (node uptr thr nodeid family sa)
     ;; (debug 'LOOKUP (number->string nodeid 16))
     (debug 'LOOKUP (hexstr nodeid 12))
     #; (debug 'LookupFamily family)
     #f))
  )
 (else #f))

(cond-expand
 (android
  (define (setup-heartbeat!)
    ;; History: lambdanative used to disable Scheme involutary thread
    ;; switches to mitigate failing of the use case Gambit does not
    ;; support: calling Gambit Scheme, back to C and from C to Scheme
    ;; again, then switch Scheme continuation.
    ;;
    ;; The line below switched back to involuntary thread switches
    ;; during development of the fix.  Retained for documentation.
    ;;
    ;; ((c-lambda () scheme-object "___setup_heartbeat_interrupt_handling"))
    (##set-heartbeat-interval! (exact->inexact 1/100)))
  (setup-heartbeat!)
  (when #f
    (begin ($kick-style 'sync) (kick/sync! (lambda () (kick-style 'sync)))))
  ;; BEWARE, experimental
  (kick! (lambda () (fossils-enable-http-hijacking #t)))
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
  (set! ;; FIXME: make this the default case!
   terminate
   (let ((old terminate))
     (lambda args
       (##print-maturity-warnings!)
       (apply old args))))))

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

($guide-frame-period
 (cond-expand
  (android 0.25)
  (else 0.5)))

(define (handle-replloop-exception e)
  (let ((port (current-error-port)))
    (continuation-capture
     (lambda (cont)
       (display-exception-in-context e cont port)
       (display-continuation-backtrace
        cont port
        #f ;; all-frames?
        #t ;; display-env?
        10 ;; max-head (default 10)
        4  ;; max-tail (default 4)
        1  ;; depth default 0
        ))))
  #!void)

(define (replloop) ;; interactive read-evaluate-print-loop
  (with-exception-catcher handle-replloop-exception (lambda () (##repl-debug #f #t)))
  (replloop))

(define beaver-stdout-redirection #t)

(register-command!
 "cerberus"
 (lambda (args)
   (cerberus (system-cmdargv 0) (cdr args) startup-delay: 2 max-fast-restarts: 2
             stdout-redirection: beaver-stdout-redirection)))

(register-command! "beaver" beaver-process-commands)

;;; BEGIN INSTEAD OF (include "~~tgt/lib/onetierzero/src/observable-notational-conventions.scm")
(include "../../modules/misc-conventions/observable-syntax.sch")

(define-macro (define-macro/rt expr nexpr)
  `(begin
     (define-macro ,expr ,nexpr)
     (eval '(define-macro ,expr ,nexpr))))

(define-macro (define-sense* name val . more)
  (let ((in (string->symbol (string-append "." (symbol->string name))))
        (in2 (string->symbol (string-append (symbol->string name) ":="))))
    `(begin
       (define ,in (SENSOR ,val . ,more))
       (define ,in2 ,in)
       (define ,name (,in)))))

(define-macro (define-SENSOR name form)
  (if (not (eq? (car form) 'SENSOR))
      (error "define-SENSOR: missuse")
      (let* ((more (cdr form))
             (in (string->symbol (string-append "." (symbol->string name))))
             (in2 (string->symbol (string-append (symbol->string name) ":="))))
        `(begin
           (define ,in (SENSOR . ,more))
           (define ,in2 ,in)
           (define ,name (,in))))))

;;; END INSTEAD OF (include "~~tgt/lib/onetierzero/src/observable-notational-conventions.scm")

(define chat-dir "beaver")

(define (system-appdirectory-subdirectory dir)
  (cond-expand
   (android
    (make-pathname (system-appdirectory) dir))
   (else dir)))

(set! chat-dir (system-appdirectory-subdirectory chat-dir))

(kick (fossils-directory (fossils-directory-location "fossils")))
(log-status "chat-dir")

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
       (audiofile-load "win")
       (audiofile-load "tata")
       (audiofile-load "thatsit")))))

(define (audible-beep!)
  (if (< (audiofile-getvolume) 0.3)
      (log-warning "Beep with low volume."))
  (let ((idx (let ((x (random-integer 100)))
               (cond
                ((= x 99) 0)
                (else 1)))))
    (audiofile-forceplay (vector-ref (force sounds) idx))
    (when (< (random-integer 10) 2)
      (kick! (box (lambda ()
                    (thread-sleep! 10)
                    (audiofile-forceplay (vector-ref (force sounds) 2))))))))

(define (calculator-adhoc-network-id) 18374687579166474240)

(define (at-phone-decoder str)
  (let* ((e0 (string-contains (substring str 1 (string-length str)) "/"))
         (e (if e0 (+ e0 1)  (string-length str))))
    (if (and (> (string-length str) 1) (or (eqv? (string-ref str 0) #\/) (eqv? (string-ref str 0) #\@)))
        (unit-id-string->unit-id (substring str 1 e))
        (unit-id-string->unit-id (if (eqv? e (string-length str)) str (substring str 0 e))))))


(define (guidot-eventually! thunk)
  (cond
   ((not (stm-atomic?)) (guide-critical-add! thunk async: #t))
   (else (thread-start! (make-thread thunk)))))

(define (init-beaverchat-gui! area launch-url beaver-domain)

;;;  (define field_gradient (list (color:shuffle #xe8e9eaff) (color:shuffle #xe8e9eaff) (color:shuffle #xfefefeff) (color:shuffle #xfefefeff)))

  (define (beaverchat-payload-sel number area)
    (foreground-service! #t)
    (case number
      ((2 about) (beaverchat-about-payload area))
      ((0 calculator) (make-calculator-payload area))
      ((1) (make-beaverchat-payload launch-url beaver-domain in: area keypad: guide-keypad/de))
      ;;;; ((16) (guidot-fossil-help-browser area))
      ;;((17) (guidot-fossil-wiki area))
      ;;;; ((18) (guidot-fossil-browser area))
      ((3) (guidot-fossil-transfer-dialog
            area done:
            (lambda args
              (match
               args
               (((? guide-payload? next) . _) (guide-toplevel-payload next))
               (else (guidot-eventually! (lambda () (kick (PIN:toplevel-selection 2)))))))))
      (else (guide-button in: (make-mdv-rect-interval 0 0 100 100)))))

  ;; No longer: Wired to globals, may now be instanciated more than once.
  ;;
  ;; (guide-define-payload "chat" 'once (beaverchat-payload-config launch-url beaver-domain))

  (guide-define-payload
   "Beaver Chat" 'ephemeral
   (lambda (area) (make-beaverchat-payload launch-url beaver-domain in: area)))
  (guide-define-payload "Beaver Chat About" 'ephemeral beaverchat-about-payload)

  (kick/sync
   (visible-tl-options '#("calculator" "chat" "about" "advanced"))
   (PIN:toplevel-selection 0))
  (let ((area
         (receive (xsw xno ysw yno)
             ;; FIXME inconsistent argument/result ordering!
             (guide-boundingbox->quadrupel area) (make-mdv-rect-interval 0 0 xno (floor (- yno 16)))))
        (conversion beaverchat-payload-sel)
        (options visible-tl-options)
        (selection PIN:toplevel-selection))
    ;; content as parameter???
    (let ((rebuild
           (lambda ()
             (guide-toplevel-payload
              (make-tool-switch-payload/dropdown selection options (conversion (selection) area))))))
      (wire! selection post: rebuild)
      (rebuild))))

;; Payloads

(guide-define-payload "status" 'ephemeral (lambda (area) (debugger-about-payload in: area)))
(guide-define-payload "Fossil Help" 'ephemeral guidot-fossil-help-browser)
(guide-define-payload "Fossil Wiki" 'ephemeral guidot-fossil-wiki)
(guide-define-payload "Fossil Transfer" 'ephemeral guidot-fossil-transfer-dialog)

(cond-expand
 ((or linux win32 debug)
  (guide-define-payload
   "debugger" 'once
   ;; enforcing Scheme (the default values) here:
   (let ((read-all read-all)
         (eval eval))
     (lambda (area)
       (receive (result dialog-control!) (guidot-layers area name: "Scheme Interpreter")
         (guidot-insert-scheme-interpreter!
          dialog-control! in: area
          ;; pass enforced default values to configure the interpreter
          read-all: read-all eval: eval)
         result))))
  (let ((name "Fossil SQL (experimental)"))
    (guide-define-payload
     name 'once
     (let ((read-all (lambda (port) (list (read-line port #f))))
           (eval (lambda (expr . _) (read-line (fossil-command/sql expr) #f))))
       (lambda (area)
         (receive (result dialog-control!) (guidot-layers area name: name)
           (guidot-insert-scheme-interpreter!
            dialog-control! in: area
            ;; configure the interpreter
            read-all: read-all eval: eval)
           result))))))
 (else))

;; Calculator

(define (calculator area dir)
  (define control-port
    (cond-expand
     (win32 1313)
     (else (make-pathname dir "control"))))
  (define (args)
    `(
      "-B" ,dir
      ip: on
      -S control ,control-port :
      -service ot0 start "\"*:0\"" join: ,(calculator-adhoc-network-id)
      ;; Don't do this here!
      ;;
      ;; join: ,(debug-adhoc-network-id) -S vpn tcp register ,(debug-adhoc-network-port) beaver-cmd3 :
      ;; -S tcp register (debug-adhoc-network-port) replloop2 :
      -wait
      ))
  (define (capdom) "bvr")
  (httpproxy-atphone-set! at-phone-decoder)
  (httpproxy-connect-set! ot0cli-connect)
  (init-beaverchat-gui! area webview-launch! capdom)
  (kick (audible-beep audible-beep!))
  ;; end of invocation independent initializations
  (log-status "Starting from " dir (object->string (args)))
  (init-beaverchat! dir use-origin: use-origin) ;; MUST be first
  (kick
   (capture-domain!
    (capdom)
    handler: fossils-directory-handler
    at-phone-decoder: at-phone-decoder
    network-id: (calculator-adhoc-network-id)))
  ;; (lwip-tcp-service-register! 80 fossils-directory-service)
  (log-status "beaver.dam done")
  (let ((job (lambda () (beaver-process-commands (args)))))
    (cond-expand
     ((or #;debug linux) #f)
     (else (kick/sync! forward-logging-to-daemonian!)))
    (log-status "begin of log for run in \"" dir "\" with arguments " (object->string (args)))
    (thread-start! (make-thread job 'beaver))))

(cond-expand
 ((and debug (not android))
  (kick
   (visible-tl-options '#("calculator" "chat" "about" "advanced"))
   (PIN:toplevel-selection 1))
  (define (avant!)
    (load "dev.scm")
    (kick (PIN:toplevel-selection 0))
    (kick (PIN:toplevel-selection 1)))
  (thread-start! (make-thread replloop)))
 (else))

;; style

($current-guide-style
 (guide-style-extend
  (guide-style-default)
  locale: 'de
  ;; font ...
  background-color: (guide-select-color-1)
  color: (guide-select-color-2)
  background-highlight-color: (guide-select-color-3)
  highlight-color: (guide-select-color-4)
  ;; end of default style definitions
  ))

;; command line

(let ((area (make-mdv-rect-interval 0 0 320 480)))
  (define (load-file-with-arguments file args)
    (load file))
  (define (with-area-parse CMD SIZE parse more)
    (define (setsize! interval)
      (set! area interval)
      (parse `(,CMD . ,more)))
    (match
     SIZE
     ((or "tiny" "320x480") (setsize! (make-mdv-rect-interval 0 0 320 480)))
     ((or "small" "640x960")  (setsize! (make-mdv-rect-interval 0 0 640 960)))
     ((or "medium" "640x1200")  (setsize! (make-mdv-rect-interval 0 0 640 1200)))
     ((or "large" "1920x1200") (setsize! (make-mdv-rect-interval 0 0 1920 1200)))
     ;; ("ask" (test-guide-size-select-toplevel-payload area more))
     (otherwise
      (println port: (current-error-port) "Unhandled size "
               (system-cmdargv 0) " did not parse: " (object->string otherwise))
      (exit 23))))
  (define parse
    (match-lambda
     ((CMD) (calculator area chat-dir))
     ((CMD (? (lambda (key) (equal? (daemonian-semifork-key) key))) loadkey . more)
      (daemonian-execute-registered-command loadkey more))
     ((CMD "-size" SIZE . more) (with-area-parse CMD SIZE parse more))
     ((CMD "-gui" . more)
      (cond
       ((null? more)
        (let* ((options (guide-payload-names))
               (select
                (lambda (n x)
                  (guide-toplevel-payload ((guide-payload-ref (vector-ref options n)) area)))))
          (guide-toplevel-payload
           (guide-list-select-payload
            area (lambda _ options)
            line-height: 60
            action: select))))
       (else
        (guide-toplevel-payload ((guide-payload-ref (car more)) area))
        (when (pair? (cdr more)) (parse (cons CMD (cdr more)))))))
     ((CMD "-l" FILE . more)
      (load-file-with-arguments FILE more))
     ((CMD "-version" . more)
      (begin
        (println (system-appversion))
        (exit 0)))
     ((CMD (? file-exists? FILE) . more) (calculator area FILE))
     ((CMD . more)
      (println port: (current-error-port) "Warning: " CMD " did not parse: " (object->string more))
      (println port: (current-error-port) "Assuming: " (object->string `(,(daemonian-semifork-key) "beaver" ,@more)))
      (ot0cli-process-commands more)
      (exit 42))
     (otherwise
      (println port: (current-error-port) "Error: " (system-cmdargv 0) " did not parse: " (object->string otherwise))
      (exit 23))))
  (parse (command-line)))

(guide-exit 0)

;; eof
