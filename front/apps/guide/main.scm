;;; https://bigtechdetective.net/firefox
;;;
;;;
;;; Lyra: A New Very Low-Bitrate Codec for Speech Compression
;;;
;;; https://soylentnews.org/article.pl?sid=21/02/28/1438232

;;; To be fixed upstream:

(define-macro (macro-fix val)
  `(if (fixnum? ,val) ,val (inexact->exact (round ,val))))

(set! gdImageCreateTrueColor
      (let ((orig gdImageCreateTrueColor))
        (lambda (w h)
          (orig (macro-fix w) (macro-fix h)))))

(set! gdImageCopyResampled
      (let ((orig gdImageCopyResampled))
        (lambda (gd2 gd x y ox oy w2 h2 w h)
          (orig gd2 gd x y ox oy w2 (macro-fix h2) w h))))

;;; Early process semi-fork

(match
 (command-line)
 ((CMD (? (lambda (key) (equal? (daemonian-semifork-key) key))) loadkey . more)
  (daemonian-execute-registered-command loadkey more))
 (otherwise #t))

;;;

(define normal-exit exit)
;;(set! exit _exit) ;; FIXME: with lambdanative we see exit 0 always!

(set!
 ##exit
 (let ((old ##exit))
   (lambda args
     (##print-maturity-warnings!)
     (apply old args))))

;; (include "DejaVuSans-14,24,32.scm") currently in guide.

(include "visit-symbol-table.scm")

(define (wire-persistent-file-as-string! pin filename)
  (MATURITY -4 "to slow, not recommended" loc: wire-persistent-file-as-string!)
  (wire-persistent-file!
   pin filename
   decode: #f load:
   (lambda (pin)
     (when (file-exists? filename)
       (pin (call-with-input-file filename (lambda (port) (read-line port #f))))))
   encode: #f save:
   (lambda (old)
     (let ((data (pin)))
       (cond
        ((u8vector? data)
         (call-with-output-file filename
           (lambda (port) (##write-subu8vector data 0 (##u8vector-length data) port))))
        (else
         (call-with-output-file filename
           (lambda (port) (display (pin) port)))))))))

(utf8string->unicode:on-encoding-error 'replace)

(cond-expand
 (android)
 (linux (setenv "TMPDIR" "/tmp/"))
 (else))

(define (debug l v)
  (let ((p  (current-error-port)))
    (display l p)
    (display ": " p)
    (write v p)
    (newline p)
    (force-output p)
    v))

(cond-expand
 (debug
  ($debug-trace-triggers #t)
  ($async-exceptions 'catch)
  ) (else #f))

(define (pin-attach-log! pin msg)
  (wire! pin post: (lambda () (log-debug msg 1 (debug msg (pin))))))

(define make-beaver-api
  (let ()
    (define (send-request! conn expr)
      (write expr conn)
      (force-output conn))
    (define (dispatch-result conn)
      (let ((result (read conn)))
        (close-port conn)
        (beaver-debug 'Beaver-Result result)
        (match
         result
         ((ref 'E . err) (error err))
         ((ref 'D . vals) (apply values vals))
         ((? eof-object?) #!void)
         (X (error "beaver call: bad protocol reply" X)))))
    (define (with-unix-client sockaddr kind expr)
      (beaver-debug (list 'beaver kind) expr)
      (let ((conn (open-unix-client sockaddr #t)))
        (if conn
            (begin
              (send-request! conn `(0 ,kind . ,expr))
              (dispatch-result conn))
            (error "beaver call failed to connect"))))
    (lambda (directory)
      (define sockaddr (make-pathname directory "control"))
      (values
       (lambda () (let ((port (open-unix-client* sockaddr (lambda () #f))))
                    (and port (begin (close-port port) #t))))
       (lambda (expr) (with-unix-client sockaddr 'Q expr))
       ;; make-beaver-caller
       (lambda (expr) (with-unix-client sockaddr 'P expr))))))

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
#;(kick/sync! (lambda () (front-beaver-directory (front-beaver-directory-default))))

(define (forward-logging-to-daemonian!)
  (let ((to (and log:on log:file)))
    (daemonian-stdout-file to)
    (daemonian-stderr-file to)))


#;(thread-start!
 (make-thread
  (lambda ()
    (do () (#f)
      (thread-sleep! (* 24 36 60)) ;; 1d
      (log-reset!)
      (kick! forward-logging-to-daemonian!)))
  'reset-log))

;; GUI helpers

(define (handle-replloop-exception e)
  (let ((port (current-error-port)))
    (continuation-capture
     (lambda (cont)
       (display-exception-in-context e cont port)
       (display-continuation-backtrace cont port))))
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

(kick/sync! (lambda () (beaver-captured-domain "bvr")))

(define-macro (%%guide-post-speculative expr)
  ;; either a thunk or a promise -- promise seems NOT to work under
  ;; gamit?
  `(lambda () ,expr))

(define (make-about-payload #!key in)
  (debugger-about-payload in: in))

(define (test-guide-size-select-toplevel-payload
         area cmdline #!key
         (on-started (lambda (process result) (exit (/ (or result 0) 256)))))
  ;; Status: very experimental
  ;;
  ;; TBD: recurse into `guide-toplevel-payload` update when selection changed
  (let ((continue
         (lambda (size)
           ;; (semi-fork& `("-size" ,size ,@cmdline))
           (let ((process
                  (open-process
                   `(path: ,(system-cmdargv 0) arguments: ("-size" ,size  . ,cmdline)
                           stdin-redirection: #f stdout-redirection: #f stderr-redirection: #f))))
             (guide-critical-add!
              (lambda ()
                ;; under gambit we must wait for the process to be
                ;; really started otherwise ... no idea, but we end
                ;; up in a tight loop.
                (let ((synchronization-point (process-status process 0.5 #f)))
                  ;; now, we know the process is set up properly
                  (on-started process synchronization-point)))
              async: #t)
             #t)))
        (options '#("320x480" "dev" "1920x1200")))
    (guide-toplevel-payload
     #; (guide-button in: area guide-callback: (lambda _ (continue "dev")))
     (guide-list-select-payload
      area (lambda _ options)
      line-height: 60
      action: (lambda (n x) (continue (vector-ref options n)))))))

(define (test-guide-select-registered-payload area)
  (let* ((options (guide-payload-names))
         (select
          (lambda (n x)
            (guide-toplevel-payload ((guide-payload-ref (vector-ref options n)) area)))))
    (guide-toplevel-payload
     (guide-list-select-payload
      area (lambda _ options)
      line-height: 40
      action: select))))

(define main-guide-area (make-mdv-rect-interval 0 0 320 480)) ;; deprecated temporary

(guide-define-payload "beaver start" 'ephemeral guidot-beaver-select-path-payload)
(guide-define-payload-calculator! "calculator")
(guide-define-payload "status" 'ephemeral (lambda (area) (debugger-about-payload in: area)))
(guide-define-payload "Fossil Help" 'ephemeral guidot-fossil-help-browser)
(guide-define-payload "Fossil Wiki" 'ephemeral guidot-fossil-wiki)
(guide-define-payload
 "Fossil Transfer" 'ephemeral
 (lambda (area)
   (guidot-fossil-transfer-dialog
    area done: (lambda _ (test-guide-select-registered-payload area)))))
(guide-define-payload "Fossil ZZ" 'ephemeral guidot-fossil-browser)
(guide-define-payload
 "debugger" 'once
 (lambda (area)
   (receive (result dialog-control!) (guidot-layers area name: "Scheme Interpreter")
     (guidot-insert-scheme-interpreter! dialog-control! in: area)
     result)))

(guide-define-payload
 "Beaver Chat" 'ephemeral
 (lambda (area)
   (make-beaverchat-payload launch-url beaver-domain in: area keypad: guide-keypad/de)))
(guide-define-payload "Beaver Chat About" 'ephemeral beaverchat-about-payload)

(let ((area (make-mdv-rect-interval 0 0 320 480))
      (verbose #f))
  (define (load-file-with-arguments file args)
    #;(load file)
    (call-with-input-file file
      (lambda (port)
        (define ce (box #f))
        (define context (box #f))
        (with-exception-catcher
         (lambda (exn)
           (let ((port (current-error-port))
                 (context (unbox context)))
             (println port: port "loading " file " in:")
             (pretty-print (unbox ce) port)
             (display-exception exn port)
             (display-exception-in-context exn context port)
             (display-continuation-backtrace context port  #t #t 10 10 0)))
         (lambda ()
           (let ((verbose verbose))
             (when verbose (debug 'loading file))
             #;(load (debug 'loading file))
             (do ((expression (read port) (read port)))
                 ((eof-object? expression))
               (set-box! ce expression)
               (when verbose
                 (let ((ep (current-error-port)))
                   (println port: ep "Eval #" expression-number)
                   (pretty-print expression ep)))
               (continuation-capture (lambda (cc) (set-box! context cc)))
               (eval expression))))))))
  (define (with-area-parse CMD SIZE parse more)
    (define (setsize! interval)
      (set! area interval)
      (set! main-guide-area interval) ;; TBD: get rid of that one
      (parse `(,CMD . ,more)))
    (match
     SIZE
     ((or "dev" "640x1200")  (setsize! (make-mdv-rect-interval 0 0 640 1200)))
     ((or "large" "1920x1200") (setsize! (make-mdv-rect-interval 0 0 1920 1200)))
     ((or "small" "320x480") (setsize! (make-mdv-rect-interval 0 0 320 480)))
     ("ask" (test-guide-size-select-toplevel-payload area more))
     (otherwise
      (println port: (current-error-port) "Unhandled size "
               (system-cmdargv 0) " did not parse: " (object->string otherwise))
      (exit 23))))
  (define parse
    (match-lambda
     ((CMD)
      (cond-expand
       ((or android)
        (kick/sync! forward-logging-to-daemonian!)
        (log-status "begin of log for glgui")
        (load-file-with-arguments (make-pathname (system-directory) "init.scm") '()))
       (else (replloop) (exit 0))))
     ((CMD (? (lambda (key) (equal? (daemonian-semifork-key) key))) loadkey . more)
      (daemonian-execute-registered-command loadkey more))
     ((CMD "-version" . more)
      (begin
        (println (system-appversion))
        (exit 0)))
     ((CMD "-size" SIZE . more) (with-area-parse CMD SIZE parse more))
     ((CMD "-l" FILE . more)
      (load-file-with-arguments FILE more))
     ((CMD (? file-exists? FILE) . more) (parse `(,CMD "-l" ,FILE ,@more)))
     ((CMD "-v" . more) (begin (set! verbose #t) (parse (cons CMD more))))
     ((CMD "+repl" . more)
      (begin
        (thread-start! (make-thread replloop))
        (parse `(,CMD ,@more))))
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
