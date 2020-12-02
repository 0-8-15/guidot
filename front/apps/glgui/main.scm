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

;;;

(define normal-exit exit)
;;(set! exit _exit) ;; FIXME: with lambdanative we see exit 0 always!

;; (include "DejaVuSans-14,24,32.scm") currently in guide.

(include "visit-symbol-table.scm")

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

(let ()
  (define (load-file-with-arguments file args)
    (with-exception-catcher
     (lambda (exn)
       (debug "While loading" file)
       (handle-replloop-exception exn)
       (cond-expand
        (android (exit 1))
        (else #f)))
     (lambda () (load (debug 'loading file)))))
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
     ((CMD "-l" FILE . more)
      (load-file-with-arguments FILE more))
     ((CMD "-version" . more)
      (begin
        (println (system-appversion))
        (exit 0)))
     ((CMD (? file-exists? FILE) . more) (parse `(,CMD "-l" ,FILE ,@more)))
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
