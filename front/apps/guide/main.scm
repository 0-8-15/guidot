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

(define-macro (%%guide-post-speculative expr)
  ;; either a thunk or a promise -- promise seems NOT to work under
  ;; gamit?
  `(lambda () ,expr))

(define (beaver-about-page-content-constructors)
  (define (beaver-number-display x) (if x (beaver-number->unicode-vector x) '#()))
  (define (beaver+guide-boolean-display x) (if x "yes" "no"))
  (define conv
    (lambda (v)
      (cond
       ((string? v) v)
       ((boolean? v) (beaver+guide-boolean-display v))
       (else (object->string v)))))
  (define val1 (lambda (a1 . more) a1))
  (define size 'medium)
  (define content
    (list
     (lambda (area buffer active)
       (guide-button
        in: area
        label: "(C) JFW [Corona edition: 2020-2021]"
        guide-callback: (lambda (rect payload event xsw ysw) (active #f) #t)))
     (lambda (area buffer active)
       (guide-valuelabel in: area label: "Version" value: (system-appversion)))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "kick-style"
        value: kick-style
        value-equal: eq?
        value-display: conv
        input:
        (lambda (rect payload event xsw ysw)
          (cond
           ((eqv? event EVENT_BUTTON1DOWN)
            (kick-style
             (case (kick-style)
               ((async) 'sync)
               ((sync) 'async)
               (else #f)))))
          #t)))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "async exceptions"
        value: $async-exceptions
        value-equal: eq? value-display: object->string))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "vpn"
        value: ot0cli-server
        value-equal: eq? value-display: beaver+guide-boolean-display))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "context directory"
        value-display: conv
        value: ot0-context))
     ;; ot0cli-origin
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "onetierzero"
        value: ot0-online
        value-equal: eq? value-display: beaver+guide-boolean-display))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "lwIP"
        value: lwIP
        value-equal: eq? value-display: beaver+guide-boolean-display))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "beaver id"
        value: beaver-local-unit-id
        value-equal: eq? value-display: beaver-number-display))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "beaver number"
        value: beaver-local-unit-id
        value-equal: eq? value-display: beaver-number-display))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "use deamonize"
        value: beaver-use-daemonize
        value-equal: eq?
        value-display: beaver+guide-boolean-display
        input:
        (lambda (rect payload event xsw ysw)
          (cond
           ((eqv? event EVENT_BUTTON1DOWN)
            (%%guide-post-speculative (beaver-use-daemonize (not (beaver-use-daemonize)))))
           (else #t)))))
     #;(lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "fossils directory"
        value: fossils-directory
        value-equal: eq? value-display: (lambda (x) (or x "n/a"))))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "code maturity level"
        value: current-maturity-level
        value-equal: eq? value-display: object->string))
     (lambda (area buffer active)
       (guide-valuelabel
        in: area size: size label: "memoize active"
        value: $memoize-active
        value-equal: eqv? value-display: beaver+guide-boolean-display
        input:
        (lambda (rect payload event xsw ysw)
          (cond
           ((eqv? event EVENT_BUTTON1DOWN)
            (%%guide-post-speculative ($memoize-active (not ($memoize-active)))))
           (else #t)))))
     ;; end of content
     ))
  content)

(define (guide-div/lineheight-payload
         #!key
         (in (current-guide-gui-interval))
         (line-height 16)
         (line-height-selectable 60)
         ;;
         ;; ;; not yet: (results values)
         (direction 'topdown)
         (name 'page)
         #!rest content-constructors
         )
  (let*
      ((interval in)
       (xsw 0) (ysw 0) ;; whatever zero is... ;-)
       #|
       (xsw (mdvector-interval-lower-bound interval 0))
       (ysw (mdvector-interval-lower-bound interval 1))
       |#
       (w (mdv-rect-interval-width interval))
       (h (mdv-rect-interval-height interval))
       ;; derived values (context may need to re-define what `+` means)
       (xno (+ xsw h))
       (active
        (let ((active #f))
          (case-lambda
           (() active)
           ((next) (set! active next)))))
       (redraw! (lambda ()
                  (let ((active (active)))
                    (and (guide-payload? active) ((guide-payload-on-redraw active))))))
       (events
        (lambda (rect payload event x y)
          (cond
           ((active) =>
            (lambda (payload)
              (guide-payload-contains/xy? payload x y)
              (guide-event-dispatch-to-payload rect payload event x y)))
           (else (mdvector-rect-interval-contains/xy? interval x y))))))
    (define result
      (cond
       ((null? content-constructors)
        (guide-button
         in: (make-x0y0x1y1-interval/coerce xsw ysw (+ xsw (* w 19/20)) (+ ysw (/ h 2)))
         label: "(C)... fallback version"
         guide-callback: (lambda (rect payload event xsw ysw) (active #f) #t)))
        (else
         (guide-ggb-layout
          interval
          (let* ((in
                  (let ((w w)
                        (h (min (/ h 2) line-height)))
                    (make-x0y0x1y1-interval/coerce 0 0 w h))))
            ;; summarize `args` in result GGB
            (let ((buffer (make-ggb)))
              (for-each
               (lambda (obj) (ggb-insert! buffer (obj in buffer active)))
               content-constructors)
              buffer)) ;; MUST return a GGB for `guide-ggb-layout` at this position
          background: #t
          fixed: #f ;; better #t if known that no scrolling required
          direction: direction
          ))))
    ;; finally
    (active result) ;; ...don't touch the "(C)"... line
    (make-guide-payload
     name: name in: interval
     on-redraw: redraw!
     on-any-event: events
     widget: #f lifespan: 'ephemeral)))

(define (make-about-payload #!key in)
  (apply
   guide-div/lineheight-payload name: 'about in: in
   line-height: 32
   (beaver-about-page-content-constructors)))

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
