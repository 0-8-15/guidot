;;* Fossil GUI

;;** Utilities

;;*** Status and Control

(define-values (current-fossil current-fossil-pathname current-fossil-remote-url)
  ;; Note: this enforces the fossil files to have a `.fossil` file
  ;; extension.  As `fossil` itself expects that to be the case (when
  ;; serving directories), this is considered a consistency enforcing
  ;; feature than the limitation it is as well.
  ;;
  ;; Furthermore it the file MUST be within the directory pinned in
  ;; `fossils-directory`.
  ((lambda ()
     (define cfpn
       (make-pin
        initial: #f
        name: "current fossil normalized path name"))
     (define current-fossil
       (make-pin
        initial: #f
        pred:
        (lambda (v)
          (cond
           ((not v) #t)
           ((not (string? v)) #f)
           (else #t)))
        filter:
        (lambda (old new)
          (cond
           ((and (string? new)) ;; remove `.fossil` extension
            (path-strip-extension new))
           (else new)))
        name: "current fossil"))
     (define current-fossil-remote-url #f)
     (wire!
      (list current-fossil fossils-directory) post:
      (lambda ()
        (let ((dir (fossils-directory))
              (basename (current-fossil)))
          (cond
           ((and dir basename)
            (let ((fn (path-normalize (make-pathname dir basename "fossil"))))
              (cond
               ((and
                 (file-exists? fn)
                 (eq? (file-type fn) 'regular)
                 ;; TBD: check file for being sqlite and fossil
                 )
                (cfpn fn))
               ;; maybe signal error, clean cf..?
               (else #f))))
           (else (cfpn #f))))))
     (wire!
      cfpn post:
      (lambda ()
        (cond
         ((cfpn) =>
          (lambda (repository)
            (let ((stored
                   ;; (read-line (fossil-command "remote-url"))
                   (unbox
                    (let ((x (box #f)))
                      (sqlite3-file-query
                       repository "select value from config where name = 'last-sync-url'"
                       accu: x row: (lambda (v) (set-box! x v)))))))
              (set! current-fossil-remote-url
                    (cond
                     ((eof-object? stored) #f)
                     ((rx~
                       (rx '(submatch
                             (seq "http://"
                                  (or domain ip-address)
                                  (? ":" (+ numeric)) ;; port
                                  (? "/" (* (or url-char "/"))))))
                       stored) =>
                       (lambda (matched) (rxm-ref matched 1)))
                     (else stored))))))
         (else (set! current-fossil-remote-url #f)))))
     (values
      current-fossil
      (lambda () (cfpn))
      (case-lambda
       (() current-fossil-remote-url)
       ((v)
        (cond
         ((or (not v) (equal? v "")) (set! current-fossil-remote-url #f))
         ((string? v) (set! current-fossil-remote-url v))
         (else (error "invalid" current-fossil-remote-url v)))))))))

(define-values
    (fossil-object-type? fossil-object-type->string)
  (values
   (lambda (obj)
     (case type
       ((technote event wiki ticket branch checkin) #t)
       (else #f)))
   (lambda (obj)
     (case obj
       ((technote event) "event")
       ((technote:) "technote")
       ((wiki) "wiki")
       ((tiket) "ticket")
       ((branch) "branch")
       ((checkin) "checkin")
       (else (error "fossil types are {technote, event, wiki, ticket, branch, checkin} not" obj))))))

(define (fossil-timeline
         #!key
         (type 'event))
  (fossil-command/json `((command . ,(string-append "timeline/" (fossil-object-type->string type))))))

(define (%%fossil-cmd
         cmd url//proto #!key
         (directory #f)
         (input #f)
         (once #f)
         (title #f) (key #f)
         (proxy #f)
         (into #f)
         (log (lambda (args) (debug '%%fossil-cmd args))))
  (let ((auth
         (cond
          ((and title key (not (or (equal? title "") (equal? key ""))))
           `("-httpauth" ,(string-append title ":" key)))
          (else '())))
        (admin '("-A" "u"))
        (user '("-user" "u"))
        (cmd-str
         (cond
          (else
           (case cmd
             ((clone) "clone")
             ((sync) "sync")
             ((pull) "pull")
             ((push) "push")
             (else (NYIE))))))
        (url
         (cond
          ((not (string? url//proto)) )
          ((rx~ (rx "^http://") url//proto) url//proto)
          (else (string-append "http://" url//proto))))
        (once-only
         (cond
          (once '("-once"))
          (else '())))
        (proxy
         (cond
          (proxy `("-proxy" ,proxy))
          (else (error "proxy port required for fossil command" cmd))))
        (stderr-redirection #t)
        (working-directory (or directory (current-directory))))
    (let* ((arguments
            (case cmd
              ((clone)
               (let ((new-repository
                      (cond
                       ((and (string? into) (not (equal? into "")))
                        into)
                       (else
                        (make-pathname
                         working-directory
                         (date->string
                          (time-utc->date (make-srfi19:time 'time-utc 0 (current-seconds)))
                          "~1.fossil"))))))
                 `(,cmd-str ,@admin ,@auth ,@proxy ,@once-only ,url ,new-repository)))
              (else
               (let ((repository `("-R" ,(current-fossil-pathname))))
                 `(,cmd-str ,url "-v" ,@user ,@auth ,@proxy ,@once-only ,@repository)))))
           (port
            (semi-fork "fossil" arguments stderr-redirection #|directory: working-directory|#)))
      (assume (begin (when (procedure? log)
                       (log `(cwd: ,(path-normalize working-directory) arguments: ,@arguments))) #t)
              "unreachable")
      (cond
       ((not input) (close-output-port port))
       ((string? input)
        (display input port)
        (close-output-port port)))
      port)))

;;*** Internal Utilities
(define (fossil-help-basic-parse-output-to-commands port)
  (do ((i 0 (+ i 1))) ;; skip 1st 3 lines
      ((eqv? i 3))
    (read-line port))
  (sort!
   string<?
   (let loop ((lines
               (let ((str (port->string port)))
                 (unless (string? str) (error "failed" line1))
                 (string-split-char str #\newline))))
     ;; Schmerzhaft ist besser als Einzelhaft, oder umgekehrt?
     (cond
      ((null? lines) '()) ;; should not happend
      ((null? (cdr lines)) '()) ;; should not happend either
      ;; last line: "This is fossil..." ignore
      ((null? (cddr lines)) '())
      (else
       ;; if there was at least SRFI-1 in lambdanative! - no filter
       ;; using irregex pieces instead
       (let l2 ((words (string-split-char (car lines) #\space)))
         (cond
          ((null? words) (loop (cdr lines)))
          ((equal? (car words) "") (l2 (cdr words)))
          (else (cons (car words) (l2 (cdr words)))))))))))

(define (fossil-help-all-parse-output-to-commands port)
  ;; crude, stupid, simple, just get it done
  (sort!
   string<?
   (let loop ((lines (string-split-char (port->string port) #\newline)))
     ;; Schmerzhaft ist besser als Einzelhaft, oder umgekehrt?
     (cond
      ((null? lines) '()) ;; should not happend
      (else
       ;; if there was at least SRFI-1 in lambdanative! - no filter
       ;; using irregex pieces instead
       (let l2 ((words (string-split-char (car lines) #\space)))
         (cond
          ((null? words) (loop (cdr lines)))
          ((equal? (car words) "") (l2 (cdr words)))
          (else (cons (car words) (l2 (cdr words)))))))))))

;;** GUI

;;*** Status & Menu

(define (guidot-fossil-menu
         area #!key
         (size 'small)
         (font (guide-select-font size: size))
         (interactive (lambda (constructor #!key in done) #f))
         (overlay-area #f)
         ;; pins
         (mode (make-pin initial: 'all pred: symbol? name: "Fossil Access Mode"))
         ;; finally
         (name "Fossil Status Menu"))
  (define line-height (guide-font-height font))
  (define status-items 3)
  (define label-width 1/4)
  (define (top-area rect)
    (or overlay-area
        (receive (xsw xno ysw yno)
            ;; FIXME inconsistent argument/result ordering!
            (guide-boundingbox->quadrupel (guide-rectangle-measures rect))
          (make-mdv-rect-interval xsw ysw xno (floor (- yno (* 16/10 status-items line-height)))))))
  (guide-table-layout
   area rows: status-items cols: 1 name: name
   (lambda (area row col)
     (guide-valuelabel
      in: area font: font label: "directory"
      label-width: label-width
      value: fossils-directory
      value-display: (lambda (x) (if x x "n/a"))
      input:
      (lambda (rect payload event xsw ysw)
        (cond
         ((eqv? event EVENT_BUTTON1DOWN)
          (interactive
           ;; (lambda (#!key in done) (guide-button in: in guide-callback: done))
           (lambda (#!key in done)
             (guide-path-select
              in: (top-area rect)
              directory: fossils-directory
              selected:
              (case-lambda
               (() (fossils-directory))
               ((x)
                (when (equal? x "..")
                  (set! x (path-directory (fossils-directory))))
                (fossils-directory x)))
              ignore-hidden: #f
              filter-pred:
              (lambda (x)
                (cond
                 ((not x) #f)
                 ((equal? x ".") #f)
                 (else (and (file-exists? x) (eq? (file-type x) 'directory)))))
              done: done))
           in: (guide-rectangle-measures rect))))
        #t)))
   (lambda (area row col)
     (guide-valuelabel
      in: area size: size label: "project"
      label-width: label-width
      value: current-fossil
      value-display: (lambda (x) (if x x "n/a"))
      input:
      (lambda (rect payload event xsw ysw)
        (cond
         ((eqv? event EVENT_BUTTON1DOWN)
          (interactive
           (lambda (#!key in done)
             (let ((directory fossils-directory))
               (guide-path-select
                in: in
                directory: directory
                selected: current-fossil
                filter-pred:
                (lambda (x)
                  (or (not x)
                      (let ((x (make-pathname (directory) x)))
                        (and (file-exists? x) (eq? (file-type x) 'regular)))))
                done: done)))
           in: (top-area rect))))
        #t)))
   (lambda (area row col)
     (define label "mode")
     (guide-valuelabel
      in: area size: size name: label label: label
      label-width: label-width
      value: mode
      value-display: (lambda (x) (case x ((all) "all") ((checkout) "checkout") (else "BROKEN")))
      input:
      (lambda (rect payload event xsw ysw)
        (cond
         ((eqv? event EVENT_BUTTON1DOWN)
          (case (mode)
            ((all)
             (mode 'checkout) ;; pretent it works aready
             (interactive
              ;; (lambda (#!key in done) (guide-button in: in guide-callback: done))
              ;;
              ;; Very nice example, keep it here for a while!
              (lambda (#!key in done)
                (guide-critical-add!
                 (lambda ()
                   (thread-sleep! 5)
                   (done)
                   (kick (mode 'all)))
                 async: #t)
                (guide-button
                 in: in label: "NYI"
                 guide-callback:
                 (lambda _
                   (mode 'all)
                   done)))
              in: (guide-rectangle-measures rect))
             #t)
            ((checkout) (mode 'all) done)
            (else (mode #f) done)))
         (else #t)))))))

;;*** Help

(define (guidot-fossil-help-browser
         area
         #!key
         (done (%%macro-guidot-capture-guide-toplevel)))
  (define menu-height 20)
  (define label-width 1/4)
  (define basic (make-pin #t))
  ;; derived
  (define-values (xsw xno ysw yno) (guide-boundingbox->quadrupel area))
  (define total-height (- yno ysw))
  (define output-font (guide-select-font height: (floor (* 10/16 menu-height))))
  ;; GUI
  (define menu
    (let ((size 'small))
      (guide-table-layout
       (make-mdv-rect-interval xsw 0 xno menu-height)
       rows: 1 cols: 2 name: "Fossil Help Menu"
       (lambda (area row col)
         (guide-valuelabel
          in: area size: size label: "mode"
          label-width: label-width
          value: basic
          value-display: (lambda (x) (if x "basic" "extended"))
          input:
          (lambda (rect payload event xsw ysw)
            (cond
             ((eqv? event EVENT_BUTTON1DOWN)
              (basic (not (basic)))))
            #t)))
       (lambda (area row col)
         (guide-button in: area label: "x" guide-callback: done)))))
  (define output-control!)
  (define work-view
    (guide-table-layout
     (make-mdv-rect-interval xsw ysw xno (- yno menu-height))
     border-ratio: 1/20 name: "Fossil Help Browser"
     cols: 3 rows: 1
     (let ((basic-options
            (apply
             vector
             (fossil-help-basic-parse-output-to-commands (fossil-command repository: #f "help"))))
           (all-options
            (apply
             vector
             (fossil-help-all-parse-output-to-commands (fossil-command repository: #f "help" "-a"))))
           (layers (make-ggb size: 1)))
       (define (options) (if (basic) basic-options all-options))
       (ggb-insert! layers #f)
       (lambda (area row col)
         (define (update-options)
           (ggb-set!
            layers 0
            (guide-list-select-payload
             area options
             line-height: 21
             action:
             (lambda (n x)
               (%%guide-post-speculative/async
                (begin
                  (output-control! text: #f)
                  (let ((result (fossil-command repository: #f "help" (vector-ref (options) n))))
                    (output-control! insert: result))))))))
         (wire! basic post: update-options)
         (update-options)
         (guide-list-select-payload
          area
          options
          action:
          (lambda (n x)
            (%%guide-post-speculative/async
             (begin
               (output-control! text: #f)
               (let ((result (fossil-command repository: #f "help" (vector-ref (options) n))))
                 (output-control! insert: result))))))
         (guide-ggb-layout area layers fixed: #t direction: 'layer name: "fossil help command selection")))
     (let ((label "1-2"))
       (lambda (area row col)
         (guide-textarea-payload
          in: area
          data-char-encoding: #f data: (lambda _ #f)
          rows: 120
          font: output-font
          horizontal-align: 'left
          vertical-align: 'bottom
          readonly: #t
          wrap: #t
          results: (lambda (pl ctrl) (set! output-control! ctrl) pl)
          name: "fossil output")))
     #f))
  (define vbuf (make-ggb size: 2))
  (ggb-insert! vbuf menu)
  (ggb-insert! vbuf work-view)
  (guide-ggb-layout area vbuf direction: 'topdown fixed: #t name: "fossil help browser"))

(define (%%guidot-interactive dialog-control! #!key (insert #f))
  (lambda (constructor #!key in (done #f))
    (cond-expand
     (debug
      (set!
       constructor
       (let ((constructor constructor))
         (lambda args
           (with-debug-exception-catcher
            (lambda ()
              (apply constructor args)))))))
     (else))
    (letrec ((this
              (constructor
               in
               (lambda args
                 (match
                  args
                  (((? guide-payload? next) . _)
                   (guide-critical-add!
                    (lambda ()
                      (dialog-control! close: this)
                      (dialog-control! top: next))
                    async: #t))
                  (_ (dialog-control! close: this)))
                 #t))))
      (when (procedure? done) (done))
      (when insert (dialog-control! insert this))
      this)))

(define (%%guidot-interactive/kw dialog-control! #!key (insert #f))
  (lambda (constructor #!key in (done #f))
    (cond-expand
     (debug
      (set!
       constructor
       (let ((constructor constructor))
         (lambda args
           (with-debug-exception-catcher
            (lambda ()
              (apply constructor args)))))))
     (else))
    (letrec ((this
              (constructor
               in: in done:
               (lambda args
                 (match
                  args
                  (((? guide-payload? next) . _)
                   (guide-critical-add!
                    (lambda ()
                      (dialog-control! close: this)
                      (dialog-control! top: next))
                    async: #t))
                  (_ (dialog-control! close: this)))
                 #t))))
      (when (procedure? done) (done))
      (when insert (dialog-control! insert this))
      this)))

;;** Transfer Dialog

(define (guidot-fossil-transfer-dialog
         area #!key
         (done (%%macro-guidot-capture-guide-toplevel))
         (remote-fossil-title #f)
         (remote-fossil-key #f)
         (http-proxy-url
          (lambda _
            (let ((pn (beaver-proxy-port-number)))
              (and pn (string-append "http://127.0.0.1:" (number->string pn))))))
         (name "fossil transfer"))
  (define menu-font (guide-select-font size: 'small))
  (define output-font menu-font)
  (define menu-line-height (guide-font-height menu-font))
  (define status-items 3)
  (define menu-height (ceiling (* status-items menu-line-height)))
  (define label-width 1/4)
  (define mode (make-pin 'pull))
  (define mode->string symbol->string)
  (define remote-tag
    (cond
     ((pin? remote-fossil-title) remote-fossil-title)
     ((procedure? remote-fossil-title) (make-pin (remote-fossil-title)))
     (else (make-pin (or remote-fossil-title "")))))
  (define apikey
    (cond
     ((pin? remote-fossil-key) remote-fossil-key)
     ((procedure? remote-fossil-key) (make-pin (remote-fossil-key)))
     (else (make-pin (or remote-fossil-key "")))))
  (define remote-url current-fossil-remote-url)
  (define clone-target (make-pin ""))
  (define clone-target-pathname (make-pin #f))
  (define directory fossils-directory)
  ;; derived
  (define dialog-area area)
  (define-values (xsw xno ysw yno) (guide-boundingbox->quadrupel area))
  (define-values (result dialog-control!) (guidot-layers area name: name))
  (define interactive (%%guidot-interactive dialog-control! insert: top:))
  ;; GUI
  (define-values (output-textarea output-control!)
    (guide-textarea-payload
     in: (make-mdv-rect-interval xsw 0 xno (- yno menu-height))
     readonly: #t
     data: (lambda _ #f)
     data-char-encoding: #f
     rows: 120
     font: output-font
     horizontal-align: 'left
     vertical-align: 'bottom
     wrap: #t
     name: "fossil output"))
  (define (conv-empty2false src)
    (case-lambda
     (() (or (src) ""))
     ((val) (src (cond ((string-empty? val) #f) (else val))))))
  (define (select-local-name area done)
    (let ((label "Local Project Name"))
      (guide-value-edit-dialog
       name: label
       in: area
       done: done
       label: label
       keypad: guide-keypad/default
       on-key:
       (lambda (p/r key m)
         (let ((x (%%guide-textarea-keyfilter p/r key m)))
           (when (and x (or (eqv? key #\return) (eqv? key EVENT_KEYENTER)))
             (done))
           x))
       ;; validate: (macro-guidot-check-ggb/string-pred validate)
       data: clone-target)))
  (define (select-clone-source area done)
    (let ((options '#("Beaver Number" "VPN IP" "URL")))
      (guide-list-select-payload
       (guide-payload-measures output-textarea)
       (lambda () options)
       action:
       (lambda (n x)
         (done)
         (cond
          ((eqv? n 0) ;; Beaver Number
           (interactive
            (lambda (area done)
              (beaverchat-service-address-edit
               in: area label: "Beaver Number"
               input: clone-target
               done: done
               success:
               (lambda _
                 (define (err text)
                   (interactive
                    (lambda (area done)
                      (guide-button in: area label: text guide-callback: done))
                    in: area done: done))
                 (let ((project (clone-target)))
                   (cond
                    ((not project) (done))
                    ((not (fossils-directory)) (err "No Fossil Directory"))
                    ((not (beaver-captured-domain)) (err "No Domain Captured"))
                    ((string-match-unit-id+port? project)
                     (let* ((project (chat-number->neatstring (unit-id-string->unit-id project) "-"))
                            (fn (and project (path-normalize (fossils-project-filename project)))))
                       (cond
                        ((not fn) (done))
                        ((file-exists? fn) (err "Already Exists"))
                        (else
                         (mode 'clone)
                         (clone-target-pathname fn)
                         (current-fossil-remote-url
                          (string-append
                           project "." (beaver-captured-domain) "/"
                           project "/" project))
                         (done)))))
                    (else (err (string-append "pardon: '" project "'"))))))))
            in: area))
          ((eqv? n 0) ;; VPN IP
           (interactive
            (lambda (area done)
              (beaverchat-service-address-edit
               in: area label: "[IPv6]:PORT" input: (conv-empty2false current-fossil-remote-url)
               done: done
               success: (lambda _ (select-local-name area done))))
            in: area))
          (else
           (interactive
            (lambda (area done)
              (let ((label "Clone Source URL"))
                (guide-value-edit-dialog
                 name: label
                 in: area
                 done: done
                 label: label
                 keypad: guide-keypad/default
                 on-key:
                 (lambda (p/r key m)
                   (let ((x (%%guide-textarea-keyfilter p/r key m)))
                     (when (and x (or (eqv? key #\return) (eqv? key EVENT_KEYENTER)))
                       (interactive
                        (lambda (area done)
                          (select-local-name area done))
                        in: area done: done))
                     x))
                 ;; validate: (macro-guidot-check-ggb/string-pred validate)
                 data: (conv-empty2false current-fossil-remote-url))))
            in: area))))
       name: "select clone source kind")))
  (define menu
    (let ((size 'small))
      (define (mk-generic area row col)
        (let ((interactive (%%guidot-interactive/kw dialog-control! insert: top:)))
          (guidot-fossil-menu area interactive: interactive)))
      (define (mk-mode area row col)
        (guide-valuelabel
         in: area font: menu-font label: "mode"
         label-width: label-width
         value: mode
         value-display: mode->string
         input:
         (let ((active (make-pin #f)))
           (define (off was) (active #f) was) ;; tail position only!
           (lambda (rect payload event x y)
             (cond
              ((eqv? event EVENT_BUTTON1DOWN)
               (cond
                ((active) => off)
                (else
                 (let ((area (guide-payload-measures output-textarea)))
                   (interactive
                    (lambda (area control)
                      (active control)
                      (let ((options '#("clone" "sync" "pull" "push" "wiki")))
                        (guide-list-select-payload
                         (guide-payload-measures output-textarea)
                         (lambda () options)
                         action:
                         (lambda (n x)
                           (cond
                            ((eqv? n -1) ;; example
                             (interactive
                              (lambda (area control)
                                (guide-button
                                 in: area label: "gut"
                                 guide-callback: control))
                              in:
                              (case 2
                                ((1) (make-mdv-rect-interval 0 0 xno menu-height))
                                (else (guide-payload-measures output-textarea)))))
                            ((eqv? n 0) ;; clone
                             (current-fossil #f)
                             (current-fossil-remote-url "")
                             #;(interactive select-clone-source in: area)
                             (interactive select-clone-source in: area)
                             (off (active)))
                            ((eqv? n 4) ;; wiki
                             (done (guidot-fossil-wiki dialog-area))
                             (off (active)))
                            (else
                             (mode (string->symbol (vector-ref options n)))
                             ;; close
                             (off (active)))))
                         name: options)))
                    in: area))
                 #t)))
              (else #t))))))
      (define (mkmk-vale label value value-display validate)
        (lambda (area row col)
          (guide-valuelabel
           in: area size: size label: label
           label-width: label-width
           value: value
           value-display: (or value-display (lambda (obj) (if (string? obj) obj (object->string obj))))
           input:
           (lambda (rect payload event xsw ysw)
             (cond
              ((eqv? event EVENT_BUTTON1DOWN)
               (guide-critical-add!
                (lambda ()
                  (letrec
                      ((this
                        (guide-value-edit-dialog
                         name: label
                         in: dialog-area
                         done: (lambda _ (dialog-control! close: this))
                         label: label
                         keypad: guide-keypad/default
                         on-key:
                         (lambda (p/r key m)
                           (let ((x (%%guide-textarea-keyfilter p/r key m)))
                             (when (and x (or (eqv? key #\return) (eqv? key EVENT_KEYENTER)))
                               (guide-critical-add! (lambda () (dialog-control! close: this))))
                             x))
                         validate:
                         (macro-guidot-check-ggb/string-pred validate)
                         data-char-encoding: #f
                         data: value)))
                    (dialog-control! top: this))))))
             #t))))
      (define mk-rem
        (mkmk-vale
         "remote" remote-url (lambda (x) (if x x "n/a"))
         (lambda (str) ;; TBD: correct check
           (cond
            (else (>= (string-length str) 3))))))
      (define mk-tag
        (mkmk-vale
         "login" remote-tag #f
         (lambda (str) ;; TBD: correct check
           (cond
            (else (> (string-length str) 1))))))
      (define mk-key
        (mkmk-vale
         "key" apikey #f
         (lambda (str) ;; TBD: correct check
           (cond
            (else (> (string-length str) 1))))))
      (define (mk-go area row col)
        (define (go?)
          (and (http-proxy-url)
               (or (current-fossil-pathname)
                   (and (eq? (mode) 'clone) (clone-target-pathname)))))
        (define running #f)
        (guide-button
         name: 'fossil-run
         in: area name: "fossil go"
         label:
         (let ((ok "go") (no "n/a") (busy "busy"))
           (lambda () (cond (running busy) ((go?) ok) (else no))))
         ;; background-color: background-color color: color
         guide-callback:
         (lambda _
           (when (and (not running) (go?))
             (guide-critical-add!
              (let ((mode (mode))
                    (remote-url (remote-url))
                    (title (remote-tag))
                    (key (apikey))
                    (directory (directory))
                    (into (clone-target-pathname))
                    (proxy (http-proxy-url)))
                (lambda ()
                  (unless running
                    (set! running #t)
                    (output-control! text: #f)
                    (let ((output
                           (%%fossil-cmd
                            mode remote-url
                            title: title key: key
                            directory: directory
                            into: into
                            log: (lambda (args) (debug 'fossil-go args))
                            proxy: proxy)))
                      (output-control! insert: output wrap: #t)
                      (let ((summary (string-append "Exited with: " (number->string (/ (process-status output) 256)) "\n")))
                        (output-control! goto: position: 'absolute row: 1 col: 0)
                        (output-control! insert: summary wrap: #f)))
                    (set! running #f))))
              async: #t)))))
      (define (mk-kx area row col)
        (guide-button
         name: 'close/warning
         in: area
         label: "x" ;; background-color: background-color color: color
         guide-callback:
         (lambda _
           (letrec
               ((this
                 (guide-button
                  name: 'close
                  in: dialog-area
                  label: "sure" ;; background-color: background-color color: color
                  guide-callback:
                  (lambda _
                    (guide-critical-add!
                     (lambda ()
                       (dialog-control! close: this)
                       (done)))))))
             (dialog-control! top: this)))))
      (cond
       ((< (mdv-rect-interval-width area) 600)
        (guide-table-layout
         (make-mdv-rect-interval xsw 0 xno (floor (* 7 21/10 menu-line-height)))
         rows: 7 cols: 4
         mk-generic #f    #f     #f
         #t         #f    #f     #f
         #t         #f    #f     #f
         mk-mode    #f    mk-go  mk-kx
         mk-rem     #f    #f #f
         mk-tag     #f    #f #f
         mk-key     #f    #f #f
         ))
       (else
        (guide-table-layout
         (make-mdv-rect-interval xsw 0 xno (floor (* 6 21/10 menu-line-height)))
         rows: 6 cols: 4
         mk-generic #f    #f     #f
         #t         #f    #f     #f
         #t         #f    #f     #f
         mk-mode    #f    mk-go  mk-kx
         mk-rem     #f    #f #f
         mk-tag     #f    mk-key #f)))))
  (define vbuf (make-ggb size: 2))
  (ggb-insert! vbuf menu)
  (ggb-insert! vbuf output-textarea)
  (dialog-control! top: (guide-ggb-layout area vbuf direction: 'topdown fixed: #f name: name))
  result)

(define (guidot-make-fossil-wiki-constructor
         area #!key
         (font (guide-select-font size: 'medium))
         (keypad guide-keypad/default)
         (dialog-control (NYIE))
         (color
          (let* ((color (guide-select-color-1))
                 (r (color-red color))
                 (g (color-green color))
                 (b (color-blue color))
                 (a 140))
            (color-rgba r g b a)))
         (background (guide-background default: in: in))
         ;; replaceable pins
         (selected (make-pin initial: #f name: "wiki selected page"))
         ;; that's it
         (name "Fossil Wiki"))
  ;; (options '#("create" "diff" "get" "list" "preview" "save" "timeline"))
  (lambda (#!key (in (NYIE)) (command (NYIE)) (dismiss (NYIE)))
    (define wiki-selected selected)
    (define use-notes (make-pin #f))
    (define interactive (%%guidot-interactive dialog-control insert: top:))
    (define (fossil-failure-message area title port)
      (interactive
       (lambda (area done)
         (guide-button
          name: "fossil failed" in: area guide-callback: done
          label:
          (guide-textarea-payload
           readonly: #t wrap: #t
           in: (make-mdv-rect-interval 0 0 (mdv-rect-interval-width area) (mdv-rect-interval-height area))
           rows: 50
           horizontal-align: 'left
           vertical-align: 'bottom
           font: (guide-select-font size: 'small)
           ;; color: color-2 hightlight-color: color-4
           ;; background: #f
           data: (lambda _
                   (close-output-port port)
                   (let* ((str (read-line port #f))
                          (status (process-status port))
                          (msg
                           (string-append
                            "Failure in " title "\n"
                            "Fossil return code "
                            (cond
                             ((>= status 0) (number->string (/ status 256)))
                             (else (number->string status)))
                            " output:\n" (if (eof-object? str) "none" str))))
                     (log-error "fossil failed on " 1 title " "  msg)
                     msg))
           data-char-encoding: #f
           results: (lambda (pl ctrl) pl))))
       in: area))
    (define (wiki-list off)
      (guidot-frame
       (let ((options
              (cond
               ((use-notes) (list->vector (read-all (fossil-command "wiki" "list" "-t") read-line)))
               (else (fossil-command/json '((command . "wiki/list")))))))
         (lambda (area)
           (cond
            ((eof-object? options)
             (off))
            ((null? options)
             (let ((label "wiki list empty"))
               (guide-button name: label in: area label: label guide-callback: off)))
            (else
             (guide-list-select-payload
              area (lambda () options)
              action:
              (lambda (n x)
                (wiki-selected (vector-ref options n))
                (off)
                (%%guide-post-speculative (begin (dialog-control top: (with-page)) #t))))))))
       in: area
       border-ratio: 1/4
       color: color background: background
       name: "wiki list"))
    (define (get-wiki-page)
      (read-line
       (cond
        ((use-notes) (fossil-command "wiki" "export" "-t" (wiki-selected) "-"))
        (else (fossil-command "wiki" "export" (wiki-selected) "-")))
       #f))
    (define (with-page #!key (mode 'commit))
      (define page-content
        (make-pin
         initial: (get-wiki-page)
         filter: (lambda (o n) (if (equal? o n) o n))
         name: "wiki page content"))
      (define this
        (let ((options '#("get" "preview" "save"))
              (rows 50)
              (edit-control! #f))
          (guide-textarea-edit
           in: area name: "wiki page edit"
           menu:
           (guidot-texteditor-menu
            (lambda () edit-control!)
            in: area
            font: font
            action-save-callback: (lambda _ (page-content (edit-control! 'string)))
            action-reload-callback:
            (lambda _
              (%%guide-post-speculative
               (begin
                 (let ((update
                        (lambda ()
                          (let ((content (get-wiki-page)))
                            (lambda ()
                              (page-content content)
                              (guide-critical-add!
                               (lambda ()
                                 (edit-control! text: #f)
                                 (edit-control! insert: content))))))))
                   (kick! (box update)))
                 #t)))
            action-close-callback:
            (lambda _ (dialog-control close: this) #t)
            name: "wiki editor menu")
           keypad: keypad
           data: page-content
           data-char-encoding: #f
           horizontal-align: 'left
           label-properties:
           `((color: ,(guide-select-color-4))
             (horizontal-align: right))
           ;; is default: on-key: %%guide-textarea-keyfilter
           rows: rows
           results:
           (lambda (payload ctrl)
             (set! edit-control! ctrl)
             payload))))
      (wire!
       page-content post:
       (lambda ()
         (let ((content (page-content))
               (name (wiki-selected)))
           (when (and name content)
             (cond
              ((use-notes)
               (let ((comment
                      ;; (call-with-input-string content read-line)
                      (let ((ggb (make-ggb size: 30)))
                        (call-with-input-string
                         content
                         (lambda (port)
                           (let loop ((c (read-char port)))
                             (cond
                              ((eof-object? c))
                              ((eqv? c #\newline))
                              (else (ggb-insert! ggb c) (loop (read-char port)))))))
                        (ggb->string/encoding-utf8 ggb)))
                     (cmd (case mode
                            ((create) "create")
                            (else "commit"))))
                 (fossil-command input: content "wiki" cmd comment "-" "-t" name "-M" "text/x-markdown")))
              (else
               ;; ignoring the response upon success
               (fossil-command/json
                `((command . ,(case mode
                                ((create) "wiki/create")
                                (else "wiki/save")))
                  (payload
                   (name . ,name)
                   (content . ,content)
                   (mimetype . "text/x-markdown")
                   (contentFormat . "raw"))))))))))
      this)
    (let ((tl-options '#("create" "list" "timeline"#; "close")))
      (define (no-fossil-selected area done)
        (guide-button
         name: 'close
         in: area
         label: "No Fossil selected"
         guide-callback: done))
      (define (subtype-ctrl area)
        (guide-valuelabel
         in: area font: font label: "subtype"
         label-width: 1/2
         value: use-notes
         value-display: (lambda (x) (if x "notes" "pages"))
         input:
         (lambda (rect payload event xsw ysw)
           (cond
            ((eqv? event EVENT_BUTTON1UP)
             (use-notes (not (use-notes)))))
           #t)))
      (guide-list-select-payload
       (receive (xsw xno ysw yno) (guide-boundingbox->quadrupel area)
         (define lh (guide-font-height font))
         ;; TBD: clean up, manipulation of layout from strange place
         (dialog-control top: (subtype-ctrl (make-mdv-rect-interval xsw (- yno 60 lh) xno (- yno 60))))
         (make-mdv-rect-interval xsw ysw xno (- yno 60 lh)))
       (lambda () tl-options)
       action:
       (lambda (n x)
         (let ((ssc (vector-ref tl-options n)))
           (cond
            ((not (current-fossil))
             (dismiss)
             ((%%guidot-interactive dialog-control insert: top:)
              no-fossil-selected in: area)
             #t)
            ((equal? ssc "list")
             (dismiss)
             (letrec ((edit (wiki-list (lambda _ (dialog-control close: edit)))))
               (dialog-control top: edit)
               #t))
            ((equal? ssc "create")
             (dismiss)
             (cond
              ((use-notes)
               (wiki-selected "now")
               (dialog-control top: (with-page mode: 'create)))
              (else
               (let ((selfie (box #f)))
                 (dialog-control
                  top:
                  (let ((label "New Wiki Page"))
                    (guide-value-edit-dialog
                     name: label
                     in: area
                     done: (lambda _ (dialog-control close: selfie))
                     label: label
                     keypad: guide-keypad/default
                     on-key: %%guide-textarea-keyfilter
                     validate:
                     (lambda (ggb) ;; TBD: correct check
                       (let ((n (ggb-length ggb)))
                         (or (eqv? n 0) (> n 3))))
                     data:
                     (case-lambda
                      (() "")
                      ((val)
                       (dialog-control close: selfie)
                       (cond
                        ((equal? val "")) ;; abort
                        (else
                         (wiki-selected val)
                         (dialog-control top: (with-page mode: 'create))))))))
                  notify: selfie))))
             #t)
            ((equal? ssc "timeline")
             (guide-critical-add!
              (lambda ()
                (dismiss)
                (let* ((result (fossil-command/json '((command . "wiki/timeline"))))
                       (rows 50)
                       (buffer (make-ggb size: 2))
                       (all (box #f)))
                  (define-values (xsw xno ysw yno) (guide-boundingbox->quadrupel area))
                  (ggb-insert!
                   buffer
                   (guide-textarea-payload
                    readonly: #t
                    in: area
                    rows: rows
                    horizontal-align: 'left
                    vertical-align: 'bottom
                    font: font
                    ;; color: color-2 hightlight-color: color-4
                    ;; background: #f
                    data:
                    (lambda _
                      (call-with-output-string
                       (lambda (p)
                         (let* ((results
                                 (cond
                                  ((eof-object? result) '#())
                                  (else (cdr (assq 'timeline result)))))
                                (limit (vector-length results)))
                           (do ((i 0 (+ i 1)))
                               ((eqv? i limit)
                                (when (eqv? limit 0) (display "no results" p)))
                             (let* ((result (vector-ref results i))
                                    (timestamp (cdr (assq 'timestamp result)))
                                    (comment (cdr (assq 'comment result))))
                               (display
                                (date->string
                                 (time-utc->date (make-srfi19:time 'time-utc 0 timestamp)))
                                p)
                               (newline p)
                               (display comment p)
                               (newline p)))))))
                    data-char-encoding: #f
                    results: (lambda (pl ctrl) pl)))
                  (ggb-insert!
                   buffer
                   (let ((size 20))
                     (guide-button
                      name: 'close
                      in: (make-x0y0x1y1-interval/coerce (- xno size) (- yno size) xno yno)
                      label: "x"
                      guide-callback: (lambda _ (dialog-control close: all)))))
                  (dialog-control
                   top:
                   (guide-ggb-layout
                    area
                    buffer direction: 'layer
                    background: (guide-background default: in: area) background-color: color
                    fixed: #f)
                   notify: all)))
              async: #t)
             #t)
            (else ((debug 'close dismiss)) #t))))
       name: name))))

(define (guidot-fossil-wiki
         area #!key
         (done (%%macro-guidot-capture-guide-toplevel))
         (style (guide-current-style))
         (font (or (guide-style-ref style font:) (guide-select-font size: 'medium)))
         (keypad guide-keypad/default)
         (menu-height 55)
         (dialog-control #f)
         (color
          (let* ((color (guide-select-color-1))
                 (r (color-red color))
                 (g (color-green color))
                 (b (color-blue color))
                 (a 140))
            (color-rgba r g b a)))
         (background #f)
         (dismiss (lambda _ #t)) ;; experimental
         ;; replaceable pins
         (selected (make-pin initial: #f name: "wiki selected page"))
         ;; that's it
         (name "Fossil Wiki"))
  (define-values (xsw xno ysw yno) (guide-boundingbox->quadrupel area))
  (let* ((selfie (box #f))
         (dialog-control
          (or dialog-control
              (guidot-layers
               area name: name
               results:
               (lambda (payload control) (set-box! selfie payload) control)))))
    (dialog-control
     top:
     ((guidot-make-fossil-wiki-constructor
       area selected: selected
       dialog-control: dialog-control
       color: color background: background
       font: font
       name: name)
      in: (make-mdv-rect-interval xsw ysw xno (- yno menu-height))
      command: "wiki" dismiss: dismiss)
     notify: (and (not (unbox selfie)) selfie))
    (let ((interactive (%%guidot-interactive/kw dialog-control insert: top:)))
      (dialog-control
       top:
       (guidot-fossil-menu
        (make-mdv-rect-interval xsw (- yno menu-height) xno yno)
        interactive: interactive
        overlay-area: area)))
    (dialog-control
     top:
     (let ((size 20))
       (guide-button
        name: 'close
        in: (make-x0y0x1y1-interval/coerce (- xno size) (- yno size) xno yno)
        label: "x"
        guide-callback: done)))
    (unbox selfie)))

(define (guidot-fossil-browser
         area #!key
         (font (guide-select-font size: 'medium))
         (keypad guide-keypad/default)
         (menu-height 48)
         (name "Fossil Browser"))
  (define-values (xsw xno ysw yno) (guide-boundingbox->quadrupel area))
  (define total-height (mdv-rect-interval-height area))
  (define label-width 1/4)
  (define-pin mode initial: 'all pred: symbol? name: "Fossil Access Mode")
  (define wiki-selected (make-pin initial: #f name: "wiki selected page"))
  (define frame-color
    (let* ((color (guide-select-color-3))
           (r (color-red color))
           (g (color-green color))
           (b (color-blue color))
           (a 140))
      (color-rgba r g b a)))
  (define frame-background (guide-background default: in: area))
  (define-values (result-payload dialog-control!) (guidot-layers area name: name))
  (define (json-commands)
    (define json-commands-ggb (make-ggb))
    (define (def! name refinement)
      (ggb-insert! json-commands-ggb name)
      (ggb-insert! json-commands-ggb refinement))
    (define (ddef! name arg)
      (define options (apply vector arg))
      (define (next #!key (in (NYIE)) (command (NYIE)) (dismiss (NYIE)))
        (guide-list-select-payload
         in (lambda () options)
         action: dismiss))
      (def! name next))
    (def! "anonymousPassword" #f)
    (ddef! "artifact" '("name"))
    (ddef! "branch" '("create" "list" "new"))
    (def! "cap" #f)
    (ddef! "config" '("get" "save"))
    (ddef! "diff" '("v1" "v2"))
    (def! "dir" #f)
    (def! "g" #f)
    (ddef! "login" '("password"))
    (ddef! "logout" '("authinfo"))
    (ddef! "query" '("sql"))
    (def! "rebuild" #f)
    (ddef! "report" '("create" "get" "list" "run" "save"))
    (def! "resultCodes" #f)
    (def! "stat" #f)
    (ddef! "tag" '("add" "cancel" "find" "list"))
    (ddef! "timeline" '("branch" "chickin" "event" "ticket" "wiki"))
    (ddef! "user" '("save" "get" "list"))
    (def! "version" #f)
    (def! "whoami" #f)
    #;(ddef! "wiki" '("create" "diff" "get" "list" "preview" "save" "timeline"))
    (def! "wiki"
      (guidot-make-fossil-wiki-constructor
       area selected: wiki-selected
       dialog-control: dialog-control!
       color: frame-color background: frame-background
       font: font))
    (ggb->vector json-commands-ggb))
  (define json-commands-mdv
    (let ((vec (json-commands)))
      (make-mdvector
       (range (vector 2 (/ (vector-length vec) 2)))
       vec)))
  (define menu
    (guidot-fossil-menu
     (make-mdv-rect-interval xsw 0 xno menu-height)
     ;; (make-mdv-rect-interval xsw (- yno menu-height) xno yno)
     interactive:
     (let ((dispatch
            (lambda (this)
              (match-lambda
               ((? guide-payload? next)
                (guide-critical-add!
                 (lambda ()
                   (dialog-control! close: this)
                   (dialog-control! top: next))
                 async: #t))
               (else (dialog-control! close: this))))))
       (lambda (constructor #!key (in area))
         (letrec ((this (constructor in: in done: (dispatch this))))
           (dialog-control! top: this))))
     mode: mode))
  (define output-control!)
  (define work-view
    (guide-table-layout
     (make-mdv-rect-interval xsw ysw xno (- yno menu-height))
     border-ratio: 1/20 name: name
     cols: 4 rows: 1
     (let ((basic-options
            (apply
             vector
             (fossil-help-basic-parse-output-to-commands (fossil-command repository: #f "help"))))
           #;(all-options
           (apply
           vector
           (fossil-help-all-parse-output-to-commands (fossil-command repository: #f "help" "-a")))))
       (lambda (area row col)
         (guide-list-select-payload
          area (lambda () basic-options)
          action:
          (lambda (n x)
            (%%guide-post-speculative/async
             (begin
               (output-control! text: #f)
               (let ((result
                      (cond
                       (else (fossil-command (vector-ref basic-options n))))))
                 (output-control! insert: result))))))))
     (lambda (area row col)
       (guide-list-select-payload
        area
        (let* ((len (range-size (mdvector-range json-commands-mdv) 1))
               (names (make-vector len)))
          (do ((i 0 (+ i 1)))
              ((eqv? i len)
               (lambda () names))
            (vector-set! names i (mdvector-ref json-commands-mdv i 0))))
        action:
        (lambda (n x)
          (cond
           ((mdvector-ref json-commands-mdv n 1) =>
            (lambda (rest)
              (letrec
                  ((tbd
                    (rest
                     in: area
                     command: (mdvector-ref json-commands-mdv n 0)
                     dismiss:
                     (lambda _ (dialog-control! close: tbd)))))
                (dialog-control! top: tbd)
                #t)))
           (else
            (%%guide-post-speculative/async
             (begin
               (output-control! text: #f)
               (let ((result
                      (cond
                       (else (fossil-command "json" (mdvector-ref json-commands-mdv n 0))))))
                 (output-control! insert: result)))))))))
     (let ((label "1-2"))
       (lambda (area row col)
         (guide-textarea-payload
          in: area
          data-char-encoding: #f data: (lambda _ #f)
          rows: 120
          font: font
          horizontal-align: 'left
          vertical-align: 'bottom
          readonly: #t
          wrap: #t
          results: (lambda (pl ctrl) (set! output-control! ctrl) pl)
          name: "fossil output")))
     #f))
  (define vbuf (make-ggb size: 2))
  (ggb-insert! vbuf menu)
  (ggb-insert! vbuf work-view)
  (dialog-control! top: (guide-ggb-layout area vbuf direction: 'topdown fixed: #t))
  result-payload)
