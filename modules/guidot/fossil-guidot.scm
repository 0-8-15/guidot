;;* Fossil GUI

;;** Utilities

;;*** Internal Utilities
(define (fossil-help-basic-parse-output-to-commands port)
  ;; crude, stupid, simple, just get it done
  (read-line port) ;; Usage...
  (read-line port) ;; Common commands.....
  (sort!
   string<?
   (let loop ((lines (string-split-char (port->string port) #\newline)))
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

(define (guidot-layers
         area #!key
         (dialog (make-ggb size: 2))
         (results values)
         (name "Guidot Layers"))
  ;; Note: as long as we have a plain ggb as INPUT, there is no way to
  ;; properly lock (using mutices at least).
  (define (push! payload #!key (notify #f))
    (assume (guide-payload? payload) "invalid" name payload)
    (check-not-observable-speculative! name key more)
    (ggb-goto! dialog (ggb-length dialog))
    (ggb-insert! dialog payload)
    (cond
     ((box? notify) (set-box! notify payload) payload))
    payload)
  (define (close! obj)
    (check-not-observable-speculative! name key more)
    (cond
     ((guide-payload? obj)
      (ggb-delete-first-match! dialog (lambda (x) (eq? x obj)))
      #t)
     ((and (box? obj) (guide-payload? (unbox obj)))
      (let ((payload (unbox obj)))
        (ggb-delete-first-match! dialog (lambda (x) (eq? x payload))))
      #t)
     (else (error "invalid payload" name key more))))
  (results
   (guide-ggb-layout area dialog direction: 'layer fixed: #t name: name)
   (lambda (key . more)
     (case key
       ((top:)
        (cond
         ((stm-atomic?) (apply push! more))
         (else (guide-critical-add! (lambda () (apply push! more))))))
       ((close:)
        (cond
         ((stm-atomic?) (apply close! more))
         (else (guide-critical-add! (lambda () (apply close! more))))))
       (else (error "invalid key" name key more))))))

;;*** Help

(define (guidot-fossil-help-browser area)
  (define menu-height 20)
  (define label-width 1/4)
  (define basic (make-pin #t))
  ;; derived
  (define-values (xsw xno ysw yno) (guide-boundingbox->quadrupel area))
  (define total-height (- yno ysw))
  ;; GUI
  (define menu
    (make-guide-table
     (make-mdvector
      (range '#(1 1))
      (let ((size 'small))
        (vector
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
              #t))))))
     in: (make-mdv-rect-interval xsw 0 xno menu-height)))
  (define output-control!)
  (define work-view
    (make-guide-table
     (make-mdvector
      (range '#(3 1))
      (vector
       (let ((basic-options
              (apply
               vector
               (fossil-help-basic-parse-output-to-commands (fossil-command "help"))))
             (all-options
              (apply
               vector
               (fossil-help-all-parse-output-to-commands (fossil-command "help" "-a"))))
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
                    (let ((result (fossil-command "help" (vector-ref (options) n))))
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
                 (let ((result (fossil-command "help" (vector-ref (options) n))))
                   (output-control! insert: result))))))
           (guide-ggb-layout area layers fixed: #t direction: 'layer name: "fossil help command selection")))
       (let ((label "1-2"))
         (lambda (area row col)
           (guide-textarea-payload
            in: area
            data: (lambda _ #f)
            rows: 120
            horizontal-align: 'left
            vertical-align: 'bottom
            readonly: #t
            wrap: #t
            results: (lambda (pl ctrl) (set! output-control! ctrl) pl)
            name: "fossil output")))
       #f))
     in: (make-mdv-rect-interval xsw ysw xno (- yno menu-height))
     border-ratio: 1/20 name: "Fossil Browser"))
  (define vbuf (make-ggb size: 2))
  (ggb-insert! vbuf menu)
  (ggb-insert! vbuf work-view)
  (guide-ggb-layout area vbuf direction: 'topdown fixed: #t name: "fossil help browser"))

(define (guidot-make-fossil-wiki-constructur
         area #!key
         (font (guide-select-font size: 'medium))
         (keypad guide-keypad/default)
         (menu-height 48)
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
    (define (wiki-list off)
      (guidot-frame
       (let ((options
              (let ((json (json-read (fossil-command "json" command "list"))))
                (cdr (assq 'payload json)))))
         (lambda (area)
           (guide-list-select-payload
            area (lambda () options)
            action:
            (lambda (n x)
              (wiki-selected (vector-ref options n))
              (off)
              (%%guide-post-speculative (begin (dialog-control top: (with-page)) #t))))))
       in: area
       border-ratio: 1/4
       color: color background: background
       name: "wiki list"))
    (define (get-wiki-page)
      (let* ((json (json-read (fossil-command "json" command "get" (wiki-selected))))
             (json-pl (cdr (assq 'payload json))))
        (cdr (assq 'content json-pl))))
    (define (with-page)
      (define page-content
        (make-pin
         initial: (get-wiki-page)
         filter: (lambda (o n) (if (equal? o n) o n))
         name: "wiki page content"))
      (define this
        (guidot-frame
         (let ((options '#("get" "preview" "save"))
               (rows 50))
           (lambda (area)
             (let ((edit-control! #f))
               (guide-textarea-edit
                in: area
                menu:
                (guidot-texteditor-menu
                 (lambda () edit-control!)
                 in: area
                 font: font
                 save-callback: (lambda _ (page-content (edit-control! 'string)))
                 reload-callback:
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
                 close-callback:
                 (lambda _ (dialog-control close: this)
                   #t)
                 name: "wiki editor menu")
                keypad: keypad
                data: page-content
                horizontal-align: 'left
                label-properties:
                `((color: ,(guide-select-color-4))
                  (horizontal-align: right))
                ;; is default: on-key: %%guide-textarea-keyfilter
                rows: rows
                results:
                (lambda (payload ctrl)
                  (set! edit-control! ctrl)
                  payload)))))
         in: area
         border-ratio: 1/8
         color: color background: background
         name: "wiki list"))
      (wire!
       page-content post:
       (lambda ()
         (let ((content (page-content))
               (name (wiki-selected)))
           (when (and name content)
             (let ((port (fossil-command/json)))
               (json-write
                `((command . "wiki/save")
                  (payload
                   (name . ,name)
                   (content . ,content)
                   (contentFormat . "raw")))
                port)
               (close-output-port port)
               ;; ignoring the response here
               (json-read port))))))
      this)
    (let ((tl-options '#("create" "list" "timeline")))
      (guidot-frame
       (lambda (area)
         (guide-list-select-payload
          area (lambda () tl-options)
          action:
          (lambda (n x)
            (let ((ssc (vector-ref tl-options n)))
              (cond
               ((equal? ssc "list")
                (dismiss)
                (letrec ((edit (wiki-list (lambda _ (dialog-control close: edit)))))
                  (dialog-control top: edit)
                  #t))
               ((equal? ssc "create")
                (dismiss)
                (let ((selfie (box #f)))
                  (dialog-control
                   top:
                   (let ((label "New Wiki Page"))
                     (guide-value-edit-dialog
                      name: label
                      in: area label: label
                      keypad: guide-keypad/default
                      on-key: %%guide-textarea-keyfilter
                      validate:
                      (macro-guidot-check-ggb/string-pred
                       (lambda (str) ;; TBD: correct check
                         (cond
                          ((equal? str "")) ;; abort
                          (else (> (string-length str) 3)))))
                      data:
                      (case-lambda
                       (() "")
                       ((val)
                        (dialog-control close: selfie)
                        (cond
                         ((equal? val "")) ;; abort
                         (else
                          (guide-critical-add!
                           (lambda ()
                             (let ((port (fossil-command/json)))
                               (json-write
                                `((command . "wiki/create")
                                  (payload
                                   (name . ,val)
                                   (content . "")
                                   (contentFormat . "raw")))
                                port)
                               (close-output-port port)
                               ;; ignoring the response here
                               (debug 'create-wiki-response (json-read port)))))))))))
                   notify: selfie))
                #t)
               (else
                (guide-critical-add!
                 (lambda ()
                   (dismiss)
                   (let ((result (json-read (fossil-command "json" command ssc)))
                         (rows 50))
                     (dialog-control
                      top:
                      (guide-textarea-payload
                       readonly: #t
                       in: area
                       rows: rows
                       horizontal-align: 'left
                       vertical-align: 'bottom
                       font: font
                       ;; color: color-2 hightlight-color: color-4
                       ;; background: #f
                       data: (lambda _ (call-with-output-string (lambda (p) (pp result p))))
                       results: (lambda (pl ctrl) pl))))))
                #t))))))
       in: area
       border-ratio: 1/4
       color: color background: background
       name: "wiki toplevel selection"))))

(define (guidot-fossil-wiki
         area #!key
         (font (guide-select-font size: 'medium))
         (keypad guide-keypad/default)
         (menu-height 48)
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
  (let* ((selfie (box #f))
         (dialog-control
          (or dialog-control
              (guidot-layers
               area name: name
               results:
               (lambda (payload control) (set-box! selfie payload) control)))))
    (dialog-control
     top:
     ((guidot-make-fossil-wiki-constructur
       area selected: selected
       dialog-control: dialog-control
       color: color background: background
       font: font menu-height: menu-height
       name: name)
      in: area command: "wiki" dismiss: dismiss)
     notify: (and (not (unbox selfie)) selfie))
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
  (define help (make-pin #t))
  (define wiki-selected (make-pin initial: #f name: "wiki selected page"))
  (define frame-color
    (let* ((color (guide-select-color-3))
           (r (color-red color))
           (g (color-green color))
           (b (color-blue color))
           (a 140))
      (color-rgba r g b a)))
  (define frame-background (guide-background default: in: area))
  (define-values (result-payload dialog-control!) (guidot-layers area))
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
      (guidot-make-fossil-wiki-constructur
       area selected: wiki-selected
       dialog-control: dialog-control!
       color: frame-color background: frame-background
       font: font menu-height: menu-height))
    (ggb->vector json-commands-ggb))
  (define json-commands-mdv
    (let ((vec (json-commands)))
      (make-mdvector
       (range (vector 2 (/ (vector-length vec) 2)))
       vec)))
  (define menu
    (make-guide-table
     (make-mdvector
      (range '#(2 2))
      (let ((size 'small))
        (vector
         (lambda (area row col)
           (guide-valuelabel
            in: area size: size label: "directory"
            label-width: label-width
            value: fossils-directory
            value-display: (lambda (x) (if x x "n/a"))
            input:
            (lambda (rect payload event xsw ysw)
              (cond
               ((eqv? event EVENT_BUTTON1DOWN)
                (NYI)))
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
                (NYI)))
              #t)))
         (lambda (area row col)
           (guide-valuelabel
            in: area size: size label: "help"
            label-width: label-width
            value: help
            value-display: (lambda (x) (if x "X" "-"))
            input:
            (lambda (rect payload event xsw ysw)
              (cond
               ((eqv? event EVENT_BUTTON1DOWN)
                (help (not (help)))))
              #t)))
         (lambda (area row col)
           (guide-valuelabel
            in: area size: size label: ""
            label-width: label-width
            value: (lambda _ #f)
            value-display: (lambda (x) (if x "" ""))
            input:
            (lambda (rect payload event xsw ysw)
              (cond
               ((eqv? event EVENT_BUTTON1DOWN)
                (NYI)))
              #t))))))
     in: (make-mdv-rect-interval xsw 0 xno menu-height)))
  (define output-control!)
  (define work-view
    (make-guide-table
     (make-mdvector
      (range '#(4 1))
      (vector
       (let ((basic-options
              (apply
               vector
               (fossil-help-basic-parse-output-to-commands (fossil-command "help"))))
             #;(all-options
             (apply
             vector
             (fossil-help-all-parse-output-to-commands (fossil-command "help" "-a")))))
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
                         ((help) (fossil-command "help" (vector-ref basic-options n)))
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
            data: (lambda _ #f)
            rows: 120
            horizontal-align: 'left
            vertical-align: 'bottom
            readonly: #t
            wrap: #t
            results: (lambda (pl ctrl) (set! output-control! ctrl) pl)
            name: "fossil output")))
       #f))
     in: (make-mdv-rect-interval xsw ysw xno (- yno menu-height))
     border-ratio: 1/20 name: name))
  (define vbuf (make-ggb size: 2))
  (ggb-insert! vbuf menu)
  (ggb-insert! vbuf work-view)
  (dialog-control! top: (guide-ggb-layout area vbuf direction: 'topdown fixed: #t))
  result-payload)
