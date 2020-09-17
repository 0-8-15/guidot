#|
(register-command! "beaver" beaver-start!)

(register-command! "cerberus" (lambda (cmd . args) (cerberus cmd args)))

(cond
 ((and (>= (system-cmdargc) 3) (equal? (system-cmdargv 1) "-s"))
  (execute-registered-command))
 (else #!void))
;;|#

(set! exit _exit) ;; FIXME: with lambdanative we see exit 0 always!

(define *registered-commands* '())

(define (register-command! cmd procedure)
  (set! *registered-commands* `((,cmd . ,procedure) . ,*registered-commands*)))

(define (cerberus1 cmd args #!key (input #f) (max-fast-restarts 5) (restart-time-limit 10))
  (define (once!)
    (let ((port (open-process `(path: ,cmd arguments: ,args stdout-redirection: #f))))
      (and (port? port)
           (begin
             (if input (write input port))
             (close-port port)
             ;; BEWARE: FIXME: HACK: This println is required (on
             ;; Linux) avoid hanging gambit in a endless loop eating
             ;; all memory!
             (println port: (current-error-port) "Watching " port)
             (eqv? (process-status port) 0)))))
  (define (endlessly!) (or (once!) (endlessly!)))
  ;; (set-process-name! (string-append "cerberus " (car args)))
  (_exit (if (with-exception-catcher (lambda (exn) #f) endlessly!) 0 1)))

(register-command! "cerberus" (lambda (args) (cerberus1 (system-cmdargv 0) (cdr args))))

(define (semi-fork cmd args)
  ;; (debug 'semi-fork `(,cmd . ,args))
  (log-debug "semi-fork " 1 cmd " on " args)
  (cond-expand
   (android
    (let ((datadir
           (jscheme-eval
            `(let* ((app ,(android-app-class))
                    (this ((method "me" app)))
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
                 (getDataDir (getApplicationContext this)))))))
      (if datadir
          (let ((exe (make-pathname (list (object->string datadir) "lib") (string-append "lib" cmd ".so"))))
            (log-debug "open-process " 1 exe " file exists? " (file-exists? exe))
            (with-exception-catcher
             (lambda (exn) (log-debug "open-process failed " 1 (debug 'fail (exception-->printable exn))) #f)
             (lambda () (open-process `(path: ,exe arguments: ,args  stdout-redirection: #f stdin-redirection: #f))))))))
   (linux
    (open-process `(path: ,(system-cmdargv 0) arguments: ("-s" ,cmd . ,args) stdout-redirection: #t stdin-redirection: #t)))
   (else
    (with-exception-catcher
     (lambda (exn) (log-debug "open-process failed " 1 (debug 'fail (exception-->printable exn))) #f)
     (lambda () (debug 'Nun (open-process `(path: #;"/proc/self/exe" ,(system-cmdargv 0) arguments: ("-s" ,cmd . ,args) stdout-redirection: #t stdin-redirection: #t))))))))

(define (semi-run cmd args)
  (let ((port (semi-fork cmd args)))
    (and (port? port)
         (begin
           (close-port port)
           (eqv? (debug 'ProcessStatus (process-status port)) 0)))))

(define (system-command-line* offset)
  (let loop ((n (system-cmdargc)) (r '()))
    (if (< n offset) r
	(let ((i (- n 1)))
	  (loop i (cons (system-cmdargv i) r))))))

(define (execute-registered-command)
  ;; (##clear-exit-jobs!)
  ;; (println "execute registered command")
  ;; (set! exit (lambda x ((c-lambda (int) void "exit") (if (pair? x) (car x) 0)))) ;; dunno yet where this segfaults
  ;; ((c-lambda () void "lambdanative_cutoff_unwind"))
  (let ((exe (system-cmdargv 0))
	(cmd (system-cmdargv 2)))
    (let ((e (assoc cmd *registered-commands*)))
      (if e ;; (debug 'CmdEntry e)
	  (begin
            (with-exception-catcher
             handle-replloop-exception
             (lambda ()
               ((cdr e) (system-command-line* 4))
               (exit 0)))
            (exit 42))
	  (exit 1)))))

(cond
 ((and (>= (system-cmdargc) 3) (equal? (system-cmdargv 1) "-s"))
  (execute-registered-command))
 (else #!void))

(cond-expand
 (debug
  ($debug-trace-triggers #t)
  ($async-exceptions 'catch)
  ) (else #f))

(define (portnumber-or-false? obj)
  (or (not obj) (and (fixnum? obj) (>= obj 0) (<= obj #xffff))))

(define (front-pin-attach-log! pin msg)
  (wire! pin post: (lambda () (log-debug msg 1 (debug msg (pin))))))

;;; make-pathname
(define make-pathname)
(define make-absolute-pathname)

(let* ((pds (system-pathseparator))
       (pdsc (string-ref pds 0)))

  (define (*char-pds? c) (char=? c pdsc))

  (define (chop-pds str)
    (and str
	 (let lp ((len (string-length str)))
	   (cond ((and (fx>= len 1)
		       (*char-pds? (string-ref str (fx- len 1))))
		  (lp (fx- len 1)))
		 ((fx< len (string-length str))
		  (substring str 0 len))
		 (else str)))))

  (define (conc-dirs dirs)
    ;; (##sys#check-list dirs 'make-pathname)
    (let loop ((strs dirs))
      (if (null? strs)
	  ""
	  (let ((s1 (car strs)))
	    (if (zero? (string-length s1))
		(loop (cdr strs))
		(string-append
		 (chop-pds (car strs))
		 pds
		 (loop (cdr strs))) ) ) ) ) )

  (define (canonicalize-dirs dirs)
    (cond ((or (not dirs) (null? dirs)) "")
	  ((string? dirs) (conc-dirs (list dirs)))
	  (else           (conc-dirs dirs)) ) )

  (define (_make-pathname loc dir file ext)
    (let ((ext (or ext ""))
	  (file (or file "")))
#|
      (##sys#check-string dir loc)
      (##sys#check-string file loc)
      (##sys#check-string ext loc)
|#
      (string-append
       dir
       (if (and (fx>= (string-length dir) 1)
		(fx>= (string-length file) 1)
		(*char-pds? (string-ref file 0)))
	   (substring file 1 (string-length file))
	   file)
       (if (and (fx> (string-length ext) 0)
		(not (char=? (string-ref ext 0) #\.)) )
	   "."
	   "")
       ext) ) )

  (set! make-pathname
    (lambda (dirs file #!optional ext)
      (_make-pathname 'make-pathname (canonicalize-dirs dirs) file ext)))

  (set! make-absolute-pathname
    (lambda (dirs file #!optional ext)
      (_make-pathname
       'make-absolute-pathname
       (let ((dir (canonicalize-dirs dirs)))
	 (if (absolute-pathname? dir)
	     dir
	     (string-append pds dir)) )
       file ext) ) ) )

;;; END make-pathname

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

(define command-line
  (let loop ((n (system-cmdargc)) (r '()))
    (if (< n 1) (lambda () r)
	(let ((i (- n 1)))
	  (loop i (cons (system-cmdargv i) r))))))

(cond
 ((and (>= (system-cmdargc) 2)
       (file-exists? (system-cmdargv 1)))
  (load (system-cmdargv 1)))
 ((>= (system-cmdargc) 2)
  (ot0cli-process-commands (cdr (command-line)) (if (null? (cdr (command-line))) replloop (lambda () #t))))
 (else (replloop)))

;; eof
