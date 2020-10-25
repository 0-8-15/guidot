#|
(register-command! "beaver" beaver-start!)

(register-command! "cerberus" (lambda (cmd . args) (cerberus cmd args)))

(cond
 ((and (>= (system-cmdargc) 3) (equal? (system-cmdargv 1) "-s"))
  (execute-registered-command))
 (else #!void))
;;|#

(define normal-exit exit)
;;(set! exit _exit) ;; FIXME: with lambdanative we see exit 0 always!

(define command-line
  (let loop ((n (system-cmdargc)) (r '()))
    (if (< n 1)
        (cond-expand
         (win32 (lambda () r))
         (else (lambda () r)))
	(let ((i (- n 1)))
	  (loop i (cons (system-cmdargv i) r))))))

(setup-child-interrupt-handling!)

(kick/sync! (lambda () (cerberus-verbose #f)))

(beaver-use-daemonize #t)

(register-command!
 "cerberus"
 (lambda (args)
   (cerberus (system-cmdargv 0) (cdr args) startup-delay: 3 max-fast-restarts: 2
             stdout-redirection: beaver-stdout-redirection)))

(cond
 ((and (>= (system-cmdargc) 3) (equal? (system-cmdargv 1) (daemonian-semifork-key)))
  (daemonian-execute-registered-command (system-cmdargv 2) 4))
 ;; Does not work for -l !
 #;((and (>= (system-cmdargc) 3) (equal? (system-cmdargv 1) "-D"))
  (daemonize
   (debug 'WAS `("cerberus" "beaver"
;     "-cs"  "daemonian-stdout-file" "NULL" "daemonian-stderr-file" "NULL"  ":"
     "-B" ,@(cddr (command-line))))))
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

;; ONLY with module "fossils"
(httpproxy-connect-set! ot0cli-connect)
(define (at-phone-decoder str)
  (let* ((e0 (string-contains (substring str 1 (string-length str)) "/"))
         (e (if e0 (+ e0 1)  (string-length str))))
    (if (and (> (string-length str) 1) (or (eqv? (string-ref str 0) #\/) (eqv? (string-ref str 0) #\@)))
        (unit-id-string->unit-id (substring str 1 e))
        (unit-id-string->unit-id (if (fx= e (string-length str)) str (substring str 0 e))))))
(httpproxy-atphone-set! at-phone-decoder)
(capture-domain! "beaver.dam" handler: fossils-directory-handler)
;;

(cond
 ((and (= (system-cmdargc) 2) (equal? (system-cmdargv 1) "-version"))
  (println (system-appversion)))
 ((and (>= (system-cmdargc) 2)
       (file-exists? (system-cmdargv 1)))
  (load (system-cmdargv 1)))
 ((>= (system-cmdargc) 2)
  (beaver-process-commands (cdr (command-line))))
 #;((>= (system-cmdargc) 2)
  (ot0cli-process-commands (cdr (command-line)) (if (null? (cdr (command-line))) replloop (lambda () #t))))
 (else (replloop)))

;; eof
