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

;; #!eof
