;(include "~~tgt/lib/onetierzero/src/observable-notational-conventions.scm")

;;;* Code Maturity Level

(define current-code-maturity-level (make-parameter 0)) ;;

(define code-maturity-stable (make-parameter =))

(define (code-maturity-stable? #!optional (quality #f) (ccml #f))
  (or (equal? quality ccml)
      (let ((quality (or quality (current-code-maturity-level)))
            (ccml (or ccml (current-code-maturity-level))))
        ((code-maturity-stable) quality ccml))))

(define code-maturity-accept (make-parameter <=))

(define (code-maturity-accept? quality #!optional (ccml (current-code-maturity-level)))
  ((code-maturity-accept) quality ccml))

(define code-maturity-tolerate (make-parameter >=))

(define (code-maturity-tolerated? quality #!optional (ccml (current-code-maturity-level)))
  ((code-maturity-tolerate) quality ccml))

(define (code-maturity-level quality message #!optional location)
  (cond-expand
   (debug
    (unless (number? quality) (error "code-maturity-level: invalid quality" quality location))
    (unless (string? message) (error "code-maturity-level: message not a string" message location)))
   (else))
  (let ((ccml (current-code-maturity-level)))
    (cond
     ((code-maturity-stable? quality ccml))
     (((code-maturity-accept) quality ccml)
      (debug 'DEPRECATED (list message location))
      #t)
     (((code-maturity-tolerate) quality ccml)
      (debug 'TOLERATING (list message location))
      #t)
     (else (error message quality location)))))

(define $code-maturity-tolerance!
  (case-lambda
   ((x)
    (let ((close-enough (lambda (m c) (<= (abs (- m c)) x))))
      (code-maturity-stable close-enough)
      (code-maturity-accept (lambda (r c) (or (close-enough r c) (< r c))))
      (code-maturity-tolerate (lambda (r c) (or (close-enough r c) (> r c))))))
   ((l h)
    (code-maturity-stable =)
    (code-maturity-accept (lambda (m c) (<= l (- c m))))
    (code-maturity-tolerate (lambda (m c) (< (- c m) h))))
   ((l h x)
    (code-maturity-stable (lambda (m c) (<= (abs (- m c)) x)))
    (code-maturity-accept (lambda (m c) (<= l (- c m))))
    (code-maturity-tolerate (lambda (m c) (< (- c m) h))))))

;;;* Utilities

(define (read-file-as-u8vector fn)
  (and (file-exists? fn)
       (let* ((size (file-size fn))
              (data (make-u8vector size))
              (got (call-with-input-file fn (lambda (p) (read-subu8vector data 0 size p)))))
         (if (eqv? got size)
             data
             (error "read-file-as-u8vector failed. following file size read and size expected"
                    fn got size)))))

(define toplevel-read-file-as-u8vector (dynamic read-file-as-u8vector))

;;;* Notational Conventions (Observable Incubator)

(define-macro (define-macro/rt expr nexpr)
  `(begin
     (define-macro ,expr ,nexpr)
     (eval '(define-macro ,expr ,nexpr))))

(define-macro/rt (kick expr . more)
  `(kick! (lambda () ,expr . ,more)))

(define-macro/rt (kick/sync expr . more)
  `(kick/sync! (lambda () ,expr . ,more)))

(define-macro/rt (define-pin name . more)
  `(define ,name (make-pin . ,more)))

(define (make-sensor . more)
  (receive
   (in out) (apply make-mval (if (null? more) (list #f) more))
   (case-lambda
    (() out)
    ((x) (in x))
    #;((k p . more) ;; UNDEFINED behavior: maybe this should be an error.
     (cond
      ((not (or k p)) (out #f #f)) ;; INTERNAL API `connect`
      (else (error "make-sensor: unexpected case"))))
    (else (error "UNDEFINED")))))

(define-macro/rt (define-sense name . more)
  (let ((in (string->symbol (string-append "." (symbol->string name))))
        (in2 (string->symbol (string-append (symbol->string name) ":="))))
    `(begin
       (define ,in ,(if (null? more) `(make-sensor #f) `(make-sensor . ,more)))
       (define ,in2 ,in)
       (define ,name (,in)))))

;;
(define (make-sensor* . more)
  (define (make-mval* #!key (initial #!void) (pred #f) (filter #f) (name #f))
    (make-mval initial pred filter name))  (receive
   (in out) (apply make-mval* more)
   (case-lambda
    (() out)
    ((x) (in x))
    #;((k p . more) ;; UNDEFINED behavior: maybe this should be an error.
     (cond
      ((not (or k p)) (out #f #f)) ;; INTERNAL API `connect`
      (else (error "make-sensor*: unexpected case"))))
    (else (error "UNDEFINED")))))

(define (wire-trivial-async-alias-connection! from to)
  (wire!
   from       ;; Any change in `from` (COULD be a list) will trigger:
   post:      ;; in next step (i.e., after commit)
   (lambda () ;; `to` to be set to the value of `from`
     (to (from)))))

(define SENSOR
  (case-lambda
   (() (make-sensor #f))
   ((x) (make-sensor x))
   (args (apply make-sensor* args))))

(define-macro/rt (define-sense* name val . more)
  (let ((in (string->symbol (string-append "." (symbol->string name))))
        (in2 (string->symbol (string-append (symbol->string name) ":="))))
    `(begin
       (define ,in (SENSOR ,val . ,more))
       (define ,in2 ,in)
       (define ,name (,in)))))

(define-macro/rt (define-SENSOR name form)
  (if (not (eq? (car form) 'SENSOR))
      (error "define-SENSOR: missuse")
      (let* ((more (cdr form))
             (in (string->symbol (string-append "." (symbol->string name))))
             (in2 (string->symbol (string-append (symbol->string name) ":="))))
        `(begin
           (define ,in (SENSOR . ,more))
           (define ,in2 ,in)
           (define ,name (,in))))))

;;; Not only gambit specific, but runtime-only as well.

(define (%DEF+leading-dot name generator more)
  (let ((name_1 (string->symbol (string-append "." (symbol->string name))))
        (ref1 (##make-global-var name_1)))
    (##global-var-set! ref1 (apply generator more))
    (##global-var-set! (##make-global-var name) ((##global-var-ref name_1)))))

(define (SENSOR! name . more)
  (%DEF+leading-dot name SENSOR more))

;;;*** Regular Expressions

;; Avoid implementation detail leaking into app level.
(define-type regex macros: prefix: %rx- v)
(define-type regex-match macros: prefix: %rxm- v)

(define-macro (%%rxm-make res)
  (let ((tmp (gensym 'result)))
    `(let ((,tmp ,res))
       (and ,tmp (%rxm-make-regex-match ,tmp)))))

(define-macro (not-a-compiled-regular-expression) "not a regular expression")
(define-macro (check-compiled-regular-expression! loc x)
  `(unless (%rx-regex? ,x) (error (not-a-compiled-regular-expression) ,loc ,x)))

(define-macro (not-a-regular-expression-match) "not a regular expression match result")
(define-macro (check-regular-expression-match! loc x)
  `(unless (%rxm-regex-match? ,x) (error (not-a-regular-expression-match) ,loc ,x)))

(define (regex x . o) (%rx-make-regex (apply irregex x o)))
(define rx regex)
(define (regex? x) (%rx-regex? x))
(define (rx? x) (%rx-regex? x))
#;(define (regex-match rx str #!optional (start 0) (end (string-length str)))
  (check-compiled-regular-expression! regex-match rx)
  (%%rxm-make (irregex-match/chunked (%rx-regex-v rx) irregex-basic-string-chunker (list str start end))))
(define (regex-match rx str #!optional (start 0) (end (string-length str)))
  (check-compiled-regular-expression! regex-match rx)
  (%%rxm-make (irregex-match (%rx-regex-v rx) str start end)))
(define (rxm-ref rxm . opt)
  (check-regular-expression-match! rxm-ref rxm)
  (apply irregex-match-substring (%rxm-regex-match-v rxm) opt))
(define rx~/anchored regex-match)
(define rx~= regex-match)
(define (rx~=? . args) (and (apply regex-match args) #t))
#;(define (regex-search x str #!optional (start 0) (end (string-length str)))
  (check-compiled-regular-expression! regex-search x)
  (%%rxm-make (irregex-search/chunked (%rx-regex-v x) irregex-basic-string-chunker (list str start end) start)))
(define (regex-search x str #!optional (start 0) (end (string-length str)))
  (check-compiled-regular-expression! regex-search x)
  (%%rxm-make (irregex-search (%rx-regex-v x) str start end)))
(define rx~ regex-search)
(define-macro/rt (~ rx str . more) `(regex-search rx ,str . ,more)) ;; syntactic sugar
(define (rx-fold rx kons nil str . args) ;; FIXME: does NOT feed opaque matches into `kons`
  (check-compiled-regular-expression! rx-fold rx)
  (apply irregex-fold (%rx-regex-v rx) kons nil str args))
(define (rx// rx str . args)
  (check-compiled-regular-expression! rx-replace rx)
  (apply irregex-replace (%rx-regex-v rx) str args))
(define (rx-replace rx str . args)
  (check-compiled-regular-expression! rx-replace rx)
  (apply irregex-replace (%rx-regex-v rx) str args))
(define (rx//all rx str . args)
  (check-compiled-regular-expression! rx-replace rx)
  (apply irregex-replace/all (%rx-regex-v rx) str args))
(define (rx-replace/all rx str . args)
  (check-compiled-regular-expression! rx-replace rx)
  (apply irregex-replace/all (%rx-regex-v rx) str args))
(define rx-replace* rx-replace/all)
(define (rx-extract rx str . args)
  (check-compiled-regular-expression! rx-extract rx)
  (apply irregex-extract (%rx-regex-v rx) str args))
(define (rx-split rx str . args)
  (check-compiled-regular-expression! rx-split rx)
  (apply irregex-split (%rx-regex-v rx) str args))
