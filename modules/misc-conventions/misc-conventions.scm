;(include "~~tgt/lib/onetierzero/src/observable-notational-conventions.scm")

;;;* Code Maturity Level

;;** Usage

(define (NYI . args)
  ;; A textual upper case marker for things in planning phase.  A three
  ;; letter acronym and a warning when used as procedure.  More use
  ;; cases might be added withouth harm.
  (println port: (current-error-port) "NYI (Not Yet Implemented) " args))

(define (NYIE . args)
  ;; Variant of NYI rasing an error exception.
  (let ((msg "NYI (Not Yet Implemented) "))
    (println port: (current-error-port) msg args)
    (error msg args)))

;;; Use (unstable) $maturity-tolerance! as TOLERATE and
;;; (current-maturity-level) as CCML and the first argument to
;;; `maturity-level` (`quality`) as QUALITY in phase:
;;;
;;; 1) "assessement, explore, draft, experiment" as (TOLERATE 100) and
;;; freely use `maturity-level` (recommended via macro expasion, see
;;; below) with values around zero to label maturity with negative
;;; values for legacy and positive values for imature sections.
;;; Values up to 100 off from QUALTIY are accepted.
;;;
;;; 2) during debug, verification, profiling, testing etc.:
;;;
;;;    a) (TOLERATE LOWER UPPER) : warn unless CCML equals QUALITY
;;;
;;;    b) (TOLERATE LOWER UPPER SILENT) : warn wenn CCML differs more
;;;       than SILENT from QUALITY.
;;;
;;;    Values of CCML outside the interval [LOWER UPPER] shall abort
;;;    execution with error MESSAGE and LOCATION from the second and
;;;    third parameter of `maturity-level`.
;;;
;;;    Use `maturity-level` with a parameter to adjust the current
;;;    values of CCML for diagnosis, trial and error.
;;;
;;;    Dispatch on `maturity-level` without parameter selects
;;;    code path.  Beware that such dispatches are invalid latest
;;;    after testing.
;;;
;;; 3) testing and refinement: use (TOLERATE 0) and review expectable
;;; violations.


;;** API

;; This API is assumed to be hidden/removed at compile time, e.g. by a
;; application defined macro.

(define current-maturity-level
  ;; COMPILE TIME: should be assumed to be fatal to access eventually.
  ;;
  ;; default: stable
  (make-parameter 0))

;;; (maturity-level quality message #!optional location) : #!void
;;;
;;; Parameters:
;;; - quality: development stat relative of "stable"
;;; - message: comment on reason
;;; - location: source code location to report

;;** CML Implementation

;; TBD: maturity code is immature and unstable.

(define (maturity-current? #!optional (offset 0))
  ;; "zero" tolerance
  (let* ((ccml (current-maturity-level))
         (quality (+ ccml offset)))
    (cond
     ((eq? quality ccml)) ;; best case; don't dare to waste time
     ((and (number? ccml) (number? quality)) (= ccml quality))
     (else (equal? ccml quality)))))

(define maturity-stable-pred (make-parameter =))

(define (maturity-stable? #!optional (quality #f) (ccml #f))
  (or (equal? quality ccml)
      (let ((quality (or quality (current-maturity-level)))
            (ccml (or ccml (current-maturity-level))))
        ((maturity-stable-pred) quality ccml))))

(define maturity-accept-pred (make-parameter <=))

(define maturity-accept maturity-accept-pred)

(define (maturity-accept? quality #!optional (ccml (current-maturity-level)))
  ((maturity-accept-pred) quality ccml))

(define maturity-tolerate-pred (make-parameter >=))

(define (maturity-tolerated? quality #!optional (ccml (current-maturity-level)))
  ((maturity-tolerate-pred) quality ccml))

(define ##print-maturity-warnings!)

(define ##MATURITY)

(define maturity-level
  (let ((seen (make-table test: equal?)))
    (define (print-maturity-warnings!)
      (let ((port (current-error-port)))
        (unless (eqv? (table-length seen) 0)
          (println port: port "MATURITY warnings REPORT")
          (table-for-each
           (lambda (k v)
             (let ((q (vector-ref k 0))
                   (msg (vector-ref k 1))
                   (loc (vector-ref k 2))
                   (calls (vector-ref v 0)))
               (println port: port "    " q " " msg " @ " loc " # " calls)))
           seen))))
    (define (count quality message location)
      (let* ((key (vector quality message location))
             (entry (table-ref seen key #f)))
        (if entry
            (let ((sumidx 0))
              (vector-set! entry sumidx (fx+ (vector-ref entry sumidx) 1)))
            (begin
              (table-set! seen key (vector 1))
              (debug quality (list message location))))
        #t))
    (define (maturity-level quality message #!key (loc '|unspecified location|) #!rest details)
      (define location loc)
      (cond-expand
       (debug
        (unless (number? quality) (error "maturity-level: invalid quality" quality location))
        (unless (string? message) (error "maturity-level: message not a string" message location)))
       (else))
      (let ((ccml (current-maturity-level))
            (give-developer-a-moment-to-think-about-the-unexpected-situation! thread-sleep!))
        (cond
         ((maturity-stable? quality ccml))
         (((maturity-accept-pred) quality ccml) (count 'DEPRECATED message location))
         (((maturity-tolerate-pred) quality ccml) (count 'TOLERATING message location))
         (else
          ;; (error message quality location)
          (count '|MATURITY OUTRIGHT REJECTED| message location)
          (let ((port (current-error-port)))
            (for-each (lambda (d) (pp d port)) details)
            (println
             port: port
             "  declared quality: "  quality
             " current maturity level: " ccml)
            (print-maturity-warnings!)
            (give-developer-a-moment-to-think-about-the-unexpected-situation! 1)
            (println port: port "... TERMINATING")
            (give-developer-a-moment-to-think-about-the-unexpected-situation! 2))
          (exit 23)))))
    (##add-exit-job! print-maturity-warnings!)
    (set! ##MATURITY count)
    (set! ##print-maturity-warnings! print-maturity-warnings!)
    maturity-level))

(define maturity-note maturity-level)
(define MATURITY maturity-note)
(define (MATURITY-LEVEL) (current-maturity-level)) ;; unable to set it
(define (MATURITY-TARGET) (maturity-current?)) ;; experimental!

(define $maturity-tolerance!
  (case-lambda
   ((x)
    (let ((close-enough (lambda (m c) (<= (abs (- m c)) x))))
      (maturity-stable-pred close-enough)
      (maturity-accept-pred (lambda (r c) (or (close-enough r c) (< r c))))
      (maturity-tolerate-pred (lambda (r c) (or (close-enough r c) (> r c))))))
   ((l h)
    (maturity-stable-pred =)
    (maturity-accept-pred (lambda (m c) (>= l (- c m))))
    (maturity-tolerate-pred (lambda (m c) (< (- c m) h))))
   ((l h x)
    (maturity-stable-pred (lambda (m c) (<= (abs (- m c)) x)))
    (maturity-accept-pred (lambda (m c) (>= l (- c m))))
    (maturity-tolerate-pred (lambda (m c) (< (- c m) h))))))

;;;* Call Caching

(define $memoize-active (make-parameter #t)) ;; to disable for debug and test

(define memoize-last
  ;; intensionaly restricted to four arguments at most
  (case-lambda
   ((f cmp)
    (unless (procedure? cmp) (error "not a procedure: cmp" memoize-last f cmp))
    (let ((last-arg #f)
          (last-vals #f))
      (lambda (arg)
        (if ($memoize-active)
            (begin
              (unless (and last-vals (cmp last-arg arg))
                (receive vals (f arg)
                  (set! last-vals vals)
                  (set! last-arg arg)))
              (apply values last-vals))
            (f arg)))))
   ((f cmp1 cmp2)
    (unless (procedure? cmp1) (error "not a procedure: cmp" memoize-last f cmp1))
    (unless (procedure? cmp2) (error "not a procedure: cmp" memoize-last f cmp2))
    (let ((last-arg1 #f)
          (last-arg2 #f)
          (last-vals #f))
      (lambda (arg1 arg2)
        (if ($memoize-active)
            (begin
              (unless (and last-vals (cmp1 last-arg1 arg1) (cmp2 last-arg2 arg2))
                (receive vals (f arg1 arg2)
                  (set! last-vals vals)
                  (set! last-arg1 arg1)
                  (set! last-arg2 arg2)))
              (apply values last-vals))
            (f arg1 arg2)))))
   ((f cmp1 cmp2 cmp3)
    (unless (procedure? cmp1) (error "not a procedure: cmp" memoize-last f cmp1))
    (unless (procedure? cmp2) (error "not a procedure: cmp" memoize-last f cmp2))
    (unless (procedure? cmp3) (error "not a procedure: cmp" memoize-last f cmp3))
    (let ((last-arg1 #f)
          (last-arg2 #f)
          (last-arg3 #f)
          (last-vals #f))
      (lambda (arg1 arg2 arg3)
        (if ($memoize-active)
            (begin
              (unless (and last-vals (cmp1 last-arg1 arg1) (cmp2 last-arg2 arg2)
                           (cmp3 last-arg3 arg3))
                (receive vals (f arg1 arg2 arg3)
                  (set! last-vals vals)
                  (set! last-arg1 arg1)
                  (set! last-arg2 arg2)
                  (set! last-arg3 arg3)))
              (apply values last-vals))
            (f arg1 arg2 arg3)))))
   ((f cmp1 cmp2 cmp3 cmp4)
    (unless (procedure? cmp1) (error "not a procedure: cmp" memoize-last f cmp1))
    (unless (procedure? cmp2) (error "not a procedure: cmp" memoize-last f cmp2))
    (unless (procedure? cmp3) (error "not a procedure: cmp" memoize-last f cmp3))
    (unless (procedure? cmp4) (error "not a procedure: cmp" memoize-last f cmp4))
    (let ((last-arg1 #f)
          (last-arg2 #f)
          (last-arg3 #f)
          (last-arg4 #f)
          (last-vals #f))
      (lambda (arg1 arg2 arg3 arg4)
        (if ($memoize-active)
            (begin
              (unless (and last-vals (cmp1 last-arg1 arg1) (cmp2 last-arg2 arg2)
                           (cmp3 last-arg3 arg3) (cmp4 last-arg4 arg4))
                (receive vals (f arg1 arg2 arg3 arg4)
                  (set! last-vals vals)
                  (set! last-arg1 arg1)
                  (set! last-arg2 arg2)
                  (set! last-arg3 arg3)
                  (set! last-arg4 arg4)))
              (apply values last-vals))
            (f arg1 arg2 arg3 arg3)))))))

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
