;;** GGB and GGB2d with encoding

;; Only UTF-8 really supported.

;; (u8vector->ggb u8 #!optional (start 0) (end (u8vector-length ggb)) #!key (encoding 'UTF-8))

;; (ggb->u8vector ggb #!optional (start 0) (end (ggb-length ggb)) #!key (encoding 'UTF-8))

;; (ggb->string ggb [encoding UTF-8]) --> string -- fresh string with elements from GGB
;; (fails when buffer contains no strings.)

;;*** GGB

(define (u8vector->ggb obj #!optional (start 0) (end (u8vector-length obj)) #!key (encoding 'UTF-8))
  (let ((result (make-ggb size: (- end start)))
        (source
         (cond
          ((eqv? end (u8vector-length obj)) obj)
          (else (subu8vector obj start end)))))
    (call-with-input-u8vector
     (list init: source char-encoding: encoding)
     (lambda (port)
       (do ((c (read-char port) (read-char port)))
           ((eof-object? c) result)
         (ggb-insert! result (char->integer c)))))))

(define (ggb->u8vector obj #!optional (start 0) (end (ggb-length ggb)) #!key (encoding 'UTF-8))
  (unless (ggb? obj) (error "invalid ggb" ggb->u8vector obj))
  (call-with-output-u8vector
   `(char-encoding: ,encoding eol-encoding: lf)
   (lambda (port)
     (ggb-for-each (lambda (obj) (display (integer->char obj) port)) start end)))
  #|
  (unless (or (eq? encoding 'UTF-8) (eq? encoding 'utf-8))
    (error "only UTF-8 encoding supported" ggb->u8vector encoding))
  (let* ((len (ggb-length obj))
         (i -1)
         (n 0)
         (result (make-u8vector len)))
    (define (shift!)
      (set! i (+ i 1))
      (when (eqv? i (##u8vector-length result))
        (let ((replacement (make-string (* 2 (####u8vector-length result)))))
          (##subu8vector-move! result 0 i replacement 0)
          (set! result replacement))))
    (ggb-for-each
     (lambda (c)
       (set! n (+ n 1))
       (cond
        ((##fx< c 128)
         (shift!)
         (##u8vector-set! result i (##integer->char c)))
        ((##fx< c 2048)
         (shift!)
         (##u8vector-set! result i (##integer->char (##fxior (##fxarithmetic-shift-right c 6) 192)))
         (shift!)
         (##u8vector-set! result i (##integer->char (##fxior (##fxand c 63) 128))))
        (else
         (shift!)
         (##u8vector-set! result i (##integer->char (##fxior (##fxarithmetic-shift-right c 12) 224)))
         (shift!)
         (##u8vector-set! result i (##integer->char (##fxior (##fxand (##fxarithmetic-shift-right c 6) 63) 128)))
         (shift!)
         (##u8vector-set! result i (##integer->char (##fxior (##fxand c 63) 128))))))
     obj)
    (set! i (+ i 1))
    (if (< i (##u8vector-length result)) (##subu8vector result 0 i) result))
|#
)

(define (ggb->string/encoding-utf8 ggb #!optional (start 0) (end (ggb-length ggb)))
  ;; Beware: UTF8 encoding *within* gambit strings seems questionable.
  (let* ((len (ggb-length ggb))
         (i -1)
         (n 0)
         (result (make-string len)))
    (define (shift!)
      (set! i (+ i 1))
      (when (eqv? i (##string-length result))
        (let ((replacement (make-string (* 2 (##string-length result)))))
          (substring-move! result 0 i replacement 0)
          (set! result replacement))))
    (ggb-for-each
     ggb
     (lambda (i c)
       (set! n (+ n 1))
       (cond
        ((##fx< c 128)
         (shift!)
         (##string-set! result i (##integer->char c)))
        ((##fx< c 2048)
         (shift!)
         (##string-set! result i (##integer->char (##fxior (##fxarithmetic-shift-right c 6) 192)))
         (shift!)
         (##string-set! result i (##integer->char (##fxior (##fxand c 63) 128))))
        (else
         (shift!)
         (##string-set! result i (##integer->char (##fxior (##fxarithmetic-shift-right c 12) 224)))
         (shift!)
         (##string-set! result i (##integer->char (##fxior (##fxand (##fxarithmetic-shift-right c 6) 63) 128)))
         (shift!)
         (##string-set! result i (##integer->char (##fxior (##fxand c 63) 128))))))
     start end)
    (set! i (+ i 1))
    (if (< i (##string-length result)) (substring result 0 i) result)))

(define (ggb->string ggb #!optional (start 0) (end (ggb-length ggb)) #!key (encoding 'UTF-8))
  (case encoding
    ((UTF-8) (ggb->string/encoding-utf8 ggb start end))
    (else
     (let ((result (make-string (fx- end start))))
       (ggb-for-each ggb (lambda (i v) (string-set! result i v)) start end)
       result))))

(define (utf8string->ggb str #!optional (encoding-error #f)) ;; deprecated
  ;;
  ;; Avoid UTF-8 encoded Gambit strings.  Those are one word per
  ;; character.
  (define (on-encoding-error i)
    (cond
     ((not encoding-error) #xfffd) ;; deliver replacement charater
     ((procedure? encoding-error) (encoding-error str i)) ;; delegate to caller
     (else (error "UTF8 encoding error" str i))))
  (let* ((len (string-length str))
         (result (make-ggb size: len)))
    (do ((i 0 (fx+ i 1)))
        ((eqv? i len) result)
      (let* ((c (string-ref str i))
             (ci (char->integer c)))
        (cond
         ((fx< ci #x80) (ggb-insert! result ci))
         (else
          (receive (size m1)
            (cond
             ((fx< ci #x80) (values 1 #x7f))
             ((fx< ci #xe0) (values 2 #x0f))
             ((fx< ci #xf0) (values 3 #x0f))
             ((fx< ci #xf8) (values 4 #x07))
             ((fx< ci #xfc) (values 5 #x03))
             (else (values 6 #x01)))
            (let ((limit (fx+ i size)))
              (let subc ((j (fx+ i 1)) (ci (##bitwise-and ci m1)))
                (cond
                 ((eqv? j limit) (ggb-insert! result ci) (set! i (##fx- j 1)))
                 ((eqv? j len)
                  (ggb-insert! result (on-encoding-error j))
                  (set! i (##fx- j 1)))
                 (else
                  (let ((cc (##char->integer (##string-ref str j))))
                    (if (or (##fx< cc #x80) (##fx>= cc #xc0))
                        (let ((ci (on-encoding-error j)))
                          (ggb-insert! result ci)
                          (set! i (##fx- j 1)))
                        (subc
                         (##fx+ j 1)
                         (##bitwise-ior
                          (##arithmetic-shift ci 6)
                          (##bitwise-and cc #x3F))))))))))))))))

;;*** GGB2d

(define (ggb2d->string/encoding-utf8 obj)
  ;; Beware: UTF8 encoding *within* gambit strings seems questionable.
  (let* ((len (ggb2d-total-length obj))
         (i -1)
         (n 0)
         (result (make-string len)))
    (define (shift!)
      (set! i (+ i 1))
      (when (eqv? i (##string-length result))
        (let ((replacement (make-string (* 2 (##string-length result)))))
          (substring-move! result 0 i replacement 0)
          (set! result replacement))))
    (ggb2d-for-each
     (lambda (c)
       (set! n (+ n 1))
       (cond
        ((##fx< c 128)
         (shift!)
         (##string-set! result i (##integer->char c)))
        ((##fx< c 2048)
         (shift!)
         (##string-set! result i (##integer->char (##fxior (##fxarithmetic-shift-right c 6) 192)))
         (shift!)
         (##string-set! result i (##integer->char (##fxior (##fxand c 63) 128))))
        (else
         (shift!)
         (##string-set! result i (##integer->char (##fxior (##fxarithmetic-shift-right c 12) 224)))
         (shift!)
         (##string-set! result i (##integer->char (##fxior (##fxand (##fxarithmetic-shift-right c 6) 63) 128)))
         (shift!)
         (##string-set! result i (##integer->char (##fxior (##fxand c 63) 128))))))
     obj)
    (set! i (+ i 1))
    (if (< i (##string-length result)) (substring result 0 i) result)))

(define (ggb2d->string obj #!optional (encoding 'UTF-8))
  (case encoding
    ((UTF-8) (ggb2d->string/encoding-utf8 obj))
    (else
     (call-with-output-string
      (lambda (port)
        (ggb2d-display-value-on-port
         obj port display: (lambda (v p) (display (integer->char c) p))))))))

(define (ggb2d->u8vector obj #!key (encoding 'UTF-8))
  (unless (ggb2d? obj) (error "invalid ggb2d" ggb2d->u8vector obj))
  #|
  (unless (or (eq? encoding 'UTF-8) (eq? encoding 'utf-8))
    (error "only UTF-8 encoding supported" ggb2d->u8vector encoding))
  (let* ((len (ggb2d-total-length obj))
         (i -1)
         (n 0)
         (result (make-u8vector len)))
    (define (shift!)
      (set! i (+ i 1))
      (when (eqv? i (##u8vector-length result))
        (let ((replacement (make-string (* 2 (####u8vector-length result)))))
          (##subu8vector-move! result 0 i replacement 0)
          (set! result replacement))))
    (ggb2d-for-each
     (lambda (c)
       (set! n (+ n 1))
       (cond
        ((##fx< c 128)
         (shift!)
         (##u8vector-set! result i (##integer->char c)))
        ((##fx< c 2048)
         (shift!)
         (##u8vector-set! result i (##integer->char (##fxior (##fxarithmetic-shift-right c 6) 192)))
         (shift!)
         (##u8vector-set! result i (##integer->char (##fxior (##fxand c 63) 128))))
        (else
         (shift!)
         (##u8vector-set! result i (##integer->char (##fxior (##fxarithmetic-shift-right c 12) 224)))
         (shift!)
         (##u8vector-set! result i (##integer->char (##fxior (##fxand (##fxarithmetic-shift-right c 6) 63) 128)))
         (shift!)
         (##u8vector-set! result i (##integer->char (##fxior (##fxand c 63) 128))))))
     obj)
    (set! i (+ i 1))
    (if (< i (##u8vector-length result)) (##subu8vector result 0 i) result))
  |#
  (call-with-output-u8vector
   `(char-encoding: ,encoding eol-encoding: lf)
   (lambda (port)
     (ggb2d-display-value-on-port
      obj port
      display: (lambda (v p) (display (integer->char c) p))))))

(define (u8vector->ggb2d obj #!optional (start 0) (end (u8vector-length obj)) #!key (encoding 'UTF-8))
  (let ((result (make-ggb2d))
        (source
         (cond
          ((eqv? end (u8vector-length obj)) obj)
          (else (subu8vector obj start end)))))
    (call-with-input-u8vector
     (list init: source char-encoding: encoding)
     (lambda (port) (ggb2d-insert-port! result port)))
    result))

(define (ggb2d-load-file name #!optional (char-encoding 'UTF-8))
  ;; TBD: try to optimize, loading 40MB takes 30'' wall clock time.
  (call-with-input-file
      `(path:
        ,name
        char-encoding: ,char-encoding
        ;; The 'lf eol-encoding is the only one that
        ;; does not make changes to serialization of
        ;; "\r", "\n" or "\r\n".
        eol-encoding: lf)
    (lambda (port)
      (let ((result (make-ggb2d))
            (line (make-ggb)))
        (do ((c (read-char port) (read-char port)))
            ((eof-object? c)
             (unless (eqv? (ggb-point line) 0)
               (ggb2d-insert-row! result line))
             (ggb2d-goto! result position: 'absolute row: 0 col: 0)
             result)
          (unless (eqv? c #\return)
            (ggb-insert! line (char->integer c)))
          (when (eqv? c #\newline)
            (ggb2d-insert-row! result line)
            (set! line (make-ggb))))))))
