;; (C) 2021 JFW - GGB Generic Gap Buffer

;;* API

;; (ggb? OBJ) --> boolean

;; (make-ggb #!key (size 30)) --> GGB

;; (ggb-length GGB) --> non-negative fixnum

;; (ggb-point GGB) --> fixnum [0..ggb-length)

;; (ggb-ref GGB INDEX) --> *

;; (ggb-set! GGB INDEX VALUE) -> #!void -- set value

;; (ggb-insert! GGB VALUE) -> #!void -- insert at point and move point

;; (ggb-insert-sequence! GGB SEQUENCE) -> #!void -- insert all
;; elements of SEQUENCE starting at point and move point.  Supported
;; sequences (so far): vector? string?

;; (ggb-delete! GGB COUNT) -> boolean -- delete (up to) CONT elements
;; forward, if count is positive, backward if negative.  If this would
;; delete more than avail, return #f.

;; (ggb-clear! GGB) -> #!void -- remove all content

;; (ggb-goto! GGB INDEX) -> #!void -- move point to INDEX

;; (ggb-for-each GGB PROC) -> #!void -- call 2ari PROC with consecutive
;; index and value from buffer.  (NOT including the gap.)

;; ggb-for-each-rtl same as ggb-for-each index sequence descending

;; (ggb->vector ggb) --> vector -- fresh vector with elements from GGB

;; (ggb->string ggb) --> string -- fresh string with elements from GGB
;; (fails when buffer contains no strings.)

;; (ggb-goto-left! GGB #!optional n)
;; (ggb-goto-right! GGB #!optional n)

;;* Implementation

(define-structure ggb ;; generic gap buffer
  opaque:
  macros:
  prefix: macro-
  (point unprintable:) ;; point
  (rest unprintable:)  ;; zero in rest
  (buffer unprintable:))

(define (ggb? x) (macro-ggb? x))

(define (make-ggb #!key (size 30))
  (unless (and (integer? size) (exact? size) (fx>= size 0))
    (error "invalid size" make-ggb size))
  (macro-make-ggb 0 size (make-vector size #f)))

(define (ggb-clear! ggb)
  (let ((point (macro-ggb-point ggb))
        (rest (macro-ggb-rest ggb))
        (buffer (macro-ggb-buffer ggb)))
    (when (fx> point 0) (subvector-fill! 0 point #f))
    (let ((limit (vector-length buffer)))
      (when (fx< rest limit) (subvector-fill! rest limit #f))
      (macro-ggb-point-set! ggb 0)
      (macro-ggb-rest-set! ggb limit))))

(define (ggb-point ggb) (macro-ggb-point ggb))

(define (ggb-length ggb)
  (unless (macro-ggb? ggb) (error "invalid gap buffer" ggb-length ggb))
  (fx+ (macro-ggb-point ggb) (fx- (##vector-length (macro-ggb-buffer ggb)) (macro-ggb-rest ggb))))

(define (ggb-ref ggb i)
  (unless (and (macro-ggb? ggb) (integer? i) (exact? i))
    (error "invalid arguments" ggb-ref ggb i))
  (let ((point (ggf-point ggb))
        (buffer (macro-ggb-buffer ggb)))
    (if (fx< i point) (##vector-ref buffer i)
        (let ((i (fx+ (macro-ggb-rest ggb) i)))
          (if (fx< i (##vector-length buffer))
              (##vector-ref buffer i)
              (error "out of range" ggb-ref i))))))

(define (ggb-set! ggb i v)
  (unless (and (macro-ggb? ggb) (integer? i) (exact? i))
    (error "invalid arguments" ggb-ref ggb i))
  (let ((point (ggf-point ggb))
        (buffer (macro-ggb-buffer ggb)))
    (if (fx< i point) (vector-set! buffer i v)
        (let ((i (fx+ (macro-ggb-rest ggb) i)))
          (if (fx< i (vector-length buffer))
              (vector-set! buffer i v)
              (error "out of range" ggb-ref i))))))

(define (%%ggb-grow-gap! ggb size)
  (let ((point (fx+ (macro-ggb-point ggb) 1))
        (rest (macro-ggb-rest ggb))
        (buffer (macro-ggb-buffer ggb))
        (max-grow-length 100))
    (cond
     ((fx>= (fx+ point size) rest)
      (cond
       ((eqv? rest (vector-length buffer))
        (let ((insert (min max-grow-length (max size (vector-length buffer)))))
          (macro-ggb-buffer-set! ggb (vector-append buffer (make-vector insert #f)))
          (macro-ggb-rest-set! ggb (fx+ rest insert))))
       (else
        (let* ((len (vector-length buffer))
               (insert (min max-grow-length (max size (vector-length buffer))))
               (new-rest (fx+ rest insert))
               (new (make-vector (fx+ len insert) #f)))
          (subvector-move! buffer 0 point new 0)
          (subvector-move! buffer rest len new new-rest)
          (macro-ggb-buffer-set! ggb new)
          (macro-ggb-rest-set! ggb new-rest))))))))

(define (ggb-insert! ggb val)
  (if (not (macro-ggb? ggb))
      (error "invalid gap buffer" ggb-insert! ggb)
      (begin
        (%%ggb-grow-gap! ggb 1)
        (let ((point (macro-ggb-point ggb))
              (rest (macro-ggb-rest ggb))
              (buffer (macro-ggb-buffer ggb)))
          (vector-set! buffer point val)
          (macro-ggb-point-set! ggb (fx+ (macro-ggb-point ggb) 1))))))

(define (ggb-insert-sequence! ggb val)
  (unless (macro-ggb? ggb) (error "invalid gap buffer" ggb-insert-sequence! ggb))
  (let* ((val
          (cond
           ((vector? val) val)
           ((pair? val) (apply vector val))
           (else (error "unsupported sequence" 'ggb-insert-sequence! val))))
         (length (vector-length val)))
    (%%ggb-grow-gap! ggb length)
    (let ((point (macro-ggb-point ggb))
          (rest (macro-ggb-rest ggb))
          (buffer (macro-ggb-buffer ggb)))
      (do ((i 0 (fx+ i 1)) (point point (fx+ point 1)))
          ((eqv? i length) (macro-ggb-point-set! ggb point))
        (vector-set! buffer point (vector-ref val i))))))

(define (ggb-delete! ggb n)
  (unless (macro-ggb? ggb) (error "invalid gap buffer" ggb-delete! ggb))
  (unless (integer? n) (exact? n) (error "invalid count" ggb-delete! n))
  (cond
   ((fx> n 0)
    (let* ((rest (macro-ggb-rest ggb))
           (buffer (macro-ggb-buffer ggb))
           (limit (vector-length buffer))
           (to (fx+ rest n))
           (new-rest (if (fx> to limit) limit to)))
      (subvector-fill! buffer rest new-rest #f)
      (macro-ggb-rest-set! ggb new-rest)
      (eqv? to new-rest)))
   ((fx< n 0)
    (let* ((point (macro-ggb-point ggb))
           (to (fx+ point n))
           (new-point (if (fx>= to 0) to 0))
           (buffer (macro-ggb-buffer ggb)))
      (subvector-fill! buffer new-point point #f)
      (macro-ggb-point-set! ggb new-point)
      (eqv? to new-point)))
   (else #t)))

(define (ggb-goto! ggb i)
  (if (not (and (macro-ggb? ggb) (integer? i) (exact? i) (fx>= i 0)))
      (error "invalid argument" ggb-goto! ggb i)
      (let ((point (macro-ggb-point ggb))
            (rest (macro-ggb-rest ggb))
            (buffer (macro-ggb-buffer ggb)))
        (cond
         ((eqv? i (macro-ggb-point ggb)))  ;; nothing to be done
         ((< i point)
          (let ((new-rest (fx- rest (fx- point i))))
            (subvector-move! buffer i point buffer new-rest)
            (subvector-fill! buffer i new-rest #f)
            (macro-ggb-rest-set! ggb new-rest)
            (macro-ggb-point-set! ggb i)))
         (else
          (let* ((gaplen (fx- rest point))
                 (i+gap (fx+ i gaplen)))
            (if (> i+gap (vector-length buffer))
                (error "out of range" ggb-goto! i)
                (begin
                  (subvector-move! buffer rest i+gap buffer point)
                  (subvector-fill! buffer i i+gap #f)
                  (macro-ggb-point-set! ggb i)
                  (macro-ggb-rest-set! ggb i+gap)))))))))

(define (ggb-for-each ggb proc #!optional (start 0) (end (ggb-length ggb)))
  (let* ((buffer (macro-ggb-buffer ggb))
         (total (vector-length buffer))
         (point (macro-ggb-point ggb))
         (rest (macro-ggb-rest ggb))
         (gap (fx- rest point)))
    (if (and (macro-ggb? ggb) (procedure? proc)
             (integer? start) (exact? start) (fx>= start 0)
             (integer? end) (exact? end) (fx>= end start)
             (fx<= end (fx- total gap)))
        (let* ((limit (fx- end start))
               (limit1 (if (fx> limit point) point limit)))
          (do ((i 0 (fx+ i 1))
               (j start (fx+ j 1)))
              ((fx>= j limit1)
               (do ((i i (fx+ i 1))
                    (j rest (fx+ j 1)))
                   ((fx>= i limit))
                 (proc i (vector-ref buffer j))))
            (proc i (vector-ref buffer j))))
        (error "invalid arguments" ggb-for-each ggb proc))))

(define (ggb-for-each-rtl ggb proc #!optional (start 0) (end (ggb-length ggb)))
  (let* ((buffer (macro-ggb-buffer ggb))
         (ggblen (ggb-length ggb))
         (total (vector-length buffer))
         (point (macro-ggb-point ggb))
         (rest (macro-ggb-rest ggb))
         (gap (fx- rest point)))
    (if (and (macro-ggb? ggb) (procedure? proc)
             (integer? start) (exact? start) (fx>= start 0)
             (integer? end) (exact? end) (fx>= end start)
             (fx<= end (fx- total gap)))
        (let* ((limit (fx- end start))
               (limit1 (if (fx> limit (fx- total rest)) rest start)))
          (do ((i limit (fx- i 1))
               (j (fx- total 1) (fx- j 1)))
              ((fx< j limit1)
               (do ((i (fx- point 1) (fx- i 1))
                    (j (fx- point 1) (fx- j 1)))
                   ((fx< j 0))
                 (proc i (vector-ref buffer j))))
            (proc i (vector-ref buffer j))))
        (error "invalid arguments" ggb-for-each ggb proc))))

(define (ggb->vector ggb #!optional (start 0) (end (ggb-length ggb)))
  (let ((result (make-vector (fx- end start))))
    (ggb-for-each ggb (lambda (i v) (vector-set! result i v)) start end)
    result))

(define (ggb->string ggb #!optional (start 0) (end (ggb-length ggb)))
  (let ((result (make-string (fx- end start))))
    (ggb-for-each ggb (lambda (i v) (string-set! result i v)) start end)
    result))

(define (ggb-goto-left! ggb #!optional (n 1))
  (ggb-goto! ggb (max 0 (fx- (macro-ggb-point ggb) n))))

(define (ggb-goto-right! ggb #!optional (n 1))
  (ggb-goto! ggb (min (ggb-length ggb) (fx+ (macro-ggb-point ggb) n))))
