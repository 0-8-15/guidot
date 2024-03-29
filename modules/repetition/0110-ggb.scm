;; (C) 2021 JFW - GGB Generic Gap Buffer

;;* API

;; (ggb? OBJ) --> boolean

;; (make-ggb #!key (size 30)) --> GGB

;; (ggb-copy GGB) -> new GGB and put original in copy-on-write mode

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

;; (ggb-goto-left! GGB #!optional n)
;; (ggb-goto-right! GGB #!optional n)

;; (ggb-delete-matching! GGB PRED) -- delete all elements matching
;; `pred` - leaving point after the last deletion
;;
;; (ggb-delete-first-match! GGB PRED) -- delete first element matching
;; `pred` and keep point

;;* Implementation

;;#|
;;; intented version
(define-structure ggb ;; generic gap buffer
  opaque: macros:
  prefix: macro-
  cow                  ;; copy on write
  (point unprintable:) ;; point
  (rest unprintable:)  ;; zero in rest
  (buffer unprintable:))
;;|#
#|
;;; debug version
(define-structure ggb ;; generic gap buffer
;;;  opaque: macros:
  prefix: macro-
  cow   ;; copy on write
  point ;; point
  rest  ;; zero in rest
  buffer
  )
;;|#

(define (ggb? x) (macro-ggb? x))

(define (make-ggb #!key (size 0))
  (unless (and (integer? size) (exact? size) (fx>= size 0))
    (error "invalid size" make-ggb size))
  (macro-make-ggb #f 0 size (if (eqv? size 0) '#() (make-vector size #f))))

(define-macro (macro-ggb-mutable-buffer ggb)
  (let ((buffer (gensym 'buffer)))
    `(if (macro-ggb-cow ,ggb)
         (let ((,buffer (##vector-copy (macro-ggb-buffer ,ggb))))
           (macro-ggb-buffer-set! ,ggb ,buffer)
           (macro-ggb-cow-set! ,ggb #f)
           ,buffer)
         (macro-ggb-buffer ,ggb))))

(define-macro (macro-ggb-mutable-buffer/reduce ggb)
  (let ((buffer (gensym 'buffer))
        (point (gensym 'point))
        (rest (gensym 'rest))
        (length (gensym 'length)))
    `(if (macro-ggb-cow ,ggb)
         (let* ((,buffer (macro-ggb-buffer ,ggb))
                (,point (macro-ggb-point ,ggb))
                (,rest (macro-ggb-rest ,ggb))
                (,length (##vector-length ,buffer)))
           (cond
            ((and (eqv? ,rest ,length) ;; gap at end
                  (< ,point (- ,rest 500)))
             (let ((,buffer (##subvector ,buffer 0 ,point)))
               (macro-ggb-buffer-set! ,ggb ,buffer)
               (macro-ggb-cow-set! ,ggb #f)
               ,buffer))
            (else
             (let ((,buffer (##vector-copy ,buffer)))
               (macro-ggb-buffer-set! ,ggb ,buffer)
               (macro-ggb-cow-set! ,ggb #f)
               ,buffer))))
         (macro-ggb-buffer ,ggb))))

(define (ggb-copy ggb)
  (unless (macro-ggb? ggb) (error "invalid gap buffer" ggb-copy ggb))
  (macro-ggb-cow-set! ggb #t)
  (macro-make-ggb
   #t
   (macro-ggb-point ggb)
   (macro-ggb-rest ggb)
   (macro-ggb-buffer ggb)))

(define (ggb-clear! ggb)
  (let ((point (macro-ggb-point ggb))
        (rest (macro-ggb-rest ggb))
        (buffer (macro-ggb-mutable-buffer ggb)))
    (when (fx> point 0) (subvector-fill! buffer 0 point #f))
    (let ((limit (vector-length buffer)))
      (when (fx< rest limit) (subvector-fill! buffer rest limit #f))
      (macro-ggb-point-set! ggb 0)
      (macro-ggb-rest-set! ggb limit))))

(define (ggb-point ggb) (macro-ggb-point ggb))

(define (ggb-length ggb)
  (unless (macro-ggb? ggb) (error "invalid gap buffer" ggb-length ggb))
  (fx+ (macro-ggb-point ggb) (fx- (##vector-length (macro-ggb-buffer ggb)) (macro-ggb-rest ggb))))

(define (ggb-ref ggb i)
  (unless (and (macro-ggb? ggb) (integer? i) (exact? i) (fx>= i 0))
    (error "invalid arguments" ggb-ref ggb i))
  (let ((point (ggb-point ggb))
        (buffer (macro-ggb-buffer ggb)))
    (if (fx< i point) (##vector-ref buffer i)
        (let ((i (fx+ (macro-ggb-rest ggb) (fx- i point))))
          (if (fx< i (##vector-length buffer))
              (##vector-ref buffer i)
              (error "out of range" ggb-ref i))))))

(define (ggb-set! ggb i v)
  (unless (and (macro-ggb? ggb) (integer? i) (exact? i) (fx>= i 0))
    (error "invalid arguments" ggb-ref ggb i))
  (let ((point (ggb-point ggb))
        (buffer (macro-ggb-mutable-buffer ggb)))
    (if (fx< i point) (##vector-set! buffer i v)
        (let ((i (fx+ (macro-ggb-rest ggb) (fx- i point))))
          (if (fx< i (##vector-length buffer))
              (##vector-set! buffer i v)
              (error "out of range" ggb-set! i))))))

(define (%%ggb-grow-gap! ggb size)
  (let ((point (+ (macro-ggb-point ggb) 1))
        (rest (macro-ggb-rest ggb))
        (buffer (macro-ggb-buffer ggb))
        (max-grow-length
         ;; #x40000000 ;; was 4 GB - which is too large 32bit
         #x10000000))
    (cond
     ((fx>= (fx+ point size) rest)
      (cond
       ((eqv? rest (##vector-length buffer))
        (let ((insert (min
                       (max size (##vector-length buffer))
                       max-grow-length)))
          (macro-ggb-buffer-set! ggb (vector-append buffer (make-vector insert #f)))
          (if (macro-ggb-cow ggb) (macro-ggb-cow-set! ggb #f))
          (macro-ggb-rest-set! ggb (fx+ rest insert))))
       (else
        (let* ((len (vector-length buffer))
               (insert (min max-grow-length (max size len)))
               (new-rest (fx+ rest insert))
               (new (make-vector (fx+ len insert) #f)))
          (subvector-move! buffer 0 point new 0)
          (subvector-move! buffer rest len new new-rest)
          (macro-ggb-buffer-set! ggb new)
          (if (macro-ggb-cow ggb) (macro-ggb-cow-set! ggb #f))
          (macro-ggb-rest-set! ggb new-rest)))))
     (else (macro-ggb-mutable-buffer ggb)))))

(define (ggb-insert! ggb val)
  (if (not (macro-ggb? ggb))
      (error "invalid gap buffer" ggb-insert! ggb)
      (begin
        (%%ggb-grow-gap! ggb 1)
        (let ((point (macro-ggb-point ggb))
              (rest (macro-ggb-rest ggb))
              (buffer (macro-ggb-buffer ggb)))
          (##vector-set! buffer point val)
          (macro-ggb-point-set! ggb (+ (macro-ggb-point ggb) 1))))))

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
        (##vector-set! buffer point (##vector-ref val i))))))

(define (ggb-insert-ggb! dst src #!optional (start 0) (end #f))
  (let* ((src-length (let ((x (ggb-length src)))
                       (or end (set! end x))
                       (ggb-length src)))
         (length (- end start)))
    (unless (and (macro-ggb? dst) (macro-ggb? src)
                 (>= start 0) (>= length 0)
                 (<= end (ggb-length src)))
      (error "invalid gap buffer" ggb-insert-ggb! dst src start end))
    (%%ggb-grow-gap! dst length)
    (let ((dst-point (macro-ggb-point dst))
          (dst-buffer (macro-ggb-buffer dst))
          (src-point (macro-ggb-point src))
          (src-rest (macro-ggb-rest src))
          (src-buffer (macro-ggb-buffer src)))
      (let* ((src-before-point (- src-point start))
             (src-in-rest (- end src-before-point)))
        (when (> src-before-point 0)
          (##subvector-move! src-buffer start src-point dst-buffer dst-point))
        (when (> src-in-rest 0)
          (##subvector-move!
           src-buffer src-rest (+ src-rest src-in-rest)
           dst-buffer (+ dst-point src-before-point)))
        (macro-ggb-point-set! dst (+ dst-point length))))))

(define (ggb-delete! ggb n)
  (unless (macro-ggb? ggb) (error "invalid gap buffer" ggb-delete! ggb))
  (unless (integer? n) (exact? n) (error "invalid count" ggb-delete! n))
  (cond
   ((fx> n 0)
    (let* ((rest (macro-ggb-rest ggb))
           (buffer (macro-ggb-mutable-buffer ggb))
           (limit (vector-length buffer))
           (to (fx+ rest n))
           (new-rest (if (fx> to limit) limit to)))
      (when (fx< rest new-rest) (subvector-fill! buffer rest new-rest #f))
      (macro-ggb-rest-set! ggb new-rest)
      (eqv? to new-rest)))
   ((fx< n 0)
    (let* ((point (macro-ggb-point ggb))
           (to (fx+ point n))
           (new-point (if (fx>= to 0) to 0))
           (buffer (macro-ggb-mutable-buffer ggb)))
      (when (fx< new-point point) (subvector-fill! buffer new-point point #f))
      (macro-ggb-point-set! ggb new-point)
      (eqv? to new-point)))
   (else #t)))

(define (ggb-goto! ggb i)
  (if (not (and (macro-ggb? ggb) (integer? i) (exact? i) (fx>= i 0)))
      (error "invalid argument" ggb-goto! ggb i)
      (let ((point (macro-ggb-point ggb))
            (rest (macro-ggb-rest ggb)))
        (cond
         ((eqv? i (macro-ggb-point ggb)))  ;; nothing to be done
         ((< i point)
          (let ((new-rest (fx- rest (fx- point i)))
                (buffer (macro-ggb-mutable-buffer/reduce ggb)))
            (subvector-move! buffer i point buffer new-rest)
            (subvector-fill! buffer i new-rest #f)
            (macro-ggb-rest-set! ggb new-rest)
            (macro-ggb-point-set! ggb i)))
         (else
          (let* ((gaplen (fx- rest point))
                 (i+gap (fx+ i gaplen))
                 (buffer (macro-ggb-mutable-buffer ggb)))
            (if (> i+gap (##vector-length buffer))
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
             (integer? start) (exact? start) (>= start 0)
             (integer? end) (exact? end) (>= end start)
             (<= end (- total gap)))
        (let ((limit (- end start)))
          (do ((i 0 (+ i 1))
               (j start (+ j 1)))
              ((or (eqv? j point) (eqv? i limit))
               (do ((i i (+ i 1))
                    (j rest (+ j 1)))
                   ((eqv? i limit))
                 (proc i (vector-ref buffer j))))
            (proc i (vector-ref buffer j))))
        (error "invalid arguments" ggb-for-each ggb proc start end))))

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

(define (ggb-goto-left! ggb #!optional (n 1))
  (ggb-goto! ggb (max 0 (min (ggb-length ggb) (fx- (macro-ggb-point ggb) n)))))

(define (ggb-goto-right! ggb #!optional (n 1))
  (ggb-goto! ggb (max 0 (min (ggb-length ggb) (fx+ (macro-ggb-point ggb) n)))))

;;** Utilities

(define (ggb-delete-matching! ggb pred)
  ;; delete all elements matching `pred` - leaving point after the last deletion
  (do ((i 0 (+ i 1)))
      ((>= i (ggb-length ggb)))
    (let ((x (ggb-ref ggb i)))
      (when (pred x)
        (ggb-goto! ggb i)
        (ggb-delete! ggb 1)))))

(define (ggb-delete-first-match! ggb pred)
  ;; delete first element matching `pred` and keep point
  (let ((point (ggb-point ggb)))
    (do ((i 0 (+ i 1))
         (done #f))
        ((or done
             (>= i (ggb-length ggb)))
         (cond
          ((not done))
          ((> done point) (ggb-goto! ggb point))
          ((eqv? done 0))
          (else (ggb-goto! ggb (- point 1)))))
      (let ((x (ggb-ref ggb i)))
        (when (pred x)
          (set! done i)
          (ggb-goto! ggb i)
          (ggb-delete! ggb 1))))))

(define (ggb#ggb-buffer ggb) ;; internal
  ;;
  ;; may be used to avoid `ggb->vector` when it the value may be
  ;; safely shared.
  (macro-ggb-buffer ggb))
