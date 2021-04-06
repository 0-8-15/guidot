;; (C) 2021 JFW

;; experimental

;; ggb2d ggb of ggb's (lines)

(define-structure
  ggb2d
  opaque: macros:
  prefix: macro-
  lines)

(define (ggb2d? obj) (macro-ggb2d? obj))

(define (make-ggb2d #!key (size 0) (ggb #f))
  (let ((lines
         (if (ggb? obj)
             (begin
               ;; Let's hope all members are ggb's!
               (MATURITY -1 "using GGB as if we knew it's actually ggb2d"
                         loc: make-ggb2d)
               obj)
             (make-ggb size: size))))
    (macro-make-ggb2d lines)))

(define (debug#ggb2d-lines obj) (macro-ggb2d-lines obj))

(define (ggb2d-lines obj #!optional location)
  (MATURITY -1 "use of ggb2d-lines is deprecated, will be removed" loc: location)
  (macro-ggb2d-lines obj))

(define (ggb2d-current-row ggb2d)
  (let ((point (ggb-point (macro-ggb2d-lines ggb2d))))
    (and (> point 0) (- point 1))))

(define (ggb2d-length ggb2d)
  (ggb-length (macro-ggb2d-lines ggb2d)))

(define (ggb2d-total-length ggb2d)
  (let ((len 0))
    (ggb-for-each
     (macro-ggb2d-lines ggb2d)
     (lambda (i v) (set! len (+ len (ggb-length v)))))
    len))

(define (ggb2d-for-each proc ggb2d)
  (ggb-for-each
   (cond ;; MATURITY-2: transparent replace
    ((macro-ggb2d? ggb2d) (macro-ggb2d-lines ggb2d))
    ((ggb? ggb2d) (make-ggb2d ggb: ggb2d))
    (else (error "invalid ggb2d" ggb2d-for-each ggb2d)))
   (lambda (row-number row) row
     (ggb-for-each
      row
      (lambda (column-number value)
        (proc value))))))

(define (ggb2d-copy ggb2d #!optional (start 0) (end #f) #!key (reserve 0))
  (let* ((source (let ((lines (macro-ggb2d-lines ggb2d)))
                   (unless end (set! end (ggb-length lines)))
                   lines))
         (point (ggb-point source))
         (end
          (case end
            ((#t point) point)
            ((#f) (ggb-length source))
            (else
             (cond
              ((and (number? end) (> end start)) end)
              (else (error "invalid arguments" ggb2d-copy start end))))))
         (result (make-ggb size: (+ (- end start) reserve))))
    (ggb-for-each
     source
     (lambda (i v) (ggb-insert! result (ggb-copy v)))
     start end)
    (ggb-goto! result (min end point))
    (macro-make-ggb2d result)))

(define (ggb2d->vector ggb2d)
  (let ((result (make-vector (ggb2d-total-length ggb2d) #f))
        (index 0))
    (ggb2d-for-each
     (lambda (v)
       (vector-set! result index v)
       (set! index (+ index 1)))
     ggb2d)
    result))

(define (ggb2d-display-value-on-port obj port #!key (display display))
  ;; TBD: check parameters
  (ggb2d-for-each (lambda (obj) (display obj port)) obj))

(define (ggb2d-insert-row! ggb2d #!optional (line (make-ggb)))
  (unless (ggb? line) (error "invalid argument" ggb2d-insert-row! line))
  (ggb-insert! (macro-ggb2d-lines ggb2d) line))

(define (ggb2d-delete-row! ggb2d #!optional (n 1))
  (ggb-delete! (macro-ggb2d-lines ggb2d) n))

(define (ggb2d-goto!
         ggb2d
         #!key
         (position #f)
         (row #f)
         (col #f)
         )
  (let ;; parse args
      ((position
        (case position
          ((relative #f relative:) position)
          ((absolute #t absolute:) position)
          (else (error "illegal argument" ggb2d-goto! position: position))))
       (row row) ;; TBD: error checks!!
       (col col)
       (lines (macro-ggb2d-lines ggb2d)))
    (let ;; current state
        ((point-d1 (ggb-point lines)))
      (case point-d1
        ((0) ;; return #t if valid
         (or (not row) (eqv? row 0)
             (ggb-goto! lines row))))
      (let ((index-d1 (ggb2d-current-row ggb2d)))
        (cond
         ((not (or row col)) index-d1)
         (index-d1
          (let* ((current-line (ggb-ref lines index-d1))
                 (point-d2 (ggb-point current-line))
                 (index-d2 (max 0 (- point-d2 1)))
                 ;; calculate
                 (move!
                  (case position
                    ((absolute) ggb-goto!)
                    ((relative) ggb-goto-right!))))
            (cond
             ((not row) (move! current-line col))
             (else
              (move! lines row)
              (let ((row (ggb2d-current-row ggb2d)))
                (when row
                  (set! current-line (ggb-ref lines row))
                  (if col
                      (move! current-line col)
                      (let ((col (min point-d2 (ggb-length current-line))))
                        (ggb-goto! current-line col)))))))))
         (else '|0x0|))))))

(define (%%ggb2d-row-ref ggb2d #!optional (row #f))
  (MATURITY -1 "whole row reference -> prevents many optimizations" loc: %%ggb2d-row-ref)
  (let ((rows (macro-ggb2d-lines ggb2d)))
    (cond
     ((eq? row #t)
      (let ((point (ggb-point rows)))
        (if (eqv? point 0) (error "point at zero") (ggb-ref rows (- point 1)))))
     (else (ggb-ref rows row)))))

(define-macro (macro-ggb2d-row-ref ggb2d row fail)
  ;; beware: internal use only - MAY re-evaluate macro arguments
  (let ((rows (gensym 'rows))
        (point (gensym 'point)))
    `(let ((,rows (macro-ggb2d-lines ,ggb2d)))
       (cond
        ((eq? ,row #t)
         (let ((,point (ggb-point ,rows)))
           (if (eqv? ,point 0) (,fail "point at zero") (ggb-ref ,rows (- ,point 1)))))
        (else (ggb-ref ,rows ,row))))))

(define (ggb2d-ref ggb2d #!optional (row #f) (col #f) #!key (fail error))
  (cond
   ((not (or col row))
    (let* ((rows (macro-ggb2d-lines ggb2d))
           (rowpoint (ggb-point rows)))
      (if (eqv? rowpoint 0) (fail "out of range")
          (let* ((cl (ggb-ref rows (- rowpoint 1)))
                 (lp (ggb-point cl)))
            (if (eqv? lp 0)
                (fail "point at begin of line")
                (ggb-ref cl (- lp 1)))))))
   (else
    (let ((rows (macro-ggb2d-lines ggb2d)))
      (if (>= row (ggb-length rows)) (fail "row out of range" ggb2d-ref row)
          (let ((cl (ggb-ref rows row)))
            (if (>= col (ggb-length cl)) (fail "column out of range" ggb2d-ref row col)
                (ggb-ref cl col))))))))

(define (ggb2d-reader/immutable data values)
  ;; assumes immutable data
  (let* ((lines (ggb2d-lines data))
         (len 0))
    (let ((i0 (ggb2d-current-row data)))
      (when (and i0 (< i0 (ggb-length lines)))
        (ggb-for-each
         lines
         (lambda (i line)
           (set! len (+ len (ggb-length line))))
         i0)))
    (let* ((lidx (##fxmax 0 (- (ggb-point lines) 1)))
           (curlin (ggb-ref lines lidx))
           (cidx 0)
           (curlinlen (ggb-length curlin)))
      (values
       (lambda ()
         (if (< len 0) (eof-object)
             (begin
               (set! len (- len 1))
               (when (>= cidx curlinlen)
                 (set! lidx (+ 1 lidx))
                 (set! curlin (ggb-ref lines lidx))
                 (set! curlinlen (ggb-length curlin))
                 (set! cidx 0))
               (let ((c (ggb-ref curlin cidx)))
                 (set! cidx (+ cidx 1))
                 c))))
       len))))

(define (ggb2d-reader data #!optional (values values))
  ;; get the total length and thunk reading values
  (ggb2d-reader/immutable
   ;; ensure copy-on-write mode on data
   (let ((lines (ggb2d-lines data)))
     (if (macro-ggb-cow lines) data (ggb2d-copy data)))
   values))

(define (ggb2d-insert-port! result port)
  (let* ((initial-line
          (and (ggb2d-current-row result)
               (macro-ggb2d-row-ref result #t NYIE)))
         (first-line initial-line)
         (line (or first-line (make-ggb))))
    (do ((c (read-char port) (read-char port)))
        ((eof-object? c)
         (unless (or (eqv? (ggb-point line) 0) (eqv? line initial-line))
           (ggb2d-insert-row! result line))
         result)
      (unless (eqv? c #\return)
        (ggb-insert! line (char->integer c)))
      (when (eqv? c #\newline)
        (unless (eqv? line initial-line)
          (ggb2d-insert-row! result line))
        (set! line (make-ggb))))))
