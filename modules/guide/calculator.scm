;;** Global Pins

;;; These are exported as input/output:

(define-pin calculator-input
  initial: #f
  name: "Calculator input value")

(define-pin calculator-result
  initial: #f
  name: "Calculator result value")

(define-pin calculator-mem1
  initial: 0
  name: "Calculator \"Mem\" value")

(define-pin calculator-operations
  initial: '()
  name: "Calculator stack")

(define-pin calculator-main-display
  initial: #f
  name: "Calculator value in display")

(define (make-calculator-payload interval)

  (define calculator-color-bgn (lambda _ Gray) #;guide-select-color-1)
  (define calculator-color-fgn guide-select-color-2)
  (define calculator-color-bge (lambda _ DarkOrange) #;guide-select-color-3)
  (define calculator-color-fge guide-select-color-4)

  (define (number->neatstring n)
    ;; TBD: suppress floating point inaccuracies
    (number->string n))

  (define (value->neatstring n)
    (cond
     ((boolean? n) "")
     ((number? n) (number->neatstring n))
     (else (object->string n))))

  (define (calculator-evaluate)
    (let ((input (calculator-main-display)))
      (calculator-result
       (let ((input (or input (and (number? (calculator-input)) (calculator-input)))))
         (and input
              (if (pair? (calculator-operations))
                  (let ((top (car (calculator-operations))))
                    (calculator-operations (cdr (calculator-operations)))
                    (with-exception-catcher
                     (lambda (exn) exn)
                     (lambda () (top input))))
                  input))))
      (calculator-input #f)
      (calculator-main-display (calculator-result))))

  (define (calculator-C)  (begin #;kick (calculator-input #t) (calculator-main-display (calculator-input))))
  ;;(define (calculator-C) (glgui-widget-set! gui calculator-display 'label ""))
  (define (calculator-MC) (begin #;kick (calculator-mem1 0)))
  (define (calculator-AC) (begin #;kick (calculator-result #t) (calculator-C) (calculator-MC) (calculator-operations '())))

  (define (calculator-MR) (begin #;kick (calculator-input (calculator-mem1))))

  (define (calculator-M+)
    (let ((input (calculator-main-display)))
      (calculator-input #f)
      (if (number? input) (begin #;kick (calculator-mem1 (+ (calculator-mem1) input))))))

  (define (take-display-value)
    (let ((input (calculator-main-display)))
      (and input
           (begin #;kick
             (calculator-main-display #f)
             input))))

  (define (calculator-M-)
    (let ((input (take-display-value)))
      (begin #;kick
        (calculator-input #f)
        (if (number? input) (begin #;kick (calculator-mem1 (- (calculator-mem1) input)))))))

  (define (calculator-push-label-to-pin! pin)
    (let ((input (take-display-value)))
      (and input (begin #;kick (pin input)))))

  (define (calculator-sqrt)
    (let ((input (take-display-value)))
      (begin #;kick
        (calculator-input input)
        (calculator-result
         (cond
          ((number? (calculator-input)) (sqrt (calculator-input)))
          ((number? (calculator-result)) (sqrt (calculator-result)))
          (else #f)))
        (calculator-main-display (calculator-result)))))

  (define (calculator-push-operation op)
    (let ((input (take-display-value)))
      (if input
          (begin
            (calculator-result input)
            (calculator-operations (cons (lambda (x) (op (calculator-result) x)) (calculator-operations)))))))

  (let* ((xsw (mdvector-interval-lower-bound interval 0))
         (ysw (mdvector-interval-lower-bound interval 1))
         (xno (mdvector-interval-upper-bound interval 0))
         (yno (mdvector-interval-upper-bound interval 1))
         (w (- xno xsw))
         (h (- yno ysw)))
    (define calculator-subdisplay
      (let ((label! (make-guide-label-view))
            (check!
             (let ((cached
                    (memoize-last
                     (lambda (update! input Ans Mem)
                       (update!
                        text:
                        (string-append
                         "In=" (value->neatstring input)
                         "Mem=" (number->neatstring Mem) " Ans=" (value->neatstring Ans)  " "))
                       (update!))
                     (lambda (a b) #t) equal? equal? equal?)))
               (lambda (update!) (cached update! (calculator-input) (calculator-result) (calculator-mem1))))))
        (label! color: (calculator-color-bgn))
        (label! size: w 20)
        (label! vertical-align: 'bottom)
        (label! horizontal-align: 'right)
        (label! position: 0 (- h 20))
        (label! font: (guide-select-font size: 'small))
        (label! check! "calculator subdisplay")))
    (define calculator-display
      (let ((label! (make-guide-label-view))
            (check!
             (let ((cached
                    (memoize-last
                     (lambda (update! Ans)
                       (update!
                        text:
                        (if (number? Ans) (number->neatstring Ans) (if (boolean? Ans) "" (object->string Ans))))
                       (update!))
                     (lambda (a b) #t) equal?)))
               (lambda (update!) (cached update! (calculator-main-display))))))
        (label! color: (calculator-color-bgn))
        (label! size: (- w 10) 60)
        (label! vertical-align: 'bottom)
        (label! horizontal-align: 'right)
        (label! position: 5 (- h 80))
        (label! font: (guide-select-font size: 'large))
        (label! check! "calculator main display")))
    (define delkey-img (apply make-glC:image glgui_keypad_delete.img))
    (define kpd
      (let* ((key-columns 4)
             (key-rows 6)
             (rng (range (vector key-columns key-rows)))
             (bgimg ;; TBD: guide-default-background ?
              (make-glC:image 4 4 (glCoreTextureCreate 4 4 (make-u8vector 16 #xff)) 0.1 0.1 .9 .9))
             (keys (make-vector (range-volume rng) #f))
             (desc
              `#(
                 (#\A "AC") (#\M "MC") #\C (,delchar ,delkey-img)
                 (#\m "MR")  (#\p "M+") (#\q "M-") #\/
                 #\7 #\8 #\9 #\*
                 #\4 #\5 #\6 #\-
                 #\1 #\2 #\3 #\+
                 (#\0 "0" 2) #f  #\. (#\= "=" 1 ,(calculator-color-bge))
                 ))
             (top (- h 100))
             (total-height top)
             (colw (floor (/ w key-columns)))
             (border-ratio 1/20)
             (min-gap (ceiling (* border-ratio colw)))
             (button-width (- colw (* 2 min-gap)))
             (used-width (* key-columns (+ button-width (* 2 min-gap))))
             (gap (* (/ w used-width) min-gap))
             (left-offset (+ (* 2 gap) (/ (- w used-width) 2)))
             ;;
             (rowh (floor (/ total-height key-rows)))
             (min-vgap (ceiling (* border-ratio rowh)))
             (button-height (- rowh (* 2 min-vgap)))
             (used-height (* key-rows (+ button-height (* 2 min-vgap))))
             (vgap (* (/ total-height used-height) min-vgap))
             (button-area (make-mdv-rect-interval 0 0 button-width button-height))
             (font (guide-select-font size: 'large)))
        (for-range2
         rng
         (lambda (row col)
           (let* ((idx (mdv-idx rng row col))
                  (pattern (vector-ref desc idx))
                  (key
                   (and
                    pattern
                    (let ((colspan
                           (and (pair? pattern)
                                (> (length pattern) 2)
                                (list-ref pattern 2)))
                          (background-color
                           (or (and (pair? pattern)
                                    (> (length pattern) 3)
                                    (list-ref pattern 3))
                               (calculator-color-bgn))))
                      (guide-button
                       in: (if colspan
                               (make-mdv-rect-interval 0 0 (- (* colspan (+ gap button-width)) gap) button-height)
                               button-area)
                       font: font
                       label: (if (pair? pattern) (cadr pattern) (string pattern))
                       ;; padding: '#(1 1 1 1)
                       background: (glC:image-t bgimg)
                       background-color: background-color
                       position:
                       (vector
                        (+ left-offset (* col (+ button-width gap)))
                        (- top vgap (* (+ 1 row) (+ button-height vgap))))
                       horizontal-align: 'center
                       vertical-align: 'center
                       guide-callback:
                       (lambda (rect payload event x y)
                         (calculator-on-event
                          rect payload EVENT_KEYRELEASE
                          (char->integer (if (pair? pattern) (car pattern) pattern)) 0)
                         #t))))))
             (vector-set! keys idx key))))
        (vector
         (lambda ()
           (calculator-subdisplay) (calculator-display)
           (do ((i (fx- (vector-length keys) 1) (fx- i 1)))
               ((eqv? i -1))
             (let ((payload (vector-ref keys i)))
               (and
                payload
                (let ((draw (guide-payload-on-redraw payload)))
                  (and draw (draw)))))))
         (lambda (rect payload event x y)
           (do ((i (fx- (vector-length keys) 1) (fx- i 1))
                (hit #f))
               ((or hit (eqv? i -1)) hit)
             (let ((payload (vector-ref keys i)))
               (and
                payload
                (cond
                 ((eqv? event EVENT_REDRAW) ;; paint all
                  (guide-event-dispatch-to-payload rect payload event x y))
                 (else (set! hit (guide-event-dispatch-to-payload rect payload event x y)))))))))))
    (define (calculator-on-event rect payload event x y)
      (cond
       ((eqv? event EVENT_KEYRELEASE)
        (cond
         ((eqv? x EVENT_KEYESCAPE)     (terminate))
         ((eqv? x (char->integer #\=)) (calculator-evaluate))
         ((eqv? x (char->integer #\+)) (calculator-push-operation +))
         ((eqv? x (char->integer #\-)) (calculator-push-operation -))
         ((eqv? x (char->integer #\*)) (calculator-push-operation *))
         ((eqv? x (char->integer #\/)) (calculator-push-operation /))
         ((eqv? x (char->integer #\A)) (calculator-AC))
         ((eqv? x (char->integer #\C)) (calculator-C))
         ((eqv? x (char->integer #\M)) (calculator-MC))
         ((eqv? x (char->integer #\p)) (calculator-M+))
         ((eqv? x (char->integer #\q)) (calculator-M-))
         ((eqv? x (char->integer #\m)) (calculator-MR))
         ((eqv? x (char->integer #\S)) (calculator-sqrt))
         ((eqv? x (char->integer delchar))
          (let ((x (calculator-main-display)))
            (when (number? x)
              (let ((str (number->string x)))
                (calculator-main-display
                 (and (> (string-length str) 1)
                      (string->number (substring str 0 (- (string-length str) 1))))))))
          #t)
         (else
          (and (memq (integer->char x) '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.))
               (let* ((d (calculator-main-display))
                      (n (or (and (number? d) (number->string d)) "")))
                 (calculator-main-display
                  (string->number (string-append n (string (integer->char x)))))
                 #t)))))
       (else ((vector-ref kpd 1) rect payload event x y))))

    (make-guide-payload in: interval widget: #f on-redraw: (vector-ref kpd 0) on-any-event: calculator-on-event)))

(define (guide-define-payload-calculator! name)
  ;; Wired to globals, MUST be instanciated once only.
  (guide-define-payload name 'once make-calculator-payload))

#| ;; test

(guide-define-payload-calculator! "calculator")

;;|#
