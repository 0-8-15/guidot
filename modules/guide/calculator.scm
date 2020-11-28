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

(define make-calculator-payload
  (let ()
    (define keypad
      `((
         ( (#\A "AC") (#\M "MC") #\C (,delchar ,glgui_keypad_delete.img) )
         ( (#\m "MR")  (#\p "M+") (#\q "M-") #\/)
         ( #\7 #\8 #\9 #\* )
         ( #\4 #\5 #\6 #\- )
         ( #\1 #\2 #\3 #\+ )
         ( (#\0 "0" 2.)  #\. (#\= "=" 1. ,DarkOrange))
         )))

    ;; suppress floating point inaccuracies
    (define (number->neatstring n)
      (if (not (##flonum? n)) (number->string n)
          (let* ((s (number->string (fl/ (flfloor (fl+ (fl* (flo n) 1.0e10) 0.5)) 1.0e10)))
                 (sl (string-length s))
                 (b (substring s 0 1))
                 (e (substring s (- sl 1) sl)))
            (string-append (if (string=? b ".") "0" "")
                           (if (string=? e ".") (substring s 0 (- sl 1)) s)))))

    (define (value->neatstring n)
      (cond
       ((boolean? n) "")
       ((number? n) (number->neatstring n))
       (else (object->string n))))

    (define (make-calculator-payload ingui interval)
      (define gui #f)
      (define calculator-subdisplay #f)
      (define calculator-display #f)

      (define (calculator-updatesub)
        (let ((Ans (calculator-result))
              (Mem (calculator-mem1)))
          (glgui-widget-set!
           gui calculator-subdisplay 'label
           (string-append "In=" (value->neatstring (calculator-input)) "Mem=" (number->neatstring Mem) " Ans=" (value->neatstring Ans)  " "))
          (glgui-wakeup!)))

      (define (calculator-update-main-display)
        (glgui-wakeup!)
        (let ((Ans (calculator-main-display)))
          (glgui-widget-set! gui calculator-display 'label (if (number? Ans) (number->neatstring Ans) (if (boolean? Ans) "" (object->string Ans))))
          (glgui-widget-set! gui calculator-display 'bgcolor (if Ans #f Red))))

      (define (calculator-evaluate)
        (let ((input (string->number (glgui-widget-get gui calculator-display 'label))))
          (kick
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
           (calculator-main-display (calculator-result)))))

      (define (calculator-C)  (kick (calculator-input #t) (calculator-main-display (calculator-input))))
      ;;(define (calculator-C) (glgui-widget-set! gui calculator-display 'label ""))
      (define (calculator-MC) (kick (calculator-mem1 0)))
      (define (calculator-AC) (kick (calculator-result #t) (calculator-C) (calculator-MC) (calculator-operations '())))

      (define (calculator-MR) (kick (calculator-input (calculator-mem1))))

      (define (calculator-M+)
        (let ((input (take-display-value)))
          (kick
           (calculator-input #f)
           (if (number? input) (kick (calculator-mem1 (+ (calculator-mem1) input)))))))
      (define (calculator-M-)
        (let ((input (take-display-value)))
          (kick
           (calculator-input #f)
           (if (number? input) (kick (calculator-mem1 (- (calculator-mem1) input)))))))

      (define (take-display-value)
        (let ((input (string->number (glgui-widget-get gui calculator-display 'label))))
          (and input
               (begin
                 (glgui-widget-set! gui calculator-display 'label "")
                 input))))

      (define (calculator-push-label-to-pin! pin)
        (let ((input (take-display-value)))
          (and input (kick (pin input)))))

      (define (calculator-sqrt)
        (let ((input (take-display-value)))
          (kick
           (calculator-input input)
           (calculator-result
            (cond
             ((number? (calculator-input)) (sqrt (calculator-input)))
             ((number? (calculator-result)) (sqrt (calculator-result)))
             (else #f)))
           (calculator-main-display (calculator-result)))))

      (define (calculator-push-operation op)
        (let ((input (string->number (glgui-widget-get gui calculator-display 'label))))
          (if input
              (begin
                (glgui-widget-set! gui calculator-display 'label "")
                (kick
                 (calculator-result input)
                 (calculator-operations (cons (lambda (x) (op (calculator-result) x)) (calculator-operations))))))))

      (define (calculator-on-event gui payload event x y)
        (let ((skipevent #t))
          (if (= event EVENT_KEYRELEASE)
              (cond
               ((= x EVENT_KEYESCAPE)     (terminate))
               ((= x (char->integer #\=)) (calculator-evaluate))
               ((= x (char->integer #\+)) (calculator-push-operation +))
               ((= x (char->integer #\-)) (calculator-push-operation -))
               ((= x (char->integer #\*)) (calculator-push-operation *))
               ((= x (char->integer #\/)) (calculator-push-operation /))
               ((= x (char->integer #\A)) (calculator-AC))
               ((= x (char->integer #\C)) (calculator-C))
               ((= x (char->integer #\M)) (calculator-MC))
               ((= x (char->integer #\p)) (calculator-M+))
               ((= x (char->integer #\q)) (calculator-M-))
               ((= x (char->integer #\m)) (calculator-MR))
               ((= x (char->integer #\S)) (calculator-sqrt))
               (else (set! skipevent #f)))
              (set! skipevent #f))
          (if (not skipevent) (guide-default-event-dispatch gui payload event x y))))

      (let* ((xsw (interval-lower-bound interval 0))
             (ysw (interval-lower-bound interval 1))
             (xno (interval-upper-bound interval 0))
             (yno (interval-upper-bound interval 1))
             (w (- xno xsw))
             (h (- yno ysw)))
        (set! gui (glgui-container (guide-rectangle-glgui ingui) xsw ysw w h))
        (set! calculator-subdisplay (glgui-label gui 0 (- h 20) w 20 "" (select-font size: 'small) White))
        (glgui-widget-set! gui calculator-subdisplay 'align GUI_ALIGNRIGHT)
        (set! calculator-display (glgui-label gui 5 (- h 80) (- w 10) 60 "" (select-font size: 'large) White))
        (glgui-widget-set! gui calculator-display 'align GUI_ALIGNRIGHT)
        (glgui-widget-set! gui calculator-display 'focus #t)
        (let ((wgt (glgui-keypad gui 5 5 (- w 10) (- h 80 5) (select-font size: 'medium) keypad)))
          (glgui-widget-set! gui wgt 'rounded #f)
          (glgui-widget-set! gui wgt 'floatinghighlight #f))
        (calculator-updatesub)

        (wire! calculator-mem1 post: calculator-updatesub)
        (wire! calculator-result post: calculator-updatesub)
        (wire! calculator-input post: calculator-updatesub)
        (wire! calculator-main-display post: calculator-update-main-display)
        ;; Wired to globals, MUST be instanciated once only.
        (make-guide-payload
         in: interval widget: gui lifespan: 'once
         on-any-event: calculator-on-event)))
    make-calculator-payload))

(define (guide-define-payload-calculator! name)
  ;; Wired to globals, MUST be instanciated once only.
  (guide-define-payload name 'once make-calculator-payload))

#| ;; test

(guide-define-payload-calculator! "calculator")

;;|#
