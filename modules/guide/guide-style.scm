;;** Guide Style

;;*** Definitions

(define-structure guide-style-keyword-definition
  ;; TBD: hide mutators
  keyword
  default
  predicate
  description
  index
  ) ;; guide-style-keyword-definition

(define-structure guide-style-environment
  ;; TBD: hide mutators
  by-keyword
  vector)

(define guide-style? guide-style-environment?)

(define (make-guide-style-definitions)
  (define (guide-style-definitions->style-environment vec)
    (let* ((len (vector-length vec))
           (kwtable (make-table size: len max-load: len test: eq?)))
      (do ((i (- len 1) (- i 1)))
          ((negative? i)
           (make-guide-style-environment kwtable vec))
        (let* ((definition (vector-ref vec i))
               (default (guide-style-keyword-definition-default definition)))
          (table-set! kwtable (guide-style-keyword-definition-keyword definition) definition)
          (vector-set!
           vec i
           (cond
            ((procedure? default) (default))
            (else default)))))))
  (let ((all (make-ggb)))
    (case-lambda
     (() (guide-style-definitions->style-environment (ggb->vector all)))
     ((keyword . more)
      (apply
       (lambda (#!key default (pred #f) (doc "n/a"))
         (let ((undefined
                (lambda (i v)
                  (when (eq? keyword (guide-style-keyword-definition-keyword v))
                    (error "style already defined" keyword)))))
           (ggb-for-each all undefined))
         (let* ((index (ggb-point all))
                (new (make-guide-style-keyword-definition
                      keyword default pred doc index)))
           (ggb-insert! all new)
           new))
       more)))))

(define (guide-style-extent envt kw val . more)
  (let* ((kwtable (guide-style-environment-by-keyword envt))
         (vec (vector-copy (guide-style-environment-vector envt)))
         (result (make-guide-style-environment kwtable vec)))
    (define (bind! name value)
      (let* ((definition (table-ref kwtable name))
             (pred (guide-style-keyword-definition-predicate definition))
             (index
              (cond
               ((not pred) (guide-style-keyword-definition-index definition))
               ((pred value) (guide-style-keyword-definition-index definition))
               (else (error "invalid value" guide-style-extent name value)))))
        (vector-set! vec index val)))
    (bind! kw val)
    (let loop ((more more))
      (cond
       ((null? more) result)
       ((and (pair? more) (pair? (cdr more)))
        (let* ((kw (car more))
               (rest (cdr more)))
          (unless (keyword? kw) (error "invalid style keyword" more))
          (bind! kw (car rest))
          (loop (cdr rest))))
       (else (error "invalid style definition" more))))
    result))

(define (guide-style-ref style key)
  (vector-ref
   (guide-style-environment-vector style)
   (cond
    ((fixnum? key) key)
    ((keyword? key)
     (let ((definition (table-ref (guide-style-environment-by-keyword style) key style)))
       (cond
        ((eq? definition style) (error "invalid style key" guide-style-ref key))
        (else (guide-style-keyword-definition-index definition)))))
    ((guide-style-keyword-definition? key) (guide-style-keyword-definition-index key))
    (else (error "invalid key" guide-style-ref key)))))

;;*** Style Keyword Definitions

(define guide-style-declare
  (let ((decl! (make-guide-style-definitions)))
    (case-lambda
     (()
      (set! guide-style-declare (decl!))
      guide-style-declare)
     ((arg1 . more)
      (cond
       ((guide-style? guide-style-declare) (error "already finalized" guide-style-declare))
       (else (apply decl! arg1 more)))))))

;; (guide-style-declare position: #f pred: ???)

(guide-style-declare background: default: #f pred: #f doc: "Background {image, payload, procedure}")
(guide-style-declare background-color: default: #f pred: #f doc: "Color for background")
(guide-style-declare color: default: #f pred: #f doc: "Color")
(guide-style-declare font: default: guide-select-font pred: ln-ttf:font? doc: "Font")
(guide-style-declare hightlight-color: default: #f pred: #f doc: "Color for highlights")
(guide-style-declare
 horizontal-align: default: 'center doc: "Alingment Horizontal"
 pred: (lambda (x) (case x ((left center right) #t) (else #f))))
(guide-style-declare
 vertical-align: default: 'center doc: "Alingment Vertical"
 pred: (lambda (x) (case x ((top center bottom) #t) (else #f))))
(guide-style-declare
 padding: default: '#(1 1 1 1) doc: "Padding"
 pred: #f)
(guide-style-declare locale: default: 'en pred: #f doc: "ISO locale symbol")
(guide-style-declare
 done: default: NYI doc: "GUI Continuation"
 pred: procedure?)

(define $current-guide-style (make-parameter #f))

(define guide-style-default
  ;; first use finalizes declaration
  (let ((guide-style-default (delay (guide-style-declare))))
    (case-lambda
     (() (force guide-style-default))
     ((key) (guide-style-ref (force guide-style-default) key)))))

(define guide-current-style
  (case-lambda
   (() (or ($current-guide-style) (force guide-style-default)))
   ((key) (guide-style-ref (or ($current-guide-style) (force guide-style-default)) key))))
