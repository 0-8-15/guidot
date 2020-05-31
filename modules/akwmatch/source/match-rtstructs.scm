(define match:set-runtime-structures
  (lambda (v) (set! match:runtime-structures v)))
(define match:primitive-vector? vector?)
(define-macro (defstruct . args)
  (let ((field? (lambda (x)
                  (if (symbol? x)
                      ((lambda () #t))
                      (if (and (pair? x)
                               (symbol? (car x))
                               (pair? (cdr x))
                               (symbol? (cadr x))
                               (null? (cddr x)))
                          ((lambda () #t))
                          ((lambda () #f))))))
        (selector-name (lambda (x)
                         (if (symbol? x)
                             ((lambda () x))
                             (if (and (pair? x)
                                      (symbol? (car x))
                                      (pair? (cdr x))
                                      (null? (cddr x)))
                                 ((lambda (s) s) (car x))
                                 (match:error x)))))
        (mutator-name (lambda (x)
                        (if (symbol? x)
                            ((lambda () #f))
                            (if (and (pair? x)
                                     (pair? (cdr x))
                                     (symbol? (cadr x))
                                     (null? (cddr x)))
                                ((lambda (s) s) (cadr x))
                                (match:error x)))))
        (filter-map-with-index (lambda (f l)
                                 (letrec ((mapi (lambda (l i)
                                                  (cond
                                                   ((null? l) '())
                                                   ((f (car l) i) =>
                                                    (lambda (x)
                                                      (cons x
                                                            (mapi (cdr l)
                                                                  (+ 1
                                                                     i)))))
                                                   (else (mapi (cdr l)
                                                               (+ 1 i)))))))
                                   (mapi l 1)))))
    (let ((g227 (lambda ()
                  (match:syntax-err `(defstruct ,@args) "syntax error in"))))
      (if (and (pair? args)
               (symbol? (car args))
               (pair? (cdr args))
               (symbol? (cadr args))
               (pair? (cddr args))
               (symbol? (caddr args))
               (list? (cdddr args)))
          (let g229 ((g230 (cdddr args)) (g228 '()))
            (if (null? g230)
                ((lambda (name constructor predicate fields)
                   (let* ((selectors (map selector-name fields))
                          (mutators (map mutator-name fields))
                          (tag (if match:runtime-structures
                                   (gentemp)
                                   `',(match:make-structure-tag name)))
                          (vectorP (cond
                                    ((eq? match:structure-control
                                          'disjoint) 'match:primitive-vector?)
                                    ((eq? match:structure-control 'vector) 'vector?))))
                     (cond
                      ((eq? match:structure-control 'disjoint) (if (eq? vector?
                                                                        match:primitive-vector?)
                                                                   (set! vector?
                                                                         (lambda (v)
                                                                           (and (match:primitive-vector?
                                                                                 v)
                                                                                (or (zero?
                                                                                     (vector-length
                                                                                      v))
                                                                                    (not (symbol?
                                                                                          (vector-ref
                                                                                           v
                                                                                           0)))
                                                                                    (not (match:structure?
                                                                                          (vector-ref
                                                                                           v
                                                                                           0))))))))
                       (if (not (memq predicate
                                      match:disjoint-predicates))
                           (set! match:disjoint-predicates
                                 (cons predicate match:disjoint-predicates))))
                      ((eq? match:structure-control 'vector) (if (not (memq predicate
                                                                            match:vector-structures))
                                                                 (set! match:vector-structures
                                                                       (cons predicate
                                                                             match:vector-structures))))
                      (else (match:syntax-err
                             '(vector disjoint)
                             "invalid value for match:structure-control, legal values are")))
                     `(begin ,@(if match:runtime-structures
                                   `((define ,tag
                                       (match:make-structure-tag ',name)))
                                   '())
                        (define ,constructor
                          (lambda ,selectors
                            (vector ,tag ,@selectors)))
                        (define ,predicate
                          (lambda (obj)
                            (and (,vectorP obj)
                                 (= (vector-length obj)
                                    ,(+ 1 (length selectors)))
                                 (eq? (vector-ref obj 0) ,tag))))
                        ,@(filter-map-with-index
                           (lambda (n i)
                             `(define ,n
                                (lambda (obj) (vector-ref obj ,i))))
                           selectors)
                        ,@(filter-map-with-index
                           (lambda (n i)
                             (and n
                                  `(define ,n
                                     (lambda (obj newval)
                                       (vector-set!
                                        obj
                                        ,i
                                        newval)))))
                           mutators))))
                 (car args)
                 (cadr args)
                 (caddr args)
                 (reverse g228))
                (if (field? (car g230))
                    (g229 (cdr g230) (cons (car g230) g228))
                    (g227))))
          (g227)))))
(define-macro (define-structure . args)
  (let ((g242 (lambda ()
                (match:syntax-err
                 `(define-structure ,@args)
                 "syntax error in"))))
    (if (and (pair? args)
             (pair? (car args))
             (list? (cdar args)))
        (if (null? (cdr args))
            ((lambda (name id1) `(define-structure (,name ,@id1) ()))
             (caar args)
             (cdar args))
            (if (and (pair? (cdr args)) (list? (cadr args)))
                (let g239 ((g240 (cadr args)) (g238 '()) (g237 '()))
                  (if (null? g240)
                      (if (null? (cddr args))
                          ((lambda (name id1 id2 val)
                             (let ((mk-id (lambda (id)
                                            (if (and (pair? id)
                                                     (equal? (car id) '@)
                                                     (pair? (cdr id))
                                                     (symbol? (cadr id))
                                                     (null? (cddr id)))
                                                ((lambda (x) x) (cadr id))
                                                ((lambda () `(! ,id)))))))
                               `(define-const-structure
                                 (,name ,@(map mk-id id1))
                                 ,(map (lambda (id v) `(,(mk-id id) ,v))
                                       id2
                                       val))))
                           (caar args)
                           (cdar args)
                           (reverse g237)
                           (reverse g238))
                          (g242))
                      (if (and (pair? (car g240))
                               (pair? (cdar g240))
                               (null? (cddar g240)))
                          (g239 (cdr g240)
                                (cons (cadar g240) g238)
                                (cons (caar g240) g237))
                          (g242))))
                (g242)))
        (g242))))
(define-macro (define-const-structure . args)
  (let ((field? (lambda (id)
                  (if (symbol? id)
                      ((lambda () #t))
                      (if (and (pair? id)
                               (equal? (car id) '!)
                               (pair? (cdr id))
                               (symbol? (cadr id))
                               (null? (cddr id)))
                          ((lambda () #t))
                          ((lambda () #f))))))
        (field-name (lambda (x) (if (symbol? x) x (cadr x))))
        (has-mutator? (lambda (x) (not (symbol? x))))
        (filter-map-with-index (lambda (f l)
                                 (letrec ((mapi (lambda (l i)
                                                  (cond
                                                   ((null? l) '())
                                                   ((f (car l) i) =>
                                                    (lambda (x)
                                                      (cons x
                                                            (mapi (cdr l)
                                                                  (+ 1
                                                                     i)))))
                                                   (else (mapi (cdr l)
                                                               (+ 1 i)))))))
                                   (mapi l 1))))
        (symbol-append (lambda l
                         (string->symbol
                          (apply
                           string-append
                           (map (lambda (x)
                                  (cond
                                   ((symbol? x) (symbol->string x))
                                   ((number? x) (number->string x))
                                   (else x)))
                                l))))))
    (let ((g266 (lambda ()
                  (match:syntax-err
                   `(define-const-structure ,@args)
                   "syntax error in"))))
      (if (and (pair? args)
               (pair? (car args))
               (list? (cdar args)))
          (if (null? (cdr args))
              ((lambda (name id1)
                 `(define-const-structure (,name ,@id1) ()))
               (caar args)
               (cdar args))
              (if (symbol? (caar args))
                  (let g259 ((g260 (cdar args)) (g258 '()))
                    (if (null? g260)
                        (if (and (pair? (cdr args)) (list? (cadr args)))
                            (let g263 ((g264 (cadr args))
                                       (g262 '())
                                       (g261 '()))
                              (if (null? g264)
                                  (if (null? (cddr args))
                                      ((lambda (name id1 id2 val)
                                         (let* ((id1id2 (append id1 id2))
                                                (raw-constructor (symbol-append
                                                                  'make-raw-
                                                                  name))
                                                (constructor (symbol-append
                                                              'make-
                                                              name))
                                                (predicate (symbol-append
                                                            name
                                                            '?)))
                                           `(begin (defstruct
                                                    ,name
                                                    ,raw-constructor
                                                    ,predicate
                                                    ,@(filter-map-with-index
                                                       (lambda (arg i)
                                                         (if (has-mutator?
                                                              arg)
                                                             `(,(symbol-append
                                                                 name
                                                                 '-
                                                                 i)
                                                                ,(symbol-append
                                                                  'set-
                                                                  name
                                                                  '-
                                                                  i
                                                                  '!))
                                                             (symbol-append
                                                              name
                                                              '-
                                                              i)))
                                                       id1id2))
                                              ,(let* ((make-fresh (lambda (x)
                                                                    (if (eq? '_
                                                                             x)
                                                                        (gentemp)
                                                                        x)))
                                                      (names1 (map make-fresh
                                                                   (map field-name
                                                                        id1)))
                                                      (names2 (map make-fresh
                                                                   (map field-name
                                                                        id2))))
                                                 `(define ,constructor
                                                    (lambda ,names1
                                                      (let* ,(map list
                                                                  names2
                                                                  val)
                                                        (,raw-constructor
                                                          ,@names1
                                                          ,@names2)))))
                                              ,@(filter-map-with-index
                                                 (lambda (field i)
                                                   (if (eq? (field-name
                                                             field)
                                                            '_)
                                                       #f
                                                       `(define ,(symbol-append
                                                                  name
                                                                  '-
                                                                  (field-name
                                                                   field))
                                                          ,(symbol-append
                                                            name
                                                            '-
                                                            i))))
                                                 id1id2)
                                              ,@(filter-map-with-index
                                                 (lambda (field i)
                                                   (if (or (eq? (field-name
                                                                 field)
                                                                '_)
                                                           (not (has-mutator?
                                                                 field)))
                                                       #f
                                                       `(define ,(symbol-append
                                                                  'set-
                                                                  name
                                                                  '-
                                                                  (field-name
                                                                   field)
                                                                  '!)
                                                          ,(symbol-append
                                                            'set-
                                                            name
                                                            '-
                                                            i
                                                            '!))))
                                                 id1id2))))
                                       (caar args)
                                       (reverse g258)
                                       (reverse g261)
                                       (reverse g262))
                                      (g266))
                                  (if (and (pair? (car g264))
                                           (field? (caar g264))
                                           (pair? (cdar g264))
                                           (null? (cddar g264)))
                                      (g263 (cdr g264)
                                            (cons (cadar g264) g262)
                                            (cons (caar g264) g261))
                                      (g266))))
                            (g266))
                        (if (field? (car g260))
                            (g259 (cdr g260) (cons (car g260) g258))
                            (g266))))
                  (g266)))
          (g266)))))
