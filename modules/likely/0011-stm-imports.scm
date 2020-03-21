;;;** Customizable Imports
(define-macro (assert expr)
  `(or ,expr (error "assertion failed" ',expr)))

(define-macro (ensure pred val)
  `(or (,pred ,val) (error "ensure failed" ',pred ,val)))

(define-macro (null-list? x) `(null? ,x))
