;;;** Customizable Imports
(define-macro (assert expr)
  `(or ,expr (error "assertion failed" ',expr)))

(define-macro (ensure pred val)
  `(or (,pred ,val) (error "ensure failed" ',pred ,val)))

#|
(define-type :connsig: (procedure () *))

(: call-with-overwrite
   (string (procedure () *) (or :connsig: (list-of :connsig:))
	   -> (procedure () *))) ;; return value declaration is incomplete
|#

(define (call-with-overwrite key thunk sig)
  (define (display-content content port)
    (cond
     ((blob? content)
      (write-u8vector (blob->u8vector/shared content) port))
     (else (display content port))))
  ;; TBD: create temp file, change owner and permissions to match
  ;; target OR PARAMETERS TO BE ADDED, write temp file, return thunk
  ;; which links temp file to target file.
  (let ((content (thunk)))
    (if content
	(receive (d f e) (decompose-pathname key) (if d (create-directory d #t))))
    ;; Here we SHALL prepare a temporary file, adjust owner and
    ;; permissions and then fill with the content.
    (lambda ()
      ;; Once that's done we will use "link" respectively "unlink"
      ;; here to update the target location from the temporary file.
      ;; For now we simply overwrite.
      (if content
	  (call-with-output-file key (lambda (port) (display-content content port)))
	  (if (file-exists? key) (delete-file key)))
      sig)))
