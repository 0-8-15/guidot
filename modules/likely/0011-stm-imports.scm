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

(define overwrite-in-toplevel
  ;; This may better use stm-consistency-error?
  (dynamic
   (lambda (file display-content content)
     (if content
         (call-with-output-file key (lambda (port) (display-content content port)))
         (if (file-exists? key) (delete-file key))))))

(define (call-with-overwrite file thunk sig)
  (define (display-content content port)
    (cond
     ((blob? content)
      (write-u8vector (blob->u8vector/shared content) port))
     (else (display content port))))
  ;; Here we SHALL prepare a temporary file, adjust owner and
  ;; permissions and then fill with the content.

  ;; TBD: create temp file, change owner and permissions to match
  ;; target file OR PARAMETERS TO BE ADDED, write temp file, pass
  ;; reference to new content
  (let ((content (thunk)))
    ;; Assert preconditions for a fast, simple and abortable external
    ;; commit are met: create required directorys, temporary files,
    ;; start external transactions etc.
    (if content
	(receive (d f e) (decompose-pathname file) (create-directory directory #t)))
    (lambda ()
      ;; commit external transactions (e.g., external database
      ;; transactions opened in the outer part).
      #t ;; nothing to be done here
      ;; TBD: Once we have temporary files use "link" respectively
      ;; "unlink" here to update the target location from the
      ;; temporary file.  For now we simply overwrite.
      (overwrite-in-toplevel file display-content content)
      sig)))
