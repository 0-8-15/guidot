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
     (cond
      ((not content) (if (file-exists? file) (delete-file file)))
      ((eq? content #t) #f)  ;; Keept old content
      (else (call-with-output-file file (lambda (port) (display-content content port))))))))

(define (call-with-overwrite file thunk sig)
  ;; FIXME: should handle volume letter for w32
  (cond-expand
   (chicken
    (define (display-content content port)
      (cond
       ((blob? content)
        (write-u8vector (blob->u8vector/shared content) port))
       (else (display content port)))))
   (gambit
    (define (display-content content port)
      (cond
       ((u8vector? content)
        (write-subu8vector content 0 (u8vector-length content) port))
       (else (display content port))))))
  (let ((content (thunk))
        (directory #f))
    ;; Assert preconditions for a fast, simple and *abortable external*
    ;; commit are met: start external transactions
    (if content
	(cond-expand
         (chicken (receive (d f e) (decompose-pathname file) (set! directory d)))
         (gambit (let ((d (path-directory file))) (unless (equal? d "") (set! directory d))))))
    (lambda ()
      ;; Here we SHALL prepare a temporary file, adjust owner and
      ;; permissions and then fill with the content.

      ;; TBD: create temp file, change owner and permissions to match
      ;; target file OR PARAMETERS TO BE ADDED, write temp file, pass
      ;; reference to new content
      (if directory
          (cond-expand
           (chicken (create-directory directory #t))
           (gambit (unless (file-exists? directory) (create-directory directory)))))
      ;; TBD: Once we have temporary files use "link" respectively
      ;; "unlink" here to update the target location from the
      ;; temporary file.
      ;;
      ;; For now we simply overwrite.
      (overwrite-in-toplevel file display-content content)
      ;; TBD: merge `sig` with "link/unlink" operations and commit
      ;; external transactions (e.g., external database transactions
      ;; opened in the outer part).
      ;;
      ;; For now we only pass back `sig.
      sig)))
