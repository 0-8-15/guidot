(cond-expand
 (android

  (define beaver-start!
    (match-lambda
     (X
      (begin
        (log-error "beaver command line not handled" X)
        #f))))
  )
 (else

  (define-cond-expand-feature embedded-module)

  (c-declare
   #<<end-of-c-declare

#if WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#include <time.h>
#endif

end-of-c-declare
)

  (include "~~tgtlib/onetierzero/ot0.scm")
  (include "~~tgtlib/onetierzero/src/ot0use.scm")

  (include "~~tgtlib/onetierzero/src/ot0cli.scm")

  (define beaver-start! ot0cli-process-commands)

  )) ;; cond-expand

;; (define-cond-expand-feature enable-beaver-debug)

(cond-expand
 ((or debug enable-beaver-debug)
  (define-macro (beaver-debug l v) `(log-debug ,l 1 ": " (debug ,l ,v))))
 (else
  (define-macro (beaver-debug l v) #!void)))

(define make-beaver-api
  (let ()
    (define (send-request! conn expr)
      (write expr conn)
      (force-output conn))
    (define (dispatch-result conn)
      (let ((result (read conn)))
        (close-port conn)
        (beaver-debug 'Beaver-Result result)
        (match
         result
         ((ref 'E . err) (error err))
         ((ref 'D . vals) (apply values vals))
         ((? eof-object?) #!void)
         (X (error "beaver call: bad protocol reply" X)))))
    (define (with-unix-client sockaddr kind expr)
      (beaver-debug (list 'beaver kind) expr)
      (let ((conn (open-unix-client sockaddr #t)))
        (if conn
            (begin
              (send-request! conn `(0 ,kind . ,expr))
              (dispatch-result conn))
            (error "beaver call failed to connect"))))
    (lambda (directory)
      (define sockaddr (make-pathname directory "control"))
      (values
       (lambda () (let ((port (open-unix-client* sockaddr (lambda () #f))))
                    (and port (begin (close-port port) #t))))
       (lambda (expr) (with-unix-client sockaddr 'Q expr))
       ;; make-beaver-caller
       (lambda (expr) (with-unix-client sockaddr 'P expr))))))

(define (beaver-process-commands args)
  (define (convert obj)
    (if (string? obj) obj (object->string obj)))
  (ot0cli-process-commands (map convert args)))
