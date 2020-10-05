(cond-expand
 (android-nonotnow

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

  (include "~~tgt/lib/onetierzero/ot0.scm")
  (include "~~tgt/lib/onetierzero/src/ot0use.scm")

  (include "~~tgt/lib/onetierzero/src/ot0cli.scm")

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

(define beaver-stdout-redirection #t)

(define beaver-use-daemonize
  (let ((v (cond-expand
            ((or win32) #f)
            (else #t))))
    (case-lambda
     (() v)
     ((x) (set! v x)))))

(define (beaver-process-commands args)
  (define (convert obj)
    (if (string? obj) obj (object->string obj)))
  (let ((args (map convert args)))
    (match
     args
     (("-D" DIR . more)
      (let ((redir '("-cs" "daemonian-stdout-file" "NULL" "daemonian-stderr-file" "NULL"  ":")))
        #;(ot0cli-process-commands (debug 'giblaut `("-D" ,DIR ,@redir ,@more)))
        (if (beaver-use-daemonize)
            (daemonize `("cerberus" "beaver" ,@redir "-B" ,DIR ,@more))
            (cerberus (system-cmdargv 0)
                      `(,@redir "-B" ,DIR ,@more)
                      startup-delay: 3 max-fast-restarts: 2
                      stdout-redirection: beaver-stdout-redirection
                      exit: exit))))
     (else (ot0cli-process-commands args)))))
