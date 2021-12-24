;;; (C) 2020, 2021 Jörg F. Wittenberger, GPL2
;;;
;;; Bindings to ontierzero, the last GPLed version of zerotier.

(declare (not interrupts-enabled))

(cond-expand
 ((or android-api19)
  (c-declare
   #<<EOF
#define lockf(fd, k, fl) 0
EOF
)
  )
 (else))

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

  (httpproxy-connect-set! ot0cli-connect)

  (define beaver-start! ot0cli-process-commands)

  )) ;; cond-expand

(on-ot0-path-lookup #f)

(define beaver-local-unit-id (make-pin #f))
(wire! ot0cli-server post: (lambda () (beaver-local-unit-id (ot0-address))))

(define (beaver-unit-id->beaver-number id)
  (and (number? id)
       (exact? id)
       (positive? id)
       (< id 1099511627776)
       (let ((n (if (even? (bit-count id)) 4000000000000 2000000000000)))
         (+ n id))))

(define (beaver-number->beaver-unit-id id)
  (cond
   ((not (and (number? id) (exact? id) (positive? id))) #f)
   ((> id 4000000000000)
    (let ((x (- id 4000000000000)))
      (and (even? (bit-count x)) x)))
   (else
    (let ((x (- id 2000000000000)))
      (and (positive? x) (odd? (bit-count x)) x)))))

(define (beaver-number->string num #!optional (gap (string (integer->char 183)))) ;; "·"
  (let ((str (number->string num)))
    (and
     (fx= (string-length str) 13)
     (string-append
      (substring str 0 3)
      gap
      (substring str 3 5)
      gap
      (substring str 5 8)
      gap
      (substring str 8 10)
      gap
      (substring str 10 13)))))

(define (beaver-unit-id->string num #!optional (gap (string (integer->char 183)))) ;; "·"
  (beaver-number->string (beaver-unit-id->beaver-number num) gap))

(define unit-id-string->unit-id
  (let ((ignore (rx "[ .-]")))
    (lambda (str)
      (and (string? str)
           (let ((despace (rx-replace/all ignore str)))
             (beaver-number->beaver-unit-id (string->number despace)))))))

(define (beaver-number->unicode-vector num #!optional (gap '#(183))) ;; "·"
  (let ((str (number->string num)))
    (and
     (eqv? (string-length str) 13)
     (let ((buffer (make-ggb size: 17)))
       (do ((i 0 (##fx+ i 1)))
           ((eqv? i 13)
            (ggb->vector buffer))
         (ggb-insert! buffer (char->integer (string-ref str i)))
         (case i
           ((2 4 7 9) (ggb-insert-sequence! buffer gap))))))))

(define (beaver-unit-id->unicode-vector num #!optional (gap '#(183))) ;; "·"
  (beaver-number->unicode-vector (beaver-unit-id->beaver-number num) gap))

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
