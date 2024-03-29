(cond-expand
 (android (define android-app-class (c-lambda () char-string "___result=android_app_class();")))
 (else (define (android-app-class) "android-app-class")))

(define jscheme-eval
  ;; Not sure that we need a mutex here.  But what if the java side
  ;; manages to call into gambit?
  (let ((mutex (make-mutex 'jscheme)))
    (define jscheme-invoke/s2s
      (c-lambda (char-string) char-string "
#ifdef ANDROID
extern const char *jscheme_eval(const char *);
#endif
___result=
#ifdef ANDROID
(char*) jscheme_eval(___arg1);
#else
NULL;
#endif
"))
    (define (jscheme-call obj)
      (let* ((s (let ((req (object->string obj)))
                  (mutex-lock! mutex)
                  (cond-expand
                   (android (jscheme-invoke/s2s req))
                   (else (error "jscheme-call: not available on platform" (system-platform))))))
             (r0 (begin
                   (mutex-unlock! mutex)
                   (if (string? s)
                       (call-with-input-string s
                        (lambda (port)
                          (let* ((key (read port))
                                 (value
                                  (with-exception-catcher
                                   (lambda (exn) (raise (string-append "jscheme-call: unreadable result: " s)))
                                   (lambda () (read port)))))
                            (case key
                              ((D) value)
                              ((E) (raise value))
                              (else (error "jscheme-call: unexpected reply " s))))))
                       (error "jscheme-call: unexpected reply " s)))))
        (cond
         ;; Numbers are always printed as inexacts by jscheme.
         ((integer? r0) (inexact->exact r0))
         (else r0))))
  jscheme-call))

(define LNjScheme-result #f)

(define jscheme-future
  ;; Not sure that we need a mutex here.  But what if the java side
  ;; manages to call into gambit?
  (let ((mutex (make-mutex 'LNjScheme)))
    (define jscheme-send
      (c-lambda (char-string) void "
#ifdef ANDROID
extern void jscheme_eval_send(const char *);
jscheme_eval_send(___arg1);
#endif
"))
    (define jscheme-receive
      (c-lambda () char-string "
#ifdef ANDROID
extern const char *jscheme_eval_receive_result();
#endif
___result=
#ifdef ANDROID
(char*) jscheme_eval_receive_result();
#else
NULL;
#endif
"))
    (define (noresult) #f)
    (define (reset!) (set! LNjScheme-result noresult))
    (define (jscheme-call obj)
      (cond-expand
       (android)
       (else (error "jscheme-call: not availible on platform" (system-platform))))
      (mutex-lock! mutex)
      (let ((resm (make-mutex obj)))
        (mutex-lock! resm)
        (set! LNjScheme-result
              (lambda ()
                (reset!)
                (mutex-specific-set! resm (jscheme-receive))
                (mutex-unlock! mutex)
                (mutex-unlock! resm)))
        (jscheme-send (object->string obj))
        (delay
          (let* ((s (begin
                      (mutex-lock! resm #f #f)
                      (mutex-specific resm)))
                 (r0 (begin
                       (if (string? s)
                           (call-with-input-string
                            s
                            (lambda (port)
                              (let* ((key (read port))
                                     (value
                                      (with-exception-catcher
                                       (lambda (exn) (raise (string-append "jscheme-call: unreadable result: " s)))
                                       (lambda () (read port)))))
                                (case key
                                  ((D) value)
                                  ((E) (raise value))
                                  (else (error "jscheme-call: unexpected reply " s))))))
                           (error "jscheme-call: unexpected reply " s)))))
            (cond
             ;; Numbers are always printed as inexacts by jscheme.
             ((integer? r0) (inexact->exact r0))
             (else r0))))))
  (reset!)
  jscheme-call))
