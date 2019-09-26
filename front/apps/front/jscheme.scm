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
                   (else (error "jscheme-call: not availible on platform" (system-platform))))))
             (r0 (begin
                   (mutex-unlock! mutex)
                   (if (string? s)
                       (call-with-input-string s read)
                       (error "jscheme-call: unexpected reply " s)))))
        (cond
         ;; Numbers are always printed as inexacts by jscheme.
         ((integer? r0) (inexact->exact r0))
         (else r0))))
  jscheme-call))
