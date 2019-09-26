(define jscheme-call
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
                  (jscheme-invoke/s2s req)))
             (r0 (begin
                   (mutex-unlock! mutex)
                   (call-with-input-string s read))))
        (cond
         ;; Numbers are always printed as inexacts by jscheme.
         ((integer? r0) (inexact->exact r0))
         (else r0))))
  jscheme-call))
