(define (run-in-LNjScheme #!key (success values) (fail raise) #!rest body)
  (cond-expand
   (android (define (evl expr) (force (lnjscheme-future expr))))
   (else (define evl eval)))
  (thread-start!
   (make-thread
    (lambda ()
      (for-each
       (lambda (expr)
         (with-exception-catcher
          (lambda (exn)
            (log-error
             (call-with-output-string
              (lambda (port)
                (display "EXN in\n" port)
                (pretty-print expr port)
                (display "EXN: " port)
                (display-exception exn port)))))
          (lambda ()
            (log-debug "jScheme EVAL:" 1 expr)
            (call-with-values (lambda () (evl expr)) success))))
       body))
    'jscheme-worker))
  #t)

(define (android-webview-code)
  `(let* ((app ,(android-app-class))
          (this ln-this)
          (intValue (method "intValue" "java.lang.Double"))  ;; FIXME teach jscheme fixnums!
          (String (lambda (x) (new "java.lang.String" x)))
          #;(ln-mglview (method "LNmGLView" app)) ;; deprecated
          #;(trigger-redraw! (let ((run (method "LNtriggerRedraw" app)))
                             (lambda () (run this))))
          )
     (let (
           (getApplicationContext (method "getApplicationContext" app))
           (getWindow (method "getWindow" app))
           (getParent (method "getParent" "android.view.View"))
           (removeView! (method "removeView" "android.view.ViewGroup" "android.view.View"))
           (setText (method "setText" "android.widget.TextView" "java.lang.CharSequence"))
           (addView! (method "addView" "android.view.ViewGroup" "android.view.View"))
           (addView/params! (method "addView" "android.view.ViewGroup" "android.view.View"
                                    "android.view.ViewGroup$LayoutParams"))
           (setContentView (method "setContentView" app "android.view.View"))
           (addContentView (method "addContentView" app "android.view.View" "android.view.ViewGroup$LayoutParams"))
           (setOrientation (method "setOrientation" "android.widget.LinearLayout" "int"))
           ;;
           (onclick-set! (method "LNjScheme_Set_OnClickListener" app "android.view.View" "java.lang.Object"))
           (checkOrRequestPermission (method "checkOrRequestPermission" app "java.lang.String"))
           (loadUrl (method "loadUrl" "android.webkit.WebView" "java.lang.String"))
           (wv-goBack! (method "goBack" "android.webkit.WebView"))
           (wv-setClient! (method "setWebViewClient" "android.webkit.WebView" "android.webkit.WebViewClient"))
           ;;
           (websettings (method "getSettings" "android.webkit.WebView"))
           (wvs-zoom-support-set! (method "setSupportZoom" "android.webkit.WebSettings" "boolean"))
           (wvs-zoom-builtin-set! (method "setBuiltInZoomControls" "android.webkit.WebSettings" "boolean"))
           (wvs-zoom-builtin-controls-set! (method "setDisplayZoomControls" "android.webkit.WebSettings" "boolean"))
           )
       (define (remove-view-from-parent! view)
         (if view
             (let ((parent (getParent view)))
               ;; IMPORTANT: be sure it has a parent!
               (if parent (removeView! parent view)))))
       (define (set-layout-vertical! x)
         (setOrientation x (intValue 1)))
       (define (arrange-in-order! parent childs)
         (for-each (lambda (v) (addView! parent v)) childs))
       (let (
             (frame (new "android.widget.LinearLayout" this))
             (wv (new "android.webkit.WebView" (getApplicationContext this)))
             (wvc (new "android.webkit.WebViewClient"))
             (navigation (new "android.widget.LinearLayout" this)) ;; horizontal is default
             (back (new "android.widget.Button" this))
             (reload (new "android.widget.Button" this))
             (redraw (new "android.widget.Button" this))
             )
         (define (switch-back-to-ln! v)
           (remove-view-from-parent! frame)
           (set! onBackPressed! (lambda () (log-message "onBackPressed!") #f))
           (setContentView this ln-mglview)
           (trigger-redraw!))
         (let ((wvs (websettings wv)))
           (wvs-zoom-support-set! wvs #t)
           (wvs-zoom-builtin-set! wvs #t)
           (wvs-zoom-builtin-controls-set! wvs #f))
         (arrange-in-order! navigation (list back reload redraw))
         (setText back (String "Back"))
         (setText reload (String "Reload"))
         (onclick-set! this back switch-back-to-ln!)
         (onclick-set! this reload (lambda (v) ((method "reload" "android.webkit.WebView") wv)))
         (begin
           (setText redraw (String "Redraw"))
           (onclick-set! this redraw (lambda (v) (trigger-redraw!) #;(redraw-view! wv))))
         (set-layout-vertical! frame)
         (wv-setClient! wv wvc)
         (set-layout-vertical! frame)
         (arrange-in-order! frame (list navigation wv))
         (lambda (cmd arg)
           (case cmd
             ((load) (loadUrl wv (String arg)))
             ((redraw) (trigger-redraw!) #;(redraw-view! frame))
             ((#t)
              (remove-view-from-parent! ln-mglview)
              ;; (set! onBackPressed! (lambda () (wv-goBack! wv)))
              (setContentView this frame)
              #;(redraw-view! frame))
             (else (setContentView this frame) (trigger-redraw!))))))))

(define android-webview
  (let ((in-android-webview
         (match-lambda
          ((#t) '(webview #t #t))
          ((url) `(webview 'load ,url))
          (otherwise
           (begin
             (log-error "android-webview:  call not understood" (object->string otherwise))
             #f))))
        (webview #f))
    (lambda args
      (cond
       ((eq? webview #t)
        (apply
         run-in-LNjScheme
         ;; success: (lambda _ (run-in-LNjScheme '(webview 'redraw #t)))
         (map in-android-webview args)))
       ((eq? webview #f)
        (set! webview 'initializing)
        (apply
         run-in-LNjScheme
         ;; success: (lambda _ (set! webview #t) (run-in-LNjScheme '(webview 'redraw #t)))
         #;`(define (log-message str)
            (let* ((app ,(android-app-class))
                   ;; DOES NOT work on Android 10!!!
                   (log (method "ln_log" app  "java.lang.String")))
              (log ln-this (new "java.lang.String" str))
              #t))
         '(log-message "log-message working, app class:")
         `(log-message ,(debug 'android-app-class (android-app-class)))
         '(define (onBackPressed!) (log-message "onBackPressed!") #f)
         `(define webview ,(android-webview-code))
         (map in-android-webview args))
        (set! webview #t))
       (else
        (log-error
         "android-webview: called again while previous call did not yet return.  IGNORED: "
         (object->string args))))
      #!void)))

(define
  webview-launch!
 (let ((orginal-launch-url launch-url))
   (lambda (url #!key (via #f))
     (cond-expand
      (android
       (case via
         ((webview) (android-webview `(,url) '(#t)))
         ((extern) (orginal-launch-url url))
         (else (orginal-launch-url url))))
      (else (orginal-launch-url url))))))
