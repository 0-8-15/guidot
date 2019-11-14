(define (run-in-LNjScheme expr)
  (cond-expand
   (android (define (evl expr) (force (jscheme-future expr))))
   (else (define evl eval)))
  (thread-start!
   (make-thread
    (lambda ()
      (with-exception-catcher
       (lambda (exn)
         (log-error "EXN: " (exception-->printable exn) " in: " expr)
         (newline))
       (lambda () (evl expr))))
    'jscheme-worker))
  #t)

(set!
 launch-url
 (let ((launch-url orginal-launch-url))
   (define (android-webview-launch! url)
     (run-in-LNjScheme '(define (onBackPressed!) #f))
     (run-in-LNjScheme
      `(let* ((app ,(android-app-class))
              (this ((method "me" app)))
              (intValue (method "intValue" "java.lang.Double"))  ;; FIXME teach jscheme fixnums!
              (String (lambda (x) (new "java.lang.String" x)))
              (ln-mglview (method "LNmGLView" app)) ;; deprecated
              (trigger-redraw! (let ((run (method "LNtriggerRedraw" app)))
                                 (lambda () (run this))))
              )
         (let (
               (getApplicationContext (method "getApplicationContext" app))
               (getWindow (method "getWindow" app))
               (getParent (method "getParent" "android.view.View"))
               (removeView! (method "removeView" "android.view.ViewGroup" "android.view.View"))
               (setText (method "setText" "android.widget.TextView" "java.lang.CharSequence"))
               (addView! (method "addView" "android.view.ViewGroup" "android.view.View"))
               (setContentView (method "setContentView" app "android.view.View"))
               (setOrientation (method "setOrientation" "android.widget.LinearLayout" "int"))
               ;;
               (onclick-set! (method "LNjScheme_Set_OnClickListener" app "android.view.View" "java.lang.Object"))
               (loadUrl (method "loadUrl" "android.webkit.WebView" "java.lang.String"))
               (wv-goBack! (method "goBack" "android.webkit.WebView"))
               (wv-setClient! (method "setWebViewClient" "android.webkit.WebView" "android.webkit.WebViewClient"))
               ;;
               (websettings (method "getSettings" "android.webkit.WebView"))
               (wvs-zoom-support-set! (method "setSupportZoom" "android.webkit.WebSettings" "boolean"))
               (wvs-zoom-builtin-set! (method "setBuiltInZoomControls" "android.webkit.WebSettings" "boolean"))
               (wvs-zoom-builtin-controls-set! (method "setDisplayZoomControls" "android.webkit.WebSettings" "boolean"))
               )
           (define (switch-back-to-ln! v)
             (let ((ln-glview (ln-mglview this)))
               ;; (removeView! (getParent ln-glview) ln-glview) ;; IMPORTANT: be sure it has a parent!
               (set! onBackPressed! (lambda () #f))
               (setContentView this ln-glview)
               (trigger-redraw!)))
           (define (set-layout-vertical! x)
             (setOrientation x (intValue 1)))
           (define (arrange-in-order! parent childs)
             (for-each (lambda (v) (addView! parent v)) childs))
           (let (
                 (frame (new "android.widget.LinearLayout" this))
                 (wv (new "android.webkit.WebView" (getApplicationContext this)))
                 (wvc (new "android.webkit.WebViewClient"))
                 (button (new "android.widget.Button" this))
                 )
             (let ((wvs (websettings wv)))
               (wvs-zoom-support-set! wvs #t)
               (wvs-zoom-builtin-set! wvs #t)
               (wvs-zoom-builtin-controls-set! wvs #f))
             (set-layout-vertical! frame)
             (setText button (new "java.lang.String" "Back"))
             (onclick-set! this button switch-back-to-ln!)
             (wv-setClient! wv wvc)
             (set! onBackPressed! (lambda () (wv-goBack! wv)))
             ;;(removeView! (getParent ln-glview) ln-glview)
             (set-layout-vertical! frame)
             (arrange-in-order! frame (list button wv))
             (loadUrl wv (String ,url))
             (setContentView this frame)
             #t)))))
   (lambda (url)
     (cond-expand
      (android (android-webview-launch! url))
      (else (orginal-launch-url url))))))
