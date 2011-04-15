#lang racket/base
(require racket/file
         racket/runtime-path
         racket/path
         racket/contract
         racket/list
         (only-in xml xexpr->string)
         "utils.rkt"
         "resource.rkt"
         "run-ant.rkt"
         "config.rkt"
         (for-syntax racket/base))


(define-runtime-path moby2-phonegap-path (build-path "phonegap" "moby2" "android-1.5"))
(define-runtime-path moby3-phonegap-path (build-path "phonegap" "moby3" "android-1.5"))

(define-runtime-path icon-path "icon.png")


;; build-android-package: string (listof resources) #:permissions -> bytes
(define (build-android-package program-name resources 
                               #:permissions (permissions '())
                               #:phonegap-path (phonegap-path moby2-phonegap-path))
  (with-temporary-directory
   (lambda (dir)
     (let ([dest (simplify-path (build-path dir program-name))])
       (build-android-package-in-path program-name
                                      resources
                                      permissions
                                      dest
                                      phonegap-path)
              
       (get-file-bytes 
        (first (find-files (lambda (a-path)
                             (equal? (filename-extension a-path)
                                     #"apk"))
                           dest)))))))



;; FIXME: name must be cleaned up: it must not have any non-alphanumeric/whitespace, or
;; else bad things happen.
(define (build-android-package-in-path name resources permissions dest
                                       moby2-phonegap-path) 
  (unless (file-exists? (current-ant-bin-path))
    (error 'build-android-package-in-path
           "The Apache ant binary appears to be missing from the current PATH."))
  (unless (directory-exists? (current-android-sdk-path))
    (error 'build-android-package-in-path
           "The Android SDK could not be found."))
  
  (make-directory* dest)
  (copy-directory/files* moby2-phonegap-path dest)
  (let* ([normal-name (normalize-name name)]
         [classname (upper-camel-case normal-name)]
         [package (string-append "plt.moby." classname)])

    ;; Write out all the resources into the android package's assets.
    (for ([r resources])
      (resource-save! r (build-path dest "assets")
                      #:exists 'replace))
    
    
    ;; Write out the icon
    (make-directory* (build-path dest "res" "drawable"))
    (copy-or-overwrite-file icon-path (build-path dest "res" "drawable" "icon.png"))
    

    ;; Put in the customized manifest.
    (write-android-manifest (build-path dest "AndroidManifest.xml")
                            #:name name
                            #:package package
                            #:activity-class (string-append package "." classname)
                            #:permissions permissions)
    
    ;; Write out local properties so that the build system knows how to compile
    (call-with-output-file (build-path dest "local.properties")
      (lambda (op)
        (fprintf op "sdk-location=~a~n" (path->string (current-android-sdk-path)))
        (fprintf op "sdk.dir=~a~n" (path->string (current-android-sdk-path))))
      #:exists 'replace)
    
    ;; HACKS!
    ;; Fix build.xml so it refers to our application.
    (let* ([build-xml-bytes (get-file-bytes (build-path dest "build.xml"))]
           [build-xml-bytes (regexp-replace #rx"DroidGap"
                                            build-xml-bytes
                                            (string->bytes/utf-8 classname))])
      (call-with-output-file (build-path dest "build.xml")
        (lambda (op) (write-bytes build-xml-bytes op))
        #:exists 'replace))
    
    
    ;; Write out a customized strings.xml
    (let* ([strings-xml-bytes (get-file-bytes (build-path dest "res" "values" "strings.xml"))]
           [strings-xml-bytes (regexp-replace #rx"DroidGap"
                                              strings-xml-bytes
                                              (string->bytes/utf-8 (xexpr->string name)))])
      ;; FIXME: don't use regular expressions here!
      (call-with-output-file (build-path dest "res" "values" "strings.xml")
        (lambda (op) (write-bytes strings-xml-bytes op))
        #:exists 'replace))
    
    
    ;; Rename DroidGap to the application name.
    (make-directory* (build-path dest "src" "plt" "moby" classname))
    (let* ([middleware 
            (get-file-bytes (build-path dest "src" "com" "phonegap" "demo" "DroidGap.java"))]
           [middleware 
            (regexp-replace #rx"package com.phonegap.demo;\n" 
                            middleware
                            (string->bytes/utf-8 (format "package plt.moby.~a;\nimport com.phonegap.demo.*;\n" classname)))]
           [middleware 
            (regexp-replace #rx"DroidGap" 
                            middleware
                            (string->bytes/utf-8 classname))])
      (call-with-output-file (build-path dest "src" "plt" "moby" classname (format "~a.java" classname))
        (lambda (op)
          (write-bytes middleware op))
        #:exists 'replace)
      (delete-file (build-path dest "src" "com" "phonegap" "demo" "DroidGap.java")))
    
    
    ;; Write out the defaults.properties so that ant can build
    ;; Run ant debug.
    (run-ant-build.xml dest "debug")))



;; normalize-name: string -> string
;; Translate a name so it doesn't screw with Java conventions.
(define (normalize-name a-name)
  (let ([a-name (regexp-replace* #px"[^\\w\\s]+" a-name "")])
    (cond
      [(or (= (string-length a-name) 0)
           (not (char-alphabetic? (string-ref a-name 0))))
       (string-append "_" a-name)]
      [else
       a-name])))



;; write-android-manifest: path (#:name string) (#:permissions (listof string)) -> void
(define (write-android-manifest path
                                #:name name
                                #:package package
                                #:activity-class activity-class
                                #:permissions (permissions '()))
  (call-with-output-file path
    (lambda (op)
      (display (get-android-manifest #:name name
                                     #:package package 
                                     #:activity-class activity-class 
                                     #:permissions permissions) op))
    #:exists 'replace))




;; get-android-manifest: (#:name string) (#:package string) (#:activity-class string) (#:permissions (listof string)) -> string
(define (get-android-manifest #:name name
                              #:package package
                              #:activity-class activity-class
                              #:permissions (permissions '()))
  (let ([AndroidManifest.xml
         `(manifest 
           ((xmlns:android "http://schemas.android.com/apk/res/android")
            (package ,package)
            (android:versionCode "1")
            (android:versionName "1.0.0"))
           
           (uses-sdk ((android:minSdkVersion "2")))
           
           ,@(map (lambda (p)
                    `(uses-permission ((android:name ,p))))
                  permissions)
           
           (application 
            ((android:label "@string/app_name")
             (android:icon "@drawable/icon"))
            
            (activity ((android:name ,activity-class)
                       (android:label ,name)
                       (android:screenOrientation "portrait")
                       (android:configChanges
                        "keyboardHidden|orientation"))
                      (intent-filter 
                       ()
                       (action ((android:name "android.intent.action.MAIN")))
                       (category
                        ((android:name
                          "android.intent.category.LAUNCHER")))))
            
            (activity ((android:name "plt.playlist.PickPlaylist")
                       (android:label "PickPlaylist")
                       (android:configChanges
                        "keyboardHidden|orientation"))
                      (action ((android:name "android.intent.action.PICK")))
                      (category ((android:name "android.intent.category.DEFAULT"))))))])
    
    (xexpr->string AndroidManifest.xml)))



(provide/contract [build-android-package 
                   ((string? (listof resource?)) 
                    (#:permissions (listof string?)
                                   #:phonegap-path path?)
                    . ->* . bytes?)])






