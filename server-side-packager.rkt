#lang racket/base

(require racket/runtime-path
         racket/cmdline
         racket/list
         racket/file
         racket/async-channel
         file/gunzip
         "uri-codec.rkt" ;; net/uri-codec
         web-server/servlet
         web-server/servlet-env
	 web-server/http/response-structs
         xml
         "resource.rkt"
         "utils.rkt"
         "packager.rkt"
         (for-syntax racket/base))




(define-runtime-path moby2-phonegap-path (build-path "phonegap" "moby2" "android-1.5"))
(define-runtime-path moby3-phonegap-path (build-path "phonegap" "moby3" "android-1.5"))


;; This is a servlet that compiles packages to Android.
;;
;; Expected input: a program.
;;
;; Expected output: the android package.
;; Should throw an error page.  If error during compilation, should include the domified
;; error structure.
;;
;; Parameters
;;
;;     t : string.  type.
;;     n : string.  default "program" name.
;;     ps : string.  Repeatable.  Permission
;;     r : resource-sexp-string.  Repeatable

;; Optionally
;;     cb : callback.  URL.  If defined, then queues off a compilation request.
;;        The system will compile the package when it's idle, and then send the result of
;;        the compilation as POST data to the callback url.
(define-runtime-path HTDOCS-PATH "htdocs")
(define-runtime-path LOG-PATH "logs")

(define current-access-logger (make-parameter #f))




;; FIXME: there's an issue here because this makes the server stateful.
;; We may want to push the content of these jobs to disk, in case the server
;; dies.
(define-struct job (builder name permissions resources callback))


(define-syntax (ignoring-errors stx)
  (syntax-case stx ()
    [(_ body ...)
     (syntax/loc stx
       (with-handlers (#;[void (lambda (exn)
                               (fprintf (current-error-port) "~e" exn))])
         body ...))]))
     


(define job-channel (make-async-channel))
(define background-compiler-thread
  (thread 
   (lambda ()
     (let loop ()
       (printf "Background compiler waiting for task.\n")
       (let ([job (sync job-channel)])
         (ignoring-errors 
          (printf "Building new job ~s...\n" (job-name job))
          (let ([package ((job-builder job)
                          (job-name job)
                          (job-permissions job)
                          (job-resources job))])
            (printf "Package built.  Sending package back to ~s\n"
                    (job-callback job))
            ;; send the package back to the callback url.
            #;(post-pure-port (string->url (job-callback job)) package))))
       (loop)))))




;; start: request -> response
(define (start req)
  (with-handlers (#;[exn:fail? handle-unexpected-error])
    (let* ([bindings (time (uncompress-bindings req))]
           [name (time (parse-program-name bindings))]
           [permissions (time (parse-permissions bindings))]
           [resources (time (parse-resources bindings))]
           [builder (time (select-builder bindings))]
           [callback-url (time (parse-callback-url bindings))])
      (cond
        [(and name (not (empty? resources)))
         (cond [(string? callback-url)
                (let ([job (make-job builder name permissions resources callback-url)])
                  (schedule-job! job)
                  (make-job-running-response job))]
               [else
                (make-package-response 
                 name 
                 (builder name permissions resources))])]
        [else
         (error-no-program)]))))


(define (schedule-job! job)
  (async-channel-put job-channel job))


;; bindings may be compressed in the raw POST data, in which case
;; we uncompress first.
(define (uncompress-bindings req)
  (cond
    [(url-mentions-gzip? (request-uri req))
     (let ([bytes (request-post-data/raw req)]
           [uncompressed (open-output-string)])
       (gunzip-through-ports (open-input-bytes bytes) uncompressed)
       (form-urlencoded->alist (get-output-string uncompressed)))]
    [else
     (request-bindings req)]))


;; parse-callback-url: bindings -> (U #f string)
;; Either we get #f, and act synchronously, or we get a callback and
;; act asynchronously.
(define (parse-callback-url bindings)
  (cond
    [(exists-binding? 'cb bindings)
     (extract-binding/single 'cb bindings)]
    [else
     #f]))


(define (url-mentions-gzip? a-url)
  (let loop ([path-chunks (url-path a-url)])
    (cond
      [(empty? path-chunks)
       #f]
      [else
       (let ([a-path-param (first path-chunks)])
         (cond
           [(and (string? (path/param-path a-path-param))
                 (string=? (path/param-path a-path-param)
                           "gzip"))
            #t]
           [else
            (loop (rest path-chunks))]))])))



;; select-builder: bindings -> (String (listof string) (listof resource) -> bytes)
(define (select-builder bindings)
  (let ([error-builder      
         (lambda (name permissions resources)
           (error 'builder "Unknown builder"))])
    (cond
      [(exists-binding? 't bindings)
       (let ([type (extract-binding/single 't bindings)])
         (cond
           [(string=? type "moby2")
            (lambda (name permissions resources)
              (build-android-package name resources 
                                     #:permissions permissions
                                     #:phonegap-path moby2-phonegap-path))]
           [(string=? type "moby3")
            (lambda (name permissions resources)
              (build-android-package name resources 
                                     #:permissions permissions
                                     #:phonegap-path moby3-phonegap-path))]
           [else
            error-builder]))]
      [else
       error-builder])))


;; parse-name: bindings -> string
;; Extracts the name from the request
(define (parse-program-name bindings)
  (cond
    [(exists-binding? 'n bindings)
     (extract-binding/single 'n bindings)]
    [else
     "program"]))


;; parse-permissions: bindings -> (listof string)
;; Produces the list of Android permissions required by this package.
(define (parse-permissions bindings)
  (cond
    [(exists-binding? 'ps bindings)
     (extract-bindings 'ps bindings)]
    [else
     '()]))
     

;; parse-resources: bindings -> (listof resource)
(define (parse-resources bindings)
  (cond [(exists-binding? 'r bindings)
         (map (lambda (val)
                (sexp->resource (read (open-input-string val))))
              (extract-bindings 'r bindings))]
        [else
         empty]))



;; make-package-response: string bytes -> response
;; Produces the proper HTTP response for the android package.
;; Headers also include the filename in the content-disposition field, so the
;; user gets a useful file name.
(define (make-package-response program-name package-bytes)
  ;; normalize-name-as-filename: string -> string
  (define (normalize-name-as-filename a-name)
    (let ([a-name (upper-camel-case a-name)])
      (cond
        [(string=? a-name "")
         "program"]
        [else
         a-name])))
  
  (response/full
   200
   #"OK"(current-seconds)
   #"application/vnd.android.package-archive"
   (list (make-header #"content-disposition"
                      (string->bytes/utf-8 
                       (format "attachment; filename=~a.apk" 
                               (normalize-name-as-filename program-name)))))
   (list package-bytes)))


(define (make-job-running-response job)
  (response/xexpr
   '(html
     (head (title "Job queued"))
     (body (p "Job queued.  Program " (i ,(job-name job))
              "will be compiled and its result be sent back to "
              (tt ,(job-callback-url job)) 
              ".")))))
           



;; error-no-program: -> response
(define (error-no-program)
  (response/full
   400
   #"Bad Request"
   (current-seconds)
   #"text/html"
   (list)
   (list (string->bytes/utf-8 
          (xexpr->string 
           `(html (head (title error))
                  (body
                   "The expected program is missing from the request.")))))))


;; handle-unexpected-error: exn:fail -> response
(define (handle-unexpected-error exn)
   (response/full
     400
     #"Bad Request"
     (current-seconds)
     #"text/html"
     (list)
     (list (string->bytes/utf-8 
            (xexpr->string 
             `(html (head (title error))
                    (body
                     "The android packager was unable to build your program due to an unexpected error.\n"
                     (br)
                     "Please contact the developer (dyoo@cs.wpi.edu), and include the following content:\n"
                     (br)
                     ,(exn-message exn))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define PORT (make-parameter 8080))
#;(define LOGFILE-PATH (make-parameter (build-path LOG-PATH "access.log")))
#;(unless (directory-exists? LOG-PATH)
    (make-directory* LOG-PATH))

(command-line #:once-each 
              [("-p" "--port") port "Use port for web server"
                               (PORT (string->number port))]
              #;[("-L" "--logfile-dir") logfile-dir "Use the directory to write access.log"
                                      (LOGFILE-PATH (build-path logfile-dir "access.log"))])


(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port (PORT)
               #:extra-files-paths (list HTDOCS-PATH)                 
               #:servlet-regexp (regexp
                                 "^.*$"  #;"^/package/.*$"))