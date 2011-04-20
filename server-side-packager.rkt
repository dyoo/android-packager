#lang racket/base

(require racket/runtime-path
         racket/cmdline
         racket/list
         racket/file
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


(define-runtime-path HTDOCS-PATH "htdocs")
(define-runtime-path LOG-PATH "logs")

(define current-access-logger (make-parameter #f))


;; start: request -> response
(define (start req)
  (with-handlers (#;[exn:fail? handle-unexpected-error])
    (let* ([name (parse-program-name req)]
           [permissions (parse-permissions req)]
           [resources (parse-resources req)]
           [builder (select-builder req)])
      ;; (write-to-access-log! req (resources->bytes resources))
      (cond
        [(and name (not (empty? resources)))
         (make-package-response 
          name 
          (builder name permissions resources))]
        [else
         (error-no-program)]))))


;; select-builder: request -> (String (listof string) (listof resource) -> bytes)
(define (select-builder req)
  (let ([error-builder      
         (lambda (name permissions resources)
           (error 'builder "Unknown builder"))])
    (cond
      [(exists-binding? 't (request-bindings req))
       (let ([type (extract-binding/single 't (request-bindings req))])
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


;; parse-name: request -> string
;; Extracts the name from the request
(define (parse-program-name req)
  (cond
    [(exists-binding? 'n (request-bindings req))
     (extract-binding/single 'n (request-bindings req))]
    [else
     "program"]))


;; parse-permissions: request -> (listof string)
;; Produces the list of Android permissions required by this package.
(define (parse-permissions req)
  (cond
    [(exists-binding? 'ps (request-bindings req))
     (extract-bindings 'ps (request-bindings req))]
    [else
     '()]))
     

;; parse-resources: bindings -> (listof resource)
(define (parse-resources req)
  (cond [(exists-binding? 'r (request-bindings req))
         (map (lambda (val)
                (sexp->resource (read (open-input-string val))))
              (extract-bindings 'r (request-bindings req)))]
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
(define LOGFILE-PATH (make-parameter (build-path LOG-PATH "access.log")))
(unless (directory-exists? LOG-PATH)
  (make-directory* LOG-PATH))

(command-line #:once-each 
              [("-p" "--port") port "Use port for web server"
                               (PORT (string->number port))]
              [("-L" "--logfile-dir") logfile-dir "Use the directory to write access.log"
                                      (LOGFILE-PATH (build-path logfile-dir "access.log"))])


#;(current-access-logger (make-logger (LOGFILE-PATH)))
(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port (PORT)
               #:extra-files-paths (list HTDOCS-PATH)                 
               #:servlet-regexp (regexp
                                 "^.*$"  #;"^/package/.*$"))