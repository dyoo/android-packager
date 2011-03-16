#lang racket/base

(require racket/contract
         (prefix-in local: "local-android-packager.ss")
         (prefix-in remote: "server-side-packager/client-side-packager.ss")
         "config.ss"
         "resource.ss")


(provide/contract
 [build-android-package (string? (listof resource?)
                                 #:permissions (listof string?)
                                 . -> . bytes?)])


;; local-android-ready?: -> boolean
;; Produces true if we can do local android packaging.
(define (local-android-ready?)
  (and (file-exists? (current-ant-bin-path))
       (directory-exists? (current-android-sdk-path))))
  

;; build-android-package: string program/resources -> bytes
;; Either tries to use the local android packager; if the resources aren't available,
;; then tries to use the web service.
(define (build-android-package program-name resources)
  (cond
    [(local-android-ready?)
     (local:build-android-package program-name
                                  resources
                                  #:permissions permissions)]
    [else
     (remote:build-android-package program-name 
                                   resources
                                   #:permissions permissions)]))
