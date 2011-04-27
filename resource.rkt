#lang racket/base

(require racket/contract
         racket/match
         racket/file)

;; A resource is something that should be associated to a program, such
;; as source code, HTML, images, and music files.
(define-struct resource (relative-path  ;; string
                         bytes          ;; bytes
                         )
  #:transparent)

;; resource-save!: resource path -> void
;; Write out the resource to disk.
(define (resource-save! a-resource a-base-path #:exists exists)
  (let ([full-path (resource-absolute-path a-resource a-base-path)])
    (let-values ([(dir file is-dir?) (split-path full-path)])
      (unless (directory-exists? dir)
        (make-directory* dir))
      (call-with-output-file full-path
        (lambda (op)
          (write-bytes (resource-bytes a-resource) op))
        #:exists exists))))


(define (resource-absolute-path a-resource a-base-path)
  (build-path a-base-path (unix-path-string->path (resource-relative-path a-resource))))


;; resource->sexp: resource -> s-expression
(define (resource->sexp a-resource)
  (list 'resource
        (resource-relative-path a-resource)
        (resource-bytes a-resource)))

;; sexp->resource: s-expression -> resource
(define (sexp->resource resource-sexp)
  (printf "sexp->resource\n")
  (match resource-sexp
    [(list 'resource 
           (and name (? string?))
           (and bytes (? bytes?)))
     (make-resource name bytes)]

    [(list 'resource 
           (and name (? string?))
           (and bytes-str (? string?)))
     (make-resource name (string->bytes/utf-8 bytes))]))


(define (unix-path-string->path a-path)
  (let ([chunks (regexp-split "[/\\]" a-path)])
    (apply build-path chunks)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide/contract [struct resource ([relative-path string?]
                                    [bytes bytes?])]
                  [resource->sexp (resource? . -> . any)]
                  [sexp->resource (any/c . -> . resource?)]
                  [resource-save! (resource? path-string? #:exists symbol? . -> . any)]
                  [resource-absolute-path (resource? path-string? . -> . path?)])