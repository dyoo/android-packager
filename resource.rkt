#lang racket/base

(require racket/class
         racket/contract
         racket/match)

;; A resource is something that should be associated to a program, such
;; as source code, HTML, images, and music files.
(define-struct resource (relative-path  ;; string
                         bytes          ;; bytes
                         )
  #:transparent)

;; resource-save!: resource path -> void
;; Write out the resource to disk.
(define (resource-save! a-resource a-root-path)
  (call-with-output-file (build-path a-root-path (resource-relative-path a-resource))
    (lambda (op)
      (write-bytes (resource-bytes a-resource) op))))


;; resource->sexp: resource -> s-expression
(define (resource->sexp a-resource)
  (list 'resource
        (resource-relative-path a-resource)
        (resource-bytes a-resource)))

;; sexp->resource: s-expression -> resource
(define (sexp->resource resource-sexp)
  (match resource-sexp
    [(list 'resource 
           (and name (? string?))
           (and bytes (? bytes?)))
     (make-resource name bytes)]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide/contract [struct resource ([relative-path string?]
                                    [bytes bytes?])]
                  [resource->sexp (resource? . -> . any)]
                  [sexp->resource (any/c . -> . resource?)]
                  [resource-save! (resource? path-string? . -> . any)])
