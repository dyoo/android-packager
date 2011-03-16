#lang racket/base

(require racket/class
         racket/contract
         racket/match)

;; A resource is something that should be associated to a program, such
;; as source code, HTML, images, and music files.


(define resource<%>
  (interface () 
    save!      ;; path -> void    
    get-name   ;; -> string
    get-bytes  ;; -> bytes
    ))

(define named-bytes-resource%
  (class* object% (resource<%>)
    (super-new)
    (init-field name)
    (init-field bytes)
    
    (define/public (save! a-path)
      (call-with-output-file (build-path a-path name)
        (lambda (op)
          (write-bytes bytes op))))
    
    (define/public (get-name)
      name)
    
    (define/public (get-bytes)
      bytes)))


;; resource->sexp: resource -> s-expression
(define (resource->sexp a-resource)
  (list 'resource
        (send a-resource get-name)
        (send a-resource get-bytes)))


;; sexp->resource: s-expression -> resource
(define (sexp->resource resource-sexp)
  (match resource-sexp
    [(list 'resource 
           (and name (? string?))
           (and bytes (? bytes?)))
     (new named-bytes-resource% 
       [name name]
       [bytes bytes])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide/contract [resource<%> interface?]
                  [named-bytes-resource% class?]
                  [resource->sexp ((is-a?/c resource<%>) . -> . any)]
                  [sexp->resource (any/c . -> . (is-a?/c resource<%>))])
