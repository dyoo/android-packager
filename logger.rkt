#lang scheme/base

(require scheme/contract
         scheme/path
         "utils.rkt")

;; Logger module; record programs that have been sent to us.

(define-struct logger (sema logfile-path))

;; -make-logger: path-string -> logger
;; Creates a new logger.
(define (-make-logger logfile-path)
  (make-logger (make-semaphore 1)
               (normalize-path logfile-path)))

;; sexp->compressed-bytes: sexp -> bytes
(define (sexp->compressed-bytes an-sexp)
  (let ([op (open-output-bytes)])
    (write an-sexp op)
    (gzip-bytes (get-output-bytes op))))


;; logger-add!: logger string program-resources any -> void
;; Write a new entry into the log.
(define (logger-add! a-logger client-ip request-content other-attributes)
  (call-with-semaphore 
   (logger-sema a-logger)
   (lambda ()
     (call-with-output-file (logger-logfile-path a-logger)
       (lambda (op)
         (write `((time ,(current-inexact-milliseconds))
                  (client-ip ,client-ip)
                  (compressed-program/resources-sexp 
                   ,(sexp->compressed-bytes 
                     request-content))
                  (other-attributes ,other-attributes))
                op)
         (newline op))
       #:exists 'append))))


(provide/contract
 [logger? (any/c . -> . boolean?)]
 [rename -make-logger make-logger (path-string? . -> . logger?)]
 [logger-add! (logger? string? bytes? any/c . -> . any)])