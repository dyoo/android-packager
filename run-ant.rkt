#lang racket/base

(require racket/contract
         racket/port
         "config.rkt")

(provide run-ant-build.xml)

;; run-ant-build.xml: path string -> void
;; Runs ant to build the program in the destination directory.
;; Assumes the build file is called "build.xml" at the top of the directory.
;;
;; Should be equivalent to:
;;
;;    $ cd [dest-dir]
;;    $ ant [target]
;;
(define (run-ant-build.xml dest-dir target)
  (parameterize ([current-directory dest-dir])
    (let*-values ([(string-output-port string-error-port)
                   (values (open-output-string "")
                           (open-output-string ""))]
                  [(a-subprocess inp outp errp)
                   (subprocess #f #f #f (current-ant-bin-path) target)]
                  [(t1 t2) 
                   (values (thread (lambda () 
                                     (copy-port-to-debug-log (peeking-input-port inp))
                                     (copy-port inp string-output-port)))
                           (thread (lambda ()
                                     (copy-port-to-error-log (peeking-input-port errp))
                                     (copy-port errp string-error-port))))])
      (close-output-port outp)
      (subprocess-wait a-subprocess)
      (sync t1)
      (sync t2)
      (unless (= 0 (subprocess-status a-subprocess))
        (error 'ant "Internal error while running ant: ~a\n\n~a"
               (get-output-string string-output-port)
               (get-output-string string-error-port)))
      (void))))




;; copy-port-to-debug-log: input-port -> void
;; Writes out the lines of the input port as debug events.
(define (copy-port-to-debug-log inp)
  (let loop ([line (read-line inp)])
    (unless (eof-object? line)
      (log-debug line)
      (loop (read-line inp)))))


;; copy-port-to-error-log: input-port -> void
;; Writes out the lines of the input port as debug events.
(define (copy-port-to-error-log inp)
  (let loop ([line (read-line inp)])
    (unless (eof-object? line)
      (log-error line)
      (loop (read-line inp)))))