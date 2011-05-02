#lang racket/base
(require racket/runtime-path
         (for-syntax racket/base))

(provide (struct-out job)
         new-job
         get-job
         get-pending-jobs
         mark-job-done!)


(require (planet jaymccarthy/sqlite))
(define-runtime-path jobsdb (build-path "jobs.sqlite"))

(define (call-with-db f)
  (let ([db #f])
    (dynamic-wind (lambda () (set! db (open jobsdb)))
                  (lambda () (f db))
                  (lambda () (close db)))))



(define (create-database)
  (with-handlers ((void (lambda (exn)
                               (void))))
    (call-with-db
     (lambda (db)
        (exec/ignore db "create table job (id integer primary key, is_done boolean not null, val blob not null, created date not null default CURRENT_DATE);")))))

(create-database)


(define-struct job (id done? val) 
  #:transparent
  #:mutable)


(define (new-job val)
  (call-with-db
   (lambda (db)
     (define stmt (prepare db "insert into job(is_done, val) values(0, ?)"))
     (run stmt val)
     (let ([id (last-insert-rowid db)])
       (finalize stmt)
       (make-job id #f val)))))


(define (get-pending-jobs)
  (call-with-db
   (lambda (db)
     (define stmt (prepare db "select id, val from job where is_done=0"))
     (let ([results (map (lambda (v)
                           (make-job (vector-ref v 0)
                                     #f
                                     (vector-ref v 1)))
                         (step* stmt))])
       (finalize stmt)
       results))))

(define (get-job id)
  (call-with-db
   (lambda (db)
     (define stmt (prepare db "select id, is_done, val from job where id=?"))
     (load-params stmt id)
     (let* ([v (step stmt)]
            [job  (make-job (vector-ref v 0)
                            (vector-ref v 1)
                            (vector-ref v 2))])
       (finalize stmt)
       job))))
     
  

(define (mark-job-done! job)
  (call-with-db
   (lambda (db)
     (define stmt (prepare db "update job set is_done=1 where id=?"))
     (run stmt (job-id job))
     (set-job-done?! job #t)
     (finalize stmt)
     (void))))