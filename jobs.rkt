#lang racket/base
(require racket/runtime-path
         (for-syntax racket/base))

(provide (except-out (struct-out job) set-job-result!)
         (rename-out (-set-job-result! set-job-result!))
	 new-job	 
         get-job
         get-pending-jobs
         mark-job-done!
	 mark-job-start!
	 mark-job-end!)


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
        (exec/ignore db #<<EOF
create table job (id integer primary key, 
                  is_done boolean not null,
                  val blob not null,
                  result blob,
                  created datetime not null default CURRENT_TIMESTAMP,
                  start_time datetime,
	          end_time datetime);
EOF
)))))

(create-database)


(define-struct job (id done? val result)
  #:transparent
  #:mutable)


(define (new-job val)
  (call-with-db
   (lambda (db)
     (define stmt (prepare db "insert into job(is_done, val) values(0, ?)"))
     (run stmt val)
     (let ([id (last-insert-rowid db)])
       (finalize stmt)
       (make-job id #f val #f)))))


(define (get-pending-jobs)
  (call-with-db
   (lambda (db)
     (define stmt (prepare db "select id, val, result from job where is_done=0"))
     (let ([results (map (lambda (v)
                           (make-job (vector-ref v 0)
                                     #f
                                     (vector-ref v 1)
				     (vector-ref v 2)))
                         (step* stmt))])
       (finalize stmt)
       results))))

(define (get-job id)
  (call-with-db
   (lambda (db)
     (define stmt (prepare db "select id, is_done, val, result from job where id=?"))
     (load-params stmt id)
     (let* ([v (step stmt)]
            [job  (make-job (vector-ref v 0)
                            (vector-ref v 1)
                            (vector-ref v 2)
			    (vector-ref v 3))])
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


(define (mark-job-start! job)
  (call-with-db
   (lambda (db)
     (define stmt (prepare db "update job set start_time=CURRENT_TIMESTAMP where id=?"))
     (run stmt (job-id job))
     (finalize stmt)
     (void))))


(define (mark-job-end! job)
  (call-with-db
   (lambda (db)
     (define stmt (prepare db "update job set end_time=CURRENT_TIMESTAMP where id=?"))
     (run stmt (job-id job))
     (finalize stmt)
     (void))))



(define (-set-job-result! job result)
  (call-with-db
   (lambda (db)
     (define stmt (prepare db "update job set result=? where id=?"))
     (run stmt result (job-id job))
     (set-job-result! job result)
     (finalize stmt)
     (void))))