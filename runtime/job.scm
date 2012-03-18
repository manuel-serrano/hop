;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/runtime/job.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 14 14:53:17 2005                          */
;*    Last change :  Sun Mar 18 10:38:09 2012 (serrano)                */
;*    Copyright   :  2005-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop JOB management                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_job

   (cond-expand
      (enable-threads
       (library pthread))
      (else
       (export (class pthread::nothread))))

   (cond-expand
      ((not enable-threads)
       (eval (class pthread))))
   
   (import  __hop_param
	    __hop_read)

   (static  (class job
	       (%thread (default #unspecified))
	       (expression::obj read-only (default #f))
	       (proc::procedure read-only)
	       date::llong
	       (name::bstring read-only)
	       (repeat::int (default 1))
	       (interval::llong read-only (default #l-1))))
	   
   (export  (job-start-scheduler!)
	    (job-schedule! ::pair-nil ::llong ::long ::llong)
	    (job-restore! ::pair-nil ::llong ::long ::llong)
	    (after ::obj ::procedure)
	    (timeout ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    Scheduler control                                                */
;*---------------------------------------------------------------------*/
(define *job-mutex* (make-mutex "hop-job"))
(define *job-condvar* (make-condition-variable "hop-job"))

;*---------------------------------------------------------------------*/
;*    List of jobs and threads                                         */
;*---------------------------------------------------------------------*/
(define *jobs-queue* '())
(define *jobs-run* '())

;*---------------------------------------------------------------------*/
;*    job-start-scheduler! ...                                         */
;*---------------------------------------------------------------------*/
(define (job-start-scheduler!)
   (cond-expand
      (enable-threads
       (job-start-scheduler-inner!))))

;*---------------------------------------------------------------------*/
;*    job-start-scheduler-inner! ...                                   */
;*---------------------------------------------------------------------*/
(define (job-start-scheduler-inner!)
   (with-lock *job-mutex*
      (lambda ()
	 (thread-start!	(instantiate::pthread
			   (name "job-scheduler")
			   (body job-scheduler)))
	 (let ((f (make-file-name (hop-var-directory) (hop-job-file))))
	    (when (file-exists? f)
	       (hop-load f))))))

;*---------------------------------------------------------------------*/
;*    start-job! ...                                                   */
;*---------------------------------------------------------------------*/
(define (start-job! job)
   (with-access::job job (proc name %thread date interval repeat)
      (let ((t (instantiate::pthread
		  (name name)
		  (body (lambda ()
			   (let ((res (proc job)))
			      (set! %thread #unspecified)
			      (set! *jobs-run* (remq! job *jobs-run*))
			      (cond
				 ((<fx repeat 0)
				  (when res
				     (set! date (+llong interval (current-milliseconds)))
				     (job-add! job)))
				 ((>fx repeat 1)
				  (set! repeat (-fx repeat 1))
				  (set! date (+llong interval (current-milliseconds))) 
				  (job-add! job))
				 (else
				  (set! repeat 0)))))))))
	 (set! %thread t)
	 (set! *jobs-run* (cons job *jobs-run*))
	 (when (pair? *jobs-queue*)
	    (jobs-dump))
	 (thread-start! t)
	 job)))

;*---------------------------------------------------------------------*/
;*    jobs-queue ...                                                   */
;*---------------------------------------------------------------------*/
(define (jobs-queue)
   (with-lock *job-mutex*
      (lambda ()
	 *jobs-queue*)))

;*---------------------------------------------------------------------*/
;*    jobs-run ...                                                     */
;*---------------------------------------------------------------------*/
(define (jobs-run)
   (with-lock *job-mutex*
      (lambda ()
	 *jobs-run*)))

;*---------------------------------------------------------------------*/
;*    job-scheduler ...                                                */
;*---------------------------------------------------------------------*/
(define (job-scheduler)
   (mutex-lock! *job-mutex*)
   (let loop ()
      (jobs-dump)
      (cond
	 ((null? *jobs-queue*)
	  (condition-variable-wait! *job-condvar* *job-mutex*)
	  (loop))
	 ((<=llong (with-access::job (car *jobs-queue*) (date) date)
	     (current-milliseconds))
	  (let ((job (car *jobs-queue*)))
	     (set! *jobs-queue* (cdr *jobs-queue*))
	     (start-job! job))
	  (loop))
	 (else
	  (with-access::job (car *jobs-queue*) (date)
	     (let ((tmt (-llong date (current-milliseconds))))
		(condition-variable-wait! *job-condvar* *job-mutex* tmt)
		(loop)))))))

;*---------------------------------------------------------------------*/
;*    job-add! ...                                                     */
;*---------------------------------------------------------------------*/
(define (job-add! j::job)
   (let ((d (with-access::job j (date) date)))
      (cond
	 ((<=llong d (current-milliseconds))
	  (start-job! j))
	 ((null? *jobs-queue*)
	  (set! *jobs-queue* (list j))
	  (mutex-lock! *job-mutex*)
	  (condition-variable-signal! *job-condvar*)
	  (mutex-unlock! *job-mutex*))
	 ((<llong d (with-access::job (car *jobs-queue*) (date) date))
	  (set! *jobs-queue* (cons d *jobs-queue*))
	  (mutex-lock! *job-mutex*)
	  (condition-variable-signal! *job-condvar*)
	  (mutex-unlock! *job-mutex*))
	 (else
	  (let loop ((jobs (cdr *jobs-queue*))
		     (prev *jobs-queue*))
	     (cond
		((null? jobs)
		 (set-cdr! prev (cons j '())))
		((<llong d (with-access::job (car jobs) (date) date))
		 (set-cdr! prev (cons j jobs)))
		(else
		 (loop (cdr jobs) jobs)))))))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    %job-schedule! ...                                               */
;*---------------------------------------------------------------------*/
(define (%job-schedule! expr start repeat interval)
   (let* ((now (current-milliseconds))
	  (job (instantiate::job
		  (name (symbol->string (gensym 'job)))
		  (expression expr)
		  (proc (eval expr))
		  (date (if (<llong start 0) now start))
		  (repeat repeat)
		  (interval interval))))
      (when (or (<llong start #l0) (>=llong start now))
	 (job-add! job))
      job))

;*---------------------------------------------------------------------*/
;*    job-schedule! ...                                                */
;*---------------------------------------------------------------------*/
(define (job-schedule! expr start repeat interval)
   (with-lock *job-mutex*
      (lambda ()
	 (%job-schedule! expr start repeat interval))))

;*---------------------------------------------------------------------*/
;*    job-restore! ...                                                 */
;*---------------------------------------------------------------------*/
(define (job-restore! expr start repeat interval)
   (%job-schedule! expr start repeat interval))

;*---------------------------------------------------------------------*/
;*    job-abort! ...                                                   */
;*---------------------------------------------------------------------*/
(define (job-abort! j::job)
   (with-lock *job-mutex*
      (lambda ()
	 (with-access::job j (%thread)
	    (when (isa? %thread thread)
	       (thread-terminate! %thread)
	       (set! *jobs-run* (remq! j *jobs-run*)))))))

;*---------------------------------------------------------------------*/
;*    job-cancel! ...                                                  */
;*---------------------------------------------------------------------*/
(define (job-cancel! j::job)
   (with-lock *job-mutex*
      (lambda ()
	 (set! *jobs-queue* (remq! j *jobs-queue*))
	 (jobs-dump))))

;*---------------------------------------------------------------------*/
;*    job-find ...                                                     */
;*---------------------------------------------------------------------*/
(define (job-find name)
   (define (search l)
      (let loop ((jobs l))
	 (cond
	    ((null? jobs)
	     #f)
	    ((string=? (with-access::job (car jobs) (name) name) name)
	     (car jobs))
	    (else
	     (loop (cdr jobs))))))
   (with-lock *job-mutex*
      (lambda ()
	 (or (search *jobs-queue*)
	     (search *jobs-run*)))))

;*---------------------------------------------------------------------*/
;*    jobs-dump ...                                                    */
;*---------------------------------------------------------------------*/
(define (jobs-dump)
   
   (define (job-dump j)
      (with-access::job j (expression date repeat interval)
	 (when expression
	    (print `"(job-restore!")
	    (display " '") (write expression) (newline)
	    (display " ") (write date)
	    (print " ;; " (seconds->date (llong->elong (/llong date #l1000))))
	    (display " ") (write repeat)
	    (display " ") (write interval) (newline)
	    (print " )"))))
   
   (when (pair? *jobs-queue*)
      (unless (directory? (hop-var-directory))
	 (make-directories (hop-var-directory)))
      (when (directory? (hop-var-directory))
	 (let ((path (make-file-name (hop-var-directory) (hop-job-file))))
	    (with-output-to-file path
	       (lambda ()
		  (for-each job-dump *jobs-queue*)))))))

;*---------------------------------------------------------------------*/
;*    current-milliseconds ...                                         */
;*---------------------------------------------------------------------*/
(define (current-milliseconds::llong)
   (/llong (current-microseconds) #l1000))
   
;*---------------------------------------------------------------------*/
;*    ->llong ...                                                      */
;*---------------------------------------------------------------------*/
(define (->llong who n)
   (cond
      ((llong? n) n)
      ((integer? n) (fixnum->llong n))
      ((elong? n) (elong->llong n))
      ((real? n) (fixnum->llong (flonum->fixnum n)))
      (else (bigloo-type-error who "number" n))))

;*---------------------------------------------------------------------*/
;*    after ...                                                        */
;*---------------------------------------------------------------------*/
(define (after tmt proc)
   (let* ((i (->llong "after" tmt))
	  (job (instantiate::job
		 (name "after")
		 (proc (lambda (j) (proc)))
		 (date (+llong (current-milliseconds) i))
		 (repeat 0))))
      (job-add! job)))

;*---------------------------------------------------------------------*/
;*    timeout ...                                                      */
;*---------------------------------------------------------------------*/
(define (timeout tmt proc)
   (when (proc)
      (let* ((i (->llong "timeout" tmt))
	     (job (instantiate::job
		     (name "timeout")
		     (proc (lambda (j) (proc)))
		     (date (+llong (current-milliseconds) i))
		     (interval i)
		     (repeat -1))))
	 (job-add! job))))
