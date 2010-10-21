;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/runtime/job.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 14 14:53:17 2005                          */
;*    Last change :  Thu Oct 21 19:20:26 2010 (serrano)                */
;*    Copyright   :  2005-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop JOB management                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_job

   (cond-expand
      (enable-threads (library pthread)))

   (import  __hop_param
	    __hop_read)

   (export  (class job
	       (%thread (default #unspecified))
	       (expression::pair-nil read-only)
	       (restore::pair-nil read-only)
	       (name::bstring read-only)
	       (state::symbol (default 'queue))
	       (data::obj (default #unspecified))
	       date::elong
	       (repeat::int (default 1))
	       (interval::elong read-only (default #e-1))))
	   
   (export  (job-start-scheduler!)
	    (jobs-queue::pair-nil)
	    (jobs-run::pair-nil)
	    (jobs-end::pair-nil)
	    (job-schedule! ::pair-nil ::pair-nil ::elong ::long ::elong)
	    (job-restore! ::pair-nil ::pair-nil ::elong ::long ::elong)
	    (job-reschedule! ::job . ::obj)
	    (job-abort! ::job)
	    (job-cancel! ::job)
	    (job-find ::bstring)))

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
(define *jobs-end* '())

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
	 (thread-start!	(make-thread job-scheduler 'job-scheduler))
	 (let ((f (make-file-name (hop-var-directory) (hop-job-file))))
	    (when (file-exists? f)
	       (hop-load f))))))

;*---------------------------------------------------------------------*/
;*    start-job! ...                                                   */
;*---------------------------------------------------------------------*/
(define (start-job! job)
   (with-access::job job (expression name state %thread date interval repeat)
      (let* ((proc (eval expression))
	     (t (make-thread (lambda () (proc job)) name)))
	 (set! %thread t)
	 (set! *jobs-run* (cons job *jobs-run*))
	 (when (pair? *jobs-queue*)
	    (jobs-dump))
	 (thread-cleanup-set! t (lambda (t)
				   (with-lock *job-mutex*
				      (lambda ()
					 (set! %thread #unspecified)
					 (set! *jobs-run*
					       (remq! job *jobs-run*))
					 (if (=fx repeat 0)
					     (begin
						(set! state 'end)
						(set! *jobs-end*
						      (cons job *jobs-end*)))
					     (set! state 'queue))))))
	 
	 (set! state 'run)
	 (thread-start! t)
	 (cond
	    ((<fx repeat 0)
	     (set! date (+elong interval date)) 
	     (job-add! job))
	    ((>fx repeat 1)
	     (set! repeat (-fx repeat 1))
	     (set! date (+elong interval date)) 
	     (job-add! job))
	    (else
	     (set! repeat 0)))
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
;*    jobs-end ...                                                     */
;*---------------------------------------------------------------------*/
(define (jobs-end)
   (with-lock *job-mutex*
      (lambda ()
	 *jobs-end*)))

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
	 ((<=elong (job-date (car *jobs-queue*))
		   (date->seconds (current-date)))
	  (start-job! (car *jobs-queue*))
	  (set! *jobs-queue* (cdr *jobs-queue*))
	  (loop))
	 (else
	  (let ((tmt (*fx 1000
			  (elong->fixnum
			   (-elong (job-date (car *jobs-queue*))
				   (date->seconds (current-date)))))))
	     (condition-variable-wait! *job-condvar* *job-mutex* tmt)
	     (loop))))))

;*---------------------------------------------------------------------*/
;*    job-add! ...                                                     */
;*---------------------------------------------------------------------*/
(define (job-add! j::job)
   (let ((d (job-date j)))
      (cond
	 ((null? *jobs-queue*)
	  (set! *jobs-queue* (list j))
	  (condition-variable-signal! *job-condvar*))
	 ((<elong d (job-date (car *jobs-queue*)))
	  (set! *jobs-queue* (cons d *jobs-queue*))
	  (condition-variable-signal! *job-condvar*))
	 (else
	  (let loop ((jobs (cdr *jobs-queue*))
		     (prev *jobs-queue*))
	     (cond
		((null? jobs)
		 (set-cdr! prev (cons j '())))
		((<elong d (job-date (car jobs)))
		 (set-cdr! prev (cons j jobs)))
		(else
		 (loop (cdr jobs) jobs))))))))

;*---------------------------------------------------------------------*/
;*    %job-schedule! ...                                               */
;*---------------------------------------------------------------------*/
(define (%job-schedule! expr restore start repeat interval)
   (let* ((now (date->seconds (current-date)))
	  (job (instantiate::job
		  (name (symbol->string (gensym 'job)))
		  (expression expr)
		  (restore restore)
		  (date (if (<elong start 0) now start))
		  (repeat repeat)
		  (interval interval))))
      (if (or (<elong start #e0) (>=elong start now))
	  (job-add! job)
	  (set! *jobs-end* (cons job *jobs-end*)))
      job))

;*---------------------------------------------------------------------*/
;*    job-schedule! ...                                                */
;*---------------------------------------------------------------------*/
(define (job-schedule! expr restore start repeat interval)
   (with-lock *job-mutex*
      (lambda ()
	 (%job-schedule! expr restore start repeat interval))))
;*                                                                     */
;*---------------------------------------------------------------------*/
;*    job-restore! ...                                                 */
;*---------------------------------------------------------------------*/
(define (job-restore! expr restore start repeat interval)
   ((eval restore))
   (%job-schedule! expr restore start repeat interval))

;*---------------------------------------------------------------------*/
;*    job-reschedule! ...                                              */
;*---------------------------------------------------------------------*/
(define (job-reschedule! j::job . start-time)
   (with-lock *job-mutex*
      (lambda ()
	 (unless (job-%thread j)
	    (with-access::job j (date)
	       (set! *jobs-queue* (remq! j *jobs-queue*))
	       (if (null? start-time)
		   (begin
		      (set! date (date->seconds (current-date)))
		      (start-job! j))
		   (begin
		      (set! date (date->seconds (car start-time)))
		      (job-add! j))))))))
   
;*---------------------------------------------------------------------*/
;*    job-abort! ...                                                   */
;*---------------------------------------------------------------------*/
(define (job-abort! j::job)
   (with-lock *job-mutex*
      (lambda ()
	 (when (thread? (job-%thread j))
	    (thread-terminate! (job-%thread j))
	    (set! *jobs-run* (remq! j *jobs-run*))))))

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
	    ((string=? (job-name (car jobs)) name)
	     (car jobs))
	    (else
	     (loop (cdr jobs))))))
   (with-lock *job-mutex*
      (lambda ()
	 (or (search *jobs-queue*)
	     (search *jobs-run*)
	     (let ((j (search *jobs-end*)))
		(when (job? j)
		   (set! *jobs-end* (remq! j *jobs-end*))
		   j))))))

;*---------------------------------------------------------------------*/
;*    jobs-dump ...                                                    */
;*---------------------------------------------------------------------*/
(define (jobs-dump)
   (define (job-dump j)
      (print `"(job-restore!")
      (display " '") (write (job-expression j)) (newline)
      (display " '") (write (job-restore j)) (newline)
      (display " ") (write (job-date j))
      (print " ;; " (seconds->date (job-date j)))
      (display " ") (write (job-repeat j))
      (display " ") (write (job-interval j)) (newline)
      (print " )"))
   (with-output-to-file (make-file-name (hop-var-directory) (hop-job-file))
      (lambda ()
	 (for-each job-dump *jobs-queue*))))
	    
