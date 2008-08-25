;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/src/queue-scheduler.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 22 14:29:19 2008                          */
;*    Last change :  Fri Aug 22 14:33:42 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    QUEUE scheduler                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_scheduler-queue

   (cond-expand
      (enable-threads
       (library pthread)))

   (library hop)
   
   (import  hop_scheduler
	    hop_param)

   (export  (class queue-scheduler::scheduler
	       (mutex::mutex read-only (default (make-mutex)))
	       (nfree::int (default 0))
	       (head::pair-nil (default '()))
	       (tail::pair-nil (default '()))
	       (free::pair-nil (default '()))
	       (flist::pair-nil (default '())))))

;*---------------------------------------------------------------------*/
;*    scheduler-init! ::queue-scheduler ...                            */
;*---------------------------------------------------------------------*/
(define-method (scheduler-init! scd::queue-scheduler)
   (with-access::queue-scheduler scd (size free nfree tail head)
      (set! free (map! (lambda (x) (make-queue-thread scd)) (make-list size)))
      (set! nfree size)
      scd))
	 
;*---------------------------------------------------------------------*/
;*    scheduler-stat ::queue-scheduler ...                             */
;*---------------------------------------------------------------------*/
(define-method (scheduler-stat scd::queue-scheduler)
   (with-access::queue-scheduler scd (size nfree)
      (format " (~a/~a)" (-fx size nfree) size)))

;*---------------------------------------------------------------------*/
;*    scheduler-load ::queue-scheduler ...                             */
;*---------------------------------------------------------------------*/
(define-method (scheduler-load scd::queue-scheduler)
   (with-access::queue-scheduler scd (nfree size)
      (flonum->fixnum
       (*fl 100.
	    (/fl (fixnum->flonum (-fx size nfree)) (fixnum->flonum size))))))

;*---------------------------------------------------------------------*/
;*    spawn ::queue-scheduler ...                                      */
;*---------------------------------------------------------------------*/
(define-method (spawn scd::queue-scheduler proc . args)
   (with-access::queue-scheduler scd (mutex)
      (mutex-lock! mutex)
      (let ((t (get-thread! scd)))
	 (if (thread? t)
	     ;; there is an available thread, we use it...
	     (begin
		(mutex-unlock! mutex)
		(thread-spawn t (lambda (s t) (apply proc s t args))))
	     ;; everybody is busy, we have to push our task in the queue...
	     (begin
		(queue-push! scd (lambda (s t) (apply proc s t args)))
		(mutex-unlock! mutex))))))

;*---------------------------------------------------------------------*/
;*    spawn0 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (spawn0 scd::queue-scheduler proc)
   (with-access::queue-scheduler scd (mutex)
      (mutex-lock! mutex)
      (let ((t (get-thread! scd)))
	 (if (thread? t)
	     ;; there is an available thread, we use it...
	     (begin
		(mutex-unlock! mutex)
		(thread-spawn t proc))
	     ;; everybody is busy, we have to push our task in the queue...
	     (begin
		(queue-push! scd proc)
		(mutex-unlock! mutex))))))

;*---------------------------------------------------------------------*/
;*    spawn4 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (spawn4 scd::queue-scheduler proc a0 a1 a2 a3)
   (with-access::queue-scheduler scd (mutex)
      (mutex-lock! mutex)
      (let ((t (get-thread! scd)))
	 (if (thread? t)
	     ;; there is an available thread, we use it...
	     (begin
		(mutex-unlock! mutex)
		(thread-spawn t (lambda (s t) (proc s t a0 a1 a2 a3))))
	     ;; everybody is busy, we have to push our task in the queue...
	     (begin
		(queue-push! scd (lambda (s t) (proc s t a0 a1 a2 a3)))
		(mutex-unlock! mutex))))))

;*---------------------------------------------------------------------*/
;*    thread-spawn ...                                                 */
;*---------------------------------------------------------------------*/
(define (thread-spawn th p)
   (with-access::hopthread th (condv mutex proc)
      (mutex-lock! mutex)
      (set! proc p)
      (condition-variable-signal! condv)
      (mutex-unlock! mutex)))

;*---------------------------------------------------------------------*/
;*    stage ::queue-scheduler ...                                      */
;*---------------------------------------------------------------------*/
(define-method (stage scd::queue-scheduler proc . args)
   (with-access::queue-scheduler scd (mutex head)
      (mutex-lock! mutex)
      (if (null? head)
	  ;; the queue is empty, we can keep going with the same thread
	  (begin
	     (mutex-unlock! mutex)
	     (apply proc scd (current-thread) args))
	  ;; the queue is filled, we have to push our task
	  (let ((nproc (queue-pop! scd)))
	     (queue-push! scd (lambda (s t) (apply proc s t args)))
	     (mutex-unlock! mutex)
	     (nproc scd (current-thread))))))

;*---------------------------------------------------------------------*/
;*    stage0 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage0 scd::queue-scheduler proc)
   (with-access::queue-scheduler scd (mutex head)
      (mutex-lock! mutex)
      (if (null? head)
	  ;; the queue is empty, we can keep going with the same thread
	  (begin
	     (mutex-unlock! mutex)
	     (proc scd (current-thread)))
	  ;; the queue is filled, we have to push our task
	  (let ((nproc (queue-pop! scd)))
	     (queue-push! scd proc)
	     (mutex-unlock! mutex)
	     (nproc scd (current-thread))))))

;*---------------------------------------------------------------------*/
;*    stage1 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage1 scd::queue-scheduler proc a0)
   (with-access::queue-scheduler scd (mutex head)
      (mutex-lock! mutex)
      (if (null? head)
	  ;; the queue is empty, we can keep going with the same thread
	  (begin
	     (mutex-unlock! mutex)
	     (proc scd (current-thread) a0))
	  ;; the queue is filled, we have to push our task
	  (let ((nproc (queue-pop! scd)))
	     (queue-push! scd (lambda (s t) (proc s t a0)))
	     (mutex-unlock! mutex)
	     (nproc scd (current-thread))))))

;*---------------------------------------------------------------------*/
;*    stage2 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage2 scd::queue-scheduler proc a0 a1)
   (with-access::queue-scheduler scd (mutex head)
      (mutex-lock! mutex)
      (if (null? head)
	  ;; the queue is empty, we can keep going with the same thread
	  (begin
	     (mutex-unlock! mutex)
	     (proc scd (current-thread) a0 a1))
	  ;; the queue is filled, we have to push our task
	  (let ((nproc (queue-pop! scd)))
	     (queue-push! scd (lambda (s t) (proc s t a0 a1)))
	     (mutex-unlock! mutex)
	     (nproc scd (current-thread))))))

;*---------------------------------------------------------------------*/
;*    stage3 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage3 scd::queue-scheduler proc a0 a1 a2)
   (with-access::queue-scheduler scd (mutex head)
      (mutex-lock! mutex)
      (if (null? head)
	  ;; the queue is empty, we can keep going with the same thread
	  (begin
	     (mutex-unlock! mutex)
	     (proc scd (current-thread) a0 a1 a2))
	  ;; the queue is filled, we have to push our task
	  (let ((nproc (queue-pop! scd)))
	     (queue-push! scd (lambda (s t) (proc s t a0 a1 a2)))
	     (mutex-unlock! mutex)
	     (nproc scd (current-thread))))))

;*---------------------------------------------------------------------*/
;*    stage4 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage4 scd::queue-scheduler proc a0 a1 a2 a3)
   (with-access::queue-scheduler scd (mutex head)
      (mutex-lock! mutex)
      (if (null? head)
	  ;; the queue is empty, we can keep going with the same thread
	  (begin
	     (mutex-unlock! mutex)
	     (proc scd (current-thread) a0 a1 a2 a3))
	  ;; the queue is filled, we have to push our task
	  (let ((nproc (queue-pop! scd)))
	     (queue-push! scd (lambda (s t) (proc s t a0 a1 a2 a3)))
	     (mutex-unlock! mutex)
	     (nproc scd (current-thread))))))

;*---------------------------------------------------------------------*/
;*    get-thread! ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function assumes that the scheduler mutex is acquired.      */
;*    -------------------------------------------------------------    */
;*    !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!!  */
;*    It is import that put-thread! and get-thread! avoiding           */
;*    CONSing because this could raise a collection which could        */
;*    introduce latency and prevent the scheduler from re-allocating   */
;*    the same thread. This is the purpose of the FLIST variable.      */
;*---------------------------------------------------------------------*/
(define (get-thread! scd::queue-scheduler)
   (with-access::queue-scheduler scd (free nfree flist)
      (when (pair? free)
	 (let ((t (car free))
	       (fl free))
	    (set! free (cdr free))
	    (set-cdr! fl flist)
	    (set! flist fl)
	    (set! nfree (-fx nfree 1))
	    t))))

;*---------------------------------------------------------------------*/
;*    put-thread! ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function assumes the schduler's lock acquired.              */
;*---------------------------------------------------------------------*/
(define (put-thread! scd thread)
   (with-access::queue-scheduler scd (mutex mutex free nfree flist)
      [assert () (not (symbol? (mutex-state mutex)))]
      (let ((nfree flist))
	 (set! flist (cdr flist))
	 (set-car! nfree thread)
	 (set-cdr! nfree free)
	 (set! free nfree))
      (set! nfree (+fx nfree 1))))

;*---------------------------------------------------------------------*/
;*    queue-thread-body ...                                            */
;*---------------------------------------------------------------------*/
(define (queue-thread-body t)
   (with-handler
      (make-scheduler-error-handler t)
      (let* ((scd (hopthread-scheduler t))
	     (mutex (hopthread-mutex t))
	     (condv (hopthread-condv t))
	     (smutex (queue-scheduler-mutex scd)))
	 (mutex-lock! mutex)
	 (let loop ()
	    (condition-variable-wait! condv mutex)
	    (let liip ((proc (hopthread-proc t)))
	       ;; execute the user task
	       (proc scd t)
	       ;; check if there is a new task
	       (mutex-lock! smutex)
	       (let ((nproc (queue-pop! scd)))
		  (if nproc
		      ;; there is a waiting task, execute it
		      (begin
			 (mutex-unlock! smutex)
			 (liip nproc))
		      ;; nothing to do, go to sleep waiting for a new task
		      (begin
			 (put-thread! scd t)
			 (mutex-unlock! smutex)
			 (loop)))))))))
   
;*---------------------------------------------------------------------*/
;*    make-queue-thread ...                                            */
;*---------------------------------------------------------------------*/
(define (make-queue-thread scd)
   (letrec ((t (instantiate::hopthread
		  (name (gensym 'queue-scheduler))
		  (scheduler scd)
		  (body (lambda () (queue-thread-body t))))))
      (thread-start! t)
      t))
		   
;*---------------------------------------------------------------------*/
;*    queue-push! ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function assumes that no thread is available.               */
;*---------------------------------------------------------------------*/
(define (queue-push! scd::queue-scheduler entry)
   (with-access::queue-scheduler scd (head tail)
      (if (null? head)
	  (begin
	     (set! head (cons entry '()))
	     (set! tail head))
	  (begin
	     (set-cdr! tail (cons entry '()))
	     (set! tail (cdr tail))))))

;*---------------------------------------------------------------------*/
;*    queue-pop! ...                                                   */
;*---------------------------------------------------------------------*/
(define (queue-pop! scd::queue-scheduler)
   (with-access::queue-scheduler scd (head tail)
      (when (pair? head)
	 (let ((task (car head)))
	    (when (eq? head tail) (set! tail '()))
	    (set! head (cdr head))
	    task))))
