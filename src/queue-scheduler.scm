;*=====================================================================*/
;*    serrano/prgm/project/hop/1.10.x/src/queue-scheduler.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 22 14:29:19 2008                          */
;*    Last change :  Fri Nov 21 19:42:17 2008 (serrano)                */
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
   (with-access::queue-scheduler scd (size free nfree head tail)
      (set! free (map! (lambda (x) (make-queue-thread scd)) (make-list size)))
      (set! nfree size)
      (set! head '())
      (set! tail '())
      scd))
	 
;*---------------------------------------------------------------------*/
;*    scheduler-stat ::queue-scheduler ...                             */
;*---------------------------------------------------------------------*/
(define-method (scheduler-stat scd::queue-scheduler)
   (with-access::queue-scheduler scd (size nfree head)
      (format " (~a/~a, queue=~a)" (-fx size nfree) size (length head))))

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
	     (if (queue-empty? scd)
		 (begin
		    (mutex-unlock! mutex)
		    ;; there is an available thread, we use it...
		    (thread-spawn t scd (lambda (s t) (apply proc s t args))))
		 (let* ((nproc (lambda (s t) (apply proc s t args)))
			(oproc (queue-pop-and-push! scd nproc)))
		    (mutex-unlock! mutex)
		    (thread-spawn t scd oproc)))
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
		(thread-spawn scd t proc))
	     ;; everybody is busy, we have to push our task in the queue...
	     (begin
		(queue-push! scd proc)
		(mutex-unlock! mutex))))))

;*---------------------------------------------------------------------*/
;*    spawn1 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (spawn1 scd::queue-scheduler proc a0)
   (with-access::queue-scheduler scd (mutex)
      (mutex-lock! mutex)
      (let ((t (get-thread! scd)))
	 (if (thread? t)
	     ;; there is an available thread, we use it...
	     (begin
		(mutex-unlock! mutex)
		(thread-spawn t scd (lambda (s t) (proc s t a0))))
	     ;; everybody is busy, we have to push our task in the queue...
	     (begin
		(queue-push! scd (lambda (s t) (proc s t a0)))
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
		(thread-spawn t scd (lambda (s t) (proc s t a0 a1 a2 a3))))
	     ;; everybody is busy, we have to push our task in the queue...
	     (begin
		(queue-push! scd (lambda (s t) (proc s t a0 a1 a2 a3)))
		(mutex-unlock! mutex))))))

;*---------------------------------------------------------------------*/
;*    thread-spawn ...                                                 */
;*---------------------------------------------------------------------*/
(define (thread-spawn th scd p)
   (with-access::hopthread th (condv mutex onerror proc)
      (mutex-lock! mutex)
      (set! onerror #f)
      (set! proc p)
      (condition-variable-signal! condv)
      (mutex-unlock! mutex)
      th))

;*---------------------------------------------------------------------*/
;*    stage ::queue-scheduler ...                                      */
;*---------------------------------------------------------------------*/
(define-method (stage scd::queue-scheduler thread proc . args)
   (with-access::queue-scheduler scd (mutex)
      (mutex-lock! mutex)
      (if (queue-empty? scd)
	  ;; the queue is empty, we can keep going with the same thread
	  (begin
	     (mutex-unlock! mutex)
	     (apply proc scd thread args))
	  ;; the queue is filled, we have to push our task
	  (let* ((proc (lambda (s t) (apply proc s t args)))
		 (nproc (queue-pop-and-push! scd proc)))
	     (mutex-unlock! mutex)
	     (nproc scd thread)))))

;*---------------------------------------------------------------------*/
;*    stage0 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage0 scd::queue-scheduler thread proc)
   (with-access::queue-scheduler scd (mutex)
      (mutex-lock! mutex)
      (if (queue-empty? scd)
	  ;; the queue is empty, we can keep going with the same thread
	  (begin
	     (mutex-unlock! mutex)
	     (proc scd thread))
	  ;; the queue is filled, we have to push our task
	  (let ((nproc (queue-pop-and-push! scd proc)))
	     (mutex-unlock! mutex)
	     (nproc scd thread)))))

;*---------------------------------------------------------------------*/
;*    stage1 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage1 scd::queue-scheduler thread proc a0)
   (with-access::queue-scheduler scd (mutex)
      (mutex-lock! mutex)
      (if (queue-empty? scd)
	  ;; the queue is empty, we can keep going with the same thread
	  (begin
	     (mutex-unlock! mutex)
	     (proc scd thread a0))
	  ;; the queue is filled, we have to push our task
	  (let* ((proc (lambda (s t) (proc s t a0)))
		 (nproc (queue-pop-and-push! scd proc)))
	     (mutex-unlock! mutex)
	     (nproc scd thread)))))

;*---------------------------------------------------------------------*/
;*    stage2 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage2 scd::queue-scheduler thread proc a0 a1)
   (with-access::queue-scheduler scd (mutex)
      (mutex-lock! mutex)
      (if (queue-empty? scd)
	  ;; the queue is empty, we can keep going with the same thread
	  (begin
	     (mutex-unlock! mutex)
	     (proc scd thread a0 a1))
	  ;; the queue is filled, we have to push our task
	  (let* ((proc (lambda (s t) (proc s t a0 a1)))
		 (nproc (queue-pop-and-push! scd proc)))
	     (mutex-unlock! mutex)
	     (nproc scd thread)))))

;*---------------------------------------------------------------------*/
;*    stage3 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage3 scd::queue-scheduler thread proc a0 a1 a2)
   (with-access::queue-scheduler scd (mutex)
      (mutex-lock! mutex)
      (if (queue-empty? scd)
	  ;; the queue is empty, we can keep going with the same thread
	  (begin
	     (mutex-unlock! mutex)
	     (proc scd thread a0 a1 a2))
	  ;; the queue is filled, we have to push our task
	  (let* ((proc (lambda (s t) (proc s t a0 a1 a2)))
		 (nproc (queue-pop-and-push! scd proc)))
	     (mutex-unlock! mutex)
	     (nproc scd thread)))))

;*---------------------------------------------------------------------*/
;*    stage4 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage4 scd::queue-scheduler thread proc a0 a1 a2 a3)
   (with-access::queue-scheduler scd (mutex)
      (mutex-lock! mutex)
      (if (queue-empty? scd)
	  ;; the queue is empty, we can keep going with the same thread
	  (begin
	     (mutex-unlock! mutex)
	     (proc scd thread a0 a1 a2 a3))
	  ;; the queue is filled, we have to push our task
	  (let* ((proc (lambda (s t) (proc s t a0 a1 a2 a3)))
		 (nproc (queue-pop-and-push! scd proc)))
	     (mutex-unlock! mutex)
	     (nproc scd thread)))))

;*---------------------------------------------------------------------*/
;*    get-thread! ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function assumes that the scheduler mutex is acquired.      */
;*    -------------------------------------------------------------    */
;*    !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!!  */
;*    -------------------------------------------------------------    */
;*    It is import that put-thread! and get-thread! avoiding           */
;*    CONSing because this could raise a collection which could        */
;*    introduce latency and prevent the scheduler from re-allocating   */
;*    the same thread. This is the purpose of the FLIST variable.      */
;*---------------------------------------------------------------------*/
(define (get-thread! scd::queue-scheduler)
   ;; this function assumes that scheduler mutex is already locked
   (with-access::queue-scheduler scd (free nfree flist )
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
;*---------------------------------------------------------------------*/
(define (put-thread! scd thread)
   ;; this function assumes that scheduler mutex is already locked
   (with-access::queue-scheduler scd (free nfree flist)
      (let ((nfree flist))
	 (set! flist (cdr flist))
	 (set-car! nfree thread)
	 (set-cdr! nfree free)
	 (set! free nfree))
      (set! nfree (+fx nfree 1))))

;*---------------------------------------------------------------------*/
;*    queue-thread-body ...                                            */
;*---------------------------------------------------------------------*/
(define (queue-thread-body scd t)
   (with-access::queue-scheduler scd (mutex)
      (let loop ()
	 (with-handler
	    (lambda (e)
	       (scheduler-error-handler e t))
	    (let ((tmutex (hopthread-mutex t))
		  (tcondv (hopthread-condv t)))
	       (define (loop)
		  (condition-variable-wait! tcondv tmutex)
		  (let liip ((proc (hopthread-proc t)))
		     ;; execute the user task
		     (proc scd t)
		     ;; check if there is a new task
		     (begin
			(mutex-lock! mutex)
			(let ((nproc (queue-pop! scd)))
			   (if nproc
			       ;; there is a waiting task, execute it
			       (begin
				  (mutex-unlock! mutex)
				  (liip nproc))
			       ;; nothing to do, go to sleep waiting for a task
			       (begin
				  (put-thread! scd t)
				  (mutex-unlock! mutex)
				  (loop)))))))
	       (with-lock tmutex loop)))
	 (mutex-lock! mutex)
	 (put-thread! scd t)
	 (mutex-unlock! mutex)
	 (loop))))
   
;*---------------------------------------------------------------------*/
;*    make-queue-thread ...                                            */
;*---------------------------------------------------------------------*/
(define (make-queue-thread scd)
   (letrec ((t (instantiate::hopthread
		  (name (gensym 'queue-scheduler))
		  (scheduler scd)
		  (body (lambda () (queue-thread-body scd t))))))
      (thread-start-joinable! t)
      t))

;*---------------------------------------------------------------------*/
;*    queue-empty? ...                                                 */
;*---------------------------------------------------------------------*/
(define (queue-empty? scd)
   ;; this function assumes that scheduler mutex is already locked
   (with-access::queue-scheduler scd (head)
      (null? head)))

;*---------------------------------------------------------------------*/
;*    queue-push! ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function assumes that no thread is available.               */
;*---------------------------------------------------------------------*/
(define (queue-push! scd::queue-scheduler entry)
   ;; this function assumes that scheduler mutex is already locked
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
   ;; this function assumes that scheduler mutex is already locked
   (with-access::queue-scheduler scd (head tail)
      (when (pair? head)
	 (let ((task (car head)))
	    (when (eq? head tail) (set! tail '()))
	    (set! head (cdr head))
	    task))))

;*---------------------------------------------------------------------*/
;*    queue-pop-and-push! ...                                          */
;*---------------------------------------------------------------------*/
(define (queue-pop-and-push! scd::queue-scheduler proc)
   (let ((nproc (queue-pop! scd)))
      (queue-push! scd proc)
      nproc))
