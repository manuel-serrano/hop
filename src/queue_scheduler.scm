;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/src/queue_scheduler.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 22 14:29:19 2008                          */
;*    Last change :  Mon Apr 21 07:40:10 2014 (serrano)                */
;*    Copyright   :  2008-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    QUEUE scheduler                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_scheduler-queue

   (library hop)
   
   (import  hop_scheduler
	    hop_param)

   (export  (class queue-scheduler::scheduler
	       (mutex::mutex read-only (default (make-mutex)))
	       (condv::condvar read-only (default (make-condition-variable)))
	       (nfree::int (default 0))
	       (head::pair-nil (default '()))
	       (qlength::int (default 0))
	       (max-qlength::int (default 50))
	       (tail::pair-nil (default '()))
	       (free::pair-nil (default '())))))

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
   (with-access::queue-scheduler scd (size nfree head qlength)
      (format " (~a/~a, queue=~a)" (-fx size nfree) size qlength)))

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
      (let* ((t #f)
	     (proc (synchronize mutex
		      (set! t (get-thread! scd))
		      (if (isa? t thread)
			  (if (queue-empty? scd)
			      (lambda (s t) (apply proc s t args))
			      (let ((nproc (lambda (s t) (apply proc s t args))))
				 (queue-pop-and-push! scd nproc)))
			  ;; everybody is busy, we have to push
			  ;; our task in the queue...
			  (begin
			     (queue-push! scd
				(lambda (s t) (apply proc s t args)))
			     #f)))))
	 (when proc
	    (thread-spawn t scd proc)))))

;*---------------------------------------------------------------------*/
;*    spawn0 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (spawn0 scd::queue-scheduler proc)
   (with-access::queue-scheduler scd (mutex)
      (let* ((t #f)
	     (proc (synchronize mutex
		      (set! t (get-thread! scd))
		      (if (isa? t thread)
			  ;; there is an available thread, we use it...
			  proc
			  ;; everybody is busy, we have to push
			  ;; our task in the queue...
			  (begin
			     (queue-push! scd proc)
			     #f)))))
	 (when proc
	    (thread-spawn t scd proc)))))

;*---------------------------------------------------------------------*/
;*    spawn1 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (spawn1 scd::queue-scheduler proc a0)
   (with-access::queue-scheduler scd (mutex)
      (let* ((t #f)
	     (proc (synchronize mutex
		      (set! t (get-thread! scd))
		      (if (isa? t thread)
			  ;; there is an available thread, we use it...
			  proc
			  ;; everybody is busy, we have to push
			  ;; our task in the queue...
			  (begin
			     (queue-push! scd (lambda (s t) (proc s t a0)))
			     #f)))))
	 (when proc
	    (thread-spawn t scd (lambda (s t) (proc s t a0)))))))

;*---------------------------------------------------------------------*/
;*    spawn4 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (spawn4 scd::queue-scheduler proc a0 a1 a2 a3)
   (with-access::queue-scheduler scd (mutex)
      (let* ((t #f)
	     (proc (synchronize mutex
		      (set! t (get-thread! scd))
		      (if (isa? t thread)
			  ;; there is an available thread, we use it...
			  proc
			  ;; everybody is busy, we have to push
			  ;; our task in the queue...
			  (begin
			     (queue-push! scd (lambda (s t) (proc s t a0 a1 a2 a3)))
			     #f)))))
	 (when proc
	    (thread-spawn t scd (lambda (s t) (proc s t a0 a1 a2 a3)))))))

;*---------------------------------------------------------------------*/
;*    thread-spawn ...                                                 */
;*---------------------------------------------------------------------*/
(define (thread-spawn th scd p)
   (with-access::scdthread th (condv mutex onerror proc)
      (synchronize mutex
	 (set! onerror #f)
	 (set! proc p)
	 (condition-variable-signal! condv)
	 th)))

;*---------------------------------------------------------------------*/
;*    stage ::queue-scheduler ...                                      */
;*---------------------------------------------------------------------*/
(define-method (stage scd::queue-scheduler thread proc . args)
   (with-access::queue-scheduler scd (mutex)
      (let ((nproc (synchronize mutex
		      (unless (queue-empty? scd)
			 (let ((proc (lambda (s t) (apply proc s t args))))
			    (queue-pop-and-push! scd proc))))))
	 (if nproc
	     ;; the queue is filled, we have to push our task
	     (nproc scd thread)
	     ;; the queue is empty, we can keep going with the same thread
	     (apply proc scd thread args)))))

;*---------------------------------------------------------------------*/
;*    stage0 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage0 scd::queue-scheduler thread proc)
   (with-access::queue-scheduler scd (mutex)
      (let ((nproc (synchronize mutex
		      (unless (queue-empty? scd)
			 (queue-pop-and-push! scd proc)))))
	 (if nproc
	     ;; the queue is empty, we can keep going with the same thread
	     (nproc scd thread)
	     ;; the queue is filled, we have to push our task
	     (proc scd thread)))))

;*---------------------------------------------------------------------*/
;*    stage1 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage1 scd::queue-scheduler thread proc a0)
   (with-access::queue-scheduler scd (mutex)
      (let ((nproc (synchronize mutex
		      (when (queue-empty? scd)
			 (let ((proc (lambda (s t) (proc s t a0))))
			    (queue-pop-and-push! scd proc))))))
	 (if nproc
	     ;; the queue is filled, we have to push our task
	     (nproc scd thread)
	     ;; the queue is empty, we can keep going with the same thread
	     (proc scd thread a0)))))

;*---------------------------------------------------------------------*/
;*    stage2 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage2 scd::queue-scheduler thread proc a0 a1)
   (with-access::queue-scheduler scd (mutex)
      (let ((nproc (synchronize mutex
		      (when (queue-empty? scd)
			 (let ((proc (lambda (s t) (proc s t a0 a1))))
			    (queue-pop-and-push! scd proc))))))
	 (if nproc
	     ;; the queue is filled, we have to push our task
	     (nproc scd thread)
	     ;; the queue is empty, we can keep going with the same thread
	     (proc scd thread a0 a1)))))

;*---------------------------------------------------------------------*/
;*    stage3 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage3 scd::queue-scheduler thread proc a0 a1 a2)
   (with-access::queue-scheduler scd (mutex)
      (let ((nproc (synchronize mutex
		      (unless (queue-empty? scd)
			 (let ((proc (lambda (s t) (proc s t a0 a1 a2))))
			    (queue-pop-and-push! scd proc))))))
	 (if nproc 
	     ;; the queue is filled, we have to push our task
	     (nproc scd thread)
	     ;; the queue is empty, we can keep going with the same thread
	     (proc scd thread a0 a1 a2)))))

;*---------------------------------------------------------------------*/
;*    stage4 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage4 scd::queue-scheduler thread proc a0 a1 a2 a3)
   (with-access::queue-scheduler scd (mutex)
      (let ((nproc (synchronize mutex
		      (unless (queue-empty? scd)
			 (let ((proc (lambda (s t) (proc s t a0 a1 a2 a3))))
			    (queue-pop-and-push! scd proc))))))
	 (if nproc 
	     ;; the queue is filled, we have to push our task
	     (nproc scd thread)
	     ;; the queue is empty, we can keep going with the same thread
	     (proc scd thread a0 a1 a2 a3)))))

;*---------------------------------------------------------------------*/
;*    get-thread! ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function assumes that the scheduler mutex is acquired.      */
;*    -------------------------------------------------------------    */
;*    !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!!  */
;*---------------------------------------------------------------------*/
(define (get-thread! scd::queue-scheduler)
   ;; this function assumes that scheduler mutex is already locked
   (with-access::queue-scheduler scd (free nfree)
      (when (pair? free)
	 (let ((t (car free))
	       (fl free))
	    (set! free (cdr free))
	    (set! nfree (-fx nfree 1))
	    t))))

;*---------------------------------------------------------------------*/
;*    put-thread! ...                                                  */
;*---------------------------------------------------------------------*/
(define (put-thread! scd thread)
   ;; this function assumes that scheduler mutex is already locked
   (with-access::queue-scheduler scd (free nfree)
      (set! free (cons thread free))
      (set! nfree (+fx nfree 1))))

;*---------------------------------------------------------------------*/
;*    queue-thread-body ...                                            */
;*---------------------------------------------------------------------*/
(define (queue-thread-body scd t)
   (with-access::queue-scheduler scd (mutex)

      (define (purge-scheduler-task!)
	 (let loop ()
	    (let ((oproc (synchronize mutex (queue-pop! scd))))
	       (when oproc
		  (oproc scd t)
		  (loop)))))
	 
      (define (run)
	 (with-handler
	    (lambda (e)
	       (scheduler-error-handler e t))
	    (with-access::scdthread t ((tmutex mutex) (tcondv condv) proc)
	       (let loop ()
		  ;; purge all the pending task
		  (purge-scheduler-task!)
		  ;; go into the free list
		  (synchronize mutex
		     (put-thread! scd t))
		  ;; wait to be awaken
		  (synchronize tmutex
		     (condition-variable-wait! tcondv tmutex))
		  ;; execute the user task
		  (proc scd t)
		  (loop)))))
      
      (let loop ()
	 (with-access::scdthread t (mutex)
	    (synchronize mutex (run)))
	 (loop))))
   
;*---------------------------------------------------------------------*/
;*    make-queue-thread ...                                            */
;*---------------------------------------------------------------------*/
(define (make-queue-thread scd)
   (letrec ((t (instantiate::scdthread
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
   (with-access::queue-scheduler scd (head tail qlength max-qlength condv)
      (set! qlength (+fx qlength 1))
      (when (=fx qlength max-qlength) (condition-variable-signal! condv))
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
   (with-access::queue-scheduler scd (head tail qlength max-qlength condv)
      (when (=fx qlength max-qlength) (condition-variable-signal! condv))
      (when (pair? head)
	 (let ((task (car head)))
	    (set! qlength (-fx qlength 1))
	    (set! head (cdr head))
	    (when (null? head) (set! tail '()))
	    task))))

;*---------------------------------------------------------------------*/
;*    queue-pop-and-push! ...                                          */
;*---------------------------------------------------------------------*/
(define (queue-pop-and-push! scd::queue-scheduler proc)
   (let ((nproc (queue-pop! scd)))
      (queue-push! scd proc)
      nproc))
