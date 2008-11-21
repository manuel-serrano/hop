;*=====================================================================*/
;*    serrano/prgm/project/hop/1.10.x/src/queue-scheduler.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 22 14:29:19 2008                          */
;*    Last change :  Fri Nov 21 09:57:16 2008 (serrano)                */
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
	       (thread-mutex::mutex read-only (default (make-mutex)))
	       (queue-mutex::mutex read-only (default (make-mutex)))
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
   (let ((t (get-thread! scd)))
      (if (thread? t)
	  ;; there is an available thread, we use it...
	  (thread-spawn t scd (lambda (s t) (apply proc s t args)))
	  ;; everybody is busy, we have to push our task in the queue...
	  (queue-push! scd (lambda (s t) (apply proc s t args))))))

;*---------------------------------------------------------------------*/
;*    spawn0 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (spawn0 scd::queue-scheduler proc)
   (let ((t (get-thread! scd)))
      (if (thread? t)
	  ;; there is an available thread, we use it...
	  (thread-spawn scd t proc)
	  ;; everybody is busy, we have to push our task in the queue...
	  (queue-push! scd proc))))

;*---------------------------------------------------------------------*/
;*    spawn1 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (spawn1 scd::queue-scheduler proc a0)
   (let ((t (get-thread! scd)))
      (if (thread? t)
	  ;; there is an available thread, we use it...
	  (thread-spawn t scd (lambda (s t) (proc s t a0)))
	  ;; everybody is busy, we have to push our task in the queue...
	  (queue-push! scd (lambda (s t) (proc s t a0))))))

;*---------------------------------------------------------------------*/
;*    spawn4 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (spawn4 scd::queue-scheduler proc a0 a1 a2 a3)
   (let ((t (get-thread! scd)))
      (if (thread? t)
	  ;; there is an available thread, we use it...
	  (thread-spawn t scd (lambda (s t) (proc s t a0 a1 a2 a3)))
	  ;; everybody is busy, we have to push our task in the queue...
	  (queue-push! scd (lambda (s t) (proc s t a0 a1 a2 a3))))))

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
   (if (queue-empty? scd)
       ;; the queue is empty, we can keep going with the same thread
       (apply proc scd thread args)
       ;; the queue is filled, we have to push our task
       (let* ((proc (lambda (s t) (apply proc s t args)))
	      (nproc (queue-pop-and-push! scd proc)))
	  (nproc scd (current-thread)))))

;*---------------------------------------------------------------------*/
;*    stage0 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage0 scd::queue-scheduler thread proc)
   (if (queue-empty? scd)
       ;; the queue is empty, we can keep going with the same thread
       (proc scd thread)
       ;; the queue is filled, we have to push our task
       (let ((nproc (queue-pop-and-push! scd proc)))
	  (nproc scd (current-thread)))))

;*---------------------------------------------------------------------*/
;*    stage1 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage1 scd::queue-scheduler thread proc a0)
   (if (queue-empty? scd)
       ;; the queue is empty, we can keep going with the same thread
       (proc scd thread a0)
       ;; the queue is filled, we have to push our task
       (let* ((proc (lambda (s t) (proc s t a0)))
	      (nproc (queue-pop-and-push! scd proc)))
	  (nproc scd (current-thread)))))

;*---------------------------------------------------------------------*/
;*    stage2 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage2 scd::queue-scheduler thread proc a0 a1)
   (if (queue-empty? scd)
       ;; the queue is empty, we can keep going with the same thread
       (proc scd thread a0 a1)
       ;; the queue is filled, we have to push our task
       (let* ((proc (lambda (s t) (proc s t a0 a1)))
	      (nproc (queue-pop-and-push! scd proc)))
	  (nproc scd (current-thread)))))

;*---------------------------------------------------------------------*/
;*    stage3 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage3 scd::queue-scheduler thread proc a0 a1 a2)
   (if (queue-empty? scd)
       ;; the queue is empty, we can keep going with the same thread
       (proc scd thread a0 a1 a2)
       ;; the queue is filled, we have to push our task
       (let* ((proc (lambda (s t) (proc s t a0 a1 a2)))
	      (nproc (queue-pop-and-push! scd proc)))
	  (nproc scd (current-thread)))))

;*---------------------------------------------------------------------*/
;*    stage4 ::queue-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage4 scd::queue-scheduler thread proc a0 a1 a2 a3)
   (if (queue-empty? scd)
       ;; the queue is empty, we can keep going with the same thread
       (proc scd thread a0 a1 a2 a3)
       ;; the queue is filled, we have to push our task
       (let* ((proc (lambda (s t) (proc s t a0 a1 a2 a3)))
	      (nproc (queue-pop-and-push! scd proc)))
	  (nproc scd (current-thread)))))

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
   (with-access::queue-scheduler scd (thread-mutex free nfree flist)
      (mutex-lock! thread-mutex)
      (if (pair? free)
	  (let ((t (car free))
		(fl free))
	     (set! free (cdr free))
	     (set-cdr! fl flist)
	     (set! flist fl)
	     (set! nfree (-fx nfree 1))
	     (mutex-unlock! thread-mutex)
	     t)
	  (begin
	     (mutex-unlock! thread-mutex)
	     #f))))

;*---------------------------------------------------------------------*/
;*    put-thread! ...                                                  */
;*---------------------------------------------------------------------*/
(define (put-thread! scd thread)
   (with-access::queue-scheduler scd (thread-mutex free nfree flist)
      (mutex-lock! thread-mutex)
      (let ((nfree flist))
	 (set! flist (cdr flist))
	 (set-car! nfree thread)
	 (set-cdr! nfree free)
	 (set! free nfree))
      (set! nfree (+fx nfree 1))
      (mutex-unlock! thread-mutex)))

;*---------------------------------------------------------------------*/
;*    queue-thread-body ...                                            */
;*---------------------------------------------------------------------*/
(define (queue-thread-body scd t)
   (with-access::queue-scheduler scd (queue-mutex)
      (let loop ()
	 (with-handler
	    (lambda (e)
	       (scheduler-error-handler e t))
	    (let ((mutex (hopthread-mutex t))
		  (condv (hopthread-condv t)))
	       (define (loop)
		  (condition-variable-wait! condv mutex)
		  (let liip ((proc (hopthread-proc t)))
		     ;; execute the user task
		     (proc scd t)
		     ;; check if there is a new task
		     (begin
			(mutex-lock! queue-mutex)
			(let ((nproc (queue-pop! scd)))
			   (if nproc
			       ;; there is a waiting task, execute it
			       (begin
				  (mutex-unlock! queue-mutex)
				  (liip nproc))
			       ;; nothing to do, go to sleep waiting for a task
			       (begin
				  (put-thread! scd t)
				  (mutex-unlock! queue-mutex)
				  (loop)))))))
	       (with-lock mutex loop)))
	 (mutex-lock! queue-mutex)
	 (put-thread! scd t)
	 (mutex-unlock! queue-mutex)
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
   (with-access::queue-scheduler scd (head)
      (null? head)))

;*---------------------------------------------------------------------*/
;*    queue-push! ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function assumes that no thread is available.               */
;*---------------------------------------------------------------------*/
(define (queue-push! scd::queue-scheduler entry)
   ;; this function assumes that queue-mutex (of the scheduler)
   ;; is already locked
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
   ;; this function assumes that queue-mutex (of the scheduler)
   ;; is already locked
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
   (with-access::queue-scheduler scd (head tail queue-mutex)
      (mutex-lock! queue-mutex)
      (let ((task (car head)))
	 (set-car! head proc)
	 (unless (eq? head tail)
	    (let ((t head))
	       (set-cdr! head (cdr head))
	       (set-cdr! t '())
	       (set-cdr! tail t)))
	 (mutex-unlock! queue-mutex)
	 task)))
