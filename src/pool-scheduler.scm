;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/src/pool-scheduler.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb 26 07:03:15 2008                          */
;*    Last change :  Mon Aug 18 12:49:31 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Pool scheduler                                                   */
;*    -------------------------------------------------------------    */
;*    The characteristics of this scheduler are:                       */
;*      - a request is handled by a single thread extracted from the   */
;*        pool.                                                        */
;*      - on competion the thread is stored in the pool.               */
;*      - on heavy load the new request waits for an old request to    */
;*        complete.                                                    */
;*    This scheduler is a little bit more complex and smarter than     */
;*    the one-to-one scheduler but it cannot handle more than SIZE     */
;*    simultaneous requests.                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_scheduler-pool

   (cond-expand
      (enable-threads
       (library pthread)))
   
   (library hop)
   
   (import  hop_scheduler
	    hop_param)

   (export  (class pool-scheduler::scheduler
	       (mutex::mutex read-only (default (make-mutex)))
	       (condv::condvar read-only (default (make-condition-variable)))
	       (nfree::int (default 0))
	       (free::pair-nil (default '())))))

;*---------------------------------------------------------------------*/
;*    scheduler-init! ::pool-scheduler ...                             */
;*---------------------------------------------------------------------*/
(define-method (scheduler-init! scd::pool-scheduler)
   (with-access::pool-scheduler scd (size free nfree)
      (set! free (map! (lambda (x) (make-pool-thread scd)) (make-list size)))
      (set! nfree size)
      scd))

;*---------------------------------------------------------------------*/
;*    scheduler-stat ::pool-scheduler ...                              */
;*---------------------------------------------------------------------*/
(define-method (scheduler-stat scd::pool-scheduler)
   (with-access::pool-scheduler scd (size nfree)
      (format " (~a/~a)" (- size nfree) size)))

;*---------------------------------------------------------------------*/
;*    scheduler-load ::pool-scheduler ...                              */
;*---------------------------------------------------------------------*/
(define-method (scheduler-load scd::pool-scheduler)
   (with-access::pool-scheduler scd (nfree size)
      (flonum->fixnum
       (*fl 100.
	    (/fl (fixnum->flonum (-fx size nfree)) (fixnum->flonum size))))))

;*---------------------------------------------------------------------*/
;*    spawn ::pool-scheduler ...                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn scd::pool-scheduler p . args)
   (with-access::pool-scheduler scd (mutex condv free nfree)
      (mutex-lock! mutex)
      (let loop ()
	 (unless (pair? free)
	    ;; we have to wait for a thread to complete
	    (condition-variable-wait! condv mutex)
	    (loop)))
      (let ((thread (car free)))
	 (set! free (cdr free))
	 (set! nfree (-fx nfree 1))
	 (mutex-unlock! mutex)
	 (with-access::hopthread thread (proc mutex condv)
	    (set! proc (lambda (s t) (apply p s t args)))
	    (mutex-lock! mutex)
	    (condition-variable-signal! condv)
	    (mutex-unlock! mutex)))))

;*---------------------------------------------------------------------*/
;*    stage ::pool-scheduler ...                                       */
;*---------------------------------------------------------------------*/
(define-method (stage scd::pool-scheduler proc . args)
   (apply proc scd (current-thread) args))

;*---------------------------------------------------------------------*/
;*    pool-thread-body ...                                             */
;*---------------------------------------------------------------------*/
(define (pool-thread-body t)
   (let* ((scd (hopthread-scheduler t))
	  (mutex (hopthread-mutex t))
	  (condv (hopthread-condv t))
	  (smutex (pool-scheduler-mutex scd)))
      (mutex-lock! mutex)
      (let loop ()
	 (condition-variable-wait! condv mutex)
	 (let liip ((proc (hopthread-proc t)))
	    ;; complete the demanded task
	    (with-handler
	       scheduler-default-handler
	       (proc scd t))
	    ;; go back to the free pool
	    (mutex-lock! smutex)
	    (with-access::pool-scheduler scd (free nfree)
	       (set! free (cons t free))
	       (set! nfree (+fx nfree 1))
	       (mutex-unlock! smutex)
	       (loop))))))
   
;*---------------------------------------------------------------------*/
;*    make-pool-thread ...                                             */
;*---------------------------------------------------------------------*/
(define (make-pool-thread scd)
   (letrec ((t (instantiate::hopthread
		  (name (gensym 'pool-scheduler))
		  (scheduler scd)
		  (body (lambda () (pool-thread-body t))))))
      (thread-start! t)
      t))
		   
		   
