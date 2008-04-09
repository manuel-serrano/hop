;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/src/cohort-scheduler.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 22 16:03:01 2008                          */
;*    Last change :  Wed Feb 27 07:47:04 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    COHORT scheduler                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_scheduler-cohort

   (cond-expand
      (enable-threads
       (library pthread)))

   (library hop)
   
   (import  hop_scheduler)

   (static  (class pool
	       (id::symbol read-only)
	       (size::int read-only)
	       (scheduler::scheduler read-only)
	       (mutex::mutex read-only (default (make-mutex)))
	       (condv::condvar read-only (default (make-condition-variable)))
	       (free::pair-nil (default '()))
	       nfree::int)

	    (class poolthread::hopthread
	       (pool::pool read-only)))
   
   (export  (class cohort-scheduler::scheduler
	       (mutex::mutex read-only (default (make-mutex 'cohort-scheduler)))
	       (names::pair-nil (default '()))
	       (pools::pair-nil (default '()))
	       (nfree::int (default 0)))))

;*---------------------------------------------------------------------*/
;*    scheduler-init! ::cohort-scheduler ...                           */
;*---------------------------------------------------------------------*/
(define-method (scheduler-init! scd::cohort-scheduler)
   (with-access::cohort-scheduler scd (size nfree pools names)
      (let ((psize (/fx size (length names))))
	 (when (<fx psize 3)
	    (error 'cohort-scheduler
		   (format "cohort scheduling requires at least ~a threads"
			   (*fx psize 3))
		   size))
	 (set! pools (map (lambda (id)
			     (let ((pool (instantiate::pool
					    (id id)
					    (scheduler scd)
					    (nfree psize)
					    (size psize))))
				(let ((ts (map! (lambda (x)
						   (make-pool-thread scd pool))
						(make-list psize))))
				   (pool-free-set! pool ts)
				   pool)))
			  names))
	 (set! nfree size)
	 scd)))

;*---------------------------------------------------------------------*/
;*    scheduler-stat ::cohort-scheduler ...                            */
;*---------------------------------------------------------------------*/
(define-method (scheduler-stat scd::cohort-scheduler)
   (with-access::cohort-scheduler scd (pools)
      (let loop ((pools pools)
		 (res '()))
	 (cond
	    ((pair? (cdr pools))
	     (loop (cdr pools) (cons* ", " (pool-info (car pools)) res)))
	    (else
	     (format " (~a~a)"
		     (pool-info (car pools))
		     (apply string-append res)))))))

;*---------------------------------------------------------------------*/
;*    pool-info ...                                                    */
;*---------------------------------------------------------------------*/
(define (pool-info pool)
   (with-access::pool pool (id size nfree)
      (format "~a: ~a/~a" id (-fx size nfree) size)))

;*---------------------------------------------------------------------*/
;*    scheduler-load ::cohort-scheduler ...                            */
;*---------------------------------------------------------------------*/
(define-method (scheduler-load scd::cohort-scheduler)
   (with-access::cohort-scheduler scd (nfree size)
      (flonum->fixnum
       (*fl 100.
	    (/fl (fixnum->flonum (-fx size nfree)) (fixnum->flonum size))))))

;*---------------------------------------------------------------------*/
;*    schedule-start ::cohort-scheduler ...                            */
;*---------------------------------------------------------------------*/
(define-method (schedule-start scd::cohort-scheduler p msg)
   (let ((thread (pool-get (cohort-get-pool scd msg) msg)))
      (with-access::hopthread thread (proc mutex condv)
	 (mutex-lock! mutex)
	 (set! proc p)
	 (condition-variable-signal! condv)
	 (mutex-unlock! mutex))))

;*---------------------------------------------------------------------*/
;*    schedule ::cohort-scheduler ...                                  */
;*---------------------------------------------------------------------*/
(define-method (schedule scd::cohort-scheduler proc msg)
   (schedule-start scd proc msg))
   
;*---------------------------------------------------------------------*/
;*    cohort-get-pool ...                                              */
;*---------------------------------------------------------------------*/
(define (cohort-get-pool::pool scd::cohort-scheduler id)
   (with-access::cohort-scheduler scd (pools)
      (let loop ((pools (cohort-scheduler-pools scd)))
	 (cond
	    ((null? pools)
	     (error 'cohor-get-pool "Cannot find pool" id))
	    ((eq? (pool-id (car pools)) id)
	     (car pools))
	    (else
	     (loop (cdr pools)))))))

;*---------------------------------------------------------------------*/
;*    pool-get ...                                                     */
;*---------------------------------------------------------------------*/
(define (pool-get pool::pool msg)
   (with-access::pool pool (id free nfree condv mutex scheduler)
      (mutex-lock! mutex)
      (let loop ()
	 (if (pair? free)
	     (let ((t (car free)))
		(set! free (cdr free))
		(set! nfree (-fx nfree 1))
		(with-access::cohort-scheduler scheduler (mutex nfree)
		   (mutex-lock! mutex)
		   (set! nfree (-fx nfree 1))
		   (mutex-unlock! mutex))
		(mutex-unlock! mutex)
		t)
	     (begin
		(condition-variable-wait! condv mutex)
		(loop))))))
   
;*---------------------------------------------------------------------*/
;*    pool-add! ...                                                    */
;*---------------------------------------------------------------------*/
(define (pool-add! pool::pool thread)
   (with-access::pool pool (id mutex condv free nfree scheduler)
      (mutex-lock! mutex)
      (set! free (cons thread free))
      (set! nfree (+fx nfree 1))
      (with-access::cohort-scheduler scheduler (mutex nfree)
	 (mutex-lock! mutex)
	 (set! nfree (+fx nfree 1))
	 (mutex-unlock! mutex))
      (condition-variable-signal! condv)
      (mutex-unlock! mutex)))

;*---------------------------------------------------------------------*/
;*    pool-thread-body ...                                             */
;*---------------------------------------------------------------------*/
(define (pool-thread-body t)
   (let ((mutex (hopthread-mutex t))
	 (condv (hopthread-condv t))
	 (scd (poolthread-scheduler t))
	 (pool (poolthread-pool t)))
      (mutex-lock! mutex)
      (let loop ()
	 (condition-variable-wait! condv mutex)
	 (with-handler
	    scheduler-default-handler
	    ((hopthread-proc t) scd t))
	 (pool-add! pool t)
	 (loop))))

;*---------------------------------------------------------------------*/
;*    make-pool-thread ...                                             */
;*---------------------------------------------------------------------*/
(define (make-pool-thread scd pool)
   (letrec ((t (instantiate::poolthread
		  (name (gensym 'queue-scheduler))
		  (scheduler scd)
		  (pool pool)
		  (body (lambda () (pool-thread-body t))))))
      (thread-start! t)
      t))
   
