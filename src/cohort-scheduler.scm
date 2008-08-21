;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/src/cohort-scheduler.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 22 16:03:01 2008                          */
;*    Last change :  Tue Aug 19 10:39:18 2008 (serrano)                */
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
	       (id::obj (default #unspecified))
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
	       (cohort::int read-only (default 1))
	       (pools::pair-nil (default '()))
	       (nfree::int (default 0)))))

;*---------------------------------------------------------------------*/
;*    scheduler-init! ::cohort-scheduler ...                           */
;*---------------------------------------------------------------------*/
(define-method (scheduler-init! scd::cohort-scheduler)
   (with-access::cohort-scheduler scd (size nfree pools cohort)
      (let ((psize (/fx size cohort)))
	 (when (<fx psize 3)
	    (error 'cohort-scheduler
		   (format "cohort scheduling requires at least ~a threads"
			   (*fx psize 3))
		   size))
	 (set! pools (map (lambda (id)
			     (let ((pool (instantiate::pool
					    (scheduler scd)
					    (nfree psize)
					    (size psize))))
				(let ((ts (map! (lambda (x)
						   (make-pool-thread scd pool))
						(make-list psize))))
				   (pool-free-set! pool ts)
				   pool)))
			  (iota cohort)))
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
   (with-access::pool pool (size nfree)
      (format "~a/~a" (-fx size nfree) size)))

;*---------------------------------------------------------------------*/
;*    scheduler-load ::cohort-scheduler ...                            */
;*---------------------------------------------------------------------*/
(define-method (scheduler-load scd::cohort-scheduler)
   (with-access::cohort-scheduler scd (nfree size)
      (flonum->fixnum
       (*fl 100.
	    (/fl (fixnum->flonum (-fx size nfree)) (fixnum->flonum size))))))

;*---------------------------------------------------------------------*/
;*    spawn ::cohort-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (spawn scd::cohort-scheduler p . args)
   (%spawn scd p args))

;*---------------------------------------------------------------------*/
;*    %spawn ...                                                       */
;*---------------------------------------------------------------------*/
(define (%spawn scd::cohort-scheduler p args)
   (let ((thread (pool-get (cohort-get-pool scd p))))
      (with-access::hopthread thread (proc mutex condv)
	 (mutex-lock! mutex)
	 (set! proc (lambda (s t) (apply p s t args)))
	 (condition-variable-signal! condv)
	 (mutex-unlock! mutex))))

;*---------------------------------------------------------------------*/
;*    stage ::cohort-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage scd::cohort-scheduler proc . args)
   (%spawn scd proc args))
   
;*---------------------------------------------------------------------*/
;*    cohort-get-pool ...                                              */
;*---------------------------------------------------------------------*/
(define (cohort-get-pool::pool scd::cohort-scheduler id)
   (with-access::cohort-scheduler scd (pools)
      (let loop ((pools (cohort-scheduler-pools scd))
		 (pool #f))
	 (cond
	    ((null? pools)
	     (if (not pool)
		 (error 'cohor-get-pool "Cannot find pool" id)
		 (begin
		    (pool-id-set! pool id)
		    pool)))
	    ((eq? (pool-id (car pools)) id)
	     (car pools))
	    (else
	     (loop (cdr pools)
		   (cond
		      (pool pool)
		      ((eq? (pool-id (car pools)) #unspecified) (car pools))
		      (else #f))))))))

;*---------------------------------------------------------------------*/
;*    pool-get ...                                                     */
;*---------------------------------------------------------------------*/
(define (pool-get pool::pool)
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
   
