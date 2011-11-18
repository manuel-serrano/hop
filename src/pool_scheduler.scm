;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/src/pool_scheduler.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb 26 07:03:15 2008                          */
;*    Last change :  Sat Nov 12 09:00:43 2011 (serrano)                */
;*    Copyright   :  2008-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Pool scheduler                                                   */
;*    -------------------------------------------------------------    */
;*    The characteristics of this scheduler are:                       */
;*      - an accept is handled by a single thread extracted from the   */
;*        pool.                                                        */
;*      - on competition the thread is stored in the pool.             */
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

   (export  (class pool-scheduler::row-scheduler
	       (mutex::mutex read-only (default (make-mutex)))
	       (condv::condvar read-only (default (make-condition-variable)))
	       (nfree::int (default 0))
	       (free::pair-nil (default '()))
	       (naccept::int (default 0)))))

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
   (with-access::pool-scheduler scd (size naccept mutex)
      (mutex-lock! mutex)
      (let ((r (format " (~a/~a)" (-fx size naccept) size)))
	 (mutex-unlock! mutex)
	 r)))

;*---------------------------------------------------------------------*/
;*    scheduler-load ::pool-scheduler ...                              */
;*---------------------------------------------------------------------*/
(define-method (scheduler-load scd::pool-scheduler)
   (with-access::pool-scheduler scd (naccept size mutex)
      (mutex-lock! mutex)
      (let ((r (flonum->fixnum
		(*fl 100.
		     (/fl (fixnum->flonum (-fx size naccept))
			  (fixnum->flonum size))))))
	 (mutex-unlock! mutex)
	 r)))

;*---------------------------------------------------------------------*/
;*    spawn ::pool-scheduler ...                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn scd::pool-scheduler p . args)
   (with-access::pool-scheduler scd ((smutex mutex) condv free nfree)
      (mutex-lock! smutex)
      (let loop ()
	 (unless (pair? free)
	    ;; we have to wait for a thread to complete
	    (condition-variable-wait! condv smutex)
	    (loop)))
      (let ((thread (car free)))
	 (with-access::hopthread thread (proc mutex condv userdata)
	    (set! userdata free)
	    (set! free (cdr free))
	    (set! nfree (-fx nfree 1))
	    (mutex-unlock! smutex)
	    (set! proc (lambda (s t) (apply p s t args)))
	    (mutex-lock! mutex)
	    (condition-variable-signal! condv)
	    (mutex-unlock! mutex)
	    thread))))

;*---------------------------------------------------------------------*/
;*    spawn5 ::pool-scheduler ...                                      */
;*---------------------------------------------------------------------*/
(define-method (spawn5 scd::pool-scheduler p a0 a1 a2 a3 a4)
   (with-access::pool-scheduler scd ((smutex mutex) condv free nfree)
      (mutex-lock! smutex)
      (let loop ()
	 (unless (pair? free)
	    ;; we have to wait for a thread to complete
	    (condition-variable-wait! condv smutex)
	    (loop)))
      (let ((thread (car free)))
	 (with-access::hopthread thread (proc mutex condv userdata)
	    (set! userdata free)
	    (set! free (cdr free))
	    (set! nfree (-fx nfree 1))
	    (mutex-unlock! smutex)
	    (set! proc (lambda (s t) (p s t a0 a1 a2 a3 a4)))
	    (mutex-lock! mutex)
	    (condition-variable-signal! condv)
	    (mutex-unlock! mutex)
	    thread))))

;*---------------------------------------------------------------------*/
;*    pool-thread-body ...                                             */
;*---------------------------------------------------------------------*/
(define (pool-thread-body t)
   (with-access::hopthread t (proc userdata mutex condv scheduler)
      (mutex-lock! mutex)
      (let loop ()
	 (condition-variable-wait! condv mutex)
	 ;; complete the demanded task
	 (with-handler
	    (make-scheduler-error-handler t)
	    (proc scheduler t))
	 ;; go back to the free pool
	 (with-access::pool-scheduler scheduler ((smutex mutex)
						 (scondv condv)
						 free nfree)
	    (mutex-lock! smutex)
	    (set-cdr! userdata free)
	    (set! free userdata)
	    (set! nfree (+fx nfree 1))
	    (condition-variable-signal! scondv)
	    (mutex-unlock! smutex)
	    (loop)))))
   
;*---------------------------------------------------------------------*/
;*    make-pool-thread ...                                             */
;*---------------------------------------------------------------------*/
(define (make-pool-thread scd)
   (letrec ((t (instantiate::hopthread
		  (name (gensym 'pool-scheduler))
		  (scheduler scd)
		  (body (lambda () (pool-thread-body t))))))
      (thread-start-joinable! t)
      t))
		   
		   
