;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/src/pool_scheduler.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb 26 07:03:15 2008                          */
;*    Last change :  Sun Nov 18 16:44:34 2012 (serrano)                */
;*    Copyright   :  2008-12 Manuel Serrano                            */
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
      (synchronize mutex
	 (format " (~a/~a)" (-fx size naccept) size))))

;*---------------------------------------------------------------------*/
;*    scheduler-load ::pool-scheduler ...                              */
;*---------------------------------------------------------------------*/
(define-method (scheduler-load scd::pool-scheduler)
   (with-access::pool-scheduler scd (naccept size mutex)
      (synchronize mutex
	 (flonum->fixnum
	    (*fl 100.
	       (/fl (fixnum->flonum (-fx size naccept))
		  (fixnum->flonum size)))))))

;*---------------------------------------------------------------------*/
;*    spawn ::pool-scheduler ...                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn scd::pool-scheduler p . args)
   (with-access::pool-scheduler scd ((smutex mutex) condv free nfree)
      (let ((thread #f))
	 (synchronize smutex
	    (let loop ()
	       (unless (pair? free)
		  ;; we have to wait for a thread to complete
		  (condition-variable-wait! condv smutex)
		  (loop)))
	    (set! thread (car free))
	    (with-access::hopthread thread (userdata)
	       (set! userdata free)
	       (set! free (cdr free))
	       (set! nfree (-fx nfree 1))))
	 (with-access::hopthread thread (proc mutex condv)
	    (set! proc (lambda (s t) (apply p s t args)))
	    (synchronize mutex
	       (condition-variable-signal! condv))
	    thread))))

;*---------------------------------------------------------------------*/
;*    spawn5 ::pool-scheduler ...                                      */
;*---------------------------------------------------------------------*/
(define-method (spawn5 scd::pool-scheduler p a0 a1 a2 a3 a4)
   (with-access::pool-scheduler scd ((smutex mutex) condv free nfree)
      (let ((thread #f))
	 (synchronize smutex
	    (let loop ()
	       (unless (pair? free)
		  ;; we have to wait for a thread to complete
		  (condition-variable-wait! condv smutex)
		  (loop)))
	    (set! thread (car free))
	    (with-access::hopthread thread (userdata)
	       (set! userdata free)
	       (set! free (cdr free))
	       (set! nfree (-fx nfree 1))))
	 (with-access::hopthread thread (proc mutex condv)
	    (set! proc (lambda (s t) (p s t a0 a1 a2 a3 a4)))
	    (synchronize mutex
	       (condition-variable-signal! condv))
	    thread))))

;*---------------------------------------------------------------------*/
;*    pool-thread-body ...                                             */
;*---------------------------------------------------------------------*/
(define (pool-thread-body t)
   (with-access::hopthread t (proc userdata mutex condv scheduler)
      (synchronize mutex
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
	       (synchronize smutex
		  (set-cdr! userdata free)
		  (set! free userdata)
		  (set! nfree (+fx nfree 1))
		  (condition-variable-signal! scondv))
	       (loop))))))
   
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
		   
		   
