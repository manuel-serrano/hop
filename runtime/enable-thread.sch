;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/enable-thread.sch               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 23 10:29:32 2007                          */
;*    Last change :  Wed May 23 11:56:10 2007 (serrano)                */
;*    Copyright   :  2007 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The __hop_thread module directives when threads are              */
;*    enabled.                                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   
   (library pthread)
   
   (static  (class pooled-thread::pthread
	       (thunk::procedure (default (lambda () #f)))
	       (condv::condvar read-only)
	       (mutex::mutex read-only)
	       (data (default #unspecified))
	       (pool (default #unspecified)))
	    
	    (class eager-pool::pool)
	    
	    (class lazy-pool::pool
	       nb-threads::int)))

;*---------------------------------------------------------------------*/
;*    execute! ...                                                     */
;*---------------------------------------------------------------------*/
(define (execute! thread body msg)
   (with-access::pooled-thread thread (thunk mutex condv data)
      (set! data msg)
      (pooled-thread-thunk-set! thread body)
      (mutex-lock! mutex)
      (condition-variable-signal! condv)
      (mutex-unlock! mutex)))

;*---------------------------------------------------------------------*/
;*    make-pool-thread ...                                             */
;*---------------------------------------------------------------------*/
(define (make-pool-thread pool)
   (let ((mutex (make-mutex))
	 (condv (make-condition-variable))
	 (name (gensym (pool-id pool))))
      (letrec ((t (instantiate::pooled-thread
		     (name name)
		     (mutex mutex)
		     (condv condv)
		     (pool pool)
		     (body (lambda ()
			      (with-handler
				 (lambda (e)
				    (fprint (current-error-port)
					    "*** INTERNAL ERROR: "
					    (current-thread) " -- "
					    "uncaught exception: "
					    (find-runtime-type e))
				    (exception-notify e)
				    (raise e))
				 (begin
				    (mutex-lock! mutex)
				    (let loop ()
				       (condition-variable-wait! condv mutex)
				       ((pooled-thread-thunk t))
				       (pool-add! pool t)
				       (loop)))))))))
	 (thread-start! t)
	 t)))
		   
   
