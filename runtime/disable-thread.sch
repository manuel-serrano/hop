;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/disable-thread.sch              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 23 10:29:32 2007                          */
;*    Last change :  Fri May 25 12:16:09 2007 (serrano)                */
;*    Copyright   :  2007 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The __hop_thread module directives when threads are              */
;*    disabled. It also complements the module with fake thread        */
;*    implementation.                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives

   (extern  (macro fork::int () "fork"))
   
   (static  (class pooled-thread::%thread
	       (thunk::procedure (default (lambda () #f)))
	       (condv::condvar read-only)
	       (mutex::mutex read-only)
	       (data (default #unspecified))
	       (pool (default #unspecified)))
	    
	    (class eager-pool::pool)
	    
	    (class lazy-pool::pool
	       nb-threads::int))

   (export  (class %thread
	       (name read-only)
	       (body::procedure read-only)
	       (cleanup::procedure (default (lambda (t) #unspecified)))
	       (specific::obj (default #unspecified)))

	    (current-thread)
	    (inline thread? ::obj)
	    (make-thread proc #!optional name)
	    (thread-start! ::%thread)
	    (thread-specific ::%thread)
	    (thread-specific-set! ::%thread ::obj)
	    (inline thread-cleanup::procedure ::%thread)
	    (inline thread-cleanup-set! ::%thread ::procedure)
	    (thread-terminate! ::%thread)
	    
	    (mutex-state ::mutex)))

;*---------------------------------------------------------------------*/
;*    *current-thread* ...                                             */
;*---------------------------------------------------------------------*/
(define *current-thread*
   #unspecified)

;*---------------------------------------------------------------------*/
;*    current-thread ...                                               */
;*---------------------------------------------------------------------*/
(define (current-thread)
   *current-thread*)

;*---------------------------------------------------------------------*/
;*    thread? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (thread? obj)
   (%thread? obj))

;*---------------------------------------------------------------------*/
;*    make-thread ...                                                  */
;*---------------------------------------------------------------------*/
(define (make-thread body #!optional name)
   (instantiate::%thread
      (name name)
      (body body)))

;*---------------------------------------------------------------------*/
;*    thread-start! ...                                                */
;*---------------------------------------------------------------------*/
(define (thread-start! th)
   (with-trace 2 'thread-start!
      (trace-item "thread=" (%thread-name th))
      (with-access::%thread th (body cleanup)
	 (let ((old (current-thread)))
	    (unwind-protect
	       (begin
		  (set! *current-thread* th)
		  (unwind-protect
		     (with-trace 2 'thread-start-body
			(body))
		     (with-trace 2 'thread-start-cleanup
			(cleanup th))))
	       (set! *current-thread* old))))))

;*---------------------------------------------------------------------*/
;*    thread-terminate! ...                                            */
;*---------------------------------------------------------------------*/
(define (thread-terminate! th)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    *thread-specific* ...                                            */
;*---------------------------------------------------------------------*/
(define *thread-specific* #unspecified)

;*---------------------------------------------------------------------*/
;*    thread-specific ...                                              */
;*---------------------------------------------------------------------*/
(define (thread-specific th)
   (if (%thread? th)
       (%thread-specific th)
       *thread-specific*))

;*---------------------------------------------------------------------*/
;*    thread-specific-set! ...                                         */
;*---------------------------------------------------------------------*/
(define (thread-specific-set! th val)
   (if (%thread? th)
       (%thread-specific-set! th val)
       (set! *thread-specific* val)))

;*---------------------------------------------------------------------*/
;*    thread-cleanup ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (thread-cleanup th)
   (%thread-cleanup th))

;*---------------------------------------------------------------------*/
;*    thread-cleanup-set! ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (thread-cleanup-set! th val)
   (%thread-cleanup-set! th val))

;*---------------------------------------------------------------------*/
;*    mutex-state ...                                                  */
;*---------------------------------------------------------------------*/
(define (mutex-state mutex)
   'not-abandoned)

;*---------------------------------------------------------------------*/
;*    execute! ...                                                     */
;*---------------------------------------------------------------------*/
(define (execute! thread body msg)
   (with-access::pooled-thread thread (name data pool)
      (with-trace 3 'execute!
	 (trace-item "th=" name)
	 (set! data msg)
	 (let ((old (current-thread)))
	    (unwind-protect
	       (begin
		  (set! *current-thread* thread)
		  (body))
	       (begin
		  (set! *current-thread* old)
		  (pool-add! pool thread)))))))
   
;*---------------------------------------------------------------------*/
;*    make-pool-thread ...                                             */
;*---------------------------------------------------------------------*/
(define (make-pool-thread pool)
   (let ((mutex (make-mutex))
	 (condv (make-condition-variable))
	 (name (gensym (pool-id pool))))
      (instantiate::pooled-thread
	 (name name)
	 (mutex mutex)
	 (condv condv)
	 (pool pool)
	 (body (lambda () #unspecified)))))
		   
;*---------------------------------------------------------------------*/
;*    call-in-background ...                                           */
;*    -------------------------------------------------------------    */
;*    In a single-threaded environment it is required to spawn         */
;*    a new process for executing in background.                       */
;*---------------------------------------------------------------------*/
(define (call-in-background thunk)
   (cond-expand
      (bigloo-c
       (when (=fx (fork) 0)
	  (thunk)
	  (exit 0)))
      (else
       (thunk))))
