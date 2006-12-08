;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/thread.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov 16 13:24:06 2004                          */
;*    Last change :  Thu Dec  7 08:08:39 2006 (serrano)                */
;*    Copyright   :  2004-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    hop_thread                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    hop_thread                                                       */
;*---------------------------------------------------------------------*/
(module __hop_thread
   
   (library pthread)
   
   (import  __hop_param
	    __hop_misc)
   
   (static  (class pooled-thread::pthread
	       (thunk::procedure (default (lambda () #f)))
	       (condv::condvar read-only)
	       (mutex::mutex read-only)
	       (data (default #unspecified)))
	    
	    (class eager-pool::pool)
	    
	    (class lazy-pool::pool
	       nb-threads::int))

   (export  (abstract-class pool
	       (id::symbol read-only)
	       (size::int read-only)
	       (mutex::mutex read-only)
	       (condv::condvar read-only)
	       (free::pair-nil (default '()))
	       (use::pair-nil (default '()))))

   (export  (hop-current-thread)
	    (thread-data ::obj)
	    (make-threads-pool ::symbol ::int ::int)
	    (pool-thread-execute ::pool ::procedure ::procedure ::obj #!optional pred)
	    (pool-thread-available::int ::pool)))

;*---------------------------------------------------------------------*/
;*    hop-current-thread ...                                           */
;*---------------------------------------------------------------------*/
(define (hop-current-thread)
   (current-thread))

;*---------------------------------------------------------------------*/
;*    thread-data ...                                                  */
;*---------------------------------------------------------------------*/
(define (thread-data th)
   (if (pooled-thread? th)
       (pooled-thread-data th)
       #f))

;*---------------------------------------------------------------------*/
;*    make-threads-pool ...                                            */
;*---------------------------------------------------------------------*/
(define (make-threads-pool id size init)
   (let* ((cv (make-condition-variable id))
	  (mu (make-mutex id))
	  (init (if (<fx init 0) size init))
	  (pool (if (>fx size init)
		    (instantiate::lazy-pool
		       (id id)
		       (mutex mu)
		       (condv cv)
		       (size size)
		       (nb-threads size))
		    (instantiate::eager-pool
		       (id id)
		       (mutex mu)
		       (condv cv)
		       (size size)))))
      (let loop ((n init))
	 (when (>fx n 0)
	    (let ((t (make-pool-thread pool)))
	       (pool-free-set! pool (cons t (pool-free pool)))
	       (loop (-fx n 1)))))
      pool))

;*---------------------------------------------------------------------*/
;*    pool-get::pooled-thread ...                                      */
;*---------------------------------------------------------------------*/
(define-generic (pool-get pool pred obj))

;*---------------------------------------------------------------------*/
;*    pool-get ::eager-pool ...                                        */
;*---------------------------------------------------------------------*/
(define-method (pool-get pool::eager-pool pred msg)
   (with-access::eager-pool pool (id free use condv mutex)
      (mutex-lock! mutex)
      (hop-verb 20
		"pool-get[" id "] (eager-pool): "
		" free: " (length free)
		" use: " (length use) "\n")
      (let loop ()
	 (if (and (pair? free) (or (not pred) (pred pool)))
	     (let ((t (car free)))
		(set! free (cdr free))
		(set! use (cons t use))
		(mutex-unlock! mutex)
		(hop-verb 3 (hop-color msg msg
				       (format " THREAD POOL ~a" id))
			  " free threads: " (length free) "\n")
		t)
	     (begin
		(when (and (pair? free) pred (not (pred pool)))
		   (hop-verb 1
			     (hop-color msg msg " MAX CONNEXION")
			     " per site reached...."
			     (trace-color 1 "TIMING OUT\n")))
		(if (not (condition-variable-wait! condv mutex (*fx 10 1000)))
		    (begin
		       (mutex-unlock! mutex)
		       #f)
		    (loop)))))))

;*---------------------------------------------------------------------*/
;*    pool-add! ...                                                    */
;*---------------------------------------------------------------------*/
(define (pool-add! pool::pool thread)
   (with-access::pool pool (id mutex condv free use)
      (hop-verb 21 "pool-add[" id "] " mutex " state: "
		(mutex-state mutex) "\n")
      (mutex-lock! mutex)
      (set! use (remq! thread use))
      (set! free (cons thread free))
      (pooled-thread-data-set! thread #unspecified)
      (hop-verb 21 "pool-add[" id "](cont)"
		" free: " (length free)
		" use: " (length use) "\n")
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
;*    pool-thread-execute ...                                          */
;*---------------------------------------------------------------------*/
(define (pool-thread-execute pool thunk err message #!optional pred)
   (let ((t (pool-get pool pred message)))
      (if (thread? t)
	  (execute! t thunk message)
	  (err message))))

;*---------------------------------------------------------------------*/
;*    pool-thread-available ...                                        */
;*---------------------------------------------------------------------*/
(define (pool-thread-available pool)
   (with-access::eager-pool pool (id free use condv mutex)
      (length free)))

