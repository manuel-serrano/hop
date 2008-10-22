;*=====================================================================*/
;*    serrano/prgm/project/hop/1.10.x/src/scheduler.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 22 11:19:21 2008                          */
;*    Last change :  Sun Oct 19 17:48:58 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Specification of the various Hop schedulers                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_scheduler
   
   (library hop)
   
   (cond-expand
      (enable-threads
       (library pthread)))
   
   (cond-expand
      (enable-threads
       (export (class hopthread::pthread
		  (proc::procedure (default (lambda (t) #f)))
		  (condv::condvar read-only (default (make-condition-variable)))
		  (mutex::mutex read-only (default (make-mutex)))
		  (scheduler::scheduler (default (scheduler-nil)))
		  (info::obj (default #unspecified))
		  (request::obj (default #f))
		  (onerror::obj (default #f))
		  (error-args::vector read-only (default (make-vector 3)))
		  (error-args-length::int (default 0))
		  (inbuf::bstring (default (make-string 512)))
		  (outbuf::bstring (default (make-string 8192)))
		  (flushbuf::bstring (default (make-string 16))))))
      (else
       (export (class hopthread::thread
		  (proc::procedure (default (lambda (t) #f)))
		  (condv::condvar read-only (default (make-condition-variable)))
		  (mutex::mutex read-only (default (make-mutex)))
		  (scheduler::scheduler (default (scheduler-nil)))
		  (info::obj (default #unspecified))
		  (request::obj (default #f))
		  (onerror::obj (default #f))
		  (error-args::vector read-only (default (make-vector 3)))
		  (error-args-length::int (default 0))
		  (inbuf::bstring (default (make-string 512)))
		  (outbuf::bstring (default (make-string 8192)))
		  (flushbuf::bstring (default (make-string 8)))
		  (body::procedure read-only)))))
   
   (export (macro debug-thread-info-set! thread info)
	   (macro with-stage-handler thread args . body)

	   (class &ignore-exception::&exception)
	   
	   (abstract-class scheduler
	      (scheduler-init!)
	      (size::int read-only (default 0)))

	   (abstract-class row-scheduler::scheduler)
	   
	   (generic scheduler-init! ::scheduler)
	   (generic scheduler-stat ::scheduler)
	   (generic scheduler-load::int ::scheduler)

	   (generic spawn ::scheduler ::procedure . args)
	   (generic spawn0 ::scheduler ::procedure)
	   (generic spawn1 ::scheduler ::procedure ::obj)
	   (generic spawn2 ::scheduler ::procedure ::obj ::obj)
	   (generic spawn3 ::scheduler ::procedure ::obj ::obj ::obj)
	   (generic spawn4 ::scheduler ::procedure ::obj ::obj ::obj ::obj)
	   (generic spawn5 ::scheduler ::procedure ::obj ::obj ::obj ::obj ::obj)
	   
	   (generic stage ::scheduler ::obj ::procedure . args)
	   (generic stage0 ::scheduler ::obj ::procedure)
	   (generic stage1 ::scheduler ::obj ::procedure ::obj)
	   (generic stage2 ::scheduler ::obj ::procedure ::obj ::obj)
	   (generic stage3 ::scheduler ::obj ::procedure ::obj ::obj ::obj)
	   (generic stage4 ::scheduler ::obj ::procedure ::obj ::obj ::obj ::obj)
	   (generic stage5 ::scheduler ::obj ::procedure ::obj ::obj ::obj ::obj ::obj)
	   
	   (generic thread-info ::obj)
	   (generic thread-info-set! ::obj ::obj)
	   
	   (scheduler-default-handler ::obj)
	   (scheduler-error-handler ::obj ::thread)
	   (make-scheduler-error-handler ::obj)))

;*---------------------------------------------------------------------*/
;*    thread-start!                                                    */
;*---------------------------------------------------------------------*/
(cond-expand
   ((not enable-threads)
    (define-method (thread-start! o::hopthread . scd)
       ((hopthread-body o)))))

;*---------------------------------------------------------------------*/
;*    debug-thread-info-set! ...                                       */
;*---------------------------------------------------------------------*/
(define-macro (debug-thread-info-set! thread info)
   `(when (>fx (bigloo-debug) 0)
       (thread-info-set! ,thread ,info)))

;*---------------------------------------------------------------------*/
;*    with-stage-handler ...                                           */
;*    -------------------------------------------------------------    */
;*    The goal of the with-stage-handler machinery is twofolds:        */
;*     - it avoids installing a new error handler at the entry         */
;*       of each stage by using a per-thread handler.                  */
;*     - it avoids creating closure for handlers by storing the        */
;*       free variables of the handler inside hopthread specific       */
;*       fields.                                                       */
;*---------------------------------------------------------------------*/
(define-macro (with-stage-handler handler args . body)
   (let ((len (length args)))
      `(begin
	  (hopthread-onerror-set! thread ,handler)
	  (hopthread-error-args-length-set! thread ,len)
	  ,@(map (lambda (v i)
		    `(vector-set! (hopthread-error-args thread) ,i ,v))
		 args
		 (iota len))
	  ,@body)))

;*---------------------------------------------------------------------*/
;*    scheduler-init! ::scheduler ...                                  */
;*---------------------------------------------------------------------*/
(define-generic (scheduler-init! scd::scheduler) scd)

;*---------------------------------------------------------------------*/
;*    scheduler-stat ::scheduler ...                                   */
;*---------------------------------------------------------------------*/
(define-generic (scheduler-stat scd::scheduler))

;*---------------------------------------------------------------------*/
;*    scheduler-load ::scheduler ...                                   */
;*---------------------------------------------------------------------*/
(define-generic (scheduler-load scd::scheduler) 100)

;*---------------------------------------------------------------------*/
;*    spawn ...                                                        */
;*---------------------------------------------------------------------*/
(define-generic (spawn scd::scheduler proc::procedure . args)
   (apply stage scd #f proc args))

;*---------------------------------------------------------------------*/
;*    spawn0 ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (spawn0 scd::scheduler proc::procedure)
   (spawn scd proc))

;*---------------------------------------------------------------------*/
;*    spawn1 ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (spawn1 scd::scheduler proc::procedure a0)
   (spawn scd proc a0))

;*---------------------------------------------------------------------*/
;*    spawn2 ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (spawn2 scd::scheduler proc::procedure a0 a1)
   (spawn scd proc a0 a1))

;*---------------------------------------------------------------------*/
;*    spawn3 ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (spawn3 scd::scheduler proc::procedure a0 a1 a2)
   (spawn scd proc a0 a1 a2))

;*---------------------------------------------------------------------*/
;*    spawn4 ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (spawn4 scd::scheduler proc::procedure a0 a1 a2 a3)
   (spawn scd proc a0 a1 a2 a3))

;*---------------------------------------------------------------------*/
;*    spawn5 ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (spawn5 scd::scheduler proc::procedure a0 a1 a2 a3 a4)
   (spawn scd proc a0 a1 a2 a3 a4))

;*---------------------------------------------------------------------*/
;*    stage ...                                                        */
;*---------------------------------------------------------------------*/
(define-generic (stage scd::scheduler thread proc::procedure . args))

;*---------------------------------------------------------------------*/
;*    stage0 ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (stage0 scd::scheduler thread proc::procedure)
   (stage scd thread proc))

;*---------------------------------------------------------------------*/
;*    stage1 ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (stage1 scd::scheduler thread proc::procedure a0)
   (stage scd thread proc a0))

;*---------------------------------------------------------------------*/
;*    stage2 ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (stage2 scd::scheduler thread proc::procedure a0 a1)
   (stage scd thread proc a0 a1))

;*---------------------------------------------------------------------*/
;*    stage3 ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (stage3 scd::scheduler thread proc::procedure a0 a1 a2)
   (stage scd thread proc a0 a1 a2))

;*---------------------------------------------------------------------*/
;*    stage4 ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (stage4 scd::scheduler thread proc::procedure a0 a1 a2 a3)
   (stage scd thread proc a0 a1 a2 a3))

;*---------------------------------------------------------------------*/
;*    stage5 ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (stage5 scd::scheduler thread proc::procedure a0 a1 a2 a3 a4)
   (stage scd thread proc a0 a1 a2 a3 a4))

;*---------------------------------------------------------------------*/
;*    stage ::row-scheduler ...                                        */
;*---------------------------------------------------------------------*/
(define-method (stage scd::row-scheduler thread proc . args)
   (apply proc scd thread args))

;*---------------------------------------------------------------------*/
;*    stage0 ::row-scheduler ...                                       */
;*---------------------------------------------------------------------*/
(define-method (stage0 scd::row-scheduler thread proc)
   (proc scd thread))

;*---------------------------------------------------------------------*/
;*    stage1 ::row-scheduler ...                                       */
;*---------------------------------------------------------------------*/
(define-method (stage1 scd::row-scheduler thread proc a0)
   (proc scd thread a0))

;*---------------------------------------------------------------------*/
;*    stage2 ::row-scheduler ...                                       */
;*---------------------------------------------------------------------*/
(define-method (stage2 scd::row-scheduler thread proc a0 a1)
   (proc scd thread a0 a1))

;*---------------------------------------------------------------------*/
;*    stage3 ::row-scheduler ...                                       */
;*---------------------------------------------------------------------*/
(define-method (stage3 scd::row-scheduler thread proc a0 a1 a2)
   (proc scd thread a0 a1 a2))

;*---------------------------------------------------------------------*/
;*    stage4 ::row-scheduler ...                                       */
;*---------------------------------------------------------------------*/
(define-method (stage4 scd::row-scheduler thread proc a0 a1 a2 a3)
   (proc scd thread a0 a1 a2 a3))

;*---------------------------------------------------------------------*/
;*    stage5 ::row-scheduler ...                                       */
;*---------------------------------------------------------------------*/
(define-method (stage5 scd::row-scheduler thread proc a0 a1 a2 a3 a4)
   (proc scd thread a0 a1 a2 a3 a4))

;*---------------------------------------------------------------------*/
;*    *thread-info* ...                                                */
;*---------------------------------------------------------------------*/
(define *thread-info* #f)

;*---------------------------------------------------------------------*/
;*    thread-info ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (thread-info thread)
   *thread-info*)

;*---------------------------------------------------------------------*/
;*    thread-info-set! ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (thread-info-set! thread info)
   (set! *thread-info* info))
	  
;*---------------------------------------------------------------------*/
;*    thread-info ::hopthread ...                                      */
;*---------------------------------------------------------------------*/
(define-method (thread-info th::hopthread)
   (hopthread-info th))

;*---------------------------------------------------------------------*/
;*    thread-info ::hopthread ...                                      */
;*---------------------------------------------------------------------*/
(define-method (thread-info-set! th::hopthread info)
   (hopthread-info-set! th info))

;*---------------------------------------------------------------------*/
;*    thread-request ::hopthread ...                                   */
;*---------------------------------------------------------------------*/
(define-method (thread-request th::hopthread)
   (hopthread-request th))

;*---------------------------------------------------------------------*/
;*    thread-request ::hopthread ...                                   */
;*---------------------------------------------------------------------*/
(define-method (thread-request-set! th::hopthread req)
   (hopthread-request-set! th req))

;*---------------------------------------------------------------------*/
;*    scheduler-default-handler ...                                    */
;*---------------------------------------------------------------------*/
(define (scheduler-default-handler e)
   (cond
      ((&ignore-exception? e)
       #unspecified)
      ((&exception? e)
       (exception-notify e))
      (else
       (fprint (current-error-port) "*** INTERNAL ERROR, uncaught exception: "
	       (find-runtime-type e))
       (let ((th (current-thread)))
	  (when (thread? th)
	     (tprint "Thread: " th
		     " thread-info: " (thread-info th)
		     " exception=" e))))))

;*---------------------------------------------------------------------*/
;*    scheduler-error-handler ...                                      */
;*---------------------------------------------------------------------*/
(define (scheduler-error-handler e t)
   (with-access::hopthread t (onerror error-args-length error-args)
      (if (procedure? onerror)
	  (with-handler
	     scheduler-default-handler
	     (case error-args-length
		((0)
		 (onerror e))
		((1)
		 (onerror e (vector-ref error-args 0)))
		((2)
		 (onerror e
			  (vector-ref error-args 0)
			  (vector-ref error-args 1)))
		((3)
		 (onerror e
			  (vector-ref error-args 0)
			  (vector-ref error-args 1)
			  (vector-ref error-args 2)))
		(else
		 (onerror e))))
	  (scheduler-default-handler e))))

;*---------------------------------------------------------------------*/
;*    make-scheduler-error-handler ...                                 */
;*---------------------------------------------------------------------*/
(define (make-scheduler-error-handler t)
   (lambda (e)
      (scheduler-error-handler e t)))
