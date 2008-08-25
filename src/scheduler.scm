;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/src/scheduler.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 22 11:19:21 2008                          */
;*    Last change :  Mon Aug 25 15:30:40 2008 (serrano)                */
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
		  (inbuf::bstring (default (make-string 512))))))
      (else
       (export (class hopthread::thread
		  (proc::procedure (default (lambda (t) #f)))
		  (condv::condvar read-only (default (make-condition-variable)))
		  (mutex::mutex read-only (default (make-mutex)))
		  (scheduler::scheduler (default (scheduler-nil)))
		  (info::obj (default #unspecified))
		  (request::obj (default #f))
		  (onerror::obj (default #f))
		  (inbuf::bstring (default (make-string 512)))
		  (body::procedure read-only)))))
   
   (export (abstract-class scheduler
	      (scheduler-init!)
	      (size::int read-only (default 0)))
	   
	   (generic scheduler-init! ::scheduler)
	   (generic scheduler-stat ::scheduler)
	   (generic scheduler-load::int ::scheduler)
	   
	   (generic spawn ::scheduler ::procedure . args)
	   (generic spawn0 ::scheduler ::procedure)
	   (generic spawn1 ::scheduler ::procedure ::obj)
	   (generic spawn2 ::scheduler ::procedure ::obj ::obj)
	   (generic spawn3 ::scheduler ::procedure ::obj ::obj ::obj)
	   (generic spawn4 ::scheduler ::procedure ::obj ::obj ::obj ::obj)
	   
	   (generic stage ::scheduler ::procedure . args)
	   (generic stage0 ::scheduler ::procedure)
	   (generic stage1 ::scheduler ::procedure ::obj)
	   (generic stage2 ::scheduler ::procedure ::obj ::obj)
	   (generic stage3 ::scheduler ::procedure ::obj ::obj ::obj)
	   (generic stage4 ::scheduler ::procedure ::obj ::obj ::obj ::obj)
	   
	   (generic thread-info ::obj)
	   (generic thread-info-set! ::obj ::obj)
	   
	   (scheduler-default-handler ::obj)
	   (make-scheduler-error-handler ::obj)))

;*---------------------------------------------------------------------*/
;*    thread-start!                                                    */
;*---------------------------------------------------------------------*/
(cond-expand
   ((not enable-threads)
    (define-method (thread-start! o::hopthread . scd)
       ((hopthread-body o)))))

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
   (apply stage scd proc args))

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
;*    stage ...                                                        */
;*---------------------------------------------------------------------*/
(define-generic (stage scd::scheduler proc::procedure . args))

;*---------------------------------------------------------------------*/
;*    stage0 ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (stage0 scd::scheduler proc::procedure)
   (stage scd proc))

;*---------------------------------------------------------------------*/
;*    stage1 ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (stage1 scd::scheduler proc::procedure a0)
   (stage scd proc a0))

;*---------------------------------------------------------------------*/
;*    stage2 ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (stage2 scd::scheduler proc::procedure a0 a1)
   (stage scd proc a0 a1))

;*---------------------------------------------------------------------*/
;*    stage3 ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (stage3 scd::scheduler proc::procedure a0 a1 a2)
   (stage scd proc a0 a1 a2))

;*---------------------------------------------------------------------*/
;*    stage4 ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (stage4 scd::scheduler proc::procedure a0 a1 a2 a3)
   (stage scd proc a0 a1 a2 a3))

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
   (if (&exception? e)
       (exception-notify e)
       (fprint (current-error-port) "*** INTERNAL ERROR"
	       "uncaught exception: "
	       (find-runtime-type e)))
   (let ((th (current-thread)))
      (when (thread? th)
	 (fprint (current-error-port) "Thread: " th " " (thread-info th)))))

;*---------------------------------------------------------------------*/
;*    make-scheduler-error-handler ...                                 */
;*---------------------------------------------------------------------*/
(define (make-scheduler-error-handler t)
   (lambda (e)
      (with-access::hopthread t (onerror)
	 (if (procedure? onerror)
	     (with-handler
		scheduler-default-handler
		(onerror e))
	     (scheduler-default-handler e)))))
