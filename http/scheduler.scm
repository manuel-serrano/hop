;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/http/scheduler.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 22 11:19:21 2008                          */
;*    Last change :  Tue May 14 16:08:56 2024 (serrano)                */
;*    Copyright   :  2008-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Specification of the various Hop schedulers                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __http_scheduler
   
   (library pthread)
   
   (export (class scdthread::pthread
	      (proc::procedure (default (lambda (t) #f)))
	      (condv::condvar read-only (default (make-condition-variable)))
	      (mutex::mutex read-only (default (make-mutex)))
	      (scheduler::scheduler (default (class-nil scheduler)))
	      (info::obj (default #unspecified))
	      (request::obj (default #f))
	      (onerror::obj (default #f))
	      (error-args::vector read-only (default (make-vector 3)))
	      (error-args-length::int (default 0))
	      (inbuf::bstring (default (make-string 512)))
	      (outbuf::bstring (default (make-string 8192)))
	      (flushbuf::bstring (default (make-string 16)))
	      (userdata::obj (default #unspecified)))

           (macro debug-thread-info-set! thread info)
	   (macro with-stage-handler handler args . body)

	   (class &ignore-exception::&exception)
	   
	   (abstract-class scheduler
	      (scheduler-init!)
	      (onready read-only (default #f))
	      (size::int read-only (default 0))
	      (filters::pair-nil read-only)
	      (accept-timeout::long read-only (default (*fx 10 1000)))
	      (keep-alive-timeout::long read-only))

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
;*       free variables of the handler inside scdthread specific       */
;*       fields.                                                       */
;*---------------------------------------------------------------------*/
(define-macro (with-stage-handler handler args . body)
   (let ((len (length args)))
      `(with-access::scdthread thread (onerror error-args-length error-args)
	  (set! onerror ,handler)
	  (set! error-args-length ,len)
	  ,@(map (lambda (v i)
		    `(vector-set! error-args ,i ,v))
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
;*    thread-info ::scdthread ...                                      */
;*---------------------------------------------------------------------*/
(define-method (thread-info th::scdthread)
   (with-access::scdthread th (info)
      info))

;*---------------------------------------------------------------------*/
;*    thread-info ::scdthread ...                                      */
;*---------------------------------------------------------------------*/
(define-method (thread-info-set! th::scdthread i)
   (with-access::scdthread th (info)
      (set! info i)))

;* {*---------------------------------------------------------------------*} */
;* {*    thread-request ::scdthread ...                                   *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (thread-request th::scdthread)                       */
;*    (with-access::scdthread th (request)                             */
;*       request))                                                     */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    thread-request-set! ::scdthread ...                              *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (thread-request-set! th::scdthread req)              */
;*    (with-access::scdthread th (request)                             */
;*       (set! request req)))                                          */

;*---------------------------------------------------------------------*/
;*    scheduler-default-handler ...                                    */
;*---------------------------------------------------------------------*/
(define (scheduler-default-handler e)
   (cond
      ((isa? e &ignore-exception)
       #unspecified)
      ((isa? e &exception)
       (exception-notify e))
      (else
       (fprint (current-error-port) "*** INTERNAL ERROR, uncaught exception: "
	       (typeof e))
       (let ((th (current-thread)))
	  (when (isa? th thread)
	     (tprint "Thread: " th
		     " thread-info: " (thread-info th)
		     " exception=" e))))))

;*---------------------------------------------------------------------*/
;*    scheduler-error-handler ...                                      */
;*---------------------------------------------------------------------*/
(define (scheduler-error-handler e t)
   (with-access::scdthread t (onerror error-args-length error-args)
      (if (procedure? onerror)
	  (let ((onerr onerror))
	     (set! onerror #f)
	     (case error-args-length
		((0)
		 (onerr e))
		((1)
		 (onerr e
		    (vector-ref error-args 0)))
		((2)
		 (onerr e
		    (vector-ref error-args 0)
		    (vector-ref error-args 1)))
		((3)
		 (onerr e
		    (vector-ref error-args 0)
		    (vector-ref error-args 1)
		    (vector-ref error-args 2)))
		(else
		 (onerr e))))
	  (scheduler-default-handler e))))

;*---------------------------------------------------------------------*/
;*    make-scheduler-error-handler ...                                 */
;*---------------------------------------------------------------------*/
(define (make-scheduler-error-handler t)
   (lambda (e)
      (scheduler-error-handler e t)))
