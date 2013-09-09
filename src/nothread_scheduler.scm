;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/src/nothread_scheduler.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 22 14:28:00 2008                          */
;*    Last change :  Fri Mar 29 10:50:28 2013 (serrano)                */
;*    Copyright   :  2008-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    NOTHREAD scheduler                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_scheduler-nothread

   (library hop)
   
   (import hop_scheduler)

   (include "stage.sch")

   (export (class nothread-scheduler::row-scheduler)
	   (nothread-scheduler-get-fake-thread proc)))

;*---------------------------------------------------------------------*/
;*    *fake-thread* ...                                                */
;*---------------------------------------------------------------------*/
(define *fake-thread* #f)

;*---------------------------------------------------------------------*/
;*    nothread-scheduler-get-fake-thread ...                           */
;*---------------------------------------------------------------------*/
(define (nothread-scheduler-get-fake-thread proc)
   (unless (isa? *fake-thread* thread)
      (set! *fake-thread* (instantiate::scdthread (body proc))))
   *fake-thread*)

;*---------------------------------------------------------------------*/
;*    scheduler-stat ::nothread-scheduler ...                          */
;*---------------------------------------------------------------------*/
(define-method (scheduler-stat scd::nothread-scheduler)
   "")

;*---------------------------------------------------------------------*/
;*    stage ...                                                        */
;*---------------------------------------------------------------------*/
(define-method (stage scd::nothread-scheduler thread proc::procedure . args)
   (apply proc scd thread args))

;*---------------------------------------------------------------------*/
;*    spawn ...                                                        */
;*---------------------------------------------------------------------*/
(define-method (spawn scd::nothread-scheduler proc::procedure . args)
   (with-access::scdthread *fake-thread* (onerror)
      (set! onerror #f))
   (apply stage scd *fake-thread* proc args))

;*---------------------------------------------------------------------*/
;*    spawn0 ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn0 scd::nothread-scheduler proc::procedure)
   (with-access::scdthread *fake-thread* (onerror)
      (set! onerror #f))
   (stage scd *fake-thread* proc))

;*---------------------------------------------------------------------*/
;*    spawn1 ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn1 scd::nothread-scheduler proc::procedure a0)
   (with-access::scdthread *fake-thread* (onerror)
      (set! onerror #f))
   (stage scd *fake-thread* proc a0))

;*---------------------------------------------------------------------*/
;*    spawn2 ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn2 scd::nothread-scheduler proc::procedure a0 a1)
   (with-access::scdthread *fake-thread* (onerror)
      (set! onerror #f))
   (stage scd *fake-thread* proc a0 a1))

;*---------------------------------------------------------------------*/
;*    spawn3 ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn3 scd::nothread-scheduler proc::procedure a0 a1 a2)
   (with-access::scdthread *fake-thread* (onerror)
      (set! onerror #f))
   (stage scd *fake-thread* proc a0 a1 a2))

;*---------------------------------------------------------------------*/
;*    spawn4 ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn4 scd::nothread-scheduler proc::procedure a0 a1 a2 a3)
   (with-access::scdthread *fake-thread* (onerror)
      (set! onerror #f))
   (stage scd *fake-thread* proc a0 a1 a2 a3))

;*---------------------------------------------------------------------*/
;*    spawn5 ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn5 scd::nothread-scheduler proc::procedure a0 a1 a2 a3 a4)
   (with-access::scdthread *fake-thread* (onerror)
      (set! onerror #f))
   (stage scd *fake-thread* proc a0 a1 a2 a3 a4))

