;*=====================================================================*/
;*    serrano/prgm/project/hop/1.10.x/src/nothread-scheduler.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 22 14:28:00 2008                          */
;*    Last change :  Wed Nov 19 11:37:26 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    NOTHREAD scheduler                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_scheduler-nothread
   
   (import hop_scheduler)

   (include "stage.sch")

   (cond-expand
      (enable-threads
       (library pthread)))
   
   (export (class nothread-scheduler::row-scheduler)
	   (nothread-scheduler-get-fake-thread)))

;*---------------------------------------------------------------------*/
;*    *fake-thread* ...                                                */
;*---------------------------------------------------------------------*/
(define *fake-thread* #f)

;*---------------------------------------------------------------------*/
;*    nothread-scheduler-get-fake-thread ...                           */
;*---------------------------------------------------------------------*/
(define (nothread-scheduler-get-fake-thread)
   (unless (thread? *fake-thread*)
      (set! *fake-thread* (instantiate::hopthread (body list))))
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
   (unless (thread? *fake-thread*)
      (set! *fake-thread* (instantiate::hopthread (body list))))
   (hopthread-onerror-set! *fake-thread* #f)
   (apply stage scd *fake-thread* proc args))

;*---------------------------------------------------------------------*/
;*    spawn0 ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn0 scd::nothread-scheduler proc::procedure)
   (unless (thread? *fake-thread*)
      (set! *fake-thread* (instantiate::hopthread (body list))))
   (hopthread-onerror-set! *fake-thread* #f)
   (stage scd *fake-thread* proc))

;*---------------------------------------------------------------------*/
;*    spawn1 ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn1 scd::nothread-scheduler proc::procedure a0)
   (unless (thread? *fake-thread*)
      (set! *fake-thread* (instantiate::hopthread (body list))))
   (hopthread-onerror-set! *fake-thread* #f)
   (stage scd *fake-thread* proc a0))

;*---------------------------------------------------------------------*/
;*    spawn2 ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn2 scd::nothread-scheduler proc::procedure a0 a1)
   (unless (thread? *fake-thread*)
      (set! *fake-thread* (instantiate::hopthread (body list))))
   (hopthread-onerror-set! *fake-thread* #f)
   (stage scd *fake-thread* proc a0 a1))

;*---------------------------------------------------------------------*/
;*    spawn3 ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn3 scd::nothread-scheduler proc::procedure a0 a1 a2)
   (unless (thread? *fake-thread*)
      (set! *fake-thread* (instantiate::hopthread (body list))))
   (hopthread-onerror-set! *fake-thread* #f)
   (stage scd *fake-thread* proc a0 a1 a2))

;*---------------------------------------------------------------------*/
;*    spawn4 ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn4 scd::nothread-scheduler proc::procedure a0 a1 a2 a3)
   (unless (thread? *fake-thread*)
      (set! *fake-thread* (instantiate::hopthread (body list))))
   (hopthread-onerror-set! *fake-thread* #f)
   (stage scd *fake-thread* proc a0 a1 a2 a3))

;*---------------------------------------------------------------------*/
;*    spawn5 ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn5 scd::nothread-scheduler proc::procedure a0 a1 a2 a3 a4)
   (unless (thread? *fake-thread*)
      (set! *fake-thread* (instantiate::hopthread (body list))))
   (hopthread-onerror-set! *fake-thread* #f)
   (stage scd *fake-thread* proc a0 a1 a2 a3 a4))

