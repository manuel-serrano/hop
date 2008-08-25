;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/src/nothread-scheduler.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 22 14:28:00 2008                          */
;*    Last change :  Fri Aug 22 14:50:45 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    NOTHREAD scheduler                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_scheduler-nothread
   
   (import hop_scheduler)

   (cond-expand
      (enable-threads
       (library pthread)))
   
   (export (class nothread-scheduler::scheduler)))

;*---------------------------------------------------------------------*/
;*    scheduler-stat ::nothread-scheduler ...                          */
;*---------------------------------------------------------------------*/
(define-method (scheduler-stat scd::nothread-scheduler)
   "")

;*---------------------------------------------------------------------*/
;*    *dummy-thread* ...                                               */
;*---------------------------------------------------------------------*/
(define *dummy-thread* #f)

;*---------------------------------------------------------------------*/
;*    spawn ...                                                        */
;*---------------------------------------------------------------------*/
(define-method (spawn scd::scheduler proc::procedure . args)
   (unless (thread? *dummy-thread*)
      (set! *dummy-thread* (instantiate::hopthread (body list))))
   (with-handler
      (make-scheduler-error-handler *dummy-thread*)
      (apply stage scd proc args)))

;*---------------------------------------------------------------------*/
;*    spawn0 ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn0 scd::scheduler proc::procedure)
   (unless (thread? *dummy-thread*)
      (set! *dummy-thread* (instantiate::hopthread (body list))))
   (with-handler
      (make-scheduler-error-handler *dummy-thread*)
      (stage0 scd proc)))

;*---------------------------------------------------------------------*/
;*    spawn1 ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn1 scd::scheduler proc::procedure a0)
   (unless (thread? *dummy-thread*)
      (set! *dummy-thread* (instantiate::hopthread (body list))))
   (with-handler
      (make-scheduler-error-handler *dummy-thread*)
      (stage1 scd proc a0)))

;*---------------------------------------------------------------------*/
;*    spawn2 ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn2 scd::scheduler proc::procedure a0 a1)
   (unless (thread? *dummy-thread*)
      (set! *dummy-thread* (instantiate::hopthread (body list))))
   (with-handler
      (make-scheduler-error-handler *dummy-thread*)
      (stage2 scd proc a0 a1)))

;*---------------------------------------------------------------------*/
;*    spawn3 ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn3 scd::scheduler proc::procedure a0 a1 a2)
   (unless (thread? *dummy-thread*)
      (set! *dummy-thread* (instantiate::hopthread (body list))))
   (with-handler
      (make-scheduler-error-handler *dummy-thread*)
      (stage3 scd proc a0 a1 a2)))

;*---------------------------------------------------------------------*/
;*    spawn4 ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (spawn4 scd::scheduler proc::procedure a0 a1 a2 a3)
   (unless (thread? *dummy-thread*)
      (set! *dummy-thread* (instantiate::hopthread (body list))))
   (with-handler
      (make-scheduler-error-handler *dummy-thread*)
      (stage4 scd proc a0 a1 a2 a3)))

;*---------------------------------------------------------------------*/
;*    stage ::nothread-scheduler ...                                   */
;*    -------------------------------------------------------------    */
;*    The simplest possible scheduler that schedule a single thread!   */
;*---------------------------------------------------------------------*/
(define-method (stage scd::nothread-scheduler proc . args)
   (apply proc scd *dummy-thread* args))

;*---------------------------------------------------------------------*/
;*    stage0 ::nothread-scheduler ...                                  */
;*    -------------------------------------------------------------    */
;*    The simplest possible scheduler that schedule a single thread!   */
;*---------------------------------------------------------------------*/
(define-method (stage0 scd::nothread-scheduler proc)
   (proc scd *dummy-thread*))

;*---------------------------------------------------------------------*/
;*    stage1 ::nothread-scheduler ...                                  */
;*    -------------------------------------------------------------    */
;*    The simplest possible scheduler that schedule a single thread!   */
;*---------------------------------------------------------------------*/
(define-method (stage1 scd::nothread-scheduler proc a0)
   (proc scd *dummy-thread* a0))

;*---------------------------------------------------------------------*/
;*    stage2 ::nothread-scheduler ...                                  */
;*    -------------------------------------------------------------    */
;*    The simplest possible scheduler that schedule a single thread!   */
;*---------------------------------------------------------------------*/
(define-method (stage2 scd::nothread-scheduler proc a0 a1)
   (proc scd *dummy-thread* a0 a1))

;*---------------------------------------------------------------------*/
;*    stage3 ::nothread-scheduler ...                                  */
;*    -------------------------------------------------------------    */
;*    The simplest possible scheduler that schedule a single thread!   */
;*---------------------------------------------------------------------*/
(define-method (stage3 scd::nothread-scheduler proc a0 a1 a2)
   (proc scd *dummy-thread* a0 a1 a2))

;*---------------------------------------------------------------------*/
;*    stage4 ::nothread-scheduler ...                                  */
;*    -------------------------------------------------------------    */
;*    The simplest possible scheduler that schedule a single thread!   */
;*---------------------------------------------------------------------*/
(define-method (stage4 scd::nothread-scheduler proc a0 a1 a2 a3)
   (proc scd *dummy-thread* a0 a1 a2 a3))
