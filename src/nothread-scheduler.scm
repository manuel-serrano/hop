;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/src/nothread-scheduler.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 22 14:28:00 2008                          */
;*    Last change :  Fri Feb 29 06:28:32 2008 (serrano)                */
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
;*    schedule ::nothread-scheduler ...                                */
;*    -------------------------------------------------------------    */
;*    The simplest possible scheduler that schedule a single thread!   */
;*---------------------------------------------------------------------*/
(define-method (schedule scd::nothread-scheduler proc msg)
   (with-handler
      scheduler-default-handler
      (proc scd #unspecified)))
