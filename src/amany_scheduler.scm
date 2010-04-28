;*=====================================================================*/
;*    serrano/prgm/project/hop/1.10.x/src/amany-scheduler.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb 26 07:03:15 2008                          */
;*    Last change :  Wed Nov 19 11:32:55 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Accept-many scheduler                                            */
;*    -------------------------------------------------------------    */
;*    A mere variation around the pool-scheduler. Here the accept      */
;*    is not executed withing the threads but inside the main          */
;*    loop which tries to accept as many as possible connections       */
;*    at a time.                                                       */
;*    -------------------------------------------------------------    */
;*    See the accept implementation to understand the difference.      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_scheduler-accept-many

   (cond-expand
      (enable-threads
       (library pthread)))
   
   (library hop)
   
   (import  hop_scheduler
	    hop_param
	    hop_scheduler-pool)

   (export  (class accept-many-scheduler::pool-scheduler)))

		   
		   
