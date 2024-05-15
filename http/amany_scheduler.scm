;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/http/amany_scheduler.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb 26 07:03:15 2008                          */
;*    Last change :  Tue May 14 09:22:10 2024 (serrano)                */
;*    Copyright   :  2008-24 Manuel Serrano                            */
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
(module __http_scheduler-accept-many

   (library pthread)
   
   (import  __http_scheduler
	    __http_scheduler-pool)

   (export  (class accept-many-scheduler::pool-scheduler)))

		   
		   
