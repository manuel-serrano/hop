;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/hopsched/make_lib.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug  9 14:00:32 2013                          */
;*    Last change :                                                    */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    THe module used to build the hopsched heap file.                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopsched_makelib
   
   (library hop)
   
   (import hopsched_accept
	   hopsched_pipeline
	   hopsched_scheduler
	   hopsched_scheduler-nothread
	   hopsched_scheduler-accept-many
	   hopsched_scheduler-one-to-one
	   hopsched_scheduler-queue
	   hopsched_scheduler-pool)
   
   (eval   (export-all)))
