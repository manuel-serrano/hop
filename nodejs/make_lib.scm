;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/make_lib.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug  9 14:00:32 2013                          */
;*    Last change :  Thu Oct 23 09:39:56 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    THe module used to build the nodejs heap file.                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs_makelib

   (library hopscript)
   
   (import __nodejs
	   __nodejs_require
	   __nodejs_process
	   __nodejs_repl
	   __nodejs_uv)
;* 	   (nodejs-event-loop __nodejs_uv)                             */
;* 	   (nodejs-event-loop-alive? __nodejs_uv))                     */
   
   (eval   (export-all)))
