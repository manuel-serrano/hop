;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/make_lib.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug  9 14:00:32 2013                          */
;*    Last change :  Tue Mar  3 19:21:15 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
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
	   __nodejs_uv
	   (JsSlowBuffer __nodejs__buffer)
	   (JsFastBuffer __nodejs__buffer))
   
   (eval  (class JsSlowBuffer)
          (class JsFastBuffer)
      
          (export-all)))
