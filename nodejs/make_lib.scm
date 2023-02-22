;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/nodejs/make_lib.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug  9 14:00:32 2013                          */
;*    Last change :  Wed Feb 22 14:39:10 2023 (serrano)                */
;*    Copyright   :  2013-23 Manuel Serrano                            */
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
	   __nodejs__hop
	   __nodejs__buffer
	   __nodejs__fs)
   
   (eval  (class JsSlowBuffer)
          (class JsFastBuffer)
      
          (export-all)))
