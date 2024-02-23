;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/nodejs/make_lib.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug  9 14:00:32 2013                          */
;*    Last change :  Fri Feb 23 08:40:17 2024 (serrano)                */
;*    Copyright   :  2013-24 Manuel Serrano                            */
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
	   __nodejs__process
	   __nodejs_repl
	   __nodejs_uv
	   __nodejs_napi
	   __nodejs__hop
	   __nodejs__buffer
	   __nodejs__fs)
   
   (eval  (class JsSlowBuffer)
          (class JsFastBuffer)
      
          (export-all)))
