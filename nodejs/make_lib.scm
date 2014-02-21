;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/nodejs/make_lib.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug  9 14:00:32 2013                          */
;*    Last change :  Sun Oct  6 08:22:56 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
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
	   __nodejs_repl)
   
   (eval   (export-all)))
