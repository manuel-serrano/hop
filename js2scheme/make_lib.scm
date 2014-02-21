;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/js2scheme/make_lib.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug  9 14:00:32 2013                          */
;*    Last change :  Sun Oct  6 08:50:41 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    THe module used to build the js2scheme heap file.                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_makelib

   (import __js2scheme_compile
	   __js2scheme_parser
	   __js2scheme_ast
	   __js2scheme_header
	   __js2scheme_symbol
	   __js2scheme_return
	   __js2scheme_ronly
	   __js2scheme_dump
	   __js2scheme_scheme
	   __js2scheme_stage)
   
   (eval   (class J2SStage)

           (class J2SProgram)
      
           (export-all)))
