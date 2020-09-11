;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-process.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  5 05:47:06 2017                          */
;*    Last change :  Fri Sep 11 11:10:58 2020 (serrano)                */
;*    Copyright   :  2017-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript Process functions.          */
;*    -------------------------------------------------------------    */
;*    The main purpose of these functions is to reduce the size of     */
;*    generated code.                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-process
   
   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_scheme
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-fun)
   
   (export (j2s-process-builtin-method fun::J2SAccess args
	      expr mode return::procedure conf)))

;*---------------------------------------------------------------------*/
;*    j2s-process-builtin-method ...                                   */
;*---------------------------------------------------------------------*/
(define (j2s-process-builtin-method fun::J2SAccess args expr mode return ctx)
   (with-access::J2SAccess fun (loc obj field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (cond
	       ((string=? val "exit")
		(case (length args)
		   ((0)
		    '(nodejs-process-exit !process (js-undefined) %this))
		   ((1)
		    `(nodejs-process-exit !process
			,(j2s-scheme (car args) mode return ctx)
			%this))
		   (else
		    #f)))
		(else
		 #f))))))


