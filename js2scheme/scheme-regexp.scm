;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-regexp.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 25 17:49:28 2017                          */
;*    Last change :  Tue Dec 26 06:54:51 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript regexp functions            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-regexp

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

   (export (j2s-regexp-test obj args mode return conf hint)))

;*---------------------------------------------------------------------*/
;*    j2s-regexp-test ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-regexp-test obj args mode return conf hint)
   (when (isa? obj J2SLiteralCnst)
      (with-access::J2SLiteralCnst obj (index val)
	 `(js-regexp-literal-test (vector-ref-ur %cnsts ,index)
	     ,@(map (lambda (arg)
		       (j2s-scheme arg mode return conf hint))
		  args)))))
