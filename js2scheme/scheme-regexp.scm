;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-regexp.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 25 17:49:28 2017                          */
;*    Last change :  Thu Mar 28 15:57:16 2019 (serrano)                */
;*    Copyright   :  2017-19 Manuel Serrano                            */
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

   (export (j2s-regexp-test obj args mode return conf)))

;*---------------------------------------------------------------------*/
;*    j2s-regexp-test ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-regexp-test obj args mode return conf)
   (when (isa? obj J2SLiteralCnst)
      (with-access::J2SLiteralCnst obj (index val)
	 `(js-regexp-literal-test (js-cnst-table-ref ,index)
	     ,@(map (lambda (arg)
		       (j2s-scheme arg mode return conf))
		  args)))))
