;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-symbol.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 25 17:49:28 2017                          */
;*    Last change :  Mon Sep 27 14:17:41 2021 (serrano)                */
;*    Copyright   :  2017-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript symbol functions            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-symbol

   (include "ast.sch"
	    "context.sch")
   
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

   (export (j2s-symbol-object-get obj::J2SExpr field::J2SExpr
	      mode return conf)))

;*---------------------------------------------------------------------*/
;*    j2s-symbol-object-get ...                                        */
;*    -------------------------------------------------------------    */
;*    Read-only symbol properties.                                     */
;*---------------------------------------------------------------------*/
(define (j2s-symbol-object-get obj field mode return ctx)
   (when (isa? field J2SString)
      (with-access::J2SString field (val)
	 (cond
	    ((string=? val "iterator")
	     '(with-access::JsGlobalObject %this (js-symbol-iterator)
	       js-symbol-iterator))
	    ((string=? val "toStringTag")
	     '(with-access::JsGlobalObject %this (js-symbol-tostringtag)
	       js-symbol-tostringtag))
	    ((string=? val "species")
	     '(with-access::JsGlobalObject %this (js-symbol-species)
	       js-symbol-species))
	    ((string=? val "unscopables")
	     '(with-access::JsGlobalObject %this (js-symbol-unscopables)
	       js-symbol-unscopables))
	    ((string=? val "hasInstance")
	     '(with-access::JsGlobalObject %this (js-symbol-hasinstance)
	       js-symbol-hasinstance))
	    (else #f)))))

