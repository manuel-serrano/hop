;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-regexp.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 25 17:49:28 2017                          */
;*    Last change :  Thu Mar 28 15:57:16 2019 (serrano)                */
;*    Copyright   :  2017-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript regexp functions            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-regexp

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

   (export (j2s-new-regexp ::J2SNew mode return::procedure ctx)
	   (j2s-regexp-test obj args mode return conf)))

;*---------------------------------------------------------------------*/
;*    j2s-new-regexp ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-new-regexp this::J2SNew mode return::procedure ctx)
   (with-access::J2SNew this (args)
      (when (and (pair? args) (null? (cdr args)))
	 (let ((prog (context-program ctx))
	       (pat (j2s-scheme (car args) mode return ctx)))
	    (if prog
		(with-access::J2SProgram prog (rxcache-size)
		   (let ((idx rxcache-size))
		      (set! rxcache-size (+fx rxcache-size 1))
		      `(js-new-regexp1/cache %this ,pat
			  (vector-ref __js_rxcaches ,idx))))
		`(js-new-regexp1 %this ,pat))))))
 
;*---------------------------------------------------------------------*/
;*    j2s-regexp-test ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-regexp-test obj args mode return ctx)
   (if (isa? obj J2SLiteralCnst)
       (with-access::J2SLiteralCnst obj (index val)
	  `(js-regexp-literal-test (js-cnst-table-ref ,index)
	      ,@(map (lambda (arg)
			(j2s-scheme arg mode return ctx))
		   args)))
       (with-access::J2SLiteralCnst obj (index val)
	  `(js-regexp-test ,(j2s-scheme obj mode return ctx)
	      ,@(map (lambda (arg)
			(j2s-scheme arg mode return ctx))
		   args)))))
