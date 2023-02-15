;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-promise.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 15 08:35:39 2023                          */
;*    Last change :  Wed Feb 15 09:16:10 2023 (serrano)                */
;*    Copyright   :  2023 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript promise functions.          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-promise
   
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
   
   (export (j2s-new-promise ::J2SNew mode return::procedure ctx)))

;*---------------------------------------------------------------------*/
;*    j2s-new-promise ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-new-promise this::J2SNew mode return::procedure ctx)
   
   (define (j2s-scheme-box o mode return ctx)
      (let ((t (j2s-type o)))
	 (box (j2s-scheme o mode return ctx) t ctx)))
   
   (with-access::J2SNew this (args)
      (when (and (pair? args) (null? (cdr args)))
	 (cond
	    ((isa? (car args) J2SFun)
	     (with-access::J2SFun (car args) (params)
		(if (eq? (length params) 2)
		    `(js-new-promise/procedure %this
			,(jsfun->lambda (car args) mode return ctx #f))
		    `(js-new-promise %this
			,(j2s-scheme-box (car args) mode return ctx)))))
	    (else
	     `(js-new-promise %this
		 ,(j2s-scheme-box (car args) mode return ctx)))))))

