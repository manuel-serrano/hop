;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-object.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr  3 14:42:40 2020                          */
;*    Last change :  Fri Apr  3 17:15:12 2020 (serrano)                */
;*    Copyright   :  2020-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript Object functions            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-object

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
	   __js2scheme_scheme-fun
	   __js2scheme_scheme-cast
	   __js2scheme_scheme-ops)

   (export (j2s-object-builtin-method fun::J2SAccess args
	      expr mode return::procedure conf)
	   (j2s-object-isfrozen obj args mode return ctx)))

;*---------------------------------------------------------------------*/
;*    j2s-object-builtin-method ...                                    */
;*---------------------------------------------------------------------*/
(define (j2s-object-builtin-method fun::J2SAccess args expr mode return conf)
   
   (define (cast-object sexp)
      (with-access::J2SCall expr (args)
	 (j2s-cast sexp expr (j2s-type (car args)) (j2s-type expr) conf)))
   
   (with-access::J2SAccess fun (loc obj field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (cond
	       ((string=? val "getOwnPropertyDescriptor")
		(when (=fx (length args) 2)
		   (cast-object
		      `(js-get-own-property-descriptor
			  ,(j2s-scheme (car args) mode return conf)
			  ,(j2s-scheme (cadr args) mode return conf)
			  %this))))
	       ((string=? val "keys")
		(when (=fx (length args) 1)
		   `(js-ownkeys ,(j2s-scheme (car args) mode return conf)
		       %this)))
	       (else
		#f))))))

;*---------------------------------------------------------------------*/
;*    j2s-object-isfrozen ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-object-isfrozen obj args mode return ctx)
   `(js-object-isfrozen
       ,@(map (lambda (a) (j2s-scheme a mode return ctx)) args)
       %this))
