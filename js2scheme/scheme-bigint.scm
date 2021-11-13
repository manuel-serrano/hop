;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-bigint.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  5 05:47:06 2017                          */
;*    Last change :  Sat Nov 13 09:03:06 2021 (serrano)                */
;*    Copyright   :  2017-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript Bigint functions.           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-bigint

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

   (export (j2s-bigint-builtin-method fun::J2SAccess args
	      expr mode return::procedure conf)))

;*---------------------------------------------------------------------*/
;*    j2s-bigint-builtin-method ...                                    */
;*---------------------------------------------------------------------*/
(define (j2s-bigint-builtin-method fun::J2SAccess args expr mode return conf)
   (with-access::J2SAccess fun (loc obj field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (cond
	       ((string=? val "asIntN")
		(when (=fx (length args) 2)
		   (cond
		      ((eq? (car args) 'uint32)
		       `(js-bigint-asintn
			   ,(uint32->fixnum
			       (j2s-scheme (car args) mode return conf))
			   ,(j2s-scheme (cadr args) mode return conf)
			   %this))
		      ((eq? (car args) 'int32)
		       `(js-bigint-asintn
			   ,(int32->fixnum
			       (j2s-scheme (car args) mode return conf))
			   ,(j2s-scheme (cadr args) mode return conf)
			   %this))
		      ((eq? (car args) 'int53)
		       `(js-bigint-asintn
			   ,(j2s-scheme (car args) mode return conf)
			   ,(j2s-scheme (cadr args) mode return conf)
			   %this))
		      (else
		       `(js-bigint-asintn
			   ,(j2s-scheme (car args) mode return conf)
			   ,(j2s-scheme (cadr args) mode return conf)
			   %this)))))
	       ((string=? val "asUintN")
		(when (=fx (length args) 2)
		   (cond
		      ((eq? (car args) 'uint32)
		       `(js-bigint-asuintn
			   ,(uint32->fixnum
			       (j2s-scheme (car args) mode return conf))
			   ,(j2s-scheme (cadr args) mode return conf)
			   %this))
		      ((eq? (car args) 'int32)
		       `(js-bigint-asuintn
			   ,(int32->fixnum
			       (j2s-scheme (car args) mode return conf))
			   ,(j2s-scheme (cadr args) mode return conf)
			   %this))
		      ((eq? (car args) 'int53)
		       `(js-bigint-asuintn
			   ,(j2s-scheme (car args) mode return conf)
			   ,(j2s-scheme (cadr args) mode return conf)
			   %this))
		      (else
		       `(js-bigint-asuintn
			   ,(j2s-scheme (car args) mode return conf)
			   ,(j2s-scheme (cadr args) mode return conf)
			   %this)))))
	       (else
		#f))))))

