;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-json.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul  9 07:28:34 2019                          */
;*    Last change :  Tue Jul  9 07:35:20 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript JSON functions              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-json

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

   (export (j2s-json-builtin-method fun::J2SAccess args
	      expr mode return::procedure conf)))

;*---------------------------------------------------------------------*/
;*    j2s-json-builtin-method ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-json-builtin-method fun::J2SAccess args expr mode return conf)
   (with-access::J2SAccess fun (loc obj field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (cond
	       ((string=? val "stringify")
		(case (length args)
		   ((1)
		    `(js-json-stringify ,(j2s-scheme obj mode return conf)
			,(j2s-scheme (car args) mode return conf)
			(js-undefined)
			(js-undefined)
			%this))
		   ((2)
		    `(js-json-stringify ,(j2s-scheme obj mode return conf)
			,(j2s-scheme (car args) mode return conf)
			,(j2s-scheme (cadr args) mode return conf)
			(js-undefined)
			%this))
		   ((3)
		    `(js-json-stringify ,(j2s-scheme obj mode return conf)
			,(j2s-scheme (car args) mode return conf)
			,(j2s-scheme (cadr args) mode return conf)
			,(j2s-scheme (caddr args) mode return conf)
			%this))
		   (else
		    #f)))
	       (else
		#f))))))
