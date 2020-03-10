;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-date.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  5 05:47:06 2017                          */
;*    Last change :  Tue Mar 10 16:09:42 2020 (serrano)                */
;*    Copyright   :  2017-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript Date functions.             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-date
   
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
   
   (export (j2s-date-new-method fun::J2SNew field args
	      mode return::procedure conf)
	   (j2s-date-builtin-method fun::J2SAccess args
	      expr mode return::procedure conf)))

;*---------------------------------------------------------------------*/
;*    j2s-date-new-method ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-date-new-method clazz field args mode return::procedure conf)
   (when (isa? field J2SString)
      (with-access::J2SString field (val)
	 (cond
	    ((string=? val "getTime")
	     `(llong->flonum (/llong (current-microseconds) #l1000)))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    j2s-date-builtin-method ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-date-builtin-method fun::J2SAccess args expr mode return conf)
   (with-access::J2SAccess fun (loc obj field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (cond
	       ((string=? val "now")
		(case (length args)
		   ((0)
		    '(js-date-now))
		   (else
		    #f)))
	       (else
		#f))))))


