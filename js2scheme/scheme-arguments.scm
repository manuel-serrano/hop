;*=====================================================================*/
;*    .../prgm/project/hop/3.2.x/js2scheme/scheme-arguments.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  5 05:47:06 2017                          */
;*    Last change :  Tue May  1 15:33:34 2018 (serrano)                */
;*    Copyright   :  2017-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript arguments functions.        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-arguments

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_array
	   __js2scheme_scheme
	   __js2scheme_scheme-cast
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-fun)

   (export (j2s-arguments-ref ::J2SAccess mode return conf)
	   (j2s-arguments-set! ::J2SAssig mode return conf)))

;*---------------------------------------------------------------------*/
;*    j2s-arguments-ref ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-arguments-ref this::J2SAccess mode return conf)
   (with-access::J2SAccess this (obj field type)
      (cond
	 ((maybe-number? field)
	  (if (eq? (j2s-vtype field) 'uint32)
	      `(js-arguments-index-ref ,(j2s-scheme obj mode return conf)
		  ,(j2s-scheme field mode return conf)
		  %this)
	      `(js-arguments-ref ,(j2s-scheme obj mode return conf)
		  ,(j2s-scheme field mode return conf)
		  %this)))
	 ((j2s-field-length? field)
	  `(js-arguments-length
	      ,(j2s-scheme obj mode return conf) %this))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    j2s-arguments-set! ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-arguments-set! this::J2SAssig mode return conf)
   (tprint "NOT IMPLEMENTED YET"))

