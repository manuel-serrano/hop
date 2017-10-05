;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-string.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  5 05:47:06 2017                          */
;*    Last change :  Thu Oct  5 10:54:08 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript string functions.           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-string

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_scheme
	   __js2scheme_scheme-utils)

   (export (j2s-jsstring-replace-regexp obj args mode return conf hint totype)
	   (j2s-jsstring-replace obj args mode return conf hint totype)))

;*---------------------------------------------------------------------*/
;*    j2s-string-replace-regexp ...                                    */
;*---------------------------------------------------------------------*/
(define (j2s-jsstring-replace-regexp obj args mode return conf hint totype)
   
   (define (literal-regexp obj)
      (when (isa? obj J2SLiteralCnst)
	 (with-access::J2SLiteralCnst obj (val index)
	    (when (isa? val J2SRegExp)
	       index))))
   
   (cond
      ((literal-regexp (car args))
       =>
       (lambda (idx)
	  (let ((tmp (gensym 'obj)))
	     `(let ((,tmp ,(j2s-scheme obj mode return conf hint totype)))
		 (with-access::JsRegExp (vector-ref-ur %cnsts ,idx) (rx)
		    (js-jsstring-replace-regexp ,tmp rx
		       0 #t
		       ,@(map (lambda (arg)
				 (j2s-scheme arg mode return conf hint totype))
			    (cdr args))))))))
      (else
       (let ((tmp (gensym 'obj))
	     (regexp (j2s-scheme (car args) mode return conf hint totype)))
	  `(let ((,tmp ,(j2s-scheme obj mode return conf hint totype)))
	      (with-access::JsRegExp ,regexp (rx)
		 (js-jsstring-replace-regexp ,tmp ,regexp
		    0 #t
		    ,@(map (lambda (arg)
			      (j2s-scheme arg mode return conf hint totype))
			 (cdr args)))))))))
	   
;*---------------------------------------------------------------------*/
;*    j2s-string-replace ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-jsstring-replace obj args mode return conf hint totype)
   `(js-jsstring-replace
       ,(j2s-scheme obj mode return conf hint totype)
       ,@(map (lambda (arg)
		 (j2s-scheme arg mode return conf hint totype))
	    args)))
	   

