;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-test.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug 21 07:41:17 2017                          */
;*    Last change :  Fri Dec 15 08:05:24 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Scheme test code generation                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-test

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

   (export (j2s-test test::J2SExpr mode return conf)))

;*---------------------------------------------------------------------*/
;*    j2s-test ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-test test::J2SExpr mode return conf)
   (let ((ty (j2s-type-ref test)))
      (cond
	 ((eq? ty 'bool)
	  (j2s-bool-test test mode return conf))
	 ((eq? ty 'object)
	  #t)
	 ((eq? ty 'int32)
	  `(not (=s32 ,(j2s-scheme test mode return conf '(bool) 'int32) #s32:0)))
	 ((eq? ty 'uint32)
	  `(not (=u32 ,(j2s-scheme test mode return conf '(bool) 'uint32) #u32:0)))
	 ((is-fixnum? test conf)
	  `(not (=fx ,(j2s-scheme test mode return conf '(bool) 'any) 0)))
	 ((type-number? ty)
	  `(not (= ,(j2s-scheme test mode return conf '(bool) 'any) 0)))
	 ((notbool-expr? test)
	  `(js-toboolean ,(j2s-scheme test mode return conf '(bool) 'any)))
	 (else
	  `(js-totest ,(j2s-scheme test mode return conf '(bool) 'any))))))
   
;*---------------------------------------------------------------------*/
;*    j2s-bool-test ::J2SNode ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-bool-test this::J2SNode mode return conf)
   (j2s-scheme this mode return conf '(bool) 'bool))

;*---------------------------------------------------------------------*/
;*    j2s-bool-test ::J2SExpr ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-bool-test this::J2SExpr mode return conf)
   (with-access::J2SExpr this (type)
      (if (eq? type 'bool)
	  (j2s-scheme this mode return conf '(bool) 'bool)
	  (j2s-test this mode return conf))))

;*---------------------------------------------------------------------*/
;*    j2s-bool-test ::J2SBinary ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-bool-test this::J2SBinary mode return conf)
   (with-access::J2SBinary this (op lhs rhs loc)
      (case op
	 ((&&)
	  (epairify loc
	     `(and ,(j2s-test lhs mode return conf)
		   ,(j2s-test rhs mode return conf))))
	 ((OR)
	  (epairify loc
	     `(or ,(j2s-test lhs mode return conf)
		  ,(j2s-test rhs mode return conf))))
	 (else
	  (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    j2s-bool-test ::J2SUnary ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-bool-test this::J2SUnary mode return conf)
   (with-access::J2SUnary this (op expr loc)
      (case op
	 ((!)
	  (epairify loc (js-not (j2s-bool-test expr mode return conf))))
	 (else
	  (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    notbool-expr? ...                                                */
;*---------------------------------------------------------------------*/
(define (notbool-expr? this::J2SNode)
   (let ((ty (j2s-type this)))
      (and (symbol? ty) (not (eq? ty 'bool) )
	   (not (eq? ty 'obj))
	   (not (eq? ty 'any)))))

