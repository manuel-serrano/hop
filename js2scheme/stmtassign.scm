;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/stmtassign.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 14:30:38 2013                          */
;*    Last change :  Fri Jan  8 15:05:39 2016 (serrano)                */
;*    Copyright   :  2013-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript stmt->assignment                                      */
;*    -------------------------------------------------------------    */
;*    This module implements a pass that transform a statement into    */
;*    an assignment. It is used to compile attribute tilde statements. */
;*    -------------------------------------------------------------    */
;*    The caller is responsible of declaring the assigned var.         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_stmtassign

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils)

   (export j2s-stmt-assign-stage
	   (generic j2s-stmt-assign ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-stmt-assign-stage ...                                        */
;*---------------------------------------------------------------------*/
(define j2s-stmt-assign-stage
   (instantiate::J2SStageProc
      (name "stmt-assign")
      (comment "Transform statements into assignments.")
      (proc j2s-stmt-assign)))

;*---------------------------------------------------------------------*/
;*    j2s-stmt-assign ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (j2s-stmt-assign this args)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-stmt-assign ::J2SProgram ...                                 */
;*---------------------------------------------------------------------*/
(define-method (j2s-stmt-assign this::J2SProgram args)
   (with-access::J2SProgram this (headers decls nodes)
      (for-each (lambda (o) (assign! o args)) headers)
      (for-each (lambda (o) (assign! o args)) decls)
      (for-each (lambda (o) (assign! o args)) nodes))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-stmt-assign ::J2SStmt ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-stmt-assign this::J2SSeq args)
   (with-access::J2SSeq this (nodes loc)
      (for-each (lambda (o) (assign! o args)) nodes)
      this))

;*---------------------------------------------------------------------*/
;*    assign! ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (assign! this::J2SNode temp)
   (default-walk! this temp))

;*---------------------------------------------------------------------*/
;*    assign! ::J2SDecl ...                                            */
;*---------------------------------------------------------------------*/
(define-method (assign! this::J2SDecl temps)
   this)

;*---------------------------------------------------------------------*/
;*    assign! ::J2SSeq ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (assign! this::J2SSeq temp)
   (with-access::J2SSeq this (nodes)
      (let ((l (last-pair nodes)))
	 (set-car! l (assign! (car l) temp))))
   this)

;*---------------------------------------------------------------------*/
;*    assign! ::J2SExpr ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (assign! this::J2SExpr temp)
   (with-access::J2SExpr this (loc)
      (let* ((decl (instantiate::J2SDecl
		      (id temp)
		      (loc loc)))
	     (ref (instantiate::J2SRef
		     (loc loc)
		     (decl decl)))
	     (assig (instantiate::J2SAssig
		       (loc loc)
		       (lhs ref)
		       (rhs this))))
	 (instantiate::J2SParen
	    (loc loc)
	    (expr (instantiate::J2SSequence
		     (loc loc)
		     (exprs (list assig ref))))))))
   
;*---------------------------------------------------------------------*/
;*    assign! ::J2SLabel ...                                           */
;*    -------------------------------------------------------------    */
;*    The JavaScript syntax is ambiguous. Blocks label and object      */
;*    literals share the same syntax. They are distinguished by        */
;*    the context. As the transformation stmt-assign, assumes that     */
;*    the statement is used as an expression, blocks label have to     */
;*    be replaced with object literals.                                */
;*---------------------------------------------------------------------*/
(define-walk-method (assign! this::J2SLabel temp)
   (with-access::J2SLabel this (id body loc)
      (if (isa? body J2SStmtExpr)
	  (with-access::J2SStmtExpr body (expr)
	     (let* ((name (instantiate::J2SString
			     (loc loc)
			     (val (symbol->string id))))
		    (init (instantiate::J2SDataPropertyInit
			     (loc loc)
			     (name name)
			     (val expr)))
		    (obj (instantiate::J2SObjInit
			    (loc loc)
			    (inits (list init)))))
		(assign! obj temp)))
	  (default-walk! this temp))))
