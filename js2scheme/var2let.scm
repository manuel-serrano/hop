;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/var2let.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 20 14:59:28 2021                          */
;*    Last change :  Mon Sep 20 19:22:08 2021 (serrano)                */
;*    Copyright   :  2021 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    An optimization that transforms global vars into global lets.    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_var2let

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_lexer
	   __js2scheme_alpha)

   (export j2s-var->let-stage))

;*---------------------------------------------------------------------*/
;*    j2s-var->let-stage ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-var->let-stage
   (instantiate::J2SStageProc
      (name "var2let")
      (comment "Var->Let tranformation")
      (proc j2s-var->let)
      (optional :optim-var2let)))

;*---------------------------------------------------------------------*/
;*    j2s-var->let ...                                                 */
;*    -------------------------------------------------------------    */
;*    Warning, headers are not scanned for variable resolution!        */
;*---------------------------------------------------------------------*/
(define (j2s-var->let this conf)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (decls nodes headers)
	 ;; transform var into let for global variables initialized
	 ;; at the beginning of nodes
	 (let ((news (vars->lets! this)))
	    (when (pair? news)
	       (j2s-alpha this (map car news) (map cdr news))
	       (init->assign! this (map cdr news))))))
   this)
	 
;*---------------------------------------------------------------------*/
;*    vars->lets ...                                                   */
;*---------------------------------------------------------------------*/
(define (vars->lets!::pair-nil this::J2SProgram)
   (with-access::J2SProgram this (decls nodes)
      (let loop ((nodes nodes)
		 (ndecls '()))
	 (if (pair? nodes)
	     (cond
		((inits? (car nodes))
		 (with-access::J2SSeq (car nodes) (nodes)
		    (let liip ((inits nodes)
			       (ndecls ndecls))
		       (if (null? inits)
			   (loop (cdr nodes) ndecls)
			   (with-access::J2SStmtExpr (car inits) (expr)
			      (with-access::J2SInit expr (lhs rhs loc)
				 (with-access::J2SRef lhs (decl)
				    (with-access::J2SDecl decl (scope)
				       (if (and (memq scope '(global %scope))
						(simple-expr? rhs))
					   (let ((ds (var->let! this decl rhs)))
					      (set-car! inits (J2SNop))
					      (liip (cdr inits)
						 (append ds ndecls)))
					   ndecls)))))))))
		((simple-stmt? (car nodes))
		 (loop (cdr nodes) ndecls))
		(else
		 ndecls))
	     ndecls))))

;*---------------------------------------------------------------------*/
;*    var->let! ...                                                    */
;*---------------------------------------------------------------------*/
(define (var->let!::pair-nil this::J2SProgram decl::J2SDecl expr::J2SExpr)
   (with-access::J2SProgram this (decls)
      (let loop ((decls decls)
		 (ndecls '()))
	 (if (eq? (car decls) decl)
	     (let ((ndecl (duplicate::J2SDeclInit decl
			     (key (ast-decl-key))
			     (binder 'let-opt)
			     (val expr))))
		(cons (cons decl ndecl) ndecls))
	     (loop (cdr decls) ndecls)))))

;*---------------------------------------------------------------------*/
;*    init->assign! ::J2SNode ...                                      */
;*    -------------------------------------------------------------    */
;*    Replacement possible remaining initializations (for variables    */
;*    declared several times) into assignments.                        */
;*---------------------------------------------------------------------*/
(define-walk-method (init->assign! this::J2SNode decls)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    init->assign! ::J2SInit ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (init->assign! this::J2SInit decls)
   (with-access::J2SInit this (lhs)
      (if (not (isa? lhs J2SRef))
	  this
	  (with-access::J2SRef lhs (decl)
	     (if (memq decl decls)
		 (duplicate::J2SAssig this)
		 this)))))

;*---------------------------------------------------------------------*/
;*    simple-stmt? ...                                                 */
;*---------------------------------------------------------------------*/
(define (simple-stmt? this::J2SStmt)
   (cond
      ((isa? this J2SNop)
       #t)
      ((isa? this J2SStmtExpr)
       (with-access::J2SStmtExpr this (expr)
	  (simple-expr? expr)))
      ((isa? this J2SSeq)
       (with-access::J2SSeq this (nodes)
	  (every simple-stmt? nodes)))
      ((isa? this J2SIf)
       (with-access::J2SIf this (test then else)
	  (when (simple-expr? test)
	     (and (simple-stmt? then) (simple-stmt? else)))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    simple-argument? ...                                             */
;*---------------------------------------------------------------------*/
(define (simple-argument? this::J2SExpr)
   (define (simple-literal?::bool this::J2SExpr)
      (or (isa? this J2SNull)
	  (isa? this J2SUndefined)
	  (and (isa? this J2SLiteralValue)
	       (not (isa? this J2SRegExp))
	       (not (isa? this J2SCmap)))
	  (isa? this J2SLiteralCnst)))
   
   (define (simple-callee? this)
      (when (isa? this J2SRef)
	 (with-access::J2SRef this (decl)
	    (isa? decl J2SDeclExtern))))
   
   (define (simple-call? this)
      (with-access::J2SCall this (fun args thisarg)
	 (and (simple-callee? fun)
	      (every simple-argument? args)
	      (every simple-argument? thisarg))))
   
   (define (simple-new? this)
      (with-access::J2SNew this (clazz args)
	 (and (simple-callee? clazz)
	      (every simple-argument? args))))
   
   (or (simple-literal? this)
       (isa? this J2SRef)
       (isa? this J2SGlobalRef)
       (when (isa? this J2SCall) (simple-call? this))
       (when (isa? this J2SNew) (simple-new? this))))

;*---------------------------------------------------------------------*/
;*    simple-expr? ...                                                 */
;*---------------------------------------------------------------------*/
(define (simple-expr? this::J2SExpr)
   (or (simple-argument? this)
       (isa? this J2SFun)))

;*---------------------------------------------------------------------*/
;*    inits? ...                                                       */
;*---------------------------------------------------------------------*/
(define (inits? this::J2SNode)
   (when (isa? this J2SSeq)
      (with-access::J2SSeq this (nodes)
	 (every (lambda (s)
		   (when (isa? s J2SStmtExpr)
		      (with-access::J2SStmtExpr s (expr)
			 (when (isa? expr J2SInit)
			    expr))))
	    nodes))))

