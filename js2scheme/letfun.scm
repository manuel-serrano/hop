;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/letfun.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 28 06:35:14 2015                          */
;*    Last change :  Fri Jan 31 08:04:38 2020 (serrano)                */
;*    Copyright   :  2015-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Let function optimisation                                        */
;*    -------------------------------------------------------------    */
;*    Rewrite the following pattern:                                   */
;*       var f = function() { ... }                                    */
;*    info                                                             */
;*       function f() { ... }                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_letfun

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_lexer
	   __js2scheme_alpha)

   (export j2s-letfun-stage))

;*---------------------------------------------------------------------*/
;*    j2s-letun-stage ...                                              */
;*---------------------------------------------------------------------*/
(define j2s-letfun-stage
   (instantiate::J2SStageProc
      (name "letfun")
      (comment "Implicit function declarations")
      (proc j2s-letfun)
      (optional :optim-letfun)))

;*---------------------------------------------------------------------*/
;*    j2s-letfun ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-letfun this args)
   (when (isa? this J2SProgram)
      (letfun! this args)))

;*---------------------------------------------------------------------*/
;*    letfun! ...                                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (letfun! this::J2SNode args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    letfun! ::J2SFun ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (letfun! this::J2SFun args)
   (call-default-walker)
   (with-access::J2SFun this (body)
      (if (isa? body J2SBlock)
	  (multiple-value-bind (vars nodes)
	     (block-collect-vars body)
	     (multiple-value-bind (inits rest)
		(nodes-collect-inits nodes)
		(let ((inits (filter (lambda (i) (init-fun? i vars)) inits)))
		   (when (pair? inits)
		      (let* ((odecls (map decloinit inits))
			     (ndecls (map declninit inits))
			     (noinits (filter (lambda (v)
						 (not (memq v odecls)))
					 vars))
			     (nblock (duplicate::J2SBlock body
					(nodes rest))))
			 (set! body 
			    (duplicate::J2SBlock body
			       (nodes (append
					 (map (lambda (n)
						 (decl-alpha n odecls ndecls))
					    noinits)
					 (map (lambda (n)
						 (decl-alpha n odecls ndecls))
					    ndecls)
					 (list (j2s-alpha nblock
						  odecls
						  ndecls)))))))))))))
   this)

;*---------------------------------------------------------------------*/
;*    decl-alpha ...                                                   */
;*---------------------------------------------------------------------*/
(define (decl-alpha d::J2SDecl olds news)
   (when (isa? d J2SDeclInit)
      (with-access::J2SDeclInit d (val)
	 (set! val (j2s-alpha val olds news))))
   d)

;*---------------------------------------------------------------------*/
;*    decloinit ...                                                    */
;*---------------------------------------------------------------------*/
(define (decloinit this::J2SInit)
   (with-access::J2SInit this (lhs rhs)
      (with-access::J2SRef lhs (decl)
	 decl)))

;*---------------------------------------------------------------------*/
;*    declninit ...                                                    */
;*---------------------------------------------------------------------*/
(define (declninit this::J2SInit)
   (with-access::J2SInit this (lhs rhs)
      (with-access::J2SRef lhs (decl)
	 (if (isa? rhs J2SFun)
	     (with-access::J2SDecl decl (id writable scope usage binder utype vtype loc)
		(instantiate::J2SDeclFun
		   (loc loc)
		   (id id)
		   (writable writable)
		   (scope scope)
		   (usage usage)
		   (binder binder)
		   (utype utype)
		   (vtype vtype)
		   (val rhs)))
	     (duplicate::J2SDeclInit decl
		(key (ast-decl-key))
		(val rhs))))))

;*---------------------------------------------------------------------*/
;*    init-fun? ...                                                    */
;*---------------------------------------------------------------------*/
(define (init-fun? this::J2SInit vars)
   (with-access::J2SInit this (lhs rhs)
      (when (isa? rhs J2SFun)
	 (with-access::J2SRef lhs (decl)
	    (memq decl vars)))))

;*---------------------------------------------------------------------*/
;*    block-collect-vars ...                                           */
;*---------------------------------------------------------------------*/
(define (block-collect-vars node::J2SBlock)
   (with-access::J2SBlock node (nodes)
      (let loop ((nodes nodes)
		 (vars '()))
	 (cond
	    ((null? nodes)
	     (values (reverse! vars) '()))
	    ((isa? (car nodes) J2SDecl)
	     (loop (cdr nodes) (cons (car nodes) vars)))
	    (else
	     (values (reverse! vars) nodes))))))

;*---------------------------------------------------------------------*/
;*    nodes-collect-inits ...                                          */
;*---------------------------------------------------------------------*/
(define (nodes-collect-inits nodes::pair-nil)
   
   (define (init node)
      (when (isa? node J2SStmtExpr)
	 (with-access::J2SStmtExpr node (expr)
	    (when (isa? expr J2SInit)
	       (with-access::J2SInit expr (rhs)
		  (when (or (isa? rhs J2SFun) (isa? rhs J2SLiteral))
		     expr))))))
   
   (let loop ((nodes nodes)
	      (inits '()))
      (cond
	 ((null? nodes)
	  (values (reverse! inits) '()))
	 ((isa? (car nodes) J2SLetBlock)
	  (values (reverse! inits) nodes))
	 ((isa? (car nodes) J2SSeq)
	  (with-access::J2SSeq (car nodes) ((bnodes nodes))
	     (loop (append bnodes (cdr nodes)) inits)))
	 ((init (car nodes))
	  =>
	  (lambda (init)
	     (loop (cdr nodes) (cons init inits))))
	 (else
	  (values (reverse! inits) nodes)))))

	 
   
