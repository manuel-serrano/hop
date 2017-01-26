;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/cast.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 18:13:46 2016                          */
;*    Last change :  Sun Jan 22 16:54:26 2017 (serrano)                */
;*    Copyright   :  2016-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Type casts introduction                                          */
;*    -------------------------------------------------------------    */
;*    This stage introduces explicit type cast coercions on            */
;*    numerical expressions.                                           */
;*                                                                     */
;*    The RANGE stage has annotated the AST with minimal type          */
;*    information. For each expression, these MTI stands for the       */
;*    minimal value set this expression might have. It does not        */
;*    denote compiler types!                                           */
;*                                                                     */
;*    The CAST stage uses these informations to produce a well typed   */
;*    AST. After this stage, the AST is well typed and no implicit     */
;*    coercions is needed.                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_cast

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_type-hint)

   (export j2s-cast-stage))

;*---------------------------------------------------------------------*/
;*    j2s-cast-stage ...                                               */
;*---------------------------------------------------------------------*/
(define j2s-cast-stage
   (instantiate::J2SStageProc
      (name "cast")
      (comment "Type casts")
      (proc j2s-cast!)))

;*---------------------------------------------------------------------*/
;*    j2s-cast! ::obj ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-cast! this args)
   (when (isa? this J2SProgram)
      (when (config-get args :optim-cast #f)
	 ;; same optim condition as the RANGE stage
	 (j2s-cast-program! this args))
      this))

;*---------------------------------------------------------------------*/
;*    j2s-cast-program! ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-cast-program! this::J2SProgram args)
   (with-access::J2SProgram this (decls nodes)
      (for-each (lambda (n) (type-cast! n 'any #f)) decls)
      (for-each (lambda (n) (type-cast! n 'any #f)) nodes)
      this))

;*---------------------------------------------------------------------*/
;*    any? ...                                                         */
;*---------------------------------------------------------------------*/
(define (any? type)
   (or (eq? type 'any) (eq? type 'unknown)))

;*---------------------------------------------------------------------*/
;*    need-cast? ...                                                   */
;*---------------------------------------------------------------------*/
(define (need-cast? type totype)

   (define (jstype? type)
      (memq type '(integer object function array string undefined)))
   
   (cond
      ((eq? type totype) #f)
      ((eq? totype 'unknown) #f)
      ((and (eq? totype 'any) (jstype? type)) #f)
      ((and (any? type) (any? totype)) #f)
      ((and (eq? type 'int30) (eq? totype 'integer)) #f)
      (else #t)))

;*---------------------------------------------------------------------*/
;*    cast ...                                                         */
;*---------------------------------------------------------------------*/
(define-generic (cast expr::J2SExpr totype)
   (with-access::J2SExpr expr (type loc)
      (if (need-cast? type totype)
	  (J2SCast totype expr)
	  expr)))

;*---------------------------------------------------------------------*/
;*    cast ::J2SNumber ...                                             */
;*---------------------------------------------------------------------*/
(define-method (cast this::J2SNumber totype)
   (with-access::J2SNumber this (val type)
      (set! type totype)
      this))

;*---------------------------------------------------------------------*/
;*    cast ::J2SCast ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cast this::J2SCast totype)
   (with-access::J2SCast this (expr)
      (cast expr totype)))
      
;*---------------------------------------------------------------------*/
;*    cast-any ...                                                     */
;*---------------------------------------------------------------------*/
(define (cast-any this::J2SExpr)
   (cast this 'any))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SNode totype fun)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SRef ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SRef totype fun)
   (with-access::J2SRef this (loc decl type)
      (with-access::J2SDecl decl (utype)
	 (if (need-cast? type utype)
	     (let ((totype type))
		(set! type utype)
		(cast this totype))
	     this))))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SAccess ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SAccess totype fun)
   (with-access::J2SAccess this (obj field)
      (set! obj (cast (type-cast! obj totype fun) 'any))
      (set! field (type-cast! field totype fun))
      this))
							   
;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SAssig ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SAssig totype fun)
   (with-access::J2SAssig this (lhs rhs type loc)
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (with-access::J2SDecl decl (utype)
		(set! rhs (type-cast! rhs totype fun))
		(with-access::J2SExpr rhs ((rtype type) loc)
		   (when (need-cast? rtype utype)
		      (set! rhs (cast rhs utype))))
		(if (need-cast? type utype)
		    (let ((totype type))
		       (set! type utype)
		       (cast this totype))
		    this)))
	  (begin
	     (set! lhs (type-cast! lhs totype fun))
	     (set! rhs (cast (type-cast! rhs totype fun) 'any))
	     this))))
		
;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SCall totype fun)
   
   (define (known-fun this fun)
      (with-access::J2SCall this (args)
	 (with-access::J2SFun fun (rtype params)
	    (let loop ((params params)
		       (vals args)
		       (nvals '()))
	       (cond
		  ((null? vals)
		   (set! args (reverse! nvals))
		   this)
		  ((null? params)
		   (loop params '()
		      (append (reverse (map cast-any vals)) nvals)))
		  (else
		   (with-access::J2SDecl (car params) (utype)
		      (loop (cdr params) (cdr vals)
			 (cons (cast (car vals) utype) nvals)))))))))
   
   (define (unknown-fun this)
      (with-access::J2SCall this (args)
	 (set! args (map! cast-any args)))
      this)
   
   (call-default-walker)
   
   (with-access::J2SCall this (fun)
      (cond
	 ((isa? fun J2SFun)
	  (known-fun this fun))
	 ((isa? fun J2SRef)
	  (with-access::J2SRef fun (decl)
	     (cond
		((isa? decl J2SDeclFunCnst)
		 (with-access::J2SDeclFunCnst decl (val)
		    (known-fun this val)))
		((isa? decl J2SDeclFun)
		 (with-access::J2SDeclFun decl (ronly val)
		    (known-fun this val)))
		(else
		 (unknown-fun this)))))
	 (else
	  (unknown-fun this)))))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SReturn ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SReturn totype fun)
   (call-default-walker)
   (with-access::J2SReturn this (expr)
      (with-access::J2SExpr expr (loc type)
	 (if fun
	     (with-access::J2SFun fun (rtype)
		(set! expr (cast expr rtype)))
	     (set! expr (cast expr 'any)))
	 this)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SFun totype fun)
   (with-access::J2SFun this (body)
      (type-cast! body 'void this)
      this))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SBinary ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SBinary totype fun)
   
   (define (set-hint! this tlhs trhs)
      (with-access::J2SBinary this (hint)
	 (cond
	    ((or (type-integer? tlhs) (type-integer? trhs))
	     (set! hint '(integer)))
	    ((or (eq? tlhs 'string) (eq? trhs 'string))
	     (set! hint '(string)))
	    (else
	     (set! hint '())))))
   
   (call-default-walker)
   (with-access::J2SBinary this (op lhs rhs type hint)
      (let ((tlhs (j2s-type lhs))
	    (trhs (j2s-type rhs)))
	 (case op
	    ((+ - * / %)
	     (let ((tym (max-type type (max-type tlhs trhs))))
		(set! lhs (cast lhs tym))
		(set! rhs (cast rhs tym))
		(when (memq tym '(any number)) (set-hint! this tlhs trhs))
		this))
	    ((< <= > >= == === != !==)
	     (let ((tym (max-type tlhs trhs)))
		(set! lhs (cast lhs tym))
		(set! rhs (cast rhs tym))
		(when (memq tym '(any number)) (set-hint! this tlhs trhs))
		this))
	    (else
	     (set! lhs (cast lhs 'any))
	     (set! rhs (cast rhs 'any))
	     this)))))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SStmtExpr ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SStmtExpr totype fun)
   (with-access::J2SStmtExpr this (expr)
      (let ((nexpr (type-cast! expr totype fun)))
	 (if (isa? nexpr J2SCast)
	     (with-access::J2SCast nexpr ((castexpr expr))
		(set! expr castexpr))
	     (set! expr nexpr)))
      this))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SDeclInit ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SDeclInit totype fun)
   (with-access::J2SDeclInit this (val utype)
      (set! val (cast (type-cast! val utype fun) utype))
      this))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SObjInit ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SObjInit totype fun)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SPragma ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SPragma totype fun)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SPragma ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SPragma totype fun)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SNew ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SNew totype fun)
   (with-access::J2SNew this (args clazz)
      (set! args (map! (lambda (a) (cast (type-cast! a totype fun) 'any)) args))
      (set! clazz (cast (type-cast! clazz totype fun) 'any))
      this))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SExpr ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SExpr totype fun)
   (cond
      ((or (isa? this J2SObjInit)
	   (isa? this J2SPragma)
	   (isa? this J2SSequence)
	   (isa? this J2SUnary)
	   (isa? this J2SParen))
       (call-default-walker))
      ((or (isa? this J2SThis)
	   (isa? this J2SLiteral)
	   (isa? this J2SHopRef)
	   (isa? this J2SUnresolvedRef))
       #unspecified)
      (else
       (tprint "TBD " (typeof this))
       (call-default-walker)))
   this)
