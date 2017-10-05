;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/cast.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 18:13:46 2016                          */
;*    Last change :  Thu Oct  5 07:52:22 2017 (serrano)                */
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
      (memq type
	 '(null integer object function array string undefined
	   fixnum ufixnum number uint29 int29 int53 regexp)))
   
   (cond
      ((eq? type totype) #f)
      ((eq? totype 'unknown) (memq type '(index uint32 uint29 length)))
      ((and (eq? totype 'any) (jstype? type)) #f)
      ((and (any? type) (any? totype)) #f)
      ((and (memq type '(int29 uint29 int30 int53)) (memq totype '(integer number int30))) #f)
      ((and (eq? type 'any) (eq? totype 'void)) #f)
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
      (when (need-cast? type totype)
	 (set! type totype))
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
      (with-access::J2SDecl decl (vtype id)
	 (if (need-cast? vtype totype)
	     (cast this totype)
	     this))))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SAccess ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SAccess totype fun)
   (with-access::J2SAccess this (obj field)
      (set! obj (type-cast! obj 'any fun))
      (set! field (type-cast! field 'any fun))
      (if (need-cast? 'any totype)
	  (cast this totype)
	  this)))
							   
;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SCacheCheck ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SCacheCheck totype fun)
   (with-access::J2SCacheCheck this (obj fields)
      (set! obj (type-cast! obj 'any fun))
      (set! fields (map (lambda (f) (type-cast! f 'any fun)) fields))
      (if (need-cast? 'any totype)
	  (cast this totype)
	  this)))
							   
;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SAssig ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SAssig totype fun)
   (with-access::J2SAssig this (lhs rhs type loc)
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (with-access::J2SDecl decl (vtype)
		(set! rhs (type-cast! rhs vtype fun))
		(if (need-cast? type vtype)
		    (let ((totype type))
		       (set! type vtype)
		       (cast this totype))
		    this)))
	  (begin
	     (set! lhs (type-cast! lhs 'any fun))
	     (set! rhs (type-cast! rhs 'any fun))
	     this))))
		
;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SPrefix ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SPrefix totype fun)
   (with-access::J2SAssig this (lhs rhs type loc)
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (with-access::J2SDecl decl (vtype)
		(set! rhs (type-cast! rhs vtype fun))
		(if (need-cast? type vtype)
		    (let ((totype type))
		       (set! type vtype)
		       (cast this totype))
		    this)))
	  (begin
	     (set! lhs (type-cast! lhs 'any fun))
	     (set! rhs (type-cast! rhs 'any fun))
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
		   (with-access::J2SDecl (car params) (vtype)
		      (loop (cdr params) (cdr vals)
			 (cons (cast (car vals) vtype) nvals)))))))))
   
   (define (unknown-fun this)
      (with-access::J2SCall this (args fun)
	 (set! fun (type-cast! fun 'any fun))
	 (set! args (map! cast-any args)))
      this)
   
   (with-access::J2SCall this (fun)
      (cond
	 ((isa? fun J2SFun)
	  (known-fun this fun))
	 ((isa? fun J2SRef)
	  (with-access::J2SRef fun (decl)
	     (cond
		((isa? decl J2SDeclFun)
		 (with-access::J2SDeclFun decl (val)
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
   
   (with-access::J2SBinary this (op lhs rhs type hint)
      (let ((tlhs (j2s-type lhs))
	    (trhs (j2s-type rhs)))
	 (case op
	    ((+ - * / %)
	     (let ((tym (max-type type (max-type tlhs trhs))))
		(set! lhs (type-cast! lhs tym fun))
		(set! rhs (type-cast! rhs tym fun))
		(when (memq tym '(any number)) (set-hint! this tlhs trhs))
		this))
	    ((< <= > >= == === != !==)
	     (let ((tym (max-type tlhs trhs)))
		(set! lhs (type-cast! lhs tym fun))
		(set! rhs (type-cast! rhs tym fun))
		(when (memq tym '(any number)) (set-hint! this tlhs trhs))
		this))
	    (else
	     (set! lhs (type-cast! lhs 'any fun))
	     (set! rhs (type-cast! rhs 'any fun))
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
   (with-access::J2SDeclInit this (val vtype)
      (set! val (cast (type-cast! val vtype fun) vtype))
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
;*    type-cast! ::J2SNew ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SNew totype fun)
   (with-access::J2SNew this (args clazz)
      (set! args (map! cast-any args))
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
	   (isa? this J2SParen)
	   (isa? this J2SCond)
	   (isa? this J2SMethod)
	   (isa? this J2SClass)
	   (isa? this J2SClassElement))
       (call-default-walker)
       this)
      ((or (isa? this J2SThis)
	   (isa? this J2SLiteral)
	   (isa? this J2SHopRef)
	   (isa? this J2SUnresolvedRef))
       this)
      (else
       (tprint "TBD " (typeof this))
       (call-default-walker)
       this)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SCast ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SCast totype fun)
   (with-access::J2SCast this (loc)
      (J2SCast totype this)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SArray ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SArray totype fun)
   (call-default-walker))


;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SNumber ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SNumber totype fun)
   (with-access::J2SNumber this (loc type val)
      (if (need-cast? type totype)
	  (J2SCast totype this)
	  this)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SSwitch ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SSwitch totype fun)
   (with-access::J2SSwitch this (key cases)
      (set! key (type-cast! key 'any fun))
      (for-each (lambda (c) (type-cast! c 'any fun)) cases)
      this))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SCase ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SCase totype fun)
   (with-access::J2SCase this (expr body)
      (set! expr (type-cast! expr 'any fun))
      (set! body (type-cast! body 'void fun))
      this))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SForIn ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (type-cast! this::J2SForIn totype fun)
   (with-access::J2SForIn this (obj body)
      (set! obj (type-cast! obj 'any obj))
      (set! body (type-cast! body 'void fun))
      this))
      
