;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/cast.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 18:13:46 2016                          */
;*    Last change :  Wed May  9 12:47:30 2018 (serrano)                */
;*    Copyright   :  2016-18 Manuel Serrano                            */
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
;*    AST. After this stage, the AST is well typed and the only        */
;*    implicit coercions left are in ++ and += operators.              */
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
      (j2s-cast-program! this args)
      this))

;*---------------------------------------------------------------------*/
;*    j2s-cast-program! ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-cast-program! this::J2SProgram args)
   (with-access::J2SProgram this (decls nodes)
      (set! decls (map! (lambda (n) (type-cast! n 'any)) decls))
      (set! nodes (map! (lambda (n) (type-cast! n 'any)) nodes))
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
   (not (or (eq? type totype)
	    (eq? totype '*)
	    (and (eq? totype 'any) (memq type *any-types*)))))

(define *any-types*
   '(undefined null bool integer number object function string real))

;*---------------------------------------------------------------------*/
;*    cast-expr ...                                                    */
;*---------------------------------------------------------------------*/
(define (cast-expr expr::J2SExpr type::symbol totype::symbol)
   (if (need-cast? type totype)
       (with-access::J2SExpr expr (loc)
	  (J2SCast totype expr))
       expr))
   
;*---------------------------------------------------------------------*/
;*    cast ...                                                         */
;*---------------------------------------------------------------------*/
(define (cast expr::J2SExpr totype::symbol)
   (cast-expr expr (j2s-vtype expr) totype))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (type-cast! this::obj totype::symbol)
   (if (pair? this)
       (map! (lambda (o) (type-cast! o totype)) this)
       this))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SNode totype)
   
   (define (is-stmt? type)
      (cond
	 ((not (class? type)) #f)
	 ((eq? type J2SStmt) #t)
	 ((eq? type J2SExpr) #f)
	 ((eq? type object) #f)
	 (else (is-stmt? (class-super type)))))

   (define (is-expr? type)
      (cond
	 ((not (class? type)) #f)
	 ((eq? type J2SStmt) #f)
	 ((eq? type J2SExpr) #t)
	 ((eq? type object) #f)
	 (else (is-expr? (class-super type)))))
      
   (let* ((clazz (object-class this))
	  (fields (class-all-fields clazz)))
      ;; instance fields
      (let loop ((i (-fx (vector-length fields) 1)))
	 (when (>=fx i 0)
	    (let* ((f (vector-ref-ur fields i))
		   (info (class-field-info f)))
	       (when (and (pair? info) (member "ast" info))
		  (let ((v ((class-field-accessor f) this)))
		     (cond
			((is-stmt? (class-field-type f))
			 (let ((nv (type-cast! v totype)))
			    (when (class-field-mutator f)
			       ((class-field-mutator f) this nv))))
			((is-expr? (class-field-type f))
			 (let ((nv (type-cast! v '*)))
			    (when (class-field-mutator f)
			       ((class-field-mutator f) this nv))))
			(else
			 (let ((nv (type-cast! v '*)))
			    (when (class-field-mutator f)
			       ((class-field-mutator f) this nv)))))))
	       (loop (-fx i 1)))))
      this))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SExpr ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SExpr totype)
   (cast (call-next-method) totype))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SArrayAbsent ...                                  */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SArrayAbsent totype)
   this)

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SRef ...                                          */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SRef totype)
   (cast this totype))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SParen ...                                        */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SParen totype)
   (with-access::J2SParen this (expr type)
      (set! expr (type-cast! expr type))
      (cast this totype)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SSequence ...                                     */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SSequence totype)
   (with-access::J2SSequence this (exprs type)
      (let loop ((exprs exprs))
	 (cond
	    ((null? exprs)
	     (cast this totype))
	    ((null? (cdr exprs))
	     (set-car! exprs (type-cast! (car exprs) type))
	     (cast this totype))
	    (else
	     (set-car! exprs (type-cast! (car exprs) '*))
	     (loop (cdr exprs)))))))
	     
;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SFun totype)
   (with-access::J2SFun this (body rtype)
      (set! body (type-cast! body rtype))
      (cast this totype)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SArray ...                                        */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SArray totype)
   (with-access::J2SArray this (exprs)
      (set! exprs (map! (lambda (e) (type-cast! e 'any)) exprs))
      (cast this totype)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SCast ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SCast totype)
   
   (define (optimize-cast? totype type)
      (or (and (eq? totype 'bool) (memq type '(int32 uint32 integer number)))))

   (with-access::J2SCast this (expr type)
      (if (optimize-cast? totype type)
	  (begin
	     (set! type totype)
	     this)
	  (cast expr totype))))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SCall totype)
   
   (define (known-fun this fun::J2SFun)
      (with-access::J2SCall this (args)
	 (with-access::J2SFun fun (rtype params vararg)
	    (let loop ((params params)
		       (vals args)
		       (nvals '()))
	       (cond
		  ((null? vals)
		   (set! args (reverse! nvals))
		   (cast-expr this rtype totype))
		  ((null? params)
		   (loop params '()
		      (append (reverse
				 (map (lambda (v)
					 (type-cast! v 'any))
				    vals))
			 nvals)))
		  (else
		   (with-access::J2SDecl (car params) (vtype)
		      (let ((ptype (if (and (eq? vtype 'array)
					    (null? (cdr params))
					    vararg)
				       'any
				       vtype)))
			 (loop (cdr params) (cdr vals)
			    (cons (type-cast! (car vals) ptype)
			       nvals))))))))))
   
   (define (unknown-fun this)
      (with-access::J2SCall this (args fun)
	 (set! fun (type-cast! fun '*))
	 (set! args (map! (lambda (a) (type-cast! a 'any)) args))
	 (cast-expr this 'any totype)))
   
   (with-access::J2SCall this (fun)
      (cond
	 ((isa? fun J2SFun)
	  (set! fun (type-cast! fun '*))
	  (known-fun this fun))
	 ((isa? fun J2SRef)
	  (with-access::J2SRef fun (decl)
	     (cond
		((isa? decl J2SDeclFun)
		 (with-access::J2SDeclFun decl (val)
		    (if (isa? val J2SFun)
			(known-fun this val)
			(with-access::J2SMethod val (function method)
			   (known-fun this function)))))
		(else
		 (unknown-fun this)))))
	 (else
	  (unknown-fun this)))))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SNew ...                                          */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SNew totype)
   (with-access::J2SNew this (args)
      (set! args (map! (lambda (a) (type-cast! a 'any)) args))
      (cast this totype)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SThrow ...                                        */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SThrow totype)
   (with-access::J2SThrow this (expr)
      (set! expr (type-cast! expr 'any))
      this))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SReturn ...                                       */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SReturn totype)
   (with-access::J2SReturn this (expr from)
      (with-access::J2SExpr expr (loc type)
	 (cond
	    ((isa? from J2SFun)
	     (with-access::J2SFun from (rtype)
		(set! expr (type-cast! expr rtype))))
	    ((isa? from J2SExpr)
	     (with-access::J2SExpr from (type)
		(set! expr (type-cast! expr type))))
	    (else
	     (tprint "PAS FROM: " (j2s->list this))
	     (set! expr (type-cast! expr '*))))
	 this)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SReturnYield ...                                  */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SReturnYield totype)
   (with-access::J2SReturnYield this (expr kont)
      (set! kont (type-cast! kont '*))
      (set! expr (type-cast! expr 'any))
      this))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SDeclInit ...                                     */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SDeclInit totype)
   (with-access::J2SDeclInit this (val vtype id)
      (set! val (type-cast! val vtype))
      this))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SStmtExpr ...                                     */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SStmtExpr totype)
   (with-access::J2SStmtExpr this (expr)
      (set! expr (type-cast! expr '*))
      this))
   
;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SAssig ...                                        */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SAssig totype)
   (with-access::J2SAssig this (lhs rhs type loc)
      (cond
	 ((eq? (j2s-vtype lhs) type)
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs (type-cast! rhs type))
	  (cast this totype))
	 ((eq? totype '*)
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs
	     (type-cast! rhs
		(if (or (isa? lhs J2SRef) (isa? lhs J2SUnresolvedRef))
		    (j2s-vtype lhs)
		    'any)))
	  this)
	 (else
	  (let* ((id (gensym 'assig))
		 (tr (j2s-type rhs))
		 (d (J2SLetOpt/vtype tr '(get) id (type-cast! rhs tr))))
	     (set! rhs
		(type-cast! (J2SRef d :type tr)
		   (if (or (isa? lhs J2SRef)
			   (isa? lhs J2SUnresolvedRef))
		       (j2s-vtype lhs)
		       'any)))
	     (let ((tyb (if (eq? totype '*) type totype)))
		(J2SBindExit/type tyb #f 
		   (J2SLetRecBlock #f  (list d)
		      (J2SStmtExpr this)
		      (type-cast! (J2SRef d :type tr) tyb)))))))))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SPrefix ...                                       */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SPrefix totype)
   (with-access::J2SAssig this (lhs rhs type loc)
      (set! lhs (type-cast! lhs '*))
      (set! rhs (type-cast! rhs '*))
      (cast this totype)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SPostfix ...                                      */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SPostfix totype)
   (with-access::J2SAssig this (lhs rhs type loc)
      (set! lhs (type-cast! lhs '*))
      (set! rhs (type-cast! rhs '*))
      (cast this totype)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SAssigOp ...                                      */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SAssigOp totype)
   (with-access::J2SAssigOp this (op lhs rhs)
      (case op
	 ((>> <<)
;* 	  (if (memq (j2s-vtype rhs) '(int32 uint32))                */
;* 	      (begin                                                   */
;* 		 (set! lhs (type-cast! lhs 'int32))                    */
;* 		 (set! rhs (type-cast! rhs 'uint32)))                  */
;* 	      (begin                                                   */
;* 		 (set! lhs (type-cast! lhs '*))                        */
;* 		 (set! rhs (type-cast! rhs '*))))                      */
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs (type-cast! rhs 'uint32))
	  (cast this totype))
	 ((>>>)
;* 	  (if (memq (j2s-vtype rhs) '(int32 uint32))                */
;* 	      (begin                                                   */
;* 		 (set! lhs (type-cast! lhs 'uint32))                   */
;* 		 (set! rhs (type-cast! rhs 'uint32))                   */
;* 	      (begin                                                   */
;* 		 (set! lhs (type-cast! lhs '*))                        */
;* 		 (set! rhs (type-cast! rhs '*)))))                     */
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs (type-cast! rhs 'uint32))
	  (cast this totype))
	 ((^ & BIT_OR)
;* 	  (if (memq (j2s-vtype rhs) '(int32 uint32))                */
;* 	      (begin                                                   */
;* 		 (set! lhs (type-cast! lhs 'int32))                    */
;* 		 (set! rhs (type-cast! rhs 'int32)))                   */
;* 	      (begin                                                   */
;* 		 (set! lhs (type-cast! lhs '*))                        */
;* 		 (set! rhs (type-cast! rhs '*))))                      */
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs (type-cast! rhs 'int32))
	  (cast this totype))
	 (else
	  (set! lhs (type-cast! lhs '*))
	  (if (eq? (j2s-type lhs) 'string)
	      (set! rhs (type-cast! rhs 'string))
	      (set! rhs (type-cast! rhs '*)))
	  (cast this totype)))))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SAccess ...                                       */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SAccess totype)
   (with-access::J2SAccess this (obj field)
      (set! obj (type-cast! obj '*))
      (set! field (type-cast! field '*))
      (cast this totype)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SUnary ...                                        */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SUnary totype)
   (with-access::J2SUnary this (op expr type)
      (if (and (eq? op '~) (eq? type 'int32))
	  (set! expr (type-cast! expr 'int32))
	  (set! expr (type-cast! expr '*)))
      (cast this totype)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SBinary ...                                       */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SBinary totype)
   (with-access::J2SBinary this (op lhs rhs type hint)
      (case op
	 ((>> <<)
	  (if (eq? type 'int32)
	      (begin
		 (set! lhs (type-cast! lhs 'int32))
		 (set! rhs (type-cast! rhs 'uint32)))
	      (begin 
		 (set! lhs (type-cast! lhs '*))
		 (set! rhs (type-cast! rhs '*))))
	  (cast this totype))
	 ((>>>)
	  (if (eq? type 'uint32)
	      (begin
		 (set! lhs (type-cast! lhs 'uint32))
		 (set! rhs (type-cast! rhs 'uint32))
	      (begin 
		 (set! lhs (type-cast! lhs '*))
		 (set! rhs (type-cast! rhs '*)))))
	  (cast this totype))
	 ((^ & BIT_OR)
	  (if (eq? type 'int32)
	      (begin
		 (set! lhs (type-cast! lhs 'int32))
		 (set! rhs (type-cast! rhs 'int32)))
	      (begin 
		 (set! lhs (type-cast! lhs '*))
		 (set! rhs (type-cast! rhs '*))))
	  (cast this totype))
	 ((OR &&)
	  (set! lhs (type-cast! lhs totype))
	  (set! rhs (type-cast! rhs totype))
	  this)
	 (else
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs (type-cast! rhs '*))
	  (cast this totype)))))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SLoop ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SLoop totype)
   (with-access::J2SLoop this (body)
      (set! body (type-cast! body totype))
      this))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SFor ...                                          */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SFor totype)
   (with-access::J2SFor this (init test incr body)
      (set! init (type-cast! init '*))
      (set! test (type-cast! test 'bool))
      (set! incr (type-cast! incr '*))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SWhile ...                                        */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SWhile totype)
   (with-access::J2SWhile this (test body)
      (set! test (type-cast! test 'bool))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SIf ...                                           */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SIf totype)
   (with-access::J2SIf this (test then else)
      (set! test (type-cast! test 'bool))
      (set! then (type-cast! then totype))
      (set! else (type-cast! else totype))
      this))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SCond ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SCond totype)
   (with-access::J2SCond this (test then else type)
      (set! test (type-cast! test 'bool))
      (set! then (type-cast! then type))
      (set! else (type-cast! else type))
      (cast this totype)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SDataPropertyInit ...                             */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SDataPropertyInit totype)
   (with-access::J2SDataPropertyInit this (val)
      (set! val (type-cast! val 'any))
      this))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SDProducer ...                                    */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SDProducer totype)
   (with-access::J2SDProducer this (expr)
      (if (eq? totype 'object)
	  ;; only handle object cast, array cast will be handled
	  ;; in the code generation of producer (see scheme.scm)
	  (set! expr (type-cast! expr totype))
	  (set! expr (type-cast! expr '*)))
      this))
