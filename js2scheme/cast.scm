;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/cast.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 18:13:46 2016                          */
;*    Last change :  Sun Nov 10 11:49:47 2024 (serrano)                */
;*    Copyright   :  2016-24 Manuel Serrano                            */
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

   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils)

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
   (unless (type-subtype? type totype)
      (or (and (eq? totype 'any) (memq type '(int32 uint32)))
	  (not (or (and (eq? type 'function) (eq? totype 'arrow))
		   (eq? totype '*)
		   (and (eq? totype 'any) (memq type *any-types*)))))))

(define *any-types*
   '(undefined null bool integer number object function arrow
     string real array regexp arguments class record map weakmap set weakset
     promise))

;*---------------------------------------------------------------------*/
;*    cast-expr ...                                                    */
;*---------------------------------------------------------------------*/
(define (cast-expr expr::J2SExpr type totype)
   (if (need-cast? type totype)
       (with-access::J2SExpr expr (loc)
	  (J2SCast totype expr))
       expr))
   
;*---------------------------------------------------------------------*/
;*    cast ...                                                         */
;*---------------------------------------------------------------------*/
(define (cast expr::J2SExpr totype)
   (cast-expr expr (j2s-type expr) totype))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (type-cast! this::obj totype)
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
;*    -------------------------------------------------------------    */
;*    Variable references might contain an implicit cast that          */
;*    is resolve at the code generation time. For instance,            */
;*    a variable of type "any" referenced as a "uint32" will not       */
;*    yield to introduce an explicit cast. I will remain as is.        */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SRef totype)
   (with-access::J2SRef this (type)
      (cast-expr this type totype)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SWithRef ...                                      */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SWithRef totype)
   (with-access::J2SWithRef this (expr)
      (set! expr (type-cast! expr totype))
      (cast this totype)))

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
;*    type-cast! ::J2SSpread ...                                       */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SSpread totype)
   (with-access::J2SSpread this (expr)
      (set! expr (type-cast! expr 'any))
      (cast this totype)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SCast ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SCast totype)
   
   (define (optimize-cast? totype type)
      (unless (eq? totype '*)
	 (or (and (eq? totype 'bool)
		  (memq type '(int32 uint32 integer number))))))
   
   (with-access::J2SCast this (expr type loc static)
      (type-cast! expr '*)
      (cond
	 (static
	  this)
	 ((optimize-cast? totype type)
	  (set! type totype)
	  this)
	 ((eq? type totype)
	  this)
	 ((need-cast? (j2s-type expr) totype)
	  (J2SCast totype expr))
	 ((need-cast? (j2s-vtype expr) type)
	  this)
	 (else
	  expr))))

;*---------------------------------------------------------------------*/
;*    type-call-cast! ...                                              */
;*---------------------------------------------------------------------*/
(define (type-call-cast! this::J2SExpr fun args totype)
   
   (define (known-fun this fun::J2SFun)
      (with-access::J2SExpr this (type)
	 (with-access::J2SFun fun (rtype params vararg)
	    (let loop ((params params)
		       (vals args)
		       (nvals '()))
	       (cond
		  ((null? vals)
		   (cond
		      ((isa? this J2SCall)
		       (with-access::J2SCall this (args)
			  (set! args (reverse! nvals))))
		      ((isa? this J2SNew)
		       (with-access::J2SNew this (args)
			  (set! args (reverse! nvals))))
		      (else
		       (error "js2scheme" "internal call error"
			  (j2s->sexp this))))
		   (cast-expr this rtype totype))
		  ((null? params)
		   (loop params '()
		      (append (reverse
				 (map (lambda (v)
					 (type-cast! v 'any))
				    vals))
			 nvals)))
		  (else
		   (with-access::J2SDecl (car params) (vtype id)
		      (let ((ptype (if (and (eq? vtype 'array)
					    (null? (cdr params))
					    vararg)
				       'any
				       vtype)))
			 (let ((pt (type-cast! (car vals) ptype)))
			    (loop (cdr params) (cdr vals)
			       (cons pt nvals)))))))))))
   
   (define (unknown-fun this)
      (set! fun (type-cast! fun '*))
      (set! args (map! (lambda (a) (type-cast! a 'any)) args))
      (cast-expr this (j2s-type this) totype))

   (cond
      ((isa? fun J2SFun)
       (set! fun (type-cast! fun '*))
       (known-fun this fun))
      ((isa? fun J2SRef)
       (with-access::J2SRef fun (decl)
	  (cond
	     ((isa? decl J2SDeclFun)
	      (with-access::J2SDeclFun decl (val)
		 (cond
		    ((isa? val J2SFun)
		     (known-fun this val))
		    ((isa? val J2SMethod)
		     (with-access::J2SMethod val (function method)
			(known-fun this function)))
		    (else
		     (error "js2scheme" "Bad declfun value" (j2s->sexp val))))))
	     (else
	      (unknown-fun this)))))
      (else
       (unknown-fun this))))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SCall totype)
   (with-access::J2SCall this (fun args thisargs loc)
      (when (pair? thisargs)
	 (set-car! thisargs (type-cast! (car thisargs) 'any)))
      (type-call-cast! this fun args totype)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SNew ...                                          */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SNew totype)
   (with-access::J2SNew this (clazz args)
      (type-call-cast! this clazz args totype)))

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
   (with-access::J2SStmtExpr this (expr loc)
      (set! expr (type-cast! expr '*))
      this))
   
;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SAssig ...                                        */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SAssig totype)
   
   (define (array-length-assign? lhs)
      (when (and (isa? lhs J2SAccess) (eq? (j2s-type lhs) 'uint32))
	 (with-access::J2SAccess lhs (obj field)
	    (when (eq? (j2s-vtype obj) 'array)
	       (when (isa? field J2SString)
		  (with-access::J2SString field (val)
		     (string=? val "length")))))))
   
   (with-access::J2SAssig this (lhs rhs type loc)
      (cond
	 ((and (isa? lhs J2SRef)
	       (not (isa? this J2SInit))
	       (with-access::J2SRef lhs (decl)
		  (with-access::J2SDecl decl (writable)
		     (not writable))))
	  (set! rhs (type-cast! rhs '*))
	  (cast this totype))
	 ((array-length-assign? lhs)
	  ;; LENGTH is a pseudo-type equivalent to uint32 but that requires
	  ;; a range check when converted
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs (type-cast! rhs 'length))
	  (cast this totype))
	 ((eq? (j2s-vtype lhs) type)
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs (type-cast! rhs type))
	  (cast this totype))
	 ((eq? totype '*)
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs (type-cast! rhs (j2s-vtype lhs)))
	  this)
	 (else
	  ;; MS 10Nov2024: the type cast of assignments has been deeply
	  ;; modified to fix the bug on octane/navier-stokes (see the
	  ;; tests/hopjs/noserv/ecma51.js). This fix also implies a modification
	  ;; in the typing of assignments in tyflow.
	  (set! lhs (type-cast! lhs totype))
	  (set! rhs (type-cast! rhs (j2s-vtype lhs)))
	  this))))

;* 	  (let* ((id (gensym 'assig))                                  */
;* 		 (tr (j2s-type rhs))                                   */
;* 		 (endloc (node-endloc this))                           */
;* 		 (d (J2SLetOpt/vtype tr '(get) id this)))              */
;* 	     (let* ((ret (J2SReturn #t (type-cast! (J2SRef d :type tr) totype))) */
;* 		    (bex (J2SBindExit/type totype #f                   */
;* 			    (J2SLetRecBlock #f (list d)                */
;* 			       ret))))                                 */
;* 		(with-access::J2SReturn ret (from)                     */
;* 		   (set! from bex)                                     */
;* 		   bex)))))))                                          */

(define (type-cast!-TBR-10nov2024 this::J2SAssig totype)

   (define (array-length-assign? lhs)
      (when (and (isa? lhs J2SAccess) (eq? (j2s-type lhs) 'uint32))
	 (with-access::J2SAccess lhs (obj field)
	    (when (eq? (j2s-vtype obj) 'array)
	       (when (isa? field J2SString)
		  (with-access::J2SString field (val)
		     (string=? val "length")))))))
		     
   (with-access::J2SAssig this (lhs rhs type loc)
      (cond
	 ((and (isa? lhs J2SRef)
	       (not (isa? this J2SInit))
	       (with-access::J2SRef lhs (decl)
		  (with-access::J2SDecl decl (writable)
		     (not writable))))
	  (set! rhs (type-cast! rhs '*))
	  (cast this totype))
	 ((array-length-assign? lhs)
	  ;; LENGTH is a pseudo-type equivalent to uint32 but that requires
	  ;; a range check when converted
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs (type-cast! rhs 'length))
	  (cast this totype))
	 ((eq? (j2s-vtype lhs) type)
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs (type-cast! rhs type))
	  (cast this totype))
	 ((eq? totype '*)
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs (type-cast! rhs (j2s-vtype lhs)))
	  this)
	 (else
	  (let* ((id (gensym 'assig))
		 (tr (j2s-type rhs))
		 (endloc (node-endloc this))
		 (d (J2SLetOpt/vtype tr '(get) id (type-cast! rhs tr))))
	     (set! rhs (type-cast! (J2SRef d :type tr) (j2s-vtype lhs)))
	     (let* ((tyb (if (eq? totype '*) type totype))
		    (ret (J2SReturn #t (type-cast! (J2SRef d :type tr) tyb)))
		    (bex (J2SBindExit/type tyb #f 
			    (J2SLetRecBlock #f (list d)
			       (J2SStmtExpr this)
			       ret))))
		(with-access::J2SReturn ret (from)
		   (set! from bex)
		   bex)))))))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SPrefix ...                                       */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SPrefix totype)
   (with-access::J2SAssig this (lhs rhs type loc)
      (let ((lty (j2s-type lhs)))
	 (set! lhs (type-cast! lhs (if (type-number? lty) lty 'number))))
      (with-access::J2SBinary rhs ((rlhs lhs) rhs)
	 (let ((lty (j2s-type rlhs)))
	    (set! rlhs (type-cast! rlhs (if (type-number? lty) lty 'number))))
	 (set! rhs (type-cast! rhs '*)))
      (cast this totype)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SPostfix ...                                      */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SPostfix totype)
   (with-access::J2SAssig this (lhs rhs type loc)
      (let ((lty (j2s-type lhs)))
	 (set! lhs (type-cast! lhs (if (type-number? lty) lty 'number))))
      (with-access::J2SBinary rhs ((rlhs lhs) rhs)
	 (let ((lty (j2s-type rlhs)))
	    (set! rlhs (type-cast! rlhs (if (type-number? lty) lty 'number))))
	 (set! rhs (type-cast! rhs '*)))
      (cast this totype)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SAssigOp ...                                      */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SAssigOp totype)
   (with-access::J2SAssigOp this (op lhs rhs type)
      (case op
	 ((>> <<)
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs (type-cast! rhs 'uint32))
	  (cast this totype))
	 ((>>>)
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs (type-cast! rhs 'uint32))
	  (cast this totype))
	 ((^ & BIT_OR)
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs (type-cast! rhs 'int32))
	  (cast this totype))
	 (else
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs (type-cast! rhs '*))
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
      (cond
	 ((and (eq? op '~) (eq? type 'int32))
	  (set! expr (type-cast! expr 'int32)))
	 (else
	  (set! expr (type-cast! expr '*))))
      (cast this totype)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SBinary ...                                       */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SBinary totype)
   (with-access::J2SBinary this (op lhs rhs type)
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
	 ((OR && OR*)
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs (type-cast! rhs '*))
	  (cast this totype))
	 ((instanceof)
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs (type-cast! rhs 'any))
	  (cast this totype))
	 (else
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs (type-cast! rhs '*))
	  (cast this totype)))))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SLoop ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SLoop totype)
   (with-access::J2SLoop this (body loc)
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
;*    type-cast! ::J2SForIn ...                                        */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SForIn totype)
   (with-access::J2SForIn this (lhs obj body)
      (set! lhs (type-cast! lhs '*))
      (set! obj (type-cast! obj '*))
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
;*    type-cast! ::J2SSwitch ...                                       */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SSwitch totype)
   
   (define (inrange-int30? expr)
      (if (isa? expr J2SNumber)
	  (with-access::J2SNumber expr (val)
	     (cond
		((uint32? val)
		 (<u32 val (bit-lshu32 #u32:1 29)))
		((int32? val)
		 (and (>=s32 val (negs32 (bit-lshs32 #u32:1 29)))
		      (<s32 val (bit-lshs32 #s32:1 29))))
		((fixnum? val)
		 (and (>=fx val (negfx (bit-lsh 1 29)))
		      (<fx val (bit-lsh 1 29))))
		(else #f)))
	  (with-access::J2SExpr expr (range)
	     (when (interval? range)
		(and (>=llong (interval-min range) (- (bit-lshllong #l1 30)))
		     (<llong (interval-max range) (bit-lshllong #l1 30)))))))
   
   (define (type-cast-switch this keytype)
      (with-access::J2SSwitch this (key cases)
	 (set! key (type-cast! key keytype))
	 (for-each (lambda (c)
		      (with-access::J2SCase c (expr body)
			 (unless (isa? c J2SDefault)
			    (set! expr (type-cast! expr keytype)))
			 (set! body (type-cast! body totype))))
	    cases)
	 this))

   (define (expr-int? expr)
      (or (eq? (j2s-type expr) 'integer)
	  (with-access::J2SExpr expr (range)
	     (and (interval? range)
		  (inrange-int30? expr)))))
   
   (with-access::J2SSwitch this (key cases)
      (cond
	 ((and (eq? (j2s-type key) 'int32)
	       (every (lambda (c)
			 (or (isa? c J2SDefault)
			     (with-access::J2SCase c (expr)
				(eq? (j2s-type expr) 'int32))))
		  cases))
	  (type-cast-switch this 'int32))
	 ((and (eq? (j2s-type key) 'uint32)
	       (every (lambda (c)
			 (or (isa? c J2SDefault)
			     (with-access::J2SCase c (expr)
				(eq? (j2s-type expr) 'uint32))))
		  cases))
	  (type-cast-switch this 'uint32))
	 ((and (eq? (j2s-type key) 'int53)
	       (every (lambda (c)
			 (or (isa? c J2SDefault)
			     (with-access::J2SCase c (expr)
				(memq (j2s-type expr) '(uint32 int32 int53)))))
		  cases))
	  (type-cast-switch this 'int53))
	 ((and (expr-int? key)
	       (every (lambda (c)
			 (or (isa? c J2SDefault)
			     (with-access::J2SCase c (expr)
				(expr-int? expr))))
		  cases))
	  (type-cast-switch this 'integer))
	 ((and (eq? (j2s-type key) 'string)
	       (every (lambda (c)
			 (or (isa? c J2SDefault)
			     (with-access::J2SCase c (expr)
				(eq? (j2s-type expr) 'string))))
		  cases))
	  (type-cast-switch this 'string))
	 (else
	  (type-cast-switch this '*)))))
			  
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

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2STemplate ...                                     */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2STemplate totype)
   (with-access::J2STemplate this (loc exprs)
      (set! exprs (map! (lambda (e) (type-cast! e 'string)) exprs))
      (cast this totype)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SImportDynamic ...                                */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SImportDynamic totype)
   (with-access::J2SImportDynamic this (path)
      (set! path (type-cast! path 'string))
      (cast this totype)))
