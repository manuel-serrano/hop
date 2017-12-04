;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/cast.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 18:13:46 2016                          */
;*    Last change :  Mon Dec  4 16:37:40 2017 (serrano)                */
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
      (for-each (lambda (n) (type-cast! n 'any)) decls)
      (for-each (lambda (n) (type-cast! n 'any)) nodes)
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

   (define (fx? type)
      (memq type '(int32 uint32 uint29 length index)))
   
   (cond
      ((eq? totype '*) #f)
      ((eq? type totype) #f)
      ((eq? totype 'any) (fx? type))
      ((fx? type) #t)
      ((fx? totype) #t)
      (else #f)))

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
   (cast-expr expr (j2s-type-ref expr) totype))

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
;*    type-cast! ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SFun totype)
   (with-access::J2SFun this (body rtype)
      (set! body (type-cast! body rtype))
      (cast this totype)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SCast ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SCast totype)
   (with-access::J2SCast this (expr)
      (cast expr totype)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SNumber ...                                       */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SNumber totype)
   (with-access::J2SNumber this (type)
      (if (memq totype '(uint29 int30 uint30 int32 uint32 integer number))
	  (begin
	     (set! type totype)
	     this)
	  (call-next-method))))
      
;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SCall totype)
   
   (define (known-fun this fun)
      (with-access::J2SCall this (args)
	 (with-access::J2SFun fun (rtype params)
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
		      (loop (cdr params) (cdr vals)
			 (cons (type-cast! (car vals) vtype)
			    nvals)))))))))
   
   (define (unknown-fun this)
      (with-access::J2SCall this (args fun)
	 (set! fun (type-cast! fun 'any))
	 (set! args (map! (lambda (a) (type-cast! a 'any)) args))
	 (cast-expr this 'any totype)))
   
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
(define-method (type-cast! this::J2SReturn totype)
   (with-access::J2SReturn this (expr from)
      (with-access::J2SExpr expr (loc type)
	 (cond
	    ((isa? from J2SFun)
	     (with-access::J2SFun from (rtype)
		(set! expr (type-cast! expr rtype)))
	     (set! expr (type-cast! expr '*)))
	    ((isa? from J2SExpr)
	     (with-access::J2SExpr from (type)
		(set! expr (type-cast! expr type)))))
	 this)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SDeclInit ...                                     */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SDeclInit totype)
   (with-access::J2SDeclInit this (val vtype id)
      (set! val (type-cast! val vtype))
      this))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SAssig ...                                        */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SAssig totype)
   (with-access::J2SAssig this (lhs rhs)
      (type-cast! lhs '*)
      (type-cast! rhs (j2s-type-ref lhs))
      (cast this totype)))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SBinary ...                                       */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SBinary totype)
(define t #f)
   (with-access::J2SBinary this (op lhs rhs type hint)
      (case op
	 ((>> <<)
	  (set! lhs (type-cast! lhs 'int32))
	  (set! rhs (type-cast! rhs 'uint32))
	  (set! type 'int32)
	  this)
	 ((>>>)
	  (set! lhs (type-cast! lhs 'uint32))
	  (set! rhs (type-cast! rhs 'uint32))
	  (set! type 'int32)
	  this)
	 ((^ & BIT_OR)
	  (set! lhs (type-cast! lhs 'int32))
	  (set! rhs (type-cast! rhs 'int32))
	  (cast this 'int32))
	 (else
	  (set! lhs (type-cast! lhs '*))
	  (set! rhs (type-cast! rhs '*))
	  this))))

;*---------------------------------------------------------------------*/
;*    type-cast! ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (type-cast! this::J2SIf totype)
   (with-access::J2SIf this (test then else)
      (set! test (type-cast! test 'bool))
      (set! then (type-cast! then totype))
      (set! else (type-cast! else totype))
      this))

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
   (with-access::J2SCond this (test then else)
      (set! test (type-cast! test 'bool))
      (set! then (type-cast! then totype))
      (set! else (type-cast! else totype))
      this))

;*---------------------------------------------------------------------*/
;*    type-cast! ::J2SBinary ...                                       */
;*---------------------------------------------------------------------*/
;* (define-method (type-cast! this::J2SBinary totype fun)         */
;*                                                                     */
;*    (define (set-hint! this tlhs trhs)                               */
;*       (with-access::J2SBinary this (hint)                           */
;* 	 (cond                                                         */
;* 	    ((or (type-integer? tlhs) (type-integer? trhs))            */
;* 	     (set! hint '(integer)))                                   */
;* 	    ((or (eq? tlhs 'string) (eq? trhs 'string))                */
;* 	     (set! hint '(string)))                                    */
;* 	    (else                                                      */
;* 	     (set! hint '())))))                                       */
;* 	                                                               */
;* 	                                                               */
;*       (let ((tlhs (j2s-type-ref lhs))                                   */
;* 	    (trhs (j2s-type-ref rhs)))                                     */
;* 	 (case op                                                      */
;* 	    ((+ - * / %)                                               */
;* 	     (let ((tym (max-type type (max-type tlhs trhs))))         */
;* 		(set! lhs (type-cast! lhs tym fun))                    */
;* 		(set! rhs (type-cast! rhs tym fun))                    */
;* 		(when (memq tym '(any number)) (set-hint! this tlhs trhs)) */
;* 		this))                                                 */
;* 	    ((< <= > >= == === != !== eq?)                             */
;* 	     (let ((tym (max-type tlhs trhs)))                         */
;* 		(set! lhs (type-cast! lhs tym fun))                    */
;* 		(set! rhs (type-cast! rhs tym fun))                    */
;* 		(when (memq tym '(any number)) (set-hint! this tlhs trhs)) */
;* 		this))                                                 */
;* 	    (else                                                      */
;* 	     (set! lhs (type-cast! lhs 'any fun))                      */
;* 	     (set! rhs (type-cast! rhs 'any fun))                      */
;* 	     this)))))                                                 */

;* {*---------------------------------------------------------------------*} */
;* {*    type-cast! ::J2SStmtExpr ...                                     *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (type-cast! this::J2SStmtExpr totype fun)       */
;*    (with-access::J2SStmtExpr this (expr)                            */
;*       (let ((nexpr (type-cast! expr totype fun)))                   */
;* 	 (if (isa? nexpr J2SCast)                                      */
;* 	     (with-access::J2SCast nexpr ((castexpr expr))             */
;* 		(set! expr castexpr))                                  */
;* 	     (set! expr nexpr)))                                       */
;*       this))                                                        */
;*                                                                     */

;* {*---------------------------------------------------------------------*} */
;* {*    type-cast! ::J2SObjInit ...                                      *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (type-cast! this::J2SObjInit totype fun)        */
;*    (call-default-walker))                                           */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    type-cast! ::J2SPragma ...                                       *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (type-cast! this::J2SPragma totype fun)         */
;*    (call-default-walker))                                           */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    type-cast! ::J2SNew ...                                          *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (type-cast! this::J2SNew totype fun)            */
;*    (with-access::J2SNew this (args clazz)                           */
;*       (set! args (map! cast-any args))                              */
;*       (set! clazz (cast (type-cast! clazz totype fun) 'any))        */
;*       this))                                                        */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    type-cast! ::J2SExpr ...                                         *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (type-cast! this::J2SExpr totype fun)           */
;*    (cond                                                            */
;*       ((or (isa? this J2SObjInit)                                   */
;* 	   (isa? this J2SPragma)                                       */
;* 	   (isa? this J2SSequence)                                     */
;* 	   (isa? this J2SParen)                                        */
;* 	   (isa? this J2SCond)                                         */
;* 	   (isa? this J2SMethod)                                       */
;* 	   (isa? this J2SClass)                                        */
;* 	   (isa? this J2SClassElement))                                */
;*        (call-default-walker)                                        */
;*        this)                                                        */
;*       ((or (isa? this J2SThis)                                      */
;* 	   (isa? this J2SLiteral)                                      */
;* 	   (isa? this J2SHopRef)                                       */
;* 	   (isa? this J2SUnresolvedRef))                               */
;*        this)                                                        */
;*       (else                                                         */
;*        (tprint "TBD " (typeof this))                                */
;*        (call-default-walker)                                        */
;*        this)))                                                      */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    type-cast! ::J2SUnary ...                                        *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (type-cast! this::J2SUnary totype fun)          */
;*    (with-access::J2SUnary this (op expr)                            */
;*       (if (eq? op 'typeof)                                          */
;* 	  (begin                                                       */
;* 	     (set! expr (type-cast! expr 'any fun))                    */
;* 	     (cast this totype))                                       */
;* 	  (call-default-walker))))                                     */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    type-cast! ::J2SCast ...                                         *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (type-cast! this::J2SCast totype fun)           */
;*    (with-access::J2SCast this (loc)                                 */
;*       (J2SCast totype this)))                                       */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    type-cast! ::J2SArray ...                                        *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (type-cast! this::J2SArray totype fun)          */
;*    (call-default-walker))                                           */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    type-cast! ::J2SExprStmt ...                                     *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (type-cast! this::J2SExprStmt totype fun)       */
;*    (with-access::J2SExprStmt this (loc stmt)                        */
;*       (set! stmt (type-cast! stmt totype fun))                      */
;*       (J2SCast totype this)))                                       */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    type-cast! ::J2SBindExit ...                                     *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (type-cast! this::J2SBindExit totype fun)       */
;*    (with-access::J2SBindExit this (loc stmt)                        */
;*       (set! stmt (type-cast! stmt totype fun))                      */
;*       (J2SCast totype this)))                                       */
;*                                                                     */
