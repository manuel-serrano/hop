;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/type.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 18 18:44:55 2016                          */
;*    Last change :  Mon Feb 15 10:54:28 2016 (serrano)                */
;*    Copyright   :  2016 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Type inference                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_type

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_type-hint)

   (static (class TypeCtx
	      (fix::bool (default #f))
	      (fun (default #f))
	      (env::pair-nil (default '()))
	      (link (default #f))))

   (export j2s-type-stage))

;*---------------------------------------------------------------------*/
;*    j2s-type-stage ...                                               */
;*---------------------------------------------------------------------*/
(define j2s-type-stage
   (instantiate::J2SStageProc
      (name "type")
      (comment "transform generator in TYPE")
      (proc j2s-type!)))

;*---------------------------------------------------------------------*/
;*    j2s-type! ::obj ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-type! this args)
   (when (isa? this J2SProgram)
      (j2s-type-program! this args)
      (let loop ((i 1))
	 (if (j2s-hint! this args)
	     (begin
		(j2s-type-program! this args)
		(j2s-call-hint! this args)
		(when (>=fx (bigloo-debug) 4)
		   (fprintf (current-error-port) "\n       ~a:" i))
		(j2s-type-program! this args)
		(loop (+fx i 1))
		this)
	     this))))

;*---------------------------------------------------------------------*/
;*    ctx->list ...                                                    */
;*---------------------------------------------------------------------*/
(define (ctx->list ctx)
   (with-access::TypeCtx ctx (env)
      (call-with-output-string
	 (lambda (p)
	    (for-each (lambda (decl)
			 (with-access::J2SDecl (car decl) (id key)
			    (display id p)
			    (display " " p)
			    (display key p)
			    (display " ::" p)
			    (display (cdr decl) p)
			    (newline p)))
	       env)))))

;*---------------------------------------------------------------------*/
;*    merge-type! ...                                                  */
;*---------------------------------------------------------------------*/
(define (merge-type! ctx current new)
   (cond
      ((or (not new) (eq? current new))
       current)
      ((not current)
       (when ctx (unfix! ctx))
       new)
      ((eq? current 'obj)
       'obj)
      ((and (eq? current 'number) (eq? new 'integer))
       'number)
      ((and (eq? current 'object) (eq? new 'string))
       'obj)
      ((and (eq? current 'integer) (eq? new 'number))
       'number)
      ((and (eq? current 'string) (eq? new 'object))
       'obj)
      (else
       'obj)))

;*---------------------------------------------------------------------*/
;*    ctx-duplicate ...                                                */
;*---------------------------------------------------------------------*/
(define (ctx-duplicate ctx)
   (with-access::TypeCtx ctx (env)
      (duplicate::TypeCtx ctx
	 (env (map (lambda (e) (cons (car e) (cdr e))) env)))))

;*---------------------------------------------------------------------*/
;*    ctx-env-type ...                                                 */
;*---------------------------------------------------------------------*/
(define (ctx-env-type this::J2SDecl ctx)
   (with-access::TypeCtx ctx (env)
      (let ((c (assq this env)))
	 (if (pair? c)
	     (cdr c)
	     'obj))))

;*---------------------------------------------------------------------*/
;*    ctx-env-type-set! ...                                            */
;*---------------------------------------------------------------------*/
(define (ctx-env-type-set! this::J2SDecl ctx::TypeCtx type)
   (when type
      (with-access::TypeCtx ctx (env)
	 (let ((cell (assq this env)))
	    (if (pair? cell)
		(let* ((oldt (cdr cell))
		       (newt (merge-type! #f oldt type)))
		   (set-cdr! cell newt)
		   newt)
		(begin
		   (set! env (cons (cons this type) env))
		   type))))))

;*---------------------------------------------------------------------*/
;*    ctx-env-type-merge! ...                                          */
;*---------------------------------------------------------------------*/
(define (ctx-env-type-merge! ctx1 ctx2)
   (with-access::TypeCtx ctx1 ((env1 env))
      (with-access::TypeCtx ctx2 ((env2 env))
	 (for-each (lambda (tydecl)
		      (let ((decl (car tydecl))
			    (ty2 (cdr tydecl)))
			 (ctx-env-type-set! decl ctx1 ty2)))
	    env2))))
      
;*---------------------------------------------------------------------*/
;*    unfix! ...                                                       */
;*---------------------------------------------------------------------*/
(define (unfix! ctx)
   (let loop ((ctx ctx))
      (with-access::TypeCtx ctx (fix link)
	 (set! fix #f)
	 (when (isa? link TypeCtx)
	    (loop link)))))

;*---------------------------------------------------------------------*/
;*    ctx-expr-type-set! ...                                           */
;*---------------------------------------------------------------------*/
(define (ctx-expr-type-set! this::J2SExpr ctx::TypeCtx exprtype)
   (when exprtype
      (with-access::J2SExpr this (type)
	 (set! type (merge-type! ctx type exprtype))
	 type)))

;*---------------------------------------------------------------------*/
;*    j2s-type-program! ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-type-program! this::J2SProgram args)
   (with-access::J2SProgram this (headers decls nodes)
      (let ((ctx (instantiate::TypeCtx)))
	 (when (>=fx (bigloo-debug) 4)
	    (display " " (current-error-port)))
	 (with-access::TypeCtx ctx (fix)
	    (let loop ((i 1))
	       (unless fix
		  (when (>=fx (bigloo-debug) 4)
		     (fprintf (current-error-port) "~a." i)
		     (flush-output-port (current-error-port)))
		  (set! fix #t)
		  (for-each (lambda (o) (type o ctx)) headers)
		  (for-each (lambda (o) (type o ctx)) decls)
		  (for-each (lambda (o) (type o ctx)) nodes)
		  (loop (+fx i 1)))))))
   this)

;*---------------------------------------------------------------------*/
;*    type ::J2SNode ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SNode ctx)
   (call-default-walker)
   'obj)

;*---------------------------------------------------------------------*/
;*    type ::J2SNumber ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SNumber ctx)
   (with-access::J2SNumber this (val)
      (ctx-expr-type-set! this ctx (if (fixnum? val) 'integer 'number))))

;*---------------------------------------------------------------------*/
;*    type ::J2SString ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SString ctx)
   (ctx-expr-type-set! this ctx 'string))

;*---------------------------------------------------------------------*/
;*    type ::J2SLiteralCnst ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SLiteralCnst ctx)
   (with-access::J2SLiteralCnst this (val)
      (ctx-expr-type-set! this ctx (type val ctx))))

;*---------------------------------------------------------------------*/
;*    type ::J2SObjInit ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SObjInit ctx)
   (ctx-expr-type-set! this ctx 'object))

;*---------------------------------------------------------------------*/
;*    type ::J2SDecl ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SDecl ctx)
   (with-access::J2SDecl this (type)
      (ctx-env-type-set! this ctx type)))

;*---------------------------------------------------------------------*/
;*    type-decl ...                                                    */
;*---------------------------------------------------------------------*/
(define (type-decl this::J2SDecl ctx valtype)
   (with-access::J2SDecl this (type)
      (set! type (merge-type! ctx type valtype))
      (ctx-env-type-set! this ctx type)
      type))

;*---------------------------------------------------------------------*/
;*    type ::J2SDeclInit ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SDeclInit ctx)
   (with-access::J2SDeclInit this (val)
      (ctx-env-type-set! this ctx (type-decl this ctx (type val ctx)))
      (ctx-env-type this ctx)))

;*---------------------------------------------------------------------*/
;*    type ::J2SDeclFun ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SDeclFun ctx)
   (with-access::J2SDeclFun this (val)
      (type val ctx)
      (type-decl this ctx 'function)))

;*---------------------------------------------------------------------*/
;*    type ::J2SDecFunCnst ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SDeclFunCnst ctx)
   (with-access::J2SDeclFunCnst this ((ftype type))
      (set! ftype 'function)
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    type ::J2SRef ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SRef ctx)
   (with-access::J2SRef this (decl type)
      (ctx-expr-type-set! this ctx (ctx-env-type decl ctx))))

;*---------------------------------------------------------------------*/
;*    type-assig ...                                                   */
;*---------------------------------------------------------------------*/
(define (type-assig this::J2SAssig ctx rhstype)
   (with-access::J2SAssig this (lhs)
      (type lhs ctx)
      (let ((atype (ctx-expr-type-set! this ctx rhstype)))
	 (when (isa? lhs J2SRef)
	    (with-access::J2SRef lhs (decl)
	       (with-access::J2SDecl decl ((dtype type))
		  (set! dtype (merge-type! #f dtype atype))
		  (ctx-env-type-set! decl ctx atype))))
	 atype)))
   
;*---------------------------------------------------------------------*/
;*    type ::J2SAssig ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SAssig ctx)
   (with-access::J2SAssig this (lhs rhs)
      (type-assig this ctx (type rhs ctx))))

;*---------------------------------------------------------------------*/
;*    type ::J2SPrefix ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SPrefix ctx)
   (with-access::J2SAssig this (lhs)
      (type-assig this ctx 'number)))

;*---------------------------------------------------------------------*/
;*    type ::J2SPostfix ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SPostfix ctx)
   (with-access::J2SAssig this (lhs)
      (type-assig this ctx 'number)))

;*---------------------------------------------------------------------*/
;*    type ::J2SFun ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SFun ctx)
   (with-access::J2SFun this (body params)
      (with-access::TypeCtx ctx (env)
	 (let* ((envparams (filter-map (lambda (p)
					  (with-access::J2SDecl p (type)
					     (when type
						(cons p type))))
			      params))
		(nctx (instantiate::TypeCtx
			 (env (append envparams env))
			 (link ctx)
			 (fun this))))
	    (type body nctx)
	    (ctx-expr-type-set! this ctx 'function)))))

;*---------------------------------------------------------------------*/
;*    type ::J2SReturn ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SReturn ctx)
   (with-access::J2SReturn this (expr)
      (let ((rtype (type expr ctx)))
	 (when rtype
	    (with-access::TypeCtx ctx (fun)
	       (when fun
		  ;; not a top-level form
		  (with-access::J2SFun fun (%info name rettype)
		     (set! rettype (merge-type! ctx rettype rtype))
		     rettype)))))))

;*---------------------------------------------------------------------*/
;*    type ::J2SCall ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SCall ctx)
   
   (define (type-fun fun)
      (with-access::J2SFun fun (rettype)
	 (ctx-expr-type-set! this ctx rettype)))
   
   (with-access::J2SCall this (fun args)
      (call-default-walker)
      (cond
	 ((isa? fun J2SFun)
	  (type-fun fun))
	 ((isa? fun J2SRef)
	  (with-access::J2SRef fun (decl)
	     (cond
		((isa? decl J2SDeclFunCnst)
		 (with-access::J2SDeclFunCnst decl ((fun val))
		    (type-fun fun)))
		((isa? decl J2SDeclFun)
		 (with-access::J2SDeclFun decl (ronly (fun val))
		    (when ronly
		       (type-fun fun))))
		(else
		 #f))))
	 ((isa? fun J2SHopRef)
	  (with-access::J2SHopRef fun ((htype type))
	     (ctx-expr-type-set! this ctx htype)))
	 (else
	  #f))))
   
;*---------------------------------------------------------------------*/
;*    type ::J2SUnary ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SUnary ctx)
   (with-access::J2SUnary this (op type expr)
      (case op
	 ((~ -)
	  (ctx-expr-type-set! this ctx 'number)))))
	  
;*---------------------------------------------------------------------*/
;*    type ::J2SBinary ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SBinary ctx)
   (with-access::J2SBinary this (op lhs rhs)
      (case op
	 ((+)
	  (let* ((lhstype (type lhs ctx))
		 (rhstype (type rhs ctx)))
	     (cond
		((and (eq? lhstype 'number) (eq? rhstype 'number))
		 (ctx-expr-type-set! this ctx 'number))
		((or (eq? lhstype 'string) (eq? rhstype 'string))
		 (ctx-expr-type-set! this ctx 'string)))))
	 ((- * / %)
	  (let* ((lhstype (type lhs ctx))
		 (rhstype (type rhs ctx)))
	     (ctx-expr-type-set! this ctx 'number)))
	 ((== === != !== < <= > >= instanceof)
	  (let* ((lhstype (type lhs ctx))
		 (rhstype (type rhs ctx)))
	     (ctx-expr-type-set! this ctx 'bool)))
	 ((&&)
	  (let* ((lhstype (type lhs ctx))
		 (rhstype (type rhs ctx)))
	     (ctx-expr-type-set! this ctx rhstype)))
	 ((OR)
	  (let* ((lhstype (type lhs ctx))
		 (rhstype (type rhs ctx)))
	     (ctx-expr-type-set! this ctx lhstype)))
	 ((<< >> >>> ^ & BIT_OR)
	  (let* ((lhstype (type lhs ctx))
		 (rhstype (type rhs ctx)))
	     (ctx-expr-type-set! this ctx 'number)))
	 (else
	  (type lhs ctx)
	  (type rhs ctx)
	  #f))))

;*---------------------------------------------------------------------*/
;*    type ::J2SParen ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SParen ctx)
   (with-access::J2SParen this (expr)
      (ctx-expr-type-set! this ctx (type expr ctx))))

;*---------------------------------------------------------------------*/
;*    type ::J2SAccess ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SAccess ctx)
   
   (define (length? field)
      (cond
	 ((isa? field J2SLiteralCnst)
	  (with-access::J2SLiteralCnst field (val)
	     (length? val)))
	 ((isa? field J2SString)
	  (with-access::J2SString field (val)
	     (string=? val "length")))
	 (else
	  #f)))
   
   (with-access::J2SAccess this (obj field)
      (let ((tyobj (type obj ctx))
	    (tyfield (type field ctx)))
	 (when (and (memq tyobj '(array string)) (j2s-field-length? field))
	    (ctx-expr-type-set! this ctx 'integer)))))

;*---------------------------------------------------------------------*/
;*    type ::J2SIf ...                                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SIf ctx)
   (with-access::J2SIf this (test then else)
      (type test ctx)
      (let ((dupctx (ctx-duplicate ctx)))
	 (type then ctx)
	 (type else dupctx)
	 (ctx-env-type-merge! ctx dupctx))))
   
