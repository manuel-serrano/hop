;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/symbol.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 13 16:57:00 2013                          */
;*    Last change :  Sat Sep  9 11:39:50 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Variable Declarations                                            */
;*    -------------------------------------------------------------    */
;*    This pass implements the J2SVarDecls lifting. After this pass,   */
;*    the AST should no longer contains any J2SVarDecls node.          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_symbol

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_lexer)

   (static (class J2SDeclArguments::J2SDecl))

   (export j2s-symbol-stage
	   (generic j2s-symbol ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-symbol-stage                                                 */
;*---------------------------------------------------------------------*/
(define j2s-symbol-stage
   (instantiate::J2SStageProc
      (name "symbol")
      (comment "symbol resolution")
      (proc j2s-symbol)))

;*---------------------------------------------------------------------*/
;*    j2s-symbol ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (j2s-symbol this conf)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-symbol ::J2SProgram ...                                      */
;*    -------------------------------------------------------------    */
;*    Warning, headers are not scanned for variable resolution!        */
;*---------------------------------------------------------------------*/
(define-method (j2s-symbol this::J2SProgram conf)
   (with-access::J2SProgram this (nodes loc mode headers decls)
      (let* ((hds (append-map (lambda (s) (collect* s)) headers))
	     (vars (append-map (lambda (s) (collect* s)) nodes))
	     (lets (collect-let nodes))
	     (env (append hds vars lets))
	     (scope (config-get conf :bind-global '%scope))
	     (vdecls (bind-decls! vars env mode scope '() '() conf)))
	 (when (pair? vars)
	    (set! decls (filter (lambda (d) (isa? d J2SDecl)) vdecls)))
	 (when (pair? lets)
	    (for-each (lambda (d::J2SDecl)
			 (with-access::J2SDecl d (scope)
			    (set! scope 'global)))
	       lets)
	    (set! decls (append decls lets)))
	 (set! nodes
	    (append (filter (lambda (d) (not (isa? d J2SDecl))) vdecls)
	       nodes))
	 (set! nodes
	    (map! (lambda (o) (resolve! o env mode '() '() #f conf)) nodes))))
   this)

;*---------------------------------------------------------------------*/
;*    eq-conf-lang? ...                                                */
;*---------------------------------------------------------------------*/
(define (eq-conf-lang? conf lang)
   (eq? (config-get conf :language 'hopscript) lang))

;*---------------------------------------------------------------------*/
;*    find-decl ...                                                    */
;*---------------------------------------------------------------------*/
(define (find-decl var::symbol env::pair-nil)
   (find (lambda (decl)
	    (with-access::J2SDecl decl (id)
	       (eq? id var)))
      env))

;*---------------------------------------------------------------------*/
;*    debug-dump-env ...                                               */
;*---------------------------------------------------------------------*/
(define (debug-dump-env env)
   (map (lambda (d::J2SDecl) (with-access::J2SDecl d (id key) (cons id key)))
      env))

;*---------------------------------------------------------------------*/
;*    bind-decls! ...                                                  */
;*---------------------------------------------------------------------*/
(define (bind-decls! decls env mode scope::symbol withs::pair-nil wenv::pair-nil conf)
   
   (define (decl->assign! decl::J2SDeclInit old::J2SDecl)
      (with-access::J2SDeclInit decl (loc id val)
	 (instantiate::J2SStmtExpr
	    (loc loc)
	    (expr (instantiate::J2SInit
		     (loc loc)
		     (lhs (j2sref old loc withs wenv))
		     (rhs val))))))

   (let loop ((decls (map (lambda (e) (bind! e env mode conf)) decls))
	      (acc '())
	      (funs '()))
      (if (null? decls)
	  (append (reverse! acc) (reverse! funs))
	  (let ((decl (car decls)))
	     (with-access::J2SDecl decl (id (bscope scope))
		(set! bscope scope)
		(let ((old (find-decl id acc)))
		   (if old
		       (if (or (isa? decl J2SDeclFun)
			       (isa? decl J2SDeclExtern))
			   (let ((fun (decl->assign! decl old)))
			      (loop (cdr decls) acc (cons fun funs)))
			   (loop (cdr decls) acc funs))
		       (loop (cdr decls) (cons decl acc) funs))))))))

;*---------------------------------------------------------------------*/
;*    bind! ::J2SDecl ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (bind! this::J2SDecl env mode conf)
   this)

;*---------------------------------------------------------------------*/
;*    bind! ::J2SDeclFun ...                                           */
;*---------------------------------------------------------------------*/
(define-method (bind! this::J2SDeclFun env mode conf)
   (with-access::J2SDeclFun this (val id)
      (set! val (resolve! val env mode '() '() #f conf))
      this))

;*---------------------------------------------------------------------*/
;*    make-ctx ...                                                     */
;*---------------------------------------------------------------------*/
(define (make-ctx typ val)
   (cons typ val))

;*---------------------------------------------------------------------*/
;*    ctx-ctor? ...                                                    */
;*---------------------------------------------------------------------*/
(define (ctx-ctor? ctx)
   (and (pair? ctx) (eq? (car ctx) 'ctor)))

;*---------------------------------------------------------------------*/
;*    ctx-value ...                                                    */
;*---------------------------------------------------------------------*/
(define (ctx-value ctx)
   (cdr ctx))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SNode env mode withs wenv ctx conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SFun ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SFun env mode withs wenv ctx conf)

   (define (check-strict-mode-params params loc)
      ;; Check the extra cnnstrains parameter names have in strict mode
      (let loop ((p params))
	 (when (pair? p)
	    (with-access::J2SDecl (car p) (id)
	       (cond
		  ((memq id '(eval arguments))
		   (raise
		      (instantiate::&io-parse-error
			 (proc "symbol resolution (symbol)")
			 (msg "Illegal parameter name")
			 (obj id)
			 (fname (cadr loc))
			 (location (caddr loc)))))
		  ((j2s-reserved-id? id)
		   (raise
		      (instantiate::&io-parse-error
			 (proc "symbol resolution (symbol)")
			 (msg "Illegal parameter name")
			 (obj id)
			 (fname (cadr loc))
			 (location (caddr loc)))))
		  ((and (eq? id 'service) (eq-conf-lang? conf 'hopscript))
		   (raise
		      (instantiate::&io-parse-error
			 (proc "symbol resolution (symbol)")
			 (msg "Illegal parameter name")
			 (obj id)
			 (fname (cadr loc))
			 (location (caddr loc)))))
		  ((find-decl id (cdr params))
		   (raise
		      (instantiate::&io-parse-error
			 (proc "symbol resolution (symbol)")
			 (msg "Illegal duplicate parameter name")
			 (obj id)
			 (fname (cadr loc))
			 (location (caddr loc))))))))))

   (define (nonstrict-params! args)
      (let loop ((args args))
	 (when (pair? args)
	    (with-access::J2SDecl (car args) (id)
	       (when (find-decl id (cdr args))
		  (set! id (gensym 'arg)))
	       (loop (cdr args))))))

   (define (not-in? d::J2SDecl params::pair-nil)
      (with-access::J2SDecl d (id)
	 (not (find-decl id params))))

   (with-access::J2SFun this (body params loc (fmode mode) params decl name
				ismethodof)
      (let ((id (or name (j2sfun-id this))))
	 ;; check parameter correctness
	 (if (eq? fmode 'normal)
	     (nonstrict-params! params)
	     (begin
		(check-strict-mode-params params loc)
		(when (symbol? id)
		   (check-strict-mode-eval id "Function name" loc))))
	 ;; walk throught the function body
	 (let* ((env0 (if (j2sfun-expression? this) (cons decl env) env))
		(decls (filter (lambda (d)
				  ;; see ecma-262-51.html#sec-10.2
				  (or (isa? d J2SDeclFun)
				      (not-in? d params)))
			  (collect* body)))
		(arguments (instantiate::J2SDeclArguments
			      (id 'arguments)
			      (loc loc)))
		(envl (append decls params))
		(env1 (append envl env0))
		(ldecls (with-access::J2SBlock body (nodes)
			   (collect-let nodes)))
		(nenv (if (find-decl 'arguments envl)
			  (append ldecls env1)
			  (cons arguments (append ldecls env1))))
		(nwenv (cons arguments (append decls params wenv)))
		(ctx (and ismethodof (or ctx (make-ctx 'proto '__proto__)))))
	    (for-each (lambda (decl::J2SDecl)
			 (with-access::J2SDecl decl (scope)
			    (set! scope 'fun)))
	       ldecls)
	    (if (pair? decls)
		(set! body
		   (with-access::J2SBlock body (endloc)
		      (instantiate::J2SBlock
			 (loc loc)
			 (endloc endloc)
			 (nodes (append
				   (bind-decls! decls nenv fmode 'inner withs wenv conf)
				   (list (walk! body nenv fmode withs nwenv
					    ctx conf)))))))
		(set! body (walk! body nenv fmode withs nwenv ctx conf)))
	    (with-access::J2SDeclArguments arguments (usecnt)
	       (when (>fx usecnt 0)
		  (with-access::J2SFun this (vararg params)
		     (if vararg
			 (with-access::J2SDecl (car (last-pair params)) (id loc)
			    (raise
			       (instantiate::&io-parse-error
				  (proc "symbol resolution (symbol)")
				  (msg "\"arguments\" object may not be used in conjunction with a rest parameter")
				  (obj id)
				  (fname (cadr loc))
				  (location (caddr loc)))))
			 (set! vararg 'arguments))))))))
   this)

;*---------------------------------------------------------------------*/
;*    resolve! ::J2STilde ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2STilde env mode withs wenv ctx conf)
   (with-access::J2STilde this (stmt)
      ;; mark global variables, used by the JS generation pass
      (when (isa? stmt J2SSeq)
	 (with-access::J2SSeq stmt (nodes)
	    (for-each (lambda (n)
			 (when (isa? n J2SVarDecls)
			    (with-access::J2SVarDecls n (decls)
			       (for-each (lambda (d)
					    (with-access::J2SDecl d (scope)
					       (set! scope 'global)))
				  decls))))
	       nodes)))
      (resolve-tilde! stmt
	 (list (lambda (this resolvers)
		  (resolve! this env mode withs wenv ctx conf))))
      this))

;*---------------------------------------------------------------------*/
;*    resolve-let! ...                                                 */
;*---------------------------------------------------------------------*/
(define (resolve-let! this::J2SBlock env mode withs wenv decls ctx conf)
   (with-access::J2SBlock this (loc endloc nodes)
      (let ((nenv (append decls env)))
	 (instantiate::J2SLetBlock
	    (loc loc)
	    (endloc endloc)
	    (decls decls)
	    (nodes (map! (lambda (n)
			    (resolve! n nenv mode withs wenv ctx conf))
		      nodes))))))
   
;*---------------------------------------------------------------------*/
;*    resolve! ::J2SBlock ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SBlock env mode withs wenv ctx conf)
   ;; a block is a letrec if it contains let or const declaration
   (with-access::J2SBlock this (nodes endloc)
      (let ((ldecls (collect-let nodes)))
	 (if (pair? ldecls)
	     (resolve-let! this env mode withs wenv ldecls ctx conf)
	     (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SWith ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SWith env mode withs wenv ctx conf)
   (if (eq? mode 'normal)
       (with-access::J2SWith this (obj block loc id)
	  (set! obj (resolve! obj env mode withs wenv ctx conf))
	  (set! block (resolve! block env mode (cons id withs) '() ctx conf))
	  this)
       (with-access::J2SWith this (loc)
	  (raise
	     (instantiate::&io-parse-error
		(proc "symbol resolution (symbol)")
		(msg "strict mode code may not include with statements")
		(obj "with")
		(fname (cadr loc))
		(location (caddr loc)))))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SCatch ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SCatch env mode withs wenv ctx conf)
   (with-access::J2SCatch this (body param loc)
      ;; propagate the compilation mode to the function
      (let ((nenv (cons param env)))
	 (when (eq? mode 'strict)
	    (with-access::J2SDecl param (id)
	       (check-strict-mode-eval id "Catch name" loc)))
	 (set! body (walk! body nenv mode withs wenv ctx conf))))
   this)

;*---------------------------------------------------------------------*/
;*    j2sref ...                                                       */
;*---------------------------------------------------------------------*/
(define (j2sref decl::J2SDecl loc withs wenv)
   (let ((ref (instantiate::J2SRef
		 (loc loc)
		 (decl decl))))
      (with-access::J2SDecl decl (id)
	 (if (and (pair? withs) (not (find-decl id wenv)))
	     (instantiate::J2SWithRef
		(loc loc)
		(withs withs)
		(id id)
		(expr ref))
	     ref))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SFor ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SFor env mode withs wenv ctx conf)
   
   (define (mark-decls-loop! decls)
      (for-each (lambda (decl::J2SDecl)
		   (with-access::J2SDecl decl (scope _scmid id binder)
		      (set! _scmid (symbol-append '%I id))
		      (set! binder 'let-opt)
		      (set! scope 'loop)))
	 decls))
   
   (define (let->init d::J2SDecl)
      (with-access::J2SDecl d (loc id)
	 (instantiate::J2SDeclInit
	    (loc loc)
	    (id id)
	    (binder 'let-opt)
	    (val (instantiate::J2SRef
		    (loc loc)
		    (decl d))))))

   (define (reassign! d::J2SDecl i::J2SDecl)
      (with-access::J2SDecl d (loc)
	 (J2SStmtExpr (J2SAssig (J2SRef d) (J2SRef i)))))
   
   (define (for-let for)
      (with-access::J2SFor for (init body)
	 (with-access::J2SVarDecls init (loc decls)
	    (mark-decls-loop! decls)
	    (with-access::J2SLoop for (body)
	       ;; create local variables for the loop body
	       (let ((inits (map let->init decls)))
		  (set! body
		     (instantiate::J2SBlock
			(endloc loc)
			(loc loc)
			(nodes (list
				  (instantiate::J2SVarDecls
				     (loc loc)
				     (decls inits))
				  body
				  (instantiate::J2SSeq
				     (loc loc)
				     (nodes (map reassign! decls inits)))))))))
	    (let ((lift init))
	       (set! init (instantiate::J2SNop (loc loc)))
	       (instantiate::J2SBlock
		  (endloc loc)
		  (loc loc)
		  (nodes (list lift for)))))))
   
   (define (for-var for)
      (with-access::J2SFor for (init)
	 (with-access::J2SVarDecls init (loc decls)
	    (let ((lift init))
	       (set! init (instantiate::J2SNop (loc loc)))
	       (instantiate::J2SBlock
		  (endloc loc)
		  (loc loc)
		  (nodes (list lift for)))))))
   
   (define (let-init? init::J2SVarDecls)
      (with-access::J2SVarDecls init (decls)
	 (j2s-let? (car decls))))
   
   (with-access::J2SFor this (init)
      (cond
	 ((not (isa? init J2SVarDecls))
	  (call-next-method))
	 ((let-init? init)
	  (resolve! (for-let this) env mode withs wenv ctx conf))
	 (else
	  (resolve! (for-var this) env mode withs wenv ctx conf)))))
   
;*---------------------------------------------------------------------*/
;*    resolve! ::J2SForIn ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SForIn env mode withs wenv ctx conf)

   (define (let->init d::J2SDecl)
      (with-access::J2SDecl d (loc id)
	 (instantiate::J2SDeclInit
	    (loc loc)
	    (id id)
	    (binder 'let-opt)
	    (val (instantiate::J2SRef
		    (loc loc)
		    (decl d))))))
   
   (define (for-in-let for)
      (with-access::J2SForIn for (lhs loc body)
	 (with-access::J2SVarDecls lhs (loc decls)
	    (with-access::J2SDecl (car decls) (loc id binder)
	       (let ((decl (instantiate::J2SDeclInit
			      (loc loc)
			      (id id)
			      (_scmid (symbol-append '%I id))
			      (binder 'let-opt)
			      (val (instantiate::J2SUndefined
				      (loc loc)))))
		     (lift lhs))
		  (set! body
		     (instantiate::J2SBlock
			(endloc loc)
			(loc loc)
			(nodes (list
				  (instantiate::J2SVarDecls
				     (loc loc)
				     (decls (list (let->init decl))))
				  body))))
		  (set! decls (list decl))
		  (set! lhs (instantiate::J2SRef (loc loc) (decl decl)))
		  (instantiate::J2SBlock
		     (endloc loc)
		     (loc loc)
		     (nodes (list lift for))))))))
   
   (define (for-in-var for)
      (with-access::J2SForIn for (lhs loc)
	 (with-access::J2SVarDecls lhs (loc decls)
	    (let ((lift lhs))
	       (set! lhs (instantiate::J2SRef (loc loc) (decl (car decls))))
	       (instantiate::J2SBlock
		  (endloc loc)
		  (loc loc)
		  (nodes (list lift for)))))))
   
   (define (let-init? init::J2SVarDecls)
      (with-access::J2SVarDecls init (decls)
	 (j2s-let? (car decls))))
   
   (with-access::J2SForIn this (loc lhs obj body)
      (cond
	 ((not (isa? lhs J2SVarDecls))
	  (set! lhs (resolve! lhs env mode withs wenv ctx conf))
	  (set! obj (resolve! obj env mode withs wenv ctx conf))
	  (set! body (resolve! body env mode withs wenv ctx conf))
	  this)
	 ((let-init? lhs)
	  (resolve! (for-in-let this) env mode withs wenv ctx conf))
	 (else
	  (resolve! (for-in-var this) env mode withs wenv ctx conf)))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SUnresolvedRef ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SUnresolvedRef env mode withs wenv ctx conf)
   (with-access::J2SUnresolvedRef this (id loc)
      (let ((decl (find-decl id env)))
	 (cond
	    ((isa? decl J2SDecl)
	     ;; mark arguments used to avoid retraversing
	     ;; the function body
	     (when (isa? decl J2SDeclArguments)
		(with-access::J2SDeclArguments decl (usecnt)
		   (set! usecnt (+fx 1 usecnt))))
	     (j2sref decl loc withs wenv))
	    ((pair? withs)
	     (instantiate::J2SWithRef
		(loc loc)
		(withs withs)
		(id id)
		(expr this)))
	    (else
	     this)))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SDecl ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SDecl env mode withs wenv ctx conf)
   (with-access::J2SDecl this (loc id)
      (when (eq? mode 'strict)
	 (check-strict-mode-eval id  "Declaration name" loc))
      (instantiate::J2SNop
	 (loc loc))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SVarDecls ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SVarDecls env mode withs wenv ctx conf)
   (with-access::J2SVarDecls this (decls loc)
      (let ((ndecls (filter-map (lambda (d)
				   (let ((nd (resolve! d env mode withs wenv ctx conf)))
				      (unless (isa? nd J2SNop) nd)))
		       decls)))
	 (if (pair? ndecls)
	     (instantiate::J2SSeq
		(loc loc)
		(nodes ndecls))
	     (instantiate::J2SNop
		(loc loc))))))

;*---------------------------------------------------------------------*/
;*    check-strict-mode-eval ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-10.1.1       */
;*---------------------------------------------------------------------*/
(define (check-strict-mode-eval id msg loc)
   (cond
      ((or (eq? id 'eval) (eq? id 'arguments))
       (raise
	  (instantiate::&io-parse-error
	     (proc "symbol resolution (symbol)")
	     (msg (format "~a eval or arguments is not allowed in strict mode"
		     msg ))
	     (obj id)
	     (fname (cadr loc))
	     (location (caddr loc)))))
      ((j2s-strict-reserved-id? id)
       (raise
	  (instantiate::&io-parse-error
	     (proc "symbol resolution (symbol)")
	     (msg "~a a reserved name is not allowd in strict mode")
	     (obj id)
	     (fname (cadr loc))
	     (location (caddr loc)))))))

;*---------------------------------------------------------------------*/
;*    check-immutable ...                                              */
;*---------------------------------------------------------------------*/
(define (check-immutable decl::J2SDecl loc conf)
   (with-access::J2SDecl decl (immutable id)
      (when (and immutable (or (isa? decl J2SDeclClass) (isa? decl J2SDeclFun)))
	 (cond
	    ((eq-conf-lang? conf 'hopscript)
	     (raise
		(instantiate::&io-parse-error
		   (proc "symbol resolution (symbol)")
		   (msg "Assignment to constant variable")
		   (obj id)
		   (fname (cadr loc))
		   (location (caddr loc)))))
	    ((> (bigloo-warning) 1)
	     (warning/loc loc "Assignment to constant variable."))))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SDeclInit ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SDeclInit env mode withs wenv ctx conf)
   (with-access::J2SDeclInit this (loc id val)
      (let ((ndecl::J2SDecl (find-decl id env)))
	 ;; strict mode restrictions
	 ;; http://www.ecma-international.org/ecma-262/5.1/#sec-10.1.1
	 (when (eq? mode 'strict)
	    (check-strict-mode-eval id "Declaration name" loc))
	 (when (j2s-let? this)
	    (unless (eq? ndecl this)
	       (raise
		  (instantiate::&io-parse-error
		     (proc "symbol resolution (symbol)")
		     (msg (format "Illegal redefinition `~a'" id))
		     (obj id)
		     (fname (cadr loc))
		     (location (caddr loc))))))
	 (let ((rhs (resolve! val env mode withs wenv ctx conf)))
	    (if (j2s-let-opt? this)
		(begin
		   (set! val rhs)
		   (instantiate::J2SNop (loc loc)))
		(begin
		   (set! val (instantiate::J2SUndefined (loc loc)))
		   (instantiate::J2SStmtExpr
		      (loc loc)
		      (expr (instantiate::J2SInit
			       (loc loc)
			       (lhs (j2sref ndecl loc withs wenv))
			       (rhs rhs))))))))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SDeclFun ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SDeclFun env mode withs wenv ctx conf)
   (with-access::J2SDecl this (loc)
      (instantiate::J2SNop
	 (loc loc))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SDeclExtern ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SDeclExtern env mode withs wenv ctx conf)
   (with-access::J2SDecl this (loc)
      (instantiate::J2SNop
	 (loc loc))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SDeclClass ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SDeclClass env mode withs wenv ctx conf)
   (with-access::J2SDeclClass this (loc val)
      (let ((nenv (cons this env)))
	 (set! val (resolve! val nenv mode withs wenv ctx conf))
	 this)))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SClass ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SClass env mode withs wenv ctx conf)
   (with-access::J2SClass this (name decl elements super)
      (set! super (resolve! super env mode withs wenv ctx conf))
      (let ((nenv (if decl (cons decl env) env))
	    (ctx (make-ctx 'class this)))
	 (set! elements
	    (map! (lambda (m) (resolve! m nenv mode withs wenv ctx conf))
	       elements))
	 this)))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SClassElement ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SClassElement env mode withs wenv ctx conf)
   (with-access::J2SClassElement this (static prop)
      (cond
	 (static
	  (set! prop (resolve! prop env mode withs withs #f conf)))
	 ((not (isa? prop J2SDataPropertyInit))
	  (set! prop (resolve! prop env mode withs withs #f conf)))
	 (else
	  (with-access::J2SDataPropertyInit prop (name)
	     (if (and (isa? name J2SString)
		      (with-access::J2SString name (val)
			 (string=? val "constructor")))
		 (let ((nctx (make-ctx 'ctor (ctx-value ctx))))
		    (set! prop (resolve! prop env mode withs withs nctx conf)))
		 (set! prop (resolve! prop env mode withs withs ctx conf)))))))
   this)

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SAssign ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SAssig env mode withs wenv ctx conf)
   (call-default-walker)
   (when (eq? mode 'strict)
      ;; strict mode restrictions
      (with-access::J2SAssig this (lhs loc)
	 (let loop ((lhs lhs))
	    (cond
	       ((isa? lhs J2SRef)
		(with-access::J2SRef lhs (decl)
		   (with-access::J2SDecl decl (id)
		      (check-strict-mode-eval id "Assignment to" loc)
		      (check-immutable decl loc conf))))
	       ((isa? lhs J2SUnresolvedRef)
		(with-access::J2SUnresolvedRef lhs (id)
		   (check-strict-mode-eval id "Assignment to" loc)))
	       ((isa? lhs J2SParen)
		(with-access::J2SParen lhs (expr)
		   (loop expr)))))))
   this)

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SObjInit ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SObjInit env mode withs wenv ctx conf)
   
   (define (find-property id::obj env::pair-nil)
      (find (lambda (prop)
	       (with-access::J2SPropertyInit prop (name)
		  (when (isa? name J2SLiteralValue)
		     (with-access::J2SLiteralValue name (val)
			(equal? id val)))))
	 env))
   
   (define (property-error id msg loc)
      (raise
	 (instantiate::&io-parse-error
	    (proc "symbol resolution (symbol)")
	    (msg msg)
	    (obj id)
	    (fname (cadr loc))
	    (location (caddr loc)))))
   
   (define (proc? o)
      (isa? o J2SFun))
   
   (with-access::J2SObjInit this (inits)
      (let loop ((inits inits)
		 (ninits '()))
	 (if (null? inits)
	     (begin
		(set! inits (reverse! ninits))
		this)
	     (with-access::J2SPropertyInit (car inits) (name loc)
		(walk! (car inits) env mode withs wenv ctx conf)
		(if (isa? name J2SLiteralValue)
		    (with-access::J2SLiteralValue name (val)
		       (let ((old (find-property val ninits)))
			  (cond
			     ((not old)
			      (loop (cdr inits) (cons (car inits) ninits)))
			     ((not (eq? (object-class old) (object-class (car inits))))
			      (property-error name
				 "duplicate data property in object literal not allowed in strict mode"
				 loc))
			     ((isa? (car inits) J2SAccessorPropertyInit)
			      (with-access::J2SAccessorPropertyInit (car inits) (get set)
				 (with-access::J2SAccessorPropertyInit old
				       ((oget get) (oset set))
				    (if (or (and (proc? oget) (proc? oset))
					    (and (proc? get) (proc? set))
					    (and (proc? oget) (proc? get))
					    (and (proc? oset) (proc? set)))
					(property-error name
					   "duplicate data property in object literal not allowed in strict mode"
					   loc)
					(begin
					   (unless (proc? oset) (set! oset set))
					   (unless (proc? oget) (set! oget get))
					   (loop (cdr inits) ninits))))))
			     ((eq? mode 'strict)
			      (property-error name
				 "duplicate data property in object literal not allowed in strict mode"
				 loc))
			     (else
			      (loop (cdr inits) (cons (car inits) ninits))))))
		    (loop (cdr inits) (cons (car inits) ninits))))))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SOctalNumber ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SOctalNumber env mode withs wenv ctx conf)
   (if (eq? mode 'strict)
       (with-access::J2SOctalNumber this (loc val)
	  (raise
	     (instantiate::&io-parse-error
		(proc "symbol resolution (symbol)")
		(msg "octal literals are not allowed in strict mode")
		(obj val)
		(fname (cadr loc))
		(location (caddr loc)))))
       (call-default-walker)))
		
;*---------------------------------------------------------------------*/
;*    resolve! ::J2SString ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SString env mode withs wenv ctx conf)
   (with-access::J2SString this (loc val escape)
      (if (and (eq? mode 'strict) (memq 'octal escape))
	  (raise
	     (instantiate::&io-parse-error
		(proc "symbol resolution (symbol)")
		(msg "octal literals are not allowed in strict mode")
		(obj val)
		(fname (cadr loc))
		(location (caddr loc))))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SComprehension ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SComprehension env mode withs wenvs ctx conf)
   (with-access::J2SComprehension this (test expr iterables decls)
      (set! iterables
	 (map (lambda (iterable)
		 (resolve! iterable env mode withs wenvs ctx conf))
	    iterables))
      (let ((nenv (append decls env)))
	 (set! test (resolve! test nenv mode withs wenvs ctx conf))
	 (set! expr (resolve! expr nenv mode withs wenvs ctx conf))
	 this)))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SCall ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SCall env mode withs withs ctx conf)
   (call-default-walker)
   (with-access::J2SCall this (fun)
      (when (isa? fun J2SSuper)
	 ;; direct calls to super are only permitted from within constructors
	 (unless (ctx-ctor? ctx)
	    (with-access::J2SSuper fun (loc)
	       (raise
		  (instantiate::&io-parse-error
		     (proc "symbol resolution (symbol)")
		     (msg "`super' keyword unexpected here")
		     (obj (j2s-expression-src loc conf "super"))
		     (fname (cadr loc))
		     (location (caddr loc)))))))
      this))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SSuper ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SSuper env mode withs wenvs ctx conf)
   (with-access::J2SSuper this (loc clazz)
      (if ctx
	  (begin
	     (set! clazz (ctx-value ctx))
	     this)
	  (raise
	     (instantiate::&io-parse-error
		(proc "symbol resolution (symbol)")
		(msg "`super' keyword unexpected here")
		(obj (j2s-expression-src loc conf "super"))
		(fname (cadr loc))
		(location (caddr loc)))))))
      
;*---------------------------------------------------------------------*/
;*    resolve-tilde! ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve-tilde! this::J2SNode resolvers)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    resolve-tilde! ::J2STilde ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve-tilde! this::J2STilde resolvers)
   (with-access::J2STilde this (stmt)
      (resolve-tilde! stmt (cons resolve-tilde! resolvers))
      this))

;*---------------------------------------------------------------------*/
;*    resolve-tilde! ::J2SDollar ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve-tilde! this::J2SDollar resolvers)
   (if (null? resolvers)
       (with-access::J2SDollar this (loc)
	  (raise
	     (instantiate::&io-parse-error
		(proc "symbol resolution (symbol)")
		(msg "Illegal ${...} expression")
		(obj this)
		(fname (cadr loc))
		(location (caddr loc)))))
       (with-access::J2SDollar this (node)
	  (set! node ((car resolvers) node (cdr resolvers)))
	  this)))

;*---------------------------------------------------------------------*/
;*    collect-let ...                                                  */
;*---------------------------------------------------------------------*/
(define (collect-let::pair-nil nodes::pair-nil)
   (append-map (lambda (d)
		  (if (isa? d J2SVarDecls)
		      (with-access::J2SVarDecls d (decls)
			 (filter (lambda (d::J2SDecl)
				    (when (j2s-let? d)
				       (with-access::J2SDecl d (scope)
					  (unless (eq? d 'loop)
					     d))))
			    decls))
		      '()))
      nodes))

;*---------------------------------------------------------------------*/
;*    collect* ::J2SNode ...                                           */
;*    -------------------------------------------------------------    */
;*    Collect all the variable declared in a tree.                     */
;*---------------------------------------------------------------------*/
(define-walk-method (collect* this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect* ::J2SVarDecls ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (collect* this::J2SVarDecls)
   (with-access::J2SVarDecls this (decls)
      (filter-map (lambda (d::J2SDecl)
		     (cond
			((j2s-let? d) #f)
			((isa? d J2SDeclFun) d)
			((isa? d J2SDeclExtern) d)
			((isa? d J2SDeclInit) (duplicate::J2SDecl d))
			(else d)))
	 decls)))

;*---------------------------------------------------------------------*/
;*    collect* ::J2SDecl ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (collect* this::J2SDecl)
   (if (j2s-var? this) (list this) '()))

;*---------------------------------------------------------------------*/
;*    collect* ::J2SDeclFun ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (collect* this::J2SDeclFun)
   (list this))

;*---------------------------------------------------------------------*/
;*    collect* ::J2SDeclExtern ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (collect* this::J2SDeclExtern)
   (list this))

;*---------------------------------------------------------------------*/
;*    collect* ::J2SFun ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (collect* this::J2SFun)
   '())

;*---------------------------------------------------------------------*/
;*    collect* ::J2SDollar ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (collect* this::J2SDollar)
   '())

;*---------------------------------------------------------------------*/
;*    rewrite-arguments! ::J2SNode ...                                 */
;*    -------------------------------------------------------------    */
;*    Rewrite argument references into an arguments access.            */
;*---------------------------------------------------------------------*/
(define-walk-method (rewrite-arguments! this::J2SNode arguments params mode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    rewrite-arguments ::J2SRef ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (rewrite-arguments! this::J2SRef arguments params mode)

   (define (find-index decl params)
      (let loop ((i 0)
		 (params params))
	 (when (pair? params)
	    (if (eq? decl (car params))
		i
		(loop (+fx i 1)
		   (cdr params))))))
   
   (with-access::J2SRef this (loc decl)
      (let ((i (find-index decl params)))
	 (if i
	    ;; rewrite id -> arguments[ i ]
	    (let ((ref (instantiate::J2SRef
			  (loc loc)
			  (decl arguments)))
		  (index (instantiate::J2SNumber
			    (loc loc)
			    (val i))))
	       (instantiate::J2SAccess
		  (loc loc)
		  (obj ref)
		  (field index)))
	    this))))

