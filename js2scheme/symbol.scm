;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/symbol.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 13 16:57:00 2013                          */
;*    Last change :  Fri Jan 31 16:30:12 2020 (serrano)                */
;*    Copyright   :  2013-20 Manuel Serrano                            */
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

   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_lexer)

   (export j2s-symbol-stage))

;*---------------------------------------------------------------------*/
;*    j2s-symbol-stage                                                 */
;*---------------------------------------------------------------------*/
(define j2s-symbol-stage
   (instantiate::J2SStageProc
      (name "symbol")
      (comment "symbol resolution")
      (footer "")
      (proc j2s-symbol)))

;*---------------------------------------------------------------------*/
;*    j2s-symbol ...                                                   */
;*    -------------------------------------------------------------    */
;*    Warning, headers are not scanned for variable resolution!        */
;*---------------------------------------------------------------------*/
(define (j2s-symbol this conf)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes loc mode headers decls)
	 ;; filters out double definitions
	 (set! nodes (decl-cleanup-duplicate! nodes))
	 (let* ((%this (instantiate::J2SDecl
			  (loc loc)
			  (utype 'object)
			  (vtype 'object)
			  (id 'this)
			  (_scmid '%this)))
		(%dummy (instantiate::J2SDecl
			   (loc loc)
			   (id '__%dummy%__)))
		(hds (append-map (lambda (s) (collect* s)) headers))
		(vars (append-map (lambda (s) (collect* s)) nodes))
		(lets (collect-let nodes))
		(env (append decls hds vars lets))
		(genv (list %this %dummy))
		(scope (config-get conf :bind-global '%scope))
		(vdecls (bind-decls! vars env mode scope '() '() genv conf)))
	    (when (pair? vars)
	       (set! decls
		  (append decls
		     (filter (lambda (d) (isa? d J2SDecl)) vdecls))))
	    (when (pair? lets)
	       (for-each (lambda (d::J2SDecl)
			    (with-access::J2SDecl d (scope)
			       (unless (eq? scope 'export)
				  (set! scope 'global))))
		  lets)
	       (set! decls (append decls lets)))
	    (set! nodes
	       (append (filter (lambda (d) (not (isa? d J2SDecl))) vdecls)
		  nodes))
	    (set! nodes
	       (map! (lambda (o) (resolve! o env mode '() '() genv 'plain conf))
		  nodes))
	    (when (config-get conf :commonjs-export #f)
	       (commonjs-export this (find-decl 'module env)))
	    (for-each (lambda (d)
			 (with-access::J2SDecl d (scope)
			    (when (eq? scope 'unbound)
			       (set! decls (cons d decls)))))
	       genv))))
   (when (and (>= (config-get conf :verbose 0) 2)
	      (not (config-get conf :verbmargin #f)))
      (newline (current-error-port)))
   this)

;*---------------------------------------------------------------------*/
;*    commonjs-export ...                                              */
;*    -------------------------------------------------------------    */
;*    For compatibility with common.js modules, this function forces   */
;*    a default module clause if none is explicitly given. The value   */
;*    of the forced default module clause is MODULE.EXPORTS.           */
;*---------------------------------------------------------------------*/
(define (commonjs-export this::J2SProgram moddecl)
   
   (define (export-default-stmt moddecl index loc)
      (co-instantiate ((expo (instantiate::J2SExport
				(id 'default)
				(alias 'default)
				(decl decl)
				(index index)))
		       (decl (instantiate::J2SDecl
				(loc loc)
				(id 'default)
				(vtype 'any)
				(exports (list expo))
				(binder 'export)
				(scope 'export))))
	 (values expo
	    ;; Moddecl (the module declaration) is #f if the module has
	    ;; been parsed for import and not for compilation. In that
	    ;; case a fake empty code that will never be executed
	    ;; is enough to get the default declaration correct
	    (if moddecl
		(J2SStmtExpr
		   (J2SAssig (J2SRef decl)
		      (J2SAccess (J2SRef moddecl)
			 (J2SString "exports"))))
		(J2SNop))
	    (and moddecl decl))))
   
   (with-access::J2SProgram this (nodes loc exports decls)
      (unless (find (lambda (e)
		       (with-access::J2SExport e (id) (eq? id 'default)))
		 exports)
	 ;; force a default export if non specified
	 (multiple-value-bind (expo stmt decl)
	    (export-default-stmt moddecl (length exports) loc)
	    (when decl (set! decls (cons decl decls)))
	    (set! exports (cons expo exports))
	    (set! nodes (append nodes (list stmt)))))))
   
;*---------------------------------------------------------------------*/
;*    decl-cleanup-duplicate! ...                                      */
;*---------------------------------------------------------------------*/
(define (decl-cleanup-duplicate! nodes)
   (let loop ((n nodes)
	      (funs '())
	      (nnodes '())
	      (env '()))
      (cond
	 ((null? n)
	  (reverse! (append nnodes funs)))
	 ((not (isa? (car n) J2SDecl))
	  (loop (cdr n) funs (cons (car n) nnodes) env))
	 (else
	  (with-access::J2SDecl (car n) (id)
	     (let ((old (find-decl id env)))
		(cond
		   ((not old)
		    (loop (cdr n)
		       funs
		       (cons (car n) nnodes)
		       (cons (car n) env)))
		   ((or (isa? (car n) J2SDeclFun)
			(isa? (car n) J2SDeclExtern))
		    (loop (cdr n)
		       (cons (decl->assign! (car n) old '() '()) funs)
		       nnodes
		       env))
		   (else
		    (loop (cdr n)
		       funs
		       (cons (decl->assign! (car n) old '() '()) nnodes)
		       env)))))))))

;*---------------------------------------------------------------------*/
;*    eq-conf-lang? ...                                                */
;*---------------------------------------------------------------------*/
(define (eq-conf-lang? conf lang)
   (string=? (config-get conf :language "hopscript") lang))

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
;*    decl->assign! ...                                                */
;*---------------------------------------------------------------------*/
(define (decl->assign! decl::J2SDeclInit old::J2SDecl withs wenv)
   (with-access::J2SDeclInit decl (loc id val)
      (instantiate::J2SStmtExpr
	 (loc loc)
	 (expr (instantiate::J2SAssig
		  (loc loc)
		  (lhs (j2sref old loc withs wenv))
		  (rhs val))))))

;*---------------------------------------------------------------------*/
;*    bind-decls! ...                                                  */
;*---------------------------------------------------------------------*/
(define (bind-decls! decls env mode scope::symbol withs::pair-nil wenv::pair-nil genv::pair-nil conf)
   (let loop ((decls (map (lambda (e) (bind! e env genv mode conf)) decls))
	      (acc '())
	      (funs '()))
      (if (null? decls)
	  (append (reverse! acc) (reverse! funs))
	  (let ((decl (car decls)))
	     (with-access::J2SDecl decl (id (bscope scope))
		(unless (eq? bscope 'export)
		   (set! bscope scope))
		(let ((old (find-decl id acc)))
		   (if old
		       (if (or (isa? decl J2SDeclFun)
			       (isa? decl J2SDeclExtern))
			   (let ((fun (decl->assign! decl old withs wenv)))
			      (loop (cdr decls) acc (cons fun funs)))
			   (loop (cdr decls) acc funs))
		       (loop (cdr decls) (cons decl acc) funs))))))))

;*---------------------------------------------------------------------*/
;*    bind! ::J2SDecl ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (bind! this::J2SDecl env genv mode conf)
   this)

;*---------------------------------------------------------------------*/
;*    bind! ::J2SDeclFun ...                                           */
;*---------------------------------------------------------------------*/
(define-method (bind! this::J2SDeclFun env genv mode conf)
   (with-access::J2SDeclFun this (val id)
      (set! val (resolve! val env mode '() '() genv 'plain conf))
      this))

;*---------------------------------------------------------------------*/
;*    make-ctx ...                                                     */
;*---------------------------------------------------------------------*/
(define (make-ctx typ val)
   (cons typ val))

;*---------------------------------------------------------------------*/
;*    ctx-value ...                                                    */
;*---------------------------------------------------------------------*/
(define (ctx-value ctx)
   (cdr ctx))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SNode env mode withs wenv genv ctx::symbol conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SFun ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SFun env mode withs wenv genv ctx conf)

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

   (with-access::J2SFun this (body params thisp loc (fmode mode) decl name
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
			      (argid (gensym 'arguments))
			      (utype (if (eq? fmode 'normal) 'any 'arguments))
			      (mode mode)
			      (loc loc)))
		(envl (append decls params))
		(env1 (append envl env0))
		(ldecls (with-access::J2SBlock body (nodes)
			   (collect-let nodes)))
		(nenv (if (find-decl 'arguments envl)
			  (append ldecls env1)
			  (cons arguments (append ldecls env1))))
		(bdenv (if (isa? thisp J2SDecl) (cons thisp nenv) nenv))
		(nwenv (cons arguments (append decls params wenv))))
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
				   (bind-decls! decls bdenv fmode 'inner
				      withs wenv genv conf)
				   (list (walk! body bdenv fmode
					    withs nwenv genv
					    ctx conf)))))))
		(set! body (walk! body bdenv fmode withs nwenv genv ctx conf)))
	    (with-access::J2SDeclArguments arguments (usecnt)
	       (when (>fx usecnt 0)
		  (with-access::J2SFun this (vararg argumentsp params)
		     (if vararg
			 (with-access::J2SDecl (car (last-pair params)) (id loc)
			    (raise
			       (instantiate::&io-parse-error
				  (proc "symbol resolution (symbol)")
				  (msg "\"arguments\" object may not be used in conjunction with a rest parameter")
				  (obj id)
				  (fname (cadr loc))
				  (location (caddr loc)))))
			 (begin
			    (set! argumentsp arguments)
			    (set! vararg 'arguments)))))))))
   this)

;*---------------------------------------------------------------------*/
;*    resolve! ::J2STilde ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2STilde env mode withs wenv genv ctx conf)
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
		  (resolve! this env mode withs wenv genv ctx conf))))
      this))

;*---------------------------------------------------------------------*/
;*    resolve-let! ...                                                 */
;*---------------------------------------------------------------------*/
(define (resolve-let! this::J2SBlock env mode withs wenv genv decls ctx conf)
   (with-access::J2SBlock this (loc endloc nodes)
      (let ((nenv (append decls env)))
	 (instantiate::J2SLetBlock
	    (loc loc)
	    (endloc endloc)
	    (decls decls)
	    (nodes (map! (lambda (n)
			    (resolve! n nenv mode withs wenv genv ctx conf))
		      nodes))))))
   
;*---------------------------------------------------------------------*/
;*    resolve! ::J2SBlock ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SBlock env mode withs wenv genv ctx conf)
   ;; a block is a letrec if it contains let or const declaration
   (with-access::J2SBlock this (nodes endloc)
      (let ((ldecls (collect-let nodes)))
	 (if (pair? ldecls)
	     (resolve-let! this env mode withs wenv genv ldecls ctx conf)
	     (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SWith ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SWith env mode withs wenv genv ctx conf)
   (if (eq? mode 'normal)
       (with-access::J2SWith this (obj block loc id)
	  (set! obj (resolve! obj env mode withs wenv genv ctx conf))
	  (set! block (resolve! block env mode (cons id withs) '() genv ctx conf))
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
(define-walk-method (resolve! this::J2SCatch env mode withs wenv genv ctx conf)
   (with-access::J2SCatch this (body param loc)
      ;; propagate the compilation mode to the function
      (let ((nenv (cons param env)))
	 (when (eq? mode 'strict)
	    (with-access::J2SDecl param (id)
	       (check-strict-mode-eval id "Catch name" loc)))
	 (set! body (walk! body nenv mode withs wenv genv ctx conf))))
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
;*    resolve! ::J2SLabel ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SLabel env mode withs wenv genv ctx conf)
   (with-access::J2SLabel this (body loc)
      (if (or (isa? body J2SFor) (isa? body J2SForIn))
	  (let ((n (resolve! body env mode withs wenv genv ctx conf)))
	     (cond
		((isa? n J2SLetBlock)
		 (with-access::J2SLetBlock n (nodes endloc)
		    (let ((nbody (instantiate::J2SBlock
				    (loc loc)
				    (endloc endloc)
				    (nodes nodes))))
		       (set! nodes
			  (list (duplicate::J2SLabel this
				   (body nbody))))
		       n)))
		((isa? n J2SBlock)
		 (with-access::J2SBlock n (nodes endloc)
		    (set! body (cadr nodes))
		    (instantiate::J2SBlock
		       (loc loc)
		       (endloc endloc)
		       (nodes (list (car nodes) this)))))
		(else
		 (set! body n)
		 this)))
	  (call-default-walker))))
	 
;*---------------------------------------------------------------------*/
;*    resolve! ::J2SFor ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SFor env mode withs wenv genv ctx conf)

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
	  (resolve! (for-let this) env mode withs wenv genv ctx conf))
	 (else
	  (resolve! (for-var this) env mode withs wenv genv ctx conf)))))
   
;*---------------------------------------------------------------------*/
;*    resolve! ::J2SForIn ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SForIn env mode withs wenv genv ctx conf)

   (define (let->init d::J2SDecl writable)
      (with-access::J2SDecl d (loc id)
	 (instantiate::J2SDeclInit
	    (loc loc)
	    (id id)
	    (writable writable)
	    (binder 'let-opt)
	    (val (instantiate::J2SRef
		    (loc loc)
		    (decl d))))))
   
   (define (for-in-let for)
      (with-access::J2SForIn for (lhs loc body)
	 (with-access::J2SVarDecls lhs (loc decls)
	    (with-access::J2SDecl (car decls) (loc id binder writable)
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
				     (decls (list (let->init decl writable))))
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
	  (set! lhs (resolve! lhs env mode withs wenv genv ctx conf))
	  (set! obj (resolve! obj env mode withs wenv genv ctx conf))
	  (set! body (resolve! body env mode withs wenv genv ctx conf))
	  this)
	 ((let-init? lhs)
	  (resolve! (for-in-let this) env mode withs wenv genv ctx conf))
	 (else
	  (resolve! (for-in-var this) env mode withs wenv genv ctx conf)))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SUnresolvedRef ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SUnresolvedRef env mode withs wenv genv ctx conf)
   (with-access::J2SUnresolvedRef this (id loc)
      (let ((decl (find-decl (if (eq? id 'super) 'this id) env)))
	 (cond
	    ((isa? decl J2SDecl)
	     (case id
		((this)
		 (instantiate::J2SThis
		    (loc loc)
		    (decl decl)))
		((super)
		 (resolve!
		    (instantiate::J2SSuper
		       (loc loc)
		       (decl decl))
		    env mode withs wenv genv ctx conf))
		(else
		 ;; mark arguments used to avoid retraversing
		 ;; the function body
		 (when (isa? decl J2SDeclArguments)
		    (with-access::J2SDeclArguments decl (usecnt)
		       (set! usecnt (+fx 1 usecnt))))
		 (j2sref decl loc withs wenv))))
	    ((pair? withs)
	     (instantiate::J2SWithRef
		(loc loc)
		(withs withs)
		(id id)
		(expr this)))
	    ((find-decl id genv)
	     =>
	     (lambda (decl)
		(if (eq? id 'this)
		    (instantiate::J2SThis
		       (loc loc)
		       (decl decl))
		    (instantiate::J2SGlobalRef
		       (id id)
		       (loc loc)
		       (decl decl)))))
	    (else
	     (let ((decl (instantiate::J2SDecl
			    (_usage (usage '()))
			    (utype 'any)
			    (scope 'unbound)
			    (loc loc)
			    (id id))))
		(set-cdr! (last-pair genv) (list decl))
		(instantiate::J2SGlobalRef
		   (id id)
		   (loc loc)
		   (decl decl))))))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SExportVars ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SExportVars env mode withs wenv genv ctx conf)
   (call-default-walker)
   (with-access::J2SExportVars this (refs aliases program)
      (when (isa? program J2SProgram)
	 (for-each (lambda (ref alias)
		      (cond
			 ((isa? ref J2SRef)
			  (with-access::J2SRef ref (decl loc)
			     (with-access::J2SDecl decl (scope id)
				(if (memq scope '(global export %scope))
				    (export-decl decl alias program loc)
				    (export-err id loc
				       (format "bad scope (~s)" scope))))))
			 ((isa? ref J2SGlobalRef)
			  (with-access::J2SGlobalRef ref (loc decl)
			     (with-access::J2SDecl decl (id)
				(export-err id loc "global ref"))))
			 ((isa? ref J2SWithRef)
			  (with-access::J2SWithRef ref (loc id)
			     (export-err id loc "with ref")))
			 ((isa? ref J2SHopRef)
			  (with-access::J2SHopRef ref (loc id)
			     (export-err id loc "hop ref")))))
	    refs aliases)))
   this)

;*---------------------------------------------------------------------*/
;*    export-decl ...                                                  */
;*---------------------------------------------------------------------*/
(define (export-decl decl::J2SDecl alias::symbol program::J2SProgram loc)
   (with-access::J2SProgram program ((allexports exports) path)
      (with-access::J2SDecl decl (id exports binder)
	 (set! binder 'export)
	 (cond
	    ((null? exports)
	     (let ((e (instantiate::J2SExport
			 (id alias)
			 (alias alias)
			 (index (j2sprogram-get-export-index program))
			 (decl decl))))
		(set! exports (list e))
		(set! allexports (cons e allexports))))
	    ((find (lambda (e)
		      (with-access::J2SExport e (id)
			 (eq? id alias)))
		allexports)
	     (export-err alias loc (format "duplicate export \"~a\"" id)))
	    (else
	     (with-access::J2SExport (car exports) (index)
		(let ((e (instantiate::J2SExport
			    (id alias)
			    (alias alias)
			    (index index)
			    (decl decl))))
		   (set! exports (cons e exports))
		   (set! allexports (cons e allexports)))))))))

;*---------------------------------------------------------------------*/
;*    export-error ...                                                 */
;*---------------------------------------------------------------------*/
(define (export-err id loc #!optional msg)
   (raise
      (instantiate::&io-parse-error
	 (proc "symbol resolution (symbol)")
	 (msg (string-append "Illegal variable export"
		 (if msg (string-append ", " msg) "")))
	 (obj id)
	 (fname (cadr loc))
	 (location (caddr loc)))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SVarDecls ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SVarDecls env mode withs wenv genv ctx conf)
   (with-access::J2SVarDecls this (decls loc)
      (let ((ndecls (filter-map (lambda (d)
				   (let ((nd (resolve! d env mode withs wenv genv ctx conf)))
				      (unless (isa? nd J2SNop) nd)))
		       decls)))
	 (if (pair? ndecls)
	     (instantiate::J2SSeq
		(loc loc)
		(nodes ndecls))
	     (instantiate::J2SNop
		(loc loc))))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SDecl ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SDecl env mode withs wenv genv ctx conf)
   (with-access::J2SDecl this (loc id)
      (when (eq? mode 'strict)
	 (check-strict-mode-eval id  "Declaration name" loc))
      (instantiate::J2SNop
	 (loc loc))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SDeclInit ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SDeclInit env mode withs wenv genv ctx conf)
   (with-access::J2SDeclInit this (loc id val exports)
      (let ((ndecl::J2SDecl (or (find-decl id env) this)))
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
	 (let ((rhs (resolve! val env mode withs wenv genv ctx conf)))
	    (when (isa? rhs J2SFun)
	       (with-access::J2SFun rhs (name)
		  (when (eq? name '||)
		     (set! name id))))
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
(define-walk-method (resolve! this::J2SDeclFun env mode withs wenv genv ctx conf)
   (with-access::J2SDecl this (loc exports id)
      (instantiate::J2SNop
	 (loc loc))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SDeclExtern ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SDeclExtern env mode withs wenv genv ctx conf)
   (with-access::J2SDecl this (loc)
      (instantiate::J2SNop
	 (loc loc))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SDeclClass ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SDeclClass env mode withs wenv genv ctx conf)
   (with-access::J2SDeclClass this (loc val)
      (let ((nenv (cons this env)))
	 (set! val (resolve! val nenv mode withs wenv genv ctx conf))
	 this)))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SClass ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SClass env mode withs wenv genv ctx conf)
   (with-access::J2SClass this (name decl elements super)
      (set! super (resolve! super env mode withs wenv genv ctx conf))
      (let ((nenv (if decl (cons decl env) env)))
	 (set! elements
	    (map! (lambda (m) (resolve! m nenv mode withs wenv genv ctx conf))
	       elements))
	 this)))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SClassElement ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SClassElement env mode withs wenv genv ctx conf)
   (with-access::J2SClassElement this (static prop)
      (cond
	 (static
	  (set! prop (resolve! prop env mode withs withs genv ctx conf)))
	 ((not (isa? prop J2SDataPropertyInit))
	  (set! prop (resolve! prop env mode withs withs genv 'class conf)))
	 (else
	  (with-access::J2SDataPropertyInit prop (name)
	     (if (and (isa? name J2SString)
		      (with-access::J2SString name (val)
			 (string=? val "constructor")))
		 (set! prop (resolve! prop env mode withs withs genv 'ctor conf))
		 (set! prop (resolve! prop env mode withs withs genv 'class conf)))))))
   this)

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SAssign ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SAssig env mode withs wenv genv ctx conf)
   
   (define (lhs-name node)
      (cond
	 ((isa? node J2SRef)
	  (with-access::J2SRef node (decl) (lhs-name decl)))
	 ((isa? node J2SDecl)
	  (with-access::J2SDecl node (id) (symbol->string id)))
	 ((isa? node J2SAccess)
	  (with-access::J2SAccess node (obj field)
	     (format "~a.~a" (lhs-name obj) (lhs-name field))))
	 ((isa? node J2SString)
	  (with-access::J2SString node (val) val))
	 ((isa? node J2SNumber)
	  (with-access::J2SNumber node (val) (number->string val)))
	 (else
	  "")))
   
   (call-default-walker)

   ;; find a good name for function assignment
   (with-access::J2SAssig this (lhs rhs)
      (when (isa? rhs J2SFun)
	 (with-access::J2SFun rhs (name)
	    (cond
	       ((eq? name '||)
		(let ((better-name (lhs-name lhs)))
		   (unless (string=? better-name "")
		      (set! name (string->symbol better-name)))))
	       ((string-index (symbol->string! name) #\@)
		=>
		(lambda (i)
		   (let ((better-name (lhs-name lhs)))
		      (unless (string=? better-name "")
			 (set! name (string->symbol better-name))))))))))
   
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
(define-walk-method (resolve! this::J2SObjInit env mode withs wenv genv ctx conf)
   
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
		(resolve! (car inits) env mode withs wenv genv 'literal conf)
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
(define-walk-method (resolve! this::J2SOctalNumber env mode withs wenv genv ctx conf)
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
(define-walk-method (resolve! this::J2SString env mode withs wenv genv ctx conf)
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
;*    resolve! ::J2SCall ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SCall env mode withs wenvs genv ctx conf)
   (call-default-walker)
   (with-access::J2SCall this (fun)
      (when (isa? fun J2SSuper)
	 ;; direct calls to super are only permitted from within constructors
	 (when (eq? ctx 'plain)
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
(define-walk-method (resolve! this::J2SSuper env mode withs wenvs genv ctx conf)
   (with-access::J2SSuper this (loc context)
      (if (eq? ctx 'plain)
	  (raise
	     (instantiate::&io-parse-error
		(proc "symbol resolution (symbol)")
		(msg "`super' keyword unexpected here")
		(obj (j2s-expression-src loc conf "super"))
		(fname (cadr loc))
		(location (caddr loc))))
	  (begin
	     (set! context ctx)
	     this))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SDProducer ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SDProducer env mode withs wenvs genv ctx conf)
   (with-access::J2SDProducer this (decl)
      (with-access::J2SDecl decl (id)
	 (let ((d (find-decl id env)))
	    (set! decl d)))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SDConsumer ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SDConsumer env mode withs wenvs genv ctx conf)
   (with-access::J2SDConsumer this (decl)
      (with-access::J2SDecl decl (id)
	 (let ((d (find-decl id env)))
	    (set! decl d)))
      (call-default-walker)))

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
;*    Collect all the variables declared in a tree.                    */
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
			((j2s-let? d)
			 #f)
			((isa? d J2SDeclFun)
			 d)
			((isa? d J2SDeclExtern)
			 d)
			((isa? d J2SDeclInit)
			 (duplicate::J2SDecl d (key (ast-decl-key))))
			(else
			 d)))
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

;*---------------------------------------------------------------------*/
;*    check-immutable ...                                              */
;*---------------------------------------------------------------------*/
(define (check-immutable decl::J2SDecl loc conf)
   (with-access::J2SDecl decl (writable id)
      (when (and (not writable) (or (isa? decl J2SDeclClass) (isa? decl J2SDeclFun)))
	 (cond
	    ((eq-conf-lang? conf "hopscript")
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
