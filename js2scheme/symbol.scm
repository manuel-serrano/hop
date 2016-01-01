;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/symbol.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 13 16:57:00 2013                          */
;*    Last change :  Thu Dec 31 09:01:39 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
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
	     (vdecls (bind-decls! vars env mode scope '() '())))
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
	    (map! (lambda (o) (resolve! o env mode '() '())) nodes))))
   this)

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
   (map (lambda (d::J2SDecl) (with-access::J2SDecl d (id) id)) env))

;*---------------------------------------------------------------------*/
;*    bind-decls! ...                                                  */
;*---------------------------------------------------------------------*/
(define (bind-decls! decls env mode scope::symbol withs::pair-nil wenv::pair-nil)
   
   (define (decl->assign! decl::J2SDeclInit old::J2SDecl)
      (with-access::J2SDeclInit decl (loc id val)
	 (instantiate::J2SStmtExpr
	    (loc loc)
	    (expr (instantiate::J2SInit
		     (loc loc)
		     (lhs (j2sref old loc withs wenv))
		     (rhs val))))))

   (let loop ((decls (map (lambda (e) (bind! e env mode)) decls))
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
(define-generic (bind! this::J2SDecl env mode)
   this)

;*---------------------------------------------------------------------*/
;*    bind! ::J2SDeclFun ...                                           */
;*---------------------------------------------------------------------*/
(define-method (bind! this::J2SDeclFun env mode)
   (with-access::J2SDeclFun this (val id)
      (set! val (resolve! val env mode '() '()))
      this))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SNode env mode withs wenv)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SFun ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SFun env mode withs wenv)

   (define (check-strict-mode-params params loc)
      ;; Check the extra cnnstrains parameter names have in strict mode
      (let loop ((p params))
	 (when (pair? p)
	    (with-access::J2SDecl (car p) (id)
	       (cond
		  ((memq id '(eval arguments))
		   (raise
		      (instantiate::&io-parse-error
			 (proc "js-symbol")
			 (msg "illegal parameter name")
			 (obj id)
			 (fname (cadr loc))
			 (location (caddr loc)))))
		  ((j2s-reserved-id? id)
		   (raise
		      (instantiate::&io-parse-error
			 (proc "js-symbol")
			 (msg "illegal parameter name")
			 (obj id)
			 (fname (cadr loc))
			 (location (caddr loc)))))
		  ((find-decl id (cdr params))
		   (raise
		      (instantiate::&io-parse-error
			 (proc "js-symbol")
			 (msg "illegal duplicate parameter name")
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
   
   (with-access::J2SFun this (body params loc (fmode mode) params decl)
      (let ((id (j2sfun-id this)))
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
		(nwenv (cons arguments (append decls params wenv))))
	    (for-each (lambda (decl::J2SDecl)
			 (with-access::J2SDecl decl (scope)
			    
			    (set! scope 'fun)))
	       ldecls)
;* 	    (tprint "**** fun=" id " decl=" (debug-dump-env decls))    */
;* 	    (tprint (j2s->list body))                                  */
	    (if (pair? decls)
		(set! body
		   (with-access::J2SBlock body (endloc)
		      (instantiate::J2SBlock
			 (loc loc)
			 (endloc endloc)
			 (nodes (append
				   (bind-decls! decls nenv fmode 'inner withs wenv)
				   (list (walk! body nenv fmode withs nwenv)))))))
		(set! body (walk! body nenv fmode withs nwenv)))
	    (with-access::J2SDeclArguments arguments (usecnt)
	       (when (>fx usecnt 0)
		  (with-access::J2SFun this (vararg params)
		     (if vararg
			 (with-access::J2SDecl (car (last-pair params)) (id loc)
			    (raise
			       (instantiate::&io-parse-error
				  (proc "js-symbol")
				  (msg "\"arguments\" object may not be used in conjunction with a rest parameter")
				  (obj id)
				  (fname (cadr loc))
				  (location (caddr loc)))))
			 (set! vararg 'arguments))))))))
   this)

;; MS CARE: 25 Juil 2014, not sure it would be a good idea to traverse ~ nodes
;* {*---------------------------------------------------------------------*} */
;* {*    resolve! ::J2STilde ...                                          *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (resolve! this::J2STilde env mode withs wenv)   */
;*    (with-access::J2STilde this (stmt loc)                           */
;*       (let* ((decls (collect* stmt))                                */
;* 	     (nwenv (append decls wenv)))                              */
;* 	 (if (pair? decls)                                             */
;* 	     (set! stmt                                                */
;* 		(instantiate::J2SBlock                                 */
;* 		   (loc loc)                                           */
;* 		   (nodes (append                                      */
;* 			     (bind-decls! decls decls mode #f withs wenv) */
;* 			     (list (walk! stmt decls mode withs nwenv)))))) */
;* 	     (set! stmt (walk! stmt decls mode withs nwenv))))         */
;*       this))                                                        */

;*---------------------------------------------------------------------*/
;*    resolve-let! ...                                                 */
;*---------------------------------------------------------------------*/
(define (resolve-let! this::J2SBlock env mode withs wenv decls)
   (with-access::J2SBlock this (loc endloc nodes)
      (let ((nenv (append decls env)))
	 (instantiate::J2SLetBlock
	    (loc loc)
	    (endloc endloc)
	    (decls decls)
	    (nodes (map! (lambda (n)
			    (resolve! n nenv mode withs wenv))
		      nodes))))))
   
;*---------------------------------------------------------------------*/
;*    resolve! ::J2SBlock ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SBlock env mode withs wenv)
   ;; a block is a letrec if it contains let or const declaration
   (with-access::J2SBlock this (nodes)
      (let ((ldecls (collect-let nodes)))
	 (if (pair? ldecls)
	     (resolve-let! this env mode withs wenv ldecls)
	     (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SWith ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SWith env mode withs wenv)
   (if (eq? mode 'normal)
       (with-access::J2SWith this (obj block loc id)
	  (set! obj (resolve! obj env mode withs wenv))
	  (set! block (resolve! block env mode (cons id withs) '()))
	  this)
       (with-access::J2SWith this (loc)
	  (raise
	     (instantiate::&io-parse-error
		(proc "js-symbol")
		(msg "strict mode code may not include with statements")
		(obj "with")
		(fname (cadr loc))
		(location (caddr loc)))))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SCatch ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SCatch env mode withs wenv)
   (with-access::J2SCatch this (body param loc)
      ;; propagate the compilation mode to the function
      (let ((nenv (cons param env)))
	 (when (eq? mode 'strict)
	    (with-access::J2SDecl param (id)
	       (check-strict-mode-eval id "Catch name" loc)))
	 (set! body (walk! body nenv mode withs wenv))))
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
(define-method (resolve! this::J2SFor env mode withs wenv)
   
   (define (mark-decls-loop! decls)
      (for-each (lambda (decl::J2SDecl)
		   (with-access::J2SDecl decl (scope _scmid id)
		      (set! _scmid (symbol-append '% id))
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
   
   (define (for-let for)
      (with-access::J2SFor for (init body)
	 (with-access::J2SVarDecls init (loc decls)
	    (mark-decls-loop! decls)
	    (with-access::J2SLoop for (body)
	       ;; create local variables for the loop body
	       (set! body
		  (instantiate::J2SBlock
		     (endloc loc)
		     (loc loc)
		     (nodes (list
			       (instantiate::J2SVarDecls
				  (loc loc)
				  (decls (map let->init decls)))
			       body)))))
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
	  (resolve! (for-let this) env mode withs wenv))
	 (else
	  (resolve! (for-var this) env mode withs wenv)))))
   
;*---------------------------------------------------------------------*/
;*    resolve! ::J2SForIn ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SForIn env mode withs wenv)

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
			      (_scmid (symbol-append '% id))
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
	  (set! lhs (resolve! lhs env mode withs wenv))
	  (set! obj (resolve! obj env mode withs wenv))
	  (set! body (resolve! body env mode withs wenv))
	  this)
	 ((let-init? lhs)
	  (resolve! (for-in-let this) env mode withs wenv))
	 (else
	  (resolve! (for-in-var this) env mode withs wenv)))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SUnresolvedRef ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SUnresolvedRef env mode withs wenv)
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
(define-walk-method (resolve! this::J2SDecl env mode withs wenv)
   (with-access::J2SDecl this (loc id)
      (when (eq? mode 'strict)
	 (check-strict-mode-eval id  "Declaration name" loc))
      (instantiate::J2SNop
	 (loc loc))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SVarDecls ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SVarDecls env mode withs wenv)
   (with-access::J2SVarDecls this (decls loc)
      (let ((ndecls (filter-map (lambda (d)
				   (let ((nd (resolve! d env mode withs wenv)))
				      (unless (isa? nd J2SNop) nd)))
		       decls)))
	 (if (pair? ndecls)
	     (instantiate::J2SSeq
		(loc loc)
		(nodes ndecls))
	     (instantiate::J2SNop
		(loc loc))))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SDollar ...                                         */
;*---------------------------------------------------------------------*/
;* (define-method (resolve! this::J2SDollar env mode withs wenv)       */
;*    this)                                                            */

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
	     (proc "js-symbol")
	     (msg (format "~a eval or arguments is not allowed in strict mode"
		     msg ))
	     (obj id)
	     (fname (cadr loc))
	     (location (caddr loc)))))
      ((j2s-strict-reserved-id? id)
       (raise
	  (instantiate::&io-parse-error
	     (proc "js-symbol")
	     (msg "~a a reserved name is not allowd in strict mode")
	     (obj id)
	     (fname (cadr loc))
	     (location (caddr loc)))))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SDeclInit ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SDeclInit env mode withs wenv)
   (with-access::J2SDeclInit this (loc id val binder)
      (let ((ndecl::J2SDecl (find-decl id env)))
	 ;; strict mode restrictions
	 ;; http://www.ecma-international.org/ecma-262/5.1/#sec-10.1.1
	 (when (eq? mode 'strict)
	    (check-strict-mode-eval id "Declaration name" loc))
	 (when (j2s-let? this)
	    (unless (eq? (find-decl id env) this)
	       (raise
		  (instantiate::&io-parse-error
		     (proc "js-symbol")
		     (msg (format "~a illegal redefinition" id))
		     (obj id)
		     (fname (cadr loc))
		     (location (caddr loc))))))
	 (let ((rhs (resolve! val env mode withs wenv)))
	    (set! val rhs)
	    (instantiate::J2SStmtExpr
	       (loc loc)
	       (expr (instantiate::J2SInit
			(loc loc)
			(lhs (j2sref ndecl loc withs wenv))
			(rhs rhs))))))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SDeclFun ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SDeclFun env mode withs wenv)
   (with-access::J2SDecl this (loc)
      (instantiate::J2SNop
	 (loc loc))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SDeclExtern ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SDeclExtern env mode withs wenv)
   (with-access::J2SDecl this (loc)
      (instantiate::J2SNop
	 (loc loc))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SAssign ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SAssig env mode withs wenv)
   (when (eq? mode 'strict)
      ;; strict mode restrictions
      (with-access::J2SAssig this (lhs loc)
	 (let loop ((lhs lhs))
	    (cond
	       ((isa? lhs J2SRef)
		(with-access::J2SRef lhs (decl)
		   (with-access::J2SDecl decl (id)
		      (check-strict-mode-eval id "Assignment to" loc))))
	       ((isa? lhs J2SUnresolvedRef)
		(with-access::J2SUnresolvedRef lhs (id)
		   (check-strict-mode-eval id "Assignment to" loc)))
	       ((isa? lhs J2SParen)
		(with-access::J2SParen lhs (expr)
		   (loop expr)))))))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SObjInit ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SObjInit env mode withs wenv)
   
   (define (find-property id::obj env::pair-nil)
      (find (lambda (prop)
	       (with-access::J2SPropertyInit prop (name)
		  (with-access::J2SLiteralValue name (val)
		     (equal? id val))))
	 env))

   (define (property-error id msg loc)
      (raise
	 (instantiate::&io-parse-error
	    (proc "js-symbol")
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
	     (set! inits (reverse! ninits))
	     (with-access::J2SPropertyInit (car inits) (name loc)
		(walk! (car inits) env mode withs wenv)
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
			  (loop (cdr inits) (cons (car inits) ninits))))))))))

   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SOctalNumber ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SOctalNumber env mode withs wenv)
   (if (eq? mode 'strict)
       (with-access::J2SOctalNumber this (loc val)
	  (raise
	     (instantiate::&io-parse-error
		(proc "js-symbol")
		(msg "octal literals are not allowed in strict mode")
		(obj val)
		(fname (cadr loc))
		(location (caddr loc)))))
       (call-default-walker)))
		
;*---------------------------------------------------------------------*/
;*    resolve! ::J2SString ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SString env mode withs wenv)
   (with-access::J2SString this (loc val escape)
      (if (and (eq? mode 'strict) (memq 'octal escape))
	  (raise
	     (instantiate::&io-parse-error
		(proc "js-symbol")
		(msg "octal literals are not allowed in strict mode")
		(obj val)
		(fname (cadr loc))
		(location (caddr loc))))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SComprehension ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SComprehension env mode withs wenvs)
   (with-access::J2SComprehension this (test expr iterables decls)
      (set! iterables
	 (map (lambda (iterable)
		 (resolve! iterable env mode withs wenvs))
	    iterables))
      (let ((nenv (append decls env)))
	 (set! test (resolve! test nenv mode withs wenvs))
	 (set! expr (resolve! expr nenv mode withs wenvs))
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

