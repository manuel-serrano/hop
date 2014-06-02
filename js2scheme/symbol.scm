;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/symbol.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 13 16:57:00 2013                          */
;*    Last change :  Wed May 28 19:15:29 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
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
(define-generic (j2s-symbol this args)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-symbol ::J2SProgram ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-symbol this::J2SProgram args)
   (with-access::J2SProgram this (nodes loc mode header)
      (let ((env (append-map (lambda (s) (collect* s)) nodes)))
	 (set! nodes
	    (if (pair? env)
		(append
		   (bind-decls! env env mode #t '() '())
		   (map (lambda (o) (resolve! o env mode '() '())) nodes))
		(map! (lambda (o) (resolve! o env mode '() '())) nodes)))))
   this)

(define (dump-env env)
   (map (lambda (decl)
	   (with-access::J2SDecl decl (id)
	      id))
      env))

;*---------------------------------------------------------------------*/
;*    find-decl ...                                                    */
;*---------------------------------------------------------------------*/
(define (find-decl var::symbol env::pair-nil)
   (find (lambda (decl)
	    (with-access::J2SDecl decl (id)
	       (eq? id var)))
      env))

;*---------------------------------------------------------------------*/
;*    bind-decls! ...                                                  */
;*---------------------------------------------------------------------*/
(define (bind-decls! decls env mode global withs::pair-nil wenv::pair-nil)
   
   (define (decl->assign! decl::J2SDeclInit old::J2SDecl)
      (with-access::J2SDeclInit decl (loc id val)
	 (instantiate::J2SInit
	    (loc loc)
	    (lhs (j2sref old loc withs wenv))
	    (rhs val))))
   
   (let loop ((decls (map (lambda (e) (bind! e env mode)) decls))
	      (acc '())
	      (funs '()))
      (if (null? decls)
	  (append (reverse! acc) (reverse! funs))
	  (let ((decl (car decls)))
	     (with-access::J2SDecl decl (id (bglobal global))
		(set! bglobal global)
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
      ;; Check the extra cnnstrains, parameter names have in in strict mode
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
   
   (define (fun->decl id fun)
      (with-access::J2SFun fun (loc)
	 (instantiate::J2SDeclInit
	    (loc loc)
	    (id id)
	    (name id)
	    (writable #f)
	    (ronly #t)
	    (global #t)
	    (val fun)
	    (use 0))))

   (with-access::J2SFun this (body params loc (fmode mode) params id)
      ;; propagate the compilation mode to the function
      (when (eq? mode 'strict)
	 (set! fmode mode))
      (if (eq? fmode 'strict)
	  (begin
	     ;; check parameter correctness
	     (check-strict-mode-params params loc)
	     (when (symbol? id)
		(check-strict-mode-eval id loc)))
	  (nonstrict-params! params))
      
      (let* ((decls (collect* body))
	     (arguments (instantiate::J2SDeclArguments
			   (id 'arguments)
			   (loc loc)))
	     (envl (append decls params))
	     (env0 (append envl env))
	     (nenv (if (find-decl 'arguments envl)
		       env0
		       (cons arguments env0)))
	     (nenv (if id (cons (fun->decl id this) nenv) nenv))
	     (nwenv (cons arguments (append decls params wenv))))
	 (if (pair? decls)
	     (set! body
		(instantiate::J2SSeq
		   (loc loc)
		   (nodes (append
			     (bind-decls! decls nenv fmode #f withs wenv)
			     (list (walk! body nenv fmode withs nwenv))))))
	     (set! body (walk! body nenv fmode withs nwenv)))
	 (with-access::J2SDeclArguments arguments (use)
	    (when (>fx use 0)
	       (with-access::J2SFun this (vararg)
		  (set! vararg #t))))))
   this)

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SWith ...                                           */
;*---------------------------------------------------------------------*/
(define-method (resolve! this::J2SWith env mode withs wenv)
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
	    (with-access::J2SParam param (id)
	       (check-strict-mode-eval id loc)))
	 (set! body (walk! body nenv mode withs wenv))))
   this)

;*---------------------------------------------------------------------*/
;*    j2sdecl ...                                                      */
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
;*    resolve! ::J2SForIn ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SForIn env mode withs wenv)
   (with-access::J2SForIn this (loc lhs obj body)
      (if (isa? lhs J2SVarDecls)
	  (with-access::J2SVarDecls lhs (decls)
	     (when (eq? mode 'strict)
		(for-each (lambda (decl)
			     (with-access::J2SDecl (car decls) (loc id)
				(check-strict-mode-eval id loc)))
		   decls))
	     (let ((ref (j2sref (car decls) loc withs wenv)))
		(set! lhs ref)))
	  (set! lhs (resolve! lhs env mode withs wenv)))
      (set! obj (resolve! obj env mode withs wenv))
      (set! body (resolve! body env mode withs wenv))
      this))

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
		(with-access::J2SDeclArguments decl (use)
		   (set! use (+fx 1 use))))
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
	 (check-strict-mode-eval id loc))
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
;*    check-strict-mode-eval ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-10.1.1       */
;*---------------------------------------------------------------------*/
(define (check-strict-mode-eval id loc)
   (cond
      ((or (eq? id 'eval) (eq? id 'arguments))
       (raise
	  (instantiate::&io-parse-error
	     (proc "js-symbol")
	     (msg "variable name may not be eval or arguments in strict mode")
	     (obj id)
	     (fname (cadr loc))
	     (location (caddr loc)))))
      ((j2s-strict-reserved-id? id)
       (raise
	  (instantiate::&io-parse-error
	     (proc "js-symbol")
	     (msg "variable name may not be a reserved name")
	     (obj id)
	     (fname (cadr loc))
	     (location (caddr loc)))))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SDeclInit ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SDeclInit env mode withs wenv)
   (with-access::J2SDeclInit this (loc id val)
      (let ((ndecl::J2SDecl (find-decl id env)))
	 ;; strict mode restrictions
	 ;; http://www.ecma-international.org/ecma-262/5.1/#sec-10.1.1
	 (when (eq? mode 'strict)
	    (check-strict-mode-eval id loc))
	 (instantiate::J2SInit
	    (loc loc)
	    (lhs (j2sref ndecl loc withs wenv))
	    (rhs (resolve! val env mode withs wenv))))))

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
	 (cond
	    ((isa? lhs J2SRef)
	     (with-access::J2SRef lhs (decl)
		(with-access::J2SDecl decl (id)
		   (check-strict-mode-eval id loc))))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id)
		(check-strict-mode-eval id loc))))))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SCall ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SCall env mode withs wenv)
   (when (eq? mode 'strict)
      (with-access::J2SCall this (fun args)
	 (if (isa? fun J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef fun (id loc)
		(when (eq? id 'eval)
		   (set! fun (instantiate::J2SHopRef
				(id '%js-eval-strict)
				(loc loc)))
		   (set! args (append args
				 (list
				    (instantiate::J2SPragma
				       (loc loc)
				       (expr '%this))))))))))
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
;*    collect ::J2SNode ...                                            */
;*    -------------------------------------------------------------    */
;*    Collect all the variable declared in a tree.                     */
;*---------------------------------------------------------------------*/
(define-walk-method (collect* this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect ::J2SVarDecls ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (collect* this::J2SVarDecls)
   (with-access::J2SVarDecls this (decls)
      (map (lambda (d)
	      (cond
		 ((isa? d J2SDeclFun) d)
		 ((isa? d J2SDeclExtern) d)
		 ((isa? d J2SDeclInit) (duplicate::J2SDecl d))
		 (else d)))
	 decls)))

;*---------------------------------------------------------------------*/
;*    collect ::J2SDecl ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (collect* this::J2SDecl)
   (list this))

;*---------------------------------------------------------------------*/
;*    collect ::J2SDeclInit ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (collect* this::J2SDeclInit)
   (list (duplicate::J2SDecl this)))

;*---------------------------------------------------------------------*/
;*    collect ::J2SDeclFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (collect* this::J2SDeclFun)
   (list this))

;*---------------------------------------------------------------------*/
;*    collect ::J2SDeclExtern ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (collect* this::J2SDeclExtern)
   (list this))

;*---------------------------------------------------------------------*/
;*    collect* ::J2SFun ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (collect* this::J2SFun)
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
