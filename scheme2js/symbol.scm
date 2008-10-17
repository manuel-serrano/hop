(module symbol
   (import mapping1 mapping2
	   tools
	   symbol-table
	   config
	   nodes
	   export-desc
	   walk
	   verbose
	   gen-js
	   pobject-conv)
   (export (symbol-resolution tree::Module
			      imports::pair-nil
			      exports::pair-nil)
	   (runtime-reference id::symbol))
   (static (final-class Env
	      runtime
	      imports
	      exports

	      export-globals
	      ;; following entries will be set in Module-resolve
	      (runtime-scope (default #f))
	      (unbound-add! (default #f)))))

(define (runtime-reference id)
   (var-reference ((thread-parameter '*runtime-id->var*) id)))

(define (runtime-reference-init! f)
   (thread-parameter-set! '*runtime-id->var* f))

;; selects runtime imported from 'runtime_mapping.sch'
(define (select-runtime)
   (cond
      ((and (config 'suspend/resume)
	    (config 'runtime-is-constant))
       *call/cc-constant-runtime-var-mapping*)
      ((config 'suspend/resume)
       *call/cc-runtime-var-mapping*)
      ((config 'runtime-is-constant)
       *default-constant-runtime-var-mapping*)
      (else
       *default-runtime-var-mapping*)))

;; symbol-resolution is done in one pass now:
;; defines are not valid everywhere, but must be at the beginning of
;; bodies, or at the top-level. The declaration will be moved to the top of
;; the Let/Module/Lambda (where it becomes a local variable).
;;
;; Every variable has a declaration-node (in the Let/Module/Lambda) and all
;; other uses are define to be References.
(define (symbol-resolution tree imports exports)
   (verbose "symbol-resolution")
   (resolve! tree
	     (instantiate::Env
		(runtime (select-runtime))
		(imports imports)
		(exports exports)
		(export-globals (config 'export-globals)))
	     '()))

(define-nmethod (Node.resolve! symbol-table)
   (default-walk! this symbol-table))

(define (js-symbol-add! scope desc imported?)
   (let ((scheme-sym (Export-Desc-id desc)))
      (symbol-var-set! scope scheme-sym
		       (instantiate::Exported-Var
			  (id scheme-sym)
			  (imported? #t)
			  (desc desc)))))
   
(define-nmethod (Module.resolve! symbol-table)
   (let* ((runtime-scope (make-scope (length (Env-runtime env))))
	  (imported-scope (make-scope (length (Env-imports env))))
	  ;; module-scope might grow, but 'length' is just an indication. 
	  (module-scope (make-scope (length (Env-exports env))))
	  (extended-symbol-table (cons* module-scope
					imported-scope
					runtime-scope
					symbol-table)))
      
      ;; add runtime
      (for-each (lambda (meta) (js-symbol-add! runtime-scope meta #t))
		(Env-runtime env))
      
      ;; add imported variables
      (for-each (lambda (meta) (js-symbol-add! imported-scope meta #t))
		(Env-imports env))
      
      ;; insert exported variables
      (for-each (lambda (meta) (js-symbol-add! module-scope meta #f))
		(Env-exports env))
      
      ;; we need to reference runtime-variables from other passes. Export
      ;; a function allowing access to them.
      (runtime-reference-init! (lambda (id::symbol)
				  (symbol-var runtime-scope id)))
      
      (Env-runtime-scope-set! env runtime-scope)
      (Env-unbound-add!-set! env
			     (lambda (scheme-sym js-str)
				(js-symbol-add! imported-scope
						(instantiate::Export-Desc
						   (id scheme-sym)
						   (js-id js-str)
						   (exported-as-const? #f))
						#t)))

      (with-access::Module this (this-var runtime-vars imported-vars
					  scope-vars body)
	 
	 (when (config 'procedures-provide-js-this)
	    (symbol-var-set! module-scope 'this this-var))
	 
	 (find-globals env body module-scope)
	 ;; generally the result should not be needed.
	 ;; however, when used as library, it is sometimes necessary to assign
	 ;; the result to a var. this var is then stored in config
	 ;; 'module-result-var'.
	 ;; We can't do this earlier, as otherwise the top-level defines are
	 ;; not found...
	 (let ((global-assig (config 'module-result-var)))
	    (when global-assig
	       (set! body (instantiate::Set!
			     (lvalue (instantiate::Ref (id global-assig)))
			     (val body)))))
	 ;; walk!
	 (default-walk! this extended-symbol-table)
	 (set! runtime-vars (scope->list runtime-scope))
	 (set! imported-vars (scope->list imported-scope))
	 (let* ((module-vars (filter! (lambda (var)
					 (not (This-Var? var)))
				      (scope->list module-scope)))
		(local-vars (cp-filter (lambda (var) (not (Exported-Var? var)))
				       module-vars)))
	    (set! scope-vars (filter (lambda (var) (Exported-Var? var))
				     module-vars))
	    (set! body (instantiate::Let
			  (scope-vars local-vars)
			  (bindings '())
			  (body body)
			  (kind 'let)))))
      this))

(define (collect decl::Ref scope)
   (with-access::Ref decl (id var)
      (let ((v (symbol-var scope id)))
	 (if v
	     ;; already declared
	     (error "symbol-resolution"
		    "Variable already declared"
		    id)
	     (let ((new-var (instantiate::Local
			       (id id))))
		(set! var new-var)
		(symbol-var-set! scope id new-var))))))

(define-nmethod (Lambda.resolve! symbol-table)
   ;; this.body must be 'return'.
   (with-access::Lambda this (body formals scope-vars this-var)
      (with-access::Return body (val)
	 (set! val (defines->letrec! val)))
      
      (let* ((formals-scope (make-scope))
	     (new-symbol-table (cons formals-scope symbol-table)))
	 (for-each (lambda (formal)
		      (collect formal formals-scope))
		   formals)
	 (set! scope-vars (map Ref-var formals))
	 
	 (when (config 'procedures-provide-js-this)
	    (symbol-var-set! formals-scope 'this this-var))
	 (default-walk! this new-symbol-table))))
   
(define-nmethod (Let.resolve! symbol-table)
   (with-access::Let this (body bindings kind scope-vars)
      (set! body (defines->letrec! body))
      (let ((local-scope (make-scope)))
	 (for-each (lambda (binding)
		      (with-access::Set! binding (lvalue)
			 (collect lvalue local-scope)))
		   bindings)
	 (let* ((extended-table (cons local-scope symbol-table))
		(bindings-table (if (eq? kind 'let)
				    symbol-table
				    extended-table)))
	    (for-each (lambda (n)
			 (with-access::Set! n (val)
			    ;; symbol-table for bindings might be different
			    ;; than the table for the body.
			    (set! val (walk! val bindings-table))
			    n))
		      bindings)
	    (set! body (walk! body extended-table))
	    
	    (set! scope-vars (map (lambda (b)
				     (with-access::Set! b (lvalue)
					(with-access::Ref lvalue (var)
					   var)))
				  bindings))
	    this))))

(define-nmethod (Ref.resolve! symbol-table)
   (with-access::Ref this (id var)
      (let ((v (any (lambda (scope)
		       (symbol-var scope id))
		    symbol-table)))
	 (cond
	    (v (set! var v))
	    ((config 'unresolved=JS)
	     ((Env-unbound-add! env) id (mangle-JS-sym id))
	     (verbose "Unresolved symbol '" id "' assumed to be a JS-var")
	     (ncall resolve! this symbol-table)) ;; try again.
	    (else
	     (error #f "Unresolved symbol: " id)))))
   this)

;; runtime-var-ref directly queries the js-var-scope (short-cutting the
;; intermediate scopes).
(define-nmethod (Runtime-Ref.resolve! symbol-table)
   (with-access::Runtime-Ref this (id var)
      (let ((v (symbol-var (Env-runtime-scope env) id)))
	 ;; error should never happen (programming error)
	 (when (not v) (error "Runtime-Var-Ref.resolve!"
			      "Runtime-variable not found"
			      id))
	 (set! var v)))
   (shrink! this)
   this)

;; all global 'defines' have been shrunk to Set!s.
;; all local 'defines' have been transformed to letrecs.
(define-nmethod (Define.resolve! symbol-table)
   (with-access::Define this (lvalue)
      (with-access::Ref lvalue (id)
	 (error "symbol-resolution"
		"Define at bad location"
		id))))

(define (find-globals env n module-scope)
   (cond
      ((Begin? n)
       (with-access::Begin n (exprs)
	  (for-each (lambda (e) (find-globals env e module-scope))
		    exprs)))
      ((Define? n)
       (shrink! n)
       (with-access::Set! n (lvalue)
	  (with-access::Ref lvalue (id)
	     (let ((var (symbol-var module-scope id)))
		(cond
		   (var
		    'do-nothing)
		   ((Env-export-globals env)
		    (let* ((js-id (mangle-JS-sym id))
			   (desc (instantiate::Export-Desc
				    (id id)
				    (js-id js-id)
				    (exported-as-const? #f)))
			   (new-var (instantiate::Exported-Var
				       (id id)
				       (imported? #f)
				       (desc desc))))
		       (symbol-var-set! module-scope id new-var)))
		   (else
		    (let ((new-var (instantiate::Local
				      (id id))))
		       (symbol-var-set! module-scope id new-var))))))))
      (else 'do-nothing)))

(define (defines->letrec! n)
   (cond
      ((Define? n)
       ;; wrap into begin.
       (defines->letrec! (instantiate::Begin
			    (exprs (list n)))))
      ((Begin? n)
       (let ((bindings (map (lambda (n) (shrink! n)) (head-defines! n))))
	  (if (null? bindings)
	      n
	      (instantiate::Let
		 (bindings bindings)
		 (body n)
		 (kind 'letrec)))))
      (else n)))

(define (head-defines! bnode::Begin)
   (define (inner bnode::Begin rev-found-defines finish-fun)
      (let loop ((exprs (Begin-exprs bnode))
		 (rev-defines rev-found-defines))
	 (cond
	    ((null? exprs)
	     rev-defines)
	    ((Begin? (car exprs))
	     (loop (cdr exprs)
		   (inner (car exprs)
			  rev-defines
			  finish-fun)))
	    ((Define? (car exprs))
	     (let ((binding (car exprs)))
		(set-car! exprs (instantiate::Const (value #unspecified)))
		(loop (cdr exprs)
		      (cons binding rev-defines))))
	    (else
	     (finish-fun rev-defines)))))

   (reverse! (bind-exit (finish-fun)
		(inner bnode '() finish-fun))))
