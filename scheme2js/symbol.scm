(module symbol
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   mapping1 mapping2
	   config
	   nodes
	   var
	   verbose
	   gen-js)
   (export (symbol-resolution tree::pobject
			      imports::pair-nil
			      exports::pair-nil)
	   (runtime-reference id::symbol)))

(define (runtime-reference id)
   (((thread-parameter '*runtime-id->var*) id).reference))

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
   (var-init!)
   (let ((runtime (select-runtime)))
      (resolve! tree runtime imports exports)))



;; ============================================================================
;; symbol-tables are a list of hashtables (each hashtable representing a scope)
;; ============================================================================
(define (make-symbol-table) '())

(define (symbol-var symbol-table symbol)
   (any (lambda (ht)
	   (hashtable-get ht symbol))
	symbol-table))

(define (local-symbol-var symbol-table symbol)
   (and (pair? symbol-table)
	(hashtable-get (car symbol-table) symbol)))

(define (symbol-var-set! symbol-table symbol var)
   (scope-symbol-var-set! (car symbol-table) symbol var))

(define (scope-symbol-var-set! scope symbol var)
   (hashtable-put! scope
		   symbol
		   var))

(define (scope-symbol-var scope symbol)
   (hashtable-get scope symbol))

(define (make-scope)
   (make-hashtable))

(define (add-scope symbol-table local-scope)
   (cons local-scope symbol-table))

(define (entry-val sym l)
   (let ((try (assq sym (cdr l))))
      (and try
	   (cadr try))))

(define (normalize-export export)
   (cond
      ((symbol? export)
       (list export
	     (list 'interface (mangle-JS-sym export))))
      ((pair? export)
       (cond
	  ((assq 'interface (cdr export))
	   export)
	  ((assq 'JS (cdr export))
	   (cons* (car export)
		  (list 'interface (cadr (assq 'JS (cdr export))))
		  (cdr export)))
	  (else
	   (let ((scheme-sym (car export)))
	      (cons* scheme-sym
		     (list 'interface (mangle-JS-sym scheme-sym))
		     (cdr export))))))
      (else
       (error "normalize-export" "bad import/export clause: " export))))


(define (resolve! tree runtime imports exports)
   ;; will be set in Module-resolve
   (define *runtime-scope* #unspecified)
   (define *unbound-add!* #unspecified)

   (define-pmethod (Node-resolve! symbol-table)
      (this.traverse1! symbol-table))

   (define (js-symbol-add! scope entry runtime?)
      (let* ((normalized (normalize-export entry))
	     (scheme-sym (car normalized))
	     (peephole (entry-val 'peephole normalized))
	     (higher? (entry-val 'call/cc? normalized))
	     (higher-params (entry-val 'call/cc-params normalized))
	     (return-type (entry-val 'type normalized))
	     (exported-as-constant? (or (entry-val 'constant? normalized)))
	     (var (new-node Imported-Var
			    (entry-val 'interface normalized)
			    scheme-sym
			    exported-as-constant?
			    runtime?)))
	 (scope-symbol-var-set! scope scheme-sym var)
	 (if peephole (set! var.peephole peephole))
	 (if higher? (set! var.higher? #t))
	 (if higher-params (set! var.higher-params higher-params))
	 (if return-type (set! var.return-type return-type))))
   
   (define-pmethod (Module-resolve! symbol-table)
      (let* ((runtime-scope (make-scope))
	     (imported-scope (make-scope))
	     (module-scope (make-scope))
	     (extended-symbol-table (add-scope
				     (add-scope
				      (add-scope
				       symbol-table ;; add to symbol-table:
				       runtime-scope)
				      imported-scope)
				     module-scope)))

	 ;; add runtime
	 (for-each (lambda (entry) (js-symbol-add! runtime-scope entry #t))
		   runtime)

	 ;; add imported variables
	 (for-each (lambda (entry) (js-symbol-add! imported-scope entry #f))
		   imports)

	 ;; insert exported variables
	 (for-each (lambda (entry)
		      (let* ((normalized (normalize-export entry))
			     (scheme-sym (car normalized))
			     (return-type (entry-val 'type normalized))
			     (var (new-node Exported-Var
					    (entry-val 'interface normalized)
					    scheme-sym
					    (entry-val 'constant? normalized))))
			 (if return-type
			     (set! var.return-type return-type))
			 (scope-symbol-var-set! module-scope scheme-sym var)))
		   exports)

	 ;; we need to reference runtime-variables from elsewhere. Export
	 ;; a function allowing access to them.
	 (runtime-reference-init! (lambda (id::symbol)
				     (scope-symbol-var runtime-scope id)))
	 
	 (set! *runtime-scope* runtime-scope)
	 (set! *unbound-add!* (lambda (scheme-sym js-sym)
				 (let ((var (new-node Imported-Var
						      js-sym
						      scheme-sym
						      #f
						      #f)))
				    (scope-symbol-var-set! imported-scope
							   scheme-sym
							   var))))
	 (when (config 'procedures-provide-js-this)
	    (let ((t (new-node JS-This-Var)))
	       (symbol-var-set! extended-symbol-table 'this t)
	       (set! this.this-var t)))

	 (find-globals this.body module-scope)
	 ;; traverse
	 (this.traverse1! extended-symbol-table)
	 (set! this.runtime-vars (hashtable->list runtime-scope))
	 (set! this.imported-vars (hashtable->list imported-scope))
	 (let* ((module-vars (filter! (lambda (var)
					 (not (inherits-from?
					       var (node 'JS-This-Var))))
				      (hashtable->list module-scope)))
		(exported-vars (filter (lambda (var)
					  var.exported?)
				       module-vars))
		(local-vars (cp-filter (lambda (var)
					  (not var.exported?))
				       module-vars)))
	    (set! this.exported-vars exported-vars)
	    (set! this.scope-vars exported-vars)
	    (set! this.body (new-node Let
				      local-vars
				      '()
				      this.body
				      'let)))
	 this))

   (define (collect decl scope)
      (let* ((id decl.id)
	     (var (scope-symbol-var scope id)))
	 (if var
	     ;; already declared
	     (error "symbol-resolution"
		    "Variable already declared"
		    id)
	     (let ((new-var (new-node Var id)))
		(set! decl.var new-var)
		(scope-symbol-var-set! scope id new-var)))))

   (define-pmethod (Lambda-resolve! symbol-table)
      ;; this.body must be 'return'.
      (set! this.body.val (defines->letrec! this.body.val))
      (let* ((formals-scope (make-scope))
	     (new-symbol-table (add-scope symbol-table formals-scope)))
	 (for-each (lambda (formal)
		      (collect formal formals-scope))
		   this.formals)
	 (set! this.scope-vars (map (lambda (formal) formal.var)
				    this.formals))
	 (when (config 'procedures-provide-js-this)
	    (let ((t (new-node JS-This-Var)))
	       (symbol-var-set! new-symbol-table 'this t)
	       (set! this.this-var t)))
	 (this.traverse1! new-symbol-table)))
   
   (define-pmethod (Let-resolve! symbol-table)
      (set! this.body (defines->letrec! this.body))
      (let ((local-scope (make-scope)))
	 (for-each (lambda (binding)
		      (collect binding.lvalue local-scope))
		   this.bindings)
	 
	 (let* ((extended-table (add-scope symbol-table local-scope))
		(bindings-table (if (eq? this.kind 'let)
				    symbol-table
				    extended-table))
		(new-bindings (map! (lambda (n)
				       ;; symbol-table for bindings might be different
				       ;; than the table for the body.
				       (set! n.val
					     (n.val.traverse! bindings-table))
				       n)
				    this.bindings))
		(vars (map (lambda (n) n.lvalue.var) new-bindings))
		(new-body (this.body.traverse! extended-table)))

	    (set! this.scope-vars vars)
	    (set! this.bindings new-bindings)
	    (set! this.body new-body)
	    this)))

   (define-pmethod (Var-ref-resolve! symbol-table)
      (let ((var (symbol-var symbol-table this.id)))
	 (cond
	    (var (set! this.var var))
	    ((config 'unresolved=JS)
	     (*unbound-add!* this.id (string->symbol (mangle-JS-sym this.id)))
	     (verbose "Unresolved symbol '" this.id "' assumed to be a JS-var")
	     (pcall this Var-ref-resolve! symbol-table))
	    (else
	     (error #f "Unresolved symbol: " this.id))))
      this)

   ;; runtime-var-ref directly queries the js-var-scope (short-cutting the
   ;; intermediate scopes).
   (define-pmethod (Runtime-Var-ref-resolve! symbol-table)
      (let ((var (scope-symbol-var *runtime-scope* this.id)))
	 (set! this.var var))
      this)

   (define-pmethod (Define-resolve! symbol-table)
      (unless this.legal?
	 (error "symbol-resolution"
		"Define at bad location"
		this.lvalue.id))
      (delete! this.legal?)
      (this.traverse1! symbol-table))
   
   ;; ================================================
   ;; procedure starts here
   ;; ================================================
   (overload traverse! resolve! (Node
				 Module
				 Lambda
				 Let
				 Var-ref
				 Runtime-Var-ref
				 Define)
	     (tree.traverse! (make-symbol-table))))


(define (collect-global decl scope)
   (let* ((id decl.id)
	  (var (scope-symbol-var scope id)))
      (unless var
	 (let ((new-var (new-node Var id)))
	    (if (config 'export-globals)
		(set! new-var.exported? #t))
	    (scope-symbol-var-set! scope id new-var)))))

(define (find-globals body module-scope)
   (overload traverse globals (Node
			      Begin
			      Define)
	     (body.traverse module-scope)))

(define-pmethod (Node-globals module-scope)
   'do-nothing)

(define-pmethod (Begin-globals module-scope)
   (this.traverse1 module-scope))

(define-pmethod (Define-globals module-scope)
   (set! this.legal? #t)
   (collect-global this.lvalue module-scope))


(define (defines->letrec! tree)
   (cond
      ((inherits-from? tree (node 'Define))
       (defines->letrec! (new-node Begin
				   (list tree))))
      ((inherits-from? tree (node 'Begin))
       (let ((bindings (map (lambda (define-n)
			       (new-node Binding
					 define-n.lvalue
					 define-n.val))
			    (head-defines! tree))))
	  (if (null? bindings)
	      tree
	      (begin
		 (new-node Let
			   '() ;; will be done in the resolve! above
			   bindings
			   tree
			   'letrec)))))
      (else tree)))

(define (head-defines! bnode)
   (define (inner bnode rev-found-defines finish-fun)
      (let loop ((exprs bnode.exprs)
		 (rev-defines rev-found-defines))
	 (cond
	    ((null? exprs)
	     rev-defines)
	    ((inherits-from? (car exprs) (node 'Begin))
	     (loop (cdr exprs)
		   (inner (car exprs)
			  rev-defines
			  finish-fun)))
	    ((inherits-from? (car exprs) (node 'Define))
	     (let ((binding (car exprs)))
		(set-car! exprs (new-node Const #unspecified))
		(loop (cdr exprs)
		      (cons binding rev-defines))))
	    (else
	     (finish-fun rev-defines)))))

   (reverse! (bind-exit (finish-fun)
		(inner bnode '() finish-fun))))
