(module symbol
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes
	   var
	   verbose
	   gen-js)
   (export (symbol-resolution tree::pobject js-interface)))

(define (symbol-resolution tree js-interface)
   (verbose "symbol-resolution")
   (var-init!)
   (let* ((js-var-scope (make-scope))
	  (symbol-table (add-scope (make-symbol-table)
				   js-var-scope))
	  (js-symbol-add! (lambda (scheme-sym js-sym)
			     (scope-symbol-var-set! js-var-scope
						    scheme-sym
						    (new-node JS-Var
							      scheme-sym
							      js-sym)))))
      (for-each (lambda (p)
		   (js-symbol-add! (car p) (cadr p)))
		js-interface)

      (collect tree symbol-table)
      (resolve tree symbol-table js-symbol-add!)

      (set! tree.id->js-var (lambda (id::symbol)
			       (scope-symbol-var js-var-scope id)))
      (set! tree.imported (hashtable->list js-var-scope))))

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

;; ============================================================================
;; collects all declared variables, and assigns them to their scope.
;; ============================================================================
(define (collect tree symbol-table)
   (verbose "  collecting")
   (overload traverse collect (Node
			       Decl
			       Set!
			       Program
			       Scope
			       With-handler
			       Lambda)
	     (tree.traverse symbol-table #f)))

(define-pmethod (Node-collect symbol-table is-global?)
   (this.traverse2 symbol-table is-global?))

(define-pmethod (Decl-collect symbol-table is-global?)
   (let* ((id this.id)
	  (var (local-symbol-var symbol-table id)))
      (if var
	  ;; already declared (most likely by 'define')
	  (begin
	     (set! this.var var)
	     ;; we don't want two decls for one var -> mark this Decl.
	     (set! this.transform-to-Var-ref #t))
	  (let ((new-var (new-node Var id)))
	     (if is-global?
		 (set! new-var.is-global? #t))
	     (set! this.var new-var)
	     (symbol-var-set! symbol-table id new-var)))))

(define-pmethod (Set!-collect symbol-table is-global?)
   (this.traverse2 symbol-table is-global?)
   (if this.lvalue.transform-to-Var-ref
       (let* ((lvalue this.lvalue)
	      (id lvalue.id)
	      (var lvalue.var)
	      (var-ref (new-node Var-ref id)))
	  (set! var-ref.var var)
	  (set! this.lvalue var-ref))))

(define-pmethod (Program-collect symbol-table is-global?)
   (let ((local-scope (make-scope)))
      (set! this.scope local-scope)
      (this.traverse2 (add-scope symbol-table local-scope) #t)))

(define-pmethod (Scope-collect symbol-table is-global?)
   (let ((local-scope (make-scope)))
      (set! this.scope local-scope)
      (this.traverse2 (add-scope symbol-table local-scope) #f)))

(define-pmethod (With-handler-collect symbol-table is-global?)
   (let ((local-scope (make-scope)))
      (set! this.scope local-scope) ;; just for the exception
      (this.exception.traverse (add-scope symbol-table local-scope) #f)
      (this.catch.traverse symbol-table is-global?)
      (this.body.traverse symbol-table is-global?)))

(define-pmethod (Lambda-collect symbol-table is-global?)
   (let ((local-scope (make-scope)))
      (set! this.scope local-scope)
      (if (config 'procedures-provide-js-this)
	  (symbol-var-set! symbol-table 'this (new-node JS-This-Var)))
      (this.traverse2 (add-scope symbol-table local-scope) #f)))
      

;; ============================================================================
;; resolves all variables: each var-ref gets a pointer to its var.
;; ============================================================================
(define (resolve tree symbol-table js-symbol-add!)
   
   (define-pmethod (Node-resolve symbol-table)
      (this.traverse1 symbol-table))
   
   (define-pmethod (Var-ref-resolve symbol-table)
      (let ((var (symbol-var symbol-table this.id)))
	 (cond
	    (var (set! this.var var))
	    ((config 'unresolved=JS)
	     (js-symbol-add! this.id (string->symbol (mangle-JS-sym this.id)))
	     (verbose "Unresolved symbol '" this.id "' assumed to be a JS-var")
	     (pcall this Var-ref-resolve symbol-table))
	    (else
	     (error #f "Unresolved symbol: " this.id)))))
   
   (define-pmethod (Decl-resolve symbol-table)
      ;; do nothing.
      ;; just an optimization to avoid the look-up for already known vars.
      #unspecified)
   
   (define-pmethod (Scope-resolve symbol-table)
      ;; if 'this' is a Lambda, we will revisit the formals, but they are
      ;; optimized anyways.
      (this.traverse1 (add-scope symbol-table this.scope)))

   (define-pmethod (With-handler-resolve symbol-table)
      ;; don't need to revisit the exception-var-decl
      (this.catch.traverse (add-scope symbol-table this.scope))
      (this.body.traverse symbol-table))
   
   (define-pmethod (Let-form-resolve symbol-table)
      (let* ((extended-table (add-scope symbol-table this.scope))
	     (bindings-table (if (eq? this.kind 'let)
				 symbol-table
				 extended-table)))
	 (for-each (lambda (binding)
		      ;; we don't need to do the 'var's again
		      (binding.val.traverse bindings-table))
		   ;;next line is just for bugloo-testing!
		   ;(binding.val.resolve #f))
		   this.bindings)
	 (this.body.traverse extended-table)))
   
   ;; ================================================
   ;; procedure starts here
   ;; ================================================
   (verbose "  resolving")
   (overload traverse resolve (Node
			       Var-ref
			       Decl
			       Scope
			       With-handler
			       Let-form)
	     (tree.traverse symbol-table)))
