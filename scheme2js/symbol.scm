;; $Id: symbol.scm 133 2006-03-08 14:06:31Z flo $
(module symbol
   (include "protobject.sch")
   (include "nodes.sch")
   (import protobject
	   nodes
	   var
	   verbose)
   (export (symbol-resolution tree::pobject js-interface)
	   (id->js-var id::symbol)
	   *unresolved=JS*
	   *procedures-provide-js-this?*))

;; every fun declares a 'this' variable, allowing it to
;; function as js-method.
(define *procedures-provide-js-this?* #f)

(define *js-var-scope* #f)

(define *unresolved=JS* #f)

;; obviously only valid, once we made the symbol-resolution pass.
(define (id->js-var id::symbol)
   (scope-symbol-var *js-var-scope* id))

;; only valid, once the *js-var-scope* has been set
(define (js-symbol-add! scheme-sym js-sym)
   (scope-symbol-var-set! *js-var-scope*
			  scheme-sym
			  (new JS-Var scheme-sym js-sym)))
   
(define (symbol-resolution tree js-interface)
   (verbose "symbol-resolution")
   (let* ((js-var-scope (make-scope))
	  (symbol-table (add-scope (make-symbol-table)
				   js-var-scope)))
      (set! *js-var-scope* js-var-scope)

      (for-each (lambda (p)
		   (js-symbol-add! (car p) (cadr p)))
		js-interface)

      (collect tree symbol-table)
      (resolve tree symbol-table)

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
			       Program
			       Scope
			       Lambda)
	     (tree.traverse symbol-table #f)))

(define-pmethod (Node-collect symbol-table is-global?)
   (this.traverse2 symbol-table is-global?))

(define-pmethod (Decl-collect symbol-table is-global?)
   (let* ((id this.id)
	  (var (local-symbol-var symbol-table id)))
      ;; already declared (most likely by 'define')
      (if var
	  (set! this.var var)
	  (let ((new-var (new Var id)))
	     (if is-global?
		 (set! new-var.is-global? #t))
	     (set! this.var new-var)
	     (symbol-var-set! symbol-table id new-var)))))

(define-pmethod (Program-collect symbol-table is-global?)
   (let ((local-scope (make-scope)))
      (set! this.scope local-scope)
      (this.traverse2 (add-scope symbol-table local-scope) #t)))

(define-pmethod (Scope-collect symbol-table is-global?)
   (let ((local-scope (make-scope)))
      (set! this.scope local-scope)
      (this.traverse2 (add-scope symbol-table local-scope) #f)))

(define-pmethod (Lambda-collect symbol-table is-global?)
   (let ((local-scope (make-scope)))
      (set! this.scope local-scope)
      (if *procedures-provide-js-this?*
	  (symbol-var-set! symbol-table 'this (new JS-This-Var)))
      (this.traverse2 (add-scope symbol-table local-scope) #f)))
      

;; ============================================================================
;; resolves all variables: each var-ref gets a pointer to its var.
;; ============================================================================
(define (resolve tree symbol-table)
   (verbose "  resolving")
   (overload traverse resolve (Node
			       Var-ref
			       Decl
			       Scope
			       Let-form)
	     (tree.traverse symbol-table)))

(define *JS* #f)

(define-pmethod (Node-resolve symbol-table)
   (this.traverse1 symbol-table))

(define-pmethod (Var-ref-resolve symbol-table)
   (let ((var (symbol-var symbol-table this.id)))
      (cond
	 (var (set! this.var var))
	 (*unresolved=JS*
	  (js-symbol-add! this.id this.id)
	  (verbose "Unresolved symbol " this.id " assumed to be a JS-var")
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
   (pcall this Node-resolve (add-scope symbol-table this.scope)))

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
