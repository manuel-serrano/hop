;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-09 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

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
	   pobject-conv
	   error)
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
	      (allow-unresolved?::procedure (default (lambda (id loc) #f)))
	      (unbound-add!::procedure (default (lambda (id) #f))))))

(define *scheme2js-compilation-runtime-vars* '(list js-call not))

(define (runtime-reference id)
   (var-reference ((thread-parameter '*runtime-id->var*) id)))

(define (runtime-reference-init! f)
   (thread-parameter-set! '*runtime-id->var*
			  (lambda (sym)
			     [assert (*scheme2js-compilation-runtime-vars* sym)
				     (memq sym *scheme2js-compilation-runtime-vars*)]
			     (f sym))))

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

(define (create-js-var id imported? desc)
   (instantiate::Var
      (id id)
      (kind (if imported? 'imported 'exported))
      (export-desc desc)))

(define (js-symbol-add! scope desc imported?)
   (let ((scheme-sym (Export-Desc-id desc)))
      (symbol-var-set! scope scheme-sym
		       (create-js-var scheme-sym imported? desc))))

;; lazy lookup will not create Vars until they are actually used.
;; if JS? is true then direct accesses are to be found/added here.
(define (lazy-imported-lookup imports JS?)
   (define (qualified? v) ;; just assume it is correctly formed.
      (pair? v))

   (define (id-symbol id)
      (if (qualified? id)
	  (car id)
	  id))
   (define (id-qualifier id)
      (if (qualified? id)
	  (cadr id)
	  #f))

   (define (update-scope! scope symbol qualified v)
      (symbol-var-set! scope qualified v))
      
   (define (lazy-lookup scope id)
      (let ((sym (id-symbol id))
	    (qualifier (id-qualifier id)))
	 (let loop ((imports imports))
	    (if (null? imports)
		#f
		(let* ((import (car imports))
		       (imps-qualifier (car import))
		       (imps (cdr import))) ;; exports from other module.
		   (cond
		      ((and qualifier
			    (not (eq? qualifier imps-qualifier)))
		       (loop (cdr imports)))
		      ((find-desc-in-exports sym imps)
		       =>
		       (lambda (desc)
			  (let ((v (create-js-var sym #t desc)))
			     (update-scope! scope sym
					    (if (qualified? id)
						id
						(list sym imps-qualifier))
					    v)
			     v)))
		      (else
		       (loop (cdr imports)))))))))
      
   ;; when this fun is called then the scope does not contain the id that we
   ;; are looking for. -> if we want to cache, just add the new var to the
   ;; scope.
   (lambda (scope id)
      (let ((v (lazy-lookup scope id)))
	 (cond
	    (v v)
	    ((not JS?) #f)
	    ((and (qualified? id)
		  (eq? (id-qualifier id) '_)) ;; JS
	     (let* ((scheme-sym (id-symbol id))
		    (js-str (symbol->string scheme-sym)) ;; do not mangle.
		    (var (create-js-var scheme-sym #t
					(instantiate::Export-Desc
					   (id scheme-sym)
					   (js-id js-str)
					   (exported-as-const? #f)))))
		(update-scope! scope scheme-sym id var)
		var))
	    (else #f)))))
    
(define-nmethod (Module.resolve! symbol-table)
   (let* ((runtime-scope (make-lazy-scope
			  (lazy-imported-lookup `((* . ,(Env-runtime env)))
						#f)))
	  (imported-scope (make-lazy-scope
			   (lazy-imported-lookup (Env-imports env) #t)))
	  ;; module-scope might grow, but 'length' is just an indication. 
	  (module-scope (make-scope (length (Env-exports env))))
	  (extended-symbol-table (cons* module-scope
					imported-scope
					runtime-scope
					symbol-table)))

      ;; no need to add runtime or imported variables. They are in a lazy
      ;; scope. HOWEVER: we need to add the internally used vars.
      ;; Otherwise 'list', ... might not be in the runtime-vars. They would be
      ;; added. But maybe too late.
      (for-each (lambda (id)
		   ;; simply searching for the variable will mark it as used.
		   (symbol-var runtime-scope id))
		*scheme2js-compilation-runtime-vars*)
      
      ;; insert exported variables
      (for-each (lambda (meta) (js-symbol-add! module-scope meta #f))
		(Env-exports env))
      
      ;; we need to reference runtime-variables from other passes. Export
      ;; a function allowing access to them.
      (runtime-reference-init! (lambda (id::symbol)
				  (symbol-var runtime-scope id)))
      
      (Env-runtime-scope-set! env runtime-scope)
      (Env-allow-unresolved?-set!
       env
       (case (config 'allow-unresolved)
	  ((#t module) (lambda (id loc) #t))
	  ((#f) (lambda (id loc) #f))
	  ((ask) (let ((oracle (config 'allow-unresolved-oracle)))
		    (lambda (id loc)
		       (oracle id (scheme2js-error-location loc)))))
	  (else (error 'symbol-resolution
		       "invalid 'allow-unresolved' configuration"
		       (config 'allow-unresolved)))))
      (Env-unbound-add!-set!
       env
       (let ((unresolved-declare! (config 'unresolved-declare)))
	  (lambda (id)
	     (when (pair? id) ;; qualified
		(scheme2js-error "symbol-resolution"
				 "could not resolve qualified variable"
				 (cons '@ id)
				 id))
	     (let ((js-str (mangle-JS-sym id)))
		(if unresolved-declare!
		    (unresolved-declare! id js-str)
		    (verbose "Unresolved symbol '"
			     id "' assumed to be a JS-var"))
		(js-symbol-add! imported-scope
				    (instantiate::Export-Desc
				       (id id)
				       (js-id js-str)
				       (exported-as-const? #f))
				    #t)))))

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
	       (js-symbol-add! module-scope
			       (instantiate::Export-Desc
				  (id global-assig)
				  (js-id (symbol->string global-assig))
				  (exported-as-const? #f))
			       #f)
	       (set! body (instantiate::Set!
			     (lvalue (instantiate::Ref (id global-assig)))
			     (val body)))))
	 ;; walk!
	 (default-walk! this extended-symbol-table)
	 (set! runtime-vars (scope->list runtime-scope))
	 (set! imported-vars (scope->list imported-scope))
	 (let* ((module-vars (filter! (lambda (var)
					 (not (eq? (Var-kind var) 'this)))
				      (scope->list module-scope)))
		(local-vars (cp-filter (lambda (var)
					  (eq? (Var-kind var) 'local))
				       module-vars)))
	    (set! scope-vars (filter (lambda (var)
					(eq? (Var-kind var) 'exported))
				     module-vars))

	    ;; the following is only useful when used as library.
	    (let ((exported-declare! (config 'exported-declare)))
	       (when exported-declare!
		  (for-each (lambda (var)
			       (with-access::Var var (export-desc)
				  (with-access::Export-Desc export-desc
					(id js-id)
				     (exported-declare! id js-id))))
			 scope-vars)))

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
	     (scheme2js-error "symbol-resolution"
			      "Variable already declared"
			      id
			      decl)
	     (let ((new-var (instantiate::Var
			       (id id)
			       (kind 'local))))
		(set! var new-var)
		(symbol-var-set! scope id new-var))))))

(define-nmethod (Lambda.resolve! symbol-table)
   ;; this.body must be 'return'.
   (with-access::Lambda this (body formals scope-vars this-var)
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
	    (((Env-allow-unresolved? env) id this)
	     ((Env-unbound-add! env) id)
	     (ncall resolve! this symbol-table)) ;; try again.
	    (else
	     (scheme2js-error #f "Unresolved symbol: " id this)))))
   this)

;; runtime-var-ref directly queries the js-var-scope (short-cutting the
;; intermediate scopes).
(define-nmethod (Runtime-Ref.resolve! symbol-table)
   (with-access::Runtime-Ref this (id var)
      (let ((v (symbol-var (Env-runtime-scope env) id)))
	 ;; error should never happen (programming error)
	 (when (not v) (error "Runtime-Var-Ref.resolve!"
			      "Internal Error: Runtime-variable not found"
		      id))
	 (set! var v)))
   (shrink! this)
   this)

;; all global 'defines' have been shrunk to Set!s.
;; all local 'defines' have been transformed to letrecs.
(define-nmethod (Define.resolve! symbol-table)
   (with-access::Define this (lvalue)
      (with-access::Ref lvalue (id)
	 (scheme2js-error "symbol-resolution"
			  "Define at bad location"
			  id
			  this))))

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
	     (unless (symbol? id)
		(let ((name (match-case id
			       ((js-field ?v ?f) (format "~a.~a" v f))
			       (else id))))
		   (scheme2js-error 'define
				    ;; probably internal error.
				    "Illegal identifier"
				    name
				    n)))
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
			   (new-var (instantiate::Var
				       (id id)
				       (kind 'exported)
				       (export-desc desc))))
		       (symbol-var-set! module-scope id new-var)))
		   (else
		    (let ((new-var (instantiate::Var
				      (id id)
				      (kind 'local))))
		       (symbol-var-set! module-scope id new-var))))))))
      (else 'do-nothing)))
