(module compile
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (include "compile-optimized-call.scm")
   (include "compile-optimized-boolify.scm")
   (option (loadq "protobject-eval.sch"))
   (export (compile::bstring tree::pobject)
	   *optimize-calls*
	   *optimize-var-number*
	   *optimize-consts*
	   *optimize-boolify*
	   *encapsulate-parts*)
   (import protobject
	   nodes
	   var
	   gen-code
	   statements
	   collect-vars
	   liveness
	   constants
	   verbose
	   gen-js))

(define *optimize-calls* #t)
(define *optimize-var-number* #f)
(define *optimize-consts* #t)
(define *optimize-boolify* #t)
(define *encapsulate-parts* #f)

(define (compile::bstring tree::pobject)
   (verbose "Compiling")
   (liveness tree)
   (if *optimize-consts*
       (constants! tree))

   (collect-vars tree)

   (gen-var-names tree)
   (gen-code tree))

(define *reusable-var-names* '())

(define (gen-var-names tree)
   (verbose "  generating names for vars")
   (set! *reusable-var-names* '())
   (overload traverse name-gen (Node Var-ref)
	     (overload allocate-name allocate-name (Var JS-Var JS-This-Var)
		       (overload free-name free-name (Var JS-Var)
				 (tree.traverse))))
   (set! *reusable-var-names* '()))

(define-pmethod (Node-name-gen)
   (if this.live-begin-vars
       (for-each (lambda (var)
		    (var.allocate-name #t))
		 this.live-begin-vars))
   (this.traverse0)
   (if this.live-end-vars
       (for-each (lambda (var)
		    (var.free-name))
		 this.live-end-vars)))

(define-pmethod (Var-ref-name-gen)
   (for-each (lambda (var)
		(var.allocate-name #t))
	     (or this.live-begin-vars '()))
   (let ((var this.var))
      (if (not var.compiled) ;; imported, captured or something else
	  (var.allocate-name #f)))
   (for-each (lambda (var)
		(var.free-name))
	     (or this.live-end-vars '())))

(define-pmethod (Var-allocate-name use-generic-name?)
   (cond
      (this.compiled
       'do-nothing)
      (this.is-global?
       (set! this.compiled (mangle-JS-sym this.id)))
      ((and *optimize-var-number*
	    use-generic-name?)
       (if (null? *reusable-var-names*)
	   (set! this.compiled (gen-code-var 'var))
	   (begin
	      (set! this.compiled (car *reusable-var-names*))
	      (set! *reusable-var-names* (cdr *reusable-var-names*)))))
      (else
       (set! this.compiled (gen-code-var this.id)))))

; (define-pmethod (Var-allocate-name)
;    (let ((compiled this.compiled))
;       (or compiled
; 	  (let ((compiled (gen-code-var this.id)))
; 	     (set! this.compiled compiled)
; 	     compiled))))
   
(define-pmethod (JS-Var-allocate-name use-generic-name?)
   (set! this.compiled (symbol->string this.js-id)))

(define-pmethod (JS-This-Var-allocate-name use-generic-name?)
   (set! this.compiled "this"))

(define-pmethod (Var-free-name)
   (set! *reusable-var-names* (cons this.compiled *reusable-var-names*)))

(define-pmethod (JS-Var-free-name)
   'do-nothing)
   


(define (gen-code tree)
   (verbose "  generating code")
   (overload compile compile (Node Program Part Node Const Var-ref Lambda
				   If Case Clause Set! Begin Bind-exit
				   Call Tail-rec Tail-rec-call Return
				   Closure-alloc Label Break Pragma)
	      (tree.compile)))

(define (check-stmt-form expr node)
   (if (statement-form? node)
       (string-append expr ";")
       expr))

(define-pmethod (Node-compile)
   (error #f "forgot node-type: " this))

(define (compile-const const)
   (cond
      ((null? const) (gen-code-nil))
      ((boolean? const) (gen-code-bool const))
      ((symbol? const) (gen-code-symbol const))
      ((char? const) (gen-code-char const))
      ((number? const) (gen-code-number const))
      ((string? const) (gen-code-string const))
      ((vector? const) (gen-code-vector (map compile-const
					    (vector->list const))))
      ((pair? const)
       (gen-code-pair (compile-const (car const))
		      (compile-const (cdr const))))
      ((eq? const #unspecified) (gen-code-unspecified))
      ((keyword? const) (gen-code-keyword const))
      (else (error #f "forgot Const-type: " const))))
   
(define-pmethod (Const-compile)
   (check-stmt-form (compile-const this.value) this))

(define-pmethod (Var-ref-compile)
   (check-stmt-form this.var.compiled this))

(define (map-node-compile l)
   (map (lambda (node)
	   (node.compile))
	l))

(define *prog* #f)

(define-pmethod (Program-compile)
   (set! *prog* this)
   (this.body.compile))

(define-pmethod (Part-compile)
   (define (split-globals collected-vars)
      ;; HACK; TODO: outer-vars are for now just global vars.
      ;;    should be all vars that are visible to the outside of the part
      (let ((outer-vars (make-eq-hashtable))
	    (part-vars (make-eq-hashtable)))
	 (hashtable-for-each collected-vars
			     (lambda (var ignored)
				(hashtable-put! (if var.is-global?
						    outer-vars
						    part-vars)
						var
						#t)))
	 (cons outer-vars part-vars)))

   (let* ((outer/part-vars (if *encapsulate-parts*
			       (split-globals this.collected-vars)
			       (cons this.collected-vars #f)))
	  (outer-vars (car outer/part-vars))
	  (part-vars (cdr outer/part-vars))
	  (part-filter-fun this.fun)
	  (compiled-body (if (and part-vars
				  (> (hashtable-size part-vars) 0))
			     (let* ((fun (new Lambda '() #f this.body))
				    (call (new Call fun '())))
				(mark-statement-form! call #t)
				(set! fun.collected-vars part-vars)
				(call.compile))
			     (this.body.compile))))
      (part-filter-fun (gen-code-begin
			(list
			 (gen-code-var-decls
			  (map! (lambda (var)
				   var.compiled)
				(hashtable-key-list outer-vars)))
			 compiled-body)
			#t))))

(define-pmethod (Lambda-compile)
   (let ((locals this.collected-vars)
	 (ht (make-hashtable)))
      ;; ht will contain the var-names. (unify local-names)
      (hashtable-for-each locals
			  (lambda (var ignored)
			     (hashtable-put! ht var.compiled #t)))
      (check-stmt-form
       (gen-code-function (map-node-compile this.formals)
			  (and this.vaarg (this.vaarg.compile))
			  (this.body.compile)
			  (hashtable-key-list ht)
			  (statement-form? this))
       this)))

(define (boolify compiled node)
   (if *optimize-boolify*
       (compile-optimized-boolify compiled node)
       (gen-code-boolify compiled)))

(define-pmethod (If-compile)
   (gen-code-if (boolify (this.test.compile) this.test)
		(this.then.compile)
		(this.else.compile)
		(statement-form? this)))

(define-pmethod (Case-compile)
   (gen-code-switch (this.key.compile)
		    (map-node-compile this.clauses)))

(define-pmethod (Clause-compile)
   (if this.default-clause?
       (gen-code-default-clause (this.expr.compile))
       (gen-code-clause (map-node-compile this.consts)
			(this.expr.compile))))

(define-pmethod (Set!-compile)
   (check-stmt-form (gen-code-assign (this.lvalue.compile)
				     (this.val.compile)
				     (not (statement-form? this)))
		    this))

(define-pmethod (Begin-compile)
   (gen-code-begin (map-node-compile this.exprs)
		   (statement-form? this)))

(define-pmethod (Bind-exit-compile)
   (gen-code-bind-exit (this.escape.compile)
		       (this.body.compile)))

(define-pmethod (Call-compile)
   (check-stmt-form
    (let ((operator this.operator))
       (or (and *optimize-calls*
		(compile-optimized-call operator this.operands))
	   (gen-code-call (this.operator.compile)
			  (map-node-compile this.operands))))
    this))

(define-pmethod (Tail-rec-compile)
   (let* ((label this.label)
	  (body (gen-code-begin (list (this.body.compile)
				      (gen-code-break label))
				#t)))
      (gen-code-while ((new Const #t).compile)
		      body
		      label)))

(define-pmethod (Tail-rec-call-compile)
   (gen-code-continue this.label))

(define-pmethod (Return-compile)
   (gen-code-return (this.val.compile)))

(define-pmethod (Closure-alloc-compile)
   (gen-code-closure-alloc (map (lambda (var)
				   var.compiled)
				this.allocated-vars)
			   (this.body.compile)))

(define-pmethod (Label-compile)
   (gen-code-label this.id (this.body.compile)))

(define-pmethod (Break-compile)
   (gen-code-begin (list (this.val.compile)
			 (gen-code-break this.label.id))
		   #t))

(define-pmethod (Pragma-compile)
   (gen-code-pragma this.str))
