(module compile
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (include "compile-optimized-call.scm")
   (include "compile-optimized-boolify.scm")
   (option (loadq "protobject-eval.sch"))
   (export (compile::bstring tree::pobject))
   (import protobject
	   config
	   nodes
	   var
	   gen-code
	   statements
	   locals
	   liveness
	   constants
	   verbose
	   allocate-names))

(define (compile::bstring tree::pobject)
   (verbose "Compiling")
   (liveness tree)
   (if (config 'optimize-consts)
       (constants! tree))

   (locals tree
	   #f)   ;; don't collect formals

   (gen-var-names tree)
   (gen-code tree))


(define (gen-code tree)
   (verbose "  generating code")
   ;; small HACK: XXX-compile is stored in XXX.prototype.compile-gen-code
   ;;             XXX.prototype.compile is actually filled with the "location"
   ;;              method, that adds the location to the output.
   (overload compile-gen-code compile
	     (Node Program Part Node Const Var-ref Lambda
		   If Case Clause Set! Begin Bind-exit
		   Call Tail-rec Tail-rec-call Return
		   Closure-alloc Label Break Pragma)
	     (overload compile location (Node)
		       (tree.compile))))

(define-pmethod (Node-location)
   (let ((compil (this.compile-gen-code)))
      (if (and (config 'print-locations)
	       this.loc)
	  (string-append compil "/*" (with-output-to-string
					(lambda () (display this.loc)))
			 "*/")
	  compil)))

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

(define-pmethod (Program-compile)
   (this.body.compile))

(define-pmethod (Part-compile)
   (define (split-globals local-vars)
      ;; HACK; TODO: outer-vars are for now just global vars.
      ;;    should be all vars that are visible to the outside of the part
      (let ((outer-vars (make-eq-hashtable))
	    (part-vars (make-eq-hashtable)))
	 (hashtable-for-each local-vars
			     (lambda (var ignored)
				(hashtable-put! (if var.is-global?
						    outer-vars
						    part-vars)
						var
						#t)))
	 (cons outer-vars part-vars)))

   (let* ((outer/part-vars (if (config 'encapsulate-parts)
			       (split-globals this.local-vars)
			       (cons this.local-vars #f)))
	  (outer-vars (car outer/part-vars))
	  (outer-vars? (not (= 0 (hashtable-size outer-vars))))
	  (part-vars (cdr outer/part-vars))
	  (part-filter-fun this.fun)
	  (compiled-body (if (and part-vars
				  (> (hashtable-size part-vars) 0))
			     (let* ((fun (new-node Lambda '() #f this.body))
				    (call (new-node Call fun '())))
				(mark-statement-form! call #t)
				(set! fun.local-vars part-vars)
				(call.compile))
			     (this.body.compile)))
	  (whole-is-stmt-form? (and outer-vars?
				    (statement-form? this)))
	  (compiled-part (if outer-vars?
			     (gen-code-begin
			      (list
				(gen-code-var-decls
				 (map! (lambda (var)
					  var.compiled)
				       (hashtable-key-list outer-vars)))
				compiled-body)
			      #t)
			     compiled-body)))

      (let ((res (part-filter-fun compiled-part whole-is-stmt-form?)))
	 (if (not whole-is-stmt-form?)
	     (string-append res ";")
	     res))))

(define-pmethod (Lambda-compile)
   (let ((locals this.local-vars)
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
   (if (config 'optimize-boolify)
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
       (or (and (config 'optimize-calls)
		(compile-optimized-call operator this.operands))
	   (gen-code-call (this.operator.compile)
			  (map-node-compile this.operands))))
    this))

(define-pmethod (Tail-rec-compile)
   (let* ((label this.label)
	  (body (gen-code-begin (list (this.body.compile)
				      (gen-code-break label))
				#t))
	  (true (new-node Const #t)))
      (gen-code-while (true.compile)
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
