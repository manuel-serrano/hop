(module statements
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   mark-statements
	   nodes
	   var
	   verbose)
   (export (statements! tree::pobject)))

(define (statements! tree::pobject)
   (verbose "statements")
   (mark-statements tree)
   (transform-statements! tree))

(define (transform-statements! tree::pobject)
   (verbose "  transform-statements")
   (overload traverse! transform-statements! (Node
					      Program
					      Part
					      (Const Value-transform-statements!)
					      (Var-ref Value-transform-statements!)
					      Lambda
					      If
					      Case
					      Clause
					      Set!
					      Begin
					      Bind-exit
					      With-handler
					      Call
					      Tail-rec
					      While
					      Tail-rec-call
					      Return
					      Closure-alloc
					      Label
					      Break
					      (Pragma Value-transform-statements!))
	     (tree.traverse! #f #t)))

(define-macro (as-expression field)
   (let* ((this-field (symbol-append 'this. field))
	  (this-field-traverse! (symbol-append this-field '.traverse!)))
      `(if (marked-node? ,this-field)
	   (let* ((stmt-id (gensym ',field))
		  (stmt-var-decl (Decl-of-new-Var stmt-id))
		  (stmt-var stmt-var-decl.var)
		  (new-stmt (,this-field-traverse! stmt-var #t))
		  (begin-node (new-node Begin (list new-stmt this))))
	      (mark-node! begin-node #t)
	      (set! ,this-field stmt-var-decl)
	      begin-node)
	   (begin
	      (set! ,this-field (,this-field-traverse! #f #f))
	      this))))
   
(define-pmethod (Node-transform-statements! state-var statement-form?)
   (error "Node-transform-statements!" "forgot Node-type: " this))

(define-pmethod (Program-transform-statements! state-var statement-form?)
   (set! this.body (this.body.traverse! state-var statement-form?)))

(define-pmethod (Part-transform-statements! state-var statement-form?)
   (if state-var
       (error "Part-transform-statements!"
	      "Parts must not have state-vars: "
	      #f))
   (if (and statement-form?                 ;; surrounding wants us to be stmt
	    (not (marked-node? this))       ;; we aren't yet stmt
	    (not this.prefer-statement-form?))  ;; and user prefers no stmt
       (begin
	  ;; stay non-stmt, but mark node, so we add a ";" when compiling.
	  (set! this.statement-expression? #t)
	  (set! this.body (this.body.traverse! #f #f)))
       (begin
	  (set! this.body (this.body.traverse! #f statement-form?))
	  (mark-node! this statement-form?)))
   this)

(define-pmethod (Value-transform-statements! state-var statement-form?)
   (let ((new-this (if state-var
		       (state-var.assig this)
		       this)))
      (mark-node! new-this statement-form?)
      new-this))

(define-pmethod (Lambda-transform-statements! state-var statement-form?)
   (set! this.body (this.body.traverse! #f #t))
   (pcall this Value-transform-statements! state-var statement-form?))

(define-pmethod (If-transform-statements! state-var statement-form?)
   (let ((new-this (as-expression test)))
      (set! this.then (this.then.traverse! state-var statement-form?))
      (set! this.else (this.else.traverse! state-var statement-form?))
      (mark-node! this statement-form?)
      new-this))

(define-pmethod (Case-transform-statements! state-var statement-form?)
   (let ((new-this (as-expression key)))
      (let loop ((clauses this.clauses))
	 (if (null? clauses)
	     new-this
	     (let ((new-clause ((car clauses).traverse! state-var #t)))
		(set-car! clauses new-clause)
		(loop (cdr clauses)))))))

(define-pmethod (Clause-transform-statements! state-var statement-form?)
   ;; the consts *must* be expressions.
   ;; the following loop should hence not be necessary.
   (let loop ((consts this.consts))
      (unless (null? consts)
	 (set-car! consts ((car consts).traverse! #f #f))
	 (loop (cdr consts))))
   ;; statement-form? must be #t (as we are always in a 'case')
   (set! this.expr (this.expr.traverse! state-var statement-form?))
   this)

(define-pmethod (Set!-transform-statements! state-var statement-form?)
   (cond
      ((marked-node? this)
       (let* ((lvalue this.lvalue)
	      (state-var-assig (and state-var
				    (state-var.assig (new-node Const #unspecified))))
	      (new-val (this.val.traverse! lvalue.var #t))
	      (bnode (new-node Begin
			  `(,@(if (inherits-from? lvalue (node 'Decl))
				  (list lvalue) ;; don't loose the Decl
				  '())
			    ,new-val
			    ,@(if state-var-assig
				  (list state-var-assig)
				  '())))))
	  (mark-node! lvalue #t) ;; is now a statement
	  (and state-var-assig (mark-node! state-var-assig #t))
	  (mark-node! new-val #t)
	  (mark-node! bnode #t)
	  bnode))
      (state-var
       (set! this.val (this.val.traverse! #f #f))
       (let* ((unspec-assig (state-var.assig (new-node Const #unspecified)))
	      (bnode (new-node Begin `(,this ,unspec-assig))))
	  (mark-node! this statement-form?)
	  (mark-node! unspec-assig statement-form?)
	  (mark-node! bnode statement-form?)
	  bnode))
      (else
       (set! this.val (this.val.traverse! #f #f))
       (mark-node! this statement-form?)
       this)))

(define-pmethod (Begin-transform-statements! state-var statement-form?)
   (let loop ((exprs this.exprs))
      (cond
	 ((null? exprs) 'do-nothing)
	 ((null? (cdr exprs))
	  (set-car! exprs ((car exprs).traverse! state-var statement-form?)))
	 (else
	  (set-car! exprs ((car exprs).traverse! #f statement-form?)) ;state-var))
	  (loop (cdr exprs)))))
   (mark-node! this statement-form?)
   this)

(define-pmethod (Bind-exit-transform-statements! state-var statement-form?)
   (set! this.body (this.body.traverse! state-var #t))
   (set! this.invoc-body (this.invoc-body.traverse! state-var #t))
   this)

(define-pmethod (With-handler-transform-statements! state-var statement-form?)
   (set! this.catch (this.catch.traverse! state-var #t))
   (set! this.body (this.body.traverse! state-var #t))
   this)

(define-pmethod (Call-transform-statements! state-var statement-form?)
   (let ((prolog '()))
      (define (transform-optr/opnd expr)
	 (if (marked-node? expr)
	     (let* ((id (gensym 'optr/opnd))
		    (optr/opnd-var-decl (Decl-of-new-Var id))
		    (expr-state-var optr/opnd-var-decl.var)
		    (new-expr (expr.traverse! expr-state-var #t)))
		(set! prolog (cons new-expr prolog))
		optr/opnd-var-decl)
	     (expr.traverse! #f #f)))

      (set! this.operator (transform-optr/opnd this.operator))
      (let loop ((opnds this.operands))
	 (unless (null? opnds)
	    (set-car! opnds (transform-optr/opnd (car opnds)))
	    (loop (cdr opnds))))

      ;; remove potential mark (we might re-add the statement-form?
      ;; mark again
      (mark-node! this #f)

      (let ((new-this (if state-var
			  (state-var.assig this)
			  this)))
	 
	 (if (not (null? prolog))
	     (let ((bnode (new-node Begin (append! prolog (list new-this)))))
		(mark-node! bnode #t)
		bnode)
	     (begin
		(mark-node! new-this statement-form?)
		new-this)))))

(define-pmethod (Tail-rec-transform-statements! state-var statement-form?)
   (this.traverse2! state-var #t))

(define-pmethod (While-transform-statements! state-var statement-form?)
   (if (marked-node? this.test)
       (error "While-transform-statements!"
	      "while-test must not be statement-form"
	      #f))
   
   (set! this.test (this.test.traverse! #f #f))
   (set! this.body (this.body.traverse! state-var #t))
   this)

(define-pmethod (Tail-rec-call-transform-statements! state-var statement-form?)
   this)

(define-pmethod (Return-transform-statements! state-var statement-form?)
   (as-expression val))

(define-pmethod (Closure-alloc-transform-statements! state-var statement-form?)
   (this.traverse2! state-var #t))

(define-pmethod (Label-transform-statements! state-var statement-form?)
   (set! this.state-var state-var)
   (set! this.body (this.body.traverse! state-var #t))
   this)

(define-pmethod (Break-transform-statements! state-var statement-form?)
   (set! this.val (this.val.traverse! this.label.state-var #t))
   this)
