(module statements
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   mark-statements
	   nodes
	   var
	   verbose)
   (export (statements! tree::pobject)))

;; these pass happens after the scope-flattening pass. As a result there don't
;; exist any Let nodes anymore, and temporary variables can not be put into
;; Let-nodes. Any allocated var has to be added into the "declared-vars" list
;; of the surrounding fun/module.
;; Also they must not have any influence on call/cc.
(define (statements! tree::pobject)
   (verbose "statements")
   (mark-statements tree)
   (transform-statements! tree))

(define (transform-statements! tree::pobject)
   (verbose "  transform-statements")
   (overload traverse! transform-statements! (Node
					      Module
					      (Const Value-transform-statements!)
					      (Var-ref Value-transform-statements!)
					      Lambda
					      (Frame-alloc Value-transform-statements!)
					      Frame-push
					      If
					      Case
					      Clause
					      Set!
					      Begin
					      Call
					      While
					      Continue
					      Return
					      (Call/cc-Counter-Update Value-transform-statements!)
					      (Call/cc-Resume Value-transform-statements!)
					      Labelled
					      Break
					      (Pragma Value-transform-statements!))
	     (tree.traverse! #f #f #t)))

(define-pclass (Return-Assig))
(set! Return-Assig.proto.assig (pmethod (val) (new-node Return val)))

(define (as-expression this surrounding-fun field)
   (if (marked-node? (pfield this field))
       (let* ((stmt-id field)
	      (stmt-var-decl (Decl-of-new-Var stmt-id))
	      (stmt-var stmt-var-decl.var)
	      (new-stmt ((pfield this field).traverse!
					    surrounding-fun stmt-var #t))
	      (bnode (new-node Begin
			       (list new-stmt this))))
	  (cons-set! surrounding-fun.declared-vars stmt-var)
	  (mark-node! bnode #t)
	  (pfield-set! this field stmt-var-decl)
	  bnode)
       (begin
	  (pfield-set! this field ((pfield this field).traverse!
						      surrounding-fun #f #f))
	  this)))


(define-pmethod (Node-transform-statements! surrounding-fun
					    state-var/return
					    statement-form?)
   (error "Node-transform-statements!"
	  "forgot Node-type: "
	  (pobject-name this)))

(define-pmethod (Module-transform-statements! surrounding-fun
					      state-var/return
					      statement-form?)
   ;; treat module like a lambda (for surrounding-fun).
   (set! this.body (this.body.traverse! this #f statement-form?))
   (mark-node! this statement-form?)
   this)

(define-pmethod (Value-transform-statements! surrounding-fun
					     state-var/return
					     statement-form?)
   (let ((new-this (if state-var/return
		       (state-var/return.assig this)
		       this)))
      (mark-node! new-this statement-form?)
      new-this))

(define-pmethod (Lambda-transform-statements! surrounding-fun
					      state-var/return
					      statement-form?)
   (set! this.body (this.body.traverse! this #f #t))
   (pcall this Value-transform-statements!
	  surrounding-fun state-var/return statement-form?))

(define-pmethod (Frame-push-transform-statements! surrounding-fun
						  state-var/return
						  statement-form?)
   (set! this.body (this.body.traverse! surrounding-fun
					state-var/return
					statement-form?))
   (mark-node! this statement-form?)
   this)

(define-pmethod (If-transform-statements! surrounding-fun
					  state-var/return
					  statement-form?)
   (let ((new-this (as-expression this surrounding-fun 'test)))
      (set! this.then (this.then.traverse! surrounding-fun
					   state-var/return
					   statement-form?))
      (set! this.else (this.else.traverse! surrounding-fun
					   state-var/return
					   statement-form?))
      (mark-node! this statement-form?)
      new-this))

(define-pmethod (Case-transform-statements! surrounding-fun
					    state-var/return
					    statement-form?)
   (let ((new-this (as-expression this surrounding-fun 'key)))
      (let loop ((clauses this.clauses))
	 (if (null? clauses)
	     new-this
	     (let ((new-clause ((car clauses).traverse!
					     surrounding-fun
					     state-var/return
					     #t)))
		(set-car! clauses new-clause)
		(loop (cdr clauses)))))))

(define-pmethod (Clause-transform-statements! surrounding-fun
					      state-var/return
					      statement-form?)
   ;; the consts *must* be expressions.
   ;; the following loop should hence not be necessary.
   (let loop ((consts this.consts))
      (unless (null? consts)
	 (set-car! consts ((car consts).traverse! surrounding-fun #f #f))
	 (loop (cdr consts))))
   ;; statement-form? must be #t (as we are always in a 'case')
   (set! this.expr (this.expr.traverse! surrounding-fun
					state-var/return
					statement-form?))
   this)

(define-pmethod (Set!-transform-statements! surrounding-fun
					    state-var/return
					    statement-form?)
   (cond
      ((marked-node? this)
       (let* ((lvalue this.lvalue)
	      (state-var/return-assig (and state-var/return
				    (state-var/return.assig (new-node Const #unspecified))))
	      (new-val (this.val.traverse! surrounding-fun lvalue.var #t))
	      (bnode (new-node Begin
			  `(,@(if (inherits-from? lvalue (node 'Decl))
				  (list lvalue) ;; don't loose the Decl
				  '())
			    ,new-val
			    ,@(if state-var/return-assig
				  (list state-var/return-assig)
				  '())))))
	  (mark-node! lvalue #t) ;; is now a statement
	  (and state-var/return-assig (mark-node! state-var/return-assig #t))
	  (mark-node! new-val #t)
	  (mark-node! bnode #t)
	  bnode))
      ;; result of assignment is unspecified. We don't need to set it to #unspecified...
;       (state-var/return
;        (set! this.val (this.val.traverse! #f #f))
;        (let* ((unspec-assig (state-var/return.assig (new-node Const #unspecified)))
; 	      (bnode (new-node Begin `(,this ,unspec-assig))))
; 	  (mark-node! this statement-form?)
; 	  (mark-node! unspec-assig statement-form?)
; 	  (mark-node! bnode statement-form?)
; 	  bnode))
      (else
       (set! this.val (this.val.traverse! surrounding-fun #f #f))
       (mark-node! this statement-form?)
       this)))

(define-pmethod (Begin-transform-statements! surrounding-fun
					     state-var/return
					     statement-form?)
   (let loop ((exprs this.exprs))
      (cond
	 ((null? exprs) 'do-nothing)
	 ((null? (cdr exprs))
	  (set-car! exprs ((car exprs).traverse!
				      surrounding-fun
				      state-var/return
				      statement-form?)))
	 (else
	  (set-car! exprs ((car exprs).traverse!
				      surrounding-fun
				      #f
				      statement-form?)) ;state-var/return))
	  (loop (cdr exprs)))))
   (mark-node! this statement-form?)
   this)

(define-pmethod (Call-transform-statements! surrounding-fun
					    state-var/return
					    statement-form?)
   (let ((prolog '())
	 (tmp-vars '()))
      (define (transform-optr/opnd expr)
	 (if (marked-node? expr)
	     (let* ((id 'optrOpnd)
		    (optr/opnd-var-decl (Decl-of-new-Var id))
		    (expr-state-var optr/opnd-var-decl.var)
		    (new-expr (expr.traverse! surrounding-fun
					      expr-state-var
					      #t)))
		(set! prolog (cons new-expr prolog))
		(set! tmp-vars (cons expr-state-var tmp-vars))
		(expr-state-var.reference))
	     (expr.traverse! surrounding-fun #f #f)))

      (set! this.operator (transform-optr/opnd this.operator))
      (let loop ((opnds this.operands))
	 (unless (null? opnds)
	    (set-car! opnds (transform-optr/opnd (car opnds)))
	    (loop (cdr opnds))))

      ;; remove potential mark (we might re-add the statement-form?
      ;; mark again)
      (mark-node! this #f)

      (let ((new-this (if state-var/return
			  (state-var/return.assig this)
			  this)))
	 
	 (if (not (null? prolog))
	     (let ((bnode (new-node Begin
				    (append! prolog (list new-this)))))
		(set! surrounding-fun.declared-vars
		      (append! tmp-vars surrounding-fun.declared-vars))
		(mark-node! bnode #t)
		(mark-node! new-this #t)
		bnode)
	     (begin
		(mark-node! new-this statement-form?)
		new-this)))))

(define-pmethod (While-transform-statements! surrounding-fun
					     state-var/return
					     statement-form?)
   (if (marked-node? this.test)
       (error "While-transform-statements!"
	      "while-test must not be statement-form"
	      #f))
   
   (set! this.test (this.test.traverse! surrounding-fun #f #f))
   (set! this.body (this.body.traverse! surrounding-fun #f #t))
   this)

(define-pmethod (Continue-transform-statements! surrounding-fun
						state-var/return
						statement-form?)
   this)

(define-pmethod (Return-transform-statements! surrounding-fun state-var/return statement-form?)
   (if (marked-node? this.val)
       ;; The class Return-Assig is declared in this file.
       (this.val.traverse! surrounding-fun (new Return-Assig) #t)
       (begin
	  (set! this.val (this.val.traverse! surrounding-fun #f #f))
	  this)))

(define-pmethod (Labelled-transform-statements! surrounding-fun
						state-var/return
						statement-form?)
   (set! this.state-var/return state-var/return)
   ;; inform Breaks of state-var/return through the label.
   (set! this.label.state-var/return state-var/return)
   (set! this.body (this.body.traverse! surrounding-fun state-var/return #t))
   (delete! this.label.state-var/return)
   this)

(define-pmethod (Break-transform-statements! surrounding-fun
					     state-var/return
					     statement-form?)
   (let ((labelled-state-var/return this.label.state-var/return))
      (if (inherits-from? labelled-state-var/return Return-Assig)
	  (this.val.traverse! surrounding-fun labelled-state-var/return #t)
	  (let ((traversed-val (this.val.traverse! surrounding-fun
						   labelled-state-var/return
						   #t)))
	     ;; from now on the Break does not have any 'val' anymore.
	     (set! this.val #f)
	     (let ((bnode (new-node Begin
				    (list traversed-val this))))
		(mark-statement-form! bnode #t)
		bnode)))))
