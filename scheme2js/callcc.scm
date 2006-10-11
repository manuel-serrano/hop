(module callcc
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes
	   side
	   js-interface
	   mark-statements
	   verbose)
   (export (callcc-check-points tree::pobject)
	   (callcc-late tree)))


(define (callcc-check-points tree::pobject)
   (when (config 'call/cc)
      (verbose "call/cc check-points")
      (side-effect tree)
      (rev-call-tree tree)
      (callcc-propagation tree)
      (check-point tree)))

(define (runtime-var-ref-id n)
   (and (inherits-from? n (node 'Var-ref))
	(let* ((var n.var)
	       (js-id var.js-id))
	   (and js-id
		(not var.muted?)
		(any? (lambda (p) (eq? js-id (cadr p))) *runtime-var-mapping*)
		var.id))))

(define (call-target operator)
   (cond
      ((inherits-from? operator (node 'Lambda))
       operator)
      ((runtime-var-ref-id operator)
       operator)
      ((and (inherits-from? operator (node 'Var-ref))
	    operator.var.single-value)
       (call-target operator.var.single-value))
      (else
       #f)))

;; every lambda receives its callers. recursive calls are ignored.
(define (rev-call-tree tree)
   (verbose " call/cc rev-call-tree")
   (overload traverse call-tree (Node
				 Program
				 Lambda
				 Call)
	     (tree.traverse #f)))

(define-pmethod (Node-call-tree current-fun)
   (this.traverse1 current-fun))

;; Program is not really a function, but we'll treat it like one, here.
(define-pmethod (Program-call-tree current-fun)
   (this.traverse1 this))

(define-pmethod (Lambda-call-tree current-fun)
   (this.traverse1 this))

(define-pmethod (Call-call-tree current-fun)
   (this.traverse1 current-fun)
   (let ((target (call-target this.operator)))
      (if target
	  (begin
	     (set! this.target target)
	     (if (inherits-from? target (node 'Lambda))
		 (set! target.callers (cons current-fun
					    (or target.callers '()))))))))

(define (callcc-propagation tree)
   (verbose " call/cc propagation")
   (overload traverse propagate (Node
				 Program
				 Lambda
				 Call)
	     (tree.traverse #f)))

(define-pmethod (Node-propagate current-fun)
   (this.traverse1 current-fun))

;; Program is not a function, but in this context we'll treat it like one.
(define-pmethod (Program-propagate current-fun)
   (this.traverse1 this))

(define-pmethod (Lambda-propagate current-fun)
   (this.traverse1 this))

(define *optimizable-higher*
   `((apply ,car)
     (map ,car)
     (for-each ,car)
     (call-with-values ,car ,cadr)
     (dynamic-wind ,car ,cadr) ;; we ignore the 'after'-part
     (js-call ,cadr)
     (with-input-from-port ,cadr)
     (with-input-from-string ,cadr)
     (with-output-to-port ,cadr)
     (with-output-to-string ,car)
     (with-output-to-procedure ,cadr) ;; car must not call call/cc
     (hashtable-for-each ,cadr)))

(define-pmethod (Call-propagate current-fun)
   (define *call/cc-names*
      (let ((interface-p (assoc 'call/cc *runtime-var-mapping*)))
	 (if (not interface-p)
	     '()
	     (map! car (filter (lambda (p) (eq? (cadr interface-p)
						(cadr p)))
			       *runtime-var-mapping*)))))

   (define (mark-call/cc-fun fun)
      (unless fun.potential-call/cc?
	 (set! fun.potential-call/cc? #t)
	 (if fun.callers (for-each mark-call/cc-fun fun.callers))))

   (this.traverse1 current-fun)
   (let* ((target this.target)
	  (runtime-id (and target (runtime-var-ref-id target))))
      (cond
	 ;; optim.
	 ((and runtime-id (assq runtime-id *optimizable-higher*))
	  =>
	  (lambda (match)
	     (if (any?
		  (lambda (which)
		     (let* ((higher-target (call-target (which this.operands)))
			    (higher-target-runtime
			     (runtime-var-ref-id higher-target)))
			(cond
			   ((and (inherits-from? higher-target (node 'Lambda))
				 (not higher-target.potential-call/cc?))
			    ;; put us into the callers list of the
			    ;; higher-target, in case it hasn't been traversed
			    ;; yet.
			    (when (or (not higher-target.callers)
				      (not (memq current-fun
						 higher-target.callers)))
			       (set! higher-target.callers
				     (cons current-fun
					   (or higher-target.callers
					       '()))))
			    ;; put us into a 'higher-targets' list so we can
			    ;; verify later on, if any of these yet unevaluated
			    ;; functions turned out to contain call/ccs.
			    (set! this.higher-targets
				  (cons higher-target
					(or this.higher-targets
					    '())))
			    ;; for now don't mark us as call/cc
			    #f)
			   ((and higher-target-runtime
				 (not (memq higher-target-runtime
					    (append *call/cc-names*
						    *higher-order-runtime*))))
			    ;; do-nothing
			    #f)
			   (else
			    ;; mark call/cc-fun and abort traversal of list.
			    #t))))
		  (cdr match))
		 (begin
		    (mark-call/cc-fun current-fun)
		    (set! this.call/cc-call? #t)))))
		 
	 ((or (not target) ;; assume call/cc
	      target.potential-call/cc?
	      (and runtime-id (memq runtime-id (append *higher-order-runtime*
						       *call/cc-names*))))
	  (begin
	     (mark-call/cc-fun current-fun)
	     (set! this.call/cc-call? #t)))
	 (else
	  'do-nothing))))

(define (check-point tree)
   (verbose " call/cc check-point")
   (overload traverse check-point (Node
				   Program
				   Lambda
				   Call)
	     (tree.traverse #f)))

(define-pmethod (Node-check-point index)
   (this.traverse1 index))

(define-pmethod (Program-check-point index)
   (this.traverse1 (list 1)))

(define-pmethod (Lambda-check-point index)
   (delete! this.callers)
   (this.traverse1 (list 1)))

(define-pmethod (Call-check-point index)
   (define (mark-call/cc-call call index)
      (set! call.call/cc-index (car index))
      (set! call.call/cc-stmt? #t)
      (for-each (lambda (operand)
		   (unless (or (inherits-from? operand (node 'Var-ref))
			       (inherits-from? operand (node 'Const)))
		      (set! operand.call/cc-stmt? #t)))
		call.operands)
      (set-car! index (+ (car index) 1)))

   (this.traverse1 index)
   (let ((target this.target)
	 ;; higher-targets have been set during propagate pass.
	 ;; at this time these targets haven't been evaluated yet.
	 ;; if one of them turned out to contain call/cc we must mark this call
	 ;; as call/cc.
	 (higher-targets this.higher-targets)
	 (call/cc-call? this.call/cc-call?))
      (delete! this.target)
      (delete! this.higher-targets)
      (delete! this.call/cc-call?)
      (cond
	 (call/cc-call?
	  (mark-call/cc-call this index))
	 ((or (and target target.potential-call/cc?)
	      (and higher-targets (any? (lambda (n)
					   n.potential-call/cc?)
					higher-targets)))
	  (mark-call/cc-call this index))
	 ((and target (not target.potential-call/cc?))
	  'do-nothing)
	 (else
	  ;; should not happen
	  (verbose "should not happen (Call-check-point)")
	  (mark-call/cc-call this index)))))

;; the statement-pass introduces new Set!s. We shift the call/cc information
;; down to the Set!s.
(define (callcc-late tree)
   (when (config 'call/cc)
      (verbose " call/cc lates")
      (overload traverse late (Node
			       Lambda
			       If
			       Case
			       Clause
			       Begin
			       Bind-exit
			       With-handler
			       Set!
			       Call
			       While)
		(set! (node 'Node).proto.default-traverse-value '())
		(tree.traverse)
		(delete! (node 'Node).proto.default-traverse-value))))

(define (range-merge Ls)
   (let ((ht (make-eq-hashtable)))
      (for-each (lambda (l)
		   (for-each (lambda (v)
				(hashtable-put! ht v #t))
			     l))
		Ls)
      (hashtable-key-list ht)))
   

(define (multi-traverse n children)
   (let* ((children-range (range-merge (map (lambda (n)
					       (n.traverse))
					    children)))
	  (this-range (if n.call/cc-index
			  (cons n.call/cc-index children-range)
			  children-range)))
      (if (and (statement-form? n)
	       this-range
	       (not (null? this-range)))
	  (set! n.call/cc-range this-range))
      this-range))

(define-pmethod (Node-late)
   (let* ((children-range (this.traverse0))
	  (this-range (if this.call/cc-index
			  (cons this.call/cc-index children-range)
			  children-range)))
      (if (and (statement-form? this)
	       this-range
	       (not (null? this-range)))
	  (set! this.call/cc-range this-range))
      this-range))

(define-pmethod (Lambda-late)
   (multi-traverse this (if this.vaarg
			    (append this.formals (list this.vaarg
						       this.body))
			    (append this.formals (list this.body))))
   '())
   
(define-pmethod (If-late)
   (begin0
    (multi-traverse this (list this.test this.then this.else))
    (let ((then-range this.then.call/cc-range)
	  (else-range this.else.call/cc-range))
       (if (and then-range (not (null? then-range)))
	   (set! this.call/cc-then-range then-range))
       (if (and else-range (not (null? else-range)))
	   (set! this.call/cc-else-range else-range)))))

(define-pmethod (Case-late)
   (multi-traverse this (cons this.key this.clauses)))

(define-pmethod (Clause-late)
   (multi-traverse this (append this.consts (list this.expr))))

(define-pmethod (Begin-late)
   (multi-traverse this this.exprs))

(define-pmethod (Bind-exit-late)
   (multi-traverse this (list this.escape
			      this.body
			      this.result-decl
			      this.invoc-body)))

(define-pmethod (With-handler-late)
   (multi-traverse this (list this.exception
			      this.catch
			      this.body)))

(define-pmethod (Call-late)
   (multi-traverse this (cons this.operator this.operands)))

(define-pmethod (While-late)
   (multi-traverse this (list this.test this.body)))

(define-pmethod (Set!-late)
   (multi-traverse this (list this.lvalue this.val)))
