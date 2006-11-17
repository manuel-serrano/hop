(module capture
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   side
	   locals
	   free-vars
	   captured-vars
	   tail
	   verbose
	   config
	   var)
   (export (capture! tree::pobject)))

(define *empty-hashtable* (make-eq-hashtable))

;; tail-rec calls are transformed into gotos.
;; however captured variables need to be fresh at each loop. We therefore
;; collect them and create an artificial scope for them.
;; Small example illustrating the problem:
;; (define *captured-lambdas* '())
;; (let loop ((x 0))
;;    (if (< x 10)
;;        (begin
;;          (set! *captured-lambdas* (cons (lambda () x)
;;                                         *captured-lambdas*))
;;          (loop (+ x 1))))
;;
;; If this is transformed into a goto/while:
;; var x = 0
;; while (x < 10) {
;;   *captured-lambdas* = cons(function() { return x; }, *captured-lambdas*);
;; }
;;
;; then the x is shared by all dynamically created functions... (and all will
;; return the same value).


(define (capture! tree::pobject)
   (verbose "capture")
   (locals tree
	   #t)   ;; collect formals    
   (free-vars tree)
   (tail-exprs tree
	       #f) ;; intermediate nodes are not tail.
   (side-effect tree)
   (captured-vars tree)

   (tail-rec-escapes tree)

   (latest-allocation tree)
   (allocate-captured! tree)
   (finish-allocation! tree))

;; every tail-rec receives a list of variables, that are captured within the loop.
(define (tail-rec-escapes tree)
   (verbose " tail-rec escapes")
   (overload traverse tail-rec-escapes (Node
					Lambda
					Tail-rec
					Decl)
	     (tree.traverse #f)))

(define-pmethod (Node-tail-rec-escapes ht)
   (this.traverse1 ht))

(define-pmethod (Lambda-tail-rec-escapes ht)
   (this.traverse1 #f))

(define-pmethod (Tail-rec-tail-rec-escapes ht)
   (let ((rec-escapes (make-eq-hashtable)))
      (set! this.escapes rec-escapes)
      (this.traverse1 rec-escapes)))

(define-pmethod (Decl-tail-rec-escapes ht)
   (when (and ht
	      this.var.captured?)
	 (hashtable-put! ht this.var #t)))

(define (latest-allocation tree)
   (verbose " latest allocation")
   (overload traverse latest (Node
			      Var-ref
			      Lambda
			      If
			      Case
			      Clause
			      Begin
			      Tail-rec
			      Set!
			      Call)
	     (set! (node 'Node).proto.default-traverse-value '())
	     (tree.traverse '())
	     (delete! (node 'Node).proto.default-traverse-value)))

(define (latest-merge . Ls)
   (let ((ht (make-eq-hashtable)))
      (for-each (lambda (l)
		   (for-each (lambda (v)
				(hashtable-put! ht v #t))
			     l))
		Ls)
      (hashtable-key-list ht)))
   
(define-pmethod (Node-latest tail-rec-escapes)
   (this.traverse1 tail-rec-escapes))

(define-pmethod (Lambda-latest tail-rec-escapes)
   ;; any free variable that is in the tail-rec-escapes must be allocated
   ;; before the creation of the function. We can't just take the
   ;; "captured-vars", as two different functions might not share the same
   ;; variable anymore. Ex:
   ;; (let loop ((x 0))
   ;;   (let ((f (lambda () (print x)))) ;; not captured
   ;;     (set! global-funs (cons (lambda () x) global-funs))
   ;;     (set! x (+ x 3))
   ;;     (f) 
   ;;	  (loop (+ x 1))))
   ;;
   ;; If we looked only at captured variables, we would allocate the "loop"-x
   ;; only just before the assignment to "global-funs". We would then change
   ;; the value of "x". the procedure "f" would hence use a different x, than
   ;; the global-funs procedure. But more importantly we would not modify the
   ;; same 'x', and the call to "f" would not print the augmented x.
   (define (latest-for-surrounding)
      (let ((free-vars-list (hashtable-key-list this.free-vars)))
	 (let loop ((escapes tail-rec-escapes)
		    (res '()))
	    (if (null? escapes)
		res
		(loop (cdr escapes)
		      (append! (filter (lambda (var)
					  (hashtable-contains? (car escapes)
							       var))
				       free-vars-list)
			       res))))))
      
   (this.traverse1 '())
   (let ((latest-for-surrounding-L (latest-for-surrounding)))
      (when (not (null? latest-for-surrounding-L))
	 (set! this.latest latest-for-surrounding-L)
	 (set! this.references-tail-rec-variables? #t))
      latest-for-surrounding-L))

(define-pmethod (Tail-rec-latest tail-rec-escapes)
   (let* ((escapes this.escapes)
	  (new-rec-escapes (if (> (hashtable-size escapes) 0)
			       (cons escapes tail-rec-escapes)
			       tail-rec-escapes))
	  (body-latest (this.body.traverse new-rec-escapes))
	  (surrounding-latest (filter (lambda (var)
					 (not (hashtable-contains? escapes
								   var)))
				      body-latest)))
      (if (not (= (length surrounding-latest) (length body-latest)))
	  (set! this.has-latest? #t))
      (set! this.latest surrounding-latest)
      surrounding-latest))

(define-pmethod (If-latest tail-rec-escapes)
   (let* ((test-latest (this.test.traverse tail-rec-escapes))
	  (then-latest (this.then.traverse tail-rec-escapes))
	  (else-latest (this.else.traverse tail-rec-escapes))
	  (merged-latest (latest-merge test-latest then-latest else-latest))
	  (test-latest? (not (null? test-latest)))
	  (then-latest? (not (null? then-latest)))
	  (else-latest? (not (null? else-latest))))
      (if (and tail-rec-escapes
	       (and test-latest? (or then-latest? else-latest?)))
	       ;; then-latest? and else-latest? can be latest in their branches.
	  (set! this.latest merged-latest))
      merged-latest))

(define-pmethod (Case-latest tail-rec-escapes)
   (let* ((key-latest (this.key.traverse tail-rec-escapes))
	  (clauses-latest (map (lambda (clause)
				  (clause.traverse tail-rec-escapes))
			       this.clauses))
	  (merged-latest (apply latest-merge (cons key-latest clauses-latest)))
	  (key-latest? (not (null? key-latest)))
	  (clauses-latest? (any? (lambda (x) x)
				 (map (lambda (latest)
					 (not (null? latest)))
				      clauses-latest))))
      (if (and tail-rec-escapes
	       (and key-latest? clauses-latest?))
	  (set! this.latest merged-latest))
      merged-latest))

(define-pmethod (Clause-latest tail-rec-escapes)
   (this.expr.traverse tail-rec-escapes))

(define-pmethod (Begin-latest tail-rec-escapes)
   (if (not tail-rec-escapes)
       (begin
	  (for-each (lambda (node)
		       (node.traverse #f))
		    this.exprs)
	  '())
       (let loop ((exprs this.exprs)
		  (merged '())
		  (latest-sub-exprs-count 0))
	  (if (null? exprs)
	      (begin
		 (if (> latest-sub-exprs-count 1)
		     (set! this.latest merged))
		 merged)
	      (let ((sub-latest ((car exprs).traverse tail-rec-escapes)))
		 (loop (cdr exprs)
		       (latest-merge sub-latest merged)
		       (if (null? sub-latest)
			   latest-sub-exprs-count
			   (+fx latest-sub-exprs-count 1))))))))

(define-pmethod (Set!-latest tail-rec-escapes)
   (let* ((lvalue-latest (this.lvalue.traverse tail-rec-escapes))
	  (val-latest (this.val.traverse tail-rec-escapes))
	  (merged-latest (latest-merge lvalue-latest val-latest)))
      (if (and tail-rec-escapes
	       (not (null? lvalue-latest)))
	  (set! this.latest merged-latest))
      merged-latest))

(define-pmethod (Call-latest tail-rec-escapes)
   (if (not tail-rec-escapes)
       (begin
	  (for-each (lambda (node)
		       (node.traverse #f))
		    (cons this.operator this.operands))
	  '())
       (let loop ((ops (cons this.operator this.operands))
		  (merged '())
		  (latest-ops-count 0))
	  (if (null? ops)
	      (begin
		 (if (>fx latest-ops-count 1)
		     (set! this.latest merged))
		 merged)
	      (let ((op-latest ((car ops).traverse tail-rec-escapes)))
		 (loop (cdr ops)
		       (latest-merge op-latest merged)
		       (if (null? op-latest)
			   latest-ops-count
			   (+fx latest-ops-count 1))))))))

(define-pmethod (Var-ref-latest tail-rec-escapes)
   (if (any? (lambda (ht)
		(hashtable-contains? ht this.var))
	     tail-rec-escapes)
       (let ((latest (list this.var)))
	  (set! this.latest latest)
	  latest)
       '()))

(define (allocate-captured! tree)
   (verbose " allocate captured")
   (overload traverse! alloc! (Node
			       Tail-rec)
	     (tree.traverse! #t)))

(define (diff-vars latest already-allocated)
   (define (difference l1 l2)
      (let ((ht (make-eq-hashtable)))
	 (for-each (lambda (x) (hashtable-put! ht x #t)) l1)
	 (for-each (lambda (x) (hashtable-remove! ht x)) l2)
	 (hashtable-key-list ht)))

   (and latest
	(not (eq? already-allocated #t))
	(let ((diff (difference latest already-allocated)))
	   (and (not (null? diff))
		diff))))

(define-pmethod (Node-alloc! already-allocated)
   (let ((diff (diff-vars this.latest already-allocated)))
      (delete! this.latest)
      (if diff
	  (let* ((new-this (this.traverse1! #t))
		 (closure-decl (Decl-of-new-Var (gensym 'closure)))
		 (closure-var closure-decl.var)
		 (alloc (new-node Closure-alloc))
		 (assig (new-node Set! closure-decl alloc))
		 (b-node (new-node Begin (list assig new-this))))
	     (set! closure-decl.var.captured? #t)
	     (set! closure-decl.var.closure-object? #t)
	     (for-each (lambda (v)
			  (let ((ref-id (symbol-append closure-var.id
						       '_
						       v.id)))
			     (set! v.closure-var
				   (new-node Field-Var
					     ref-id
					     closure-var
					     v))))
		       diff)
	     b-node)
	  (this.traverse1! already-allocated))))

(define-pmethod (Tail-rec-alloc! already-allocated)
   (if this.has-latest?
       (set! this.body (this.body.traverse! this.latest))
       (set! this.body (this.body.traverse! #t)))
   (delete! this.has-latest?)
   (let ((diff (diff-vars this.latest already-allocated)))
      (delete! this.latest)
      (if diff
	  (let* ((new-this (this.traverse1! #t))
		 (closure-decl (Decl-of-new-Var (gensym 'closure)))
		 (closure-var closure-decl.var)
		 (alloc (new-node Closure-alloc))
		 (assig (new-node Set! closure-decl alloc))
		 (b-node (new-node Begin (list assig new-this))))
	     (set! closure-decl.var.captured? #t)
	     (set! closure-decl.var.closure-object? #t)
	     (for-each (lambda (v)
			  (let ((ref-id (symbol-append closure-var.id
						       '_
						       v.id)))
			     (set! v.closure-var
				   (new-node Field-Var
					     ref-id
					     closure-var
					     v))))
		       diff)
	     b-node)
	  this)))

(define (finish-allocation! tree)
   (verbose " finish allocation")
   (overload traverse! finish! (Node
				Var-ref
				Lambda)
	     (tree.traverse! (make-eq-hashtable))))

(define-pmethod (Node-finish! with-variables)
   (this.traverse1! with-variables))

(define-pmethod (Var-ref-finish! with-variables)
   (if (and this.var.closure-var
	    (not (hashtable-get with-variables this.var)))
       (this.var.closure-var.reference)
       this))

(define-pmethod (Lambda-finish! with-variables)
   (let ((closure-obj-vars '()))
      (if this.captured-vars
	  (hashtable-for-each this.captured-vars
			      (lambda (key ignored)
				 (if (and key.closure-var
					  (not (memq key.closure-var.obj
						     closure-obj-vars)))
				     (set! closure-obj-vars
					   (cons key.closure-var.obj
						 closure-obj-vars))))))
      (cond
	 ((null? closure-obj-vars)
	  (this.traverse1! with-variables))
	 ((config 'with-closures)
	  (hashtable-for-each this.captured-vars
			      (lambda (v ignored)
				 (hashtable-put! with-variables v #t)))
	  (this.traverse1! with-variables)
	  (hashtable-for-each this.captured-vars
			      (lambda (v ignored)
				 (hashtable-remove! with-variables v)))
	  (new-node Closure-with-use
		    (map (lambda (v) (v.reference)) closure-obj-vars)
		    this))
	 (else
	  (this.traverse1! with-variables)
	  (new-node Closure-use
		    (map (lambda (v) (v.reference)) closure-obj-vars)
		    this)))))
