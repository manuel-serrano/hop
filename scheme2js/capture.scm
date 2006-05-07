(module capture
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   side
	   tail
	   verbose)
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
   (locals tree)
   (free-vars tree)
   (tail-exprs tree #f)
   (side-effect tree)
   (collect-captured tree)
   (latest-allocation tree)
   (allocate-captured! tree))

(define (debug-var-ht-property probj name ht)
   (pfield-set! probj name
		(apply string-append (map (lambda (v)
					     (string-append " "
							    (symbol->string v.id)))
					  (hashtable-key-list ht)))))

(define (locals tree::pobject)
   (verbose " locals (collect)")
   (overload traverse locals (Node
			      (Program Fun-locals)
			      (Lambda Fun-locals)
			      (Tail-rec Fun-locals)
			      Decl)
	     (tree.traverse #f)))

(define-pmethod (Node-locals ht)
   (this.traverse1 ht))

(define-pmethod (Fun-locals ht)
   (let ((new-ht (make-eq-hashtable)))
      (set! this.local-vars new-ht)
      (this.traverse1 new-ht)
      (debug-var-ht-property this 'local-vars_s new-ht)))

(define-pmethod (Decl-locals ht)
   (hashtable-put! ht this.var #t))

(define (free-vars tree::pobject)
   (verbose " free vars")
   (overload traverse capt-fun (Node
				(Program Fun-capt-fun)
				(Lambda Fun-capt-fun)
				(Tail-rec Fun-capt-fun)
				Var-ref)
	     (tree.traverse #f (make-eq-hashtable))))

(define-pmethod (Node-capt-fun local-scope free-vars)
   (this.traverse2 local-scope free-vars))

(define-pmethod (Fun-capt-fun local-scope free-vars)
   (let ((local-vars this.local-vars)
	 (fun-free-vars (make-eq-hashtable)))
      
      ;; store all free vars in fun-free-vars
      (this.traverse2 local-vars fun-free-vars)

      ;; fun-free-vars might contain local vars.
      ;; remove them.
      (hashtable-for-each local-vars
			  (lambda (key val)
			     (hashtable-remove! fun-free-vars key)))
      
      ;; store remaining free vars
      (if (> (hashtable-size fun-free-vars) 0)
	  (begin
	     (set! this.free-vars? #t)
	     (set! this.free-vars fun-free-vars))
	  (set! this.free-vars *empty-hashtable*))

      (debug-var-ht-property this 'free-vars_s fun-free-vars)
      
      ;; pass free vars to parent-fun.
      (hashtable-for-each fun-free-vars
			  (lambda (key val)
			     (hashtable-put! free-vars key #t)))))

(define-pmethod (Var-ref-capt-fun local-scope free-vars)
   (let ((var this.var))
      (unless (hashtable-get local-scope var)
	 (hashtable-put! free-vars var #t))))


(define (collect-captured tree::pobject)
   (verbose " collect captured")
   (overload traverse cc (Node
			  Program
			  Lambda
			  Tail-rec
			  Call
			  Set!
			  Var-ref)
	     (tree.traverse '())))

(define (mark-closure! proc)
   (unless proc.closure? ;; already done
      (set! proc.closure? #t)
      (let ((captured-vars (make-eq-hashtable)))
	 (set! proc.captured-vars captured-vars)
	 (hashtable-for-each proc.free-vars
			     (lambda (key val)
				(hashtable-put! captured-vars key #t)
				(set! key.captured? #t)))
	 (debug-var-ht-property proc 'captured-vars_s captured-vars))))

(define (in-visible-vars var visible-vars)
   (any? (lambda (ht) (hashtable-contains? ht var))
	 visible-vars))

(define (indirect-fun-call proc-var-ref visible-vars)
   (let ((single-val proc-var-ref.var.single-value))
      (if (and single-val
	       (inherits-from? single-val (node 'Lambda)))
	  (let* ((proc single-val)
		 (free-vars proc.free-vars)
		 (captured-vars (make-eq-hashtable)))
	     (hashtable-for-each free-vars
				 (lambda (key val)
				    (if (not (in-visible-vars key
							      visible-vars))
					(begin
					   (hashtable-put! captured-vars key #t)
					   (set! key.captured? #t)))))

	     (debug-var-ht-property proc 'captured-vars_s captured-vars)

	     (if (> (hashtable-size captured-vars) 0)
		 (begin
		    (set! proc.closure? #t)
		    (set! proc.captured-vars captured-vars)))))))

(define-pmethod (Node-cc visible-vars)
   (this.traverse1 visible-vars))

(define-pmethod (Program-cc visible-vars)
   (let ((imported-ht (make-eq-hashtable)))
      (for-each (lambda (var)
		   (hashtable-put! imported-ht var #t))
		this.imported)
      (pcall this Scope-cc (cons imported-ht visible-vars))))

(define-pmethod (Lambda-cc visible-vars)
   (if this.free-vars?
       (mark-closure! this))
   (this.traverse1 (cons this.local-vars visible-vars)))

;; a tail-rec can't capture anything, but can have local vars captured.
(define-pmethod (Tail-rec-cc visible-vars)
   (this.traverse1 (cons this.local-vars visible-vars))
   (let ((captured-locals (make-eq-hashtable)))
      (hashtable-for-each this.local-vars
			  (lambda (var val)
			     (if var.captured?
				 (hashtable-put! captured-locals var #t))))
      (set! this.captured-locals captured-locals)
      (debug-var-ht-property this 'captured-locals_s captured-locals)))

(define-pmethod (Scope-cc visible-vars)
   (this.traverse1 (cons this.local-vars visible-vars)))

;; this is the only place, where we allow capturing functions.
(define-pmethod (Call-cc visible-vars)
   (let ((operator this.operator)
	 (operands this.operands))
      (cond
	 ((inherits-from? operator (node 'Lambda))
	      (pcall operator Scope-cc visible-vars))
	 ((inherits-from? operator (node 'Var-ref))
	  (indirect-fun-call operator visible-vars))
	 (else
	  (operator.traverse visible-vars)))
      (for-each (lambda (node)
		   (node.traverse visible-vars))
		operands)))

(define-pmethod (Set!-cc visible-vars)
   (let ((var this.lvalue.var)
	 (val this.val))
      ;; if this is the single assignment, we can ignore it.
      (if (and var.single-value
	       (inherits-from? val (node 'Lambda)))
	  (pcall val Scope-cc visible-vars)
	  (val.traverse visible-vars))))

(define-pmethod (Var-ref-cc visible-vars)
   (let ((single-val this.var.single-value))
      (if (and single-val
	       (inherits-from? single-val (node 'Lambda)))
	  (mark-closure! single-val))))

(define (latest-allocation tree)
   (verbose " latest allocation")
   (overload traverse latest (Node
			      Program
			      Var-ref
			      Lambda
			      If
			      Case
			      Clause
			      Begin
			      Tail-rec
			      Bind-exit
			      Set!
			      Call)
	     (set! (node 'Node).proto.default-traverse-value '())
	     (tree.traverse #f #f)
	     (delete! (node 'Node).proto.default-traverse-value)))

(define (latest-merge . Ls)
   (let ((ht (make-eq-hashtable)))
      (for-each (lambda (l)
		   (for-each (lambda (v)
				(hashtable-put! ht v #t))
			     l))
		Ls)
      (hashtable-key-list ht)))
   
(define-pmethod (Node-latest captured-locals already-allocated?)
   (this.traverse2 captured-locals already-allocated?))

(define-pmethod (Program-latest captured-locals already-allocated?)
   (this.traverse2 #f #t))

;; only indirectly called.
(define-pmethod (Fun-latest captured-locals already-allocated?)
   (let* ((latest-for-surrounding
	   (if already-allocated?
	       '()
	       (filter (lambda (var)
			  (hashtable-contains? captured-locals var))
		       (hashtable-key-list this.free-vars)))))
      (if (not (null? latest-for-surrounding))
	  (set! this.latest latest-for-surrounding))
      latest-for-surrounding))

(define-pmethod (Lambda-latest captured-locals already-allocated?)
   (this.traverse2 #f #t)
   (pcall this Fun-latest captured-locals already-allocated?))

(define-pmethod (Tail-rec-latest captured-locals already-allocated?)
   (let ((captured-locals this.captured-locals))
      (if (> (hashtable-size captured-locals) 0)
	  ;; don't go into formals and vaargs
	  (this.body.traverse captured-locals #f)
	  (this.traverse2 #f #t)))
   (pcall this Fun-latest captured-locals already-allocated?))

(define-pmethod (If-latest captured-locals already-allocated?)
   (let* ((test-latest (this.test.traverse captured-locals already-allocated?))
	  (then-latest (this.then.traverse captured-locals already-allocated?))
	  (else-latest (this.else.traverse captured-locals already-allocated?))
	  (merged-latest (latest-merge test-latest then-latest else-latest))
	  (test-latest? (not (null? test-latest)))
	  (then-latest? (not (null? then-latest)))
	  (else-latest? (not (null? else-latest))))
      (if (and (not already-allocated?)
	       (and test-latest? (or then-latest? else-latest?)))
	       ;; then-latest? and else-latest? can be latest in their branches.
	  (set! this.latest merged-latest))
      merged-latest))

(define-pmethod (Case-latest captured-locals already-allocated?)
   (let* ((key-latest (this.key.traverse captured-locals already-allocated?))
	  (clauses-latest (map (lambda (clause)
				  (clause.traverse captured-locals
						   already-allocated?))
			       this.clauses))
	  (merged-latest (apply latest-merge (cons key-latest clauses-latest)))
	  (key-latest? (not (null? key-latest)))
	  (clauses-latest? (any? (lambda (x) x)
				 (map (lambda (latest)
					 (not (null? latest)))
				      clauses-latest))))
      (if (and (not already-allocated?)
	       (and key-latest? clauses-latest?))
	  (set! this.latest merged-latest))
      merged-latest))

(define-pmethod (Clause-latest captured-locals already-allocated?)
   (this.expr.traverse captured-locals already-allocated?))

(define-pmethod (Begin-latest captured-locals already-allocated?)
   (if already-allocated?
       (begin
	  (for-each (lambda (node)
		       (node.traverse captured-locals already-allocated?))
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
	      (let ((sub-latest ((car exprs).traverse captured-locals #f)))
		 (loop (cdr exprs)
		       (latest-merge sub-latest merged)
		       (if (null? sub-latest)
			   latest-sub-exprs-count
			   (+fx latest-sub-exprs-count 1))))))))

(define-pmethod (Set!-latest captured-locals already-allocated?)
   (let* ((lvalue-latest (this.lvalue.traverse captured-locals
					       already-allocated?))
	  (val-latest (this.val.traverse captured-locals
					 already-allocated?))
	  (merged-latest (latest-merge lvalue-latest val-latest)))
      (if (and (not already-allocated?)
	       (not (null? lvalue-latest)))
	  (set! this.latest merged-latest))
      merged-latest))

(define-pmethod (Bind-exit-latest captured-locals already-allocated?)
   (let* ((escape-latest (this.escape.traverse captured-locals
					       already-allocated?))
	  (body-latest (this.body.traverse captured-locals
					   already-allocated?))
	  (merged-latest (latest-merge escape-latest body-latest)))
      (if (and (not already-allocated?)
	       (not (null? escape-latest)))
	  (set! this.latest merged-latest))
      merged-latest))

(define-pmethod (Call-latest captured-locals already-allocated?)
   (if already-allocated?
       (begin
	  (for-each (lambda (node)
		       (node.traverse captured-locals already-allocated?))
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
	      (let ((op-latest ((car ops).traverse captured-locals #f)))
		 (loop (cdr ops)
		       (latest-merge op-latest merged)
		       (if (null? op-latest)
			   latest-ops-count
			   (+fx latest-ops-count 1))))))))

(define-pmethod (Var-ref-latest captured-locals already-allocated?)
   (if (and (not already-allocated?)
	    (hashtable-contains? captured-locals this.var))
       (let ((latest (list this.var)))
	  (set! this.latest latest)
	  latest)
       '()))

(define (allocate-captured! tree)
   (verbose " allocate captured")
   (overload traverse! alloc! (Node
			       Tail-rec)
	     (tree.traverse! #t #f)))

(define-pmethod (Node-alloc! already-allocated? surrounding-tail-rec)
   (if (and (not already-allocated?)
	    this.latest)
       (let ((body (this.traverse! #t #f)))
	  (new-node Closure-alloc this.latest body))
       (this.traverse2! already-allocated? surrounding-tail-rec)))

(define-pmethod (Tail-rec-alloc! already-allocated? surrounding-tail-rec)
   (let ((captured-locals this.captured-locals))
      (if (> (hashtable-size captured-locals) 0)
	  (set! this.body (this.body.traverse! #f this))
	  (set! this.body (this.body.traverse! #t this))))
   (pcall this Node-alloc! already-allocated? surrounding-tail-rec))
