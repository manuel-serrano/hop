(module inline
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes
	   var
	   side
	   transform-util
	   verbose)
   (export (inline! tree::pobject)))

(define *second-pass* #t)

(define (inline! tree)
   (if (config 'do-inlining)
       (begin
	  (verbose "inlining")
	  (constant-propagation! tree)
	  (clean! tree)
	  (let ((inlined-funs? (inline-funs! tree)))
	     (if (and inlined-funs?
		      *second-pass*)
		 (begin
		    (constant-propagation! tree)
		    (clean! tree)))))))

;; currently only single-assig propagation:
;;  if a value is assigned only once, we can safely propagate this value.
(define (constant-propagation! tree)
   (verbose " constant-propagation")
   (side-effect tree)
   ;; we propagate functions only, if they are used once.
   (use-count tree)
   (propagate! tree))

(define (use-count tree)
   (overload traverse use-count (Node
				 Var-ref
				 Set!)
	     (tree.traverse)))

(define-pmethod (Node-use-count)
   (this.traverse0))

(define-pmethod (Var-ref-use-count)
   (let ((var this.var))
      (if var.uses
	  (set! var.uses (+ var.uses 1))
	  (set! var.uses 1))))

;; don't count lvalue of 'set!'s
(define-pmethod (Set!-use-count)
   (this.val.traverse))


(define (propagate! tree)
   (verbose " propagate")
   (overload traverse! propagate! (Node
				   Var-ref
				   Call
				   Set!)
	     (tree.traverse!)))

(define-pmethod (Node-propagate!)
   (this.traverse0!))

(define-pmethod (Var-ref-propagate!)
   (let ((single-value this.var.single-value))
      (if (and single-value
	       (inherits-from? single-value (node 'Const))
	       (or (config 'inline-globals)
		   (not this.var.is-global?)))
	  (begin
	     (set! this.var.inlined? #t)
	     (new-node Const single-value.value))
	  this)))

(define-pmethod (Call-propagate!)
   (let ((op this.operator))
      (if (inherits-from? op (node 'Var-ref))
	  (let* ((var op.var)
		 (single-value var.single-value))
	     (if (and single-value
		      (inherits-from? single-value (node 'Lambda))
		      (not single-value.closure?)
		      (eq? var.uses 1))
		 (begin
		    (set! this.operator op.var.single-value)
		    (set! op.var.inlined? #t)
		    (pcall this Call-propagate!))
		 (this.traverse0!)))
	  (this.traverse0!))))

(define-pmethod (Set!-propagate!)
   (set! this.val (this.val.traverse!))
   this)


(define (clean! tree)
   (verbose " clean")
   (overload traverse! clean! (Node
			       Set!)
	     (tree.traverse!)))

(define-pmethod (Node-clean!)
   (this.traverse0!))

(define-pmethod (Set!-clean!)
   (let ((lvar this.lvalue.var))
      (cond
	 (lvar.inlined?
	  (new-node Const #unspecified))
	 ((and (not lvar.uses)
	       (not (inherits-from? lvar (node 'JS-Var)))
	       (not lvar.is-global?))
	  (this.val.traverse!))
	 (else (this.traverse0!)))))

(define (inline-funs! tree)
   (define *inlined-funs* #f)
   (define *id->js-var* #unspecified)

   (define-pmethod (Node-inline! label)
      (this.traverse1! label))

   (define-pmethod (Program-inline! label)
      (set! *id->js-var* this.id->js-var)
      (this.traverse1! label))

   (define-pmethod (Call-inline! label)
      (let ((op this.operator))
	 (if (inherits-from? op (node 'Lambda))
	     (let* ((fun op)
		    (assigs (parameter-assigs this.operands
					      fun.formals
					      fun.vaarg
					      #f ;; don't take reference
					      *id->js-var*))
		    (traversed-assigs (map (lambda (node)
					      (node.traverse! label))
					   assigs))
		    (label (new-node Label fun.body (gensym 'inlined)))
		    (traversed-label (label.traverse! label)))
		(set! *inlined-funs* #t)
		(new-node Begin (append! traversed-assigs
				    (list (if traversed-label.used
					      traversed-label
					      traversed-label.body)))))
	     (this.traverse1! label))))

   (define-pmethod (Lambda-inline! label)
      (this.traverse1! #f))

   (define-pmethod (Return-inline! label)
      (if label
	  (if this.tail?
	      (this.val.traverse! label)
	      (begin
		 (set! label.used #t)
		 ((new-node Break this.val label).traverse! label)))
	  (this.traverse1! label)))


   ;;=====================================================
   ;; method-start
   ;;====================================================
   (verbose " inline-funs!")
   (overload traverse! inline! (Node
				Program
				Call
				Lambda
				Return)
	     (tree.traverse! #f))
   *inlined-funs*)
