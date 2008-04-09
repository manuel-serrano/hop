(module constant-propagation
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
      (import protobject
	      var-ref-util
	      config
	      nodes
	      var
	      side
	      use-count
	      verbose)
   (export (constant-propagation! tree::pobject)))

(define (constant-propagation! tree)
   (if (config 'constant-propagation)
       (unless (config 'call/cc)
	  (verbose "propagation")
	  (side-effect tree)
	  (use-count tree)
	  (overload traverse! propagate! (Node
					  Var-ref
					  Set!)
		    (tree.traverse!)))))

(define-pmethod (Node-propagate!)
   (this.traverse0!))

(define-pmethod (Var-ref-propagate!)
   (define (transitive-closure var-ref)
      (if (runtime-var-ref? var-ref)
	  var-ref
	  (let* ((var var-ref.var)
		 (constant? var.constant?)
		 (value var.value))
	     (cond
		((and constant?
		      value
		      (inherits-from? value (node 'Const))
		      ;; do not propagate vectors and lists.
		      (or (number? value.value)
			  (symbol? value.value)
			  (char? value.value)
			  (boolean? value.value)
			  (eqv? #unspecified value.value)))
		 value)
		((and constant?
		      value
		      (inherits-from? value (node 'Var-ref))
		      value.var.constant?
		      ;; this variable changes according to context.
		      (not (inherits-from? value.var (node 'JS-This-Var))))
		 (transitive-closure value))
		(else var-ref)))))

   (let* ((target (transitive-closure this)))
      (cond
	 ((inherits-from? target (node 'Const))
	  (new-node Const target.value))
	 ((and (inherits-from? target (node 'Var-ref))
	       (not (eq? this target)))
	  (target.var.reference))
	 (else this))))

(define-pmethod (Set!-propagate!)
   ;; don't visit lvalue
   (set! this.val (this.val.traverse!))
   this)
