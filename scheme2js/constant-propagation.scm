(module constant-propagation
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes
	   var
	   side
	   use-count
	   verbose)
   (export (constant-propagation! tree::pobject)))

(define (constant-propagation! tree)
   (if (config 'constant-propagation)
       (begin
	  (verbose "constant-propagation")
	  (side-effect tree)
	  (use-count tree)
	  (overload traverse! propagate! (Node
					  Var-ref
					  Set!)
		    (tree.traverse!)))))

(define-pmethod (Node-propagate!)
   (this.traverse0!))

(define-pmethod (Var-ref-propagate!)
   (let ((single-value this.var.single-value))
      (if (and single-value
	       (inherits-from? single-value (node 'Const))
	       (or (config 'inline-globals)
		   (not this.var.is-global?))
	       (or (= this.var.uses 1)
		   (number? single-value.value)
		   (symbol? single-value.value)
		   (char? single-value.value)))
	  (new-node Const single-value.value)
	  this)))

(define-pmethod (Set!-propagate!)
   ;; don't visit lvalue
   (set! this.val (this.val.traverse!))
   this)
