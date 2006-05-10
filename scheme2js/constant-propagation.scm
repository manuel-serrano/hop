(module constant-propagation
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes
	   var
	   side
	   verbose)
   (export (constant-propagation! tree::pobject)))

(define (constant-propagation! tree)
   (if (config 'constant-propagation)
       (begin
	  (verbose "constant-propagation")
	  (side-effect tree)
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
		   (not this.var.is-global?)))
	  (new-node Const single-value.value)
	  this)))

(define-pmethod (Set!-propagate!)
   ;; don't visit lvalue
   (set! this.val (this.val.traverse!))
   this)
