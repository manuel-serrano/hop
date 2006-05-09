(module var-propagation
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes
	   var
	   side
	   verbose)
   (export (var-propagation! tree::pobject)))

(define (var-propagation! tree)
   (if (config 'var-propagation)
       (begin
	  (verbose "var-propagation")
	  (side-effect tree)
	  (overload traverse propagate (Node
					Var-ref
					Set!)
		    (tree.traverse)))))

(define-pmethod (Node-propagate)
   (this.traverse0))

(define-pmethod (Var-ref-propagate)
   (if this.var.replacement-var
       (set! this.var this.var.replacement-var)))

(define-pmethod (Set!-propagate)
   (let ((lvalue this.lvalue)
	 (val this.val))
      (if (and (inherits-from? lvalue (node 'Var-ref))
	       (inherits-from? val (node 'Var-ref))
	       lvalue.var.single-value
	       (not lvalue.var.muted?)
	       (not val.var.muted?)
	       (or (config 'inline-globals)
		   (and (not lvalue.var.is-global?)
			(not val.var.is-global?))))
	  (set! lvalue.var.replacement-var (or val.var.replacement-var
					       val.var))))
   (this.val.traverse))
