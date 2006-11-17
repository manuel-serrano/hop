(module loop-hoist
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   side
	   captured-vars
	   nodes
	   verbose)
   (export (loop-hoist! tree::pobject)))

(define (loop-hoist! tree)
   (when (config 'loop-hoist)
      (verbose "loop hoist")
      (side-effect tree)
      (captured-vars tree)
      (overload traverse! hoist! (Node
				 (Module Fun-hoist!)
				 (Lambda Fun-hoist!)
				 Set!
				 (While Loop-hoist!)
				 (Tail-rec Loop-hoist!))
		(tree.traverse! #f))))

(define-pmethod (Node-hoist! outer-loop)
   (this.traverse1! outer-loop))

(define-pmethod (Fun-hoist! outer-loop)
   (this.traverse1! #f))

(define-pmethod (Set!-hoist! outer-loop)
   (this.traverse1! outer-loop)
   (if (and outer-loop
	    this.lvalue.var.single-value
	    (inherits-from? this.val (node 'Lambda))
	    (not this.val.closure?))
       (begin
	  (cons-set! outer-loop.decls-to-hoist this)
	  (new-node Const #unspecified))
       this))

(define-pmethod (Loop-hoist! outer-loop)
   (this.traverse1! (or outer-loop this))
   (if this.decls-to-hoist
       (let ((decls this.decls-to-hoist))
	  (delete! this.decls-to-hoist)
	  (new-node Begin (append decls (list this))))
       this))
