(module rm-unused-vars
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   var
	   use-count
	   verbose)
   (export (rm-unused-vars! tree::pobject)))

(define (rm-unused-vars! tree)
   (verbose " removing unused vars")
   (use-count tree)
   (overload traverse! rm! (Node
			   Set!
			   Lambda
			   Decl)
	     (tree.traverse!)))

(define-pmethod (Node-rm!)
   (this.traverse0!))

(define-pmethod (Set!-rm!)
   (if (and (not this.lvalue.var.is-global?)
	    (= this.lvalue.var.uses 0))
       (this.val.traverse!)
       (this.traverse0!)))

(define-pmethod (Lambda-rm!)
   ;; don't go into formals (they must not be removed)
   (set! this.body (this.body.traverse!))
   this)

(define-pmethod (Decl-rm!)
   (if (and (not this.var.is-global?)
	    (= this.var.uses 0))
       (new-node Const #unspecified)
       (this.traverse0!)))
