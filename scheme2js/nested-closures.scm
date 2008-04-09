(module nested-closures
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   locals
	   free-vars
	   captured-vars
	   verbose)
   (export (nested-closures tree::pobject)))


(define (nested-closures tree::pobject)
   (verbose " nested-closures")
   (captured-vars tree)
   (overload traverse nested-closures (Node
				       (Module Fun-nested-closures)
				       (Lambda Fun-nested-closures))
	     (tree.traverse #f)))

(define-pmethod (Node-nested-closures surrounding-scope)
   (this.traverse1 surrounding-scope))

(define-pmethod (Fun-nested-closures surrounding-scope)
   (if (and surrounding-scope
	    this.closure?)
       (set! surrounding-scope.nested-closures? #t))
   (this.traverse1 this))
