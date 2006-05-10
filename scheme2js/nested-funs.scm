(module nested-funs
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   verbose)
   (export (nested-funs tree::pobject)))


(define (nested-funs tree::pobject)
   (verbose " nested-funs")
   (overload traverse nested-funs (Node
				   (Program Fun-nested-funs)
				   (Lambda Fun-nested-funs))
	     (tree.traverse #f)))

(define-pmethod (Node-nested-funs surrounding-scope)
   (this.traverse1 surrounding-scope))

(define-pmethod (Fun-nested-funs surrounding-scope)
   (if surrounding-scope
       (set! surrounding-scope.nested-funs? #t))
   (this.traverse1 this))
