(module fun-size
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   var
	   verbose)
   (export (fun-size tree::pobject)))

;; rough estimation of function sizes.
;; nested functions are not counted.

(define (fun-size tree::pobject)
   (verbose " fun-size")
   (overload traverse fun-size (Node
				   (Program Fun-fun-size)
				   (Lambda Fun-fun-size)
				   (Tail-rec Fun-fun-size))
	     (tree.traverse #f)))

(define-pmethod (Node-fun-size surrounding-scope)
   ;; TODO: optimize fun-size.
   (if surrounding-scope
       (set! surrounding-scope.size (+ surrounding-scope.size 1)))
   (this.traverse1 surrounding-scope))

(define-pmethod (Fun-fun-size surrounding-scope)
   (set! this.size 0)
   (this.traverse1 this))
