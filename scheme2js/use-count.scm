(module use-count
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   var
	   verbose)
   (export (use-count tree::pobject)))

(define (use-count tree)
   (overload traverse clean-uses (Node
				  Var-ref)
	     (tree.traverse))
   (overload traverse use-count (Node
				 Var-ref
				 Set!)
	     (tree.traverse)))

(define-pmethod (Node-clean-uses)
   (this.traverse0))

(define-pmethod (Var-ref-clean-uses)
   (delete! this.var.uses))


(define-pmethod (Node-use-count)
   (this.traverse0))

(define-pmethod (Var-ref-use-count)
   (let ((var this.var))
      (if var.uses
	  (set! var.uses (+ var.uses 1))
	  (set! var.uses 1))))

;; don't count lvalue of 'set!'s, but set the use-count to 0 (if necessary)
(define-pmethod (Set!-use-count)
   (if (not this.lvalue.var.uses)
       (set! this.lvalue.var.uses 0))
   (this.val.traverse))
