(module side
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   var
	   verbose)
   (export (side-effect tree::pobject)))

(define (side-effect tree::pobject)
   (verbose "side-effect")
   (overload traverse clean-side (Node
				  Program
				  Decl)
	     (tree.traverse))
   (overload traverse side (Node
			    Program
			    Set!)
	     (tree.traverse)))

(define-pmethod (Node-clean-side)
   (this.traverse0))

(define-pmethod (Program-clean-side)
   (for-each (lambda (js-var)
		(delete! js-var.already-defined)
		(delete! js-var.muted)
		(delete! js-var.single-value))
	     this.imported)
   (this.traverse0))

(define-pmethod (Decl-clean-side)
   (let ((var this.var))
      (delete! var.already-defined)
      (delete! var.muted)
      (delete! var.single-value)))


(define-pmethod (Node-side)
   (this.traverse0))

(define-pmethod (Program-side)
   (for-each (lambda (js-var)
		(set! js-var.already-defined #t))
	     this.imported)
   (this.traverse0))

(define-pmethod (Set!-side)
   (this.val.traverse)
   (let ((var this.lvalue.var))
      (if var.already-defined
	  (begin
	     (set! var.muted #t)
	     (delete! var.single-value))
	  (begin
	     (set! var.already-defined #t)
	     (set! var.single-value this.val)))))
