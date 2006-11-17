(module check
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   verbose)
   (export (check tree::pobject)))

(define (check tree)
   (verbose "checking tree")
   (overload traverse check (Node
			     Decl)
	     (tree.traverse))
   (overload traverse check-clean (Node
				   Decl)
	     (tree.traverse)))

(define-pmethod (Node-check)
   (if this.checked?
       (error "Check"
	      "Recursive Node: "
	      (pobject-name this))
       ;(set! this.BAD-BAD-BAD-BAD #t)
       (begin
	  (set! this.checked? #t)
	  (this.traverse0))))

(define-pmethod (Decl-check)
   (if this.var.checked?
       (begin
	  ;(error "Check"
	;	 "Double Decl: "
	;	 this.var.id)
	  (set! this.BAD-BAD-BAD-BAD #t)
	  (set! this.var.BAD-BAD-BAD-BAD #t)
	  )
       (begin
	  (set! this.var.checked? #t)))
   (pcall this Node-check))
       

(define-pmethod (Node-check-clean)
   (delete! this.checked?)
   (this.traverse0))

(define-pmethod (Decl-check-clean)
   (delete! this.checked?)
   (delete! this.var.checked?)
   (this.traverse0))
