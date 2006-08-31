(module hack-encapsulation
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   mark-statements
	   nodes
	   var
	   config
	   verbose)
   (export (hack-encapsulation! tree::pobject)))

(define (hack-encapsulation! tree::pobject)
   (verbose " hack-encapsulation")
   (overload traverse encapsulate (Node
				     Part)
	     (tree.traverse)))

(define-pmethod (Node-encapsulate)
   (this.traverse0))

(define-pmethod (Part-encapsulate)
   (this.traverse0)
   (if (or (config 'call/cc)
	   (config 'encapsulate-parts))
       (let ((part-decl (Decl-of-new-Var (gensym 'partVar))))
	  (set! this.part-var part-decl.var)
	  (set! this.body
		(new-node Begin
			  (list (new-node Set!
					  part-decl
					  this.body)
				(new-node Return
					  (part-decl.var.reference))))))))
