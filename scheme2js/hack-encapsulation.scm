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

;; if we are encapsulation the parts (ie putting it into an anonymous
;; function), then we need/want a 'return' in the anonymous function.
;; this should all become better, once we have modules.
(define (hack-encapsulation! tree::pobject)
   (verbose " hack-encapsulation")
   (overload traverse encapsulate (Node
				   Module)
	     (tree.traverse)))

(define-pmethod (Node-encapsulate)
   (this.traverse0))

(define-pmethod (Module-encapsulate)
   (this.traverse0)
   (if (or (config 'call/cc)
	   (config 'encapsulate-parts))
       (let ((module-decl (Decl-of-new-Var (gensym 'moduleVar))))
	  (set! this.part-var module-decl.var)
	  (set! this.body
		(new-node Begin
			  (list (new-node Set!
					  module-decl
					  this.body)
				(new-node Return
					  (module-decl.var.reference))))))))
