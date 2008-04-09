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

;; Variables with use-count 0 are not used, and can be removed.
;; Attention: external variables are excempted from this rule.
(define (rm-unused-vars! tree)
   (verbose " removing unused vars")
   (use-count tree)
   (overload traverse! rm! (Node
			    Set!
			    Let)
	     (tree.traverse!)))


(define-pmethod (Node-rm!)
   (this.traverse0!))

(define-pmethod (Set!-rm!)
   (if this.lvalue.var.remove?
       (this.val.traverse!)
       (this.traverse0!)))

(define-pmethod (Let-rm!)
   (set! this.scope-vars (filter! (lambda (var)
				     (if (zero? var.uses)
					 (begin (set! var.remove? #t)
						#f)
					 #t))
				  this.scope-vars))
   (this.traverse0!))
