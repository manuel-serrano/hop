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
				  (Module Scope-clean-uses)
				  (Lambda Scope-clean-uses)
				  (Let Scope-clean-uses)
				  (Tail-rec Scope-clean-uses))
	     (tree.traverse))
   (overload traverse use-count (Node
				 Module
				 Lambda
				 Var-ref
				 Set!
				 Frame-alloc)
	     (tree.traverse)))

(define-pmethod (Node-clean-uses)
   (this.traverse0))

(define-pmethod (Scope-clean-uses)
   (this.traverse0)
   (when this.this-var
      (delete! this.this-var.uses))
   (for-each (lambda (var) (delete! var.uses))
	     this.scope-vars))


(define-pmethod (Node-use-count)
   (this.traverse0))

(define-pmethod (Module-use-count)
   (when this.this-var (set! this.this-var.uses 0))
   (this.traverse0))

(define-pmethod (Lambda-use-count)
   ;; don't go into formals (that's not a use, but the initial set...)
   (for-each (lambda (decl)
		(set! decl.var.uses 0))
	     this.formals)
   (when this.this-var (set! this.this-var.uses 0))
   (this.body.traverse))

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

(define-pmethod (Frame-alloc-use-count)
   (let ((var this.storage-var))
      (if var.uses
	  (set! var.uses (+ var.uses 1))
	  (set! var.uses 1))))
