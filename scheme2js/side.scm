(module side
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes
	   var
	   verbose)
   (export (side-effect tree::pobject)))

(define (side-effect tree::pobject)
   (verbose "side-effect")
   (overload traverse clean-side (Node
				  Module
				  (Tail-rec Scope-clean-side)
				  (Let Scope-clean-side)
				  (Lambda Scope-clean-side))
	     (tree.traverse))
   (overload traverse side (Node
			    Module
			    Lambda
			    Set!
			    Tail-rec)
	     (tree.traverse)))
   
(define-pmethod (Node-clean-side)
   (this.traverse0))

(define-pmethod (Module-clean-side)
   (for-each (lambda (js-var)
		(delete! js-var.already-defined?)
		(delete! js-var.constant?)
		(delete! js-var.value))
	     this.runtime-vars)
   (for-each (lambda (js-var)
		(delete! js-var.already-defined?)
		(delete! js-var.constant?)
		(delete! js-var.value))
	     this.imported-vars)
   (for-each (lambda (js-var)
		(delete! js-var.already-defined?)
		(delete! js-var.constant?)
		(delete! js-var.value))
	     this.exported-vars)
   (for-each (lambda (js-var)
		(delete! js-var.already-defined?)
		(delete! js-var.constant?)
		(delete! js-var.value))
	     this.scope-vars)
   (this.traverse0))

(define-pmethod (Scope-clean-side)
   (this.traverse0)
   (for-each (lambda (v)
		(delete! v.already-defined?)
		(delete! v.constant?)
		(delete! v.value))
	     this.scope-vars))

(define-pmethod (Node-side)
   (this.traverse0))

(define-pmethod (Module-side)
   (for-each (lambda (js-var)
		(set! js-var.constant? (config 'runtime-is-constant))
		(set! js-var.already-defined? #t))
	     this.runtime-vars)
   (for-each (lambda (js-var)
		(set! js-var.constant? js-var.exported-as-const?)
		(set! js-var.already-defined? #t))
	     this.imported-vars)
   (for-each (lambda (js-var)
		(when (not js-var.exported-as-const?)
		   (set! js-var.constant? #f)
		   (set! js-var.already-defined? #t)))
	     this.exported-vars)
   (this.traverse0))

(define-pmethod (Lambda-side)
   (for-each (lambda (decl)
		(let ((var decl.var))
		   (set! var.already-defined? #t)
		   (set! var.constant? #t)))
	     this.formals)
   ;; revisits the formals, but doesn't make any difference.
   (this.traverse0))

(define-pmethod (Set!-side)
   (this.val.traverse)
   (let ((var this.lvalue.var))
      (if (and var.imported?
	       var.exported-as-const?)
	  (error "Set!"
		 "Imported variable is constant, and must not be modified."
		 var.id))
      (if var.already-defined?
	  (begin
	     (set! var.constant? #f)
	     (delete! var.value))
	  (begin
	     (set! var.already-defined? #t)
	     (set! var.constant? #t)
	     (set! var.value this.val)))))

(define-pmethod (Tail-rec-side)
   (for-each (lambda (init)
		(init.traverse))
	     this.inits)
   ;; we can leave the "constant?" flag, but we have to remove the
   ;; value-entry. Otherwise we might propagate the init-value.
   (for-each (lambda (var)
		(delete! var.value))
	     this.scope-vars)
   (this.body.traverse))
