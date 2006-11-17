(module free-vars
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   verbose)
   (export (free-vars tree::pobject)))

(define *empty-hashtable* (make-eq-hashtable))

(define (free-vars tree::pobject)
   (verbose " free vars")
   (overload traverse clean (Node
			     (Module Fun-clean)
			     (Lambda Fun-clean)
			     Var-ref)
	     (tree.traverse))
   (overload traverse capt-fun (Node
				(Module Fun-capt-fun)
				(Lambda Fun-capt-fun)
				Var-ref
				Closure-ref)
	     (tree.traverse #f (make-eq-hashtable))))

(define-pmethod (Node-clean)
   (this.traverse0))

(define-pmethod (Fun-clean)
   (delete! this.free-vars)
   (delete! this.free-vars?)
   (this.traverse0))

(define-pmethod (Var-ref-clean)
   (delete! this.var.escapes?))

(define-pmethod (Node-capt-fun local-scope free-vars)
   (this.traverse2 local-scope free-vars))

(define-pmethod (Fun-capt-fun local-scope free-vars)
   (let ((local-vars this.local-vars)
	 (fun-free-vars (make-eq-hashtable)))
      
      ;; store all free vars in fun-free-vars
      (this.traverse2 local-vars fun-free-vars)

      ;; fun-free-vars might contain local vars.
      ;; remove them.
      ;; This is possible, if we have nested lambdas.
      ;; In this case the "Var-ref-capt-fun" checks for local vars, but once we
      ;; got out of the nested Lambda, we still have the now local vars in it.
      (hashtable-for-each local-vars
			  (lambda (var ignored)
			     (hashtable-remove! fun-free-vars var)))
      
      ;; store remaining free vars
      (if (> (hashtable-size fun-free-vars) 0)
	  (begin
	     (set! this.free-vars? #t)
	     (set! this.free-vars fun-free-vars)
	     ;; mark these vars as escaping
	     (hashtable-for-each fun-free-vars
				 (lambda (var ignored)
				    (set! var.escapes? #t))))
	  (set! this.free-vars *empty-hashtable*))

      ;; pass free vars to parent-fun.
      (hashtable-for-each fun-free-vars
			  (lambda (key val)
			     (hashtable-put! free-vars key #t)))))

(define-pmethod (Var-ref-capt-fun local-scope free-vars)
   (let ((var this.var))
      (unless (hashtable-get local-scope var)
	 (hashtable-put! free-vars var #t))))

(define-pmethod (Closure-ref-capt-fun local-scope free-vars)
   (let ((obj-var this.var.obj))
      (unless (hashtable-get local-scope obj-var)
	 (hashtable-put! free-vars obj-var #t))))
