(module free-vars
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   locals
	   nodes
	   verbose)
   (export (free-vars tree::pobject)))

(define *empty-hashtable* (make-eq-hashtable))

;; Every Lambda/Module receives a hashtable free-vars-ht and a flag free-vars?
;; indicating all free variables. Modules will have imported variables marked
;; as free.
;; variables that are escaping receive the flag .escapes?
(define (free-vars tree::pobject)
   (verbose " free vars")
   (locals tree)
   (overload traverse clean (Node
			     (Module Fun-clean)
			     (Lambda Fun-clean)
			     Let)
	     (tree.traverse))
   (overload traverse capt-fun (Node
				(Module Fun-capt-fun)
				(Lambda Fun-capt-fun)
				Frame-alloc
				Var-ref)
	     (tree.traverse #f (make-eq-hashtable))))

(define-pmethod (Node-clean)
   (this.traverse0))

(define-pmethod (Fun-clean)
   (delete! this.free-vars-ht)
   (delete! this.free-vars?)
   (for-each (lambda (var) (delete! var.escapes?)) this.scope-vars)
   (this.traverse0))

(define-pmethod (Let-clean)
   (for-each (lambda (var) (delete! var.escapes?)) this.scope-vars))

(define-pmethod (Node-capt-fun local-scope free-vars-ht)
   (this.traverse2 local-scope free-vars-ht))

(define-pmethod (Fun-capt-fun local-scope free-vars-ht)
   (let ((local-vars-ht this.local-vars-ht)
	 (fun-free-vars-ht (make-eq-hashtable)))
      
      ;; store all free vars in fun-free-vars-ht
      (this.traverse2 local-vars-ht fun-free-vars-ht)

      ;; fun-free-vars might contain local vars.
      ;; remove them.
      ;; This is possible, if we have nested lambdas.
      ;; In this case the "Var-ref-capt-fun" checks for local vars, but once we
      ;; got out of the nested Lambda, we still have the now local vars in it.
      (hashtable-for-each local-vars-ht
			  (lambda (var ignored)
			     (hashtable-remove! fun-free-vars-ht var)))
      
      ;; store remaining free vars
      (if (> (hashtable-size fun-free-vars-ht) 0)
	  (begin
	     (set! this.free-vars? #t)
	     (set! this.free-vars-ht fun-free-vars-ht)
	     ;; mark these vars as escaping
	     (hashtable-for-each fun-free-vars-ht
				 (lambda (var ignored)
				    (set! var.escapes? #t))))
	  (set! this.free-vars-ht *empty-hashtable*))

      ;; pass free vars to parent-fun.
      (hashtable-for-each fun-free-vars-ht
			  (lambda (key val)
			     (hashtable-put! free-vars-ht key #t)))))

(define-pmethod (Frame-alloc-capt-fun local-scope free-vars-ht)
   (this.traverse2 local-scope free-vars-ht)
   (set! this.storage-var.escapes? #t))

(define-pmethod (Var-ref-capt-fun local-scope free-vars-ht)
   (let ((var this.var))
      (unless (hashtable-get local-scope var)
	 (hashtable-put! free-vars-ht var #t))))
