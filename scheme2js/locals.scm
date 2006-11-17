(module locals
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   verbose)
   (export (locals tree::pobject collect-formals?::bool)))


(define (locals tree::pobject collect-formals?)
   (define-pmethod (Node-locals ht)
      (this.traverse1 ht))
   
   (define-pmethod (Module-locals ht)
      (let ((new-ht (make-eq-hashtable)))
	 (set! this.local-vars new-ht)
	 (this.traverse1 new-ht)))

   (define-pmethod (Lambda-locals ht)
      (let ((new-ht (make-eq-hashtable)))
	 (set! this.local-vars new-ht)
	 (if collect-formals?
	     ;; go into formals and vaarg and collect them.
	     (this.traverse1 new-ht)
	     ;; don't go into formals and vaarg.
	     (this.body.traverse new-ht))))

   (define-pmethod (Closure-ref-locals ht)
      ;; don't go into field-ref
      (this.obj-ref.traverse ht))

   (define-pmethod (Decl-locals ht)
      (hashtable-put! ht this.var #t))
   
   (verbose " locals (collect)")
   (overload traverse locals (Node
			      Module
			      Lambda
			      Closure-ref
			      Decl)
	     (tree.traverse #f)))

