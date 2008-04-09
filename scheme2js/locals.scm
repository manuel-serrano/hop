(module locals
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   verbose)
   (export (locals tree::pobject)))


;; Every function (and the Module too) receives a hashtable 'local-vars-ht'
;; containing all local variables. This function does not take into account
;; storage objects introduced by Scopes.
;; Variables of Scopes are considered to be locals, and are hence in the
;; hashtable.
(define (locals tree::pobject)
   (verbose " locals (collect)")
   (overload traverse locals (Node
			      (Module Lambda-locals)
			      Lambda
			      (Tail-rec Scope-locals)
			      (While Scope-locals)
			      (Let Scope-locals))
	     (tree.traverse #f)))

(define-pmethod (Node-locals ht)
   (this.traverse1 ht))

(define-pmethod (Lambda-locals ht)
   (let ((new-ht (make-eq-hashtable)))
      (set! this.local-vars-ht new-ht)
      (for-each (lambda (var)
		   (hashtable-put! new-ht var #t))
		this.scope-vars)
      (this.traverse1 new-ht)))

(define-pmethod (Scope-locals ht)
   (this.traverse1 ht)
   (for-each (lambda (var)
		(hashtable-put! ht var #t))
	     this.scope-vars))
