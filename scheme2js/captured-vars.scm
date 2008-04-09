(module captured-vars
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   free-vars
	   side
	   verbose)
   (export (captured-vars tree::pobject)))

;; if a function captures a variable, it is marked as ".closure?".
;; Every variable that is captured is marked as ".captured?".
;;
;; we handle some cases, where the function is only used within the scope
;; of its variables. That is, if a function has free variables, but the
;; lifetime of the function itself is shorter than those of its free variables,
;; then the function is not considered to be a closure. (and the function is
;; not marked es .closure, nor are its free variables marked as .captured?.
(define (captured-vars tree::pobject)
   (verbose " collect captured")
   (free-vars tree)
   (side-effect tree)
   (overload traverse clean (Node
			     (Module Scope-clean)
			     (Lambda Scope-clean)
			     (Let Scope-clean)
			     (Tail-rec Scope-clean))
	     (tree.traverse))
   (overload traverse captured (Node
				Lambda
				Call
				Set!
				Frame-alloc
				Var-ref)
	     (tree.traverse)))

(define-pmethod (Node-clean)
   (this.traverse0))

(define-pmethod (Scope-clean)
   (this.traverse0)
   (delete! this.closure?) ;; only for Lambdas
   (for-each (lambda (var) (delete! var.captured?))
	     this.scope-vars))

(define (mark-closure! proc)
   (unless proc.closure? ;; already done
      (set! proc.closure? #t)
      (hashtable-for-each proc.free-vars-ht
			  (lambda (key val)
			     (set! key.captured? #t)))))

(define-pmethod (Node-captured)
   (this.traverse0))

(define-pmethod (Lambda-captured . Lno-mark?)
   (if (and (null? Lno-mark?)
	    this.free-vars?)
       (mark-closure! this))
   (this.traverse0))

;; a Call is the only place, where we allow capturing functions.
(define-pmethod (Call-captured)
   (let ((operator this.operator)
	 (operands this.operands))
      (cond
	 ((inherits-from? operator (node 'Lambda))
	  (pcall operator Lambda-captured 'no-mark))
	 ((inherits-from? operator (node 'Var-ref))
	  ;; no need to go into Var-ref. if it references a lambda, we don't
	  ;; want to know (as we allow lambda-refs in calls).
	  'done)
	 (else
	  (operator.traverse)))
      (for-each (lambda (node)
		   (node.traverse))
		operands)))

(define-pmethod (Set!-captured)
   (let ((var this.lvalue.var)
	 (val this.val))
      ;; If val is a lambda do not yet mark it as closure (if it has free
      ;; vars), but wait for its first use. (In the best case we are able to
      ;; determine that all free vars are still alive.
      (if (and var.constant?
	       (inherits-from? val (node 'Lambda)))
	  (pcall val Lambda-captured 'no-mark)
	  (val.traverse))))

(define-pmethod (Frame-alloc-captured)
   (this.traverse0)
   (set! this.storage-var.captured? #t))

(define-pmethod (Var-ref-captured)
   (let ((constant? this.var.constant?)
	 (value this.var.value))
      (if (and constant?
	       (inherits-from? value (node 'Lambda)))
	  (mark-closure! value))))
