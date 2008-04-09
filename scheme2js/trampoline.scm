(module trampoline
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes
	   symbol
	   var
	   tail
	   side
	   verbose
	   var-ref-util)
   (export (trampoline tree::pobject)))

;; find tail-calls, and mark them with .tail-call?
;; for our trampolines to work we must not execute any tail-call during
;; evaluation of the operator and operands. -> if any of them is not just a
;; const or a var-ref, move them in front of the call (using temporary
;; variables). As the creation of these variables creates new scopes, we have
;; to do this pass before the scope-resolution pass.
(define (trampoline tree::pobject)
   (when (config 'trampoline)
      (verbose "trampoline")
      (tail-exprs tree
		  #f) ;; intermediate nodes are not considered to be tail.
      (side-effect tree)
      (overload traverse! trampoline! (Node
				      Lambda
				      Call)
		(tree.traverse! #f))))

(define-pmethod (Node-trampoline! current-fun)
   (this.traverse1! current-fun))

(define-pmethod (Lambda-trampoline! current-fun)
   (unless (or this.trampolined? this.in-progress)
      (set! this.in-progress #t)
      (this.traverse1! this)
      (delete! this.in-progress)
      (set! this.trampolined? #t))
   this)

(define (potential-tail-fun? operator)
   (cond
      ((inherits-from? operator (node 'Lambda))
       (cond
	  (operator.in-progress
	   #t)
	  (operator.trampolined?
	   operator.contains-tail-calls?)
	  (else
	   (operator.traverse! #f)
	   operator.contains-tail-calls?)))
      ((runtime-var-ref? operator)
       #f)
      ((and (inherits-from? operator (node 'Var-ref))
	    operator.var.constant?)
       (potential-tail-fun? operator.var.value))
      (else
       #t)))

(define (a-normal-form call)
   (let ((hoisted '()))
      (define (hoist n)
	 (cond
	    ((or (inherits-from? n (node 'Const))
		 (inherits-from? n
				 (node 'Var-ref)))
	     n)
	    ((and (inherits-from? n (node 'Call))
		  (runtime-var-ref? n.operator)
		  (not (runtime-var n.operator).higher?))
	     n)
	    (else
	     ;; TODO: variables must be in scope
	     (let* ((tmp-decl (Decl-of-new-Var (gensym 'tail)))
		    (assig (new-node Set! tmp-decl n)))
		(set! hoisted (cons assig hoisted))
		(tmp-decl.var.reference)))))

      (set! call.operands (map! hoist call.operands))
      (set! call.operator (hoist call.operator))
      (if (null? hoisted)
	  call
	  (new-node Let
		    (map (lambda (assig) assig.lvalue.var) hoisted)
		    hoisted
		    call
		    'let))))

(define-pmethod (Call-trampoline! current-fun)
   (this.traverse1! current-fun)
   (if (and this.tail?
	    (potential-tail-fun? this.operator))
       (begin
	  (set! this.tail-call? #t)
	  (and current-fun (set! current-fun.contains-tail-calls? #t))))
   (if this.tail-call?
       (a-normal-form this)
       this))

