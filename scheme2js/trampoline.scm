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
	   js-interface
	   verbose)
   (export (trampoline tree::pobject)))

   
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

(define (runtime-var-ref? n)
   (and (inherits-from? n (node 'Var-ref))
	(let* ((var n.var)
	       (js-id var.js-id))
;	   (and (verbose "id: " var.id))
;	   (and (verbose "js-id: " var.js-id))
	   (and (inherits-from? var (node 'JS-Var))
		js-id
		(not var.muted?)
		(any? (lambda (p) (eq? js-id (cadr p))) *runtime-var-mapping*)))))

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
       (memq operator.var.id *higher-order-runtime*))
      ((and (inherits-from? operator (node 'Var-ref))
	    operator.var.single-value)
       (potential-tail-fun? operator.var.single-value))
      (else
       #t)))

;; only called, once we already established, that the operator is a potential
;; tail-call.
;; "certain" means, we are going to decrement the tail-counter later on.
;; "uncertain" means, that we might call a "non-tail"-function.
;;       in other words: "uncertain" means, we don't know what we are calling.
(define (certain-tail-fun? operator)
   (cond
      ((inherits-from? operator (node 'Lambda))
       #t)
      ((and (inherits-from? operator (node 'Var-ref))
	    operator.var.single-value)
       #t)
      (else
       #f)))

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
		  (not (memq n.operator.var.js-id
			     *higher-order-runtime*)))
	     n)
	    (else
	     (let* ((tmp-decl (Decl-of-new-Var (gensym 'tail)))
		    (assig (new-node Set! tmp-decl n)))
		(set! hoisted (cons assig hoisted))
		(tmp-decl.var.reference)))))

      (set! call.operands (map! hoist call.operands))
      (set! call.operator (hoist call.operator))
      (if (null? hoisted)
	  call
	  (new-node Begin (append! hoisted (list call))))))

(define-pmethod (Call-trampoline! current-fun)
   (this.traverse1! current-fun)
   (if (and this.tail?
	    (potential-tail-fun? this.operator))
       (begin
	  (set! this.tail-call? #t)
	  (and current-fun (set! current-fun.contains-tail-calls? #t))))
   (if (and this.tail-call?
	    (certain-tail-fun? this.operator))
       (set! this.certain-tail-call? #t))
   (if this.tail-call?
       (a-normal-form this)
       this))

