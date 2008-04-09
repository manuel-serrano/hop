(module callcc
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes
	   side
	   free-vars
	   tail
	   locals
	   mark-statements
	   var
	   var-ref-util
	   verbose
	   node-elimination
	   callcc-a-normal-form
	   callcc-check-point
	   callcc-locations
	   callcc-resume-push)
   (export (call/cc-early! tree::pobject)
	   (call/cc-late! tree::pobject)
	   ))

(define (call/cc-early! tree)
   (when (config 'suspend/resume)
      (verbose "call/cc early")
      (call/cc-locations tree)
      ;; must extract call/cc calls out of Tail-calls
      (call/cc-a-normal-form! tree)
      (call/cc-check-point! tree)))
   
;; the statement-pass introduces new Set!s. We shift the call/cc information
;; down to the Set!s.
(define (call/cc-late! tree)
   (when (config 'suspend/resume)
      (verbose "call/cc late")
      (call/cc-resume-push! tree)
      (call/cc-scoping tree)
      (node-elimination! tree)
      (call/cc-ranges tree)))

;; ever call/cc-call now has a list of scopes associated. move this information
;; to the fun-node. (that's where it's needed during output)
;; We also add a finally-scopes-list to funs and whiles.
;; And we add a Call/cc-Counter-Update object, if the While needs one.
(define (call/cc-scoping tree)
   (verbose " call/cc scope assoc")
   (overload traverse scope! (Node
			     (Module Lambda-scope!)
			     Lambda
			     While
			     Call/cc-Call)
	     (tree.traverse #f)))

(define-pmethod (Node-scope! surrounding-fun)
   (this.traverse1 surrounding-fun))

;; scopes with direct-vars inside, that are marked .call/cc?
;; direct vars are stored inside the scope as ".call/cc-vars"
(define (call/cc-scopes! scopes)
   (cp-filter (lambda (scope)
		 (and scope.call/cc?
		      (let ((direct-vars (filter (lambda (var)
						    (not var.indirect?))
						 scope.vars)))
			 (set! scope.call/cc-vars direct-vars)
			 (not (null? direct-vars)))))
	      scopes))
   
;; scopes with vars inside, that need updates.
;; these vars are stored inside the scope as ".finally-vars"
(define (finally-scopes! scopes)
   (cp-filter (lambda (scope)
		 (let ((finally-vars (filter (lambda (var)
						var.needs-update?)
					     scope.vars)))
		    (set! scope.finally-vars finally-vars)
		    (not (null? finally-vars))))
	      scopes))

(define-pmethod (Lambda-scope! surrounding-fun)
   (set! this.counter 0)
   (this.traverse1 this)
   ;; just rename the counter-var
   (set! this.call/cc-nb-counters this.counter)
   (delete! this.counter)
   (set! this.call/cc-scopes (call/cc-scopes! this.contained-scopes))
   (let ((scopes-outside-whiles (filter (lambda (scope)
					      (not scope.surrounding-while))
					   this.call/cc-scopes)))
      (set! this.finally-scopes (finally-scopes! scopes-outside-whiles))))

(define-pmethod (While-scope! surrounding-fun)
   (this.traverse1 surrounding-fun)
   (set! this.call/cc-scopes (call/cc-scopes! this.contained-scopes))
   (let ((scopes-outside-nested-whiles
	  (filter (lambda (scope)
			(eq? scope.surrounding-while this))
		     this.call/cc-scopes)))
      (set! this.finally-scopes
	    (finally-scopes! scopes-outside-nested-whiles)))
   (if this.call/cc-loop-counter
       (let ((counter-update (new-node Call/cc-Counter-Update
				       this.call/cc-loop-counter)))
	  (mark-statement-form! counter-update #t)
	  (cond
	     ((inherits-from? this.body (node 'Begin))
	      (cons-set! this.body.exprs counter-update))
	     (else
	      (let ((bnode (new-node Begin (list counter-update this.body))))
		 (mark-statement-form! bnode #t)
		 (set! this.body bnode)))))))

(define-pmethod (Call/cc-Call-scope! surrounding-fun)
   (this.traverse1 surrounding-fun)
   (for-each (lambda (scope)
		(set! scope.call/cc-indices
		      (cons this.call/cc-index (or scope.call/cc-indices '())))
		(set! scope.call/cc? #t))
	     this.visible-scopes)
   (when (config 'call/cc)
      ;; add a counter-index to while-loops (if necessary)
      (let loop ((scopes this.visible-scopes))
	 (if (null? scopes)
	     'done
	     (let ((scope (car scopes)))
		(cond
		   ((not scope.surrounding-while)
		    'done)
		   (scope.surrounding-while.call/cc-loop-counter
		    ;; already has a counter-var
		    'done)
		   (else
		    (let ((counter surrounding-fun.counter)
			  (while scope.surrounding-while))
		       (set! surrounding-fun.counter (+ 1 counter))
		       (set! while.call/cc-loop-counter counter))))
		(loop (cdr scopes)))))))

(define (call/cc-ranges tree)
   (verbose " call/cc ranges")
   (overload traverse ranges (Node
			      Call/cc-Resume
			      Lambda
			      If
			      Case
			      Clause
			      Begin
			      Set!
			      Call
			      While
			      Break)
	     (set! (node 'Node).proto.default-traverse-value '())
	     (tree.traverse)
	     (delete! (node 'Node).proto.default-traverse-value)))

(define (range-merge Ls)
   (let ((ht (make-eq-hashtable)))
      (for-each (lambda (l)
		   (for-each (lambda (v)
				(hashtable-put! ht v #t))
			     l))
		Ls)
      (hashtable-key-list ht)))
   

(define (multi-traverse n children)
   (let* ((children-range (range-merge (map (lambda (n)
					       (n.traverse))
					    children))))
      (if (and (statement-form? n)
	       children-range
	       (not (null? children-range)))
	  (set! n.call/cc-range children-range))
      children-range))

(define-pmethod (Node-ranges)
   (let* ((children-range (this.traverse0)))
      (if (and (statement-form? this)
	       children-range
	       (not (null? children-range)))
	  (set! this.call/cc-range children-range))
      children-range))

(define-pmethod (Call/cc-Resume-ranges)
   (set! this.call/cc-range this.indices/vars)
   this.indices/vars)

(define-pmethod (Lambda-ranges)
   (multi-traverse this (list this.body))
   '())
   
(define-pmethod (If-ranges)
   (begin0
    (multi-traverse this (list this.test this.then this.else))
    (let ((then-range this.then.call/cc-range)
	  (else-range this.else.call/cc-range))
       (if (and then-range (not (null? then-range)))
	   (set! this.call/cc-then-range then-range))
       (if (and else-range (not (null? else-range)))
	   (set! this.call/cc-else-range else-range)))))

(define-pmethod (Case-ranges)
   (multi-traverse this (cons this.key this.clauses)))

(define-pmethod (Clause-ranges)
   (multi-traverse this (append this.consts (list this.expr))))

(define-pmethod (Begin-ranges)
   (multi-traverse this this.exprs))

(define-pmethod (Call-ranges)
   (multi-traverse this (cons this.operator this.operands)))

(define-pmethod (While-ranges)
   (multi-traverse this (list this.test this.body)))

(define-pmethod (Set!-ranges)
   (multi-traverse this (list this.lvalue this.val)))

(define-pmethod (Break-ranges)
   (if this.val
       (this.val.traverse)
       '()))
