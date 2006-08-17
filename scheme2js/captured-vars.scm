(module captured-vars
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   verbose)
   (export (captured-vars tree::pobject)))

;; free-vars and locals must be executed before using captured-vars

;; assigns a "captured-vars" hashtable to each function.
;; if a function captures a variable, it is marked as ".closure?" too.
;; we handle the special case, where the function is only used within the scope
;; of its variables. That is, if a function has free variables, but the
;; lifetime of the function itself is shorter than those of its free variables,
;; the function is not considered to be a closure (and the '.captured-vars'
;; would be empty.
(define (captured-vars tree::pobject)
   (verbose " collect captured")
   (overload traverse cc (Node
			  Program
			  (Part Scope-cc)
			  Lambda
			  Call
			  Set!
			  (With-handler Scope-cc)
			  Var-ref)
	     (tree.traverse '())))

(define (mark-closure! proc)
   (unless proc.closure? ;; already done
      (set! proc.closure? #t)
      (let ((captured-vars (make-eq-hashtable)))
	 (set! proc.captured-vars captured-vars)
	 (hashtable-for-each proc.free-vars
			     (lambda (key val)
				(hashtable-put! captured-vars key #t)
				(set! key.captured? #t))))))

(define (in-visible-vars var visible-vars)
   (any? (lambda (ht) (hashtable-contains? ht var))
	 visible-vars))

(define (indirect-fun-call proc-var-ref visible-vars)
   (let ((single-val proc-var-ref.var.single-value))
      (if (and single-val
	       (inherits-from? single-val (node 'Lambda)))
	  (let* ((proc single-val)
		 (free-vars proc.free-vars)
		 (captured-vars (make-eq-hashtable)))
	     (hashtable-for-each free-vars
				 (lambda (key val)
				    (if (not (in-visible-vars key
							      visible-vars))
					(begin
					   (hashtable-put! captured-vars key #t)
					   (set! key.captured? #t)))))

	     (if (> (hashtable-size captured-vars) 0)
		 (begin
		    (set! proc.closure? #t)
		    (set! proc.captured-vars captured-vars)))))))

(define-pmethod (Node-cc visible-vars)
   (this.traverse1 visible-vars))

(define-pmethod (Program-cc visible-vars)
   (let ((imported-ht (make-eq-hashtable)))
      (for-each (lambda (var)
		   (hashtable-put! imported-ht var #t))
		this.imported)
      (this.traverse1 (cons imported-ht visible-vars))))

(define-pmethod (Lambda-cc visible-vars)
   (if this.free-vars?
       (mark-closure! this))
   (this.traverse1 (cons this.local-vars visible-vars))
   (let ((escaping-locals (make-eq-hashtable)))
      (hashtable-for-each this.local-vars
			  (lambda (var val)
			     (if var.captured?
				 (hashtable-put! escaping-locals var #t))))
      (set! this.escaping-locals escaping-locals)))

(define-pmethod (Scope-cc visible-vars)
   (this.traverse1 (cons this.local-vars visible-vars)))

;; this is the only place, where we allow capturing functions.
(define-pmethod (Call-cc visible-vars)
   (let ((operator this.operator)
	 (operands this.operands))
      (cond
	 ((inherits-from? operator (node 'Lambda))
	      (pcall operator Scope-cc visible-vars))
	 ((inherits-from? operator (node 'Var-ref))
	  (indirect-fun-call operator visible-vars))
	 (else
	  (operator.traverse visible-vars)))
      (for-each (lambda (node)
		   (node.traverse visible-vars))
		operands)))

(define-pmethod (Set!-cc visible-vars)
   (let ((var this.lvalue.var)
	 (val this.val))
      ;; if this is the single assignment, we can ignore it.
      (if (and var.single-value
	       (inherits-from? val (node 'Lambda)))
	  (pcall val Scope-cc visible-vars)
	  (val.traverse visible-vars))))

(define-pmethod (Var-ref-cc visible-vars)
   (let ((single-val this.var.single-value))
      (if (and single-val
	       (inherits-from? single-val (node 'Lambda)))
	  (mark-closure! single-val))))


