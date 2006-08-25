(module mark-statements
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   var
	   verbose)
   (export (mark-statements tree::pobject)
	   (statement-form? o::pobject)
	   (marked-node? o::pobject)
	   (mark-node! o::pobject statement-form?)
	   (mark-statement-form! o::pobject statement-form?)))

(define (mark-statements tree::pobject)
   (verbose "  mark-statements")
   (overload traverse mark-statements (Node
				       Part
				       Lambda
				       If
				       Case
				       Begin
				       Bind-exit
				       With-handler
				       Call
				       Tail-rec
				       While
				       Tail-rec-call
				       Return
				       Closure-alloc
				       Labelled
				       Break
				       Set!)
	     (set! (node 'Node).proto.default-traverse-value #f)
	     (tree.traverse)
	     (delete! (node 'Node).proto.default-traverse-value)))

(define (mark-statement-form! o statement-form?)
   (mark-node! o statement-form?))

(define (mark-node! o statement-form?)
   (if statement-form?
       (set! o.statement-form? #t)
       (delete! o.statement-form?)))

(define (statement-form? o::pobject)
   (marked-node? o))

(define (marked-node? o)
   o.statement-form?)

(define (list-mark-statements l)
   (let loop ((l l)
	      (res #f))
      (if (null? l)
	  res
	  (loop (cdr l)
		(if ((car l).traverse)
		    #t
		    res)))))

(define-pmethod (Node-mark-statements)
   (let ((res (this.traverse0)))
      (mark-node! this res)
      res))

(define-pmethod (Part-mark-statements)
   (let* ((body-res (this.body.traverse))
	  (res (or body-res this.prefer-statement-form?)))
      (mark-node! this res)
      res))

(define-pmethod (Lambda-mark-statements)
   (this.body.traverse)
   (mark-node! this #f)
   #f)

(define-pmethod (If-mark-statements)
   (let* ((test-res (this.test.traverse))
	  (then-res (this.then.traverse))
	  (else-res (this.else.traverse))
	  (res (or test-res then-res else-res)))
      (mark-node! this res)
      res))

(define-pmethod (Case-mark-statements)
   (this.key.traverse)
   (map (lambda (node) (node.traverse)) this.clauses)
   (mark-node! this #t)
   #t)

(define-pmethod (Begin-mark-statements)
   (let ((res (list-mark-statements this.exprs)))
      (mark-node! this res)
      res))

(define-pmethod (Bind-exit-mark-statements)
   ;; don't need to go into escape and result-vars.
   (this.body.traverse)
   (this.invoc-body.traverse)
   (mark-node! this #t)
   #t)

(define-pmethod (With-handler-mark-statements)
   ;; don't need to go into exception-var
   (this.catch.traverse)
   (this.body.traverse)
   (mark-node! this #t)
   #t)

(define-pmethod (Call-mark-statements)
   (let* ((operands-tmp (list-mark-statements this.operands))
	  (operator-tmp (this.operator.traverse))
	  (res (or this.call/cc-stmt? operands-tmp operator-tmp)))
      (mark-node! this res)
      res))

(define-pmethod (Tail-rec-mark-statements)
   (this.body.traverse)
   (mark-node! this #t)
   #t)

(define-pmethod (While-mark-statements)
   (this.test.traverse)
   (this.body.traverse)
   (mark-node! this #t)
   #t)

(define-pmethod (Tail-rec-call-mark-statements)
   (mark-node! this #t)
   #t)

(define-pmethod (Return-mark-statements)
   (this.val.traverse)
   (mark-node! this #t)
   #t)

(define-pmethod (Closure-alloc-mark-statements)
   (this.body.traverse)
   (mark-node! this #t)
   #t)

(define-pmethod (Labelled-mark-statements)
   (this.body.traverse)
   (mark-node! this #t)
   #t)

(define-pmethod (Break-mark-statements)
   (this.val.traverse)
   (mark-node! this #t)
   #t)

(define-pmethod (Set!-mark-statements)
   (let ((res (or (this.val.traverse)
		  this.call/cc-stmt?)))
      (mark-node! this res)
      res))
