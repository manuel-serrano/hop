(module mark-statements
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   config
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
				       Module
				       Lambda
				       Let
				       Frame-push
				       If
				       Case
				       Begin
				       Call/cc-Call
				       Call
				       While
				       Tail-rec
				       Tail-call
				       Continue
				       Return
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

(define-pmethod (Module-mark-statements)
   (this.body.traverse)
   (mark-node! this #t)
   #t)

(define-pmethod (Lambda-mark-statements)
   (this.body.traverse)
   (mark-node! this #f)
   #f)

(define-pmethod (Let-mark-statements)
   (let* ((bindings-res (list-mark-statements this.bindings))
	  (body-res (this.body.traverse)))
      (mark-node! this #t)
      #t))

(define-pmethod (Frame-push-mark-statements)
   (let* ((body-tmp (this.body.traverse))
	  (res (or body-tmp
		   (config 'with-closures))))
      (mark-node! this res)
      res))

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

(define-pmethod (Call/cc-Call-mark-statements)
   (let* ((operands-tmp (list-mark-statements this.operands))
	  (operator-tmp (this.operator.traverse)))
      (mark-node! this #t)
      #t))
   
(define-pmethod (Call/cc-Resume-mark-statements)
   (mark-node! this #t)
   #t)

(define-pmethod (Call-mark-statements)
   (let* ((operands-tmp (list-mark-statements this.operands))
	  (operator-tmp (this.operator.traverse))
	  (res (or operands-tmp operator-tmp)))
      (mark-node! this res)
      res))

(define-pmethod (While-mark-statements)
   (this.test.traverse)
   (this.body.traverse)
   (mark-node! this #t)
   #t)

(define-pmethod (Tail-rec-mark-statements)
   (let* ((inits-res (list-mark-statements this.inits))
	  (body-res (this.body.traverse)))
      (mark-node! this #t)
      #t))

(define-pmethod (Tail-call-mark-statements)
   (this.traverse0)
   (mark-node! this #t)
   #t)

(define-pmethod (Continue-mark-statements)
   (mark-node! this #t)
   #t)

(define-pmethod (Return-mark-statements)
   (this.val.traverse)
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
   (let ((res (this.val.traverse)))
      (mark-node! this res)
      res))
