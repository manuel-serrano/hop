(module tail
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   verbose)
   (export (tail-exprs tree::pobject intermediate-nodes-are-tail?::bool)))

;; might be called only after Node-elimination.
;; but then anytime. so don't assume nodes exist or not.
(define (tail-exprs tree intermediate-nodes-are-tail?)
   (verbose "tail")
   (overload traverse clean (Node)
	     (tree.traverse))
   (overload traverse tail (Node
			    Program
			    (Module Inter-tail)
			    (Const Value-tail)
			    (Var-ref Value-tail)
			    (Scope Inter-tail)
			    Lambda
			    If
			    Case
			    Clause
			    (Set! Enclosing-tail)
			    Begin
			    (Call Enclosing-tail)
			    (Tail-rec Inter-tail)
			    (While Inter-tail)
			    (Tail-rec-call Value-tail)
			    Return
			    (Closure-alloc Value-tail)
			    Closure-use
			    (Closure-ref Value-tail)
			    Labelled
			    Break
			    (Pragma Value-tail))
	     (tree.traverse #f intermediate-nodes-are-tail?)))

(define-pmethod (Node-clean)
   (delete! this.tail?)
   (this.traverse0))

(define-pmethod (Node-tail tail? inter-tail?)
   (error #f "tail. forgot node-type" this))

(define-pmethod (Value-tail tail? inter-tail?)
   (set! this.tail? tail?))
   
(define-pmethod (Inter-tail tail? inter-tail?)
   (set! this.tail? (and inter-tail? tail?))
   (this.traverse2 tail? inter-tail?))

(define-pmethod (Enclosing-tail tail? inter-tail?)
   (set! this.tail? tail?)
   (this.traverse2 #f inter-tail?))

(define-pmethod (Program-tail tail? inter-tail?)
   (set! this.tail? #t)
   ;; program's content is tail
   (this.traverse2 #t inter-tail?))

(define-pmethod (Lambda-tail tail? inter-tail?)
   (set! this.tail? tail?)
   ;; function's content is tail.
   (this.traverse2 #t inter-tail?))

(define-pmethod (If-tail tail? inter-tail?)
   (set! this.tail? (and inter-tail? tail?))
   (this.test.traverse #f inter-tail?)
   (this.then.traverse tail? inter-tail?)
   (this.else.traverse tail? inter-tail?))

(define-pmethod (Case-tail tail? inter-tail?)
   (set! this.tail? (and inter-tail? tail?))
   (this.key.traverse #f inter-tail?)
   (for-each (lambda (clause)
		(clause.traverse tail? inter-tail?))
	     this.clauses))

(define-pmethod (Clause-tail tail? inter-tail?)
   (set! this.tail? (and inter-tail? tail?))
   (for-each (lambda (const)
		(const.traverse #f inter-tail?))
	     this.consts)
   (this.expr.traverse tail? inter-tail?))

(define-pmethod (Begin-tail tail? inter-tail?)
   (set! this.tail? (and inter-tail? tail?))
   (let loop ((exprs this.exprs))
      (cond
	 ((null? exprs) 'do-nothing)
	 ((null? (cdr exprs))
	  ((car exprs).traverse tail? inter-tail?))
	 (else
	  ((car exprs).traverse #f inter-tail?)
	  (loop (cdr exprs))))))

(define-pmethod (Return-tail tail? inter-tail?)
   (set! this.tail? tail?)
   (this.val.traverse #t inter-tail?))

(define-pmethod (Closure-use-tail tail? inter-tail?)
   (set! this.tail? (and inter-tail? tail?))
   (this.body.traverse tail? inter-tail?))

(define-pmethod (Labelled-tail tail? inter-tail?)
   (set! this.tail? (and inter-tail? tail?))
   (set! this.break-tail? tail?)
   (this.traverse2 tail? inter-tail?)
   (delete! this.break-tail?))

(define-pmethod (Break-tail tail? inter-tail?)
   (set! this.tail? tail?)
   (this.val.traverse this.labelled.break-tail? inter-tail?))
