(module tail
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   verbose)
   (export (tail-exprs tree::pobject intermediate-nodes-are-tail?::bool)))

(define *inter-tail?* #f)

;; might be called only after Node-elimination.
;; but then anytime. so don't assume nodes exist or not.
(define (tail-exprs tree intermediate-nodes-are-tail?)
   (verbose "tail")
   (set! *inter-tail?* intermediate-nodes-are-tail?)
   (overload traverse tail (Node
			    Program
			    (Part Inter-tail)
			    (Const Value-tail)
			    (Var-ref Value-tail)
			    (Scope Inter-tail)
			    Lambda
			    If
			    Case
			    Clause
			    (Set! Enclosing-tail)
			    Begin
			    Bind-exit
			    (Call Enclosing-tail)
			    (Tail-rec Inter-tail)
			    (Tail-rec-call Value-tail)
			    Return
			    (Closure-alloc Inter-tail)
			    (Label Inter-tail)
			    Break
			    (Pragma Value-tail))
	     (tree.traverse #f)))

(define-pmethod (Node-tail tail?)
   (error #f "tail. forgot node-type" this))

(define-pmethod (Value-tail tail?)
   (set! this.tail? tail?))
   
(define-pmethod (Inter-tail tail?)
   (set! this.tail? (and *inter-tail?* tail?))
   (this.traverse1 tail?))

(define-pmethod (Enclosing-tail tail?)
   (set! this.tail? tail?)
   (this.traverse1 #f))

(define-pmethod (Program-tail tail?)
   (set! this.tail? #t)
   ;; program's content is tail
   (this.traverse1 #t))

(define-pmethod (Lambda-tail tail?)
   (set! this.tail? tail?)
   ;; function's content is tail.
   (this.traverse1 #t))

(define-pmethod (If-tail tail?)
   (set! this.tail? (and *inter-tail?* tail?))
   (this.test.traverse #f)
   (this.then.traverse tail?)
   (this.else.traverse tail?))

(define-pmethod (Case-tail tail?)
   (set! this.tail? (and *inter-tail?* tail?))
   (this.key.traverse #f)
   (for-each (lambda (clause)
		(clause.traverse tail?))
	     this.clauses))

(define-pmethod (Clause-tail tail?)
   (set! this.tail? (and *inter-tail?* tail?))
   (for-each (lambda (const)
		(const.traverse #f))
	     this.consts)
   (this.expr.traverse tail?))

(define-pmethod (Begin-tail tail?)
   (set! this.tail? (and *inter-tail?* tail?))
   (let loop ((exprs this.exprs))
      (cond
	 ((null? exprs) 'do-nothing)
	 ((null? (cdr exprs))
	  ((car exprs).traverse tail?))
	 (else
	  ((car exprs).traverse #f)
	  (loop (cdr exprs))))))

(define-pmethod (Bind-exit-tail tail?)
   (set! this.tail? (and *inter-tail?* tail?))
   (this.escape.traverse #f)
   (this.body.traverse tail?))

(define-pmethod (Return-tail tail?)
   (set! this.tail? tail?)
   (this.val.traverse #t))

(define-pmethod (Break-tail tail?)
   (set! this.tail? tail?)
   (this.val.traverse this.label.tail?))
