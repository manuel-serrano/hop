(module while
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   mark-statements
	   config
	   nodes
	   var
	   tail
	   verbose)
   (export (while! tree::pobject)))

(define (while! tree)
   (when (config 'while)
      (verbose "while")
      (gather-info tree)
      (mark-statements tree)
      (apply-patterns! tree)
      (finish-transformation! tree)
      ))

(define (gather-info tree)
   (overload traverse gather (Node
			      Scope
			      If
			      Case
			      Tail-rec
			      Set!
			      Begin
			      Call
			      Tail-rec-call)
	     (set! (node 'Node).proto.default-traverse-value '())
	     (tree.traverse #f)
	     (delete! (node 'Node).proto.default-traverse-value)))

(define (union . Lls)
   (let ((ht (make-eq-hashtable)))
      (for-each (lambda (l)
		   (for-each (lambda (x)
				(hashtable-put! ht x #t))
			     l))
		Lls)
      (hashtable-key-list ht)))

(define-pmethod (Node-gather surrounding-tail-rec)
   (this.traverse1 surrounding-tail-rec))

(define-pmethod (Scope-gather surrounding-tail-rec)
   (this.traverse1 #f)
   '())

(define-pmethod (If-gather surrounding-tail-rec)
   (let ((test-tr-calls (this.test.traverse surrounding-tail-rec))
	 (then-tr-calls (this.then.traverse surrounding-tail-rec))
	 (else-tr-calls (this.else.traverse surrounding-tail-rec)))
      (set! this.test-tail-rec-calls test-tr-calls)
      (set! this.then-tail-rec-calls then-tr-calls)
      (set! this.else-tail-rec-calls else-tr-calls)
      (union test-tr-calls then-tr-calls else-tr-calls)))

(define-pmethod (Case-gather surrounding-tail-rec)
   (apply union (map (lambda (n) (n.traverse surrounding-tail-rec))
		     this.clauses)))

(define-pmethod (Tail-rec-gather surrounding-tail-rec)
   (this.traverse1 this))

(define-pmethod (Set!-gather surrounding-tail-rec)
   ;; don't need to visit the lvalue
   (this.val.traverse surrounding-tail-rec))

(define-pmethod (Begin-gather surrounding-tail-rec)
   (apply union (map (lambda (n) (n.traverse surrounding-tail-rec))
		     this.exprs)))

(define-pmethod (Call-gather surrounding-tail-rec)
   (apply union (map (lambda (n) (n.traverse surrounding-tail-rec))
		     (cons this.operator this.operands))))

(define-pmethod (Tail-rec-call-gather surrounding-tail-rec)
   (list this.label))

(define (apply-patterns! tree)
   (define *id->js-var* #unspecified)

   (define-pmethod (Node-apply-patterns!)
      (this.traverse0!))

   (define-pmethod (Program-apply-patterns!)
      (set! *id->js-var* this.id->js-var)
      (this.traverse0!))

   (define-pmethod (Tail-rec-apply-patterns!)
      (define (apply-while-pattern! body tail-rec-label then-continue?)
	 (let* ((while (new-node While
				 (if then-continue?
				     body.test
				     (new-node Call
					       ((*id->js-var* 'not).reference)
					       (list body.test)))
				 (if then-continue?
				     body.then
				     body.else)))
		(bnode (new-node Begin
				 (list while (if then-continue?
						 body.else
						 body.then))))
		(break-labelled (new-node Labelled bnode (gensym 'break))))
	    (set! while.break-labelled break-labelled)
	    (set! while.continue-label this.label)
	    break-labelled))

      (this.traverse0!)
      (let ((body this.body)
	    (label this.label))
	 (if (and (not this.contains-closure-allocs?)
		  (inherits-from? body (node 'If))
		  (not (statement-form? body.test)))
	     (let* ((test-trs body.test-tail-rec-calls)
		    (then-trs body.then-tail-rec-calls)
		    (else-trs body.else-tail-rec-calls)
		    (test-continue? (memq label test-trs))
		    (then-continue? (memq label then-trs))
		    (else-continue? (memq label else-trs)))
		(cond
		   ((not (or test-continue? then-continue? else-continue?))
		    ;; should never happen: the loop is never used
		    ;; just return the If
		    body)
		   ((or (and then-continue? else-continue?)
			test-continue?)
		    ;; both branches may recursively call continue
		    ;; or the test calls continue
		    ;; we don't know how to optimize this.
		    this)
		   ((or then-continue? else-continue?) ;; not both.
		    ;; then-branch calls continue, but else-branch doesn't.
		    ;; or else-branch calls continue, but not then-branch.
		    (apply-while-pattern! body this.label then-continue?))
		   (else
		    this)))
	     this)))

   (overload traverse! apply-patterns! (Node
					Program
					Tail-rec)
	     (tree.traverse!)))

;; n.while-tail is true, if n is a tail-node within the enclosing while.
(define (while-tail tree)
   (verbose " while-tail")
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
			    While
			    (Tail-rec-call Value-tail)
			    Return
			    (Closure-alloc Value-tail)
			    Closure-use
			    (Closure-ref Value-tail)
			    (Labelled Inter-tail)
			    Break
			    (Pragma Value-tail))
	     (tree.traverse #f)))

(define-pmethod (Node-tail tail?)
   (error #f "tail. forgot node-type" this))

(define-pmethod (Value-tail tail?)
   (set! this.while-tail? tail?))
   
(define-pmethod (Inter-tail tail?)
   (set! this.while-tail? #f)
   (this.traverse1 tail?))

(define-pmethod (Enclosing-tail tail?)
   (set! this.while-tail? tail?)
   (this.traverse1 #f))

(define-pmethod (Program-tail tail?)
   (set! this.while-tail? #f)
   (this.traverse1 #f))

(define-pmethod (Lambda-tail tail?)
   (set! this.while-tail? tail?)
   (this.traverse1 #f))

(define-pmethod (If-tail tail?)
   (set! this.while-tail? #f)
   (this.test.traverse #f)
   (this.then.traverse tail?)
   (this.else.traverse tail?))

(define-pmethod (Case-tail tail?)
   (set! this.while-tail? #f)
   (this.key.traverse #f)
   (for-each (lambda (clause)
		(clause.traverse tail?))
	     this.clauses))

(define-pmethod (Clause-tail tail?)
   (set! this.while-tail? #f)
   (for-each (lambda (const)
		(const.traverse #f))
	     this.consts)
   (this.expr.traverse tail?))

(define-pmethod (Begin-tail tail?)
   (set! this.while-tail? #f)
   (let loop ((exprs this.exprs))
      (cond
	 ((null? exprs) 'do-nothing)
	 ((null? (cdr exprs))
	  ((car exprs).traverse tail?))
	 (else
	  ((car exprs).traverse #f)
	  (loop (cdr exprs))))))

(define-pmethod (Return-tail tail?)
   (set! this.while-tail? #f)
   (this.val.traverse #f))

(define-pmethod (Closure-use-tail tail?)
   (set! this.while-tail? #f)
   (this.body.traverse tail?))

(define-pmethod (Break-tail tail?)
   (set! this.while-tail? #f)
   (this.val.traverse #f))

(define-pmethod (While-tail tail?)
   (set! this.while-tail? #f)
   (this.test.traverse #f)
   (this.body.traverse #t))

(define (finish-transformation! tree)
   (while-tail tree)
   (overload traverse! finish! (Node
				While
				Tail-rec
				Tail-rec-call)
	     (tree.traverse! #f #f)))

(define-pmethod (Node-finish! continue-label break-labelled)
   (let ((this-while-tail? this.while-tail?))
      (delete! this.while-tail?)
      (if (and this-while-tail?
	       break-labelled)
	  (new-node Break (this.traverse2! #f #f) break-labelled)
	  (this.traverse2! continue-label break-labelled))))

(define-pmethod (While-finish! continue-label break-labelled)
   (delete! this.while-tail?)
   (this.traverse2! this.continue-label this.break-labelled))

(define-pmethod (Tail-rec-finish! continue-label break-labelled)
   (delete! this.while-tail?)
   (this.traverse2! #f #f))

(define-pmethod (Tail-rec-call-finish! continue-label break-labelled)
   (delete! this.while-tail?)
   (if (and continue-label
	    (eq? this.label continue-label))
       (new-node Const #unspecified)
       this))
