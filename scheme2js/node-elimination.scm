(module node-elimination
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   verbose
	   mark-statements)
   (export (node-elimination! tree::pobject)))

(define (node-elimination! tree::pobject)
   (verbose "node-elimination")
   (overload traverse! node-elimination! (Node
					  Body
					  Let-form
					  Set!
					  Begin)
	     (tree.traverse!)))

(define-pmethod (Node-node-elimination!)
   (this.traverse0!))

;; let-form becomes a sequence ('begin') of bindings followed by the body.
(define-pmethod (Let-form-node-elimination!)
   (new-node Begin (append (map (lambda (binding)
				   (binding.traverse!))
				this.bindings)
			   (list (this.body.traverse!)))))

;; drop Body-node
(define-pmethod (Body-node-elimination!)
   (this.expr.traverse!))

;; remove x=x sets
;; leave Decl, if there was one.
(define-pmethod (Set!-node-elimination!)
   (this.traverse0!)
   (if (and (inherits-from? this.lvalue (node 'Var-ref))
	    (inherits-from? this.val (node 'Var-ref))
	    (eq? this.lvalue.var this.val.var))
       (cond
	  ((inherits-from? this.lvalue (node 'Decl))
	   this.lvalue)
	  ((inherits-from? this.val (node 'Decl))
	   this.val)
	  (else (new-node Const #unspecified)))
       this))
   
;; if Begin only contains one entry, replace it by this entry.
;; if a Begin contains another Begin merge them.
(define-pmethod (Begin-node-elimination!)
   (let ((exprs this.exprs)
	 (statement? (statement-form? this)))
      (cond
	 ((null? exprs)
	  (new-node Const #unspecified))
	 ((null? (cdr exprs))
	  ((car exprs).traverse!))
	 (else
	  (let loop ((exprs exprs))
	     (unless (null? exprs)
		(let ((expr ((car exprs).traverse!))
		      (exprs-tail (cdr exprs)))
		   (set-car! exprs expr)
		   (if (inherits-from? expr (node 'Begin))
		       ;; insert into our list.
		       (let ((other-exprs expr.exprs))
			  ;; we know there must be at least 2 elements.
			  ;; otherwise we wouldn't have gotten a 'Begin'.
			  (set-car! exprs (car other-exprs))
			  (set-cdr! exprs (cdr other-exprs))
			  (set-cdr! (last-pair other-exprs) exprs-tail)))
		   (loop exprs-tail))))
	  ;; weed out atoms or functions in non-last position (dead-code)
	  ;; a 'filter!' that ignores the last element.
	  ;; if there's only one element left, it is returned.
	  (let loop ((exprs exprs)
		     (head exprs)
		     (last #f)) ;; last-pair, that is in the 'accepted' list
	     (cond
		((null? exprs) ;; should only happen if statement
		 (cond
		    ((not last) ;; no element got through our weeding
		     (new-node Const #unspecified))
		    ((null? (cdr head)) ;; only one element got through
		     (car head))
		    (else
		     (set! this.exprs head)
		     this)))
		((and (not statement?) (null? (cdr exprs)))
		 (if last
		     (begin
			(set! this.exprs head)
			this)
		     (car exprs)))
		((or (inherits-from? (car exprs) (node 'Lambda))
		     (inherits-from? (car exprs) (node 'Const))
		     (and (inherits-from? (car exprs) (node 'Var-ref))
			  (not (inherits-from? (car exprs) (node 'Decl)))))
		 (if last
		     (begin
			(set-cdr! last (cdr exprs)) ;; remove the current el.
			(loop (cdr exprs)
			      head   ;; keep old head
			      last)) ;; keep old last
		     (loop (cdr exprs)
			   (cdr exprs) ;; head is the next element (for now)
			   #f)))
		((or (inherits-from? (car exprs) (node 'Break))
		     (inherits-from? (car exprs) (node 'Return))
		     (inherits-from? (car exprs) (node 'Tail-rec-call)))
		 ;; remove remaining els
		 (set-cdr! exprs '())
		 (if last
		     (begin
			(set! this.exprs head)
			this)
		     (car exprs)))
		(else
		 (loop (cdr exprs)
		       head          ;; keep head
		       exprs)))))))) ;; we are the last pair that got through
