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
					  Module
					  Set!
					  Begin
					  Labelled
					  Break
					  Let
					  )
	     (tree.traverse! #f)))

;; last? is a hack to see, if the element is the last element in a Module.
;; In this case it must not be removed (this is especially important in the
;; Begin-expression, which is the only one looking for it.

(define-pmethod (Node-node-elimination! last?)
   (this.traverse1! last?))

(define-pmethod (Module-node-elimination! last?)
   (this.traverse1! #t))

;; drop Body-node
(define-pmethod (Body-node-elimination! last?)
   (this.expr.traverse! last?))

;; remove x=x sets
(define-pmethod (Set!-node-elimination! last?)
   (this.traverse1! #f)
   (if (and (inherits-from? this.lvalue (node 'Var-ref))
	    (inherits-from? this.val (node 'Var-ref))
	    (eq? this.lvalue.var this.val.var))
       (let ((const-node (new-node Const #unspecified)))
	  (when (statement-form? this) (mark-statement-form! const-node #t))
	  const-node)
       this))
   
;; if Begin only contains one entry, replace it by this entry.
;; if a Begin contains another Begin merge them.
(define-pmethod (Begin-node-elimination! last-for-module?)
   (let ((exprs this.exprs)
	 (statement? (statement-form? this)))
      (cond
	 ((null? exprs)
	  (let ((const-node (new-node Const #unspecified)))
	     (when (statement-form? this) (mark-statement-form! const-node #t))
	     const-node))
	 ((null? (cdr exprs))
	  ((car exprs).traverse! last-for-module?))
	 (else
	  (let loop ((exprs exprs))
	     (unless (null? exprs)
		(let ((expr ((car exprs).traverse! (and last-for-module?
							(null? (cdr exprs)))))
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
		((null? exprs) ;; happens only if the last element is not
		               ;; always copied
		 (cond
		    ((not last) ;; no element got through our weeding
		     (let ((const-node (new-node Const #unspecified)))
			(when (statement-form? this)
			   (mark-statement-form! const-node #t))
			const-node))
		    ((null? (cdr head)) ;; only one element got through
		     (car head))
		    (else
		     (set! this.exprs head)
		     this)))
		((and (null? (cdr exprs))
		      (or last-for-module?
			  (not statement?)))
		 (if last
		     (begin
			(set! this.exprs head)
			this)
		     (car exprs)))
		((or (inherits-from? (car exprs) (node 'Lambda))
		     (inherits-from? (car exprs) (node 'Const))
		     (inherits-from? (car exprs) (node 'Var-ref)))
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
		     (inherits-from? (car exprs) (node 'Continue)))
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

;; remove unused labels.
(define-pmethod (Labelled-node-elimination! last?)
   (delete! this.label.used?)
   (this.traverse1! last?)
   (if (not this.label.used?)
       this.body
       (begin
	  (delete! this.label.used?)
	  this)))

(define-pmethod (Break-node-elimination! last?)
   (set! this.label.used? #t)
   (this.traverse1! last?))

;; remove empty Lets
(define-pmethod (Let-node-elimination! last?)
   (if (null? this.scope-vars)
       (let ((bnode (new-node Begin
			      (append this.bindings (list this.body)))))
	  (bnode.traverse! last?))
       (this.traverse1! last?)))
