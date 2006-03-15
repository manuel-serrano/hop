;; $Id: node-elimination.scm 126 2006-02-22 11:22:58Z flo $
(module node-elimination
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   verbose)
   (export (node-elimination! tree::pobject)))

(define (node-elimination! tree::pobject)
   (verbose "node-elimination")
   (overload traverse! node-elimination! (Node
					  Body
					  Let-form
					  Begin)
	     (tree.traverse!)))

(define-pmethod (Node-node-elimination!)
   (this.traverse0!))

;; let-form becomes a suite ('begin') of bindings followed by the body.
(define-pmethod (Let-form-node-elimination!)
   (new Begin (append (map (lambda (binding)
			      (binding.traverse!))
			   this.bindings)
		      (list (this.body.traverse!)))))

;; drop Body-node
(define-pmethod (Body-node-elimination!)
   (this.expr.traverse!))

;; if Begin only contains one entry, replace it by this entry.
;; if a Begin contains another Begin merge them.
(define-pmethod (Begin-node-elimination!)
   (let ((exprs this.exprs))
      (cond
	 ((null? exprs)
	  (new Const #unspecified))
	 ((null? (cdr exprs))
	  ((car exprs).traverse!))
	 (else
	  (let loop ((exprs exprs))
	     (unless (null? exprs)
		(let ((expr ((car exprs).traverse!))
		      (exprs-tail (cdr exprs)))
		   (set-car! exprs expr)
		   (if (inherits-from? expr Begin)
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
		((null? exprs) ;; should never happen
		 (new Const #unspecified))
		((null? (cdr exprs))
		 (if last
		     (begin
			(set! this.exprs head)
			this)
		     (car exprs)))
		((or (inherits-from? (car exprs) Lambda)
		     (inherits-from? (car exprs) Const)
		     (and (inherits-from? (car exprs) Var-ref)
			  (not (inherits-from? (car exprs) Decl))))
		 (if last
		     (begin
			(set-cdr! last (cdr exprs)) ;; remove the current el.
			(loop (cdr exprs)
			      head   ;; keep old head
			      last)) ;; keep old last
		     (loop (cdr exprs)
			   (cdr exprs) ;; head is the next element (for now)
			   #f)))
		(else
		 (loop (cdr exprs)
		       head          ;; keep head
		       exprs)))))))) ;; we are the last pair that got through
