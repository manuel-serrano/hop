;; $Id: liveness.scm 126 2006-02-22 11:22:58Z flo $
(module liveness
   (include "protobject.sch")
   (include "nodes.sch")
   (import protobject
	   nodes
	   verbose)
   (export (liveness tree::pobject)))

;; very simple liveness analysis:
;; every node represents a nesting-level.
;; we accumulate the levels (in the 'nesting'-var), and whenever a var is used
;; we define the var's range as the shared levels.
(define (liveness tree)
   (verbose "liveness")
   (overload traverse clean (Node Decl)
	     (tree.traverse))
   (overload traverse liveness (Node
				Lambda
				Var-ref)
	     (tree.traverse '()))
   (overload traverse rev-liveness (Node
				    Decl)
	     (tree.traverse)))

(define-pmethod (Node-clean)
   (delete! this.live-begin-vars)
   (delete! this.live-end-vars)
   (this.traverse0))

(define-pmethod (Decl-clean)
   (let ((var this.var))
      (delete! var.live-begin)
      (delete! var.live-end)))

(define-pmethod (Node-liveness nesting)
   (this.traverse1 (cons this nesting)))

(define-pmethod (Lambda-liveness nesting)
   (hashtable-for-each this.free-vars
		       (lambda (var ignored)
			  (delete! var.live-begin)
			  (delete! var.live-end)
			  (set! var.free? #t)))
   (pcall this Node-liveness nesting))

(define-pmethod (Var-ref-liveness nesting)
   (define (advance-list l n)
      (if (=fx n 0)
	  l
	  (advance-list (cdr l) (-fx n 1))))
   
   (let* ((var this.var)
	  (live-begin var.live-begin)
	  (live-end var.live-end))
      (cond
	 (var.captured?
	  'do-nothing)
	 (var.imported?
	  'do-nothing)
	 (var.free?
	  'do-nothing)
	 ((not live-begin)
	  (begin
	     (set! var.live-begin (cons this nesting))
	     (set! var.live-end (cons this nesting))))
	 ;; common case (same level)
	 ((eq? nesting (cdr live-begin))
	  (set! var.live-end (cons this nesting)))
	 (else
	  (let* ((begin-length (length live-begin))
		 (this-list (cons this nesting))
		 (this-length (length this-list))
		 (min-length (min begin-length this-length))
		 (shorted-begin (advance-list live-begin (-fx begin-length
							      min-length)))
		 (shorted-this (advance-list this-list (-fx this-length
							    min-length))))
	     (let loop ((s-begin shorted-begin)
			(s-end shorted-this))
		(cond
		   ((null? s-begin)
		    (error #f "Must not happen: liveness" '()))
		   ((eq? (cdr s-begin) (cdr s-end))
		    ;; this even works for 'if/then/else'
		    ;; although it isn't optimal.
		    ;; suppose x is used in the test, and
		    ;; the else branch.
		    ;; the x is not used in the 'then'-branch.
		    ;; this simple analysis marks it as used
		    ;; even in the 'then'-branch.
		    (set! var.live-begin s-begin)
		    (set! var.live-end s-end))
		   (else (loop (cdr s-begin)
			       (cdr s-end))))))))))

(define-pmethod (Node-rev-liveness)
   (this.traverse0))

(define-pmethod (Decl-rev-liveness)
   (let ((var this.var))
      (if var.live-begin
	  (let* ((begin-node (car var.live-begin))
		 (begin-node-vars begin-node.live-begin-vars)
		 (end-node (car var.live-end))
		 (end-node-vars end-node.live-end-vars))
	     ;; switch from lists to nodes, as we don't need the lists anymore, and
	     ;; this helps debugging (not, that we would need it ;)
	     (set! var.live-begin begin-node)
	     (set! var.live-end end-node)
	     ;; reverse-pointer from the begin/end-node to the var.
	     (set! begin-node.live-begin-vars
		   (cons var (or begin-node-vars '())))
	     (set! end-node.live-end-vars
		   (cons var (or end-node-vars '())))))))
