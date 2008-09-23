(module liveness
   (import nodes
	   walk
	   captured-vars
	   verbose)
   (export (liveness tree::Module)))

;; very simple liveness analysis:
;; every node represents a nesting-level.
;; we accumulate the levels (in the 'nesting'-var), and whenever a var is used
;; we define the var's range as the shared levels.
(define (liveness tree)
   (verbose "liveness")
   (captured-vars tree)
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
      (delete! var.live-begin-stack)
      (delete! var.live-end-stack)))

(define-pmethod (Node-liveness nesting)
   (this.traverse1 (cons this nesting)))

(define-pmethod (Lambda-liveness nesting)
   (hashtable-for-each this.free-vars-ht
		       (lambda (var ignored)
			  (delete! var.live-begin-stack)
			  (delete! var.live-end-stack)
			  (set! var.free? #t)))
   ;; Note: Javascript parameters are assigned from left to right.
   ;;       An unused parameter is hence only live at its declaration.
   ;;       This means: an unused parameter's name can be reused for
   ;;       the next parameter.
   (pcall this Node-liveness nesting))

(define-pmethod (Var-ref-liveness nesting)
   (let* ((var this.var)
	  (live-begin-stack var.live-begin-stack)
	  (live-end-stack var.live-end-stack))
      (cond
	 (var.extern?
	  'do-nothing)
	 (var.captured?
	  'do-nothing)
	 (var.free?
	  'do-nothing)
	 ((not live-begin-stack)
	  (begin
	     (set! var.live-begin-stack (cons this nesting))
	     (set! var.live-end-stack (cons this nesting))))
	 ;; common case (same level)
	 ((eq? nesting (cdr live-begin-stack))
	  (set! var.live-end-stack (cons this nesting)))
	 (else
	  (let* ((begin-length (length live-begin-stack))
		 (this-list (cons this nesting))
		 (this-length (length this-list))
		 (min-length (min begin-length this-length))
		 (shorted-begin (list-tail live-begin-stack
					   (-fx begin-length min-length)))
		 (shorted-this (list-tail this-list
					  (-fx this-length min-length))))
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
		    (set! var.live-begin-stack s-begin)
		    (set! var.live-end-stack s-end))
		   (else (loop (cdr s-begin)
			       (cdr s-end))))))))))

(define-pmethod (Node-rev-liveness)
   (this.traverse0))

(define-pmethod (Decl-rev-liveness)
;   (if this.done
;       (error "Decl-rev-liveness"
;	      "Recursive nodes"
;	      this.var.id)
;       (set! this.done #t))
   (let ((var this.var))
      (if var.live-begin
	  (error "Decl-rev-liveness"
		 "Recursive nodes or 2 Decls for one var: "
		 var.id))
      (if var.live-begin-stack
	  (let* ((begin-node (car var.live-begin-stack))
		 (begin-node-vars begin-node.live-begin-vars)
		 (end-node (car var.live-end-stack))
		 (end-node-vars end-node.live-end-vars))
	     ;; switch from lists to nodes, as we don't need the lists anymore, and
	     ;; this helps debugging (not, that we would need it ;)
	     (delete! var.live-begin-stack)
	     (delete! var.live-end-stack)
	     (set! var.live-begin begin-node)
	     (set! var.live-end end-node)
	     ;; reverse-pointer from the begin/end-node to the var.
	     (set! begin-node.live-begin-vars
		   (cons var (or begin-node-vars '())))
	     (set! end-node.live-end-vars
		   (cons var (or end-node-vars '())))))))
