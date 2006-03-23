(module inline
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   var
	   side
	   transform-util
	   verbose)
   (export (inline! tree::pobject)
	   *inline-globals?*))

(define *inline-globals?* #f)

(define *second-pass* #t)
(define *inlined-funs* #f)

(define *max-uses* 99999)

(define (inline! tree)
   (verbose "inlining")
   (constant-propagation! tree)
   (clean! tree)
   (inline-funs! tree)
   (if (and *inlined-funs*
	    *second-pass*)
       (begin
	  (constant-propagation! tree)
	  (clean! tree))))

;; currently only single-assig propagation:
;;  if a value is assigned only once, we can safely propagate this value.
(define (constant-propagation! tree)
   (verbose " constant-propagation")
   (side-effect tree)
   ;; we propagate functions only, if they are used once.
   (use-count tree)
   (propagate! tree))

(define (use-count tree)
   (overload traverse use-count (Node
				 Var-ref
				 Set!)
	     (tree.traverse)))

(define-pmethod (Node-use-count)
   (this.traverse0))

(define-pmethod (Var-ref-use-count)
   (let ((var this.var))
      (if var.uses
	  (set! var.uses (+ var.uses 1))
	  (set! var.uses 1))))

;; don't count lvalue of 'set!'s
(define-pmethod (Set!-use-count)
   (this.val.traverse))


(define (propagate! tree)
   (verbose " propagate")
   (overload traverse! propagate! (Node
				   Var-ref
				   Call
				   Set!)
	     (tree.traverse!)))

(define-pmethod (Node-propagate!)
   (this.traverse0!))

(define-pmethod (Var-ref-propagate!)
   (let ((single-value this.var.single-value))
      (if (and single-value
	       (inherits-from? single-value Const)
	       (or *inline-globals?*
		   (not this.var.is-global?)))
	  (begin
	     (set! this.var.inlined? #t)
	     (new Const single-value.value))
	  this)))

(define-pmethod (Call-propagate!)
   (let ((op this.operator))
      (if (inherits-from? op Var-ref)
	  (let* ((var op.var)
		 (single-value var.single-value))
	     (if (and single-value
		      (inherits-from? single-value Lambda)
		      (not single-value.closure?)
		      (eq? var.uses 1))
		 (begin
		    (set! this.operator op.var.single-value)
		    (set! op.var.inlined? #t)
		    (pcall this Call-propagate!))
		 (this.traverse0!)))
	  (this.traverse0!))))

(define-pmethod (Set!-propagate!)
   (set! this.val (this.val.traverse!))
   this)


(define (clean! tree)
   (verbose " clean")
   (overload traverse! clean! (Node
			       Set!)
	     (tree.traverse!)))

(define-pmethod (Node-clean!)
   (this.traverse0!))

(define-pmethod (Set!-clean!)
   (let ((lvar this.lvalue.var))
      (cond
	 (lvar.inlined?
	  (new Const #unspecified))
	 ((and (not lvar.uses)
	       (not (inherits-from? lvar JS-Var))
	       (not lvar.is-global?))
	  (this.val.traverse!))
	 (else (this.traverse0!)))))

(define (inline-funs! tree)
   (verbose " inline-funs!")
   (overload traverse! inline! (Node
				Call
				Lambda
				Return)
	     (tree.traverse! #f)))

(define-pmethod (Node-inline! label)
   (this.traverse1! label))

(define-pmethod (Call-inline! label)
   (let ((op this.operator))
      (if (inherits-from? op Lambda)
	  (let* ((fun op)
		 (assigs (parameter-assigs this.operands
					   fun.formals
					   fun.vaarg
					   #f)) ;; don't take reference
		 (traversed-assigs (map (lambda (node)
					   (node.traverse! label))
					assigs))
		 (label (new Label fun.body (gensym 'inlined)))
		 (traversed-label (label.traverse! label)))
	     (set! *inlined-funs* #t)
	     (new Begin (append! traversed-assigs
				 (list (if traversed-label.used
					   traversed-label
					   traversed-label.body)))))
	  (this.traverse1! label))))

(define-pmethod (Lambda-inline! label)
   (this.traverse1! #f))

(define-pmethod (Return-inline! label)
   (if label
       (if this.tail?
	   (this.val.traverse! label)
	   (begin
	      (set! label.used #t)
	      ((new Break this.val label).traverse! label)))
       (this.traverse1! label)))
