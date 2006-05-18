(module inline
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes
	   var
	   side
	   locals
	   use-count
	   nested-funs
	   fun-size
	   transform-util
	   verbose)
   (export (inline! tree::pobject)))

(define (can-be-inlined? var)
   (and (or (config 'inline-globals)
	    (not var.is-global?))
	var.single-value
	(inherits-from? var.single-value (node 'Lambda))
	(not var.single-value.closure?)))
   
(define (good-for-inlining? var nested-counter)
   (and (can-be-inlined? var)
	(< nested-counter 1)
	(or (= var.uses 1)
	    (and (not var.single-value.nested-funs?)
		 (< var.single-value.size 50)))))

(define (inline! tree)
   (if (config 'do-inlining)
       (let ((called-inline-funs? #f))
	  (verbose "inlining")
	  (side-effect tree)
	  (use-count tree)
	  (if (single-use! tree)
	      (begin
		 (inline-funs! tree)
		 (set! called-inline-funs? #t)
		 (side-effect tree)
		 (use-count tree)))
	  (fun-size tree)
	  (nested-funs tree)
	  (locals tree
		  #t)   ;; collect formals.
	  (if (or (clone-funs tree)
		  (not called-inline-funs?))
	      (inline-funs! tree))
	  )))

(define (single-use! tree)
   (define single-use-inline-count 0)
   
   (define-pmethod (Node-single-use!)
      (this.traverse0!))

   (define-pmethod (Set!-single-use!)
      (let ((var this.lvalue.var))
	 (if (and (= var.uses 1)
		  (can-be-inlined? var))
	     (set! this.val (new-node Const 'inlined)))
	 (this.traverse0!)))

   (define-pmethod (Call-single-use!)
      (if (inherits-from? this.operator (node 'Var-ref))
	  (let* ((op this.operator)
		 (var op.var))
	     (if (and (= var.uses 1)
		      (can-be-inlined? var))
		 (begin
		    (set! single-use-inline-count (+ single-use-inline-count 1))
		    (set! this.operator var.single-value)))))
      (this.traverse0!))
   
   (verbose " clone-funs")
   (overload traverse! single-use! (Node
				   Set!
				   Call)
	     (tree.traverse!))
   (> single-use-inline-count 0))

(define (clone-fun fun)
   (define to-clone (make-eq-hashtable))
   
   (define label-ht (make-eq-hashtable))
   (define (label-map label)
      (or (hashtable-get label-ht label)
	  (let ((label-replacement (gensym 'label)))
	     (hashtable-put! label-ht label-replacement label-replacement)
	     (hashtable-put! label-ht label label-replacement)
	     label-replacement)))
   
   (define (to-clone-add! ht)
      (hashtable-for-each ht
			  (lambda (key val)
			     (hashtable-put! to-clone key #t))))
   
   (define-pmethod (Var-deep-clone cloned-ht)
      (if (hashtable-get to-clone this)
	  (pcall this pobject-deep-clone cloned-ht)
	  this))

   (define-pmethod (Lambda-deep-clone cloned-ht)
      (to-clone-add! this.local-vars)
      (pcall this pobject-deep-clone cloned-ht))

   (define-pmethod (Call-deep-clone cloned-ht)
      (let ((cloned-fun this.cloned-fun))
	 (delete! this.cloned-fun)
	 (let ((res (pcall this pobject-deep-clone cloned-ht)))
	    (if cloned-fun
		(set! this.cloned-fun cloned-fun))
	    res)))

   (define-pmethod (Tail-rec-deep-clone cloned-ht)
      (let ((res (pcall this pobject-deep-clone cloned-ht)))
	 (set! res.label (label-map res.label))
	 res))

   (define-pmethod (Tail-rec-call-deep-clone cloned-ht)
      (let ((res (pcall this pobject-deep-clone cloned-ht)))
	 (set! res.label (label-map res.label))
	 res))

   (define-pmethod (With-handler-deep-clone cloned-ht)
      (to-clone-add! this.local-vars)
      (pcall this pobject-deep-clone cloned-ht))
   
   (define-pmethod (Label-deep-clone cloned-ht)
      (let ((res (pcall this pobject-deep-clone cloned-ht)))
	 (set! res.id (label-map res.id))
	 res))

   (define-pmethod (Break-deep-clone cloned-ht)
      (let ((res (pcall this pobject-deep-clone cloned-ht)))
	 (set! res.label (label-map res.label))
	 res))

   (to-clone-add! fun.local-vars)
   (overload deep-clone deep-clone (Var
				    Lambda
				    Call
				    Tail-rec
				    Tail-rec-call
				    With-handler
				    Label
				    Break)
	     (pcall fun pobject-deep-clone (make-eq-hashtable))))

(define (clone-funs tree)
   (define clone-funs-counter 0)
   
   (define-pmethod (Node-clone-funs nested-counter)
      (this.traverse1 nested-counter))

   (define-pmethod (Call-clone-funs nested-counter)
      (this.traverse1 nested-counter)
      (if (inherits-from? this.operator (node 'Var-ref))
	  (let* ((op this.operator)
		 (var op.var))
	     (if (good-for-inlining? var nested-counter)
		 (let* ((single-value var.single-value)
			(cloned (clone-fun single-value)))
		    (set! clone-funs-counter (+ clone-funs-counter 1))
		    (set! this.cloned-fun cloned)
		    (cloned.traverse (+ nested-counter 1)))))))

   (verbose " clone-funs")
   (overload traverse clone-funs (Node
				  Call)
	     (tree.traverse 0))
   (> clone-funs-counter 0))


(define (inline-funs! tree)
   (define *inlined-funs* #f)
   (define *id->js-var* #unspecified)

   (define-pmethod (Node-inline! label)
      (this.traverse1! label))

   (define-pmethod (Program-inline! label)
      (set! *id->js-var* this.id->js-var)
      (this.traverse1! label))

   (define-pmethod (Call-inline! label)
      (if this.cloned-fun
	  (begin
	     (set! this.operator this.cloned-fun)
	     (delete! this.cloned-fun)))
      (if (inherits-from? this.operator (node 'Lambda))
	  (let* ((fun this.operator)
		 (assigs-mapping (parameter-assig-mapping this.operands
							  fun.formals
							  fun.vaarg
							  *id->js-var*))
		 (assigs (map (lambda (p)
				 (new-node Set! (car p) (cdr p)))
			      assigs-mapping))
		 (traversed-assigs (map (lambda (node)
					   (node.traverse! label))
					assigs))
		 (label (new-node Label fun.body (gensym 'inlined)))
		 (traversed-label (label.traverse! label)))
	     (set! *inlined-funs* #t)
	     (new-node Begin (append! traversed-assigs
				      (list (if traversed-label.used
						traversed-label
						traversed-label.body)))))
	  (this.traverse1! label)))

   (define-pmethod (Lambda-inline! label)
      (this.traverse1! #f))

   (define-pmethod (Return-inline! label)
      (if label
	  (if this.tail?
	      (this.val.traverse! label)
	      (begin
		 (set! label.used #t)
		 ((new-node Break this.val label).traverse! label)))
	  (this.traverse1! label)))

   ;;=====================================================
   ;; method-start
   ;;====================================================
   (verbose " inline-funs!")
   (overload traverse! inline! (Node
				Program
				Call
				Lambda
				Return)
	     (tree.traverse! #f))
   *inlined-funs*)
