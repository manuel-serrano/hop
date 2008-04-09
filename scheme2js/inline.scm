(module inline
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   deep-clone
	   config
	   nodes
	   var
	   side
	   locals
	   use-count
	   nested-closures
	   fun-size
	   transform-util
	   verbose)
   (export (inline! tree::pobject)))

(define (can-be-inlined? var)
   (and (not var.imported?)
	(or (not var.exported?)
	    var.exported-as-const?)
	var.constant?
	(inherits-from? var.value (node 'Lambda))
	(not var.value.closure?)
	(or (not var.value.this-var) ;; do not inline functions that access 'this'
	    (zero? var.value.this-var.uses))))
   
(define (good-for-inlining? var nested-counter)
   (and (can-be-inlined? var)
 	(< nested-counter (config 'rec-inline-nb))
 	(or (= var.uses 1)
 	    (and (not var.value.nested-closures?)
 		 (< var.value.size (/ (config 'max-inline-size)
				      (+ nested-counter 1)))))))

(define (inline! tree)
   (if (config 'do-inlining)
       (let ((called-inline-funs? #f))
	  (verbose "inlining")
	  (side-effect tree)
	  (use-count tree)
	  (fun-size tree)
	  (when (single-use! tree)
	     (inline-funs! tree)
	     (set! called-inline-funs? #t)
	     (side-effect tree)
	     (use-count tree)
	     (fun-size tree))
	  (nested-closures tree)
	  (when (or (clone-funs tree)
		    (not called-inline-funs?))
	     (inline-funs! tree))
	  )))


;; can we move a function from its definition to its use?
(define (can-be-moved? var)
   (and (not var.extern?)
	(can-be-inlined? var)))

;; functions that are only used once are directly moved to this position
;; obvious exception: exported functions (and functions that capture at their
;; creation).
(define (single-use! tree)
   (define single-use-inline-count 0)
   
   (define-pmethod (Node-single-use! surrounding-funs)
      (this.traverse1! surrounding-funs))

   (define-pmethod (Lambda-single-use! surrounding-funs)
      (this.traverse1! (cons this surrounding-funs)))
   
   (define-pmethod (Call-single-use! surrounding-funs)
      (when (inherits-from? this.operator (node 'Var-ref))
	  (let* ((op this.operator)
		 (var op.var))
	     (if (and (= var.uses 1)
		      (can-be-moved? var)
		      ;; don't inline, if we are inside ourselves.
		      ;; in this case the function is inaccesible, but that's
		      ;; not our problem...
		      (not (memq var.value surrounding-funs)))
		 (begin
		    (set! var.inlined? #t)
		    (set! single-use-inline-count (+ single-use-inline-count 1))
		    (set! this.operator var.value)))))
      (this.traverse1! surrounding-funs))
      
   (verbose " single-use")
   (overload traverse! single-use! (Node
				    Lambda
				    Call)
	     (tree.traverse! '()))
   (> single-use-inline-count 0))

;; clones function definitions to their call-targets if they are suitable for
;; inlining.
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
		 (let* ((value var.value)
			(cloned (deep-clone value)))
		    (set! clone-funs-counter (+ clone-funs-counter 1))
		    (set! this.cloned-fun cloned)
		    (cloned.traverse (+ nested-counter 1)))))))

   (verbose " clone-funs")
   (overload traverse clone-funs (Node
				  Call)
	     (tree.traverse 0))
   (> clone-funs-counter 0))


;; if a Call has a lambda as its operator, then the lambda is inlined, thus
;; avoiding the creation of the closure.
;; Other passes in this file only move/copy lambdas to the
;; call-target. Inlining itself is always done here.
(define (inline-funs! tree)
   (define *inlined-funs* #f)

   (define-pmethod (Node-inline! label)
      (this.traverse1! label))

   (define-pmethod (Set!-inline! label)
      (when this.lvalue.var.inlined?
	 (set! this.val (new-node Const #unspecified))
	 (delete! this.lvalue.var.inlined?))
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
							  fun.vaarg?))
		 (assigs (map (lambda (p)
				 (new-node Binding (car p) (cdr p)))
			      assigs-mapping))
		 (traversed-assigs (map (lambda (node)
					   (node.traverse! label))
					assigs))
		 (return-label (new-node Label (gensym 'inlined)))
		 (return-labelled (new-node Labelled fun.body return-label))
		 (traversed-labelled (return-labelled.traverse! return-label)))
	     (set! *inlined-funs* #t)
	     (new-node Let
		       (map (lambda (decl) decl.var) fun.formals)
		       traversed-assigs
		       (if return-label.used
			   traversed-labelled
			   traversed-labelled.body)
		       'let))
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
				Set!
				Call
				Lambda
				Return)
	     (tree.traverse! #f))
   *inlined-funs*)
