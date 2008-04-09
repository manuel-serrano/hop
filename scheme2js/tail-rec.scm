(module tail-rec
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes
	   symbol
	   var
	   var-ref-util
	   transform-util
	   side
	   tail
	   loop-updates
	   captured-vars
	   verbose)
   (export (tail-rec! tree::pobject)))

;; (define (foo x y z)
;;     bar
;;     (foo x_up y_up z_up))
;;
;; is transformed into:
;;
;; (define (foo x_ y_ z_)
;;    (tail-rec (x y z)
;;              (x_ y_ z_))
;;       bar
;;       (tail-call (x_up y_up z_up))))
;;
;; The meaning of tail-rec being: (let loop ((x x_) (y y_) (z z_))
;; The meaning of tail-call: (loop x_up y_up z_up)
(define (tail-rec! tree::pobject)
   (verbose "tail-rec")
   (when (config 'optimize-tail-rec)
      (tail-exprs tree
		  #f) ;; intermediate nodes are not considered to be tail.
      (side-effect tree)
      (overload traverse! tail-rec! (Node
				     Lambda
				     Call)
		(tree.traverse! #f))))

(define-pmethod (Node-tail-rec! current-fun)
   (this.traverse1! current-fun))

(define-pmethod (Call-tail-rec! current-fun)
   (this.traverse1! current-fun)
   (let ((op this.operator))
      (if (and this.tail?
	       current-fun
	       (config 'optimize-tail-rec)
	       (inherits-from? op (node 'Var-ref))
	       op.var.constant?
	       (eq? op.var.value current-fun))
	  (let* ((assig-mapping (parameter-assig-mapping this.operands
							 current-fun.formals
							 current-fun.vaarg?))
		 (updates (map! cdr assig-mapping))
		 (tail-rec-label (or current-fun.tail-rec-label
				     (new-node Label (gensym 'continue)))))
	     (set! current-fun.tail-recced? #t)
	     ;; in case it was not yet in there.
	     (set! current-fun.tail-rec-label tail-rec-label)
	     (new-node Tail-call updates tail-rec-label))
	  this)))

(define-pmethod (Lambda-tail-rec! current-fun)
   (this.traverse1! this)
   (when this.tail-recced?
      ;; replace formals with replacement variables, and replace body
      ;; with while.
      (let* ((return this.body) ;; body must be a return
	     (return-val return.val)

	     (tail-rec-label this.tail-rec-label)

	     (formals this.formals)
	     
	     (formals-vars (map (lambda (ref)
				   ref.var)
				formals))
	     
	     (replacement-decls (map (lambda (formal)
					(let ((decl (Decl-of-new-Var formal.id)))
					   (set! formal.var.replacement-decl decl)
					   decl))
				     formals))
	     (replacement-vars (map (lambda (decl)
				       decl.var)
				    replacement-decls))
	     (inits (map (lambda (formal repl-var)
			    (new-node Binding
				      formal
				      (repl-var.reference)))
			 formals
			 replacement-vars))
	     (tail-rec (new-node Tail-rec
				 formals-vars
				 inits
				 return-val
				 tail-rec-label)))
				 
	 (set! this.scope-vars (map! (lambda (var)
					(if var.replacement-decl
					    var.replacement-decl.var
					    var))
				     this.scope-vars))
	 (set! this.formals (map (lambda (formal)
				    (let ((repl-decl formal.var.replacement-decl))
				       (delete! formal.var.replacement-decl)
				       repl-decl))
				 this.formals))
	 (set! return.val tail-rec)))
   this)
