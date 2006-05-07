(module tail-rec
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes
	   symbol
	   var
	   transform-util
	   tail
	   verbose)
   (export (tail-rec! tree::pobject)))

(define (tail-rec! tree::pobject)
   (define *id->js-var* #unspecified)
   
   (define (optimize-tail-rec call-node target)
      (define (duplicate-formals old-formals)
	 (map (lambda (formal)
		 (Decl-of-new-Var (gensym formal.var.id)))
	      old-formals))
      (define (duplicate-vaarg old-vaarg)
	 (and old-vaarg
	      (Decl-of-new-Var (gensym old-vaarg))))
      
      (if (not target.tail-recced?)
	  (let ((old-formals target.formals)
		(old-vaarg target.vaarg))
	     (set! target.tail-recced? #t)
	     (set! target.tail-rec-formals old-formals)
	     (set! target.tail-rec-vaarg old-vaarg)
	     (set! target.formals (duplicate-formals old-formals))
	     (set! target.vaarg (duplicate-vaarg old-vaarg))
	     (set! target.tail-rec-label (gensym 'rec-call))))
      
      (let* ((fun-body target.body)
	     (fun-formals target.formals)
	     (fun-vaarg target.vaarg)
	     (tail-rec-label target.tail-rec-label)
	     (call-operands call-node.operands)
	     (formal-vaarg-assigs (parameter-assigs call-operands
						    fun-formals
						    fun-vaarg
						    #t ;; take reference
						    *id->js-var*)))
	 
	 ;; replace call by reassig of vars, followed by 'continue'
	 (new-node Begin (append formal-vaarg-assigs
			    (list (new-node Tail-rec-call tail-rec-label))))))
   
   (define-pmethod (Node-tail-rec! current-fun)
      (this.traverse1! current-fun))

   (define-pmethod (Program-tail-rec! current-fun)
      (set! *id->js-var* this.id->js-var)
      (this.traverse1! current-fun))
   
   (define-pmethod (Lambda-tail-rec! current-fun)
      (let* ((new-body (this.body.traverse! this)))
	 (if this.tail-recced?
	     (let* ((tail-rec-label this.tail-rec-label)
		    (tail-rec-formals this.tail-rec-formals)
		    (tail-rec-vaarg this.tail-rec-vaarg)
		    (fun-formals this.formals)
		    (fun-vaarg this.vaarg)
		    (formals-copy
		     (map (lambda (tr-formal fun-formal)
			     (tr-formal.var.assig (fun-formal.var.reference)))
			  tail-rec-formals
			  fun-formals))
		    (copy-assigs (if fun-vaarg
				     (cons (tail-rec-vaarg.var.assig
					    (fun-vaarg.var.reference))
					   formals-copy)
				     formals-copy))
		    (tail-rec-body (new-node Begin (append copy-assigs
						      (list new-body))))
		    (loop (new-node Tail-rec tail-rec-formals
			       tail-rec-vaarg tail-rec-body tail-rec-label)))
		(delete! this.tail-rec-label)
		(delete! this.tail-rec-formals)
		(delete! this.tail-rec-vaarg)
		(set! this.body loop))
	     (set! this.body new-body)))
      this)
   
   (define-pmethod (Call-tail-rec! current-fun)
      (let loop ((operands this.operands))
	 (unless (null? operands)
	    (set-car! operands ((car operands).traverse! current-fun))
	    (loop (cdr operands))))
      
      (let ((op this.operator))
	 (set! this.operator (op.traverse! current-fun)) ;; might be a lambda
	 (if this.tail?
	     (set! this.tail-call #t))
	 (if (and this.tail?
		  current-fun
		  (config 'optimize-tail-rec)
		  (inherits-from? op (node 'Var-ref))
		  (eq? op.var.single-value current-fun))
	     (optimize-tail-rec this current-fun)
	     this)))

   ;; ================================================
   ;; procedure starts here
   ;; ================================================
   (verbose "tail-rec")
   (tail-exprs tree #f) ;; intermediate nodes are not considered to be tail.
   (verbose " traversing")
   (overload traverse! tail-rec! (Node
				  Program
				  Lambda
				  Call)
	     (tree.traverse! #f)))

