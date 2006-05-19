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
	   side
	   locals
	   free-vars
	   captured-vars
	   tail
	   capture
	   verbose)
   (export (tail-rec! tree::pobject)))

(define (tail-rec! tree::pobject)
   (define *id->js-var* #unspecified)

   ;; current (I hope I don't forget to update the comment...) strategy:
   ;; - non-captured variables are assigned to tmp-variables, and then assigned
   ;; back to their loop-variables.
   ;; - captured variables are assigned to the tmp-variable (that is also used
   ;; as formal for the surrounding procedure. The "back-assigning" is done at
   ;; the beginning of the body. This is necessary to capture the variables
   ;; correctly with the 'with' statement. The beginning-part is done in the
   ;; "Lambda-tail-rec" function.
   ;; ex:
   ;; (define (foo x y z) ;; x is captured, y and z aren't.
   ;;    (store! (lambda () x))
   ;;    (if test
   ;;        (foo (+ x 1) z y)
   ;;        (foo (+ x 2) z y)))
   ;;
   ;; (define (foo tmp-x y z)
   ;;    (tail-rec
   ;;       (set! x tmp-x)
   ;;       (store! (lambda () x))
   ;;       (if test
   ;;           (begin
   ;;             (set! tmp-y z)
   ;;             (set! tmp-z y)
   ;;             (set! tmp-x (+ x 1))
   ;;             (set! y tmp-y)
   ;;             (set! z tmp-z)
   ;;             (continue!))
   ;;           (begin
   ;;             (set! tmp2-y z)
   ;;             (set! tmp2-z y)
   ;;             (set! tmp-x (+ x 2))
   ;;             (set! y tmp2-y)
   ;;             (set! z tmp2-z)
   ;;             (continue!))
   (define (optimize-tail-rec call-node target)
      ;; we are creating quite some tmp-vars here, but we have a
      ;; pass that eliminates them...

      (define (tmp-formal/vaarg formal/vaarg)
	 (let ((var formal/vaarg.var))
	    (cond
	       (var.tmp-var => (lambda (tmp-var) (tmp-var.reference)))
	       (var.captured?
		(let ((tmp-var (Decl-of-new-Var var.id)))
		   (set! var.tmp-var tmp-var)
		   (tmp-var.var.reference)))
	       (else
		(Decl-of-new-Var 'tmp)))))
	    
      (if (not target.tail-recced?)
	  (begin
	     (set! target.tail-rec-label (gensym 'rec-call))
	     (set! target.tail-recced? #t)))
      
      (let* ((tail-rec-label target.tail-rec-label)

	     (fun-formals target.formals)
	     (fun-vaarg target.vaarg)

	     (tmp-formals (map tmp-formal/vaarg fun-formals))
	     (tmp-vaarg (and fun-vaarg (tmp-formal/vaarg fun-vaarg)))
	     
	     (tmp-assigs (parameter-assigs call-node.operands
					   tmp-formals
					   tmp-vaarg
					   #f ;; don't take reference
					   *id->js-var*))
	     
	     (back-assigs
	      (map (lambda (formal/vaarg tmp-var)
		      (if formal/vaarg.var.captured?
			  (new-node Const #unspecified)
			  (new-node Set!
				    (formal/vaarg.var.reference)
				    (tmp-var.var.reference))))
		   (if fun-vaarg (cons fun-vaarg fun-formals) fun-formals)
		   (if fun-vaarg (cons tmp-vaarg tmp-formals) tmp-formals))))
	 
	 ;; replace call by reassig of vars, followed by 'continue'
	 (new-node Begin (append tmp-assigs
				 back-assigs
				 (list (new-node Tail-rec-call
						 tail-rec-label))))))
   
   (define-pmethod (Node-tail-rec! current-fun)
      (this.traverse1! current-fun))

   (define-pmethod (Program-tail-rec! current-fun)
      (set! *id->js-var* this.id->js-var)
      (this.traverse1! current-fun))
   
   (define-pmethod (Lambda-tail-rec! current-fun)
      (define (escaping-formal/vaarg-exchange!)
	 (let loop ((formals-ptr this.formals)
		    (rev-exchanged '()))
	    (if (null? formals-ptr)
		(if (and this.vaarg
			 this.vaarg.var.tmp-var)
		    (let* ((old-vaarg this.vaarg)
			   (tmp-vaarg old-vaarg.var.tmp-var))
		       (set! this.vaarg tmp-vaarg)
		       (delete! tmp-vaarg.var.tmp-var)
		       (cons (cons old-vaarg tmp-vaarg)
			     rev-exchanged))
		    rev-exchanged)
		(if (car formals-ptr).var.tmp-var
		    (let* ((formal (car formals-ptr))
			   (tmp-formal formal.var.tmp-var))
		       (set-car! formals-ptr tmp-formal)
		       (delete! tmp-formal.var.tmp-var)
		       (loop (cdr formals-ptr)
			     (cons (cons formal tmp-formal)
				   rev-exchanged)))
		    (loop (cdr formals-ptr)
			  rev-exchanged)))))
		    
      (let ((new-body (this.body.traverse! this)))
	 (if this.tail-recced?
	     (let* ((tail-rec-label this.tail-rec-label)
		    (escaping-f/v-mapping (escaping-formal/vaarg-exchange!))
		    (prolog (map (lambda (vp)
				    (new-node Set!
					      (car vp)
					      ((cdr vp).var.reference)))
				 escaping-f/v-mapping))
		    (loop-body (new-node Begin (append! prolog
							(list new-body))))
		    (loop (new-node Tail-rec loop-body tail-rec-label)))
		(delete! this.tail-rec-label)
		(delete! this.tail-recced?)
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
   (tail-exprs tree
	       #f) ;; intermediate nodes are not considered to be tail.
   (side-effect tree)
   (locals tree
	   #t) ;; collect formals
   (free-vars tree)
   (captured-vars tree)
   (overload traverse! tail-rec! (Node
				  Program
				  Lambda
				  Call)
	     (tree.traverse! #f)))

