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
   ;;
   ;; An optimization reduces the 'tmp-variable'-affections,  by directly
   ;; assigning to the loop-variable.
   ;;
   (define (optimize-tail-rec call-node target)
      ;; we are creating quite some tmp-vars in this function, but we have a
      ;; pass that eliminates them...

      ;; builds a dependency tree of the variables. A var v1 is dependent on
      ;; v2, if v2 appears on the right hand side of the assignment-expression.
      ;; Exception is v1 itself ('v1 = v1 + 1' wouldn't need a tmp-var).
      ;; the tree is stored in hashtables directly in the vars:
      ;;   - var.referenced-vars
      ;;   - and var.rev-referenced-vars (the back-pointers
      ;;
      ;; this procedure stops at function boundaries.
      (define (var-ref-tree vars)
	 (let ((assig-vars-ht (make-eq-hashtable)))
	    (for-each (lambda (var)
			 (hashtable-put! assig-vars-ht var #t))
		      vars)
	    (define-pmethod (Node-shallow-refs self-var ht)
	       (this.traverse2 self-var ht))
	    (define-pmethod (Var-ref-shallow-refs self-var ht)
	       ;; don't count self.
	       (if (and (not (eq? this.var self-var))
			(hashtable-contains? assig-vars-ht this.var))
		   (hashtable-put! ht this.var #t)))
	    (define-pmethod (Lambda-shallow-refs self-var ht)
	       'do-nothing)
	    (overload traverse shallow-refs (Node
					     Var-ref
					     Lambda)
		      (for-each (lambda (var)
				   (let ((referenced-vars (make-eq-hashtable)))
				      (set! var.referenced-vars referenced-vars)
				      (var.new-val.traverse var referenced-vars)))
				vars))
	    (for-each (lambda (var)
			 (set! var.rev-referenced-vars (make-eq-hashtable)))
		      vars)
	    (for-each (lambda (var)
			 (hashtable-for-each var.referenced-vars
					     (lambda (v ignored)
						(hashtable-put!
						 v.rev-referenced-vars
						 var
						 #t))))
		      vars)))

      ;; breaks dependency cycles, by marking cyclic vars, that should be
      ;; replaced by a tmp-var. (marked with 'var.cyclic-var?'
      (define (break-cycles vars)
	 ;; TODO: replace this suboptimal algo (break-cycles)
	 (define (break-cycle var)
	    (unless var.visited?
	       (set! var.active? #t)
	       (set! var.visited? #t)
	       (hashtable-for-each var.referenced-vars
				   (lambda (v ignored)
				      (if v.active?
					  (set! v.cyclic-var? #t)
					  (break-cycle v))))
	       (delete! var.active?)))
	 (var-ref-tree vars)
	 (for-each (lambda (var)
		      (break-cycle var))
		   vars))

      ;; creates break-var, and breaks cycle (decreasing rev-dep.
      ;; ex: var x should be broken:
      ;;         tmp-x.new-val <- x.new-val
      ;;         x.new-val <- tmp-x
      ;; (modulo taking the reference....)
      ;; At the same time x does become completely independent of any other
      ;; variable. (except tmp-x, which is going to be treated at the very
      ;; beginning anyways).
      (define (break-var-decl cyclic-var)
	 (let ((new-var-decl (Decl-of-new-Var cyclic-var.id)))
	       (set! new-var-decl.var.new-val cyclic-var.new-val)
	       (set! cyclic-var.new-val (new-var-decl.var.reference))
	       ;; the cyclic-var is not cyclic anymore, and doesn't depend on
	       ;; any variables anymore. We therefore remove the cyclic-var
	       ;; from the revdeps of its "ex"-dependencies.
	       ;; We don't add these references to the break-vars, as these are
	       ;; going to be treated differently anyways, and are going to be
	       ;; assigned first. (As we know, that they don't have any revdeps.
	       (hashtable-for-each cyclic-var.referenced-vars
				   (lambda (v ignored)
				      (hashtable-remove! v.rev-referenced-vars
							 cyclic-var)))
	       ;; and we replace the deps by an empty hashtable.
	       (set! cyclic-var.referenced-vars (make-eq-hashtable))
	       new-var-decl))
	 
      ;; breaks dependency cycles and returns an ordered list of "Set!"s.
      ;; the given vars are referenced (so not directly put into these "Set!"s).
      ;; the "new-val"-field within the given vars is deleted.
      (define (non-cyclic-assigs vars)
	 (define (clean-var! var)
	    (delete! var.new-val)
	    (delete! var.referenced-vars)
	    (delete! var.rev-referenced-vars)
	    (delete! var.cyclic-var?)
	    (delete! var.break-var))

	 ;; mark cycle-vars
	 (break-cycles vars)

	 ;; cycle-vars and reverse-independent vars can be assigned at the
	 ;; beginning:
	 ;;    suppose we had:  x = y; y = z; z = x; t = x;
	 ;;    we break the cycle by flagging x.
	 ;;  now we can start with assigning tmp-x (the cyclic tmp-vars)
	 ;;  then with the "reverse independent" variable 't'. nobody depends
	 ;;  on 't'.
	 (let* ((cyclic-vars (filter (lambda (var)
					var.cyclic-var?)
				     vars))
		(break-var-decls (map break-var-decl cyclic-vars))
		;; now that we have introduced the break-vars we can search for
		;; rev-independant variables. IE vars, that are not used by
		;; anyone else.
		(indeps (filter (lambda (var)
                                   (= (hashtable-size var.rev-referenced-vars)
                                      0))
                                vars)))

	    ;; the indeps will be our pending set.

	    ;; mark all indeps vars as pending?
	    (for-each (lambda (var)
			 (set! var.pending? #t))
		      indeps)

	    ;; We must treat the break-vars independantly, as the assignment
	    ;; must not take a reference, but directly use the Decl.
	    ;; the rev-res is therefore initially filled with these
	    ;; tmp-assignments. We then add the pending-vars one by one.
	    ;; Once we added a pending, we remove its deps in the tree.
	    ;; if this leads to free another var, we add it to the pendings.
	    (let loop ((pending indeps)
		       (rev-res (map (lambda (break-var-decl)
					(let* ((val break-var-decl.var.new-val))
					   (clean-var! break-var-decl.var)
					   (new-node Set!
						     break-var-decl
						     val)))
				     break-var-decls)))
	       (if (null? pending)
		   (reverse! rev-res)
		   (let* ((var (car pending))
			  (new-val var.new-val))
		      ;; remove rev-deps.
		      (hashtable-for-each var.referenced-vars
					  (lambda (v ignored)
					     (if v.rev-referenced-vars
						 ;; hasn't been cleaned yet
						 (hashtable-remove!
						  v.rev-referenced-vars
						  var))))
		      ;; retain vars, without rev-dep that aren't yet pending?
		      (hashtable-filter! var.referenced-vars
					 (lambda (v ignored)
					    (and (not v.pending?)
						 ;; hasn't been cleaned yet
						 v.rev-referenced-vars
						 (= (hashtable-size
						     v.rev-referenced-vars)
						    0))))
		      ;; mark them as pending?
		      (hashtable-for-each var.referenced-vars
					  (lambda (var ignored)
					     (set! var.pending? #t)))
		      (let ((new-pending
			     (append (hashtable-key-list var.referenced-vars)
				     (cdr pending))))
			 (clean-var! var)
			 (loop new-pending
			       (cons (var.assig new-val)
				     rev-res))))))))
      
      (define (replace-captured-formals/vaarg! vars)
	 (map! (lambda (var)
		  (cond
		     (var.tmp-var => (lambda (tmp-var)
					tmp-var))
		     (var.captured?
		      (let ((tmp-var (Decl-of-new-Var var.id)))
			 (set! var.tmp-var tmp-var)
			 tmp-var.var))
		     (else
		      var)))
	      vars))

      (if (not target.tail-recced?)
	  (begin
	     (set! target.tail-rec-label (gensym 'rec-call))
	     (set! target.tail-recced? #t)))
      
      (let* ((tail-rec-label target.tail-rec-label)

	     (fun-formals target.formals)
	     (fun-vaarg target.vaarg)

	     (assig-mapping (parameter-assig-mapping call-node.operands
						     fun-formals
						     fun-vaarg
						     *id->js-var*))
	     (assig-vars (map (lambda (p)
				 (car p).var)
			      assig-mapping)))
	 ;; temporarely store the rvalue in the vars.
	 (for-each (lambda (p)
		      (set! (car p).var.new-val (cdr p)))
		   assig-mapping)
	 (let* ((replaced-vars (replace-captured-formals/vaarg! assig-vars))
		(assigs (non-cyclic-assigs replaced-vars)))
	 
	 ;; replace call by reassig of vars, followed by 'continue'
	 (new-node Begin (append assigs
				 (list (new-node Tail-rec-call
						 tail-rec-label)))))))
   
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

