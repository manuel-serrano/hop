(module while
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   mark-statements
	   config
	   nodes
	   var
	   symbol
	   tail
	   loop-updates
	   verbose)
   (export (tail-rec->while! tree::pobject)
	   (optimize-while! tree::pobject)))

;; transform Tail-recs into While-loops.
;;
;; this pass still introduces temporary variables (and scopes)
;; -> should be before scope-flattening
(define (tail-rec->while! tree)
   (verbose " tail-rec->while")
   (overload traverse! while! (Node
			       Tail-rec
			       Tail-call)
	     (tree.traverse!)))

(define-pmethod (Node-while!)
   (this.traverse0!))

(define-pmethod (Tail-rec-while!)
   (set! this.label.vars this.scope-vars)
   (this.traverse0!)
   (delete! this.label.vars)
   (let* ((break-label (new-node Label (gensym 'break)))
	  (while (new-node While
			   (new-node Const #t)
			   (new-node Break
				     this.body
				     break-label)
			   this.label))
	  (labelled (new-node Labelled
			      while
			      break-label))
	  (bnode (new-node Begin
			   (append! this.inits
				    (list labelled)))))
      (set! while.scope-vars this.scope-vars)
      bnode))

(define-pmethod (Tail-call-while!)
   (this.traverse0!)
   (new-node Begin
	     (list (loop-updates-free-order this.label.vars
					    this.updates)
		   (new-node Continue this.label))))
   
;; try to find loops, that can be transformed into optimized whiles.
;; In particular we want the test of the while to have a meaning (and not just
;; be "true").
;; Ex:
;; while (true) {
;;   break (
;;      if test
;;         foo
;;         blah_all_branches_continue
;;   )
;; }
;;   =>
;; while (not test) {
;;   blah_all_branches_continue [ remove continues ]
;; }
;; foo
;;
(define (optimize-while! tree)
   (when (config 'while)
      (verbose " optimize-while")
      ;; we need to know, if the test of an 'if' is in statement-form.
      (mark-statements tree)
      (search-patterns! tree)
      (remove-tail-continues! tree)))

;; search for our pattern(s) and apply them/it if found.
(define (search-patterns! tree)
   (verbose " search-patterns")
   (overload traverse! patterns! (Node
				  While
				  Break)
	     (tree.traverse!)))

(define-pmethod (Node-patterns!)
   (this.traverse0!))

(define-pmethod (While-patterns!)
   (define (apply-pattern! continue-branch)
      (let* ((break this.body)
	     (iff break.val))
	 (case continue-branch
	    ((no-continue-in-else)
	     (set! this.test iff.test)
	     (set! break.val iff.then)
	     (new-node Begin (list this iff.else)))
	    ((no-continue-in-then)
	     (set! this.test (new-node Call
				       (runtime-reference 'not)
				       (list iff.test)))
	     (set! break.val iff.else)
	     (new-node Begin (list this iff.then)))
	    (else
	     (error "While-patterns!" "should never happen" #f)))))

   (if (and (inherits-from? this.test (node 'Const))
	    (eq? this.test.value #t)
	    (inherits-from? this.body (node 'Break))
	    (inherits-from? this.body.val (node 'If))
	    (not (statement-form? this.body.val.test)))
       (let* ((iff this.body.val)
	      (new-this (cond
			   ((not (continue-in-branch? iff.else))
			    (if (only-continues-in-branch? iff.then)
				(apply-pattern! 'no-continue-in-else)
				this))
			   ((not (continue-in-branch? iff.then))
			    (if (only-continues-in-branch? iff.else)
				(apply-pattern! 'no-continue-in-then)
				this))
			   (else
			    ;; we don't know how to optimize this case.
			    this))))
	  (new-this.traverse0!))
       (this.traverse0!)))

;; if the enclosed node is a Begin or an If push the Break into the node.
;; That is, for a Begin only the last element is enclosed by the Break, and for
;; the If both branches receive a Break.
(define-pmethod (Break-patterns!)
   (cond
      ((not this.val) ;; should not happen
       this)
      ((not (statement-form? this.val))
       (this.traverse0!))
      ((inherits-from? this.val (node 'If))
       (let* ((iff this.val)
	      (then-break (new-node Break
				    iff.then
				    this.label)))
	  (set! this.val iff.else)
	  (set! iff.then then-break)
	  (set! iff.else this)
	  (iff.traverse!)))
      ((inherits-from? this.val (node 'Begin))
       (let ((bnode this.val))
	  (let loop ((exprs bnode.exprs))
	     (cond
		((null? exprs) ;; should never happen
		 (this.traverse0!))
		((null? (cdr exprs))
		 (set! this.val (car exprs))
		 (set-car! exprs this)
		 (bnode.traverse!))
		(else
		 (loop (cdr exprs)))))))
      ((inherits-from? this.val (node 'Continue))
       (this.val.traverse!))
      (else
       (this.traverse0!))))
   
;; #t if the given branch contains a continue
(define (continue-in-branch? branch)
      (overload traverse search-shallow-continue (Node
						  Lambda
						  While
						  Continue)
		(bind-exit (found-fun)
		   (branch.traverse found-fun)
		   #f)))

(define-pmethod (Node-search-shallow-continue found-fun)
   (this.traverse1 found-fun))

(define-pmethod (Lambda-search-shallow-continue found-fun)
   'do-not-go-into-lambdas)

(define-pmethod (While-search-shallow-continue found-fun)
   'do-not-go-into-whiles)

(define-pmethod (Continue-search-shallow-continue found-fun)
;   (verbose "found continue")
   (found-fun #t))

;; same as continue-in-branch?, but with difference, that all sub-branches have
;; to have a 'continue'
(define (only-continues-in-branch? branch)
   (overload traverse all-continues (Node
				      Lambda
				      While
				      If
				      Case
				      Continue)
	     (bind-exit (found-fun)
		(branch.traverse found-fun)
		#f)))
   
(define-pmethod (Node-all-continues found-fun)
   (this.traverse1 found-fun))

(define-pmethod (Lambda-all-continues found-fun)
   'do-not-go-into-lambdas)

(define-pmethod (While-all-continues found-fun)
   'do-not-go-into-whiles)

(define-pmethod (If-all-continues found-fun)
   (this.test.traverse found-fun)
   (when (bind-exit (then-found-fun)
	    (this.then.traverse then-found-fun)
	    #f)
      (this.else.traverse found-fun))
   ;; there might be a continue later. but if we reach this line, not both
   ;; sub-branches had a continue.
   #f)

(define-pmethod (Case-all-continues found-fun)
   (this.key.traverse found-fun) ;; can't be (IMHO)
   (unless (null? this.clauses)
      (let loop ((clauses this.clauses))
	 (cond
	    ((null? clauses)
	     ;; all clauses had a continue
	     (found-fun #t))
	    ((bind-exit (clause-found-fun)
		((car clauses).traverse clause-found-fun)
		#f)
	     (loop (cdr clauses)))
	    (else
	     ;; one clause did not have a continue.
	     #f)))))
	     
(define-pmethod (Continue-all-continues found-fun)
   (found-fun #t))


(define (remove-tail-continues! tree)
   (verbose " tail-continues")
   (overload traverse! tail-continue! (Node
				       (Module Enclosing-tail-continue!)
				       (Lambda Enclosing-tail-continue!)
				       (Const Value-tail-continue!)
				       (Var-ref Value-tail-continue!)
				       (Frame-alloc Value-tail-continue!)
				       If
				       Case
				       Clause
				       (Set! Enclosing-tail-continue!)
				       Begin
				       (Call Enclosing-tail-continue!)
				       While
				       (Return Enclosing-tail-continue!)
				       (Labelled Inter-tail-continue!)
				       Break
				       (Frame-push Inter-tail-continue!)
				       Continue
				       (Pragma Value-tail-continue!))
	     (tree.traverse! #f)))

(define-pmethod (Node-tail-continue! tail?)
   (error #f "tail. forgot node-type" (pobject-name this)))

(define-pmethod (Value-tail-continue! tail?)
   this)
   
(define-pmethod (Inter-tail-continue! tail?)
   (this.traverse1! tail?))

(define-pmethod (Enclosing-tail-continue! tail?)
   (this.traverse1! #f))

(define-pmethod (If-tail-continue! tail?)
   (set! this.test (this.test.traverse! #f))
   (set! this.then (this.then.traverse! tail?))
   (set! this.else (this.else.traverse! tail?))
   this)

(define-pmethod (Case-tail-continue! tail?)
   (set! this.key (this.key.traverse! #f))
   (set! this.clauses
	 (map! (lambda (clause)
		  (clause.traverse! tail?))
	       this.clauses))
   this)

(define-pmethod (Clause-tail-continue! tail?)
   (set! this.consts
	 (map! (lambda (const)
		  (const.traverse! #f))
	       this.consts))
   (set! this.expr (this.expr.traverse! tail?))
   this)

(define-pmethod (Begin-tail-continue! tail?)
   (let loop ((exprs this.exprs))
      (cond
	 ((null? exprs)
	  this)
	 ((null? (cdr exprs))
	  (set-car! exprs
		    ((car exprs).traverse! tail?)))
	 (else
	  (set-car! exprs
		    ((car exprs).traverse! #f))
	  (loop (cdr exprs)))))
   this)

(define-pmethod (While-tail-continue! tail?)
   (this.traverse1! #t))

(define-pmethod (Break-tail-continue! tail?)
   (this.traverse1! #f))

(define-pmethod (Continue-tail-continue! tail?)
   (if tail?
       (new-node Const #unspecified)
       this))
