(module callcc
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes
	   side
	   free-vars
	   tail
	   locals
	   js-interface
	   mark-statements
	   var
	   verbose)
   (export (callcc-early tree::pobject)
	   (callcc-check-points tree::pobject)
	   (callcc-late tree::pobject)))


(define (callcc-early tree)
   (when (config 'call/cc)
      (verbose "call/cc early")
      (side-effect tree)
      (rev-call-tree tree)
      (callcc-propagation tree)
      (clean-callers tree)))
      
(define (callcc-check-points tree::pobject)
   (when (config 'call/cc)
      (verbose "call/cc check-points")
      (tail-exprs tree
		  #f) ;; intermediate nodes are not considered to be tail.
      (check-point! tree)
      (implicit-closures! tree)))

(define (runtime-var-ref-id n)
   (and (inherits-from? n (node 'Var-ref))
	(inherits-from? n.var (node 'JS-Var))
	(let* ((var n.var)
	       (js-id var.js-id))
	   (and js-id
		(not var.muted?)
		(any? (lambda (p) (eq? js-id (cadr p))) *runtime-var-mapping*)
		var.id))))

(define (call-target operator)
   (cond
      ((inherits-from? operator (node 'Lambda))
       operator)
      ((runtime-var-ref-id operator)
       operator)
      ((and (inherits-from? operator (node 'Var-ref))
	    operator.var.single-value)
       (call-target operator.var.single-value))
      (else
       #f)))

;; every lambda receives its callers. recursive calls are ignored.
(define (rev-call-tree tree)
   (verbose " call/cc rev-call-tree")
   (overload traverse call-tree (Node
				 Module
				 Lambda
				 Call)
	     (tree.traverse #f)))

(define-pmethod (Node-call-tree current-fun)
   (this.traverse1 current-fun))

;; Module is not really a function, but we'll treat it like one, here.
(define-pmethod (Module-call-tree current-fun)
   (this.traverse1 this))

(define-pmethod (Lambda-call-tree current-fun)
   (this.traverse1 this))

(define-pmethod (Call-call-tree current-fun)
   (this.traverse1 current-fun)
   (let ((target (call-target this.operator)))
      (if target
	  (begin
	     (set! this.target target)
	     (if (inherits-from? target (node 'Lambda))
		 (set! target.callers (cons current-fun
					    (or target.callers '()))))))))

(define (callcc-propagation tree)
   (verbose " call/cc propagation")
   (overload traverse propagate (Node
				 Module
				 Lambda
				 Call)
	     (tree.traverse #f)))

(define-pmethod (Node-propagate current-fun)
   (this.traverse1 current-fun))

;; Module is not a function, but in this context we'll treat it like one.
(define-pmethod (Module-propagate current-fun)
   (this.traverse1 this))

(define-pmethod (Lambda-propagate current-fun)
   (this.traverse1 this))

(define *optimizable-higher*
   `((apply ,car)
     (map ,car)
     (for-each ,car)
     (call-with-values ,car ,cadr)
     (js-call ,cadr)
     (with-input-from-port ,cadr)
     (with-input-from-string ,cadr)
     (with-output-to-port ,cadr)
     (with-output-to-string ,car)
     (hashtable-for-each ,cadr)
     (dynamic-wind ,car ,cadr ,caddr)
     (bind-exit-lambda ,car)
     (with-handler-lambda ,car ,cadr)))

(define-pmethod (Call-propagate current-fun)
   (define (mark-call/cc-fun fun)
      (unless fun.potential-call/cc?
	 (set! fun.potential-call/cc? #t)
	 (if fun.callers (for-each mark-call/cc-fun fun.callers))))

   (this.traverse1 current-fun)
   (let* ((target this.target)
	  (runtime-id (and target (runtime-var-ref-id target))))
      (cond
	 ;; optim.
	 ((and runtime-id (assq runtime-id *optimizable-higher*))
	  =>
	  (lambda (match)
	     (if (any?
		  (lambda (which)
		     (let* ((higher-target (call-target (which this.operands)))
			    (higher-target-runtime
			     (runtime-var-ref-id higher-target)))
			(cond
			   ((and (inherits-from? higher-target (node 'Lambda))
				 (not higher-target.potential-call/cc?))
			    ;; put us into the callers list of the
			    ;; higher-target, in case it hasn't been traversed
			    ;; yet.
			    (when (or (not higher-target.callers)
				      (not (memq current-fun
						 higher-target.callers)))
			       (set! higher-target.callers
				     (cons current-fun
					   (or higher-target.callers
					       '()))))
			    ;; put us into a 'higher-targets' list so we can
			    ;; verify later on, if any of these yet unevaluated
			    ;; functions turned out to contain call/ccs.
			    (set! this.higher-targets
				  (cons higher-target
					(or this.higher-targets
					    '())))
			    ;; for now don't mark us as call/cc
			    #f)
			   ((and higher-target-runtime
				 (not (memq higher-target-runtime
					    *higher-order-runtime*)))
			    ;; do-nothing
			    #f)
			   (else
			    ;; mark call/cc-fun and abort traversal of list.
			    #t))))
		  (cdr match))
		 (begin
		    (mark-call/cc-fun current-fun)
		    (set! this.call/cc-call? #t)))))
		 
	 ((or (not target) ;; assume call/cc
	      target.potential-call/cc?
	      (and runtime-id (memq runtime-id *higher-order-runtime*)))
	  (mark-call/cc-fun current-fun)
	  (set! this.call/cc-call? #t))
	 (else
	  'do-nothing))))

(define (clean-callers tree)
   (verbose " clean nodes")
   (overload traverse clean-callers (Node Lambda)
	     (tree.traverse)))

(define-pmethod (Node-clean-callers)
   (this.traverse0))

(define-pmethod (Lambda-clean-callers)
   (delete! this.callers)
   (this.traverse0))

(define (check-point! tree)
   (verbose " call/cc check-point!")
   (overload traverse! check-point! (Node
				     Module
				     Lambda
				     Call)
	     (tree.traverse! #f)))

(define-pmethod (Node-check-point! index)
   (this.traverse1! index))

(define-pmethod (Module-check-point! index)
   (this.traverse1! (list 1)))

(define-pmethod (Lambda-check-point! index)
   (this.traverse1! (list 1)))

(define-pmethod (Call-check-point! index)
   (this.traverse1! index)
   
   ;; higher-targets have been set during propagate pass.
   ;; at this time these targets haven't been evaluated yet (at least not
   ;; always.)
   ;; if one of them turned out to contain call/cc we must mark this call
   ;; as call/cc.
   ;; the surrounding function however has already been marked correctly.
   (let* ((higher-targets this.higher-targets))
      (if (and higher-targets
	       (any? (lambda (n)
			n.potential-call/cc?)
		     higher-targets))
	  (set! this.call/cc-call? #t)))
   ;; same is true, if the call-target has been marked later on.
   (let ((target this.target))
      (if (and target target.potential-call/cc?)
	  (set! this.call/cc-call? #t)))

   (let ((unsafe-call? (and (not this.tail?)
			    this.call/cc-call?)))
      (delete! this.target)
      (delete! this.higher-targets)
      (delete! this.call/cc-call?)
      (if unsafe-call?
	  (let ((call/cc-call (new-node Call/cc-Call
					this.operator
					this.operands)))
	     (set! call/cc-call.call/cc-index (car index))
	     (set-car! index (+ (car index) 1))
	     call/cc-call)
	  this)))

(define (implicit-closures! tree)
   (locals tree
	   #t) ;; collect-formals
   (free-vars tree)
   (verbose " implicit call/cc closures")
   (overload traverse! closures! (Node
				  Module
				  Lambda
				  Var-ref
				  Closure-ref)
	     (tree.traverse!)))

(define-pmethod (Node-closures!)
   (this.traverse0!))

(define (Scope-closure-assig! n local-vars)
   (if n.potential-call/cc?
       (let ((escaping (filter! (lambda (v)
				   (and v.escapes?
					(not v.is-global?)))
				(hashtable-key-list local-vars))))
	  (if (not (null? escaping))
	      (let* ((closure-decl (Decl-of-new-Var (gensym 'callccClosure)))
		     (closure-var closure-decl.var)
		     (alloc (new-node Closure-alloc))
		     (assig (new-node Set! closure-decl alloc)))
		 ;; closure is obviously captured.
		 (set! closure-decl.var.captured? #t)
		 (set! closure-decl.var.closure-object? #t)
		 ;; all escaping non-constant args have to be changed to
		 ;; closure-refs. 
		 ;; exception are closures. They are already safe.
		 (hashtable-for-each
		  local-vars
		  (lambda (var ignored)
		     (if (and var.escapes?
			      var.muted?
			      (not var.closure-object?))
			 (let ((ref-id (symbol-append closure-var.id
						      '_ var.id)))
			    (set! var.closure-var
				  (new-node Field-Var
					    ref-id
					    closure-var
					    var))))))
		 assig)
	      #f))
       #f))
   
(define-pmethod (Module-closures!)
   (let ((assig (Scope-closure-assig! this this.local-vars)))
      (if assig
	  (set! this.body
		(new-node Begin
			  (list assig this.body)))))
   (this.traverse0!))

(define-pmethod (Lambda-closures!)
   (let ((assig (Scope-closure-assig! this this.local-vars)))
      (if assig
	  (let* ((closure-var assig.lvalue.var)
		 (escaping-args (map (lambda (var-ref)
					var-ref.var)
				     (filter (lambda (var-ref)
						var-ref.var.closure-var)
					     (if this.vaarg
						 (cons this.vaarg
						       this.formals)
						 this.formals))))
		 ;; the assigs copying the args into the closure
		 (arg-assigs
		  (map
		   (lambda (v)
		      (new-node Set!
				(new-node Closure-ref
					  v.closure-var.id
					  v.closure-var)
				(v.reference)))
		   escaping-args)))
	     ;; now that we have marked the vars, we can traverse the body
	     (set! this.body
		   (new-node Begin
			     (cons assig
				   (append! arg-assigs
					    (list (this.body.traverse!))))))
	     this)
	  (this.traverse0!))))

(define-pmethod (Var-ref-closures!)
   (if this.var.closure-var
       (this.var.closure-var.reference)
       this))

(define-pmethod (Closure-ref-closures!)
   this)

;; the statement-pass introduces new Set!s. We shift the call/cc information
;; down to the Set!s.
(define (callcc-late tree)
   (when (config 'call/cc)
      (verbose "call/cc late")
      (callcc-resumes tree)
      (callcc-ranges tree)
      (callcc-indicators tree)))

(define (callcc-resumes tree)
   (verbose " call/cc resumes")
   (overload traverse! resume! (Node
			       Call/cc-Call
			       If
			       Case
			       Begin
			       Set!
			       Tail-rec
			       While
			       Labelled)
	     (tree.traverse!)))

;; a resume-begin is a begin finishing with a resume-node.
(define (resume-begin? n)
   (and (inherits-from? n (node 'Begin))
	(inherits-from? (car (last-pair n.exprs)) (node 'Call/cc-Resume))))

(define (resume-begin-split bnode)
   (let ((exprs bnode.exprs))
      (if (= (length exprs) 2) ;; something + resume
	  (cons (car exprs) (cadr exprs))
	  (begin
	     ;; remove last element
	     ;; not very elegant. I know.
	     (let* ((rev-exprs (reverse! exprs))
		    (resume (car rev-exprs)))
		(set! bnode.exprs (reverse! (cdr rev-exprs)))
		(cons bnode resume))))))

;; merges all Resumes into the first of the list.
;; returns #f if the list is empty.
(define (resumes-merge! resumes)
   (and resumes
	(not (null? resumes))
	(let ((first-resume (car resumes)))
	   ;; first merge all the Resumes
	   (let loop ((resumes (cdr resumes))
		      (indices/vars first-resume.indices/vars))
	      (if (null? resumes)
		  (set! first-resume.indices/vars indices/vars)
		  (loop (cdr resumes)
			(append (car resumes).indices/vars
				indices/vars))))
	   first-resume)))
   
(define-pmethod (Node-resume!)
   (this.traverse0!))

;; if the Call/cc-call result is stored in a variable, then the statement-begin
;; is invalid, but we will correct this in the Set! method.
(define-pmethod (Call/cc-Call-resume!)
   (this.traverse0!)
   (let* ((resume-node (new-node Call/cc-Resume this.call/cc-index))
	  (bnode (new-node Begin (list this resume-node))))
      (mark-statement-form! resume-node #t)
      (mark-statement-form! bnode #t)
      bnode))

(define-pmethod (If-resume!)
   (this.traverse0!)
   (let ((then/resume (and (resume-begin? this.then)
			   (resume-begin-split this.then)))
	 (else/resume (and (resume-begin? this.else)
			   (resume-begin-split this.else))))
      (when then/resume
	 (set! this.then (car then/resume)))
      (when else/resume
	 (set! this.else (car else/resume)))
      (if (or then/resume else/resume)
	  (let* ((resume (cond
			    ((and then/resume else/resume)
			     (resumes-merge! (list (cdr then/resume)
						   (cdr else/resume))))
			    (then/resume
			     (cdr then/resume))
			    (else
			     (cdr else/resume))))
		 (bnode (new-node Begin (list this resume))))
	     (mark-statement-form! bnode #t)
	     bnode)
	  this)))

(define-pmethod (Case-resume!)
   ;; TODO: implement Case-resume
   (this.traverse0!))

(define-pmethod (Begin-resume!)
   (this.traverse0!)
   (let ((exprs this.exprs))
      ;; merge nested Begins.
      (let loop ((exprs exprs))
	 (unless (null? exprs)
	    (let ((expr (car exprs)))
	       (cond
		  ((inherits-from? expr (node 'Begin))
		   ;; insert into our list.
		   (let ((other-exprs expr.exprs)
			 (exprs-tail (cdr exprs)))
		      ;; we know there must be at least 2 elements.
		      ;; otherwise we wouldn't have gotten a 'Begin'.
		      (set-car! exprs (car other-exprs))
		      (set-cdr! exprs (cdr other-exprs))
		      (set-cdr! (last-pair other-exprs) exprs-tail))
		   (loop exprs))
		  ((inherits-from? expr (node 'Call/cc-Resume))
		   (let ((next (and (not (null? (cdr exprs)))
				    (cadr exprs))))
		      (cond
			 ((not next)
			  (loop (cdr exprs)))
			 ;; merge two consecutive Resumes into one.
			 ((inherits-from? next (node 'Call/cc-Resume))
			  (set-car! exprs (resumes-merge! (list (car exprs) next)))
			  (set-cdr! exprs (cddr exprs))
			  (loop exprs))
			 ;; if a Resume is followed by a Break, we can move the
			 ;; Resume after the target of the Break.
			 ((inherits-from? next (node 'Break))
			  (set! next.labelled.resumes
				(cons expr (or next.labelled.resumes
					       '())))
			  ;; remove Resume from this Begin
			  (set-car! exprs (cadr exprs))
			  (set-cdr! exprs (cddr exprs))
			  (loop exprs))
			 ;; TODO: enable tail-rec-call optim for Call/cc resumes.
; 			 ((inherits-from? next (node 'Tail-rec-call))
; 			  (set! next.target.resumes
; 				(cons expr (or next.target.resumes
; 					       '()))))
;			  ;; remove Resume from this Begin
;			  (set-car! exprs (cadr exprs))
;			  (set-cdr! exprs (cddr exprs))
;			  (loop exprs))
			 (else
			  (loop (cdr exprs))))))
		  (else
		   (loop (cdr exprs))))))))
   this)

(define-pmethod (Set!-resume!)
   (this.traverse0!)
   (if (resume-begin? this.val)
       (let* ((val/resume (resume-begin-split this.val))
	      (new-val (car val/resume))
	      (resume (cdr val/resume)))
	  ;; TODO: optimize. reuse the begin-node.
	  (set! this.val new-val)
	  ;; there must only one entry in the a-list.
	  ;; before resuming the execution at our resume-point we need to
	  ;; update the lvalue.
	  ;; ex:
	  ;;    case 0:
	  ;;      index = 2;
	  ;;      x = call/cc();
	  ;;    case 2:
	  ;;      ....
	  ;; In this case we would need to update 'x' before resuming at 'case 2:'.
	  (set-cdr! (car resume.indices/vars) this.lvalue.var)
	  (let ((bnode (new-node Begin (list this resume))))
	     (mark-statement-form! bnode #t)
	     bnode))
       this))

(define-pmethod (Tail-rec-resume!)
   (this.traverse0!)
   (if (resume-begin? this.body)
       (let* ((body/resume (resume-begin-split this.body))
	      (bnode (new-node Begin (list this
					   (cdr body/resume)))))
	  (mark-statement-form! bnode #t)
	  (set! this.body (car body/resume))
	  bnode)
       this))

(define-pmethod (While-resume!)
   (this.traverse0!)
   (if (resume-begin? this.body)
       (let* ((body/resume (resume-begin-split this.body))
	      ;; note, that the Resume is now before the while.
	      (bnode (new-node Begin (list (cdr body/resume)
					   this))))
	  (mark-statement-form! bnode #t)
	  (set! this.body (car body/resume))
	  bnode)
       this))

(define-pmethod (Labelled-resume!)
   (this.traverse0!)
   (let* ((body/resume (and (resume-begin? this.body)
			    (resume-begin-split this.body)))
	  (body-resume (if body/resume
			   (list (cdr body/resume))
			   '()))
	  (resumes (or this.resumes
		       '()))
	  (resume (resumes-merge! (append body-resume resumes))))
      (delete! this.resumes)
      (if body/resume
	  (set! this.body (car body/resume)))
      (if resume
	  (let ((bnode (new-node Begin (list this resume))))
	     (mark-statement-form! bnode #t)
	     bnode)
	  this)))

(define (callcc-ranges tree)
   (verbose " call/cc ranges")
   (overload traverse ranges (Node
			      Call/cc-Resume
			      Lambda
			      If
			      Case
			      Clause
			      Begin
			      Set!
			      Call
			      While)
	     (set! (node 'Node).proto.default-traverse-value '())
	     (tree.traverse)
	     (delete! (node 'Node).proto.default-traverse-value)))

(define (range-merge Ls)
   (let ((ht (make-eq-hashtable)))
      (for-each (lambda (l)
		   (for-each (lambda (v)
				(hashtable-put! ht v #t))
			     l))
		Ls)
      (hashtable-key-list ht)))
   

(define (multi-traverse n children)
   (let* ((children-range (range-merge (map (lambda (n)
					       (n.traverse))
					    children))))
      (if (and (statement-form? n)
	       children-range
	       (not (null? children-range)))
	  (set! n.call/cc-range children-range))
      children-range))

(define-pmethod (Node-ranges)
   (let* ((children-range (this.traverse0)))
      (if (and (statement-form? this)
	       children-range
	       (not (null? children-range)))
	  (set! this.call/cc-range children-range))
      children-range))

(define-pmethod (Call/cc-Resume-ranges)
   this.indices/vars
   (set! this.call/cc-range this.indices/vars))

(define-pmethod (Lambda-ranges)
   (set! this.call/cc-var-mapping '())
   (multi-traverse this
		   (if this.vaarg
		       (append this.formals (list this.vaarg
						  this.body))
		       (append this.formals (list this.body))))
   '())
   
(define-pmethod (If-ranges)
   (begin0
    (multi-traverse this (list this.test this.then this.else))
    (let ((then-range this.then.call/cc-range)
	  (else-range this.else.call/cc-range))
       (if (and then-range (not (null? then-range)))
	   (set! this.call/cc-then-range then-range))
       (if (and else-range (not (null? else-range)))
	   (set! this.call/cc-else-range else-range)))))

(define-pmethod (Case-ranges)
   (multi-traverse this (cons this.key this.clauses)))

(define-pmethod (Clause-ranges)
   (multi-traverse this (append this.consts (list this.expr))))

(define-pmethod (Begin-ranges)
   (multi-traverse this this.exprs))

(define-pmethod (Call-ranges)
   (multi-traverse this (cons this.operator this.operands)))

(define-pmethod (While-ranges)
   (multi-traverse this (list this.test this.body)))

(define-pmethod (Set!-ranges)
   (multi-traverse this (list this.lvalue this.val)))

(define (callcc-indicators tree)
   (verbose " call/cc indicators")
   (overload traverse indicator (Node
				 Module
				 Lambda
				 Call/cc-Call
				 Var-ref
				 (Tail-rec Loop-indicator)
				 (While Loop-indicator))
	     (tree.traverse '() #f (list 0))))

(define-pmethod (Node-indicator surrounding-loops surrounding-fun counter)
   (this.traverse3 surrounding-loops surrounding-fun counter))

;; treat Module as fun.
(define-pmethod (Module-indicator surrounding-loops surrounding-fun counter)
   (this.traverse3 '() this (list 0)))

(define-pmethod (Lambda-indicator surrounding-loops surrounding-fun counter)
   (this.traverse3 '() this (list 0)))

(define-pmethod (Call/cc-Call-indicator surrounding-loops
					surrounding-fun
					counter)
   (this.traverse3 surrounding-loops surrounding-fun counter)
   (let ((indicator-indices (map (lambda (loop)
				    loop.indicator-index)
				 surrounding-loops)))
      (unless (null? indicator-indices)
	 (set! surrounding-fun.indices/indicators
	       (cons (cons this.call/cc-index
			   indicator-indices)
		     (or surrounding-fun.indices/indicators '()))))))

(define-pmethod (Var-ref-indicator surrounding-loops surrounding-fun counter)
   (if (inherits-from? this.var (node 'Call/cc-indicator-Var))
       (let ((indicator-index (car surrounding-loops).indicator-index))
	  (set! this.var.indicator-index indicator-index))))

(define-pmethod (Loop-indicator surrounding-loops surrounding-fun counter)
   (set! this.indicator-index (car counter))
   (set-car! counter (+ (car counter) 1))
   (this.traverse3 (cons this surrounding-loops) surrounding-fun counter)
   (delete! this.indicator-index))
