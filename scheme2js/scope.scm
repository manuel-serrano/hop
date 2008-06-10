(module scope
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   config
	   verbose
	   var
	   locals
	   free-vars
	   captured-vars
	   mark-statements)
   (export (scope-resolution! tree::pobject)
	   (scope-flattening! tree::pobject)
	   Scope))

(define-pclass (Scope id vars surrounding-while)
   (set! this.id id)
   (set! this.vars vars)
   (set! this.surrounding-while surrounding-while))

;; When call/cc is activated (and not just suspend/resume) then we treat While
;; and Call/cc-Calls the same way. Both are repetition
;; points. Whereas the While is an well defined interval, Call/cc-Calls reach
;; to the end of the function. In other words Call/cc-repetitions have as
;; body the remaining body. If they are inside a While, than the whole body of
;; the While is of course part of this Call/cc-repetition too.
;;
;; Suppose a repetition (be it While or a Call/cc-loop) contains a Scope of
;; variable 'x'. If x is not captured or it is constant and the init-value is
;; not changed during the loop, then x can be reused for several iterations,
;; and thus may be defined as fun-var. (In other words the Scope can be
;; discarded.) The term ".. init-value not changed during the loop .." is
;; transitive. That is, if the init-value itself is initialized during the
;; loop, then the init-value of the init-value must not be changed during the
;; loop...
;;
;; Note that call/cc calls may return different values at each call. The
;; letrec-expansion pass however ensures us, that call/cc-calls can not happen
;; inside a Letrec-form. Handling call/cc-calls inside Lets is not that
;; difficult, though: the let-frame is constructed only
;; after the call and the variable's value stays constant during its lifetime.
;;
;; Once some Scopes have been removed we need to look at function-vars. If
;; suspend/resume (which is implied by call/cc) is activated, then some
;; variables need to be boxed.
;; Function variables that escape and are not constant (independent of
;; init-var), and which have Call/cc-call in their Scope, need to be boxed.
;; If a frame (outside the concerned Scope) is already constructed for
;; some variable, they can simply add themselves into this frame.
;;
;; If a loop is initiated by a call/cc then not all Scopes need to allocate a
;; storage-object. Only if the Scope's variable escapes and there is a call/cc
;; inside the Scope, then a Storage-object is needed. Otherwise the
;; exception-based iteration already creates a new frame.


(define (scope-resolution! tree)
   (verbose "Scope resolution")
   (let-split! tree)
   (captured-vars tree)
   (scope-call/cc-when-alive? tree)
   (scope-needs-frame/uniquization? tree)
   (scope-needs-boxing? tree)
   (scope-needs-updates? tree)
   (scope-loops! tree)
   (let-merge! tree)
   (scope-frame-alloc! tree))

;; some predicates (which are added as .flags to the vars):
;;  - call/cc-when-alive? : a call/cc is inside the scope
;;  - modified-after-call/cc? : the variable is updated after a call/cc-call.
;;  - needs-boxing? : the variable is not constant, escapes, and has a call/cc
;;         call inside the scope. Due to the call/cc it must 
;;         be boxed to ensure that the function that uses the escaping variable
;;         still references the same var after restoration. (The boxing can
;;         occur anywhere before its use).
;;  - needs-uniquization? : the variable is "constant", captured, and is inside a
;;         loop. The init-value may change depending on the iteration. Each
;;         capturing function must capture a "uniquized" variable.
;;  - needs-frame? : the variable is not constant, inside a loop, and is
;;         captured. Each capturing function needs to capture a separate
;;         frame. The frame must be constructed at each iteration.
;;  - needs-update? : the variable is not constant, has a call/cc, and is
;;         modified after a call/cc. The call/ccs that have captured the
;;         variable need to be updated during a finally.
;;  - indirect? : if a variable is only referenced through a storage-object.


;; split lets so they became nested lets (thereby forcing an order of
;; evaluation).
;; Most let's are going to be merged later again.
;; Difficulty: we haven't paid attention to the order of scope-vars and
;; bindings (they might not even be of same number)
(define (let-split! tree)
   (verbose " let-split")
   (overload traverse! split! (Node
			      Let)
	     (tree.traverse!)))

(define-pmethod (Node-split!)
   (this.traverse0!))

(define-pmethod (Let-split!)
   (if (eq? this.kind 'letrec)
       (this.traverse0!)
       (let ((len-bindings (length this.bindings))
	     (len-scope-vars (length this.scope-vars)))
	  (cond
	     ((and (=fx len-bindings 1)
		   (=fx len-scope-vars 1))
	      (this.traverse0!))
	     ((and (>fx len-bindings 0)
		   (=fx len-scope-vars 0))
	      ;; no need to keep the let.
	      ((new-node Begin
			 (append! this.bindings
				  (list this.body))).traverse!))
	     ((and (=fx len-bindings 0)
		   (>= len-scope-vars 0))
	      (this.traverse0!))
	     ((and (=fx len-bindings 0)
		   (=fx len-scope-vars 0))
	      (this.body.traverse!))
	     (else
	      ;; more than one scope-var
	      ;; more than one binding.
	      ;; each scope-var might only appear in one binding. (otherwise it
	      ;; would not be a 'let'.
	      ;; search the first binding for a scope-var
	      ;; if none is found -> begin
	      ;; otherwise split the binding with the corresponding var.
	      (let*((binding (car this.bindings))
		    (binding-var (search-binding-var binding this.scope-vars)))
		 (set! this.bindings (cdr this.bindings))
		 (if binding-var
		     (begin
			(set! this.scope-vars
			      (filter! (lambda (var)
					  (not (eq? var binding-var)))
				       this.scope-vars))
			((new-node Let
				   (list binding-var)
				   (list binding)
				   this
				   'let).traverse!))
		     (begin
			((new-node Begin
				   (list binding
					 this)).traverse!)))))))))

(define (search-binding-var tree vars)
   (overload traverse search (Node
			      Lambda
			      Var-ref)
	     (bind-exit (found-fun)
		(tree.traverse vars found-fun)
		#f)))

(define-pmethod (Node-search vars found-fun)
   (this.traverse2 vars found-fun))

(define-pmethod (Lambda-search vars found-fun)
   ;; don't go into lambdas
   'done)

(define-pmethod (Var-ref-search vars found-fun)
   (let ((varL (memq this.var vars)))
      (if varL
	  (found-fun (car varL))
	  #f)))

;; determine if a let contains a call/cc. Every variable of the let is
;; flagged by .call/cc-when-alive? if it has.
;; also: for any variable inside a let determine if it is modified after a
;; call/cc. If it is modified, then the var is flagged with
;; .modified-after-call/cc? If the modification is inside a loop, then the
;; location of the mutation might before the actual call/cc-call.
;; Ex:
;;    (let ((x ..))
;;       (let loop ()
;;          (set! x ..)
;;          (call/cc ...))
;;          (loop)
;; Due to the loop, a 'set!' happens after the call/cc, even though the
;; set!-line is before the call/cc-line.
;;   
;; Global variables are excluded (the are simply not affected by call/ccs).
(define (scope-call/cc-when-alive? tree)
   (when (config 'suspend/resume)
      (verbose " Looking for call/ccs inside scopes")
      (overload traverse call/ccs (Node
				   Lambda
				   Let
				   Tail-rec
				   Call/cc-Call
				   Set!)
		(tree.traverse '() (make-eq-hashtable)))))
   
(define-pmethod (Node-call/ccs surrounding-scopes var->scope-ht)
   (this.traverse2 surrounding-scopes var->scope-ht))

(define-pmethod (Lambda-call/ccs surrounding-scopes var->scope-ht)
   (let ((ht (make-eq-hashtable)))
      (for-each (lambda (var)
		   (hashtable-put! ht var this))
		this.scope-vars)
      (this.traverse2 (list this) ht)
      (when this.contains-call/cc?
	 (for-each (lambda (var)
		      (set! var.call/cc-when-alive? #t))
		   this.scope-vars)
	 (delete! this.contains-call/cc?))))

(define-pmethod (Let-call/ccs surrounding-scopes var->scope-ht)
   ;; in theory lets and letrecs should be identic for this pass (due to the
   ;; letrec-expansion). The if here should hence not be necessary.
   (if (eq? this.kind 'letrec)
       (begin
	  ;; letrec -> add variables before traversing bindings
	  (for-each (lambda (var)
		       (hashtable-put! var->scope-ht var this))
		    this.scope-vars)
	  (for-each (lambda (binding)
		       (binding.traverse (cons this surrounding-scopes)
					 var->scope-ht))
		    this.bindings))
       (begin
	  ;; we do not yet add the variables into the
	  ;; hashtable. The refs to the Let-variables are hence 'unresolved'.
	  ;; The Set! has to be aware of that.
	  ;; At the same time, the rhs are analysed in the scope outside the
	  ;; current Let (which is what should happen).
	  (for-each (lambda (binding)
		       (binding.traverse surrounding-scopes
					 var->scope-ht))
		    this.bindings)
	  (for-each (lambda (var)
		       (hashtable-put! var->scope-ht var this))
		    this.scope-vars)))
   (this.body.traverse (cons this surrounding-scopes) var->scope-ht)
   (when this.contains-call/cc?
      (for-each (lambda (var)
		   (set! var.call/cc-when-alive? #t))
		this.scope-vars)
      (delete! this.contains-call/cc?))
   (for-each (lambda (var)
		(hashtable-remove! var->scope-ht var))
	     this.scope-vars))

(define-pmethod (Tail-rec-call/ccs surrounding-scopes var->scope-ht)
   ;; the loop-variables are treated similary to Let-variables. the variables
   ;; are added to the ht only after the inits have been traversed. Any call/cc
   ;; during initialisation is hence part of the surrounding scope, and not the
   ;; loop.
   (for-each (lambda (init)
		(init.traverse surrounding-scopes var->scope-ht))
	     this.inits)
   (for-each (lambda (var)
		(hashtable-put! var->scope-ht var this))
	     this.scope-vars)
   (set! this.modified-vars '())
   (this.body.traverse (cons this surrounding-scopes) var->scope-ht)
   (when this.contains-call/cc?
      (for-each (lambda (var)
		   (set! var.call/cc-when-alive? #t))
		this.scope-vars)
      (delete! this.contains-call/cc?)
      (for-each (lambda (var)
		   (set! var.modified-after-call/cc? #t))
		this.modified-vars)
      (delete! this.modified-vars))
   (for-each (lambda (var)
		(hashtable-remove! var->scope-ht var))
	     this.scope-vars))

(define-pmethod (Call/cc-Call-call/ccs surrounding-scopes var->scope-ht)
   (this.traverse2 surrounding-scopes var->scope-ht)
   (for-each (lambda (scope)
		(set! scope.contains-call/cc? #t))
	     surrounding-scopes))

(define-pmethod (Set!-call/ccs surrounding-scopes var->scope-ht)
   (this.val.traverse surrounding-scopes var->scope-ht)
   (let ((lvar this.lvalue.var))
      (when (not lvar.modified-after-call/cc?)
	 (let ((scope (hashtable-get var->scope-ht lvar)))
	    (cond
	       ((and scope
		     scope.contains-call/cc?)
		;; there was already a call/cc in the scope of the lvar
		(set! lvar.modified-after-call/cc? #t))
		;; otherwise let's see if we are inside a while, and register
		;; us, in case the loop has a call/cc later on.
	       ((inherits-from? scope (node 'Tail-rec))
		(set! scope.modified-vars (cons lvar scope.modified-vars)))
	       (else
		;; find outer-most while inside the scope
		(let loop ((scopes surrounding-scopes)
			   (last-loop #f))
		   (cond
		      ((or (null? scopes)
			   (eq? scope (car scopes)))
		       (if last-loop
			   (set! last-loop.modified-vars
				 (cons lvar last-loop.modified-vars))))
		      ((inherits-from? (car scopes) (node 'Tail-rec))
		       (loop (cdr scopes) (car scopes)))
		      (else
		       (loop (cdr scopes) last-loop))))))))))

;; determine if a scope is inside a loop. Loop can be either a Tail-rec, or a
;; call/cc-loop (repetetive application of a captured continuation).
;; This excludes suspend/resume.
;;
;; If a scope is inside a loop we look if the variable can be reused or if we
;; need to build a proper frame. (predicate needs-frame?)
;; If it can _not_ be reused we flag it with .needs-frame?
(define (scope-needs-frame/uniquization? tree)
   (verbose " Scope Needs Frame?")
   (overload traverse frame (Node
			     (Module Lambda-frame)
			     Lambda
			     Let
			     Tail-rec
			     Call/cc-Call)
	     (tree.traverse #f (cons #f #unspecified))))

;; TODO: room for optimization. Init-value may be a variable, but only, if the
;; variable is not changed within the loop.
(define (needs-frame? var)
   (and var.captured?
	(not var.constant?)))
(define (needs-uniquization? var)
   (and var.captured?
	var.constant?
	(or (not var.value)
	    (not (inherits-from? var.value (node 'Const))))))

(define-pmethod (Node-frame inside-loop? inside-call/cc-loop?)
   (this.traverse2 inside-loop? inside-call/cc-loop?))

(define-pmethod (Lambda-frame inside-loop? inside-call/cc-loop?)
   (this.traverse2 #f (cons #f #unspecified)))

(define-pmethod (Let-frame inside-loop? inside-call/cc-loop?)
   (when (or inside-loop? (car inside-call/cc-loop?))
      (for-each (lambda (var)
		   (cond
		      ((needs-frame? var)
		       (set! var.needs-frame? #t))
		      ((needs-uniquization? var)
		       (set! var.needs-uniquization? #t))))
		this.scope-vars))
   ;; traverse must be after tests, as inside-call/cc-loop? will be physically modified.
   (this.traverse2 inside-loop? inside-call/cc-loop?))

;; this method establishes an order for the loop-variables. whereas up to now
;; we were free to change the order of the inits, due to potential call/cc
;; calls this is not possible from now on.
(define-pmethod (Tail-rec-frame inside-loop? inside-call/cc-loop?)
   (for-each (lambda (init)
		(init.traverse inside-loop? inside-call/cc-loop?))
	     this.inits)
   (this.body.traverse #t inside-call/cc-loop?)
   ;; loop-variables are not (yet) handled specially.
   (for-each (lambda (var)
		(cond
		   ((needs-frame? var)
		    (set! var.needs-frame? #t))
		   ((needs-uniquization? var)
		    (set! var.needs-uniquization? #t))))
	     this.scope-vars))

(define-pmethod (Call/cc-Call-frame inside-loop? inside-call/cc-loop?)
   (this.traverse2 inside-loop? inside-call/cc-loop?)
   (if (config 'call/cc)
       (set-car! inside-call/cc-loop? #t)))

;; Variables that are not inside any frame, or that do not need any frame
;; (either cause they are not in any loop, or cause they are reused) may need
;; to be boxed (or put into a frame again) if they escape (and if there is a
;; call/cc inside their scope)
;; If a var needs to be boxed it is marked with .needs-boxing?
;; Arguments must not be boxed, so if one of them responds to a predicate, we
;; introduce a temporary variable that takes its place.
(define (scope-needs-boxing? tree)
   (when (config 'suspend/resume)
      (verbose " needs boxing?")
      (overload traverse box! (Node
			       Lambda
			       Let
			       Tail-rec)
		(tree.traverse0))))

(define (needs-boxing? var)
   (and (not var.constant?)
	(not var.needs-frame?)
	(not var.needs-uniquization?)
	var.escapes?
	var.call/cc-when-alive?))

(define-pmethod (Node-box!)
   (this.traverse0))

;; Module should not be concerned.

(define-pmethod (Lambda-box!)
   (this.traverse0)
   (let* ((new-formals
	   (map (lambda (formal-decl)
		   (if (needs-boxing? formal-decl.var)
		       (let ((repl-decl
			      (Decl-of-new-Var 'arg)))
			  (set! formal-decl.var.repl-var repl-decl.var)
			  repl-decl)
		       formal-decl))
		this.formals))
	  (filtered-formals (filter (lambda (formal-decl)
				       (needs-boxing? formal-decl.var))
				    this.formals))
	  (extracted-vars (map (lambda (decl)
				  decl.var)
			       filtered-formals))
	  (new-scope-vars (map! (lambda (var)
				   (or var.repl-var var))
				this.scope-vars))
	  (assigs (map (lambda (decl)
			  (let ((repl-var decl.var.repl-var))
			     (delete! decl.var.repl-var)
			     (new-node Binding decl (repl-var.reference))))
		       filtered-formals)))
      (unless (null? filtered-formals)
	 (set! this.formals new-formals)
	 (set! this.scope-vars new-scope-vars)
	 (for-each (lambda (var)
		      (set! var.needs-boxing? #t))
		   extracted-vars)
	 (set! this.body.val ;; this.body == return
	       (new-node Let
			 extracted-vars
			 assigs
			 this.body.val
			 'let)))))

(define-pmethod (Let-box!)
   (this.traverse0)
   (for-each (lambda (var)
		(if (needs-boxing? var)
		    (set! var.needs-boxing? #t)))
	     this.scope-vars))

(define-pmethod (Tail-rec-box!)
   (this.traverse0)
   (for-each (lambda (var)
		(if (needs-boxing? var)
		    (set! var.needs-boxing? #t)))
	     this.scope-vars))

;; Vars that may be modified after a call/cc need to update the
;; call/cc-storage. If the variable is inside a loop and it is reused, then
;; this must happen at the end of the loop-body.
;; Variables that need a finally-update are marked with a .needs-update? flag.
(define (scope-needs-updates? tree)
   (when (config 'suspend/resume)
      (verbose " finding vars, that need updates")
      (overload traverse update (Node
				 (Lambda Scope-update)
				 (Tail-rec Scope-update)
				 (Let Scope-update))
		(tree.traverse))))

(define (needs-update? var)
;       (verbose " var: " var.id)
;       (verbose "   constant? " var.constant?)
;       (verbose "   only-loop-updates? " var.only-loop-updates?)
;       (verbose "   needs-scope? " var.needs-scope?)
;       (verbose "   needs-boxing? " var.needs-boxing?)
;       (verbose "   call/cc-when-alive? " var.call/cc-when-alive?)
;       (verbose "   modified-after-call/cc? " var.modified-after-call/cc?)
   (and (not var.constant?)
	(not var.needs-frame?)
	(not var.needs-boxing?)
	var.call/cc-when-alive?
	var.modified-after-call/cc?))
	 
(define-pmethod (Node-update)
   (this.traverse0))

(define-pmethod (Scope-update)
   (for-each (lambda (var)
		(when (needs-update? var)
		   (set! var.needs-update? #t)))
	     this.scope-vars)
   (this.traverse0))

;; loop-variables that need to be inside a frame are moved inside the loop's
;; body, so that the loop variables are only temporary variables (that are not
;; concerned by frame allocation anymore...).
(define (scope-loops! tree)
   (overload traverse! loop! (Node
			      Tail-rec)
	     (tree.traverse!)))

(define-pmethod (Node-loop!)
   (this.traverse0!))

(define-pmethod (Tail-rec-loop!)
   (let loop ((vars this.scope-vars)
	      (inits this.inits)
	      (let-vars '())
	      (bindings '()))
      (cond
	 ((and (null? vars)
	       (null? bindings))
	  'do-nothing)
	 ((null? vars)
	  (set! this.body (new-node Let
				    let-vars
				    bindings
				    this.body
				    'let)))
	 ((let ((var (car vars)))
	     (or var.needs-frame?
		 var.needs-boxing?
		 var.needs-uniquization?))
	  (let ((tmp-decl (Decl-of-new-Var 'tmploop))
		(old-var (car vars)))
	     (replace-var! (car inits)
			   old-var
			   tmp-decl.var)
	     (set-car! vars tmp-decl.var)
	     (loop (cdr vars)
		   (cdr inits)
		   (cons old-var let-vars)
		   (cons (new-node Binding
				   (old-var.reference)
				   tmp-decl)
			 bindings))))
	 (else
	  (loop (cdr vars)
		(cdr inits)
		let-vars
		bindings))))
   (this.traverse0!))
   
;; merge Scopes, if there is no "offending" instruction between both scopes:
;;
;; (define (f)
;;   (let (x)
;;     (tail-rec (y)
;;       (print y)
;;       (let (z)
;;           .....
;;
;; We can merge 'x' with lambda 'f', and 'z' with the tail-rec. But we can't
;; move 'z' to lambda 'f'. (at least not yet).
;;
;; ->
;; (define (f)
;;   [local var x]
;;   (tail-rec (y z)
;;     (print y)
;;     .....
(define (let-merge! tree)
   (overload traverse! merge! (Node
			       Module
			       Lambda
			       Let
			       Call/cc-Call
			       Tail-rec)
	     (tree.traverse! #f (cons #f #f))))

(define-pmethod (Node-merge! last-scope last-scope-valid?)
   (this.traverse2! last-scope last-scope-valid?))

;; Modules are only for parameters and exported vars.
(define-pmethod (Module-merge! last-scope last-scope-valid?)
   (this.traverse2! #f (cons #f #f)))

;; Lambdas are only for parameters and exported vars.
;; but we want a Let directly within the Return. In the worst case it will have
;; no variables.
(define-pmethod (Lambda-merge! last-scope last-scope-valid?)
   ;; body must be a Return.
   (set! this.body.val (new-node Let '() '() this.body.val 'let))
   (this.traverse2! #f (cons #f #f)))

;; if possible move variables to outer scope.
(define-pmethod (Let-merge! last-scope last-scope-valid?)
   (set! this.bindings (map! (lambda (n)
				(n.traverse! last-scope last-scope-valid?))
			     this.bindings))
   (set! this.body (this.body.traverse! this (cons #t #t)))
   (let ((valid? (car last-scope-valid?)))
      (when valid?
	 (set! last-scope.scope-vars
	       (append! this.scope-vars last-scope.scope-vars))
	 (set! this.scope-vars '()))
      (if (null? this.scope-vars)
	  (new-node Begin (append! this.bindings (list this.body)))
	  this)))

;; Calls must be in A-Normal form when using call/cc.
;; Otherwise the order of parameter-evaluation is not yet clear, which
;; could lead to big troubles...
;; Ex:
;; (some_f (let ((x 0)) (capture! x))
;;         (call/cc))
;; there are two ways to evaluate this:
;; (let ((tmp (let ((x 0)) (capture! x))))
;;    (some_f tmp (call/cc)))
;; or
;; (let ((tmp (call/cc)))
;;   (some_f (let ((x 0)) (capture! x)) tmp))
;;
;; in the first case the x can/must be reused.
;; in the second it must be reconstructed at each iteration.
(define-pmethod (Call/cc-Call-merge! last-scope last-scope-valid?)
   (this.traverse2! last-scope last-scope-valid?)
   (set-car! last-scope-valid? #f)
   this)

;; loops invalidate the last scope
(define-pmethod (Tail-rec-merge! last-scope last-scope-valid?)
   (set! this.inits (map! (lambda (n)
			     (n.traverse! last-scope last-scope-valid?))
			  this.inits))
   (set! this.body (this.body.traverse! #f (cons #f #f)))
   this)

;; Creates a Scope-Allocate for every Scope that needs it, and creates the
;; necessary frame-pushes.
;;
;; when the storage-variable itself is reused (as is the case in loops) then
;; the storage object needs to be pushed on the activation frame. (Either using
;; 'with', or with an anonymous function.)
(define (scope-frame-alloc! tree)
   (verbose " create storage allocations for Lets and adding frame pushes")
   (free-vars tree)
   (captured-vars tree)
   (overload traverse! alloc! (Node
			       Lambda
			       Let
			       Tail-rec)
	     (tree.traverse! '() #f)))

(define-pmethod (Node-alloc! scope-hts inside-loop?)
   (this.traverse2! scope-hts inside-loop?))

(define-pmethod (Lambda-alloc! scope-hts inside-loop?)
   (define (used-storage-vars free-vars)
      (filter-map
       (lambda (storage/scope-vars-ht)
	  (let ((storage-var (car storage/scope-vars-ht))
		(scope-vars-ht (cdr storage/scope-vars-ht)))
	     (if (any? (lambda (free-var)
			  (hashtable-get scope-vars-ht free-var))
		       free-vars)
		 storage-var
		 #f)))
       scope-hts))
      
   (this.traverse2! '() #f)
   (if inside-loop?
       (let* ((free-vars (hashtable-key-list this.free-vars-ht))
	      (storage-vars (used-storage-vars free-vars)))
	  (if (null? storage-vars)
	      this
	      (new-node Frame-push
			storage-vars
			this)))
       this))

;; TODO: optimize: currently needs-boxing? and needs-frame? are treated the
;; same way.
;; TODO: optimize: No need to allocate a storage-object, if the recursion is
;; due to to call/cc (and all variables do not have any call/cc inside their
;; scope). See comments at top.
(define-pmethod (Let-alloc! scope-hts inside-loop?)
   (let ((frame-vars (cp-filter (lambda (var)
				   (or var.needs-frame?
				       var.needs-boxing?
				       var.needs-uniquization?))
				this.scope-vars)))
      (if (null? frame-vars)
	  (this.traverse2! scope-hts inside-loop?)
	  ;; create storage-var
	  (let* ((storage-decl (Decl-of-new-Var 'storage))
		 (storage-var storage-decl.var)
		 (current-this this))
	     (for-each (lambda (var)
			  (set! var.indirect? #t))
		       frame-vars)
	     (set! storage-var.call/cc-when-alive?
		   (any? (lambda (var) var.call/cc-when-alive?)
			 frame-vars))
	     (this.traverse2! scope-hts inside-loop?)
	     (new-node Let
		       (list storage-var)
		       (list (new-node Binding
				       storage-decl
				       (new-node Frame-alloc
						 storage-var
						 frame-vars)))
		       (new-node Frame-push
				 (list storage-var)
				 this)
		       'let)))))

;; we can't use traverse! when inside a traverse! pass.
;; -> just physically modify the Var-refs.
(define (replace-var! tree var-to-replace new-var)
   (overload traverse replace! (Node
				Lambda
				Var-ref)
	     (tree.traverse var-to-replace new-var)))

(define-pmethod (Node-replace! old-var new-var)
   (this.traverse2 old-var new-var))

(define-pmethod (Lambda-replace! old-var new-var)
   ;; don't go into lambdas.
   'do-nothing)

;; physically modify the Var-ref
(define-pmethod (Var-ref-replace! old-var new-var)
   (when (eq? this.var old-var)
      (set! this.var new-var)
      (set! this.id new-var.id)))

;; TODO: optimize: needs-boxing? and needs-frame? treated the same way.
(define-pmethod (Tail-rec-alloc! scope-vars-ht inside-loop?)
   (set! this.inits (map! (lambda (n)
			     (n.traverse! scope-vars-ht inside-loop?))
			  this.inits))
   (set! this.body (this.body.traverse! scope-vars-ht #t))
   this)


(define (scope-flattening! tree)
   (verbose "scope-flattening")
   (let-removal! tree))
	    
;; Remove Let-nodes.
;; In the process create scope-nodes, that contain the following information:
;;  - vars, and
;;  - surrounding while (if any)
;; Each node needing this information (in particular call/cc nodes), get a list
;; of these structures.
;;
;; Also create a declared-vars list for each module/lambda. These nodes need to
;; be declared by "var".
;;
;; Finally every Module, Lambda and While receive a list of contained scopes.
(define (let-removal! tree)
   (overload traverse! remove! (Node
				Module
				Lambda
				Let
				While
				Call/cc-Call)
	     (tree.traverse! '() #f #f)))

(define-pmethod (Node-remove! scopes surrounding-fun surrounding-while)
   (this.traverse3! scopes surrounding-fun surrounding-while))

(define-pmethod (Module-remove! scopes surrounding-fun surrounding-while)
   (let ((scope (new Scope 'module this.scope-vars #f)))
      (set! this.declared-vars this.scope-vars)
      (set! this.contained-scopes (list scope))
      ;; treat module like a fun.
      (this.traverse3! (list scope) this #f)))

(define-pmethod (Lambda-remove! scopes surrounding-fun surrounding-while)
   (let ((scope (new Scope (gensym 'lambda) this.scope-vars #f))
	 (formal-vars (map (lambda (decl) decl.var) this.formals)))
      (set! this.declared-vars '()) ;; don't add formals. they don't need 'var'
      (set! this.contained-scopes (list scope))
      (this.traverse3! (list scope) this #f)))

(define-pmethod (Let-remove! scopes surrounding-fun surrounding-while)
   (let ((fun-vars (filter (lambda (var)
			      (not var.indirect?))
			   this.scope-vars))
	 (scope (new Scope (gensym 'let) this.scope-vars surrounding-while)))
      (set! surrounding-fun.declared-vars
	    (append fun-vars surrounding-fun.declared-vars))
      (let ((outer-while/fun (or surrounding-while surrounding-fun)))
	 (cons-set! outer-while/fun.contained-scopes scope))
      ;; the differentiation of letrec and let should not be necessary, but
      ;; just in case we change other parts of the compiler somewhere in the
      ;; future...
      (if (eq? this.kind 'letrec)
	  (set! this.bindings (map! (lambda (n)
				       (n.traverse! (cons scope scopes)
						    surrounding-fun
						    surrounding-while))
				    this.bindings))
	  (set! this.bindings (map! (lambda (n)
				       (n.traverse! scopes
						    surrounding-fun
						    surrounding-while))
				    this.bindings)))
      (set! this.body (this.body.traverse! (cons scope scopes)
					   surrounding-fun
					   surrounding-while))
      ;; remove Let-node
      (let ((bnode (new-node Begin
			     (append! this.bindings (list this.body)))))
	 (mark-statement-form! bnode (statement-form? this))
	 bnode)))

(define-pmethod (While-remove! scopes surrounding-fun surrounding-while)
   (let ((fun-vars (filter (lambda (var)
			      (not var.indirect?)) ;; should be all of them.
			   this.scope-vars))
	 (scope (new Scope (gensym 'while) this.scope-vars this)))
      (set! surrounding-fun.declared-vars
	    (append fun-vars surrounding-fun.declared-vars))
      (set! this.contained-scopes (list scope))
      (this.traverse3! (cons scope scopes) surrounding-fun this)
      (let ((outer-while/fun (or surrounding-while surrounding-fun)))
	 (set! outer-while/fun.contained-scopes
	       (append this.contained-scopes
		       outer-while/fun.contained-scopes)))
      this))

(define-pmethod (Call/cc-Call-remove! scopes surrounding-fun surrounding-while)
   (set! this.visible-scopes scopes)
   (this.traverse3! scopes surrounding-fun surrounding-while))
