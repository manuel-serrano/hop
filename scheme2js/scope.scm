(module scope
   (import config
	   tools
	   nodes
	   walk
	   verbose
	   free-vars
	   captured-vars)
   (static (wide-class Call/cc-Let::Let)
	   (wide-class Call/cc-Lambda::Lambda)
	   (wide-class Call/cc-Tail-rec::Tail-rec
	      contains-call/cc?::bool
	      modified-vars::pair-nil)
	   (wide-class Box-Var::Local
	      repl-var::Local)
	   (class Frame-Env
	      call/cc?::bool))
   (export (scope-resolution! tree::Module)
	   (scope-flattening! tree::Module))
   (export (class Scope-Info
	      id::symbol
	      vars::pair-nil
	      surrounding-while

	      (call/cc-vars::pair-nil (default '()))
	      (call/cc-indices::pair-nil (default '())))))

;; When call/cc is activated (and not just suspend/resume) then we treat While
;; and Call/cc-Calls the same way. Both are repetition
;; points. Whereas the While is an well defined interval, Call/cc-Calls reach
;; to the end of the function. In other words Call/cc-repetitions have as
;; body the remaining function body. If they are inside a While, than the whole
;; body of the While is part of this Call/cc-repetition too.
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
;; letrec-expansion pass however ensures, that call/cc-calls can not happen
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
   (split! tree #f))

(define-nmethod (Node.split!)
   (default-walk! this))

(define-nmethod (Let.split!)
   (with-access::Let this (kind bindings body scope-vars)
      (if (eq? kind 'letrec)
	  (default-walk! this)
	  (let ((len-bindings (length bindings))
		(len-scope-vars (length scope-vars)))
	     (cond
		((and (=fx len-bindings 1)
		      (=fx len-scope-vars 1))
		 (default-walk! this))
		((and (>fx len-bindings 0)
		      (=fx len-scope-vars 0))
		 ;; no need to keep the let.
		 (walk! (instantiate::Begin
			   (exprs (append! bindings (list body))))))
		((and (=fx len-bindings 0)
		      (>= len-scope-vars 0))
		 (default-walk! this))
		((and (=fx len-bindings 0)
		      (=fx len-scope-vars 0))
		 (walk! body))
		(else
		 ;; more than one scope-var
		 ;; more than one binding.
		 ;; each scope-var might only appear in one binding. (otherwise it
		 ;; would not be a 'let'.
		 ;; search the first binding for a scope-var
		 ;; if none is found -> begin
		 ;; otherwise split the binding with the corresponding var.
		 (let*((binding (car bindings))
		       (binding-var (search-binding-var binding scope-vars)))
		    (set! bindings (cdr bindings))
		    (if binding-var
			(begin
			   (set! scope-vars
				 (filter! (lambda (var)
					     (not (eq? var binding-var)))
					  scope-vars))
			   (walk! (instantiate::Let
				     (scope-vars (list binding-var))
				     (bindings (list binding))
				     (body this)
				     (kind 'let))))
			(walk! (instantiate::Begin
				  (exprs (list binding this))))))))))))

(define (search-binding-var tree vars)
   (bind-exit (found-fun)
      (search tree #f vars found-fun)
      #f))

(define-nmethod (Node.search vars found-fun)
   (default-walk this vars found-fun))

(define-nmethod (Lambda.search vars found-fun)
   ;; don't go into lambdas
   'done)

(define-nmethod (Ref.search vars found-fun)
   (with-access::Ref this (var)
      (let ((varL (memq var vars)))
	 (if varL
	     (found-fun (car varL))
	     #f))))

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
      (call/ccs tree #f '() (make-eq-hashtable))))
   
(define-nmethod (Node.call/ccs surrounding-scopes var->scope-ht)
   (default-walk this surrounding-scopes var->scope-ht))

(define-nmethod (Lambda.call/ccs surrounding-scopes var->scope-ht)
   (with-access::Lambda this (scope-vars)
      (let ((ht (make-eq-hashtable)))
	 (for-each (lambda (var)
		      (hashtable-put! ht var this))
		   scope-vars)
	 (default-walk this (list this) ht)
	 (when (Call/cc-Lambda? this)
	    (for-each (lambda (var)
			 (with-access::Var var (call/cc-when-alive?)
			    (set! call/cc-when-alive? #t)))
		      scope-vars)
	    (shrink! this)))))

(define-nmethod (Let.call/ccs surrounding-scopes var->scope-ht)
   (with-access::Let this (kind scope-vars bindings body)
      (let ((this+surrounding-scopes (cons this surrounding-scopes)))
	 ;; in theory lets and letrecs should be identic for this pass (due to
	 ;; the letrec-expansion). The if here should hence not be necessary.
	 (if (eq? kind 'letrec)
	     (begin
		;; letrec -> add variables before traversing bindings
		(for-each (lambda (var)
			     (hashtable-put! var->scope-ht var this))
			  scope-vars)
		(for-each (lambda (binding)
			     (walk binding this+surrounding-scopes
				   var->scope-ht))
			  bindings))
	     (begin
		;; we do not yet add the variables into the
		;; hashtable. The refs to the Let-variables are hence
		;; 'unresolved'. The Set! has to be aware of that.
		;; At the same time, the rhs are analysed in the scope outside
		;; the current Let (which is what should happen).
		(for-each (lambda (binding)
			     (walk binding surrounding-scopes
				   var->scope-ht))
			  bindings)
		(for-each (lambda (var)
			     (hashtable-put! var->scope-ht var this))
			  scope-vars)))
	 (walk body this+surrounding-scopes var->scope-ht)
	 (when (Call/cc-Let? this)
	    (for-each (lambda (var)
			 (with-access::Var var (call/cc-when-alive?)
			    (set! call/cc-when-alive? #t)))
		      scope-vars)
	    (shrink! this))
	 (for-each (lambda (var)
		      (hashtable-remove! var->scope-ht var))
		   scope-vars))))

(define-nmethod (Tail-rec.call/ccs surrounding-scopes var->scope-ht)
   (with-access::Tail-rec this (scope-vars inits body)
      ;; the loop-variables are treated similary to Let-variables. the
      ;; variables are added to the ht only after the inits have been
      ;; traversed. Any call/cc during initialisation is hence part of the
      ;; surrounding scope, and not the loop.
      (for-each (lambda (init)
		   (walk init surrounding-scopes var->scope-ht))
		inits)
      (for-each (lambda (var)
		   (hashtable-put! var->scope-ht var this))
		scope-vars)
      (widen!::Call/cc-Tail-rec this
	 (contains-call/cc? #f)
	 (modified-vars '()))
      (walk body (cons this surrounding-scopes) var->scope-ht)
      (with-access::Call/cc-Tail-rec this (contains-call/cc? modified-vars)
	 (when contains-call/cc?
	    (for-each (lambda (var)
			 (with-access::Var var (call/cc-when-alive?)
			    (set! call/cc-when-alive? #t)))
		      scope-vars)
	    (for-each (lambda (var)
			 (with-access::Var var (modified-after-call/cc?)
			    (set! modified-after-call/cc? #t)))
		      modified-vars)
	    (shrink! this)))
      (for-each (lambda (var)
		   (hashtable-remove! var->scope-ht var))
		scope-vars)))

(define-nmethod (Call/cc-Call.call/ccs surrounding-scopes var->scope-ht)
   (default-walk this surrounding-scopes var->scope-ht)
   (for-each (lambda (scope)
		(cond
		   ((Lambda? scope) (widen!::Call/cc-Lambda scope))
		   ((Let? scope) (widen!::Call/cc-Let scope))
		   ((Call/cc-Tail-rec? scope)
		    (with-access::Call/cc-Tail-rec scope (contains-call/cc?)
		       (set! contains-call/cc? #t)))))
	     surrounding-scopes))

(define-nmethod (Set!.call/ccs surrounding-scopes var->scope-ht)
   (default-walk this surrounding-scopes var->scope-ht)
   (with-access::Set! this (lvalue val)
      (with-access::Ref lvalue (var)
	 (with-access::Var var (modified-after-call/cc?)
	    (when (not modified-after-call/cc?)
	       (let ((scope (hashtable-get var->scope-ht var)))
		  (cond
		     ((or (Call/cc-Lambda? scope)
			  (Call/cc-Let? scope))
		      ;; there was already a call/cc in the scope of the var
		      (set! modified-after-call/cc? #t))
		     ((Call/cc-Tail-rec? scope)
		      (with-access::Call/cc-Tail-rec scope (contains-call/cc?
							    modified-vars)
			 (if contains-call/cc?
			     (set! modified-after-call/cc? #t)
			     ;; otherwise let's see if we are inside a while,
			     ;; and register us, in case the loop has a call/cc
			     ;; later on.
			     (cons-set! modified-vars var))))
		     (else
		      ;; find outer-most while inside the scope
		      (let loop ((scopes surrounding-scopes)
				 (last-loop #f))
			 (cond
			    ((or (null? scopes)
				 (eq? scope (car scopes)))
			     (if last-loop
				 (with-access::Call/cc-Tail-rec last-loop
				       (modified-vars)
				    (cons-set! modified-vars var))))
			    ((Call/cc-Tail-rec? (car scopes))
			     (loop (cdr scopes) (car scopes)))
			    (else
			     (loop (cdr scopes) last-loop))))))))))))

;; determine if a scope is inside a loop. Loop can be either a Tail-rec, or a
;; call/cc-loop (repetetive application of a captured continuation).
;; This excludes suspend/resume.
;;
;; If a scope is inside a loop we look if the variable can be reused or if we
;; need to build a proper frame. (predicate needs-frame?)
;; If it can _not_ be reused we flag it with .needs-frame?
(define (scope-needs-frame/uniquization? tree)
   (verbose " Scope Needs Frame?")
   (frame tree (make-Frame-Env (config 'call/cc))
	  #f (cons #f #unspecified)))

;; TODO: room for optimization. Init-value may be a variable, but only, if the
;; variable is not changed within the loop.
(define (needs-frame? var::Var)
   (with-access::Var var (captured? constant?)
      (and captured? (not constant?))))
(define (needs-uniquization? var::Var)
   (with-access::Var var (captured? constant? value)
      (and captured?
	   constant?
	   (not (Const? value)))))

(define-nmethod (Node.frame inside-loop? inside-call/cc-loop?)
   (default-walk this inside-loop? inside-call/cc-loop?))

;; Module.frame should not be necessaire. just for clarity.
(define-nmethod (Module.frame inside-loop? inside-call/cc-loop?)
   (default-walk this #f (cons #f #unspecified)))

(define-nmethod (Lambda.frame inside-loop? inside-call/cc-loop?)
   (default-walk this #f (cons #f #unspecified)))

(define-nmethod (Let.frame inside-loop? inside-call/cc-loop?)
   (with-access::Let this (scope-vars)
      (when (or inside-loop? (car inside-call/cc-loop?))
	 (for-each (lambda (var)
		      ;; can't use with-access here, as this would shadow the
		      ;; functions.
		      (cond
			 ((needs-frame? var)
			  (Var-needs-frame?-set! var #t))
			 ((needs-uniquization? var)
			  (Var-needs-uniquization?-set! var #t))))
		   scope-vars))
      ;; walk must be after tests, as inside-call/cc-loop? will be physically modified.
      (default-walk this inside-loop? inside-call/cc-loop?)))

;; this method establishes an order for the loop-variables. whereas up to now
;; we were free to change the order of the inits, due to potential call/cc
;; calls this is not possible from now on.
(define-nmethod (Tail-rec.frame inside-loop? inside-call/cc-loop?)
   (with-access::Tail-rec this (inits body scope-vars)
      (for-each (lambda (init)
		   (walk init inside-loop? inside-call/cc-loop?))
		inits)
      (walk body #t inside-call/cc-loop?)
      ;; loop-variables are not (yet) handled specially.
      (for-each (lambda (var)
		   ;; can't use with-access here, as this would shadow the
		   ;; functions.
		   (cond
		      ((needs-frame? var)
		       (Var-needs-frame?-set! var #t))
		      ((needs-uniquization? var)
		       (Var-needs-uniquization?-set! var #t))))
		scope-vars)))

(define-nmethod (Call/cc-Call.frame inside-loop? inside-call/cc-loop?)
   (default-walk this inside-loop? inside-call/cc-loop?)
   (if (Frame-Env-call/cc? env)
       (set-car! inside-call/cc-loop? #t)))

;; Variables that are not inside any scope, or that do not need any scope
;; (either cause they are not in any loop, or cause they are reused) may need
;; to be boxed (or put into a scope again) if they escape (and if there is a
;; call/cc inside their scope)
;; If a var needs to be boxed it is marked with .needs-boxing?
;; Arguments must not be boxed, so if one of them responds to a predicate, we
;; introduce a temporary variable that takes its place.
(define (scope-needs-boxing? tree)
   (when (config 'suspend/resume)
      (verbose " needs boxing?")
      (box!_ tree #f)))

(define (needs-boxing? var::Var)
   (with-access::Var var (constant? needs-frame? needs-uniquization? escapes?
				    call/cc-when-alive?)
      (and (not constant?)
	   (not needs-frame?)
	   (not needs-uniquization?)
	   escapes?
	   call/cc-when-alive?)))

(define-nmethod (Node.box!_)
   (default-walk this))

;; Module should not be concerned.

(define-nmethod (Lambda.box!_)
   (with-access::Lambda this (formals scope-vars body)
      (default-walk this)
      (let* ((new-formals
	      (map (lambda (formal-decl)
		      (with-access::Ref formal-decl (var)
			 (if (needs-boxing? var)
			     (let ((repl-decl (Ref-of-new-Var 'arg)))
				(widen!::Box-Var var
				   (repl-var (Ref-var repl-decl)))
				repl-decl)
			     formal-decl)))
		   formals))
	     (extracted-vars (filter-map (lambda (decl)
					    (with-access::Ref decl (var)
					       (and (needs-boxing? var)
						    var)))
					 formals))
	     (new-scope-vars (map! (lambda (var)
				      (if (Box-Var? var)
					  (with-access::Box-Var var (repl-var)
					     repl-var)
					  var))
				   scope-vars))
	     (assigs (map (lambda (decl)
			     (with-access::Ref decl (var)
				(with-access::Box-Var var (repl-var)
				   (instantiate::Set!
				      (lvalue decl)
				      (val (var-reference repl-var))))
				(shrink! var)))
			  extracted-vars)))
	 (unless (null? extracted-vars)
	    (set! formals new-formals)
	    (set! scope-vars new-scope-vars)
	    (for-each (lambda (var)
			 (with-access::Var var (needs-boxing?)
			    (set! needs-boxing? #t)))
		      extracted-vars)
	    (with-access::Return body (val)
	       (set! val
		     (instantiate::Let
			(scope-vars extracted-vars)
			(bindings assigs)
			(body val)
			(kind 'let))))))))

(define-nmethod (Let.box!_)
   (default-walk this)
   (with-access::Let this (scope-vars)
      (for-each (lambda (var)
		   (when (needs-boxing? var)
		      (with-access::Var var (needs-boxing?)
			 (set! needs-boxing? #t))))
		scope-vars)))

(define-nmethod (Tail-rec.box!_)
   (default-walk this)
   (with-access::Tail-rec this (scope-vars)
      (for-each (lambda (var)
		   (when (needs-boxing? var)
		      (with-access::Var var (needs-boxing?)
			 (set! needs-boxing? #t))))
		scope-vars)))

;; Vars that may be modified after a call/cc need to update the
;; call/cc-storage. If the variable is inside a loop and it is reused, then
;; this must happen at the end of the loop-body.
;; Variables that need a finally-update are marked with a .needs-update? flag.
(define (scope-needs-updates? tree)
   (when (config 'suspend/resume)
      (verbose " finding vars, that need updates")
      (update tree #f)))

(define (needs-update? var::Var)
   (with-access::Var var (constant? needs-frame? needs-boxing?
				    call/cc-when-alive?
				    modified-after-call/cc?)
      (and (not constant?)
	   (not needs-frame?)
	   (not needs-boxing?)
	   call/cc-when-alive?
	   modified-after-call/cc?)))

(define (mark-updates vars)
   (for-each (lambda (var)
		(when (needs-update? var)
		   (with-access::Var var (needs-update?)
		      (set! needs-update? #t))))
	     vars))

(define-nmethod (Node.update)
   (default-walk this))

(define-nmethod (Lambda.update)
   (mark-updates (Lambda-scope-vars this))
   (default-walk this))
(define-nmethod (Tail-rec.update)
   (mark-updates (Tail-rec-scope-vars this))
   (default-walk this))
(define-nmethod (Let.update)
   (mark-updates (Let-scope-vars this))
   (default-walk this))

;; loop-variables that need to be inside a frame are moved inside the loop's
;; body, so that the loop variables are only temporary variables (that are not
;; concerned by frame allocation anymore...).
(define (scope-loops! tree)
   (loop!_ tree #f))

(define-nmethod (Node.loop!_)
   (default-walk this))

(define-nmethod (Tail-rec.loop!_)
   (with-access::Tail-rec this (scope-vars inits body)
      (let loop ((vars scope-vars)
		 (inits inits)
		 (let-vars '())
		 (bindings '()))
	 (cond
	    ((and (null? vars)
		  (null? bindings))
	     'do-nothing)
	    ((null? vars)
	     (set! body (instantiate::Let
			   (scope-vars let-vars)
			   (bindings bindings)
			   (body body)
			   (kind 'let))))
	    ((with-access::Var (car vars)
		   (needs-frame? needs-boxing? needs-uniquization?)
		(or needs-frame?
		    needs-boxing?
		    needs-uniquization?))
	     (let* ((tmp-decl (Ref-of-new-Var 'tmploop))
		    (tmp-var (Ref-var tmp-decl))
		    (old-var (car vars)))
		(replace-var! (car inits)
			      old-var
			      tmp-var)
		(set-car! vars tmp-var)
		(loop (cdr vars)
		      (cdr inits)
		      (cons old-var let-vars)
		      (cons (instantiate::Set!
			       (lvalue (var-reference old-var))
			       (val tmp-decl))
			    bindings))))
	    (else
	     (loop (cdr vars)
		   (cdr inits)
		   let-vars
		   bindings))))
      (default-walk this)))

;; physically modify Refs with .var == var-to-replace
(define (replace-var! tree var-to-replace new-var)
   (replace!_ tree #f var-to-replace new-var))

(define-nmethod (Node.replace!_ old-var new-var)
   (default-walk this old-var new-var))

(define-nmethod (Lambda.replace!_ old-var new-var)
   ;; don't go into lambdas.
   'do-nothing)

(define-nmethod (Ref.replace!_ old-var new-var)
   ;; physically modify the Ref
   (with-access::Ref this (var id)
      (when (eq? var old-var)
	 (set! var new-var)
	 (set! id (Var-id new-var)))))


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
   (merge! tree #f #f (cons #f #f)))

(define-nmethod (Node.merge! last-scope last-scope-valid?)
   (default-walk! this last-scope last-scope-valid?))

;; Modules are only for parameters and exported vars.
(define-nmethod (Module.merge! last-scope last-scope-valid?)
   (default-walk! this #f (cons #f #f)))

;; Lambdas are only for parameters and exported vars.
;; but we want a Let directly within the Return. In the worst case it will have
;; no variables.
(define-nmethod (Lambda.merge! last-scope last-scope-valid?)
   ;; body must be a Return.
   (with-access::Lambda this (body)
      (with-access::Return body (val)
	 (set! val (instantiate::Let
		      (scope-vars '())
		      (bindings '())
		      (body val)
		      (kind 'let)))
	 (default-walk! this #f (cons #f #f)))))

;; if possible move variables to outer scope.
(define-nmethod (Let.merge! last-scope last-scope-valid?)
   (with-access::Let this (bindings body scope-vars)
      (set! bindings (map! (lambda (n)
			      (walk! n last-scope last-scope-valid?))
			   bindings))
      (set! body (walk! body this (cons #t #t)))
      (let ((valid? (car last-scope-valid?)))
	 (when valid?
	    (let ((this-scope-vars scope-vars))
	       (with-access::Let last-scope (scope-vars)
		  (set! scope-vars (append! this-scope-vars scope-vars))))
	    (set! scope-vars '())))
      (cond
	 ((and (null? scope-vars)
	       (null? bindings))
	  body)
	 ((null? scope-vars)
	  (instantiate::Begin
	     (exprs (append! bindings (list body)))))
	 (else
	  this))))

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
(define-nmethod (Call/cc-Call.merge! last-scope last-scope-valid?)
   (default-walk! this last-scope last-scope-valid?)
   (set-car! last-scope-valid? #f)
   this)

;; loops invalidate the last scope
(define-nmethod (Tail-rec.merge! last-scope last-scope-valid?)
   (with-access::Tail-rec this (inits body)
      (set! inits (map! (lambda (n)
			   (walk! n last-scope last-scope-valid?))
			inits))
      (set! body (walk! body #f (cons #f #f)))
      this))

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
   (alloc! tree #f
	   '() #f))

(define-nmethod (Node.alloc! scope-hts inside-loop?)
   (default-walk! this scope-hts inside-loop?))

(define-nmethod (Lambda.alloc! scope-hts inside-loop?)
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

   (default-walk! this '() #f)
   (if inside-loop?
       (with-access::Lambda this (free-vars)
	  (let* ((storage-vars (used-storage-vars free-vars)))
	     (if (null? storage-vars)
		 this
		 (instantiate::Frame-push
		    (storage-vars storage-vars)
		    (body this)))))
       this))

;; TODO: optimize: currently needs-boxing? and needs-frame? are treated the
;; same way.
;; TODO: optimize: No need to allocate a storage-object, if the recursion is
;; due to to call/cc (and all variables do not have any call/cc inside their
;; scope). See comments at top.
(define-nmethod (Let.alloc! scope-hts inside-loop?)
   (with-access::Let this (scope-vars body)
      (let ((frame-vars (cp-filter
			 (lambda (var)
			    (with-access::Var var (needs-frame? needs-boxing?
						   needs-uniquization?)
			       (or needs-frame?
				   needs-boxing?
				   needs-uniquization?)))
			 scope-vars)))
	 (if (null? frame-vars)
	     (default-walk! this scope-hts inside-loop?)
	     ;; create storage-var
	     (let* ((storage-decl (Ref-of-new-Var 'storage))
		    (storage-var (Ref-var storage-decl)))
		(for-each (lambda (var)
			     (with-access::Var var (indirect?)
				(set! indirect? #t)))
			  frame-vars)
		(with-access::Var storage-var (call/cc-when-alive?)
		   (set! call/cc-when-alive?
			 (any? Var-call/cc-when-alive? frame-vars)))

		(default-walk! this scope-hts inside-loop?)

		(instantiate::Let
		   (scope-vars (list storage-var))
		   (bindings (list (instantiate::Set!
				      (lvalue storage-decl)
				      (val (instantiate::Frame-alloc
					      (storage-var (Ref-var storage-decl))
					      (vars frame-vars))))))
		   (body (if (not inside-loop?)
			     this
			     (instantiate::Frame-push
				(storage-vars (list storage-var))
				(body this))))
		   (kind 'let)))))))

;; TODO: optimize: needs-boxing? and needs-frame? treated the same way.
(define-nmethod (Tail-rec.alloc! scope-vars-ht inside-loop?)
   (with-access::Tail-rec this (inits body)
      (set! inits (map! (lambda (n)
			   (walk! n scope-vars-ht inside-loop?))
			inits))
      (set! body (walk! body scope-vars-ht #t))
      this))


(define (scope-flattening! tree)
   (verbose "scope-flattening")
   (let-removal! tree))
	    
;; Remove Let-nodes.
;; In the process create scope-nodes that contain the following information:
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
   (remove! tree #f #f #f #f))

;; surrounding-fun could be a Module too!
(define-nmethod (Node.remove! scopes surrounding-fun surrounding-while)
   (default-walk! this scopes surrounding-fun surrounding-while))

(define-nmethod (Module.remove! scopes surrounding-fun surrounding-while)
   (with-access::Module this (scope-vars declared-vars contained-scopes)
      (let ((scope (instantiate::Scope-Info
		      (id 'module)
		      (vars scope-vars)
		      (surrounding-while #f))))
	 (set! declared-vars scope-vars)
	 (set! contained-scopes (list scope))

	 (default-walk! this (list scope) this #f))))

(define-nmethod (Lambda.remove! scopes surrounding-fun surrounding-while)
   (with-access::Lambda this (scope-vars declared-vars contained-scopes
					 formals)
      (let ((scope (instantiate::Scope-Info
		      (id (gensym 'lambda))
		      (vars scope-vars)
		      (surrounding-while #f)))
	    (formal-vars (map Ref-var formals)))
	 (set! declared-vars '()) ;; don't add formals. they don't need 'var'
	 (set! contained-scopes (list scope))
	 (default-walk! this (list scope) this #f))))

(define-nmethod (Let.remove! scopes surrounding-fun surrounding-while)
   (with-access::Let this (scope-vars bindings body kind)
      (let ((fun-vars (filter (lambda (var)
				 (with-access::Var var (indirect?)
				    (not indirect?)))
			      scope-vars))
	    (scope (instantiate::Scope-Info
		      (id (gensym 'let))
		      (vars scope-vars)
		      (surrounding-while surrounding-while))))
	 (if (Module? surrounding-fun)
	     (with-access::Module surrounding-fun (declared-vars)
		(set! declared-vars (append fun-vars declared-vars)))
	     (with-access::Lambda surrounding-fun (declared-vars)
		(set! declared-vars (append fun-vars declared-vars))))
	 (let ((outer-while/fun (or surrounding-while surrounding-fun)))
	    (cond
	       ((Module? outer-while/fun)
		(with-access::Module outer-while/fun (contained-scopes)
		   (cons-set! contained-scopes scope)))
	       ((Lambda? outer-while/fun)
		(with-access::Lambda outer-while/fun (contained-scopes)
		   (cons-set! contained-scopes scope)))
	       (else
		(with-access::While outer-while/fun (contained-scopes)
		   (cons-set! contained-scopes scope)))))
      ;; the differentiation of letrec and let should not be necessary, but
      ;; just in case we change other parts of the compiler somewhere in the
      ;; future...
      (if (eq? kind 'letrec)
	  (set! bindings (map! (lambda (n)
				  (walk! n (cons scope scopes)
					 surrounding-fun
					 surrounding-while))
			       bindings))
	  (set! bindings (map! (lambda (n)
				  (walk! n scopes
					 surrounding-fun
					 surrounding-while))
			       bindings)))
      (set! body (walk! body (cons scope scopes)
			surrounding-fun
			surrounding-while))
      ;; remove Let-node
      (instantiate::Begin
	 (exprs (append! bindings (list body)))))))

(define-nmethod (While.remove! scopes surrounding-fun surrounding-while)
   (with-access::While this (scope-vars contained-scopes)
      (let ((fun-vars (filter (lambda (var)
				 (with-access::Var var (indirect?)
				    (not indirect?))) ;; should be all of them.
			      scope-vars))
	    (scope (instantiate::Scope-Info
		      (id (gensym 'while))
		      (vars scope-vars)
		      (surrounding-while this))))
	 
	 (if (Module? surrounding-fun)
	     (with-access::Module surrounding-fun (declared-vars)
		(set! declared-vars (append fun-vars declared-vars)))
	     (with-access::Lambda surrounding-fun (declared-vars)
		(set! declared-vars (append fun-vars declared-vars))))
	 (set! contained-scopes (list scope))
	 (default-walk! this (cons scope scopes) surrounding-fun this)
	 (let ((outer-while/fun (or surrounding-while surrounding-fun))
	       (this-scopes contained-scopes))
	    (cond
	       ((Module? outer-while/fun)
		(with-access::Module outer-while/fun (contained-scopes)
		   (set! contained-scopes
			 (append this-scopes contained-scopes)))) 
	       ((Lambda? outer-while/fun)
		(with-access::Lambda outer-while/fun (contained-scopes)
		   (set! contained-scopes
			 (append this-scopes contained-scopes))))
	       (else
		(with-access::While outer-while/fun (contained-scopes)
		   (set! contained-scopes
			 (append this-scopes contained-scopes))))))
	 this)))

(define-nmethod (Call/cc-Call.remove! scopes surrounding-fun surrounding-while)
   (with-access::Call/cc-Call this (visible-scopes)
      (set! visible-scopes scopes))
   (default-walk! this scopes surrounding-fun surrounding-while))
