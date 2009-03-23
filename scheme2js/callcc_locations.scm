(module callcc-locations
   (import config
	   nodes
	   walk
	   export-desc
	   side
	   free-vars
	   var-ref-util
	   verbose
	   tools)
   (static (wide-class Call/cc-Lambda::Lambda
	      (callers::pair-nil (default '())))
	   (wide-class Call/cc-Call::Call
	      (target (default #f))
	      (verify-later-targets::pair-nil (default '())))
	   (final-class Mark-Env
	      assume-call/cc?::bool
	      extern-always-call/cc?::bool))
   (export (call/cc-locations tree::Module)))

;; sets .call/cc? for calls and functions that might reach call/cc.
(define (call/cc-locations tree)
   (when (config 'suspend/resume)
      (verbose "call/cc locations")
      (side-effect tree)
      (rev-call-tree tree)   ;; build call-tree
      (callcc-mark tree)     ;; detect call/ccs and mark calls/funs.
      (shrink tree)))        ;; shrink all nodes.

;; every lambda receives its callers. recursive calls are ignored.
(define (rev-call-tree tree)
   (verbose " call/cc rev-call-tree")
   (rev-tree tree #f #f))

(define-nmethod (Node.rev-tree current-fun)
   (default-walk this current-fun))

;; Module is not really a function, but we'll treat it like one, here.
(define-nmethod (Module.rev-tree current-fun)
   (default-walk this this))

(define-nmethod (Lambda.rev-tree current-fun)
   (widen!::Call/cc-Lambda this)
   (default-walk this this))

(define-nmethod (Call/cc-Lambda.rev-tree current-fun)
   (default-walk this this))

;; set target for call, and add current-fun to callers of target.
(define-nmethod (Call.rev-tree current-fun)
   (default-walk this current-fun)
   (with-access::Call this (operator)
      (let ((target (call-target operator)))
	 (when target
	    (widen!::Call/cc-Call this (target target))
	    (when (Lambda? target)
	       (unless (Call/cc-Lambda? target)
		  (widen!::Call/cc-Lambda target))
	       (with-access::Call/cc-Lambda target (callers)
		  (unless (memq current-fun callers)
		     (set! callers (cons current-fun callers)))))))))

(define (callcc-mark tree)
   (verbose " call/cc mark")
   (mark tree (instantiate::Mark-Env
		 (assume-call/cc? (config 'assume-callcc?)) ;; for benchmarking
		 (extern-always-call/cc? (config 'extern-always-call/cc)))
	 #f)
   (finish-marking tree #f))

(define-nmethod (Node.mark current-fun)
   (default-walk this current-fun))

(define-nmethod (Module.mark current-fun)
   (default-walk this #f))

(define-nmethod (Lambda.mark current-fun)
   (when (Mark-Env-assume-call/cc? env)
      (mark-call/cc-fun this))
   (default-walk this this))

(define (unsafe-param? param call current-fun env)
   (let* ((higher-target (call-target param))
	  (target-var (constant-var higher-target)))
      (cond
	 ((and (Lambda? higher-target)
	       (Lambda-call/cc? higher-target))
	  #t)
	 ((Lambda? param)
	  ;; maybe the target has just not yet been traversed.
	  ;; -> put us into the callers list of the higher-target.
	  ;; we are technically not calling the higher-target, but this
	  ;; simplifies the code. (we are indirectly calling the target
	  ;; anyways).
	  (with-access::Call/cc-Lambda higher-target (callers)
	     (unless (or (not current-fun) (memq current-fun callers))
		(set! callers (cons current-fun callers))))
	  ;; put us into a 'verify-later-targets' list so we can
	  ;; verify later on, if any of these yet unevaluated
	  ;; functions turned out to contain call/ccs.
	  (with-access::Call/cc-Call call (verify-later-targets)
	     (unless (memq higher-target verify-later-targets)
		(cons-set! verify-later-targets higher-target)))
	  ;; for now don't mark us as call/cc
	  #f)
	 ((and target-var
	       (eq? (Var-kind target-var) 'imported)
	       (Export-Desc-runtime?
		(Var-export-desc target-var)))
	  ;; true if target-var is higher.
	  (Export-Desc-higher? (Var-export-desc target-var)))
	 ((and target-var
	       (eq? (Var-kind target-var) 'imported)
	       (not (Export-Desc-higher?
		     (Var-export-desc target-var)))
	       (not (Mark-Env-extern-always-call/cc? env)))
	  ;; do-nothing
	  #f)
	 (else
	  ;; assume the parameter is unsafe.
	  #t))))

(define (mark-call/cc-fun fun)
   (with-access::Call/cc-Lambda fun (call/cc? callers)
      (unless call/cc?
	 (set! call/cc? #t)
	 (for-each mark-call/cc-fun callers))))

(define-nmethod (Call.mark current-fun)
   (default-walk this current-fun)
   ;; As this is not a Call/cc-Call we do not know the target. -> mark as
   ;; call/cc-call.
   (with-access::Call this (call/cc?)
      (set! call/cc? #t))
   (unless (not current-fun)
      (mark-call/cc-fun current-fun)))

(define-nmethod (Call/cc-Call.mark current-fun)
   (default-walk this current-fun)
   (with-access::Call/cc-Call this (target operands call/cc?)
      (let ((var (constant-var target))
	    (nb-params (length operands)))
	 (cond
	    ;; optim.
	    ((and var
		  (eq? (Var-kind var) 'imported)
		  (Export-Desc-higher? (Var-export-desc var))
		  (Export-Desc-higher-params (Var-export-desc var)))
	     ;; higher-params contains a list of parameters that are treated as
	     ;; functions. If any of them is .call/cc? then the call is unsafe.
	     ;; The other parameters are safe (never invoked).
	     (when (any? (lambda (param-nb)
			    (if (>fx param-nb nb-params)
				;; not enough arguments. This will yield an
				;; error, but not a call/cc-call.
				#f
				(unsafe-param? (list-ref operands param-nb)
					       this current-fun env)))
			 (Export-Desc-higher-params (Var-export-desc var)))
		(unless (not current-fun) (mark-call/cc-fun current-fun))
		(set! call/cc? #t)))
	    ((and var
		  (eq? (Var-kind var) 'imported)
		  (not (Export-Desc-higher? (Var-export-desc var)))
		  (or (Export-Desc-runtime? (Var-export-desc var))
		      (not (Mark-Env-extern-always-call/cc? env))))
	     ;; do nothing. safe call.
	     'do-nothing)
	    ((and (Lambda? target)
		  (not (Lambda-call/cc? target)))
	     ;; the target has not yet been marked as call/cc. However this
	     ;; might happen later on. -> add this fun to the verify list.
	     (with-access::Call/cc-Call this (verify-later-targets)
		(set! verify-later-targets (list target))))
	    (else ;; assume call/cc.
	     (unless (not current-fun) (mark-call/cc-fun current-fun))
	     (set! call/cc? #t))))))

(define-nmethod (Node.finish-marking)
   (default-walk this))

(define-nmethod (Call/cc-Call.finish-marking)
   (default-walk this)
   
   ;; verify-later-targets have been set during first marking pass.
   ;; at that time these targets hadn't been evaluated yet (at least not
   ;; always.)
   ;; if one of them turned out to contain call/cc we must mark this call
   ;; as call/cc.
   ;; the surrounding function however has already been marked correctly.
   (with-access::Call/cc-Call this (verify-later-targets call/cc?)
      (when (any? Lambda-call/cc? verify-later-targets)
	 (set! call/cc? #t))))

(define (shrink tree)
   (verbose " clean nodes")
   (clean tree #f))

(define-nmethod (Node.clean)
   (default-walk this))

(define-nmethod (Call/cc-Call.clean)
   (shrink! this)
   (default-walk this))

(define-nmethod (Call/cc-Lambda.clean)
   (shrink! this)
   (default-walk this))
