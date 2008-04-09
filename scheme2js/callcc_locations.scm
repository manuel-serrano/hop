(module callcc-locations
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes
	   side
	   free-vars
	   locals
	   var
	   var-ref-util
	   verbose)
   (export (call/cc-locations tree::pobject)))

;; sets .call/cc? for calls and functions that might reach call/cc.
(define (call/cc-locations tree)
   (when (config 'suspend/resume)
      (verbose "call/cc locations")
      (side-effect tree)
      (rev-call-tree tree)
      (callcc-typing tree)
      (clean-callers tree)))

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
      (when target
	 (set! this.target target)
	 (if (inherits-from? target (node 'Lambda))
	     (set! target.callers (cons current-fun
					(or target.callers '())))))))

;; 'typing' is probably the wrong term...
(define (callcc-typing tree)
   (verbose " call/cc typing")
   (overload traverse type (Node
			    Module
			    Lambda
			    Call)
	     (tree.traverse #f))
   (overload traverse finish-type (Node
				   Call)
	     (tree.traverse)))

(define-pmethod (Node-type current-fun)
   (this.traverse1 current-fun))

;; Module is not a function, but in this context we'll treat it like one.
(define-pmethod (Module-type current-fun)
   (this.traverse1 this))

(define-pmethod (Lambda-type current-fun)
   (this.traverse1 this))

(define-pmethod (Call-type current-fun)
   (define (mark-call/cc-fun fun)
      (unless fun.call/cc?
	 (set! fun.call/cc? #t)
	 (if fun.callers (for-each (lambda (fun)
				      (mark-call/cc-fun fun))
				   fun.callers))))

   (this.traverse1 current-fun)
   (let* ((target this.target)
	  (var (constant-var target)))
      (cond
	 ;; optim.
	 ((and var
	       var.higher?
	       var.higher-params)
	  (if (any?
	       (lambda (which)
		  (let* ((higher-target (call-target (list-ref this.operands which)))
			 (target-var (constant-var higher-target)))
		     (cond
			((and (inherits-from? higher-target (node 'Lambda))
			      (not higher-target.call/cc?))
			 ;; put us into the callers list of the
			 ;; higher-target, in case it hasn't been traversed
			 ;; yet.
			 ;; we are technically not calling the higher-target,
			 ;; but this simplifies the code.
			 (set! higher-target.callers
			       (cons current-fun
				     (or higher-target.callers
					 '())))
			 ;; put us into a 'higher-targets' list so we can
			 ;; verify later on, if any of these yet unevaluated
			 ;; functions turned out to contain call/ccs.
			 (set! this.higher-targets
			       (cons higher-target
				     (or this.higher-targets
					 '())))
			 ;; for now don't mark us as call/cc
			 #f)
			((and target-var
			      target-var.runtime?)
			 ;; true if target-var is higher.
			 ;; we could optimize even more...
			 ;; TODO: optimize more.
			 target-var.higher?)
			((and target-var
			      target-var.extern?
			      (not target-var.higher?)
			      (not (config 'extern-always-call/cc)))
			 ;; do-nothing
			 #f)
			(else
			 ;; mark call/cc-fun and abort traversal of list.
			 #t))))
	       var.higher-params)
	      (begin
		 (mark-call/cc-fun current-fun)
		 (set! this.call/cc? #t))))
; 	 ((not target)
; 	  (print "not target")
; 	  #f)
; 	 (target.potential-call/cc?
; 	  (print "potential")
; 	  #f)
; 	 ((and var var.higher)
; 	  (print "higher"))
; 	 ((and var var.extern (not var.runtime?) (config
;    'extern-always-call/cc))
; 	  (print "long one"))
	 ((or (not target) ;; assume call/cc
	      target.call/cc?
	      (and var var.higher?)
	      (and var
		   var.extern
		   (not var.runtime?)
		   (config 'extern-always-call/cc)))
	  (mark-call/cc-fun current-fun)
	  (set! this.call/cc? #t))
	 (else
	  'do-nothing))))

(define-pmethod (Node-finish-type)
   (this.traverse0))

(define-pmethod (Call-finish-type)
   (this.traverse0)
   
   ;; higher-targets have been set during typing pass.
   ;; at that time these targets hadn't been evaluated yet (at least not
   ;; always.)
   ;; if one of them turned out to contain call/cc we must mark this call
   ;; as call/cc.
   ;; the surrounding function however has already been marked correctly.
   (let* ((higher-targets this.higher-targets))
      (if (and higher-targets
	       (any? (lambda (n)
			n.call/cc?)
		     higher-targets))
	  (set! this.call/cc? #t)))
   ;; same is true, if the call-target has been marked later on.
   ;; TODO: can this actually happen?
   (let ((target this.target))
      (if (and target target.call/cc?)
	  (set! this.call/cc? #t))))

(define (clean-callers tree)
   (verbose " clean nodes")
   (overload traverse clean-callers (Node Lambda)
	     (tree.traverse)))

(define-pmethod (Node-clean-callers)
   (this.traverse0))

(define-pmethod (Lambda-clean-callers)
   (delete! this.callers)
   (this.traverse0))


