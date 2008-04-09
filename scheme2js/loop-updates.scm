(module loop-updates
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   var-ref-util
	   var
	   config
	   verbose)
   (export (loop-updates-free-order loop-vars::pair-nil assigs::pair-nil)))

;; assigs may appear in any order.
;; make the best of it, so we use few temporary variables
(define (loop-updates-free-order loop-vars updates)
   ;; we will work on vars only.
   (for-each (lambda (var update)
		(set! var.new-val update))
	     loop-vars
	     updates)
   (dep-tree loop-vars)
   (break-cycles loop-vars)
   (create-updates loop-vars))

;; builds a dependency tree of the variables. A var v1 is dependent on
;; v2, if v2 appears on the right hand side of the assignment-expression.
;; Exception is v1 itself ('v1 = v1 + 1' wouldn't need a tmp-var).
;; We also need to capture deps like:
;;   (define (loop i j k)
;;     (define (f) (print i j k) (+ i 1))
;;     (loop (f)
;;           (f)
;;           (f)))
;; In this case i, j and k depend on each other.
;;
;; the tree is stored in hashtables directly in the vars:
;;   - var.referenced-vars-ht
;;   - and var.rev-referenced-vars-ht (the back-pointers)
;;
;; this procedure stops at function boundaries.
(define (dep-tree vars)
   (let ((vars-ht (make-eq-hashtable))
	 (escaping-vars (filter (lambda (v)
				   v.escapes?)
				vars)))
      (for-each (lambda (var)
		   (hashtable-put! vars-ht var #t))
		vars)
      
      ;; traverse new values
      (define (potential-uses use-vars self-var ht)
	 (for-each (lambda (v)
		      ;; don't count self.
		      (if (and (not (eq? v self-var))
			       (hashtable-contains? vars-ht v))
			  (hashtable-put! ht v #t)))
		   use-vars))
      
      (define-pmethod (Node-shallow-refs self-var ht)
	 (this.traverse2 self-var ht))
      (define-pmethod (Var-ref-shallow-refs self-var ht)
	 (potential-uses (list this.var) self-var ht))
      (define-pmethod (Call-shallow-refs self-var ht)
	 (this.traverse2 self-var ht)
	 (let* ((op this.operator)
		(target (call-target op)))
	    (cond
	       ((not target)
		(potential-uses escaping-vars self-var ht))
	       ((higher-order-runtime-var-ref? target)
		(potential-uses escaping-vars self-var ht))
	       ((runtime-var-ref? target)
		;; arguments are already taken care of.
		'do-nothing)
	       ((inherits-from? target (node 'Lambda))
		(if target.free-vars?
		    (potential-uses (hashtable-key-list target.free-vars-ht)
				    self-var
				    ht)))
	       (else
		(potential-uses escaping-vars self-var ht)))))
      (define-pmethod (Lambda-shallow-refs self-var ht)
	 'do-not-go-into-lambdas)
      
      (overload traverse shallow-refs (Node
				       Var-ref
				       Call
				       Lambda)
		(for-each (lambda (var)
			     (let* ((value var.new-val)
				    (referenced-vars-ht (make-eq-hashtable)))
				(set! var.referenced-vars-ht referenced-vars-ht)
				(value.traverse var referenced-vars-ht)))
			  vars))
      ;; fill rev-pointers.
      (for-each (lambda (var)
		   (set! var.rev-referenced-vars-ht (make-eq-hashtable)))
		vars)
      (for-each (lambda (var)
		   (hashtable-for-each var.referenced-vars-ht
				       (lambda (v ignored)
					  (hashtable-put!
					   v.rev-referenced-vars-ht
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
	 (hashtable-for-each var.referenced-vars-ht
			     (lambda (v ignored)
				(if v.active?
				    (set! v.cyclic-var? #t)
				    (break-cycle v))))
	 (delete! var.active?)))
   (for-each (lambda (var)
		(break-cycle var))
	     vars)
   (for-each (lambda (var)
		(delete! var.visited?))
	     vars))

(define (clean-var! var)
   (delete! var.new-val)
   (delete! var.referenced-vars-ht)
   (delete! var.rev-referenced-vars-ht)
   (delete! var.cyclic-var?)
   (delete! var.break-var)
   (delete! var.pending?))

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
      ;; TODO decl must be in Let
      (set! new-var-decl.var.new-val cyclic-var.new-val)
      (set! cyclic-var.new-val (new-var-decl.var.reference))
      ;; the cyclic-var is not cyclic anymore, and doesn't depend on
      ;; any variables anymore. We therefore remove the cyclic-var
      ;; from the revdeps of its "ex"-dependencies.
      ;; We don't add these references to the break-vars, as these are
      ;; going to be treated differently anyways, and are going to be
      ;; assigned first. (As we know, that they don't have any revdeps.
      (hashtable-for-each cyclic-var.referenced-vars-ht
			  (lambda (v ignored)
			     (hashtable-remove! v.rev-referenced-vars-ht
						cyclic-var)))
      ;; and we replace the deps by an empty hashtable.
      (set! cyclic-var.referenced-vars-ht (make-eq-hashtable))
      new-var-decl))


;; every var has its new-value and dependencies in it. Some variables are
;; furthermore marked as 'cyclic-var?'. Only these variables should need
;; temporary variables.
(define (create-updates vars)
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
	  ;; break-var-decls are tmp-vars, that are inside a Let.
	  (break-var-decls (map break-var-decl cyclic-vars))
	  (let-vars (map (lambda (decl) decl.var) break-var-decls))
	  (let-bindings (map! (lambda (break-var-decl)
				 (let* ((val break-var-decl.var.new-val))
				    (clean-var! break-var-decl.var)
				    (new-node Binding
					      break-var-decl
					      val)))
			      break-var-decls))
	  ;; now that we have introduced the break-vars we can search for
	  ;; rev-independant variables. IE vars, that are not used by
	  ;; anyone else.
	  (indeps (filter (lambda (var)
			     (= (hashtable-size var.rev-referenced-vars-ht)
				0))
			  vars)))

      ;(verbose "cyclic-vars: " (map (lambda (var) var.id) cyclic-vars))
      ;(verbose "indeps: " (map (lambda (var) var.id) indeps))
      
      ;; the indeps will be our pending set.

      ;; mark all indeps vars as .pending?
      (for-each (lambda (var)
		   (set! var.pending? #t))
		indeps)

      ;; We then add the pending-vars one by one.
      ;; Once we added a pending, we remove its deps in the tree.
      ;; if this leads to free another var, we add it to the pendings.
      (let loop ((pending indeps)
		 (rev-assigs '()))
	 (if (null? pending)
	     (new-node Let
		       let-vars
		       let-bindings
		       (new-node Begin
				 (reverse! rev-assigs))
		       'let)
	     (let* ((var (car pending))
		    (new-val var.new-val))
		;; remove rev-deps.
		(hashtable-for-each var.referenced-vars-ht
				    (lambda (v ignored)
				       (if v.rev-referenced-vars-ht
					   ;; hasn't been cleaned yet
					   (hashtable-remove!
					    v.rev-referenced-vars-ht
					    var))))
		;; retain vars, without rev-dep that aren't yet pending?
		(hashtable-filter! var.referenced-vars-ht
				   (lambda (v ignored)
				      (and (not v.pending?)
					   ;; hasn't been cleaned yet
					   v.rev-referenced-vars-ht
					   (= (hashtable-size
					       v.rev-referenced-vars-ht)
					      0))))
		;; mark them as pending?
		(hashtable-for-each var.referenced-vars-ht
				    (lambda (var ignored)
				       (set! var.pending? #t)))
		(let ((new-pending
		       (append (hashtable-key-list var.referenced-vars-ht)
			       (cdr pending))))
		   (clean-var! var)
		   (loop new-pending
			 (cons (var.assig new-val)
			       rev-assigs))))))))
