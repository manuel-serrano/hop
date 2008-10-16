(module loop-updates
   (import config
	   tools
	   nodes
	   export
	   walk
	   var-ref-util
	   verbose)
   (static (wide-class Update-Var::Local
	      new-val
	      (referenced-vars (default #f))
	      (rev-referenced-vars (default #f))
	      (cyclic-var? (default #f))
	      (active? (default #f))
	      (visited? (default #f))
	      (break-var (default #f))
	      (pending? (default #f)))
	   (class Shallow-Env
	      escaping-vars)
	   (class List-Box
	      l::pair-nil))
   (export (loop-updates-free-order loop-vars::pair-nil assigs::pair-nil)))

;; assigs may appear in any order.
;; make the best of it, so we use few temporary variables
(define (loop-updates-free-order loop-vars updates)
   ;; we will work on vars only.
   (for-each (lambda (var update)
		(widen!::Update-Var var
		   (new-val update)))
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
;; the tree is stored in lists directly in the vars:
;;   - var.referenced-vars
;;   - and var.rev-referenced-vars (the back-pointers)
;;
;; this procedure stops at function boundaries.
(define (dep-tree vars)
   (let* ((escaping-vars (filter Var-escapes? vars))
	  (shallow-refs-env (make-Shallow-Env escaping-vars)))

      (for-each (lambda (var)
		   (with-access::Update-Var var (new-val referenced-vars)
		      (let ((box (make-List-Box '())))
			 (shallow-refs new-val shallow-refs-env var box)
			 (set! referenced-vars (List-Box-l box)))))
		vars)
      ;; fill rev-pointers.
      (for-each (lambda (var)
		   (with-access::Update-Var var (rev-referenced-vars)
		      (set! rev-referenced-vars '())))
		vars)
      (for-each (lambda (var)
		   (with-access::Update-Var var (referenced-vars)
		      (for-each
		       (lambda (v)
			  (with-access::Update-Var v (rev-referenced-vars)
			     (unless (memq v rev-referenced-vars)
				(cons-set! rev-referenced-vars var))))
		       referenced-vars)))
		vars)))

;; traverse new values
(define (potential-uses use-vars self-var b::List-Box)
   (with-access::List-Box b (l)
      (for-each (lambda (v)
		   ;; don't count self.
		   (when (and (not (eq? v self-var))
			      (Update-Var? v)
			      (not (memq v l)))
		      (cons-set! l v)))
	     use-vars)))
      
(define-nmethod (Node.shallow-refs self-var b::List-Box)
   (default-walk this self-var b))
(define-nmethod (Ref.shallow-refs self-var b::List-Box)
   (with-access::Ref this (var)
      (potential-uses (list var) self-var b)))
(define-nmethod (Call.shallow-refs self-var b)
   (default-walk this self-var b)
   (with-access::Call this (operator)
      (let ((target (call-target operator)))
	 (cond
	    ((not target)
	     (potential-uses (Shallow-Env-escaping-vars env) self-var b))
	    ((higher-order-runtime-var-ref? target)
	     (potential-uses (Shallow-Env-escaping-vars env) self-var b))
	    ((runtime-var-ref? target)
	     ;; arguments are already taken care of.
	     'do-nothing)
	    ((Lambda? target)
	     (with-access::Lambda target (free-vars)
		(unless  (null? free-vars)
		   (potential-uses free-vars self-var b))))
	    (else
	     (potential-uses (Shallow-Env-escaping-vars env) self-var b))))))
(define-nmethod (Lambda.shallow-refs self-var ht)
   'do-not-go-into-lambdas)

;; breaks dependency cycles, by marking cyclic vars, that should be
;; replaced by a tmp-var. (marked with 'var.cyclic-var?'
(define (break-cycles vars)
   ;; TODO: replace this suboptimal algo (break-cycles)
   (define (break-cycle var)
      (with-access::Update-Var var (visited? active? referenced-vars)
	 (unless visited?
	    (set! active? #t)
	    (set! visited? #t)
	    (for-each (lambda (v)
			 (with-access::Update-Var v
			       (active? cyclic-var?)
			    (if active?
				(set! cyclic-var? #t)
				(break-cycle v))))
		      referenced-vars)
	    (set! active? #f))))
   (for-each break-cycle vars))

(define (clean-var! var::Update-Var)
   (shrink! var))

;; creates break-var, and breaks cycle (decreasing rev-dep.
;; ex: var x should be broken:
;;         tmp-x.new-val <- x.new-val
;;         x.new-val <- tmp-x
;; (modulo taking the reference....)
;; At the same time x does become completely independent of any other
;; variable. (except tmp-x, which is going to be treated at the very
;; beginning anyways).
(define (break-var-decl cyclic-var)
   (with-access::Update-Var cyclic-var (id new-val referenced-vars)
      (let ((new-var-decl (Ref-of-new-Var id)))
	 ;; decls will be put into Lets later on.
	 (with-access::Ref new-var-decl (var)
	    (widen!::Update-Var var
	       (new-val new-val))
	    (set! new-val (var-reference var)))
	 ;; the cyclic-var is not cyclic anymore, and doesn't depend on
	 ;; any variables anymore. We therefore remove the cyclic-var
	 ;; from the revdeps of its "ex"-dependencies.
	 ;; We don't add these references to the break-vars, as these are
	 ;; going to be treated differently anyways, and are going to be
	 ;; assigned first. (As we know, that they don't have any revdeps.)
	 (for-each (lambda (v)
		      (with-access::Update-Var v (rev-referenced-vars)
			 (set! rev-referenced-vars
			       (remq! cyclic-var rev-referenced-vars))))
		   referenced-vars)
	  
	 ;; and we replace the deps by an empty list.
	 (set! referenced-vars '())
	 new-var-decl)))


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
   (let* ((cyclic-vars (filter Update-Var-cyclic-var? vars))
	  ;; break-var-decls are tmp-vars, that are inside a Let.
	  (break-var-decls (map break-var-decl cyclic-vars))
	  (let-vars (map Ref-var break-var-decls))
	  (let-bindings (map! (lambda (break-var-decl)
				 (with-access::Ref break-var-decl (var)
				    (with-access::Update-Var var (new-val)
				       (begin0
					(instantiate::Set!
					   (lvalue break-var-decl)
					   (val new-val))
					(clean-var! var)))))
			      break-var-decls))
	  ;; now that we have introduced the break-vars we can search for
	  ;; rev-independant variables. i.e. vars, that are not used by
	  ;; anyone else.
	  (indeps (filter (lambda (var)
			     (with-access::Update-Var var (rev-referenced-vars)
				(null? rev-referenced-vars)))
			  vars)))

      ;; the indeps will be our pending set.

      ;; mark all indeps vars as .pending?
      (for-each (lambda (var)
		   (with-access::Update-Var var (pending?)
		      (set! pending? #t)))
		indeps)

      ;; We then add the pending-vars one by one.
      ;; Once we added a pending, we remove its deps in the tree.
      ;; if this leads to free another var, we add it to the pendings.
      (let loop ((pending indeps)
		 (rev-assigs '()))
	 (if (null? pending)
	     (instantiate::Let
		(scope-vars let-vars)
		(bindings let-bindings)
		(body (instantiate::Begin
			 (exprs (reverse! rev-assigs))))
		(kind 'let))
	     (let ((var (car pending)))
		(with-access::Update-Var var (new-val referenced-vars)
		   ;; remove rev-deps.
		   (for-each
		    (lambda (v)
		       (when (Update-Var? v)
			  ;; hasn't been cleaned yet
			  (with-access::Update-Var v (rev-referenced-vars)
			     (set! rev-referenced-vars
				   (remq! var rev-referenced-vars)))))
		    referenced-vars)
		   ;; retain vars, without rev-dep, that aren't yet .pending?
		   (set! referenced-vars
			 (filter!
			  (lambda (v)
			     (with-access::Update-Var v (pending?
							 rev-referenced-vars)
				(and (not pending?)
				     ;; hasn't been cleaned yet
				     rev-referenced-vars
				     (null? rev-referenced-vars))))
			  referenced-vars))

		   ;; mark them as .pending?
		   (for-each (lambda (var)
				(with-access::Update-Var var (pending?)
				   (set! pending? #t)))
			     referenced-vars)

		   (let ((new-pending (append referenced-vars
					      (cdr pending)))
			 (new-val-tmp new-val)) ;; will be erased by clean-var!
		      (clean-var! var)
		      (loop new-pending
			    (cons (var-assig var new-val-tmp)
				  rev-assigs)))))))))
