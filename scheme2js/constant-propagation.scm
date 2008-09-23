(module constant-propagation
   (import config
	   nodes
	   walk
	   var-ref-util
	   side
	   use-count
	   verbose)
   (export (constant-propagation! tree::Module)))

(define (constant-propagation! tree)
   (if (config 'constant-propagation)
       (unless (config 'call/cc)
	  (verbose "propagation")
	  (side-effect tree)
	  (propagate! tree #f))))

(define-nmethod (Node.propagate!)
   (default-walk! this))

(define-nmethod (Ref.propagate!)
   (let* ((target (transitive-value this)))
      (cond
	 ((Const? target)
	  (instantiate::Const (value (Const-value target))))
	 ((and (Ref? target)
	       (not (eq? this target)))
	  (with-access::Ref target (var)
	     (var-reference var)))
	 (else this))))

(define-nmethod (Set!.propagate!)
   ;; don't visit lvalue
   (with-access::Set! this (val)
      (set! val (walk! val)))
   this)
