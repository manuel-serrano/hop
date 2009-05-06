(module constant-propagation
   (import config
	   nodes
	   export-desc
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


(define (transitive-value var-ref::Ref)
   (if (runtime-ref? var-ref)
       var-ref
       (with-access::Ref var-ref (var)
	  (with-access::Var var (constant? value)
	     (cond
		((and constant?
		      value
		      (Const? value)
		      (let ((const (Const-value value)))
			 ;; do not propagate vectors, lists and
			 ;; strings. Otherwise 'eq?' might not work anymore.
			 ;; Also strings can be quite long.
			 (or (number? const)  
			     (symbol? const)
			     (char? const)
			     (boolean? const)
			     (eqv? #unspecified const))))
		 value)
		((and constant?
		      value
		      (Ref? value)
		      (with-access::Ref value (var)
			 (with-access::Var var (constant?)
			    (and constant?
				 (not (eq? (Var-kind var) 'this))))))
		 (transitive-value value))
		(else var-ref))))))

(define-nmethod (Ref.propagate!)
   (let* ((target (transitive-value this)))
      (cond
	 ((Const? target)
	  (instantiate::Const
	     (location (Node-location target))
	     (value (Const-value target))))
	 ((and (Ref? target)
	       (not (eq? this target)))
	  (with-access::Ref target (var)
	     (var-reference var :location target)))
	 (else this))))

(define-nmethod (Set!.propagate!)
   ;; don't visit lvalue
   (with-access::Set! this (val)
      (set! val (walk! val)))
   this)
