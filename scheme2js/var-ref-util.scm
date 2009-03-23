(module var-ref-util
   (import config
	   tools
	   nodes
	   export-desc)
   (export (constant-var n)
	   (runtime-ref-var n)
	   (call-target operator::Node)
	   (runtime-ref-var-id n)
	   (runtime-ref?::bool n)
	   (higher-order-runtime-ref?::bool n)
	   (transitive-value var-ref::Ref)))

(define (constant-var n)
   (and n
	(Ref? n)
	(with-access::Ref n (var)
	   (with-access::Var var (constant?)
	      (and constant?
		   var)))))

;; if n is a runtime-ref then return the var. otherwise #f
(define (runtime-ref-var n)
   (let ((v (constant-var n)))
      (and v
	   (eq? (Var-kind v) 'imported)
	   (Export-Desc-runtime? (Var-export-desc v))
	   v)))

(define (runtime-ref? n)
   (and (runtime-ref-var n)
	#t))

(define (higher-order-runtime-ref? n)
   (let ((v (runtime-ref-var n)))
      (and v
	   (Export-Desc-higher? (Var-export-desc v))
	   #t)))

(define (runtime-ref-var-id n)
   (let ((v (runtime-ref-var n)))
      (and v
	   (Var-id v))))

(define (call-target operator)
   (cond
      ((Lambda? operator)
       operator)
      ((runtime-ref? operator)
       operator)
      ((and (Ref? operator)
	    (with-access::Ref operator (var)
	       (with-access::Var var (constant? value)
		  (and constant? value))))
       (with-access::Ref operator (var)
	  (with-access::Var var (value)
	     (call-target value))))
      (else
       #f)))

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
			 ;; do not propagate vectors and lists.
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
