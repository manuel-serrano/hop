(module var-ref-util
   (import config
	   tools
	   nodes)
   (export (constant-var n)
	   (runtime-var n)
	   (call-target operator::Node)
	   (runtime-var-ref-id n)
	   (runtime-var-ref? n)
	   (higher-order-runtime-var-ref? n)
	   (transitive-value var-ref::Ref)))

(define (constant-var n)
   (and n
	(Ref? n)
	(with-access::Ref n (var)
	   (with-access::Var var (constant?)
	      (and constant?
		   var)))))

(define (runtime-var n)
   (let ((v (constant-var n)))
      (and (Imported-Var? v)
	   (Imported-Var-runtime? v)
	   v)))

(define (runtime-var-ref? n)
   (and (runtime-var n)
	#t))

(define (higher-order-runtime-var-ref? n)
   (let ((v (runtime-var n)))
      (and v
	   (Imported-Var-higher? v)
	   #t)))

(define (runtime-var-ref-id n)
   (let ((v (runtime-var n)))
      (and v
	   (Var-id v))))

(define (call-target operator)
   (cond
      ((Lambda? operator)
       operator)
      ((runtime-var-ref-id operator)
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
   (if (runtime-var-ref? var-ref)
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
				 (not (This-Var? var))))))
		 (transitive-value value))
		(else var-ref))))))
