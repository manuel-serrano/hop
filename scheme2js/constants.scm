(module constants
   (import config
	   tools
	   nodes
	   export-desc
	   walk
	   verbose)
   (export (constants! tree::Module)))

(define (constants! tree)
   (verbose "constants")
   (when (config 'optimize-consts)
      (const! tree #f #f)))

(define-nmethod (Node.const! constant-ht)
   (default-walk! this constant-ht))

(define (make-constants-let constants-ht body)
   (let ((bindings (hashtable-map
		    constants-ht
		    (lambda (const decl)
		       (instantiate::Set!
			  (lvalue decl)
			  (val (instantiate::Const
				  (value const))))))))
      (if (pair? bindings)
	  (instantiate::Let
	     (scope-vars (map (lambda (binding)
				 (with-access::Set! binding (lvalue)
				    (with-access::Ref lvalue (var)
				       var)))
			      bindings))
	     (bindings bindings)
	     (body body)
	     (kind 'let))
	  body)))
   
(define-nmethod (Module.const! ht)
   (with-access::Module this (body)
      (if (config 'encapsulate-modules)
	  (default-walk! this #f)
	  (let ((ht (make-hashtable)))
	     (default-walk! this ht)
	     (set! body (make-constants-let ht body))
	     this))))

(define-nmethod (Lambda.const! ht)
   (with-access::Lambda this (body)
      (if ht ;; either module or another lambda already created the ht)
	  (default-walk! this ht)
	  (let ((ht (make-hashtable)))
	     (default-walk! this ht)
	     ;; Lambda-body must be Return.
	     (with-access::Return body (val)
		(set! val (make-constants-let ht val)))
	     this))))

(define-nmethod (Const.const! constant-ht)
   (define (long-enough-list? l)
      (let loop ((l l)
		 (nb 0))
	 (cond
	    ((> nb 5) #t)
	    ((and (pair? l)
		  (or (pair? (car l))
		      (vector? (car l))))
	     #t)
	    ((pair? l)
	     (loop (cdr l) (+ nb 1)))
	    ((vector? l)
	     #t)
	    (else
	     #f))))
   (define (long-enough-vector? v)
      (or (> (vector-length v) 5)
	  (let loop ((i 0))
	     (cond
		((>= i (vector-length v))
		 #f)
		((or (pair? (vector-ref v i))
		     (vector? (vector-ref v i)))
		 #t)
		(else (loop (+ i 1)))))))
   (with-access::Const this (value)
      (if (or (and (pair? value)
		   (long-enough-list? value))
	      (and (vector? value)
		   (long-enough-vector? value)))
	  (let ((cached (hashtable-get constant-ht value)))
	     (if cached
		 (with-access::Ref cached (var)
		    (var-reference var))
		 (let ((new-const (Ref-of-new-Var 'const)))
		    (hashtable-put! constant-ht value new-const)
		    (with-access::Ref new-const (var)
		       (var-reference var)))))
	  this)))
