(module compile-optimized-boolify
   (export (compile-boolified p
			      env
			      compile::procedure
			      node::Node))
   (import config
	   tools
	   nodes
	   export
	   template-display
	   verbose))

(define (compile-optimized-if-boolify p env compile n)
   (with-access::If n (test then else)
      (if (and (Const? else)
	       (with-access::Const else (value)
		  (not value)))
	  (template-display p env
	     "(~e&&~e)"
	     (compile test p #f)
	     (compile-optimized-boolify p env compile then))
	  (compile-unoptimized-boolify p env compile n))))

(define (compile-optimized-boolify p env compile n)
   (cond
      ((Call? n)
       (with-access::Call n (operator operands)
	  (if (Ref? operator)
	      (with-access::Ref operator (var)
		 (if (and (Exported-Var? var)
			  (Exported-Var-constant? var)
			  (eq? (Export-return-type (Exported-Var-meta var))
			       'bool))
		     (compile n p #f)
		     (compile-unoptimized-boolify p env compile n)))
	      (compile-unoptimized-boolify p env compile n))))
      ((If? n)
       (compile-optimized-if-boolify p env compile n))
      ((Const? n)
       (with-access::Const n (value)
	  (template-display p env
	     "~a" (if value "true" "false"))))
      (else
       (compile-unoptimized-boolify p env compile n))))

(define (compile-unoptimized-boolify p env compile node)
   (template-display p env
      "(~e !== false)"
      (compile node p #f)))
   
(define (compile-boolified p env compile node)
   ;; TODO: get rid of '(config ... )
   (if (config 'optimize-boolify)
       (compile-optimized-boolify p env compile node)
       (compile-unoptimized-boolify p env compile node)))

