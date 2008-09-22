(module compile-optimized-set
   (export (compile-set! p env compile::procedure node::Node))
   (import config
	   tools
	   template-display
	   nodes))

(define *set!-operators*
   '(("sc_plus" "+")
     ("sc_multi" "*")
     ("sc_minus" "-")
     ("sc_div" "/")
     ;; TODO: modulo can't be converted directly to '%'
     ("sc_modulo" "%")
     ("sc_bitAnd" "&")
     ("sc_bitOr" "|")
     ("sc_bitXor" "^")))

(define (compile-optimized-set! p env compile n)
   (with-access::Set! n (lvalue val)
      (with-access::Ref lvalue (var)
	 (if (Call? val)
	     (with-access::Call val (operator operands)
		(if (and (not (null? operands))
			 (not (null? (cdr operands)))
			 ;; next test not strictly necessary, but
			 ;; simplifies cases like "(set! x (- x 1 2 3))"
			 (null? (cddr operands))
			 (Ref? (car operands))
			 (eq? (Ref-var (car operands)) var)
			 (Ref? operator)
			 (let ((op-var (Ref-var operator)))
			    (with-access::Var op-var (constant?)
			       (and (Imported-Var? op-var)
				    constant?))))
		    (let* ((op-var (Ref-var operator))
			   (js-id (Imported-Var-js-id op-var))
			   (entry (assoc js-id *set!-operators*)))
		       (if entry
			   ;; get ++ and --
			   (if (and (or (string=? (cadr entry) "+")
					(string=? (cadr entry) "-"))
				    (Const? (cadr operands))
				    (eq? 1 (Const-value (cadr operands))))
			       (template-display p env
				  "(~a~a~a)"
				  (cadr entry) (cadr entry) ;; ++ or --
				  (Var-compiled var))
			       (with-access::Var var (compiled)
				  (template-display p env
				     "($compiled ~a= ~e)"
				     (cadr entry)
				     (compile (cadr operands) p #f))))
			   (compile-unoptimized-set! p env compile n)))
		    (compile-unoptimized-set! p env compile n)))
	     (compile-unoptimized-set! p env compile n)))))

(define (compile-unoptimized-set! p env compile n)
   (with-access::Set! n (lvalue val)
      (template-display p env
	 "~e = ~e"
	 (compile lvalue p #f)
	 (compile val p #f))))

(define (compile-set! p env compile n)
   ;; TODO: get rid of '(config ...)
   (if (config 'optimize-set!)
       (compile-optimized-set! p env compile n)
       (compile-unoptimized-set! p env compile n)))
