;; part of compile-module

(define *set!-operators*
   '((sci_plus "+")
     (sci_multi "*")
     (sci_minus "-")
     (sci_div "/")
     ;; TODO: modulo can't be converted directly to '%'
     (sci_modulo "%")
     (sci_bitAnd "&")
     (sci_bitOr "|")
     (sci_bitXor "^")))

(define (compile-optimized-set! p n)
   (let ((lvar n.lvalue.var)
	 (rvalue n.val))
      (if (inherits-from? rvalue (node 'Call))
	  (let ((operator rvalue.operator)
		(operands rvalue.operands))
	     (if (and (not (null? operands))
		      (not (null? (cdr operands)))
		      ;; next test not strictly necessary, but
		      ;; simplifies cases like "(set! x (- x 1 2 3))"
		      (null? (cddr operands))
		      (inherits-from? (car operands) (node 'Var-ref))
		      (eq? (car operands).var lvar)
		      (inherits-from? operator (node 'Var-ref))
		      (not operator.var.muted?))
		 (let ((entry (assq operator.var.js-id *set!-operators*)))
		    (if entry
			(if (and (or (string=? (cadr entry) "+")
				     (string=? (cadr entry) "-"))
				 (inherits-from? (cadr operands) (node 'Const))
				 (eq? (cadr operands).value 1))
			    (p-display p
				       "(" (cadr entry) (cadr entry)
				       lvar.compiled ")")
			    (begin
			       (p-display p "(" lvar.compiled " "
					  (cadr entry) "=")
			       ((cadr operands).compile p)
			       (p-display p ")")))
			(compile-unoptimized-set! p n)))
		 (compile-unoptimized-set! p n)))
	  (compile-unoptimized-set! p n))))
