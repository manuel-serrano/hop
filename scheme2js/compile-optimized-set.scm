(module compile-optimized-set
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (export (compile-set! p node::pobject))
   (import protobject
	   config
	   nodes
	   var
	   verbose))

(define *set!-operators*
   '((sc_plus "+")
     (sc_multi "*")
     (sc_minus "-")
     (sc_div "/")
     ;; TODO: modulo can't be converted directly to '%'
     (sc_modulo "%")
     (sc_bitAnd "&")
     (sc_bitOr "|")
     (sc_bitXor "^")))

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
		      operator.var.constant?)
		 (let ((entry (assq operator.var.js-id *set!-operators*)))
		    (if entry
			;; get ++ and --
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

(define (compile-unoptimized-set! p n)
   (p-display p "(")
   (n.lvalue.compile p)
   (p-display p " = ")
   (n.val.compile p)
   (p-display p ")"))

(define (compile-set! p n)
   (if (config 'optimize-set!)
       (compile-optimized-set! p n)
       (compile-unoptimized-set! p n)))
