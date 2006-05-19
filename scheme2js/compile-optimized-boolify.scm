;; part of compile-module

(define *bool-operators* '(
			   sci_isEq
			   sci_isEqv
			   sci_isEqual
			   sci_isNumber
			   sci_isComplex
			   sci_isReal
			   sci_isRational
			   sci_isInteger
			   sci_isExact
			   sci_isInexact
			   sci_equal
			   sci_less
			   sci_greater
			   sci_lessEqual
			   sci_greaterEqual
			   sci_isPositive
			   sci_isNegative
			   sci_not
			   sci_cons
			   sci_isPair
			   sci_isNull
			   sci_isSymbol
			   sci_isString
			   sci_isStringEqual
			   sci_isStringLess
			   sci_isStringGreater
			   sci_isStringLessEqual
			   sci_isStringGreaterEqual
			   sci_isVector
			   sci_isProcedure
			   ))

(define (compile-optimized-boolify compiled::bstring n)
   (if (inherits-from? n (node 'Call))
       (let ((op n.operator))
	  (if (inherits-from? op (node 'Var-ref))
	      (let* ((var op.var)
		     (id var.js-id))
		 (if (and (not var.muted?)
			  id
			  (memq id *bool-operators*))
		     compiled
		     (gen-code-boolify compiled)))
	      (gen-code-boolify compiled)))
       (gen-code-boolify compiled)))
