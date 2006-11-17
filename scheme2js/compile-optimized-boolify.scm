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
			   sci_isZero
			   sci_isPositive
			   sci_isNegative
			   sci_isOdd
			   sci_isEven
			   sci_not
			   sci_cons
			   sci_isPair
			   sci_isNull
			   sci_isList
			   sci_isChar
			   sci_isCharEqual
			   sci_isCharLess
			   sci_isCharGreater
			   sci_isCharLessEqual
			   sci_isCharGreaterEqual
			   sci_isCharCIEqual
			   sci_isCharCILess
			   sci_isCharCIGreater
			   sci_isCharCILessEqual
			   sci_isCharCIGreaterEqual
			   sci_isSymbol_mutable
			   sci_isString_mutable
			   sci_isStringEqual_mutable
			   sci_isStringLess_mutable
			   sci_isStringGreater_mutable
			   sci_isStringLessEqual_mutable
			   sci_isStringGreaterEqual_mutable
			   sci_isStringCIEqual_mutable
			   sci_isStringCILess_mutable
			   sci_isStringCIGreater_mutable
			   sci_isStringCILessEqual_mutable
			   sci_isStringCIGreaterEqual_mutable
			   sci_isSymbol_immutable
			   sci_isString_immutable
			   sci_isStringEqual_immutable
			   sci_isStringLess_immutable
			   sci_isStringGreater_immutable
			   sci_isStringLessEqual_immutable
			   sci_isStringGreaterEqual_immutable
			   sci_isStringCIEqual_immutable
			   sci_isStringCILess_immutable
			   sci_isStringCIGreater_immutable
			   sci_isStringCILessEqual_immutable
			   sci_isStringCIGreaterEqual_immutable
			   sci_isVector
			   sci_isProcedure
			   sci_isStruct
			   sci_isCharReady
			   sci_isEOFObject
			   sci_isInputPort
			   sci_isOutputPort
			   ))

(define (compile-optimized-if-boolify p n)
   (if (and (inherits-from? n.else (node 'Const))
	    (not n.else.value))
       (begin
	  (p-display p "(")
	  (n.test.compile p)
	  (p-display p "&&")
	  (compile-optimized-boolify p n.then)
	  (p-display p ")"))
       (compile-unoptimized-boolify p n)))

(define (compile-optimized-boolify p n)
   (cond
      ((inherits-from? n (node 'Call))
       (let ((op n.operator))
	  (if (inherits-from? op (node 'Var-ref))
	      (let* ((var op.var)
		     (id var.js-id))
		 (if (and (inherits-from? var (node 'JS-Var)) ;; should be true
			  (not var.muted?)
			  id
			  (memq id *bool-operators*))
		     (n.compile p)
		     (compile-unoptimized-boolify p n)))
	      (compile-unoptimized-boolify p n))))
      ((inherits-from? n (node 'If))
       (compile-optimized-if-boolify p n))
      (else
       (compile-unoptimized-boolify p n))))
