(directives
   (include "tools.sch"))

;; part of compile-module
(define (infix-op nb-operands-min nb-operands-max infix-operator . Ldefault-val)
   (lambda (p operands)
      (let ((nb-operands (length operands)))
	 (and (>=fx nb-operands nb-operands-min)
	      (or (not nb-operands-max)
		  (<=fx nb-operands nb-operands-max))
	      (if (=fx nb-operands 0)
		  (p-display p (car Ldefault-val))
		  (begin
		     (p-display p "(")
		     ((car operands).compile p)
		     (let loop ((rest (cdr operands)))
			(unless (null? rest)
			   (p-display p infix-operator)
			   ((car rest).compile p)
			   (loop (cdr rest))))
		     (p-display p ")")
		     #t))))))

(define (postfix-op postfix-operator)
   (lambda (p operands)
      (and (pair? operands)
	   (null? (cdr operands))
	   (begin
	      (p-display p "(")
	      ((car operands).compile p)
	      (p-display p postfix-operator ")")
	      #t))))

(define (prefix-op prefix-operator)
   (lambda (p operands)
      (and (pair? operands)
	   (null? (cdr operands))
	   (begin
	      (p-display p "(" prefix-operator)
	      ((car operands).compile p)
	      (p-display p ")")
	      #t))))

(define (hole-op nb-holes . pattern)
   (lambda (p operands)
      (and (=fx (length operands) nb-holes)
	   (begin
	      (p-display p "(")
	      (let loop ((pattern pattern)
			 (operands operands))
		 (cond
		    ((null? pattern)
		     'do-nothing)
		    ((string? (car pattern))
		     (p-display p (car pattern))
		     (loop (cdr pattern)
			   operands))
		    ((number? (car pattern))
		     ((list-ref operands (car pattern)).compile p)
		     (loop (cdr pattern)
			   operands))
		    (else
		     ((car operands).compile p)
		     (loop (cdr pattern)
			   (cdr operands)))))
	      (p-display p ")")
	      #t))))

(define (minus-op p operands)
   (cond
      ((null? operands) #f)
      ((null? (cdr operands))
       (p-display p "(- ")
       ((car operands).compile p)
       (p-display p ")")
       #t)
      (else
       ((infix-op 1 #f "-") p operands))))

(define (div-op p operands)
   (cond
      ((null? operands) #f)
      ((null? (cdr operands))
       (p-display p "(1/")
       ((car operands).compile p)
       (p-display p ")")
       #t)
      (else
       ((infix-op 1 #f "/") p operands))))

(define (vector-op p operands)
   (p-display p "[")
   (compile-separated-list p operands ", ")
   (p-display p "]")
   #t)
		  
(define (id p operand)
   (and (pair? operand)
	(null? (cdr operand))
	(begin
	   ((car operand).compile p)
	   #t)))

(define (jsNew-op p operands)
   (and (not (null? operands))
	(begin
	   (p-display p "new ")
	   ((car operands).compile p)
	   (p-display p "(")
	   (compile-separated-list p (cdr operands) ", ")
	   (p-display p ")")
	   #t)))

(define (jsCall-op p operands)
   (and (not (null? operands))
	(not (null? (cdr operands)))
	(begin
	   ((cadr operands).compile p)
	   (p-display p ".call(")
	   ((car operands.compile) p)
	   (for-each (lambda (n)
			(p-display p ", ")
			(n.compile p))
		     (cddr operands))
	   (p-display p ")")
	   #t)))

(define (jsMethodCall-op p operands)
   (and (not (null? operands))
	(not (null? (cdr operands)))
	(begin
	   ((car operands).compile p)
	   (p-display p "[")
	   ((cadr operands).compile p)
	   (p-display p "](")
	   (compile-separated-list p (cddr operands) ", ")
	   (p-display p ")")
	   #t)))

(define (symbolAppend_immutable-op p operands)
   (let ((nb-operands (length operands)))
      (cond
	 ((= nb-operands 0) "'\\u1E9C'") ;; sc_SYMBOL_PREFIX
	 ((= nb-operands 1) ((car operands).compile p))
	 (else
	  (p-display p "(")
	  ((car operands).compile p)
	  (for-each (lambda (operand)
		       (p-display p "+")
		       (if (instance-of? operand (node 'Const))
			   (if (symbol? operand.value)
			       operand.value
			       (error "symbolAppend_immutable-op"
				      "symbol-append requires symbols as arguments"
				      operand.value))
			   (begin
			      (operand.compile p)
			      (p-display p ".slice(1)"))))
		    (cdr operands))
	  (p-display p ")")
	  #t))))

(define (stringAppend_mutable-op p operands)
   (define (string-val operand)
      (if (instance-of? operand (node 'Const))
	  (if (string? operand.value)
	      (p-display p "\"" (string-for-read operand.value) "\"")
	      (error "stringAppend_mutable-op"
		     "string-append requires strings as arguments"
		     operand.value))
	  (begin
	     (operand.compile p)
	     (p-display p ".val"))))

   (let ((nb-operands (length operands)))
      (cond
	 ((= nb-operands 0) "(new sc_String(''))")
	 ((= nb-operands 1) ((car operands).compile p))
	 (else
	  (p-display p "(new sc_String(")
	  (string-val (car operands))
	  (for-each (lambda (n)
		       (p-display p "+")
		       (string-val n))
		    (cdr operands))
	  (p-display p "))")))))


(define (modulo-op p operands)
   (if (config 'correct-modulo)
       #f
       ((infix-op 2 2 "%") p operands)))

(define *optimizable-operators*
   `(
    (sci_isEq ,(infix-op 2 2 "==="))
    (sci_isEqv ,(infix-op 2 2 "==="))

    (sci_equal ,(infix-op 2 2 "==="))
    (sci_less ,(infix-op 2 2 "<"))
    (sci_greater  ,(infix-op 2 2 ">"))
    (sci_lessEqual ,(infix-op 2 2 "<="))
    (sci_greaterEqual ,(infix-op 2 2 ">="))
    (sci_isPositive ,(postfix-op "> 0"))
    (sci_isNegative ,(postfix-op "< 0"))
    (sci_plus ,(infix-op 0 #f "+" "0"))
    (sci_multi ,(infix-op 0 #f "*" "1"))
    (sci_minus ,minus-op)
    (sci_div ,div-op)

    (sci_modulo ,modulo-op)
    
    (sci_quotient ,(hole-op 2 "parseInt(" 'x "/" 'y ")"))
    (sci_bitAnd ,(infix-op 2 2 "&"))
    (sci_bitOr ,(infix-op 2 2 "|"))
    (sci_bitXor ,(infix-op 2 2 "^"))
    (sci_bitLsh ,(infix-op 2 2 "<<"))
    (sci_bitRsh ,(infix-op 2 2 ">>"))
    (sci_bitUrsh ,(infix-op 2 2 ">>>"))
    
    (sci_exact2inexact ,id)
    (sci_inexact2exact ,id)
    
    ;; TODO: merge somehow with compile-optimize-bool
    (sci_not ,(postfix-op "=== false"))
    
    (sci_cons ,(hole-op 2 "new sc_Pair(" 'car ", " 'cdr ")"))
    (sci_isPair ,(postfix-op " instanceof sc_Pair"))
    (sci_car ,(postfix-op ".car"))
    (sci_cdr ,(postfix-op ".cdr"))
    (sci_setCar ,(hole-op 2 'p ".car = " 'val))
    (sci_setCdr ,(hole-op 2 'p ".cdr = " 'val))
    
    (sci_caar ,(postfix-op ".car.car"))
    (sci_cadr ,(postfix-op ".cdr.car"))
    (sci_cdar ,(postfix-op ".car.cdr"))
    (sci_cddr ,(postfix-op ".cdr.cdr"))
    (sci_caaar ,(postfix-op ".car.car.car"))
    (sci_cadar ,(postfix-op ".car.cdr.car"))
    (sci_caadr ,(postfix-op ".cdr.car.car"))
    (sci_caddr ,(postfix-op ".cdr.cdr.car"))
    (sci_cdaar ,(postfix-op ".car.car.cdr"))
    (sci_cdadr ,(postfix-op ".cdr.car.cdr"))
    (sci_cddar ,(postfix-op ".car.cdr.cdr"))
    (sci_cdddr ,(postfix-op ".cdr.cdr.cdr"))
    (sci_caaaar ,(postfix-op ".car.car.car.car"))
    (sci_caadar ,(postfix-op ".car.cdr.car.car"))
    (sci_caaadr ,(postfix-op ".cdr.car.car.car"))
    (sci_caaddr ,(postfix-op ".cdr.cdr.car.car"))
    (sci_cdaaar ,(postfix-op ".car.car.car.cdr"))
    (sci_cdadar ,(postfix-op ".car.cdr.car.cdr"))
    (sci_cdaadr ,(postfix-op ".cdr.car.car.cdr"))
    (sci_cdaddr ,(postfix-op ".cdr.cdr.car.cdr"))
    (sci_cadaar ,(postfix-op ".car.car.cdr.car"))
    (sci_caddar ,(postfix-op ".car.cdr.cdr.car"))
    (sci_cadadr ,(postfix-op ".cdr.car.cdr.car"))
    (sci_cadddr ,(postfix-op ".cdr.cdr.cdr.car"))
    (sci_cddaar ,(postfix-op ".car.car.cdr.cdr"))
    (sci_cdddar ,(postfix-op ".car.cdr.cdr.cdr"))
    (sci_cddadr ,(postfix-op ".cdr.car.cdr.cdr"))
    (sci_cddddr ,(postfix-op ".cdr.cdr.cdr.cdr"))

    (sci_isNull ,(postfix-op " === null"))

    (sci_isSymbol_mutable ,(hole-op 1 "typeof " 'hole " === 'string'"))

    (sci_isString_mutable ,(postfix-op " instanceof sc_String"))
    (sci_isStringEqual_mutable ,(hole-op 2 'str1 ".val === " 'str2 ".val"))
    (sci_isStringLess_mutable ,(hole-op 2 'str1 ".val < " 'str2 ".val"))
    (sci_isStringGreater_mutable ,(hole-op 2 'str1 ".val > " 'str2 ".val"))
    (sci_isStringLessEqual_mutable ,(hole-op 2 'str1 ".val <= " 'str2 ".val"))
    (sci_isStringGreaterEqual_mutable ,(hole-op 2 'str1 ".val >= " 'str2 ".val"))
    (sci_isStringCIEqual_mutable ,(hole-op 2 'str1 ".val.toLowerCase() === " 'str2 ".val.toLowerCase()"))
    (sci_isStringCILess_mutable ,(hole-op 2 'str1 ".val.toLowerCase() < " 'str2 ".val.toLowerCase()"))
    (sci_isStringCIGreater_mutable ,(hole-op 2 'str1 ".val.toLowerCase() > " 'str2 ".val.toLowerCase()"))
    (sci_isStringCILessEqual_mutable ,(hole-op 2 'str1 ".val.toLowerCase() <= " 'str2 ".val.toLowerCase()"))
    (sci_isStringCIGreaterEqual_mutable ,(hole-op 2 'str1 ".val.toLowerCase() >= " 'str2 ".val.toLowerCase()"))

    (sci_isStringEqual_immutable ,(hole-op 2 'str1 " === " 'str2))
    (sci_isStringLess_immutable ,(hole-op 2 'str1 " < " 'str2))
    (sci_isStringGreater_immutable ,(hole-op 2 'str1 " > " 'str2))
    (sci_isStringLessEqual_immutable ,(hole-op 2 'str1 " <= " 'str2))
    (sci_isStringGreaterEqual_immutable ,(hole-op 2 'str1 " >= " 'str2))
    (sci_isStringCIEqual_immutable ,(hole-op 2 'str1 ".toLowerCase() === " 'str2 ".toLowerCase()"))
    (sci_isStringCILess_immutable ,(hole-op 2 'str1 ".toLowerCase() < " 'str2 ".toLowerCase()"))
    (sci_isStringCIGreater_immutable ,(hole-op 2 'str1 ".toLowerCase() > " 'str2 ".toLowerCase()"))
    (sci_isStringCILessEqual_immutable ,(hole-op 2 'str1 ".toLowerCase() <= " 'str2 ".toLowerCase()"))
    (sci_isStringCIGreaterEqual_immutable ,(hole-op 2 'str1 ".toLowerCase() >= " 'str2 ".toLowerCase()"))

    (sci_symbolAppend_mutable ,(infix-op 0 #f "+" "''"))
    (sci_symbolAppend_immutable ,symbolAppend_immutable-op)
    (sci_symbol2string_mutable ,(hole-op 1 "new sc_String(" 'sym ")"))
    (sci_symbol2string_immutable ,(postfix-op ".slice(1)")) ;; sc_SYMBOL_PREFIX_LENGTH
    (sci_string2symbol_mutable ,(postfix-op ".val"))
    (sci_string2symbol_immutable
     ,(prefix-op "'\\u1E9C' +")) ;; sc_SYMBOL_PREFIX
    (sci_char2symbol_mutable ,(postfix-op ".val"))
    (sci_char2symbol_immutable
     ,(hole-op 1 "'\\u1E9C' + " 'c ".val")) ;; sc_SYMBOL_PREFIX
    (sci_char2string_mutable ,(hole-op 1 "new sc_string(" 'char ".val)"))
    (sci_char2string_immutable ,(postfix-op ".val"))

    (sci_stringAppend_mutable ,stringAppend_mutable-op)
    (sci_stringAppend_immutable ,(infix-op 0 #f "+" "''"))

    (sci_isVector ,(postfix-op " instanceof sc_Vector"))
    (sci_vector ,vector-op)
    (sci_vectorLength ,(postfix-op ".length"))
    (sci_vectorRef ,(hole-op 2 'vector "[" 'index "]"))
    (sci_vectorSet ,(hole-op 3 'vector "[" 'index "] = " 'val))

    (sci_isProcedure ,(hole-op 1 "typeof " 'hole " === 'function'"))

    (sci_makeStruct ,(hole-op 1 "new sc_Struct(" 'name ")"))
    (sci_isStruct ,(postfix-op " instanceof sc_Struct"))
    (sci_isStructNamed ,(hole-op 2 "(" 1 " instanceof sc_Struct) &&"
				 " (" 1 ".name === " 0 ")"))
    (sci_getStructField ,(hole-op 3 0 "[" 2 "]"))
    (sci_setStructField ,(hole-op 4 0 "[" 2 "] = " 3))

    ;; scheme2js extension
    (sci_jsField ,(hole-op 2 'o "[" 'field "]"))
    (sci_setJsField ,(hole-op 3 'o "[" 'field "] = " 'val))
    (sci_deleteJsField ,(hole-op 2 "delete " 'o "[" 'field "]"))
    (sci_jsNew ,jsNew-op)
    (sci_jsCall ,jsCall-op)
    (sci_jsMethodCall ,jsMethodCall-op)
))

(define (compile-optimized-call p
				operator::pobject
				operands::pair-nil)
   (if (inherits-from? operator (node 'Var-ref))
       (let* ((var operator.var)
	      (id var.js-id)
	      (optimize-fun (and (not var.muted?)
				 id
				 (assq id *optimizable-operators*))))
	  (and optimize-fun ((cadr optimize-fun) p operands)))))
