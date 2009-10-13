;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-2009 Florian Loitsch, see LICENSE file       */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module compile-optimized-call
   (export (compile-optimized-call  p
				    compile::procedure
				    operator::Node
				    operands::pair-nil))
   (import config
	   error
	   tools
	   template-display
	   nodes
	   export-desc
	   verbose))

(define (infix-op nb-operands-min nb-operands-max infix-operator #!optional default-val)
   (lambda (p compile operands)
      (let ((nb-operands (length operands)))
	 (and (>=fx nb-operands nb-operands-min)
	      (or (not nb-operands-max)
		  (<=fx nb-operands nb-operands-max))
	      (begin
		 (if (=fx nb-operands 0)
		     (template-display p "$default-val")
		     (template-display p
			"(~e)"
			(separated infix-operator
				   (lambda (e)
				      "~e" (compile e p #f))
				   operands)))
		 #t)))))

(define (postfix-op postfix-operator)
   (lambda (p compile operands)
      (and (pair? operands)
	   (null? (cdr operands))
	   (begin
	      (template-display p
		 "((~e)~a)"
		 (compile (car operands) p #f)
		 postfix-operator)
	      #t))))

(define (prefix-op prefix-operator)
   (lambda (p compile operands)
      (and (pair? operands)
	   (null? (cdr operands))
	   (begin
	      (template-display p
		 "(~a~e)"
		 prefix-operator
		 (compile (car operands) p #f))
	      #t))))

(define (hole-op nb-holes . pattern)
   (lambda (p compile operands)
      (and (=fx (length operands) nb-holes)
	   (begin
	      (template-display p
		 "(~e)"
		 (let loop ((pattern pattern)
			    (operands operands))
		    (cond
		       ((null? pattern)
			'do-nothing)
		       ((string? (car pattern))
			(template-display p
			   "$(car pattern)")
			(loop (cdr pattern)
			      operands))
		       ((number? (car pattern))
			(compile (list-ref operands (car pattern)) p #f)
			(loop (cdr pattern)
			      operands))
		       (else
			(compile (car operands) p #f)
			(loop (cdr pattern)
			      (cdr operands))))))
	      #t))))

(define (minus-op p compile operands)
   (cond
      ((null? operands) #f)
      ((null? (cdr operands))
       (template-display p
	  "(- ~e)"
	  (compile (car operands) p #f))
       #t)
      (else
       ((infix-op 1 #f "-") p compile operands))))

(define (div-op p compile operands)
   (cond
      ((null? operands) #f)
      ((null? (cdr operands))
       (template-display p
	  "(1/~e)"
	  (compile (car operands) p #f))
       #t)
      (else
       ((infix-op 1 #f "/") p compile operands))))

(define (vector-op p compile operands)
   (template-display p
      "[~e]"
      (separated ", "
		 (lambda (e) "~e" (compile e p #f))
		 operands))
   #t)
		  
(define (id p compile operand)
   (and (pair? operand)
	(null? (cdr operand))
	(begin
	   (compile (car operand) p #f)
	   #t)))

(define (jsNew-op p compile operands)
   (and (not (null? operands))
	(begin
	   (template-display p
	      "new ~e(~e)"
	      (compile (car operands) p #f)
	      (separated ", "
			 (lambda (e) "~e" (compile e p #f))
			 (cdr operands)))
	   #t)))

(define (jsCall-op p compile operands)
   (and (not (null? operands))
	(not (null? (cdr operands)))
	(begin
	   (template-display p
	      "(~e).call(~e~e)"
	      (compile (cadr operands) p #f)
	      (compile (car operands) p #f)
	      (each (lambda (e) ", ~e" (compile e p #f))
		    (cddr operands)))
	   #t)))

(define (jsMethodCall-op p compile operands)
   (and (not (null? operands))
	(not (null? (cdr operands)))
	(begin
	   (template-display p
	      "~e[~e](~e)"
	      (compile (car operands) p #f)
	      (compile (cadr operands) p #f)
	      (separated ", "
			 (lambda (e) "~e" (compile e p #f))
			 (cddr operands)))
	   #t)))

(define (symbolAppend_immutable-op p compile operands)
   (let ((nb-operands (length operands)))
      (cond
	 ((= nb-operands 0)
	  (template-display p "'\\uEBAC'")) ;; sc_SYMBOL_PREFIX
	 ((= nb-operands 1) (compile (car operands) p #f))
	 (else
	  (template-display p
	     "(~e~e)"
	     (compile (car operands) p #f)
	     (for-each (lambda (operand)
			  (if (Const? operand)
			      (with-access::Const operand (value)
				 (if (symbol? value)
				     (template-display p
					"+\"$value\"")
				     (scheme2js-error
				      "symbolAppend_immutable-op"
				      "symbol-append requires symbols as arguments"
				      value
				      operand)))
			      (template-display p
				 "+~e.slice(1)"
				 (compile operand p #f))))
		       (cdr operands)))))
      #t))

(define (stringAppend_mutable-op p compile operands)
   (define (string-val operand)
      (if (Const? operand)
	  (with-access::Const operand (value)
	     (if (string? value)
		 (template-display p "\"$(string-for-read value)\"")
		 (scheme2js-error
		  "stringAppend_mutable-op"
		  "string-append requires strings as arguments"
		  value
		  operand)))
	  (template-display p
	     "~e.val" (compile operand p #f))))

   (let ((nb-operands (length operands)))
      (cond
	 ((= nb-operands 0) (template-display p "(new sc_String(''))"))
	 ((= nb-operands 1) (compile (car operands) p #f))
	 (else
	  (template-display p
	     "(new sc_String(~e~e))"
	     (string-val (car operands))
	     (for-each (lambda (n)
			  (template-display p
			     "+~e" (string-val n)))
		       (cdr operands))))))
   #t)


(define (modulo-op p compile operands)
   ;; TODO: get rid of config.
   (if (config 'correct-modulo)
       #f
       ((infix-op 2 2 "%") p compile operands)))

(define (values-op p compile operands)
   (let ((nb-operands (length operands)))
      (cond
	 ((= nb-operands 0) (template-display p "(new sc_Values([]))"))
	 ((= nb-operands 1) (compile (car operands) p))
	 (else
	  (template-display p
	     "(new sc_Values([~e]))"
	     (separated ","
			(lambda (e) "~e" (compile e p #f))
			operands))))
      #t))

(define (not-op p compile operands)
   (let ((nb-operands (length operands)))
      (and (= nb-operands 1)
	   (let ((operand (car operands)))
	      (cond
		 ((Const? operand)
		  (with-access::Const operand (value)
		     (set! value (not value)))
		  (compile operand p #f))
		 ((and (Call? operand)
		       (Ref? (Call-operator operand))
		       (with-access::Call operand (operator)
			  (with-access::Ref operator (var)
			     (with-access::Var var (constant?)
				(and constant?
				     (or (eq? (Var-kind var) 'exported)
					 (eq? (Var-kind var) 'imported))
				     (let ((desc (Var-export-desc var)))
					(eq? (Export-Desc-return-type desc)
					     'bool)))))))
		  (template-display p
		     "!~e" (compile operand p #f)))
		 (else
		  (template-display p
		     "(~e === false)"
		     (compile operand p #f))))
	      #t))))

(define (string2jsstring_mutable-op p compile operands)
   (let ((nb-operands (length operands)))
      (cond
	 ((= nb-operands 0) (template-display p "''"))
	 ((= nb-operands 1)
	  (let ((operand (car operands)))
	     (if (Const? operand)
		 (with-access::Const operand (value)
		    (if (string? value)
			(template-display p
			   "\"$(string-for-read value)\"")
			(scheme2js-error
			 "string2jsstring_mutable-op"
			 "string->jsstring requires string as argument"
			 value
			 operand)))
		 (template-display p
		    "(~e.val)"
		    (compile operand p #f))))))
      (< nb-operands 2))) ;; 0 et 1 have been handled.

(define (symbol2jsstring_immutable-op p compile operands)
   (let ((nb-operands (length operands)))
      (cond
	 ((= nb-operands 0) (template-display p "''"))
	 ((= nb-operands 1)
	  (let ((operand (car operands)))
	     (if (Const? operand)
		 (with-access::Const operand (value)
		    (if (symbol? value)
			(template-display p "\"$value\"")
			(scheme2js-error
			 "symbol2jsstring_immutable-op"
			 "symbol->jsstring requires symbol as argument"
			 value
			 operand)))
		 (template-display p
		    "(~e).slice(1)" (compile operand p #f))))))
      (< nb-operands 2))) ;; 0 et 1 have been handled.

(define (compile-optimized-call p compile operator operands)
   (when (and (Ref? operator)
	      (Var-constant? (Ref-var operator))
	      (eq? (Var-kind (Ref-var operator)) 'imported))
      (let* ((var (Ref-var operator))
	     (desc (Var-export-desc var))
	     (peephole (Export-Desc-peephole desc)))
	 (when peephole
	    (let* ((optimize-fun
		    (case (car peephole)
		       ((infix) (apply infix-op (cdr peephole)))
		       ((postfix) (apply postfix-op (cdr peephole)))
		       ((prefix) (apply prefix-op (cdr peephole)))
		       ((hole) (apply hole-op (cdr peephole)))
		       ((minus) minus-op)
		       ((div) div-op)
		       ((vector) vector-op)
		       ((id) id)
		       ((jsNew) jsNew-op)
		       ((jsCall) jsCall-op)
		       ((jsMethodCall) jsMethodCall-op)
		       ((symbolAppend_immutable)
			symbolAppend_immutable-op)
		       ((stringAppend_mutable)
			stringAppend_mutable-op)
		       ((string2jsstring_mutable
			 string2symbol_mutable)
			string2jsstring_mutable-op)
		       ((symbol2jsstring_immutable
			 symbol2string_immutable)
			symbol2jsstring_immutable-op)
		       ((modulo) modulo-op)
		       ((values) values-op)
		       ((not) not-op)
		       (else (scheme2js-error
			      "compile-optimized-call"
			      "forgot optimize-fun:"
			      (car peephole)
			      operator)))))
	       (optimize-fun p compile operands))))))
