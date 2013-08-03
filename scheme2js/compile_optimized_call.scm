;*=====================================================================*/
;*    .../project/hop/2.5.x/scheme2js/compile_optimized_call.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-11                                           */
;*    Last change :  Wed Jul 31 14:24:21 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Call compilation                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module compile-optimized-call
   
   (import config
	   error
	   tools
	   template-display
	   nodes
	   export-desc
	   verbose)
   
   (export (compile-optimized-call  p
	      compile::procedure
	      operator::Node
	      operands::pair-nil
	      tmp)))

;*---------------------------------------------------------------------*/
;*    infix-op ...                                                     */
;*---------------------------------------------------------------------*/
(define (infix-op nb-operands-min nb-operands-max infix-operator
	   #!optional default-val)
   (lambda (p compile operands tmp)
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
			      "~e" (compile e p #f tmp))
			   operands)))
		 #t)))))

;*---------------------------------------------------------------------*/
;*    postfix-op ...                                                   */
;*---------------------------------------------------------------------*/
(define (postfix-op postfix-operator)
   (lambda (p compile operands tmp)
      (and (pair? operands)
	   (null? (cdr operands))
	   (begin
	      (template-display p
		 "((~e)~a)"
		 (compile (car operands) p #f tmp)
		 postfix-operator)
	      #t))))

;*---------------------------------------------------------------------*/
;*    prefix-op ...                                                    */
;*---------------------------------------------------------------------*/
(define (prefix-op prefix-operator)
   (lambda (p compile operands tmp)
      (and (pair? operands)
	   (null? (cdr operands))
	   (begin
	      (template-display p
		 "(~a~e)"
		 prefix-operator
		 (compile (car operands) p #f tmp))
	      #t))))


;*---------------------------------------------------------------------*/
;*    hole-op ...                                                      */
;*---------------------------------------------------------------------*/
(define (hole-op nb-holes . pattern)
   (lambda (p compile operands tmp)
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
			(template-display p "$(car pattern)")
			(loop (cdr pattern) operands))
		       ((number? (car pattern))
			(compile (list-ref operands (car pattern)) p #f tmp)
			(loop (cdr pattern) operands))
		       (else
			(compile (car operands) p #f tmp)
			(loop (cdr pattern) (cdr operands))))))
	      #t))))

;*---------------------------------------------------------------------*/
;*    minus-op ...                                                     */
;*---------------------------------------------------------------------*/
(define (minus-op p compile operands tmp)
   (cond
      ((null? operands) #f)
      ((null? (cdr operands))
       (template-display p
	  "(- ~e)"
	  (compile (car operands) p #f tmp))
       #t)
      (else
       ((infix-op 1 #f "-") p compile operands tmp))))

;*---------------------------------------------------------------------*/
;*    div-op ...                                                       */
;*---------------------------------------------------------------------*/
(define (div-op p compile operands tmp)
   (cond
      ((null? operands) #f)
      ((null? (cdr operands))
       (template-display p
	  "(1/~e)"
	  (compile (car operands) p #f tmp))
       #t)
      (else
       ((infix-op 1 #f "/") p compile operands tmp))))

;*---------------------------------------------------------------------*/
;*    vector-op ...                                                    */
;*---------------------------------------------------------------------*/
(define (vector-op p compile operands tmp)
   (template-display p
      "[~e]"
      (separated ", "
	 (lambda (e) "~e" (compile e p #f tmp))
	 operands))
   #t)

;*---------------------------------------------------------------------*/
;*    id ...                                                           */
;*---------------------------------------------------------------------*/
(define (id p compile operand tmp)
   (and (pair? operand)
	(null? (cdr operand))
	(begin
	   (compile (car operand) p #f tmp)
	   #t)))

;*---------------------------------------------------------------------*/
;*    jsNew-op ...                                                     */
;*---------------------------------------------------------------------*/
(define (jsNew-op p compile operands tmp)
   (and (not (null? operands))
	(begin
	   (template-display p
	      "new ~e(~e)"
	      (compile (car operands) p #f tmp)
	      (separated ", "
		 (lambda (e) "~e" (compile e p #f tmp))
		 (cdr operands)))
	   #t)))

;*---------------------------------------------------------------------*/
;*    jsCall-op ...                                                    */
;*---------------------------------------------------------------------*/
(define (jsCall-op p compile operands tmp)
   (and (not (null? operands))
	(not (null? (cdr operands)))
	(begin
	   (template-display p
	      "(~e).call(~e~e)"
	      (compile (cadr operands) p #f tmp)
	      (compile (car operands) p #f tmp)
	      (each (lambda (e) ", ~e" (compile e p #f tmp))
		 (cddr operands)))
	   #t)))

;*---------------------------------------------------------------------*/
;*    jsMethodCall-op ...                                              */
;*---------------------------------------------------------------------*/
(define (jsMethodCall-op p compile operands tmp)
   (and (not (null? operands))
	(not (null? (cdr operands)))
	(begin
	   (template-display p
	      "~e[~e](~e)"
	      (compile (car operands) p #f tmp)
	      (compile (cadr operands) p #f tmp)
	      (separated ", "
		 (lambda (e) "~e" (compile e p #f tmp))
		 (cddr operands)))
	   #t)))

;*---------------------------------------------------------------------*/
;*    symbolAppend_immutable-op ...                                    */
;*---------------------------------------------------------------------*/
(define (symbolAppend_immutable-op p compile operands tmp)
   (let ((nb-operands (length operands)))
      (cond
	 ((= nb-operands 0)
	  (template-display p "'\\uEBAC'")) ;; sc_SYMBOL_PREFIX
	 ((= nb-operands 1) (compile (car operands) p #f tmp))
	 (else
	  (template-display p
	     "(~e~e)"
	     (compile (car operands) p #f tmp)
	     (for-each (lambda (operand)
			  (if (isa? operand Const)
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
				 (compile operand p #f tmp))))
		       (cdr operands)))))
      #t))

;*---------------------------------------------------------------------*/
;*    stringAppend_mutable-op ...                                      */
;*---------------------------------------------------------------------*/
(define (stringAppend_mutable-op p compile operands tmp)
   (define (string-val operand)
      (if (isa? operand Const)
	  (with-access::Const operand (value)
	     (if (string? value)
		 (template-display p "\"$(string-for-read value)\"")
		 (scheme2js-error
		  "stringAppend_mutable-op"
		  "string-append requires strings as arguments"
		  value
		  operand)))
	  (template-display p
	     "~e.val" (compile operand p #f tmp))))

   (let ((nb-operands (length operands)))
      (cond
	 ((= nb-operands 0) (template-display p "(new sc_String(''))"))
	 ((= nb-operands 1) (compile (car operands) p #f tmp))
	 (else
	  (template-display p
	     "(new sc_String(~e~e))"
	     (string-val (car operands))
	     (for-each (lambda (n)
			  (template-display p
			     "+~e" (string-val n)))
		       (cdr operands))))))
   #t)


;*---------------------------------------------------------------------*/
;*    modulo-op ...                                                    */
;*---------------------------------------------------------------------*/
(define (modulo-op p compile operands tmp)
   ;; TODO: get rid of config.
   (if (config 'correct-modulo)
       #f
       ((infix-op 2 2 "%") p compile operands tmp)))

;*---------------------------------------------------------------------*/
;*    values-op ...                                                    */
;*---------------------------------------------------------------------*/
(define (values-op p compile operands tmp)
   (let ((nb-operands (length operands)))
      (cond
	 ((= nb-operands 0) (template-display p "(new sc_Values([]))"))
	 ((= nb-operands 1) (compile (car operands) p tmp))
	 (else
	  (template-display p
	     "(new sc_Values([~e]))"
	     (separated ","
		(lambda (e) "~e" (compile e p #f tmp))
		operands))))
      #t))

;*---------------------------------------------------------------------*/
;*    not-op ...                                                       */
;*---------------------------------------------------------------------*/
(define (not-op p compile operands tmp)
   (let ((nb-operands (length operands)))
      (and (= nb-operands 1)
	   (let ((operand (car operands)))
	      (cond
		 ((isa? operand Const)
		  (with-access::Const operand (value)
		     (set! value (not value)))
		  (compile operand p #f tmp))
		 ((and (isa? operand Call)
		       (isa? (with-access::Call operand (operator) operator) Ref)
		       (with-access::Call operand (operator)
			  (with-access::Ref operator (var)
			     (with-access::Var var (constant? kind)
				(and constant?
				     (or (eq? kind 'exported)
					 (eq? kind 'imported))
				     (let ((desc (with-access::Var var (export-desc) export-desc)))
					(eq? (with-access::Export-Desc desc (return-type) return-type)
					   'bool)))))))
		  (template-display p
		     "!~e" (compile operand p #f tmp)))
		 (else
		  (template-display p
		     "(~e === false)"
		     (compile operand p #f tmp))))
	      #t))))

;*---------------------------------------------------------------------*/
;*    string2jsstring_mutable-op ...                                   */
;*---------------------------------------------------------------------*/
(define (string2jsstring_mutable-op p compile operands tmp)
   (let ((nb-operands (length operands)))
      (cond
	 ((= nb-operands 0) (template-display p "''"))
	 ((= nb-operands 1)
	  (let ((operand (car operands)))
	     (if (isa? operand Const)
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
		    (compile operand p #f tmp))))))
      ;; 0 et 1 have been handled.
      (< nb-operands 2))) 

;*---------------------------------------------------------------------*/
;*    symbol2jsstring_immutable-op ...                                 */
;*---------------------------------------------------------------------*/
(define (symbol2jsstring_immutable-op p compile operands tmp)
   (let ((nb-operands (length operands)))
      (cond
	 ((= nb-operands 0) (template-display p "''"))
	 ((= nb-operands 1)
	  (let ((operand (car operands)))
	     (if (isa? operand Const)
		 (with-access::Const operand (value)
		    (if (symbol? value)
			(template-display p "\"$value\"")
			(scheme2js-error
			   "symbol2jsstring_immutable-op"
			   "symbol->jsstring requires symbol as argument"
			   value
			   operand)))
		 (template-display p
		    "(~e).slice(1)" (compile operand p #f tmp))))))
      ;; 0 et 1 have been handled.
      (< nb-operands 2))) 

;*---------------------------------------------------------------------*/
;*    compile-optimized-call ...                                       */
;*---------------------------------------------------------------------*/
(define (compile-optimized-call p compile operator operands tmp)
   (when (and (=fx (bigloo-debug) 0)
	      (isa? operator Ref)
	      (with-access::Ref operator (var)
		 (with-access::Var var (constant?) constant?))
	      (with-access::Ref operator (var)
		 (eq? (with-access::Var var (kind) kind) 'imported)))
      (let* ((var (with-access::Ref operator (var)  var))
	     (desc (with-access::Var var (export-desc) export-desc))
	     (peephole (with-access::Export-Desc desc (peephole) peephole)))
	 (when peephole
	    (let* ((optimize-fun
		      (case (car peephole)
			 ((infix) (apply infix-op (cdr peephole)))
			 ((postfix)
			  (match-case peephole
			     ((?- ?v) (postfix-op v))
			     (else (scheme2js-error
				      "compile-optimized-call"
				      "Illegal postfix arity"
				      (car peephole)
				      operator))))
			 ((prefix)
			  (match-case peephole
			     ((?- ?v) (prefix-op v))
			     (else (scheme2js-error
				      "compile-optimized-call"
				      "Illegal prefix arity"
				      (car peephole)
				      operator))))
			 ((hole) (apply hole-op (cdr peephole)))
			 ((minus) minus-op)
			 ((div) div-op)
			 ((vector) vector-op)
			 ((id) id)
			 ((jsNew) jsNew-op)
			 ((jsCall) jsCall-op)
			 ((jsMethodCall) jsMethodCall-op)
			 ((symbolAppend_immutable) symbolAppend_immutable-op)
			 ((stringAppend_mutable) stringAppend_mutable-op)
			 ((string2jsstring_mutable string2symbol_mutable)
			  string2jsstring_mutable-op)
			 ((symbol2jsstring_immutable symbol2string_immutable)
			  symbol2jsstring_immutable-op)
			 ((modulo) modulo-op)
			 ((values) values-op)
			 ((not) not-op)
			 (else
			  (scheme2js-error
			     "compile-optimized-call"
			     "forgot optimize-fun:"
			     (car peephole)
			     operator)))))
	       (optimize-fun p compile operands tmp))))))
