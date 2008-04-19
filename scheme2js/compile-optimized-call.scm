(module compile-optimized-call
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (export (compile-optimized-call  p
				    operator::pobject
				    operands::pair-nil))
   (import protobject
	   config
	   nodes
	   var
	   verbose))

(define (compile-separated-list p els sep . Ldefault)
   (define (iter els sep default)
      (cond
	 ((null? els) (if default
			  (p-display p default)))
	 ;; last element is displayed verbatim
	 ((null? (cdr els))
	  ((car els).compile p))
	 ;; otherwise add "," between elements
	 (else
	  ((car els).compile p)
	  (p-display p sep)
	  (iter (cdr els) sep default))))
   (iter els sep (and (not (null? Ldefault)) (car Ldefault))))


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
	   (p-display p "(")
	   ((cadr operands).compile p)
	   (p-display p ").call(")
	   ((car operands).compile p)
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
	 ((= nb-operands 0) (p-display p "'\\u1E9C'")) ;; sc_SYMBOL_PREFIX
	 ((= nb-operands 1) ((car operands).compile p))
	 (else
	  (p-display p "(")
	  ((car operands).compile p)
	  (for-each (lambda (operand)
		       (p-display p "+")
		       (if (instance-of? operand (node 'Const))
			   (if (symbol? operand.value)
			       (p-display p "\"" operand.value "\"")
			       (error "symbolAppend_immutable-op"
				      "symbol-append requires symbols as arguments"
				      operand.value))
			   (begin
			      (operand.compile p)
			      (p-display p ".slice(1)"))))
		    (cdr operands))
	  (p-display p ")")))
      #t))

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
	 ((= nb-operands 0) (p-display p "(new sc_String(''))"))
	 ((= nb-operands 1) ((car operands).compile p))
	 (else
	  (p-display p "(new sc_String(")
	  (string-val (car operands))
	  (for-each (lambda (n)
		       (p-display p "+")
		       (string-val n))
		    (cdr operands))
	  (p-display p "))"))))
   #t)


(define (modulo-op p operands)
   (if (config 'correct-modulo)
       #f
       ((infix-op 2 2 "%") p operands)))

(define (values-op p operands)
   (let ((nb-operands (length operands)))
      (cond
	 ((= nb-operands 0) (p-display p "(new sc_Values([]))"))
	 ((= nb-operands 1) ((car operands).compile p))
	 (else
	  (p-display p "(new sc_Values([")
	  ((car operands).compile p)
	  (for-each (lambda (n)
		       (p-display p ",")
		       (n.compile p))
		    (cdr operands))
	  (p-display p "]))")))))

(define (not-op p operands)
   (let ((nb-operands (length operands)))
      (and (= nb-operands 1)
	   (let ((operand (car operands)))
	      (cond
		 ((inherits-from? operand (node 'Const))
		  (set! operand.value (not operand.value))
		  (operand.compile p))
		 ((and (inherits-from? operand (node 'Call))
		       (inherits-from? operand.operator (node 'Var-ref))
		       operand.operator.var.constant?
		       (eq? operand.operator.var.return-type 'bool))
		  (p-display p "!")
		  (operand.compile p))
		 (else
		  (p-display p "(")
		  (operand.compile p)
		  (p-display p "=== false)")))
	      #t))))

(define (string2jsstring_mutable-op p operands)
   (let ((nb-operands (length operands)))
      (cond
	 ((= nb-operands 0) (p-display p "''"))
	 ((= nb-operands 1)
	  (let ((operand (car operands)))
	     (if (instance-of? operand (node 'Const))
		 (if (string? operand.value)
		     (p-display p "\"" (string-for-read operand.value) "\"")
		     (error "string2jsstring_mutable-op"
			    "string->jsstring requires string as argument"
			    operand.value))
		 (begin
		    (p-display p "(")
		    (operand.compile p)
		    (p-display p ".val)"))))))
      (< nb-operands 2))) ;; 0 et 1 have been handled.

(define (symbol2jsstring_immutable-op p operands)
   (let ((nb-operands (length operands)))
      (cond
	 ((= nb-operands 0) (p-display p "''"))
	 ((= nb-operands 1)
	  (let ((operand (car operands)))
	     (if (instance-of? operand (node 'Const))
		 (if (symbol? operand.value)
		     (p-display p "\"" operand.value "\"")
		     (error "symbol2jsstring_immutable-op"
			    "symbol->jsstring requires symbol as argument"
			    operand.value))
		 (begin
		    (p-display p "(")
		    (operand.compile p)
		    (p-display p ").slice(1)"))))))
      (< nb-operands 2))) ;; 0 et 1 have been handled.

(define (compile-optimized-call p
				operator::pobject
				operands::pair-nil)
   (if (inherits-from? operator (node 'Var-ref))
       (let* ((var operator.var))
	  (if (and var.peephole
		   var.constant?)
	      (let* ((peephole var.peephole)
		     (optimize-fun
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
			 (else (error "compile-optimized-call"
				      "forgot optimize-fun:"
				      (car peephole))))))
		 (optimize-fun p operands))
	      #f))
       #f))
