(module call-check
   (import config
	   error
	   nodes
	   export-desc
	   walk
	   side
	   verbose)
   (export (call-check tree::Module)))

(define (call-check tree)
   (when (config 'call-check)
      (verbose "call-check")
      (side-effect tree)
      (check tree #f)))

(define-nmethod (Node.check)
   (default-walk this))


(define (check-arity call-len target-len id loc-node)
   (cond
      ((and (>=fx target-len 0)
	    (=fx call-len target-len))
       'ok)
      ((and (<fx target-len 0)
	    (>=fx call-len (-fx -1 target-len)))
       'ok)
      (else
       (let ((ln (cond
		    ((not (Node? loc-node)) #f)
		    ((Node-location loc-node)
		     loc-node)
		    ((Call? loc-node)
		     (any (lambda (n)
			     (and (Node-location n)
				  n))
			  (cons (Call-operator loc-node)
				(Call-operands loc-node))))
		    (else #f))))
       (scheme2js-error
	'call-check
	(format "Wrong number of arguments: ~a expected, ~a provided"
		target-len call-len)
	id
	ln)))))

(define-nmethod (Call.check)
   (with-access::Call this (operator operands)
      (when (Ref? operator)
	 (with-access::Ref operator (var)
	    (with-access::Var var (value id constant? kind export-desc)
	       (when (and constant? value)
		  (when (Const? value)
		     ;; all others could potentially become functions.
		     (scheme2js-error 'call-check
				      "Call target not a function"
				      (Const-value value)
				      this))
		  (when (Lambda? value)
		     (with-access::Lambda value (formals vaarg?)
			(let ((call-len (length operands))
			      (target-len (if vaarg?
					      (negfx (length formals))
					      (length formals))))
			   (check-arity call-len target-len id this)))))
	       (when (and constant? (eq? kind 'imported))
		  (with-access::Export-Desc export-desc (arity)
		     (when arity
			(check-arity (length operands) arity id this))))))))
   (default-walk this))

	       

