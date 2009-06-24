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

(define-nmethod (Call.check)
   (with-access::Call this (operator operands)
      (when (Ref? operator)
	 (with-access::Ref operator (var)
	    (with-access::Var var (value constant?)
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
			      (target-len (length formals)))
			   (cond
			      ((and (not vaarg?)
				    (=fx call-len target-len))
			       'ok)
			      ((and vaarg?
				    (>=fx call-len (-fx target-len 1)))
			       'ok)
			      (else
			       (scheme2js-error
				'call-check
				"Bad number of arguments"
				call-len
				this)))))))))))
   (default-walk this))

	       

