(module mark-statements
   (import nodes
	   export
	   walk
	   config
	   verbose)
   (export (wide-class Stmt-If::If
	      stmt-test?::bool
	      stmt-then?::bool
	      stmt-else?::bool)
	   (wide-class Stmt-Case::Case
	      stmt-key?::bool)
	   (wide-class Stmt-Begin::Begin
	      stmt-exprs) ;; may be #f if none is stmt.
	   (wide-class Stmt-Set!::Set!
	      stmt-val?::bool)
	   (wide-class Stmt-Call::SCall
	      stmt-operator?::bool
	      stmt-operands)) ;; may be #f if none is stmt.
   (export (mark-statements tree::Module)))

(define (mark-statements tree)
   (verbose "  mark-statements")
   (mark tree #f))

(define (list-mark l walk)
   (let loop ((l l)
	      (res #f))
      (if (null? l)
	  res
	  (loop (cdr l)
		(or (walk (car l)) res)))))

(define-nmethod (Node.mark)
   (error 'stmt-mark
	  "Forgot node type"
	  this))

(define-nmethod (Const.mark)
   #f)

(define-nmethod (Ref.mark)
   #f)

(define-nmethod (Module.mark)
   (default-walk this)
   #t)

(define-nmethod (Lambda.mark)
   (with-access::Lambda this (body)
      ;; no need to go into formals
      (walk body)
      #f))

(define-nmethod (If.mark)
   (with-access::If this (test then else)
      (let* ((test-res (walk test))
	     (then-res (walk then))
	     (else-res (walk else))
	     (res (or test-res then-res else-res)))
	 (widen!::Stmt-If this
	    (stmt-test? test-res)
	    (stmt-then? then-res)
	    (stmt-else? else-res))
	 res)))

(define-nmethod (Case.mark)
   (with-access::Case this (key clauses)
      (widen!::Stmt-Case this
	 (stmt-key? (walk key)))
      (for-each walk clauses)
      #t))

(define-nmethod (Clause.mark)
   ;; consts must be either consts or var-refs (after consts-pass)
   (with-access::Clause this (expr)
      (walk expr)
      #t))

(define-nmethod (Set!.mark)
   ;; no need to look at lvalue.
   (with-access::Set! this (val)
      (let ((stmt-val? (walk val)))
	 (widen!::Stmt-Set! this
	    (stmt-val? stmt-val?)))))

;; Let does not exist anymore (at this level)

(define-nmethod (Begin.mark)
   (with-access::Begin this (exprs)
      (let* ((tmp (map walk exprs))
	     (contains-stmt? (any? (lambda (x) x) tmp)))
	 (widen!::Stmt-Begin this
	    (stmt-exprs (and contains-stmt? tmp)))
	 contains-stmt?)))

(define-nmethod (Call.mark)
   (with-access::Call this (operator operands)
      (let* ((operator-tmp (walk operator))
	     (operands-tmp (map walk operands))
	     (operands-contain-stmt? (any? (lambda (x) x) operands-tmp))
	     (call-contains-stmt? (or operator-tmp operands-contain-stmt?)))
	 (widen!::Stmt-Call this
	    (stmt-operator? operator-tmp)
	    (stmt-operands (and operands-contain-stmt? operands-tmp)))
	 call-contains-stmt?)))

(define-nmethod (Frame-alloc.mark)
   #f)

(define-nmethod (Frame-push.mark)
   (with-access::Frame-push this (body)
      (walk body)
      #t))

(define-nmethod (Return.mark)
   (default-walk this)
   #t)

(define-nmethod (Labeled.mark)
   (default-walk this)
   #t)

(define-nmethod (Break.mark)
   (default-walk this)
   #t)

(define-nmethod (Continue.mark)
   #t)

(define-nmethod (Pragma.mark)
   #f)

(define-nmethod (While.mark)
   (default-walk this)
   #t)

(define-nmethod (Call/cc-Call.mark)
   (with-access::Call/cc-Call this (operator operands)
      (let* ((operands-tmp (list-mark operands walk))
	     (operator-tmp (walk operator)))
	 #t)))
   
(define-nmethod (Call/cc-Resume.mark)
   #t)
