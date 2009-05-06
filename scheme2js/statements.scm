(module statements
   (import nodes
	   error
	   tools
	   export-desc
	   config
	   walk
	   mark-statements
	   side
	   push-set
	   verbose)
   (static (wide-class Stmt-Label::Label
	      state-var/return))
   (export (statements! tree::Module)))

;; these pass happens after the scope-flattening pass. As a result there don't
;; exist any Let nodes anymore, and temporary variables can not be put into
;; Let-nodes. Any allocated var has to be added into the "declared-vars" list
;; of the surrounding fun/module.
;; Also they must not have any influence on call/cc.
(define (statements! tree)
   (verbose "statements")
   (push-set!s/return! tree)
   (mark-statements tree)
   (side-effect tree)
   (transform-statements! tree)
   (push-set!s/return! tree)
   )

(define (transform-statements! tree::Module)
   (verbose "  transform-statements")
   (stmts! tree #f
	   #f #f))

(define (move-to-begin n walk! surrounding-fun stmt-begin
		       #!optional (use-var? #t))
   (if use-var?
       (let* ((tmp (Ref-of-new-Var 'stmp))
	      (tmp-var (Ref-var tmp)))
	  (with-access::Begin stmt-begin (exprs)
	     (cons-set! exprs
			(walk! (var-assig tmp-var n)
			       surrounding-fun
			       #f))) ;; if is at stmt-level now.
	  (with-access::Execution-Unit surrounding-fun (declared-vars)
	     (cons-set! declared-vars tmp-var))
	  tmp) ;; replace n with the tmp-var.
       (with-access::Begin stmt-begin (exprs)
	  (cons-set! exprs
		     (walk! n surrounding-fun #f)) ;; n is at stmt-level now.
	  (instantiate::Const
	     (location (Node-location n))
	     (value #unspecified)))))


;; if state-var/return is not #f, then we need to assign any value to it
;; if stmt-begin is not #f, then the construct must be converted to expression
;; (by moving any stmt to stmt-begin).
(define-nmethod (Node.stmts! surrounding-fun stmt-begin)
   (scheme2js-error "Node-stmts!"
		    "forgot Node-type: "
		    this
		    this))

(define-nmethod (Const.stmts! surrounding-fun stmt-begin)
   this)

(define-nmethod (Ref.stmts! surrounding-fun stmt-begin)
   this)

(define-nmethod (Module.stmts! surrounding-fun stmt-begin)
   ;; treat module like a lambda (for surrounding-fun).
   (default-walk! this this #f))

(define-nmethod (Lambda.stmts! surrounding-fun stmt-begin)
   ;; treat module like a lambda (for surrounding-fun).
   (default-walk! this this #f))

(define-nmethod (If.stmts! surrounding-fun stmt-begin)
   (with-access::Stmt-If this (test then else stmt-test? stmt-then? stmt-else?)
      (cond
	 ((and (not stmt-begin)
	       stmt-test?)
	  (shrink! this)
	  ;; test-statement needs to be moved out of test.
	  (let ((bnode (instantiate::Begin (exprs (list this)))))
	     (set! test (walk! test surrounding-fun bnode))
	     (set! then (walk! then surrounding-fun #f))
	     (set! else (walk! else surrounding-fun #f))
	     bnode))
	 ((and stmt-begin
	       (or stmt-then? stmt-else?))
	  (move-to-begin this walk! surrounding-fun stmt-begin))
	 ((and stmt-begin
	       stmt-test?)
	  (shrink! this)
	  ;; we can leave the if at the current location, but we have to move
	  ;; the test to the surrounding stmt-begin.
	  (let ((tmp (move-to-begin test walk! surrounding-fun stmt-begin)))
	     (set! test tmp)
	     ;; test, then and else are now
	     ;; expressions. no need for the stmt-begin anymore.
	     (default-walk! this surrounding-fun #f)))
	 (else
	  (shrink! this)
	  (default-walk! this surrounding-fun stmt-begin)))))

(define-nmethod (Case.stmts! surrounding-fun stmt-begin)
   (with-access::Stmt-Case this (key clauses stmt-key?)
      (cond
	 ((and (not stmt-begin)
	       stmt-key?)
	  (shrink! this)
	  ;; key-statement needs to be moved out of key.
	  (let ((bnode (instantiate::Begin (exprs (list this)))))
	     (set! key (walk! key surrounding-fun bnode))
	     (set! clauses (map! (lambda (clause)
				    (walk! clause surrounding-fun #f))
				 clauses))
	     bnode))
	 (stmt-begin
	  (move-to-begin this walk! surrounding-fun stmt-begin))
	 (else
	  (shrink! this)
	  (default-walk! this surrounding-fun #f)))))

(define-nmethod (Clause.stmts! surrounding-fun stmt-begin)
   (default-walk! this surrounding-fun #f))

(define-nmethod (Set!.stmts! surrounding-fun stmt-begin)
   (default-walk! this surrounding-fun #f))

(define-nmethod (Stmt-Set!.stmts! surrounding-fun stmt-begin)
   ;; value needs to be expression. but we can ignore this for now.
   ;; another push-set!s/return will take care of this.
   ;; we need to move the whole assignment, though.
   (if stmt-begin
       (move-to-begin this walk! surrounding-fun stmt-begin
		      #f) ;; no need to get result. assigs are unspecified.
       (default-walk! this surrounding-fun #f)))

;; Lets don't exist anymore in this pass.

(define-nmethod (Begin.stmts! surrounding-fun stmt-begin)
   ;; stmt-begin must be #f
   [assert (stmt-begin) (not stmt-begin)]
   (default-walk! this surrounding-fun #f))
   
(define-nmethod (Stmt-Begin.stmts! surrounding-fun stmt-begin)
   (with-access::Stmt-Begin this (exprs stmt-exprs)
      (cond
	 ((null? exprs)
	  (shrink! this)
	  this)
	 ((or (not stmt-exprs)
	      (not stmt-begin))
	  (shrink! this)
	  (default-walk! this surrounding-fun #f))
	 ((and (car stmt-exprs)
	       (not (any? (lambda (x) x) (cdr stmt-exprs))))
	  (shrink! this)
	  ;; only the first expression is in stmt-form.
	  ;; simply pass the stmt-begin to it.
	  (default-walk! this surrounding-fun stmt-begin))
	 (else ;; we have a stmt-begin, and at least one el in stmt-form.
	  ;; find the last one.
	  (let* ((last-p (let loop ((exprs exprs)
				    (stmt-exprs stmt-exprs)
				    (last-p #f))
			    (cond
			       ((null? exprs)
				last-p) ;; there must be one.
			       ((car stmt-exprs)
				(loop (cdr exprs) (cdr stmt-exprs) exprs))
			       (else
				(loop (cdr exprs) (cdr stmt-exprs) last-p)))))
		 (remaining (cdr last-p)))
	     (if (null? remaining)
		 (move-to-begin this walk! surrounding-fun stmt-begin)
		 (let ((this-exprs exprs))
		    (shrink! this)
		    ;; cut the list
		    (set-cdr! last-p '())
		    ;; move the stmt-elements to the surrounding stmt-begin
		    (move-to-begin (instantiate::Begin
				      (exprs this-exprs))
				   walk!
				   surrounding-fun
				   stmt-begin
				   #f) ;; no need to retrieve result
		    ;; update this begin.
		    (set! exprs remaining)
		    (default-walk! this surrounding-fun #f))))))))

(define-nmethod (Call.stmts! surrounding-fun stmt-begin)
   (with-access::Stmt-Call this (operator operands stmt-operator? stmt-operands)
      (define (stmt-ops-count)
	 (let loop ((ops (cons stmt-operator? (or stmt-operands '())))
		    (count 0))
	    (cond
	       ((null? ops)
		count)
	       ((car ops)
		(loop (cdr ops) (+ count 1)))
	       (else
		(loop (cdr ops) count)))))

      (let ((stmt-count (stmt-ops-count)))
	 (cond
	    ((zero? stmt-count) ;; no el is in stmt-form.
	     (shrink! this)
	     (default-walk! this surrounding-fun stmt-begin))
	    ((not stmt-begin)
	     (let ((bnode (instantiate::Begin
			     (exprs (list this)))))
		(walk! this surrounding-fun bnode)
		bnode))
	    (else
	     ;; we can't simply recurse, as this could yield undefined
	     ;; order:
	     ;; ((if s1 a b) (if s2 c d))
	     ;; would yield
	     ;; x=s1, y=s2, ((if x a b) (if y c d)), and we would have the first
	     ;; 'if' mixed with second 'if' (in execution).
	     ;;  for instance we could have the execution: s1, s2, a, c.
	     ;; allow the first stmt-op to be partially moved. the remaining ones
	     ;; are completely moved.
	     (let ((dummy 'dummy)) ;; we need a let so we can have 'define's here.
		(define (move-stmt-walk n stmt-form? partial?)
		   (cond
		      ((and stmt-form? partial?)
		       ;; we are allowed to partially move the stmt.
		       (walk! n surrounding-fun stmt-begin))
		      (stmt-form?
		       ;; we must move the entire operand/operator
		       ;; don't care if it is before or after other elements.
		       (move-to-begin n walk! surrounding-fun stmt-begin))
		      (else
		       (walk! n surrounding-fun #f))))
		
		(define (unaffected? n)
		   (or (Const? n)
		       (and (Ref? n)
			    (with-access::Ref n (var)
			       (with-access::Var var (constant?)
				  constant?)))))
		
		(set! operator (move-stmt-walk operator stmt-operator? #t))

		(if (not stmt-operands)
		    (set! operands
			  (map! (lambda (o)
				   (walk! o surrounding-fun stmt-begin))
				operands))
		    (let loop ((ops operands)
			       (stmts? stmt-operands)
			       (partial? (unaffected? operator)))
		       (cond
			  ((null? ops)
			   'done)
			  (else
			   (set-car! ops
				     (move-stmt-walk (car ops) (car stmts?)
						     partial?))
			   (loop (cdr ops) (cdr stmts?)
				 (and partial? (unaffected? (car ops))))))))
		(shrink! this)
		this))))))

(define-nmethod (Frame-alloc.stmts! surrounding-fun stmt-begin)
   this)

(define-nmethod (Frame-push.stmts! surrounding-fun stmt-begin)
   (if (not stmt-begin)
       (default-walk! this surrounding-fun #f)
       (move-to-begin this walk! surrounding-fun stmt-begin)))

(define-nmethod (Return.stmts! surrounding-fun stmt-begin)
   (if stmt-begin
       (move-to-begin this walk! surrounding-fun stmt-begin
		      #f) ;; no need to get result.
       (default-walk! this surrounding-fun #f)))

(define-nmethod (Labeled.stmts! surrounding-fun stmt-begin)
   (if stmt-begin
       (move-to-begin this walk! surrounding-fun stmt-begin)
       (default-walk! this surrounding-fun #f)))

(define-nmethod (Break.stmts! surrounding-fun stmt-begin)
   (if stmt-begin
       (move-to-begin this walk! surrounding-fun stmt-begin
		      #f) ;; no need to get result
       (default-walk! this surrounding-fun #f)))

(define-nmethod (Continue.stmts! surrounding-fun stmt-begin)
   (if stmt-begin
       (move-to-begin this walk! surrounding-fun stmt-begin
		      #f) ;; no need to get result
       this))

(define-nmethod (Pragma.stmts! surrounding-fun stmt-begin)
   this)

;; Tail-rec and Tail-rec-Call do not exist anymore.

(define-nmethod (While.stmts! surrounding-fun stmt-begin)
   (with-access::While this (init test body)
      ;; by construction test cannot be in statement-form.
      (if stmt-begin
	  (move-to-begin this walk! surrounding-fun stmt-begin)
	  (default-walk! this surrounding-fun #f))))
      
(define-nmethod (Call/cc-Resume.stmts! surrounding-fun stmt-begin)
   (scheme2js-error
    "statements"
    "internal error: Call/cc-Resume nodes should not exist yet."
    #f
    this))
