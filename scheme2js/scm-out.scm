(module scm-out
   (import nodes
	   export
	   tools
	   walk)
   (export (scm-out tree p)))

;; NOT THREAD-SAFE.
;; (should not be a problem, as this module should only be used for
;; debugging...)

(define (scm-out tree p)
   (pp (scm tree #f)
       p))

(define (var-out v::Var)
   (with-access::Var v (id)
      id))

(define (label-out label::Label)
   (with-access::Label label (id)
      (symbol-append 'Label- id)))

(define-nmethod (Node.scm)
   (error "scm-out"
	  "forgot node-type"
	  this))

(define-nmethod (Const.scm)
   (with-access::Const this (value)
      value))

(define-nmethod (Ref.scm)
   (with-access::Ref this (var)
      (var-out var)))

(define-nmethod (Module.scm)
   (with-access::Module this (declared-vars body)
      `(let ,(map (lambda (var)
		     `(,(var-out var) #unspecified))
		  declared-vars)
	  ,(walk body))))

(define-nmethod (Lambda.scm)
   (with-access::Lambda this (vaarg? formals body)
      (let* ((scm-formals (map walk formals))
	     (vaarg-formals (if vaarg?
				(let loop ((frms scm-formals))
				   (if (null? (cdr frms))
				       (car frms)
				       (cons (car frms)
					     (loop (cdr frms)))))
				scm-formals)))
	 `(lambda ,vaarg-formals
	     (bind-exit (return)
		,(walk body))))))

(define-nmethod (If.scm)
   (with-access::If this (test then else)
      `(if ,(walk test)
	   ,(walk then)
	   ,(walk else))))

;; not thread-safe! (but scm-out should only used for debugging purposes...
(define *key-id* #unspecified)

(define-nmethod (Case.scm)
   (with-access::Case this (key clauses)
      ;; clauses might contain var-refs instead of consts...
      ;; -> use cond now...
      (let ((tmp (gensym 'tmp)))
	 (set! *key-id* tmp)
	 `(let ((,tmp ,(walk key)))
	     ,(map walk clauses)))))

(define-nmethod (Clause.scm)
   (with-access::Clause this (consts expr default-clause?)
      (if default-clause?
	  `(else ,(walk expr))
	  `((member ,*key-id* ,(map walk consts))
	    ,(walk expr)))))

(define-nmethod (Set!.scm)
   (with-access::Set! this (lvalue val)
      `(set! ,(walk lvalue) ,(walk val))))

(define-nmethod (Let.scm)
   (with-access::Let this (scope-vars bindings body kind)
      `(,kind
	,(map (lambda (var)
		 `(,(var-out var) #unspecified))
	      scope-vars)
	,@(map walk bindings)
	,(walk body))))

(define-nmethod (Begin.scm)
   (with-access::Begin this (exprs)
      `(begin ,@(map walk exprs))))

(define-nmethod (Call.scm)
   (with-access::Call this (operator operands)
      `(,(walk operator) ,@(map walk operands))))

(define-nmethod (Frame-alloc.scm)
   (with-access::Frame-alloc this (storage-var vars)
      `(let ((storage-var ,(var-out storage-var))
	     (vars ,(map var-out vars)))
	  'TODO)))

(define-nmethod (Frame-push.scm)
   (with-access::Frame-push this (storage-vars body)
      `(let ((storage-vars ,(map var-out storage-vars)))
	  ,(walk body))))

(define-nmethod (Return.scm)
   (with-access::Return this (val)
      `(return ,(walk val))))

(define-nmethod (Labeled.scm)
   (with-access::Labeled this (label body)
      `(bind-exit (,(label-out label))
	  ,(walk body))))

(define-nmethod (Break.scm)
   (with-access::Break this (val label)
      `(,(label-out label) ,(walk val))))

(define-nmethod (Continue.scm)
   (with-access::Continue this (label)
      `(,(label-out label) #unspecified)))

(define-nmethod (Pragma.scm)
   (with-access::Pragma this (str)
      str))

(define-nmethod (Tail-rec.scm)
   (with-access::Tail-rec this (scope-vars inits body label)
      (let ((loop (label-out label)))
	 `(let ,loop ,(map (lambda (set)
			      (with-access::Set! set (lvalue val)
				 `(,(walk lvalue) ,(walk val))))
			   inits)
	       ,(walk body)))))

(define-nmethod (Tail-rec-Call.scm)
   (with-access::Tail-rec-Call this (updates label)
      `(,(label-out label) ,(map walk updates))))

(define-nmethod (While.scm)
   (with-access::While this (init scope-vars test body label)
      (let ((loop (gensym 'while)))
	 `(begin
	     ,(walk init)
	     (let ,loop ()
	       (when ,(walk test)
		  (bind-exit (,(label-out label))
		     ,(walk body))))))))
