(module deep-clone
   (import nodes
	   export
	   tools
	   walk
	   verbose)
   (static (wide-class Cloned-Label::Label
	      replacement::Label)
	   (wide-class Cloned-Local::Local
	      replacement::Local)
	   (wide-class Cloned-Lambda::Lambda
	      replacement::Lambda)
	   (wide-class Cloned-This::This-Var
	      replacement::This-Var))
   (export (deep-clone o::Node)))

;; recursively clones the given node and its children.
;; made for 'inlining'. If it is used otherwise the code has to be reviewed!
;;     (maybe it works, but I haven't payed attention).
;;
;; main-difficulty: when cloning a Let, Lambda, or Labeled the bodies have to
;; reference the new vars/labels. (that's what the Cloned-XXX above are for.
;;
;; most Nodes are redirected to 'do-clone' which simply recursively
;; clones. Others (vars, lambdas, ...) are directly treated in 'clone'. They
;; more or less represent special cases. Note, that 'clone' is generic on
;; 'object', but 'do-clone' is generic on 'Nodes' only.

(define (deep-clone o)
   (clone o))

(define-generic (clone this::object)
   (error "clone"
	  "forgot something..."
	  this))

(define-method (clone this::Node)
   (do-clone this))

(define-method (clone this::Label)
   this)
(define-method (clone this::Cloned-Label)
   (with-access::Cloned-Label this (replacement)
      replacement))

(define-method (clone this::Var)
   this)
(define-method (clone this::Cloned-Local)
   (with-access::Cloned-Local this (replacement)
      replacement))
(define-method (clone this::Cloned-This)
   (with-access::Cloned-This this (replacement)
      replacement))
(define (duplicate-Local var::Local)
   (with-access::Local var (value)
      (let ((new-value (cond
			  ((Cloned-Local? value)
			   (Cloned-Local-replacement value))
			  ((Cloned-Lambda? value)
			   (Cloned-Lambda-replacement value))
			  ((Cloned-This? value)
			   (Cloned-This-replacement value))
			  (else value))))
	 (duplicate::Local var
	    (value new-value)))))

(define-method (clone this::Lambda)
   (with-access::Lambda this (scope-vars this-var formals body)
      (let* ((new-scope-vars (map (lambda (var) (duplicate-Local var))
				  scope-vars))
	     (new-this-var (duplicate::This-Var this-var))
	     (new-lambda (duplicate::Lambda this
			    (scope-vars new-scope-vars)
			    (this-var new-this-var))))
	 ;; store clone-info
	 (map (lambda (old-var new-var)
		 (widen!::Cloned-Local old-var (replacement new-var)))
	      scope-vars new-scope-vars)
	 (widen!::Cloned-This this-var (replacement new-this-var))
	 (widen!::Cloned-Lambda this (replacement new-lambda))

	 (Lambda-formals-set! new-lambda (map clone formals))
	 (Lambda-body-set! new-lambda (clone body))
	 new-lambda)))

(define-method (clone this::Let)
   (with-access::Let this (scope-vars)
      (for-each (lambda (var)
		   (widen!::Cloned-Local var
		      (replacement (duplicate-Local var))))
		scope-vars)
      (do-clone this)))
   

(define-method (clone this::Labeled)
   (with-access::Labeled this (label)
      (widen!::Cloned-Label label
	 (replacement (make-Label (gensym (Label-id label)))))
      (do-clone this)))

(define-method (clone this::Tail-rec)
   (with-access::Tail-rec this (label scope-vars)
      (for-each (lambda (var)
		   (widen!::Cloned-Local var
		      (replacement (duplicate-Local var))))
		scope-vars)
      (widen!::Cloned-Label label
	 (replacement (make-Label (gensym (Label-id label)))))
      (do-clone this)))

(define-method (clone this::Call/cc-Resume)
   (error "clone"
	  "clone on Call/cc-Resume should never happen. Change method!"
	  #f))

(define-macro (define-do-clone class . fields)
   `(define-method (do-clone ,(symbol-append 'this:: class))
       (,(symbol-append 'duplicate:: class)
	this
	,@(map (lambda (f)
		  (cond
		     ;; ex: (exprs) => (exprs (map clone (N-exprs this)))
		     ((pair? f)
		      (let ((name (car f)))
			 `(,name (map clone
				      (,(symbol-append class '- name) this)))))
		     (else
		      `(,f (clone (,(symbol-append class '- f) this))))))
	       fields))))

(define-generic (do-clone this::Node)
   (error "do-clone"
	  "forgot Node-type"
	  this))

(define-do-clone Const)
(define-do-clone Ref var)
;; Lambda can't appear here
(define-do-clone If test then else)
(define-do-clone Case key (clauses))
(define-do-clone Clause (consts) expr)
(define-do-clone Set! lvalue val)
(define-do-clone Let (scope-vars) (bindings) body)
(define-do-clone Begin (exprs))
(define-do-clone SCall operator (operands))
(define-do-clone Return val)
(define-do-clone Labeled label body)
(define-do-clone Break val label)
(define-do-clone Continue label)
(define-do-clone Pragma)
(define-do-clone Tail-rec (scope-vars) (inits) body label)
(define-do-clone Tail-rec-Call (updates) label)

;; should not be necessary, as deep-clone is only called from inside 'inline'.
(define-do-clone Frame-alloc storage-var (vars))
(define-do-clone Frame-push body (storage-vars))
(define-do-clone While test body label)
(define-do-clone Call/cc-Call operator (operands))
