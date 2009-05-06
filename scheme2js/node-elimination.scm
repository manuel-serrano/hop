(module node-elimination
   (import nodes
	   export-desc
	   walk
	   verbose
	   mark-statements)
   (static (class Elim-Env
	      ;; default-label represents a normal label.
	      ;; the target is in here.
	      (default-label-val-is-needed? (default #f)))
	   (wide-class Elim-Label::Label
	      val-is-needed?::bool
	      (used?::bool (default #f))))
   (export (node-elimination! tree::Module)))

(define (node-elimination! tree::Module)
   (verbose "node-elimination")
   (elim! tree (instantiate::Elim-Env) #f))

(define-nmethod (Node.elim! val-is-needed?)
   (error 'elim
	  "Internal Error: Forgot node type"
	  this))

(define-nmethod (Const.elim! val-is-needed?)
   this)

(define-nmethod (Ref.elim! val-is-needed?)
   this)

(define-nmethod (Module.elim! val-is-needed?)
   ;; if the result of a module is needed, assign the complete expression...
   (default-walk! this #f))

(define-nmethod (Lambda.elim! val-is-needed?)
   (default-walk! this #f)) ;; only something in a return is needed.

(define-nmethod (If.elim! val-is-needed?)
   (with-access::If this (test then else)
      (set! test (walk! test #t))
      (set! then (walk! then val-is-needed?))
      (set! else (walk! else val-is-needed?))
      ;; some unlikely cases, but does not cost much to test for them...
      (cond
	 ((Const? test) ;; (if #t x y)
	  (if (Const-value test)
	      then
	      else))
	 ((and (Const? then) ;; (begin (if (do-something) 1 2) ... )
	       (Const? else)
	       (not val-is-needed?))
	  test)
	 (else
	  this))))

(define-nmethod (Case.elim! val-is-needed?)
   (with-access::Case this (key clauses)
      (set! key (walk! key #t))
      (set! clauses (map! (lambda (clause) (walk! clause val-is-needed?))
			  clauses))
      this))

(define-nmethod (Clause.elim! val-is-needed?)
   (default-walk! this val-is-needed?))

;; remove x=x sets
(define-nmethod (Set!.elim! val-is-needed?)
   (default-walk! this #t) ;; both are needed.
   (with-access::Set! this (lvalue val)
      (if (and (Ref? val)
	       (eq? (Ref-var lvalue)
		    (Ref-var val)))
	  (instantiate::Const
	     (value #unspecified))
	  this)))

(define-nmethod (Let.elim! val-is-needed?)
   (with-access::Let this (scope-vars bindings body)
      ;; remove empty Lets
      (if (null? scope-vars)
	  (let ((bnode (instantiate::Begin
			  (exprs (append bindings (list body))))))
	     (walk! bnode val-is-needed?))
	  (begin
	     (set! bindings (map! (lambda (b) (walk! b #f)) bindings))
	     (set! body (walk! body val-is-needed?))
	     this))))

;; if Begin only contains one entry, replace it by this entry.
;; if a Begin contains another Begin merge them.
(define-nmethod (Begin.elim! val-is-needed?)
   (with-access::Begin this (exprs)
      (cond
	 ((null? exprs)
	  (instantiate::Const
	     (value #unspecified)))
	 ((null? (cdr exprs))
	  (walk! (car exprs) val-is-needed?))
	 (else
	  ;; walk through all exprs.
	  ;; if the expr is a Begin, merge it with this one.
	  (let loop ((exprs exprs))
	     (unless (null? exprs)
		(let ((expr (walk! (car exprs) (and val-is-needed?
						    (null? (cdr exprs)))))
		      (exprs-tail (cdr exprs)))
		   (set-car! exprs expr)
		   (if (Begin? expr)
		       ;; insert into our list.
		       (let ((other-exprs (Begin-exprs expr)))
			  ;; we know there must be at least 2 elements.
			  ;; otherwise we wouldn't have gotten a 'Begin'.
			  (set-car! exprs (car other-exprs))
			  (set-cdr! exprs (cdr other-exprs))
			  (set-cdr! (last-pair other-exprs) exprs-tail)))
		   (loop exprs-tail))))
	  ;; weed out atoms or functions in non-last position (dead-code)
	  ;; a 'filter!' that ignores the last element.
	  ;; if there's only one element left, it is returned.
	  (let loop ((exprs exprs)
		     (head exprs)
		     (last #f)) ;; last-pair, that is in the 'accepted' list
	     (cond
		((null? exprs) ;; happens only if the not val-is-needed (in
		               ;; which case the last el can be removed too).
		 (cond
		    ((not last) ;; no element got through our weeding
		     (instantiate::Const
			(value #unspecified)))
		    ((null? (cdr head)) ;; only one element got through
		     (car head))
		    (else
		     (Begin-exprs-set! this head)
		     this)))
		((and (null? (cdr exprs))
		      val-is-needed?)
		 (if last
		     (begin
			(Begin-exprs-set! this head)
			this)
		     (car exprs)))
		((or (Lambda? (car exprs))
		     (Const? (car exprs))
		     (Ref? (car exprs)))
		 (if last
		     (begin
			(set-cdr! last (cdr exprs)) ;; remove the current el.
			(loop (cdr exprs)
			      head   ;; keep old head
			      last)) ;; keep old last
		     (loop (cdr exprs)
			   (cdr exprs) ;; head is the next element (for now)
			   #f)))
		((or (Break? (car exprs))
		     (Return? (car exprs))
		     (Continue? (car exprs)))
		 ;; remove remaining els
		 (set-cdr! exprs '())
		 (if last
		     (begin
			(Begin-exprs-set! this head)
			this)
		     (car exprs)))
		(else
		 (loop (cdr exprs)
		       head          ;; keep head
		       exprs)))))))) ;; we are the last pair that got through

(define-nmethod (Call.elim! val-is-needed?)
   (default-walk! this #t))

(define-nmethod (Frame-alloc.elim! val-is-needed?)
   this)

(define-nmethod (Frame-push.elim! val-is-needed?)
   (default-walk! this val-is-needed?))

(define-nmethod (Return.elim! val-is-needed?)
   (default-walk! this #t))

;; remove unused labeled.
(define-nmethod (Labeled.elim! val-is-needed?)
   (with-access::Labeled this (label body)
      (widen!::Elim-Label label
	 (val-is-needed? val-is-needed?))
      (default-walk! this val-is-needed?)
      (with-access::Elim-Label label (used?)
	 (if (not used?)
	     body
	     (begin
		(shrink! label)
		this)))))

(define-nmethod (Break.elim! val-is-needed?)
   (with-access::Break this (label)
      (if (eq? label (default-label))
	  (with-access::Elim-Env env (default-label-val-is-needed?)
	     (default-walk! this default-label-val-is-needed?))
	  (with-access::Elim-Label label (used? val-is-needed?)
	     (set! used? #t)
	     (default-walk! this val-is-needed?)))))

(define-nmethod (Continue.elim! val-is-needed?)
   (with-access::Continue this (label)
      (unless (eq? label (default-label))
	 (with-access::Elim-Label label (used?)
	    (set! used? #t))))
   this)

(define-nmethod (Pragma.elim! val-is-needed?)
   this)

(define-nmethod (Tail-rec.elim! val-is-needed?)
   (with-access::Tail-rec this (inits body)
      (set! inits (map! (lambda (init) (walk! init #f))
			inits))
      (set! body (walk! body val-is-needed?))
      this))

(define-nmethod (Tail-rec-Call.elim! val-is-needed?)
   (default-walk! this #t))

(define-nmethod (While.elim! val-is-needed?)
   (with-access::While this (label init test body)
      (with-access::Elim-Env env (default-label-val-is-needed?)
	 (set! default-label-val-is-needed? val-is-needed?))

      (unless (eq? label (default-label))
	 (widen!::Elim-Label label
	    ;; will not be used but needs to be set.
	    ;; important for While-labels is the 'used?'-field.
	    (val-is-needed? val-is-needed?)))

      (set! init (walk! init #f))
      (set! test (walk! test #t))
      (set! body (walk! body #f)) ;; if needed, then there is a Labeled.

      (unless (eq? label (default-label))
	 (with-access::Elim-Label label (used?)
	    (if used?
		(shrink! label)
		(set! label (default-label)))))
      this))

(define-nmethod (Call/cc-Resume.elim! val-is-needed?)
   this)
