(module tail-rec
   (import config
	   tools
	   nodes
	   walk
	   symbol
	   var-ref-util
	   transform-util
	   side
	   tail
	   loop-updates
	   captured-vars
	   verbose)
   (static (wide-class Tail-Lambda::Lambda
	      label::Label)
	   (wide-class Repl-Var::Local
	      replacement::Ref))
   (export (tail-rec! tree::Module)))

;; (define (foo x y z)
;;     bar
;;     (foo x_up y_up z_up))
;;
;; is transformed into:
;;
;; (define (foo x_ y_ z_)
;;    (tail-rec (x y z)
;;              (x_ y_ z_))
;;       bar
;;       (tail-rec-call (x_up y_up z_up))))
;;
;; The meaning of tail-rec being: (let loop ((x x_) (y y_) (z z_))
;; The meaning of tail-rec-call: (loop x_up y_up z_up)
(define (tail-rec! tree)
   (verbose "tail-rec")
   (when (config 'optimize-tail-rec)
      (tail-calls tree)
      (side-effect tree)
      (rec! tree #f #f)))

(define-nmethod (Node.rec! current-fun)
   (default-walk! this current-fun))

(define-nmethod (Tail-Call.rec! current-fun)
   (default-walk! this current-fun)
   (with-access::Call this (operator operands)
      (if (and current-fun
	       (Ref? operator)
	       (with-access::Ref operator (var)
		  (with-access::Var var (constant? value)
		     (and constant?
			  (eq? value current-fun)))))
	  (with-access::Lambda current-fun (formals vaarg?)
	     (let* ((assig-mapping (parameter-assig-mapping operands
							    formals
							    vaarg?))
		    (updates (map! cdr assig-mapping)))
		(unless (Tail-Lambda? current-fun)
		   (widen!::Tail-Lambda current-fun
		      (label (make-Label (gensym 'continue)))))
		(instantiate::Tail-rec-Call
		   (updates updates)
		   (label (Tail-Lambda-label current-fun)))))
	  this)))

(define-nmethod (Lambda.rec! current-fun)
   (default-walk! this this)
   (when (Tail-Lambda? this)
      (with-access::Tail-Lambda this (label body formals scope-vars)
	 ;; replace formals with replacement variables, and replace body
	 ;; with while.
	 (let* ((return-val (Return-val body)) ;; body must be a Return
		(formals-vars (map Ref-var formals))
		(replacement-decls (map (lambda (formal)
					   (with-access::Ref formal (var id)
					      (let ((decl (Ref-of-new-Var id)))
						 (widen!::Repl-Var var
						       (replacement decl))
						 decl)))
					formals))
		(replacement-vars (map Ref-var replacement-decls))
		(inits (map (lambda (formal repl-var)
			       (instantiate::Set!
				  (lvalue formal)
				  (val (var-reference repl-var))))
			    formals
			    replacement-vars))
		(tail-rec (instantiate::Tail-rec
			     (scope-vars formals-vars)
			     (inits inits)
			     (body return-val)
			     (label label))))
				 
	    (set! scope-vars
		  (map! (lambda (var)
			   (if (Repl-Var? var)
			       (with-access::Repl-Var var (replacement)
				  (with-access::Ref replacement (var)
				     var))
			       var))
			scope-vars))
	    (set! formals (map (lambda (formal)
				  (with-access::Ref formal (var)
				     (with-access::Repl-Var var (replacement)
					(let ((tmp replacement))
					   (shrink! var)
					   tmp))))
			       formals))
	    (Return-val-set! body tail-rec)
	    (shrink! this))))
   this)
