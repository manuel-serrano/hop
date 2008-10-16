(module loop-hoist
   (import config
	   tools
	   nodes
	   export
	   walk
	   side
	   captured-vars
	   verbose)
   (static (wide-class Hoist-While::While
	      (decls-to-hoist (default '()))))
   (export (loop-hoist! tree::Module)))

;; moves function-definitions outside loops (unless they are closures).
(define (loop-hoist! tree)
   (when (and (config 'loop-hoist)
	      (not (config 'suspend/resume)))
      (verbose "loop hoist")
      (side-effect tree)
      (captured-vars tree)
      (hoist! tree #f #f)))

;; outer-loop represents the least nested loop.
(define-nmethod (Node.hoist! outer-loop)
   (default-walk! this outer-loop))

;; Module.hoist! is not necessary.
(define-nmethod (Module.hoist! outer-loop)
   (default-walk! this #f))
(define-nmethod (Lambda.hoist! outer-loop)
   (default-walk! this #f))

(define-nmethod (Set!.hoist! outer-loop)
   (default-walk! this outer-loop)
   (with-access::Set! this (lvalue val)
      (with-access::Ref lvalue (var)
	 (with-access::Var var (constant?)
	    (if (and outer-loop
		     constant?
		     (Lambda? val)
		     (not (Lambda-closure? val)))
		(with-access::Hoist-While outer-loop (decls-to-hoist)
		   ;; moves this Set! outside all surrounding-loops.
		   (cons-set! decls-to-hoist this)
		   (instantiate::Const (value #unspecified)))
		this)))))

(define-nmethod (While.hoist! outer-loop)
   (if outer-loop
       (default-walk! this outer-loop)
       (begin
	  (widen!::Hoist-While this)
	  (default-walk! this this)
	  (with-access::Hoist-While this (decls-to-hoist)
	     (if (pair? decls-to-hoist)
		 (begin0
		  (instantiate::Begin
		     (exprs (append! decls-to-hoist (list this))))
		  (shrink! this))
		 this)))))
