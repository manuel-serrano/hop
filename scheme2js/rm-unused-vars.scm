(module rm-unused-vars
   (import nodes
	   export-desc
	   tools
	   walk
	   use-count
	   verbose)
   (static (wide-class Rm-Var::Local))
   (export (rm-unused-vars! tree::Module)))

;; Variables with use-count 0 are not used, and can be removed.
;; Attention: external variables are excempted from this rule.
(define (rm-unused-vars! tree)
   (verbose " removing unused vars")
   (use-count tree)
   (rm! tree #f))

(define-nmethod (Node.rm!)
   (default-walk! this))

(define-nmethod (Set!.rm!)
   (with-access::Set! this (lvalue val)
      (with-access::Ref lvalue (var)
	 (if (Rm-Var? var)
	     (walk! val)
	     (default-walk! this)))))

(define-nmethod (Let.rm!)
   (with-access::Let this (scope-vars)
      (set! scope-vars
	    (filter! (lambda (var)
			(with-access::Var var (uses)
			   (if (zero? uses)
			       (begin
				  (widen!::Rm-Var var)
				  #f)
			       #t)))
		     scope-vars))
      (default-walk! this)))
