;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-10 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module inline
   (import config
	   tools
	   nodes
	   export-desc
	   walk
	   deep-clone
	   side
	   use-count
	   nested-closures
	   fun-size
	   transform-util
	   verbose)
   (static (class Single-Use-Env
	      count::bint)
	   (class Clone-Env
	      counter::bint
	      max-rec-inline::bint
	      max-inline-size::bint)
	   (class Inline-Env
	      counter::bint)
	   (wide-class Inlined-Call::Call
	      cloned-fun::Lambda)
	   (wide-class Inlined-Local::Var))
   (export (inline! tree::Module full?::bool)))


(define (inline! tree full?)
   (if (config 'do-inlining)
       (let ((called-inline-funs? #f))
	  (verbose "inlining")
	  (side-effect tree)
	  (use-count tree)
	  (fun-size tree)
	  (when (single-use! tree) 
	     (inline-funs! tree)
	     (when full?
		(set! called-inline-funs? #t)
		(side-effect tree)
		(use-count tree)
		(fun-size tree)))
	  (when full?
	     (nested-closures tree)
	     (when (or (clone-funs tree)
		       (not called-inline-funs?))
		(inline-funs! tree))))))

(define (lambda-can-be-inlined? l::Lambda)
   (with-access::Lambda l (closure? this-var)
      (and (not closure?)
	   ;; do not inline functions that access 'this'
	   (zero? (Var-uses this-var)))))
      
(define (can-be-inlined? var::Var)
   (define (imported-var? v)
      (eq? (Var-kind v) 'imported))
   (define (exported-as-mutable? v)
      (and (eq? (Var-kind v) 'exported)
	   (not (Export-Desc-exported-as-const? (Var-export-desc v)))))
   (with-access::Var var (constant? value)
      (and constant?
	   value
	   (not (imported-var? var))
	   (not (exported-as-mutable? var))
	   (Lambda? value)
	   (lambda-can-be-inlined? value))))

;; can we move a function from its definition to its use?
(define (can-be-moved? var::Var)
   (and (eq? (Var-kind var) 'local)
	(can-be-inlined? var)))

;; functions that are only used once are directly moved to this position
;; obvious exception: exported functions (and functions that capture at their
;; creation).

(define (single-use! tree)
   (verbose " single-use")
   (let ((env (make-Single-Use-Env 0)))
      (single! tree env '())
      (> (Single-Use-Env-count env) 0)))

(define-nmethod (Node.single! surrounding-funs)
   (default-walk! this surrounding-funs))

(define-nmethod (Lambda.single! surrounding-funs)
   (default-walk! this (cons this surrounding-funs)))

(define-nmethod (Call.single! surrounding-funs)
   (with-access::Call this (operator)
      (when (Ref? operator)
	 (with-access::Ref operator (var)
	    (with-access::Var var (uses value)
	       (if (and (= uses 1)
			(can-be-moved? var)
			;; don't inline, if we are inside ourselves.
			;; in this case the function is inaccesible, but that's
			;; not our problem...
			(not (memq value surrounding-funs)))
		   (begin
		      (widen!::Inlined-Local var)
		      (with-access::Single-Use-Env env (count)
			 (set! count (+fx count 1)))
		      (set! operator value)))))))
   (default-walk! this surrounding-funs))
      

;; clones function definitions to their call-targets if they are suitable for
;; inlining.
(define (clone-funs tree)
   (verbose " clone-funs")
   (let ((env (instantiate::Clone-Env
		 (counter 0)
		 (max-rec-inline (config 'rec-inline-nb))
		 (max-inline-size (config 'max-inline-size)))))
      (clone tree env 0)
      (> (Clone-Env-counter env) 0)))


(define-nmethod (Node.clone nested-counter)
   (default-walk this nested-counter))

(define-nmethod (Call.clone nested-counter)
   (define (good-for-inlining? var::Var nested-counter)
      (with-access::Var var (uses value)
	 (and (can-be-inlined? var)
	      (< nested-counter (Clone-Env-max-rec-inline env))
	      (or (=fx uses 1)
		  (with-access::Lambda value (size nested-closures?)
		     (and (not nested-closures?)
			  (<=fx size (/fx (Clone-Env-max-inline-size env)
					  (+fx nested-counter 1)))))))))

   (default-walk this nested-counter)
   (with-access::Call this (operator)
      (when (Ref? operator)
	 (with-access::Ref operator (var)
	    (when (good-for-inlining? var nested-counter)
	       (with-access::Var var (value)
		  (let ((cloned (deep-clone value)))
		     (with-access::Clone-Env env (counter)
			(set! counter (+fx counter 1)))
		     (widen!::Inlined-Call this
			(cloned-fun cloned))
		     (walk cloned (+fx nested-counter 1)))))))))


;; if a Call has a lambda as its operator, then the lambda is inlined, thus
;; avoiding the creation of the closure.
;; Other passes in this file only move/copy lambdas to the
;; call-target. Inlining itself is always done here.
(define (inline-funs! tree)
   (verbose " inline-funs!")
   (let ((env (instantiate::Inline-Env
		 (counter 0))))
      (absorb! tree env #f #f)
      (> (Inline-Env-counter env) 0)))

(define-nmethod (Node.absorb! label fun)
   (default-walk! this label fun))

(define-nmethod (Set!.absorb! label fun)
   (with-access::Set! this (lvalue val)
      (with-access::Ref lvalue (var)
	 (when (Inlined-Local? var)
	    (set! val (instantiate::Const (value #unspecified)))
	    (shrink! var)))
      (default-walk! this label fun)))

(define-nmethod (Inlined-Call.absorb! label fun)
   (with-access::Inlined-Call this (operator cloned-fun)
      (let ((nfun operator))
	 (set! operator cloned-fun)
	 (shrink! this)
	 (walk! this label nfun))))

(define-nmethod (Call.absorb! label fun)
   (with-access::Call this (operator operands)
      (if (and (Lambda? operator)
	       (lambda-can-be-inlined? operator))
	  (with-access::Lambda operator (formals vaarg? body)
	     ;; body must be a Return.
	     ;; no need to include it...
	     (let* ((return-body (Return-val body))
		    (assigs-mapping (parameter-assig-mapping
				     (if (Ref? fun) (Var-id (Ref-var fun)))
				     this
				     operands
				     formals
				     vaarg?))
		    (assigs (map (lambda (p)
				    (instantiate::Set!
				       (lvalue (car p))
				       (val (cdr p))))
				 assigs-mapping))
		    (traversed-assigs (map (lambda (node)
					      (walk! node label fun))
					   assigs))
		    (return-label (make-Label (gensym 'inlined)))
		    (return-labeled (instantiate::Labeled
					(body return-body)
					(label return-label)))
		    (traversed-labeled (walk! return-labeled return-label fun)))
		(with-access::Inline-Env env (counter)
		   (set! counter (+ counter 1)))
		(instantiate::Let
		   (scope-vars (map Ref-var formals))
		   (bindings traversed-assigs)
		   (body traversed-labeled)
		   (kind 'let))))
	  (default-walk! this label fun))))

(define-nmethod (Lambda.absorb! label fun)
   (default-walk! this #f fun))

(define-nmethod (Return.absorb! label fun)
   (if label
       (with-access::Return this (val)
	  (walk! (instantiate::Break
		    (val val)
		    (label label))
		 label
		 fun))
       (default-walk! this label fun)))
