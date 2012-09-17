;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-12 Florian Loitsch, see LICENSE file         */
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
	   (with-access::Var this-var (uses)
	      (zero? uses)))))
      
(define (can-be-inlined? var::Var)
   (define (imported-var? v)
      (with-access::Var v (kind)
	 (eq? kind 'imported)))
   (define (exported-as-mutable? v)
      (with-access::Var v (kind export-desc)
	 (and (eq? kind 'exported)
	      (with-access::Export-Desc export-desc (exported-as-const?)
	      (not exported-as-const?)))))
   (with-access::Var var (constant? value)
      (and constant?
	   value
	   (not (imported-var? var))
	   (not (exported-as-mutable? var))
	   (isa? value Lambda)
	   (lambda-can-be-inlined? value))))

;; can we move a function from its definition to its use?
(define (can-be-moved? var::Var)
   (and (with-access::Var var (kind) (eq? kind 'local))
	(can-be-inlined? var)))

;; functions that are only used once are directly moved to this position
;; obvious exception: exported functions (and functions that capture at their
;; creation).

(define (single-use! tree)
   (verbose " single-use")
   (let ((env (instantiate::Single-Use-Env (count 0))))
      (with-access::Single-Use-Env env (count)
	 (single! tree env '())
	 (> count 0))))

(define-nmethod (Node.single! surrounding-funs)
   (default-walk! this surrounding-funs))

(define-nmethod (Lambda.single! surrounding-funs)
   (default-walk! this (cons this surrounding-funs)))

(define-nmethod (Call.single! surrounding-funs)
   (with-access::Call this (operator)
      (when (isa? operator Ref)
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
      (with-access::Clone-Env env (counter)
	 (clone tree env 0)
	 (> counter 0))))


(define-generic (clone this::Node env nested-counter)
  (letrec*
    ((default-walk
       (lambda (node nested-counter)
          (walk1 node env clone nested-counter)))
     (walk (lambda (node nested-counter)
              (clone node env nested-counter))))
    (default-walk this nested-counter)))
;* (define-nmethod (Node.clone nested-counter)                         */
;*    (default-walk this nested-counter))                              */

(define-nmethod (Call.clone nested-counter)
   (define (good-for-inlining? var::Var nested-counter)
      (with-access::Var var (uses value)
	 (with-access::Clone-Env env (max-rec-inline max-inline-size)
	 (and (can-be-inlined? var)
	      (< nested-counter max-rec-inline)
	      (or (=fx uses 1)
		  (with-access::Lambda value (size nested-closures?)
		     (and (not nested-closures?)
			  (<=fx size (/fx max-inline-size
					  (+fx nested-counter 1))))))))))

   (default-walk this nested-counter)
   (with-access::Call this (operator)
      (when (isa? operator Ref)
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
      (with-access::Inline-Env env (counter)
	 (absorb! tree env #f #f)
	 (> counter 0))))

(define-nmethod (Node.absorb! label fun)
   (default-walk! this label fun))

(define-nmethod (Set!.absorb! label fun)
   (with-access::Set! this (lvalue val)
      (with-access::Ref lvalue (var)
	 (when (isa? var Inlined-Local)
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
      (if (and (isa? operator Lambda)
	       (lambda-can-be-inlined? operator))
	  (with-access::Lambda operator (formals vaarg? body)
	     ;; body must be a Return.
	     ;; no need to include it...
	     (let* ((return-body (with-access::Return body (val) val))
		    (assigs-mapping (parameter-assig-mapping
				     (if (isa? fun Ref)
					 (with-access::Ref fun (var)
					    (with-access::Var var (id)
					    id)))
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
		    (return-label (instantiate::Label
				     (id (gensym 'inlined))))
		    (return-labeled (instantiate::Labeled
					(body return-body)
					(label return-label)))
		    (traversed-labeled (walk! return-labeled return-label fun)))
		(with-access::Inline-Env env (counter)
		   (set! counter (+ counter 1)))
		(instantiate::Let
		   (scope-vars (map (lambda (f) (with-access::Ref f (var) var))
				  formals))
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
