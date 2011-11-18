;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-11 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module trampoline
   (import config
	   nodes
	   export-desc
	   walk
	   symbol
	   tail
	   side
	   verbose
	   var-ref-util
	   tools)
   (static (wide-class Trampoline-Lambda::Lambda
	      (finished?::bool (default #f))))
   (export (trampoline tree::Module)))

;; find tail-calls, and mark them with .tail-call?
;; for our trampolines to work we must not execute any tail-call during
;; evaluation of the operator and operands. -> if any of them is not just a
;; const or a var-ref, move them in front of the call (using temporary
;; variables). As the creation of these variables creates new scopes, we have
;; to do this pass before the scope-resolution pass.
(define (trampoline tree::Module)
   (when (config 'trampoline)
      (verbose "trampoline")
      ;; we need the result of both analyses. For now this is possible.
      ;; However, this is slightly a hack. If side-effect was to use
      ;; wide-classes we would be doomed.
      (tail-calls tree)
      (side-effect tree)
      (trampoline! tree #f #f)))

(define-nmethod (Node.trampoline! current-fun)
   (default-walk! this current-fun))

(define-nmethod (Trampoline-Lambda.trampoline! current-fun)
   this)

(define-nmethod (Lambda.trampoline! current-fun)
   (widen!::Trampoline-Lambda this)
   (default-walk! this this)
   (with-access::Trampoline-Lambda this (finished?)
      (set! finished? #t))
   this)

(define (potential-tail-fun? operator walk!)
   (let ((target (call-target operator)))
      (cond
	 ((not target) #t)
	 ((isa? target Trampoline-Lambda)
	  (with-access::Trampoline-Lambda target (finished?
						  contains-trampoline-call?)
	     (or (not finished?) contains-trampoline-call?)))
	 ((isa? target Lambda)
	  (walk! target #f))
	 ((runtime-ref? target)
	  #f)
	 (else #t))))

(define (a-normal-form call)
   (define (allowed-call? call)
      (with-access::Call call (operator operands)
	 (let ((target (call-target operator)))
	    (and (runtime-ref? target)
		 (not (higher-order-runtime-ref? target))
		 ;; finally weed things like: (+ (f ..) (let ...))
		 (every? (lambda (op)
			    (or (isa? op Const)
				(isa? op Ref)))
			 operands)))))

   (let ((hoisted '()))
      (define (hoist n)
	 (cond
	    ((or (isa? n Const)
		 (isa? n Ref))
	     n)
	    ((and (isa? n Call)
		  (allowed-call? n))
	     n)
	    (else
	     (let* ((tmp-decl (Ref-of-new-Var 'tail))
		    (tmp-var (with-access::Ref tmp-decl (var) var))
		    (assig (var-assig tmp-var n)))
		(cons-set! hoisted assig)
		tmp-decl))))

      (with-access::Call call (operator operands)
	 (set! operator (hoist operator))
	 (set! operands (map! hoist operands))
	 (if (null? hoisted)
	     call
	     (instantiate::Let
		(scope-vars (map (lambda (assig)
				    (with-access::Set! assig (lvalue)
				       (with-access::Ref lvalue (var) var)))
				 hoisted))
		(bindings hoisted)
		(body call)
		(kind 'let))))))

(define-nmethod (Call.trampoline! current-fun)
   (default-walk! this current-fun))

(define-nmethod (Tail-Call.trampoline! current-fun)
   (default-walk! this current-fun)
   (with-access::Tail-Call this (operator trampoline?)
      (set! trampoline? (potential-tail-fun? operator walk!))

      (when (and trampoline? current-fun)
	 (with-access::Trampoline-Lambda current-fun
	       (contains-trampoline-call?)
	    (set! contains-trampoline-call? #t)))
      (if trampoline?
	  (a-normal-form this)
	  this)))
