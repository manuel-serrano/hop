;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-13 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module letrec-expansion
   (import config
	   error
	   nodes
	   pobject-conv
	   export-desc
	   walk
	   verbose
	   gen-js)
   (static (final-class Letrec-Env
	      (call/cc?::bool read-only)))
   (export (letrec-expansion! tree::Module)))

;; This pass servers two purposes:
;;   1. it transforms non-global defines to letrecs
;;   2. it decomposes letrecs when call/cc is activated.

;; According to the spec a (letrec ((x0 v0) (x1 v1) ...) body) is equivalent
;; to: (let ((x0 _undef) (x1 _undef) ...)
;;       (let ((tmp0 v0) (tmp1 v1) ...)
;;         (set! x0 tmp0) (set! x1 tmp1) ...
;;         body)
;;
;; Our optimizations however do not like it, when there are two assignments to
;; the same variable. Therefore we expand into this form only for call/cc, and
;; when this expansion actually could make a difference.
;;
;; In other words: if v0, v1, ... are constants (or lambda-constructions), then
;; they can stay where they are. Obvious example:
;;  (letrec ((x0 (lambda (...) ... x3))
;;           (x1 3)
;;           (x2 "some_string")
;;           (x3 (lambda (...) x0 x1 x2)))
;;      body)
;;
;; Obviously the initial assignments can directly be replaced by the following
;; assignments. In this case the letrec only served as a way to mutually
;; reference the variables (the most common use for letrec).
;;
;; If however we have:
;;  (letrec ((x0 (some-call ...))
;;           (x1 (some-other-call ...)))
;;     body)
;;
;; or even
;;   (letrec ((x0 (lambda () ...))
;;            (x1 (some-call ...)))
;;     body)
;;
;; then the calls might be call/cc calls, and we expand the letrec. Note that
;; in the second example we coulde leave 'x0' inside the letrec if we were
;; certain, that it was constant (which we do not know yet...)
;;
;; This pass has to happen quite early, as we assume that the bindings are of
;; the form (Set! x0 v0). (Something, that could easily change in later
;; passes).
;;
;; Let-recs are introduced by symbol-pass. This pass must hence happen after
;; the symbol-pass.

(define (letrec-expansion! tree)
   (verbose "letrec-expansion")
   (letrec-expand tree (instantiate::Letrec-Env (call/cc? (config 'call/cc)))))

(define-nmethod (Node.letrec-expand)
   (default-walk this))

(define (letrec-constant? n)
   (or (isa? n Const)
       (isa? n Ref)
       (isa? n Lambda)))

(define-nmethod (Lambda.letrec-expand)
   (with-access::Lambda this (body)
      (with-access::Return body (val)
	 (set! val (defines->letrec! val)))
      (default-walk this)))
   
(define-nmethod (Let.letrec-expand)
   (with-access::Let this (kind body bindings)
      (set! body (defines->letrec! body))
      (default-walk this)
      (when (and (eq? kind 'letrec)
		 (with-access::Letrec-Env env (call/cc?) call/cc?)
		 (any (lambda (binding)
			 (with-access::Set! binding (val)
			    (not (letrec-constant? val))))
		       bindings))
	 (let* ((tmp-vars (map (lambda (ign) (gensym 'ltr-tmp))
			       bindings))
		(location (with-access::Node this (location) location))
		(new-bindings (map (lambda (tmp-var binding)
				      (with-access::Set! binding (val)
					 (instantiate::Set!
					    (location (or location -70))
					    (lvalue (instantiate::Ref
						       (location location)
						       (id tmp-var)))
					    (val val))))
				   tmp-vars
				   bindings))
		(assigs (map (lambda (tmp-var binding)
				(with-access::Set! binding (lvalue)
				   (instantiate::Set!
				      (location (or location -80))
				      (lvalue lvalue)
				      (val (instantiate::Ref
					      (location location)
					      (id tmp-var))))))
			     tmp-vars
			     bindings)))
	    ;; initially declare the bindings to #unspecified
	    (for-each (lambda (binding)
			 (with-access::Set! binding (val)
			    (set! val
				  (instantiate::Const
				     (location location)
				     (value #unspecified)))))
		      bindings)
	    ;; then evaluate the inits and store them in temporary variables
	    (set! body
		  (instantiate::Let
		     (location location)
		     (bindings new-bindings)
		     (body (instantiate::Begin
			      ;; and finally assign them back to the originals.
			      (exprs (append! assigs (list body)))))
		     (kind 'let)))))))

(define (defines->letrec! n)
   (cond
      ((isa? n Define)
       ;; wrap into begin.
       (defines->letrec! (instantiate::Begin
			    (exprs (list n)))))
      ((isa? n Begin)
       (let ((bindings (map (lambda (n) (shrink! n)) (head-defines! n))))
	  (if (null? bindings)
	      n
	      (instantiate::Let
		 (location (with-access::Node n (location) location))
		 (bindings bindings)
		 (body n)
		 (kind 'letrec)))))
      (else n)))

(define (head-defines! bnode::Begin)
   (define (inner bnode::Begin rev-found-defines finish-fun)
      (let loop ((exprs (with-access::Begin bnode (exprs) exprs))
		 (rev-defines rev-found-defines))
	 (cond
	    ((null? exprs)
	     rev-defines)
	    ((not (pair? exprs))
	     (scheme2js-error  'begin "Illegal form" bnode
		(with-access::Node bnode (location) location)))
	    ((isa? (car exprs) Begin)
	     (loop (cdr exprs)
		   (inner (car exprs)
			  rev-defines
			  finish-fun)))
	    ((isa? (car exprs) Define)
	     (let ((binding (car exprs)))
		(set-car! exprs (instantiate::Const
				   (location (with-access::Node (car exprs) (location)
						location))
				   (value #unspecified)))
		(loop (cdr exprs)
		      (cons binding rev-defines))))
	    (else
	     (finish-fun rev-defines)))))

   (reverse! (bind-exit (finish-fun)
		(inner bnode '() finish-fun))))
