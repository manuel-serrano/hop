;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-math.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  5 05:47:06 2017                          */
;*    Last change :  Sat Dec 16 06:17:13 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript Math functions.             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-math

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_scheme
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-fun)

   (export (j2s-math-inline-method fun::J2SAccess args
	      mode return::procedure conf hint)))

;*---------------------------------------------------------------------*/
;*    j2s-math-inline-method ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-math-inline-method fun::J2SAccess args mode return conf hint)
   (with-access::J2SAccess fun (obj field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (cond
	       ((string=? val "floor")
		(when (=fx (length args) 1)
		   (j2s-math-inline-floor
		      (car args) mode return conf hint)))
	       ((string=? val "ceil")
		(when (=fx (length args) 1)
		   `(js-math-ceil
		       ,(j2s-scheme (car args) mode return conf hint))))
	       ((string=? val "round")
		(when (=fx (length args) 1)
		   `(js-math-round
		       ,(j2s-scheme (car args) mode return conf hint))))
	       ((string=? val "random")
		(when (=fx (length args) 0)
		   `(randomfl)))
	       (else
		#f))))))

;*---------------------------------------------------------------------*/
;*    j2s-math-inline-floor ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-math-inline-floor arg mode return conf hint)
   
   (define (find-power2 n)
      (let loop ((k 1))
	 (let ((m (bit-lsh 1 k)))
	    (cond
	       ((=fx m n) k)
	       ((>fx m n) #f)
	       (else (loop (+fx k 1)))))))
   
   (define (divide-power2 arg)
      ;; detect the pattern Math.floor( m / 2^n ) (see crypto-aes.js)
      (when (isa? arg J2SBinary)
	 (with-access::J2SBinary arg (op lhs rhs)
	    (when (and (eq? op '/)
		       (type-fixnum? (j2s-type-ref lhs))
		       (type-fixnum? (j2s-type-ref rhs))
		       (isa? rhs J2SNumber))
	       (with-access::J2SNumber rhs (val)
		  (when (>fx val 0)
		     (let ((k (find-power2 val)))
			(when k
			   (cons lhs k)))))))))
   
   (define (positive? n)
      (memq (j2s-type-ref n) '(index uint29 ufixnum)))
   
   (let ((p2 (divide-power2 arg)))
      (cond
	 ((not p2)
	  `(js-math-floor ,(j2s-scheme arg mode return conf hint)))
	 ((positive? (car p2))
	  `(bit-rsh ,(j2s-scheme (car p2) mode return conf hint)
	      ,(cdr p2)))
	 (else
	  `(/pow2fx ,(j2s-scheme (car p2) mode return conf hint)
	      ,(cdr p2))))))
