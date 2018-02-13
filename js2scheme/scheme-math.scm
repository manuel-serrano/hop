;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-math.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  5 05:47:06 2017                          */
;*    Last change :  Tue Feb 13 16:56:10 2018 (serrano)                */
;*    Copyright   :  2017-18 Manuel Serrano                            */
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
	   __js2scheme_scheme-fun
	   __js2scheme_scheme-ops)

   (export (j2s-math-builtin-method fun::J2SAccess args
	      mode return::procedure conf hint)))

;*---------------------------------------------------------------------*/
;*    j2s-math-builtin-method ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-math-builtin-method fun::J2SAccess args mode return conf hint)
   (with-access::J2SAccess fun (loc obj field)
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
	       ((string=? val "max")
		(when (=fx (length args) 2)
		   (j2s-math-inline-min-max loc 'MAX
		      (car args) (cadr args) mode return conf hint)))
	       ((string=? val "min")
		(when (=fx (length args) 2)
		   (j2s-math-inline-min-max loc 'MIN
		      (car args) (cadr args) mode return conf hint)))
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
		       (type-fixnum? (j2s-vtype lhs))
		       (type-fixnum? (j2s-vtype rhs))
		       (isa? rhs J2SNumber))
	       (with-access::J2SNumber rhs (val)
		  (when (>fx val 0)
		     (let ((k (find-power2 val)))
			(when k
			   (cons lhs k)))))))))
   
   (define (positive? n)
      (memq (j2s-vtype n) '(index uint32)))
   
   (let ((p2 (divide-power2 arg)))
      (cond
	 ((not p2)
	  (let ((sexp (j2s-scheme arg mode return conf hint)))
	     (if (eq? (j2s-type arg) 'real)
		 `(js-math-floorfl ,sexp)
		 (match-case sexp
		    ((/fl . ?-)
		     `(js-math-floorfl ,sexp))
		    ((let ?bindings (/fl . ?-))
		     `(js-math-floorfl ,sexp))
		    (else
		     `(js-math-floor ,sexp))))))
	 ((positive? (car p2))
	  (case (j2s-vtype (car p2))
	     ((int32)
	      `(js-int32-tointeger
		  (bit-rshs32 ,(j2s-scheme (car p2) mode return conf hint)
		     ,(cdr p2))))
	     ((uint32)
	      `(js-uint32-tointeger
		  (bit-rshu32 ,(j2s-scheme (car p2) mode return conf hint)
		     ,(cdr p2))))
	     (else
	      `(bit-rsh ,(j2s-scheme (car p2) mode return conf hint)
		 ,(cdr p2)))))
	 
	 (else
	  (case (j2s-vtype (car p2))
	     ((int32)
	      `(js-int32->tointeger
		  (/pow2s32 ,(j2s-scheme (car p2) mode return conf hint)
		     ,(cdr p2))))
	     ((uint32)
	      `(js-uint32-tointeger
		  (/pow2u32 ,(j2s-scheme (car p2) mode return conf hint)
		     ,(cdr p2))))
	     (else
	      `(/pow2fx ,(j2s-scheme (car p2) mode return conf hint)
		  ,(cdr p2))))))))

;*---------------------------------------------------------------------*/
;*    j2s-math-inline-min-max ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-math-inline-min-max loc op x::J2SExpr y::J2SExpr mode return conf hint)
   (js-binop2 loc op 'number x y mode return conf hint))
   
