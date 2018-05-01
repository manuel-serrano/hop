;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-math.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  5 05:47:06 2017                          */
;*    Last change :  Tue May  1 18:53:39 2018 (serrano)                */
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
	      mode return::procedure conf)))

;*---------------------------------------------------------------------*/
;*    j2s-math-builtin-method ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-math-builtin-method fun::J2SAccess args mode return conf)
   (with-access::J2SAccess fun (loc obj field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (cond
	       ((string=? val "floor")
		(when (=fx (length args) 1)
		   (j2s-math-inline-floor
		      (car args) mode return conf)))
	       ((string=? val "ceil")
		(when (=fx (length args) 1)
		   `(js-math-ceil
		       ,(j2s-scheme (car args) mode return conf))))
	       ((string=? val "round")
		(when (=fx (length args) 1)
		   `(js-math-round
		       ,(j2s-scheme (car args) mode return conf))))
	       ((string=? val "random")
		(when (=fx (length args) 0)
		   `(randomfl)))
	       ((string=? val "max")
		(when (=fx (length args) 2)
		   (j2s-math-inline-min-max loc 'MAX
		      (car args) (cadr args) mode return conf)))
	       ((string=? val "min")
		(when (=fx (length args) 2)
		   (j2s-math-inline-min-max loc 'MIN
		      (car args) (cadr args) mode return conf)))
	       ((string=? val "sqrt")
		(when (=fx (length args) 1)
		   (j2s-math-inline-sqrt (car args) mode return conf)))
	       (else
		#f))))))

;*---------------------------------------------------------------------*/
;*    j2s-math-inline-floor ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-math-inline-floor arg mode return conf)
   
   (define (find-power2 n)
      (let ((n (cond
		  ((fixnum? n) (fixnum->uint32 n))
		  ((int32? n) (int32->uint32 n))
		  (else n))))
	 (let loop ((k 1))
	    (let ((m (bit-lshu32 #u32:1 k)))
	       (cond
		  ((=u32 m n) k)
		  ((>u32 m n) #f)
		  (else (loop (+fx k 1))))))))
   
   (define (divide-power2 arg)
      ;; detect the pattern Math.floor( m / 2^n ) (see crypto-aes.js)
      (when (isa? arg J2SBinary)
	 (with-access::J2SBinary arg (op lhs rhs)
	    (when (and (eq? op '/)
;* 		       (type-fixnum? (j2s-vtype lhs))                  */
;* 		       (type-fixnum? (j2s-vtype rhs))                  */
		       (isa? rhs J2SNumber))
	       (with-access::J2SNumber rhs (val)
		  (when (and (integer? val) (> val 0))
		     (let ((k (find-power2 val)))
			(when k
			   (cons lhs k)))))))))

   (define (divide arg)
      ;; detect the pattern Math.floor( m / 2^n ) (see crypto-aes.js)
      (when (isa? arg J2SBinary)
	 (with-access::J2SBinary arg (op lhs rhs)
	    (when (and (eq? op '/) (isa? rhs J2SNumber))
	       (with-access::J2SNumber rhs (val)
		  (when (and (integer? val) (> val 0))
		     (cons lhs val)))))))
   
   (define (positive? n)
      (memq (j2s-vtype n) '(index uint32)))
   
   (let ((p2 (divide-power2 arg)))
      (cond
	 ((not p2)
	  (let ((p3 (divide arg)))
	     (if (not p3)
		 (let ((sexp (j2s-scheme arg mode return conf)))
		    (if (eq? (j2s-type arg) 'real)
			`(js-math-floorfl ,sexp)
			(match-case sexp
			   ((/fl . ?-)
			    `(js-math-floorfl ,sexp))
			   ((let ?bindings (/fl . ?-))
			    `(js-math-floorfl ,sexp))
			   (else
			    `(js-math-floor ,sexp %this)))))
		 (case (j2s-vtype (car p3))
		    ((int32)
		     `(js-int32->tointeger
			 (/s32 ,(j2s-scheme (car p3) mode return conf)
			    ,(cdr p3))))
		    ((uint32)
		     `(js-uint32-tointeger
			 (/u32 ,(j2s-scheme (car p3) mode return conf)
			    ,(cdr p3))))
		    ((int53 bint)
		     `(/fx ,(j2s-scheme (car p3) mode return conf)
			 ,(cdr p3)))
		    (else
		     (let ((tmp (gensym 'tmp)))
			`(let ((,tmp ,(j2s-scheme (car p3) mode return conf)))
			    (if (fixnum? ,tmp)
				(/fx ,tmp ,(cdr p3))
				(js-math-floor (/js ,tmp ,(cdr p3) %this) %this)))))))))
	 ((positive? (car p2))
	  (case (j2s-vtype (car p2))
	     ((int32)
	      `(js-int32-tointeger
		  (bit-rshs32 ,(j2s-scheme (car p2) mode return conf)
		     ,(cdr p2))))
	     ((uint32)
	      `(js-uint32-tointeger
		  (bit-rshu32 ,(j2s-scheme (car p2) mode return conf)
		     ,(cdr p2))))
	     ((int53 bint)
	      `(bit-rsh ,(j2s-scheme (car p2) mode return conf)
		  ,(cdr p2)))
	     (else
	      (let ((tmp (gensym 'tmp)))
		 `(let ((,tmp ,(j2s-scheme (car p2) mode return conf)))
		     (if (fixnum? ,tmp)
			 (bit-rsh ,tmp ,(cdr p2))
			 (js-math-floor (/js ,tmp ,(cdr p2) %this) %this)))))))
	 (else
	  (case (j2s-vtype (car p2))
	     ((int32)
	      `(js-int32->tointeger
		  (/pow2s32 ,(j2s-scheme (car p2) mode return conf)
		     ,(cdr p2))))
	     ((uint32)
	      `(js-uint32-tointeger
		  (/pow2u32 ,(j2s-scheme (car p2) mode return conf)
		     ,(cdr p2))))
	     ((int53 bint)
	      `(/pow2fx ,(j2s-scheme (car p2) mode return conf)
		  ,(cdr p2)))
	     (else
	      (let ((tmp (gensym 'tmp)))
		 `(let ((,tmp ,(j2s-scheme (car p2) mode return conf)))
		     (if (fixnum? ,tmp)
			 (/pow2fx ,tmp ,(cdr p2))
			 (js-math-floor (/js ,tmp ,(cdr p2) %this) %this))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-math-inline-sqrt ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-math-inline-sqrt arg mode return conf)
   (case (j2s-vtype arg)
      ((int32)
       (let ((tmp (gensym 'tmp)))
	  `(let ((,tmp ,(j2s-scheme arg mode return conf)))
	      (if (<s32 ,tmp 0) +nan.0 (sqrtfl (int32->flonum ,tmp))))))
      ((uint32)
       `(sqrtfl (uint32->flonum ,(j2s-scheme arg mode return conf))))
      ((int53 bint)
       (let ((tmp (gensym 'tmp)))
	  `(let ((,tmp ,(j2s-scheme arg mode return conf)))
	      (if (<fx ,tmp 0) +nan.0 (sqrtfl (fixnum->flonum ,tmp))))))
      ((real)
       `(js-math-sqrtfl ,(j2s-scheme arg mode return conf)))
      (else
       `(js-math-sqrt ,(j2s-scheme arg mode return conf) %this))))

;*---------------------------------------------------------------------*/
;*    j2s-math-inline-min-max ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-math-inline-min-max loc op x::J2SExpr y::J2SExpr mode return conf)
   (js-binop2 loc op 'number x y mode return conf))
   
