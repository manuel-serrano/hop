;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-math.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  5 05:47:06 2017                          */
;*    Last change :  Tue Mar 28 15:55:07 2023 (serrano)                */
;*    Copyright   :  2017-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript Math functions.             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-math

   (library hop hopscript)
   
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
	   __js2scheme_scheme-cast
	   __js2scheme_scheme-ops)

   (export (j2s-math-builtin-method fun::J2SAccess args
	      expr mode return::procedure conf)
	   (j2s-math-object-get obj::J2SRef field::J2SExpr
	      mode return conf)))

;*---------------------------------------------------------------------*/
;*    j2s-math-builtin-method ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-math-builtin-method fun::J2SAccess args expr mode return conf)

   (define (cast-math sexp)
      (with-access::J2SCall expr (args)
	 (j2s-cast sexp expr (j2s-type (car args)) (j2s-type expr) conf)))

   (define (integer-as-real? expr)
      (cond
	 ((memq (j2s-type expr) '(integer int53 int32 uint32))
	  #t)
	 ((isa? expr J2SNumber)
	  (with-access::J2SNumber expr (val)
	     (integer? val)))
	 ((isa? expr J2SCast)
	  (with-access::J2SCast expr (expr)
	     (integer-as-real? expr)))
	 (else
	  #f)))
   
   (define (integer-division? exp)
      (when (isa? exp J2SBinary)
	 (with-access::J2SBinary exp (op lhs rhs)
	    (and (eq? op '/)
		 (or (type-integer? (j2s-type rhs))
		     (integer-as-real? rhs))))))
   
   (with-access::J2SAccess fun (loc obj field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (cond
	       ((string=? val "abs")
		(when (=fx (length args) 1)
		   (cast-math
		      (if (eq? (j2s-type (car args)) 'real)
			  `(js-math-absfl
			      ,(j2s-scheme (car args) mode return conf))
			  `(js-math-abs
			      ,(j2s-scheme (car args) mode return conf)
			      %this)))))
	       ((string=? val "floor")
		(when (=fx (length args) 1)
		   (cast-math
		      (j2s-math-inline-floor
			 (car args) mode return conf))))
	       ((string=? val "ceil")
		(when (=fx (length args) 1)
		   (cast-math
		      `(,(if (integer-division? (car args))
			     'js-math-ceil-as-integer
			     'js-math-ceil)
			  ,(j2s-scheme (car args) mode return conf)))))
	       ((string=? val "round")
		(when (=fx (length args) 1)
		   (if (eq? (j2s-type (car args)) 'real)
		       `(js-math-roundfl
			   ,(j2s-scheme (car args) mode return conf))
		       `(js-math-round
			   ,(j2s-scheme (car args) mode return conf)))))
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
	       ((member val '("cos" "sin" "acos" "asin"))
		(when (=fx (length args) 1)
		   (j2s-math-inline-trigonometry
		      (string->symbol val)
		      (car args) mode return conf)))
	       ((string=? val "atan2")
		(when (=fx (length args) 2)
		   (j2s-math-inline-atan2
		      (car args) (cadr args) mode return conf)))
	       ((string=? val "sign")
		(when (=fx (length args) 1)
		   (j2s-math-sign
		      (car args) mode return conf)))
	       ((string=? val "pow")
		(when (=fx (length args) 2)
		   (j2s-math-inline-pow
		      (car args) (cadr args) mode return conf)))
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
      (memq (j2s-type n) '(index uint32)))
   
   (let ((p2 (divide-power2 arg)))
      (cond
	 ((not p2)
	  (let ((p3 (divide arg)))
	     (if (not p3)
		 (let* ((arg (j2s-asreal arg))
			(sexp (j2s-scheme arg mode return conf)))
		    (if (eq? (j2s-type arg) 'real)
			`(js-math-floorfl ,sexp)
			(match-case sexp
			   ((/fl . ?-)
			    `(js-math-floorfl ,sexp))
			   ((let ?bindings (/fl . ?-))
			    `(js-math-floorfl ,sexp))
			   (else
			    `(js-math-floor ,sexp %this)))))
		 (case (j2s-type (car p3))
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
	  (case (j2s-type (car p2))
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
	  (case (j2s-type (car p2))
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
   (case (j2s-type arg)
      ((int32)
       (let ((tmp (gensym 'int)))
	  `(let ((,tmp ,(j2s-scheme arg mode return conf)))
	      (if (<s32 ,tmp 0) +nan.0 (sqrtfl (int32->flonum ,tmp))))))
      ((uint32)
       `(sqrtfl (uint32->flonum ,(j2s-scheme arg mode return conf))))
      ((int53 bint)
       (let ((tmp (gensym 'int)))
	  `(let ((,tmp ,(j2s-scheme arg mode return conf)))
	      (js-math-sqrtfl (fixnum->flonum ,tmp)))))
      ((real)
       `(js-math-sqrtfl ,(j2s-scheme arg mode return conf)))
      ((number)
       (let ((tmp (gensym 'num)))
	  `(let ((,tmp ,(j2s-scheme arg mode return conf)))
	      (if (flonum? ,tmp)
		  (js-math-sqrtfl ,tmp)
		  (js-math-sqrtfl (fixnum->flonum ,tmp))))))
      (else
       `(js-math-sqrt ,(j2s-scheme arg mode return conf) %this))))

;*---------------------------------------------------------------------*/
;*    j2s-math-inline-trigonometry ...                                 */
;*---------------------------------------------------------------------*/
(define (j2s-math-inline-trigonometry fun::symbol arg mode return conf)
   (case (j2s-type arg)
      ((int32)
       (let ((tmp (gensym 'int)))
	  `(let ((,tmp ,(j2s-scheme arg mode return conf)))
	      (if (<s32 ,tmp 0) +nan.0
		  (,fun (int32->flonum ,tmp))))))
      ((uint32)
       `(,fun (uint32->flonum ,(j2s-scheme arg mode return conf))))
      ((int53 bint)
       (let ((tmp (gensym 'int)))
	  `(let ((,tmp ,(j2s-scheme arg mode return conf)))
	      (,fun (fixnum->flonum ,tmp)))))
      ((real)
       `(,fun ,(j2s-scheme arg mode return conf)))
      ((number)
       (let ((tmp (gensym 'num)))
	  `(let ((,tmp ,(j2s-scheme arg mode return conf)))
	      (if (flonum? ,tmp)
		  (,fun ,tmp)
		  (,fun (fixnum->flonum ,tmp))))))
      (else
       `(,fun ,(j2s-scheme arg mode return conf)))))

;*---------------------------------------------------------------------*/
;*    j2s-math-inline-atan2 ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-math-inline-atan2 x y mode return conf)
   (if (and (eq? (j2s-type x) 'real) (eq? (j2s-type y) 'real))
       `(js-math-atan2fl ,(j2s-scheme x mode return conf)
	   ,(j2s-scheme y mode return conf))
       `(js-math-atan2 ,(j2s-scheme x mode return conf)
	   ,(j2s-scheme y mode return conf))))

;*---------------------------------------------------------------------*/
;*    j2s-math-inline-pow ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-math-inline-pow x y mode return conf)
   
   (define (number->flonum v)
      (cond
	 ((flonum? v) v)
	 ((fixnum? v) (fixnum->flonum v))
	 ((uint32? v) (uint32->flonum v))
	 ((int32? v) (int32->flonum v))
	 (else (error "Math.pow" "illegal number" v))))
   
   (let ((tyx (j2s-type x))
	 (tyy (j2s-type y))
	 (sx (j2s-scheme x mode return conf))
	 (sy (j2s-scheme y mode return conf)))
      (cond
	 ((and (number? sx) (not (bignum? sx)) (number? sy) (not (bignum? sy)))
	  (exptfl (number->flonum sx) (number->flonum sy)))
	 ((eq? tyx 'real)
	  (case tyy
	     ((uint32)
	      `(js-math-powfl ,sx (uint32->flonum ,sy)))
	     ((int32)
	      `(js-math-powfl ,sx (int32->flonum ,sy)))
	     ((real)
	      `(js-math-powfl ,sx ,sy))
	     (else
	      `(js-math-pow ,sx ,sy %this))))
	 ((eq? tyx 'uint32)
	  (case tyy
	     ((uint32)
	      `(js-math-powfl (uint32->flonum ,sx) (uint32->flonum ,sy)))
	     ((int32)
	      `(js-math-powfl (uint32->flonum ,sx) (int32->flonum ,sy)))
	     ((real)
	      `(js-math-powfl (uint32->flonum ,sx) ,sy))
	     (else
	      `(js-math-pow ,sx ,sy %this))))
	 ((eq? tyx 'int32)
	  (case tyy
	     ((uint32)
	      `(js-math-powfl (int32->flonum ,sx) (uint32->flonum ,sy)))
	     ((int32)
	      `(js-math-powfl (int32->flonum ,sx) (int32->flonum ,sy)))
	     ((real)
	      `(js-math-powfl (int32->flonum ,sx) ,sy))
	     (else
	      `(js-math-pow ,sx ,sy %this))))
	 (else
	  `(js-math-pow ,sx ,sy %this)))))

;*---------------------------------------------------------------------*/
;*    j2s-math-sign ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-math-sign x mode return conf)
   (if (eq? (j2s-type x) 'real)
       `(js-math-signfl ,(j2s-scheme x mode return conf))
       `(js-math-sign ,(j2s-scheme x mode return conf) %this)))

;*---------------------------------------------------------------------*/
;*    j2s-math-inline-min-max ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-math-inline-min-max loc op x::J2SExpr y::J2SExpr mode return conf)
   (js-binop2 loc op 'number x y mode return conf))

;*---------------------------------------------------------------------*/
;*    j2s-math-object-get ...                                          */
;*    -------------------------------------------------------------    */
;*    Read-only math properties.                                       */
;*---------------------------------------------------------------------*/
(define (j2s-math-object-get obj field mode return ctx)
   (when (isa? field J2SString)
      (with-access::J2SString field (val)
	 (cond
	    ((string=? val "E") 2.7182818284590452354)
	    ((string=? val "LN10") (log 10))
	    ((string=? val "LN2") (log 2))
	    ((string=? val "LOG2E") 1.4426950408889634)
	    ((string=? val "LOG10E") 0.4342944819032518)
	    ((string=? val "PI") (* 2 (atan 1 0)))
	    ((string=? val "SQRT1_2") (sqrt (/ 1 2)))
	    ((string=? val "SQRT2") (sqrt 2))
	    (else #f)))))

;*---------------------------------------------------------------------*/
;*    j2s-asreal ::J2SExpr ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (j2s-asreal expr::J2SExpr)
   (with-access::J2SExpr expr (loc)
      (J2SCast 'real expr)))

;*---------------------------------------------------------------------*/
;*    j2s-asreal ::J2SBinary ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-asreal expr::J2SBinary)
   (with-access::J2SBinary expr (loc op type lhs rhs)
      (case op
	 ((/ * + -)
	  (set! type 'real)
	  (set! lhs (j2s-asreal lhs))
	  (set! rhs (j2s-asreal rhs))
	  expr)
	 (else
	  (J2SCast 'real expr)))))
