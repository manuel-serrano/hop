;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/arithmetic64.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec  4 19:36:39 2017                          */
;*    Last change :  Wed Dec  6 19:51:36 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Arithmetic operations on 64 bit platforms                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_arithmetic64

   (library hop)

   (include "types.sch" "stringliteral.sch")

   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_string
	   __hopscript_error
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public)

   (cond-expand
      ((or bint61 bint64)
       (export
	  (inline overflow53::obj ::long)
	  
	  (js-number-toint32::int32 ::obj)
	  (js-number-touint32::uint32 ::obj)
	  
	  (inline js-int32-tointeger::obj ::int32)
	  (inline js-uint32-tointeger::obj ::uint32)
	  
	  (inline +fx/overflow::obj ::long ::long)
	  (+/overflow::obj ::obj ::obj)
	  
	  (inline -fx/overflow::obj ::long ::long)
	  (*fx/overflow ::long ::long)
	  (*/overflow ::obj ::obj)))))

;*---------------------------------------------------------------------*/
;*    overflow53 ...                                                   */
;*    -------------------------------------------------------------    */
;*    2^53-1 overflow                                                  */
;*---------------------------------------------------------------------*/
(define-inline (overflow53 v::long)
   (if (=fx (bit-rsh v 52) 0) v (fixnum->flonum v)))

;*---------------------------------------------------------------------*/
;*    js-number-toint32 ::obj ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.5          */
;*---------------------------------------------------------------------*/
(define (js-number-toint32::int32 obj)
   
   (define (int64->int32::int32 obj::int64)
      (let* ((i::elong (int64->elong obj))
	     (^31 (fixnum->elong (bit-lsh 1 31)))
	     (^32 (fixnum->elong (bit-lsh 1 32)))
	     (posint (if (<elong i #e0) (+elong ^32 i) i))
	     (int32bit (moduloelong posint ^32))
	     (n (if (>=elong int32bit ^31)
		    (-elong int32bit ^32)
		    int32bit)))
	 (elong->int32 n)))
   
   (cond
      ((fixnum? obj)
       (cond-expand
	  (bint61
	   (if (and (<=fx obj (-fx (bit-lsh 1 31) 1))
		    (>=fx obj (negfx (bit-lsh 1 31))))
	       (fixnum->int32 obj)
	       (let* ((^31 (bit-lsh 1 31))
		      (^32 (bit-lsh 1 32))
		      (posint (if (<fx obj 0) (+fx ^32 obj) obj))
		      (int32bit (modulofx posint ^32))
		      (n (if (>=fx int32bit ^31)
			     (-fx int32bit ^32)
			     int32bit)))
		  (fixnum->int32 n))))
	  (else
	   (int64->int32 (fixnum->int64 obj)))))
      ((flonum? obj)
       (cond
	  ((or (= obj +inf.0) (= obj -inf.0) (nanfl? obj))
	   (fixnum->int32 0))
	  ((<fl obj 0.)
	   (let ((i (*fl -1. (floor (abs obj)))))
	      (if (>=fl i (negfl (exptfl 2. 31.)))
		  (fixnum->int32 (flonum->fixnum i))
		  (int64->int32 (flonum->int64 i)))))
	  (else
	   (let ((i (floor obj)))
	      (if (<=fl i (-fl (exptfl 2. 31.) 1.))
		  (fixnum->int32 (flonum->fixnum i))
		  (int64->int32 (flonum->int64 i)))))))
      (else
       (error "js-number-toint32" "bad number type" obj))))

;*---------------------------------------------------------------------*/
;*    js-number-touint32 ...                                           */
;*---------------------------------------------------------------------*/
(define (js-number-touint32::uint32 obj)
   
   (define 2^32 (exptfl 2. 32.))
   
   (define (positive-double->uint32::uint32 obj::double)
      (if (<fl obj 2^32)
	  (flonum->uint32 obj)
	  (flonum->uint32 (remainderfl obj 2^32))))
   
   (define (double->uint32::uint32 obj::double)
      (cond
	 ((or (= obj +inf.0) (= obj -inf.0) (not (= obj obj)))
	  #u32:0)
	 ((<fl obj 0.)
	  (positive-double->uint32 (+fl 2^32 (*fl -1. (floor (abs obj))))))
	 (else
	  (positive-double->uint32 obj))))
   
   (cond
      ((fixnum? obj)
       (if (<=fx obj (-fx (bit-lsh 1 32) 1))
	   (fixnum->uint32 obj)
	   (let* ((^31 (bit-lsh 1 31))
		  (^32 (bit-lsh 1 32))
		  (posint (if (<fx obj 0) (+fx ^32 obj) obj))
		  (int32bit (modulofx posint ^32)))
	      (fixnum->uint32 int32bit))))
      ((flonum? obj)
       (double->uint32 obj))
      (else
       (error "js-number-touint32" "bad number type" obj))))

;*---------------------------------------------------------------------*/
;*    js-int32-tointeger ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-int32-tointeger::obj i::int32)
   (int32->fixnum i))

;*---------------------------------------------------------------------*/
;*    js-uint32-tointeger ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-uint32-tointeger::obj u::uint32)
   (uint32->fixnum u))

;*---------------------------------------------------------------------*/
;*    tolong ...                                                       */
;*---------------------------------------------------------------------*/
(define (tolong x)
   (cond
      ((fixnum? x) x)
      ((int32? x) (int32->fixnum x))
      ((uint32? x) (uint32->fixnum x))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    todouble ...                                                     */
;*---------------------------------------------------------------------*/
(define (todouble::double x)
   (cond
      ((fixnum? x) (fixnum->flonum x))
      ((int32? x) (int32->flonum x))
      ((uint32? x) (uint32->flonum x))
      (else x)))

;*---------------------------------------------------------------------*/
;*    +fx/overflow ...                                                 */
;*    -------------------------------------------------------------    */
;*    Fixnum addition on 64 bits machines (three tagging bits).        */
;*---------------------------------------------------------------------*/
(define-inline (+fx/overflow::obj x::long y::long)
   (overflow53 (+fx x y)))

;*---------------------------------------------------------------------*/
;*    +/overflow ...                                                   */
;*    -------------------------------------------------------------    */
;*    This function is the compiler fallback used when it finds no     */
;*    specialized addition function. In practice it should hardly      */
;*    be called.                                                       */
;*---------------------------------------------------------------------*/
(define (+/overflow x::obj y::obj)
   (let ((ll (tolong x)))
      (if ll
	  (let ((rl (tolong y)))
	     (if rl
		 (+fx/overflow ll rl)
		 (+fl (todouble x) (todouble y))))
	  (+fl (todouble x) (todouble y)))))

;*---------------------------------------------------------------------*/
;*    -fx/overflow ...                                                 */
;*    -------------------------------------------------------------    */
;*    see +fx/overflow                                                 */
;*---------------------------------------------------------------------*/
(define-inline (-fx/overflow::obj x::long y::long)
   (overflow53 (-fx x y)))

;*---------------------------------------------------------------------*/
;*    -/overflow ...                                                   */
;*    -------------------------------------------------------------    */
;*    see +/overflow                                                   */
;*---------------------------------------------------------------------*/
(define (-/overflow x y)
   (let ((ll (tolong x)))
      (if ll
	  (let ((rl (tolong y)))
	     (if rl
		 (-fx/overflow ll rl)
		 (-fl (todouble x) (todouble y))))
	  (-fl (todouble x) (todouble y)))))

;*---------------------------------------------------------------------*/
;*    *fx/overflow ...                                                 */
;*---------------------------------------------------------------------*/
(define (*fx/overflow x::long y::long)
   (cond-expand
      ((and bigloo-c (config have-overflow #t))
       (let ((res::long 0))
	  (if (pragma::bool "__builtin_smull_overflow((long)$1, (long)$2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1)) * ((double)($2)))"
		 x y)
	      (overflow53 res))))
      (else
       (define (neg? o)
	  (if (flonum? o)
	      (not (=fx (signbitfl o) 0))
	      (<fx o 0)))
       
       (let ((rnum (if (number? right) right (js-tonumber right %this)))
	     (r (* x y)))
	  (cond
	     ((fixnum? r)
	      (if (=fx r 0)
		  (if (or (and (neg? lnum) (not (neg? rnum)))
			  (and (not (neg? lnum)) (neg? rnum)))
		      -0.0)
		  (oveflow53 r)))
	     ((flonum? r)
	      r)
	     ((bignum? r)
	      (bignum->flonum r))
	     ((elong? r)
	      (elong->flonum r))
	     ((llong? r)
	      (llong->flonum r))
	     (else
	      r))))))

;*---------------------------------------------------------------------*/
;*    */overflow ...                                                   */
;*---------------------------------------------------------------------*/
(define (*/overflow x y)
   (let ((ll (tolong x)))
      (if ll
	  (let ((rl (tolong y)))
	     (if rl
		 (*fx/overflow ll rl)
		 (*fl (todouble x) (todouble y))))
	  (*fl (todouble x) (todouble y)))))

