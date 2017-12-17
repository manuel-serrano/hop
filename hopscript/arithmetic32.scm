;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/arithmetic32.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec  4 19:36:39 2017                          */
;*    Last change :  Sun Dec 17 19:12:06 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Arithmetic operations on 32 bit platforms                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_arithmetic32

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
      ((or bint30 bint32)
       (export
	  (inline overflow29 ::long)
	  
	  (inline js-toint32::int32 ::obj ::JsGlobalObject)
	  (js-toint32-slow::int32 ::obj ::JsGlobalObject)
	  (inline js-touint32::uint32 ::obj ::JsGlobalObject)
	  (js-touint32-slow::uint32 obj %this)
	  
	  (js-number-toint32::int32 ::obj)
	  (js-number-touint32::uint32 ::obj)
	  
	  (inline js-int32-tointeger::obj ::int32)
	  (inline js-uint32-tointeger::obj ::uint32)
	  
	  (inline +fx/overflow::obj ::long ::long)
	  (inline +fx32/overflow::obj ::long ::long)
	  (inline +s32/overflow::obj ::int32 ::int32)
	  (inline +u32/overflow::obj ::uint32 ::uint32)
	  (+/overflow::obj ::obj ::obj)
	  
	  (inline -fx/overflow::obj ::long ::long)
	  (inline -fx32/overflow::obj ::long ::long)
	  (inline -s32/overflow::obj ::int32 ::int32)
	  (inline -u32/overflow::obj ::uint32 ::uint32)
	  (-/overflow::obj ::obj ::obj)
	  
	  (inline *fx/overflow::obj ::long ::long)
	  (inline *u32/overflow::obj ::uint32 ::uint32)
	  (*/overflow::obj ::obj ::obj)
	  ))))

;*---------------------------------------------------------------------*/
;*    js-toint32 ::obj ...                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.5          */
;*---------------------------------------------------------------------*/
(define-inline (js-toint32::int32 obj %this)
   (if (fixnum? obj)
       (fixnum->int32 obj)
       (js-toint32-slow obj %this)))

;*---------------------------------------------------------------------*/
;*    js-toint32-slow ...                                              */
;*---------------------------------------------------------------------*/
(define (js-toint32-slow::int32 obj %this)
   (cond
      ((flonum? obj) (js-number-toint32 obj))
      ((uint32? obj) (tprint "should not be here.uint32 " obj) (uint32->int32 obj))
      ((int32? obj) (tprint "should not be here.int32 " obj) (tprint (/s32 #s32:1 (car (list #s32:0)))) obj)
      ((fixnum? obj) (fixnum->int32 obj))
      (else (js-number-toint32 (js-tonumber obj %this)))))

;*---------------------------------------------------------------------*/
;*    js-touint32 ::obj ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.6          */
;*---------------------------------------------------------------------*/
(define-inline (js-touint32::uint32 obj %this)
   (if (fixnum? obj)
       (fixnum->uint32 obj)
       (js-touint32-slow obj %this)))

;*---------------------------------------------------------------------*/
;*    js-touint32-slow ...                                             */
;*---------------------------------------------------------------------*/
(define (js-touint32-slow::uint32 obj %this)
   (cond
      ((fixnum? obj) (fixnum->uint32 obj))
      ((flonum? obj) (js-number-touint32 obj))
      ((int32? obj) (tprint "should not be here.int32") (int32->uint32 obj))
      ((uint32? obj) (tprint "should not be here.uint32") obj)
      (else (js-number-touint32 (js-tointeger obj %this)))))

;*---------------------------------------------------------------------*/
;*    js-number-toint32 ::obj ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.5          */
;*---------------------------------------------------------------------*/
(define (js-number-toint32::int32 obj)
   
   (define (int64->int32::int32 obj::int64)
      (let* ((i::llong (int64->llong obj))
	     (^31 (*llong #l8 (fixnum->llong (bit-lsh 1 28))))
	     (^32 (*llong #l2 ^31))
	     (posint (if (<llong i #l0) (+llong ^32 i) i))
	     (int32bit (modulollong posint ^32))
	     (n (if (>=llong int32bit ^31)
		    (-llong int32bit ^32)
		    int32bit)))
	 (llong->int32 n)))
   
   (cond
      ((fixnum? obj)
       (fixnum->int32 obj))
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
      ((uint32? obj)
       (tprint "should not be here")
       (uint32->int32 obj))
      ((int32? obj)
       (tprint "should not be here")
       obj)
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
       (cond-expand
	  (bint30 (fixnum->uint32 obj))
	  (else (int32->uint32 (fixnum->int32 obj)))))
      ((flonum? obj)
       (double->uint32 obj))
      ((int32? obj)
       (tprint "should not be here")
       (int32->uint32 obj))
      ((uint32? obj)
       (tprint "should not be here")
       obj)
      (else
       (error "js-number-touint32" "bad number type" obj))))

;*---------------------------------------------------------------------*/
;*    overflow29 ...                                                   */
;*    -------------------------------------------------------------    */
;*    2^29-1 overflow                                                  */
;*    -------------------------------------------------------------    */
;*    See Hacker's Delight (second edition), H. Warren J.r,            */
;*    Chapter 4, section 4.1, page 68                                  */
;*---------------------------------------------------------------------*/
(define-inline (overflow29 v::long)
   (let* ((a (-fx 0 (bit-lsh 1 29)))
	  (b (-fx (bit-lsh 1 29) 1))
	  (b-a (-fx b a)))
      (if (<=u32 (fixnum->uint32 (-fx v a)) (fixnum->uint32 b-a))
	  v
	  (fixnum->flonum v))))

;*---------------------------------------------------------------------*/
;*    js-int32-tointeger ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-int32-tointeger::obj i::int32)
   (overflow29 (int32->fixnum i)))

;*---------------------------------------------------------------------*/
;*    js-uint32-tointeger ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-uint32-tointeger::obj i::uint32)
   (cond-expand
      (bint30
       (if (<u32 i (bit-lshu32 #u32:1 29))
	   (uint32->fixnum i)
	   (uint32->flonum i)))
      (bint32
       (if (<u32 i (-u32 (bit-lshu32 #u32:1 30) #u32:1))
	   (uint32->fixnum i)
	   (uint32->flonum i)))))

;*---------------------------------------------------------------------*/
;*    tolong ...                                                       */
;*---------------------------------------------------------------------*/
(define (tolong x)
   (cond
      ((fixnum? x) x)
      ((int32? x)
       (tprint "SHOULD NOT int32") (int32->fixnum x))
      ((and (uint32? x) (<u32 x (bit-lshu32 #u32:1 29)))
       (tprint "SHOULD NOT int32") (uint32->fixnum x))
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
;*    The argument are 30bit integers encoded into long values.        */
;*---------------------------------------------------------------------*/
(define-inline (+fx/overflow x::long y::long)
   (overflow29 (+fx x y)))

;*---------------------------------------------------------------------*/
;*    +fx32/overflow ...                                               */
;*    -------------------------------------------------------------    */
;*    X and Y are true 32bit values.                                   */
;*---------------------------------------------------------------------*/
(define-inline (+fx32/overflow x::long y::long)
   (cond-expand
      ((and bigloo-c (config have-overflow #t))
       (let ((res::long 0))
	  (if (pragma::bool "__builtin_saddl_overflow($1, $2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1))+((double)($2)))"
		 x y)
	      (overflow29 res))))
      (else
       (let ((z::long (pragma::long "(~($1 ^ $2)) & 0x80000000" x y)))
	  (if (pragma::bool "$1 & (~((($2 ^ $1) + ($3)) ^ ($3)))" z x y)
	      (fixnum->flonum (+fx x y))
	      (overflow29 (+fx x y)))))))

;*---------------------------------------------------------------------*/
;*    +s32/overflow ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (+s32/overflow x::int32 y::int32)
   (+fx32/overflow (int32->fixnum x) (int32->fixnum y)))

;*---------------------------------------------------------------------*/
;*    +u32/overflow ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (+u32/overflow x::uint32 y::uint32)
   (cond-expand
      ((and bigloo-c (config have-overflow #t))
       (let ((res::long 0))
	  (if (pragma::bool "__builtin_uaddl_overflow($1, $2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1))+((double)($2)))"
		 x y)
	      (overflow29 res))))
      (else
       (let ((z::long (pragma::long "(~($1 ^ $2)) & 0x80000000" x y)))
	  (if (pragma::bool "$1 & (~((($2 ^ $1) + ($3)) ^ ($3)))" z x y)
	      (fixnum->flonum (+fx x y))
	      (overflow29 (+fx x y)))))))

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
		 (+fx32/overflow ll rl)
		 (+fl (todouble x) (todouble y))))
	  (+fl (todouble x) (todouble y)))))

;*---------------------------------------------------------------------*/
;*    -fx/overflow ...                                                 */
;*    -------------------------------------------------------------    */
;*    The argument are 30bit integers encoded into long values.        */
;*---------------------------------------------------------------------*/
(define-inline (-fx/overflow x::long y::long)
   (overflow29 (-fx x y)))

;*---------------------------------------------------------------------*/
;*    -fx32/overflow ...                                               */
;*    -------------------------------------------------------------    */
;*    X and Y are true 32bit values.                                   */
;*---------------------------------------------------------------------*/
(define-inline (-fx32/overflow x::long y::long)
   (cond-expand
      ((and bigloo-c (config have-overflow #t))
       (let ((res::long 0))
	  (if (pragma::bool "__builtin_ssubl_overflow($1, $2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1))-((double)($2)))"
		 x y)
	      res)))
      (else
       (let ((z::long (pragma::long "($1 ^ $2) & 0x80000000" x y)))
	  (if (pragma::bool "$1 & ((($2 ^ (long)$1) - ($3)) ^ ($3))" z x y)
	      (fixnum->flonum (-fx x y))
	      (-fx x y))))))
   
;*---------------------------------------------------------------------*/
;*    -s32/overflow ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (-s32/overflow x::int32 y::int32)
   (-fx32/overflow (int32->fixnum x) (int32->fixnum y)))

;*---------------------------------------------------------------------*/
;*    -u32/overflow ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (-u32/overflow x::uint32 y::uint32)
   (cond-expand
      ((and bigloo-c (config have-overflow #t))
       (let ((res::long 0))
	  (if (pragma::bool "__builtin_usubl_overflow($1, $2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1))+((double)($2)))"
		 x y)
	      (overflow29 res))))
      (else
       (let ((z::long (pragma::long "(~($1 ^ $2)) & 0x80000000" x y)))
	  (if (pragma::bool "$1 & ((($2 ^ (long)$1) - ($3)) ^ ($3))" z x y)
	      (fixnum->flonum (+fx x y))
	      (overflow29 (+fx x y)))))))

;*---------------------------------------------------------------------*/
;*    -/overflow ...                                                   */
;*    -------------------------------------------------------------    */
;*    This function is the compiler fallback used when it finds no     */
;*    specialized addition function. In practice it should hardly      */
;*    be called.                                                       */
;*---------------------------------------------------------------------*/
(define (-/overflow x::obj y::obj)
   (let ((ll (tolong x)))
      (if ll
	  (let ((rl (tolong y)))
	     (if rl
		 (-fx32/overflow ll rl)
		 (-fl (todouble x) (todouble y))))
	  (-fl (todouble x) (todouble y)))))

;*---------------------------------------------------------------------*/
;*    *fx/overflow ...                                                 */
;*    -------------------------------------------------------------    */
;*    The argument are 30bit integers encoded into long values.        */
;*---------------------------------------------------------------------*/
(define-inline (*fx/overflow x::long y::long)
   (cond-expand
      ((and bigloo-c (config have-overflow #t))
       (let ((res::long 0))
	  (cond
	     ((pragma::bool "__builtin_smull_overflow($1, $2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1))*((double)($2)))"
		 x y))
	     ((=fx res 0)
	      (if (or (and (<fx x 0) (>=fx y 0))
		      (and (>=fx x 0) (<fx y 0)))
		  -0.0
		  (overflow29 res)))
	     (else
	      (overflow29 res)))))
      
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
;*    *u32/overflow ...                                                */
;*    -------------------------------------------------------------    */
;*    The argument are 30bit integers encoded into long values.        */
;*---------------------------------------------------------------------*/
(define-inline (*u32/overflow x::uint32 y::uint32)
   (cond-expand
      ((and bigloo-c (config have-overflow #t))
       (let ((res::long 0))
	  (if (pragma::bool "__builtin_umull_overflow($1, $2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1))*((double)($2)))"
		 x y)
	      (if (<u32 res (bit-lshu32 #u32:1 29))
		  (uint32->fixnum res)
		  (uint32->flonum res)))))
      (else
       (let ((r (*fl (uint32->flonum x) (uint32->flonum y))))
	  (if (integer? r)
	      (overflow29 (flonum->fixnum r))
	      r)))))

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
