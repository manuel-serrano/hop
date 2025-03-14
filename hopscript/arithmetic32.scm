;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/arithmetic32.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec  4 19:36:39 2017                          */
;*    Last change :  Fri Mar 14 09:38:31 2025 (serrano)                */
;*    Copyright   :  2017-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Arithmetic operations on 32bit and nan64 platforms               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_arithmetic32

   (library hop)

   (include "types.sch" "stringliteral.sch" "names.sch")

   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_string
	   __hopscript_error
	   __hopscript_property
	   __hopscript_private
	   __hopscript_lib
	   __hopscript_public)

   (extern (macro $real32-set!::real (::real ::double) "BGL_REAL_SET"))
   
   (cond-expand
      ((or bint30 bint32)
       (export
	  (inline js-fixnum->length::uint32 ::long ::JsGlobalObject)
	  
	  (js-number->jsnumber ::obj)
	  
	  (inline js-flonum->integer::obj ::double)
	  (inline negjs-int::obj ::obj)
	  
	  (inline overflowfx ::long)
	  (inline overflowu64::obj ::uint64)
	  (macro overflowu32?)
	  
	  (inline js-toint32::int32 ::obj ::JsGlobalObject)
	  (js-toint32-slow::int32 ::obj ::JsGlobalObject)
	  (inline js-touint32::uint32 ::obj ::JsGlobalObject)
	  (js-touint32-slow::uint32 obj %this)
	  
	  (js-number-toint32::int32 ::obj)
	  (js-number-touint32::uint32 ::obj)
	  
	  (inline js-int32-tointeger::obj ::int32)
	  (inline js-uint32-tointeger::obj ::uint32)

	  (inline js-int53-tointeger::obj ::obj)
	  (js-int53-toint32::int32 ::obj)
	  (js-int53-touint32::uint32 ::obj)
	  
	  (inline ++fx/overflow::obj ::obj ::obj)
	  (inline +fx/overflow::obj ::obj ::obj)
	  (inline +fx32/overflow::obj ::long ::long)
	  (inline +s32/overflow::obj ::int32 ::int32)
	  (inline +u32/overflow::obj ::uint32 ::uint32)
	  (+/overflow::obj ::obj ::obj)
	  (+/overflowfl::double ::obj ::obj)
	  (+/overflow!::obj ::obj ::obj)
	  
	  (inline --fx/overflow::obj ::obj ::obj)
	  (inline -fx/overflow::obj ::obj ::obj)
	  (inline -fx32/overflow::obj ::long ::long)
	  (inline -s32/overflow::obj ::int32 ::int32)
	  (inline -u32/overflow::obj ::uint32 ::uint32)
	  (-/overflow::obj ::obj ::obj)
	  (-/overflowfl::double ::obj ::obj)
	  (-/overflow!::obj ::obj ::obj)
	  
	  (inline *fx/overflow::obj ::obj ::obj)
	  (inline *s32/overflow::obj ::int32 ::int32)
	  (inline *u32/overflow::obj ::uint32 ::uint32)
	  (*/overflow::obj ::obj ::obj)
	  (*/overflowfl::obj ::obj ::obj)
	  (*/overflow!::obj ::obj ::obj)))))

;*---------------------------------------------------------------------*/
;*    __js_strings ...                                                 */
;*---------------------------------------------------------------------*/
(define __js_strings #f)

;*---------------------------------------------------------------------*/
;*    js-fixnum->length ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-fixnum->length len %this)
   (if (>=fx len 0)
       (fixnum->uint32 len)
       (js-raise-range-error %this "index out of range ~a" len)))

;*---------------------------------------------------------------------*/
;*    overflow? ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (overflowfx? num)
   (if (=fx (bigloo-config 'int-size) 32)
       num
       `(overflowfx (int32->fixnum ,val))))

(define-macro (overflowu32? num)
   `(>u32 ,num ,(fixnum->uint32 (maxvalfx))))

(define-macro (overflows64? num)
   `(or (>s64 ,num ,(fixnum->int64 (maxvalfx))) (<s64 ,num ,(fixnum->int64 (minvalfx)))))

(define-macro (overflowu64? num)
   `(>u64 ,num ,(fixnum->uint64 (maxvalfx))))
   
(define-macro (overflowllong? num)
   `(or (>llong ,num ,(fixnum->llong (maxvalfx))) (<llong ,num ,(fixnum->llong (minvalfx)))))
   
;*---------------------------------------------------------------------*/
;*    overflowfx ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (overflowfx v::long)
   (cond-expand
      ((config 'int-size 32)
       v)
      (else
       (if (or (>fx v (maxvalfx)) (<fx v (minvalfx)))
	   (fixnum->flonum v)
	   v))))

;*---------------------------------------------------------------------*/
;*    overflowu64 ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (overflowu64 v::uint64)
   (if (pragma::bool "$1 < ((uint64_t)(1L << 29))" v)
       (uint64->fixnum v)
       (uint64->flonum v)))

;*---------------------------------------------------------------------*/
;*    js-number->jsnumber ...                                          */
;*---------------------------------------------------------------------*/
(define (js-number->jsnumber val)
   (cond
      ((fixnum? val)
       val)
      ((flonum? val)
       val)
      ((uint32? val)
       (js-uint32-tointeger val))
      ((int32? val)
       (overflowfx (int32->fixnum val)))
      ((uint8? val)
       (uint8->fixnum val))
      ((int8? val)
       (int8->fixnum val))
      ((uint16? val)
       (uint16->fixnum val))
      ((int16? val)
       (int16->fixnum val))
      ((int64? val)
       (if (overflows64? val)
	   (int64->flonum val)
	   (int64->fixnum val)))
      ((uint64? val)
       (if (overflowu64? val)
	   (uint64->flonum val)
	   (uint64->fixnum val)))
      ((elong? val)
       (overflowfx (elong->fixnum val)))
      ((llong? val)
       (if (overflowllong? val)
	   (llong->flonum val)
	   (llong->fixnum val)))
      ((bignum? val)
       (bignum->flonum val))
      (else
       (bigloo-type-error "js-number->jsnumber" "number" val))))

;*---------------------------------------------------------------------*/
;*    js-flonum->integer ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-flonum->integer num)
   (if (and (<fl num (-fl (exptfl 2. 30.) 1.))
	    (>=fl num (negfl (exptfl 2. 30.))))
       (flonum->fixnum num)
       num))

;*---------------------------------------------------------------------*/
;*    negjs-int ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (negjs-int num)
   (if (fixnum? num) (if (=fx num 0) -0.0 (negfx num)) (negfl num)))

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
      ((fixnum? obj) (fixnum->int32 obj))
      ((eq? obj (js-undefined)) 0)
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
      ((flonum? obj) (js-number-touint32 obj))
      ((fixnum? obj) (fixnum->uint32 obj))
      ((eq? obj (js-undefined)) 0)
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
	   (let ((i (*fl -1. (floorfl (absfl obj)))))
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
   
   (define (double->uint32::uint32 obj::double)
      (cond
	 ((or (= obj +inf.0) (= obj -inf.0) (not (= obj obj)))
	  #u32:0)
	 ((<fl obj 0.)
	  (llong->uint32
	     (+llong (bit-lshllong #l1 32)
		(flonum->llong (*fl -1. (floorfl (absfl obj)))))))
	 (else
	  (llong->uint32
	     (+llong (bit-lshllong #l1 32)
		(flonum->llong (floorfl (absfl obj))))))))
   
   (cond
      ((flonum? obj) (double->uint32 obj))
      ((fixnum? obj) (fixnum->uint32 obj))
      (else (error "js-number-touint32" "bad number type" obj))))

;*---------------------------------------------------------------------*/
;*    js-int32-tointeger ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-int32-tointeger::obj i::int32)
   (overflowfx (int32->fixnum i)))

;*---------------------------------------------------------------------*/
;*    js-uint32-tointeger ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-uint32-tointeger::obj i::uint32)
   (if (overflowu32? i)
       (uint32->flonum i)
       (uint32->fixnum i)))

;*---------------------------------------------------------------------*/
;*    js-int53-tointeger ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-int53-tointeger::obj i::obj)
   i)

;*---------------------------------------------------------------------*/
;*    js-int53-touint32 ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.6          */
;*---------------------------------------------------------------------*/
(define (js-int53-touint32 i)
   
   (define 2^32 (exptfl 2. 32.))
   
   (define (positive-double->uint32::uint32 i::double)
      (if (<fl i 2^32)
	  (flonum->uint32 i)
	  (flonum->uint32 (remainderfl i 2^32))))
   
   (define (double->uint32::uint32 i::double)
      (cond
	 ((or (= i +inf.0) (= i -inf.0) (not (= i i)))
	  #u32:0)
	 ((<fl i 0.)
	  (positive-double->uint32 (+fl 2^32 (*fl -1. (floor (abs i))))))
	 (else
	  (positive-double->uint32 i))))
   
   (cond
      ((fixnum? i)
       (fixnum->uint32 i))
      ((uint32? i)
       i)
      ((int32? i)
       (int32->uint32 i))
      ((flonum? i)
       (double->uint32 i))
      (else
       (error "js-int53-touint32" "Illegal value" i))))

;*---------------------------------------------------------------------*/
;*    js-int53-toint32 ...                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.5          */
;*---------------------------------------------------------------------*/
(define (js-int53-toint32 i)
   
   (define (int64->int32::int32 i::int64)
      (let* ((i::llong (int64->llong i))
	     (^31 (*llong #l8 (fixnum->llong (bit-lsh 1 28))))
	     (^32 (*llong #l2 ^31))
	     (posint (if (<llong i #l0) (+llong ^32 i) i))
	     (int32bit (modulollong posint ^32))
	     (n (if (>=llong int32bit ^31)
		    (-llong int32bit ^32)
		    int32bit)))
	 (llong->int32 n)))
   
   (cond
      ((int32? i)
       i)
      ((uint32? i)
       (uint32->int32 i))
      ((fixnum? i)
       (fixnum->int32 i))
      ((flonum? i)
       (cond
	  ((or (= i +inf.0) (= i -inf.0) (nanfl? i))
	   (fixnum->int32 0))
	  ((<fl i 0.)
	   (let ((i (*fl -1. (floor (abs i)))))
	      (if (>=fl i (negfl (exptfl 2. 31.)))
		  (fixnum->int32 (flonum->fixnum i))
		  (int64->int32 (flonum->int64 i)))))
	  (else
	   (let ((i (floor i)))
	      (if (<=fl i (-fl (exptfl 2. 31.) 1.))
		  (fixnum->int32 (flonum->fixnum i))
		  (int64->int32 (flonum->int64 i)))))))
      (else
       (error "js-int53-toint32" "Illegal value" i))))

;*---------------------------------------------------------------------*/
;*    tolong ...                                                       */
;*---------------------------------------------------------------------*/
(define (tolong x)
   (cond
      ((fixnum? x) x)
      ((=fl x 0.0) 0)
      ((int32? x) (int32->fixnum x))
      ((uint32? x) (uint32->fixnum x))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    todouble ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (todouble::double x)
   (cond
      ((fixnum? x) (fixnum->flonum x))
      ((int32? x) (int32->flonum x))
      ((uint32? x) (uint32->flonum x))
      (else x)))

;*---------------------------------------------------------------------*/
;*    ++fx/overflow ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (++fx/overflow x::obj y::obj)
   (+fx/overflow x 1))

;*---------------------------------------------------------------------*/
;*    +fx/overflow ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (+fx/overflow x::obj y::obj)
   (cond-expand
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t) (or (config nan-tagging #t) (config nun-tagging #t)))
       (let ((res::int 0))
	  (if (pragma::bool "__builtin_sadd_overflow((int)((long)$1), (int)((long)$2), &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)(CINT($1)))+((double)(CINT($2))))"
		 x y)
	      (pragma::bint "BINT($1)" res))))
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t))
       (let ((res::long 0))
	  (if (pragma::bool "__builtin_saddl_overflow((long)$1, (long)$2-TAG_INT, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)(CINT($1)))+((double)(CINT($2))))"
		 x y)
	      (pragma::bint "(obj_t)($1)" res))))
      (else
       (overflowfx (+fx x y)))))

;*---------------------------------------------------------------------*/
;*    +fx32/overflow ...                                               */
;*    -------------------------------------------------------------    */
;*    X and Y are true 32bit values.                                   */
;*---------------------------------------------------------------------*/
(define-inline (+fx32/overflow x::long y::long)
   (cond-expand
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t) (or (config nan-tagging #t) (config nun-tagging #t)))
       (let ((res::int 0))
	  (if (pragma::bool "__builtin_sadd_overflow((int)$1, (int)$2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1))+((double)($2)))"
		 x y)
	      (pragma::bint "BINT($1)" res))))
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t))
       (let ((res::long 0))
	  (if (pragma::bool "__builtin_saddl_overflow($1, $2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1))+((double)($2)))"
		 x y)
	      (overflowfx res))))
      (else
       (let ((z::long (pragma::long "(~($1 ^ $2)) & 0x80000000" x y)))
	  (if (pragma::bool "$1 & (~((($2 ^ $1) + ($3)) ^ ($3)))" z x y)
	      (fixnum->flonum (+fx x y))
	      (overflowfx (+fx x y)))))))

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
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t) (or (config nan-tagging #t) (config nun-tagging #t)))
       (let ((res::uint32 0))
	  (if (pragma::bool "__builtin_uadd_overflow($1, $2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1))+((double)($2)))"
		 x y)
	      (js-uint32-tointeger res))))
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t))
       (let ((res::ulong 0))
	  (if (pragma::bool "__builtin_uaddl_overflow($1, $2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1))+((double)($2)))"
		 x y)
	      (js-uint32-tointeger (pragma::uint32 "(uint32_t)$1" res)))))
      (else
       (let ((z::long (pragma::long "(~($1 ^ $2)) & 0x80000000" x y)))
	  (if (pragma::bool "$1 & (~((($2 ^ $1) + ($3)) ^ ($3)))" z x y)
	      (uint32->flonum (+u32 x y))
	      (overflowfx (uint32->fixnum (+u32 x y))))))))

;*---------------------------------------------------------------------*/
;*    +/overflow ...                                                   */
;*    -------------------------------------------------------------    */
;*    This function is the compiler fallback used when it finds no     */
;*    specialized addition function. In practice it should hardly      */
;*    be called.                                                       */
;*---------------------------------------------------------------------*/
(define (+/overflow x::obj y::obj)
   (let ((ll (tolong x)))
      (cond
	 (ll
	  (let ((rl (tolong y)))
	     (if rl
		 (+fx32/overflow ll rl)
		 (+fl (todouble x) (todouble y)))))
	 ((flonum? x)
	  (+fl x (todouble y)))
	 ((flonum? y)
	  (+fl y (todouble x)))
	 (else
	  (+fl (todouble x) (todouble y))))))

;*---------------------------------------------------------------------*/
;*    +/overflowfl ...                                                 */
;*---------------------------------------------------------------------*/
(define (+/overflowfl::double x::obj y::obj)
   (+fl (todouble x) (todouble y)))

;*---------------------------------------------------------------------*/
;*    +/overflow! ...                                                  */
;*    -------------------------------------------------------------    */
;*    Specialized version used by the compiler when real mutation      */
;*    is possible.                                                     */
;*---------------------------------------------------------------------*/
(define (+/overflow! x::obj y::obj)
   (let ((ll (tolong x)))
      (if ll
	  (let ((rl (tolong y)))
	     (if rl
		 (+fx/overflow ll rl)
		 (+fl (todouble x) (todouble y))))
	  (cond
	     ((flonum? x)
	      ($real32-set! x (+fl x (todouble y))))
	     ((flonum? y)
	      ($real32-set! y (+fl y (todouble x))))
	     (else
	      (+fl (todouble x) (todouble y)))))))

;*---------------------------------------------------------------------*/
;*    --fx/overflow ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (--fx/overflow x::obj y::obj)
   (-fx/overflow x 1))

;*---------------------------------------------------------------------*/
;*    -fx/overflow ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (-fx/overflow x::obj y::obj)
   (cond-expand
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t) (or (config nan-tagging #t) (config nun-tagging #t)))
       (let ((res::int 0))
	  (if (pragma::bool "__builtin_ssub_overflow((int)((long)$1), (int)((long)$2), &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)(CINT($1)))+((double)(CINT($2))))"
		 x y)
	      (pragma::bint "BINT($1)" res))))
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t))
       (let ((res::long 0))
	  (if (pragma::bool "__builtin_ssubl_overflow((long)$1, (long)$2-TAG_INT, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)(CINT($1)))+((double)(CINT($2))))"
		 x y)
	      (pragma::bint "(obj_t)($1)" res))))
      (else
       (overflowfx (-fx x y)))))

;*---------------------------------------------------------------------*/
;*    -fx32/overflow ...                                               */
;*    -------------------------------------------------------------    */
;*    X and Y are true 32bit values.                                   */
;*---------------------------------------------------------------------*/
(define-inline (-fx32/overflow x::long y::long)
   (cond-expand
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t) (or (config nan-tagging #t) (config nun-tagging #t)))
       (let ((res::int32 #s32:0))
	  (if (pragma::bool "__builtin_ssub_overflow($1, $2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1))-((double)($2)))"
		 x y)
	     (pragma::bint "BINT($1)" res))))
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t))
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
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t) (or (config nan-tagging #t) (config nun-tagging #t)))
       (let ((res::uint32 0))
	  (if (pragma::bool "__builtin_usub_overflow($1, $2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1))-((double)($2)))"
		 x y)
	      (js-uint32-tointeger res))))
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t))
       (let ((res::ulong 0))
	  (if (pragma::bool "__builtin_usubl_overflow($1, $2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1))-((double)($2)))"
		 x y)
	      (js-uint32-tointeger (pragma::uint32 "(uint32_t)$1" res)))))
      (else
       (if (>=u32 x y)
	   (js-uint32-tointeger (-u32 x y))
	   (-fl (uint32->flonum x) (uint32->flonum y))))))

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
;*    -/overflowfl ...                                                 */
;*---------------------------------------------------------------------*/
(define (-/overflowfl::double x::obj y::obj)
   (-fl (todouble x) (todouble y)))

;*---------------------------------------------------------------------*/
;*    -/overflow! ...                                                  */
;*---------------------------------------------------------------------*/
(define (-/overflow! x y)
   (let ((ll (tolong x)))
      (cond
	 (ll
	  (let ((rl (tolong y)))
	     (if rl
		 (-fx32/overflow ll rl)
		 (-fl (todouble x) (todouble y)))))
	 ((flonum? x)
	  ($real32-set! x (-fl x (todouble y))))
	 ((flonum? y)
	  ($real32-set! y (-fl (todouble x) y)))
	 (else
	  (-fl (todouble x) (todouble y))))))

;*---------------------------------------------------------------------*/
;*    *fx/overflow ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (*fx/overflow x::obj y::obj)
   (cond-expand
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t) (or (config nan-tagging #t) (config nun-tagging #t)))
       (let ((res::int32 #s32:0))
	  (cond
	     ((pragma::bool "__builtin_smul_overflow((int)((long)$1), (int)((long)$2), &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)CINT($1))*((double)CINT($2)))"
		 x y))
	     ((=s32 res #s32:0)
	      (if (or (and (<fx x 0) (>=fx y 0))
		      (and (>=fx x 0) (<fx y 0)))
		  -0.0
		  (pragma::bint "BINT($1)" res)))
	     (else
	      (pragma::bint "BINT($1)" res)))))
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t))
       (let ((res::long 0))
	  (cond
	     ((pragma::bool "__builtin_smull_overflow((long)(CINT($1)), (long)(CINT($2)), &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)(CINT(($1))))*((double)(CINT($2))))"
		 x y))
	     ((=fx res 0)
	      (if (or (and (<fx x 0) (>=fx y 0))
		      (and (>=fx x 0) (<fx y 0)))
		  -0.0
		  (overflowfx res)))
	     (else
	      (overflowfx res)))))
      (else
       (define (neg? o)
	  (if (flonum? o)
	      (not (=fx (signbitfl o) 0))
	      (<fx o 0)))
       (let ((r (* x y)))
	  (cond
	     ((fixnum? r)
	      (if (=fx r 0)
		  (if (or (and (neg? x) (not (neg? y)))
			  (and (not (neg? x)) (neg? y)))
		      -0.0)
		  (overflowfx r)))
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
;*    *s32/overflow ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (*s32/overflow x::int32 y::int32)
   (*fx/overflow (int32->fixnum x) (int32->fixnum y)))

;*---------------------------------------------------------------------*/
;*    *u32/overflow ...                                                */
;*    -------------------------------------------------------------    */
;*    The argument are 30bit integers encoded into long values.        */
;*---------------------------------------------------------------------*/
(define-inline (*u32/overflow x::uint32 y::uint32)
   (cond-expand
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t) (or (config nan-tagging #t) (config nun-tagging #t)))
       (let ((res::uint32 0))
	  (if (pragma::bool "__builtin_umul_overflow($1, $2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1))*((double)($2)))"
		 x y)
	      (js-uint32-tointeger (pragma::uint32 "(uint32_t)$1" res)))))
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t))
       (let ((res::ulong 0))
	  (if (pragma::bool "__builtin_umull_overflow($1, $2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1))*((double)($2)))"
		 x y)
	      (js-uint32-tointeger res))))
      (else
       (let ((r (*fl (uint32->flonum x) (uint32->flonum y))))
	  (if (integer? r)
	      (overflowfx (flonum->fixnum r))
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

;*---------------------------------------------------------------------*/
;*    */overflowfl ...                                                 */
;*---------------------------------------------------------------------*/
(define (*/overflowfl x y)
   (*fl (todouble x) (todouble y)))

;*---------------------------------------------------------------------*/
;*    */overflow! ...                                                  */
;*---------------------------------------------------------------------*/
(define (*/overflow! x y)
   (let ((ll (tolong x)))
      (cond
	 (ll
	  (let ((rl (tolong y)))
	     (if rl
		 (*fx/overflow ll rl)
		 (*fl (todouble x) (todouble y)))))
	 ((flonum? x)
	  ($real32-set! x (*fl x (todouble y))))
	 ((flonum? y)
	  ($real32-set! y (*fl y (todouble x))))
	 (else
	  (*fl (todouble x) (todouble y))))))
