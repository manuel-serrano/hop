;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/arithmetic64.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec  4 19:36:39 2017                          */
;*    Last change :  Thu Jul 20 09:07:54 2023 (serrano)                */
;*    Copyright   :  2017-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Arithmetic operations on 64 bit platforms                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_arithmetic64
   
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
   
   (extern (macro $real64-set!::real (::real ::double) "BGL_REAL_SET")
	   (macro $+fx/ov53::obj (::bint ::bint ::long) "BGL_ADDFX_OV53"))
   
   (cond-expand
      ((or bint61 bint64)
       (export
	  (inline js-fixnum->length::uint32 ::long ::JsGlobalObject)
	  (js-number->jsnumber ::obj)

	  (inline js-flonum->integer::long ::double)
	  (inline negjs-int::obj ::long)
	  
	  (inline overflowfx ::long)
	  (inline overflow53::obj ::long)
	  (inline overflowu64::obj ::uint64)
	  
	  (inline js-toint32::int32 ::obj ::JsGlobalObject)
	  (js-toint32-slow::int32 ::obj ::JsGlobalObject)
	  (inline js-touint32::uint32 ::obj ::JsGlobalObject)
	  (js-touint32-slow::uint32 obj %this)
	  
	  (js-number-toint32::int32 ::obj)
	  (js-number-touint32::uint32 ::obj)
	  
	  (inline js-int32-tointeger::obj ::int32)
	  (inline js-uint32-tointeger::obj ::uint32)
	  
	  (inline js-int53-tointeger::bint ::obj)
	  (inline js-int53-toint32::int32 ::obj)
	  (inline js-int53-touint32::uint32 ::obj)

	  (inline js-int53-inc::long ::long)
	  (inline js-int53-dec::long ::long)
	  
	  (inline +fx/overflow::obj ::long ::long)
	  (inline +fx/overflow53::obj ::bint ::bint)
	  (inline +s32/overflow::obj ::int32 ::int32)
	  (inline +u32/overflow::obj ::uint32 ::uint32)
	  (+/overflow::obj ::obj ::obj)
	  (+/overflowfl::double ::obj ::obj)
	  (+/overflow!::obj ::obj ::obj)
	  
	  (inline -fx/overflow::obj ::long ::long)
	  (inline -s32/overflow::long ::int32 ::int32)
	  (inline -u32/overflow::long ::uint32 ::uint32)
	  (-/overflow::obj ::obj ::obj)
	  (-/overflowfl::double ::obj ::obj)
	  (-/overflow!::obj ::obj ::obj)
	  
	  (inline *fx/overflow::obj ::long ::long)
	  (inline *fx/overflow-sans-zero::obj ::long ::long)
	  (inline *s32/overflow::obj ::int32 ::int32)
	  (inline *u32/overflow::obj ::uint32 ::uint32)
	  (*/overflow ::obj ::obj)
	  (*/overflowfl::double ::obj ::obj)
	  (*/overflow!::obj ::obj ::obj)))))

;*---------------------------------------------------------------------*/
;*    __js_strings ...                                                 */
;*---------------------------------------------------------------------*/
(define __js_strings #f)

;*---------------------------------------------------------------------*/
;*    js-fixnum->length ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-fixnum->length len %this)
   (if (and (>=fx len 0) (<fx len 4294967296))
       (fixnum->uint32 len)
       (js-raise-range-error %this "index out of range ~a" len)))

;*---------------------------------------------------------------------*/
;*    overflow? ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (overflows64? num shift)
   `(not (=s64 (bit-ands64 ,num (bit-nots64 (fixnum->int64 (-fx (bit-lsh 1 ,shift) 1)))) #s64:0)))

(define-macro (overflowu64? num shift)
   `(not (=u64 (bit-andu64 ,num (bit-notu64 (fixnum->uint64 (-fx (bit-lsh 1 ,shift) 1)))) #u64:0)))
   
(define-macro (overflowllong? num shift)
   `(not (=llong (bit-andllong ,num (bit-notllong (fixnum->llong (-fx (bit-lsh 1 ,shift) 1)))) 0)))
   
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
       (uint32->fixnum val))
      ((int32? val)
       (int32->fixnum val))
      ((uint8? val)
       (uint8->fixnum val))
      ((int8? val)
       (int8->fixnum val))
      ((uint16? val)
       (uint16->fixnum val))
      ((int16? val)
       (int16->fixnum val))
      ((int64? val)
       (if (overflows64? val 53)
	   (int64->flonum val)
	   (int64->fixnum val)))
      ((uint64? val)
       (if (overflowu64? val 53)
	   (uint64->flonum val)
	   (uint64->fixnum val)))
      ((elong? val)
       (elong->fixnum val))
      ((llong? val)
       (if (overflowllong? val 53)
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
   (flonum->fixnum num))

;*---------------------------------------------------------------------*/
;*    negjs-int ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (negjs-int num)
   (if (=fx num 0) -0.0 (negfx num)))

;*---------------------------------------------------------------------*/
;*    overflowfx ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (overflowfx v::long)
   (overflow53 v))

;*---------------------------------------------------------------------*/
;*    overflow53 ...                                                   */
;*    -------------------------------------------------------------    */
;*    2^53-1 overflow                                                  */
;*    -------------------------------------------------------------    */
;*    See Hacker's Delight (second edition), H. Warren J.r,            */
;*    Chapter 4, section 4.1, page 68                                  */
;*---------------------------------------------------------------------*/
(define-inline (overflow53 v::long)
   ;; (let* ((a (-fx 0 (bit-lsh 1 53)))
   ;; 	  (b (-fx (bit-lsh 1 53) 1))
   ;;	  (b-a (-fx b a)))
   ;;  (if (<=u64 (fixnum->uint64 (-fx v a)) (fixnum->uint64 b-a))
   ;;	  v
   ;;	  (fixnum->flonum v)))
   ;;
   ;; use expanded constant and test to minimize code size generation
   (if (pragma::bool "(uint64_t)($1 - -9007199254740992) <= (uint64_t)18014398509481983" v)
       v
       (fixnum->flonum v)))

;*---------------------------------------------------------------------*/
;*    overflowu64 ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (overflowu64 v::uint64)
   (if (pragma::bool "$1 < ((uint64_t)(1L << 53))" v)
       (uint64->fixnum v)
       (uint64->flonum v)))

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
	  ((or (=fl obj +inf.0) (=fl obj -inf.0) (nanfl? obj))
	   (fixnum->int32 0))
	  ((<fl obj 0.)
	   (let ((i (*fl -1. (floorfl (absfl obj)))))
	      (if (>=fl i (negfl (exptfl 2. 31.)))
		  (fixnum->int32 (flonum->fixnum i))
		  (int64->int32 (flonum->int64 i)))))
	  (else
	   (let ((i (floorfl obj)))
	      (if (<=fl i (-fl (exptfl 2. 31.) 1.))
		  (fixnum->int32 (flonum->fixnum i))
		  (int64->int32 (flonum->int64 i)))))))
      ((uint32? obj)
       (js-number-toint32 (uint32->fixnum obj)))
      ((int32? obj)
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
	 ((or (=fl obj +inf.0) (=fl obj -inf.0) (not (=fl obj obj)))
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
      ((int32? obj)
       (js-number-touint32 (int32->fixnum obj)))
      ((uint32? obj)
       obj)
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
;*    js-int53-tointeger ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-int53-tointeger::bint i::obj)
   i)

;*---------------------------------------------------------------------*/
;*    js-int53-touint32 ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.6          */
;*---------------------------------------------------------------------*/
(define-inline (js-int53-touint32 i)
   (fixnum->uint32 i))
   
(define (js-int53-touint32-TOBEREMOVED-4sep2018 i)
   (define 2^32 (exptfl 2. 32.))
   
   (define (positive-double->uint32::uint32 i::double)
      (if (<fl i 2^32)
	  (flonum->uint32 i)
	  (flonum->uint32 (remainderfl i 2^32))))
   
   (define (double->uint32::uint32 i::double)
      (cond
	 ((or (=fl i +inf.0) (=fl i -inf.0) (not (=fl i i)))
	  #u32:0)
	 ((<fl i 0.)
	  (positive-double->uint32 (+fl 2^32 (*fl -1. (floor (abs i))))))
	 (else
	  (positive-double->uint32 i))))
   
   (cond
      ((fixnum? i)
       (if (<=fx i (-fx (bit-lsh 1 32) 1))
	   (fixnum->uint32 i)
	   (let* ((^31 (bit-lsh 1 31))
		  (^32 (bit-lsh 1 32))
		  (posint (if (<fx i 0) (+fx ^32 i) i))
		  (int32bit (modulofx posint ^32)))
	      (fixnum->uint32 int32bit))))
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
(define-inline (js-int53-toint32 i)
   (fixnum->int32 i))

(define (js-int53-toint32-TOBEREMOVED-4sep2018 i)
   
   (define (int64->int32::int32 i::int64)
      (let* ((i::elong (int64->elong i))
	     (^31 (fixnum->elong (bit-lsh 1 31)))
	     (^32 (fixnum->elong (bit-lsh 1 32)))
	     (posint (if (<elong i #e0) (+elong ^32 i) i))
	     (int32bit (moduloelong posint ^32))
	     (n (if (>=elong int32bit ^31)
		    (-elong int32bit ^32)
		    int32bit)))
	 (elong->int32 n)))
   
   (cond
      ((int32? i)
       i)
      ((uint32? i)
       (uint32->int32 i))
      ((fixnum? i)
       (cond-expand
	  (bint61
	   (if (and (<=fx i (-fx (bit-lsh 1 31) 1))
		    (>=fx i (negfx (bit-lsh 1 31))))
	       (fixnum->int32 i)
	       (let* ((^31 (bit-lsh 1 31))
		      (^32 (bit-lsh 1 32))
		      (posint (if (<fx i 0) (+fx ^32 i) i))
		      (int32bit (modulofx posint ^32))
		      (n (if (>=fx int32bit ^31)
			     (-fx int32bit ^32)
			     int32bit)))
		  (fixnum->int32 n))))
	  (else
	   (int64->int32 (fixnum->int64 i)))))
      ((flonum? i)
       (cond
	  ((or (=fl i +inf.0) (=fl i -inf.0) (nanfl? i))
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
      (else #f)))

;*---------------------------------------------------------------------*/
;*    todouble ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (todouble::double x)
   (cond
      ((flonum? x) x)
      ((fixnum? x) (fixnum->flonum x))
      (else +nan.0)))

;*---------------------------------------------------------------------*/
;*    js-int53-inc ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (js-int53-inc x::long)
   (if (>fx (bit-rsh x 53) 0) x (+fx x 1)))

;*---------------------------------------------------------------------*/
;*    js-int53-dec ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (js-int53-dec x::long)
   (if (<fx (bit-rsh x 53) -1) x (-fx x 1)))

;*---------------------------------------------------------------------*/
;*    +fx/overflow ...                                                 */
;*    -------------------------------------------------------------    */
;*    Fixnum addition on 64 bits machines (three tagging bits).        */
;*---------------------------------------------------------------------*/
(define-inline (+fx/overflow::obj x::long y::long)
   ;; see arithmetic.sch
   (overflow53 (+fx x y)))

(define-inline (+fx/overflow53::obj x::bint y::bint)
   ;; see arithmetic.sch
   ($let ((tmp::bint 0))
      ($+fx/ov53 x y tmp)))

;*---------------------------------------------------------------------*/
;*    +s32/overflow ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (+s32/overflow x::int32 y::int32)
   (+fx (int32->fixnum x) (int32->fixnum y)))

;*---------------------------------------------------------------------*/
;*    +u32/overflow ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (+u32/overflow x::uint32 y::uint32)
   (+fx (uint32->fixnum x) (uint32->fixnum y)))

;*---------------------------------------------------------------------*/
;*    +/overflow ...                                                   */
;*    -------------------------------------------------------------    */
;*    This function is the compiler fallback used when it finds no     */
;*    specialized addition function. In practice it should hardly      */
;*    be called.                                                       */
;*---------------------------------------------------------------------*/
(define (+/overflow x::obj y::obj)
   ;; (tprint "+/over x=" x " " (typeof x) " y=" y " " (typeof y))
   (let ((ll (tolong x)))
      (if ll
	  (let ((rl (tolong y)))
	     (if rl
		 (+fx/overflow ll rl)
		 (+fl (todouble x) (todouble y))))
	  (+fl (todouble x) (todouble y)))))

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
      (cond
	 (ll
	  (let ((rl (tolong y)))
	     (if rl
		 (+fx/overflow ll rl)
		 (+fl (todouble x) (todouble y)))))
	 ((flonum? x)
	  ($real64-set! x (+fl x (todouble y))))
	 ((flonum? y)
	  ($real64-set! y (+fl y (todouble x))))
	 (else
	  (+fl (todouble x) (todouble y))))))

;*---------------------------------------------------------------------*/
;*    -fx/overflow ...                                                 */
;*    -------------------------------------------------------------    */
;*    see +fx/overflow                                                 */
;*---------------------------------------------------------------------*/
(define-inline (-fx/overflow::obj x::long y::long)
   (overflow53 (-fx x y)))

;*---------------------------------------------------------------------*/
;*    -s32/overflow ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (-s32/overflow x::int32 y::int32)
   (-fx (int32->fixnum x) (int32->fixnum y)))

;*---------------------------------------------------------------------*/
;*    -u32/overflow ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (-u32/overflow x::uint32 y::uint32)
   (-fx (int32->fixnum x) (int32->fixnum y)))

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
		 (-fx/overflow ll rl)
		 (-fl (todouble x) (todouble y)))))
	 ((flonum? x)
	  ($real64-set! x (-fl x (todouble y))))
	 ((flonum? y)
	  ($real64-set! y (-fl (todouble x) y)))
	 (else
	  (-fl (todouble x) (todouble y))))))

;*---------------------------------------------------------------------*/
;*    *fx/overflow ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (*fx/overflow x::long y::long)
   (cond-expand
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t))
       (let ((res::long 0))
	  (cond
	     ((pragma::bool "__builtin_smull_overflow((long)$1, (long)$2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1)) * ((double)($2)))"
		 x y))
	     ((=fx res 0)
	      (if (or (and (<fx x 0) (>=fx y 0))
		      (and (>=fx x 0) (<fx y 0)))
		  -0.0
		  (overflow53 res)))
	     (else
	      (overflow53 res)))))
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
		  (overflow53 r)))
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
;*    *fx/overflow-sans-zero ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (*fx/overflow-sans-zero x::long y::long)
   (cond-expand
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t))
       (let ((res::long 0))
	  (cond
	     ((pragma::bool "__builtin_smull_overflow((long)$1, (long)$2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1)) * ((double)($2)))"
		 x y))
	     (else
	      (overflow53 res)))))
      (else
       (let ((r (* x y)))
	  (cond
	     ((fixnum? r)
	      (if (=fx r 0)
		  (overflow53 r)))
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
   (cond-expand
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t))
       (let ((res::long 0))
	  ;; left align the 32bit numbers (64-53-1)=10
	  (if (pragma::bool "__builtin_smull_overflow($1 << 10, $2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1))*((double)($2)))"
		 x y)
	      (pragma::obj "BINT(($1) >> 10)" res))))
      (else
       (*fx/overflow-sans-zero (int32->fixnum x) (int32->fixnum y)))))

;*---------------------------------------------------------------------*/
;*    *u32/overflow ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (*u32/overflow x::uint32 y::uint32)
   (cond-expand
      ((and bigloo-c (not bigloo-saw) (config have-overflow #t))
       (let ((res::long 0))
	  (if (pragma::bool "__builtin_umull_overflow($1, $2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1))*((double)($2)))"
		 x y)
	      (if (<fx (uint32->fixnum res) (bit-lsh 1 53))
		  (uint32->fixnum res)
		  (uint32->flonum res)))))
      (else
       (let ((r (*fl (uint32->flonum x) (uint32->flonum y))))
	  (if (integer? r)
	      (overflow53 (flonum->fixnum r))
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
	  ($real64-set! x (*fl x (todouble y))))
	 ((flonum? y)
	  ($real64-set! y (*fl y (todouble x))))
	 (else
	  (*fl (todouble x) (todouble y))))))
