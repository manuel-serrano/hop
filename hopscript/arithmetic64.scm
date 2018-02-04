;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/arithmetic64.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec  4 19:36:39 2017                          */
;*    Last change :  Sun Feb  4 07:52:55 2018 (serrano)                */
;*    Copyright   :  2017-18 Manuel Serrano                            */
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
	  (js-number->jsnumber ::obj)
	  
	  (inline overflow29 ::long)
	  (inline overflow53::obj ::long)
	  
	  (js-toint32::int32 ::obj ::JsGlobalObject)
	  (js-touint32::uint32 ::obj ::JsGlobalObject)
	  
	  (js-number-toint32::int32 ::obj)
	  (js-number-touint32::uint32 ::obj)
	  
	  (inline js-int32-tointeger::obj ::int32)
	  (inline js-uint32-tointeger::obj ::uint32)
	  
	  (inline +fx/overflow::obj ::long ::long)
	  (inline +s32/overflow::obj ::int32 ::int32)
	  (inline +u32/overflow::obj ::uint32 ::uint32)
	  (+/overflow::obj ::obj ::obj)
	  
	  (inline -fx/overflow::obj ::long ::long)
	  (inline -s32/overflow::obj ::int32 ::int32)
	  (inline -u32/overflow::obj ::uint32 ::uint32)
	  (-/overflow::obj ::obj ::obj)
	  
	  (inline *fx/overflow::obj ::long ::long)
	  (inline *s32/overflow::obj ::int32 ::int32)
	  (inline *u32/overflow::obj ::uint32 ::uint32)
	  (*/overflow ::obj ::obj)))))

;*---------------------------------------------------------------------*/
;*    oveflow? ...                                                     */
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
;*    overflow53 ...                                                   */
;*    -------------------------------------------------------------    */
;*    2^53-1 overflow                                                  */
;*    -------------------------------------------------------------    */
;*    See Hacker's Delight (second edition), H. Warren J.r,            */
;*    Chapter 4, section 4.1, page 68                                  */
;*---------------------------------------------------------------------*/
(define-inline (overflow53 v::long)
   (let* ((a (-fx 0 (bit-lsh 1 53)))
	  (b (-fx (bit-lsh 1 53) 1))
	  (b-a (-fx b a)))
      (if (<=u64 (fixnum->uint64 (-fx v a)) (fixnum->uint64 b-a))
	  v
	  (fixnum->flonum v))))

(define (forcefail)
   (tprint (/s32 #s32:1 (car (list #s32:0)))))

;*---------------------------------------------------------------------*/
;*    js-toint32 ::obj ...                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.5          */
;*---------------------------------------------------------------------*/
(define (js-toint32::int32 obj %this)
   (cond
      ((or (fixnum? obj) (flonum? obj)) (js-number-toint32 obj))
      ((uint32? obj) (tprint "should not be here.uint32 " obj) (forcefail) (uint32->int32 obj))
      ((int32? obj) (tprint "should not be here.int32 " obj) (forcefail) obj)
      (else (js-number-toint32 (js-tonumber obj %this)))))

;*---------------------------------------------------------------------*/
;*    js-touint32 ::obj ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.6          */
;*---------------------------------------------------------------------*/
(define (js-touint32::uint32 obj %this)
   (cond
      ((or (fixnum? obj) (flonum? obj)) (js-number-touint32 obj))
      ((int32? obj) (tprint "should not be here.int32") (forcefail) (int32->uint32 obj))
      ((uint32? obj) (tprint "should not be here.uint32") (forcefail) obj)
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
;*    +s32/overflow ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (+s32/overflow x::int32 y::int32)
   (+fx/overflow (int32->fixnum x) (int32->fixnum y)))

;*---------------------------------------------------------------------*/
;*    +u32/overflow ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (+u32/overflow x::uint32 y::uint32)
   (+fx/overflow (uint32->fixnum x) (uint32->fixnum y)))

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
;*    -s32/overflow ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (-s32/overflow x::int32 y::int32)
   (-fx/overflow (int32->fixnum x) (int32->fixnum y)))

;*---------------------------------------------------------------------*/
;*    -u32/overflow ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (-u32/overflow x::uint32 y::uint32)
   (-fx/overflow (int32->fixnum x) (int32->fixnum y)))

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
(define-inline (*fx/overflow x::long y::long)
   (cond-expand
      ((and bigloo-c (config have-overflow #t))
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
;*    *s32/overflow ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (*s32/overflow x::int32 y::int32)
   (*fx/overflow (int32->fixnum x) (int32->fixnum y)))

;*---------------------------------------------------------------------*/
;*    *u32/overflow ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (*u32/overflow x::uint32 y::uint32)
   (cond-expand
      ((and bigloo-c (config have-overflow #t))
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

