;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/arithmetic.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec  4 07:42:21 2017                          */
;*    Last change :  Mon Dec  4 18:10:09 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    JS mostly machine dependent arithmetic operations.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_arithmetic

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

   (export (inline js-int29-tointeger::bint ::int32)
	   (inline js-uint29-tointeger::bint ::uint32)
	   
	   (inline js-int32-tointeger::obj ::int32)
	   (inline js-uint32-tointeger::obj ::uint32)
	   
	   (js-int53-toint32::int32 ::obj)
	   (js-int53-touint32::uint32 ::obj)
	   
	   (js-toint32::int32 ::obj ::JsGlobalObject)
	   (js-touint32::uint32 obj %this))

   (cond-expand
      ((or bint30 bint32)
       (export (inline js-int53-tointeger::obj ::obj)
	  (inline +s32/safe::obj ::int32 ::int32)
	  (inline +u32/safe::obj ::uint32 ::uint32)
	  (inline js+fx32::obj ::long ::long)))
      ((or bint61 bint64)
       (export (inline js-int53-tointeger::bint ::obj)
	       (inline js+fx64::obj ::long ::long)))
      (else
       (error "arithmetic" "unknown integer format" #f))))

;*---------------------------------------------------------------------*/
;*    js-uint29-tointeger ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-int29-tointeger::bint i::int32)
   (int32->fixnum i))

;*---------------------------------------------------------------------*/
;*    js-int29-tointeger ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-uint29-tointeger::bint i::uint32)
   (uint32->fixnum i))

;*---------------------------------------------------------------------*/
;*    js-int32-tointeger ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-int32-tointeger::obj i::int32)
   (cond-expand
      (bint30
       (if (and (<s32 i (fixnum->int32 (bit-lsh 1 28)))
		(>=s32 i (fixnum->int32 (negfx (bit-lsh 1 28)))))
	   (int32->fixnum i)
	   (elong->flonum (uint32->elong i))))
      (bint32
       (if (and (<s32 i (fixnum->int32 (bit-lsh 1 30)))
		(>=s32 i (fixnum->int32 (negfx (bit-lsh 1 30)))))
	   (int32->fixnum i)
	   (elong->flonum (uint32->elong i))))
      (else
       (int32->fixnum i))))

;*---------------------------------------------------------------------*/
;*    js-uint32-tointeger ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-uint32-tointeger::obj u::uint32)
   (cond-expand
      (bint30
       (if (<u32 u (bit-lshu32 #u32:1 29))
	   (uint32->fixnum u)
	   (uint32->flonum u)))
      (else
       (uint32->fixnum u))))

;*---------------------------------------------------------------------*/
;*    js-int53-tointeger ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-int53-tointeger i)
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
       (cond-expand
	  (bint30
	   (fixnum->uint32 i))
	  (bint32
	   (int32->uint32 (fixnum->int32 i)))
	  (else
	   (if (<=fx i (-fx (bit-lsh 1 32) 1))
	       (fixnum->uint32 i)
	       (let* ((^31 (bit-lsh 1 31))
		      (^32 (bit-lsh 1 32))
		      (posint (if (<fx i 0) (+fx ^32 i) i))
		      (int32bit (modulofx posint ^32)))
		  (fixnum->uint32 int32bit))))))
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
      (cond-expand
	 ((or bint30 bint32)
	  (let* ((i::llong (int64->llong i))
		 (^31 (*llong #l8 (fixnum->llong (bit-lsh 1 28))))
		 (^32 (*llong #l2 ^31))
		 (posint (if (<llong i #l0) (+llong ^32 i) i))
		 (int32bit (modulollong posint ^32))
		 (n (if (>=llong int32bit ^31)
			(-llong int32bit ^32)
			int32bit)))
	     (llong->int32 n)))
	 (else
	  (let* ((i::elong (int64->elong i))
		 (^31 (fixnum->elong (bit-lsh 1 31)))
		 (^32 (fixnum->elong (bit-lsh 1 32)))
		 (posint (if (<elong i #e0) (+elong ^32 i) i))
		 (int32bit (moduloelong posint ^32))
		 (n (if (>=elong int32bit ^31)
			(-elong int32bit ^32)
			int32bit)))
	     (elong->int32 n)))))
   
   (cond
      ((int32? i)
       i)
      ((uint32? i)
       (uint32->int32 i))
      ((fixnum? i)
       (cond-expand
	  ((or bint30 bint32)
	   (fixnum->int32 i))
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
;*    js-toint32 ::obj ...                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.5          */
;*---------------------------------------------------------------------*/
(define (js-toint32::int32 obj %this)

   (define (int64->int32::int32 obj::int64)
      (cond-expand
	 ((or bint30 bint32)
	  (let* ((i::llong (int64->llong obj))
		 (^31 (*llong #l8 (fixnum->llong (bit-lsh 1 28))))
		 (^32 (*llong #l2 ^31))
		 (posint (if (<llong i #l0) (+llong ^32 i) i))
		 (int32bit (modulollong posint ^32))
		 (n (if (>=llong int32bit ^31)
			(-llong int32bit ^32)
			int32bit)))
	     (llong->int32 n)))
	 (else
	  (let* ((i::elong (int64->elong obj))
		 (^31 (fixnum->elong (bit-lsh 1 31)))
		 (^32 (fixnum->elong (bit-lsh 1 32)))
		 (posint (if (<elong i #e0) (+elong ^32 i) i))
		 (int32bit (moduloelong posint ^32))
		 (n (if (>=elong int32bit ^31)
			(-elong int32bit ^32)
			int32bit)))
	     (elong->int32 n)))))

   (cond
      ((int32? obj)
       obj)
      ((uint32? obj)
       (uint32->int32 obj))
      ((fixnum? obj)
       (cond-expand
	  ((or bint30 bint32)
	   (fixnum->int32 obj))
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
       (js-toint32 (js-tonumber obj %this) %this))))

;*---------------------------------------------------------------------*/
;*    js-touint32 ::obj ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.6          */
;*---------------------------------------------------------------------*/
(define (js-touint32::uint32 obj %this)
   
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
	  (bint30
	   (fixnum->uint32 obj))
	  (bint32
	   (int32->uint32 (fixnum->int32 obj)))
	  (else
	   (if (<=fx obj (-fx (bit-lsh 1 32) 1))
	       (fixnum->uint32 obj)
	       (let* ((^31 (bit-lsh 1 31))
		      (^32 (bit-lsh 1 32))
		      (posint (if (<fx obj 0) (+fx ^32 obj) obj))
		      (int32bit (modulofx posint ^32)))
		  (fixnum->uint32 int32bit))))))
      ((uint32? obj)
       (let ((r::uint32 obj)) r))
      ((int32? obj)
       (int32->uint32 obj))
      ((flonum? obj)
       (double->uint32 obj))
      (else
       (js-touint32 (js-tointeger obj %this) %this))))
		  
;*---------------------------------------------------------------------*/
;*    +s32/safe ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (+s32/safe x::int32 y::int32)
   ;; requires x and y to be tagged
   (cond-expand
      ((and bigloo-c (config have-overflow #t))
       (let ((res::long 0))
	  (cond
	     ((pragma::bool "__builtin_saddl_overflow( (long)$1, (long)$2, &$3 )"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL( ((double)($1) + ((double)($2))) )"
		 x y))
	     ((=s32 res (bit-ands32 res (bit-lshs32 #s32:3 30)))
	      (pragma::real "DOUBLE_TO_REAL( ((double)($1) + ((double)($2))) )"
		 x y))
	     (else
	      (pragma::bint "(obj_t)($1 - TAG_INT)" res)))))
      (else
       (let ((z::long (pragma::long "(~((long)$1 ^ (long)$2)) & 0x80000000" x y)))
	  (if (pragma::bool "$1 & (~((((long)$2 ^ (long)$1) + ((long)$3)) ^ ((long) $3)))" z x y)
	      (+fl (int32->flonum x) (int32->flonum y))
	      (+s32 x y))))))

;*---------------------------------------------------------------------*/
;*    +u32/safe ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (+u32/safe x::uint32 y::uint32)
   ;; requires x and y to be tagged
   (cond-expand
      ((and bigloo-c (config have-overflow #t))
       (let ((res::ulong (pragma::ulong "(ulong)($1)" #u32:0)))
	  (cond
	     ((pragma::bool "__builtin_uaddl_overflow( (ulong)$1, (ulong)$2, &$3 )"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL( ((double)($1) + ((double)($2))) )"
		 x y))
	     ((=u32 res (bit-andu32 res (bit-lshu32 #u32:3 30)))
	      (pragma::real "DOUBLE_TO_REAL( ((double)($1) + ((double)($2))) )"
		 x y))
	     (else
	      (pragma::bint "(obj_t)($1 - TAG_INT)" res)))))
      (else
       (let ((z::long (pragma::long "(~((long)$1 ^ (long)$2)) & 0x80000000" x y)))
	  (if (pragma::bool "$1 & (~((((long)$2 ^ (long)$1) + ((long)$3)) ^ ((long) $3)))" z x y)
	      (+fl (int32->flonum x) (int32->flonum y))
	      (+s32 x y))))))

;*---------------------------------------------------------------------*/
;*    js+fx32 ...                                                      */
;*    -------------------------------------------------------------    */
;*    Fixnum addition on 32 bits machines (two tagging bits).          */
;*    -------------------------------------------------------------    */
;*    See Hacker's Delight, second edition, page 29.                   */
;*    -------------------------------------------------------------    */
;*    This is the generic portable C implementation. Compilers and     */
;*    platforms that supporting inlined assembly, this definition is   */
;*    overriden the macro found in the include file arithmetic.sch     */
;*---------------------------------------------------------------------*/
(define-inline (js+fx32::obj x::long y::long)
   ;; requires x and y to be tagged
   (+s32/safe (fixnum->int32 x) (fixnum->int32 y)))

;*---------------------------------------------------------------------*/
;*    js+fx64 ...                                                      */
;*    -------------------------------------------------------------    */
;*    Fixnum addition on 64 bits machines (three tagging bits).        */
;*---------------------------------------------------------------------*/
(define-inline (js+fx64::obj x::long y::long)
   (let ((r::long (+fx x y)))
      (if (bit-and r (bit-lsh 1 52))
	  r
	  (fixnum->flonum r))))
   

