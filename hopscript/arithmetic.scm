;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/arithmetic.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec  4 07:42:21 2017                          */
;*    Last change :  Wed Jan 24 09:50:19 2018 (serrano)                */
;*    Copyright   :  2017-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JS arithmetic operations (see 32 and 64 implementations).        */
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
	   __hopscript_public
	   __hopscript_arithmetic32
	   __hopscript_arithmetic64)

   (from   __hopscript_arithmetic32 __hopscript_arithmetic64)

   (export (js-toflonum::double ::obj)
	   (+js::obj ::obj ::obj ::JsGlobalObject)
	   (-js::obj ::obj ::obj ::JsGlobalObject)
	   (*js::obj ::obj ::obj ::JsGlobalObject)
	   (/js::obj ::obj ::obj ::JsGlobalObject)
	   (negjs ::obj ::JsGlobalObject)

	   (inline /pow2s32::int32 x::int32 y::long)
	   (inline /pow2u32::uint32 x::uint32 y::long)
	   (inline /pow2fx::long n::long k::long)

	   (%$$NN ::obj ::obj)
	   (%$$NZ ::obj ::obj)

	   (bit-lshjs::obj ::obj ::obj ::JsGlobalObject)
	   (bit-rshjs::obj ::obj ::obj ::JsGlobalObject)
	   (bit-urshjs::obj ::obj ::obj ::JsGlobalObject)

	   (bit-andjs::obj ::obj ::obj ::JsGlobalObject)
	   (bit-orjs::obj ::obj ::obj ::JsGlobalObject)
	   (bit-xorjs::obj ::obj ::obj ::JsGlobalObject)
	   (bit-notjs::obj ::obj ::JsGlobalObject)

	   (<js::bool ::obj ::obj ::JsGlobalObject)
	   (>js::bool ::obj ::obj ::JsGlobalObject)
	   (<=js::bool ::obj ::obj ::JsGlobalObject)
	   (>=js::bool ::obj ::obj ::JsGlobalObject)
	   
	   (inline fixnums?::bool ::obj ::obj)
	   )
   
   (export (js-int53-toint32::int32 ::obj)
	   (js-int53-touint32::uint32 ::obj)
	   (inline js-uint32->jsnum::obj ::uint32)
	   )

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
;*    js-toflonum ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-toflonum r)
   (cond
      ((flonum? r) r)
      ((fixnum? r) (fixnum->flonum r))
      ((uint32? r) (uint32->flonum r))
      ((int32? r) (int32->flonum r))
      (else (error "js-toflonum" (format "Illegal number (~a)" (typeof r)) r))))

;*---------------------------------------------------------------------*/
;*    +js ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.6.1       */
;*---------------------------------------------------------------------*/
(define (+js x::obj y::obj %this)
   (let* ((nx (if (number? x) x (js-tonumber x %this)))
	  (ny (if (number? y) y (js-tonumber y %this))))
      (+/overflow nx ny)))
   
;*---------------------------------------------------------------------*/
;*    -js ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.6.2       */
;*---------------------------------------------------------------------*/
(define (-js x::obj y::obj %this)
   (let* ((nx (if (number? x) x (js-tonumber x %this)))
	  (ny (if (number? y) y (js-tonumber y %this))))
      (-/overflow nx ny)))
   
;*---------------------------------------------------------------------*/
;*    *js ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.5.1       */
;*---------------------------------------------------------------------*/
(define (*js x::obj y::obj %this)
   (let* ((nx (if (number? x) x (js-tonumber x %this)))
	  (ny (if (number? y) y (js-tonumber y %this))))
      (*/overflow nx ny)))

;*---------------------------------------------------------------------*/
;*    /js ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.5.2       */
;*---------------------------------------------------------------------*/
(define (/js x::obj y::obj %this)
   (let* ((nx (js-toflonum (js-tonumber x %this)))
	  (ny (js-toflonum (js-tonumber y %this))))
      (/fl nx ny)))

;*---------------------------------------------------------------------*/
;*    /pow2s32 ...                                                     */
;*    -------------------------------------------------------------    */
;*    See Hacker's Delight (second edition), H. Warren J.r,            */
;*    Chapter 10, section 10.1.                                        */
;*---------------------------------------------------------------------*/
(define-inline (/pow2s32::int32 n::int32 k::long)
   (/s32 n (bit-lsh 1 k)))

;*---------------------------------------------------------------------*/
;*    /pow2u32 ...                                                     */
;*    -------------------------------------------------------------    */
;*    See Hacker's Delight (second edition), H. Warren J.r,            */
;*    Chapter 10, section 10.1.                                        */
;*---------------------------------------------------------------------*/
(define-inline (/pow2u32::uint32 n::uint32 k::long)
   (/u32 n (bit-lsh 1 k)))

;*---------------------------------------------------------------------*/
;*    /pow2fx ...                                                      */
;*    -------------------------------------------------------------    */
;*    See Hacker's Delight (second edition), H. Warren J.r,            */
;*    Chapter 10, section 10.1.                                        */
;*---------------------------------------------------------------------*/
(define-inline (/pow2fx::long n::long k::long)
   (/fx n (bit-lsh 1 k)))
 
;*---------------------------------------------------------------------*/
;*    negjs ...                                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.7       */
;*---------------------------------------------------------------------*/
(define (negjs expr %this)
   (let loop ((expr expr))
      (cond
	 ((and (number? expr) (= expr 0))
	  (if (flonum? expr)
	      (if (=fx (signbitfl expr) 0) -0.0 +0.0)
	      -0.0))
	 ((number? expr)
	  (- expr))
	 (else
	  (loop (js-tonumber expr %this))))))


;*---------------------------------------------------------------------*/
;*    %$$NN ...                                                        */
;*---------------------------------------------------------------------*/
(define (%$$NN lnum rnum)
   (if (= rnum 0)
       +nan.0
       (%$$NZ lnum rnum)))

;*---------------------------------------------------------------------*/
;*    %$$NZ ...                                                        */
;*---------------------------------------------------------------------*/
(define (%$$NZ lnum rnum)
   (cond
      ((and (flonum? lnum) (or (=fl lnum +inf.0) (=fl lnum -inf.0)))
       +nan.0)
      ((and (flonum? rnum) (or (=fl rnum +inf.0) (=fl rnum -inf.0)))
       lnum)
      ((or (and (flonum? rnum) (nanfl? rnum))
           (and (flonum? lnum) (nanfl? lnum)))
       +nan.0)
      (else
       (let ((alnum (abs lnum))
             (arnum (abs rnum)))
          (if (or (flonum? alnum) (flonum? arnum))
              (begin
                 (unless (flonum? alnum)
                    (set! alnum (exact->inexact alnum)))
                 (unless (flonum? arnum)
                    (set! arnum (exact->inexact arnum)))
                 (let ((m (remainderfl alnum arnum)))
                    (if (or (< lnum 0)
                            (and (flonum? lnum) (=fl lnum 0.0)
                                 (not (=fx (signbitfl lnum) 0))))
                        (if (= m 0) -0.0 (- m))
                        (if (= m 0) +0.0 m))))
              (let ((m (remainder alnum arnum)))
                 (if (< lnum 0)
                     (if (= m 0) -0.0 (- m))
		     ;; MS: CARE 21 dec 2016, why returning a flonum?
                     ;; (if (= m 0) 0.0 m)
		     m)))))))

;*---------------------------------------------------------------------*/
;*    bit-lshjs ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.7.1       */
;*---------------------------------------------------------------------*/
(define (bit-lshjs x y %this)
   (let* ((lnum (js-toint32 x %this))
	  (rnum (js-touint32 y %this))
	  (shiftcount (bit-andu32 rnum #u32:31)))
      (js-int32-tointeger (bit-lshu32 lnum (uint32->fixnum shiftcount)))))

;*---------------------------------------------------------------------*/
;*    bit-rshjs ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.7.2       */
;*---------------------------------------------------------------------*/
(define (bit-rshjs x y %this)
   (let* ((lnum (js-toint32 x %this))
	  (rnum (js-touint32 y %this))
	  (shiftcount (bit-andu32 rnum #u32:31)))
      (js-int32-tointeger (bit-rshs32 lnum (uint32->fixnum shiftcount)))))

;*---------------------------------------------------------------------*/
;*    bit-urshjs ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.7.3       */
;*---------------------------------------------------------------------*/
(define (bit-urshjs x y %this)
   (let* ((lnum (js-touint32 x %this))
	  (rnum (js-touint32 y %this))
	  (shiftcount (bit-andu32 rnum #u32:31)))
      (js-uint32-tointeger (bit-urshu32 lnum (uint32->fixnum shiftcount)))))

;*---------------------------------------------------------------------*/
;*    bit-andjs ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.10        */
;*---------------------------------------------------------------------*/
(define (bit-andjs x y %this)
   (let* ((lnum (js-toint32 x %this))
	  (rnum (js-toint32 y %this)))
      (js-int32-tointeger (bit-ands32 lnum rnum))))

;*---------------------------------------------------------------------*/
;*    bit-orjs ...                                                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.10        */
;*---------------------------------------------------------------------*/
(define (bit-orjs x y %this)
   (let* ((lnum (js-toint32 x %this))
	  (rnum (js-toint32 y %this)))
      (js-int32-tointeger (bit-ors32 lnum rnum))))

;*---------------------------------------------------------------------*/
;*    bit-xorjs ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.10        */
;*---------------------------------------------------------------------*/
(define (bit-xorjs x y %this)
   (let* ((lnum (js-toint32 x %this))
	  (rnum (js-toint32 y %this)))
      (js-int32-tointeger (bit-xors32 lnum rnum))))

;*---------------------------------------------------------------------*/
;*    bit-notjs ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.8       */
;*---------------------------------------------------------------------*/
(define (bit-notjs expr %this)
   (let ((num (js-toint32 expr %this)))
      (js-int32-tointeger (bit-nots32 num))))

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
;*    js-uint32->jsnum ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (js-uint32->jsnum n::uint32)
   
   (define-macro (intszu32)
      (minfx (-fx (bigloo-config 'int-size) 1) 53))
   
   (define-macro (shiftu32)
      (bit-lsh 1 (intszu32)))
   
   (define-macro (maxintu32)
      (fixnum->uint32 (-fx (shiftu32) 1)))

   (if (>u32 n (maxintu32))
       (uint32->flonum n)
       (uint32->fixnum n)))
   
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
   
;*---------------------------------------------------------------------*/
;*    <js                                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.8.1       */
;*---------------------------------------------------------------------*/
(define (<js left right %this::JsGlobalObject)
   (cond
      ((flonum? left)
       (cond
	  ((flonum? right)
	   (<fl left right))
	  ((fixnum? right)
	   (<fl left (fixnum->flonum right)))
	  (else
	   (let* ((ny (js-tonumber (js-toprimitive right 'number %this) %this)))
	      (< left ny)))))
      ((fixnum? left)
       (cond
	  ((fixnum? right)
	   (<fx left right))
	  ((flonum? right)
	   (<fl (fixnum->flonum left) right))
	  (else
	   (let* ((ny (js-tonumber (js-toprimitive right 'number %this) %this)))
	      (< left ny)))))
      ((or (fixnum? right) (flonum? right))
       (let* ((nx (js-tonumber (js-toprimitive left 'number %this) %this)))
	  (< nx right)))
      (else
       (let* ((px (js-toprimitive left 'number %this))
	      (py (js-toprimitive right 'number %this)))
	  (if (and (js-jsstring? px) (js-jsstring? py))
	      (js-jsstring<? px py)
	      (let ((nx (js-tonumber px %this))
		    (ny (js-tonumber py %this)))
		 (< nx ny)))))))

;*---------------------------------------------------------------------*/
;*    >js                                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.8.2       */
;*---------------------------------------------------------------------*/
(define (>js left right %this::JsGlobalObject)
   (if (and (number? left) (number? right))
       (> left right)
       (let* ((px (js-toprimitive left 'number %this))
	      (py (js-toprimitive right 'number %this)))
	  (if (and (js-jsstring? px) (js-jsstring? py))
	      (js-jsstring>? px py)
	      (let ((nx (js-tonumber px %this))
		    (ny (js-tonumber py %this)))
		 (> nx ny))))))

;*---------------------------------------------------------------------*/
;*    <=js                                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.8.3       */
;*---------------------------------------------------------------------*/
(define (<=js left right %this::JsGlobalObject)
   (if (and (number? left) (number? right))
       (<= left right)
       (let* ((px (js-toprimitive left 'number %this))
	      (py (js-toprimitive right 'number %this)))
	  (if (and (js-jsstring? px) (js-jsstring? py))
	      (js-jsstring<=? px py)
	      (let ((nx (js-tonumber px %this))
		    (ny (js-tonumber py %this)))
		 (<= nx ny))))))

;*---------------------------------------------------------------------*/
;*    >=js                                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.8.4       */
;*---------------------------------------------------------------------*/
(define (>=js left right %this::JsGlobalObject)
   (if (and (number? left) (number? right))
       (>= left right)
       (let* ((px (js-toprimitive left 'number %this))
	      (py (js-toprimitive right 'number %this)))
	  (if (and (js-jsstring? px) (js-jsstring? py))
	      (js-jsstring>=? px py)
	      (let ((nx (js-tonumber px %this))
		    (ny (js-tonumber py %this)))
		 (>= nx ny))))))

;*---------------------------------------------------------------------*/
;*    fixnums? ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (fixnums? a b)
   (cond-expand
      (bigloo-c
       (pragma::bool "INTEGERP( (long)$1 & (long)$2 )" a b))
      (else
       (and (fixnum? a) (fixnum? b)))))
