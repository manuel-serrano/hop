;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/arithmetic.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec  4 07:42:21 2017                          */
;*    Last change :  Sun Jun 27 18:06:25 2021 (serrano)                */
;*    Copyright   :  2017-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JS arithmetic operations (see 32 and 64 implementations).        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_arithmetic

   (library hop)

   (include "types.sch" "stringliteral.sch" "names.sch")

   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_string
	   __hopscript_error
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_lib
	   __hopscript_arithmetic32
	   __hopscript_arithmetic64)

   (from   __hopscript_arithmetic32 __hopscript_arithmetic64)

   (extern (macro $real-set!::real (::real ::double) "BGL_REAL_SET"))
   
   (export (inline js-toflonum::double ::obj)
	   (+js::obj ::obj ::obj ::JsGlobalObject)
	   (-js::obj ::obj ::obj ::JsGlobalObject)
	   (*js::obj ::obj ::obj ::JsGlobalObject)
	   (**js::obj ::obj ::obj ::JsGlobalObject)
	   (/js::obj ::obj ::obj ::JsGlobalObject)
	   (negjs ::obj)

	   (inline +l!fl::real ::real ::double)
	   (inline +r!fl::real ::double ::real)
	   (inline -l!fl::real ::real ::double)
	   (inline -r!fl::real ::double ::real)
	   (inline *l!fl::real ::real ::double)
	   (inline *r!fl::real ::double ::real)
	   (inline /l!fl::real ::real ::double)
	   (inline /r!fl::real ::double ::real)
	   
	   (inline /pow2s32::int32 x::int32 y::long)
	   (inline /pow2u32::uint32 x::uint32 y::long)
	   (inline /pow2fx::long n::long k::long)
	   (inline /integer::obj ::double ::double)
	   (inline jsintegerfl?::bool ::double)
	   
	   (inline %$$II ::long ::long)
	   (%$$NN ::obj ::obj)
	   (%$$NZ ::obj ::obj)
	   (%$$FF::double ::double ::double)
	   (%$$NF::double ::obj ::double)
	   (%$$FN::double ::double ::obj)
	   (%$$__ x y ::JsGlobalObject)
	   
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
	   
	   (inline >>=js::bool ::obj ::obj ::JsGlobalObject)
	   (inline <<=js::bool ::obj ::obj ::JsGlobalObject)
	   
	   (>>=::bool ::obj ::obj)
	   (<<=::bool ::obj ::obj)
	   
	   (>>=fl::bool ::double ::double)
	   (<<=fl::bool ::double ::double)
	   
	   ))

;*---------------------------------------------------------------------*/
;*    __js_strings ...                                                 */
;*---------------------------------------------------------------------*/
(define __js_strings #f)

;*---------------------------------------------------------------------*/
;*    js-toflonum ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (js-toflonum r)
   (if (flonum? r) r (fixnum->flonum r)))

;*---------------------------------------------------------------------*/
;*    +js ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.6.1       */
;*---------------------------------------------------------------------*/
(define (+js x::obj y::obj %this)
   (cond 
      ((and (js-number? x) (js-number? y))
       (+/overflow x y))
      ((bignum? x)
       (if (bignum? y)
	   (+bx x y)
	   (js-raise-type-error %this "Cannot mix BigInt and other types, use explicit converspions"
	      y)))
      ((bignum? y)
       (js-raise-type-error %this "Cannot mix BigInt and other types, use explicit converspions"
	  x))
      (else
       (let* ((nx (js-tonumber x %this))
	      (ny (js-tonumber y %this)))
	  (+/overflow nx ny)))))
   
;*---------------------------------------------------------------------*/
;*    -js ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.6.2       */
;*---------------------------------------------------------------------*/
(define (-js x::obj y::obj %this)
   (cond 
      ((and (js-number? x) (js-number? y))
       (-/overflow x y))
      ((bignum? x)
       (if (bignum? y)
	   (-bx x y)
	   (js-raise-type-error %this "Cannot mix BigInt and other types, use explicit converspions"
	      y)))
      ((bignum? y)
       (js-raise-type-error %this "Cannot mix BigInt and other types, use explicit converspions"
	  x))
      (else
       (let* ((nx (js-tonumber x %this))
	      (ny (js-tonumber y %this)))
	  (-/overflow nx ny)))))
   
;*---------------------------------------------------------------------*/
;*    *js ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.5.1       */
;*---------------------------------------------------------------------*/
(define (*js x::obj y::obj %this)
   (cond
      ((and (js-number? x) (js-number? y))
       (*/overflow x y))
      ((bignum? x)
       (if (bignum? y)
	   (*bx x y)
	   (js-raise-type-error %this "Cannot mix BigInt and other types, use explicit converspions"
	      y)))
      ((bignum? y)
       (js-raise-type-error %this "Cannot mix BigInt and other types, use explicit converspions"
	  x))
      (else
       (let* ((nx (js-tonumber x %this))
	      (ny (js-tonumber y %this)))
	  (*/overflow nx ny)))))

;*---------------------------------------------------------------------*/
;*    **js ...                                                         */
;*    -------------------------------------------------------------    */
;*    https://www.ecma-international.org/ecma-262/8.0/                 */
;*       #sec-applying-the-exp-operator                                */
;*---------------------------------------------------------------------*/
(define (**js x::obj y::obj %this)
   (cond
      ((and (js-number? x) (js-number? y))
       (exptfl (js-toflonum x) (js-toflonum y)))
      ((bignum? x)
       (if (bignum? y)
	   (exptbx x y)
	   (js-raise-type-error %this "Cannot mix BigInt and other types, use explicit converspions"
	      y)))
      ((bignum? y)
       (js-raise-type-error %this "Cannot mix BigInt and other types, use explicit converspions"
	  x))
      (else
       (let* ((nx (js-tonumber x %this))
	      (ny (js-tonumber y %this)))
	  (exptfl (js-toflonum nx) (js-toflonum ny))))))

;*---------------------------------------------------------------------*/
;*    /js ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.5.2       */
;*---------------------------------------------------------------------*/
(define (/js x::obj y::obj %this)
   (cond
      ((and (flonum? x) (flonum? y))
       (/fl x y))
      ((bignum? x)
       (if (bignum? y)
	   (/bx x y)
	   (js-raise-type-error %this "Cannot mix BigInt and other types, use explicit converspions"
	      y)))
      (else
       (/fl (toflonum x %this) (toflonum y %this)))))

;*---------------------------------------------------------------------*/
;*    toflonum ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (toflonum::double x %this)
   (cond
      ((flonum? x) x)
      ((fixnum? x) (fixnum->flonum x))
      (else (js-toflonum (js-tonumber x %this)))))

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
;*    jsintegerfl? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (jsintegerfl? n::double)
   (and (cond-expand
	   (bigloo-c
	    (let ((intpart::double 0.0))
	       (=fl ($modf n (pragma::void* "&($1)" (pragma intpart))) 0.0)))
	   (else
	    (integerfl? n)))
	(<=fl n 9007199254740992.0)
	(>=fl n -9007199254740992.0)))
   
;*---------------------------------------------------------------------*/
;*    /integer ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (/integer x::double y::double)
   (let ((v (/fl x y)))
      (if (jsintegerfl? v)
	  (flonum->fixnum v)
	  v)))

;*---------------------------------------------------------------------*/
;*    negjs ...                                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.7       */
;*---------------------------------------------------------------------*/
(define (negjs expr)
   (if (fixnum? expr)
       (if (=fx expr 0)
	   -0.0
	   (negfx expr))
       (if (=fl expr 0.0)
	   (if (=fx (signbitfl expr) 0) -0.0 +0.0)
	   (negfl expr))))

;*---------------------------------------------------------------------*/
;*    oplr!fl ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (+l!fl::real x::real y::double) ($real-set! x (+fl x y)))
(define-inline (+r!fl::real x::double y::real) ($real-set! y (+fl x y)))

(define-inline (-l!fl::real x::real y::double) ($real-set! x (-fl x y)))
(define-inline (-r!fl::real x::double y::real) ($real-set! y (-fl x y)))

(define-inline (*l!fl::real x::real y::double) ($real-set! x (*fl x y)))
(define-inline (*r!fl::real x::double y::real) ($real-set! y (*fl x y)))

(define-inline (/l!fl::real x::real y::double) ($real-set! x (/fl x y)))
(define-inline (/r!fl::real x::double y::real) ($real-set! y (/fl x y)))

;*---------------------------------------------------------------------*/
;*    %$$II ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (%$$II lnum rnum)
   (if (=fx rnum 0)
       +nan.0
       (if (>=fx lnum 0)
	   (remainderfx lnum rnum)
	   (%$$NZ lnum rnum))))

;*---------------------------------------------------------------------*/
;*    %$$NN ...                                                        */
;*---------------------------------------------------------------------*/
(define (%$$NN lnum rnum)
   (cond
      ((and (fixnums? lnum rnum) (>=fx lnum 0) (not (=fx rnum 0)))
       (remainderfx lnum rnum))
      ((= rnum 0)
       +nan.0)
      (else
       (%$$NZ lnum rnum))))

;*---------------------------------------------------------------------*/
;*    %$$__ ...                                                        */
;*---------------------------------------------------------------------*/
(define (%$$__ x y %this)
   (cond
      ((and (fixnums? x y) (>=fx x 0) (not (=fx y 0)))
       (remainderfx x y))
      ((bignum? x)
       (if (bignum? y)
	   (remainderbx x y)
	   (js-raise-type-error %this "Cannot mix BigInt and other types, use explicit conversions"
	      y)))
      ((bignum? y)
       (js-raise-type-error %this "Cannot mix BigInt and other types, use explicit conversions"
	  x))
      (else
       (let ((lnum (js-tonumber x %this))
	     (rnum (js-tonumber y %this)))
	  (cond
	     ((and (fixnums? lnum rnum) (>=fx lnum 0) (not (=fx rnum 0)))
	      (remainderfx lnum rnum))
	     ((= rnum 0)
	      +nan.0)
	     (else
	      (%$$NZ lnum rnum)))))))

;*---------------------------------------------------------------------*/
;*    %$$NZ ...                                                        */
;*---------------------------------------------------------------------*/
(define (%$$NZ lnum rnum)
   (cond
      ((and (fixnums? lnum rnum) (>=fx lnum 0) (not (=fx rnum 0)))
       (remainderfx lnum rnum))
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
;*    %$$FF ...                                                        */
;*---------------------------------------------------------------------*/
(define (%$$FF lnum rnum)
   (if (or (=fl rnum 0.0) (nanfl? rnum))
       +nan.0
       (let* ((alnum (absfl lnum))
	      (arnum (absfl rnum))
	      (m (remainderfl alnum arnum)))
	  (if (or (<fl lnum 0.0)
		  (and (=fl lnum 0.0)
		       (not (=fx (signbitfl lnum) 0))))
	      (if (=fl m 0.0) -0.0 (negfl m))
	      (if (=fl m 0.0) +0.0 m)))))

;*---------------------------------------------------------------------*/
;*    %$$FN ...                                                        */
;*---------------------------------------------------------------------*/
(define (%$$FN lnum rnum)
   (if (flonum? rnum)
       (%$$FF lnum rnum)
       (%$$FF lnum (fixnum->flonum rnum))))

;*---------------------------------------------------------------------*/
;*    %$$NF ...                                                        */
;*---------------------------------------------------------------------*/
(define (%$$NF lnum rnum)
   (if (flonum? lnum)
       (%$$FF lnum rnum)
       (%$$FF (fixnum->flonum lnum) rnum)))

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
   (if (and (js-number? left) (js-number? right))
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
   (if (and (js-number? left) (js-number? right))
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
   (if (and (js-number? left) (js-number? right))
       (>= left right)
       (let* ((px (js-toprimitive left 'number %this))
	      (py (js-toprimitive right 'number %this)))
	  (if (and (js-jsstring? px) (js-jsstring? py))
	      (js-jsstring>=? px py)
	      (let ((nx (js-tonumber px %this))
		    (ny (js-tonumber py %this)))
		 (>= nx ny))))))

;*---------------------------------------------------------------------*/
;*    >>=js ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (>>=js left right %this::JsGlobalObject)
   (>>= (js-tonumber left %this) (js-tonumber right %this)))

;*---------------------------------------------------------------------*/
;*    <<=js ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (<<=js left right %this::JsGlobalObject)
   (<<= (js-tonumber left %this) (js-tonumber right %this)))

;*---------------------------------------------------------------------*/
;*    >>= ...                                                          */
;*---------------------------------------------------------------------*/
(define (>>= left right)
   (cond
      ((not (= left left)) #t)
      ((not (= right right)) #f)
      (else (>= left right))))

;*---------------------------------------------------------------------*/
;*    <<= ...                                                          */
;*---------------------------------------------------------------------*/
(define (<<= left right)
   (cond
      ((not (= left left)) #t)
      ((not (= right right)) #f)
      (else (<= left right))))

;*---------------------------------------------------------------------*/
;*    >>=fl ...                                                        */
;*---------------------------------------------------------------------*/
(define (>>=fl left right)
   (cond
      ((not (=fl left left)) #t)
      ((not (=fl right right)) #f)
      (else (>=fl left right))))

;*---------------------------------------------------------------------*/
;*    <<=fl ...                                                        */
;*---------------------------------------------------------------------*/
(define (<<=fl left right)
   (cond
      ((not (=fl left left)) #t)
      ((not (=fl right right)) #f)
      (else (<=fl left right))))

