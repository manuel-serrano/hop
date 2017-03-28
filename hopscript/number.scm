;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/number.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:41:39 2013                          */
;*    Last change :  Mon Mar 27 19:19:14 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript numbers                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_number
   
   (library hop)
   
   (include "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_string
	   __hopscript_error
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public)
   
   (export (js-init-number! ::JsGlobalObject)
	   (js-number->jsnumber ::obj ::JsGlobalObject)

	   (inline fixnums?::bool ::obj ::obj)
	   (inline js-uint32->jsnum::obj ::uint32)
	   
	   (js+ left right ::JsGlobalObject)
	   (inline js+fx::obj ::long ::long)
	   (inline js+fx32::obj ::obj ::obj)
	   (inline js+fx64::obj ::long ::long)
	   (inline js-fx::obj ::long ::long)
	   (inline js-fx32::obj ::obj ::obj)
	   (inline js-fx64::obj ::long ::long)
	   (inline js*fx::obj ::long ::long)
	   (js*fx32::obj ::obj ::obj)
	   (js*fx64::obj ::obj ::obj)
	   (inline js/fx::obj ::long ::long)
	   (js-slow+ left right ::JsGlobalObject)
	   (js- left right ::JsGlobalObject)
	   (js-neg expr ::JsGlobalObject)
	   (js* left right ::JsGlobalObject)
	   (js/ left right ::JsGlobalObject)
	   (js/num left right)
	   (js% left right ::JsGlobalObject)
	   (js-%$$NN left right)
	   (js-%$$NZ left right)
	   (inline js-%u32 ::uint32 ::uint32)
	   (js-bitlsh::obj left right ::JsGlobalObject)
	   (js-bitrsh::obj left right ::JsGlobalObject)
	   (js-bitursh::obj left right ::JsGlobalObject)
	   (js<::bool left right ::JsGlobalObject)
	   (js>::bool left right ::JsGlobalObject)
	   (js<=::bool left right ::JsGlobalObject)
	   (js>=::bool left right ::JsGlobalObject)
	   
	   (js-bitand left right ::JsGlobalObject)
	   (js-bitor left right ::JsGlobalObject)
	   (js-bitxor left right ::JsGlobalObject)
	   (js-bitnot expr ::JsGlobalObject)

	   (js-jsnumber-tostring ::obj ::obj ::JsGlobalObject)
	   (js-jsnumber-maybe-tostring ::obj ::obj ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-begin!)

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsNumber ...                                 */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsNumber
   (lambda (o)
      (with-access::JsNumber o (val) val))
   (lambda (o %this)
      (js-number->jsnumber o (or %this (js-initial-global-object)))))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsNumber ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsNumber worker::WorkerHopThread %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-number)
	 (let ((nobj (call-next-method)))
	    (with-access::JsNumber nobj (__proto__ val)
	       (with-access::JsNumber obj ((_val val))
		  (set! __proto__ (js-get js-number 'prototype %this))
		  (set! val (js-donate _val worker %_this))))
	    nobj))))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsNumber ...                                   */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsNumber op compile isexpr)
   (with-access::JsNumber o (val)
      (display "new Number(" op)
      (display (if (and (flonum? val) (nanfl? val)) "undefined" val) op)
      (display ")" op)))

;*---------------------------------------------------------------------*/
;*    js-init-number! ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.1       */
;*---------------------------------------------------------------------*/
(define (js-init-number! %this)
   (with-access::JsGlobalObject %this (__proto__ js-number js-function)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 (define js-number-prototype
	    (instantiate::JsNumber
	       (val 0)
	       (__proto__ __proto__)))

	 (define (js-number-constructor f value)
	    (instantiate::JsNumber
	       (__proto__ (js-get f 'prototype %this))
	       (val (if (eq? value (js-null)) 0 (js-tonumber value %this)))))
		
;* 	 (define (js-number-construct this::JsNumber . arg)            */
;* 	    (tprint "DEPRECATED, SHOULD NOT BE HERE...")               */
;* 	    (with-access::JsNumber this (val)                          */
;* 	       (set! val (if (pair? arg) (js-tonumber (car arg) %this) 0))) */
;* 	    this)                                                      */

	 (define (js-number-alloc constructor::JsFunction)
	    (instantiate::JsNumber
	       (__proto__ (js-get constructor 'prototype %this))))

	 ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.1
	 (define (%js-number this . arg)
	    (js-tonumber (if (pair? arg) (car arg) 0) %this))

	 (define (is-integer?::bbool this arg)
	    (cond
	       ((number? arg)
		(integer? arg))
	       ((isa? this JsNumber)
		(with-access::JsNumber this (val)
		   (is-integer? this val)))
	       (else
		#f)))

	 ;; Create a HopScript number object constructor
	 (set! js-number
	    (js-make-function %this %js-number 1 'Number
	       :__proto__ js-function-prototype
	       :prototype js-number-prototype
	       :constructor js-number-constructor
;* 	       :construct js-number-construct                         */
	       :alloc js-number-alloc))
	 
	 ;; other properties of the Number constructor
	 (js-bind! %this js-number 'POSITIVE_INFINITY
	    :value +inf.0
	    :writable #f
	    :enumerable #f
	    :configurable #f)
	 (js-bind! %this js-number 'NEGATIVE_INFINITY
	    :value -inf.0
	    :writable #f
	    :enumerable #f
	    :configurable #f)
	 (js-bind! %this js-number 'MAX_VALUE
	    :value (*fl 1.7976931348623157 (exptfl 10. 308.))
	    :writable #f
	    :enumerable #f
	    :configurable #f)
	 (js-bind! %this js-number 'MIN_VALUE
	    :value 5e-324
	    :writable #f
	    :enumerable #f
	    :configurable #f)
	 (js-bind! %this js-number 'NaN
	    :value +nan.0
	    :writable #f
	    :enumerable #f
	    :configurable #f)
	 (js-bind! %this js-number 'isInteger
	    :value (js-make-function %this is-integer? 1 'isInteger)
	    :writable #f :configurable #f :enumerable #f)
	 ;; bind the builtin prototype properties
	 (init-builtin-number-prototype! %this js-number js-number-prototype)
	 ;; bind Number in the global object
	 (js-bind! %this %this 'Number
	    :configurable #f :enumerable #f :value js-number)
	 js-number)))

;*---------------------------------------------------------------------*/
;*    js-valueof ::JsNumber ...                                        */
;*---------------------------------------------------------------------*/
(define-method (js-valueof this::JsNumber %this::JsGlobalObject)
   (js-tonumber this %this))

;*---------------------------------------------------------------------*/
;*    js-tonumber ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-method (js-tonumber this::JsNumber %this::JsGlobalObject)
   (with-access::JsNumber this (val)
      val))

;*---------------------------------------------------------------------*/
;*    js-tointeger ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-method (js-tointeger this::JsNumber %this::JsGlobalObject)
   (with-access::JsNumber this (val)
      (cond
	 ((fixnum? val)
	  val)
	 ((integer? val)
	  (if (inexact? val)
	      (inexact->exact val)
	      val))
	 ((flonum? val)
	  (inexact->exact (floor val)))
	 (else
	  val))))

;*---------------------------------------------------------------------*/
;*    init-builtin-number-prototype! ...                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4       */
;*---------------------------------------------------------------------*/
(define (init-builtin-number-prototype! %this::JsGlobalObject js-number obj)
   
   ;; constructor
   (js-bind! %this obj 'constructor
      :value js-number
      :writable #t
      :configurable #t
      :enumerable #f)

   (define (js-cast-number this shape)
      (cond
	 ((number? this) this)
	 ((isa? this JsNumber) (with-access::JsNumber this (val) val))
	 (else (js-raise-type-error %this "Not a number ~a"
		  (if shape (shape this) this)))))
   
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.2
   (define (js-number-tostring this #!optional (radix (js-undefined)))
      (js-jsnumber-tostring (js-cast-number this typeof) radix %this))

   (js-bind! %this obj 'toString
      :value (js-make-function %this js-number-tostring 2 'toString)
      :writable #t
      :configurable #t
      :enumerable #f)

   ;; toLocaleString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.3
   (define (js-number-tolocalestring this #!optional (radix (js-undefined)))
      (js-number-tostring this radix))

   (js-bind! %this obj 'toLocaleString
      :value (js-make-function %this js-number-tolocalestring 2 'toLocaleString)
      :writable #t
      :configurable #t
      :enumerable #f)

   ;; valueOf
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.4
   (define (js-number-valueof this)
      (js-cast-number this #f))

   (js-bind! %this obj 'valueOf
      :value (js-make-function %this js-number-valueof 0 'valueOf)
      :writable #t
      :configurable #t
      :enumerable #f)

   ;; toFixed
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.5
   (define (js-number-tofixed this fractiondigits)
      
      (define (signed val s)
	 (if (>= val 0)
	     (js-string->jsstring s)
	     (js-string->jsstring (string-append "-" s))))
      
      (let ((val (js-cast-number this #f)))
	 (let ((f (if (eq? fractiondigits (js-undefined))
		      0
		      (js-tointeger fractiondigits %this))))
	    (if (or (< f 0) (> f 20))
		(js-raise-range-error %this
		   "Fraction digits out of range: ~a" f)
		(if (and (flonum? val) (nanfl? val))
		    (js-string->jsstring "NaN")
		    (let ((x (abs val))
			  (f (->fixnum f)))
		       (if (>= x (exptfl 10. 21.))
			   (signed val (js-tostring x %this))
			   (let ((n (round x)))
			      (cond
				 ((= n 0)
				  (signed val
				     (if (= f 0)
					 "0"
					 (let* ((d (- x n))
						(m (inexact->exact (round (* d (expt 10 f)))))
						(s (integer->string m))
						(l (string-length s)))
					    (cond
					       ((>fx l f)
						(string-append "0." (substring s 0 f)))
					       ((=fx l f)
						(string-append "0." s))
					       (else
						(string-append "0." (make-string (-fx f l) #\0) s)))))))
				 ((= f 0)
				  (signed val (number->string n)))
				 (else
				  (let* ((m (inexact->exact
					       (round (* x (expt 10 f)))))
					 (s (integer->string m))
					 (l (string-length s)))
				     (signed val
					(string-append (substring s 0 (-fx l f))
					   "."
					   (substring s (-fx l f)))))))))))))))

   (js-bind! %this obj 'toFixed
      :value (js-make-function %this js-number-tofixed 1 'toFixed)
      :writable #t
      :configurable #t
      :enumerable #f)

   ;; toExponential
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.6
   (js-bind! %this obj 'toExponential
      :value (js-make-function %this
		(lambda (this val)
		   (error "toExponential" "not implemented" "yet"))
		1 'toExponential)
      :writable #t
      :configurable #t
      :enumerable #f)

   ;; toPrecision
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.7
   (js-bind! %this obj 'toPrecision
      :value (js-make-function %this
		(lambda (this val)
		   (error "toPrecision" "not implemented" "yet"))
		1 'toPrecision)
      :writable #t
      :configurable #t
      :enumerable #f))

;*---------------------------------------------------------------------*/
;*    js-number->jsnumber ...                                          */
;*---------------------------------------------------------------------*/
(define (js-number->jsnumber val %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-number)
      (js-new1 %this js-number val)))

;*---------------------------------------------------------------------*/
;*    fixnums? ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (fixnums? x y)
   (cond-expand
      ((and bigloo-c (or bint29 bint30 bint32))
       (pragma::bool "(((long)($1) & TAG_MASK) + ((long)($2) & TAG_MASK)) == (TAG_INT<<1)" x y))
      (bigloo-c
       (pragma::bool "INTEGERP( $1 ) && INTEGERP( $2 )" x y))
      (else (and (fixnum? x) (fixnum? y)))))

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
;*    js+ ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.6.1       */
;*---------------------------------------------------------------------*/
(define (js+ left right %this::JsGlobalObject)
   (cond
      ((and (fixnum? left) (fixnum? right))
       (let ((r (+fx left right)))
	  (if (or (>=fx r (bit-lsh 1 29)) (<fx r (negfx (bit-lsh 1 29))))
	      (fixnum->flonum r)
	      r)))
      ((and (number? left) (number? right))
       (+ left right))
      (else
       (js-slow+ left right %this))))

;*---------------------------------------------------------------------*/
;*    js+fx ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (js+fx::obj left::long right::long)

   (define-macro (intsz+)
      (minfx (-fx (bigloo-config 'int-size) 1) 53))
   
   (define-macro (shift+)
      (bit-lsh 1 (intsz+)))
   
   (define-macro (maxint+)
      (-fx (shift+) 1))
   
   (define-macro (minint+)
      (-fx 0 (shift+)))
   
   (let ((tmp (+fx left right)))
      (if (or (>fx tmp (maxint+)) (<fx tmp (minint+)))
	  (fixnum->flonum tmp)
	  tmp)))

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
(define-inline (js+fx32::obj x::obj y::obj)
   ;; requires x and y to be tagged
   (let ((z::long (pragma::long "(~((long)$1 ^ (long)$2)) & 0x80000000" x y)))
      (if (pragma::bool "$1 & (~((((long)$2 ^ (long)$1) + ((long)$3)) ^ ((long) $3)))" z x y)
          (fixnum->flonum (+fx x y))
          (pragma::obj "(obj_t)((long)$1 + (long)$2 - TAG_INT)" x y))))

;*---------------------------------------------------------------------*/
;*    js+fx64 ...                                                      */
;*    -------------------------------------------------------------    */
;*    Fixnum addition on 64 bits machines (three tagging bits).        */
;*---------------------------------------------------------------------*/
(define-inline (js+fx64 x::long y::long)
   (let ((r::long (+fx x y)))
      (if (bit-and r (bit-lsh 1 52))
	  r
	  (fixnum->flonum r))))
   
;*---------------------------------------------------------------------*/
;*    js-fx32 ...                                                      */
;*    -------------------------------------------------------------    */
;*    Fixnum substraction on 32 bits machines (two tagging bits).      */
;*    -------------------------------------------------------------    */
;*    See Hacker's Delight, second edition, page 29.                   */
;*---------------------------------------------------------------------*/
(define-inline (js-fx32::obj x::obj y::obj)
   ;; requires x and y to be tagged
   (let ((z::long (pragma::long "((long)$1 ^ (long)$2) & 0x80000000" x y)))
      (if (pragma::bool "$1 & ((((long)$2 ^ (long)$1) - ((long)$3)) ^ ((long) $3))" z x y)
	  (fixnum->flonum (-fx x y))
	  (pragma::obj "(obj_t)(((long)$1 - (long)$2) + TAG_INT)" x y))))

;*---------------------------------------------------------------------*/
;*    js-fx64 ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (js-fx64 x::long y::long)
   (let ((r::long (-fx x y)))
      (if (bit-and r (bit-lsh 1 52))
	  r
	  (fixnum->flonum r))))
   
;*---------------------------------------------------------------------*/
;*    js*fx ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (js*fx::obj left::long right::long)
   (js*fx32 left right))

;*---------------------------------------------------------------------*/
;*    js*fx32 ...                                                      */
;*---------------------------------------------------------------------*/
(define (js*fx32 x::obj y::obj)
   
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
		  -0.0
		  r)
	      r))
	 ((bignum? r)
	  (bignum->flonum r))
	 (else
	  r))))

;*---------------------------------------------------------------------*/
;*    js*fx64 ...                                                      */
;*---------------------------------------------------------------------*/
(define (js*fx64 x::obj y::obj)
   
   (define (neg? o)
      (if (flonum? o)
	  (not (=fx (signbitfl o) 0))
	  (<fx o 0)))
   
   (let ((r (* x y)))
      (cond
	 ((fixnum? r)
	  (cond-expand
	     ((or bint30 bint32)
	      (cond
		 ((=fx r 0)
		  (if (or (and (neg? x) (not (neg? y)))
			  (and (not (neg? x)) (neg? y)))
		      -0.0
		      r))
		 (else
		  r)))
	     (else
	      (cond
		 ((=fx r 0)
		  (if (or (and (neg? x) (not (neg? y)))
			  (and (not (neg? x)) (neg? y)))
		      -0.0
		      r))
		 ((>fx r (bit-lsh 1 53))
		  (fixnum->flonum r))
		 ((<fx r (negfx (bit-lsh 1 53)))
		  (fixnum->flonum r))
		 (else
		  r)))))
	 ((bignum? r)
	  (bignum->flonum r))
	 (else
	  r))))

;*---------------------------------------------------------------------*/
;*    js-fx ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (js-fx left::long right::long)
   
   (define-macro (intsz-)
      (minfx (-fx (bigloo-config 'int-size) 1) 53))
   
   (define-macro (shift-)
      (bit-lsh 1 (intsz-)))
   
   (define-macro (maxint-)
      (-fx (bit-lsh 1 (intsz-)) 1))
   
   (define-macro (minint-)
      (-fx 0 (bit-lsh 1 (intsz-))))
   
   (let ((tmp (-fx left right)))
      (if (or (>fx tmp (maxint-)) (<fx tmp (minint-)))
	  (fixnum->flonum tmp)
	  tmp)))

;*---------------------------------------------------------------------*/
;*    js/fx ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (js/fx left::long right::long)
   (if (=fx right 0)
       (js/num left right)
       (/fx left right)))

;*---------------------------------------------------------------------*/
;*    js-slow+ ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-slow+ left right %this)
   (let* ((left (js-toprimitive left 'any %this))
	  (right (js-toprimitive right 'any %this)))
      (cond
	 ((js-jsstring? left)
	  (js-jsstring-append left (js-tojsstring right %this)))
	 ((js-jsstring? right)
	  (js-jsstring-append (js-tojsstring left %this) right))
	 (else
	  (let* ((left (js-tonumber left %this))
		 (right (js-tonumber right %this)))
	     (if (or (not (= left left)) (not (= right right)))
		 +nan.0
		 (+ left right)))))))

;*---------------------------------------------------------------------*/
;*    js- ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.6.1       */
;*---------------------------------------------------------------------*/
(define (js- left right %this)
   (if (and (number? left) (number? right))
       (- left right)
       (let* ((lnum (js-tonumber left %this))
	      (rnum (js-tonumber right %this)))
	  (- lnum rnum))))

;*---------------------------------------------------------------------*/
;*    js-neg ...                                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.7       */
;*---------------------------------------------------------------------*/
(define (js-neg expr %this)
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
;*    js* ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.5.1       */
;*---------------------------------------------------------------------*/
(define (js* left right %this)
   
   (define (neg? o)
      (if (flonum? o)
	  (not (=fx (signbitfl o) 0))
	  (<fx o 0)))
   
   (let* ((lnum (if (number? left) left (js-tonumber left %this)))
	  (rnum (if (number? right) right (js-tonumber right %this)))
	  (r (* lnum rnum)))
      (cond
	 ((fixnum? r)
	  (if (=fx r 0)
	      (if (or (and (neg? lnum) (not (neg? rnum)))
		      (and (not (neg? lnum)) (neg? rnum)))
		  -0.0
		  r)
	      r))
	 ((bignum? r)
	  (bignum->flonum r))
	 (else
	  r))))

;*---------------------------------------------------------------------*/
;*    js/ ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.5.2       */
;*---------------------------------------------------------------------*/
(define (js/ left right %this)
   (let* ((lnum (js-tonumber left %this))
	  (rnum (js-tonumber right %this)))
      (js/num lnum rnum)))

;*---------------------------------------------------------------------*/
;*    js/num ...                                                       */
;*---------------------------------------------------------------------*/
(define (js/num lnum rnum)
   (if (= rnum 0)
       (if (flonum? rnum)
	   (cond
	      ((and (flonum? lnum) (nanfl? lnum))
	       lnum)
	      ((=fx (signbitfl rnum) 0)
	       (cond
		  ((> lnum 0) +inf.0)
		  ((< lnum 0) -inf.0)
		  (else +nan.0)))
	      (else
	       (cond
		  ((< lnum 0) +inf.0)
		  ((> lnum 0) -inf.0)
		  (else +nan.0))))
	   (cond
	      ((and (flonum? lnum) (nanfl? lnum)) lnum)
	      ((> lnum 0) +inf.0)
	      ((< lnum 0) -inf.0)
	      (else +nan.0)))
       (/ lnum rnum)))

;*---------------------------------------------------------------------*/
;*    js% ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.5.3       */
;*---------------------------------------------------------------------*/
(define (js% left right %this)
   (let* ((lnum (js-tonumber left %this))
	  (rnum (js-tonumber right %this)))
      (js-%$$NN lnum rnum)))

;*---------------------------------------------------------------------*/
;*    js-%$$NN ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-%$$NN lnum rnum)
   (if (= rnum 0)
       +nan.0
       (js-%$$NZ lnum rnum)))

;*---------------------------------------------------------------------*/
;*    js-%$$NZ ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-%$$NZ lnum rnum)
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
;*    js-%u32 ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (js-%u32 lnum rnum)
   (if (=u32 rnum #u32:0)
       +nan.0
       (remainderu32 lnum rnum)))

;*---------------------------------------------------------------------*/
;*    js-bitlsh ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.7.1       */
;*---------------------------------------------------------------------*/
(define (js-bitlsh left right %this)
   (let* ((lnum (js-toint32 left %this))
	  (rnum (js-touint32 right %this))
	  (shiftcount (bit-andu32 rnum #u32:31)))
      (int32->integer (bit-lshu32 lnum (uint32->fixnum shiftcount)))))

;*---------------------------------------------------------------------*/
;*    js-bitrsh ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.7.2       */
;*---------------------------------------------------------------------*/
(define (js-bitrsh left right %this)
   (let* ((lnum (js-toint32 left %this))
	  (rnum (js-touint32 right %this))
	  (shiftcount (bit-andu32 rnum #u32:31)))
      (int32->integer (bit-rshs32 lnum (uint32->fixnum shiftcount)))))

;*---------------------------------------------------------------------*/
;*    js-bitursh ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.7.3       */
;*---------------------------------------------------------------------*/
(define (js-bitursh left right %this)
   (let* ((lnum (js-touint32 left %this))
	  (rnum (js-touint32 right %this))
	  (shiftcount (bit-andu32 rnum #u32:31)))
      (uint32->integer (bit-urshu32 lnum (uint32->fixnum shiftcount)))))

;*---------------------------------------------------------------------*/
;*    js<                                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.8.1       */
;*---------------------------------------------------------------------*/
(define (js< left right %this::JsGlobalObject)
   (if (and (number? left) (number? right))
       (< left right)
       (let* ((px (js-toprimitive left 'number %this))
	      (py (js-toprimitive right 'number %this)))
	  (if (and (js-jsstring? px) (js-jsstring? py))
	      (js-jsstring<? px py)
	      (let ((nx (js-tonumber px %this))
		    (ny (js-tonumber py %this)))
		 (< nx ny))))))

;*---------------------------------------------------------------------*/
;*    js>                                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.8.2       */
;*---------------------------------------------------------------------*/
(define (js> left right %this::JsGlobalObject)
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
;*    js<=                                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.8.3       */
;*---------------------------------------------------------------------*/
(define (js<= left right %this::JsGlobalObject)
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
;*    js>=                                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.8.4       */
;*---------------------------------------------------------------------*/
(define (js>= left right %this::JsGlobalObject)
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
;*    js-bitand ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.10        */
;*---------------------------------------------------------------------*/
(define (js-bitand left right %this)
   (let* ((lnum (int32->elong (js-toint32 left %this)))
	  (rnum (int32->elong (js-toint32 right %this))))
      (int32->integer (elong->int32 (bit-andelong lnum rnum)))))

;*---------------------------------------------------------------------*/
;*    js-bitor ...                                                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.10        */
;*---------------------------------------------------------------------*/
(define (js-bitor left right %this)
   (let* ((lnum (int32->elong (js-toint32 left %this)))
	  (rnum (int32->elong (js-toint32 right %this))))
      (int32->integer (elong->int32 (bit-orelong lnum rnum)))))

;*---------------------------------------------------------------------*/
;*    js-bitxor ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.10        */
;*---------------------------------------------------------------------*/
(define (js-bitxor left right %this)
   (let* ((lnum (int32->elong (js-toint32 left %this)))
	  (rnum (int32->elong (js-toint32 right %this))))
      (int32->integer (elong->int32 (bit-xorelong lnum rnum)))))

;*---------------------------------------------------------------------*/
;*    js-bitnot ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.8       */
;*---------------------------------------------------------------------*/
(define (js-bitnot expr %this)
   (let ((num (int32->elong (js-toint32 expr %this))))
      (int32->integer (elong->int32 (bit-notelong num)))))

;*---------------------------------------------------------------------*/
;*    js-jsnumber-tostring ...                                         */
;*---------------------------------------------------------------------*/
(define (js-jsnumber-tostring val radix %this)
   (let ((r (if (eq? radix (js-undefined))
		10
		(js-tointeger radix %this))))
      (cond
	 ((or (< r 2) (> r 36))
	  (js-raise-range-error %this "Radix out of range: ~a" r))
	 ((and (flonum? val) (nanfl? val))
	  (js-string->jsstring "NaN"))
	 ((= val +inf.0)
	  (js-string->jsstring "Infinity"))
	 ((= val -inf.0)
	  (js-string->jsstring "-Infinity"))
	 ((or (= r 10) (= r 0))
	  (js-tojsstring val %this))
	 (else
	  (js-string->jsstring (number->string val r))))))

;*---------------------------------------------------------------------*/
;*    js-jsnumber-maybe-tostring ...                                   */
;*---------------------------------------------------------------------*/
(define (js-jsnumber-maybe-tostring this radix %this)
   (let loop ((this this))
      (cond
	 ((number? this)
	  (js-jsnumber-tostring this radix %this))
	 ((isa? this JsObject)
	  (js-call1 %this (js-get this 'toString %this) this radix))
	 (else
	  (loop (js-toobject %this this))))))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral end                                              */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-end!)
