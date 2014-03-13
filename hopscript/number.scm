;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/number.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:41:39 2013                          */
;*    Last change :  Wed Mar 12 12:47:53 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript numbers                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_number

   (library hop)
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_string
	   __hopscript_error
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public)

   (export js-number
	   js-number-prototype
	   (js-init-number! ::JsObject)
	   
	   (js+ left right)
	   (js-slow+ left right)
	   (js- left right)
	   (js-neg expr)
	   (js* left right)
	   (js/ left right)
	   (js% left right)
	   (js-bitlsh::obj left right)
	   (js-bitrsh::obj left right)
	   (js-bitursh::obj left right)
	   (js<::bool left right)
	   (js>::bool left right)
	   (js<=::bool left right)
	   (js>=::bool left right)
	   (js-bitand left right)
	   (js-bitor left right)
	   (js-bitxor left right)
	   (js-bitnot expr)))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsNumber ...                                   */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsNumber op compile isexpr)
   (with-access::JsNumber o (val)
      (display "new NUmber(" op)
      (display val op)
      (display ")" op)))

;*---------------------------------------------------------------------*/
;*    js-number ...                                                    */
;*---------------------------------------------------------------------*/
(define js-number #f)
(define js-number-prototype #f)
   
;*---------------------------------------------------------------------*/
;*    js-init-number! ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.1       */
;*---------------------------------------------------------------------*/
(define (js-init-number! %this)
   (set! js-number-prototype
      (instantiate::JsNumber
	 (val 0)
	 (__proto__ js-object-prototype)
	 (extensible #t)))
   ;; Create a HopScript number object constructor
   (let ((obj (js-make-function %js-number 1 "JsNumber"
		 :__proto__ js-function-prototype
		 :prototype js-number-prototype
		 :alloc js-number-alloc
		 :construct js-number-construct))
	 (inf+ (instantiate::JsValueDescriptor
		  (name 'POSITIVE_INFINITY)
		  (writable #f)
		  (enumerable #f)
		  (configurable #f)
		  (value +inf.0)))
	 (inf- (instantiate::JsValueDescriptor
		  (name 'NEGATIVE_INFINITY)
		  (writable #f)
		  (enumerable #f)
		  (configurable #f)
		  (value -inf.0)))
	 (maxv (instantiate::JsValueDescriptor
		  (name 'MAX_VALUE)
		  (writable #f)
		  (enumerable #f)
		  (configurable #f)
		  (value (*fl 1.7976931348623157 (exptfl 10. 308.)))))
	 (minv (instantiate::JsValueDescriptor
		  (name 'MIN_VALUE)
		  (writable #f)
		  (enumerable #f)
		  (configurable #f)
		  (value 5e-324)))
	 (nan (instantiate::JsValueDescriptor
		  (name 'NaN)
		  (writable #f)
		  (enumerable #f)
		  (configurable #f)
		  (value +nan.0))))
      (with-access::JsObject obj (properties)
	 (set! properties (cons* inf+ inf- maxv minv nan properties)))
      (set! js-number obj)
      ;; bind the builtin prototype properties
      (init-builtin-number-prototype! js-number-prototype)
      ;; bind Number in the global object
      (js-bind! %this 'Number :configurable #f :enumerable #f :value js-number)
      js-number))

;*---------------------------------------------------------------------*/
;*    %js-number ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.1       */
;*---------------------------------------------------------------------*/
(define (%js-number this . arg)
   (js-tonumber (if (pair? arg) (car arg) 0)))

;*---------------------------------------------------------------------*/
;*    js-number-alloc ...                                              */
;*---------------------------------------------------------------------*/
(define (js-number-alloc constructor::JsFunction)
   (instantiate::JsNumber
      (__proto__ (js-get constructor 'prototype))))

;*---------------------------------------------------------------------*/
;*    js-number-construct ...                                          */
;*---------------------------------------------------------------------*/
(define (js-number-construct this::JsNumber . arg)
   (with-access::JsNumber this (val)
      (set! val (if (pair? arg) (js-tonumber (car arg)) 0)))
   this)

;*---------------------------------------------------------------------*/
;*    js-valueof ::JsNumber ...                                        */
;*---------------------------------------------------------------------*/
(define-method (js-valueof this::JsNumber)
   (js-tonumber this))

;*---------------------------------------------------------------------*/
;*    js-tonumber ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-method (js-tonumber this::JsNumber)
   (with-access::JsNumber this (val)
      val))

;*---------------------------------------------------------------------*/
;*    js-tointeger ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-method (js-tointeger this::JsNumber)
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
(define (init-builtin-number-prototype! obj)
   ;; prototype fields
   (js-bind! obj 'constructor
      :value js-number
      :writable #t
      :configurable #t
      :enumerable #f)
   (js-bind! obj 'toString
      :value (js-make-function js-number-to-string 2 "toString")
      :writable #t
      :configurable #t
      :enumerable #f)
   (js-bind! obj 'toLocaleString
      :value (js-make-function js-number-to-localestring 2 "toLocaleString")
      :writable #t
      :configurable #t
      :enumerable #f)
   (js-bind! obj 'valueOf
      :value (js-make-function js-number-valueof 0 "valueOf")
      :writable #t
      :configurable #t
      :enumerable #f)
   (js-bind! obj 'toFixed
      :value (js-make-function js-number-tofixed 1 "toFixed")
      :writable #t
      :configurable #t
      :enumerable #f)
   (js-bind! obj 'toExponential
      :value (js-make-function (lambda l (error "not" "implemented" "yet")) 1 "toExponential")
      :writable #t
      :configurable #t
      :enumerable #f)
   (js-bind! obj 'toPrecision
      :value (js-make-function (lambda l (error "not" "implemented" "yet")) 1 "toPrecision")
      :writable #t
      :configurable #t
      :enumerable #f))

;*---------------------------------------------------------------------*/
;*    js-number-to-string ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.2     */
;*---------------------------------------------------------------------*/
(define (js-number-to-string this #!optional (radix (js-undefined)))
   (if (not (isa? this JsNumber))
       (js-raise-type-error "Not a number ~s" (typeof this))
       (with-access::JsNumber this (val)
	  (let ((r (if (eq? radix (js-undefined)) 10 (js-tointeger radix))))
	     (cond
		((or (< r 2) (> r 36))
		 (js-raise
		    (js-new js-range-error
		       (format "Radix out of range: ~a" r))))
		((and (flonum? val) (nanfl? val))
		 "NaN")
		((= val +inf.0)
		 "Infinity")
		((= val -inf.0)
		 "-Infinity")
		((or (= r 10) (= r 0))
		 (js-tostring val))
		(else
		 (number->string val r)))))))

;*---------------------------------------------------------------------*/
;*    js-number-to-localestring ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.3     */
;*---------------------------------------------------------------------*/
(define (js-number-to-localestring this #!optional (radix (js-undefined)))
   (js-number-to-string this radix))

;*---------------------------------------------------------------------*/
;*    js-number-valueof ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.4     */
;*---------------------------------------------------------------------*/
(define (js-number-valueof this)
   (if (not (isa? this JsNumber))
       (js-raise
	  (js-new js-type-error
	     (format "Not a number ~s" (js-tostring this))))
       (with-access::JsNumber this (val)
	  val)))
	  
;*---------------------------------------------------------------------*/
;*    js-number-tofixed ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.5     */
;*---------------------------------------------------------------------*/
(define (js-number-tofixed this fractiondigits)

   (define (signed val s)
      (if (>= val 0)
	  s
	  (string-append "-" s)))
   
   (if (not (isa? this JsNumber))
       (js-raise
	  (js-new js-type-error
	     (format "Not a number ~s" (js-tostring this))))
       (let ((f (if (eq? fractiondigits (js-undefined))
		    0
		    (js-tointeger fractiondigits))))
	  (if (or (< f 0) (> f 20))
	      (js-raise
		 (js-new js-range-error
		    (format "Fraction digits out of range: ~a" f)))
	      (with-access::JsNumber this (val)
		 (if (and (flonum? val) (nanfl? val))
		     "NaN"
		     (let ((x (abs val))
			   (f (->fixnum f)))
			(if (>= x (exptfl 10. 21.))
			    (signed val (js-tostring x))
			    (let ((n (round x)))
			       (cond
				  ((= n 0)
				   (signed val
				      (if (= f 0)
					  "0"
					  (string-append "0."
					     (make-string f #\0)))))
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
					    (substring s (-fx l f))))))))))))))))

;*---------------------------------------------------------------------*/
;*    js+ ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.6.1       */
;*---------------------------------------------------------------------*/
(define (js+ left right)
   (cond
      ((and (fixnum? left) (fixnum? right))
       (let ((r (+fx left right)))
	  (if (or (>=fx r (bit-lsh 1 29)) (<fx r (negfx (bit-lsh 1 29))))
	      (fixnum->flonum r)
	      r)))
      ((and (number? left) (number? right))
       (+ left right))
      (else
       (js-slow+ left right))))

;*---------------------------------------------------------------------*/
;*    js-slow+ ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-slow+ left right)
   (let ((left (js-toprimitive left 'any))
	 (right (js-toprimitive right 'any)))
      (if (or (string? left) (string? right))
	  (js-string-append (js-tostring left) (js-tostring right))
	  (let* ((left (js-tonumber left))
		 (right (js-tonumber right)))
	     (if (or (not (= left left)) (not (= right right)))
		 +nan.0
		 (+ left right))))))

;*---------------------------------------------------------------------*/
;*    js- ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.6.1       */
;*---------------------------------------------------------------------*/
(define (js- left right)
   (if (and (number? left) (number? right))
       (- left right)
       (let* ((lnum (js-tonumber left))
	      (rnum (js-tonumber right)))
	  (- lnum rnum))))

;*---------------------------------------------------------------------*/
;*    js-neg ...                                                       */
;*---------------------------------------------------------------------*/
(define (js-neg expr)
   (let loop ((expr expr))
      (cond
	 ((and (number? expr) (= expr 0))
	  (if (flonum? expr)
	      (if (=fx (signbitfl expr) (signbitfl +0.0)) -0.0 +0.0)
	      -0.0))
	 ((number? expr)
	  (- expr))
	 (else
	  (loop (js-tonumber expr))))))
       
;*---------------------------------------------------------------------*/
;*    js* ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.5.1       */
;*---------------------------------------------------------------------*/
(define (js* left right)
   (let ((r (if (and (number? left) (number? right))
		(* left right)
		(let* ((lnum (js-tonumber left))
		       (rnum (js-tonumber right)))
		   (* lnum rnum)))))
      (if (bignum? r)
	  (bignum->flonum r)
	  r)))

;*---------------------------------------------------------------------*/
;*    js/ ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.5.2       */
;*---------------------------------------------------------------------*/
(define (js/ left right)
   (let* ((lnum (js-tonumber left))
	  (rnum (js-tonumber right)))
      (if (= rnum 0)
	  (if (flonum? rnum)
	      (cond
		 ((and (flonum? lnum) (nanfl? lnum))
		  lnum)
		 ((=fx (signbitfl rnum) (signbitfl +0.0))
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
	  (/ lnum rnum))))

;*---------------------------------------------------------------------*/
;*    js% ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.5.3       */
;*---------------------------------------------------------------------*/
(define (js% left right)
   (let* ((lnum (js-tonumber left))
	  (rnum (js-tonumber right)))
      (cond
	 ((= rnum 0)
	  +nan.0)
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
				    (=fx (signbitfl lnum) (signbitfl -0.0))))
			   (if (= m 0) -0.0 (- m))
			   (if (= m 0) +0.0 m))))
		 (let ((m (remainder alnum arnum)))
		    (if (< lnum 0)
			(if (= m 0) -0.0 (- m))
			(if (= m 0) +0.0 m)))))))))

;*---------------------------------------------------------------------*/
;*    js-bitlsh ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.7.1       */
;*---------------------------------------------------------------------*/
(define (js-bitlsh left right)
   (let* ((lnum (int32->elong (js-toint32 left)))
	  (rnum (int32->elong (js-touint32 right)))
	  (shiftcount (bit-andelong rnum #e31)))
      (int32->integer (elong->int32 (bit-lshelong lnum shiftcount)))))

;*---------------------------------------------------------------------*/
;*    js-bitrsh ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.7.2       */
;*---------------------------------------------------------------------*/
(define (js-bitrsh left right)
   (let* ((lnum (int32->elong (js-toint32 left)))
	  (rnum (uint32->elong (js-touint32 right)))
	  (shiftcount (bit-andelong rnum #e31)))
      (int32->integer (elong->int32 (bit-rshelong lnum shiftcount)))))

;*---------------------------------------------------------------------*/
;*    js-bitursh ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.7.3       */
;*---------------------------------------------------------------------*/
(define (js-bitursh left right)
   (let* ((lnum (int32->elong (js-touint32 left)))
	  (rnum (uint32->elong (js-touint32 right)))
	  (shiftcount (bit-andelong rnum #e31)))
      (int32->integer (elong->int32 (bit-urshelong lnum shiftcount)))))

;*---------------------------------------------------------------------*/
;*    js<                                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.8.1       */
;*---------------------------------------------------------------------*/
(define (js< left right)
   (if (and (number? left) (number? right))
       (< left right)
       (let* ((px (js-toprimitive left 'number))
	      (py (js-toprimitive right 'number)))
	  (if (and (string? px) (string? py))
	      (string<? px py)
	      (let ((nx (js-tonumber px))
		    (ny (js-tonumber py)))
		 (< nx ny))))))

;*---------------------------------------------------------------------*/
;*    js>                                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.8.2       */
;*---------------------------------------------------------------------*/
(define (js> left right)
   (if (and (number? left) (number? right))
       (> left right)
       (let* ((px (js-toprimitive left 'number))
	      (py (js-toprimitive right 'number)))
	  (if (and (string? px) (string? py))
	      (string>? px py)
	      (let ((nx (js-tonumber px))
		    (ny (js-tonumber py)))
		 (> nx ny))))))

;*---------------------------------------------------------------------*/
;*    js<=                                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.8.3       */
;*---------------------------------------------------------------------*/
(define (js<= left right)
   (if (and (number? left) (number? right))
       (<= left right)
       (let* ((px (js-toprimitive left 'number))
	      (py (js-toprimitive right 'number)))
	  (if (and (string? px) (string? py))
	      (string<=? px py)
	      (let ((nx (js-tonumber px))
		    (ny (js-tonumber py)))
		 (<= nx ny))))))

;*---------------------------------------------------------------------*/
;*    js>=                                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.8.4       */
;*---------------------------------------------------------------------*/
(define (js>= left right)
   (if (and (number? left) (number? right))
       (>= left right)
       (let* ((px (js-toprimitive left 'number))
	      (py (js-toprimitive right 'number)))
	  (if (and (string? px) (string? py))
	      (string>=? px py)
	      (let ((nx (js-tonumber px))
		    (ny (js-tonumber py)))
		 (>= nx ny))))))

;*---------------------------------------------------------------------*/
;*    js-bitand ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.10        */
;*---------------------------------------------------------------------*/
(define (js-bitand left right)
   (let* ((lnum (int32->elong (js-toint32 left)))
	  (rnum (int32->elong (js-toint32 right))))
      (int32->integer (elong->int32 (bit-andelong lnum rnum)))))

;*---------------------------------------------------------------------*/
;*    js-bitor ...                                                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.10        */
;*---------------------------------------------------------------------*/
(define (js-bitor left right)
   (let* ((lnum (int32->elong (js-toint32 left)))
	  (rnum (int32->elong (js-toint32 right))))
      (int32->integer (elong->int32 (bit-orelong lnum rnum)))))

;*---------------------------------------------------------------------*/
;*    js-bitxor ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.10        */
;*---------------------------------------------------------------------*/
(define (js-bitxor left right)
   (let* ((lnum (int32->elong (js-toint32 left)))
	  (rnum (int32->elong (js-toint32 right))))
      (int32->integer (elong->int32 (bit-xorelong lnum rnum)))))

;*---------------------------------------------------------------------*/
;*    js-bitnot ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.8       */
;*---------------------------------------------------------------------*/
(define (js-bitnot expr)
   (let ((num (int32->elong (js-toint32 expr))))
      (int32->integer (elong->int32 (bit-notelong num)))))
