;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/number.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:41:39 2013                          */
;*    Last change :  Wed Dec 20 13:05:34 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript numbers                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_number
   
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
	   __hopscript_arithmetic)
   
   (export (js-init-number! ::JsGlobalObject)
	   (js-number->jsnumber ::obj ::JsGlobalObject)
	   
	   (js-jsnumber-tostring ::obj ::obj ::JsGlobalObject)
	   (js-jsnumber-maybe-tostring ::obj ::obj ::JsGlobalObject)
	   
	   (js+ ::obj ::obj ::JsGlobalObject)
	   (js-slow+ ::obj ::obj ::JsGlobalObject)
	   (js- ::obj ::obj ::JsGlobalObject)
	   (js/ ::obj ::obj ::JsGlobalObject)
	   (js/num left right)))

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
	    (instantiateJsNumber
	       (val 0)
	       (__proto__ __proto__)))

	 (define (js-number-constructor f value)
	    (instantiateJsNumber
	       (__proto__ (js-get f 'prototype %this))
	       (val (if (eq? value (js-null)) 0 (js-tonumber value %this)))))
		
	 (define (js-number-alloc constructor::JsFunction)
	    (instantiateJsNumber
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
	       :alloc js-number-alloc
	       :shared-cmap #f))
	 
	 ;; other properties of the Number constructor
	 (js-bind! %this js-number 'POSITIVE_INFINITY
	    :value +inf.0
	    :writable #f :enumerable #f :configurable #f :hidden-class #f)
	 (js-bind! %this js-number 'NEGATIVE_INFINITY
	    :value -inf.0
	    :writable #f :enumerable #f :configurable #f :hidden-class #f)
	 (js-bind! %this js-number 'MAX_VALUE
	    :value (*fl 1.7976931348623157 (exptfl 10. 308.))
	    :writable #f :enumerable #f :configurable #f :hidden-class #f)
	 (js-bind! %this js-number 'MAX_SAFE_INTEGER
	    :value (-fl (exptfl 2. 53.) 1.)
	    :writable #f :enumerable #f :configurable #f :hidden-class #f)
	 (js-bind! %this js-number 'MIN_VALUE
	    :value 5e-324
	    :writable #f :enumerable #f :configurable #f :hidden-class #f)
	 (js-bind! %this js-number 'MIN_SAFE_INTEGER
	    :value (negfl (-fl (exptfl 2. 53.) 1.))
	    :writable #f :enumerable #f :configurable #f :hidden-class #f)
	 (js-bind! %this js-number 'NaN
	    :value +nan.0
	    :writable #f :enumerable #f :configurable #f :hidden-class #f)
	 (js-bind! %this js-number 'isInteger
	    :value (js-make-function %this is-integer? 1 'isInteger)
	    :writable #f :configurable #f :enumerable #f :hidden-class #f)
	 ;; bind the builtin prototype properties
	 (init-builtin-number-prototype! %this js-number js-number-prototype)
	 ;; bind Number in the global object
	 (js-bind! %this %this 'Number
	    :configurable #f :enumerable #f :value js-number :hidden-class #f)
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
      :enumerable #f
      :hidden-class #f)

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
      :enumerable #f
      :hidden-class #f)

   ;; toLocaleString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.3
   (define (js-number-tolocalestring this #!optional (radix (js-undefined)))
      (js-number-tostring this radix))

   (js-bind! %this obj 'toLocaleString
      :value (js-make-function %this js-number-tolocalestring 2 'toLocaleString)
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #f)

   ;; valueOf
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.4
   (define (js-number-valueof this)
      (js-cast-number this #f))

   (js-bind! %this obj 'valueOf
      :value (js-make-function %this js-number-valueof 0 'valueOf)
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #f)

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
					   (if (=fx (-fx l f) 0)
					       "0."
					       ".")
					   (substring s (-fx l f)))))))))))))))

   (js-bind! %this obj 'toFixed
      :value (js-make-function %this js-number-tofixed 1 'toFixed)
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #f)

   ;; toExponential
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.6
   (js-bind! %this obj 'toExponential
      :value (js-make-function %this
		(lambda (this val)
		   (error "toExponential" "not implemented" "yet"))
		1 'toExponential)
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #f)

   ;; toPrecision
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.7
   (js-bind! %this obj 'toPrecision
      :value (js-make-function %this
		(lambda (this val)
		   (error "toPrecision" "not implemented" "yet"))
		1 'toPrecision)
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #f))

;*---------------------------------------------------------------------*/
;*    js-number->jsnumber ...                                          */
;*---------------------------------------------------------------------*/
(define (js-number->jsnumber val %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-number)
      (js-new1 %this js-number val)))

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
;*    js+ ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.6.1       */
;*---------------------------------------------------------------------*/
(define (js+ left right %this::JsGlobalObject)
   (if (and (number? left) (number? right))
       (+js left right %this)
       (js-slow+ left right %this)))

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
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.6.2       */
;*---------------------------------------------------------------------*/
(define (js- left right %this::JsGlobalObject)
   (if (and (number? left) (number? right))
       (-/overflow left right)
       (let* ((lnum (js-tonumber left %this))
	      (rnum (js-tonumber right %this)))
	  (-/overflow lnum rnum))))

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
   (cond
      ((= rnum 0)
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
	      (else +nan.0))))
      (else
       (/ lnum rnum))))

