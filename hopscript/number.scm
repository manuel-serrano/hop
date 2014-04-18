;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/number.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:41:39 2013                          */
;*    Last change :  Mon Apr 14 16:36:24 2014 (serrano)                */
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
   
   (export (js-init-number! ::JsGlobalObject)
	   
	   (js+ left right ::JsGlobalObject)
	   (js-slow+ left right ::JsGlobalObject)
	   (js- left right ::JsGlobalObject)
	   (js-neg expr ::JsGlobalObject)
	   (js* left right ::JsGlobalObject)
	   (js/ left right ::JsGlobalObject)
	   (js% left right ::JsGlobalObject)
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
	   (js-bitnot expr ::JsGlobalObject)))

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
;* (define js-number #f)                                               */
;* (define js-number-prototype #f)                                     */
   
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
	       (__proto__ __proto__)
	       (extensible #t)))

	 (define (js-number-construct this::JsNumber . arg)
	    (with-access::JsNumber this (val)
	       (set! val (if (pair? arg) (js-tonumber (car arg) %this) 0)))
	    this)

	 (define (js-number-alloc constructor::JsFunction)
	    (instantiate::JsNumber
	       (__proto__ (js-get constructor 'prototype %this))))

	 ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.1
	 (define (%js-number this . arg)
	    (js-tonumber (if (pair? arg) (car arg) 0) %this))

	 ;; Create a HopScript number object constructor
	 (set! js-number
	    (js-make-function %this %js-number 1 "JsNumber"
	       :__proto__ js-function-prototype
	       :prototype js-number-prototype
	       :alloc js-number-alloc
	       :construct js-number-construct))
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

   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.2
   (define (js-number-to-string this #!optional (radix (js-undefined)))
      (if (not (isa? this JsNumber))
	  (js-raise-type-error %this "Not a number ~s" (typeof this))
	  (with-access::JsNumber this (val)
	     (let ((r (if (eq? radix (js-undefined))
			  10
			  (js-tointeger radix %this))))
		(cond
		   ((or (< r 2) (> r 36))
		    (js-raise-range-error %this "Radix out of range: ~a" r))
		   ((and (flonum? val) (nanfl? val))
		    "NaN")
		   ((= val +inf.0)
		    "Infinity")
		   ((= val -inf.0)
		    "-Infinity")
		   ((or (= r 10) (= r 0))
		    (js-tostring val %this))
		   (else
		    (number->string val r)))))))

   (js-bind! %this obj 'toString
      :value (js-make-function %this js-number-to-string 2 "toString")
      :writable #t
      :configurable #t
      :enumerable #f)

   ;; toLocaleString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.3
   (define (js-number-to-localestring this #!optional (radix (js-undefined)))
      (js-number-to-string this radix))

   (js-bind! %this obj 'toLocaleString
      :value (js-make-function %this js-number-to-localestring 2 "toLocaleString")
      :writable #t
      :configurable #t
      :enumerable #f)

   ;; valueOf
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.4
   (define (js-number-valueof this)
      (if (not (isa? this JsNumber))
	  (js-raise-type-error %this
	     "Not a number ~s" (js-tostring this %this))
	  (with-access::JsNumber this (val)
	     val)))

   (js-bind! %this obj 'valueOf
      :value (js-make-function %this js-number-valueof 0 "valueOf")
      :writable #t
      :configurable #t
      :enumerable #f)

   ;; toFixed
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.5
   (define (js-number-tofixed this fractiondigits)
      
      (define (signed val s)
	 (if (>= val 0)
	     s
	     (string-append "-" s)))
      
      (if (not (isa? this JsNumber))
	  (js-raise-type-error %this
	     "Not a number ~s" (js-tostring this %this))
	  (let ((f (if (eq? fractiondigits (js-undefined))
		       0
		       (js-tointeger fractiondigits %this))))
	     (if (or (< f 0) (> f 20))
		 (js-raise-range-error %this
		    "Fraction digits out of range: ~a" f)
		 (with-access::JsNumber this (val)
		    (if (and (flonum? val) (nanfl? val))
			"NaN"
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

   (js-bind! %this obj 'toFixed
      :value (js-make-function %this js-number-tofixed 1 "toFixed")
      :writable #t
      :configurable #t
      :enumerable #f)

   ;; toExponential
   (js-bind! %this obj 'toExponential
      :value (js-make-function %this
		(lambda l (error "not" "implemented" "yet"))
		1 "toExponential")
      :writable #t
      :configurable #t
      :enumerable #f)

   ;; toPrecision
   (js-bind! %this obj 'toPrecision
      :value (js-make-function %this
		(lambda l (error "not" "implemented" "yet"))
		1 "toPrecision")
      :writable #t
      :configurable #t
      :enumerable #f))

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
;*    js-slow+ ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-slow+ left right %this)
   (let ((left (js-toprimitive left 'any %this))
	 (right (js-toprimitive right 'any %this)))
      (if (or (string? left) (string? right))
	  (js-string-append
	     (js-tostring left %this)
	     (js-tostring right %this))
	  (let* ((left (js-tonumber left %this))
		 (right (js-tonumber right %this)))
	     (if (or (not (= left left)) (not (= right right)))
		 +nan.0
		 (+ left right))))))

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
;*---------------------------------------------------------------------*/
(define (js-neg expr %this)
   (let loop ((expr expr))
      (cond
	 ((and (number? expr) (= expr 0))
	  (if (flonum? expr)
	      (if (=fx (signbitfl expr) (signbitfl +0.0)) -0.0 +0.0)
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
   (let ((r (if (and (number? left) (number? right))
		(* left right)
		(let* ((lnum (js-tonumber left %this))
		       (rnum (js-tonumber right %this)))
		   (* lnum rnum)))))
      (if (bignum? r)
	  (bignum->flonum r)
	  r)))

;*---------------------------------------------------------------------*/
;*    js/ ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.5.2       */
;*---------------------------------------------------------------------*/
(define (js/ left right %this)
   (let* ((lnum (js-tonumber left %this))
	  (rnum (js-tonumber right %this)))
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
(define (js% left right %this)
   (let* ((lnum (js-tonumber left %this))
	  (rnum (js-tonumber right %this)))
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
	  (if (and (string? px) (string? py))
	      (string<? px py)
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
	  (if (and (string? px) (string? py))
	      (string>? px py)
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
	  (if (and (string? px) (string? py))
	      (string<=? px py)
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
	  (if (and (string? px) (string? py))
	      (string>=? px py)
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
