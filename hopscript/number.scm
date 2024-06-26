;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/number.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:41:39 2013                          */
;*    Last change :  Wed May 22 06:43:55 2024 (serrano)                */
;*    Copyright   :  2013-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript numbers                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_number
   
   (library hop)

   (extern (macro $snprintf::int (::string ::int ::string ::double) "snprintf"))
   
   (include "types.sch" "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_string
	   __hopscript_error
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_lib
	   __hopscript_arithmetic
	   __hopscript_bigint)
   
   (export (js-init-number! ::JsGlobalObject)

	   (js-number->jsNumber ::obj ::JsGlobalObject)
	   (js-real->jsstring::JsStringLiteral ::double)
	   (js-jsnumber-tostring ::obj ::obj ::JsGlobalObject)
	   (js-jsnumber-maybe-tostring ::obj ::obj ::JsGlobalObject)

	   (js-maybe-tofixed ::obj ::obj ::JsGlobalObject ::obj))

   ;; exports for bmem profiling
   (export (js-number-alloc ::JsGlobalObject ::JsFunction)
	   (js-number-tofixed ::obj ::obj ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsNumber ...                                 */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsNumber
   (lambda (o)
      (with-access::JsNumber o (val) val))
   (lambda (o ctx)
      (if (isa? ctx JsGlobalObject)
	  (js-number->jsNumber o ctx)
	  (error "string->obj ::JsNumber" "Not a JavaScript context" ctx))))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsNumber ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsNumber worker::WorkerHopThread %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-number)
	 (let ((nobj (call-next-method)))
	    (with-access::JsNumber nobj (val)
	       (with-access::JsNumber obj ((_val val))
		  (js-object-proto-set! nobj (js-get js-number (& "prototype") %this))
		  (set! val (js-donate _val worker %_this))))
	    nobj))))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsNumber ...                                   */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsNumber op compile isexpr ctx)
   (with-access::JsNumber o (val)
      (display "new Number(" op)
      (display (if (and (flonum? val) (nanfl? val)) "undefined" val) op)
      (display ")" op)))

;*---------------------------------------------------------------------*/
;*    js-number->jsNumber ...                                          */
;*---------------------------------------------------------------------*/
(define (js-number->jsNumber o %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-number-prototype js-initial-cmap)
      (instantiateJsNumber
	 (val o)
	 (__proto__ js-number-prototype)
	 (cmap js-initial-cmap))))

;*---------------------------------------------------------------------*/
;*    js-init-number! ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.1       */
;*---------------------------------------------------------------------*/
(define (js-init-number! %this)
   (unless (vector? __js_strings) (set! __js_strings (&init!)))
   (with-access::JsGlobalObject %this (js-number js-function js-number-pcache
					 js-number-prototype)
      (set! js-number-pcache
	 ((@ js-make-pcache-table __hopscript_property) 2 "number"))
      
      (set! js-number-prototype
	 (instantiateJsNumber
	    (val 0)
	    (__proto__ (js-object-proto %this))))
      
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.1
      (define (%js-number this #!optional (arg 0))
	 (let ((num (cond
		       ((bignum? arg) (js-bigint->number arg))
		       ((number? arg) arg)
		       (else (js-tonumber arg %this)))))
	    (if (eq? (js-new-target-pop! %this) (js-undefined))
		num
		(with-access::JsNumber this (val)
		   (set! val num)
		   this))))
      
      (define (is-integer?::bbool this arg)
	 (cond
	    ((js-number? arg)
	     (integer? arg))
	    ((isa? this JsNumber)
	     (with-access::JsNumber this (val)
		(is-integer? this val)))
	    (else
	     #f)))
      
      ;; Create a HopScript number object constructor
      (set! js-number
	 (js-make-function %this %js-number
	    (js-function-arity 0 1 'scheme-optional)
	    (js-function-info :name "Number" :len 1)
	    :__proto__ (js-object-proto js-function)
	    :prototype js-number-prototype
	    :size 8
	    :alloc js-number-alloc
	    :shared-cmap #f))
      
      ;; other properties of the Number constructor
      (js-bind! %this js-number (& "POSITIVE_INFINITY")
	 :value +inf.0
	 :writable #f :enumerable #f :configurable #f :hidden-class #t)
      (js-bind! %this js-number (& "NEGATIVE_INFINITY")
	 :value -inf.0
	 :writable #f :enumerable #f :configurable #f :hidden-class #f)
      (js-bind! %this js-number (& "MAX_VALUE")
	 :value (*fl 1.7976931348623157 (exptfl 10. 308.))
	 :writable #f :enumerable #f :configurable #f :hidden-class #f)
      (js-bind! %this js-number (& "MAX_SAFE_INTEGER")
	 :value (-fl (exptfl 2. 53.) 1.)
	 :writable #f :enumerable #f :configurable #f :hidden-class #f)
      (js-bind! %this js-number (& "MIN_VALUE")
	 :value 5e-324
	 :writable #f :enumerable #f :configurable #f :hidden-class #f)
      (js-bind! %this js-number (& "MIN_SAFE_INTEGER")
	 :value (negfl (-fl (exptfl 2. 53.) 1.))
	 :writable #f :enumerable #f :configurable #f :hidden-class #f)
      (js-bind! %this js-number (& "NaN")
	 :value +nan.0
	 :writable #f :enumerable #f :configurable #f :hidden-class #f)
      (js-bind! %this js-number (& "isFinite")
	 :value (js-make-function %this
		   (lambda (this val)
		      (cond
			 ((not (number? val)) #f)
			 ((fixnum? val) #t)
			 ((bignum? val) #t)
			 (else (finitefl? val))))
		   (js-function-arity 1 0)
		   (js-function-info :name "isFinite" :len 1)
		   :prototype (js-undefined))
	 :enumerable #f :configurable #t :writable #t :hidden-class #f)
      (js-bind! %this js-number (& "isInteger")
	 :value (js-make-function %this is-integer?
		   (js-function-arity 1 0)
		   (js-function-info :name "isInteger" :len 1))
	 :writable #f :configurable #f :enumerable #f :hidden-class #f)
      (js-bind! %this js-number (& "isNaN")
	 :value (js-make-function %this
		   (lambda (this val)
		      (when (number? val)
			 (js-isnan? val %this)))
		   (js-function-arity 1 0)
		   (js-function-info :name "isNaN" :len 1)
		   :prototype (js-undefined))
	 :enumerable #f :configurable #t :writable #t :hidden-class #f)
      (js-bind! %this js-number (& "isSafeInteger")
	 :value (js-make-function %this
		   (lambda (this val)
		      (cond
			 ((fixnum? val) #t)
			 ((bignum? val) #t)
			 ((flonum? val) (<=fl (absfl val) (-fl (exptfl 2 53) 1)))
			 (else #f)))
		   (js-function-arity 1 0)
		   (js-function-info :name "isSafeFinite" :len 1)
		   :prototype (js-undefined))
	 :enumerable #f :configurable #t :writable #t :hidden-class #f)
      (js-bind! %this js-number (& "parseInt")
	       :value (js-make-function %this
			 (lambda (this string radix)
			    (js-parseint string radix %this))
			 (js-function-arity 3 0)
			 (js-function-info :name "parseInt" :len 2)
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)
      (js-bind! %this js-number (& "parseFloat")
	       :value (js-make-function %this
			 (lambda (this string)
			    (js-parsefloat string %this))
			 (js-function-arity 3 0)
			 (js-function-info :name "parseFloat" :len 2)
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)
			 
      ;; bind the builtin prototype properties
      (init-builtin-number-prototype! %this js-number js-number-prototype)
      ;; bind Number in the global object
      (js-bind! %this %this (& "Number")
	 :configurable #f :enumerable #f :value js-number :hidden-class #f)
      js-number))

;*---------------------------------------------------------------------*/
;*    js-number-alloc ...                                              */
;*---------------------------------------------------------------------*/
(define (js-number-alloc %this::JsGlobalObject constructor::JsFunction)
   (with-access::JsGlobalObject %this (js-new-target js-number-pcache)
      (set! js-new-target constructor)
      (with-access::JsFunction constructor (constrmap)
	 (unless constrmap
	    (set! constrmap
	       (js-make-jsconstructmap :ctor constructor)))
	 (instantiateJsNumber
	    (cmap constrmap)
	    (__proto__ (js-get-jsobject-name/cache constructor
			  (& "prototype")
			  #f %this (js-pcache-ref js-number-pcache 0)))))))

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
;*    js-tonumeric ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-method (js-tonumeric this::JsNumber %this::JsGlobalObject)
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
;*    js-toindex ::JsNumber ...                                        */
;*---------------------------------------------------------------------*/
(define-method (js-toindex this::JsNumber)
   (with-access::JsNumber this (val) (js-toindex val)))

;*---------------------------------------------------------------------*/
;*    init-builtin-number-prototype! ...                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4       */
;*---------------------------------------------------------------------*/
(define (init-builtin-number-prototype! %this::JsGlobalObject js-number obj)
   
   ;; constructor
   (js-bind! %this obj (& "constructor")
      :value js-number
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #t)

   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.2
   (define (js-number-tostring this #!optional (radix (js-undefined)))
      (js-jsnumber-tostring (js-cast-number this typeof %this) radix %this))

   (js-bind! %this obj (& "toString")
      :value (js-make-function %this
		js-number-tostring
		(js-function-arity 0 1 'scheme)
		(js-function-info :name "toString" :len 2))
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #f)

   ;; toLocaleString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.3
   (define (js-number-tolocalestring this #!optional (radix (js-undefined)))
      (js-number-tostring this radix))

   (js-bind! %this obj (& "toLocaleString")
      :value (js-make-function %this
		js-number-tolocalestring
		(js-function-arity 0 1 'scheme)
		(js-function-info :name "toLocaleString" :len 2))
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #f)

   ;; valueOf
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.4
   (define (js-number-valueof this)
      (js-cast-number this #f %this))

   (js-bind! %this obj (& "valueOf")
      :value (js-make-function %this js-number-valueof
		(js-function-arity 0 0)
		(js-function-info :name "valueOf" :len 0))
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #f)

   ;; toFixed
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.5
   (define (number-tofixed this fractiondigits)
      (js-number-tofixed (js-cast-number this #f %this) fractiondigits %this))

   (js-bind! %this obj (& "toFixed")
      :value (js-make-function %this number-tofixed
		(js-function-arity 1 0)
		(js-function-info :name "toFixed" :len 1))
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #f)

   ;; toExponential
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.6
   (define (number-toexponential this fractiondigits)
      (js-number-toexponential (js-cast-number this #f %this) fractiondigits %this))
   
   (js-bind! %this obj (& "toExponential")
      :value (js-make-function %this number-toexponential
		(js-function-arity 1 0)
		(js-function-info :name "toExponential" :len 1))
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #f)

   ;; toPrecision
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.7.4.7
   (define (number-toprecision this precision)
      (js-number-toprecision (js-cast-number this #f %this) precision %this))

   (js-bind! %this obj (& "toPrecision")
      :value (js-make-function %this number-toprecision
		(js-function-arity 1 0)
		(js-function-info :name "toPrecision" :len 1))
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #f))

;*---------------------------------------------------------------------*/
;*    js-cast-number ...                                               */
;*    -------------------------------------------------------------    */
;*    int32 and uint32 might reach this function when profiling.       */
;*---------------------------------------------------------------------*/
(define (js-cast-number this shape %this)
   (cond
      ((js-number? this) this)
      ((isa? this JsNumber) (with-access::JsNumber this (val) val))
      ((uint32? this) (js-uint32-tointeger this))
      ((int32? this) (js-int32-tointeger this))
      (else (js-raise-type-error %this "Not a number ~a"
	       (if shape (shape this) this)))))

;*---------------------------------------------------------------------*/
;*    js-number-tofixed ...                                            */
;*---------------------------------------------------------------------*/
(define (js-number-tofixed val fractiondigits %this::JsGlobalObject)
   
   (define (signed val s)
      (if (>= val 0)
	  (js-ascii->jsstring s)
	  (js-ascii->jsstring (string-append "-" s))))

   (let ((f (if (eq? fractiondigits (js-undefined))
		0
		(js-tointeger fractiondigits %this))))
      (if (or (< f 0) (> f 20))
	  (js-raise-range-error %this "Fraction digits out of range: ~a" f)
	  (if (and (flonum? val) (nanfl? val))
	      (& "NaN")
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
				     (substring s (-fx l f))))))))))))))

;*---------------------------------------------------------------------*/
;*    js-maybe-tofixed ...                                             */
;*---------------------------------------------------------------------*/
(define (js-maybe-tofixed this fractiondigits %this::JsGlobalObject cache)
   (if (or (fixnum? this) (flonum? this))
       (js-number-tofixed this fractiondigits %this)
       (with-access::JsGlobalObject %this (js-number-pcache)
	  (js-call1 %this
	     (js-get-name/cache this (& "toFixed") #f %this
		(or cache (js-pcache-ref js-number-pcache 1)))
	     this fractiondigits))))

;*---------------------------------------------------------------------*/
;*    js-number-toexponential ...                                      */
;*    -------------------------------------------------------------    */
;*    https://tc39.es/ecma262/2022/#sec-number.prototype.toexponential */
;*---------------------------------------------------------------------*/
(define (js-number-toexponential x fractiondigits %this::JsGlobalObject)

   (define (step-10ai x f)
      ;; 10.ai
      (let ((stop (exptfl 10. (fixnum->flonum f))))
	 (cond
	    ((<fl x stop)
	     (let  loop ((k 0)
			 (m x))
		(if (>=fl m stop)
		    (let ((n (flonum->fixnum (round m))))
		       (values (+fx k f) n f))
		    (loop (-fx k 1) (*fl m 10.)))))
	    ((<fl x (*fl stop 10.))
	     (let ((n (flonum->fixnum x)))
		(values f n f)))
	    (else
	     (let ((stop (*fl stop 10.)))
		(let  loop ((k 0)
			    (m x))
		   (if (<fl m stop)
		       (let ((n (flonum->fixnum (round m))))
			  (values (+fx k f) n f))
		       (loop (+fx k 1) (/fl m 10.)))))))))
   
   (define (step-10a x f)
      (cond
	 ((not (eq? fractiondigits (js-undefined)))
	  ;; 10.a
	  (step-10ai x f))
	 ((integer? x)
	  (step-10ai x (-fx (string-length (fixnum->string (flonum->fixnum x))) 1)))
	 (else
	  ;; 10.b
	  (step-10ai x (-fx (string-length (real->string x)) 2)))))
	  
   (define (step-10 x f)
      (multiple-value-bind (e n f)
	 (step-10a x f)
	 (let ((m (fixnum->string n)))
	    (step-11-14 x f m e))))
   
   (define (step-11-14 x f m e)
      (unless (=fx f 0)
	 ;; 11.
	 (let ((a (substring m 0 1))
	       (b (substring m 1 (+fx f 1))))
	    (set! m (string-append a "." b))))
      (let (c d)
	 (cond
	    ((=fx e 0)
	     ;; 12.
	     (set! c "+")
	     (set! d "0"))
	    ((>fx e 0)
	     ;; 13.a
	     (set! c "+")
	     (set! d (fixnum->string e)))
	    (else
	     ;; 13.b
	     (set! c "-")
	     (set! d (fixnum->string (negfx e)))))
	 ;; 14.
	 (string-append m "e" c d)))

   ;; 2.
   (let ((f (if (eq? fractiondigits (js-undefined))
		0
		(js-tointeger fractiondigits %this))))
      (when (fixnum? x) (set! x (fixnum->flonum x)))
      ;; 4.
      (cond
	 ((nanfl? x)
	  (& "NaN"))
	 ;; 4.
	 ((=fl x +inf.0)
	  ;; 4.
	  (& "Infinity"))
	 ((=fl x -inf.0)
	  ;; 4.
	  (& "-Infinity"))
	 ((or (< f 0) (> f 100))
	  ;; 5.
	  (js-raise-range-error %this
	     "fractiondigits out of range: ~a" f))
	 (else
	  (cond
	     ((<fl x 0.0)
	      ;; 8.
	      (js-string->jsstring
		 (string-append "-" (step-10 (negfx x) f))))
	     ((>fl x 0.0)
	      ;; 10.
	      (js-string->jsstring
		 (step-10 x f)))
	     (else
	      (let ((m (make-string (+fx f 1) #\0))
		    (e 0))
		 (js-string->jsstring
		    (step-11-14 x f m e)))))))))

;*---------------------------------------------------------------------*/
;*    js-number-toprecision ...                                        */
;*    -------------------------------------------------------------    */
;*    https://tc39.es/ecma262/2022/#sec-number.prototype.toprecision   */
;*---------------------------------------------------------------------*/
(define (js-number-toprecision x precision %this::JsGlobalObject)

   (define (step-10a x p)
      ;; Let e and n be integers such that 10^(p - 1) <= n < 10^p
      ;; and for which n × 10^(e - p + 1) - x is as close to zero as possible.
      ;; If there are two such sets of e and n, pick the e and n for
      ;; which n × 10^(e - p + 1) is larger.
      (let ((stop (exptfl 10. (fixnum->flonum (-fx p 1)))))
	 (cond
	    ((<fl x stop)
	     (let  loop ((k 0)
			 (m x))
		(if (>=fl m stop)
		    (let ((n (flonum->fixnum (round m))))
		       (values (+fx k (-fx p 1)) n))
		    (loop (-fx k 1) (*fl m 10.)))))
	    ((<fl x (*fl stop 10.))
	     (let ((n (flonum->fixnum x)))
		(values (-fx p 1) n)))
	    (else
	     (let ((stop (*fl stop 10.)))
		(let  loop ((k 0)
			    (m x))
		   (if (<fl m stop)
		       (let ((n (flonum->fixnum (round m))))
			  (values (+fx k (-fx p 1)) n))
		       (loop (+fx k 1) (/fl m 10.)))))))))

   (define (step-10 x p)
      (multiple-value-bind (e n)
	 (step-10a x p)
	 ;; 10.b
	 (let ((m (fixnum->string n)))
	    ;; 10.c
	    (if (or (<fx e -6) (>=fx e p))
		;; 10.c.i
		(let ((m (if (not (=fx p 1))
			     (let ((a (substring m 0 1))
				   (b (substring m 1)))
				;; 10.c.ii
				(string-append a "." b))
			     ;; 10.b
			     (fixnum->string n))))
		   (let (c d)
		      (cond
			 ((=fx e 0)
			  (set! c "")
			  (set! d "0"))
			 ((>fx e 0)
			  ;; 10.c.iii
			  (set! c "+"))
			 (else
			  ;; 10.iv
			  (set! c "-")
			  (set! e (negfx e))))
		      ;; 10.c.v
		      (set! d (fixnum->string e))
		      ;; 10.c.vi
		      (string-append m "e" c d)))
		;; 10.b
		(step-11-13 x p m e)))))

   (define (step-11-13 x p m e)
      (cond
	 ((=fx e (-fx p 1))
	  ;; 11.
	  m)
	 ((>=fx e 0)
	  ;; 12.
	  (string-append
	     (substring m 0 (+fx e 1))
	     "."
	     (substring m (+fx e 1))))
	 (else
	  ;; 13
	  (string-append "0."
	     (make-string (negfx (+fx e 1)) #\0)
	     m))))

   (if (eq? precision (js-undefined))
       ;; 2.
       (js-tojsstring x %this)
       ;; 3.
       (let ((p (js-tointeger precision %this)))
	  (when (fixnum? x) (set! x (fixnum->flonum x)))
	  ;; 4.
	  (cond
	     ((nanfl? x)
	      (& "NaN"))
	     ;; 4.
	     ((=fl x +inf.0)
	      ;; 4.
	      (& "Infinity"))
	     ((=fl x -inf.0)
	      ;; 4.
	      (& "-Infinity"))
	     ((or (< p 1) (> p 100))
	      ;; 5.
	      (js-raise-range-error %this
		 "Precision out of range: ~a" p))
	     (else
	      (cond
		 ((<fl x 0.0)
		  ;; 8.
		  (js-string->jsstring
		     (string-append "-" (step-10 (negfx x) precision))))
		 ((>fl x 0.0)
		  ;; 10.
		  (js-string->jsstring
		     (step-10 x precision)))
		 (else
		  (let ((m (make-string p #\0))
			(e 0))
		     (js-string->jsstring
			(step-11-13 x precision m e))))))))))
       
;*---------------------------------------------------------------------*/
;*    %real->string ...                                                */
;*---------------------------------------------------------------------*/
(define (%real->string n)
   (let ((str (make-string 50)))
      (let ((n ($snprintf str 50 "%f" n)))
	 (string-shrink! str n))))
      
;*---------------------------------------------------------------------*/
;*    js-real->jsstring ...                                            */
;*    -------------------------------------------------------------    */
;*    MS 1 jan 2018: This should be improved not to use bignums!       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.8.1        */
;*---------------------------------------------------------------------*/
(define (js-real->jsstring obj)
   
   (define (js-bignum->string m)
      (if (=bx m #z0)
	  "0"
	  (let loop ((p 0)
		     (s m))
	     (if (=bx (modulobx s #z10) #z0)
		 (loop (+ p 1) (/bx s #z10))
		 (let liip ((k 1)
			    (t #z10))
		    (if (>bx t s)
			(let ((n (+ p k)))
			   (cond
			      ((and (>= n k) (<= n 21))
			       ;; 6
			       (format "~a~a" (bignum->string s)
				  (make-string (- n k) #\0)))
			      ((and (< 0 n) (<= n 21))
			       ;; 7
			       (let ((s (bignum->string s)))
				  (format "~a.~a"
				     (substring s 0 n)
				     (substring s n))))
			      ((and (< -6 n) (<= n 0))
			       ;; 8
			       (format "0.~a~a"
				  (make-string (- n) #\0)
				  (substring (bignum->string s) n k)))
			      ((= k 1)
			       ;; 9
			       (format "~ae~a~a"
				  s
				  (if (>= n 1) "+" "-")
				  (number->string (abs (- n 1)))))
			      (else
			       ;; 10
			       (let ((s (bignum->string s)))
				  (format "~a.~ae~a~a"
				     (substring s 0 1)
				     (substring s 1)
				     (if (>= n 1) "+" "-")
				     (number->string (abs (- n 1))))))))
			(liip (+ k 1) (*bx t #z10))))))))
   
   (define (match->bignum::bignum m::pair)
      (let ((exp (string->integer (cadddr m)))) 
	 (+bx
	    (*bx (string->bignum (cadr m))
	       (exptbx #z10 (fixnum->bignum exp)))
	    (*bx (string->bignum (caddr m))
	       (exptbx #z10
		  (fixnum->bignum (-fx exp (string-length (caddr m)))))))))
   
   (define (js-real->string m)
      (if (=fl m 0.0)
	  "0"
	  (let ((s (real->string m)))
	     (cond
		((pregexp-match "^([-]?[0-9]+)[eE]([0-9]+)$" s)
		 =>
		 (lambda (m)
		    (js-bignum->string
		       (*bx (string->bignum (cadr m))
			  (exptbx #z10 (string->bignum (caddr m)))))))
		((pregexp-match "^([0-9]+).([0-9]+)[eE]([0-9]+)$" s)
		 =>
		 (lambda (m) (js-bignum->string (match->bignum m))))
		((pregexp-match "^-([0-9]+).([0-9]+)[eE]([0-9]+)$" s)
		 =>
		 (lambda (m)
		    (js-bignum->string (negbx (match->bignum m)))))
		((pregexp-match "^([-]?[.0-9]+)[.]0+$" s)
		 =>
		 cadr)
		(else
		 s)))))
   
   (cond
      ((not (= obj obj)) (& "NaN"))
      ((= obj +inf.0) (& "Infinity"))
      ((= obj -inf.0) (& "-Infinity"))
      (else (js-ascii->jsstring (js-real->string obj)))))

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
	 ((fixnum? val)
	  (js-string->jsstring (fixnum->string val r)))
	 ((and (flonum? val) (nanfl? val))
	  (& "NaN"))
	 ((= val +inf.0)
	  (& "Infinity"))
	 ((= val -inf.0)
	  (& "-Infinity"))
	 ((or (= r 10) (= r 0))
	  (js-tojsstring val %this))
	 ((integer? val)
	  (js-string->jsstring (llong->string (flonum->llong val) r)))
	 (else
	  (js-ascii->jsstring (number->string val r))))))

;*---------------------------------------------------------------------*/
;*    js-jsnumber-maybe-tostring ...                                   */
;*---------------------------------------------------------------------*/
(define (js-jsnumber-maybe-tostring this radix %this)
   (let loop ((this this))
      (cond
	 ((js-number? this)
	  (js-jsnumber-tostring this radix %this))
	 ((js-object? this)
	  (js-call1 %this (js-get this (& "toString") %this) this radix))
	 (else
	  (loop (js-toobject %this this))))))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
