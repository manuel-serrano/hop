;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/bigint.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri Jun 25 13:34:53 2021                          */
;*    Last change :  Tue Jul  6 19:01:13 2021 (serrano)                */
;*    Copyright   :  2021 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript BigInt                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_bigint

   (library hop)

   (include "types.sch" "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_private
	   __hopscript_public
	   __hopscript_lib
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_regexp
	   __hopscript_error)

   (export (js-init-bigint! ::JsGlobalObject)
	   (js-bigint->jsbigint::JsBigInt ::bignum %this::JsGlobalObject)
	   (inline js-bigint->jsstring::JsStringLiteral ::bignum)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsBigInt ...                                 */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsBigInt
   (lambda (o)
      (with-access::JsBigInt o (val) val))
   (lambda (o ctx)
      (if (isa? ctx JsGlobalObject)
	  (js-bigint->jsbigint o ctx)
	  (error "string->obj ::JsBigInt" "Not a JavaScript context" ctx))))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsBigInt ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsBigInt worker::WorkerHopThread %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-bigint)
	 (let ((nobj (call-next-method)))
	    (with-access::JsBigInt nobj (val)
	       (with-access::JsBigInt obj ((_val val))
		  (js-object-proto-set! nobj (js-get js-bigint (& "prototype") %this))
		  (set! val (js-donate _val worker %_this))))
	    nobj))))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsBigInt ...                                   */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsBigInt op compile isexpr ctx)
   (with-access::JsBigInt o (val)
      (display "Object(" op)
      (display val op)
      (display "n)" op)))

;*---------------------------------------------------------------------*/
;*    js-bigint->jsbigint ...                                          */
;*---------------------------------------------------------------------*/
(define (js-bigint->jsbigint o %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-bigint-prototype js-initial-cmap)
      (instantiateJsBigInt
	 (val o)
	 (__proto__ js-bigint-prototype)
	 (cmap js-initial-cmap))))

;*---------------------------------------------------------------------*/
;*    js-init-bigint! ...                                              */
;*---------------------------------------------------------------------*/
(define (js-init-bigint! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-function js-bigint js-bigint-prototype)
      ;; local constant strings
      (unless (vector? __js_strings) (set! __js_strings (&init!)))
      
      (set! js-bigint-prototype
	 (instantiateJsBigInt
	    (__proto__ (js-object-proto %this))
	    (val #z0)))
      
      (define (%js-bigint this value)
	 ;; https://tc39.es/ecma262/multipage/numbers-and-dates.html#sec-bigint-objects
	 (js-tobigint value %this))
      
      ;; then, Create a HopScript string object
      (set! js-bigint
	 (js-make-function %this %js-bigint
	    (js-function-arity %js-bigint)
	    (js-function-info :name "BigInt" :len 1)
	    :__proto__ (js-object-proto js-function)
	    :prototype #f
	    :alloc js-not-a-constructor-alloc))
      ;; now the bigint constructor is fully built,
      ;; initialize the prototype properties
      (init-builtin-bigint-prototype! %this js-bigint-prototype)
      ;; asUintN
      ;; https://tc39.es/ecma262/multipage/numbers-and-dates.html#sec-bigint.asuintn
      (js-bind! %this js-bigint (& "asUintN")
	 :value (js-make-function %this
		   (lambda (this bits bigint)
		      (if (bignum? bigint)
			  this
			  (js-raise-type-error %this
			     "BigInt.prototype.asUintN requires that 'this' be a BigInt" this)))
		   (js-function-arity 2 0)
		   (js-function-info :name "asUintN" :len 2))
	 :enumerable #f
	 :hidden-class #t)
      ;; asIntN
      (js-bind! %this js-bigint (& "asIntN")
	 :value (js-make-function %this
		   (lambda (this bits bigint)
		      (if (bignum? bigint)
			  this
			  (js-raise-type-error %this
			     "BigInt.prototype.asIntN requires that 'this' be a BigInt" this)))
		   (js-function-arity 2 0)
		   (js-function-info :name "asIntN" :len 2))
	 :enumerable #f
	 :hidden-class #t)
      ;; bind BigInt in the global object
      (js-bind! %this %this (& "BigInt")
	 :configurable #f :enumerable #f :value js-bigint
	 :hidden-class #t)
      js-bigint))

;*---------------------------------------------------------------------*/
;*    js-bigint->jsstring ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-bigint->jsstring n)
   (js-ascii->jsstring (bignum->string n)))

;*---------------------------------------------------------------------*/
;*    js-tobigint ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-tobigint value %this)
   (cond
      ((bignum? value) value)
      ((fixnum? value) (fixnum->bignum value))
      ((flonum? value) (flonum->bignum value))
      ((js-jsstring? value) (string->bignum (js-jsstring->string value)))
      (else #z0)))
   
;*---------------------------------------------------------------------*/
;*    init-builtin-bigint-prototype! ...                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.6.3.1     */
;*---------------------------------------------------------------------*/
(define (init-builtin-bigint-prototype! %this::JsGlobalObject obj)
   ;; toString
   (js-bind! %this obj (& "toString")
      :value (js-make-function %this
		(lambda (this)
		   (if (bignum? this)
		       (js-bigint->jsstring this)
		       (js-raise-type-error %this
			  "BigInt.prototype.toString requires that 'this' be a BigInt" this)))
		(js-function-arity 0 0)
		(js-function-info :name "toString" :len 0))
      :enumerable #f
      :hidden-class #t)
   ;; toLocaleString
   (js-bind! %this obj (& "toLocaleString")
      :value (js-make-function %this
		(lambda (this)
		   (if (bignum? this)
		       (js-ascii->jsstring (bignum->string this))
		       (js-raise-type-error %this
			  "BigInt.prototype.toLocaleString requires that 'this' be a BigInt" this)))
		(js-function-arity 0 0)
		(js-function-info :name "toLocaleString" :len 0))
      :enumerable #f
      :hidden-class #t)
   ;; valueOf
   (js-bind! %this obj (& "valueOf")
      :value (js-make-function %this
		(lambda (this)
		   (if (bignum? this)
		       this
		       (js-raise-type-error %this
			  "BigInt.prototype.valueOf requires that 'this' be a BigInt" this)))
		(js-function-arity 0 0)
		(js-function-info :name "valueOf" :len 0))
      :enumerable #f
      :hidden-class #t))

;*---------------------------------------------------------------------*/
;*    js-tostring ::JsBigInt ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-tostring obj::JsBigInt %this::JsGlobalObject)
   (with-access::JsBigInt obj (val)
      (bignum->string val)))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)

