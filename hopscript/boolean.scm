;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/boolean.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Mon Jan 29 13:05:50 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript booleans                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.6         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_boolean

   (library hop)

   (include "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_private
	   __hopscript_public
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_regexp
	   __hopscript_error)

   (export (js-init-boolean! ::JsGlobalObject)
	   (js-bool->jsBoolean::JsBoolean ::bool ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-begin!)

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsBoolean ...                                */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsBoolean
   (lambda (o)
      (with-access::JsBoolean o (val) val))
   (lambda (o %this)
      (js-bool->jsBoolean o (or %this (js-initial-global-object)))))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsBoolean ...                                        */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsBoolean worker::WorkerHopThread %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-boolean)
	 (let ((nobj (call-next-method)))
	    (with-access::JsBoolean nobj (__proto__ val)
	       (with-access::JsBoolean obj ((_val val))
		  (set! __proto__ (js-get js-boolean 'prototype %this))
		  (set! val (js-donate _val worker %_this))))
	    nobj))))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsBoolean ...                                  */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsBoolean op compile isexpr)
   (with-access::JsBoolean o (val)
      (display (if val "new Boolean(true)" "new Boolean(false)") op)))

;*---------------------------------------------------------------------*/
;*    js-init-boolean! ...                                             */
;*---------------------------------------------------------------------*/
(define (js-init-boolean! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (__proto__ js-boolean js-function)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 (define js-boolean-prototype
	    (instantiate::JsBoolean
	       (val #f)
	       (__proto__ __proto__)))
	 
	 (define (js-boolean-alloc constructor::JsFunction)
	    (instantiate::JsBoolean
	       (__proto__ (js-get constructor 'prototype %this))))

	 ;; then, Create a HopScript string object
	 (set! js-boolean
	    (js-make-function %this %js-boolean 1 'Boolean
	       :__proto__ js-function-prototype
	       :prototype js-boolean-prototype
	       :alloc js-boolean-alloc
	       :construct js-boolean-construct))
	 ;; now the boolean constructor is fully built,
	 ;; initialize the prototype properties
	 (init-builtin-boolean-prototype! %this js-boolean js-boolean-prototype)
	 ;; bind Boolean in the global object
	 (js-bind! %this %this 'Boolean
	    :configurable #f :enumerable #f :value js-boolean
	    :hidden-class #t)
	 js-boolean)))

;*---------------------------------------------------------------------*/
;*    %js-boolean ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.6.1.1     */
;*---------------------------------------------------------------------*/
(define (%js-boolean this value)
   (js-toboolean value))

;*---------------------------------------------------------------------*/
;*    js-boolean-construct ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.6.2       */
;*---------------------------------------------------------------------*/
(define (js-boolean-construct this::JsBoolean arg)
   (with-access::JsBoolean this (val)
      (set! val (js-toboolean arg)))
   this)

;*---------------------------------------------------------------------*/
;*    js-valueof ::JsBoolean ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-valueof this::JsBoolean %this)
   (with-access::JsBoolean this (val)
      val))
   
;*---------------------------------------------------------------------*/
;*    init-builtin-boolean-prototype! ...                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.6.3.1     */
;*---------------------------------------------------------------------*/
(define (init-builtin-boolean-prototype! %this::JsGlobalObject js-boolean obj)

   (define (js-cast-boolean this shape)
      (cond
	 ((boolean? this) this)
	 ((isa? this JsBoolean) (with-access::JsBoolean this (val) val))
	 (else (js-raise-type-error %this "Not a boolean ~a"
		  (if shape (shape this) this)))))
   
   ;; prototype fields
   (js-bind! %this obj 'constructor
      :value js-boolean
      :enumerable #f
      :hidden-class #t)
   ;; toString
   (js-bind! %this obj 'toString
      :value (js-make-function %this
		(lambda (this)
		   (let ((val (js-cast-boolean this typeof)))
		      (if val
			  (js-string->jsstring "true")
			  (js-string->jsstring "false"))))
		0
		'toString)
      :enumerable #f
      :hidden-class #t)
   ;; valueOf
   (js-bind! %this obj 'valueOf
      :value (js-make-function %this
		(lambda (this)
		   (js-cast-boolean this #f))
		0 'valueOf)
      :enumerable #f
      :hidden-class #t))
      
;*---------------------------------------------------------------------*/
;*    js-bool->jsBoolean ...                                           */
;*---------------------------------------------------------------------*/
(define (js-bool->jsBoolean val::bool %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-boolean)
      (js-new1 %this js-boolean val)))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral end                                              */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-end!)

