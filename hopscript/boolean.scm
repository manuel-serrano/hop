;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/boolean.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Mon Dec  8 07:52:08 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
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

   (export (js-init-boolean! ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-string-literal-begin!)

;* {*---------------------------------------------------------------------*} */
;* {*    js-intern-finalizer ::JsBoolean ...                              *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (js-intern-finalizer obj::JsBoolean %this::JsGlobalObject) */
;*    (with-access::JsGlobalObject %this (js-boolean)                  */
;*       (with-access::JsFunction js-boolean (construct)               */
;* 	 (with-access::JsBoolean obj (__proto__)                       */
;* 	    (set! __proto__ (js-get construct 'prototype %this)))))    */
;*    obj)                                                             */
   
;* {*---------------------------------------------------------------------*} */
;* {*    object-serializer ::JsBoolean ...                                *} */
;* {*---------------------------------------------------------------------*} */
;* (register-class-serialization! JsBoolean                            */
;*    (lambda (o)                                                      */
;*       (call-with-output-string                                      */
;* 	 (lambda (op)                                                  */
;* 	    (obj->javascript-expr o op))))                             */
;*    (lambda (s)                                                      */
;*       (call-with-input-string s                                     */
;* 	 javascript->jsobj)))                                          */

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
	       (__proto__ __proto__)
	       (extensible #t)))
	 
	 (define (js-boolean-alloc constructor::JsFunction)
	    (instantiate::JsBoolean
	       (__proto__ (js-get constructor 'prototype %this))))

	 ;; then, Create a HopScript string object
	 (set! js-boolean
	    (js-make-function %this %js-boolean 1 'JsBoolean
	       :__proto__ js-function-prototype
	       :prototype js-boolean-prototype
	       :alloc js-boolean-alloc
	       :construct js-boolean-construct))
	 ;; now the boolean constructor is fully built,
	 ;; initialize the prototype properties
	 (init-builtin-boolean-prototype! %this js-boolean js-boolean-prototype)
	 ;; bind Boolean in the global object
	 (js-bind! %this %this 'Boolean
	    :configurable #f :enumerable #f :value js-boolean)
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
   ;; prototype fields
   (js-bind! %this obj 'constructor
      :value js-boolean
      :enumerable #f)
   ;; toString
   (js-bind! %this obj 'toString
      :value (js-make-function %this
		(lambda (this)
		   (if (isa? this JsBoolean)
		       (with-access::JsBoolean this (val)
			  (if val
			      (string->js-string "true")
			      (string->js-string "false")))
		       (js-raise-type-error %this "not a boolean"
			  (typeof this))))
		0
		'toString)
      :enumerable #f)
   ;; valueOf
   (js-bind! %this obj 'valueOf
      :value (js-make-function %this
		(lambda (this)
		   (if (isa? this JsBoolean)
		       (with-access::JsBoolean this (val) val)
		       (js-raise-type-error %this "not a boolean"
			  (typeof this))))
		0 'valueOf)
      :enumerable #f))
      
;*---------------------------------------------------------------------*/
;*    JsStringLiteral end                                              */
;*---------------------------------------------------------------------*/
(%js-string-literal-end!)

