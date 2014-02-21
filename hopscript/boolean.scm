;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/hopscript/boolean.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Fri Jan 31 18:58:15 2014 (serrano)                */
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
   
   (import __hopscript_types
	   __hopscript_private
	   __hopscript_public
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_regexp
	   __hopscript_error)

   (export js-boolean
	   (js-init-boolean! ::JsObject)))

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
;*    js-boolean ...                                                   */
;*---------------------------------------------------------------------*/
(define js-boolean #f)
(define js-boolean-prototype #f)
   
;*---------------------------------------------------------------------*/
;*    js-init-boolean! ...                                             */
;*---------------------------------------------------------------------*/
(define (js-init-boolean! %this)
   ;; first, bind the builtin string prototype
   (set! js-boolean-prototype
      (instantiate::JsBoolean
	 (val #f)
	 (__proto__ js-object-prototype)
	 (extensible #t)))
   ;; then, Create a HopScript string object
   (let ((obj (js-make-function %js-boolean 1 "JsBoolean"
		 :__proto__ js-function-prototype
		 :prototype js-boolean-prototype
		 :alloc js-boolean-alloc
		 :construct js-boolean-construct)))
      (set! js-boolean obj)
      ;; now the boolean constructor is fully built,
      ;; initialize the prototype properties
      (init-builtin-boolean-prototype! js-boolean-prototype)
      ;; bind Boolean in the global object
      (js-bind! %this 'Boolean :configurable #f :enumerable #f :value js-boolean)
      js-boolean))

;*---------------------------------------------------------------------*/
;*    %js-boolean ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.6.1.1     */
;*---------------------------------------------------------------------*/
(define (%js-boolean this value)
   (js-toboolean value))

;*---------------------------------------------------------------------*/
;*    js-boolean-alloc ...                                             */
;*---------------------------------------------------------------------*/
(define (js-boolean-alloc constructor::JsFunction)
   (instantiate::JsBoolean
      (__proto__ (js-get constructor 'prototype))))

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
(define-method (js-valueof this::JsBoolean)
   (with-access::JsBoolean this (val)
      val))
   
;*---------------------------------------------------------------------*/
;*    init-builtin-boolean-prototype! ...                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.6.3.1     */
;*---------------------------------------------------------------------*/
(define (init-builtin-boolean-prototype! obj)
   ;; prototype fields
   (js-bind! obj 'constructor
      :value js-boolean
      :enumerable #f)
   ;; toString
   (js-bind! obj 'toString
      :value (js-make-function
		(lambda (this)
		   (if (isa? this JsBoolean)
		       (with-access::JsBoolean this (val)
			  (if val "true" "false"))
		       (js-raise-type-error "not a boolean" (typeof this))))
		0
		"toString")
      :enumerable #f)
   ;; valueOf
   (js-bind! obj 'valueOf
      :value (js-make-function
		(lambda (this)
		   (if (isa? this JsBoolean)
		       (with-access::JsBoolean this (val) val)
		       (js-raise-type-error "not a boolean" (typeof this))))
		0 "valueOf")
      :enumerable #f))
      
