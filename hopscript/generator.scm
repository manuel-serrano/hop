;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/generator.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct 29 21:14:17 2015                          */
;*    Last change :  Fri Oct 30 20:41:05 2015 (serrano)                */
;*    Copyright   :  2015 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Native BIgloo support of JavaScript generators                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/6.0/#14.4             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_generator
   
   (include "../nodejs/nodejs_debug.sch")
   
   (library hop)
   
   (include "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_lib
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_regexp
	   __hopscript_array
	   __hopscript_error
	   __hopscript_worker)
   
   (export (js-init-generator! ::JsGlobalObject)
	   (js-make-generator::JsGenerator ::procedure ::JsGlobalObject)
	   (js-generator-yield ::obj ::bool ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    Jsstringliteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-begin!)

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsGenerator ...                              */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsGenerator
   (lambda (o)
      (js-undefined))
   (lambda (o %this) o))

;*---------------------------------------------------------------------*/
;*    js-toprimitive ...                                               */
;*---------------------------------------------------------------------*/
(define-method (js-toprimitive o::JsGenerator preferredtype %this::JsGlobalObject)
   (js-undefined))
   
;*---------------------------------------------------------------------*/
;*    js-init-generator! ...                                           */
;*---------------------------------------------------------------------*/
(define (js-init-generator! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (__proto__ js-generator-prototype)

      (define (js-generator-next this val)
	 (if (isa? this JsGenerator)
	     (with-access::JsGenerator this (%next)
		(if (procedure? %next)
		    (%next val)
		    (js-generator-yield (js-undefined) #t %this)))
	     (js-raise-type-error %this "argument not a generator ~a"
		(typeof this))))
      
      (let ((js-gen-proto (instantiate::JsObject
			     (__proto__ __proto__))))
	 
	 (js-bind! %this js-gen-proto 'next
	    :configurable #f :enumerable #f
	    :value (js-make-function %this js-generator-next 1 'next))
	 
	 (set! js-generator-prototype js-gen-proto))))
	    
;*---------------------------------------------------------------------*/
;*    js-make-generator ...                                            */
;*---------------------------------------------------------------------*/
(define (js-make-generator proc %this)
   (with-access::JsGlobalObject %this (js-generator-prototype)
      (instantiate::JsGenerator
	 (__proto__ js-generator-prototype)
	 (%next proc))))

;*---------------------------------------------------------------------*/
;*    js-generator-yield ...                                           */
;*---------------------------------------------------------------------*/
(define (js-generator-yield val done %this)
   (let ((obj (instantiate::JsObject)))
      (js-put! obj 'value val #f %this)
      (js-put! obj 'done done #f %this)
      obj))
