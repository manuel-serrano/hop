;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/reflect.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  5 22:00:24 2018                          */
;*    Last change :  Wed Dec  5 22:21:17 2018 (serrano)                */
;*    Copyright   :  2018 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript Reflect object.              */
;*    -------------------------------------------------------------    */
;*    https://developer.mozilla.org/en-US/docs/Web/JavaScript/         */
;*       Reference/Global_Objects/Reflect                              */ 
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_reflect
   
   (include "../nodejs/nodejs_debug.sch")
   
   (library hop)
   
   (include "types.sch" "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_lib
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_regexp
	   __hopscript_array
	   __hopscript_error
	   __hopscript_worker
	   __hopscript_spawn)
   
   (export (js-init-reflect! ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    js-init-reflect! ...                                             */
;*---------------------------------------------------------------------*/
(define (js-init-reflect! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (__proto__ js-reflect)
      
      (set! js-reflect
	 (instantiateJsObject
	    (__proto__ __proto__)))

      (define (js-reflect-get this target prop receiver)
	 (if (isa? target JsObject)
	     (js-get-jsobject target receiver prop %this)
	     (js-raise-type-error %this "Not an object ~s" target)))
      
      (js-bind! %this js-reflect 'get
	 :configurable #t :enumerable #f :writable #t
	 :value (js-make-function %this js-reflect-get 3 'get))

      ;; bind Reflect in the global object
      (js-bind! %this %this 'Reflect
	 :configurable #t :enumerable #f :writable #t
	 :value js-reflect :hidden-class #t)
	 
      js-reflect))
   
