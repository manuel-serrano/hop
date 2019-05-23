;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/reflect.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  5 22:00:24 2018                          */
;*    Last change :  Thu May 23 08:55:58 2019 (serrano)                */
;*    Copyright   :  2018-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript REFLECT object.              */
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
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    js-init-reflect! ...                                             */
;*---------------------------------------------------------------------*/
(define (js-init-reflect! %this::JsGlobalObject)
   (unless (vector? __js_strings) (set! __js_strings (&init!)))
   (with-access::JsGlobalObject %this (__proto__)
      
      (define js-reflect
	 (instantiateJsObject
	    (__proto__ __proto__)))

      (define (js-reflect-apply this target thisarg argarray)
	 (js-apply-array %this target thisarg argarray))

      (define (js-reflect-construct this target argarray newtarget)
	 (cond
	    ((eq? newtarget (js-undefined))
	     (apply js-new %this target (js-iterable->list argarray %this)))
	    ((not (js-object? newtarget))
	     (js-raise-type-error %this "construct: Not an object ~s" newtarget))
	    ((js-function? target)
	     (with-access::JsFunction target (construct alloc)
		(let* ((o (alloc %this newtarget))
		       (t (js-apply% %this target construct o
			     (js-iterable->list argarray %this)))
		       (r (if (js-object? t) t o))
		       (p (js-get newtarget (& "prototype") %this)))
		   (js-setprototypeof r p %this "construct"))))
	    (else
	     (js-raise-type-error %this "construct: Not a function ~s" target))))
      
      (define (js-reflect-defprop this target prop attr)
	 (let ((name (js-toname prop %this)))
	    (js-define-own-property target name
	       (js-to-property-descriptor %this attr name) #f %this)))
	 
      (define (js-reflect-delete this target prop attr)
	 (js-delete! target (js-toname prop %this) #f %this))
	 
      (define (js-reflect-get this target prop receiver)
	 (if (js-object? target)
	     (if (eq? receiver (js-undefined))
		 (js-get target prop %this)
		 (js-get-jsobject target receiver prop %this))
	     (js-raise-type-error %this "Not an object ~s" target)))

      (define (js-reflect-getown this target prop)
	 (js-get-own-property target (js-toname prop %this) %this))
	 
      (define (js-reflect-getproto this target)
	 (js-getprototypeof target %this "getPrototypeOf"))

      (define (js-reflect-hasown this target prop)
	 (js-has-property target prop %this))
	 
      (define (js-reflect-is-extensible this target)
	 (js-extensible? target %this))
	 
      (define (js-reflect-ownkeys this target)
	 (js-vector->jsarray
	    (vector-append
	       (js-properties-name target #f %this)
	       (js-properties-symbol target %this))
	    %this))
	 
      (define (js-reflect-preventext this target)
	 (js-preventextensions target %this))
	 
      (define (js-reflect-set this target prop v)
	 (js-put! target prop v #t %this))
	 
      (define (js-reflect-setproto this target v)
	 (with-handler
	    (lambda (e)
	       #f)
	    (begin
	       (js-setprototypeof target v %this "setPrototypeOf")
	       #t)))
      
      (js-bind! %this js-reflect (& "apply")
	 :configurable #t :enumerable #f :writable #t
	 :value (js-make-function %this js-reflect-apply 3 "apply"))

      (js-bind! %this js-reflect (& "construct")
	 :configurable #t :enumerable #f :writable #t
	 :value (js-make-function %this js-reflect-construct 2 "construct"))
	 
      (js-bind! %this js-reflect (& "defineProperty")
	 :configurable #t :enumerable #f :writable #t
	 :value (js-make-function %this js-reflect-defprop 3 "defineProperty"))
	 
      (js-bind! %this js-reflect (& "deleteProperty")
	 :configurable #t :enumerable #f :writable #t
	 :value (js-make-function %this js-reflect-delete 2 "deleteProperty"))
	 
      (js-bind! %this js-reflect (& "get")
	 :configurable #t :enumerable #f :writable #t
	 :value (js-make-function %this js-reflect-get 3 "get"))

      (js-bind! %this js-reflect (& "getOwnPropertyDescriptor")
	 :configurable #t :enumerable #f :writable #t
	 :value (js-make-function %this js-reflect-getown 2 "getOwnPropertyDescriptor"))
	 
      (js-bind! %this js-reflect (& "getPrototypeOf")
	 :configurable #t :enumerable #f :writable #t
	 :value (js-make-function %this js-reflect-getproto 1 "getPrototypeof"))
	 
      (js-bind! %this js-reflect (& "has")
	 :configurable #t :enumerable #f :writable #t
	 :value (js-make-function %this js-reflect-hasown 2 "has"))
	 
      (js-bind! %this js-reflect (& "isExtensible")
	 :configurable #t :enumerable #f :writable #t
	 :value (js-make-function %this js-reflect-is-extensible 1 "isExtensible"))
	 
      (js-bind! %this js-reflect (& "ownKeys")
	 :configurable #t :enumerable #f :writable #t
	 :value (js-make-function %this js-reflect-ownkeys 1 "ownKeys"))
	 
      (js-bind! %this js-reflect (& "preventExtensions")
	 :configurable #t :enumerable #f :writable #t
	 :value (js-make-function %this js-reflect-preventext 1 "preventExtensions"))
	 
      (js-bind! %this js-reflect (& "set")
	 :configurable #t :enumerable #f :writable #t
	 :value (js-make-function %this js-reflect-set 3 "set"))
	 
      (js-bind! %this js-reflect (& "setPrototypeOf")
	 :configurable #t :enumerable #f :writable #t
	 :value (js-make-function %this js-reflect-setproto 2 "setPrototypeOf"))
	 
      ;; bind Reflect in the global object
      (js-bind! %this %this (& "Reflect")
	 :configurable #t :enumerable #f :writable #t
	 :value js-reflect :hidden-class #t)
	 
      js-reflect))
   

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
