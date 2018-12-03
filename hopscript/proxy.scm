;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/proxy.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec  2 20:51:44 2018                          */
;*    Last change :  Mon Dec  3 11:15:10 2018 (serrano)                */
;*    Copyright   :  2018 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript proxy objects.               */
;*    -------------------------------------------------------------    */
;*    https://developer.mozilla.org/en-US/docs/Web/JavaScript/         */
;*       Reference/Global_Objects/Proxy                                */ 
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_proxy
   
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
   
   (export (js-init-proxy! ::JsGlobalObject)
	   (js-proxy-debug-name::bstring ::JsProxy)))

;*---------------------------------------------------------------------*/
;*    js-init-proxy! ...                                               */
;*---------------------------------------------------------------------*/
(define (js-init-proxy! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (__proto__ js-function-prototype js-proxy)

      (define (js-proxy-alloc constructor::JsFunction)
	 (instantiateJsProxy
	    (__proto__ (js-get constructor 'prototype %this))))

      (define (js-proxy-construct this::JsProxy t h)
	 (with-access::JsProxy this (target handler)
	    (set! target t)
	    (set! handler h))
	 this)

      ;; create a HopScript object
      (define (%js-proxy this . args)
	 (js-raise-type-error %this "Constructor Proxy requires 'new'" this))
;* 	 (apply js-proxy-construct (js-proxy-alloc js-proxy) args))    */
      
      (set! js-proxy
	 (js-make-function %this %js-proxy 2 'Proxy
	    :__proto__ js-function-prototype
	    :prototype '()
	    :alloc js-proxy-alloc
	    :construct js-proxy-construct))

      ;; bind Proxy in the global object
      (js-bind! %this %this 'Proxy
	 :configurable #f :enumerable #f :value js-proxy :hidden-class #t)
	 
      js-proxy))

;*---------------------------------------------------------------------*/
;*    js-proxy-debug-name ...                                          */
;*---------------------------------------------------------------------*/
(define (js-proxy-debug-name::bstring obj::JsProxy)
   (with-access::JsProxy obj (target)
      (if (isa? target JsFunction)
	  (js-function-debug-name target)
	  "proxy")))

;*---------------------------------------------------------------------*/
;*    js-for-in ::JsProxy ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-for-in obj::JsProxy proc %this)
   (with-access::JsProxy obj (target)
      (js-for-in target proc %this)))

;*---------------------------------------------------------------------*/
;*    js-get ::JsProxy ...                                             */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsProxy prop %this::JsGlobalObject)
   (with-access::JsProxy o (target handler)
      (let ((get (js-get handler 'get %this)))
	 (if (isa? get JsFunction)
	     (js-call3 %this get handler target
		(js-string->jsstring (symbol->string! prop))
		o)
	     (js-get target prop %this)))))

;*---------------------------------------------------------------------*/
;*    js-object-get-name/cache-miss ::JsProxy ...                      */
;*---------------------------------------------------------------------*/
(define-method (js-object-get-name/cache-miss o::JsProxy prop::obj
		   throw::bool %this::JsGlobalObject
		   cache::JsPropertyCache
		   #!optional (point -1) (cspecs '()))
   (js-get o prop %this))

;*---------------------------------------------------------------------*/
;*    js-object-get-lookup ::JsProxy ...                               */
;*---------------------------------------------------------------------*/
(define-method (js-object-get-lookup o::JsProxy prop::obj throw::bool
		  %this::JsGlobalObject
		  cache::JsPropertyCache point::long cspecs::pair-nil)
   (js-get o prop %this))

;*---------------------------------------------------------------------*/
;*    js-put! ::JsProxy ...                                            */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::JsProxy prop v throw %this::JsGlobalObject)
   (with-access::JsProxy o (target handler)
      (let ((set (js-get handler 'set %this)))
	 (if (isa? set JsFunction)
	     (js-call3 %this set handler target
		(js-string->jsstring (symbol->string! prop)) v)
	     (js-put! target prop v throw %this)))))

;*---------------------------------------------------------------------*/
;*    js-object-get-name/cache-miss ::JsProxy ...                      */
;*---------------------------------------------------------------------*/
(define-method (js-object-get-name/cache-miss obj::JsProxy name::obj
		  throw::bool %this::JsGlobalObject
		  cache::JsPropertyCache
		  #!optional (point -1) (cspecs '()))
   (with-access::JsProxy obj (target)
      (js-object-get-name/cache-miss target name throw %this cache point cspecs)))

;*---------------------------------------------------------------------*/
;*    js-delete! ::JsProxy ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-delete! o::JsProxy p throw %this)
   (with-access::JsProxy o (target handler)
      (let ((delete (js-get handler 'deleteProperty %this)))
	 (if (isa? delete JsFunction)
	     (js-call1 %this delete target p)
	     (js-delete! target p throw %this)))))
	  
