;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/proxy.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec  2 20:51:44 2018                          */
;*    Last change :  Wed Dec  5 14:57:39 2018 (serrano)                */
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
	   (js-proxy-debug-name::bstring ::JsProxy)
	   (js-proxy-property-descriptor-index ::JsProxy ::obj)
	   (js-proxy-property-descriptor ::JsProxy ::obj)
	   (js-proxy-property-value ::JsProxy ::JsObject ::obj ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    js-init-proxy! ...                                               */
;*---------------------------------------------------------------------*/
(define (js-init-proxy! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (__proto__ js-function-prototype js-proxy)

      (define (js-proxy-alloc constructor::JsFunction)
	 (instantiateJsProxy
	    (__proto__ (js-get constructor 'prototype %this))
	    (elements (vector #unspecified))))

      (define (js-proxy-construct this::JsProxy t h)
	 (with-access::JsProxy this (target handler)
	    (set! target t)
	    (set! handler h))
	 this)

      ;; create a HopScript object
      (define (%js-proxy this . args)
	 (js-raise-type-error %this "Constructor Proxy requires 'new'" this))
      
      (set! js-proxy
	 (js-make-function %this %js-proxy 2 'Proxy
	    :__proto__ js-function-prototype
	    :prototype '()
	    :alloc js-proxy-alloc
	    :construct js-proxy-construct))

      ;; bind Proxy in the global object
      (js-bind! %this %this 'Proxy
	 :writable #t :configurable #t :enumerable #f
	 :value js-proxy :hidden-class #t)
	 
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
;*    js-proxy-property-descriptor ...                                 */
;*    -------------------------------------------------------------    */
;*    Returns a fake generic property descriptor unique to the         */
;*    proxy object.                                                    */
;*---------------------------------------------------------------------*/
(define (js-proxy-property-descriptor obj::JsProxy prop)
   (with-access::JsProxy obj (elements)
      (if (eq? (vector-ref elements 0) #unspecified)
	  (let ((descr (instantiate::JsProxyDescriptor
			  (name prop)
			  (proxy obj))))
	     (vector-set! elements 0 descr)
	     descr)
	  (let ((descr (vector-ref elements 0)))
	     (with-access::JsProxyDescriptor descr (name)
		(set! name prop))
	     descr))))

;*---------------------------------------------------------------------*/
;*    js-proxy-property-descriptor-index ...                           */
;*    -------------------------------------------------------------    */
;*    Returns a fake generic property descriptor unique to the         */
;*    proxy object.                                                    */
;*    -------------------------------------------------------------    */
;*    This function returns a fake property descriptor that is shared  */
;*    for all proxy accesses. This works only because of               */
;*    single-threaded JavaScript execution.                            */
;*---------------------------------------------------------------------*/
(define (js-proxy-property-descriptor-index obj::JsProxy prop)
   (js-proxy-property-descriptor obj prop)
   0)

;*---------------------------------------------------------------------*/
;*    js-proxy-property-value ...                                      */
;*---------------------------------------------------------------------*/
(define (js-proxy-property-value o::JsProxy owner::JsObject prop %this::JsGlobalObject)
   (with-access::JsProxy o (target handler)
      (let ((get (js-get handler 'get %this)))
	 (if (isa? get JsFunction)
	     (js-call3 %this get handler target
		(js-string->jsstring (symbol->string! prop))
		owner)
	     (js-get-jsobject target owner prop %this)))))

;*---------------------------------------------------------------------*/
;*    js-get ::JsProxy ...                                             */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsProxy prop %this::JsGlobalObject)
   (js-proxy-property-value o o prop %this))

;* {*---------------------------------------------------------------------*} */
;* {*    js-object-get-name/cache-miss ::JsProxy ...                      *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (js-object-get-name/cache-miss o::JsProxy prop::obj  */
;* 		   throw::bool %this::JsGlobalObject                   */
;* 		   cache::JsPropertyCache                              */
;* 		   #!optional (point -1) (cspecs '()))                 */
;*    (js-get o prop %this))                                           */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    js-object-get-lookup ::JsProxy ...                               *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (js-object-get-lookup o::JsProxy prop::obj throw::bool */
;* 		  %this::JsGlobalObject                                */
;* 		  cache::JsPropertyCache point::long cspecs::pair-nil) */
;*    (js-get o prop %this))                                           */

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
;*    js-delete! ::JsProxy ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-delete! o::JsProxy p throw %this)
   (with-access::JsProxy o (target handler)
      (let ((delete (js-get handler 'deleteProperty %this)))
	 (if (isa? delete JsFunction)
	     (js-call1 %this delete target p)
	     (js-delete! target p throw %this)))))

;*---------------------------------------------------------------------*/
;*    js-has-property ::JsProxy ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-has-property o::JsProxy p::obj %this)
   (with-access::JsProxy o (target handler)
      (let ((has (js-get handler 'has %this)))
	 (when (isa? has JsFunction)
	    (js-call2 %this has o target
	       (js-string->jsstring (symbol->string! p)))))))

;*---------------------------------------------------------------------*/
;*    js-has-own-property ::JsProxy ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-has-own-property o::JsProxy p::obj %this)
   (with-access::JsProxy o (target handler)
      (let ((has (js-get handler 'has %this)))
	 (when (isa? has JsFunction)
	    (js-call2 %this has o target
	       (js-string->jsstring (symbol->string! p)))))))

;*---------------------------------------------------------------------*/
;*    js-for-in ::JsProxy ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-for-in obj::JsProxy proc %this)
   (with-access::JsProxy obj (target)
      (js-for-in target proc %this)))

