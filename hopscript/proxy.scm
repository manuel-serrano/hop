;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/proxy.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec  2 20:51:44 2018                          */
;*    Last change :  Sat Dec  8 19:04:00 2018 (serrano)                */
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
	   (js-proxy-property-value ::JsProxy ::JsObject ::obj ::JsGlobalObject)
	   (js-proxy-property-value-set! ::JsProxy ::JsObject ::obj ::JsGlobalObject ::obj)))

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
	     (let ((v (js-call3 %this get handler target
			 (js-string->jsstring (symbol->string! prop))
			 owner)))
		(if (null? (js-object-properties target))
		    v
		    (proxy-check-property-value target owner prop %this v 'get)))
	     (js-get-jsobject target owner prop %this)))))

;*---------------------------------------------------------------------*/
;*    js-proxy-property-value-set! ...                                 */
;*---------------------------------------------------------------------*/
(define (js-proxy-property-value-set! o::JsProxy owner::JsObject prop %this::JsGlobalObject v)
   (with-access::JsProxy o (target handler)
      (let ((set (js-get handler 'set %this)))
	 (when (isa? set JsFunction)
	    (let ((v (js-call4 %this set handler target
			(js-string->jsstring (symbol->string! prop))
			v
			owner)))
	       (if (js-totest v)
		   v
		   (js-raise-type-error %this "Proxy \"set\" returns false on property \"~a\""
		      prop)))))))

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
	     (begin
		(proxy-check-property-value target target prop %this v 'set)
		(js-call4 %this set handler target
		   (js-string->jsstring (symbol->string! prop)) v o))
	     (js-put! target prop v throw %this)))))

;*---------------------------------------------------------------------*/
;*    js-delete! ::JsProxy ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-delete! o::JsProxy p throw %this)
   (with-access::JsProxy o (target handler)
      (let ((delete (js-get handler 'deleteProperty %this)))
	 (if (isa? delete JsFunction)
	     (let ((r (js-call2 %this delete o target p)))
		(proxy-check-property-delete target p %this r))
	     (js-delete! target p throw %this)))))

;*---------------------------------------------------------------------*/
;*    js-has-property ::JsProxy ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-has-property o::JsProxy p::obj %this)
   (with-access::JsProxy o (target handler)
      (let ((has (js-get handler 'has %this)))
	 (if (isa? has JsFunction)
	     (let ((v (js-call2 %this has o target
			 (js-string->jsstring (symbol->string! p)))))
		(or v (proxy-check-property-has target p %this v)))
	     (js-has-property target p %this)))))

;*---------------------------------------------------------------------*/
;*    js-has-own-property ::JsProxy ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-has-own-property o::JsProxy p::obj %this)
   (with-access::JsProxy o (target handler)
      (let ((has (js-get handler 'has %this)))
	 (if (isa? has JsFunction)
	     (js-call2 %this has o target
		(js-string->jsstring (symbol->string! p)))
	     (js-has-own-property target p %this)))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property ::JsProxy ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property o::JsProxy p::obj %this)
   (with-access::JsProxy o (target handler)
      (let ((get (js-get handler 'getOwnPropertyDescriptor %this)))
	 (if (isa? get JsFunction)
	     (let ((desc (js-call2 %this get o target p)))
		(proxy-check-property-getown target p %this desc))
	     (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    js-for-in ::JsProxy ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-for-in obj::JsProxy proc %this)
   (with-access::JsProxy obj (target)
      (js-for-in target proc %this)))

;*---------------------------------------------------------------------*/
;*    proxy-check-property-value ...                                   */
;*---------------------------------------------------------------------*/
(define (proxy-check-property-value target owner prop %this v get-or-set)
   (let ((prop (js-get-own-property target prop %this)))
      (if (eq? prop (js-undefined))
	  v
	  (with-access::JsPropertyDescriptor prop (configurable)
	     (cond
		(configurable
		 v)
		((isa? prop JsValueDescriptor)
		 (with-access::JsValueDescriptor prop (writable value)
		    (if (or writable (js-strict-equal? value v))
			v
			(js-raise-type-error %this "Proxy \"get\" inconsitency"
			   owner))))
		((isa? prop JsAccessorDescriptor)
		 (with-access::JsAccessorDescriptor prop (get set)
		    (cond
		       ((and (eq? get (js-undefined)) (eq? get-or-set 'get))
			(js-raise-type-error %this "Proxy \"get\" inconsitency"
			   owner))
		       ((and (eq? set (js-undefined)) (eq? get-or-set 'set))
			(js-raise-type-error %this "Proxy \"set\" inconsitency"
			   owner))
		       (else
			v))))
		(else
		 v))))))

;*---------------------------------------------------------------------*/
;*    proxy-check-property-has ...                                     */
;*---------------------------------------------------------------------*/
(define (proxy-check-property-has target prop %this v)
   (let ((prop (js-get-own-property target prop %this)))
      (if (eq? prop (js-undefined))
	  v
	  (with-access::JsPropertyDescriptor prop (configurable)
	     (if (and configurable (js-object-mode-extensible? target))
		 v
		 (js-raise-type-error %this "Proxy \"has\" inconsitency"
		    target))))))

;*---------------------------------------------------------------------*/
;*    proxy-check-property-delete ...                                  */
;*---------------------------------------------------------------------*/
(define (proxy-check-property-delete target prop %this r)
   (when r
      (let ((prop (js-get-own-property target prop %this)))
	 (unless (eq? prop (js-undefined))
	    (with-access::JsPropertyDescriptor prop (configurable)
	       (unless configurable
		  (js-raise-type-error %this "Proxy \"delete\" inconsitency"
		     target)))))))

;*---------------------------------------------------------------------*/
;*    proxy-check-property-getown ...                                  */
;*---------------------------------------------------------------------*/
(define (proxy-check-property-getown target prop %this desc)
   
   (define (err)
      (js-raise-type-error %this
	 "Proxy \"getOwnPropertyDescriptor\" inconsitency"
	 target))
   
   (cond
      ((eq? desc (js-undefined))
       (let ((prop (js-get-own-property target prop %this)))
	  (cond
	     ((not (eq? prop (js-undefined)))
	      (with-access::JsPropertyDescriptor prop (configurable)
		 (if (js-totest configurable)
		     (if (js-object-mode-extensible? target)
			 desc
			 (err))
		     (err))))
	     (else
	      (err)))))
      ((js-object-mode-extensible? target)
       (let ((conf (js-get desc 'configurable %this)))
	  (if (js-totest conf)
	      desc
	      (let ((prop (js-get-own-property target prop %this)))
		 (cond
		    ((eq? prop (js-undefined))
		     (err))
		    ((js-totest (js-get prop 'configurable %this))
		     (err))
		    (else
		     desc))))))
      (else
       (err))))

