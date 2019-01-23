;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/proxy.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec  2 20:51:44 2018                          */
;*    Last change :  Wed Jan 23 08:39:39 2019 (serrano)                */
;*    Copyright   :  2018-19 Manuel Serrano                            */
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
	   (js-proxy-debug-name::bstring ::JsProxy ::JsGlobalObject)
	   (js-proxy-property-descriptor-index ::JsProxy ::obj)
	   (js-proxy-property-descriptor ::JsProxy ::obj)
	   (js-proxy-property-value ::JsProxy ::JsObject ::obj ::JsGlobalObject)
	   (js-proxy-property-value-set! ::JsProxy ::JsObject ::obj ::JsGlobalObject ::obj)))

;*---------------------------------------------------------------------*/
;*    js-init-proxy! ...                                               */
;*---------------------------------------------------------------------*/
(define (js-init-proxy! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (__proto__ js-function-prototype js-proxy)

      (define (js-proxy-alloc %this constructor::JsFunction)
	 (instantiateJsProxy
	    (__proto__ (js-get constructor 'prototype %this))
	    (elements (vector #unspecified))))

      (define (js-proxy-construct this::JsProxy t h)
	 (with-access::JsProxy this (target handler)
	    (set! target t)
	    (set! handler h))
	 this)

      (define js-proxy-revoke
	 (js-make-function %this
	    (lambda (this)
	       (let ((prox (js-get this 'proxy %this)))
		  (if (isa? prox JsProxy)
		      (with-access::JsProxy prox (revoked)
			 (set! revoked #t))
		      (js-raise-type-error %this
			 "Not a Revocable proxy" this))))
	    0 "revoke"
	    :prototype '()))

      ;; create a HopScript object
      (define (%js-proxy this . args)
	 (js-raise-type-error %this "Constructor Proxy requires 'new'" this))

      ;; create a revokable proxy
      (define (%js-revocable this t h)
	 (js-plist->jsobject
	    `(:proxy ,(js-proxy-construct (js-proxy-alloc %this js-proxy) t h)
		:revoke ,js-proxy-revoke)
	    %this))

      (set! js-proxy
	 (js-make-function %this %js-proxy 2 "Proxy"
	    :__proto__ js-function-prototype
	    :prototype '()
	    :alloc js-proxy-alloc
	    :size 1
	    :construct js-proxy-construct))

      (js-bind! %this js-proxy 'revocable
	 :writable #t :configurable #t :enumerable #f
	 :value (js-make-function %this %js-revocable 2 "revocable"
		   :__proto__ js-function-prototype
		   :prototype '())
	 :hidden-class #t)

      ;; bind Proxy in the global object
      (js-bind! %this %this 'Proxy
	 :writable #t :configurable #t :enumerable #f
	 :value js-proxy :hidden-class #t)
	 
      js-proxy))

;*---------------------------------------------------------------------*/
;*    js-proxy-debug-name ...                                          */
;*---------------------------------------------------------------------*/
(define (js-proxy-debug-name::bstring obj::JsProxy %this)
   (with-access::JsProxy obj (target)
      (if (isa? target JsFunction)
	  (js-function-debug-name target %this)
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
;*    symbol->pname ...                                                */
;*---------------------------------------------------------------------*/
(define (symbol->pname obj)
   (if (symbol? obj)
       (js-string->jsstring (symbol->string! obj))
       obj))

;*---------------------------------------------------------------------*/
;*    js-proxy-property-value ...                                      */
;*---------------------------------------------------------------------*/
(define (js-proxy-property-value o::JsProxy owner::JsObject prop %this::JsGlobalObject)
   (with-access::JsProxy o (target handler)
      (let ((get (js-get handler 'get %this)))
	 (if (isa? get JsFunction)
	     (let ((v (js-call3 %this get handler target
			 (symbol->pname prop)
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
			(symbol->pname prop)
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
   (proxy-check-revoked! o "get" %this)
   (js-proxy-property-value o o prop %this))

;*---------------------------------------------------------------------*/
;*    js-put! ::JsProxy ...                                            */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::JsProxy prop v throw %this::JsGlobalObject)
   (proxy-check-revoked! o "put" %this)
   (with-access::JsProxy o (target handler)
      (let ((set (js-get handler 'set %this)))
	 (if (isa? set JsFunction)
	     (begin
		(proxy-check-property-value target target prop %this v 'set)
		(js-call4 %this set handler target
		   (symbol->pname prop) v o))
	     (js-put! target prop v throw %this)))))

;*---------------------------------------------------------------------*/
;*    js-delete! ::JsProxy ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-delete! o::JsProxy p throw %this)
   (proxy-check-revoked! o "delete" %this)
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
			 (symbol->pname p))))
		(or v (proxy-check-property-has target p %this v)))
	     (js-has-property target p %this)))))

;*---------------------------------------------------------------------*/
;*    js-has-own-property ::JsProxy ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-has-own-property o::JsProxy p::obj %this)
   (proxy-check-revoked! o "has" %this)
   (with-access::JsProxy o (target handler)
      (let ((has (js-get handler 'has %this)))
	 (if (isa? has JsFunction)
	     (js-call2 %this has o target
		(symbol->pname p))
	     (js-has-own-property target p %this)))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property ::JsProxy ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property o::JsProxy p::obj %this)
   (proxy-check-revoked! o "getOwn" %this)
   (with-access::JsProxy o (target handler)
      (let ((get (js-get handler 'getOwnPropertyDescriptor %this)))
	 (if (isa? get JsFunction)
	     (let ((desc (js-call2 %this get o target p)))
		(proxy-check-property-getown target p %this desc))
	     (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    js-for-in ::JsProxy ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-for-in o::JsProxy proc %this)
   (proxy-check-revoked! o "for..in" %this)
   (with-access::JsProxy o (target)
      (js-for-in target proc %this)))

;*---------------------------------------------------------------------*/
;*    js-define-own-property ::JsProxy ...                             */
;*---------------------------------------------------------------------*/
(define-method (js-define-own-property::bool o::JsProxy p
		  desc::JsPropertyDescriptor throw::bool %this)
   (proxy-check-revoked! o "defineProperty" %this)
   (with-access::JsProxy o (target handler)
      (let ((def (js-get handler 'defineProperty %this)))
	 (if (isa? def JsFunction)
	     (let ((v (js-call3 %this def o target
			 (symbol->pname p) desc)))
		(proxy-check-property-defprop target o p %this desc v))
	     (js-define-own-property target p desc throw %this)))))

;*---------------------------------------------------------------------*/
;*    js-getprototypeof ::JsProxy ...                                  */
;*---------------------------------------------------------------------*/
(define-method (js-getprototypeof o::JsProxy %this::JsGlobalObject msg::obj)
   (proxy-check-revoked! o "getPrototypeOf" %this)
   (with-access::JsProxy o (target handler)
      (let ((get (js-get handler 'getPrototypeOf %this)))
	 (if (isa? get JsFunction)
	     (let ((v (js-call1 %this get o target)))
		(proxy-check-property-getproto target o %this msg v))
	     (js-getprototypeof target %this msg)))))

;*---------------------------------------------------------------------*/
;*    js-setprototypeof ::JsProxy ...                                  */
;*---------------------------------------------------------------------*/
(define-method (js-setprototypeof o::JsProxy v %this::JsGlobalObject msg::obj)
   (proxy-check-revoked! o "setPrototypeOf" %this)
   (with-access::JsProxy o (target handler)
      (let ((set (js-get handler 'setPrototypeOf %this)))
	 (if (isa? set JsFunction)
	     (let ((r (js-call2 %this set o target v)))
		(proxy-check-property-setproto target o v %this msg r))
	     (js-setprototypeof target v %this msg)))))

;*---------------------------------------------------------------------*/
;*    js-extensible? ::JsProxy ...                                     */
;*---------------------------------------------------------------------*/
(define-method (js-extensible? o::JsProxy %this::JsGlobalObject)
   (proxy-check-revoked! o "isExtensible" %this)
   (with-access::JsProxy o (target handler)
      (let ((ise (js-get handler 'isExtensible %this)))
	 (if (isa? ise JsFunction)
	     (let ((r (js-call1 %this ise o target)))
		(proxy-check-is-extensible target o %this r))
	     (js-extensible? target %this)))))

;*---------------------------------------------------------------------*/
;*    js-preventextensions ::JsProxy ...                               */
;*---------------------------------------------------------------------*/
(define-method (js-preventextensions o::JsProxy %this::JsGlobalObject)
   (proxy-check-revoked! o "preventExtensions" %this)
   (with-access::JsProxy o (target handler)
      (let ((p (js-get handler 'preventExtensions %this)))
	 (if (isa? p JsFunction)
	     (let ((r (js-call1 %this p o target)))
		(proxy-check-preventext target o %this r))
	     (js-preventextensions target %this)))))

;*---------------------------------------------------------------------*/
;*    js-ownkeys ::JsProxy ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-ownkeys o::JsProxy %this::JsGlobalObject)
   (proxy-check-revoked! o "ownKeys" %this)
   (with-access::JsProxy o (target handler)
      (let ((ownk (js-get handler 'ownKeys %this)))
	 (if (isa? ownk JsFunction)
	     (let ((r (js-call1 %this ownk o target)))
		(proxy-check-ownkeys target o %this r))
	     (js-ownkeys target %this)))))

;*---------------------------------------------------------------------*/
;*    proxy-check-revoked! ...                                         */
;*---------------------------------------------------------------------*/
(define (proxy-check-revoked! o::JsProxy action %this::JsGlobalObject)
   (with-access::JsProxy o (revoked)
      (when revoked
	 (js-raise-type-error %this
	    (format "Cannot perform \"~s\" on a revoked proxy" action)
	    (js-string->jsstring (typeof o))))))

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
			(js-raise-type-error %this "Proxy \"get\" inconsistency"
			   owner))))
		((isa? prop JsAccessorDescriptor)
		 (with-access::JsAccessorDescriptor prop (get set)
		    (cond
		       ((and (eq? get (js-undefined)) (eq? get-or-set 'get))
			(js-raise-type-error %this "Proxy \"get\" inconsistency"
			   owner))
		       ((and (eq? set (js-undefined)) (eq? get-or-set 'set))
			(js-raise-type-error %this "Proxy \"set\" inconsistency"
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
		 (js-raise-type-error %this "Proxy \"has\" inconsistency"
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
		  (js-raise-type-error %this "Proxy \"delete\" inconsistency"
		     target)))))))

;*---------------------------------------------------------------------*/
;*    proxy-check-property-getown ...                                  */
;*---------------------------------------------------------------------*/
(define (proxy-check-property-getown target prop %this desc)
   
   (define (err)
      (js-raise-type-error %this
	 "Proxy \"getOwnPropertyDescriptor\" inconsistency"
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

;*---------------------------------------------------------------------*/
;*    proxy-check-property-defprop ...                                 */
;*---------------------------------------------------------------------*/
(define (proxy-check-property-defprop target owner p %this desc v)
   (cond
      ((and (not (js-object-mode-extensible? target))
	    (eq? (js-get-own-property target p %this) (js-undefined)))
       (js-raise-type-error %this "Proxy \"defineProperty\" inconsistency"
	  target))
      ((and (eq? (js-get desc 'configurable %this) #f)
	    (let ((odesc (js-get-own-property target p %this)))
	       (and (not (eq? odesc (js-undefined)))
		    (not (eq? (js-get odesc 'configurable %this) #f)))))
       (js-raise-type-error %this "Proxy \"defineProperty\" inconsistency"
	  target))
      (else
       v)))

;*---------------------------------------------------------------------*/
;*    proxy-check-property-getproto ...                                */
;*---------------------------------------------------------------------*/
(define (proxy-check-property-getproto target owner %this msg v)
   (cond
      ((and (not (js-object-mode-extensible? target))
	    (not (eq? (js-getprototypeof target %this msg) v)))
       (js-raise-type-error %this "Proxy \"getPrototypeOf\" inconsistency"
	  target))
      (else
       v)))

;*---------------------------------------------------------------------*/
;*    proxy-check-property-setproto ...                                */
;*---------------------------------------------------------------------*/
(define (proxy-check-property-setproto target owner v %this msg r)
   (cond
      ((and (not (js-object-mode-extensible? target))
	    (not (eq? (js-getprototypeof target %this msg) v)))
       (js-raise-type-error %this "Proxy \"setPrototypeOf\" inconsistency"
	  target))
      (else
       r)))

;*---------------------------------------------------------------------*/
;*    proxy-check-is-extensible ...                                    */
;*---------------------------------------------------------------------*/
(define (proxy-check-is-extensible target o %this r)
   (if (eq? (js-extensible? target %this) r)
       r
       (js-raise-type-error %this "Proxy \"isExtensible\" inconsistency"
	  target)))

;*---------------------------------------------------------------------*/
;*    proxy-check-preventext ...                                       */
;*---------------------------------------------------------------------*/
(define (proxy-check-preventext target o %this r)
   (if (eq? r (js-extensible? target %this))
       (js-raise-type-error %this "Proxy \"preventExtensions\" inconsistency"
	  target)
       r))

;*---------------------------------------------------------------------*/
;*    proxy-check-ownkeys ...                                          */
;*---------------------------------------------------------------------*/
(define (proxy-check-ownkeys target o %this r)
   
   (define (err)
      (js-raise-type-error %this "Proxy \"ownKeys\" inconsistency"
	 target))
   
   (define (all-symbol-or-string? r)
      (js-for-of r
	 (lambda (el %this)
	    (unless (or (js-jsstring? el) (isa? el JsSymbol))
	       (err)))
	 #t %this)
      #f)
   
   (define (find-in? name vec)
      (let loop ((i (-fx (vector-length vec) 1)))
	 (cond
	    ((=fx i -1) #f)
	    ((js-jsstring=? (vector-ref vec i) name) #t)
	    (else (loop (-fx i 1))))))
   
   (define (same-list names r)
      (when (=uint32 (fixnum->uint32 (vector-length names))
	       (js-array-length r))
	 (js-for-of r
	    (lambda (el %this)
	       (unless (find-in? el names)
		  (err)))
	    #t %this)
	 r))
   
   (cond
      ((not (isa? r JsArray))
       (err))
      ((all-symbol-or-string? r)
       (err))
      ((null? (js-object-properties target))
       (if (js-extensible? target %this)
	   r
	   (same-list (js-properties-name target #t %this) r)))
      (else
       (let ((names (js-properties-name target #t %this)))
	  (let loop ((i (-fx (vector-length names) 1)))
	     (if (=fx i -1)
		 (if (js-extensible? target %this)
		     r
		     (same-list names r))
		 (let* ((name (vector-ref names i))
			(p (js-get-own-property target name %this)))
		    (if (eq? p (js-undefined))
			(loop (-fx i 1))
			(with-access::JsPropertyDescriptor p (configurable)
			   (cond
			      (configurable
			       (loop (-fx i 1)))
			      ((find-in? name r)
			       (loop (-fx i 1)))
			      (else
			       (err))))))))))))
