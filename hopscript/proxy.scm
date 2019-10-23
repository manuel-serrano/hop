;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/proxy.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec  2 20:51:44 2018                          */
;*    Last change :  Wed Oct 23 11:04:42 2019 (serrano)                */
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
   
   (include "types.sch" "stringliteral.sch" "property.sch")
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_lib
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_array
	   __hopscript_error
	   __hopscript_profile)
   
   (export (js-init-proxy! ::JsGlobalObject)
	   (js-new-proxy ::JsGlobalObject ::obj ::obj)
	   (js-new-proxy/caches ::JsGlobalObject ::obj ::obj
	      ::JsPropertyCache ::JsPropertyCache ::JsPropertyCache)
	   (js-proxy-debug-name::bstring ::JsProxy ::JsGlobalObject)
	   (js-proxy-property-value ::JsObject ::JsProxy ::JsStringLiteral ::JsGlobalObject)
	   (js-proxy-get ::JsProxy prop ::JsGlobalObject)
	   (js-object-proxy-get-name/cache-miss ::JsObject
	      ::JsStringLiteral ::bool ::JsGlobalObject ::JsPropertyCache)
	   (js-object-proxy-put-name/cache-miss! ::JsObject ::JsStringLiteral
	      ::obj ::bool
	      ::JsGlobalObject
	      ::JsPropertyCache #!optional (point -1))
	   (inline js-proxy-property-descriptor-index ::JsProxy ::obj)
	   (inline js-proxy-typeof ::JsProxy ::JsGlobalObject)
	   (js-call-proxy/cache-miss0 ::JsGlobalObject
	      ::JsProxy ::obj)
	   (js-call-proxy/cache-miss1 ::JsGlobalObject
	      ::JsProxy ::obj a0)
	   (js-call-proxy/cache-miss2 ::JsGlobalObject
	      ::JsProxy ::obj a0 a1)
	   (js-call-proxy/cache-miss3 ::JsGlobalObject
	      ::JsProxy ::obj a0 a1 a2)
	   (js-call-proxy/cache-miss4 ::JsGlobalObject
	      ::JsProxy ::obj a0 a1 a2 a3)
	   (js-call-proxy/cache-miss5 ::JsGlobalObject
	      ::JsProxy ::obj a0 a1 a2 a3 a4)
	   (js-call-proxy/cache-miss6 ::JsGlobalObject
	      ::JsProxy ::obj a0 a1 a2 a3 a4 a5)
	   (js-call-proxy/cache-miss7 ::JsGlobalObject
	      ::JsProxy ::obj a0 a1 a2 a3 a4 a5 a6)
	   (js-call-proxy/cache-miss8 ::JsGlobalObject
	      ::JsProxy ::obj a0 a1 a2 a3 a4 a5 a6 a7)
	   (js-call-proxy/cache-miss9 ::JsGlobalObject
	      ::JsProxy ::obj a0 a1 a2 a3 a4 a5 a6 a7 a8)
	   (js-call-proxy/cache-miss10 ::JsGlobalObject
	      ::JsProxy ::obj a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    js-debug-object ::JsProxy ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-debug-object obj::JsProxy #!optional (msg ""))
   (with-access::JsProxy obj ((target __proto__) handler)
      (fprint (current-error-port) ">>> JsProxy" msg)
      (fprint (current-error-port) ">>> target: ")
      (js-debug-object target)
      (fprint (current-error-port) ">>> handler: ")
      (js-debug-object handler)))

;*---------------------------------------------------------------------*/
;*    jsarray ...                                                      */
;*---------------------------------------------------------------------*/
(define-macro (jsarray %this . args)
   `(let ((a (js-array-construct-alloc-small-sans-init ,%this
		 ,(fixnum->uint32 (length args)))))
       (with-access::JsArray a (vec ilen length)
	  (let ((v vec))
	     ,@(map (lambda (i o)
		       `(vector-set! v ,i ,o))
		  (iota (length args)) args)
	     (set! ilen ,(fixnum->uint32 (length args)))
	     a))))

;*---------------------------------------------------------------------*/
;*    local caches                                                     */
;*---------------------------------------------------------------------*/
(define proxy-cmap
   ;; this cmap is shared by all proxy objects
   (instantiate::JsConstructMap
      (methods '#(#f))
      (props '#())))

(define proxy-elements #f)

;*---------------------------------------------------------------------*/
;*    js-init-proxy! ...                                               */
;*---------------------------------------------------------------------*/
(define (js-init-proxy! %this::JsGlobalObject)
   
   (unless (vector? __js_strings) (set! __js_strings (&init!)))

   (set! proxy-elements
      (vector
	 (instantiate::JsWrapperDescriptor
	    (writable #t)
	    (configurable #t)
	    (enumerable #t)
	    (name (& "%%proxy"))
	    (%get js-proxy-property-value)
	    (%set js-proxy-property-value-set!))))

   (with-access::JsGlobalObject %this (js-function-prototype js-proxy js-proxy-pcache)

      (define (js-proxy-alloc %this constructor::JsFunction)
	 ;; not used in optimized code, see below
	 ;; js-new-proxy and js-new-proxy/caches
	 (instantiateJsProxy
	    (cmap proxy-cmap)
	    (__proto__ (js-null))
	    (elements proxy-elements)))

      (define (js-proxy-construct this::JsProxy t h)
	 (cond
	    ((not (js-object? h))
	     (js-raise-type-error %this
		"Cannot create proxy with a non-object as handler" this))
	    ((not (js-object? t))
	     (js-raise-type-error %this
		"Cannot create proxy with a non-object as target" this))
	    (else
	     (with-access::JsProxy this ((target __proto__) handler id)
		(set! target t)
		(set! handler h))))
	 this)

      (define js-proxy-revoke
	 (js-make-function %this
	    (lambda (this)
	       (let ((prox (js-get this (& "proxy") %this)))
		  (if (js-proxy? prox)
		      (js-object-mode-revoked-set! prox #t)
		      (js-raise-type-error %this
			 "Not a Revocable proxy" this))))
	    0 "revoke"
	    :prototype '()))

      ;; create a HopScript object
      (define (%js-proxy this t h)
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

      (set! js-proxy-pcache
	 ((@ js-make-pcache-table __hopscript_property) 3 "proxy"))
      
      (js-bind! %this js-proxy (& "revocable")
	 :writable #t :configurable #t :enumerable #f
	 :value (js-make-function %this %js-revocable 2 "revocable"
		   :__proto__ js-function-prototype
		   :prototype '())
	 :hidden-class #t)

      ;; bind Proxy in the global object
      (js-bind! %this %this (& "Proxy")
	 :writable #t :configurable #t :enumerable #f
	 :value js-proxy :hidden-class #t)
	 
      js-proxy))

;*---------------------------------------------------------------------*/
;*    js-new-proxy ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-new-proxy %this t h)
   (instantiateJsProxy
      (cmap proxy-cmap)
      (__proto__ t)
      (elements proxy-elements)
      (handler h)))

;*---------------------------------------------------------------------*/
;*    js-new-proxy/caches ...                                          */
;*---------------------------------------------------------------------*/
(define (js-new-proxy/caches %this t h gcache scache acache)
   (instantiateJsProxy
      (cmap proxy-cmap)
      (__proto__ t)
      (elements proxy-elements)
      (handler h)
      (getcache gcache)
      (setcache scache)
      (applycache acache)))

;*---------------------------------------------------------------------*/
;*    js-proxy-debug-name ...                                          */
;*---------------------------------------------------------------------*/
(define (js-proxy-debug-name::bstring obj::JsProxy %this)
   (with-access::JsProxy obj ((target __proto__))
      (if (js-function? target)
	  (js-function-debug-name target %this)
	  "proxy")))

;*---------------------------------------------------------------------*/
;*    js-proxy-property-descriptor-index ...                           */
;*    -------------------------------------------------------------    */
;*    Returns a fake generic property descriptor unique to the         */
;*    proxy object.                                                    */
;*---------------------------------------------------------------------*/
(define-inline (js-proxy-property-descriptor-index obj::JsProxy prop)
   0)

;*---------------------------------------------------------------------*/
;*    js-proxy-property-value ...                                      */
;*    -------------------------------------------------------------    */
;*    Although almost similar to JS-GET ::JsProxy, this code differs   */
;*    when no proxy GET handler is defined and then two different      */
;*    functions have to be used.                                       */
;*---------------------------------------------------------------------*/
(define (js-proxy-property-value obj proxy prop %this)
   
   (define (check target v)
      (cond
	 ((js-object-mode-plain? target)
	  v)
	 (else
	  (proxy-check-property-value target obj prop %this v (& "get")))))

   (with-access::JsProxy proxy ((target __proto__) handler getcache)
      (proxy-check-revoked! proxy "get" %this)
	 (let ((get (js-object-get-name/cache handler (& "get") #f %this
		       getcache -1 '(imap emap cmap pmap amap vtable))))
	    (cond
	       ((and (object? get) (eq? (object-class get) JsFunction3))
		(with-access::JsFunction get (procedure)
		   (check target
		      (js-call3% %this get procedure handler target prop obj))))
	       ((js-function? get)
		(check target
		   (js-call4 %this get handler target prop obj proxy)))
	       ((eq? get (js-undefined))
		;; the difference with JS-GET is here...
		(js-get-jsobject target obj prop %this))
	       ((js-proxy? get)
		(check target
		   (js-call3 %this get handler target prop obj)))
	       (else
		(js-raise-type-error %this "not a function" get))))))

;*---------------------------------------------------------------------*/
;*    js-proxy-property-value-set! ...                                 */
;*---------------------------------------------------------------------*/
(define (js-proxy-property-value-set! obj v proxy prop %this)
   
   (define (check target v r)
      (cond
	 ((not (js-totest r))
	  (js-raise-type-error %this
	     "Proxy \"set\" returns false on property \"~a\""
	     prop))
	 ((null? (js-object-properties target))
	  r)
	 (else
	  (proxy-check-property-value target obj prop %this v (& "set")))))

   (with-access::JsProxy proxy ((target __proto__) handler setcache)
      (proxy-check-revoked! proxy "put" %this)
      (let ((set (js-object-get-name/cache handler (& "set") #f %this
		    setcache -1 '(imap emap cmap pmap amap vtable))))
	 (cond
	    ((js-function? set)
	     (with-access::JsFunction set (procedure)
		(check target v
		   (js-call4% %this set procedure handler target prop v obj))))
	    ((eq? set (js-undefined))
	     (if (eq? proxy obj)
		 (js-put/cache! target prop v #f %this)
		 (js-absent)))
	    ((js-proxy? set)
	     (check target v (js-call4 %this set handler target prop v obj)))
	    ((eq? proxy obj)
	     (js-put/cache! target prop v #f %this))
	    (else
	     (js-absent))))))

;*---------------------------------------------------------------------*/
;*    js-proxy-typeof ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (js-proxy-typeof o::JsProxy %this::JsGlobalObject)
   (with-access::JsProxy o ((target __proto__))
      (js-typeof target %this)))

;*---------------------------------------------------------------------*/
;*    js-jsproxy-get ...                                               */
;*    -------------------------------------------------------------    */
;*    This function is called when an inline cache is armed for        */
;*    proxy (amap cache hit) and when the proxy wrapper accesor        */
;*    is called (see property_expd.sch).                               */
;*---------------------------------------------------------------------*/
(define-inline (js-jsproxy-get proxy::JsProxy prop %this::JsGlobalObject)

   (define (check o target v)
      (if (js-object-mode-plain? target)
	  v
	  (proxy-check-property-value target o prop %this v (& "get"))))

   (with-access::JsProxy proxy ((target __proto__) handler getcache)
      (proxy-check-revoked! proxy "get" %this)
      (let ((get (js-object-get-name/cache handler (& "get") #f %this
		    getcache -1 '(imap emap cmap pmap amap vtable))))
	 (cond
	    ((and (object? get) (eq? (object-class get) JsFunction3))
	     (with-access::JsFunction get (procedure)
		(check proxy target
		   (procedure handler target prop))))
	    ((js-function? get)
	     (check proxy target
		(js-call3 %this get handler target prop proxy)))
	    ((eq? get (js-undefined))
	     (js-get-jsobject target proxy (js-toname prop %this) %this))
	    ((js-proxy? get)
	     (check proxy target
		(js-call3 %this get handler target prop proxy)))
	    (else
	     (js-raise-type-error %this "not a function" get))))))

;*---------------------------------------------------------------------*/
;*    js-proxy-get ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-proxy-get proxy::JsProxy prop %this::JsGlobalObject)
   (js-jsproxy-get proxy prop %this))

;*---------------------------------------------------------------------*/
;*    js-get ::JsProxy ...                                             */
;*    -------------------------------------------------------------    */
;*    See JS-PROXY-PROPERTY-VALUE.                                     */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsProxy prop %this::JsGlobalObject)
   (let ((name (js-toname prop %this)))
      (js-profile-log-get name -1)
      (js-jsproxy-get o name %this)))

;*---------------------------------------------------------------------*/
;*    js-object-proxy-get-name/cache-miss ...                          */
;*    -------------------------------------------------------------    */
;*    The performance of cache misses only matters for proxy object.   */
;*    The purpose of this function is to favor them by eliminating the */
;*    cost of an expensive generic function dispatch.                  */
;*---------------------------------------------------------------------*/
(define (js-object-proxy-get-name/cache-miss o::JsObject
		   name::JsStringLiteral
		   throw::bool %this::JsGlobalObject
		   cache::JsPropertyCache)
   (if (js-proxy? o)
       (js-jsproxy-get o name %this)
       (js-object-get-name/cache-miss o name throw %this cache)))

;*---------------------------------------------------------------------*/
;*    js-object-get-name/cache-miss ::JsProxy ...                      */
;*---------------------------------------------------------------------*/
(define-method (js-object-get-name/cache-miss proxy::JsProxy
		  prop::JsStringLiteral
		  throw::bool %this::JsGlobalObject
		  cache::JsPropertyCache)
   (js-jsproxy-get proxy prop %this))

;*---------------------------------------------------------------------*/
;*    js-proxy-put! ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-proxy-put! o::JsProxy prop::JsStringLiteral v::obj
		  throw %this::JsGlobalObject)
   (proxy-check-revoked! o "put" %this)
   (with-access::JsProxy o ((target __proto__) handler setcache)
      (let ((set (js-object-get-name/cache handler (& "set") #f %this
		    setcache point '(imap emap cmap pmap amap vtable))))
	 (cond
	    ((and (object? set) (eq? (object-class set) JsFunction4))
	     (unless (js-object-mode-plain? target)
		(proxy-check-property-value target target prop %this v (& "set")))
	     (with-access::JsFunction set (procedure)
		(procedure handler target prop v)))
	    ((js-function? set)
	     (unless (js-object-mode-plain? target)
		(proxy-check-property-value target target prop %this v (& "set")))
	     (js-call4 %this set handler target prop v o))
	    ((js-proxy? set)
	     (js-call4 %this set handler target prop v o))
	    (else
	     (js-put! target prop v throw %this))))))

;*---------------------------------------------------------------------*/
;*    js-put! ::JsProxy ...                                            */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::JsProxy prop v throw %this::JsGlobalObject)
   (let ((name (js-toname prop %this)))
      (cond-expand (profile (js-profile-log-put name -1)))
      (js-proxy-put! o name v throw %this)))

;*---------------------------------------------------------------------*/
;*    js-put/cache! ::JsProxy ...                                      */
;*---------------------------------------------------------------------*/
(define-method (js-put/cache! o::JsProxy prop v::obj throw::bool %this
		  #!optional (point -1) (cspecs '()))
   (let ((name (js-toname prop %this)))
      (cond-expand (profile (js-profile-log-put name -1)))
      (js-proxy-put! o name v throw %this)))

;*---------------------------------------------------------------------*/
;*    js-object-proxy-put-name/cache-miss! ...                         */
;*---------------------------------------------------------------------*/
(define (js-object-proxy-put-name/cache-miss! o::JsObject prop::JsStringLiteral
	   v::obj throw::bool
	   %this::JsGlobalObject
	   cache::JsPropertyCache #!optional (point -1))
   (if (js-proxy? o)
       (js-proxy-put! o prop v throw %this)
       (js-object-put-name/cache-miss! o prop v throw %this cache point)))

;*---------------------------------------------------------------------*/
;*    js-object-put-name/cache-miss! ::JsProxy ...                     */
;*---------------------------------------------------------------------*/
(define-method (js-object-put-name/cache-miss! o::JsProxy prop::JsStringLiteral
	   v::obj throw::bool
	   %this::JsGlobalObject
	   cache::JsPropertyCache #!optional (point -1))
   (js-proxy-put! o prop v throw %this))

;*---------------------------------------------------------------------*/
;*    js-delete! ::JsProxy ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-delete! o::JsProxy p throw %this)
   (proxy-check-revoked! o "delete" %this)
   (with-access::JsProxy o ((target __proto__) handler)
      (let ((delete (js-get-jsobject handler handler (& "deleteProperty") %this)))
	 (if (js-function? delete)
	     (let ((r (js-call2 %this delete o target p)))
		(proxy-check-property-delete target p %this r))
	     (js-delete! target p throw %this)))))

;*---------------------------------------------------------------------*/
;*    js-has-property ::JsProxy ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-has-property o::JsProxy p::obj %this)
   (with-access::JsProxy o ((target __proto__) handler)
      (let ((has (js-get-jsobject handler handler (& "has") %this)))
	 (if (js-function? has)
	     (let ((v (js-call2 %this has o target p)))
		(or v (proxy-check-property-has target p %this v)))
	     (js-has-property target p %this)))))

;*---------------------------------------------------------------------*/
;*    js-has-own-property ::JsProxy ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-has-own-property o::JsProxy p::obj %this)
   (proxy-check-revoked! o "has" %this)
   (with-access::JsProxy o ((target __proto__) handler)
      (let ((has (js-get-jsobject handler handler (& "has") %this)))
	 (if (js-function? has)
	     (js-call2 %this has o target p)
	     (js-has-own-property target p %this)))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property ::JsProxy ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property o::JsProxy p::obj %this)
   (proxy-check-revoked! o "getOwn" %this)
   (with-access::JsProxy o ((target __proto__) handler)
      (let ((get (js-get-jsobject handler handler (& "getOwnPropertyDescriptor") %this)))
	 (if (js-function? get)
	     (let ((desc (js-call2 %this get o target p)))
		(proxy-check-property-getown target p %this desc))
	     (js-get-own-property target p %this)))))

;*---------------------------------------------------------------------*/
;*    js-for-in ::JsProxy ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-for-in o::JsProxy proc %this)
   (proxy-check-revoked! o "for..in" %this)
   (with-access::JsProxy o ((target __proto__))
      (js-for-in target proc %this)))

;*---------------------------------------------------------------------*/
;*    js-define-own-property ::JsProxy ...                             */
;*---------------------------------------------------------------------*/
(define-method (js-define-own-property::bool o::JsProxy p
		  desc::JsPropertyDescriptor throw::bool %this)
   (proxy-check-revoked! o "defineProperty" %this)
   (with-access::JsProxy o ((target __proto__) handler)
      (let ((def (js-get-jsobject handler handler (& "defineProperty") %this)))
	 (if (js-function? def)
	     (let ((v (js-call3 %this def o target p
			 (js-from-property-descriptor %this p desc target))))
		(proxy-check-property-defprop target o p %this desc v))
	     (js-define-own-property target p desc throw %this)))))

;*---------------------------------------------------------------------*/
;*    js-getprototypeof ::JsProxy ...                                  */
;*---------------------------------------------------------------------*/
(define-method (js-getprototypeof o::JsProxy %this::JsGlobalObject msg::obj)
   (proxy-check-revoked! o "getPrototypeOf" %this)
   (with-access::JsProxy o ((target __proto__) handler)
      (let ((get (js-get-jsobject handler handler (& "getPrototypeOf") %this)))
	 (if (js-function? get)
	     (let ((v (js-call1 %this get o target)))
		(proxy-check-property-getproto target o %this msg v))
	     (js-getprototypeof target %this msg)))))

;*---------------------------------------------------------------------*/
;*    js-setprototypeof ::JsProxy ...                                  */
;*---------------------------------------------------------------------*/
(define-method (js-setprototypeof o::JsProxy v %this::JsGlobalObject msg::obj)
   (proxy-check-revoked! o "setPrototypeOf" %this)
   (with-access::JsProxy o ((target __proto__) handler)
      (let ((set (js-get-jsobject handler handler (& "setPrototypeOf") %this)))
	 (if (js-function? set)
	     (let ((r (js-call2 %this set o target v)))
		(proxy-check-property-setproto target o v %this msg r)
		o)
	     (js-setprototypeof target v %this msg)))))

;*---------------------------------------------------------------------*/
;*    js-extensible? ::JsProxy ...                                     */
;*---------------------------------------------------------------------*/
(define-method (js-extensible? o::JsProxy %this::JsGlobalObject)
   (proxy-check-revoked! o "isExtensible" %this)
   (with-access::JsProxy o ((target __proto__) handler)
      (let ((ise (js-get-jsobject handler handler (& "isExtensible") %this)))
	 (if (js-function? ise)
	     (let ((r (js-call1 %this ise o target)))
		(proxy-check-is-extensible target o %this r))
	     (js-extensible? target %this)))))

;*---------------------------------------------------------------------*/
;*    js-preventextensions ::JsProxy ...                               */
;*---------------------------------------------------------------------*/
(define-method (js-preventextensions o::JsProxy %this::JsGlobalObject)
   (proxy-check-revoked! o "preventExtensions" %this)
   (with-access::JsProxy o ((target __proto__) handler)
      (let ((p (js-get-jsobject handler handler (& "preventExtensions") %this)))
	 (if (js-function? p)
	     (let ((r (js-call1 %this p o target)))
		(proxy-check-preventext target o %this r))
	     (js-preventextensions target %this)))))

;*---------------------------------------------------------------------*/
;*    js-ownkeys ::JsProxy ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-ownkeys o::JsProxy %this::JsGlobalObject)
   (proxy-check-revoked! o "ownKeys" %this)
   (with-access::JsProxy o ((target __proto__) handler)
      (let ((ownk (js-get-jsobject handler handler (& "ownKeys") %this)))
	 (if (js-function? ownk)
	     (let ((r (js-call1 %this ownk o target)))
		(proxy-check-ownkeys target o %this r))
	     (js-ownkeys target %this)))))

;*---------------------------------------------------------------------*/
;*    proxy-check-revoked! ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (proxy-check-revoked! o::JsProxy action %this::JsGlobalObject)
   (when (js-object-mode-revoked? o)
      (js-raise-type-error %this
	 (format "Cannot perform \"~s\" on a revoked proxy" action)
	 (js-string->jsstring (typeof o)))))

;*---------------------------------------------------------------------*/
;*    proxy-check-property-value ...                                   */
;*---------------------------------------------------------------------*/
(define (proxy-check-property-value target owner prop %this v get-or-set)
   (cond
      ((null? (js-object-properties target))
       v)
      (else
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
			    (js-raise-type-error %this
			       (format "Proxy \"~a\" inconsistency" get-or-set)
			       owner))))
		    ((isa? prop JsAccessorDescriptor)
		     (with-access::JsAccessorDescriptor prop (get set)
			(cond
			   ((and (eq? get (js-undefined))
				 (eq? get-or-set (& "get")))
			    (js-raise-type-error %this
			       "Proxy \"get\" inconsistency"
			       owner))
			   ((and (eq? set (js-undefined))
				 (eq? get-or-set (& "set")))
			    (js-raise-type-error %this
			       "Proxy \"set\" inconsistency"
			       owner))
			   (else
			    v))))
		    (else
		     v))))))))

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
       (let ((conf (js-get desc (& "configurable") %this)))
	  (if (js-totest conf)
	      desc
	      (let ((prop (js-get-own-property target prop %this)))
		 (cond
		    ((eq? prop (js-undefined))
		     (err))
		    ((js-totest (js-get prop (& "configurable") %this))
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
      ((and (eq? (js-get desc (& "configurable") %this) #f)
	    (let ((odesc (js-get-own-property target p %this)))
	       (and (not (eq? odesc (js-undefined)))
		    (not (eq? (js-get odesc (& "configurable") %this) #f)))))
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
      ((not (js-array? r))
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

;*---------------------------------------------------------------------*/
;*    js-call-proxy/cache-miss ...                                     */
;*---------------------------------------------------------------------*/
(define-macro (gen-call-proxy/cache-miss %this fun this . args)
   `(with-access::JsProxy ,fun ((target __proto__) handler applycache)
       (proxy-check-revoked! ,fun "apply" %this)
       (cond
	  ((and (not (js-function? target)) (not (js-proxy? target)))
	   (js-raise-type-error ,%this
	      ,(format "call~a: not a function ~~s" (length args))
	      target))
	  ((js-object-get-name/cache handler (& "apply") #f %this
	      applycache -1 '(imap emap cmap pmap amap vtable))
	   =>
	   (lambda (xfun)
	      (cond
		 ((js-function? xfun)
		  (with-access::JsFunction xfun (procedure)
		     (js-call3% %this xfun procedure handler target
			,this (jsarray ,%this ,@args))))
		 (else
		  (with-access::JsFunction target (procedure)
		     (,(string->symbol (format "js-call~a%" (length args)))
		      ,%this target procedure ,this ,@args))))))
	  (else
	   (with-access::JsFunction target (procedure)
	      (,(string->symbol (format "js-call~a%" (length args)))
	       ,%this target procedure ,this ,@args))))))

(define (js-call-proxy/cache-miss0 %this proxy this)
   (gen-call-proxy/cache-miss %this proxy this))

(define (js-call-proxy/cache-miss1 %this proxy this a0)
   (gen-call-proxy/cache-miss %this proxy this a0))

(define (js-call-proxy/cache-miss2 %this proxy this a0 a1)
   (gen-call-proxy/cache-miss %this proxy this a0 a1))

(define (js-call-proxy/cache-miss3 %this proxy this a0 a1 a2)
   (gen-call-proxy/cache-miss %this proxy this a0 a1 a2))

(define (js-call-proxy/cache-miss4 %this proxy this a0 a1 a2 a3)
   (gen-call-proxy/cache-miss %this proxy this a0 a1 a2 a3))

(define (js-call-proxy/cache-miss5 %this proxy this a0 a1 a2 a3 a4)
   (gen-call-proxy/cache-miss %this proxy this a0 a1 a2 a3 a4))

(define (js-call-proxy/cache-miss6 %this proxy this a0 a1 a2 a3 a4 a5)
   (gen-call-proxy/cache-miss %this proxy this a0 a1 a2 a3 a4 a5))

(define (js-call-proxy/cache-miss7 %this proxy this a0 a1 a2 a3 a4 a5 a6)
   (gen-call-proxy/cache-miss %this proxy this a0 a1 a2 a3 a4 a5 a6))

(define (js-call-proxy/cache-miss8 %this proxy this a0 a1 a2 a3 a4 a5 a6 a7)
   (gen-call-proxy/cache-miss %this proxy this a0 a1 a2 a3 a4 a5 a6 a7))

(define (js-call-proxy/cache-miss9 %this proxy this a0 a1 a2 a3 a4 a5 a6 a7 a8)
   (gen-call-proxy/cache-miss %this proxy this a0 a1 a2 a3 a4 a5 a6 a7 a8))

(define (js-call-proxy/cache-miss10 %this proxy this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
   (gen-call-proxy/cache-miss %this proxy this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
