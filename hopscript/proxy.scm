;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/proxy.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec  2 20:51:44 2018                          */
;*    Last change :  Tue Apr 30 13:51:11 2019 (serrano)                */
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
	   __hopscript_regexp
	   __hopscript_array
	   __hopscript_error
	   __hopscript_worker
	   __hopscript_spawn)
   
   (export (js-init-proxy! ::JsGlobalObject)
	   (js-proxy-debug-name::bstring ::JsProxy ::JsGlobalObject)
	   (js-proxy-property-value proxy obj prop %this)
	   (inline js-jsproxy-get/name-cache ::JsProxy ::obj ::JsGlobalObject
	      #!optional (point -1) (cspecs '()))
	   (inline js-proxy-property-descriptor-index ::JsProxy ::obj)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

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
   
   (set! __js_strings (&init!))

   (set! proxy-elements
      (vector
	 (instantiate::JsWrapperDescriptor
	    (writable #t)
	    (configurable #t)
	    (enumerable #t)
	    (name (& ""))
	    (%get js-proxy-property-value)
	    (%set js-proxy-property-value-set!))))
   
   (with-access::JsGlobalObject %this (__proto__ js-function-prototype)

      (define (js-proxy-alloc %this constructor::JsFunction)
	 (instantiateJsProxy
	    (cmap proxy-cmap)
	    (__proto__ (js-null))
	    (elements proxy-elements)))

      (define (js-proxy-construct this::JsProxy t h)
	 (cond
	    ((not (isa? h JsObject))
	     (js-raise-type-error %this
		"Cannot create proxy with a non-object as handler" this))
	    ((not (isa? t JsObject))
	     (js-raise-type-error %this
		"Cannot create proxy with a non-object as target" this))
	    (else
	     (with-access::JsProxy this (target handler id)
		(set! target t)
		(set! handler h))))
	 this)

      (define js-proxy-revoke
	 (js-make-function %this
	    (lambda (this)
	       (let ((prox (js-get this (& "proxy") %this)))
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

      (define js-proxy
	 (js-make-function %this %js-proxy 2 "Proxy"
	    :__proto__ js-function-prototype
	    :prototype '()
	    :alloc js-proxy-alloc
	    :size 1
	    :construct js-proxy-construct))

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
;*    js-proxy-debug-name ...                                          */
;*---------------------------------------------------------------------*/
(define (js-proxy-debug-name::bstring obj::JsProxy %this)
   (with-access::JsProxy obj (target)
      (if (isa? target JsFunction)
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
;*---------------------------------------------------------------------*/
(define (js-proxy-property-value proxy obj prop %this)

   (define (check target v)
      (if (null? (js-object-properties target))
	  v
	  (proxy-check-property-value target obj prop %this v (& "get"))))
   
   (with-access::JsProxy proxy (target handler cacheget cachegetfun cachegetproc id)
      (proxy-check-revoked! proxy "get" %this)
      (let ((get (js-object-get-name/cache handler (& "get") #f %this cacheget)))
	 (cond
	    ((eq? get cachegetfun)
	     (check target (cachegetproc handler target prop)))
	    ((and (object? get) (eq? (object-class get) JsFunction3))
	     (with-access::JsFunction get (procedure)
		(let ((v (procedure handler target prop)))
		   (set! cachegetfun get)
		   (set! cachegetproc procedure)
		   (check target v))))
	    ((isa? get JsFunction)
	     (with-access::JsFunction get (procedure)
		(check target 
		   (js-call3% %this get procedure handler target prop obj))))
	    ((isa? get JsProxy)
	     (check target (js-call3 %this get handler target prop obj)))
	    (else
	     (js-get-jsobject target obj prop %this))))))

;*---------------------------------------------------------------------*/
;*    js-proxy-property-value-set! ...                                 */
;*---------------------------------------------------------------------*/
(define (js-proxy-property-value-set! proxy obj prop v %this)
   
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
   
   (with-access::JsProxy proxy (target handler cacheset cachesetfun cachesetproc)
      (proxy-check-revoked! proxy "put" %this)\
      (let ((set (js-object-get-name/cache handler (& "set") #f %this cacheset)))
	 (cond
	    ((eq? set cachesetfun)
	     (check target v (cachesetproc handler target prop v)))
	    ((and (object? set) (eq? (object-class set) JsFunction4))
	     (with-access::JsFunction set (procedure)
		(let ((r (procedure handler target prop v)))
		   (set! cachesetfun set)
		   (set! cachesetproc procedure)
		   (check target v r))))
	    ((isa? set JsFunction)
	     (with-access::JsFunction set (procedure)
		(check target v
		   (js-call4% %this set procedure handler target prop v obj))))
	    ((isa? set JsProxy)
	     (check target v (js-call4 %this set handler target prop v obj)))
	    ((eq? proxy obj)
	     (js-put/cache! target prop v #f %this))
	    (else
	     (js-absent))))))

;*---------------------------------------------------------------------*/
;*    js-get ::JsProxy ...                                             */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsProxy prop %this::JsGlobalObject)
   (js-proxy-property-value o o (js-toname prop %this) %this))

;*---------------------------------------------------------------------*/
;*    js-jsproxy-get/name-cache ::JsProxy ...                          */
;*---------------------------------------------------------------------*/
(define-inline (js-jsproxy-get/name-cache o::JsProxy prop::obj %this::JsGlobalObject
		  #!optional (point -1) (cspecs '()))
   (js-proxy-property-value o o (js-toname prop %this) %this))

;*---------------------------------------------------------------------*/
;*    js-get/cache ::JsProxy ...                                       */
;*---------------------------------------------------------------------*/
;* (define-method (js-get/cache o::JsProxy prop::obj %this::JsGlobalObject */
;* 		  #!optional (point -1) (cspecs '()))                  */
;*    (js-jsproxy-get-name/cache o prop %this point cspecs))           */

;*---------------------------------------------------------------------*/
;*    js-object-get-name/cache-miss ::JsProxy ...                      */
;*---------------------------------------------------------------------*/
(define-method (js-object-get-name/cache-miss obj::JsProxy
		  name::JsStringLiteral
		  throw::bool %this::JsGlobalObject
		  cache::JsPropertyCache
		  #!optional (point -1) (cspecs '()))
   (with-access::JsProxy obj (elements)
      (let ((desc (vector-ref elements 0)))
	 (js-pcache-update-descriptor! cache 0 obj obj)
	 (js-proxy-property-value obj obj name %this))))
   
;*---------------------------------------------------------------------*/
;*    js-put! ::JsProxy ...                                            */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::JsProxy prop v throw %this::JsGlobalObject)
   (proxy-check-revoked! o "put" %this)
   (with-access::JsProxy o (target handler cacheset)
      (let ((set (js-object-get-name/cache handler (& "set") #f %this cacheset)))
	 (if (isa? set JsFunction)
	     (begin
		(proxy-check-property-value target target prop %this v (& "set"))
		(js-call4 %this set handler target prop v o))
	     (js-put! target prop v throw %this)))))

;*---------------------------------------------------------------------*/
;*    js-put/cache! ::JsProxy ...                                      */
;*---------------------------------------------------------------------*/
(define-method (js-put/cache! o::JsProxy prop v::obj throw::bool %this
		  #!optional (point -1) (cspecs '()))
   (js-put! o prop v throw %this))

;*---------------------------------------------------------------------*/
;*    js-delete! ::JsProxy ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-delete! o::JsProxy p throw %this)
   (proxy-check-revoked! o "delete" %this)
   (with-access::JsProxy o (target handler)
      (let ((delete (js-get handler (& "deleteProperty") %this)))
	 (if (isa? delete JsFunction)
	     (let ((r (js-call2 %this delete o target p)))
		(proxy-check-property-delete target p %this r))
	     (js-delete! target p throw %this)))))

;*---------------------------------------------------------------------*/
;*    js-has-property ::JsProxy ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-has-property o::JsProxy p::obj %this)
   (with-access::JsProxy o (target handler)
      (let ((has (js-get handler (& "has") %this)))
	 (if (isa? has JsFunction)
	     (let ((v (js-call2 %this has o target p)))
		(or v (proxy-check-property-has target p %this v)))
	     (js-has-property target p %this)))))

;*---------------------------------------------------------------------*/
;*    js-has-own-property ::JsProxy ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-has-own-property o::JsProxy p::obj %this)
   (proxy-check-revoked! o "has" %this)
   (with-access::JsProxy o (target handler)
      (let ((has (js-get handler (& "has") %this)))
	 (if (isa? has JsFunction)
	     (js-call2 %this has o target p)
	     (js-has-own-property target p %this)))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property ::JsProxy ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property o::JsProxy p::obj %this)
   (proxy-check-revoked! o "getOwn" %this)
   (with-access::JsProxy o (target handler)
      (let ((get (js-get handler (& "getOwnPropertyDescriptor") %this)))
	 (if (isa? get JsFunction)
	     (let ((desc (js-call2 %this get o target p)))
		(proxy-check-property-getown target p %this desc))
	     (js-get-own-property target p %this)))))

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
      (let ((def (js-get handler (& "defineProperty") %this)))
	 (if (isa? def JsFunction)
	     (let ((v (js-call3 %this def o target p
			 (js-from-property-descriptor %this p desc target))))
		(proxy-check-property-defprop target o p %this desc v))
	     (js-define-own-property target p desc throw %this)))))

;*---------------------------------------------------------------------*/
;*    js-getprototypeof ::JsProxy ...                                  */
;*---------------------------------------------------------------------*/
(define-method (js-getprototypeof o::JsProxy %this::JsGlobalObject msg::obj)
   (proxy-check-revoked! o "getPrototypeOf" %this)
   (with-access::JsProxy o (target handler)
      (let ((get (js-get handler (& "getPrototypeOf") %this)))
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
      (let ((set (js-get handler (& "setPrototypeOf") %this)))
	 (if (isa? set JsFunction)
	     (let ((r (js-call2 %this set o target v)))
		(proxy-check-property-setproto target o v %this msg r)
		o)
	     (js-setprototypeof target v %this msg)))))

;*---------------------------------------------------------------------*/
;*    js-extensible? ::JsProxy ...                                     */
;*---------------------------------------------------------------------*/
(define-method (js-extensible? o::JsProxy %this::JsGlobalObject)
   (proxy-check-revoked! o "isExtensible" %this)
   (with-access::JsProxy o (target handler)
      (let ((ise (js-get handler (& "isExtensible") %this)))
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
      (let ((p (js-get handler (& "preventExtensions") %this)))
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
      (let ((ownk (js-get handler (& "ownKeys") %this)))
	 (if (isa? ownk JsFunction)
	     (let ((r (js-call1 %this ownk o target)))
		(proxy-check-ownkeys target o %this r))
	     (js-ownkeys target %this)))))

;*---------------------------------------------------------------------*/
;*    proxy-check-revoked! ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (proxy-check-revoked! o::JsProxy action %this::JsGlobalObject)
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

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
