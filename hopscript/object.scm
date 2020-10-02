;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/object.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 17 08:43:24 2013                          */
;*    Last change :  Mon May 18 11:46:57 2020 (serrano)                */
;*    Copyright   :  2013-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo implementation of JavaScript objects               */
;*                                                                     */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2         */
;*    Complete: 27 sep 2013, untested.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_object
   
   (library hop hopwidget js2scheme)
   
   (include "types.sch" "stringliteral.sch" "property.sch" "function.sch")
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_string
	   __hopscript_stringliteral
	   __hopscript_symbol
	   __hopscript_promise
	   __hopscript_generator
	   __hopscript_proxy
	   __hopscript_reflect
	   __hopscript_map
	   __hopscript_set
	   __hopscript_function
	   __hopscript_number
	   __hopscript_math
	   __hopscript_boolean
	   __hopscript_regexp
	   __hopscript_array
	   __hopscript_arraybuffer
	   __hopscript_arraybufferview
	   __hopscript_date
	   __hopscript_error
	   __hopscript_json
	   __hopscript_service
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_worker
	   __hopscript_websocket
	   __hopscript_lib
	   __hopscript_arguments
	   __hopscript_pair
	   __hopscript_dom
	   __hopscript_profile)

   (export (js-new-global-object::JsGlobalObject #!key (size 64) name)
	   
	   (generic js-extensible?::bool ::obj ::JsGlobalObject)
	   (generic js-preventextensions ::obj ::JsGlobalObject)
	   (generic js-ownkeys ::obj ::JsGlobalObject)
	   
	   (js-object-isfrozen o %this::JsGlobalObject)

	   (js-object-no-setter? ::JsObject)
	   (js-object-prototype-tostring ::obj ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsObject ...                                 */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsObject
   (lambda (o ctx)
      (if (isa? ctx JsGlobalObject)
	  (js-jsobject->plist o ctx)
	  (error "obj->string"
	     (format "Not a JavaScript context (~a)" (typeof o))
	     ctx)))
   (lambda (o ctx)
      (if (isa? ctx JsGlobalObject)
	  (js-plist->jsobject o ctx)
	  (error "obj->string"
	     (format "Not a JavaScript context (~a)" (typeof o)) ctx))))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsResponse ...                               */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsResponse
   (lambda (o ctx)
      (with-access::JsResponse o (%this obj)
	 (js-jsobject->plist obj %this)))
   (lambda (o)
      (error "string->obj" "Cannot unserialize JsResponse" o)))

;*---------------------------------------------------------------------*/
;*    js-extensible? ...                                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.13    */
;*---------------------------------------------------------------------*/
(define-generic (js-extensible? obj::obj %this)
   (let ((o (js-cast-object obj %this "Object.isExtensible")))
      (js-object-mode-extensible? o)))

;*---------------------------------------------------------------------*/
;*    js-preventextensions ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.10    */
;*---------------------------------------------------------------------*/
(define-generic (js-preventextensions obj::obj %this)
   (let ((o (js-cast-object obj %this "Object.preventExtensions")))
      (js-prevent-extensions o)
      obj))

;*---------------------------------------------------------------------*/
;*    js-ownkeys ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.14    */
;*---------------------------------------------------------------------*/
(define-generic (js-ownkeys obj %this)
   (let ((o (js-cast-object obj %this "Object.keys")))
      (js-ownkeys o %this)))

;*---------------------------------------------------------------------*/
;*    js-tostring ::JsObject ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-tostring obj::JsObject %this::JsGlobalObject)
   (js-tostring (js-toprimitive obj 'string %this) %this))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsObject ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsObject worker::WorkerHopThread %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-object js-initial-cmap)
	 (with-access::JsObject obj (elements)
	    (let ((nobj (duplicate::JsObject obj
			   (cmap js-initial-cmap)
			   (elements (make-vector (vector-length elements))))))
	       (js-object-proto-set! nobj (js-get js-object (& "prototype") %this))
	       (js-object-mode-set! nobj (js-object-mode obj))
	       (js-for-in obj
		  (lambda (k %this)
		     (js-put! nobj (js-donate k worker %_this)
			(js-donate (js-get/name-cache obj k %_this) worker %_this)
			#f %this))
		  %this)
	       nobj)))))

;*---------------------------------------------------------------------*/
;*    xml-primitive-value ::JsObject ...                               */
;*---------------------------------------------------------------------*/
(define-method (xml-primitive-value obj::JsObject ctx)
   (if (isa? ctx JsGlobalObject)
       (js-jsobject->plist obj ctx)
       (error "xml-primitive-value ::JsObject"
	  (format "Not a JavaScript context (~a)" (typeof obj))
	  ctx)))

;*---------------------------------------------------------------------*/
;*    xml-unpack ::JsObject ...                                        */
;*    -------------------------------------------------------------    */
;*    Used when an JS object is to pack the arguments sent to          */
;*    an XML constructor.                                              */
;*---------------------------------------------------------------------*/
(define-method (xml-unpack o::JsObject ctx)
   (if (isa? ctx JsGlobalObject)
       (js-jsobject->keyword-plist o ctx)
       o))

;*---------------------------------------------------------------------*/
;*    obj->json ::JsObject ...                                         */
;*---------------------------------------------------------------------*/
(define-method (obj->json obj::JsObject op::output-port ctx)
   (if (isa? ctx JsGlobalObject)
       (let ((str (js-json-stringify (js-undefined) obj (js-undefined) 1 ctx)))
	  (display str op))
       (error "obj->json"
	  (format "Not a JavaScript context (~a)" (typeof obj))
	  ctx)))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsGlobalObject ...                                   */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsGlobalObject worker %_this)
   (with-access::JsGlobalObject obj (elements)
      (js-new-global-object :size (vector-length elements) :name "donate")))

;*---------------------------------------------------------------------*/
;*    scheme->response ::JsObject ...                                  */
;*---------------------------------------------------------------------*/
(define-method (scheme->response obj::JsObject req ctx)
   
   (define (responder ctx thunk)
      (js-with-context ctx "response" thunk))
   
   (js-with-context ctx "scheme->response"
      (lambda ()
	 (let ((%this ctx))
	    (with-access::JsGlobalObject %this (js-service-pcache)
	       (let ((proc (js-get-jsobject-name/cache obj (& "toResponse") #f %this
			      (js-pcache-ref js-service-pcache 0))))
		  (if (js-procedure? proc)
		      (scheme->response (js-call1 %this proc obj req) req ctx)
		      (let ((rep (call-next-method)))
			 (if (isa? rep http-response-hop)
			     (with-access::http-response-hop rep ((rctx ctx))
				(set! rctx ctx)
				(instantiate::http-response-responder
				   (ctx ctx)
				   (response rep)
				   (responder responder)))
			     rep)))))))))

;*---------------------------------------------------------------------*/
;*    jsobject-fields ...                                              */
;*---------------------------------------------------------------------*/
(define jsobject-fields (vector (find-class-field JsObject 'elements)))

;*---------------------------------------------------------------------*/
;*    javascript-class-all-fields ::JsObject ...                       */
;*---------------------------------------------------------------------*/
(define-method (javascript-class-all-fields obj::JsObject)
   jsobject-fields)

;*---------------------------------------------------------------------*/
;*    j2s-js-literal ::JsObject ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-js-literal o::JsObject ctx)
   (with-access::JsService o (svc)
      (with-access::hop-service svc (path)
	 (call-with-output-string
	    (lambda (op)
	       (obj->json o op ctx))))))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsObject ...                                   */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsObject op compile isexpr ctx)
   (js-with-context ctx "hop->javascript"
      (lambda ()
	 (display "{" op)
	 (let ((sep ""))
	    (js-for-in o
	       (lambda (p %this)
		  (display sep op)
		  (display "\"" op)
		  (display p op)
		  (display "\":" op)
		  (hop->javascript
		     (js-get/name-cache o p %this)
		     op compile isexpr ctx)
		  (set! sep ","))
	       ctx))
	 (display "}" op))))

;*---------------------------------------------------------------------*/
;*    js-new-global-object ...                                         */
;*---------------------------------------------------------------------*/
(define (js-new-global-object #!key (size 64) name)
   (let* ((%proto (instantiateJsObject
		     (cmap (instantiate::JsConstructMap
			      (inline #t)))
		     (__proto__ (js-null))
		     (elements (make-vector 128))))
	  (%this (instantiateJsGlobalObject
		    (name name)
		    (cmap (instantiate::JsConstructMap
			     (methods '#())
			     (props '#())))
		    (__proto__ %proto)
		    (elements (make-vector size)))))
      ;; local constant strings
      (js-init-names!)
      (unless (vector? __js_strings) (set! __js_strings (&init!)))
      (js-init-property! %this)
      ;; mark the top proto has holding no numeral properties
      (js-object-mode-hasnumeralprop-set! %proto #f)
      ;; for bootstrap, first allocate hasInstance symbol
      (with-access::JsGlobalObject %this (js-symbol-hasinstance)
	 (set! js-symbol-hasinstance
	    (instantiate::JsSymbolLiteral
	       (val (& "hasInstance")))))

      ;; pcache for object
      (with-access::JsGlobalObject %this (js-object-pcache)
	 (set! js-object-pcache
	    ((@ js-make-pcache-table __hopscript_property) 2 "object")))
      
      ;; init the builtin function class
      (js-init-function! %this)
      ;; the object constructor
      (with-access::JsGlobalObject %this (js-function js-object js-initial-cmap)
	 (set! js-initial-cmap (instantiate::JsConstructMap))
	 (let ((js-function-prototype (js-object-proto js-function)))
	    ;; the prototypes and other builtin classes
	    (js-init-public! %this)
	    (js-init-stringliteral! %this)
	    (js-init-arguments! %this)
	    (js-init-symbol! %this)
	    (js-init-array! %this)
	    (js-init-arraybuffer! %this)
	    (js-init-arraybufferview! %this)
	    (js-init-string! %this)
	    (js-init-boolean! %this)
	    (js-init-number! %this)
	    (js-init-math! %this)
	    (js-init-regexp! %this)
	    (js-init-date! %this)
	    (js-init-error! %this)
	    (js-init-json! %this)
	    (js-init-service! %this)
	    (js-init-worker! %this)
	    (js-init-websocket! %this)
	    (js-init-promise! %this)
	    (js-init-generator! %this)
	    (js-init-proxy! %this)
	    (js-init-reflect! %this)
	    (js-init-map! %this)
	    (js-init-weakmap! %this)
	    (js-init-set! %this)
	    (js-init-weakset! %this)
	    (js-init-object! %this)
	    (js-init-object-prototype! %this)
	    (js-init-pair! %this)
	    (js-init-dom! %this)

	    ;; bind the global object properties
	    (js-bind! %this %this (& "Object")
	       :value js-object
	       :writable #t :enumerable #f :configurable #f :hidden-class #f)
	    (js-bind! %this %this (& "NaN")
	       :value +nan.0
	       :writable #f :enumerable #f :configurable #f :hidden-class #f)
	    (js-bind! %this %this (& "Infinity")
	       :value +inf.0
	       :writable #f :enumerable #f :configurable #f :hidden-class #f)
	    (js-bind! %this %this (& "undefined")
	       :value (js-undefined)
	       :writable #f :enumerable #f :configurable #f :hidden-class #f)

	    ;; eval
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.1
	    ;; not used, see nodejs/require.scm
	    (define (js-eval _ string)
	       (if (not (js-jsstring? string))
		   string
		   (call-with-input-string (js-jsstring->string string)
		      (lambda (ip)
			 (%js-eval ip 'eval %this %this %this)))))
	    
	    (js-bind! %this %this (& "eval")
	       :value (js-make-function %this js-eval
			 (procedure-arity js-eval)
			 (js-function-info :name "eval" :len 1)
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)
	    
	    (js-bind! %this %this (& "parseInt")
	       :value (js-make-function %this
			 (lambda (this string radix)
			    (js-parseint string radix %this))
			 (js-function-arity 3 0)
			 (js-function-info :name "parseInt" :len 2)
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    (js-bind! %this %this (& "parseFloat")
	       :value (js-make-function %this
			 (lambda (this string)
			    (js-parsefloat string %this))
			 (js-function-arity 1 0)
			 (js-function-info :name "parseFloat" :len 1)
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    ;; isNaN
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.4
	    (define (isnan this val)
	       (js-isnan? val %this))

	    (js-bind! %this %this (& "isNaN")
	       :value (js-make-function %this isnan
			 (js-function-arity isnan)
			 (js-function-info :name "isNaN" :len 1)
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    ;; isFinite
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.5
	    (define (isfinite this number)
	       (let ((n (js-tonumber number %this)))
		  (cond
		     ((not (flonum? n)) #t)
		     ((or (not (=fl n n)) (=fl n +inf.0) (=fl n -inf.0)) #f)
		     (else #t))))

	    (js-bind! %this %this (& "isFinite")
	       :value (js-make-function %this isfinite
			 (js-function-arity isfinite)
			 (js-function-info :name "isFinite" :len 1)
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    ;; check-utf8-validity
	    (define (check-utf8-validity str)
	       (if (utf8-string? str)
		   (js-string->jsstring str)
		   (js-raise-uri-error %this "Not a utf8 string ~s" string)))
	    
	    ;; decodeURI
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.3.1
	    (define (decodeuri this string)
	       (let ((str (js-tostring string %this)))
		  (if (url? str)
		      (check-utf8-validity (uri-decode str))
		      (js-raise-uri-error %this "Badly formed url ~s" string))))
   
	    (js-bind! %this %this (& "decodeURI")
	       :value (js-make-function %this decodeuri
			 (js-function-arity decodeuri)
			 (js-function-info :name "decodeURI" :len 1)
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    ;; decodeURIComponent
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.3.2
	    (define (decodeuricomponent this string)
	       (let ((str (js-tostring string %this)))
		  (if (url? str)
		      (check-utf8-validity (uri-decode-component str))
		      (js-raise-uri-error %this "Badly formed url ~s" string))))
		
	    (js-bind! %this %this (& "decodeURIComponent")
	       :value (js-make-function %this decodeuricomponent
			 (js-function-arity decodeuricomponent)
			 (js-function-info :name "decodeURIComponent" :len 1)
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    ;; encodeURI
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.3.3
	    (define (encodeuri this string)
	       (js-jsstring-maybe-encodeuri string %this))

	    (js-bind! %this %this (& "encodeURI")
	       :value (js-make-function %this encodeuri
			 (js-function-arity encodeuri)
			 (js-function-info :name "encodeURI" :len 1)
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    ;; encodeURIComponent
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.3.4
	    (define (encodeuricomponent this string)
	       (js-jsstring-maybe-encodeuricomponent string %this))

	    (js-bind! %this %this (& "encodeURIComponent")
	       :value (js-make-function %this encodeuricomponent
			 (js-function-arity encodeuricomponent)
			 (js-function-info :name "encodeURIComponent" :len 1)
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    ;; escape
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.1
	    (define (escape this string)
	       (js-jsstring-escape (js-tojsstring string %this)))
	    
	    (js-bind! %this %this (& "escape")
	       :value (js-make-function %this escape
			 (js-function-arity escape)
			 (js-function-info :name "escape" :len 1)
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    ;; unescape
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.1
	    (define (unescape this string)
	       (js-jsstring-unescape (js-tojsstring string %this) %this))

	    (js-bind! %this %this (& "unescape")
	       :value (js-make-function %this unescape
			 (js-function-arity unescape)
			 (js-function-info :name "unescape" :len 1)
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    ;; html
	    (define (js-html-html %this)
	       (js-make-function %this
		  (lambda (this attrs . nodes)
		     (if (js-object? attrs)
			 (apply <HTML> :idiom "javascript" :%context %this
			    (append
			       (js-jsobject->keyword-plist attrs %this)
			       nodes))
			 (apply <HTML> :idiom "javascript" nodes)))
		  (js-function-arity 1 -1 'scheme)
		  (js-function-info :name "HTML" :len 1)))
	    
	    (js-bind! %this %this (& "HTML")
	       :value (js-html-html %this) :enumerable #f :hidden-class #f)

	    ;; only used with the global object, see nodejs/require.scm
	    ;; rebound in generated code
	    (js-bind! %this %this (& "HEAD")
	       :value (js-html-head %this) :enumerable #f :hidden-class #f)
	    (js-bind! %this %this (& "SCRIPT")
	       :value (js-html-script %this) :enumerable #f :hidden-class #f)

	    (define (string->xml-tilde body)
	       (let ((expr (js-tostring body %this)))
		  (instantiateJsWrapper
		     (__proto__ %proto)
		     (data body)
		     (obj (instantiate::xml-tilde
			     (lang 'javascript)
			     (%js-expression expr)
			     (body (vector body '() '() '() expr #f)))))))

	    ;; tilde object
	    (js-bind! %this %this (& "Tilde")
	       :value (js-make-function %this
			 (lambda (this body)
			    (string->xml-tilde body))
			 (js-function-arity 1 0)
			 (js-function-info :name "Tilde" :len 1)
			 :__proto__ js-function-prototype)
	       :enumerable #f :writable #f :configurable #f :hidden-class #f)

	    ;; return the newly created object
	    %this))))

;*---------------------------------------------------------------------*/
;*    js-init-object! ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3       */
;*---------------------------------------------------------------------*/
(define (js-init-object! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-object js-function)

      (define %proto (js-object-proto %this))
      
      ;; Object.constructor
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.2.1
      (define (%js-object _ value)
	 (with-access::JsGlobalObject %this (js-string js-boolean js-number js-object js-symbol)
	    (cond
	       ((or (eq? value (js-null)) (eq? value (js-undefined)))
		;; 2
		(with-access::JsFunction js-object (constrmap constrsize)
		   (js-make-jsobject constrsize constrmap %proto)))
	       ((js-object? value)
		;; 1.a
		value)
	       ((js-jsstring? value)
		;; 1.b
		(js-new %this js-string value))
	       ((boolean? value)
		;; 1.c
		(js-new %this js-boolean value))
	       ((js-number? value)
		;; 1.c
		(js-new %this js-number value))
	       (else
		(js-toobject %this value)))))

      (let ((js-function-prototype (js-object-proto js-function)))
	 (set! js-object
	    (js-function-set-constrmap!
	       (js-make-function %this %js-object
		  (js-function-arity %js-object)
		  (js-function-info :name "Object" :len 1 :maxconstrsize 4)
		  :__proto__ js-function-prototype
		  :constrsize 3
		  :prototype %proto
		  :alloc js-no-alloc
		  :size 21
		  :shared-cmap #f))))

      ;; getPrototypeOf
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.2
      (define (getprototypeof this o)
	 (js-getprototypeof o %this "getPrototypeOf"))
      
      (js-bind! %this js-object (& "getPrototypeOf")
	 :value (js-make-function %this getprototypeof
		   (js-function-arity getprototypeof)
		   (js-function-info :name "getPrototypeOf" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 ;; all first properties must use a duplicated hidden class
	 :hidden-class #t)
      
      ;; setPrototypeOf
      ;; http://www.ecma-international.org/ecma-262/6.0/#sec-object.setprototypeof
      (define (setprototypeof this o v)
	 (js-setprototypeof o v %this "setPrototypeOf"))
      
      (js-bind! %this js-object (& "setPrototypeOf")
	 :value (js-make-function %this setprototypeof
		   (js-function-arity setprototypeof)
		   (js-function-info :name "setPrototypeOf" :len 2))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; getOwnPropertyDescriptor
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.3
      (define (getownpropertydescriptor this o p)
	 (js-get-own-property-descriptor o p %this))
      
      (js-bind! %this js-object (& "getOwnPropertyDescriptor")
	 :value (js-make-function %this
		   getownpropertydescriptor
		   (js-function-arity getownpropertydescriptor)
		   (js-function-info :name "getOwnPropertyDescriptor" :len 2))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; getOwnPropertyDescriptors
      ;; https://www.ecma-international.org/ecma-262/8.0/#sec-object.getownpropertydescriptors
      (define (getownpropertydescriptors this o p)
	 (let* ((o (if (js-object? o)
		       o
		       (js-cast-object o %this "getOwnPropertyDescriptors")))
		(keys (js-properties-names o #f %this))
		(res (js-new0 %this js-object)))
	    (when (pair? keys)
	       (for-each (lambda (k)
			    (let ((desc (js-get-own-property o k %this)))
			       (js-put! res k
				  (js-from-property-descriptor %this k desc o)
				  #f %this)))
		  keys))
	    res))
      
      (js-bind! %this js-object (& "getOwnPropertyDescriptors")
	 :value (js-make-function %this
		   getownpropertydescriptors
		   (js-function-arity getownpropertydescriptors)
		   (js-function-info :name "getOwnPropertyDescriptors" :len 2))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; getOwnPropertyNames
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.4
      (define (getownpropertynames this o p)
	 (let ((o (js-cast-object o %this "getOwnPropertyNames")))
	    (let ((p (js-properties-name o #f %this)))
	       (js-vector->jsarray p %this))))
      
      (js-bind! %this js-object (& "getOwnPropertyNames")
	 :value (js-make-function %this
		   getownpropertynames
		   (js-function-arity getownpropertynames)
		   (js-function-info :name "getOwnPropertyNames" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; getOwnPropertySymbols
      ;; http://www.ecma-international.org/ecma-262/6.0/#sec-19.1.2.8
      (define (getownpropertysymbols this o p)
	 (let ((o (js-cast-object o %this "getOwnPropertySymbols")))
	    (js-vector->jsarray (js-properties-symbol o %this) %this)))
      
      (js-bind! %this js-object (& "getOwnPropertySymbols")
	 :value (js-make-function %this
		   getownpropertysymbols
		   (js-function-arity getownpropertysymbols)
		   (js-function-info :name "getOwnPropertySymbols" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; create
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.5
      (define (create this o properties)
	 (if (not (or (eq? o (js-null)) (js-object? o)))
	     (js-raise-type-error %this "create: bad object ~s" o)
	     (let ((obj (js-new0 %this js-object)))
		(with-access::JsObject obj (elements)
		   (js-object-proto-set! obj o)
		   (unless (eq? properties (js-undefined))
		      (object-defineproperties %this this obj properties)))
		obj)))

      (js-bind! %this js-object (& "create")
	 :value (js-make-function %this create
		   (js-function-arity create)
		   (js-function-info :name "create" :len 2))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)

      ;; defineProperty
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.6
      (define (defineproperty this o p attributes)
	 (if (not (js-object? o))
	     (js-raise-type-error %this "Object.defineProperty called on non-object ~s" o)
	     (let* ((name (js-toname p %this))
		    (desc (js-to-property-descriptor %this attributes name)))
		(js-define-own-property o name desc #t %this)
		o)))
      
      (js-bind! %this js-object (& "defineProperty")
	 :value (js-make-function %this
		   defineproperty
		   (js-function-arity defineproperty)
		   (js-function-info :name "defineProperty" :len 3))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)

      ;; assign
      ;; http://www.ecma-international.org/ecma-262/6.0/#sec-object.assign
      ;; http://www.ecma-international.org/ecma-262/6.0/#sec-9.1.12
      (define (assign this target . sources)
	 
	 (define (idx-cmp a b)
	    (<=fx (string-natural-compare3
		     (js-name->string a) (js-name->string b))
	       0))

	 (let ((to (js-cast-object target %this "assign")))
	    (for-each (lambda (src)
			 (unless (or (null? src) (eq? src (js-undefined)))
			    (let* ((fro (js-toobject %this src))
				   (names (js-properties-names fro #t %this))
				   (keys '())
				   (idx '()))
			       ;; keys have to be sorted as specified in
			       ;; section 9.1.12
			       (for-each (lambda (name)
					    (if (js-isindex? (js-toindex name))
						(set! idx (cons name idx))
						(set! keys (cons name keys))))
				  names)
			       (for-each (lambda (k)
					    (let ((val (js-get fro k %this)))
					       (js-put! to k val #t %this)))
				  (sort idx-cmp idx))
			       (for-each (lambda (k)
					    (let ((val (js-get fro k %this)))
					       (js-put! to k val #t %this)))
				  (reverse! keys)))))
	       sources)
	    to))

      (js-bind! %this js-object (& "assign")
	 :value (js-make-function %this assign
		   (js-function-arity 1 -1 'scheme)
		   (js-function-info :name "assign" :len 2))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; defineProperties
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.7
      (js-bind! %this js-object (& "defineProperties")
	 :value (js-make-function %this
		   (lambda (this obj properties)
		      (object-defineproperties %this this obj properties))
		   (js-function-arity 2 0)
		   (js-function-info :name "defineProperties" :len 2))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; seal
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.8
      (define (seal this obj)
	 (if (not (js-object? obj))
	     obj
	     (js-seal obj obj)))
      
      (js-bind! %this js-object (& "seal")
	 :value (js-make-function %this seal
		   (js-function-arity seal)
		   (js-function-info :name "seal" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; freeze
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.9
      (define (freeze this obj)
	 (if (not (js-object? obj))
	     obj
	     (js-freeze obj obj)))
      
      (js-bind! %this js-object (& "freeze")
	 :value (js-make-function %this freeze
		   (js-function-arity freeze)
		   (js-function-info :name "freeze" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      (js-bind! %this js-object (& "preventExtensions")
	 :value (js-make-function %this
		   (lambda (this obj)
		      (js-preventextensions obj %this))
		   (js-function-arity 1 0)
		   (js-function-info :name "preventExtensions" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)

      ;; is
      ;; https://www.ecma-international.org/ecma-262/6.0/#sec-object.is
      (define (is this x y)
	 ;; SameValue algorithm
	 (or (eq? x y)
	     (if (or (js-jsstring? x) (js-jsstring? y)) (js-eqstring? x y))
	     ;; eqstring test eq first
	     (and (flonum? x) (flonum? y)
		  (or (and (=fl x y) (=fx (signbitfl x) (signbitfl y)))
		      (and (nanfl? x) (nanfl? y))))))

      (js-bind! %this js-object (& "is")
	 :value (js-make-function %this is
		   (js-function-arity is)
		   (js-function-info :name "is" :len 2))
	 :enumerable #f :configurable #t :writable #t :hidden-class #f)
	    
      ;; isSealed
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.11
      (define (issealed this o)
	 ;; 1
	 (let ((o (js-cast-object o %this "isSealed")))
	    (with-access::JsObject o (cmap elements)
	       (and
		;; 2
		(or
		 (cond
		    ((js-object-mapped? o)
		     (with-access::JsConstructMap cmap (props)
			(vector-every (lambda (p)
					 (let ((flags (prop-flags p)))
					    (not (flags-configurable? flags))))
			   props)))
		    ((js-object-hashed? o)
		     (let ((sealed #t))
			(hashtable-for-each elements
			   (lambda (key e)
			      (when sealed
				 (let ((d (cell-ref e)))
				    (if (isa? d JsPropertyDescriptor)
					(with-access::JsPropertyDescriptor d (configurable)
					   (when (eq? configurable #t)
					      (set! sealed #f)))
					(set! sealed #f))))))
			sealed))
		    (else
		     (vector-every (lambda (desc::JsPropertyDescriptor)
				      (with-access::JsPropertyDescriptor desc (configurable)
					 (not (eq? configurable #t))))
			elements))))
		;; 3
		(not (js-object-mode-extensible? o))))))
      
      (js-bind! %this js-object (& "isSealed")
	 :value (js-make-function %this issealed
		   (js-function-arity issealed)
		   (js-function-info :name "isSealed" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; isFrozen
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.12
      (js-bind! %this js-object (& "isFrozen")
	 :value (js-make-function %this (lambda (this o) (js-object-isfrozen o %this))
		   (js-function-arity 1 0)
		   (js-function-info :name "isFrozen" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)

      ;; isExtensible
      ;; https://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.13
      (define (isextensible this obj)
	 (let ((o (js-cast-object obj %this "Object.isExtensible")))
	    (js-object-mode-extensible? o)))
      
      (js-bind! %this js-object (& "isExtensible")
	 :value (js-make-function %this
		   (lambda (this obj)
		      (js-extensible? obj %this))
		   (js-function-arity 1 0)
		   (js-function-info :name "isExtensible" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)

      ;; keys
      ;; https://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.14
      (js-bind! %this js-object (& "keys")
	 :value (js-make-function %this
		   (lambda (this obj)
		      (js-ownkeys obj %this))
		   (js-function-arity 1 0)
		   (js-function-info :name "keys" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)))

;*---------------------------------------------------------------------*/
;*    js-init-object-prototype! ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4       */
;*---------------------------------------------------------------------*/
(define (js-init-object-prototype! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-object js-symbol-tostringtag)

      (define obj (js-object-proto %this))
      
      ;; __proto__
      (js-bind! %this obj (& "__proto__")
	 :enumerable #f
	 :configurable #f
	 :get (js-make-function %this
		 (lambda (o)
		    (js-getprototypeof o %this "__proto__"))
		 (js-function-arity 0 0)
		 (js-function-info :name "get" :len 0))
	 :set (js-make-function %this
		 (lambda (o v)
		    (js-setprototypeof o v %this "__proto__"))
		 (js-function-arity 1 0)
		 (js-function-info :name "set" :len 1))
	 :hidden-class #f)
      
      ;; constructor
      (js-bind! %this obj (& "constructor")
	 :value js-object
	 :enumerable #f
	 :hidden-class #f)
      
      ;; toString
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.2
      (js-bind! %this obj (& "toString")
	 :value (js-make-function %this
		   (lambda (this) (%js-object-prototype-tostring this %this))
		   (js-function-arity 0 0)
		   (js-function-info :name "toString" :len 0)
		   :prototype (js-undefined))
	 :enumerable #f
	 :hidden-class #f)
      
      ;; toLocaleString
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.3
      (define (js-object-prototype-tolocalestring this)
	 (js-call0 %this (js-get this (& "toString") %this) this))
      
      (js-bind! %this obj (& "toLocaleString")
	 :value (js-make-function %this
		   js-object-prototype-tolocalestring
		   (js-function-arity js-object-prototype-tolocalestring)
		   (js-function-info :name "toLocaleString" :len 0)
		   :prototype (js-undefined))
	 :enumerable #f
	 :hidden-class #f)
      
      ;; valueOf
      (js-bind! %this obj (& "valueOf")
	 :value (js-make-function %this
		   (lambda (o) (js-valueof o %this))
		   (js-function-arity 0 0)
		   (js-function-info :name "valueOf" :len 0)
		   :prototype (js-undefined))
	 :enumerable #f
	 :hidden-class #f)
      
      ;; hasOwnProperty
      (js-bind! %this obj (& "hasOwnProperty")
	 :value (js-make-function %this
		   (lambda (this v)
		      (js-object-prototype-hasownproperty this v %this))
		   (js-function-arity 1 0)
		   (js-function-info :name "hasOwnProperty" :len 1)
		   :prototype (js-undefined))
	 :enumerable #f
	 :hidden-class #f)
      
      ;; isPrototypeOf
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.6
      (define (js-object-prototype-isprototypeof this v)
	 (when (js-object? v)
	    (let ((o (js-toobject %this this)))
	       (let loop ((v v))
		  (let ((v (js-object-proto v)))
		     (unless (eq? v (js-null))
			(or (eq? v o) (loop v))))))))
      
      (js-bind! %this obj (& "isPrototypeOf")
	 :value (js-make-function %this
		   js-object-prototype-isprototypeof
		   (js-function-arity js-object-prototype-isprototypeof)
		   (js-function-info :name "isPrototypeOf" :len 1)
		   :prototype (js-undefined))
	 :enumerable #f
	 :hidden-class #f)
      
      ;; propertyIsEnumerable
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.7
      (define (js-object-prototype-propertyisenumerable this v)
	 (let* ((p (js-toname v %this))
		(o (js-toobject %this this))
		(desc (js-get-own-property o p %this)))
	    (if (eq? desc (js-undefined))
		#f
		(with-access::JsPropertyDescriptor desc (enumerable) enumerable))))
      
      (js-bind! %this obj (& "propertyIsEnumerable")
	 :value (js-make-function %this
		   js-object-prototype-propertyisenumerable
		   (js-function-arity js-object-prototype-propertyisenumerable)
		   (js-function-info :name "propertyIsEnumerable" :len 1)
		   :prototype (js-undefined))
	 :enumerable #f
	 :hidden-class #f)))

;*---------------------------------------------------------------------*/
;*    js-object-prototype-hasownproperty ...                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.5     */
;*---------------------------------------------------------------------*/
(define-inline (js-object-prototype-hasownproperty this v %this)
   ;; the conversion ToPropertyKey is implemented by js-has-own-property
   (let ((obj (js-toobject-fast this %this)))
      (if (eq? (object-class obj) JsObject)
	  (js-has-own-property-jsobject obj v %this)
	  (js-has-own-property obj v %this))))

;*---------------------------------------------------------------------*/
;*    js-extensible? ::JsObject ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.13    */
;*---------------------------------------------------------------------*/
(define-method (js-extensible? obj::JsObject %this)
   (js-object-mode-extensible? obj))

;*---------------------------------------------------------------------*/
;*    js-ownkeys ::JsObject ...                                        */
;*---------------------------------------------------------------------*/
(define-method (js-ownkeys obj::JsObject %this)

   (define (cmap->names cmap)
      (with-access::JsConstructMap cmap (props)
	 (let* ((len (vector-length props))
		(arr (js-array-construct-alloc/length %this len)))
	    (with-access::JsArray arr (length ilen vec)
	       (let loop ((i 0)
			  (j 0))
		  (cond
		     ((=fx i len)
		      (set! ilen (fixnum->uint32 j))
		      (set! length (fixnum->uint32 j))
		      arr)
		     ((vector-ref props i)
		      =>
		      (lambda (prop)
			 (if (flags-enumerable? (prop-flags prop))
			     (begin
				(vector-set! vec j (prop-name prop))
				(loop (+fx i 1) (+fx j 1)))
			     (loop (+fx i 1) j))))
		     (else
		      (loop (+fx i 1) j))))))))
   
   (define (hash->names elements)
      (map! js-string->jsstring (hashtable-key-list elements)))
   
   (define (enum-desc->names elements)
      (let* ((len (vector-length elements))
	     (arr (js-array-construct-alloc/length %this len)))
	 (with-access::JsArray arr (length ilen vec)
	    (let loop ((i 0)
		       (j 0))
	       (if (=fx i len)
		   (begin
		      (set! ilen (fixnum->uint32 j))
		      (set! length (fixnum->uint32 j))
		      arr)
		   (with-access::JsPropertyDescriptor (vector-ref elements i) (enumerable name)
		      (if enumerable
			  (begin
			     (vector-set! vec j name)
			     (loop (+fx i 1) (+fx j 1)))
			  (loop (+fx i 1) j))))))))
   
   
   (with-access::JsObject obj (cmap elements)
      (cond
	 ((js-object-mapped? obj)
	  (cmap->names cmap))
	 ((js-object-hashed? obj)
	  (js-vector->jsarray (apply vector (hash->names elements)) %this))
	 (else
	  (enum-desc->names elements)))))

;*---------------------------------------------------------------------*/
;*    js-tonumber ::JsObject ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-method (js-tonumber obj::JsObject %this)
   (let ((v (js-toprimitive obj 'number %this)))
      (js-tonumber (js-toprimitive obj 'number %this) %this)))
   
;*---------------------------------------------------------------------*/
;*    js-tointeger ::JsObject ...                                      */
;*---------------------------------------------------------------------*/
(define-method (js-tointeger obj::JsObject %this)
   (js-tointeger (js-tonumber obj %this) %this))
   
;*---------------------------------------------------------------------*/
;*    js-valueof ::JsGlobalObject ...                                  */
;*---------------------------------------------------------------------*/
(define-method (js-valueof this::JsGlobalObject %this)
   (js-toobject this (js-null)))

;*---------------------------------------------------------------------*/
;*    js-valueof ::JsObject ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.4     */
;*---------------------------------------------------------------------*/
(define-method (js-valueof this::JsObject %this)
   this)

;*---------------------------------------------------------------------*/
;*    object-defineproperties ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.7     */
;*---------------------------------------------------------------------*/
(define (object-defineproperties %this::JsGlobalObject this _obj _properties)
   
   
   (define (vfor-each proc vec)
      (let ((len (vector-length vec)))
	 (let loop ((i 0))
	    (when (<fx i len)
	       (proc (vector-ref vec i) i)
	       (loop (+fx i 1))))))
   
   (define (enumerable-mapped-property? obj i)
      (with-access::JsObject obj (elements)
	 (let ((el (vector-ref elements i)))
	    (if (isa? el JsPropertyDescriptor)
		(with-access::JsPropertyDescriptor el (enumerable) enumerable)
		#t))))
   
   (define (define-own-property obj name prop properties)
      (let* ((descobj (cond
			 ((isa? prop JsPropertyDescriptor)
			  (js-property-value properties properties name prop %this))
			 ((not (js-object? prop))
			  (js-raise-type-error %this "Property description must be an object: ~s" prop))
			 (else
			  prop)))
	     (desc (js-to-property-descriptor %this descobj name)))
	 (js-define-own-property obj name desc #t %this)))
   
   (define (defineproperties/cmap cmap obj properties)
      (with-access::JsObject properties (elements)
	 (with-access::JsConstructMap cmap (props)
	    (vfor-each
	       (lambda (d::struct i)
		  (when (flags-enumerable? (prop-flags d))
		     (let ((prop (vector-ref elements i)))
			(define-own-property obj (prop-name d) prop properties))))
	       props))))
   
   (define (defineproperties/hash oprops obj properties)
      (hashtable-for-each oprops
	 (lambda (k e)
	    (let ((prop (cell-ref e)))
	       (if (isa? prop JsPropertyDescriptor)
		   (with-access::JsPropertyDescriptor prop (name enumerable)
		      (when (eq? enumerable #t)
			 (define-own-property obj name prop properties)))
		   (let* ((name (js-string->name k))
			  (nprop (instantiate::JsValueDescriptor
				    (configurable #t)
				    (enumerable #t)
				    (writable #t)
				    (name name)
				    (value prop))))
		      (cell-set! e nprop)
		      (define-own-property obj name nprop properties)))))))

   (define (defineproperties/properties oprops obj properties)
      (vector-for-each (lambda (prop)
			  (with-access::JsPropertyDescriptor prop (name enumerable)
			     (when (eq? enumerable #t)
				(define-own-property obj name prop properties))))
	 oprops))

   (if (not (js-object? _obj))
       (js-raise-type-error %this
	  "Object.defineProperties called on non-object: ~s" _obj)
       (let ((properties (js-cast-object (js-toobject %this _properties) %this
			    "defineProperties")))
	  (with-access::JsObject properties (cmap elements)
	     (cond
		((js-object-mapped? properties)
		 (defineproperties/cmap cmap _obj properties))
		((js-object-hashed? properties)
		 (defineproperties/hash elements _obj properties))
		(else
		 (defineproperties/properties elements _obj properties))))
	  _obj)))

;*---------------------------------------------------------------------*/
;*    js-seal ::JsObject ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.8     */
;*---------------------------------------------------------------------*/
(define-method (js-seal o::JsObject obj)
   
   (define (prop-seal p)
      (let ((flags (prop-flags p)))
	 (prop (prop-name p)
	    (property-flags
	       (flags-writable? flags)
	       (flags-enumerable? flags)
	       #f
	       (flags-accessor? flags)))))

   (js-object-mode-extensible-set! o #f)
   (js-object-mode-sealed-set! o #t)
   (cond
      ((js-object-mapped? o)
       (with-access::JsObject o (cmap)
	  (with-access::JsConstructMap cmap (props)
	     (let ((ncmap (duplicate::JsConstructMap cmap
			     (props (vector-map prop-seal props)))))
		(set! cmap ncmap)))))
      ((js-object-hashed? o)
       (with-access::JsObject o (elements)
	  (hashtable-for-each elements
	     (lambda (k e)
		(let ((d (cell-ref e)))
		   (if (isa? d JsPropertyDescriptor)
		       (js-seal-property! d)
		       (cell-set! d
			  (instantiate::JsValueDescriptor
			     (name (js-string->name k))
			     (writable #t)
			     (enumerable #t)
			     (configurable #f)
			     (value d)))))))))
      (else
       (with-access::JsObject o (elements)
	  (vector-for-each js-seal-property! elements))))
   obj)

;*---------------------------------------------------------------------*/
;*    js-freeze ::JsObject ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.9     */
;*---------------------------------------------------------------------*/
(define-method (js-freeze o::JsObject obj)
   
   (define (prop-freeze p)
      (let ((flags (prop-flags p)))
	 (prop (prop-name p)
	    (property-flags
	       #f
	       (flags-enumerable? flags)
	       #f
	       (flags-accessor? flags)))))
   
   (js-object-mode-extensible-set! o #f)
   (js-object-mode-frozen-set! o #t)
   (with-access::JsObject o (cmap elements)
      (cond
	 ((js-object-mapped? o)
	  (with-access::JsConstructMap cmap (props)
	     (let ((ncmap (duplicate::JsConstructMap cmap
			     (props (vector-map prop-freeze props)))))
		(set! cmap ncmap))))
	 ((js-object-hashed? o)
	  (with-access::JsObject o (elements)
	     (hashtable-for-each elements
		(lambda (k e)
		   (let ((d (cell-ref e)))
		      (if (isa? d JsPropertyDescriptor)
			  (js-freeze-property! d)
			  (cell-set! d
			     (instantiate::JsValueDescriptor
				(name (js-string->name k))
				(writable #f)
				(enumerable #t)
				(configurable #f)
				(value d)))))))))
	 (else
	  (with-access::JsObject o (elements)
	     (vector-for-each js-freeze-property! elements)))))
   obj)

;*---------------------------------------------------------------------*/
;*    js-object-isfrozen ...                                           */
;*---------------------------------------------------------------------*/
(define (js-object-isfrozen o %this::JsGlobalObject)
   ;; 1
   (let ((o (js-cast-object o %this "isFrozen")))
      (with-access::JsObject o (cmap elements)
	 (and
	  ;; 3
	  (not (js-object-mode-extensible? o))
	  ;; 2
	  (cond
	     ((js-object-mapped? o)
	      (with-access::JsConstructMap cmap (props)
		 (vector-every (lambda (p)
				  (let ((flags (prop-flags p)))
				     (and (not (flags-writable? flags))
					  (not (flags-configurable? flags)))))
		    props)))
	     ((js-object-hashed? o)
	      (let ((frozen #t))
		 (hashtable-for-each elements
		    (lambda (key e)
		       (when frozen
			  (let ((d (cell-ref e)))
			     (if (isa? d JsPropertyDescriptor)
				 (with-access::JsPropertyDescriptor d (configurable)
				    (cond
				       ((eq? configurable #t)
					(set! frozen #f))
				       ((isa? d JsDataDescriptor)
					(with-access::JsDataDescriptor d (writable)
					   (when (eq? writable #t)
					      (set! frozen #f))))))
				 (set! frozen #f))))))
		 frozen))
	     (else
	      (vector-every (lambda (desc::JsPropertyDescriptor)
			       (with-access::JsPropertyDescriptor desc (configurable)
				  (and (not (eq? configurable #t))
				       (or (not (isa? desc JsValueDescriptor))
					   (with-access::JsValueDescriptor desc (writable)
					      (not (eq? writable #t)))))))
		 elements)))))))

;*---------------------------------------------------------------------*/
;*    vector-every ...                                                 */
;*---------------------------------------------------------------------*/
(define (vector-every proc v)
   (let loop ((i (-fx (vector-length v) 1)))
      (cond
	 ((=fx i -1) #t)
	 ((proc (vector-ref v i)) (loop (-fx i 1)))
	 (else #f))))

;*---------------------------------------------------------------------*/
;*    js-toprimitive ...                                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.8       */
;*---------------------------------------------------------------------*/
(define-method (js-toprimitive o::JsObject preferredtype %this::JsGlobalObject)
   
   (define (err)
      (js-raise-type-error %this
	 (format "toPrimitive: illegal default value \"~~a\" (~a)"
	    preferredtype)
	 (symbol->string! (class-name (object-class o)))))

   (define (get-tostring o)
      (with-access::JsGlobalObject %this (js-object-pcache)
	 (js-get-jsobject-name/cache o (& "toString")
	    #f %this (js-pcache-ref js-object-pcache 0))))

   (define (get-valueof o)
      (with-access::JsGlobalObject %this (js-object-pcache)
	 (js-get-jsobject-name/cache o (& "valueOf")
	    #f %this (js-pcache-ref js-object-pcache 1))))

   ;; toprimitive-as-string
   (define (toprimitive-as-string)

      (define (toprimitive-as-string-valueof)
	 (let ((proc (get-valueof o)))
	    (if (js-procedure? proc)
		(let ((r (js-call0 %this proc o)))
		   (if (not (js-object? r))
		       r
		       (err)))
		(err))))
      
      (let ((proc (get-tostring o)))
	 (if (js-procedure? proc)
	     (let ((r (js-call0 %this proc o)))
		(if (not (js-object? r))
		    r
		    (toprimitive-as-string-valueof)))
	     (toprimitive-as-string-valueof))))

   ;; toprimitive-as-number
   (define (toprimitive-as-number)

      (define (toprimitive-as-number-tostring)
	 (let ((proc (get-tostring o)))
	    (if (js-procedure? proc)
		(let ((r (js-call0 %this proc o)))
		   (if (not (js-object? r))
		       r
		       (err)))
		(err))))

      (let ((proc (get-valueof o)))
	 (if (js-procedure? proc)
	     (let ((r (js-call0 %this proc o)))
		(if (not (js-object? r))
		    r
		    (toprimitive-as-number-tostring)))
	     (toprimitive-as-number-tostring))))

   (cond
      ((eq? preferredtype 'string)
       (toprimitive-as-string))
      ((eq? preferredtype 'number)
       (toprimitive-as-number))
      ((isa? o JsDate)
       (toprimitive-as-string))
      ((isa? o JsGlobalObject)
       ;; according to
       ;;   http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.8
       ;; the GlobalObject might be considered as a host object, which gives
       ;; the freedom to default to string instead of number
       (toprimitive-as-string))
      (else
       (toprimitive-as-number))))

;*---------------------------------------------------------------------*/
;*    js-for-in ::JsObject ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.4       */
;*---------------------------------------------------------------------*/
(define-method (js-for-in obj::JsObject proc %this)
   
   (define (in-mapped-property prop)
      (when prop
	 (let ((name (prop-name prop)))
	    (when (js-jsstring? name)
	       (when (flags-enumerable? (prop-flags prop))
		  (proc name %this))))))
   
   (define (vfor-in vecname)
      (let ((len (vector-length vecname)))
	 (let loop ((i 0))
	    (when (<fx i len)
	       (in-mapped-property (vector-ref vecname i))
	       (loop (+fx i 1))))))
   
   (define (in-property p)
      (when (isa? p JsPropertyDescriptor)
	 (with-access::JsPropertyDescriptor p (name enumerable)
	    (when (js-jsstring? name)
	       (when (eq? enumerable #t)
		  (proc name %this))))))

   (define (in-hash k c)
      (let ((p (cell-ref c)))
	 (if (isa? p JsPropertyDescriptor)
	     (with-access::JsPropertyDescriptor p (name enumerable)
		(when (js-jsstring? name)
		   (when (eq? enumerable #t)
		      (proc name %this))))
	     (proc (js-string->jsstring k) %this))))
   
   (with-access::JsObject obj (cmap)
      (when (js-object-mode-enumerable? obj)
	 (cond
	    ((js-object-mapped? obj)
	     (with-access::JsConstructMap cmap (props)
		(vfor-in props)))
	    ((js-object-hashed? obj)
	     (with-access::JsObject obj (elements)
		(hashtable-for-each elements
		   in-hash)))
	    (else
	     (with-access::JsObject obj (elements)
		(vector-for-each in-property elements)))))
      (let ((__proto__ (js-object-proto obj)))
	 (when (js-object? __proto__)
	    (js-for-in-prototype __proto__ obj proc %this)))))

;*---------------------------------------------------------------------*/
;*    js-object-no-setter? ...                                         */
;*---------------------------------------------------------------------*/
(define (js-object-no-setter? obj)
   (when (js-object-mode-plain? obj)
      (let ((__proto__ (js-object-proto obj)))
	 (or (eq? __proto__ obj)
	     (eq? __proto__ (js-null))
	     (js-object-no-setter? __proto__)))))

;*---------------------------------------------------------------------*/
;*    %js-object-prototype-tostring ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.2     */
;*---------------------------------------------------------------------*/
(define-inline (%js-object-prototype-tostring this %this)
   (with-access::JsGlobalObject %this (js-symbol-tostringtag)
      (cond
	 ((js-jsstring? this)
	  (& "[object String]"))
	 ((js-number? this)
	  (& "[object Number]"))
	 ((boolean? this)
	  (& "[object Boolean]"))
	 ((eq? this (js-undefined))
	  (& "[object Undefined]"))
	 ((eq? this (js-null))
	  (& "[object Null]"))
	 (else
	  (let* ((obj (js-toobject %this this))
		 ;; don't cache this field access as it is unlikely that
		 ;; obj has a toStringTag prop
		 (tag (js-get obj js-symbol-tostringtag %this)))
	     (if (js-jsstring? tag)
		 (js-jsstring-append
		    (& "[object")
		    (js-jsstring-append tag (& "]")))
		 (let ((clazz (object-class obj)))
		    (cond
		       ((eq? clazz JsObject)
			(& "[object Object]"))
		       ((eq? clazz JsArray)
			(& "[object Array]"))
		       ((js-procedure? obj)
			(& "[object Function]"))
		       ((eq? clazz JsDate)
			(& "[object Date]"))
		       ((eq? clazz JsRegExp)
			(& "[object RegExp]"))
		       ((eq? clazz JsArguments)
			(& "[object Arguments]"))
		       ((eq? clazz JsString)
			(& "[object String]"))
		       ((eq? clazz JsNumber)
			(& "[object Number]"))
		       ((eq? clazz JsBoolean)
			(& "[object Boolean]"))
		       ((or (eq? clazz JsGlobalObject) (eq? clazz JsProxy))
			(& "[object Object]"))
		       ((isa? this JsWrapper)
			(with-access::JsWrapper this (obj)
			   (js-string->jsstring (js-tostring obj %this))))
		       (else
			(let ((name (symbol->string! (class-name clazz))))
			   (cond
			      ((not (string-prefix? "Js" name))
			       (js-string->jsstring
				  (string-append "[object " name "]")))
			      ((isa? obj JsArrayBufferView)
			       (let ((ctor (js-get obj (& "constructor") %this)))
				  (js-jsstring-append
				     (& "[object ")
				     (js-jsstring-append
					(js-get ctor (& "name") %this) (& "]")))))
			      (else
			       (js-string->jsstring
				  (string-append "[object "
				     (substring name 2)
				     "]"))))))))))))))

;*---------------------------------------------------------------------*/
;*    js-object-prototype-tostring ...                                 */
;*---------------------------------------------------------------------*/
(define (js-object-prototype-tostring this %this)
   (%js-object-prototype-tostring this %this))
   
;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
