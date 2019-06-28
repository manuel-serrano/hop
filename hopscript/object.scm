;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/object.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 17 08:43:24 2013                          */
;*    Last change :  Fri Jun 28 11:08:44 2019 (serrano)                */
;*    Copyright   :  2013-19 Manuel Serrano                            */
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
   
   (include "types.sch" "stringliteral.sch" "property.sch")
   
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
	   __hopscript_dom)

   (export (js-new-global-object::JsGlobalObject #!key (size 64) name)
	   
	   (generic js-extensible?::bool ::obj ::JsGlobalObject)
	   (generic js-preventextensions ::obj ::JsGlobalObject)
	   (generic js-ownkeys ::obj ::JsGlobalObject)

	   (js-object-has-setter? ::JsObject ::JsGlobalObject)))

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
	  (error "obj->string" "Not a JavaScript context" ctx)))
   (lambda (o ctx)
      (if (isa? ctx JsGlobalObject)
	  (js-plist->jsobject o ctx)
	  (error "obj->string" "Not a JavaScript context" ctx))))

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
      (with-access::JsGlobalObject %this (js-object)
	 (let ((nobj (duplicate::JsObject obj
			(__proto__ (js-get js-object (& "prototype") %this)))))
	    (js-object-properties-set! nobj '())
	    (js-object-mode-set! nobj (js-object-mode obj))
	    (js-for-in obj
	       (lambda (k %this)
		  (js-put! nobj (js-donate k worker %_this)
		     (js-donate (js-get/cache obj k %_this) worker %_this)
		     #f %this))
	       %this)
	    nobj))))

;*---------------------------------------------------------------------*/
;*    xml-primitive-value ::JsObject ...                               */
;*---------------------------------------------------------------------*/
(define-method (xml-primitive-value obj::JsObject ctx)
   (if (isa? ctx JsGlobalObject)
       (js-jsobject->plist obj ctx)
       (error "xml-primitive-value ::JsObject" "Not a JavaScript context" ctx)))

;*---------------------------------------------------------------------*/
;*    xml-unpack ::JsObject ...                                        */
;*    -------------------------------------------------------------    */
;*    Used when an JS object is to pack the arguments sent to          */
;*    an XML constructor.                                              */
;*---------------------------------------------------------------------*/
(define-method (xml-unpack o::JsObject ctx)
   (if (isa? ctx JsGlobalObject)
       (js-jsobject->keyword-plist o ctx)
       (error "xml-unpack ::JsObject" "Not a JavaScript context" ctx)))

;*---------------------------------------------------------------------*/
;*    obj->json ::JsObject ...                                         */
;*---------------------------------------------------------------------*/
(define-method (obj->json obj::JsObject op::output-port ctx)
   (if (isa? ctx JsGlobalObject)
       (let ((%this ctx))
	  (let ((stringify (js-json-stringify %this)))
	     (display (stringify (js-undefined) obj (js-undefined) 1) op)))
       (error "obj->json" "Not a JavaScript context" ctx)))

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
	       (let ((proc (js-object-get-name/cache obj (& "toResponse") #f %this
			      (js-pcache-ref js-service-pcache 0))))
		  (if (js-function? proc)
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
		     (js-get/cache o p %this)
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
      
      ;; init the builtin function class
      (js-init-function! %this)
      ;; the object constructor
      (with-access::JsGlobalObject %this (js-function js-object js-initial-cmap)
	 (set! js-initial-cmap (instantiate::JsConstructMap))
	 (with-access::JsFunction js-function ((js-function-prototype __proto__))
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
	       :value (js-make-function %this js-eval 1 "eval"
			 :prototype (js-undefined)
			 :src "object.scm")
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)
	    
	    (js-bind! %this %this (& "parseInt")
	       :value (js-make-function %this
			 (lambda (this string radix)
			    (js-parseint string radix %this))
			 2 "parseInt"
			 :prototype (js-undefined)
			 :src "object.scm")
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    (js-bind! %this %this (& "parseFloat")
	       :value (js-make-function %this
			 (lambda (this string)
			    (js-parsefloat string %this))
			 1 "parseFloat"
			 :prototype (js-undefined)
			 :src "object.scm")
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    ;; isNaN
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.4
	    (define (isnan this val)
	       (js-isnan? val %this))

	    (js-bind! %this %this (& "isNaN")
	       :value (js-make-function %this isnan 1 "isNaN"
			 :prototype (js-undefined)
			 :src "object.scm")
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
	       :value (js-make-function %this isfinite 1 "isFinite"
			 :prototype (js-undefined)
			 :src "object.scm")
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
	       :value (js-make-function %this decodeuri 1 "decodeURI"
			 :prototype (js-undefined)
			 :src "object.scm")
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
			 1 "decodeURIComponent"
			 :prototype (js-undefined)
			 :src "object.scm")
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    ;; encodeURI
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.3.3
	    (define (encodeuri this string)
	       (let ((str (js-tostring string %this)))
		  (if (utf8-string? str #t)
		      (js-string->jsstring (uri-encode str))
		      (js-raise-uri-error %this "Badly formed url ~s" string))))

	    (js-bind! %this %this (& "encodeURI")
	       :value (js-make-function %this encodeuri 1 "encodeURI"
			 :prototype (js-undefined)
			 :src "object.scm")
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    ;; encodeURIComponent
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.3.4
	    (define (encodeuricomponent this string)
	       (let ((str (js-tostring string %this)))
		  (if (utf8-string? str #t)
		      (js-string->jsstring (uri-encode-component str))
		      (js-raise-uri-error %this "Badly formed url ~s" string))))

	    (js-bind! %this %this (& "encodeURIComponent")
	       :value (js-make-function %this encodeuricomponent
			 1 "encodeURIComponent"
			 :prototype (js-undefined)
			 :src "object.scm")
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    ;; escape
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.1
	    (define (escape this string)
	       (js-jsstring-escape (js-tojsstring string %this)))
	    
	    (js-bind! %this %this (& "escape")
	       :value (js-make-function %this escape 1 "escape"
			 :prototype (js-undefined)
			 :src "object.scm")
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    ;; unescape
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.1
	    (define (unescape this string)
	       (js-jsstring-unescape (js-tojsstring string %this) %this))

	    (js-bind! %this %this (& "unescape")
	       :value (js-make-function %this unescape 1 "unescape"
			 :prototype (js-undefined)
			 :src "object.scm")
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
		  1 "HTML"
		  :src "object.scm"))
	    
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
			 1 "Tilde"
			 :__proto__ js-function-prototype
			 :construct (lambda (this body)
				       (string->xml-tilde body))
			 :src "object.scm")
	       :enumerable #f :writable #f :configurable #f :hidden-class #f)

	    ;; return the newly created object
	    %this))))

;*---------------------------------------------------------------------*/
;*    js-init-object! ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3       */
;*---------------------------------------------------------------------*/
(define (js-init-object! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this ((%proto __proto__)
				       js-object js-function)
      
      ;; Object.prototype
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.1
      (define (%js-object this . value)
	 (cond
	    ((null? value)
	     (js-new %this js-object (js-undefined)))
	    ((or (eq? (car value) (js-null)) (eq? (car value) (js-undefined)))
	     (js-new %this js-object (car value)))
	    (else
	     (js-toobject %this (car value)))))

      ;; Object.constructor
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.2.1
      (define (js-object-constructor %this f value)
	 (with-access::JsGlobalObject %this (js-string js-boolean js-number)
	    (cond
	       ((or (eq? value (js-null)) (eq? value (js-undefined)))
		;; 2
		(with-access::JsFunction f (constrmap constrsize)
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
		(js-raise-type-error %this "illegal value ~s" value)))))

      (define (js-object-construct f . arg)
	 (with-access::JsGlobalObject %this (js-object)
	    (js-object-constructor %this js-object
	       (if (pair? arg) (car arg) (js-undefined)))))
      
      (with-access::JsObject js-function ((js-function-prototype __proto__))
	 (set! js-object
	    (js-function-set-constrmap!
	       (js-make-function %this %js-object 1 "Object"
		  :__proto__ js-function-prototype
		  :constrsize 3 :maxconstrsize 4
		  :prototype %proto
		  :alloc js-no-alloc
		  :size 21
		  :construct js-object-construct
		  :src "object.scm"
		  :shared-cmap #f))))

      ;; getPrototypeOf
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.2
      (define (getprototypeof this o)
	 (js-getprototypeof o %this "getPrototypeOf"))
      
      (js-bind! %this js-object (& "getPrototypeOf")
	 :value (js-make-function %this getprototypeof 1 "getPrototypeOf"
		   :src "object.scm")
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
	 :value (js-make-function %this setprototypeof 1 "setPrototypeOf"
		   :src "object.scm")
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; getOwnPropertyDescriptor
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.3
      (define (getownpropertydescriptor this o p)
	 (let* ((o (if (isa? o object)
		       o
		       (js-cast-object o %this "getOwnPropertyDescriptor")))
		(desc (js-get-own-property o p %this)))
	    (js-from-property-descriptor %this o desc o)))
      
      (js-bind! %this js-object (& "getOwnPropertyDescriptor")
	 :value (js-make-function %this
		   getownpropertydescriptor 2 "getOwnPropertyDescriptor"
		   :src "object.scm")
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; getOwnPropertyDescriptors
      ;; https://www.ecma-international.org/ecma-262/8.0/#sec-object.getownpropertydescriptors
      (define (getownpropertydescriptors this o p)
	 (let* ((o (if (isa? o object)
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
		   getownpropertydescriptors 1 "getOwnPropertyDescriptors"
		   :src "object.scm")
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
		   getownpropertynames 1 "getOwnPropertyNames"
		   :src "object.scm")
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
		   getownpropertysymbols 1 "getOwnPropertySymbols"
		   :src "object.scm")
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
		(with-access::JsObject obj (__proto__ elements)
		   (set! __proto__ o)
		   (unless (eq? properties (js-undefined))
		      (object-defineproperties %this this obj properties)))
		obj)))

      (js-bind! %this js-object (& "create")
	 :value (js-make-function %this create 2 "create"
		   :src "object.scm")
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
	 :value (js-make-function %this defineproperty 3 "defineProperty"
		   :src "object.scm")
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
	 :value (js-make-function %this assign 2 "assign"
		   :src "object.scm")
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
		   2 "defineProperties"
		   :src "object.scm")
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
	 :value (js-make-function %this seal 1 "seal"
		   :src "object.scm")
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
	 :value (js-make-function %this freeze 1 "freeze"
		   :src "object.scm")
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      (js-bind! %this js-object (& "preventExtensions")
	 :value (js-make-function %this
		   (lambda (this obj)
		      (js-preventextensions obj %this))
		   1 "preventExtensions"
		   :src "object.scm")
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)

      (define (vector-every proc v)
	 (let loop ((i (-fx (vector-length v) 1)))
	    (cond
	       ((=fx i -1) #t)
	       ((proc (vector-ref v i)) (loop (-fx i 1)))
	       (else #f))))
      
      ;; is
      ;; https://www.ecma-international.org/ecma-262/6.0/#sec-object.is
      (define (is this x y)
	 ;; SameValue algorithm
	 (or (js-eqstring? x y)
	     ;; eqstring test eq first
	     (and (flonum? x) (flonum? y)
		  (or (and (=fl x y) (=fx (signbitfl x) (signbitfl y)))
		      (and (nanfl? x) (nanfl? y))))))

      (js-bind! %this js-object (& "is")
	 :value (js-make-function %this is 2 "is"
		   :src "object.scm")
	 :enumerable #f :configurable #t :writable #t :hidden-class #f)
	    
      ;; isSealed
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.11
      (define (issealed this o)
	 ;; 1
	 (let ((o (js-cast-object o %this "isSealed")))
	    (with-access::JsObject o (cmap)
	       (and
		;; 2
		(with-access::JsConstructMap cmap (props)
		   (vector-every (lambda (p)
				    (let ((flags (prop-flags p)))
				       (not (flags-configurable? flags))))
		      props))
		(every (lambda (desc::JsPropertyDescriptor)
			  (with-access::JsPropertyDescriptor desc (configurable)
			     (not (eq? configurable #t))))
		   (js-object-properties o))
		;; 3
		(not (js-object-mode-extensible? o))))))
      
      (js-bind! %this js-object (& "isSealed")
	 :value (js-make-function %this issealed 1 "isSealed"
		   :src "object.scm")
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; isFrozen
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.12
      (define (isfrozen this o)
	 ;; 1
	 (let ((o (js-cast-object o %this "isFrozen")))
	    (with-access::JsObject o (cmap)
	       (and
		;; 2
		(with-access::JsConstructMap cmap (props)
		   (vector-every (lambda (p)
				    (let ((flags (prop-flags p)))
				       (and (not (flags-writable? flags))
					    (not (flags-configurable? flags)))))
			props))	     
		(every (lambda (desc::JsPropertyDescriptor)
			  (with-access::JsPropertyDescriptor desc (configurable)
			     (and (not (eq? configurable #t))
				  (or (not (isa? desc JsValueDescriptor))
				      (with-access::JsValueDescriptor desc (writable)
					 (not (eq? writable #t)))))))
		   (js-object-properties o))
		;; 3
		(not (js-object-mode-extensible? o))))))
      
      (js-bind! %this js-object (& "isFrozen")
	 :value (js-make-function %this isfrozen 1 "isFrozen"
		   :src "object.scm")
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
		   1 "isExtensible"
		   :src "object.scm")
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)

      ;; keys
      ;; https://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.14
      (js-bind! %this js-object (& "keys")
	 :value (js-make-function %this (lambda (this obj)
					   (js-ownkeys obj %this))
		   1 "keys"
		   :src "object.scm")
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
   (with-access::JsGlobalObject %this ((obj __proto__)
				       js-object
				       js-symbol-tostringtag)
      
      ;; __proto__
      (js-bind! %this obj (& "__proto__")
	 :enumerable #f
	 :configurable #f
	 :get (js-make-function %this
		 (lambda (o)
		    (js-getprototypeof o %this "__proto__"))
		 1 "get"
		 :src "object.scm")
	 :set (js-make-function %this
		 (lambda (o v)
		    (js-setprototypeof o v %this "__proto__"))
		 2 "set"
		 :src "object.scm")
	 :hidden-class #f)
      
      ;; constructor
      (js-bind! %this obj (& "constructor")
	 :value js-object
	 :enumerable #f
	 :hidden-class #f)
      
      ;; toString
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.2
      (define (js-object-prototype-tostring this)
	 (cond
	    ((eq? this (js-undefined))
	     (js-ascii->jsstring "[object Undefined]"))
	    ((eq? this (js-null))
	     (js-ascii->jsstring "[object Null]"))
	    ((isa? this JsWrapper)
	     (with-access::JsWrapper this (obj)
		(js-string->jsstring (js-tostring obj %this))))
	    (else
	     (let* ((obj (js-toobject %this this))
		    (tag (js-get obj js-symbol-tostringtag %this)))
		(if (js-jsstring? tag)
		    (js-jsstring-append
		       (js-string->jsstring "[object")
		       (js-jsstring-append tag
			  (js-string->jsstring "]")))
		    (let* ((clazz (if (js-function? obj)
				      JsFunction
				      (object-class obj)))
			   (name (symbol->string! (class-name clazz))))
		       (js-string->jsstring
			  (cond
			     ((not (string-prefix? "Js" name))
			      (string-append "[object " name "]"))
			     ((string=? name "JsGlobalObject")
			      "[object Object]")
			     ((isa? obj JsArrayBufferView)
			      (let ((ctor (js-get obj (& "constructor") %this)))
				 (string-append "[object "
				    (js-get ctor (& "name") %this)
				    "]")))
			     
			     (else
			      (string-append "[object "
				 (substring name 2)
				 "]"))))))))))
      
      (js-bind! %this obj (& "toString")
	 :value (js-make-function %this
		   js-object-prototype-tostring 0 "toString"
		   :prototype (js-undefined)
		   :src "object.scm")
	 :enumerable #f
	 :hidden-class #f)
      
      ;; toLocaleString
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.3
      (define (js-object-prototype-tolocalestring this)
	 (js-call0 %this (js-get this (& "toString") %this) this))
      
      (js-bind! %this obj (& "toLocaleString")
	 :value (js-make-function %this
		   js-object-prototype-tolocalestring 0 "toLocaleString"
		   :prototype (js-undefined)
		   :src "object.scm")
	 :enumerable #f
	 :hidden-class #f)
      
      ;; valueOf
      (js-bind! %this obj (& "valueOf")
	 :value (js-make-function %this (lambda (o) (js-valueof o %this))
		   0 "valueOf"
		   :prototype (js-undefined)
		   :src "object.scm")
	 :enumerable #f
	 :hidden-class #f)
      
      ;; hasOwnProperty
      (js-bind! %this obj (& "hasOwnProperty")
	 :value (js-make-function %this
		   (lambda (this v)
		      (js-object-prototype-hasownproperty this v %this))
		   1 "hasOwnProperty"
		   :prototype (js-undefined)
		   :src "object.scm")
	 :enumerable #f
	 :hidden-class #f)
      
      ;; isPrototypeOf
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.6
      (define (js-object-prototype-isprototypeof this v)
	 (when (js-object? v)
	    (let ((o (js-toobject %this this)))
	       (let loop ((v v))
		  (with-access::JsObject v ((v __proto__))
		     (unless (eq? v (js-null))
			(or (eq? v o) (loop v))))))))
      
      (js-bind! %this obj (& "isPrototypeOf")
	 :value (js-make-function %this
		   js-object-prototype-isprototypeof 1 "isPrototypeOf"
		   :prototype (js-undefined)
		   :src "object.scm")
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
		   js-object-prototype-propertyisenumerable 1 "propertyIsEnumerable"
		   :prototype (js-undefined)
		   :src "object.scm")
	 :enumerable #f
	 :hidden-class #f)))

;*---------------------------------------------------------------------*/
;*    js-object-prototype-hasownproperty ...                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.5     */
;*---------------------------------------------------------------------*/
(define (js-object-prototype-hasownproperty this v %this)
   ;; the conversion ToPropertyKey is implemented by js-has-own-property
   (js-has-own-property (js-toobject %this this) v %this))

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
   (js-vector->jsarray (js-properties-name obj #t %this) %this))

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
   
   (define (defineproperties/properties oprops obj properties)
      (for-each (lambda (prop)
		   (with-access::JsPropertyDescriptor prop (name enumerable)
		      (when (eq? enumerable #t)
			 (define-own-property obj name prop properties))))
	 oprops))
   
   (if (not (js-object? _obj))
       (js-raise-type-error %this
	  "Object.defineProperties called on non-object: ~s" _obj)
       (let ((properties (js-cast-object (js-toobject %this _properties) %this
			    "defineProperties")))
	  (with-access::JsObject properties (cmap)
	     (if (eq? cmap (js-not-a-cmap))
		 (let ((oprops (js-object-properties properties)))
		    (defineproperties/properties oprops _obj properties))
		 (defineproperties/cmap cmap _obj properties)))
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

   (for-each js-seal-property! (js-object-properties o))
   (js-object-mode-extensible-set! o #f)
   (js-object-mode-sealed-set! o #t)
   (with-access::JsObject o (cmap)
      (unless (eq? cmap (js-not-a-cmap))
	 (with-access::JsConstructMap cmap (props)
	    (let ((ncmap (duplicate::JsConstructMap cmap
			    (props (vector-map prop-seal props)))))
	       (set! cmap ncmap)))))
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

   (for-each js-freeze-property! (js-object-properties o))
   (js-object-mode-extensible-set! o #f)
   (js-object-mode-frozen-set! o #t)
   (with-access::JsObject o (cmap)
      (unless (eq? cmap (js-not-a-cmap))
	 (with-access::JsConstructMap cmap (props)
	    (let ((ncmap (duplicate::JsConstructMap cmap
			    (props (vector-map prop-freeze props)))))
	       (set! cmap ncmap)))))
   obj)

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
   
   (define (get-field-value . fields)
      (let loop ((fields fields))
	 (if (null? fields)
	     (err)
	     (let ((proc (js-get o (car fields) %this)))
		(if (js-function? proc)
		    (let ((r (js-call0 %this proc o)))
		       (if (not (js-object? r))
			   r
			   (loop (cdr fields))))
		    (loop (cdr fields)))))))
   
   (define (primitive-as-string)
      (get-field-value (& "toString") (& "valueOf")))
   
   (define (primitive-as-number)
      (get-field-value (& "valueOf") (& "toString")))

   (cond
      ((eq? preferredtype 'string)
       (primitive-as-string))
      ((eq? preferredtype 'number)
       (primitive-as-number))
      ((isa? o JsDate)
       (primitive-as-string))
      ((isa? o JsGlobalObject)
       ;; according to
       ;;   http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.8
       ;; the GlobalObject might be considered as a host object, which gives
       ;; the freedom to default to string instead of number
       (primitive-as-string))
      (else
       (primitive-as-number))))

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
   
   (with-access::JsObject obj (cmap __proto__ elements)
      (when (js-object-mode-enumerable? obj)
	 (if (not (eq? cmap (js-not-a-cmap)))
	     (with-access::JsConstructMap cmap (props)
		(vfor-in props))
	     (for-each in-property (js-object-properties obj))))
      (when (js-object? __proto__)
	 (js-for-in-prototype __proto__ obj proc %this))))

;*---------------------------------------------------------------------*/
;*    js-object-has-setter? ...                                        */
;*    -------------------------------------------------------------    */
;*    THis function is an over-approximation of the HAS-A-SETTER       */
;*    predicate. An object is said to have no setter if:               */
;*      1) it does not own any setter.                                 */
;*      2) it's __proto__ is Object.__proto__                          */
;*      3) Object.__proto__ has no setter                              */
;*---------------------------------------------------------------------*/
(define (js-object-has-setter? obj %this)
   (when (js-object-mode-plain? obj)
      (with-access::JsObject obj (__proto__)
	 (when (js-object-mode-plain? __proto__)
	    (with-access::JsGlobalObject %this ((%proto __proto__))
	       (eq? %proto __proto__))))))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
