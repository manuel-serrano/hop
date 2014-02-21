;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/hopscript/object.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 17 08:43:24 2013                          */
;*    Last change :  Wed Feb 12 18:28:29 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
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

   (import __hopscript_types
	   __hopscript_string
	   __hopscript_function
	   __hopscript_number
	   __hopscript_math
	   __hopscript_boolean
	   __hopscript_regexp
	   __hopscript_array
	   __hopscript_date
	   __hopscript_error
	   __hopscript_json
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public)

   (export js-object
	   js-object-prototype
	   js-global-object
	   (js-init-global-object!)
	   
	   js-eval
	   js-parse-int
	   js-parse-float
	   js-is-nan
	   js-is-finite
	   js-decode-uri
	   js-decode-uri-component
	   js-encode-uri
	   js-encode-uri-component
	   js-escape
	   js-unescape

	   (js-object-prototype-hasownproperty this v)

	   (js-get-global-object-name
	      ::JsObject ::symbol ::obj)
	   (inline js-get-global-object-name/cache
	      ::JsObject ::symbol ::JsPropertyCache ::obj)))

;*---------------------------------------------------------------------*/
;*    js-object ...                                                    */
;*---------------------------------------------------------------------*/
(define js-object #f)
(define js-object-prototype #f)

;*---------------------------------------------------------------------*/
;*    js-global-object ...                                             */
;*---------------------------------------------------------------------*/
(define js-global-object #f)

;*---------------------------------------------------------------------*/
;*    global runtime constants ...                                     */
;*---------------------------------------------------------------------*/
(define js-eval #f)
(define js-parse-int #f)
(define js-parse-float #f)
(define js-is-nan #f)
(define js-is-finite #f)
(define js-decode-uri #f)
(define js-decode-uri-component #f)
(define js-encode-uri #f)
(define js-encode-uri-component #f)
(define js-escape #f)
(define js-unescape #f)

;*---------------------------------------------------------------------*/
;*    js-init-global-object! ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1         */
;*---------------------------------------------------------------------*/
(define (js-init-global-object!)
   (unless js-global-object
      ;; before all, initialize the builtin object prototype
      (set! js-object-prototype
	 (instantiate::JsObject
	    (__proto__ (js-null))
	    (elements (make-vector 11))
	    (cmap (instantiate::JsConstructMap))
	    (extensible #t)))
      ;; then create the global object
      (set! js-global-object
	 (instantiate::JsGlobalObject
	    (__proto__ js-object-prototype)
	    (cmap (instantiate::JsConstructMap))))
      ;; init the builtin classes
      (js-init-function! js-global-object)
      ;; the object constructor
      (set! js-object 
	 (js-make-function %js-object 1 "Object" 
	    :__proto__ js-function-prototype
	    :prototype js-object-prototype
	    :construct js-object-construct))
      (js-init-builtin-object-prototype! js-object-prototype)
      (js-init-object-properties!)
      (js-init-array! js-global-object)
      (js-init-string! js-global-object)
      (js-init-boolean! js-global-object)
      (js-init-number! js-global-object)
      (js-init-math! js-global-object)
      (js-init-regexp! js-global-object)
      (js-init-date! js-global-object)
      (js-init-error! js-global-object)
      (js-init-json! js-global-object)
      ;; bind the global object properties
      (js-bind! js-global-object 'Object
	 :value js-object
	 :writable #t :enumerable #f :configurable #f)
      (js-bind! js-global-object 'NaN
	 :value +nan.0
	 :writable #f :enumerable #f :configurable #f)
      (js-bind! js-global-object 'Infinity
	 :value +inf.0
	 :writable #f :enumerable #f :configurable #f)
      (js-bind! js-global-object 'undefined
	 :value (js-undefined)
	 :writable #f :enumerable #f :configurable #f)
      (js-bind! js-global-object 'eval
	 :value (js-make-function
		   js-global-object-eval 1 "eval"
		   :prototype (js-undefined))
	 :enumerable #f :configurable #t :writable #t)
      (js-bind! js-global-object 'parseInt
	 :value (js-make-function
		   js-global-object-parseint 2 "parseInt"
		   :prototype (js-undefined))
	 :enumerable #f :configurable #t :writable #t)
      (js-bind! js-global-object 'parseFloat
	 :value (js-make-function
		   js-global-object-parsefloat 1 "parseFloat"
		   :prototype (js-undefined))
	 :enumerable #f :configurable #t :writable #t)
      (js-bind! js-global-object 'isNaN
	 :value (js-make-function
		   js-global-object-isnan 1 "isNaN"
		   :prototype (js-undefined))
	 :enumerable #f :configurable #t :writable #t)
      (js-bind! js-global-object 'isFinite
	 :value (js-make-function
		   js-global-object-isfinite 1 "isFinite"
		   :prototype (js-undefined))
	 :enumerable #f :configurable #t :writable #t)
      (js-bind! js-global-object 'decodeURI
	 :value (js-make-function
		   js-global-object-decodeuri 1 "decodeURI"
		   :prototype (js-undefined))
	 :enumerable #f :configurable #t :writable #t)
      (js-bind! js-global-object 'decodeURIComponent
	 :value (js-make-function
		   js-global-object-decodeuricomponent 1 "decodeURIComponent"
		   :prototype (js-undefined))
	 :enumerable #f :configurable #t :writable #t)
      (js-bind! js-global-object 'encodeURI
	 :value (js-make-function
		   js-global-object-encodeuri 1 "encodeURI"
		   :prototype (js-undefined))
	 :enumerable #f :configurable #t :writable #t)
      (js-bind! js-global-object 'encodeURIComponent
	 :value (js-make-function
		   js-global-object-encodeuricomponent 1 "encodeURIComponent"
		   :prototype (js-undefined))
	 :enumerable #f :configurable #t :writable #t)
      (js-bind! js-global-object 'escape
	 :value (js-make-function
		   js-global-object-escape 1 "escape"
		   :prototype (js-undefined))
	 :enumerable #f :configurable #t :writable #t)
      (js-bind! js-global-object 'unescape
	 :value (js-make-function
		   js-global-object-unescape 1 "unescape"
		   :prototype (js-undefined))
	 :enumerable #f :configurable #t :writable #t))
   js-global-object)

;*---------------------------------------------------------------------*/
;*    js-init-object-properties! ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3       */
;*    -------------------------------------------------------------    */
;*    Cannot be called by js-init-object! because require functions    */
;*    to be properly initialized first (see js-global-object).         */
;*---------------------------------------------------------------------*/
(define (js-init-object-properties!)
   (let ((obj js-object))
      ;; getPrototypeOf
      (js-bind! obj 'getPrototypeOf
	 :value (js-make-function object-getprototypeof 1 "getPrototypeOf")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      ;; getOwnPropertyDescriptor
      (js-bind! obj 'getOwnPropertyDescriptor
	 :value (js-make-function object-getownpropertydescriptor 2 "getOwnPropertyDescriptor")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      ;; getOwnPropertyNames
      (js-bind! obj 'getOwnPropertyNames
	 :value (js-make-function object-getownpropertynames 1 "getOwnPropertyNames")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      ;; create
      (js-bind! obj 'create
	 :value (js-make-function object-create 2 "create")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      ;; defineProperty
      (js-bind! obj 'defineProperty
	 :value (js-make-function object-defineproperty 3 "defineProperty")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      ;; defineProperties
      (js-bind! obj 'defineProperties
	 :value (js-make-function object-defineproperties 2 "defineProperties")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      ;; seal
      (js-bind! obj 'seal
	 :value (js-make-function object-seal 1 "seal")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      ;; freeze
      (js-bind! obj 'freeze
	 :value (js-make-function object-freeze 1 "freeze")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      ;; preventExtensions
      (js-bind! obj 'preventExtensions
	 :value (js-make-function object-preventextensions 1 "preventExtensions")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      ;; isSealed
      (js-bind! obj 'isSealed
	 :value (js-make-function object-issealed 1 "isSealed")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      ;; isFrozen
      (js-bind! obj 'isFrozen
	 :value (js-make-function object-isfrozen 1 "isFrozen")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      ;; isExtensible
      (js-bind! obj 'isExtensible
	 :value (js-make-function object-isextensible 1 "isExtensible")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      ;; keys
      (js-bind! obj 'keys
	 :value (js-make-function object-keys 1 "keys")
	 :writable #t
	 :configurable #t
	 :enumerable #f)))

;*---------------------------------------------------------------------*/
;*    js-init-builtin-object-prototype! ...                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4       */
;*    -------------------------------------------------------------    */
;*    Cannot be merge with JS-INIT-BUILTIN-OBJECT-PROTOTYPE! because   */
;*    when this forme function is invoked, the                         */
;*    JS-BUILTIN-FUNCTION-PROTOTYPE has not already been constructed.  */
;*---------------------------------------------------------------------*/
(define (js-init-builtin-object-prototype! obj)
   ;; __proto__
   (js-bind! obj '__proto__
      :enumerable #f
      :configurable #f
      :get (js-make-function
	      (lambda (o)
		 (let ((o (js-cast-object o "__proto__")))
		    (with-access::JsObject o (__proto__)
		       __proto__)))
	      1 "get")
      :set (js-make-function
	      (lambda (o v)
		 (let ((o (js-cast-object o "__proto__"))
		       (v (js-cast-object v "__proto__")))
		    (with-access::JsObject o (extensible)
		       (if (not extensible)
			   (js-raise-type-error 
			      "Prototype of non-extensible object mutated" v)
			   (with-access::JsObject o (__proto__)
			      (set! __proto__ v))))))
	      2 "set"))
   ;; constructor
   (js-bind! obj 'constructor
      :value js-object
      :enumerable #f)
   ;; toString
   (js-bind! obj 'toString
      :value (js-make-function
		js-object-prototype-tostring 0 "toString"
		:prototype (js-undefined))
      :enumerable #f)
   ;; toLocaleString
   (js-bind! obj 'toLocaleString
      :value (js-make-function
		js-object-prototype-tolocalestring 0 "toLocaleString"
		:prototype (js-undefined))
      :enumerable #f)
   ;; valueOf
   (js-bind! obj 'valueOf
      :value (js-make-function
		js-valueof 0 "valueOf"
		:prototype (js-undefined))
      :enumerable #f)
   ;; hasOwnProperty
   (js-bind! obj 'hasOwnProperty
      :value (js-make-function
		js-object-prototype-hasownproperty 1 "hasOwnProperty"
		:prototype (js-undefined))
      :enumerable #f)
   ;; isPrototypeOf
   (js-bind! obj 'isPrototypeOf
      :value (js-make-function
		js-object-prototype-isprototypeof 1 "isPrototypeOf"
		:prototype (js-undefined))
      :enumerable #f)
   ;; propertyIsEnumerable
   (js-bind! obj 'propertyIsEnumerable
      :value (js-make-function
		js-object-prototype-propertyisenumerable 1 "propertyIsEnumerable"
		:prototype (js-undefined))
      :enumerable #f))

;*---------------------------------------------------------------------*/
;*    %js-object ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.1.1     */
;*---------------------------------------------------------------------*/
(define (%js-object this . value)
   (cond
      ((null? value)
       (js-new js-object (js-undefined)))
      ((or (eq? (car value) (js-null)) (eq? (car value) (js-undefined)))
       (js-new js-object (car value)))
      (else
       (js-toobject (car value)))))

;*---------------------------------------------------------------------*/
;*    js-object-construct ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.2.1     */
;*---------------------------------------------------------------------*/
(define (js-object-construct _ . arg)

   (define (object-alloc)
      (instantiate::JsObject
	 (cmap (instantiate::JsConstructMap))
	 (elements (make-vector 4))
	 (__proto__ js-object-prototype)
	 (extensible #t)))
   
   (if (null? arg)
       ;; 2
       (object-alloc)
       (let ((value (car arg)))
	  (cond
	     ((or (eq? value (js-null)) (eq? value (js-undefined)))
	      ;; 2
	      (object-alloc))
	     ((isa? value JsObject)
	      ;; 1.a
	      value)
	     ((string? value)
	      ;; 1.b
	      (js-new js-string value))
	     ((boolean? value)
	      ;; 1.c
	      (js-new js-boolean value))
	     ((number? value)
	      ;; 1.c
	      (js-new js-number value))
	     (else
	      (js-raise-type-error "illegal value ~s" value))))))

;*---------------------------------------------------------------------*/
;*    js-get ::JsObject ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.3       */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsObject p)
   (js-getvalue o o (js-toname p) #f))

;*---------------------------------------------------------------------*/
;*    js-tonumber ::JsObject ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-method (js-tonumber obj::JsObject)
   (let ((v (js-toprimitive obj 'number)))
      (js-tonumber (js-toprimitive obj 'number))))
   
;*---------------------------------------------------------------------*/
;*    js-tointeger ::JsObject ...                                      */
;*---------------------------------------------------------------------*/
(define-method (js-tointeger obj::JsObject)
   (js-tointeger (js-tonumber obj)))
   
;*---------------------------------------------------------------------*/
;*    js-object-prototype-tostring ...                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.2     */
;*---------------------------------------------------------------------*/
(define (js-object-prototype-tostring this)
   (cond
      ((eq? this (js-undefined))
       "[object Undefined]")
      ((eq? this (js-null))
       "[object Null]")
      (else
       (let* ((obj (js-toobject this))
	      (name (symbol->string! (class-name (object-class obj)))))
	  (format "[object ~a]"
	     (cond
		((not (string-prefix? "Js" name))
		 name)
		((string=? name "JsGlobalObject")
		 "Object")
		(else
		 (substring name 2))))))))

;*---------------------------------------------------------------------*/
;*    js-object-prototype-tolocalestring ...                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.3     */
;*---------------------------------------------------------------------*/
(define (js-object-prototype-tolocalestring this)
   (js-call0 (js-get this 'toString) this))

;*---------------------------------------------------------------------*/
;*    js-valueof ::JsObject ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.4     */
;*---------------------------------------------------------------------*/
(define-method (js-valueof this::JsObject)
   this)

;*---------------------------------------------------------------------*/
;*    js-valueof ::JsGlobalObject ...                                  */
;*---------------------------------------------------------------------*/
(define-method (js-valueof this::JsGlobalObject)
   (js-toobject (js-null)))

;*---------------------------------------------------------------------*/
;*    js-object-prototype-hasownproperty ...                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.5     */
;*---------------------------------------------------------------------*/
(define (js-object-prototype-hasownproperty this v)
   (let* ((p (js-tostring v))
	  (desc (js-get-own-property this p)))
      (not (eq? desc (js-undefined)))))

;*---------------------------------------------------------------------*/
;*    js-object-prototype-isprototypeof ...                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.6     */
;*---------------------------------------------------------------------*/
(define (js-object-prototype-isprototypeof this v)
   (when (isa? v JsObject)
      (let ((o (js-toobject this)))
	 (let loop ((v v))
	    (with-access::JsObject v ((v __proto__))
	       (unless (eq? v (js-null))
		  (or (eq? v o) (loop v))))))))

;*---------------------------------------------------------------------*/
;*    js-object-prototype-propertyisenumerable ...                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.7     */
;*---------------------------------------------------------------------*/
(define (js-object-prototype-propertyisenumerable this v)
   (let* ((p (js-tostring v))
	  (desc (js-get-own-property this p)))
      (if (eq? desc (js-undefined))
	  #f
	  (with-access::JsPropertyDescriptor desc (enumerable) enumerable))))
   
;*---------------------------------------------------------------------*/
;*    object-getprototypeof ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.2     */
;*---------------------------------------------------------------------*/
(define (object-getprototypeof this o)
   (let ((o (js-cast-object o "getPrototypeOf")))
      (with-access::JsObject o (__proto__)
	 __proto__)))

;*---------------------------------------------------------------------*/
;*    object-getownpropertydescriptor ...                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.3     */
;*---------------------------------------------------------------------*/
(define (object-getownpropertydescriptor this o p)
   (let* ((o (js-cast-object o "getOwnPropertyDescriptor"))
	  (desc (js-get-own-property o p)))
      (js-from-property-descriptor desc)))

;*---------------------------------------------------------------------*/
;*    object-getownpropertynames ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.4     */
;*---------------------------------------------------------------------*/
(define (object-getownpropertynames this o p)
   (let ((o (js-cast-object o "getOwnPropertyNames")))
      (js-vector->jsarray
	 (js-property-names o #f))))

;*---------------------------------------------------------------------*/
;*    object-create ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.5     */
;*---------------------------------------------------------------------*/
(define (object-create this o properties)
   (if (not (or (eq? o (js-null)) (isa? o JsObject)))
       (js-raise-type-error "create: bad object ~s" o)
       (let ((obj (js-new js-object)))
	  (with-access::JsObject obj (__proto__)
	     (set! __proto__ o)
	     (unless (eq? properties (js-undefined))
		(object-defineproperties this obj properties)))
	  obj)))

;*---------------------------------------------------------------------*/
;*    object-defineproperty ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.6     */
;*---------------------------------------------------------------------*/
(define (object-defineproperty this obj p attributes)
   (let* ((o (js-cast-object obj "defineProperty"))
	  (name (js-toname p))
	  (desc (js-to-property-descriptor attributes name)))
      (js-define-own-property o name desc #t)
      obj))

;*---------------------------------------------------------------------*/
;*    object-defineproperties ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.7     */
;*---------------------------------------------------------------------*/
(define (object-defineproperties this obj properties)
   
   (define (vfor-each proc vec)
      (let ((len (vector-length vec)))
	 (let loop ((i 0))
	    (when (<fx i len)
	       (proc (vector-ref-ur vec i) i)
	       (loop (+fx i 1))))))

   (define (defineproperties/cmap cmap elements o props)
      (with-access::JsConstructMap cmap (names descriptors)
	 (vfor-each (lambda (name::symbol i)
		       (let ((prop (vector-ref-ur descriptors i)))
			  (with-access::JsPropertyDescriptor prop (enumerable)
			     (when (eq? enumerable #t)
				(let* ((descobj (if (isa? prop JsAccessorDescriptor)
						    (js-property-value properties prop)
						    (vector-ref-ur elements i)))
				       (desc (js-to-property-descriptor descobj name)))
				   (js-define-own-property o name desc #t))))))
	    names)))
   
   (define (defineproperties/properties oprops o props)
      (for-each (lambda (prop)
		   (with-access::JsPropertyDescriptor prop ((p name) enumerable)
		      (when (eq? enumerable #t)
			 (let* ((descobj (js-property-value properties prop))
				(desc (js-to-property-descriptor descobj p)))
			    (js-define-own-property o p desc #t)))))
	 oprops))
   
   (let* ((o (js-cast-object obj "defineProperties"))
	  (props (js-cast-object (js-toobject properties) "defineProperties")))
      (with-access::JsObject props (cmap elements (oprops properties))
	 (if cmap
	     (defineproperties/cmap cmap elements o props)
	     (defineproperties/properties oprops o props)))
      obj))

;*---------------------------------------------------------------------*/
;*    object-seal ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.8     */
;*---------------------------------------------------------------------*/
(define-generic (object-seal this obj)
   (js-seal (js-cast-object obj "seal") obj))

;*---------------------------------------------------------------------*/
;*    js-seal ::JsObject ...                                           */
;*---------------------------------------------------------------------*/
(define-method (js-seal o::JsObject obj)
   (js-object-unmap! obj)
   (with-access::JsObject o (properties)
      ;; 2
      (for-each (lambda (desc::JsPropertyDescriptor)
		   (with-access::JsPropertyDescriptor desc (name configurable)
		      (set! configurable #f)))
	 properties)
      ;; 3
      (with-access::JsObject o (extensible)
	 (set! extensible #f))
      ;; 4
      obj))

;*---------------------------------------------------------------------*/
;*    object-freeze ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.9     */
;*---------------------------------------------------------------------*/
(define (object-freeze this obj)
   (js-freeze (js-cast-object obj "freeze") obj))

;*---------------------------------------------------------------------*/
;*    js-freeze ::JsObject ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-freeze o::JsObject obj)
   (js-object-unmap! obj)
   (with-access::JsObject o (properties)
      (for-each js-freeze-property! properties)
      (with-access::JsObject o (extensible)
	 (set! extensible #f))
      obj))

;*---------------------------------------------------------------------*/
;*    object-preventextensions ...                                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.10    */
;*---------------------------------------------------------------------*/
(define (object-preventextensions this obj)
   (let ((o (js-cast-object obj "preventExtensions")))
      (with-access::JsObject o (extensible)
	 (set! extensible #f))
      obj))

;*---------------------------------------------------------------------*/
;*    object-issealed ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.11    */
;*---------------------------------------------------------------------*/
(define (object-issealed this o)
   ;; 1
   (let ((o (js-cast-object o "isSealed")))
      (with-access::JsObject o (properties cmap)
	 (and
	  (not cmap)
	  ;; 2
	  (every (lambda (desc::JsPropertyDescriptor)
		    (with-access::JsPropertyDescriptor desc (configurable)
		       (not (eq? configurable #t))))
	     properties)
	  ;; 3
	  (with-access::JsObject o (extensible)
	     (not extensible))))))
   
;*---------------------------------------------------------------------*/
;*    object-isfrozen ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.12    */
;*---------------------------------------------------------------------*/
(define (object-isfrozen this o)
   ;; 1
   (let ((o (js-cast-object o "isFrozen")))
      (with-access::JsObject o (properties cmap)
	 (and
	  (or (not cmap)
	      (with-access::JsConstructMap cmap (names)
		 (=fx (vector-length names) 0)))
	  ;; 2
	  (every (lambda (desc::JsPropertyDescriptor)
		    (with-access::JsPropertyDescriptor desc (configurable)
		       (and (not (eq? configurable #t))
			    (or (not (isa? desc JsValueDescriptor))
				(with-access::JsValueDescriptor desc (writable)
				   (not (eq? writable #t)))))))
	     properties)
	  ;; 3
	  (with-access::JsObject o (extensible)
	     (not extensible))))))

;*---------------------------------------------------------------------*/
;*    object-isextensible ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.13    */
;*---------------------------------------------------------------------*/
(define (object-isextensible this obj)
   (let ((o (js-cast-object obj "isExtensible")))
      (with-access::JsObject o (extensible)
	 extensible)))
   
;*---------------------------------------------------------------------*/
;*    object-keys ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.14    */
;*---------------------------------------------------------------------*/
(define (object-keys this obj)
   (let ((o (js-cast-object obj "keys")))
      (js-vector->jsarray (js-property-names o #t))))

;*---------------------------------------------------------------------*/
;*    js-global-object-eval ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.1     */
;*---------------------------------------------------------------------*/
(define (js-global-object-eval this string)
   (if (not (string? string))
       string
       (%js-eval string)))

;*---------------------------------------------------------------------*/
;*    js-global-object-parseInt ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.2     */
;*---------------------------------------------------------------------*/
(define (js-global-object-parseint this string radix)
   (js-parseint (trim-whitespaces+ (js-tostring string) :plus #t) radix #f))

;*---------------------------------------------------------------------*/
;*    js-global-object-parseFloat ...                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.3     */
;*---------------------------------------------------------------------*/
(define (js-global-object-parsefloat this string)
   (js-parsefloat (trim-whitespaces+ (js-tostring string) :plus #t) #f))

;*---------------------------------------------------------------------*/
;*    js-global-object-isnan ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.4     */
;*---------------------------------------------------------------------*/
(define (js-global-object-isnan this number)
   (let ((n (js-tonumber number)))
      (and (flonum? n) (not (=fl n n)))))

;*---------------------------------------------------------------------*/
;*    js-global-object-isfinite ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.5     */
;*---------------------------------------------------------------------*/
(define (js-global-object-isfinite this number)
   (let ((n (js-tonumber number)))
      (cond
	 ((not (flonum? n)) #t)
	 ((or (not (=fl n n)) (=fl n +inf.0) (=fl n -inf.0)) #f)
	 (else #t))))

;*---------------------------------------------------------------------*/
;*    js-global-object-decodeuri ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.3.1     */
;*---------------------------------------------------------------------*/
(define (js-global-object-decodeuri this string)
   
   (define (check-utf8-validity str)
      (if (utf8-string? str)
	  str
	  (js-raise
	     (js-new js-uri-error (format "Not a utf8 string ~s" string)))))

   (let ((str (js-tostring string)))
      (if (url? str)
	  (check-utf8-validity (uri-decode str))
	  (js-raise
	     (js-new js-uri-error (format "Badly formed url ~s" string))))))
   
;*---------------------------------------------------------------------*/
;*    js-global-object-decodeuricomponent ...                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.3.2     */
;*---------------------------------------------------------------------*/
(define (js-global-object-decodeuricomponent this string)
   
   (define (check-utf8-validity str)
      (if (utf8-string? str)
	  str
	  (js-raise
	     (js-new js-uri-error (format "Not a utf8 string ~s" string)))))

   (let ((str (js-tostring string)))
      (if (url? str)
	  (check-utf8-validity (uri-decode-component str))
	  (js-raise
	     (js-new js-uri-error (format "Badly formed url ~s" string))))))

;*---------------------------------------------------------------------*/
;*    js-global-object-encodeuri ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.3.3     */
;*---------------------------------------------------------------------*/
(define (js-global-object-encodeuri this string)
   (let ((str (js-tostring string)))
      (if (utf8-string? str #t)
	  (uri-encode str)
	  (js-raise
	     (js-new js-uri-error (format "Badly formed url ~s" string))))))

;*---------------------------------------------------------------------*/
;*    js-global-object-encodeuricomponent ...                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.3.4     */
;*---------------------------------------------------------------------*/
(define (js-global-object-encodeuricomponent this string)
   (let ((str (js-tostring string)))
      (if (utf8-string? str #t)
	  (uri-encode-component str)
	  (js-raise
	     (js-new js-uri-error (format "Badly formed url ~s" string))))))

;*---------------------------------------------------------------------*/
;*    js-global-object-escape ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.1        */
;*---------------------------------------------------------------------*/
(define (js-global-object-escape this string)
   (url-path-encode (js-tostring string)))
   
;*---------------------------------------------------------------------*/
;*    js-global-object-unescape ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.1        */
;*---------------------------------------------------------------------*/
(define (js-global-object-unescape this string)
   (url-decode! (js-tostring string)))

;*---------------------------------------------------------------------*/
;*    js-get-global-object-name ...                                    */
;*    -------------------------------------------------------------    */
;*    This is an inlined version of js-get-own-property.               */
;*---------------------------------------------------------------------*/
(define (js-get-global-object-name o::JsObject name throw)
   (js-getvalue o o name throw))

;*---------------------------------------------------------------------*/
;*    js-get-global-object-name/cache ...                              */
;*---------------------------------------------------------------------*/
(define-inline (js-get-global-object-name/cache o::JsObject name::symbol cache::JsPropertyCache throw)
   (with-access::JsObject o ((omap cmap) elements)
      (with-access::JsPropertyCache cache (cmap index)
	 (if (eq? cmap omap)
	     (vector-ref-ur elements index)
	     (js-get-name/cache-miss o name cache throw)))))

;*---------------------------------------------------------------------*/
;*    js-toprimitive ...                                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.8       */
;*---------------------------------------------------------------------*/
(define-method (js-toprimitive o::JsObject preferredtype)
   
   (define (err)
      (js-raise-type-error 
	 "toPrimitive: illegal default value \"~s\""
	 (class-name (object-class o))))
   
   (define (get-field-value . fields)
      (let loop ((fields fields))
	 (if (null? fields)
	     (err)
	     (let ((proc (js-get o (car fields))))
		(if (isa? proc JsFunction)
		    (let ((r (js-call0 proc o)))
		       (if (not (isa? r JsObject))
			   r
			   (loop (cdr fields))))
		    (loop (cdr fields)))))))
   
   (define (primitive-as-string)
      (get-field-value 'toString 'valueOf))
   
   (define (primitive-as-number)
      (get-field-value 'valueOf 'toString))

   (cond
      ((eq? preferredtype 'string)
       (primitive-as-string))
      ((eq? preferredtype 'number)
       (primitive-as-number))
      ((isa? o JsDate)
       (primitive-as-string))
      (else
       (primitive-as-number))))

