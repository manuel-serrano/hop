;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/function.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 22 06:56:33 2013                          */
;*    Last change :  Mon Mar 16 08:16:01 2020 (serrano)                */
;*    Copyright   :  2013-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript function implementation                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_function
   
   (library hop js2scheme)
   
   (include "types.sch" "stringliteral.sch" "property.sch")
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_property
	   __hopscript_object
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_lib
	   __hopscript_worker
	   __hopscript_array
	   __hopscript_arguments)
   
   (export (js-init-function! ::JsGlobalObject)
	   
	   thrower-get
	   thrower-set
	   
	   (js-function-debug-name::bstring ::JsFunction ::JsGlobalObject)
	   (js-make-function::JsFunction
	      ::JsGlobalObject ::procedure ::int ::JsStringLiteral
	      #!key
	      method construct alloc
	      __proto__ prototype
	      (strict 'normal) arity (minlen -1) src 
	      (size 0) (constrsize 3) (maxconstrsize 100)
	      (constrmap (js-not-a-cmap)) (shared-cmap #t))
	   (js-make-function-strict::JsFunction
	      ::JsGlobalObject ::procedure ::int ::JsStringLiteral
	      #!key
	      method alloc
	      arity (minlen -1) src 
	      (constrsize 3)
	      (constrmap (js-not-a-cmap)))
	   (js-make-function-strict-lazy::JsFunction
	      ::JsGlobalObject ::procedure ::int ::JsStringLiteral
	      #!key
	      method 
	      arity (minlen -1) src 
	      (constrsize 3))
	   (js-make-function-strict-lazy1::JsFunction
	      ::JsGlobalObject ::procedure ::int ::JsStringLiteral
	      #!key
	      method 
	      (minlen -1) src 
	      (constrsize 3))
	   (js-make-function-strict-lazy2::JsFunction
	      ::JsGlobalObject ::procedure ::int ::JsStringLiteral
	      #!key
	      method 
	      (minlen -1) src 
	      (constrsize 3))
	   (js-make-function-strict-lazy3::JsFunction
	      ::JsGlobalObject ::procedure ::int ::JsStringLiteral
	      #!key
	      method 
	      (minlen -1) src 
	      (constrsize 3))
	   (js-make-function-strict-lazy4::JsFunction
	      ::JsGlobalObject ::procedure ::int ::JsStringLiteral
	      #!key
	      method 
	      (minlen -1) src 
	      (constrsize 3))
	   (js-make-function-strict-lazy5::JsFunction
	      ::JsGlobalObject ::procedure ::int ::JsStringLiteral
	      #!key
	      method 
	      (minlen -1) src 
	      (constrsize 3))
	   (js-make-function-simple::JsFunction ::JsGlobalObject ::procedure
	      ::int ::JsStringLiteral ::int ::int ::symbol ::int)
	   
	   (inline js-function-prototype-get ::obj ::JsFunction ::obj ::JsGlobalObject)
	   (js-function-setup-prototype!::JsObject ::JsGlobalObject ::JsFunction)
	   
	   (js-apply-array ::JsGlobalObject ::obj ::obj ::obj)
	   (js-apply-vec ::JsGlobalObject ::obj ::obj ::vector ::uint32)
	   (js-function-apply-vec ::JsGlobalObject ::JsFunction ::obj ::vector ::uint32)
	   (js-function-apply ::JsGlobalObject ::obj ::obj ::obj ::obj)
	   (js-function-maybe-apply ::JsGlobalObject ::obj ::obj ::obj ::obj)
	   (js-function-maybe-call0 ::JsGlobalObject ::obj ::obj ::obj)
	   (js-function-maybe-call1 ::JsGlobalObject ::obj ::obj ::obj ::obj)
	   (js-function-maybe-call2 ::JsGlobalObject ::obj ::obj ::obj ::obj ::obj)
	   (js-function-maybe-call3 ::JsGlobalObject ::obj ::obj ::obj ::obj ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsFunction ...                               */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsFunction
   (lambda (o)
      (js-undefined))
   (lambda (o %this) o))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsFunction ...                                 */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsFunction op compile isexpr _)
   (error "js" "Cannot compile function" o))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsFunction ...                                 */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsFunction op compile isexpr ctx)
   (error "js" "Cannot compile function" o))

;*---------------------------------------------------------------------*/
;*    object-equal? ::JsFunction ...                                   */
;*---------------------------------------------------------------------*/
(define-method (object-equal? x::JsFunction y::object)
   (eq? x y))

;*---------------------------------------------------------------------*/
;*    xml-primitive-value ::JsFunction ...                             */
;*---------------------------------------------------------------------*/
(define-method (xml-primitive-value obj::JsFunction ctx)
   obj)

;*---------------------------------------------------------------------*/
;*    xml-unpack ::JsObject ...                                        */
;*    -------------------------------------------------------------    */
;*    Simply returns the function. This will give XML write            */
;*    a chance to raise an appropriate error messages.                 */
;*---------------------------------------------------------------------*/
(define-method (xml-unpack o::JsFunction ctx)
   o)

;*---------------------------------------------------------------------*/
;*    js-donate ::Jsfunction ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsFunction worker::WorkerHopThread %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-function)
	 (with-access::JsFunction obj (procedure src elements)
	    (if (eq? src 'builtin)
		(let ((nobj (duplicate::JsFunction obj
			       (elements '#()))))
		   (js-object-proto-set! nobj (js-get js-function (& "prototype") %this))
		   (js-object-mode-set! nobj (js-object-mode obj))
		   nobj)
		(js-undefined))))))

;*---------------------------------------------------------------------*/
;*    js-debug-object ::JsFunction ...                                 */
;*---------------------------------------------------------------------*/
(define-method (js-debug-object obj::JsFunction #!optional (msg ""))
   (call-next-method)
   (with-access::JsFunction obj (src)
      (fprint (current-error-port) "   src=" src)))
      
;*---------------------------------------------------------------------*/
;*    js-get-jsobject-name/cache-miss ...                              */
;*---------------------------------------------------------------------*/
(define-method (js-get-jsobject-name/cache-miss o::JsFunction p::obj
		  throw::bool %this::JsGlobalObject
		  cache::JsPropertyCache)
   (if (eq? (js-toname p %this) (& "prototype"))
       (with-access::JsFunction o (prototype) prototype)
       (call-next-method)))
   
;*---------------------------------------------------------------------*/
;*    throwers                                                         */
;*---------------------------------------------------------------------*/
(define thrower-get #f)
(define thrower-set #f)

(define strict-arguments-property #f)
(define strict-caller-property #f)

;*---------------------------------------------------------------------*/
;*    make-cmap ...                                                    */
;*---------------------------------------------------------------------*/
(define (make-cmap inline props)
   (instantiate::JsConstructMap
      (inline inline)
      (methods (make-vector (vector-length props)))
      (props props)))

;*---------------------------------------------------------------------*/
;*    current-loc ...                                                  */
;*---------------------------------------------------------------------*/
(define-expander current-loc
   (lambda (x e)
      (when (epair? x) `',(cer x))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js-literal ::JsFunction ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s-js-literal o::JsFunction ctx)
   (error "js" "Cannot compile function" o))

;*---------------------------------------------------------------------*/
;*    js-define-own-property ::JsFunction ...                          */
;*---------------------------------------------------------------------*/
(define-method (js-define-own-property o::JsFunction p desc throw %this)
   (with-access::JsGlobalObject %this (js-symbol-hasinstance)
      (when (eq? p js-symbol-hasinstance)
	 ;; we are defining the property @@hasInstance, mark that function
	 ;; for slow instanceof path
	 (js-object-mode-hasinstance-set! o #t)))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    js-init-function! ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.3       */
;*---------------------------------------------------------------------*/
(define (js-init-function! %this::JsGlobalObject)
   ;; local constant strings
   (unless (vector? __js_strings) (set! __js_strings (&init!)))
   ;; create function cmap
   (js-init-function-cmap! %this)
   ;; bind the builtin function prototype
   (with-access::JsGlobalObject %this (js-function-prototype
					 js-function-strict-prototype
					 js-function js-function-pcache)
      (set! js-function-pcache
	 ((@ js-make-pcache-table __hopscript_property) 5 "function"))
      
      (let ((proc (lambda l (js-undefined)))
	    (js-object-prototype (js-object-proto %this)))
	 (set! js-function-prototype
	    (instantiateJsFunction
	       (procedure proc)
	       (method proc)
	       (construct proc)
	       (cmap (instantiate::JsConstructMap))
	       (alloc js-not-a-constructor-alloc)
	       (src "[Function.__proto__@function.scm]")
	       (name (& ""))
	       (len -1)
	       (arity -1)
	       (prototype js-object-prototype)
	       (%prototype js-object-prototype)
	       (__proto__ js-object-prototype)
	       (elements ($create-vector 10)))))

      (set! js-function-strict-prototype
	 (instantiateJsObject
	    (cmap (instantiate::JsConstructMap (inline #t)))
	    (__proto__ js-function-prototype)
	    (elements (make-vector 10 (js-undefined)))))
      
      ;; then, create the properties of the function contructor
      (set! js-function
	 (js-make-function %this
	    (%js-function %this) 1 (& "Function")
	    :alloc js-no-alloc
	    :src (cons (current-loc) "Function() { /* function.scm */}")
	    :__proto__ js-function-prototype
	    :prototype js-function-prototype))
      ;; throwers
      (let* ((throwget (lambda (o)
			  (js-raise-type-error %this
			     "[[ThrowTypeError]] ~a" o)))
	     (throwset (lambda (o v)
			  (js-raise-type-error %this
			     "[[ThrowTypeError]] ~a" o)))
	     (thrower (js-make-function %this
			 (lambda (o v)
			    (js-raise-type-error %this
			       "[[ThrowTypeError]] ~a" o))
			 1 (& "thrower"))))
	 ;; pre-allocated prototype property descriptors
	 (js-init-function-property! %this thrower throwget throwset)
	 (set! thrower-get thrower)
	 (set! thrower-set thrower)
	 (set! strict-arguments-property
	    (instantiate::JsAccessorDescriptor
	       (name (& "arguments"))
	       (get thrower-get)
	       (set thrower-set)
	       (%get throwget)
	       (%set throwset)
	       (enumerable #f)
	       (configurable #f)))
	 (set! strict-caller-property
	    (instantiate::JsAccessorDescriptor
	       (name (& "caller"))
	       (get thrower-get)
	       (set thrower-set)
	       (%get throwget)
	       (%set throwset)
	       (enumerable #f)
	       (configurable #f))))

      ;; prototype properties
      (init-builtin-function-prototype! %this js-function js-function-prototype)
      
      ;; bind Function in the global object
      (js-bind! %this %this (& "Function")
	 :value js-function
	 :configurable #f :enumerable #f)
      ;; return the js-function object
      js-function))

;*---------------------------------------------------------------------*/
;*    js-init-function-property! ...                                   */
;*---------------------------------------------------------------------*/
(define (js-init-function-property! %this::JsGlobalObject thrower %tget %tset)
   (with-access::JsGlobalObject %this (js-function-prototype-property-rw 
					 js-function-prototype-property-ro
					 js-function-prototype-property-null
					 js-function-prototype-property-undefined
					 js-function-strict-elements)
      
      (set! js-function-prototype-property-rw
	 (instantiate::JsWrapperDescriptor
	    (name (& "prototype"))
	    (enumerable #f)
	    (configurable #f)
	    (writable #t)
	    (%get js-function-prototype-get)
	    (%set js-function-prototype-set)))
      
      (set! js-function-prototype-property-ro
	 (instantiate::JsWrapperDescriptor
	    (name (& "prototype"))
	    (enumerable #f)
	    (configurable #f)
	    (writable #f)
	    (%get js-function-prototype-get)
	    (%set js-function-prototype-set)))
      
      (set! js-function-prototype-property-null
	 (instantiate::JsValueDescriptor
	    (name (& "%null"))
	    (enumerable #f)
	    (configurable #f)
	    (writable #f)
	    (value '())))
      
      (set! js-function-prototype-property-undefined
	 (instantiate::JsValueDescriptor
	    (name (& "%undefined"))
	    (enumerable #f)
	    (configurable #f)
	    (writable #f)
	    (value (js-undefined))))
      
      (set! js-function-strict-elements
	 (vector
	    js-function-prototype-property-rw
	    (instantiate::JsWrapperDescriptor
	       (name (& "length"))
	       (enumerable #f)
	       (configurable #f)
	       (writable #f)
	       (%get (lambda (obj owner propname %this)
			(with-access::JsFunction owner (len)
			   len)))
	       (%set list))
	    (instantiate::JsWrapperDescriptor
	       (name (& "name"))
	       (enumerable #f)
	       (configurable #f)
	       (writable #f)
	       (%get (lambda (obj owner propname %this)
			(with-access::JsFunction owner (name)
			   name)))
	       (%set list))
	    (instantiate::JsAccessorDescriptor
	       (name (& "arguments"))
	       (get thrower)
	       (set thrower)
	       (%get %tget)
	       (%set %tset)
	       (enumerable #f)
	       (configurable #f))
	    (instantiate::JsAccessorDescriptor
	       (name (& "caller"))
	       (get thrower)
	       (set thrower)
	       (%get %tget)
	       (%set %tset)
	       (enumerable #f)
	       (configurable #f))))))

;*---------------------------------------------------------------------*/
;*    js-init-function-cmap! ...                                       */
;*---------------------------------------------------------------------*/
(define (js-init-function-cmap! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-function-cmap
					 js-function-sans-prototype-cmap
					 js-function-strict-cmap
					 js-function-strict-bind-cmap
					 js-function-writable-cmap
					 js-function-writable-strict-cmap
					 js-function-prototype-cmap)
      (set! js-function-cmap
	 (make-cmap #f
	    `#(,(prop (& "prototype") (property-flags #f #f #f #f))
	       ,(prop (& "length") (property-flags #f #f #f #f))
	       ,(prop (& "name") (property-flags #f #f #t #f)))))
      
      (set! js-function-sans-prototype-cmap
	 (make-cmap #f
	    `#(,(prop (& "%null") (property-flags #f #f #f #f))
	       ,(prop (& "length") (property-flags #f #f #f #f))
	       ,(prop (& "name") (property-flags #f #f #t #f)))))
      
      (set! js-function-strict-bind-cmap
	 (make-cmap #f
	    `#(,(prop (& "%bind") (property-flags #f #f #f #f))
	       ,(prop (& "length") (property-flags #f #f #f #f))
	       ,(prop (& "name") (property-flags #f #f #t #f))
	       ,(prop (& "arguments") (property-flags #f #f #f #f))
	       ,(prop (& "caller") (property-flags #f #f #f #f)))))

      (set! js-function-strict-cmap
	 (make-cmap #f
	    `#(,(prop (& "prototype") (property-flags #f #f #f #f))
	       ,(prop (& "length") (property-flags #f #f #f #f))
	       ,(prop (& "name") (property-flags #f #f #t #f))
	       ,(prop (& "arguments") (property-flags #f #f #f #f))
	       ,(prop (& "caller") (property-flags #f #f #f #f)))))
      
      (set! js-function-writable-cmap
	 (make-cmap #f
	    `#(,(prop (& "prototype") (property-flags #t #f #f #f))
	       ,(prop (& "length") (property-flags #f #f #f #f))
	       ,(prop (& "name") (property-flags #f #f #t #f)))))
      
      (set! js-function-writable-strict-cmap
	 (make-cmap #f
	    `#(,(prop (& "prototype") (property-flags #t #f #f #f))
	       ,(prop (& "length") (property-flags #f #f #f #f))
	       ,(prop (& "name") (property-flags #f #f #t #f))
	       ,(prop (& "arguments") (property-flags #f #f #f #f))
	       ,(prop (& "caller") (property-flags #f #f #f #f)))))
      
      (set! js-function-prototype-cmap
	 (make-cmap #t
	    `#(,(prop (& "constructor") (property-flags #t #f #t #f)))))))

;*---------------------------------------------------------------------*/
;*    %js-function ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.1.1     */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.2.1     */
;*    -------------------------------------------------------------    */
;*    This definition is overriden by the definition of                */
;*    nodejs_require (nodejs/require.scm).                             */
;*---------------------------------------------------------------------*/
(define (%js-function %this::JsGlobalObject)
   (lambda (this . args)
      (if (null? args)
	  (js-make-function %this (lambda (this) (js-undefined)) 0 (& "")
	     :alloc js-object-alloc)
	  (let* ((len (length args))
		 (formals (take args (-fx len 1)))
		 (body (car (last-pair args)))
		 (fun (format "(function(~(,)) { ~a })"
			 (map (lambda (o) (js-tostring o %this)) formals)
			 (js-tostring body %this))))
	     (call-with-input-string fun
		(lambda (ip)
		   (%js-eval ip 'eval %this this %this)))))))

;*---------------------------------------------------------------------*/
;*    js-function-debug-name ...                                       */
;*---------------------------------------------------------------------*/
(define (js-function-debug-name::bstring obj::JsFunction %this)
   (with-access::JsFunction obj (src)
      (with-access::JsGlobalObject %this (js-function-pcache)
	 (let ((pname (js-get-property obj (& "name") %this)))
	    (cond
	       ((isa? pname JsValueDescriptor)
		(let ((name (js-property-value obj obj (& "name") pname %this)))
		   (cond
		      ((js-jsstring? name)
		       (js-jsstring->string name))
		      ((number? name)
		       name)
		      ((pair? src)
		       (format "~a:~a" (cadr (car src)) (caddr (car src))))
		      (else "function"))))
	       ((pair? src)
		(format "~a:~a" (cadr (car src)) (caddr (car src))))
	       (else
		"function"))))))

;*---------------------------------------------------------------------*/
;*    INSTANTIATE-JSFUNCTION ...                                       */
;*---------------------------------------------------------------------*/
(define-macro (INSTANTIATE-JSFUNCTION . attrs)
   (let ((arity (assq 'arity attrs)))
      (case (cadr arity)
	 ((1)
	  `(instantiateJsFunction1 ,@attrs))
	 ((2)
	  `(instantiateJsFunction2 ,@attrs))
	 ((3)
	  `(instantiateJsFunction3 ,@attrs))
	 ((4)
	  `(instantiateJsFunction4 ,@attrs))
	 ((5)
	  `(instantiateJsFunction5 ,@attrs))
	 (else
	  `(case ,(cadr arity)
	      ,@(map (lambda (n)
			`((,n)
			  (,(string->symbol (format "instantiateJsFunction~a" n))
			   ,@attrs)))
		 (iota 5 1))
	      (else
	       (instantiateJsFunction
		  ,@attrs)))))))

;*---------------------------------------------------------------------*/
;*    js-make-function ...                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.3.1     */
;*---------------------------------------------------------------------*/
(define (js-make-function %this procedure length name
	   #!key
	   method construct alloc
	   __proto__ prototype
	   (strict 'normal) arity (minlen -1) src 
	   (size 0) (constrsize 3) (maxconstrsize 100)
	   (constrmap (js-not-a-cmap)) (shared-cmap #t))
   (with-access::JsGlobalObject %this (js-function js-object
					 js-function-cmap
					 js-function-strict-cmap
					 js-function-writable-cmap
					 js-function-writable-strict-cmap
					 js-function-strict-bind-cmap
					 js-function-sans-prototype-cmap
					 js-function-prototype-cmap
					 js-function-prototype-property-rw 
					 js-function-prototype-property-ro
					 js-function-prototype-property-null
					 js-function-prototype-property-undefined)
      (let* ((%__proto__ (js-object-proto js-function))
	     (els ($create-vector (+fx size (if (eq? strict 'normal) 3 5))))
	     (cmap (if (eq? strict 'normal)
		       (cond
			  ((js-object? prototype)
			   js-function-cmap)
			  ((or (eq? prototype '())
			       (eq? prototype (js-undefined))
			       (eq? prototype 'bind))
			   js-function-sans-prototype-cmap)
			  (else
			   js-function-writable-cmap))
		       (cond
			  ((js-object? prototype)
			   js-function-strict-cmap)
			  ((or (eq? prototype '())
			       (eq? prototype (js-undefined)))
			   js-function-sans-prototype-cmap)
			  ((eq? prototype 'bind)
			   js-function-strict-bind-cmap)
			  (else
			   js-function-writable-strict-cmap))))
	     (fun (INSTANTIATE-JSFUNCTION
		     (procedure procedure)
		     (method (or method procedure))
		     (construct (or construct procedure))
		     (alloc (or alloc js-not-a-constructor-alloc))
		     (arity (or arity (procedure-arity procedure)))
		     (len length)
		     (__proto__ (or __proto__ %__proto__))
		     (src src)
		     (name name)
		     (constrsize constrsize)
		     (constrmap constrmap)
		     (maxconstrsize maxconstrsize)
		     (elements els)
		     (cmap (if shared-cmap
			       ;; normal functions, i.e., user functions,
			       ;; use shared-cmap
			       cmap
			       ;; non shared-cmap are used by builtin
			       ;; objects, such as Date or Number to create
			       ;; a single cmap for all their fields
			       (duplicate::JsConstructMap cmap
				  (%id (gencmapid)))))
		     (prototype #f)
		     (%prototype #f))))
	 ;; the prototype property
	 ;; the builtin "%prototype" property
	 (with-access::JsFunction fun (%prototype (fprototype prototype) alloc)
	    (vector-set! els 0
	       (cond
		  ((not prototype)
		   ;; MS 2019-01-19
		   ;; all default prototypes share the same cmap
		   ;; because on method conflict a new cmap with
		   ;; a fake transition will be created
		   ;; see extend-mapped-object!@property.scm
		   (let ((p (instantiateJsObject
			       (cmap js-function-prototype-cmap)
			       (__proto__ (js-object-proto %this))
			       (elements (vector fun)))))
		      (set! fprototype p)
		      (set! %prototype p)
		      js-function-prototype-property-rw))
		  ((js-object? prototype)
		   (js-bind! %this prototype (& "constructor")
		      :value fun
		      :configurable #t :enumerable #f :writable #t
		      :hidden-class #t)
		   (set! fprototype prototype)
		   (set! %prototype prototype)
		   js-function-prototype-property-ro)
		  ((eq? prototype (js-undefined))
		   (set! fprototype prototype)
		   (unless (eq? alloc js-not-a-constructor-alloc)
		      (set! alloc js-object-alloc-lazy))
		   js-function-prototype-property-undefined)
		  ((null? prototype)
		   (set! fprototype prototype)
		   (set! alloc js-object-alloc-lazy)
		   js-function-prototype-property-null)
		  ((eq? prototype 'bind)
		   (set! fprototype (js-undefined))
		   js-function-prototype-property-undefined)
		  (else
		   (error "js-make-function" "Illegal :prototype"
		      prototype)))))
	 ;; length
	 (vector-set! els 1 length)
	 ;; name
	 (vector-set! els 2 name)
	 ;; strict properties
	 (unless (eq? strict 'normal)
	    (vector-set! els 3 strict-arguments-property)
	    (vector-set! els 4 strict-caller-property))
	 fun)))

;*---------------------------------------------------------------------*/
;*    js-make-function-strict ...                                      */
;*    -------------------------------------------------------------    */
;*    specialized function constructor for regular strict functions.   */
;*---------------------------------------------------------------------*/
(define (js-make-function-strict %this procedure length name
	   #!key
	   method alloc
	   arity (minlen -1) src 
	   (constrsize 3)
	   (constrmap (js-not-a-cmap)))
   (with-access::JsGlobalObject %this (js-function 
					 js-function-writable-strict-cmap
					 js-function-prototype-property-rw
					 js-function-strict-elements)
      (let ((fun (INSTANTIATE-JSFUNCTION
		    (procedure procedure)
		    (method method)
		    (construct procedure)
		    (alloc alloc)
		    (arity arity)
		    (len length)
		    (__proto__ (js-object-proto js-function))
		    (src src)
		    (name name)
		    (constrsize constrsize)
		    (constrmap constrmap)
		    (maxconstrsize 100)
		    (elements js-function-strict-elements)
		    (cmap js-function-writable-strict-cmap)
		    (prototype #f)
		    (%prototype #f))))
	 (unless (eq? alloc js-object-alloc-lazy)
	    (js-function-setup-prototype! %this fun))
	 fun)))

;*---------------------------------------------------------------------*/
;*    $js-make-function-strict-lazy ...                                */
;*    -------------------------------------------------------------    */
;*    specialized function constructor for regular strict functions.   */
;*---------------------------------------------------------------------*/
(define-macro (%js-make-function-strict-lazy arity)
   `(with-access::JsGlobalObject %this (js-function 
					  js-function-writable-strict-cmap
					  js-function-prototype-property-rw
					  js-function-strict-elements)
       (INSTANTIATE-JSFUNCTION
	  (procedure procedure)
	  (method method)
	  (construct procedure)
	  (alloc js-object-alloc-lazy)
	  (arity ,arity)
	  (len length)
	  (__proto__ (js-object-proto js-function))
	  (src src)
	  (name name)
	  (constrsize constrsize)
	  (constrmap (js-not-a-cmap))
	  (maxconstrsize 100)
	  (elements js-function-strict-elements)
	  (cmap js-function-writable-strict-cmap)
	  (prototype #f)
	  (%prototype #f))))

(define (js-make-function-strict-lazy %this procedure length name
	   #!key method arity (minlen -1) src (constrsize 3))
   (%js-make-function-strict-lazy arity))

(define (js-make-function-strict-lazy1 %this procedure length name
	   #!key method (minlen -1) src (constrsize 3))
   (%js-make-function-strict-lazy 1))

(define (js-make-function-strict-lazy2 %this procedure length name
	   #!key method (minlen -1) src (constrsize 3))
   (%js-make-function-strict-lazy 2))

(define (js-make-function-strict-lazy3 %this procedure length name
	   #!key method (minlen -1) src (constrsize 3))
   (%js-make-function-strict-lazy 3))

(define (js-make-function-strict-lazy4 %this procedure length name
	   #!key method (minlen -1) src (constrsize 3))
   (%js-make-function-strict-lazy 4))

(define (js-make-function-strict-lazy5 %this procedure length name
	   #!key method (minlen -1) src (constrsize 3))
   (%js-make-function-strict-lazy 5))

;*---------------------------------------------------------------------*/
;*    js-function-setup-prototype! ...                                 */
;*---------------------------------------------------------------------*/
(define (js-function-setup-prototype! %this fun::JsFunction)
   (with-access::JsGlobalObject %this (js-function-prototype-cmap)
      (with-access::JsFunction fun (prototype %prototype)
	 (let ((p (instantiateJsObject
		     (cmap js-function-prototype-cmap)
		     (__proto__ (js-object-proto %this))
		     (elements (vector fun)))))
	    (set! prototype p)
	    (set! %prototype p)
	    p))))

;*---------------------------------------------------------------------*/
;*    js-function-prototype-get ...                                    */
;*---------------------------------------------------------------------*/
(define-inline (js-function-prototype-get obj owner::JsFunction propname %this)
   (with-access::JsFunction owner (prototype)
      prototype))

;*---------------------------------------------------------------------*/
;*    js-function-prototype-set ...                                    */
;*---------------------------------------------------------------------*/
(define (js-function-prototype-set obj v owner::JsFunction propname %this)
   
   (define (find-desc vec prop)
      (let ((len (vector-length vec)))
	 (let loop ((i 0))
	    (when (<fx i len)
	       (with-access::JsPropertyDescriptor (vector-ref vec i) (name)
		  (if (eq? name prop)
		      (vector-ref vec i)
		      (loop (+fx i 1))))))))
   
   (with-access::JsFunction owner (constrmap %prototype prototype elements cmap)
      ;; as the prototype property is not configurable,
      ;; it is always owned by the object
      (let ((desc (if (eq? cmap (js-not-a-cmap))
		      (find-desc elements (& "prototype"))
		      (vector-ref elements 0))))
	 (with-access::JsDataDescriptor desc (writable)
	    (when writable
	       ;; changing the prototype invalidates the fun's constrmap
	       ;; (MS, change 2019-01-18)
	       (unless (eq? constrmap (js-not-a-cmap))
		  (js-function-set-constrmap! owner))
	       (set! prototype v)
	       (set! %prototype (if (js-object? v) v (js-object-proto %this)))))))
   v)

;*---------------------------------------------------------------------*/
;*    js-make-function-simple ...                                      */
;*---------------------------------------------------------------------*/
(define (js-make-function-simple %this::JsGlobalObject proc::procedure
	   len::int name::JsStringLiteral arity::int minlen::int strict::symbol
	   constrsize::int)
   (js-make-function %this proc len name
      :prototype #f :__proto__ #f
      :arity arity :strict strict :minlen minlen
      :src #f
      :alloc js-object-alloc
      :construct proc :constrsize constrsize))

;*---------------------------------------------------------------------*/
;*    init-builtin-function-prototype! ...                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4       */
;*---------------------------------------------------------------------*/
(define (init-builtin-function-prototype! %this::JsGlobalObject js-function obj)
   
   ;; length
   (js-bind! %this obj (& "length")
      :value 0
      :enumerable #f :configurable #f :writable #f
      :hidden-class #t)
   
   ;; constructor
   (js-bind! %this obj (& "constructor")
      :value js-function
      :enumerable #f :configurable #t :writable #t
      :hidden-class #t)

   ;; name
   (js-bind! %this obj (& "name")
      :value (js-ascii->jsstring "builtin")
      :enumerable #f :configurable #t :writable #f
      :hidden-class #t)
   
   ;; toString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.2
   (define (tostring this)
      (cond
	 ((js-function? this)
	  (with-access::JsFunction this (src)
	     (cond
		((pair? src)
		 (if (string? (cdr src))
		     (js-string->jsstring (cdr src))
		     (js-string->jsstring
			(format "[Function ~a:~a]"
			   (cadr (car src)) (caddr (car src))))))
		((string? src)
		 (js-string->jsstring src))
		(else
		 (let ((name (js-get this (& "name") %this)))
		    (if (js-jsstring? name)
			(js-jsstring-append
			   (js-ascii->jsstring "[function ")
			   (js-jsstring-append
			      name
			      (js-ascii->jsstring "]")))
			(js-ascii->jsstring "[Function]")))))))
	 ((js-proxy-function? this)
	  (tostring (js-proxy-target this)))
	 (else
	  (js-raise-type-error %this "toString: not a function ~s"
	     (js-typeof this %this)))))

   (js-bind! %this obj (& "toString")
      :value (js-make-function %this tostring 0 (& "toString")
		:prototype (js-undefined))
      :enumerable #f :writable #t :configurable #t
      :hidden-class #t)

   ;; source
   ;; Hop extension
   (define (source this)
      (if (js-function? this)
	  (with-access::JsFunction this (src)
	     (when (pair? src)
		(js-string->jsstring
		   (format "~a:~a" (cadr (car src)) (caddr (car src))))))
	  (js-raise-type-error %this "source: not a function ~s"
	     (js-typeof this %this))))
   
   (js-bind! %this obj (& "source")
      :value (js-make-function %this source 0 (& "source")
		:prototype (js-undefined))
      :enumerable #f :writable #t :configurable #t
      :hidden-class #t)

   ;; @@hasInstance
   (with-access::JsGlobalObject %this (js-symbol-hasinstance)
      (js-bind! %this obj js-symbol-hasinstance
	 :value (js-make-function %this
		   (lambda (this o) (js-ordinary-instanceof? %this o this))
		   1 (& "[Symbol.hasInstance]") :prototype (js-undefined))
	 :enumerable #f :writable #f :configurable #f
	 :hidden-class #t))
   
   ;; apply
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.3
   (define (prototype-apply this::obj thisarg argarray)
      (js-apply-array %this this thisarg argarray))

   (js-bind! %this obj (& "apply")
      :value (js-make-function %this prototype-apply 2 (& "apply")
		:prototype (js-undefined))
      :enumerable #f :writable #t :configurable #t
      :hidden-class #t)
   
   ;; call
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.4
   (define (call this::obj thisarg . args)
      (cond
	 ((null? args)
	  (js-call0 %this this thisarg))
	 ((null? (cdr args))
	  (js-call1 %this this thisarg (car args)))
	 ((null? (cddr args))
	  (js-call2 %this this thisarg (car args) (cadr args)))
	 ((null? (cdddr args))
	  (js-call3 %this this thisarg (car args) (cadr args) (caddr args)))
	 (else
	  (js-apply %this this thisarg args))))
   
   (with-access::JsGlobalObject %this (js-call)
      (set! js-call
	 (js-make-function %this call 1 (& "call") :prototype (js-undefined)))
      (js-bind! %this obj (& "call")
	 :value js-call
	 :enumerable #f :writable #t :configurable #t
	 :hidden-class #t))
   
   ;; bind
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.5
   (define (bind this::obj thisarg . args)
      (if (not (js-function? this))
	  (js-raise-type-error %this "bind: this not a function ~s" this)
	  (with-access::JsFunction this (len construct alloc procedure prototype)
	     (when (eq? prototype 'lazy)
		;; force creating the true prototype before binding
		(js-function-setup-prototype! %this this)
		(set! alloc js-object-alloc))
	     (let* ((bproc (lambda (_ . actuals)
			      (js-apply %this this
				 thisarg (append args actuals))))
		    (bctor (lambda (self . actuals)
			      (js-apply %this this
				 self (append args actuals))))
		    (bproto (js-getprototypeof this %this "getPrototypeOf"))
		    (balloc (lambda (%this ctor)
			       (alloc %this this))))
		(js-make-function
		   %this
		   bproc
		   (maxfx 0 (-fx len (length args)))
		   (js-name->jsstring 
		      (string-append "bound "
			 (js-tostring (js-get this (& "name") %this)
			    %this)))
		   :__proto__ bproto
		   :prototype 'bind
		   :strict 'strict
		   :alloc balloc
		   :construct bctor)))))

   (js-bind! %this obj (& "bind")
      :value (js-make-function %this bind 1 (& "bind")
		:prototype (js-undefined))
      :enumerable #f :writable #t :configurable #t
      :hidden-class #t)
   
   ;; hopscript does not support caller
   (js-bind! %this obj (& "caller")
      :get thrower-get
      :set thrower-set
      :enumerable #f :configurable #f
      :hidden-class #t))

;*---------------------------------------------------------------------*/
;*    vector->sublist ...                                              */
;*---------------------------------------------------------------------*/
(define (vector->sublist vec len)
   (if (=fx (vector-length vec) len)
       (vector->list vec)
       (let loop ((i (-fx len 1))
		  (acc '()))
	  (if (=fx i -1)
	      acc
	      (loop (-fx i 1) (cons (vector-ref vec i) acc))))))

;*---------------------------------------------------------------------*/
;*    js-apply-array ...                                               */
;*---------------------------------------------------------------------*/
(define (js-apply-array %this::JsGlobalObject this thisarg argarray)
   (cond
      ((js-array? argarray)
       (with-access::JsArray argarray (vec ilen)
	  (cond
	     ((js-object-mode-inline? argarray)
	      ;; fast path
	      (js-apply-vec %this this thisarg vec ilen))
	     ((js-object-mode-holey? argarray)
	      ;; fast path
	      (let ((len (js-get argarray (& "length") %this)))
		 (js-apply %this this thisarg
		    (map (lambda (p)
			    (if (eq? p (js-absent)) (js-undefined) p))
		       (vector->sublist vec len)))))
	     (else
	      ;; slow path
	      (let ((len (js-get argarray (& "length") %this)))
		 (js-apply %this this thisarg
		    (map! (lambda (idx)
			     (js-array-fixnum-ref argarray idx %this))
		       (iota len))))))))
      ((vector? argarray)
       (js-apply-vec %this this thisarg
	  argarray (fixnum->uint32 (vector-length argarray))))
      ((and (isa? argarray JsArguments) (js-function? this))
       (let ((len (js-arguments-length argarray %this)))
	  (with-access::JsFunction this (arity procedure)
	     (cond
		((=fx arity (+fx 1 len))
		 (case arity
		    ((1)
		     (procedure thisarg))
		    ((2)
		     (procedure thisarg
			(js-arguments-ref argarray 0 %this)))
		    ((3)
		     (procedure thisarg
			(js-arguments-ref argarray 0 %this)
			(js-arguments-ref argarray 1 %this)))
		    ((4)
		     (procedure thisarg
			(js-arguments-ref argarray 0 %this)
			(js-arguments-ref argarray 1 %this)
			(js-arguments-ref argarray 2 %this)))
		    ((5)
		     (procedure thisarg
			(js-arguments-ref argarray 0 %this)
			(js-arguments-ref argarray 1 %this)
			(js-arguments-ref argarray 2 %this)
			(js-arguments-ref argarray 3 %this)))
		    ((6)
		     (procedure thisarg
			(js-arguments-ref argarray 0 %this)
			(js-arguments-ref argarray 1 %this)
			(js-arguments-ref argarray 2 %this)
			(js-arguments-ref argarray 3 %this)
			(js-arguments-ref argarray 4 %this)))
		    ((7)
		     (procedure thisarg
			(js-arguments-ref argarray 0 %this)
			(js-arguments-ref argarray 1 %this)
			(js-arguments-ref argarray 2 %this)
			(js-arguments-ref argarray 3 %this)
			(js-arguments-ref argarray 4 %this)
			(js-arguments-ref argarray 5 %this)))
		    ((8)
		     (procedure thisarg
			(js-arguments-ref argarray 0 %this)
			(js-arguments-ref argarray 1 %this)
			(js-arguments-ref argarray 2 %this)
			(js-arguments-ref argarray 3 %this)
			(js-arguments-ref argarray 4 %this)
			(js-arguments-ref argarray 5 %this)
			(js-arguments-ref argarray 6 %this)))
		    (else
		     (js-apply %this this thisarg
			(map! (lambda (idx)
				 (js-arguments-ref argarray idx %this))
			   (iota len))))))
		((=fx arity -2048)
		 (procedure thisarg (js-arguments->vector argarray %this)))
		((=fx arity -2047)
		 (procedure thisarg (js-arguments->vector argarray %this)))
		(else
		 (js-apply %this this thisarg
		    (map! (lambda (idx)
			     (js-arguments-ref argarray idx %this))
		       (iota len))))))))
      ((or (eq? argarray (js-null)) (eq? argarray (js-undefined)))
       (js-call0 %this this thisarg))
      ((not (js-object? argarray))
       (js-raise-type-error %this
	  "apply: argument not an object ~s" argarray))
      (else
       ;; slow path
       (let ((len (uint32->fixnum
		     (js-touint32
			(js-get argarray (& "length") %this)
			%this))))
	  ;; assumes here a fixnum length as an iteration over the range
	  ;; 1..2^32-1 is not computable with 2014 computer's performance
	  (js-apply %this this thisarg
	     (map! (lambda (idx)
		      (js-get argarray idx %this))
		(iota len)))))))

;*---------------------------------------------------------------------*/
;*    js-apply-vec ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-apply-vec %this this thisarg vec::vector ilen::uint32)
   (cond
      ((js-function? this)
       (js-function-apply-vec %this this thisarg vec ilen))
      ((js-function-proxy? this)
       (js-apply %this this thisarg
	  (vector->sublist vec (uint32->fixnum ilen))))
      (else
       (js-raise-type-error %this
	  "apply: argument not a function ~s" this))))

;*---------------------------------------------------------------------*/
;*    js-function-apply-vec ...                                        */
;*---------------------------------------------------------------------*/
(define (js-function-apply-vec %this this thisarg vec::vector ilen::uint32)
   (with-access::JsFunction this (arity procedure)
      (let ((n (uint32->fixnum ilen)))
	 (cond
	    ((=fx arity (+fx 1 n))
	     (case arity
		((1)
		 (procedure thisarg))
		((2)
		 (procedure thisarg (vector-ref vec 0)))
		((3)
		 (procedure thisarg (vector-ref vec 0)
		    (vector-ref vec 1)))
		((4)
		 (procedure thisarg (vector-ref vec 0)
		    (vector-ref vec 1)
		    (vector-ref vec 2)))
		((5)
		 (procedure thisarg (vector-ref vec 0)
		    (vector-ref vec 1)
		    (vector-ref vec 2)
		    (vector-ref vec 3)))
		((6)
		 (procedure thisarg (vector-ref vec 0)
		    (vector-ref vec 1)
		    (vector-ref vec 2)
		    (vector-ref vec 3)
		    (vector-ref vec 4)))
		((7)
		 (procedure thisarg (vector-ref vec 0)
		    (vector-ref vec 1)
		    (vector-ref vec 2)
		    (vector-ref vec 3)
		    (vector-ref vec 4)
		    (vector-ref vec 5)))
		((8)
		 (procedure thisarg (vector-ref vec 0)
		    (vector-ref vec 1)
		    (vector-ref vec 2)
		    (vector-ref vec 3)
		    (vector-ref vec 4)
		    (vector-ref vec 5)
		    (vector-ref vec 6)))
		(else
		 (js-apply %this this thisarg (vector->sublist vec n)))))
	    ((=fx arity -2048)
	     (procedure thisarg vec))
	    ((=fx arity -2047)
	     (procedure thisarg vec))
	    (else
	     (js-apply %this this thisarg (vector->sublist vec n)))))))

;*---------------------------------------------------------------------*/
;*    js-function-apply ...                                            */
;*---------------------------------------------------------------------*/
(define (js-function-apply %this this thisarg argarray cache)
   (if (js-object-mode-plain? this)
       (js-apply-array %this this thisarg argarray)
       (with-access::JsGlobalObject %this (js-function-pcache)
	  (js-call2 %this
	     (js-get-name/cache this (& "apply") #f %this
		(or cache (js-pcache-ref js-function-pcache 2)))
	     this thisarg argarray))))

;*---------------------------------------------------------------------*/
;*    js-function-maybe-apply ...                                      */
;*---------------------------------------------------------------------*/
(define (js-function-maybe-apply %this this thisarg argarray cache)
   (let loop ((this this))
      (cond
	 ((js-function? this)
	  (js-function-apply %this this thisarg argarray cache))
	 ((js-object? this)
	  (with-access::JsGlobalObject %this (js-function-pcache)
	     (js-call2 %this
		(js-get-jsobject-name/cache this (& "apply") #f %this
		   (or cache (js-pcache-ref js-function-pcache 3)))
		this thisarg argarray)))
	 (else
	  (loop (js-toobject %this this))))))

;*---------------------------------------------------------------------*/
;*    js-function-call0 ...                                            */
;*---------------------------------------------------------------------*/
(define (js-function-call0 %this this thisarg cache)
   (if (js-object-mode-plain? this)
       (js-call0 %this this thisarg)
       (with-access::JsGlobalObject %this (js-function-pcache)
	  (js-call1 %this
	     (js-get-jsobject-name/cache this (& "call") #f %this
		(or cache (js-pcache-ref js-function-pcache 4)))
	     this thisarg))))

;*---------------------------------------------------------------------*/
;*    js-function-call1 ...                                            */
;*---------------------------------------------------------------------*/
(define (js-function-call1 %this this thisarg arg cache)
   (if (js-object-mode-plain? this)
       (js-call1 %this this thisarg arg)
       (with-access::JsGlobalObject %this (js-function-pcache)
	  (js-call2 %this
	     (js-get-jsobject-name/cache this (& "call") #f %this
		(or cache (js-pcache-ref js-function-pcache 4)))
	     this thisarg arg))))

;*---------------------------------------------------------------------*/
;*    js-function-call2 ...                                            */
;*---------------------------------------------------------------------*/
(define (js-function-call2 %this this thisarg arg0 arg1 cache)
   (if (js-object-mode-plain? this)
       (js-call2 %this this thisarg arg0 arg1)
       (with-access::JsGlobalObject %this (js-function-pcache)
	  (js-call3 %this
	     (js-get-jsobject-name/cache this (& "call") #f %this
		(or cache (js-pcache-ref js-function-pcache 4)))
	     this thisarg arg0 arg1))))

;*---------------------------------------------------------------------*/
;*    js-function-call3 ...                                            */
;*---------------------------------------------------------------------*/
(define (js-function-call3 %this this thisarg arg0 arg1 arg2 cache)
   (if (js-object-mode-plain? this)
       (js-call3 %this this thisarg arg0 arg1 arg2)
       (with-access::JsGlobalObject %this (js-function-pcache)
	  (js-call4 %this
	     (js-get-jsobject-name/cache this (& "call") #f %this
		(or cache (js-pcache-ref js-function-pcache 4)))
	     this thisarg arg0 arg1 arg2))))

;*---------------------------------------------------------------------*/
;*    js-function-maybe-call0 ...                                      */
;*---------------------------------------------------------------------*/
(define (js-function-maybe-call0 %this this thisarg cache)
   (let loop ((this this))
      (cond
	 ((js-function? this)
	  (js-function-call0 %this this thisarg cache))
	 ((js-object? this)
	  (with-access::JsGlobalObject %this (js-function-pcache)
	     (js-call1 %this
		(js-get-jsobject-name/cache this (& "call") #f %this
		   (or cache (js-pcache-ref js-function-pcache 5)))
		this thisarg)))
	 (else
	  (loop (js-toobject %this this))))))

;*---------------------------------------------------------------------*/
;*    js-function-maybe-call1 ...                                      */
;*---------------------------------------------------------------------*/
(define (js-function-maybe-call1 %this this thisarg arg cache)
   (let loop ((this this))
      (cond
	 ((js-function? this)
	  (js-function-call1 %this this thisarg arg cache))
	 ((js-object? this)
	  (with-access::JsGlobalObject %this (js-function-pcache)
	     (js-call2 %this
		(js-get-jsobject-name/cache this (& "call") #f %this
		   (or cache (js-pcache-ref js-function-pcache 5)))
		this thisarg arg)))
	 (else
	  (loop (js-toobject %this this))))))

;*---------------------------------------------------------------------*/
;*    js-function-maybe-call2 ...                                      */
;*---------------------------------------------------------------------*/
(define (js-function-maybe-call2 %this this thisarg arg0 arg1 cache)
   (let loop ((this this))
      (cond
	 ((js-function? this)
	  (js-function-call2 %this this thisarg arg0 arg1 cache))
	 ((js-object? this)
	  (with-access::JsGlobalObject %this (js-function-pcache)
	     (js-call3 %this
		(js-get-jsobject-name/cache this (& "call") #f %this
		   (or cache (js-pcache-ref js-function-pcache 5)))
		this thisarg arg0 arg1)))
	 (else
	  (loop (js-toobject %this this))))))

;*---------------------------------------------------------------------*/
;*    js-function-maybe-call3 ...                                      */
;*---------------------------------------------------------------------*/
(define (js-function-maybe-call3 %this this thisarg arg0 arg1 arg2 cache)
   (let loop ((this this))
      (cond
	 ((js-function? this)
	  (js-function-call3 %this this thisarg arg0 arg1 arg2 cache))
	 ((js-object? this)
	  (with-access::JsGlobalObject %this (js-function-pcache)
	     (js-call4 %this
		(js-get-jsobject-name/cache this (& "call") #f %this
		   (or cache (js-pcache-ref js-function-pcache 5)))
		this thisarg arg0 arg1 arg2)))
	 (else
	  (loop (js-toobject %this this))))))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
