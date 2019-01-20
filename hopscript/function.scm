;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/function.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 22 06:56:33 2013                          */
;*    Last change :  Sun Jan 20 10:08:30 2019 (serrano)                */
;*    Copyright   :  2013-19 Manuel Serrano                            */
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
	   __hopscript_worker)
   
   (export (js-init-function! ::JsGlobalObject)

	   thrower-get
	   thrower-set
	   
	   (js-function-debug-name::bstring ::JsFunction ::JsGlobalObject)
	   (js-make-function::JsFunction
	      ::JsGlobalObject ::procedure ::int ::bstring
	      #!key
	      method construct alloc
	      __proto__ prototype
	      (strict 'normal) arity (minlen -1) src rest
	      (constrsize 3) (maxconstrsize 100)
	      (constrmap (js-not-a-cmap)) (shared-cmap #t))
	   (js-make-function-simple::JsFunction ::JsGlobalObject ::procedure
	      ::int ::bstring ::int ::int ::symbol ::bool ::int)

	   (inline js-function-prototype-get ::JsFunction ::JsGlobalObject)

	   (js-apply-array ::JsGlobalObject ::obj ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsFunction ...                               */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsFunction
   (lambda (o)
      (js-undefined))
   (lambda (o %this) o))

;*---------------------------------------------------------------------*/
;*    object-equal? ::JsFunction ...                                   */
;*---------------------------------------------------------------------*/
(define-method (object-equal? x::JsFunction y::object)
   (eq? x y))

;*---------------------------------------------------------------------*/
;*    xml-primitive-value ::JsFunction ...                             */
;*---------------------------------------------------------------------*/
(define-method (xml-primitive-value obj::JsFunction)
   obj)

;*---------------------------------------------------------------------*/
;*    js-donate ::Jsfunction ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsFunction worker::WorkerHopThread %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-function)
	 (with-access::JsFunction obj (procedure src)
	    (if (eq? src 'builtin)
		(let ((nobj (duplicate::JsFunction obj
			       (__proto__ (js-get js-function 'prototype %this)))))
		   (js-object-properties-set! nobj '())
		   (js-object-mode-set! nobj (js-object-mode obj))
		   nobj)
		(js-undefined))))))
   
;*---------------------------------------------------------------------*/
;*    property caches ...                                              */
;*---------------------------------------------------------------------*/
(%define-pcache 11)
(define %pcache (js-make-pcache-table 11 "hopscript/array.scm"))

;*---------------------------------------------------------------------*/
;*    js-object-get-name/cache-miss ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-object-get-name/cache-miss o::JsFunction p::obj
		  throw::bool %this::JsGlobalObject
		  cache::JsPropertyCache
		  #!optional (point -1) (cspecs '()))
   (if (eq? p 'prototype)
       (with-access::JsFunction o (%prototype) %prototype)
       (call-next-method)))
   
;*---------------------------------------------------------------------*/
;*    xml-unpack ::JsObject ...                                        */
;*    -------------------------------------------------------------    */
;*    Simply returns the function. This will give XML write            */
;*    a chance to raise an appropriate error messages.                 */
;*---------------------------------------------------------------------*/
(define-method (xml-unpack o::JsFunction)
   o)

;*---------------------------------------------------------------------*/
;*    throwers                                                         */
;*---------------------------------------------------------------------*/
(define thrower-get #f)
(define thrower-set #f)

(define strict-arguments-property #f)
(define strict-caller-property #f)

;*---------------------------------------------------------------------*/
;*    pre-allocated prototype property descriptors                     */
;*---------------------------------------------------------------------*/
(define prototype-property-rw
   (instantiate::JsWrapperDescriptor
      (name 'prototype)
      (enumerable #f)
      (configurable #f)
      (writable #t)
      (%get js-function-prototype-get)
      (%set js-function-prototype-set)))

(define prototype-property-ro
   (instantiate::JsWrapperDescriptor
      (name 'prototype)
      (enumerable #f)
      (configurable #f)
      (writable #f)
      (%get js-function-prototype-get)
      (%set js-function-prototype-set)))

(define prototype-property-null
   (instantiate::JsValueDescriptor
      (name '%null)
      (enumerable #f)
      (configurable #f)
      (writable #f)
      (value '())))

(define prototype-property-undefined
   (instantiate::JsValueDescriptor
      (name '%undefined)
      (enumerable #f)
      (configurable #f)
      (writable #f)
      (value (js-undefined))))

;*---------------------------------------------------------------------*/
;*    make-cmap ...                                                    */
;*---------------------------------------------------------------------*/
(define (make-cmap props)
   (instantiate::JsConstructMap
      (methods (make-vector (vector-length props)))
      (props props)))

;*---------------------------------------------------------------------*/
;*    js-function-cmap ...                                             */
;*---------------------------------------------------------------------*/
(define js-function-cmap
   (make-cmap
      `#(,(prop 'prototype (property-flags #f #f #f #f))
	 ,(prop 'length (property-flags #f #f #f #f))
	 ,(prop 'name (property-flags #f #f #t #f)))))

(define js-function-cmap-sans-prototype
   (make-cmap
      `#(,(prop '%null (property-flags #f #f #f #f))
	 ,(prop 'length (property-flags #f #f #f #f))
	 ,(prop 'name (property-flags #f #f #t #f)))))

(define js-function-strict-cmap
   (make-cmap
      `#(,(prop 'prototype (property-flags #f #f #f #f))
	 ,(prop 'length (property-flags #f #f #f #f))
	 ,(prop 'name (property-flags #f #f #t #f))
	 ,(prop 'arguments (property-flags #f #f #f #f))
	 ,(prop 'caller (property-flags #f #f #f #f)))))

(define js-function-writable-cmap
   (make-cmap
      `#(,(prop 'prototype (property-flags #t #f #f #f))
	 ,(prop 'length (property-flags #f #f #f #f))
	 ,(prop 'name (property-flags #f #f #t #f)))))

(define js-function-writable-strict-cmap
   (make-cmap
      `#(,(prop 'prototype (property-flags #t #f #f #f))
	 ,(prop 'length (property-flags #f #f #f #f))
	 ,(prop 'name (property-flags #f #f #t #f))
	 ,(prop 'arguments (property-flags #f #f #f #f))
	 ,(prop 'caller (property-flags #f #f #f #f)))))

(define js-function-prototype-cmap
   (make-cmap
      `#(,(prop 'constructor (property-flags #t #f #t #f)))))

;*---------------------------------------------------------------------*/
;*    current-loc ...                                                  */
;*---------------------------------------------------------------------*/
(define-expander current-loc
   (lambda (x e)
      (when (epair? x) `',(cer x))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js-literal ::JsFunction ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s-js-literal o::JsFunction)
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
   ;; first, bind the builtin function prototype
   (with-access::JsGlobalObject %this ((js-object-prototype __proto__)
				       js-function-prototype
				       js-function-strict-prototype
				       js-function)
      (let ((proc (lambda l (js-undefined))))
	 (set! js-function-prototype
	    (instantiateJsFunction
	       (procedure proc)
	       (method proc)
	       (construct proc)
	       (cmap (instantiate::JsConstructMap))
	       (alloc js-not-a-constructor-alloc)
	       (src "[Function.__proto__@function.scm]")
	       (len -1)
	       (arity -1)
	       (%prototype js-object-prototype)
	       (__proto__ js-object-prototype))))

      (set! js-function-strict-prototype
	 (instantiateJsObject
	    (cmap (instantiate::JsConstructMap))
	    (__proto__ js-function-prototype)))
      
      ;; then, create the properties of the function contructor
      (set! js-function
	 (js-make-function %this
	    (%js-function %this) 1 "Function"
	    :alloc js-no-alloc
	    :src (cons (current-loc) "Function() { /* function.scm */}")
	    :__proto__ js-function-prototype
	    :prototype js-function-prototype))
      ;; throwers
      (let* ((throw (lambda (o)
			(js-raise-type-error %this "[[ThrowTypeError]] ~a" o)))
	     (thrower (js-make-function %this throw 1 "thrower")))
	 (set! thrower-get thrower)
	 (set! thrower-set thrower)
	 (set! strict-arguments-property
	    (instantiate::JsAccessorDescriptor
	       (name 'arguments)
	       (get thrower-get)
	       (set thrower-set)
	       (%get throw)
	       (%set throw)
	       (enumerable #f)
	       (configurable #f)))
	 (set! strict-caller-property
	    (instantiate::JsAccessorDescriptor
	       (name 'caller)
	       (get thrower-get)
	       (set thrower-set)
	       (%get throw)
	       (%set throw)
	       (enumerable #f)
	       (configurable #f))))
      
      ;; prototype properties
      (init-builtin-function-prototype! %this js-function js-function-prototype)
      
      ;; bind Function in the global object
      (js-bind! %this %this 'Function
	 :value js-function
	 :configurable #f :enumerable #f)
      ;; return the js-function object
      js-function))

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
	  (js-make-function %this (lambda (this) (js-undefined)) 0 ""
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
      (let ((name (js-object-get-name/cache obj 'name #f %this
		     (js-pcache-ref %pcache 0) -1 '())))
	 (cond
	    ((js-jsstring? name) (js-jsstring->string name))
	    ((pair? src) (format "~a:~a" (cadr (car src)) (caddr (car src))))
	    (else "function")))))

;*---------------------------------------------------------------------*/
;*    INSTANTIATE-JSFUNCTION ...                                       */
;*---------------------------------------------------------------------*/
(define-macro (INSTANTIATE-JSFUNCTION arity . rest)
   `(case ,(cadr arity)
       ,@(map (lambda (n)
		 `((,n)
		   (,(string->symbol (format "instantiateJsFunction~a" n))
		    ,arity ,@rest)))
	  (iota 5 1))
       (else
	(instantiateJsFunction
	   ,arity ,@rest))))

;*---------------------------------------------------------------------*/
;*    js-make-function ...                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.3.1     */
;*---------------------------------------------------------------------*/
(define (js-make-function %this procedure length name
	   #!key
	   method construct alloc
	   __proto__ prototype
	   (strict 'normal) arity (minlen -1) src rest
	   (constrsize 3) (maxconstrsize 100)
	   (constrmap (js-not-a-cmap)) (shared-cmap #t))
   (with-access::JsGlobalObject %this (js-function js-object)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 (let* ((els ($create-vector (if (eq? strict 'normal) 3 5)))
		(cmap (if (eq? strict 'normal)
			  (cond
			     ((js-object? prototype)
			      js-function-cmap)
			     ((eq? prototype '())
			      js-function-cmap-sans-prototype)
			     (else
			      js-function-writable-cmap))
			  (if (js-object? prototype)
			      js-function-strict-cmap
			      js-function-writable-strict-cmap)))
		(fun (INSTANTIATE-JSFUNCTION
			(procedure procedure)
			(method (or method procedure))
			(construct (or construct procedure))
			(alloc (or alloc js-not-a-constructor-alloc))
			(arity (or arity (procedure-arity procedure)))
			(rest rest)
			(len length)
			(__proto__ (or __proto__ js-function-prototype))
			(src src)
			(constrsize constrsize)
			(constrmap constrmap)
			(maxconstrsize maxconstrsize)
			(elements els)
			(cmap (if (and shared-cmap #f)
				  ;; normal functions, i.e., user functions,
				  ;; use shared-cmap
				  cmap
				  ;; non shared-cmap are used by builtin
				  ;; objects, such as Date or Number to create
				  ;; a single cmap for all their fields
				  (duplicate::JsConstructMap cmap
				     (%id (gencmapid)))))
;* 			(%prototype #f))                               */
;* 			(cmap (if (and shared-cmap (js-object? prototype)) */
;* 				  cmap                                 */
;* 				  (duplicate::JsConstructMap cmap      */
;* 				     (%id (gencmapid)))))              */
			(%prototype #f))))
	    ;; the prototype property
	    ;; the builtin "%prototype" property
	    (with-access::JsFunction fun (%prototype)
	       (vector-set! els 0
		  (cond
		     ((not prototype)
		      ;; MS 2019-01-19
		      ;; all default prototypes share the same cmap
		      ;; because on method conflict a new cmap with
		      ;; a fake transition will be created
		      ;; see extend-mapped-object!@property.scm
		      (let ((p (with-access::JsObject %this (__proto__)
				  (instantiateJsObject
				     (cmap js-function-prototype-cmap)
				     (elements (vector fun))
				     (__proto__ __proto__)))))
			 (set! %prototype p)
			 prototype-property-rw))
		     ((js-object? prototype)
		      (js-bind! %this prototype 'constructor
			 :value fun
			 :configurable #t :enumerable #f :writable #t
			 :hidden-class #t)
		      (set! %prototype prototype)
		      prototype-property-ro)
		     ((eq? prototype (js-undefined))
		      (set! %prototype prototype)
		      prototype-property-undefined)
		     ((null? prototype)
		      (set! %prototype prototype)
		      prototype-property-null)
		     (else
		      (error "js-make-function" "Illegal :prototype"
			 prototype)))))
;* 	    (with-access::JsFunction fun (%prototype)                  */
;* 	       (set! %prototype                                        */
;* 		  (cond                                                */
;* 		     ((isa? prototype JsObject)                        */
;* 		      (begin                                           */
;* 			 (js-bind! %this prototype 'constructor        */
;* 			    :value fun                                 */
;* 			    :configurable #t :enumerable #f :writable #t */
;* 			    :hidden-class #t)                          */
;* 			 prototype))                                   */
;* 		     (prototype                                        */
;* 		      prototype)                                       */
;* 		     (else                                             */
;* 		      (with-access::JsObject %this (__proto__)         */
;* 			 (instantiateJsObject                          */
;* 			    (cmap js-function-prototype-cmap)          */
;* 			    (elements (vector fun))                    */
;* 			    (__proto__ __proto__))))))                 */
;* 	       ;; regular function properties                          */
;* 	       (vector-set! els 0                                      */
;* 		  (cond                                                */
;* 		     ((isa? prototype JsObject)                        */
;* {* 		      (instantiate::JsWrapperDescriptor                *} */
;* {* 			 (name 'prototype)                             *} */
;* {* 			 (enumerable #f)                               *} */
;* {* 			 (configurable #f)                             *} */
;* {* 			 (writable #f)                                 *} */
;* {* 			 (value %prototype)                            *} */
;* {* 			 (%get js-function-prototype-get)              *} */
;* {* 			 (%set js-function-prototype-set))             *} */
;* 		      prototype-property-ro)                           */
;* {* 		      (instantiate::JsValueDescriptor                  *} */
;* {* 			 (name 'prototype)                             *} */
;* {* 			 (enumerable #f)                               *} */
;* {* 			 (configurable #f)                             *} */
;* {* 			 (writable #f)                                 *} */
;* {* 			 (value %prototype)))                          *} */
;* 		     ((eq? prototype '())                              */
;* 		      ;; the Proxy global object has no prototype field */
;* {* 		      (instantiate::JsValueDescriptor                  *} */
;* {* 			 (name '%null)                                 *} */
;* {* 			 (enumerable #f)                               *} */
;* {* 			 (configurable #f)                             *} */
;* {* 			 (writable #f)                                 *} */
;* {* 			 (value '()))                                  *} */
;* 		      prototype-property-null                          */
;* 		      )                                                */
;* 		     (else                                             */
;* {* 		      (instantiate::JsWrapperDescriptor                *} */
;* {* 			 (name 'prototype)                             *} */
;* {* 			 (enumerable #f)                               *} */
;* {* 			 (configurable #f)                             *} */
;* {* 			 (writable #t)                                 *} */
;* {* 			 (value %prototype)                            *} */
;* {* 			 (%get js-function-prototype-get)              *} */
;* {* 			 (%set js-function-prototype-set))             *} */
;* 		      prototype-property-rw                            */
;* 		      )))                                              */
;* 	                                                               */
	    ;; length
	    (vector-set! els 1 length)
	    ;; name
	    (vector-set! els 2 name)
	    ;; strict properties
	    (unless (eq? strict 'normal)
	       (vector-set! els 3 strict-arguments-property)
	       (vector-set! els 4 strict-caller-property))
;* 	       ;; constrmap                                            */
;* 	       (when constrmap                                         */
;* 		  (with-access::JsFunction fun (constrsize constrmap)  */
;* 		     (set! constrmap                                   */
;* 			(instantiate::JsConstructMap                   */
;* 			   (ctor fun)                                  */
;* 			   (size constrsize)))))                       */
	    fun))))

;*---------------------------------------------------------------------*/
;*    js-function-prototype-get ...                                    */
;*---------------------------------------------------------------------*/
(define-inline (js-function-prototype-get o::JsFunction %this)
   (with-access::JsFunction o (%prototype)
      %prototype))

;*---------------------------------------------------------------------*/
;*    js-function-prototype-set ...                                    */
;*---------------------------------------------------------------------*/
(define (js-function-prototype-set o::JsFunction v %this)
   (with-access::JsFunction o (constrmap constrsize %prototype elements cmap)
      ;; as the prototype property is not configurable,
      ;; it is always owned by the object
      (let ((desc (if (eq? cmap (js-not-a-cmap))
		      (find (lambda (d)
			       (with-access::JsPropertyDescriptor d (name)
				  (eq? name 'prototype)))
			 (js-object-properties o))
		      (vector-ref elements 0))))
	 (with-access::JsDataDescriptor desc (writable)
	    (when writable
;* 	       (cond                                                   */
;* {* 		  ((isa? desc JsValueDescriptor)                       *} */
;* {* 		   (with-access::JsValueDescriptor desc (value)        *} */
;* {* 		      (set! value v)))                                 *} */
;* 		  ((isa? desc JsWrapperDescriptor)                     */
;* 		   (with-access::JsWrapperDescriptor desc (value)      */
;* 		      (set! value v))))                                */
	       ;; changing the prototype invalidates the fun's constrmap
	       ;; (MS, change 2019-01-18)
	       (unless (eq? constrmap (js-not-a-cmap))
		  (js-function-set-constrmap! o))
;* 	       (when constrmap                                         */
;* 		  (set! constrmap                                      */
;* 		     (instantiate::JsConstructMap                      */
;* 			(ctor o)                                       */
;* 			(size constrsize))))                           */
	       (set! %prototype v)))))
   v)

;*---------------------------------------------------------------------*/
;*    js-make-function-simple ...                                      */
;*---------------------------------------------------------------------*/
(define (js-make-function-simple %this::JsGlobalObject proc::procedure
	   len::int name::bstring arity::int minlen::int strict::symbol rest::bool
	   constrsize::int)
   (js-make-function %this proc len name
      :prototype #f :__proto__ #f
      :arity arity :strict strict :rest rest :minlen minlen
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
   (js-bind! %this obj 'length
      :value 0
      :enumerable #f :configurable #f :writable #f
      :hidden-class #t)
   
   ;; constructor
   (js-bind! %this obj 'constructor
      :value js-function
      :enumerable #f :configurable #t :writable #t
      :hidden-class #t)

   ;; name
   (js-bind! %this obj 'name
      :value (js-ascii->jsstring "builtin")
      :enumerable #f :configurable #t :writable #f
      :hidden-class #t)
   
   ;; toString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.2
   (define (tostring this)
      (cond
	 ((isa? this JsFunction)
	  (with-access::JsFunction this (src)
	     (if (pair? src)
		 (if (string? (cdr src))
		     (js-string->jsstring (cdr src))
		     (js-string->jsstring
			(format "[Function ~a:~a]"
			   (cadr (car src)) (caddr (car src)))))
		 (let ((name (js-get this 'name %this)))
		    (if (js-jsstring? name)
			(js-jsstring-append
			   (js-ascii->jsstring "[function ")
			   (js-jsstring-append
			      name
			      (js-ascii->jsstring "]")))
			(js-ascii->jsstring "[Function]"))))))
	 (else
	  (js-raise-type-error %this "toString: not a function ~s"
	     (js-typeof this)))))

   (js-bind! %this obj 'toString
      :value (js-make-function %this tostring 0 "toString"
		:prototype (js-undefined))
      :enumerable #f :writable #t :configurable #t
      :hidden-class #t)

   ;; source
   ;; Hop extension
   (define (source this)
      (if (isa? this JsFunction)
	  (with-access::JsFunction this (src)
	     (when (pair? src)
		(js-string->jsstring
		   (format "~a:~a" (cadr (car src)) (caddr (car src))))))
	  (js-raise-type-error %this "source: not a function ~s"
	     (js-typeof this))))
   
   (js-bind! %this obj 'source
      :value (js-make-function %this source 0 "source"
		:prototype (js-undefined))
      :enumerable #f :writable #t :configurable #t
      :hidden-class #t)

   ;; @@hasInstance
   (with-access::JsGlobalObject %this (js-symbol-hasinstance)
      (js-bind! %this obj js-symbol-hasinstance
	 :value (js-make-function %this
		   (lambda (this o) (js-ordinary-instanceof? %this o this))
		   1 "[Symbol.hasInstance]" :prototype (js-undefined))
	 :enumerable #f :writable #f :configurable #f
	 :hidden-class #t))
   
   ;; apply
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.3
   (define (prototype-apply this::obj thisarg argarray)
      (js-apply-array %this this thisarg argarray))

   (js-bind! %this obj 'apply
      :value (js-make-function %this prototype-apply 2 "apply"
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
	 (js-make-function %this call 1 "call" :prototype (js-undefined)))
      (js-bind! %this obj 'call
	 :value js-call
	 :enumerable #f :writable #t :configurable #t
	 :hidden-class #t))
   
   ;; bind
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.5
   (define (bind this::obj thisarg . args)
      (if (not (isa? this JsFunction))
	  (js-raise-type-error %this "bind: this not a function ~s" this)
	  (with-access::JsFunction this (len construct alloc procedure)
	     (let* ((proc (lambda (_ . actuals)
			     (js-apply %this this
				thisarg (append args actuals))))
		    (fun (js-make-function
			    %this
			    proc
			    (maxfx 0 (-fx len (length args)))
			    (js-tostring (js-get this 'name %this) %this)
			    :strict 'strict
			    :alloc alloc
			    :construct proc)))
		fun))))

   (js-bind! %this obj 'bind
      :value (js-make-function %this bind 1 "bind" :prototype (js-undefined))
      :enumerable #f :writable #t :configurable #t
      :hidden-class #t)
   
   ;; hopscript does not support caller
   (js-bind! %this obj 'caller
      :get thrower-get
      :set thrower-set
      :enumerable #f :configurable #f
      :hidden-class #t))

;*---------------------------------------------------------------------*/
;*    js-apply-array ...                                               */
;*---------------------------------------------------------------------*/
(define (js-apply-array %this::JsGlobalObject this thisarg argarray)
   
   (define (vector->sublist vec len)
      (if (=fx (vector-length vec) len)
	  (vector->list vec)
	  (let loop ((i (-fx len 1))
		     (acc '()))
	     (if (=fx i -1)
		 acc
		 (loop (-fx i 1) (cons (vector-ref vec i) acc))))))

   (cond
      ((not (isa? this JsFunction))
       (js-raise-type-error %this
	  "apply: argument not a function ~s" this))
      ((or (eq? argarray (js-null)) (eq? argarray (js-undefined)))
       (js-call0 %this this thisarg))
      ((not (js-object? argarray))
       (js-raise-type-error %this
	  "apply: argument not an object ~s" argarray))
      ((isa? argarray JsArray)
       (let ((len (js-get argarray 'length %this)))
	  (with-access::JsArray argarray (vec)
	     (if (>fx (vector-length vec) 0)
		 ;; fast path
		 (js-apply %this this thisarg
		    (map (lambda (p)
			    (if (eq? p (js-absent)) (js-undefined) p))
		       (vector->sublist vec len)))
		 ;; slow path
		 (let ((properties (js-object-properties argarray)))
		    (js-apply %this this thisarg
		       (map! (lambda (d)
				(js-property-value argarray argarray %this))
			  (filter (lambda (d)
				     (with-access::JsPropertyDescriptor d (name)
					(js-isindex? (js-toindex name))))
			     properties))))))))
      (else
       ;; slow path
       (let ((len (uint32->fixnum
		     (js-touint32
			(js-get argarray 'length %this)
			%this))))
	  ;; assumes here a fixnum length as an iteration over the range
	  ;; 1..2^32-1 is not computable with 2014 computer's performance
	  (let loop ((i 0)
		     (acc '()))
	     (if (=fx i len)
		 ;; fast path
		 (js-apply %this this thisarg (reverse! acc))
		 ;; slow path
		 (loop (+fx i 1)
		    (cons (js-get argarray i %this) acc))))))))
