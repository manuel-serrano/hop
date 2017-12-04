;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/function.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 22 06:56:33 2013                          */
;*    Last change :  Mon Dec  4 11:07:01 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
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
	   __hopscript_worker)
   
   (export (js-init-function! ::JsGlobalObject)

	   thrower-get
	   thrower-set
	   
	   (js-function-debug-name::bstring ::JsFunction)
	   (js-make-function::JsFunction ::JsGlobalObject
	      ::procedure ::int ::obj
	      #!key
	      __proto__ prototype constructor construct alloc
	      (strict 'normal) arity (minlen -1) src rest
	      (constrsize 3) (maxconstrsize 100) method (shared-cmap #t))
	   (js-make-function-simple::JsFunction ::JsGlobalObject ::procedure
	      ::int ::obj ::int ::int ::symbol ::bool ::int)))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsFunction ...                               */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsFunction
   (lambda (o)
      (js-undefined))
   (lambda (o %this) o))

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
	 ,(prop 'name (property-flags #f #f #f #f))
	 ,(prop 'source (property-flags #f #f #f #f)))))

(define js-function-strict-cmap
   (make-cmap
      `#(,(prop 'prototype (property-flags #f #f #f #f))
	 ,(prop 'length (property-flags #f #f #f #f))
	 ,(prop 'name (property-flags #f #f #f #f))
	 ,(prop 'source (property-flags #f #f #f #f))
	 ,(prop 'arguments (property-flags #f #f #f #f))
	 ,(prop 'caller (property-flags #f #f #f #f)))))

(define js-function-writable-cmap
   (make-cmap
      `#(,(prop 'prototype (property-flags #t #f #f #f))
	 ,(prop 'length (property-flags #f #f #f #f))
	 ,(prop 'name (property-flags #f #f #f #f))
	 ,(prop 'source (property-flags #f #f #f #f)))))

(define js-function-writable-strict-cmap
   (make-cmap
      `#(,(prop 'prototype (property-flags #t #f #f #f))
	 ,(prop 'length (property-flags #f #f #f #f))
	 ,(prop 'name (property-flags #f #f #f #f))
	 ,(prop 'source (property-flags #f #f #f #f))
	 ,(prop 'arguments (property-flags #f #f #f #f))
	 ,(prop 'caller (property-flags #f #f #f #f)))))
   
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
      
      (set! js-function-prototype
	 (instantiateJsFunction
	    (name "builtin")
	    (src "[Function.__proto__@function.scm]")
	    (len -1)
	    (procedure (lambda l (js-undefined)))
	    (method (lambda l (js-undefined)))
	    (alloc (lambda (_) #unspecified))
	    (construct (lambda (constructor args)
			  (js-raise-type-error %this "not a constructor ~s"
			     js-function-prototype)))
	    (arity -1)
	    (%prototype (with-access::JsGlobalObject %this (__proto__)
			   __proto__))
	    (cmap (instantiate::JsConstructMap))
	    (__proto__ js-object-prototype)))

      (set! js-function-strict-prototype
	 (instantiateJsObject
	    (cmap (instantiate::JsConstructMap))
	    (__proto__ js-function-prototype)))
      
      ;; then, create the properties of the function contructor
      (set! js-function
	 (js-make-function %this
	    (%js-function %this) 1 "Function"
	    :src (cons (current-loc) "Function() { /* function.scm */}")
	    :__proto__ js-function-prototype
	    :prototype js-function-prototype
	    :construct (js-function-construct %this)))
      ;; throwers
      (let* ((throw1 (lambda (o)
			(js-raise-type-error %this "[[ThrowTypeError]] ~a" o)))
	     (throw2 (lambda (o v)
			(js-raise-type-error %this "[[ThrowTypeError]] ~a" o)))
	     (thrower (js-make-function %this throw1
			 1 'thrower)))
	 (set! thrower-get thrower)
	 (set! thrower-set thrower)
	 (set! strict-arguments-property
	    (instantiate::JsAccessorDescriptor
	       (name 'arguments)
	       (get thrower-get)
	       (set thrower-set)
	       (%get throw1)
	       (%set throw2)
	       (enumerable #f)
	       (configurable #f)))
	 (set! strict-caller-property
	    (instantiate::JsAccessorDescriptor
	       (name 'caller)
	       (get thrower-get)
	       (set thrower-set)
	       (%get throw1)
	       (%set throw2)
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
;*---------------------------------------------------------------------*/
(define (%js-function %this::JsGlobalObject)
   (lambda (this . value)
      (with-access::JsGlobalObject %this (js-function)
	 (apply js-new %this js-function value))))

;*---------------------------------------------------------------------*/
;*    js-function-construct ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.2.1     */
;*    -------------------------------------------------------------    */
;*    This definition is overriden by the definition of                */
;*    nodejs_require (nodejs/require.scm).                             */
;*---------------------------------------------------------------------*/
(define (js-function-construct %this::JsGlobalObject)
   (lambda (this . args)
      (if (null? args)
	  (js-make-function %this (lambda (this) (js-undefined))
	     0 "" :construct (lambda (_) (js-undefined)))
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
(define (js-function-debug-name::bstring obj::JsFunction)
   (with-access::JsFunction obj (name src)
      (cond
	 ((and (string? name) (not (string-null? name))) name)
	 ((pair? src) (format "~a:~a" (cadr (car src)) (caddr (car src))))
	 (else "function"))))

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
	   #!key __proto__ prototype
	   constructor alloc construct (strict 'normal)
	   arity (minlen -1) src rest
	   (constrsize 3) (maxconstrsize 100) method (shared-cmap #t))
   
   (define (js-not-a-constructor constr)
      (with-access::JsFunction constr (name)
	 (js-raise-type-error %this (format "~s not a constructor ~~a" name)
	    name)))

   (define (get-source)
      
      (define (source this::JsFunction)
	 (with-access::JsFunction this (src)
	    (when (pair? src)
	       (js-string->jsstring
		  (format "~a:~a" (cadr (car src)) (caddr (car src)))))))
      
      (if js-get-source
	  js-get-source
	  (set! js-get-source
	     (instantiateJsFunction
		(procedure source)
		(method source)
		(arity 0)
		(minlen 0)
		(len 0)
		(alloc js-not-a-constructor)
		(construct list)
		(name "source")
		(%prototype (with-access::JsGlobalObject %this (__proto__)
			       __proto__))))))

   (define (prototype-set o::JsFunction v)
      (with-access::JsFunction o (constrmap %prototype elements cmap)
	 ;; as the prototype property is not configurable, we are sure
	 ;; to find it in the object
	 (let ((desc (if (eq? cmap (js-not-a-cmap))
			 (find (lambda (d)
				  (with-access::JsPropertyDescriptor d (name)
				     (eq? name 'prototype)))
			    (js-object-properties o))
			 (vector-ref elements 0))))
	    (with-access::JsDataDescriptor desc (writable)
	       (when writable
		  (cond
		     ((isa? desc JsValueDescriptor)
		      (with-access::JsValueDescriptor desc (value)
			 (set! value v)))
		     ((isa? desc JsWrapperDescriptor)
		      (with-access::JsWrapperDescriptor desc (value)
			 (set! value v))))
		  (set! constrmap #f)
		  (with-access::JsGlobalObject %this (__proto__)
		     (set! %prototype (if (isa? v JsObject) v __proto__)))))))
      v)

   (define (get__proto__ __proto__)
      (or __proto__
	  (with-access::JsGlobalObject %this (js-function-prototype
						js-function-strict-prototype)
	     (if (eq? strict 'normal)
		 js-function-prototype
		 js-function-strict-prototype))))
   
   (with-access::JsGlobalObject %this (js-function js-object)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 ;; MS CARE: replace js-function-prototype with
	 ;; JsGlobalObject js-function-prototype!
	 (let* ((constr (or construct list))
		(fname (if (symbol? name) (symbol->string! name) name))
		(els (if (eq? strict 'normal)
			 ($create-vector 4)
			 ($create-vector 6)))
		(proto (cond
			  ((isa? prototype JsObject)
			   prototype)
			  (construct
			   (with-access::JsObject %this (__proto__)
			      (instantiateJsObject
				 (cmap (instantiate::JsConstructMap))
				 (__proto__ __proto__))))
			  (else
			   #f)))
		(cmap (if (eq? strict 'normal)
			  (if (isa? prototype JsObject)
			      js-function-cmap
			      js-function-writable-cmap)
			  (if (isa? prototype JsObject)
			      js-function-strict-cmap
			      js-function-writable-strict-cmap)))
		(fun (INSTANTIATE-JSFUNCTION
			(arity (or arity (procedure-arity procedure)))
			(procedure procedure)
			(method (or method procedure))
			(rest rest)
			(len length)
			(__proto__ (or __proto__ js-function-prototype))
			(name fname)
			(src src)
			(alloc (cond
				  (alloc alloc)
				  (construct (lambda (_) #unspecified))
				  (else js-not-a-constructor)))
			(constrsize constrsize)
			(maxconstrsize maxconstrsize)
			(construct constr)
			(elements els)
			(cmap (if (and shared-cmap (isa? prototype JsObject))
				  cmap
				  (duplicate::JsConstructMap cmap
				     (%id (gencmapid)))))
			(%prototype (or proto
					(with-access::JsObject %this (__proto__)
					   __proto__)))
			(constructor constructor))))
	    ;; the builtin %prototype field and the prototype property
	    ;; are not aliases. when the property is not an object,
	    ;; %prototype will be the default prototype
	    ;; while the property will retain the user value.
	    (when proto
	       (js-bind! %this proto 'constructor
		  :value fun
		  :configurable #t :enumerable #f :writable #t
		  :hidden-class #t))
	    (vector-set! els 0
	       (if (isa? prototype JsObject)
		   (instantiate::JsValueDescriptor
		      (name 'prototype)
		      (enumerable #f)
		      (configurable #f)
		      (writable #f)
		      (value prototype))
		   (instantiate::JsWrapperDescriptor
		      (name 'prototype)
		      (enumerable #f)
		      (configurable #f)
		      (writable #t)
		      (value (if construct proto (js-undefined)))
		      (%set prototype-set))))
	    ;; length
	    (vector-set! els 1 length)
	    ;; name
	    (vector-set! els 2 (js-string->jsstring fname))
	    ;; source is an hop extension
	    (vector-set! els 3 (get-source))
	    ;; strict properties
	    (unless (eq? strict 'normal)
	       (vector-set! els 4 strict-arguments-property)
	       (vector-set! els 5 strict-caller-property))
	    fun))))

;*---------------------------------------------------------------------*/
;*    js-get-source ...                                                */
;*---------------------------------------------------------------------*/
(define js-get-source #f)

;*---------------------------------------------------------------------*/
;*    js-make-function-simple ...                                      */
;*---------------------------------------------------------------------*/
(define (js-make-function-simple %this::JsGlobalObject proc::procedure
	   len::int name arity::int minlen::int strict::symbol rest::bool
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
   
   ;; toString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.2
   (define (tostring this)
      (cond
	 ((isa? this JsFunction)
	  (with-access::JsFunction this (name src)
	     (cond
		((pair? src)
		 (if (string? (cdr src))
		     (js-string->jsstring (cdr src))
		     (js-string->jsstring
			(format "[Function ~a~a]"
			   (cadr (car src)) (caddr (car src))))))
		((>fx (string-length name) 0)
		 (js-string->jsstring (format "[Function ~a]" name)))
		(else
		 (js-string->jsstring "[Function]")))))
	 (else
	  (js-raise-type-error %this "toString: not a function ~s"
	     (js-typeof this)))))

   (define (vector->sublist vec len)
      (if (=fx (vector-length vec) len)
	  (vector->list vec)
	  (let loop ((i (-fx len 1))
		     (acc '()))
	     (if (=fx i -1)
		 acc
		 (loop (-fx i 1) (cons (vector-ref vec i) acc))))))

   
   (js-bind! %this obj 'toString
      :value (js-make-function %this tostring 0 "toString"
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
      (cond
	 ((not (isa? this JsFunction))
	  (js-raise-type-error %this
	     "apply: argument not a function ~s" this))
	 ((or (eq? argarray (js-null)) (eq? argarray (js-undefined)))
	  (js-call0 %this this thisarg))
	 ((not (isa? argarray JsObject))
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
		    ;; CARE (5 jul 2014): MS NOT SURE OF THE SECOND ARGARRAY BELOW
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
	     (with-access::JsArray argarray (vec)
		(let loop ((i 0)
			   (acc '()))
		   (if (=fx i len)
		       ;; fast path
		       (js-apply %this this thisarg (reverse! acc))
		       ;; slow path
		       (loop (+fx i 1)
			  (cons (js-get argarray i %this) acc)))))))))
   
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
	  (with-access::JsFunction this (name len construct alloc procedure)
	     (let ((fun (lambda (_ . actuals)
			   (js-apply %this this thisarg (append args actuals)))))
		(js-make-function
		   %this
		   fun
		   (maxfx 0 (-fx len (length args)))
		   name
		   :strict 'strict
		   :alloc alloc
		   :construct fun)))))

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
