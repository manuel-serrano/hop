;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/function.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 22 06:56:33 2013                          */
;*    Last change :  Wed Jun 11 11:46:16 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript function implementation                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_function

   (library hop)

   (import __hopscript_types
	   __hopscript_property
	   __hopscript_object
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_worker)

   (export (js-init-function! ::JsGlobalObject)

	   thrower-get
	   thrower-set
	   
	   (js-make-function::JsFunction ::JsGlobalObject
	      ::procedure ::int ::obj
	      #!key __proto__ prototype construct alloc strict)))

;*---------------------------------------------------------------------*/
;*    throwers                                                         */
;*---------------------------------------------------------------------*/
(define thrower-get #f)
(define thrower-set #f)

;*---------------------------------------------------------------------*/
;*    js-init-function! ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.3       */
;*---------------------------------------------------------------------*/
(define (js-init-function! %this::JsGlobalObject)
   ;; first, bind the builtin function prototype
   (with-access::JsGlobalObject %this ((js-object-prototype __proto__)
				       js-function)
      
      (define js-function-prototype
	 (instantiate::JsFunction
	    (name 'builtin)
	    (arity -1)
	    (procedure (lambda l (js-undefined)))
	    (alloc (lambda (_) #unspecified))
	    (construct (lambda (constructor args)
			  (js-raise-type-error %this "not a constructor ~s"
			     js-function-prototype)))
	    (__proto__ js-object-prototype)))
      
      ;; then, create the properties of the function contructor
      (set! js-function
	 (js-make-function %this
	    (%js-function %this) 1 "Function"
	    :__proto__ js-function-prototype
	    :prototype js-function-prototype
	    :construct (js-function-construct %this)))
      ;; throwers
      (let ((thrower (js-make-function %this
			(lambda (o)
			   (js-raise-type-error %this "[[ThrowTypeError]] ~a" o))
			1 'thrower)))
	 (set! thrower-get thrower)
	 (set! thrower-set thrower))
      ;; prototype properties
      (init-builtin-function-prototype! %this js-function js-function-prototype)
      ;; bind Function in the global object
      (js-bind! %this %this 'Function
	 :value js-function
	 :configurable #f :enumerable #f)
      ;; return the js-function object
      js-function))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsFunction ...                                 */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsService op compile isexpr)
   (with-access::JsService o (svc)
      (compile svc op)))

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
;*---------------------------------------------------------------------*/
(define (js-function-construct %this::JsGlobalObject)
   (lambda (_ . args)
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
		   (%js-eval ip 'eval %this)))))))

;*---------------------------------------------------------------------*/
;*    js-make-function ...                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.3.1     */
;*---------------------------------------------------------------------*/
(define (js-make-function %this procedure length name
	   #!key __proto__ prototype alloc construct strict)
   
   (define (js-not-a-constructor constr)
      (with-access::JsFunction constr (name)
	 (js-raise-type-error %this "not a constructor ~a" name)))
   
   (with-access::JsGlobalObject %this (js-function js-object)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 (let ((fun (instantiate::JsFunction
		       (procedure procedure)
		       (arity (-fx (procedure-arity procedure) 1))
		       (__proto__ (or __proto__ js-function-prototype))
		       (name (or name procedure))
		       (alloc (cond
				 (alloc alloc)
				 (construct (lambda (_) #unspecified))
				 (else js-not-a-constructor)))
		       (construct (or construct list))
		       (constrmap (when construct (instantiate::JsConstructMap))))))
	    (cond
	       (prototype
		(when (isa? prototype JsObject)
		   (js-bind! %this prototype 'constructor
		      :value fun
		      :configurable #t :writable #t :enumerable #f))
		(js-bind! %this fun 'prototype
		   :value prototype
		   :enumerable #f :configurable #f :writable #f))
	       (construct
		(with-access::JsObject %this ((js-object-prototype __proto__))
		   (let ((prototype (instantiate::JsObject
				       (__proto__ js-object-prototype))))
		      (js-bind! %this prototype 'constructor
			 :value fun
			 :configurable #t :writable #t :enumerable #f)
		      (js-bind! %this fun 'prototype
			 :value prototype
			 :enumerable #f :configurable #f :writable #t)))))
	    (js-bind! %this fun 'length
	       :value length
	       :enumerable #f :configurable #f :writable #f)
	    (when strict
	       (js-bind! %this fun 'arguments
		  :get thrower-get :set thrower-set
		  :enumerable #f :configurable #f)
	       (js-bind! %this fun 'caller
		  :get thrower-get :set thrower-set
		  :enumerable #f :configurable #f))
	    fun))))

;*---------------------------------------------------------------------*/
;*    init-builtin-function-prototype! ...                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4       */
;*---------------------------------------------------------------------*/
(define (init-builtin-function-prototype! %this::JsGlobalObject js-function obj)
   ;; length
   (js-bind! %this obj 'length
      :value 0
      :enumerable #f :configurable #f :writable #f)
   
   ;; constructor
   (js-bind! %this obj 'constructor
      :value js-function
      :enumerable #f :configurable #t :writable #t)
   
   ;; toString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.2
   (define (tostring this)
      (cond
	 ((isa? this JsFunction)
	  (with-access::JsFunction this (name)
	     (format "[JsFunction ~a]" name)))
	 (else
	  (js-raise-type-error %this "toString: not a function ~s"
	     (js-typeof this)))))
   
   (js-bind! %this obj 'toString
      :value (js-make-function %this tostring 0 "toString"
		:prototype (js-undefined))
      :enumerable #f :writable #t :configurable #t)
   
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
	     (with-access::JsArray argarray (vec properties)
		(if (>fx (vector-length vec) 0)
		    ;; fast path
		    (js-apply %this this thisarg
		       (map (lambda (p)
			       (if (eq? p (js-absent)) (js-undefined) p))
			  (vector->list vec)))
		    ;; slow path
		    (js-apply %this this thisarg
		       (map! (lambda (d) (js-property-value argarray d %this))
			  (filter (lambda (d)
				     (with-access::JsPropertyDescriptor d (name)
					(js-isindex? (js-toindex name))))
			     properties)))))))
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
      :enumerable #f :writable #t :configurable #t)
   
   ;; call
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.4
   (define (call this::obj thisarg . args)
      (js-apply %this this thisarg args))
   
   (js-bind! %this obj 'call
      :value (js-make-function %this call 1 "call" :prototype (js-undefined))
      :enumerable #f :writable #t :configurable #t)
   
   ;; bind
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.5
   (define (bind this::obj thisarg . args)
      (if (not (isa? this JsFunction))
	  (js-raise-type-error %this "bind: this not a function ~s" this)
	  (with-access::JsFunction this (name arity construct alloc procedure)
	     (let ((fun (lambda (_ . actuals)
			   (js-apply %this this thisarg (append args actuals)))))
		(js-make-function
		   %this
		   fun
		   (maxfx 0 (-fx arity (length args)))
		   name
		   :strict #t
		   :alloc alloc
		   :construct fun)))))
   
   (js-bind! %this obj 'bind
      :value (js-make-function %this bind 1 "bind" :prototype (js-undefined))
      :enumerable #f :writable #t :configurable #t)
   
   ;; hopscript does not support caller
   (js-bind! %this obj 'caller
      :get thrower-get
      :set thrower-set
      :enumerable #f :configurable #f))

