;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/function.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 22 06:56:33 2013                          */
;*    Last change :  Fri Mar  7 10:17:44 2014 (serrano)                */
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
	   __hopscript_public)

   (export js-function
	   js-function-prototype
	   (js-init-function! ::JsObject)

	   thrower-get
	   thrower-set
	   
	   (js-make-function::JsFunction ::procedure ::int ::obj
	      #!key __proto__ prototype construct alloc strict)))

;*---------------------------------------------------------------------*/
;*    js-function ...                                                  */
;*---------------------------------------------------------------------*/
(define js-function #f)
(define js-function-prototype #f)

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
(define (js-init-function! %this)
   ;; first, bind the builtin function prototype
   (set! js-function-prototype 
      (instantiate::JsFunction
	 (name "builtin")
	 (arity -1)
	 (procedure (lambda l (js-undefined)))
	 (alloc (lambda (_) #unspecified))
	 (construct (lambda (constructor args)
		       (js-raise-type-error "not a constructor ~s"
			  js-function-prototype)))
	 (__proto__ js-object-prototype)))
   ;; then, create the properties of the function contructor
   (set! js-function
      (js-make-function %js-function 1 "Function"
	 :__proto__ js-function-prototype
	 :prototype js-function-prototype
	 :construct js-function-construct))
   ;; throwers
   (let ((thrower
	    (js-make-function
	       (lambda (o)
		  (js-raise-type-error "[[ThrowTypeError]] ~a" o))
	       1 'thrower)))
      (set! thrower-get thrower)
      (set! thrower-set thrower))
   ;; prototype properties
   (init-builtin-function-prototype! js-function-prototype)
   ;; bind Function in the global object
   (js-bind! %this 'Function :configurable #f :enumerable #f :value js-function)
   js-function)

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
(define (%js-function this . value)
   (apply js-new js-function value))

;*---------------------------------------------------------------------*/
;*    js-function-construct ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.2.1     */
;*---------------------------------------------------------------------*/
(define (js-function-construct _ . args)
   (if (null? args)
       (js-make-function (lambda (this) (js-undefined))
	  0 "" :construct (lambda (_) (js-undefined)))
       (let* ((len (length args))
	      (formals (take args (-fx len 1)))
	      (body (car (last-pair args)))
	      (fun (format "(function(~(,)) { ~a })"
		      (map js-tostring formals)
		      (js-tostring body))))
	  (%js-eval fun))))

;*---------------------------------------------------------------------*/
;*    js-make-function ...                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.3.1     */
;*---------------------------------------------------------------------*/
(define (js-make-function procedure length name
	   #!key __proto__ prototype alloc construct strict)
   
   (define (js-not-a-constructor constr)
      (with-access::JsFunction constr (name)
	 (js-raise-type-error "not a constructor ~a" name)))
   
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
	     (js-bind! prototype 'constructor :value fun
		:configurable #t :writable #t :enumerable #f))
	  (js-bind! fun 'prototype
	     :value prototype
	     :enumerable #f
	     :configurable #f
	     :writable #f))
	 (construct
	  (let ((prototype (instantiate::JsObject
			      (__proto__ js-object-prototype))))
	     (js-bind! prototype 'constructor :value fun
		:configurable #t :writable #t :enumerable #f)
	     (js-bind! fun 'prototype
		:value prototype
		:enumerable #f
		:configurable #f
		:writable #t))))
      (js-bind! fun 'length
	 :enumerable #f
	 :configurable #f
	 :writable #f
	 :value length)
      (when strict
	 (js-bind! fun 'arguments
	    :get thrower-get
	    :set thrower-set
	    :enumerable #f
	    :configurable #f)
	 (js-bind! fun 'caller
	    :get thrower-get
	    :set thrower-set
	    :enumerable #f :configurable #f))
      fun))

;*---------------------------------------------------------------------*/
;*    init-builtin-function-prototype! ...                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4       */
;*---------------------------------------------------------------------*/
(define (init-builtin-function-prototype! obj)
   (with-access::JsFunction obj (properties)
      (js-bind! obj 'length
	 :value 0
	 :enumerable #f :configurable #f :writable #f)
      (js-bind! obj 'constructor
	 :value js-function
	 :enumerable #f :configurable #t :writable #t)
      (js-bind! obj 'toString
	 :value (js-make-function js-function-prototype-tostring 0 "toString"
		   :prototype (js-undefined))
	 :enumerable #f :writable #t :configurable #t)
      (js-bind! obj 'apply
	 :value (js-make-function js-function-prototype-apply 2 "apply"
		   :prototype (js-undefined))
	 :enumerable #f :writable #t :configurable #t)
      (js-bind! obj 'call
	 :value (js-make-function js-function-prototype-call 1 "call"
		   :prototype (js-undefined))
	 :enumerable #f :writable #t :configurable #t)
      (js-bind! obj 'bind
	 :value (js-make-function js-function-prototype-bind 1 "bind"
		   :prototype (js-undefined))
	 :enumerable #f :writable #t :configurable #t)
      ;; hopscript does not support caller
      (js-bind! obj 'caller
	 :get thrower-get
	 :set thrower-set
	 :enumerable #f :configurable #f)))

;*---------------------------------------------------------------------*/
;*    js-function-prototype-tostring ...                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.2     */
;*---------------------------------------------------------------------*/
(define (js-function-prototype-tostring this)
   (cond
      ((isa? this JsFunction)
       (with-access::JsFunction this (name)
	  (format "[JsFunction ~a]" name)))
      (else
       (js-raise-type-error "toString: not a function ~s" (js-typeof this)))))

;*---------------------------------------------------------------------*/
;*    js-function-prototype-apply ...                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.3     */
;*---------------------------------------------------------------------*/
(define (js-function-prototype-apply this::obj thisarg argarray)
   (cond
      ((not (isa? this JsFunction))
       (js-raise-type-error "apply: argument not a function ~s" this))
      ((or (eq? argarray (js-null)) (eq? argarray (js-undefined)))
       (js-call0 this thisarg))
      ((not (isa? argarray JsObject))
       (js-raise-type-error "apply: argument not an object ~s" argarray))
      ((isa? argarray JsArray)
       (let ((len (js-get argarray 'length)))
	  (with-access::JsArray argarray (vec properties)
	     (if (>fx (vector-length vec) 0)
		 ;; fast path
		 (js-apply this thisarg
		    (map (lambda (p)
			    (if (eq? p (js-absent)) (js-undefined) p))
		       (vector->list vec)))
		 ;; slow path
		 (js-apply this thisarg
		    (map! (lambda (d) (js-property-value argarray d))
		       (filter (lambda (d)
				  (with-access::JsPropertyDescriptor d (name)
				     (js-isindex? (js-toindex name))))
			  properties)))))))
      (else
       ;; slow path
       (let ((len (uint32->fixnum (js-touint32 (js-get argarray 'length)))))
	  ;; assumes here a fixnum length as an iteration over the range
	  ;; 1..2^32-1 is not computable with 2014 computer's performance
	  (with-access::JsArray argarray (vec)
	     (let loop ((i 0)
			(acc '()))
		(if (=fx i len)
		    ;; fast path
		    (js-apply this thisarg (reverse! acc))
		    ;; slow path
		    (loop (+fx i 1) (cons (js-get argarray i) acc)))))))))

;*---------------------------------------------------------------------*/
;*    js-function-prototype-call ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.4     */
;*---------------------------------------------------------------------*/
(define (js-function-prototype-call this::obj thisarg . args)
   (js-apply this thisarg args))

;*---------------------------------------------------------------------*/
;*    js-function-prototype-bind ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.5     */
;*---------------------------------------------------------------------*/
(define (js-function-prototype-bind this::obj thisarg . args)
   (if (not (isa? this JsFunction))
       (js-raise-type-error "bind: this not a function ~s" this)
       (with-access::JsFunction this (name arity construct alloc procedure)
	  (let ((fun (lambda (_ . actuals)
			(js-apply this thisarg (append args actuals)))))
	     (js-make-function
		fun
		(maxfx 0 (-fx arity (length args)))
		name
		:strict #t
		:alloc alloc
		:construct fun)))))
