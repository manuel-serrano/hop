;*=====================================================================*/
;*    /tmp/HOPNEW/hop/hopscript/generator.scm                          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct 29 21:14:17 2015                          */
;*    Last change :  Sun Feb 23 14:54:07 2020 (serrano)                */
;*    Copyright   :  2015-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript generators                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/6.0/#14.4             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_generator
   
   (include "../nodejs/nodejs_debug.sch")
   
   (library hop)
   
   (include "types.sch" "stringliteral.sch" "names.sch" "property.sch")
   
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
   
   (export (js-init-generator! ::JsGlobalObject)
	   (js-make-generator::JsGenerator ::procedure ::JsObject ::JsGlobalObject)
	   (js-make-iterator ::obj ::JsGlobalObject)
	   (js-make-map-iterator ::object ::procedure ::JsGlobalObject)
	   (js-make-vector-iterator ::vector ::procedure ::JsGlobalObject)
	   (js-make-list-iterator ::pair-nil ::procedure ::JsGlobalObject)
	   (js-generator-yield ::JsGenerator ::obj ::bool ::obj ::JsGlobalObject)
	   (js-generator-yield* ::JsGenerator ::obj ::bool ::obj ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    %spawn ...                                                       */
;*---------------------------------------------------------------------*/
(define %spawn::procedure list)

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsGenerator ...                              */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsGenerator
   (lambda (o)
      (js-undefined))
   (lambda (o %this) o))

;*---------------------------------------------------------------------*/
;*    js-toprimitive ...                                               */
;*---------------------------------------------------------------------*/
(define-method (js-toprimitive o::JsGenerator preferredtype %this::JsGlobalObject)
   (js-undefined))
   
;*---------------------------------------------------------------------*/
;*    current-loc ...                                                  */
;*---------------------------------------------------------------------*/
(define-expander current-loc
   (lambda (x e)
      (when (epair? x) `',(cer x))))

;*---------------------------------------------------------------------*/
;*    js-init-generator! ...                                           */
;*    -------------------------------------------------------------    */
;*    https://www.ecma-international.org/ecma-262/6.0/#sec-25.2        */
;*---------------------------------------------------------------------*/
(define (js-init-generator! %this::JsGlobalObject)
   (unless (vector? __js_strings) (set! __js_strings (&init!)))
   (js-init-generator-yield-cmap! %this)
   (with-access::JsGlobalObject %this (js-function-prototype
					 js-generator-prototype
					 js-generatorfunction-prototype
					 js-symbol-iterator
					 js-symbol-tostringtag
					 js-yield-cmap)
      
      (define js-gen-proto-proto
	 (let ((proto (instantiateJsObject
			 (cmap (instantiate::JsConstructMap (inline #t)))
			 (__proto__ (js-object-proto %this))
			 (elements ($create-vector 1)))))
	    (js-bind! %this proto js-symbol-iterator
	       :value (js-make-function %this
			 (lambda (this)
			    this)
			 (js-function-arity 0 0)
			 (js-function-info :name "@@iterator" :len 0)
			 :prototype (js-undefined))
	       :writable #t :enumerable #f :configurable #t)
	    proto))
      
      (define js-gen-proto
	 (instantiateJsObject
	    (cmap (instantiate::JsConstructMap (inline #t)))
	    (__proto__ js-gen-proto-proto)
	    (elements ($create-vector 4))))
      
      (define js-genfun-proto
	 (instantiateJsObject
	    (cmap (instantiate::JsConstructMap (inline #t)))
	    (__proto__ js-function-prototype)
	    (elements ($create-vector 2))))
      
      (define (js-generator-done)
	 (instantiateJsObject
	    (cmap js-yield-cmap)
	    (__proto__ (js-object-proto %this))
	    (elements (vector (js-undefined) #t))))
      
      (define (js-generator-next this val exn)
	 (if (isa? this JsGenerator)
	     (with-access::JsGenerator this (%next)
		(if (procedure? %next)
		    (%next val exn)
		    (js-generator-done)))
	     (js-raise-type-error %this "argument not a generator ~a"
		(typeof this))))
      
      (define (js-generator-return this val exn)
	 (if (isa? this JsGenerator)
	     (with-access::JsGenerator this (%next)
		(let ((done (instantiateJsObject
			       (cmap js-yield-cmap)
			       (__proto__ (js-object-proto %this))
			       (elements (vector val #t)))))
		   (set! %next #f)
		   done))
	     (js-raise-type-error %this "argument not a generator ~a"
		(typeof this))))
      
      (define (js-generator-construct this . args)
	 (if (null? args)
	     (js-make-generator
		(lambda (val exn)
		   (js-generator-done))
		js-generator-prototype
		%this)
	     (let* ((len (length args))
		    (formals (take args (-fx len 1)))
		    (body (car (last-pair args)))
		    (fun (format "(function* (~(,)) { ~a })"
			    (map (lambda (o) (js-tostring o %this)) formals)
			    (js-tostring body %this))))
		(call-with-input-string fun
		   (lambda (ip)
		      (%js-eval ip 'eval %this this %this))))))
      
      (js-bind! %this js-gen-proto (& "next")
	 :configurable #f :enumerable #f
	 :value (js-make-function %this
		   (lambda (this val)
		      (js-generator-next this val #f))
		   (js-function-arity 1 0)
		   (js-function-info :name "next" :len 1))
	 :hidden-class #t)
      
      (js-bind! %this js-gen-proto (& "return")
	 :configurable #f :enumerable #f
	 :value (js-make-function %this
		   (lambda (this val)
		      (js-generator-return this val #f))
		   (js-function-arity 1 0)
		   (js-function-info :name "return" :len 1))
	 :hidden-class #t)
      
      (js-bind! %this js-gen-proto (& "throw")
	 :configurable #f :enumerable #f
	 :value (js-make-function %this
		   (lambda (this val)
		      (js-generator-next this val #t))
		   (js-function-arity 1 0)
		   (js-function-info :name "throw" :len 1))
	 :hidden-class #t)
      
      (js-bind! %this js-gen-proto js-symbol-tostringtag
	 :configurable #t :enumerable #f :writable #f
	 :value (js-string->jsstring "Generator"))
      
      
      (js-bind! %this js-genfun-proto (& "constructor")
	 :configurable #t :enumerable #f :writable #f
	 :value (js-make-function %this js-generator-construct
		   (js-function-arity js-generator-construct)
		   (js-function-info :name "constructor" :len 1)
		   :alloc js-no-alloc)
	 :hidden-class #t)
      
      (js-bind! %this js-genfun-proto js-symbol-tostringtag
	 :configurable #t :enumerable #f :writable #f
	 :value (js-string->jsstring "GeneratorFunction"))
      
      (set! js-generator-prototype js-gen-proto)
      (set! js-generatorfunction-prototype js-genfun-proto)
      ))

;*---------------------------------------------------------------------*/
;*    js-init-generator-yield-cmap! ...                                */
;*---------------------------------------------------------------------*/
(define (js-init-generator-yield-cmap! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-yield-cmap)
      (set! js-yield-cmap
	 (let ((props `#(,(prop (& "value") (property-flags #t #t #t #f))
			 ,(prop (& "done") (property-flags #t #t #t #f)))))
	    (instantiate::JsConstructMap
	       (props props)
	       (methods (make-vector (vector-length props))))))))
	    
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
	     (js-function-arity 0 0)
	     (js-function-info :name "" :len 0))
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
;*    js-put! ::JsGenerator ...                                        */
;*    -------------------------------------------------------------    */
;*    The first time [[PUT]] is called on a generator it is            */
;*    de-optimized (see JS-MAKE-CONSTRUCT).                            */
;*---------------------------------------------------------------------*/
(define-method (js-put! this::JsGenerator prop v throw %this)
   (with-access::JsGenerator this (cmap elements)
      (when (isa? cmap JsConstructMap)
	 ;; de-optimze the generator first
	 (js-object-unmap! this))
      ;; regular [[PUT]] invocation
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    js-make-generator ...                                            */
;*    -------------------------------------------------------------    */
;*    Generators use a special encoding. They directly point to the    */
;*    elements and cmap of their prototype. This optimizes "next"      */
;*    and "throw" access. To preserve the semantics, JS-PUT! is        */
;*    overriden for that class.                                        */
;*---------------------------------------------------------------------*/
(define (js-make-generator proc proto %this)
   (with-access::JsGlobalObject %this (js-generatorfunction-prototype)
      (with-access::JsObject js-generatorfunction-prototype (cmap elements)
	 (instantiateJsGenerator
	    (cmap cmap)
	    (elements elements)
	    (__proto__ proto)
	    (%next proc)))))

;*---------------------------------------------------------------------*/
;*    js-make-map-iterator ...                                         */
;*---------------------------------------------------------------------*/
(define (js-make-map-iterator obj proc %this)
   (letrec ((%gen (js-make-generator
		     (lambda (%v %e)
			(let ((i 0))
			   (let loop ((%v %v) (%e %e))
			      (let ((len (js-get obj (& "length") %this)))
				 (if (>=fx i len)
				     (js-generator-yield %gen
					(js-undefined) #t
					loop %this)
				     (let ((val (proc i (js-get obj i %this))))
					(set! i (+fx i 1))
					(js-generator-yield %gen
					   val #f
					   loop %this)))))))
		     (with-access::JsGlobalObject %this (js-generator-prototype)
			js-generator-prototype)
		     %this)))
      %gen))

;*---------------------------------------------------------------------*/
;*    js-make-vector-iterator ...                                      */
;*---------------------------------------------------------------------*/
(define (js-make-vector-iterator obj::vector proc %this)
   (letrec ((%gen (js-make-generator
		     (lambda (%v %e)
			(let ((l obj)
			      (i 0))
			   (let laap ((%v %v) (%e %e))
			      (if (=fx i (vector-length obj))
				  (js-generator-yield %gen
				     (js-undefined) #t
				     laap %this)
				  (let ((val (vector-ref obj i)))
				     (set! i (+fx i 1))
				     (if (eq? val (js-absent))
					 (laap %v %e) 
					 (js-generator-yield %gen
					    (proc %this val) #f
					    laap %this)))))))
		     (with-access::JsGlobalObject %this (js-generator-prototype)
			js-generator-prototype)
		     %this)))
      %gen))

;*---------------------------------------------------------------------*/
;*    js-make-list-iterator ...                                        */
;*---------------------------------------------------------------------*/
(define (js-make-list-iterator obj::pair-nil proc %this)
   '(letrec ((%gen (js-make-generator
		     (lambda (%v %e)
			(let ((l obj))
			   (let loop ((%v %v) (%e %e))
			      (if (null? l)
				  (js-generator-yield %gen
				     (js-undefined) #t
				     loop %this)
				  (let ((val (car l)))
				     (set! l (cdr l))
				     (if (eq? val (js-absent))
					 (loop %v %e)
					 (js-generator-yield %gen
					    (proc %this val) #f
					    loop %this)))))))
		     (with-access::JsGlobalObject %this (js-generator-prototype)
			js-generator-prototype)
		     %this)))
      %gen))

;*---------------------------------------------------------------------*/
;*    js-make-iterator ...                                             */
;*---------------------------------------------------------------------*/
(define (js-make-iterator obj %this)
   (js-make-map-iterator obj (lambda (key val) val) %this))

;*---------------------------------------------------------------------*/
;*    js-generator-yield ...                                           */
;*---------------------------------------------------------------------*/
(define (js-generator-yield gen val done kont %this)
   (with-access::JsGenerator gen (%next)
      (set! %next kont)
      (with-access::JsGlobalObject %this (js-yield-cmap)
	 (instantiateJsObject
	    (cmap js-yield-cmap)
	    (__proto__ (js-object-proto %this))
	    (elements (vector val done))))))

;*---------------------------------------------------------------------*/
;*    js-generator-yield* ...                                          */
;*---------------------------------------------------------------------*/
(define (js-generator-yield* gen val done kont %this)
   
   (define (yield* val)
      (let ((next (js-get val (& "next") %this)))
	 (let loop ((v (js-undefined)) (e #f))
	    (let* ((n (js-call0 %this next val))
		   (value (js-get n (& "value") %this))
		   (done (js-get n (& "done") %this)))
	       (if done
		   (kont value #f)
		   (js-generator-yield gen value #f
		      loop %this))))))

   (with-access::JsGlobalObject %this (js-symbol-iterator)
      (cond
	 ((isa? val JsGenerator)
	  (yield* val))
	 ((js-get val js-symbol-iterator %this)
	  =>
	  (lambda (g)
	     (yield* (js-call0 %this g val))))
	 (else
	  (yield* val)))))


;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
