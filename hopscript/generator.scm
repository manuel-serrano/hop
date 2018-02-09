;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/generator.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct 29 21:14:17 2015                          */
;*    Last change :  Fri Feb  9 11:14:04 2018 (serrano)                */
;*    Copyright   :  2015-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native BIgloo support of JavaScript generators                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/6.0/#14.4             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_generator
   
   (include "../nodejs/nodejs_debug.sch")
   
   (library hop)
   
   (include "types.sch" "stringliteral.sch")
   
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
	   js-yield-cmap
	   (js-make-generator::JsGenerator ::procedure ::JsObject ::JsGlobalObject)
	   (js-make-iterator ::object ::JsGlobalObject)
	   (js-generator-yield ::JsGenerator ::obj ::bool ::obj ::JsGlobalObject)
	   (js-generator-yield* ::JsGenerator ::obj ::bool ::obj ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    Jsstringliteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-begin!)

;*---------------------------------------------------------------------*/
;*    %spawn ...                                                       */
;*---------------------------------------------------------------------*/
(define %spawn::procedure list)

;*---------------------------------------------------------------------*/
;*    js-yield-cmap ...                                                */
;*---------------------------------------------------------------------*/
(define js-yield-cmap #f)

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
   (with-access::JsGlobalObject %this (__proto__ js-function-prototype js-generator-prototype js-generatorfunction-prototype js-symbol-iterator js-symbol-tostringtag)

      (define js-gen-proto
	 (instantiateJsObject
	    (cmap (instantiate::JsConstructMap))
	    (__proto__ __proto__)))

      (define js-genfun-proto
	 (instantiateJsObject
	    (cmap (instantiate::JsConstructMap))
	    (__proto__ js-function-prototype)))
      
      (define (js-generator-done)
	 (instantiateJsObject
	    (__proto__ __proto__)
	    (cmap js-yield-cmap)
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
			       (__proto__ __proto__)
			       (cmap js-yield-cmap)
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
      
      (js-bind! %this js-gen-proto 'next
	 :configurable #f :enumerable #f
	 :value (js-make-function %this
		   (lambda (this val)
		      (js-generator-next this val #f))
		   1 'next)
	 :hidden-class #t)
      
      (js-bind! %this js-gen-proto 'return
	 :configurable #f :enumerable #f
	 :value (js-make-function %this
		   (lambda (this val)
		      (js-generator-return this val #f))
		   1 'return)
	 :hidden-class #t)
      
      (js-bind! %this js-gen-proto 'throw
	 :configurable #f :enumerable #f
	 :value (js-make-function %this
		   (lambda (this val)
		      (js-generator-next this val #t))
		   1 'throw)
	 :hidden-class #t)

      (js-bind! %this js-gen-proto js-symbol-iterator
	 :configurable #t :enumerable #f :writable #t
	 :value (js-make-function %this
		   (lambda (this val)
		      this)
		   0 '@@iterator
		   :prototype (js-undefined)))

      (js-bind! %this js-gen-proto js-symbol-tostringtag
	 :configurable #t :enumerable #f :writable #f
	 :value (js-string->jsstring "Generator"))

      
      (js-bind! %this js-genfun-proto 'constructor
	 :configurable #t :enumerable #f :writable #f
	 :value (js-make-function %this
		   js-generator-construct
		   1 'constructor
		   :construct js-generator-construct)
	 :hidden-class #t)
      
      (js-bind! %this js-genfun-proto js-symbol-tostringtag
	 :configurable #t :enumerable #f :writable #f
	 :value (js-string->jsstring "GeneratorFunction"))
      
      (set! js-generator-prototype js-gen-proto)
      (set! js-generatorfunction-prototype js-genfun-proto)
      
      (set! js-yield-cmap (js-names->cmap '#(value done)))
      
      ))
	    
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
;*    js-make-generator ...                                            */
;*    -------------------------------------------------------------    */
;*    Generator use a special encoding. They directly points to the    */
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
;*    js-put! ::JsGenerator ...                                        */
;*    -------------------------------------------------------------    */
;*    The first time [[PUT]] is called on a generator it is            */
;*    de-optimized (see JS-MAKE-CONSTRUCT).                            */
;*---------------------------------------------------------------------*/
(define-method (js-put! this::JsGenerator prop v throw %this)
   (with-access::JsGenerator this (cmap elements)
      (when (and (null? (js-object-properties this)) (isa? cmap JsConstructMap))
	 ;; de-optimze the generator first
	 (set! cmap (js-not-a-cmap))
	 (set! elements '#()))
      ;; regular [[PUT]] invocation
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    js-make-iterator ...                                             */
;*---------------------------------------------------------------------*/
(define (js-make-iterator obj %this)
   (letrec ((%gen (js-make-generator
		     (lambda (%v %e)
			(let ((i 0))
			   (let loop ((%v %v) (%e %e))
			      (let ((len (js-get obj 'length %this)))
				 (if (>=fx i len)
				     (js-generator-yield %gen
					(js-undefined) #t
					loop %this)
				     (let ((val (js-get obj i %this)))
					(set! i (+fx i 1))
					(js-generator-yield %gen
					   val #f
					   loop %this)))))))
		     (with-access::JsGlobalObject %this (js-generator-prototype)
			js-generator-prototype)
		     %this)))
      %gen))				  
;*---------------------------------------------------------------------*/
;*    js-generator-yield ...                                           */
;*---------------------------------------------------------------------*/
(define (js-generator-yield gen val done kont %this)
   (with-access::JsGenerator gen (%next)
      (set! %next kont)
      (with-access::JsGlobalObject %this (__proto__)
	 (instantiateJsObject
	    (__proto__ __proto__)
	    (cmap js-yield-cmap)
	    (elements (vector val done))))))

;*---------------------------------------------------------------------*/
;*    js-generator-yield* ...                                          */
;*---------------------------------------------------------------------*/
(define (js-generator-yield* gen val done kont %this)
   
   (define (yield* val)
      (let ((next (js-get val 'next %this)))
	 (let loop ((v (js-undefined)) (e #f))
	    (let* ((n (js-call0 %this next val))
		   (value (js-get n 'value %this))
		   (done (js-get n 'done %this)))
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
;*    Jsstringliteral end                                              */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-end!)

