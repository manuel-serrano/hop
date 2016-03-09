;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/generator.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct 29 21:14:17 2015                          */
;*    Last change :  Wed Mar  9 14:15:28 2016 (serrano)                */
;*    Copyright   :  2015-16 Manuel Serrano                            */
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
   
   (include "stringliteral.sch"
	    "property.sch")
   
   (import __hopscript_types
	   __hopscript_lib
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_regexp
	   __hopscript_array
	   __hopscript_error
	   __hopscript_worker)
   
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
;*    js-yield-cmap ...                                                */
;*---------------------------------------------------------------------*/
(define js-yield-cmap #f)
(define js-generator-cmap #f)

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
;*---------------------------------------------------------------------*/
(define (js-init-generator! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (__proto__ js-generator-prototype)
      
      (define js-gen-proto
	 (instantiate::JsObject
	    (cmap (instantiate::JsConstructMap))
	    (__proto__ __proto__)))
      
      (define (js-generator-done)
	 (instantiate::JsObject
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
		(let ((done (instantiate::JsObject
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
      
      (js-bind! %this js-gen-proto 'constructor
	 :configurable #t :enumerable #f :writable #f
	 :value (js-make-function %this
		   js-generator-construct
		   1 'constructor
		   :construct js-generator-construct))
      
      (js-bind! %this js-gen-proto 'next
	 :configurable #f :enumerable #f
	 :value (js-make-function %this
		   (lambda (this val)
		      (js-generator-next this val #f))
		   1 'next))
      
      (js-bind! %this js-gen-proto 'return
	 :configurable #f :enumerable #f
	 :value (js-make-function %this
		   (lambda (this val)
		      (js-generator-return this val #f))
		   1 'return))
      
      (js-bind! %this js-gen-proto 'throw
	 :configurable #f :enumerable #f
	 :value (js-make-function %this
		   (lambda (this val)
		      (js-generator-next this val #t))
		   1 'throw))
      
      (set! js-generator-prototype js-gen-proto)
      
      (set! js-yield-cmap (js-names->cmap '#(value done)))
      
      (with-access::JsObject js-gen-proto (cmap)
	 (set! js-generator-cmap cmap))
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
   (with-access::JsGlobalObject %this (js-generator-prototype)
      (with-access::JsObject js-generator-prototype (cmap elements)
	 (instantiate::JsGenerator
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
   (with-access::JsGenerator this (cmap properties elements)
      (when (and (null? properties) (isa? cmap JsConstructMap))
	 ;; de-optimze the generator first
	 (set! cmap #f)
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
	 (instantiate::JsObject
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

