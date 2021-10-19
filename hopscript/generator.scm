;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/generator.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct 29 21:14:17 2015                          */
;*    Last change :  Tue Oct 19 07:13:08 2021 (serrano)                */
;*    Copyright   :  2015-21 Manuel Serrano                            */
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
	   (js-generator-done ::obj ::obj ::JsGenerator ::JsGlobalObject)
	   (js-make-generator::JsGenerator ::long ::procedure ::JsObject ::JsGlobalObject)
	   (js-make-iterator ::obj ::JsGlobalObject)
	   (inline js-generator-ref ::JsGenerator ::long)
	   (inline js-generator-set! ::JsGenerator ::long ::obj)
	   (js-make-map-iterator ::object ::procedure ::JsGlobalObject)
	   (js-make-vector-iterator ::vector ::procedure ::JsGlobalObject)
	   (js-make-list-iterator ::pair-nil ::procedure ::JsGlobalObject)
	   (inline js-make-yield ::obj ::bool ::JsGlobalObject)
	   (inline js-generator-yield ::JsGenerator ::obj ::bool ::obj ::JsGlobalObject)
	   (js-generator-yield* ::JsGenerator ::obj ::bool ::obj ::JsGlobalObject)
	   (js-generator-maybe-next ::obj ::obj ::JsGlobalObject ::obj)
	   (js-generator-maybe-next0 ::obj ::JsGlobalObject ::obj)))

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
   (js-init-generator-cmap! %this)
   (with-access::JsGlobalObject %this (js-function-prototype
					 js-generator-prototype
					 js-generatorfunction-prototype
					 js-symbol-iterator
					 js-symbol-tostringtag
					 js-yield-cmap
					 js-generator-pcache)
      
      (define js-gen-proto-proto
	 (let ((proto (instantiateJsObject
			 (cmap (js-make-jsconstructmap))
			 (__proto__ (js-object-proto %this))
			 (elements ($create-vector 1)))))
	    (js-bind! %this proto js-symbol-iterator
	       :value (js-make-function %this
			 (lambda (this) this)
			 (js-function-arity 0 0)
			 (js-function-info :name "@@iterator" :len 0)
			 :prototype (js-undefined))
	       :writable #t :enumerable #f :configurable #t)
	    proto))
      
      (define js-gen-proto
	 (instantiateJsObject
	    (cmap (js-make-jsconstructmap))
	    (__proto__ js-gen-proto-proto)
	    (elements ($create-vector 4))))
      
      (define js-genfun-proto
	 (let ((v ($create-vector 2)))
	    ;; force a non-inline allocation for vector
	    (instantiateJsObject
	       (cmap (js-make-jsconstructmap))
	       (__proto__ js-function-prototype)
	       (elements v))))

      (define (js-generator-next this val)
	 (if (isa? this JsGenerator)
	     (with-access::JsGenerator this (%next)
		(%next val #f this %this))
	     (js-raise-type-error %this "argument not a generator ~a"
		(typeof this))))

      (define (js-generator-throw this val)
	 (if (isa? this JsGenerator)
	     (with-access::JsGenerator this (%next)
		(%next val #t this %this))
	     (js-raise-type-error %this "argument not a generator ~a"
		(typeof this))))
      
      (define (js-generator-return this val)
	 (if (isa? this JsGenerator)
	     (with-access::JsGenerator this (%next)
		(let ((done (instantiateJsObject
			       (cmap js-yield-cmap)
			       (__proto__ (js-object-proto %this))
			       (elements (vector val #t)))))
		   (set! %next js-generator-done)
		   done))
	     (js-raise-type-error %this "argument not a generator ~a"
		(typeof this))))
      
      (define (js-generator-construct this . args)
	 (if (null? args)
	     (js-make-generator 0
		js-generator-done
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

      (set! js-generator-pcache
	 ((@ js-make-pcache-table __hopscript_property) 3 "generator"))
      
      (js-bind! %this js-gen-proto (& "next")
	 :configurable #f :enumerable #f
	 :value (js-make-function %this
		   js-generator-next
		   (js-function-arity 1 0)
		   (js-function-info :name "next" :len 1))
	 :hidden-class #t)
      
      (js-bind! %this js-gen-proto (& "return")
	 :configurable #f :enumerable #f
	 :value (js-make-function %this
		   js-generator-return
		   (js-function-arity 1 0)
		   (js-function-info :name "return" :len 1))
	 :hidden-class #t)
      
      (js-bind! %this js-gen-proto (& "throw")
	 :configurable #f :enumerable #f
	 :value (js-make-function %this
		   js-generator-throw
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
;*    js-generator-done ...                                            */
;*---------------------------------------------------------------------*/
(define (js-generator-done val exn %gen %this)
   (js-make-yield (js-undefined) #t %this))

;*---------------------------------------------------------------------*/
;*    js-init-generator-cmap! ...                                      */
;*---------------------------------------------------------------------*/
(define (js-init-generator-cmap! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-yield-cmap js-generator-cmap)
      ;; the order of "value" and "done" is used by
      ;; JS-GENERATOR-YIELD, JS-GENERATOR-YIELD*, and JS-YIELD-SET!
      (set! js-yield-cmap
	 (let ((props `#(,(prop (& "value") (property-flags #t #t #t #f #f))
			 ,(prop (& "done") (property-flags #t #t #t #f #f)))))
	    (js-make-jsconstructmap
	       :props props
	       :methods (make-vector (vector-length props)))))
      (set! js-generator-cmap
	 (js-make-jsconstructmap))))
	    
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
;*    js-jsobject->jsarray ::JsObject ...                              */
;*---------------------------------------------------------------------*/
(define-method (js-jsobject->jsarray o::JsGenerator %this::JsGlobalObject)
   
   (define (js-jsgenerator->jsarray o %this)
      (with-access::JsGenerator o (%next)
	 (let loop ((acc '()))
	    (let ((n (%next (js-undefined) #f o %this)))
	       (if (eq? (js-object-inline-ref n 1) #t)
		   (js-vector->jsarray (list->vector (reverse! acc)) %this)
		   (let ((val (js-object-inline-ref n 0)))
		      (loop (cons val acc))))))))
   
   (with-access::JsGlobalObject %this (js-symbol-iterator)
      (if (and #t (js-object-mode-plain? o))
	  (js-jsgenerator->jsarray o %this)
	  (let ((fun (js-get o js-symbol-iterator %this))
		(acc '()))
	     (if (js-procedure? fun)
		 (begin
		    (js-for-of-iterator (js-call0 %this fun o) o
		       (lambda (e %this)
			  (set! acc (cons e acc))) #f %this)
		    (js-vector->jsarray (list->vector (reverse! acc)) %this))
		 (js-raise-type-error %this "call: not an interator ~s"
		    o))))))

;*---------------------------------------------------------------------*/
;*    js-make-generator ...                                            */
;*---------------------------------------------------------------------*/
(define (js-make-generator size proc proto %this)
   (with-access::JsGlobalObject %this (js-generator-cmap)
      (instantiateJsGenerator
	 (cmap js-generator-cmap)
	 (elements '#())
	 (__proto__ proto)
	 (%next proc)
	 (%env (if (>fx size 0) (make-vector size) '#())))))

;*---------------------------------------------------------------------*/
;*    js-generator-ref ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (js-generator-ref obj::JsGenerator idx)
   (with-access::JsGenerator obj (%env)
      (vector-ref %env idx)))
   
;*---------------------------------------------------------------------*/
;*    js-generator-set! ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-generator-set! obj::JsGenerator idx val)
   (with-access::JsGenerator obj (%env)
      (vector-set! %env idx val)))
   
;*---------------------------------------------------------------------*/
;*    js-make-yield ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-make-yield val done %this)
   (with-access::JsGlobalObject %this (js-yield-cmap)
      (let ((o (js-make-jsobject 2 js-yield-cmap (js-object-proto %this))))
	 (js-object-inline-set! o 0 val)
	 (js-object-inline-set! o 1 done)
	 o)))

;*---------------------------------------------------------------------*/
;*    js-yield-set! ...                                                */
;*---------------------------------------------------------------------*/
(define (js-yield-set! yield::JsObject val done)
   (js-object-inline-set! yield 0 val)
   (js-object-inline-set! yield 1 done)
   yield)
   
;*---------------------------------------------------------------------*/
;*    js-make-map-iterator ...                                         */
;*---------------------------------------------------------------------*/
(define (js-make-map-iterator obj proc %this)
   (js-make-generator 0
      (lambda (%v %e %gen %this)
	 (let ((i 0))
	    (let loop ((%v %v) (%e %e) (%gen %gen) (%this %this))
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
      %this))

;*---------------------------------------------------------------------*/
;*    js-make-vector-iterator ...                                      */
;*---------------------------------------------------------------------*/
(define (js-make-vector-iterator obj::vector proc %this)
   (let ((gen (js-make-generator 4
		 (lambda (%v %e %gen %this)
		    (let laap ((%v %v) (%e %e) (%gen %gen) (%this %this))
		       (let ((obj (js-generator-ref %gen 0))
			     (i (js-generator-ref %gen 1))
			     (l (js-generator-ref %gen 2)))
			  (if (=fx i l)
			      (js-generator-yield %gen
				 (js-undefined) #t
				 laap %this)
			      (let ((val (vector-ref obj i)))
				 (js-generator-set! %gen 1 (+fx i 1))
				 (if (eq? val (js-absent))
				     (laap %v %e %gen %this)
				     (let ((proc (js-generator-ref %gen 3)))
					(js-generator-yield %gen
					   (proc %this val) #f
					   laap %this))))))))
		 (with-access::JsGlobalObject %this (js-generator-prototype)
		    js-generator-prototype)
		 %this)))
      (js-generator-set! gen 0 obj)
      (js-generator-set! gen 1 0)
      (js-generator-set! gen 2 (vector-length obj))
      (js-generator-set! gen 3 proc)
      gen))

;*---------------------------------------------------------------------*/
;*    js-make-list-iterator ...                                        */
;*---------------------------------------------------------------------*/
(define (js-make-list-iterator obj::pair-nil proc %this)
   (js-make-generator 0
      (lambda (%v %e %gen %this)
	 (let ((l obj))
	    (let loop ((%v %v) (%e %e) (%gen %gen) (%this %this))
	       (if (null? l)
		   (js-generator-yield %gen
		      (js-undefined) #t
		      loop %this)
		   (let ((val (car l)))
		      (set! l (cdr l))
		      (if (eq? val (js-absent))
			  (loop %v %e %gen %this)
			  (js-generator-yield %gen
			     (proc %this val) #f
			     loop %this)))))))
      (with-access::JsGlobalObject %this (js-generator-prototype)
	 js-generator-prototype)
      %this))

;*---------------------------------------------------------------------*/
;*    js-make-iterator ...                                             */
;*---------------------------------------------------------------------*/
(define (js-make-iterator obj %this)
   (js-make-map-iterator obj (lambda (key val) val) %this))

;*---------------------------------------------------------------------*/
;*    js-generator-yield ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-generator-yield gen val done kont %this)
   (with-access::JsGenerator gen (%next)
      (set! %next kont)
      (js-make-yield val done %this)))

;*---------------------------------------------------------------------*/
;*    js-generator-yield*-loop ...                                     */
;*---------------------------------------------------------------------*/
(define (js-generator-yield*-loop v e g %this)
   (let ((%arg* (js-generator-ref g 0))
	 (%kont* (js-generator-ref g 1)))
      (with-access::JsGenerator %arg* (%next)
	 (let* ((n (%next (js-undefined) #f %arg* %this))
		(done (js-object-inline-ref n 1)))
	    (if done
		(let ((value (js-object-inline-ref n 0)))
		   (%kont* value #f g %this))
		n)))))

;*---------------------------------------------------------------------*/
;*    js-generator-yield* ...                                          */
;*---------------------------------------------------------------------*/
(define (js-generator-yield* gen val done kont %this)

   (define (yield*-generator val %this)
      (with-access::JsGenerator gen (%next)
	 (set! %next js-generator-yield*-loop)
	 (js-generator-set! gen 0 val)
	 (js-generator-set! gen 1 kont))
      (js-generator-yield*-loop (js-undefined) #f gen %this))

   (define (yield*-procedure next val %this js-generator-pcache)
      (define (loop v e g %this)
	 (let* ((n (next val (js-undefined)))
		(done (js-get-name/cache n (& "done") #f %this
			 (vector-ref js-generator-pcache 2))))
	    (if done
		(let ((value (js-get-name/cache n (& "value") #f %this
				(vector-ref js-generator-pcache 1))))
		   (kont value #f g %this))
		n)))
      (with-access::JsGenerator gen (%next)
	 (set! %next loop))
      (loop (js-undefined) #f gen %this))

   (define (yield*-obj next val %this js-generator-pcache)
      (define (loop v e g %this)
	 (let* ((n (js-call0 %this next val))
		(done (js-get-name/cache n (& "done") #f %this
			 (vector-ref js-generator-pcache 2))))
	    (if done
		(let ((value (js-get-name/cache n (& "value") #f %this
				(vector-ref js-generator-pcache 1))))
		   (kont value #f g %this))
		n)))
      (with-access::JsGenerator gen (%next)
	 (set! %next loop))
      (loop (js-undefined) #f gen %this))

   (define (yield* val %this)
      (with-access::JsGlobalObject %this (js-generator-pcache)
	 (if (and (isa? val JsGenerator) (js-object-mode-plain? val))
	     (yield*-generator val %this)
	     (let ((next (js-get-name/cache val (& "next") #f %this
			    (vector-ref js-generator-pcache 0))))
		(if (and (js-procedure? next) (=fx (js-procedure-arity next) 2))
		    (yield*-procedure (js-procedure-procedure next) val %this js-generator-pcache)
		    (yield*-obj next val %this js-generator-pcache))))))

   (with-access::JsGlobalObject %this (js-symbol-iterator)
      (cond
	 ((isa? val JsGenerator)
	  (yield* val %this))
	 ((js-get val js-symbol-iterator %this)
	  =>
	  (lambda (g)
	     (yield* (js-call0 %this g val) %this)))
	 (else
	  (yield* val %this)))))
 
;*---------------------------------------------------------------------*/
;*    js-generator-maybe-next ...                                      */
;*---------------------------------------------------------------------*/
(define (js-generator-maybe-next this val %this cache)
   (if (and (isa? this JsGenerator) (js-object-mode-plain? this))
       (with-access::JsGenerator this (%next)
	  (%next val #f this %this))
       (let ((next (js-get-name/cache this (& "next") #f %this cache)))
	  (js-call1 %this next this val))))

;*---------------------------------------------------------------------*/
;*    js-generator-maybe-next0 ...                                     */
;*---------------------------------------------------------------------*/
(define (js-generator-maybe-next0 this %this cache)
   (if (and (isa? this JsGenerator) (js-object-mode-plain? this))
       (with-access::JsGenerator this (%next)
	  (%next (js-undefined) #f this %this))
       (let ((next (js-get-name/cache this (& "next") #f %this cache)))
	  (js-call0 %this next this))))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
