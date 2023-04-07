;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/function.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 22 06:56:33 2013                          */
;*    Last change :  Fri Apr  7 12:37:34 2023 (serrano)                */
;*    Copyright   :  2013-23 Manuel Serrano                            */
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
   
   (include "types.sch" "stringliteral.sch" "property.sch" "function.sch" "array.sch")
   
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
	   __hopscript_arguments
	   __hopscript_proxy
	   __hopscript_profile)
   
   (export (js-init-function! ::JsGlobalObject)
	   
	   thrower-get
	   thrower-set

	   (js-function-src ::JsProcedureInfo)
	   (js-function-loc ::JsProcedureInfo)
	   (inline js-function-length obj)
	   (inline js-function-new-target? obj)
	   (inline js-function-path ::JsFunction)
	   (js-function-debug-name::bstring ::JsProcedure ::JsGlobalObject)
	   (js-function-arity::long ::obj #!optional opl (protocol 'fix))
	   (js-function-info
	      #!key name len tostring path start end
	      (maxconstrsize 100) new-target)
	   (js-make-function::JsFunction ::JsGlobalObject
	      ::procedure ::int ::vector
	      #!key
	      method constructor construct alloc
	      __proto__ prototype
	      (strict 'normal) (minlen -1)
	      (size 0) (constrsize 3)
	      (constrmap (js-not-a-cmap)) (shared-cmap #t) (clazz #f))
	   (js-make-function-strict::JsFunction ::JsGlobalObject
	      ::procedure ::int ::vector ::long
	      #!key
	      alloc (constrmap (js-not-a-cmap)))
	   (inline js-make-function-strict-lazy::JsFunction
	      ::JsGlobalObject
	      ::procedure ::int ::vector ::long)
	   (js-make-method-strict::JsMethod ::JsGlobalObject
	      ::procedure ::int ::vector ::long ::procedure
	      #!key
	      alloc (constrmap (js-not-a-cmap)))
	   (inline js-make-method-strict-lazy::JsMethod
	      ::JsGlobalObject
	      ::procedure ::int ::vector ::long
	      ::procedure)
	   (inline js-make-procedure::JsProcedure ::JsGlobalObject
	      ::procedure ::int)
	   (js-make-procedure/debug::JsProcedure ::JsGlobalObject
	      ::procedure ::int ::obj)
	   (inline js-make-procedure-hopscript::JsProcedure ::JsGlobalObject
	      ::procedure ::int)
	   (js-make-procedure-hopscript/debug::JsProcedure ::JsGlobalObject
	      ::procedure ::int loc)
	   
	   (inline js-function-prototype-get ::obj ::JsFunction ::obj ::JsGlobalObject)
	   (js-function-maybe-prototype-get ::obj ::obj ::obj ::JsGlobalObject)
	   (js-function-setup-prototype!::JsObject ::JsGlobalObject ::JsFunction)
	   
	   (js-function-apply-vec ::JsGlobalObject ::JsProcedure ::obj ::vector ::uint32)
	   (js-function-apply ::JsGlobalObject ::obj ::obj ::obj ::obj)
	   (js-apply-array ::JsGlobalObject ::obj ::obj ::obj)
	   (js-apply-vec ::JsGlobalObject ::obj ::obj ::vector ::uint32)
	   (js-function-maybe-apply-vec ::JsGlobalObject ::obj ::obj ::vector ::uint32 ::obj)
	   (js-function-maybe-apply ::JsGlobalObject ::obj ::obj ::obj ::obj)
	   (js-function-maybe-call0 ::JsGlobalObject ::obj ::obj ::obj)
	   (js-function-maybe-call1 ::JsGlobalObject ::obj ::obj ::obj ::obj)
	   (js-function-maybe-call2 ::JsGlobalObject ::obj ::obj ::obj ::obj ::obj)
	   (js-function-maybe-call3 ::JsGlobalObject ::obj ::obj ::obj ::obj ::obj ::obj)
	   (js-function-maybe-extend-cmap::JsConstructMap fun cmap::JsConstructMap)))

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
	 (with-access::JsFunction obj (procedure elements)
	    (js-undefined)))))

;*---------------------------------------------------------------------*/
;*    js-function-src ...                                              */
;*---------------------------------------------------------------------*/
(define (js-function-src obj::JsProcedureInfo)
   (with-access::JsProcedureInfo obj (info)
      (match-case info
	 (#(?- ?- (and (? js-jsstring?) ?src) ?- ?- ?- ?- ?-)
	  src)
	 (#(?- ?- (and (? string?) ?src) ?- ?- ?- ?- ?-)
	  (let ((jstr (js-string->jsstring src)))
	     (vector-set! info 2 jstr)
	     jstr))
	 (#(?- ?- #f (and (? string?) ?path) ?start ?end ?- ?-)
	  (let* ((str (read-function-source info path start end))
		 (jstr (js-string->jsstring str)))
	     (vector-set! info 2 jstr)
	     jstr))
	 (#(?- ?- #f ?- ?- ?- ?- ?-)
	  (let ((jstr (js-jsstring-append
			 (js-ascii->jsstring "[function ")
			 (js-jsstring-append
			    (js-string->jsstring (vector-ref info 0))
			    (js-ascii->jsstring "]")))))
	     (vector-set! info 2 jstr)
	     jstr))
	 (else
	  (let ((jstr (js-ascii->jsstring "[Function]")))
	     (vector-set! info 2 jstr)
	     jstr)))))

;*---------------------------------------------------------------------*/
;*    js-function-loc ...                                              */
;*---------------------------------------------------------------------*/
(define (js-function-loc obj::JsProcedureInfo)
   (with-access::JsProcedureInfo obj (info)
      (match-case info
	 (#(?- ?- ?- (and (? string?) ?path) ?start ?end ?- ?-)
	  `(at ,path ,start))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    js-function-path ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (js-function-path obj::JsFunction)
   (with-access::JsFunction obj (info)
      (vector-ref info 3)))

;*---------------------------------------------------------------------*/
;*    js-function-new-length ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (js-function-length obj)
   (with-access::JsProcedureInfo obj (info)
      (vector-ref info 1)))

;*---------------------------------------------------------------------*/
;*    js-function-new-target? ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (js-function-new-target? obj)
   (with-access::JsProcedureInfo obj (info)
      (vector-ref info 7)))

;*---------------------------------------------------------------------*/
;*    js-function-debug-name ...                                       */
;*---------------------------------------------------------------------*/
(define (js-function-debug-name::bstring obj::JsProcedure %this)
   (cond
      ((js-function? obj)
       (with-access::JsFunction obj (info)
	  (vector-ref info 0)))
      ((isa? obj JsProcedureInfo)
       (with-access::JsProcedureInfo obj (info)
	  (vector-ref info 0)))
      (else
       "procedure")))

;*---------------------------------------------------------------------*/
;*    js-debug-object ::JsFunction ...                                 */
;*---------------------------------------------------------------------*/
(define-method (js-debug-object obj::JsFunction #!optional (msg ""))
   (call-next-method)
   (with-access::JsFunction obj (info arity)
      (fprint (current-error-port) "   src=" (js-function-src obj))
      (fprint (current-error-port) "   arity=" arity)
      (fprint (current-error-port) "   path=" (js-function-path obj))))
      
;*---------------------------------------------------------------------*/
;*    js-get-jsobject-name/cache-miss ...                              */
;*---------------------------------------------------------------------*/
(define-method (js-get-jsobject-name/cache-miss o::JsFunction p::obj
		  throw::bool %this::JsGlobalObject
		  cache::JsPropertyCache)
   (if (eq? (js-toname p %this) (& "prototype"))
       (js-function-prototype-get o o p %this)
       (call-next-method)))

;*---------------------------------------------------------------------*/
;*    js-get-jsobject-name/cache-miss ...                              */
;*---------------------------------------------------------------------*/
(define-method (js-get-jsobject-name/cache-miss o::JsProcedure p::obj
		  throw::bool %this::JsGlobalObject
		  cache::JsPropertyCache)
   (let ((n (js-toname p %this)))
      (cond
	 ((eq? n (& "length"))
	  (if (js-function? o)
	      (call-next-method)
	      (js-get-length-jsprocedure o)))
	 ((eq? n (& "prototype"))
	  (js-undefined))
	 ((eq? n (& "name"))
	  (js-string->jsstring (js-function-debug-name o %this)))
	 ((eq? n (& "caller"))
	  (if (isa? o JsFunction)
	      (call-next-method)
	      (js-undefined)))
	 ((eq? n (& "arguments"))
	  (if (isa? o JsFunction)
	      (call-next-method)
	      (js-undefined)))
	 (else
	  (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    js-get ::JsProcedure ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsProcedure p %this)
   (let ((pn (js-toname p %this)))
      (cond
	 ((eq? pn (& "length"))
	  (if (js-function? o)
	      (call-next-method)
	      (js-get-length-jsprocedure o)))
	 ((and (eq? pn (& "prototype"))
	       (eq? (object-class o) JsProcedure))
	  (js-undefined))
	 ((eq? pn (& "name"))
	  (js-string->jsstring (js-function-debug-name o %this)))
	 (else
	  (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    js-get-length ::JsProcedure ...                                  */
;*---------------------------------------------------------------------*/
(define-method (js-get-length o::JsProcedure %this::JsGlobalObject #!optional cache)
   (if (js-function? o)
       (call-next-method)
       (js-get-length-jsprocedure o)))

;*---------------------------------------------------------------------*/
;*    js-get-length-jsprocedure ...                                    */
;*---------------------------------------------------------------------*/
(define (js-get-length-jsprocedure o::JsProcedure)
   (with-access::JsProcedure o (arity)
      (cond
	 ((>fx arity 0) (-fx arity 1))
	 ((>fx arity -1024) (- (negfx arity) 1))
	 ((>fx arity -2049) (+fx arity 2049))
	 ((>fx arity -3049) (+fx arity 3049))
	 ((>fx arity -4049) (+fx arity 4049))
	 (else (+fx arity 5049)))))

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
(define-inline (make-cmap props)
   (js-make-jsconstructmap 
      :methods (make-vector (vector-length props))
      :props props))

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
	 ((@ js-make-pcache-table __hopscript_property) 6 "function"))
      
      (let ((proc (lambda l (js-undefined)))
	    (js-object-prototype (js-object-proto %this)))
	 (set! js-function-prototype
	    (instantiateJsFunction
	       (procedure proc)
	       (cmap (js-make-jsconstructmap))
	       (alloc js-not-a-constructor-alloc)
	       (info (js-function-info :name "" :len 0))
	       (arity (js-function-arity 0 0))
	       (prototype js-object-prototype)
	       (__proto__ js-object-prototype)
	       (elements ($create-vector 10)))))

      (set! js-function-strict-prototype
	 (instantiateJsObject
	    (cmap (js-make-jsconstructmap))
	    (__proto__ js-function-prototype)
	    (elements (make-vector 10 (js-undefined)))))
      
      ;; then, create the properties of the function contructor
      (set! js-function
	 (let ((proc (%js-function %this)))
	    (js-make-function %this
	       proc
	       (js-function-arity proc)
	       (js-function-info :name "Function" :len 1)
	       :alloc js-no-alloc
	       :__proto__ js-function-prototype
	       :prototype js-function-prototype)))
      ;; throwers
      (let* ((throwget (lambda (o)
			  (js-raise-type-error %this
			     "[[ThrowTypeError]] ~a" o)))
	     (throwset (lambda (o v)
			  (js-raise-type-error %this
			     "[[ThrowTypeError]] ~a" o)))
	     (thrower (js-make-function %this
			 (lambda (o v)
			    (tprint "ICI")
			    (js-raise-type-error %this
			       "[[ThrowTypeError]] ~a" o))
			 (js-function-arity 1 0)
			 (js-function-info :name "thrower" :len 1))))
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

      ;; init function allocation
      (with-access::JsGlobalObject %this (js-function 
					    js-function-writable-strict-cmap
					    js-function-prototype-property-rw
					    js-function-strict-elements
					    js-initial-cmap)
	 ($js-init-jsalloc-function (js-not-a-cmap)
	    js-function-writable-strict-cmap
	    js-function-strict-elements js-object-alloc-lazy
	    (js-function-default-mode))
	 ($js-init-jsalloc-method (js-not-a-cmap)
	    js-function-writable-strict-cmap
	    js-function-strict-elements js-object-alloc-lazy
	    (js-function-default-mode))
	 ($js-init-jsalloc-procedure
	    js-initial-cmap
	    (js-procedure-default-mode)))
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
			(with-access::JsFunction obj (info)
			   (vector-ref info 1))))
	       (%set list))
	    (instantiate::JsWrapperDescriptor
	       (name (& "name"))
	       (enumerable #f)
	       (configurable #f)
	       (writable #f)
	       (%get (lambda (obj owner propname %this)
			(with-access::JsFunction obj (info)
			   (js-string->jsstring (vector-ref info 0)))))
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
	 (make-cmap 
	    `#(,(prop (& "prototype") (property-flags #f #f #f #f #f))
	       ,(prop (& "length") (property-flags #f #f #f #f #f))
	       ,(prop (& "name") (property-flags #f #f #t #f #f)))))
      
      (set! js-function-sans-prototype-cmap
	 (make-cmap 
	    `#(,(prop (& "%null") (property-flags #f #f #f #f #f))
	       ,(prop (& "length") (property-flags #f #f #f #f #f))
	       ,(prop (& "name") (property-flags #f #f #t #f #f)))))
      
      (set! js-function-strict-bind-cmap
	 (make-cmap 
	    `#(,(prop (& "%bind") (property-flags #f #f #f #f #f))
	       ,(prop (& "length") (property-flags #f #f #f #f #f))
	       ,(prop (& "name") (property-flags #f #f #t #f #f))
	       ,(prop (& "arguments") (property-flags #f #f #f #f #f))
	       ,(prop (& "caller") (property-flags #f #f #f #f #f)))))

      (set! js-function-strict-cmap
	 (make-cmap 
	    `#(,(prop (& "prototype") (property-flags #f #f #f #f #f))
	       ,(prop (& "length") (property-flags #f #f #f #f #f))
	       ,(prop (& "name") (property-flags #f #f #t #f #f))
	       ,(prop (& "arguments") (property-flags #f #f #f #f #f))
	       ,(prop (& "caller") (property-flags #f #f #f #f #f)))))
      
      (set! js-function-writable-cmap
	 (make-cmap 
	    `#(,(prop (& "prototype") (property-flags #t #f #f #f #f))
	       ,(prop (& "length") (property-flags #f #f #f #f #f))
	       ,(prop (& "name") (property-flags #f #f #t #f #f)))))
      
      (set! js-function-writable-strict-cmap
	 (make-cmap 
	    `#(,(prop (& "prototype") (property-flags #t #f #f #f #f))
	       ,(prop (& "length") (property-flags #f #f #f #f #f))
	       ,(prop (& "name") (property-flags #f #f #t #f #f))
	       ,(prop (& "arguments") (property-flags #f #f #f #f #f))
	       ,(prop (& "caller") (property-flags #f #f #f #f #f)))))
      
      (set! js-function-prototype-cmap
	 (make-cmap 
	    `#(,(prop (& "constructor") (property-flags #t #f #t #f #t)))))))

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
	  (js-make-function %this (lambda (this) (js-undefined))
	     (js-function-arity 0 0)
	     (js-function-info :name "" :len 0)
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
;*    js-make-function ...                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.3.1     */
;*---------------------------------------------------------------------*/
(define (js-make-function %this procedure arity info
	   #!key
	   method constructor construct alloc
	   __proto__ prototype
	   (strict 'normal) (minlen -1) 
	   (size 0) (constrsize 3)
 	   (constrmap (js-not-a-cmap)) (shared-cmap #t)
	   (clazz #f))
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
      (let* ((els ($create-vector (+fx size (if (eq? strict 'normal) 3 5))))
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
	     (fun (cond
		     (method
		      (instantiateJsMethod
			 (procedure procedure)
			 (method method)
			 (alloc (or alloc js-not-a-constructor-alloc))
			 (arity arity)
			 (__proto__ (or __proto__ (js-object-proto js-function)))
			 (info info)
			 (constrsize constrsize)
			 (constrmap constrmap)
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
			 (prototype #\F)))
		     (clazz
		      (instantiateJsClass
			 (procedure procedure)
			 (constructor constructor)
			 (clazz clazz)
			 (alloc (or alloc js-not-a-constructor-alloc))
			 (arity arity)
			 (__proto__ (or __proto__ (js-object-proto js-function)))
			 (info info)
			 (constrsize constrsize)
			 (constrmap constrmap)
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
			 (prototype #\F)))
		     (else
		      (instantiateJsFunction
			 (procedure procedure)
			 (alloc (or alloc js-not-a-constructor-alloc))
			 (arity arity)
			 (__proto__ (or __proto__ (js-object-proto js-function)))
			 (info info)
			 (constrsize constrsize)
			 (constrmap constrmap)
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
			 (prototype #\F))))))
	 ;; adjust the user constrmap
	 (unless (eq? constrmap (js-not-a-cmap))
	    (with-access::JsConstructMap constrmap (ctor)
	       (set! ctor fun)))
	 ;; the prototype property
	 (with-access::JsFunction fun ((fprototype prototype) alloc)
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
		      js-function-prototype-property-rw))
		  ((js-object? prototype)
		   (js-bind! %this prototype (& "constructor")
		      :value fun
		      :configurable #t :enumerable #f :writable #t
		      :hidden-class #t)
		   (set! fprototype prototype)
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
	 (vector-set! els 1 (vector-ref info 1))
	 ;; name
	 (vector-set! els 2 (js-string->jsstring (vector-ref info 0)))
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
(define (js-make-function-strict %this procedure arity info constrsize
	   #!key alloc (constrmap (js-not-a-cmap)))
   (let ((fun (js-make-function-strict-lazy %this procedure arity info constrsize)))
      (with-access::JsFunction fun ((falloc alloc)
				    (fconstrmap constrmap))
	 (set! falloc alloc)
	 (set! fconstrmap constrmap)
	 (js-function-setup-prototype! %this fun)
	 fun)))

;*---------------------------------------------------------------------*/
;*    js-make-function-strict-lazy ...                                 */
;*---------------------------------------------------------------------*/
(define-inline (js-make-function-strict-lazy %this procedure arity info constrsize)
   (with-access::JsGlobalObject %this (js-function)
      ($js-make-jsfunction procedure
	 arity constrsize
	 (js-object-proto js-function)
	 info)))

;*---------------------------------------------------------------------*/
;*    js-make-method-strict ...                                        */
;*    -------------------------------------------------------------    */
;*    specialized method constructor for regular strict methods.       */
;*---------------------------------------------------------------------*/
(define (js-make-method-strict %this procedure
	   arity info constrsize method
	   #!key
	    alloc
	   (constrmap (js-not-a-cmap)))
   (let ((fun (js-make-method-strict-lazy %this procedure arity
		 info constrsize method)))
      (with-access::JsMethod fun ((falloc alloc)
				  (fconstrmap constrmap))
	 (set! falloc alloc)
	 (set! fconstrmap constrmap)
	 (js-function-setup-prototype! %this fun)
	 fun)))

;*---------------------------------------------------------------------*/
;*    js-make-method-strict-lazy ...                                   */
;*---------------------------------------------------------------------*/
(define-inline (js-make-method-strict-lazy %this procedure
		  arity info constrsize method)
   (with-access::JsGlobalObject %this (js-function)
      ($js-make-jsmethod procedure method
	 arity constrsize
	 (js-object-proto js-function)
	 info)))

;*---------------------------------------------------------------------*/
;*    js-make-procedure ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-make-procedure %this procedure arity)
   
   (define ($js-make-procedure proc arity proto)
      ($js-make-jsprocedure proc arity proto))

   (with-access::JsGlobalObject %this (js-function)
      ($js-make-procedure procedure arity (js-object-proto js-function))))

;*---------------------------------------------------------------------*/
;*    js-make-procedure/debug ...                                      */
;*---------------------------------------------------------------------*/
(define (js-make-procedure/debug %this procedure arity info)
   
   (define ($js-make-procedure proc arity loc proto)
      (let ((o (instantiate::JsProcedureInfo
		  (procedure proc)
		  (arity arity)
		  (info info))))
	 (js-object-proto-set! o proto)
	 (js-object-mode-set! o (js-procedure-default-mode))
	 o))

   (with-access::JsGlobalObject %this (js-function)
      ($js-make-procedure procedure arity info (js-object-proto js-function))))

;*---------------------------------------------------------------------*/
;*    js-make-procedure-hopscript ...                                  */
;*---------------------------------------------------------------------*/
(define-inline (js-make-procedure-hopscript %this procedure arity)
   (let ((o (js-make-procedure %this procedure arity)))
      (js-object-mode-set! o (js-procedure-hopscript-mode))
      o))

;*---------------------------------------------------------------------*/
;*    js-make-procedure-hopscript/debug ...                            */
;*---------------------------------------------------------------------*/
(define (js-make-procedure-hopscript/debug %this procedure arity loc)
   (let ((o (js-make-procedure/debug %this procedure arity loc)))
      (js-object-mode-set! o (js-procedure-hopscript-mode))
      o))

;*---------------------------------------------------------------------*/
;*    js-function-setup-prototype! ...                                 */
;*---------------------------------------------------------------------*/
(define (js-function-setup-prototype! %this fun::JsFunction)
   (with-access::JsGlobalObject %this (js-function-prototype-cmap)
      (with-access::JsFunction fun (prototype alloc)
	 (let ((p (instantiateJsObject
		     (cmap js-function-prototype-cmap)
		     (__proto__ (js-object-proto %this))
		     (elements (vector fun)))))
	    (set! prototype p)
	    p))))

;*---------------------------------------------------------------------*/
;*    js-function-prototype-get ...                                    */
;*---------------------------------------------------------------------*/
(define-inline (js-function-prototype-get obj owner::JsFunction propname %this)
   (with-access::JsFunction owner (prototype alloc name src)
      (when (eq? prototype #\F)
	 (js-function-setup-prototype! %this owner))
      prototype))

;*---------------------------------------------------------------------*/
;*    js-function-maybe-prototype-get ...                              */
;*---------------------------------------------------------------------*/
(define (js-function-maybe-prototype-get obj owner propname %this)
   (let loop ((owner owner))
      (cond
	 ((js-function? owner)
	  (js-function-prototype-get obj owner propname %this))
	 ((js-proxy-function? owner)
	  (loop (js-proxy-target owner)))
	 (else
	  (js-raise-type-error %this "prototype: not a function ~s"
	     (js-typeof owner %this))))))

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
	       (set! prototype v)))))
   v)

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
	 ((isa? this JsProcedureInfo)
	  (js-function-src this))
	 ((js-procedure? this)
	  (& "[Function]"))
	 ((js-proxy-function? this)
	  (tostring (js-proxy-target this)))
	 (else
	  (js-raise-type-error %this "toString: not a function ~s"
	     (js-typeof this %this)))))

   (js-bind! %this obj (& "toString")
      :value (js-make-function %this tostring
		(js-function-arity tostring)
		(js-function-info :name "toString" :len 0)
		:prototype (js-undefined))
      :enumerable #f :writable #t :configurable #t
      :hidden-class #t)

   ;; source
   ;; Hop extension
   (define (source this)
      (cond
	 ((js-function? this)
	  (with-access::JsFunction this (info)
	     (when (>=fx (vector-length info) 5)
		(js-string->jsstring
		   (format "~a:~a"
		      (vector-ref info 3)
		      (vector-ref info 4))))))
	 ((js-procedure? this)
	  (js-undefined))
	 (else
	  (js-raise-type-error %this "source: not a function ~s"
	     (js-typeof this %this)))))
   
   (js-bind! %this obj (& "source")
      :value (js-make-function %this source
		(js-function-arity source)
		(js-function-info :name "source" :len 0)
		:prototype (js-undefined))
      :enumerable #f :writable #t :configurable #t
      :hidden-class #t)

   ;; @@hasInstance
   (with-access::JsGlobalObject %this (js-symbol-hasinstance)
      (js-bind! %this obj js-symbol-hasinstance
	 :value (js-make-function %this
		   (lambda (this o) (js-ordinary-instanceof? %this o this))
		   (js-function-arity 1 0)
		   (js-function-info :name "[Symbol.hasInstance]" :len 1)
		   :prototype (js-undefined))
	 :enumerable #f :writable #f :configurable #f
	 :hidden-class #t))
   
   ;; apply
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.3
   (define (prototype-apply this::obj thisarg argarray)
      (js-apply-array %this this thisarg argarray))

   (js-bind! %this obj (& "apply")
      :value (js-make-function %this prototype-apply
		(js-function-arity prototype-apply)
		(js-function-info :name "apply" :len 2)
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
	 (js-make-function %this call
	    (js-function-arity 1 -1 'scheme)
	    (js-function-info :name "call" :len 1)
	    :prototype (js-undefined)))
      (js-bind! %this obj (& "call")
	 :value js-call
	 :enumerable #f :writable #t :configurable #t
	 :hidden-class #t))
   
   ;; bind
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.5
   (define (bind this::obj thisarg . args)
      (if (not (js-function? this))
	  (js-raise-type-error %this "bind: this not a function ~s" this)
	  (with-access::JsFunction this (info construct alloc procedure prototype)
	     (when (eq? prototype 'lazy)
		;; force creating the true prototype before binding
		(js-function-setup-prototype! %this this)
		(set! alloc js-object-alloc))
	     (let* ((bproc (lambda (self . actuals)
			      (if (eq? (js-new-target-pop! %this) (js-undefined))
				  (js-apply %this this
				     thisarg (append args actuals))
				  (js-apply %this this
				     self (append args actuals)))))
		    (bproto (js-getprototypeof this %this "getPrototypeOf"))
		    (balloc (lambda (%this ctor)
			       (js-new-target-push! %this ctor)
			       (alloc %this this))))
		(js-make-function %this bproc
		   (js-function-arity 0 -1 'scheme)
		   (js-function-info
		      :name
		      (string-append "bind:"
			 (js-tostring (js-get this (& "name") %this)
			    %this))
		      :len (maxfx 0 (-fx (vector-ref info 1) (length args))))
		   :__proto__ bproto
		   :prototype 'bind
		   :strict 'strict
		   :alloc balloc)))))

   (js-bind! %this obj (& "bind")
      :value (js-make-function %this bind
		(js-function-arity 1 1 'scheme)
		(js-function-info :name "bind" :len 1)
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
	     ((js-object-mode-arrayinline? argarray)
	      ;; fast path
	      (js-apply-vec %this this thisarg vec ilen))
	     ((js-object-mode-arrayholey? argarray)
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
      ((and (isa? argarray JsArguments) (js-procedure? this))
       (let ((n (js-arguments-length argarray %this)))
	  (case n
	     ((0)
	      (js-call0-jsprocedure %this this thisarg))
	     ((1)
	      (js-call1-jsprocedure %this this thisarg
		 (js-arguments-ref argarray 0 %this)))
	     ((2)
	      (js-call2-jsprocedure %this this thisarg
		 (js-arguments-ref argarray 0 %this)
		 (js-arguments-ref argarray 1 %this)))
	     ((3)
	      (js-call3-jsprocedure %this this thisarg
		 (js-arguments-ref argarray 0 %this)
		 (js-arguments-ref argarray 1 %this)
		 (js-arguments-ref argarray 2 %this)))
	     ((4)
	      (js-call4-jsprocedure %this this thisarg
		 (js-arguments-ref argarray 0 %this)
		 (js-arguments-ref argarray 1 %this)
		 (js-arguments-ref argarray 2 %this)
		 (js-arguments-ref argarray 3 %this)))
	     ((5)
	      (js-call5-jsprocedure %this this thisarg
		 (js-arguments-ref argarray 0 %this)
		 (js-arguments-ref argarray 1 %this)
		 (js-arguments-ref argarray 2 %this)
		 (js-arguments-ref argarray 3 %this)
		 (js-arguments-ref argarray 4 %this)))
	     ((6)
	      (js-call6-jsprocedure %this this thisarg
		 (js-arguments-ref argarray 0 %this)
		 (js-arguments-ref argarray 1 %this)
		 (js-arguments-ref argarray 2 %this)
		 (js-arguments-ref argarray 3 %this)
		 (js-arguments-ref argarray 4 %this)
		 (js-arguments-ref argarray 5 %this)))
	     ((7)
	      (js-call7-jsprocedure %this this thisarg
		 (js-arguments-ref argarray 0 %this)
		 (js-arguments-ref argarray 1 %this)
		 (js-arguments-ref argarray 2 %this)
		 (js-arguments-ref argarray 3 %this)
		 (js-arguments-ref argarray 4 %this)
		 (js-arguments-ref argarray 5 %this)
		 (js-arguments-ref argarray 6 %this)))
	     ((8)
	      (js-call8-jsprocedure %this this thisarg
		 (js-arguments-ref argarray 0 %this)
		 (js-arguments-ref argarray 1 %this)
		 (js-arguments-ref argarray 2 %this)
		 (js-arguments-ref argarray 3 %this)
		 (js-arguments-ref argarray 4 %this)
		 (js-arguments-ref argarray 5 %this)
		 (js-arguments-ref argarray 6 %this)
		 (js-arguments-ref argarray 7 %this)))
	     ((9)
	      (js-call9-jsprocedure %this this thisarg
		 (js-arguments-ref argarray 0 %this)
		 (js-arguments-ref argarray 1 %this)
		 (js-arguments-ref argarray 2 %this)
		 (js-arguments-ref argarray 3 %this)
		 (js-arguments-ref argarray 4 %this)
		 (js-arguments-ref argarray 5 %this)
		 (js-arguments-ref argarray 6 %this)
		 (js-arguments-ref argarray 7 %this)
		 (js-arguments-ref argarray 8 %this)))
	     ((10)
	      (js-call10-jsprocedure %this this
		 thisarg (js-arguments-ref argarray 0 %this)
		 (js-arguments-ref argarray 1 %this)
		 (js-arguments-ref argarray 2 %this)
		 (js-arguments-ref argarray 3 %this)
		 (js-arguments-ref argarray 4 %this)
		 (js-arguments-ref argarray 5 %this)
		 (js-arguments-ref argarray 6 %this)
		 (js-arguments-ref argarray 7 %this)
		 (js-arguments-ref argarray 8 %this)
		 (js-arguments-ref argarray 9 %this)))
	     (else
	      (js-apply %this this thisarg
		 (map! (lambda (idx)
			  (js-arguments-ref argarray idx %this))
		    (iota n)))))))
      ((or (eq? argarray (js-null)) (eq? argarray (js-undefined)))
       (js-call0 %this this thisarg))
      ((not (js-object? argarray))
       (js-raise-type-error %this "apply: argument not an object ~s" argarray))
      (else
       ;; slow path
       (let ((n (uint32->fixnum
		   (js-touint32
		      (js-get argarray (& "length") %this)
		      %this))))
	  ;; assumes here a fixnum length as an iteration over the range
	  ;; 1..2^32-1 is not computable with 2014 computer's performance
	  (js-apply %this this thisarg
	     (map! (lambda (idx)
		      (js-get argarray idx %this))
		(iota n)))))))

;*---------------------------------------------------------------------*/
;*    js-function-apply-vec ...                                        */
;*---------------------------------------------------------------------*/
(define (js-function-apply-vec %this this thisarg vec::vector ilen::uint32)
   (with-access::JsProcedure this (arity procedure)
      (let ((n (uint32->fixnum ilen)))
	 (cond
	    ((>fx arity -2047)
	     (case n
		((0)
		 (js-call0-jsprocedure %this this thisarg))
		((1)
		 (js-call1-jsprocedure %this this thisarg (vector-ref vec 0)))
		((2)
		 (js-call2-jsprocedure %this this thisarg (vector-ref vec 0)
		    (vector-ref vec 1)))
		((3)
		 (js-call3-jsprocedure %this this thisarg (vector-ref vec 0)
		    (vector-ref vec 1) (vector-ref vec 2)))
		((4)
		 (js-call4-jsprocedure %this this thisarg (vector-ref vec 0)
		    (vector-ref vec 1) (vector-ref vec 2) (vector-ref vec 3)))
		((5)
		 (js-call5-jsprocedure %this this thisarg (vector-ref vec 0)
		    (vector-ref vec 1) (vector-ref vec 2) (vector-ref vec 3)
		    (vector-ref vec 4)))
		((6)
		 (js-call6-jsprocedure %this this thisarg (vector-ref vec 0)
		    (vector-ref vec 1) (vector-ref vec 2) (vector-ref vec 3)
		    (vector-ref vec 4) (vector-ref vec 5)))
		((7)
		 (js-call7-jsprocedure %this this thisarg (vector-ref vec 0)
		    (vector-ref vec 1) (vector-ref vec 2) (vector-ref vec 3)
		    (vector-ref vec 4) (vector-ref vec 5) (vector-ref vec 6)))
		((8)
		 (js-call8-jsprocedure %this this thisarg (vector-ref vec 0)
		    (vector-ref vec 1) (vector-ref vec 2) (vector-ref vec 3)
		    (vector-ref vec 4) (vector-ref vec 5) (vector-ref vec 6)
		    (vector-ref vec 7)))
		((9)
		 (js-call9-jsprocedure %this this thisarg (vector-ref vec 0)
		    (vector-ref vec 1) (vector-ref vec 2) (vector-ref vec 3)
		    (vector-ref vec 4) (vector-ref vec 5) (vector-ref vec 6)
		    (vector-ref vec 7) (vector-ref vec 8)))
		((10)
		 (js-call10-jsprocedure %this this thisarg (vector-ref vec 0)
		    (vector-ref vec 1) (vector-ref vec 2) (vector-ref vec 3)
		    (vector-ref vec 4) (vector-ref vec 5) (vector-ref vec 6)
		    (vector-ref vec 7) (vector-ref vec 8) (vector-ref vec 9)))
		(else
		 (js-apply %this this thisarg (vector->sublist vec n)))))
	    ((=fx arity -2048)
	     (procedure thisarg (vector-copy vec 0 n)))
	    ((=fx arity -2047)
	     (vector-shrink! vec n)
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
;*    js-apply-vec ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-apply-vec %this this thisarg vec::vector ilen::uint32)
   (cond
      ((js-procedure? this)
       (js-function-apply-vec %this this thisarg vec ilen))
      ((js-procedure-proxy? this)
       (js-calln %this this thisarg
	  (vector->sublist vec (uint32->fixnum ilen))))
      (else
       (js-raise-type-error %this "apply: argument not a function ~s" this))))

;*---------------------------------------------------------------------*/
;*    js-function-maybe-apply-vec ...                                  */
;*---------------------------------------------------------------------*/
(define (js-function-maybe-apply-vec %this this thisarg vec ilen::uint32 cache)
   (let loop ((this this))
      (cond
	 ((js-procedure? this)
	  (js-function-apply-vec %this this thisarg vec ilen))
	 ((js-object? this)
	  (let ((argarray (js-vector->jsarray (vector-copy vec) %this)))
	     (with-access::JsGlobalObject %this (js-function-pcache)
		(js-call2 %this
		   (js-get-jsobject-name/cache this (& "apply") #f %this
		      (or cache (js-pcache-ref js-function-pcache 3)))
		   this thisarg argarray))))
	 (else
	  (loop (js-toobject %this this))))))

;*---------------------------------------------------------------------*/
;*    js-function-maybe-apply ...                                      */
;*---------------------------------------------------------------------*/
(define (js-function-maybe-apply %this this thisarg argarray cache)
   (let loop ((this this))
      (cond
	 ((js-procedure? this)
	  (js-function-apply %this this thisarg argarray cache))
	 ((js-object? this)
	  (with-access::JsGlobalObject %this (js-function-pcache)
	     (js-method-call-name/cache %this this (& "apply")
		(or cache (js-pcache-ref js-function-pcache 3))
		(js-pcache-ref js-function-pcache 5)
		0
		'(pmap cmap vtable poly)
		'(cmap vtable)
		thisarg argarray)))
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
		(or cache (js-pcache-ref js-function-pcache 4))
		'(imap emap cmap vtable))
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
	 ((js-procedure? this)
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
   (with-access::JsFunction this (info)
      (let loop ((this this))
	 (cond
	    ((js-procedure? this)
	     (if (js-object-mode-plain? this)
		 (js-call1-jsprocedure %this this thisarg arg)
		 (js-function-call1 %this this thisarg arg cache)))
	    ((js-object? this)
	     (with-access::JsGlobalObject %this (js-function-pcache)
		(js-call2 %this
		   (js-get-jsobject-name/cache this (& "call") #f %this
		      (or cache (js-pcache-ref js-function-pcache 5)))
		   this thisarg arg)))
	    (else
	     (loop (js-toobject %this this)))))))

;*---------------------------------------------------------------------*/
;*    js-function-maybe-call2 ...                                      */
;*---------------------------------------------------------------------*/
(define (js-function-maybe-call2 %this this thisarg arg0 arg1 cache)
   (let loop ((this this))
      (cond
	 ((js-procedure? this)
	  (if (js-object-mode-plain? this)
	      (js-call2-jsprocedure %this this thisarg arg0 arg1)
	      (js-function-call2 %this this thisarg arg0 arg1 cache)))
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
	 ((js-procedure? this)
	  (if (js-object-mode-plain? this)
	      (js-call3-jsprocedure %this this thisarg arg0 arg1 arg2)
	      (js-function-call3 %this this thisarg arg0 arg1 arg2 cache)))
	 ((js-object? this)
	  (with-access::JsGlobalObject %this (js-function-pcache)
	     (js-call4 %this
		(js-get-jsobject-name/cache this (& "call") #f %this
		   (or cache (js-pcache-ref js-function-pcache 5)))
		this thisarg arg0 arg1 arg2)))
	 (else
	  (loop (js-toobject %this this))))))

;*---------------------------------------------------------------------*/
;*    read-function-source ...                                         */
;*---------------------------------------------------------------------*/
(define (read-function-source info path start end)
   (if (and (string? path) (file-exists? path))
       (let* ((mmap (open-mmap path :write #f))
	      (s (mmap-substring mmap
		    (fixnum->elong start) (fixnum->elong end))))
	  (close-mmap mmap)
	  s)
       (format "[~a:~a..~a]" path start end)))

;*---------------------------------------------------------------------*/
;*    js-function-info ...                                             */
;*---------------------------------------------------------------------*/
(define (js-function-info #!key name len tostring path start end (maxconstrsize 100) new-target)
   (vector name len tostring path start end maxconstrsize new-target))

;*---------------------------------------------------------------------*/
;*    js-function-arity ...                                            */
;*---------------------------------------------------------------------*/
(define (js-function-arity req #!optional opl (protocol 'fix))
   (cond
      ((procedure? req)
       (procedure-arity req))
      ((not opl)
       (if (eq? protocol 'fix)
	   ((@ js-function-arity __hopscript_function) req 0)
	   (error "js-function-arity"
	      "illegal oplional" opl)))
      ((and (integer? req) (integer? opl))
       (if (null? protocol)
	   (if (=fx opl 0)
	       (+fx req 1)
	       (error "js-function-arity"
		  "illegal optional for fix args"
		  (vector req opl protocol)))
	   (case protocol
	      ((arguments-lazy)
	       -2047)
	      ((arguments-eager)
	       -2048)
	      ((arguments)
	       0)
	      ((rest-lazy)
	       (let ((offset (if (=fx opl 0) 2049 4049)))
		  (negfx (+fx offset (-fx req 1)))))
	      ((rest)
	       (let ((offset (if (=fx opl 0) 3049 5049)))
		  (negfx (+fx offset (-fx req 1)))))
	      ((scheme)
	       (cond
		  ((=fx opl 0)
		   (+fx req 1))
		  ((=fx opl -1)
		   (negfx (+fx (+fx 1 req) 1)))
		  (else
		   (negfx (+fx (+fx 1 req) opl)))))
	      ((scheme-optional)
	       (cond
		  ((and (=fx opl 1) (=fx req 0))
		   -512)
		  (else
		   (error "js-function-arity"
		      "Illegal scheme-optional"
		      (vector req opl protocol)))))
	      ((optional)
	       (if (=fx opl 0)
		   (+fx req 1)
		   (negfx (+fx req 1024))))
	      ((fix)
	       (if (=fx opl 0)
		   (+fx req 1)
		   (error "js-function-arity"
		      "illegal optional for fix args"
		      (vector req opl protocol))))
	      (else
	       (error "js-function-arity"
		  "illegal protocol" (vector req opl protocol))))))
      (else
       (error "js-function-arity"
	  "illegal arity" (vector req opl protocol)))))

;*---------------------------------------------------------------------*/
;*    js-function-maybe-extend-cmap ...                                */
;*    -------------------------------------------------------------    */
;*    This function is used when declaring a class to merge to         */
;*    cmap of the super class and the newly defined class. This is     */
;*    require to handle correctly super class private fields.          */
;*    See js2scheme/scheme-class.scm                                   */
;*---------------------------------------------------------------------*/
(define (js-function-maybe-extend-cmap::JsConstructMap this cmap::JsConstructMap)
   (let loop ((this this))
      (cond
	 ((js-function? this)
	  (with-access::JsFunction this (constrmap)
	     (merge-cmap! cmap constrmap)))
	 ((js-proxy-function? this)
	  (loop (js-proxy-target this)))
	 (else
	  cmap))))
	 
;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)

