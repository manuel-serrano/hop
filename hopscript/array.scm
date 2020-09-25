;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/array.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:41:39 2013                          */
;*    Last change :  Wed Apr 15 18:29:14 2020 (serrano)                */
;*    Copyright   :  2013-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript arrays                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_array
   
   (library hop)
   
   (include "../nodejs/nodejs_debug.sch"
	    "types.sch"
	    "stringliteral.sch"
	    "property.sch")
   
   (extern ($js-make-jsarray::JsArray (::long ::uint32 ::JsConstructMap ::obj ::obj ::uint32)
	      "bgl_make_jsarray")
	   ($js-make-jsarray-sans-init::JsArray (::long ::uint32 ::JsConstructMap ::obj ::uint32)
	      "bgl_make_jsarray_sans_init")
	   ($js-jsarray-shift-builtin::obj (::JsArray)
	      "bgl_jsarray_shift_builtin")
	   ($js-vector-bytesize::long (::long)
	      "bgl_vector_bytesize")
	   ($js-init-vector::vector (::void* ::long ::obj)
              "bgl_init_vector")
	   ($js-init-vector-sans-fill::vector (::void* ::long)
              "bgl_init_vector_sans_fill")
	   ($alloca::void* (::long)
              "alloca"))
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_lib
	   __hopscript_number
	   __hopscript_worker
	   __hopscript_symbol
	   __hopscript_string
	   __hopscript_generator
	   __hopscript_profile
	   __hopscript_names)
   
   (cond-expand
      (profile (import __hopscript_profile)))
   
   (export (js-init-array! ::JsGlobalObject)
	   (inline js-make-vector ::long ::obj)
	   (inline js-vector-stack . args)
	   (inline js-array-mark::long)
	   *JS-ARRAY-MARK*
	   (inline js-array-length::uint32 ::JsArray)
	   (inline js-array-update-length!::long ::JsArray ::long)
	   
	   (inline js-array-vec::vector ::JsArray)
	   (inline js-array-ilen::uint32 ::JsArray)
	   
	   (inline js-array-ref ::JsArray ::obj ::JsGlobalObject)
	   (js-array-ref-ur ::JsArray ::uint32 ::JsGlobalObject)
	   (inline js-array-inl-ref ::JsArray ::obj
	      ::vector ::uint32 ::obj ::JsGlobalObject)
	   (js-array-noindex-ref ::JsArray ::obj ::JsGlobalObject)
	   (inline js-array-index-ref ::JsArray ::uint32 ::JsGlobalObject)
	   (js-array-index-noinl-ref ::JsArray ::uint32 ::JsGlobalObject)
	   (inline js-array-index-inl-ref ::JsArray ::uint32
	      ::vector ::uint32 ::obj ::JsGlobalObject)
	   (inline js-array-fixnum-ref ::JsArray ::long ::JsGlobalObject)
	   (js-array-fixnum-noinl-ref ::JsArray ::long ::JsGlobalObject)
	   (inline js-array-fixnum-inl-ref ::JsArray ::long
	      ::vector ::uint32 ::obj ::JsGlobalObject)
	   (inline js-array-string-ref ::JsArray ::obj ::JsGlobalObject)
	   
	   (inline js-array-set! ::JsArray idx ::obj ::bool ::JsGlobalObject)
	   (inline js-array-inl-set! ::JsArray ::obj ::obj
	      ::vector ::uint32 ::obj ::bool ::JsGlobalObject)
	   (js-array-noindex-set! ::JsArray ::obj ::obj ::bool ::JsGlobalObject)
	   (inline js-array-index-set! ::JsArray ::uint32 ::obj ::bool ::JsGlobalObject)
	   (inline js-array-index-inl-set! ::JsArray ::uint32 ::obj
	      ::vector ::uint32 ::obj ::bool ::JsGlobalObject)
	   (inline js-array-fixnum-set! ::JsArray ::long ::obj
	      ::bool ::JsGlobalObject)
	   (inline js-array-fixnum-inl-set! ::JsArray ::long ::obj
	      ::vector ::uint32 ::obj ::bool ::JsGlobalObject)
	   (inline js-array-string-set! ::JsArray ::obj ::obj
	      ::bool ::JsGlobalObject)
	   
	   (js-get-fixnum ::JsArray ::long ::JsGlobalObject)
	   (js-array-put! ::JsArray p ::obj ::bool ::JsGlobalObject)
	   
	   (js-array-set-ur! ::JsArray ::uint32 ::obj ::bool ::JsGlobalObject)
	   (js-vector->jsarray::JsArray ::vector ::JsGlobalObject)
	   (js-vector->sparse-jsarray::JsArray ::vector ::JsGlobalObject)
	   
	   (js-array-new1::JsArray ::obj ::JsGlobalObject)
	   (js-array-alloc::JsArray ::JsGlobalObject)
	   (js-array-construct::JsArray ::JsGlobalObject ::JsArray ::obj)
	   (js-array-construct1::JsArray ::JsGlobalObject ::JsArray ::obj)
	   (js-array-construct-alloc ::JsGlobalObject ::obj)
	   (js-array-construct-alloc/length ::JsGlobalObject ::obj)
	   (inline js-array-construct-alloc-small::JsArray ::JsGlobalObject ::uint32)
	   (inline js-array-construct-alloc-small-sans-init::JsArray ::JsGlobalObject ::uint32)
	   (inline js-array-construct-alloc/lengthu32::JsArray ::JsGlobalObject ::uint32)
	   (js-array-construct/lengthu32::JsArray ::JsGlobalObject ::JsArray ::uint32)
	   (js-array-construct/length::JsArray ::JsGlobalObject ::JsArray ::obj)
	   (jsarray->list::pair-nil ::JsArray ::JsGlobalObject)
	   (jsarray->vector::vector ::JsArray ::JsGlobalObject)
	   (js-array-concat1 ::JsArray ::JsArray ::JsGlobalObject ::obj)
	   (js-array-maybe-concat1 ::obj ::obj ::JsGlobalObject ::obj)
	   (js-array-fill ::JsArray ::obj ::obj ::obj ::JsGlobalObject ::obj)
	   (js-array-maybe-fill ::obj ::obj ::obj ::obj ::JsGlobalObject ::obj)
	   (js-array-foreach ::JsArray ::obj ::obj ::JsGlobalObject ::obj)
	   (js-array-maybe-foreach ::obj ::obj ::obj ::JsGlobalObject ::obj)
	   (js-array-foreach-procedure ::JsArray ::procedure ::obj ::JsGlobalObject ::obj)
	   (js-array-maybe-foreach-procedure ::obj ::procedure ::obj ::JsGlobalObject ::obj)
	   (js-array-map ::JsArray ::obj ::obj ::JsGlobalObject ::obj)
	   (js-array-maybe-map ::obj ::obj ::obj ::JsGlobalObject ::obj)
	   (js-array-map-procedure ::JsArray ::procedure ::obj ::JsGlobalObject ::obj)
	   (js-array-maybe-map-procedure ::obj ::procedure ::obj ::JsGlobalObject ::obj)
	   (js-array-join ::JsArray ::obj ::JsGlobalObject ::obj)
	   (js-array-maybe-join ::obj ::obj ::JsGlobalObject ::obj)
	   (js-array-maybe-map-join ::obj ::procedure ::obj ::obj ::JsGlobalObject ::obj ::obj)
	   (js-array-push ::JsArray ::obj ::JsGlobalObject ::obj)
	   (js-array-maybe-push ::obj ::obj ::JsGlobalObject ::obj)
	   (js-array-pop ::JsArray ::JsGlobalObject ::obj)
	   (js-array-maybe-pop ::obj ::JsGlobalObject ::obj)
	   (js-array-indexof ::JsArray ::obj ::obj ::JsGlobalObject ::obj)
	   (js-array-prototype-slice ::obj ::obj ::obj ::JsGlobalObject)
	   (js-array-maybe-slice0 ::obj ::JsGlobalObject ::obj)
	   (js-array-maybe-slice1 ::obj ::obj ::JsGlobalObject ::obj)
	   (js-array-maybe-slice2 ::obj ::obj ::obj ::JsGlobalObject ::obj)
	   (js-array-maybe-shift0 ::obj ::JsGlobalObject ::obj)
	   (js-array-sort ::JsArray ::obj ::JsGlobalObject ::obj)
	   (js-array-maybe-sort ::obj ::obj ::JsGlobalObject ::obj)
	   (js-iterator-to-array ::obj ::long ::JsGlobalObject)
	   (js-call-with-stack-vector ::vector ::procedure)
	   
	   (inline js-empty-vector->jsarray::JsArray ::JsGlobalObject)
	   (inline DEFAULT-EMPTY-ARRAY-SIZE::long))
   
   ;; export for bmem profiling
   (export (js-array-alloc-ctor::JsArray ::JsGlobalObject ::JsFunction)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    js-debug-object ::JsArray ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-debug-object obj::JsArray #!optional (msg ""))
   (call-next-method)
   (with-access::JsArray obj (vec ilen length)
      (fprint (current-error-port)
	 " ilen=" ilen " length=" length " vlen=" (vector-length vec)
	 " plain=" (js-object-mode-plain? obj)
	 " a.inl=" (js-array-inlined? obj)
	 " mode.inl=" (js-object-mode-inline? obj)
	 " holey=" (js-object-mode-holey? obj))
      (if (<fx (vector-length vec) 20)
	  (fprint (current-error-port) " vec=" vec)
	  (let ((v (copy-vector vec 20)))
	     (vector-set! v 19 "...")
	     (fprint (current-error-port) " vec=" v)))
      (flush-output-port (current-error-port))))

;*---------------------------------------------------------------------*/
;*    js-toindex ...                                                   */
;*---------------------------------------------------------------------*/
(define-macro (js-toindex p)
   (cond-expand
      ((or bint30 bint32)
       `(cond
	   ((and (fixnum? ,p) (>=fx ,p 0))
	    (fixnum->uint32 ,p))
	   ((uint32? ,p)
	    ,p)
	   ((and (js-jsstring? ,p) (js-jsstring-index? ,p))
	    (with-access::JsStringLiteralIndex ,p (index)
	       index))
	   (else
	    ((@ js-toindex  __hopscript_public) ,p))))
      ((or bint61 bint64)
       `(cond
	   ((and (fixnum? ,p) (>=fx ,p 0) (<fx ,p (-fx (bit-lsh 1 32) 1)))
	    (fixnum->uint32 ,p))
	   ((and (js-jsstring? ,p) (js-jsstring-index? ,p))
	    (with-access::JsStringLiteralIndex ,p (index)
	       index))
	   (else
	    ((@ js-toindex  __hopscript_public) ,p))))
      (else
       `((@ js-toindex  __hopscript_public) ,p))))
       
;*---------------------------------------------------------------------*/
;*    js-make-vector ...                                               */
;*    -------------------------------------------------------------    */
;*    This function is overriden by a macro in array.sch. The          */
;*    overriden macro allocates the vector in the stack as the         */
;*    hopc compiler generates JS-MAKE-VECTOR only for vectors that     */
;*    never escapes their dynamic scope.                               */
;*---------------------------------------------------------------------*/
(define-inline (js-make-vector len init)
   (make-vector len init))

;*---------------------------------------------------------------------*/
;*    js-vector-stack ...                                              */
;*    -------------------------------------------------------------    */
;*    This function is overriden by a macro in array.sch. The          */
;*    overriden macro allocates the vector in the stack as the         */
;*    hopc compiler generates JS-VECTOR only for vectors that          */
;*    never escapes their dynamic scope.                               */
;*---------------------------------------------------------------------*/
(define-inline (js-vector-stack . args)
   (apply vector args))

;*---------------------------------------------------------------------*/
;*    js-create-vector ...                                             */
;*    -------------------------------------------------------------    */
;*    Creator used in this file to avoid duplicating in the source     */
;*    code the default init slot value.                                */
;*---------------------------------------------------------------------*/
(define-inline (js-create-vector len)
   (make-vector len (js-absent)))

;*---------------------------------------------------------------------*/
;*    global parameters                                                */
;*---------------------------------------------------------------------*/
(define-inline (DEFAULT-EMPTY-ARRAY-SIZE) 4)

(define-inline (MAX-EXPANDABLE-ARRAY-SIZE::uint32)
   (bit-lshu32 #u32:1 20))
(define-inline (MAX-EXPANDABLE-ARRAY-SIZE/2::uint32)
      (/u32 (MAX-EXPANDABLE-ARRAY-SIZE) #u32:2))
(define-inline (MAX-EXPANDABLE-ARRAY-SIZE/8::uint32)
   (/u32 (MAX-EXPANDABLE-ARRAY-SIZE) #u32:8))

;*---------------------------------------------------------------------*/
;*    js-isname? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (js-isname?::bool p name::JsStringLiteral %this::JsGlobalObject)
   (or (eq? p name) (eq? (js-toname p %this) name)))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsArray ...                                  */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsArray
   (lambda (o ctx)
      (if (isa? ctx JsGlobalObject)
	  (jsarray->vector o ctx)
	  (error "obj->string ::JsArray" "Not a JavaScript context" ctx)))
   (lambda (o ctx)
      (if (isa? ctx JsGlobalObject)
	  (js-vector->jsarray o ctx)
	  (error "string->obj ::JsArray" "Not a JavaScript context" ctx))))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsArray ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsArray worker %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-array)
	 (with-access::JsArray obj (vec frozen sealed length ilen)
	    (let ((nobj (js-vector->jsarray
			   (vector-map (lambda (e)
					  (js-donate e worker %_this))
			      vec)
			   %this)))
	       (with-access::JsArray nobj ((nlength length) (nilen ilen))
		  (set! nlength length)
		  (set! nilen ilen)
		  (js-object-mode-inline-set! nobj
		     (js-object-mode-inline? obj))
		  (js-object-mode-holey-set! nobj
		     (js-object-mode-holey? obj)))
	       ;; donate the value of the array
	       (js-for-in obj
		  (lambda (k %this)
		     (js-put! nobj (js-donate k worker %_this)
			(js-donate (js-get obj k %_this) worker %_this)
			#f %this))
		  %this)
	       nobj)))))
	    
;*---------------------------------------------------------------------*/
;*    xml-unpack ::JsArray ...                                         */
;*---------------------------------------------------------------------*/
(define-method (xml-unpack obj::JsArray ctx)
   
   (define (subvector->list vec alen::long)
      (if (=fx alen 0)
	  '()
	  (let loop ((i (-fx alen 1))
		     (acc '()))
	     (if (=fx i 0)
		 (cons (vector-ref vec i) acc)
		 (loop (-fx i 1) (cons (vector-ref vec i) acc))))))
   
   (if (isa? ctx JsGlobalObject)
       (with-access::JsArray obj (vec length)
	  (if (js-array-full-inlined? obj)
	      (subvector->list vec (uint32->fixnum length))
	      (let* ((%this ctx)
		     (len::uint32 length))
		 (let loop ((i::uint32 #u32:0))
		    (cond
		       ((=u32 i len)
			'())
		       ((js-has-property obj (js-toname i %this) %this)
			(cons (js-get obj i %this) (loop (+u32 i #u32:1))))
		       (else
			(loop (+u32 i #u32:1))))))))
       (error "xml-unpack ::JsArray" "Not a JavaScript context" ctx)))

;*---------------------------------------------------------------------*/
;*    xml-write ::JsArray ...                                          */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::JsArray p backend)
   (error "xml-write ::JsArray" "Should not be here" (typeof obj)))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsArray ...                                    */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsArray op compile isexpr ctx)
   (js-with-context ctx "hop->javascript"
      (lambda ()
	 (let* ((%this ctx)
		(len::uint32 (js-array-length o)))
	    (if (=u32 len #u32:0)
		(display "sc_vector2array([])" op)
		(begin
		   (display "sc_vector2array([" op)
		   (hop->javascript
		      (js-array-index-ref o #u32:0 %this)
		      op compile isexpr ctx)
		   (let loop ((i #u32:1))
		      (if (=u32 i len)
			  (display "])" op)
			  (let ((n (js-integer-name->jsstring (uint32->fixnum i))))
			     (display "," op)
			     (when (js-has-property o n %this)
				(hop->javascript
				   (js-array-index-ref o i %this)
				   op compile isexpr ctx))
			     (loop (+u32 i #u32:1)))))))))))

;*---------------------------------------------------------------------*/
;*    xml-write-attribute ::JsArray ...                                */
;*---------------------------------------------------------------------*/
(define-method (xml-write-attribute o::JsArray id op backend)
   (error "xml-write-attribute ::JsArray" "should not be here" o))

;*---------------------------------------------------------------------*/
;*    jsarray->list ...                                                */
;*---------------------------------------------------------------------*/
(define (jsarray->list o::JsArray %this)
   (let ((len::uint32 (js-array-length o)))
      (if (=u32 len #u32:0)
	  '()
	  (if (js-array-full-inlined? o)
	      (with-access::JsArray o (vec)
		 (let ((alen::long (uint32->fixnum len)))
		    (let loop ((i (-fx alen 1))
			       (acc '()))
		       (if (=fx i 0)
			   (cons (vector-ref vec i) acc)
			   (loop (-fx i 1) (cons (vector-ref vec i) acc))))))
	      (let loop ((i #u32:0))
		 (cond
		    ((=u32 i len)
		     '())
		    ((js-has-property o (js-toname i %this) %this)
		     (cons (js-get o i %this) (loop (+u32 i #u32:1))))
		    (else
		     (cons (js-undefined) (loop (+u32 i #u32:1))))))))))

;*---------------------------------------------------------------------*/
;*    jsarray->vector ...                                              */
;*---------------------------------------------------------------------*/
(define (jsarray->vector o::JsArray %this)
   (let ((len::uint32 (js-array-length o)))
      (if (=u32 len #u32:0)
	  '#()
	  (let ((res (js-create-vector (uint32->fixnum len))))
	     (let loop ((i #u32:0))
		(cond
		   ((=u32 i len)
		    res)
		   ((js-has-property o (js-toname i %this) %this)
		    (vector-set! res (uint32->fixnum i) (js-get o i %this))
		    (loop (+u32 i #u32:1)))
		   (else
		    (loop (+u32 i #u32:1)))))))))

;*---------------------------------------------------------------------*/
;*    jsarray-fields ...                                               */
;*---------------------------------------------------------------------*/
(define jsarray-fields (vector (find-class-field JsObject 'vec)))

;*---------------------------------------------------------------------*/
;*    javascript-class-all-fields ::JsArray ...                        */
;*    -------------------------------------------------------------    */
;*    JSON serialization, see runtime/json.scm                         */
;*---------------------------------------------------------------------*/
(define-method (javascript-class-all-fields obj::JsArray)
   jsarray-fields)
   
;*---------------------------------------------------------------------*/
;*    js-init-array! ...                                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4         */
;*---------------------------------------------------------------------*/
(define (js-init-array! %this)
   (with-access::JsGlobalObject %this (js-array js-array-prototype
					 js-function js-array-cmap
					 js-array-pcache)
      
      ;; local constant strings
      (unless (vector? __js_strings) (set! __js_strings (&init!)))
      
      ;; array pcache
      (set! js-array-pcache
	 ((@ js-make-pcache-table __hopscript_property) 21 "array"))
      (js-validate-pmap-pcache!
	 (js-pcache-ref js-array-pcache 20))
      
      ;; default arrays cmap
      (set! js-array-cmap
	 (instantiate::JsConstructMap
	    (methods '#())
	    (props '#())))
      
      ;; builtin prototype
      (set! js-array-prototype
	 (instantiateJsArray
	    (vec '#())
	    (__proto__ (js-object-proto %this))
	    (cmap (js-not-a-cmap))
	    (elements (vector
			 ;; cannot be defined with js-bind! because
			 ;; of bootstrap specificities
			 (instantiate::JsValueDescriptor
			    (name (& "length"))
			    (value 0)
			    (enumerable #f)
			    (configurable #f)
			    (writable #t))))))
      (js-object-mode-enumerable-set! js-array-prototype #f)
      (js-object-mode-hasnumeralprop-set! js-array-prototype #f)
      
      ;; DO NOT REMOVE !
      ;; when array are ready for caching this should replace
      ;; the old definition of js-array-prototype
      ;; (set! js-array-prototype
      ;; (instantiateJsArray
      ;; (vec '#())
      ;; (__proto__ __proto__)
      ;; (elements '#(0))
      ;; (cmap (js-descriptors->cmap
      ;; (vector
      ;; (instantiate::JsIndexDescriptor
      ;; (name 'length)
      ;; (configurable #f)
      ;; (writable #t)))
      ;; '#(#unspecified))))
      
      ;; create the array object constructor
      (let ((proc (%js-array %this)))
	 (set! js-array
	    (js-make-function %this proc
	       (js-function-arity proc)
	       (js-function-info :name  "Array" :len 1)
	       :__proto__ (js-object-proto js-function)
	       :prototype js-array-prototype
	       :size 17
	       :alloc js-array-alloc-ctor)))
      
      ;; other properties of the Array constructor
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.5.1
      (js-bind! %this js-array (& "isArray")
	 :value (js-make-function %this
		   (lambda (this arg)
		      (or (js-array? arg) (js-proxy-array? arg)))
		   (js-function-arity 1 0)
		   (js-function-info :name "isArray" :len 1))
	 :writable #t
	 :enumerable #f
	 :hidden-class #t)
      
      ;; from
      ;; http://www.ecma-international.org/ecma-262/6.0/#sec-22.1.2.1
      (define (array-from this::obj arr mapfn T)
	 ;; see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/from
	 ;; 1. Let C be the this value.
	 (let ((C this)
	       ;; 2. Let items be ToObject(arrayLike).
	       (items (js-toobject %this arr)))
	    ;; 3. ReturnIfAbrupt(items).
	    (when (or (eq? arr (js-undefined)) (eq? arr '()))
	       (js-raise-type-error %this
		  "from requires an array-like object - not null or undefined"
		  arr))
	    ;; 4. If mapfn is undefined, then let mapping be false.
	    ;; 5. a If IsCallable(mapfn) is false, throw a TypeError exception.
	    (when (and (not (eq? mapfn (js-undefined)))
		       (not (js-procedure? mapfn)))
	       (js-raise-type-error %this
		  "Array.from: when provided, the second argument must be a function"
		  mapfn))
	    ;; 5. b. If thisArg was supplied, let T be thisArg;
	    ;; else let T be undefined.
	    ;;  10. Let lenValue be Get(items, "length").
	    ;; 11. Let len be ToLength(lenValue).
	    (let ((len (js-get-length items %this)))
	       ;; 13. If IsConstructor(C) is true, then
	       ;; 13. a. Let A be the result of calling the [[Construct]]
	       ;;     internal method of C with an argument list containing
	       ;;     the single item len.
	       ;; 14. a. Else, Let A be ArrayCreate(len).
	       (let ((A (if (js-function? C)
			    (js-toobject %this (js-new1 %this C len))
			    (js-species->jsarray this (js-create-vector len) %this))))
		  ;; 16. Let k be 0.
		  ;; 17. Repeat, while k < len... (also steps a - h)
		  (let loop ((k 0))
		     (when (<fx k len)
			(let ((kvalue (js-get items k %this)))
			   (if (eq? mapfn (js-undefined))
			       (js-put! A k kvalue #f %this)
			       (let ((v (js-call2 %this mapfn T kvalue k)))
				  (js-put! A k v #f %this)))
			   (loop (+fx k 1)))))
		  (js-put-length! A len #f #f %this)
		  A))))
      
      (js-bind! %this js-array (& "from")
	 :value (js-make-function %this array-from
		   (js-function-arity array-from)
		   (js-function-info :name "from" :len 0)
		   :prototype (js-undefined))
	 :enumerable #f
	 :hidden-class #t)
      
      ;; of
      ;; https://www.ecma-international.org/ecma-262/6.0/#sec-array.of
      (define (array-of this::obj . items)
	 (with-access::JsGlobalObject %this (js-array)
	    (if (and (not (eq? this js-array)) (js-function? this))
		(let ((arr (js-new1 %this this 0)))
		   (let loop ((i 0)
			      (is items))
		      (if (null? is)
			  arr
			  (begin
			     (js-put! arr (js-toname i %this) (car is) #f %this)
			     (loop (+fx i 1) (cdr items))))))
		(js-vector->jsarray (list->vector items) %this))))
      
      (js-bind! %this js-array (& "of")
	 :value (js-make-function %this array-of
		   (js-function-arity array-of)
		   (js-function-info :name "of" :len 0)
		   :prototype (js-undefined))
	 :enumerable #f
	 :hidden-class #t)
      
      ;; init the prototype properties
      (init-builtin-array-prototype! %this js-array js-array-prototype)
      
      ;; bind Array in the global object
      (js-bind! %this %this (& "Array")
	 :configurable #f :enumerable #f :value js-array
	 :hidden-class #t)
      
      ;; @@species
      ;; www.ecma-international.org/ecma-262/6.0/#sec-get-array-@@species
      (with-access::JsGlobalObject %this (js-symbol-species)
	 (js-bind! %this js-array js-symbol-species
	    :get (js-make-function %this (lambda (this) this)
		    (js-function-arity 0 0)
		    (js-function-info :name "get [Symbol.species]" :len 0))
	    :enumerable #f
	    :configurable #t))
      
      js-array))

;*---------------------------------------------------------------------*/
;*    *JS-ARRAY-MARK* ...                                              */
;*---------------------------------------------------------------------*/
(define *JS-ARRAY-MARK* 0)

;*---------------------------------------------------------------------*/
;*    js-array-mark ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-array-mark)
   *JS-ARRAY-MARK*)

;*---------------------------------------------------------------------*/
;*    js-array-mark-invalidate! ...                                    */
;*---------------------------------------------------------------------*/
(define-inline (js-array-mark-invalidate!)
   (set! *JS-ARRAY-MARK* (+fx 1 *JS-ARRAY-MARK*)))

;*---------------------------------------------------------------------*/
;*    js-array-length ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (js-array-length arr::JsArray)
   (with-access::JsArray arr (length)
      length))

;*---------------------------------------------------------------------*/
;*    js-get-length ::JsArray ...                                      */
;*---------------------------------------------------------------------*/
(define-method (js-get-length arr::JsArray %this #!optional cache)
   (js-uint32-tointeger (js-array-length arr)))

;*---------------------------------------------------------------------*/
;*    js-array-inlined? ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-array-inlined? arr::JsArray)
   (with-access::JsArray arr (vec length ilen)
      (or (>u32 ilen #u32:0) (=u32 length #u32:0))))

;*---------------------------------------------------------------------*/
;*    js-array-full-inlined? ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (js-array-full-inlined? arr::JsArray)
   (js-object-mode-inline? arr))

;*---------------------------------------------------------------------*/
;*    %assert-array! ...                                               */
;*---------------------------------------------------------------------*/
(define-macro (%assert-array! o loc)
   (if (>= (bigloo-compiler-debug) 1)
       (let ((v (gensym)))
	  `(let ((,v ,o)
		 (%loc ,loc))
	      (assert (%loc o)
		 (with-access::JsArray ,v (vec length ilen)
		    (and (<=u32 ilen (fixnum->uint32 (vector-length vec)))
			 (<=u32 ilen length))))
	      ,v))
       o))

;*---------------------------------------------------------------------*/
;*    js-array-update-ilen! ...                                        */
;*    -------------------------------------------------------------    */
;*    This function is called when a inline/holey array is added       */
;*    or deleted an element to compute its largest ILEN and to         */
;*    re-inline the array is ILEN becomes equal to LENGTH.             */
;*---------------------------------------------------------------------*/
(define (js-array-update-ilen! arr::JsArray start::uint32 end::uint32)
   (with-access::JsArray arr (ilen vec length)
      (let loop ((j start))
	 (cond
	    ((<=u32 j end)
	     (if (js-absent? (u32vref vec j))
		 (set! ilen j)
		 (loop (+u32 j #u32:1))))
	    ((=u32 j length)
	     (set! ilen length)
	     (js-object-mode-inline-set! arr #t))
	    (else
	     (set! ilen (+u32 end 1)))))))

;*---------------------------------------------------------------------*/
;*    js-array-find-length-property ...                                */
;*---------------------------------------------------------------------*/
(define (js-array-find-length-property arr::JsArray)
   (with-access::JsArray arr (elements)
      (when (>=fx (vector-length elements) 1)
	 (when (isa? (vector-ref elements 0) JsPropertyDescriptor)
	    (with-access::JsPropertyDescriptor (vector-ref elements 0) (name)
	       (when (eq? name (& "length"))
		  (vector-ref elements 0)))))))

;*---------------------------------------------------------------------*/
;*    js-array-update-length-property! ...                             */
;*---------------------------------------------------------------------*/
(define (js-array-update-length-property! arr::JsArray)
   
   (define (add-length-property! arr::JsArray)
      (with-access::JsObject arr (elements)
	 (let ((prop (instantiate::JsValueDescriptor
			(name (& "length"))
			(value (js-uint32-tointeger (js-array-length arr)))
			(configurable #f)
			(writable #t)))
	       (vec (make-vector (+fx 1 (vector-length elements)))))
	    (vector-set! vec 0 prop)
	    (vector-copy! vec 1 elements)
	    (set! elements vec)
	    prop)))
   
   (let ((desc (js-array-find-length-property arr)))
      (if desc
	  (with-access::JsValueDescriptor desc (value name)
	     (set! value (js-uint32-tointeger (js-array-length arr)))
	     desc)
	  (add-length-property! arr))))

;*---------------------------------------------------------------------*/
;*    js-array-update-length! ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (js-array-update-length!::long arr::JsArray nlen::long)
   (with-access::JsArray arr (length)
      (set! length (fixnum->uint32 nlen))
      nlen))

;*---------------------------------------------------------------------*/
;*    js-array-vec ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (js-array-vec::vector a::JsArray)
   (with-access::JsArray a (vec) vec))

;*---------------------------------------------------------------------*/
;*    js-array-ilen ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-array-ilen::uint32 a::JsArray)
   (with-access::JsArray a (ilen) ilen))

;*---------------------------------------------------------------------*/
;*    copy-vector-fill! ...                                            */
;*---------------------------------------------------------------------*/
(define (copy-vector-fill! vec::vector nlen::long fill)
   (let ((new ($create-vector nlen))
	 (olen (vector-length vec)))
      (let loop ((i 0))
	 (if (=fx i olen)
	     (begin
		(vector-fill! new fill i)
		new)
	     (begin
		(vector-set! new i (vector-ref vec i))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    js-array-ref ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (js-array-ref arr::JsArray idx %this)
   (cond-expand
      ((or bint30 bint32)
       (if (and (fixnum? idx) (>=fx idx 0))
	   (js-array-index-ref arr (fixnum->uint32 idx) %this)
	   (js-array-noindex-ref arr idx %this)))
      (else
       (if (fixnum? idx)
	   (js-array-fixnum-ref arr idx %this)
	   (js-array-noindex-ref arr idx %this)))))

;*---------------------------------------------------------------------*/
;*    js-array-inl-ref ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (js-array-inl-ref arr::JsArray idx::obj
		  avec::vector alen::uint32 mark::obj %this::JsGlobalObject)
   (if (and (fixnum? idx)
 	    (<u32 (fixnum->uint32 idx) alen)
	    (eq? mark (js-array-mark)))
       (vector-ref avec idx)
       (js-array-ref arr (fixnum->uint32 idx) %this)))
   
;*---------------------------------------------------------------------*/
;*    js-array-noindex-ref ...                                         */
;*---------------------------------------------------------------------*/
(define (js-array-noindex-ref arr::JsArray prop::obj %this)
   (with-access::JsArray arr (vec ilen length)
      (let ((i::uint32 (js-toindex prop)))
	 (if (<u32 i ilen)
	     (u32vref vec i)
	     (js-get arr prop %this)))))

;*---------------------------------------------------------------------*/
;*    js-array-index-ref ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-array-index-ref arr::JsArray idx::uint32 %this)
   (with-access::JsArray arr (vec ilen)
      (if (<u32 idx ilen)
	  (vector-ref vec (uint32->fixnum idx))
	  (js-array-index-noinl-ref arr idx %this))))

;*---------------------------------------------------------------------*/
;*    js-array-index-noinl-ref ...                                     */
;*---------------------------------------------------------------------*/
(define (js-array-index-noinl-ref arr::JsArray idx::uint32 %this)
   (with-access::JsArray arr (vec ilen)
      (cond
	 ((and (js-object-mode-holey? arr)
	       (<u32 idx (fixnum->uint32 (vector-length vec))))
	  (let ((v (vector-ref vec (uint32->fixnum idx))))
	     (if (js-absent? v)
		 (js-get-fixnum arr (uint32->fixnum idx) %this)
		 v)))
	 (else
	  (cond-expand
	     ((or bint30 bint32)
	      (js-get arr (js-uint32-tointeger idx) %this))
	     (else
	      (js-get-fixnum arr (uint32->fixnum idx) %this)))))))

;*---------------------------------------------------------------------*/
;*    js-array-index-inl-ref ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (js-array-index-inl-ref arr::JsArray idx::uint32
		  avec::vector alen::uint32 mark::obj %this::JsGlobalObject)
   (if (and (<u32 idx alen) (eq? mark (js-array-mark)))
       (vector-ref avec (uint32->fixnum idx))
       (js-array-index-ref arr idx %this)))
   
;*---------------------------------------------------------------------*/
;*    js-array-fixnum-ref ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-array-fixnum-ref arr::JsArray idx::long %this)
   (with-access::JsArray arr (vec ilen)
      (if (cond-expand
	     ((or bint30 bint32)
	      (<u32 (fixnum->uint32 idx) ilen))
	     (else
	      (pragma::bool "(unsigned long)($1) < (unsigned long)($2)" idx ilen)))
	  (vector-ref vec idx)
	  (js-array-fixnum-noinl-ref arr idx %this))))

;*---------------------------------------------------------------------*/
;*    js-array-fixnum-noinl-ref ...                                    */
;*---------------------------------------------------------------------*/
(define (js-array-fixnum-noinl-ref arr::JsArray idx::long %this)
   (with-access::JsArray arr (vec ilen)
      (cond
	 ((and (js-object-mode-holey? arr)
	       (cond-expand
		  ((or bint30 bint32)
		   (<u32 (fixnum->uint32 idx) (fixnum->uint32 (vector-length vec))))
		  (else
		   (pragma::bool "(unsigned long)($1) < (unsigned long)($2)" idx (vector-length vec)))))
	  (let ((v (vector-ref vec idx)))
	     (if (js-absent? v)
		 (js-get-fixnum arr idx %this)
		 v)))
	 (else
	  (js-get-fixnum arr idx %this)))))
   
;*---------------------------------------------------------------------*/
;*    js-array-fixnum-inl-ref ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (js-array-fixnum-inl-ref arr::JsArray idx::long
		  avec::vector alen::uint32 mark::obj %this::JsGlobalObject)
   (if (and (>=fx idx 0)
	    (<u32 (fixnum->uint32 idx) alen)
	    (eq? mark (js-array-mark)))
       (vector-ref avec idx)
       (js-array-ref arr (fixnum->uint32 idx) %this)))
   
;*---------------------------------------------------------------------*/
;*    js-array-string-ref ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-array-string-ref arr::JsArray idx::obj %this)
   (let* ((s (js-jsstring->string idx))
	  (n (string->integer s)))
      (if (or (>fx n 0) (eq? idx (js-integer->jsstring 0)))
	  (js-array-fixnum-ref arr n %this)
	  (js-get arr idx %this))))
   
;*---------------------------------------------------------------------*/
;*    js-array-ref-ur ...                                              */
;*---------------------------------------------------------------------*/
(define (js-array-ref-ur arr::JsArray idx::uint32 %this)
   (with-access::JsArray arr (vec ilen)
      (cond
	 ((<u32 idx ilen)
	  (vector-ref vec (uint32->fixnum idx)))
	 ((<u32 idx (fixnum->uint32 (vector-length vec)))
	  (if (js-object-mode-holey? arr)
	      (let ((v (u32vref vec idx)))
		 (if (js-absent? v)
		     (js-get-fixnum arr (uint32->fixnum idx) %this)
		     v))))
	 ((cond-expand
	     ((or bint30 bint32)
	      (expandable-array arr idx
		 (fixnum->uint32 (vector-length vec))))
	     (else
	      (and (<u32 idx (fixnum->uint32 (-fx (bit-lsh 1 32) 1)))
		   (expandable-array arr idx
		      (fixnum->uint32 (vector-length vec))))))
	  =>
	  (lambda (len)
	     (let ((nlen (uint32->fixnum len)))
		(cond-expand (profile (profile-vector-extension nlen len)))
		(let ((nvec (copy-vector-fill! vec nlen (js-undefined))))
		   (when ($jsobject-vector-inline? arr)
		      (vector-fill! vec #unspecified))
		   (set! vec nvec)))
	     (cond-expand
		((or bint30 bint32)
		 (js-get arr (js-uint32-tointeger idx) %this))
		(else
		 (js-get-fixnum arr (uint32->fixnum idx) %this)))))
	 (else
	  (cond-expand
	     ((or bint30 bint32)
	      (js-get arr (js-uint32-tointeger idx) %this))
	     (else
	      (js-get-fixnum arr (uint32->fixnum idx) %this)))))))
   
;*---------------------------------------------------------------------*/
;*    js-array-inl-set! ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-array-inl-set! arr::JsArray idx::obj val
		  avec::vector alen::uint32 mark::obj
		  throw::bool %this::JsGlobalObject)
   (if (and (fixnum? idx)
	    ;; (>=fx idx 0)
	    (<u32 (fixnum->uint32 idx) alen)
	    (eq? mark (js-array-mark)))
       (vector-set! avec idx val)
       (js-array-index-set! arr (fixnum->uint32 idx) val throw %this)))
    
;*---------------------------------------------------------------------*/
;*    js-array-index-inl-set! ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (js-array-index-inl-set! arr::JsArray idx::uint32 val
		  avec::vector alen::uint32 mark::obj throw %this::JsGlobalObject)
   (if (and (<u32 idx alen) (eq? mark (js-array-mark)))
       (vector-set! avec (uint32->fixnum idx) val)
       (js-array-index-set! arr idx val throw %this)))
   
;*---------------------------------------------------------------------*/
;*    js-array-fixnum-inl-set! ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (js-array-fixnum-inl-set! arr::JsArray idx::long val
		  avec::vector alen::uint32 mark::obj
		  throw::bool %this::JsGlobalObject)
   (if (and (<u32 (fixnum->uint32 idx) alen) (eq? mark (js-array-mark)))
       (vector-set! avec idx val)
       (js-array-index-set! arr (fixnum->uint32 idx) val throw %this)))
   
;*---------------------------------------------------------------------*/
;*    js-array-string-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (js-array-string-set! arr::JsArray idx::obj val throw %this)
   (with-access::JsArray arr (vec ilen)
      (let* ((s (js-jsstring->string idx))
	     (n (string->integer s)))
	 (if (or (>fx n 0) (eq? idx (js-integer->jsstring 0)))
	     (js-array-fixnum-set! arr n val throw %this)
	     (js-array-put! arr idx val throw %this)))))
      
;*---------------------------------------------------------------------*/
;*    js-array-set! ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-array-set! arr::JsArray idx val throw %this)
   (if (fixnum? idx)
       (js-array-fixnum-set! arr idx val throw %this)
       (js-array-put! arr idx val throw %this)))

;*---------------------------------------------------------------------*/
;*    js-array-fixnum-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (js-array-fixnum-set! arr::JsArray idx::long val throw %this)
   (with-access::JsArray arr (vec ilen)
      (cond
	 ((cond-expand
	     ((or bint30 bint32) (<u32 (fixnum->uint32 idx) ilen))
	     (else (pragma::bool "(unsigned long)($1) < (unsigned long)($2)" idx ilen)))
	  (vector-set! vec idx val)
	  val)
	 ((cond-expand
	     ((or bint30 bint32)
	      (<u32 (fixnum->uint32 idx) (bit-lshu32 #u32:1 29)))
	     (else
	      (pragma::bool "(unsigned long)($1) < (unsigned long)(1<<29)" idx)))
	  (js-array-set-ur! arr (fixnum->uint32 idx) val throw %this))
	 (else
	  (js-array-put! arr idx val throw %this)))))
   
;*---------------------------------------------------------------------*/
;*    js-array-noindex-set! ...                                        */
;*---------------------------------------------------------------------*/
(define (js-array-noindex-set! arr::JsArray p::obj val throw %this)
   (with-access::JsArray arr (vec ilen length)
      (let ((idx::uint32 (js-toindex p)))
	 (if (<u32 idx ilen)
	     (begin
		(vector-set! vec (uint32->fixnum idx) val)
		val)
	     (js-array-put! arr p val throw %this)))))

;*---------------------------------------------------------------------*/
;*    js-array-index-set! ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-array-index-set! arr::JsArray idx::uint32 val throw %this)
   (with-access::JsArray arr (vec ilen)
      (cond
	 ((<u32 idx ilen)
	  (vector-set! vec (uint32->fixnum idx) val)
	  val)
	 (else
	  (js-array-set-ur! arr idx val throw %this)))))
   
;*---------------------------------------------------------------------*/
;*    js-array-set-ur! ...                                             */
;*---------------------------------------------------------------------*/
(define (js-array-set-ur! arr::JsArray idx::uint32 val throw::bool %this)
   (with-access::JsArray arr (vec ilen length)
      (cond
	 ((and (<u32 idx (fixnum->uint32 (vector-length vec)))
	       (<u32 idx length))
	  ;; MS 2019-01-28, assert: ilen >= idx < (vector-length vec)
	  (cond
	     ((js-has-fixnum-property arr (js-uint32-tointeger idx) %this)
	      (js-array-put! arr (js-uint32-tointeger idx) val throw %this))
	     ((and (=u32 idx ilen) (js-object-mode-inline? arr))
	      (vector-set! vec (uint32->fixnum idx) val)
	      (let ((nilen (+u32 ilen #u32:1)))
		 (set! ilen nilen)
		 (when (>=u32 idx length)
		    (set! length nilen)))
	      val)
	     ((js-object-mode-holey? arr)
	      (vector-set! vec (uint32->fixnum idx) val)
	      (cond
		 ((>=u32 idx length)
		  (js-object-mode-inline-set! arr #f)
		  (set! length (+u32 idx #u32:1)))
		 ((=u32 idx ilen)
		  ;; update ilen and check inliness again
		  (let ((len (-fx (vector-length vec) 1)))
		     (js-array-update-ilen! arr ilen (fixnum->uint32 len)))
		  (js-object-mode-inline-set! arr (=u32 ilen length)))
		 (else
		  (js-object-mode-inline-set! arr #f)))
	      val)
	     (else
	      (js-array-put! arr (js-uint32-tointeger idx) val throw %this))))
	 (else
	  (js-array-put! arr (js-uint32-tointeger idx) val throw %this)))))
   
;*---------------------------------------------------------------------*/
;*    init-builtin-array-prototype! ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.3       */
;*---------------------------------------------------------------------*/
(define (init-builtin-array-prototype! %this js-array js-array-prototype)

   ;; constructor
   (js-bind! %this js-array-prototype (& "constructor")
      :value js-array :enumerable #f
      :hidden-class #t)
   
   ;; tostring
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.2
   (define (array-prototype-tostring this::obj)
      (let* ((o (js-toobject %this this))
	     (func (js-get this (& "join") %this)))
	 (if (js-procedure? func)
	     (js-call1 %this func this (js-undefined))
	     (js-tojsstring this %this))))
   
   (js-bind! %this js-array-prototype (& "toString")
      :value (js-make-function %this array-prototype-tostring
		(js-function-arity array-prototype-tostring)
		(js-function-info :name "toString" :len 0)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; tolocaleString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.3
   (define (array-prototype-tolocalestring this::obj)
      
      (define (el->string el)
	 (if (or (eq? el (js-undefined)) (eq? el (js-null)))
	     ""
	     (let ((obj (js-toobject %this el)))
		;; MS CARE: I'm not sure that the conversion js-tojsstring
		;; is %this correct as I don't see where it is
		;; demanded by the spec
		(js-tostring
		   (js-call0 %this (js-get obj (& "toLocaleString") %this) obj)
		   %this))))
      
      (let* ((o (js-toobject %this this))
	     (lenval::uint32 (js-get-lengthu32 o %this)))
	 (if (=u32 lenval #u32:0)
	     (js-ascii->jsstring "")
	     (let* ((sep ",")
		    (el0 (el->string (js-get o 0 %this))))
		(let loop ((r (list el0))
			   (i 1))
		   (if (=u32 i lenval)
		       (js-stringlist->jsstring (reverse! r))
		       (loop (cons* (el->string (js-get o (uint32->fixnum i) %this))
				sep r)
			  (+u32 i #u32:1))))))))
   
   (js-bind! %this js-array-prototype (& "toLocaleString")
      :value (js-make-function %this array-prototype-tolocalestring
		(js-function-arity array-prototype-tolocalestring)
		(js-function-info :name "toLocaleString" :len 0)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; concat
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.4
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-array.prototype.concat
   (define (array-prototype-concat this::obj . l)
      
      (define (copy-array-slow target tstart src sstart send)
	 (js-object-mode-inline-set! target #f)
	 ;; slow copy, elements by elements
	 (let loop ((i sstart)
		    (j tstart))
	    (if (= i send)
		j
		(let ((d (js-get-property src i %this)))
		   (if (eq? d (js-undefined))
		       (js-object-mode-holey-set! target #t)
		       (js-define-own-property target (js-toname j %this)
			  d #f %this))
		   (loop (+ i 1) (+ j 1))))))
      
      (define (copy-array src dst i)
	 (if (and (js-object-mode-inline? src)
		  (js-object-mode-inline? dst))
	     (with-access::JsArray src ((vsrc vec))
		(with-access::JsArray dst ((vdst vec) ilen)
		   ;; try to use a vector copy
		   (if (and (>fx (vector-length vdst) 0)
			    (>fx (vector-length vsrc) 0))
		       (let* ((lsrc (vector-length vsrc))
			      (slen (js-get-length src %this))
			      (alen (minfx slen lsrc)))
			  ;; fast vector-copy
			  (when (>fx (+fx i alen) (vector-length vdst))
			     (set! vdst
				(copy-vector vdst (*fx (+fx i alen) 2))))
			  (vector-copy! vdst i vsrc 0 alen)
			  (set! ilen (fixnum->uint32 (+fx i alen)))
			  (if (> slen lsrc)
			      (copy-array-slow dst (+fx i lsrc) src lsrc slen)
			      (+fx i alen)))
		       ;; slow copy
		       (copy-array-slow dst i src 0 (js-get-length src %this)))))
	     (copy-array-slow dst i src 0 (js-get-length src %this))))

      (define (copy-proxy src dst i)
	 (copy-array-slow dst i src 0 (js-get-length src %this)))
      
      (let* ((o (js-toobject %this this))
	     (l (cons o l))
	     (new-len (let loop ((l l)
				 (len 0))
			 (cond
			    ((null? l)
			     len)
			    ((or (js-array? (car l)) (js-proxy? (car l)))
			     (loop (cdr l)
				(+fx/overflow len
				   (js-get-length (car l) %this))))
			    (else
			     (loop (cdr l) (+ 1 len))))))
	     (arr (js-array-species-create %this o new-len)))
	 (if (and (js-object-mode-inline? arr)
		  (every (lambda (o)
			    (and (js-array? o) (js-object-mode-inline? o)))
		     l))
	     ;; super fast copy
	     (with-access::JsArray arr (vec ilen length)
		(let ((vdst vec))
		   (let loop ((l l)
			      (i 0))
		      (if (null? l)
			  (begin
			     (set! ilen (fixnum->uint32 new-len))
			     (set! length (fixnum->uint32 new-len))
			     arr)
			  (with-access::JsArray (car l) (vec ilen)
			     (vector-copy! vdst i vec 0 ilen)
			     (loop (cdr l) (+fx i (uint32->fixnum ilen))))))))
	     (with-access::JsArray arr (vec ilen)
		;; fill the vector
		(let loop ((l l)
			   (i #u32:0))
		   (cond
		      ((null? l)
		       arr)
		      ((js-array? (car l))
		       (loop (cdr l)
			  (fixnum->uint32
			     (copy-array (car l) arr (uint32->fixnum i)))))
		      ((js-proxy? (car l))
		       (loop (cdr l)
			  (fixnum->uint32
			     (copy-proxy (car l) arr (uint32->fixnum i)))))
		      ((<u32 i ilen)
		       (vector-set! vec (uint32->fixnum i) (car l))
		       (loop (cdr l) (+u32 i #u32:1)))
		      ((js-object-mode-inline? arr)
		       (js-array-index-set! arr i (car l) #f %this)
		       (loop (cdr l) (+u32 i #u32:1)))
		      (else
		       (js-array-fixnum-set! arr (uint32->fixnum i) (car l) #f %this)
		       (loop (cdr l) (+u32 i #u32:1)))))))))
   
   (js-bind! %this js-array-prototype (& "concat")
      :value (js-make-function %this array-prototype-concat
		(js-function-arity array-prototype-concat)
		(js-function-info :name "concat" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; copyWithin
   ;; https://www.ecma-international.org/ecma-262/7.0/#sec-array.prototype.copywithin
   (define (array-prototype-copywithin this::obj target start end)
      (let* ((o (js-toobject %this this))
	     (len (js-get-length o %this))
	     (relativetarget (js-tointeger target %this))
	     (to (if (< relativetarget 0)
		     (max (+ len relativetarget) 0)
		     (min relativetarget len)))
	     (relativestart (js-tointeger start %this))
	     (from (if (< relativestart 0)
		       (max (+ len relativestart) 0)
		       (min relativestart len)))
	     (relativeend (if (eq? end (js-undefined))
			      len
			      (js-tointeger end %this)))
	     (final (if (< relativeend 0)
			(max (+ len relativeend) 0)
			(min relativeend len)))
	     (count (min (- final from) (- len to)))
	     (direction 1))
	 ;; step 10
	 (when (and (< from to) (< to (+ from count)))
	    (set! direction -1)
	    (set! from (- (+ from count) 1))
	    (set! o (- (- to count) 1)))
	 (if (and (js-array? o) (js-array-inlined? o))
	     (with-access::JsArray o (vec)
		(let loop ((from (->fixnum from))
			   (to (->fixnum to))
			   (count count))
		   (when (>fx count 0)
		      ;; step 12.d
		      (let ((fromval (vector-ref vec from)))
			 (vector-set! vec to fromval))
		      ;; step 12.3
		      (loop (+ from direction)
			 (+ to direction)
			 (-fx count 1)))))
	     (let loop ((from from)
			(to to)
			(count count))
		(when (>fx count 0)
		   (let ((toi (js-tointeger to %this))
			 (fromi (js-tointeger from %this)))
		      (if (js-has-property o fromi %this)
			  ;; step 12.d
			  (let ((fromval (js-get o fromi %this)))
			     (js-put! o toi fromval #f %this))
			  ;; step 12.3
			  (js-delete! o toi #t %this))
		      (loop (+ from direction)
			 (+ to direction)
			 (-fx count 1))))))
	 o))

   (js-bind! %this js-array-prototype (& "copyWithin")
      :value (js-make-function %this array-prototype-copywithin
		(js-function-arity array-prototype-copywithin)
		(js-function-info :name "copyWithin" :len 2)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; entries
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-array.prototype.entries
   (define (array-prototype-entries this::obj)
      (js-make-map-iterator (js-toobject %this this)
	 (lambda (key val)
	    (js-vector->jsarray (vector key val) %this))
	 %this))
   
   (js-bind! %this js-array-prototype (& "entries")
      :value (js-make-function %this array-prototype-entries
		(js-function-arity array-prototype-entries)
		(js-function-info :name "entries" :len 0)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; join
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.5
   (define (array-prototype-join this::obj separator)
      
      (define (el->string el)
	 (if (or (eq? el (js-undefined)) (eq? el (js-null)))
	     ""
	     (js-tostring el %this)))

      (if (js-array? this)
	  (js-array-prototype-join this separator %this)
	  (let* ((o (js-toobject %this this))
		 (lenval::uint32 (js-get-lengthu32 o %this))
		 (sep (if (eq? separator (js-undefined))
			  ","
			  (js-tostring separator %this))))
	     (if (=u32 lenval #u32:0)
		 (& "")
		 (let ((el0 (el->string (js-get o 0 %this))))
		    (let loop ((r (list el0))
			       (i #u32:1))
		       (if (=u32 i lenval)
			   (js-stringlist->jsstring (reverse! r))
			   (loop (cons* (el->string (js-get o i %this)) sep r)
			      (+u32 i #u32:1)))))))))
   
   (js-bind! %this js-array-prototype (& "join")
      :value (js-make-function %this array-prototype-join
		(js-function-arity array-prototype-join)
		(js-function-info :name "join" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; pop
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.6
   (define (array-prototype-pop this::obj)
      (if (not (js-array? this))
	  (let* ((o (js-toobject %this this))
		 (len::uint32 (js-get-lengthu32 o %this)))
	     (cond
		((=u32 len #u32:0)
		 (js-put-length! o 0 #f #f %this)
		 (js-undefined))
		(else
		 (let* ((indx (-u32 len #u32:1))
			(el (js-get o (js-uint32-tointeger indx) %this)))
		    (js-delete! o indx #t %this)
		    (js-put-length! o (js-uint32-tointeger indx) #f #f %this)
		    el))))
	  (js-array-prototype-pop this %this)))

   (js-bind! %this js-array-prototype (& "pop")
      :value (js-make-function %this array-prototype-pop
		(js-function-arity array-prototype-pop)
		(js-function-info :name "pop" :len 0)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; push
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.7
   (define (array-prototype-push this::obj . items)
      (if (not (js-array? this))
	  (let ((o (js-toobject %this this)))
	     (let ((n (js-uint32-tointeger (js-get-lengthu32 o %this))))
		(for-each (lambda (item)
			     (js-put! o n item #f %this)
			     (set! n (+ 1 n)))
		   items)
		(js-put-length! o n #f #f %this)
		n))
	  (begin
	     (for-each (lambda (item)
			  (js-array-prototype-push this item %this))
		items)
	     (js-get-length this %this))))
   
   (js-bind! %this js-array-prototype (& "push")
      :value (js-make-function %this array-prototype-push
		(js-function-arity array-prototype-push)
		(js-function-info :name "push" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; reverse
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.8
   (define (array-prototype-reverse this::obj)
      
      (define (vector-reverse! val len)
	 (let ((len/2 (/fx len 2)))
	    (let loop ((i 0))
	       (unless (=fx i len/2)
		  (let ((t (vector-ref val i))
			(ni (+fx i 1)))
		     (vector-set! val i (vector-ref val (-fx len ni)))
		     (vector-set! val (-fx len ni) t)
		     (loop ni))))))

      (define (array-reverse! o)
	 (with-access::JsArray o (ilen length)
	    (let* ((len::uint32 (js-get-lengthu32 o %this))
		   (len/2::uint32 (/u32 len #u32:2)))
	       (let loop ((i #u32:0))
		  (cond
		     ((=u32 i len/2)
		      o)
		     ((js-has-property o (js-toname i %this) %this)
		      (let* ((t (js-get o (uint32->fixnum i) %this))
			     (ni (+u32 i (fixnum->uint32 1)))
			     (rni (js-uint32-tointeger (-u32 len ni))))
			 (if (js-has-property o (js-toname rni %this) %this)
			     (begin
				(js-put! o (uint32->fixnum i)
				   (js-get o rni %this) #f %this)
				(js-put! o rni t #f %this))
			     (begin
				(js-delete! o (uint32->fixnum i) #t %this)
				(js-put! o rni t #f %this)))
			 (loop ni)))
		     (else
		      (let* ((ni (+u32 i (fixnum->uint32 1)))
			     (rni (js-uint32-tointeger (-u32 len ni))))
			 (if (js-has-property o (js-toname rni %this) %this)
			     (js-put! o (uint32->fixnum i)
				(js-get o rni %this) #f %this)
			     (js-delete! o (uint32->fixnum i) #t %this))
			 (js-delete! o rni #t %this)
			 (loop ni))))))))

      (if (js-array? this)
	  (with-access::JsArray this (vec)
	     (if (js-object-mode-inline? this)
		 ;; fast path
		 (with-access::JsArray this (ilen)
		    (vector-reverse! vec (uint32->fixnum ilen))
		    this)
		 (array-reverse! this)))
	  (let ((o (js-toobject %this this)))
	     (array-reverse! o))))
   
   (js-bind! %this js-array-prototype (& "reverse")
      :value (js-make-function %this array-prototype-reverse
		(js-function-arity array-prototype-reverse)
		(js-function-info :name "reverse" :len 0)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; shift
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.9
   (define (array-prototype-shift this::obj)
      
      (define (vector-shift! o len::uint32)
	 [%assert-array! o "vector-shift!"]
	 (with-access::JsArray o (vec length ilen)
	    (let ((first (vector-ref vec 0))
		  (nlen (-u32 len #u32:1))
		  (vlen (vector-length vec)))
	       (vector-copy! vec 0 vec 1)
	       (vector-set! vec (-fx vlen 1) (js-absent))
	       (set! length nlen)
	       (set! ilen (-u32 ilen 1))
	       first)))
      
      (define (array-shift! o len::uint32)
	 (let ((first (js-get o 0 %this))
	       (len (uint32->fixnum len)))
	    (let loop ((i 1))
	       (cond
		  ((=fx i len)
		   (js-delete! o (-fx i 1) #t %this)
		   (js-put-length! o (-fx i 1) #f #f %this)
		   first)
		  ((js-absent? (js-get-property o (js-toname i %this) %this))
		   (js-delete! o (-fx i 1) #t %this)
		   (loop (+fx i 1)))
		  (else
		   (let ((v (js-get o i %this)))
		      (js-put! o (-fx i 1) v #f %this)
		      (loop (+fx i 1))))))))
      
      (let ((o (js-toobject %this this)))
	 (if (js-array? o)
	     (with-access::JsArray o (vec length)
		(cond
		   ((=u32 length #u32:0)
		    (js-undefined))
		   ((js-object-mode-inline? o)
		    ;; ilen guard is needed when shifting array
		    ;; with prototype fields
		    (vector-shift! o length))
		   (else
		    (array-shift! o length))))
	     (let ((len (js-get-lengthu32 o %this)))
		(cond
		   ((=u32 len #u32:0)
		    (js-put-length! o 0 #f #f %this)
		    (js-undefined))
		   (else
		    (array-shift! o len)))))))

   (js-bind! %this js-array-prototype (& "shift")
      :value (js-make-function %this array-prototype-shift
		(js-function-arity array-prototype-shift)
		(js-function-info :name "shift" :len 0)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; slice
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.10
   (define (array-prototype-slice this start end)
      (js-array-prototype-slice this start end %this))
      
   (js-bind! %this js-array-prototype (& "slice")
      :value (js-make-function %this array-prototype-slice
		(js-function-arity array-prototype-slice)
		(js-function-info :name "slice" :len 2)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; sort
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.11
   (define (array-prototype-sort this::obj comparefn)
      (js-array-prototype-sort (js-toobject %this this) comparefn %this))

   (js-bind! %this js-array-prototype (& "sort")
      :value (js-make-function %this array-prototype-sort
		(js-function-arity array-prototype-sort)
		(js-function-info :name "sort" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; splice
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.12
   (define (array-prototype-splice this::obj start . opts)

      (define (vector-splice this len actualstart actualdeletecount)
	 (with-access::JsArray this (vec ilen)
	    (let* ((alen (vector-length vec))
		   (items (if (pair? opts) (cdr opts) '()))
		   (litems (length items))
		   (rlen (+fx actualstart actualdeletecount))
		   (nlen (+fx len (-fx litems actualdeletecount)))
		   (cstart (+fx actualstart actualdeletecount))
		   (vres (js-create-vector actualdeletecount))
		   (res (js-species->jsarray this vres %this)))
	       ;; populate the result vector
	       (when (<fx actualstart alen)
		  ;; from the inlined vector
		  (vector-copy! vres 0 vec actualstart
		     (minfx alen (+fx actualstart actualdeletecount))))
 	       (when (>fx actualdeletecount (-fx alen actualstart))
		  ;;  from the protype object
		  (let loop ((k (+fx actualstart (-fx alen actualstart))))
		     (when (<fx k actualdeletecount)
			(let ((o (js-get this k %this)))
			   (vector-set! vres k o)
			   (loop (+fx k 1))))))
	       ;; modify the source array
	       (cond
		  ((>fx nlen len)
		   ;; enlarge the vector if needed
		   (let ((tmp (make-vector nlen)))
		      (cond-expand
			 (profile (profile-vector-extension nlen len)))
		      (vector-copy! tmp 0 vec 0 actualstart)
		      (vector-copy! tmp (-fx nlen (-fx len cstart))
			 vec cstart len)
		      (set! ilen (fixnum->uint32 nlen))
		      (set! vec tmp)))
		  ((<=fx nlen 0)
		   (set! vec '#()))
		  ((<fx nlen len)
		   ;; shift the vector
		   (set! ilen (fixnum->uint32 nlen))
		   (vector-copy! vec (-fx nlen (-fx len cstart))
		      vec cstart len)))
	       ;; insert the new items
	       (let loop ((k actualstart)
			  (items items))
		  (if (pair? items)
		      (begin
			 (vector-set! vec k (car items))
			 (loop (+fx k 1) (cdr items)))
		      (js-put-length! this nlen #f #f %this)))
	       res)))

      (define (array-splice arr len actualstart actualdeletecount)
	 (let* ((els (array-get-elements arr actualstart
			(+ actualstart actualdeletecount) %this))
		(items (if (pair? opts) (cdr opts) '()))
		(res (js-vector->jsarray (list->vector els) %this))
		(rest (array-get-elements arr (+ actualstart actualdeletecount)
			 len %this)))
	    ;; add all the new elements
	    (for-each (lambda (el)
			 (js-put! arr actualstart el #f %this)
			 (set! actualstart (+ 1 actualstart)))
	       items)
	    (for-each (lambda (el)
			 (js-put! arr actualstart el #f %this)
			 (set! actualstart (+ 1 actualstart)))
	       rest)
	    ;; remove all the remaining elements
	    (let loop ()
	       (when (< actualstart len)
		  (js-delete! arr actualstart #f %this)
		  (set! len (- len 1))))
	    ;; shrink the vector
	    (js-put-length! arr actualstart #f #f %this)
	    res))

      (let* ((o (js-toobject %this this))
	     (relstart (js-tointeger start %this))
	     (len (js-uint32-tointeger (js-get-lengthu32 o %this)))
	     (actualstart (if (< relstart 0)
			      (max (+ len relstart) 0)
			      (min relstart len)))
	     (actualdeletecount (if (null? opts)
				    (-fx len (->fixnum actualstart))
				    (min (max (js-tointeger (car opts) %this) 0)
				       (- len actualstart)))))
	 (if (not (js-array? this))
	     (array-splice this len actualstart actualdeletecount)
	     (with-access::JsArray this (vec)
		(cond
		   ((js-object-mode-inline? o)
		    (vector-splice this len
		       (->fixnum actualstart) (->fixnum actualdeletecount)))
		   (else
		    (array-splice this len actualstart actualdeletecount)))))))

   (js-bind! %this js-array-prototype (& "splice")
      :value (js-make-function %this array-prototype-splice
		(js-function-arity array-prototype-splice)
		(js-function-info :name "splice" :len 2)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; unshift
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.13
   (define (array-prototype-unshift this::obj . items)
      
      (define (vector-unshift arr)
	 (with-access::JsArray arr (vec (vlength length) ilen)
	    (let* ((litems (length items))
		   (nlen (+u32 vlength (fixnum->uint32 litems)))
		   (nvec ($create-vector (uint32->fixnum nlen))))
	       ;; copy the existing inlined elements
	       (vector-copy! nvec litems vec)
	       ;; insert the new elements
	       (let ((i 0))
		  (for-each (lambda (el)
			       (vector-set! nvec i el)
			       (set! i (+fx i 1)))
		     items)
		  (cond-expand
		     (profile
		      (profile-vector-extension
			 (uint32->fixnum nlen)
			 (vector-length nvec))))
		  (set! vec nvec)
		  (js-put-length! arr (uint32->fixnum nlen) #f #f %this)
		  (with-access::JsArray arr (length)
		     (set! ilen length))
		  (uint32->fixnum nlen)))))
      
      (define (array-unshift arr len::uint32)
	 (let ((rest (array-get-elements arr 0 (js-uint32-tointeger len) %this))
	       (i 0))
	    ;; add all the new elements
	    (for-each (lambda (el)
			 (js-put! arr i el #f %this)
			 (set! i (+fx 1 i)))
	       items)
	    (for-each (lambda (el)
			 (js-put! arr i el #f %this)
			 (set! i (+fx 1 i)))
	       rest)
	    ;; shrink the vector
	    (js-put-length! arr i #f #f %this)
	    i))
      
      (let ((o (js-toobject %this this)))
	 (if (js-array? this)
	     (with-access::JsArray this (length)
		(cond
		   ((null? items)
		    (js-uint32-tointeger length))
		   ((js-object-mode-inline? this)
		    (vector-unshift this))
		   (else
		    (array-unshift this length))))
	     (let ((len::uint32 (js-get-lengthu32 o %this)))
		(if (null? items)
		    (let ((nlen (js-uint32-tointeger len)))
		       ;; override the length as the conversion touin32
		       ;; might have changed it
		       (js-put-length! o nlen #f #f %this)
		       nlen)
		    (array-unshift this len))))))

   (js-bind! %this js-array-prototype (& "unshift")
      :value (js-make-function %this array-prototype-unshift
		(js-function-arity array-prototype-unshift)
		(js-function-info :name "unshift" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; indexOf
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.14
   (define (array-prototype-indexof this::obj el #!optional (indx 0))
      (if (js-array? this)
	  (js-array-prototype-indexof this el indx %this)
	  (js-array-prototype-indexof (js-toobject %this this) el indx %this)))

   (js-bind! %this js-array-prototype (& "indexOf")
      :value (js-make-function %this array-prototype-indexof
		(js-function-arity 1 1 'optional)
		(js-function-info :name "indexOf" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; lastIndexOf
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.15
   (define (array-prototype-lastindexof this::obj el . indx)
      
      (define (vector-lastindexof::int arr vec k::long)
	 (let loop ((k k))
	    (cond
	       ((<fx k 0)
		-1)
	       ((js-strict-equal? (vector-ref vec k) el)
		k)
	       (else
		(loop (-fx k 1))))))

      (define (vector-holey-lastindexof::int arr vec k::long)
	 (let loop ((k k))
	    (cond
	       ((<fx k 0)
		-1)
	       ((js-strict-equal? (vector-ref vec k) el)
		k)
	       ((and (js-absent? (vector-ref vec k))
		     (let ((name (js-toname k %this)))
			(and (js-has-property arr name %this)
			     (js-strict-equal? (js-get arr name %this) el))))
		(js-uint32-tointeger k))
	       (else
		(loop (-fx k 1))))))
      
      (define (array-lastindexof::int arr k::uint32)
	 (let loop ((k k))
	    (cond
	       ((let ((name (js-toname k %this)))
		   (and (js-has-property arr name %this)
			(js-strict-equal? (js-get arr name %this) el)))
		(js-uint32-tointeger k))
	       ((=u32 k #u32:0)
		-1)
	       (else
		(loop (-u32 k #u32:1))))))
      
      (define (lastindexof::int o::JsObject k::uint32)
	 (if (js-array? o)
	     (with-access::JsArray o (vec ilen)
		(cond
		   ((and (js-object-mode-inline? o) (<u32 k ilen))
		    (vector-lastindexof o vec
		       (minfx (uint32->fixnum k)
			  (uint32->fixnum (-u32 ilen #u32:1)))))
		   ((js-object-mode-holey? o)
		    (vector-holey-lastindexof o vec
		       (minfx (uint32->fixnum k)
			  (-fx (vector-length vec) 1))))
		   (else
		    (array-lastindexof o k))))
	     (array-lastindexof o k)))

      (let* ((o (js-toobject %this this))
	     (len::uint32 (js-get-lengthu32 o %this)))
	 (if (=u32 len #u32:0)
	     -1
	     (let ((n (if (pair? indx)
			  (js-tointeger (car indx) %this)
			  (js-uint32-tointeger (-u32 len #u32:1)))))
		(if (< n 0)
		    (let ((absn (abs n)))
		       (if (>uint32 len absn)
			   (lastindexof o (-u32 len (->uint32 (abs n))))
			   -1))
		    (if (>=uint32 (-u32 len #u32:1) n)
			(lastindexof o (->uint32 n))
			(lastindexof o (-u32 len #u32:1))))))))

   (js-bind! %this js-array-prototype (& "lastIndexOf")
      :value (js-make-function %this array-prototype-lastindexof
		(js-function-arity array-prototype-lastindexof)
		(js-function-info :name "lastIndexOf" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; every
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.16
   (define (array-prototype-every this::obj proc t)

      (define (test-val proc t v i::uint32 o)
	 (js-totest (js-call3 %this proc t v (js-uint32-tointeger i) o)))

      (define (vector-every this o len::uint32 proc t i::uint32 %this)
	 (with-access::JsArray o (vec ilen)
	    (let ((ilen0 ilen)
		  (full0 (js-object-mode-inline? o)))
	       (let loop ((i i))
		  (cond
		     ((>=u32 i ilen)
		      (or (js-object-mode-inline? o)
			  (array-every this o len proc t i %this)))
		     ((>=u32 i ilen0)
		      (or full0 (array-every this o len proc t i %this)))
		     (else
		      (let ((v (u32vref vec i)))
			 (cond
			    ((test-val proc t v i o)
			     (loop (+u32 i 1)))
			    (else
			     #f)))))))))

      (define (array-every this o len::uint32 proc t i::uint32 %this)
	 (let loop ((i i))
	    (if (>=u32 i len)
		#t
		(let ((pv (js-get-property-value o o i %this)))
		   (cond
		      ((js-absent? pv)
		       (loop (+u32 i 1)))
		      ((test-val proc t pv i o)
		       (loop (+u32 i 1)))
		      (else
		       #f))))))

      (array-prototype-iterator this proc t array-every vector-every %this))
   
   (js-bind! %this js-array-prototype (& "every")
      :value (js-make-function %this array-prototype-every
		(js-function-arity array-prototype-every)
		(js-function-info :name "every" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; some
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.17
   (define (array-prototype-some this::obj proc t)

      (define (test-val proc t v i::uint32 o)
	 (js-totest (js-call3 %this proc t v (js-uint32-tointeger i) o)))

      (define (vector-some this o len::uint32 proc t i::uint32 %this)
	 (with-access::JsArray o (vec ilen)
	    (let loop ((i i))
	       (cond
		  ((>=u32 i ilen)
		   (unless (js-object-mode-inline? o)
		      (array-some this o len proc t i %this)))
		  (else
		   (let ((v (vector-ref vec (uint32->fixnum i))))
		      (cond
			 ((test-val proc t v i o)
			  #t)
			 (else
			  (loop (+u32 i 1))))))))))

      (define (array-some this o len proc t i::uint32 %this)
	 (let loop ((i i))
	    (if (>=u32 i len)
		#f
		(let ((pv (js-get-property-value o o i %this)))
		   (cond
		      ((js-absent? pv)
		       (loop (+u32 i 1)))
		      ((test-val proc t pv i o)
		       #t)
		      (else
		       (loop (+u32 i 1))))))))

      (array-prototype-iterator this proc t array-some vector-some %this))

   (js-bind! %this js-array-prototype (& "some")
      :value (js-make-function %this array-prototype-some
		(js-function-arity array-prototype-some)
		(js-function-info :name "some" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; forEach
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.18
   (define (array-prototype-foreach this::obj proc t)
      (js-array-prototype-foreach this proc t %this))

   (js-bind! %this js-array-prototype (& "forEach")
      :value (js-make-function %this array-prototype-foreach
		(js-function-arity array-prototype-foreach)
		(js-function-info :name "forEach" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; map
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.19
   (define (array-prototype-map this::obj proc t)
      (js-array-prototype-map this proc t %this))

   (js-bind! %this js-array-prototype (& "map")
      :value (js-make-function %this array-prototype-map
		(js-function-arity array-prototype-map)
		(js-function-info :name "map" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; filter
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.20
   (define (array-prototype-filter this::obj proc t)

      (define (array-filter/array this o len proc t i::uint32 j::uint32 a %this)
	 (let loop ((i i)
		    (j::uint32 j))
	    (if (<u32 i len)
		(let ((pv (js-get-property-value o o i %this)))
		   (if (js-absent? pv)
		       (with-access::JsArray a (ilen)
			  (when (js-object-mode-inline? a)
			     (js-object-mode-inline-set! a #f)
			     (if (>u32 i 0)
				 (set! ilen (-u32 i #u32:1))
				 (set! ilen #u32:0)))
			  (loop (+u32 i 1) j))
		       (let ((v pv)
			     (nj (js-toname j %this)))
			  (if (js-totest (js-call3 %this proc t v
					    (js-uint32-tointeger i) o))
			      (let ((newdesc (instantiate::JsValueDescriptor
						(name nj)
						(value v)
						(writable #t)
						(enumerable #t)
						(configurable #t))))
				 ;; 6
				 (js-define-own-property a nj newdesc #f %this)
				 (loop (+u32 i 1) (+u32 j 1)))
			      (loop (+u32 i 1) j)))))
		a)))
      
      (define (vector-filter this o len::uint32 proc t i::uint32 %this)
	 [%assert-array! o "vector-filter"]
	 (with-access::JsArray o (vec ilen length)
	    (if (js-object-mode-inline? o)
		(let ((v (js-create-vector (vector-length vec))))
		   (let loop ((i i)
			      (j 0))
		      (cond
			 ((>=u32 i ilen)
			  (let ((a (js-species->jsarray this v %this)))
			     (with-access::JsArray a (length ilen)
				(set! length j)
				(set! ilen j)
				(if (js-object-mode-inline? o)
				    a
				    (array-filter/array this o len proc t i j a %this)))))
			 (else
			  (let ((val (vector-ref vec (uint32->fixnum i))))
			     (cond
				((js-totest (js-call3 %this proc t val
					       (js-uint32-tointeger i) o))
				 (vector-set! v (uint32->fixnum j) val)
				 (loop (+u32 i 1) (+u32 j 1)))
				(else
				 (loop (+u32 i 1) j))))))))
		(array-filter this o len proc t i %this))))

      (define (array-filter this o len proc t i::uint32 %this)
	 (let ((a (js-vector->jsarray '#() %this)))
	    (array-filter/array this o len proc t i 0 a %this)))

      (array-prototype-iterator this proc t array-filter vector-filter %this))

   (js-bind! %this js-array-prototype (& "filter")
      :value (js-make-function %this array-prototype-filter
		(js-function-arity array-prototype-filter)
		(js-function-info :name "filter" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; fill
   ;; http://www.ecma-international.org/ecma-262/6.0/#sec-array.prototype.fill
   (define (array-prototype-fill this::obj value start end)
      (if (js-array? this)
	  (js-array-prototype-fill this value start end %this)
	  (let* ((o (js-toobject %this this))
		 (len (js-get-length o %this))
		 (relativestart (js-tointeger start %this))
		 (relativeend (if (eq? end (js-undefined))
				  len
				  (js-tointeger end %this)))
		 (final (if (< relativeend 0)
			    (max (+ end relativeend) 0)
			    (min relativeend len))))
	     (let loop ((k (if (< relativestart 0)
			       (max (+ len relativestart) 0)
			       (min relativestart len))))
		(when (< k final)
		   (let ((pk (js-integer-name->jsstring k)))
		      (let ((setstatus (js-put! o pk value #t %this)))
			 (loop (+ k 1))))))
	     o)))
   
   (js-bind! %this js-array-prototype (& "fill")
      :value (js-make-function %this array-prototype-fill
		(js-function-arity array-prototype-fill)
		(js-function-info :name "fill" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; find
   ;; http://www.ecma-international.org/ecma-262/7.0/#sec-indexed-collections#sec-array.prototype.find
   (define (array-prototype-find this::obj proc t)

      (define (vector-find this o::JsArray len::uint32 proc::JsProcedure t i::uint32 %this)
	 (with-access::JsArray o (vec ilen)
	    (let loop ((i i))
	       (cond
		  ((>=u32 i ilen)
		   (unless (js-object-mode-inline? o)
		      (array-find this o len proc t i %this)))
		  (else
		   (let ((v (vector-ref vec (uint32->fixnum i))))
		      (cond
			 ((js-totest (js-call3 %this proc t v (js-uint32-tointeger i) o))
			  v)
			 (else
			  (loop (+u32 i 1))))))))))

      (define (array-find this o::JsArray len::uint32 proc::JsProcedure t i::uint32 %this)
	 (let loop ((i i))
	    (if (>=u32 i len)
		(js-undefined)
		(let* ((pv (js-get-property-value o o i %this))
		       (v (if (js-absent? pv) (js-undefined) pv)))
		   (if (js-totest (js-call3 %this proc t v (js-uint32-tointeger i) o))
		       v
		       (loop (+u32 i 1)))))))

      (array-prototype-iterator this proc t array-find vector-find %this))

   (js-bind! %this js-array-prototype (& "find")
      :value (js-make-function %this array-prototype-find
		(js-function-arity array-prototype-find)
		(js-function-info :name "find" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; findIndex
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-array.prototype.findindex
   (define (array-prototype-find-index this::obj proc t)

      (define (vector-find-idx this o::JsArray len::uint32 proc::JsProcedure t i::uint32 %this)
	 (with-access::JsArray o (vec ilen)
	    (let loop ((i i))
	       (cond
		  ((>=u32 i ilen)
		   (if (js-object-mode-inline? o)
		       -1
		       (array-find-idx this o len proc t i %this)))
		  (else
		   (let ((v (vector-ref vec (uint32->fixnum i))))
		      (cond
			 ((js-totest (js-call3 %this proc t v (js-uint32-tointeger i) o))
			  (js-uint32-tointeger i))
			 (else
			  (loop (+u32 i 1))))))))))

      (define (array-find-idx this o::JsArray len::uint32 proc::JsProcedure t i::uint32 %this)
	 (let loop ((i i))
	    (if (>=u32 i len)
		-1
		(let* ((pv (js-get-property-value o o i %this))
		       (v (if (js-absent? pv) (js-undefined) pv)))
		   (if (js-totest (js-call3 %this proc t v (js-uint32-tointeger i) o))
		       (js-uint32-tointeger i)
		       (loop (+u32 i 1)))))))

      (array-prototype-iterator this proc t array-find-idx vector-find-idx %this))

   (js-bind! %this js-array-prototype (& "findIndex")
      :value (js-make-function %this array-prototype-find-index
		(js-function-arity array-prototype-find-index)
		(js-function-info :name "findIndex" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; includes
   ;; https://www.ecma-international.org/ecma-262/7.0/#sec-array.prototype.includes
   (define (array-prototype-includes this::obj val idx)
      
      (define (vector-includes o::JsArray len::uint32 i::uint32)
	 (with-access::JsArray o (vec ilen)
	    (let loop ((i i))
	       (cond
		  ((>=u32 i ilen)
		   (if (js-array-inlined? o)
		       #f
		       (array-includes o len i)))
		  ((js-strict-equal? (vector-ref vec (uint32->fixnum i)) val)
		   #t)
		  ((and (flonum? val)
			(nanfl? val)
			(flonum? (vector-ref vec (uint32->fixnum i)))
			(nanfl? (vector-ref vec (uint32->fixnum i))))
		   #t)
		  (else
		   (loop (+u32 i 1)))))))
      
      (define (array-includes o len::uint32 i::uint32)
	 (let loop ((i i))
	    (if (>=u32 i len)
		#f
		(let* ((pv (js-get-property-value o o i %this))
		       (v (if (js-absent? pv) (js-undefined) pv)))
		   (cond
		      ((js-strict-equal? v val)
		       #t)
		      ((and (flonum? v) (nanfl? v) (flonum? val) (nanfl? val))
		       #t)
		      (else
		       (loop (+u32 i 1))))))))
      
      (define (startidx len i)
	 (cond
	    ((>=s32 i 0) (int32->uint32 i))
	    ((>u32 (int32->uint32 (negs32 i)) len) #u32:0)
	    (else (-u32 len (int32->uint32 (negs32 i))))))
      
      (let ((o (js-toobject %this this))
	    (i (if (eq? idx (js-undefined)) #u32:0 (js-toint32 idx %this))))
	 (cond
	    ((not (js-array? o))
	     (let ((len (js-get-lengthu32 o %this)))
		(array-includes o len (startidx len i))))
	    (else
	     (with-access::JsArray o (length vec ilen)
		(if (js-array-inlined? o)
		    (vector-includes o length (startidx length i))
		    (array-includes o length (startidx length i))))))))
   
   (js-bind! %this js-array-prototype (& "includes")
      :value (js-make-function %this array-prototype-includes
		(js-function-arity array-prototype-includes)
		(js-function-info :name "includes" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; reduce
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.21
   (define (array-prototype-reduce this::obj proc . init)
      
      (define (reduce/accumulator o len::uint32 i::uint32 accumulator)
	 (let loop ((i i)
		    (acc accumulator))
	    (if (<u32 i len)
		(let ((pv (js-get-property-value o o i %this)))
		   (if (js-absent? pv)
		       (loop (+u32 i #u32:1) acc)
		       (let ((v pv))
			  (loop (+u32 i #u32:1)
			     (js-call4 %this proc (js-undefined) acc v
				(js-uint32-tointeger i) o)))))
		acc)))
      
      (let* ((o (js-toobject %this this))
	     (len::uint32 (js-get-lengthu32 o %this)))
	 (if (not (js-procedure? proc))
	     (js-raise-type-error %this "Not a procedure ~s" proc)
	     ;; find the accumulator init value
	     (if (null? init)
		 (let loop ((i #u32:0))
		    (if (<u32 i len)
			(let ((pv (js-get-property-value o o i %this)))
			   (if (js-absent? pv)
			       (loop (+u32 i #u32:1))
			       (reduce/accumulator o len (+u32 i #u32:1) pv)))
			(js-raise-type-error %this
			   "reduce: cannot find accumulator ~s" this)))
		 (reduce/accumulator o len #u32:0 (car init))))))
   
   (js-bind! %this js-array-prototype (& "reduce")
      :value (js-make-function %this array-prototype-reduce
		(js-function-arity array-prototype-reduce)
		(js-function-info :name "reduce" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; reduceRight
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.22
   (define (array-prototype-reduceright this::obj proc . init)
      
      (define (reduce/accumulator o len::uint32 i::uint32 accumulator)
	 (let loop ((i i)
		    (acc accumulator))
	    (let ((pv (js-get-property-value o o i %this)))
	       (if (<u32 i len)
		   (if (js-absent? pv)
		       (loop (-u32 i #u32:1) acc)
		       (let* ((v pv)
			      (acc (js-call4 %this proc (js-undefined) acc v
				      (js-uint32-tointeger i) o)))
			  (loop (-u32 i #u32:1) acc)))
		   acc))))
      
      (let* ((o (js-toobject %this this))
	     (len::uint32 (js-get-lengthu32 o %this)))
	 (if (not (js-procedure? proc))
	     (js-raise-type-error %this "Not a procedure ~s" proc)
	     ;; find the accumulator init value
	     (if (null? init)
		 (let loop ((k (-u32 len #u32:1)))
		    (if (<u32 k len)
			(let ((pv (js-get-property-value o o k %this)))
			   (if (js-absent? pv)
			       (loop (-u32 k #u32:1))
			       (let ((v pv))
				  (reduce/accumulator o len (-u32 k #u32:1) v))))
			(js-raise-type-error %this
			   "reduce: cannot find accumulator ~s" this)))
		 (reduce/accumulator o len (-u32 len #u32:1) (car init))))))
   
   (js-bind! %this js-array-prototype (& "reduceRight")
      :value (js-make-function %this array-prototype-reduceright
		(js-function-arity array-prototype-reduceright)
		(js-function-info :name "reduceRight" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; @@iterator
   ;; http://www.ecma-international.org/ecma-262/6.0/#sec-22.1.3.30
   (define (array-prototype-array-values this::obj)
      (js-make-iterator this %this))
   
   (with-access::JsGlobalObject %this (js-symbol-iterator)
      (js-bind! %this js-array-prototype js-symbol-iterator
	 :value (js-make-function %this array-prototype-array-values
		   (js-function-arity array-prototype-array-values)
		   (js-function-info :name "@@iterator" :len 0)
		   :prototype (js-undefined))
	 :enumerable #f
	 :hidden-class #t))

   ;; @@unscopable
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-22.1.3.31
   (with-access::JsGlobalObject %this (js-symbol-unscopables)
      (let ((unscopables (instantiateJsObject
			    (__proto__ (js-object-proto %this))
			    (elements ($create-vector 7)))))
	 (for-each (lambda (id)
		      (js-bind! %this unscopables
			 (js-ascii-name->jsstring id)
			 :value #t :writable #f
			 :enumerable #f :configurable #t))
	    '("copyWithin" "entries" "fill" "find" "findIndex" "keys" "values"))
	 (js-bind! %this js-array-prototype js-symbol-unscopables
	    :value unscopables
	    :enumerable #f
	    :hidden-class #t))))

;*---------------------------------------------------------------------*/
;*    js-array-species-create ...                                      */
;*---------------------------------------------------------------------*/
(define (js-array-species-create %this origin new-len)
   (with-access::JsGlobalObject %this (js-symbol-species js-array js-array-prototype js-array-pcache)
      
      (define (check-array val)
	 (if (js-array? val)
	     val
	     (js-vector->jsarray (vector val) %this))) 

      (if (eq? (js-object-proto origin) js-array-prototype)
	  (js-array-construct-alloc/lengthu32 %this (fixnum->uint32 new-len))
	  (let ((ctor (js-get-name/cache origin (& "constructor") #f %this
			 (js-pcache-ref js-array-pcache 13))))
	     (if (and (js-function? ctor) (not (eq? js-array ctor)))
		 (let ((species (js-get ctor js-symbol-species %this)))
		    (check-array
		       (cond
			  ((js-function? species)
			   (js-new1 %this species 0))
			  ((js-function? ctor)
			   (js-new1 %this ctor 0))
			  (else
			   (js-raise-type-error %this
			      "Not a constructor" ctor)))))
		 (js-array-construct/lengthu32 %this
		    (js-array-alloc %this)
		    (fixnum->uint32 new-len)))))))

;*---------------------------------------------------------------------*/
;*    %js-array ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.1       */
;*---------------------------------------------------------------------*/
(define (%js-array %this::JsGlobalObject)
   (lambda (this . items)
      (with-access::JsGlobalObject %this (js-new-target)
	 (if (eq? js-new-target (js-undefined))
	     (let ((arr (js-array-alloc %this)))
		(js-array-construct %this arr items)
		arr)
	     (begin
		(set! js-new-target (js-undefined))
		(js-array-construct %this this items))))))

;*---------------------------------------------------------------------*/
;*    js-array-new1 ...                                                */
;*---------------------------------------------------------------------*/
(define (js-array-new1 item-or-len %this)
   (cond
      ((fixnum? item-or-len)
       (if (<fx item-or-len (bit-lsh 1 16))
	   (js-array-construct-alloc-small %this (fixnum->uint32 item-or-len))
	   (let ((arr (js-array-alloc %this)))
	      (js-array-construct/length %this arr item-or-len)
	      arr)))
      ((js-number? item-or-len)
       (let ((arr (js-array-alloc %this)))
	  (js-array-construct/length %this arr item-or-len)
	  arr))
      (else
       (let* ((arr (js-array-construct-alloc-small %this #u32:1)))
	  (with-access::JsArray arr (vec ilen length)
	     (set! ilen #u32:1)
	     (set! length #u32:1)
	     (vector-set! vec 0 item-or-len)
	     arr)))))

;*---------------------------------------------------------------------*/
;*    js-array-alloc ...                                               */
;*---------------------------------------------------------------------*/
(define (js-array-alloc::JsArray %this)
   (with-access::JsGlobalObject %this (js-array-prototype)
      (instantiateJsArray
	 (cmap (js-not-a-cmap))
	 (__proto__ js-array-prototype))))

;*---------------------------------------------------------------------*/
;*    js-array-alloc-ctor ...                                          */
;*---------------------------------------------------------------------*/
(define (js-array-alloc-ctor::JsArray %this constructor::JsFunction)
   (with-access::JsGlobalObject %this (js-array js-array-prototype js-array-pcache js-new-target)
      (let ((proto (if (eq? constructor js-array)
		       js-array-prototype
		       (js-get-jsobject-name/cache constructor (& "prototype") #f %this
			  (js-pcache-ref js-array-pcache 0)))))
	 (set! js-new-target constructor)
	 (instantiateJsArray
	    (cmap (js-not-a-cmap))
	    (__proto__ proto)))))

;*---------------------------------------------------------------------*/
;*    js-array-construct-alloc-small ...                               */
;*    -------------------------------------------------------------    */
;*    Specialised version of js-array-construct and js-array-alloc     */
;*    for small arrays.                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-array-construct-alloc-small %this::JsGlobalObject len::uint32)
   (cond-expand
      ((and bigloo-c (not devel) (not debug))
       (with-access::JsGlobalObject %this (js-array-prototype)
	  ($js-make-jsarray (uint32->fixnum len) len
	     (js-not-a-cmap)
	     js-array-prototype
	     (js-absent) (js-array-default-mode))))
      (else
       (let* ((this (js-array-alloc %this))
	      (v (make-vector (uint32->fixnum len) (js-absent))))
	  (js-object-mode-set! this (js-array-default-mode))
	  (with-access::JsArray this (vec ilen length)
	     (set! length len)
	     (set! ilen #u32:0)
	     (set! vec v))
	  this))))

;*---------------------------------------------------------------------*/
;*    js-array-construct-alloc-small-sans-init ...                     */
;*    -------------------------------------------------------------    */
;*    Specialised version of js-array-construct and js-array-alloc     */
;*    for small arrays.                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-array-construct-alloc-small-sans-init %this::JsGlobalObject len::uint32)
   (cond-expand
      ((and bigloo-c (not devel) (not debug))
       (with-access::JsGlobalObject %this (js-array-prototype)
	  ($js-make-jsarray-sans-init (uint32->fixnum len) len
	     (js-not-a-cmap)
	     js-array-prototype
	     (js-array-default-mode))))
      (else
       (let* ((this (js-array-alloc %this))
	      (v (make-vector (uint32->fixnum len) (js-absent))))
	  (js-object-mode-set! this (js-array-default-mode))
	  (with-access::JsArray this (vec ilen length)
	     (set! length #u32:0)
	     (set! ilen len)
	     (set! vec v)
	     this)))))

;*---------------------------------------------------------------------*/
;*    js-array-construct-alloc/lengthu32 ...                           */
;*---------------------------------------------------------------------*/
(define-inline (js-array-construct-alloc/lengthu32 %this::JsGlobalObject len::uint32)
   (if (<u32 len #u32:1024)
       (js-array-construct-alloc-small %this len)
       (js-array-construct/lengthu32 %this (js-array-alloc %this) len)))
       
;*---------------------------------------------------------------------*/
;*    js-array-construct/lengthu32 ...                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.2.1     */
;*---------------------------------------------------------------------*/
(define (js-array-construct/lengthu32 %this::JsGlobalObject this::JsArray len::uint32)
   
   (define (array-set! v::vector iln::uint32 ulen::uint32)
      (with-access::JsArray this (vec ilen length)
	 (set! length ulen)
	 (set! ilen iln)
	 (set! vec v))
      this)

   (cond
      ((<=u32 len (bit-lshu32 #u32:1 16))
       ;; MS CARE: the max boundary for a concrete vector
       ;; is pure heuristic. This should be confirmed by
       ;; actual tests
       (let ((vec (js-create-vector (uint32->fixnum len))))
	  (array-set! vec #u32:0 len)))
      (else
       (array-set! (js-create-vector 8) #u32:0 len))))

;*---------------------------------------------------------------------*/
;*    js-array-construct/length ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.2.1     */
;*---------------------------------------------------------------------*/
(define (js-array-construct/length %this::JsGlobalObject this::JsArray len)
   (let ((l32 (js-touint32 len %this)))
      (if (not (=uint32 l32 len))
	  (js-raise-range-error %this "index out of range ~a" len)
	  (js-array-construct/lengthu32 %this this l32))))

;*---------------------------------------------------------------------*/
;*    js-array-construct ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.2.1     */
;*---------------------------------------------------------------------*/
(define (js-array-construct %this::JsGlobalObject this::JsArray items-or-len)

   (define (array-set! v::vector iln::uint32 ulen::uint32)
      (with-access::JsArray this (vec ilen length)
	 (set! length ulen)
	 (set! ilen iln)
	 (set! vec v))
      this)

   (cond
      ((null? items-or-len)
       (js-array-construct/length %this this 0))
      ((and (fixnum? (car items-or-len)) (null? (cdr items-or-len)))
       (js-array-construct/length %this this (car items-or-len)))
      ((and (js-number? (car items-or-len)) (null? (cdr items-or-len)))
       (js-array-construct/length %this this (car items-or-len)))
      (else
       (let* ((vec (list->vector items-or-len))
	      (len (vector-length vec)))
	  (array-set! vec (fixnum->uint32 len) (fixnum->uint32 len))))))

;*---------------------------------------------------------------------*/
;*    js-array-construct1 ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.2.1     */
;*---------------------------------------------------------------------*/
(define (js-array-construct1 %this::JsGlobalObject this::JsArray item-or-len)

   (define (array-set! v::vector iln::uint32 ulen::uint32)
      (with-access::JsArray this (vec ilen length)
	 (set! length ulen)
	 (set! ilen iln)
	 (set! vec v))
      this)

   (cond
      ((fixnum? item-or-len)
       (if (and (>=fx item-or-len 0) (<fx item-or-len 1024))
	   (js-array-construct-alloc-small %this (fixnum->uint32 item-or-len))
	   (js-array-construct/length %this this item-or-len)))
      ((js-number? item-or-len)
       (js-array-construct/length %this this item-or-len))
      (else
       (let* ((vec (vector item-or-len))
	      (len (vector-length vec)))
	  (array-set! vec #u32:1 #u32:1)))))

;*---------------------------------------------------------------------*/
;*    js-array-construct-alloc/length ...                              */
;*---------------------------------------------------------------------*/
(define (js-array-construct-alloc/length %this::JsGlobalObject len)
   (if (and (>=fx len 0) (<fx len 1024))
       (js-array-construct-alloc-small %this (fixnum->uint32 len))
       (js-array-construct/length %this (js-array-alloc %this) len)))
       
;*---------------------------------------------------------------------*/
;*    js-array-construct-alloc ...                                     */
;*---------------------------------------------------------------------*/
(define (js-array-construct-alloc %this::JsGlobalObject item-or-len)
   (if (fixnum? item-or-len)
       (if (and (>=fx item-or-len 0) (<fx item-or-len 1024))
	   (js-array-construct-alloc-small %this (fixnum->uint32 item-or-len))
	   (js-array-construct/length %this (js-array-alloc %this) item-or-len))
       (js-array-construct1 %this (js-array-alloc %this) item-or-len)))
       
;*---------------------------------------------------------------------*/
;*    js-vector->jsarray ...                                           */
;*---------------------------------------------------------------------*/
(define (js-vector->jsarray::JsArray vec::vector %this::JsGlobalObject)
   (let* ((len (vector-length vec)))
      (with-access::JsGlobalObject %this (js-array-prototype)
	 (instantiateJsArray
	    (__proto__ js-array-prototype)
	    (length (fixnum->uint32 len))
	    (ilen (fixnum->uint32 len))
	    (vec vec)))))

;*---------------------------------------------------------------------*/
;*    js-vector->sparse-jsarray ...                                    */
;*---------------------------------------------------------------------*/
(define (js-vector->sparse-jsarray vec::vector %this::JsGlobalObject)
   (let ((arr (js-vector->jsarray vec %this)))
      (js-object-mode-inline-set! arr #f)
      (js-object-mode-holey-set! arr #t)
      (js-array-update-ilen! arr 0 (fixnum->uint32 (-fx (vector-length vec) 1)))
      arr))

;*---------------------------------------------------------------------*/
;*    js-empty-vector->jsarray ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (js-empty-vector->jsarray::JsArray %this::JsGlobalObject)
   (let ((mode (js-array-default-mode)))
      (cond-expand
	 ((and bigloo-c (not devel) (not debug))
	  (with-access::JsGlobalObject %this (js-array-prototype)
	     ($js-make-jsarray (DEFAULT-EMPTY-ARRAY-SIZE) #u32:0 (js-not-a-cmap)
		js-array-prototype (js-absent) mode)))
	 (else
	  (with-access::JsGlobalObject %this (js-array-prototype)
	     (let* ((vec (make-vector (DEFAULT-EMPTY-ARRAY-SIZE) (js-absent)))
		    (o (instantiate::JsArray
			  (length #u32:0)
			  (ilen #u32:0)
			  (vec vec))))
		(js-object-mode-set! o mode)
		(js-object-proto-set! o js-array-prototype)
		o))))))

;*---------------------------------------------------------------------*/
;*    js-species->jsarray ...                                          */
;*---------------------------------------------------------------------*/
(define (js-species->jsarray this::JsObject vec::vector %this::JsGlobalObject)
   (let ((arr (js-vector->jsarray vec %this)))
      (with-access::JsGlobalObject %this (js-symbol-species js-array-prototype)
	 (let ((__species_proto__ (js-object-proto this)))
	    (if (eq? __species_proto__ js-array-prototype)
		arr
		(begin
		   (js-object-proto-set! arr __species_proto__)
		   arr))))))

;*---------------------------------------------------------------------*/
;*    js-properties-names ::JsArray ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-properties-names obj::JsArray enump %this)
   (js-array-update-length-property! obj)
   (with-access::JsArray obj (vec ilen)
      (let loop ((i (-fx (uint32->fixnum ilen) 1))
		 (acc '()))
	 (if (=fx i -1)
	     (append! acc (call-next-method))
	     (loop (-fx i 1) (cons (js-integer->jsstring i) acc))))))

;*---------------------------------------------------------------------*/
;*    js-has-property ::JsArray ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.5.2     */
;*---------------------------------------------------------------------*/
(define-method (js-has-property o::JsArray p %this)
   (with-access::JsArray o (vec ilen length)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((<u32 i ilen) #t)
	    ((js-isname? p (& "length") %this) #t)
	    ((and (js-object-mode-holey? o)
		  (<u32 i (fixnum->uint32 (vector-length vec)))
		  (<u32 i length))
	     (if (js-absent? (u32vref vec i)) (call-next-method) #t))
	    (else (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-has-own-property ::JsArray ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-has-own-property o::JsArray p %this::JsGlobalObject)
   (with-access::JsArray o (vec ilen length)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((<u32 i ilen) #t)
	    ((js-isname? p (& "length") %this) #t)
	    ((and (js-object-mode-holey? o)
		  (<u32 i (fixnum->uint32 (vector-length vec)))
		  (<u32 i length))
	     (if (js-absent? (u32vref vec i)) (call-next-method) #t))
	    (else (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property o::JsArray p %this::JsGlobalObject)
   (with-access::JsArray o (vec ilen length)
      (let ((i::uint32 (js-toindex p))
	    (frozen (js-object-mode-frozen? o)))
	 (cond
	    ((<u32 i ilen)
	     ;; fast zone
	     (instantiate::JsValueDescriptor
		(name (js-toname p %this))
		(value (u32vref vec i))
		(enumerable #t)
		(writable (not frozen))
		(configurable (not frozen))))
	    ;; MS: 23 feb 2017
	    ((not (js-isindex? i))
	     (set! p (js-toname p %this))
	     (if (eq? p (& "length"))
		 (js-array-update-length-property! o)
		 (call-next-method)))
	    ((and (js-object-mode-holey? o)
		  (<u32 i (fixnum->uint32 (vector-length vec))))
	     (if (>=u32 i length)
		 (js-undefined)
		 (let ((v (u32vref vec i)))
		    (if (js-absent? v)
			(call-next-method)
			(instantiate::JsValueDescriptor
			   (name (js-toname p %this))
			   (value v)
			   (enumerable #t)
			   (writable (not frozen))
			   (configurable (not frozen)))))))
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property-descriptor ...                               */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property-descriptor o::JsArray p %this::JsGlobalObject)
   (with-access::JsArray o (vec ilen length)
      (let ((i::uint32 (js-toindex p))
	    (frozen (js-object-mode-frozen? o)))
	 (cond
	    ((<u32 i ilen)
	     ;; fast zone
	     (js-property-descriptor %this #t
		:value (u32vref vec i)
		:enumerable #t
		:writable (not frozen)
		:configurable (not frozen)))
	    ;; MS: 23 feb 2017
	    ((not (js-isindex? i))
	     (set! p (js-toname p %this))
	     (if (eq? p (& "length"))
		 (js-array-update-length-property! o)
		 (call-next-method)))
	    ((and (js-object-mode-holey? o)
		  (<u32 i (fixnum->uint32 (vector-length vec))))
	     (if (>=u32 i length)
		 (js-undefined)
		 (let ((v (u32vref vec i)))
		    (if (js-absent? v)
			(call-next-method)
			(js-property-descriptor %this #t
			   :value v
			   :enumerable #t
			   :writable (not frozen)
			   :configurable (not frozen))))))
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-get-property-value ::JsArray ...                              */
;*    -------------------------------------------------------------    */
;*    This method is optional. It could be removed without changing    */
;*    the programs behaviors. It merely optimizes access to strings.   */
;*---------------------------------------------------------------------*/
(define-method (js-get-property-value o::JsArray base p %this)
   (with-access::JsArray o (vec ilen length)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((<u32 i ilen)
	     (u32vref vec i))
	    ((not (js-isindex? i))
	     (set! p (js-toname p %this))
	     (if (eq? p (& "length"))
		 (js-uint32-tointeger length)
		 (call-next-method)))
	    ((and (js-object-mode-holey? o)
		  (<u32 i (fixnum->uint32 (vector-length vec))))
	     (if (>=u32 i length)
		 (call-next-method)
		 (let ((v (u32vref vec i)))
		    (if (js-absent? v)
			(call-next-method)
			v))))
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-get ::JsArray ...                                             */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsArray p %this)
   (with-access::JsArray o (vec ilen length)
      (if (eq? p (& "length"))
	  (js-uint32-tointeger length)
	  (let ((i::uint32 (js-toindex p)))
	     (cond
		((<u32 i ilen)
		 (u32vref vec i))
		((not (js-isindex? i))
		 (set! p (js-toname p %this))
		 (call-next-method))
		((and (js-object-mode-holey? o)
		      (<u32 i (fixnum->uint32 (vector-length vec))))
		 (if (>=u32 i length)
		     (call-next-method)
		     (let ((v (u32vref vec i)))
			(if (js-absent? v)
			    (call-next-method)
			    v))))
		(else
		 (call-next-method)))))))

;*---------------------------------------------------------------------*/
;*    js-get-fixnum ...                                                */
;*---------------------------------------------------------------------*/
(define (js-get-fixnum arr::JsArray idx::long %this)
   (cond
      ((not (js-object-mode-plain? arr))
       (js-get arr idx %this))
      ((cond-expand
	  ((or bint30 bint32) #f)
	  (else (>=fx idx (-fx (bit-lsh 1 32) 1))))
       (js-get arr idx %this))
      (else
       (let loop ((o (js-object-proto arr)))
	  (cond
	     ((not (js-object-mode-hasnumeralprop? o))
	      (let ((__proto__ (js-object-proto o)))
		 (if (eq? __proto__ '())
		     (js-undefined)
		     (loop __proto__))))
	     ((js-array? o)
	      (js-array-fixnum-ref o idx %this))
	     (else
	      (js-get o idx %this)))))))

;*---------------------------------------------------------------------*/
;*    js-has-fixnum-property ...                                       */
;*    -------------------------------------------------------------    */
;*    This function is used when extending an array. If the            */
;*    prototype type contains a property whose name matches the        */
;*    index, and if that property is a setter, that setter must        */
;*    be used instead of extending the array.                          */
;*---------------------------------------------------------------------*/
(define (js-has-fixnum-property arr::JsArray idx::long %this)
   (with-access::JsObject arr (__proto__)
      (let loop ((o (js-object-proto arr)))
	 (cond
	    ((not (js-object-mode-hasnumeralprop? o))
	     (let ((__proto__ (js-object-proto o)))
		(if (eq? __proto__ '())
		    #f
		    (loop __proto__))))
	    ((js-array? o)
	     (with-access::JsArray o (vec ilen)
		(cond
		   ((>=u32 (fixnum->uint32 idx) (js-array-length o))
		    (let ((__proto__ (js-object-proto o)))
		       (unless (eq? __proto__ '())
			  (loop __proto__))))
		   ((<fx idx (vector-length vec))
		    (not (js-absent? (vector-ref vec idx))))
		   (else
		    (let ((__proto__ (js-object-proto o)))
		       (unless (eq? __proto__ '())
			  (loop __proto__)))))))
	    (else
	     (js-has-property o idx %this))))))

;*---------------------------------------------------------------------*/
;*    js-get-jsobject-name/cache-miss ...                              */
;*---------------------------------------------------------------------*/
(define-method (js-get-jsobject-name/cache-miss o::JsArray p::obj
		  throw::bool %this::JsGlobalObject cache::JsPropertyCache)
   (with-access::JsArray o (vec ilen length)
      (let ((i::uint32 ((@ js-toindex __hopscript_public) p)))
	 (cond
	    ((<u32 i ilen)
	     (u32vref vec i))
	    ((not (js-isindex? i))
	     (set! p (js-toname p %this))
	     (if (eq? p (& "length"))
		 (js-uint32-tointeger length)
		 (call-next-method)))
	    ((and (js-object-mode-holey? o)
		  (<u32 i (fixnum->uint32 (vector-length vec))))
	     (let ((v (u32vref vec i)))
		(if (js-absent? v)
		    (call-next-method)
		    v)))
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-array-inline-value-descriptor? ...                            */
;*---------------------------------------------------------------------*/
(define (js-array-inline-value-descriptor?::bool desc)
   (when (isa? desc JsValueDescriptor)
      (with-access::JsValueDescriptor desc (enumerable writable configurable)
	 (and (eq? enumerable #t)
	      (eq? writable #t)
	      (eq? configurable #t)))))

;*---------------------------------------------------------------------*/
;*    vector-fill-properties! ...                                      */
;*---------------------------------------------------------------------*/
(define (vector-fill-properties! obj::JsArray nvec)
   (with-access::JsArray obj (vec ilen)
      (let loop ((i 0))
	 (when (<fx i (uint32->fixnum ilen))
	    (let* ((v (vector-ref vec i))
		   (desc (instantiate::JsValueDescriptor
			    (name (js-integer-name->jsstring i))
			    (value v)
			    (writable #t)
			    (enumerable #t)
			    (configurable #t))))
	       (vector-set! vec i (js-absent))
	       (vector-set! nvec i desc)
	       (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    uninline-array! ...                                              */
;*---------------------------------------------------------------------*/
(define (uninline-array! arr::JsArray %this::JsGlobalObject)
   (when (js-object-mode-inline? arr)
      ;; this function switches from a fast inlined array representation
      ;; to a slow inefficient object representation
      (with-access::JsArray arr (vec ilen elements)
	 (js-object-mode-inline-set! arr #f)
	 (js-object-mode-holey-set! arr #t)
	 (js-array-update-length-property! arr)
	 (when (>fx (vector-length vec) 0)
	    (let* ((len (vector-length elements))
		   (nvec (make-vector (+fx len (uint32->fixnum ilen)))))
	       (vector-fill-properties! arr nvec)
	       (vector-copy! nvec (uint32->fixnum ilen) elements 0)
	       (set! elements nvec)
	       (set! ilen #u32:0)
	       (js-array-mark-invalidate!)))
	 arr)))

;*---------------------------------------------------------------------*/
;*    vector-remq! ...                                                 */
;*---------------------------------------------------------------------*/
(define (vector-remq! el vec)
   (let loop ((i (-fx (vector-length vec) 1)))
      (cond
	 ((=fx i -1)
	  vec)
	 ((eq? (vector-ref vec i) el)
	  (vector-copy! vec i vec (+fx i 1))
	  (vector-shrink! vec (-fx (vector-length vec) 1)))
	 (else
	  (loop (-fx i 1))))))

;*---------------------------------------------------------------------*/
;*    reinline-array! ...                                              */
;*    -------------------------------------------------------------    */
;*    When the inline portion of an array is expanded it might         */
;*    reach a non inline property that after expansion can be          */
;*    included in the inline part. This is handled by this function.   */
;*---------------------------------------------------------------------*/
(define (reinline-array! o::JsArray nilen %this)
   (with-access::JsArray o (vec ilen length cmap elements)
      (if (=u32 ilen length)
	  (js-object-mode-inline-set! o #t)
	  (js-object-mode-holey-set! o #t))
      (let ((len (minu32 (fixnum->uint32 (vector-length vec)) length)))
	 (let loop ((i nilen))
	    (if (<u32 i len)
		(let ((d (js-get-own-property o (js-toname i %this) %this)))
		   (if (js-array-inline-value-descriptor? d)
		       (if (not (eq? cmap (js-not-a-cmap)))
			   (error "reinilne-array!" "array cmap not implemented" i)
			   (begin
			      (set! elements (vector-remq! d elements))
			      (with-access::JsValueDescriptor d (value)
				 (vector-set! vec (uint32->fixnum i) value)
				 (loop (+u32 i 1)))))
		       (set! ilen i)))
		(set! ilen i))))))

;*---------------------------------------------------------------------*/
;*    js-array-holey-elements-length ...                               */
;*---------------------------------------------------------------------*/
(define (js-array-holey-elements-length obj::JsArray)
   (with-access::JsArray obj (vec ilen)
      (let loop ((i (-fx (vector-length vec) 1))
		 (acc 0))
	 (if (<fx i 0)
	     acc
	     (let ((v (vector-ref vec i)))
		(if (js-absent? v)
		    (loop (-fx i 1) acc)
		    (loop (-fx i 1) (+fx acc 1))))))))

;*---------------------------------------------------------------------*/
;*    vector-fill-holey-properties ...                                 */
;*---------------------------------------------------------------------*/
(define (vector-fill-holey-properties obj::JsArray nvec end)
   (with-access::JsArray obj (vec ilen)
      (let loop ((i 0)
		 (j 0))
	 (when (<fx i end)
	    (let ((v (vector-ref vec j)))
	       (if (js-absent? v)
		   (loop i (+fx j 1))
		   (let ((desc (instantiate::JsValueDescriptor
				  (name (js-integer-name->jsstring j))
				  (value v)
				  (writable #t)
				  (enumerable #t)
				  (configurable #t))))
		      (vector-set! vec i (js-absent))
		      (vector-set! nvec i desc)
		      (loop (+fx i 1) (+fx j 1)))))))))

;*---------------------------------------------------------------------*/
;*    unholey-array! ...                                               */
;*---------------------------------------------------------------------*/
(define (unholey-array! arr::JsArray %this::JsGlobalObject)
   (when (js-object-mode-holey? arr)
      ;; this function switches from a fast holey array representation
      ;; to a slow inefficient object representation
      (with-access::JsArray arr (vec ilen elements)
	 (js-object-mode-inline-set! arr #f)
	 (js-object-mode-holey-set! arr #f)
	 (js-array-update-length-property! arr)
	 (set! ilen 0)
	 (when (>fx (vector-length vec) 0)
	    (let* ((len (vector-length elements))
		   (vlen (js-array-holey-elements-length arr))
		   (nvec (make-vector (+fx len vlen))))
	       (vector-fill-holey-properties arr nvec vlen)
	       (vector-copy! nvec vlen elements 0)
	       (set! elements nvec)
	       (js-array-mark-invalidate!)))
	 arr)))

;*---------------------------------------------------------------------*/
;*    js-put! ::JsArray ...                                            */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::JsArray p v throw %this)
   (js-array-put! o p v throw %this))

;*---------------------------------------------------------------------*/
;*    js-put/cache! ::JsArray ...                                      */
;*---------------------------------------------------------------------*/
(define-method (js-put/cache! o::JsArray prop v::obj throw::bool %this
		  #!optional (point -1) (cspecs '()) (src "") (cachefun #t))
   (if (eq? prop (& "length"))
       (js-put-length! o v throw #f %this)
       (js-array-set! o prop v throw %this)))

;*---------------------------------------------------------------------*/
;*    js-array-put! ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.5       */
;*---------------------------------------------------------------------*/
(define (js-array-put! o::JsArray p v throw %this)
   
   (define (aput! o::JsArray q::obj v)
      (when (js-jsstring? p) (js-profile-log-put p #f))
      (if (not (js-can-put o q %this))
	  ;; 1
	  (if throw 
	      (js-raise-type-error %this
		 (if (js-object-mode-extensible? o)
		     "Can't add property ~a"
		     "Can't add property ~a, object not extensible")
		 q)
	      (js-undefined))
	  (let ((owndesc (js-get-own-property o q %this)))
	     ;; 2
	     (if (js-is-data-descriptor? owndesc)
		 ;; 3
		 (if (js-isname? q (& "length") %this)
		     (let ((newdesc (duplicate::JsValueDescriptor owndesc
				       (value v))))
			(js-array-mark-invalidate!)
			(js-array-update-length-property! o)
			(js-define-own-property o q newdesc throw %this))
		     (with-access::JsValueDescriptor owndesc ((valuedesc value))			
			(set! valuedesc v)
			(js-define-own-property o q owndesc throw %this)))
		 (let ((desc (js-get-property o q %this)))
		    ;; 4
		    (if (js-is-accessor-descriptor? desc)
			;; 5
			(js-property-value-set! o o q desc v %this)
			(let ((newdesc (instantiate::JsValueDescriptor
					  (name q)
					  (value v)
					  (writable #t)
					  (enumerable #t)
					  (configurable #t))))
			   ;; 6
			   (js-define-own-property o q newdesc throw %this)))))
	     v)))

   (define (property-check-and-setter arr::JsArray idx::uint32 %this)
      (when (js-has-fixnum-property arr (js-uint32-tointeger idx) %this)
	 (let ((desc (js-get-property arr (js-uint32-tointeger idx) %this)))
	    (cond
	       ((isa? desc JsAccessorDescriptor)
		desc)
	       ((isa? desc JsDataDescriptor)
		(with-access::JsDataDescriptor desc (writable)
		   (unless writable
		      (js-raise-type-error %this
			 "Cannot assign to read only property \"~a\" array" idx)
		      #f)))))))

   (with-access::JsArray o (vec ilen length)
      (let ((idx::uint32 (js-toindex p)))
	 (cond
	    ((<u32 idx ilen)
	     (vector-set! vec (uint32->fixnum idx) v)
	     v)
	    ((and (js-isindex? idx) (property-check-and-setter o idx %this))
	     =>
	     (lambda (desc)
		(if (>=u32 idx length)
		    (if (array-extensible? o)
			(set! length (+u32 idx #u32:1))
			(if throw
			    (js-raise-type-error %this
			       "Cannot add property ~a, object is not extensible" p)
			    v)))
		(uninline-array! o %this)
		(js-property-value-set! o o idx desc v %this)))
	    ((<u32 idx (fixnum->uint32 (vector-length vec)))
	     (js-object-mode-hasnumeralprop-set! o #t)
	     (with-access::JsArray o (length)
		(let loop ()
		   (cond
		      ((and (>=u32 idx length) (not (array-extensible? o)))
		       (if throw
			   (js-raise-type-error %this
			      "Cannot add property ~a, object is not extensible" p)
			   v))
		      ((and (=u32 idx ilen) (js-object-mode-inline? o))
		       (vector-set! vec (uint32->fixnum idx) v)
		       (let ((nilen (+u32 ilen #u32:1)))
			  (set! ilen nilen)
			  (when (>=u32 idx length)
			     (set! length nilen)))
		       v)
		      ((js-object-mode-holey? o)
		       (js-object-mode-inline-set! o #f)
		       (vector-set! vec (uint32->fixnum idx) v)
		       (when (>=u32 idx length)
			  (set! length (+u32 idx #u32:1)))
		       v)
		      ((js-object-mode-inline? o)
		       (js-object-mode-inline-set! o #f)
		       (js-object-mode-holey-set! o #t)
		       (loop))
		      (else
		       (aput! o (js-toname p %this) v))))))
	    ((and (=u32 idx (fixnum->uint32 (vector-length vec)))
		  (array-extensible? o)
		  ;; MS: 21 aug 2020
		  ;; used to be (js-object-mode-inline? o) only
		  (or (js-object-mode-inline? o) (js-object-mode-holey? o)))
	     (js-object-mode-hasnumeralprop-set! o #t)
	     ;; extend the inlined vector
	     (with-access::JsArray o (length vec ilen)
		;; use max when vector-length == 0
		(let* ((len (vector-length vec))
		       (nlen (max (*fx len 2) (+fx (uint32->fixnum idx) 1)))
		       (nvec (copy-vector-fill! vec nlen (js-absent))))
		   (cond-expand (profile (profile-vector-extension nlen len)))
		   (when ($jsobject-vector-inline? o)
		      (vector-fill! vec #unspecified))
		   (set! vec nvec))
		(vector-set! vec (uint32->fixnum idx) v)
		(if (=u32 ilen idx)
		    (set! ilen (+u32 idx #u32:1))
		    (begin
		       (js-object-mode-inline-set! o #f)
		       (js-object-mode-holey-set! o #t)))
		(when (>=u32 idx length)
		   (set! length (+u32 idx #u32:1)))
		v))
	    ((and (js-isindex? idx)
		  (array-extensible? o)
		  (js-object-mode-inline? o)
		  (<u32 idx (MAX-EXPANDABLE-ARRAY-SIZE)))
	     (let* ((len (vector-length vec))
		    (nlen (max (*fx len 2) (+fx (uint32->fixnum idx) 1)))
		    (nvec (copy-vector-fill! vec nlen (js-absent))))
		 (cond-expand (profile (profile-vector-extension nlen len)))
		 (with-access::JsArray o (vec)
		    (set! vec nvec))
		 (js-array-put! o p v throw %this)))
	    ((and (js-object-mode-plain? o)
		  (js-isname? p (& "length") %this)
		  (js-isindex? idx))
	     (let ((vu (->uint32 v)))
		(if (>=u32 vu length)
		    (let ((nv (js-tonumber v %this)))
		       (if (=uint32 vu nv)
			   (js-array-put-length! o vu)
			   (js-raise-range-error %this
			      "Illegal length: ~s" (js-tostring v %this))))
		    (aput! o (js-toname p %this) v))))
	    (else
	     (aput! o (js-toname p %this) v))))))

;*---------------------------------------------------------------------*/
;*    array-extensible? ...                                            */
;*---------------------------------------------------------------------*/
(define (array-extensible? o::JsArray)
   (when (js-object-mode-extensible? o)
      (let ((p (js-array-find-length-property o)))
	 (or (not p)
	     (with-access::JsValueDescriptor p (writable)
		writable)))))

;*---------------------------------------------------------------------*/
;*    array-shrinkable? ...                                            */
;*---------------------------------------------------------------------*/
(define (array-shrinkable? o::JsArray)
   (let ((p (js-array-find-length-property o)))
      (or (not p)
	  (with-access::JsValueDescriptor p (writable)
	     writable))))

;*---------------------------------------------------------------------*/
;*    js-put-length! ...                                               */
;*---------------------------------------------------------------------*/
(define-method (js-put-length! o::JsArray v::obj throw::bool cache %this)
   (js-array-put! o (& "length") v throw %this)
   (with-access::JsArray o (length ilen)
      (let ((len (->uint32 v)))
	 (set! length len)
	 (when (<u32 len ilen)
	    (js-array-mark-invalidate!)
	    (set! ilen len))
	 (%assert-array! o "js-put-length!"))))

;*---------------------------------------------------------------------*/
;*    js-put-length! ...                                               */
;*---------------------------------------------------------------------*/
(define-method (js-put-length! o::JsArray v::obj throw::bool cache %this)
   (let ((vu (->uint32 v)))
      (with-access::JsArray o (length ilen)
	 (if (>=u32 vu length)
	     (js-array-put-length! o (->uint32 v))
	     (js-array-put! o (& "length") v throw %this)))))

;*---------------------------------------------------------------------*/
;*    js-array-put-length! ...                                         */
;*---------------------------------------------------------------------*/
(define (js-array-put-length! o::JsArray len::uint32)
   (with-access::JsArray o (length ilen)
      (set! length len)
      (cond
	 ((<u32 len ilen)
	  (js-array-mark-invalidate!)
	  (set! ilen len))
	 ((js-object-mode-inline? o)
	  (js-object-mode-inline-set! o #f)
	  (js-object-mode-holey-set! o #t)))
      (%assert-array! o "js-put-length!")))
   
;*---------------------------------------------------------------------*/
;*    js-delete! ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (js-delete! o::JsArray p throw %this)
   (with-access::JsArray o (vec length ilen)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((and (<u32 i ilen) (js-object-mode-inline? o))
	     (unless (js-object-mode-frozen? o)
		(js-array-mark-invalidate!)
		(js-object-mode-inline-set! o #f)
		(js-object-mode-holey-set! o #t)
		(u32vset! vec i (js-absent))
		(set! ilen i)
		#t))
	    ((js-isname? p (& "length") %this)
	     #f)
	    (else
	     (when (<u32 i (fixnum->uint32 (vector-length vec)))
		(js-array-mark-invalidate!)
		(u32vset! vec i (js-absent))
		(when (<u32 i ilen) (set! ilen i)))
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    string->uint32 ...                                               */
;*---------------------------------------------------------------------*/
(define (string->uint32 n)
   (let ((n (string->number n)))
      (cond
	 ((fixnum? n) (fixnum->uint32 n))
	 ((bignum? n) (elong->uint32 (bignum->elong n)))
	 ((elong? n) (elong->uint32 n))
	 ((llong? n) (llong->uint32 n)))))

;*---------------------------------------------------------------------*/
;*    expandable-array ...                                             */
;*---------------------------------------------------------------------*/
(define (expandable-array vec::JsArray index::uint32 len::uint32)
   ;; Check is an inline array can be expanded based
   ;; on a simple heuristic.
   (when (or (js-array-inlined? vec) (js-object-mode-holey? vec))
      ;; the vector is inlined, make the real check
      (when (<u32 index (MAX-EXPANDABLE-ARRAY-SIZE))
	 (cond
	    ((=u32 index #u32:0)
	     #u32:2)
	    ((<u32 (*u32 #u32:2 index)
		(/u32  (MAX-EXPANDABLE-ARRAY-SIZE) #u32:2))
	     (*u32 index #u32:2))
	    ((<u32 (+u32 (MAX-EXPANDABLE-ARRAY-SIZE/8) index)
		(MAX-EXPANDABLE-ARRAY-SIZE))
	     (+u32 index (MAX-EXPANDABLE-ARRAY-SIZE/8)))
	    (else
	     (MAX-EXPANDABLE-ARRAY-SIZE))))))

;*---------------------------------------------------------------------*/
;*    js-define-own-property ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.5.1     */
;*---------------------------------------------------------------------*/
(define-method (js-define-own-property a::JsArray p desc throw %this::JsGlobalObject)
   
   (define rejected #f)
   
   (define (reject fmt)
      (if throw
	  (js-raise-type-error %this fmt p)
	  (set! rejected #t)))
   
   (define (newwritable! oldlendesc newlendesc)
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.5.1
      (or (not (isa? newlendesc JsValueDescriptor))
	  (with-access::JsValueDescriptor newlendesc (writable)
	     (let ((r (not (eq? writable #f))))
		(set! writable #t)
		r))))
   
   (define (js-array-property-names arr)
      (let loop ((o arr))
	 (with-access::JsObject o (cmap elements)
	    (append (if (not (eq? cmap (js-not-a-cmap)))
			(with-access::JsConstructMap cmap (props)
			   (map prop-name (vector->list props)))
			(map (lambda (d)
				(with-access::JsPropertyDescriptor d (name)
				   name))
			   (vector->list elements)))
	       (if (js-object? (js-object-proto o))
		   (loop (js-object-proto o))
		   '())))))
   
   (define (sym->string n)
      (cond
	 ((js-jsstring? n)
	  (js-jsstring->string n))
	 ((isa? n JsSymbolLiteral)
	  (with-access::JsSymbolLiteral n (val) (sym->string val)))
	 (else
	  (error "js-define-own-property" "bad property" n))))
   
   (define (delete-out-of-range! a newlendesc newwritable oldlen newlen::uint32 inlp)
      ;; delete all the properties that are at an index greater that the
      ;; new length
      ;; the cannonical implementation would be as follows:
      ;;    (let while ()
      ;;      ;; 3.l
      ;;      (when (< newlen oldlen)
      ;;         (set! oldlen (- oldlen 1))
      ;;         (let ((r (js-get-delete! a %this oldlen #f)))
      ;;            (if (eq? r #f)
      ;;                ;; 3.l.iii
      ;;                (with-access::JsValueDescriptor newlendesc
      ;;                      (value writable)
      ;;                   ;; 3.l.iii.1
      ;;                   (set! value (+ oldlen 1))
      ;;                   ;; 3.l.iii.2
      ;;                   (set! writable newwritable)
      ;;                   ;; 3.l.iii.3
      ;;                   (js-define-own-property% a "length" newlendesc #f)
      ;;                   ;; 3.l.iii.4
      ;;                   (reject))
      ;;                (while)))))
      ;; however, this implementation is not practical for sparse vectors
      (if inlp
	  ;; an inline vector
	  (with-access::JsArray a (vec length)
	     (let ((stop (uint32->fixnum newlen)))
		(let loop ((i (-fx (vector-length vec) 1)))
		   (if (>=fx i stop)
		       (begin
			  ;; 3.l
			  (vector-set! vec i (js-absent))
			  (loop (-fx i 1)))
		       (set! length newlen)))))
	  (for-each (lambda (name)
		       (unless rejected
			  (let ((num (string->uint32 (sym->string name))))
			     (when (and num (<=uint32 newlen num))
				(let ((r (js-delete! a name #f %this)))
				   (unless r
				      ;; 3.l.iii
				      (with-access::JsValueDescriptor newlendesc (value writable)
					 ;; 3.l.iii.1
					 (set! value (js-uint32-tointeger (+u32 num #u32:1)))
					 ;; 3.l.iii.2
					 (set! writable newwritable)
					 ;; 3.l.iii.3
					 (js-define-own-length% a
					    newlendesc #f %this)
					 ;; 3.l.iii.4
					 (reject (format "Cannot delete element ~a" num)))))))))
	     (sort (lambda (n1 n2)
		      (string>? (sym->string n1) (sym->string n2)))
		(js-array-property-names a)))))
   
   (define (js-define-own-length% a newlendesc throw %this)
      (with-access::JsArray a (length ilen elements)
	 (let ((old (vector-ref elements 0)))
	    (let ((r (js-define-own-property% a (& "length")
			newlendesc throw %this)))
	       (when r
		  (with-access::JsValueDescriptor old (value)
		     (let ((ulen (->uint32 value)))
			(set! length ulen)
			(cond
			   ((<u32 ulen ilen)
			    (set! ilen ulen))
			   ((>u32 ulen ilen)
			    (when (js-object-mode-inline? a)
			       (js-object-mode-holey-set! a #t)
			       (js-object-mode-inline-set! a #f)))))))
	       r))))
   
   (define (define-own-property-length oldlendesc)
      (with-access::JsValueDescriptor oldlendesc (value (owritable writable))
	 (let ((oldlen value))
	    (if (not (isa? desc JsValueDescriptor))
		;; 3.a
		(call-next-method)
		(with-access::JsValueDescriptor desc (value)
		   (let ((newlendesc (duplicate::JsValueDescriptor desc))
			 (newlen (js-touint32 value %this))
			 (nvalue (js-tonumber value %this)))
		      (unless (=uint32 newlen nvalue)
			 ;; 3.d
			 (js-raise-range-error %this
			    "Illegal length: ~s" (js-tostring value %this)))
		      (with-access::JsValueDescriptor newlendesc (value)
			 ;; 3.e
			 (set! value (js-uint32-tointeger newlen)))
		      (if (>=uint32 newlen oldlen)
			  ;; 3.f
			  (js-define-own-length% a newlendesc throw %this)
			  (if (not owritable)
			      ;; 3.g
			      (reject "property read-only \"~a\"")
			      ;; 3.h
			      (let* ((inlp (or (js-object-mode-inline? a)
					       (js-object-mode-holey? a)))
				     (deferredwritable (newwritable!
							  oldlendesc newlendesc))
				     (desc (js-define-own-length% a
					      newlendesc throw %this)))
				 (if (not desc)
				     ;; 3.k
				     desc
				     (begin
					;; 3.l
					(delete-out-of-range!
					   a newlendesc deferredwritable
					   oldlen newlen inlp)
					(unless rejected
					   ;; 3.m
					   (if (not deferredwritable)
					       ;; 3.m.i
					       (js-define-own-length% a
						  (instantiate::JsDataDescriptor
						     (name (& "length"))
						     (writable #f))
						  #f %this)
					       ;; 3.n
					       #t)))))))))))))
   
   (define (js-default-array-property! desc)
      (with-access::JsPropertyDescriptor desc (enumerable configurable)
	 ;; complete array property with default attribute value
	 (unless (boolean? configurable)
	    (set! configurable #t))
	 (unless (boolean? enumerable)
	    (set! enumerable #t))))
   
   (define (js-default-array-data-property! desc)
      (js-default-array-property! desc)
      (with-access::JsDataDescriptor desc (writable)
	 (unless (boolean? writable)
	    (set! writable #t))))
   
   (define (js-default-array-accessor-property! desc)
      (js-default-array-property! desc)
      (with-access::JsAccessorDescriptor desc (get set)
	 (unless (js-procedure? get) (set! get (js-undefined)))
	 (unless (js-procedure? set) (set! set (js-undefined)))))
   
   (define (js-default-array-generic-property! desc)
      (cond
	 ((isa? desc JsDataDescriptor) (js-default-array-data-property! desc))
	 ((isa? desc JsAccessorDescriptor) (js-default-array-accessor-property! desc))
	 (else (js-default-array-property! desc))))
   
   (define (js-define-own-property-array a p::uint32 desc throw)
      (with-access::JsArray a (vec ilen length)
	 [assert (p) (<u32 p (fixnum->uint32 (vector-length vec)))]
	 (cond
	    ((and (>=u32 p ilen) (not (js-object-mode-extensible? a)))
	     (let ((r (js-define-own-property% a
			 (js-toname p %this) desc #f %this)))
		(unless (js-array-inline-value-descriptor? desc)
		   (js-object-mode-plain-set! a #f)
		   (uninline-array! a %this)
		   (unholey-array! a %this))
		r))
	    ((isa? desc JsValueDescriptor)
	     (with-access::JsValueDescriptor desc (value)
		(cond
		   ((not (js-array-inline-value-descriptor? desc))
		    (uninline-array! a %this)
		    (unholey-array! a %this)
		    (js-object-mode-plain-set! a #f)
		    (js-define-own-property% a (js-toname p %this) desc #f %this))
		   ((<u32 p ilen)
		    (u32vset! vec p value))
		   ((=u32 p ilen)
		    (u32vset! vec p value)
		    (reinline-array! a (+u32 ilen #u32:1) %this)
		    #t)
		   ((and (<u32 p (fixnum->uint32 (vector-length vec)))
			 (js-object-mode-holey? a))
		    (uninline-array! a %this)
		    (u32vset! vec p value)
		    #t)
		   (else
		    ;; MS: 22 feb 2017
		    (cond
		       ((js-object-mode-inline? a)
			(uninline-array! a %this))
		       ((js-object-mode-holey? a)
			(unholey-array! a %this)))
		    (js-object-mode-plain-set! a #f)
		    (js-define-own-property% a (js-toname p %this) desc #f %this)))))
	    ((isa? desc JsAccessorDescriptor)
	     (uninline-array! a %this)
	     (unholey-array! a %this)
	     (js-object-mode-plain-set! a #f)
	     (js-define-own-property% a (js-toname p %this) desc #f %this))
	    (else
	     (uninline-array! a %this)
	     (unholey-array! a %this)
	     (js-object-mode-plain-set! a #f)
	     (js-define-own-property% a (js-toname p %this) desc #f %this)))))

   (if (js-isname? p (& "length") %this)
       ;; 3
       (define-own-property-length (js-get-own-property a p %this))
       (let ((index::uint32 (js-toindex p)))
	  (if (js-isindex? index)
	      ;; 4
	      (with-access::JsArray a (vec ilen length)
		 (js-object-mode-hasnumeralprop-set! a #t)
		 (let* ((oldlen length)
			(oldlendesc (js-array-find-length-property a))
			(writable (if oldlendesc
				      (with-access::JsValueDescriptor oldlendesc (writable)
					 writable)
				      #t)))
		    (if (and (>=uint32 index oldlen) (not (eq? writable #t)))
			;; 4.b
			(reject "wrong index ~a")
			;; 4.c
			(let ((s (cond
				    ((<u32 index (u32vlen vec))
				     ;; fast access, inline vector
				     (js-define-own-property-array
					a index desc #f))
				    ((expandable-array a index (u32vlen vec))
				     ;; expand the vector
				     =>
				     (lambda (len)
					(let* ((olen (vector-length vec))
					       (nlen (uint32->fixnum len))
					       (nvec (copy-vector-fill! vec nlen (js-absent))))
					   (cond-expand
					      (profile (profile-vector-extension
							  nlen olen)))
					   (when ($jsobject-vector-inline? a)
					      (vector-fill! vec #unspecified))
					   (set! vec nvec))
					(js-define-own-property-array
					   a index desc #f)))
				    (else
				     ;; slow access
				     (uninline-array! a %this)
				     (unholey-array! a %this)
				     (js-object-mode-plain-set! a #f)
				     (js-define-own-property%
					a (js-toname p %this) desc #f
					%this)))))
			   (cond
			      ((not s)
			       (reject "wrong index \"~a\""))
			      ((>=uint32 index oldlen)
			       (let ((l (+u32 index #u32:1)))
				  ;; 4.e.i,
				  (set! length l)
				  ;; 4.e.ii
				  (when oldlendesc
				     (js-define-own-property a (& "length")
					oldlendesc #f %this)))
			       ;; 4.f
			       #t))))))
	      ;; 5
	      (begin
		 (js-object-mode-plain-set! a #f)
		 (call-next-method))))))
   
;*---------------------------------------------------------------------*/
;*    array-get-elements ...                                           */
;*---------------------------------------------------------------------*/
(define (array-get-elements arr start len %this)
   (let loop ((i (- len 1))
	      (acc '()))
      (if (< i start)
	  acc
	  (loop (- i 1) (cons (js-get arr i %this) acc)))))

;*---------------------------------------------------------------------*/
;*    array-prototype-iterator ...                                     */
;*---------------------------------------------------------------------*/
(define (array-prototype-iterator this proc t
	   array-iterator::procedure vector-iterator::procedure
	   %this::JsGlobalObject)
   ;; length must be evaluated before checking the function
   ;; see ch15/15.4/15.4.4/15.4.4.16/15.4.4.16-4-8.js
   (let ((o (js-toobject %this this)))
      (cond
	 ((not (js-array? o))
	  (let ((len (js-get-lengthu32 o %this)))
	     (if (not (js-procedure? proc))
		 (js-raise-type-error %this "Not a procedure ~s" proc)
		 (array-iterator this o len proc t #u32:0 %this))))
	 (else
	  [%assert-array! o "array-prototype-iterator"]
	  (if (not (js-procedure? proc))
	      (js-raise-type-error %this "Not a procedure ~s" proc)
	      (with-access::JsArray o (length vec ilen)
		 (if (js-array-inlined? o)
		     (vector-iterator this o length proc t #u32:0 %this)
		     (array-iterator this o length proc t #u32:0 %this))))))))

;*---------------------------------------------------------------------*/
;*    js-seal ::JsArray ...                                            */
;*---------------------------------------------------------------------*/
(define-method (js-seal o::JsArray obj)
   (js-object-mode-sealed-set! o #t)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    js-freeze ::JsArray ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-freeze o::JsArray obj)
   (js-object-mode-frozen-set! o #t)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    js-preventextensions ::JsArray ...                               */
;*---------------------------------------------------------------------*/
(define-method (js-preventextensions o::JsArray %this)
   (with-access::JsArray o (vec length)
      (when (<u32 length (fixnum->uint32 (vector-length vec)))
	 (vector-shrink! vec (uint32->fixnum length)))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    js-for-in ::JsArray ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-for-in o::JsArray proc %this)
   (with-access::JsArray o (vec ilen)
      (cond
	 ((js-array-inlined? o)
	  (let ((len ilen))
	     (let loop ((i #u32:0))
		(cond
		   ((<u32 i len)
		    (let ((key (or (js-index-name (uint32->fixnum i))
				   (js-integer->jsstring (uint32->fixnum i)))))
		       (proc key %this))
		    (loop (+u32 i #u32:1)))
		   (else
		    (call-next-method))))))
	 ((js-object-mode-holey? o)
	  (let ((len ilen)
		(vlen (fixnum->uint32 (vector-length vec))))
	     (let loop ((i #u32:0))
		(cond
		   ((<u32 i len)
		    (proc (js-integer->jsstring (uint32->fixnum i)) %this)
		    (loop (+u32 i #u32:1)))
		   ((js-object-mode-inline? o)
		    (call-next-method))
		   ((<u32 i vlen)
		    (unless (js-absent? (u32vref vec i))
		       (proc (js-integer->jsstring (uint32->fixnum i)) %this))
		    (loop (+u32 i #u32:1)))
		   (else
		    (call-next-method))))))
	 (else
	  (call-next-method)))))
  
;*---------------------------------------------------------------------*/
;*    js-for-of ::JsArray ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-for-of o::JsArray proc close %this)
   
   (define cmap-fast-forof #f)
   
   (define (vector-forof o len::uint32 proc i::uint32)
      (with-access::JsArray o (vec ilen)
	 (let loop ((i i))
	    (cond
	       ((>=u32 i ilen)
		(if (js-array-inlined? o)
		    (js-undefined)
		    (array-forof o len proc i)))
	       (else
		(proc (vector-ref vec (uint32->fixnum i)) %this)
		(loop (+u32 i 1)))))))
   
   (define (array-forof o len proc i::uint32)
      (let loop ((i i))
	 (when (<u32 i len)
	    (let ((pv (js-get-property-value o o i %this)))
	       (proc (if (js-absent? pv) (js-undefined) pv) %this)
	       (loop (+u32 i 1))))))
   
   (with-access::JsGlobalObject %this (js-symbol-iterator js-array-pcache)
      (with-access::JsArray o (cmap length vec ilen)
	 (if (eq? cmap (js-pcache-pmap (js-pcache-ref js-array-pcache 20)))
	     (vector-forof o length proc #u32:0)
	     (let ((fun (js-get-jsobject-name/cache o js-symbol-iterator #f %this
			   (js-pcache-ref js-array-pcache 19))))
		(if (and (js-function? fun)
			 (with-access::JsFunction fun (info)
			    (not (string=? (vector-ref info 0) "@@iterator"))))
		    (js-for-of-iterator (js-call0 %this fun o) o proc close %this)
		    (if (js-array-inlined? o)
			(with-access::JsPropertyCache (js-pcache-ref js-array-pcache 20) (pmap)
			   (set! pmap cmap)
			   (vector-forof o length proc #u32:0))
			(array-forof o length proc #u32:0))))))))

;*---------------------------------------------------------------------*/
;*    js-array-prototype-concat1 ...                                   */
;*---------------------------------------------------------------------*/
(define (js-array-prototype-concat1 this arg %this)
   (with-access::JsArray this ((tvec vec) (tilen ilen))
      (with-access::JsArray arg ((avec vec) (ailen ilen))
	 (let* ((new-len (+fx/overflow (uint32->fixnum tilen)
			    (uint32->fixnum ailen)))
		(arr (js-array-species-create %this this new-len)))
	    (with-access::JsArray arr ((vdst vec) ilen)
	       (vector-copy! vdst 0
		  tvec 0 (uint32->fixnum tilen))
	       (vector-copy! vdst (uint32->fixnum tilen)
		  avec 0 (uint32->fixnum ailen))
	       (set! ilen (uint32->fixnum (+u32 tilen ailen))))
	    arr))))

;*---------------------------------------------------------------------*/
;*    js-array-concat1 ...                                             */
;*---------------------------------------------------------------------*/
(define (js-array-concat1 this::JsArray arg %this cache)
   (if (and (js-object-mode-plain? this)
	    (js-object-mode-inline? this)
	    (js-object-mode-plain? arg)
	    (js-object-mode-inline? arg))
       (js-array-prototype-concat1 this arg %this)
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call1 %this
	     (js-get-jsobject-name/cache this (& "concat") #f %this
		(or cache (js-pcache-ref js-array-pcache 15)))
	     this arg))))

;*---------------------------------------------------------------------*/
;*    js-array-maybe-concat1 ...                                       */
;*---------------------------------------------------------------------*/
(define (js-array-maybe-concat1 this::obj arg %this cache)
   (if (and (js-array? this) (js-array? arg))
       (js-array-concat1 this arg %this cache)
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call1 %this
	     (js-get-name/cache this (& "concat") #f %this
		(or cache (js-pcache-ref js-array-pcache 16)))
	     this arg))))

;*---------------------------------------------------------------------*/
;*    js-array-prototype-join ...                                      */
;*---------------------------------------------------------------------*/
(define (js-array-prototype-join o::JsArray separator %this)
   
   (define (el->jsstring el)
      (if (or (eq? el (js-undefined)) (eq? el (js-null)))
	  (& "")
	  (js-tojsstring el %this)))
   
   (let* ((lenval::uint32 (js-get-lengthu32 o %this))
	  (sep (if (eq? separator (js-undefined))
		   (& ",")
		   (js-tojsstring separator %this))))
      (cond
	 ((=u32 lenval #u32:0)
	  (& ""))
	 ((=u32 lenval #u32:1)
	  (el->jsstring (js-array-index-ref o (-u32 lenval #u32:1) %this)))
	 (else
	  (let loop ((r (js-jsstring-append sep
			   (el->jsstring
			      (js-array-index-ref o (-u32 lenval #u32:1)
				 %this))))
		     (i (-u32 lenval #u32:2)))
	     (if (=u32 i 0)
		 (let* ((v0 (js-array-index-ref o #u32:0 %this))
			(el0 (el->jsstring v0)))
		    (js-jsstring-append el0 r))
		 (let ((v (js-array-index-ref o i %this)))
		    (loop (js-jsstring-append sep
			     (js-jsstring-append (el->jsstring v) r))
		       (-u32 i #u32:1)))))))))

;*---------------------------------------------------------------------*/
;*    js-array-join ...                                                */
;*---------------------------------------------------------------------*/
(define (js-array-join this::JsArray separator %this cache)
   (if (js-object-mode-plain? this)
       (js-array-prototype-join this separator %this)
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call1 %this
	     (js-get-jsobject-name/cache this (& "join") #f %this
		(or cache (js-pcache-ref js-array-pcache 2)))
	     this separator))))

;*---------------------------------------------------------------------*/
;*    js-array-maybe-join ...                                          */
;*---------------------------------------------------------------------*/
(define (js-array-maybe-join this separator %this cache)
   (if (js-array? this)
       (js-array-join this separator %this cache)
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call1 %this
	     (js-get-name/cache this (& "join") #f %this
		(or cache (js-pcache-ref js-array-pcache 3)))
	     this separator))))

;*---------------------------------------------------------------------*/
;*    js-array-map-join ...                                            */
;*    -------------------------------------------------------------    */
;*    This function is called on patterns combining map and join as:   */
;*      arr.map( fun ).join( str )                                     */
;*    It does not construct the internal map array.                    */
;*---------------------------------------------------------------------*/
(define (js-array-map-join this::JsArray proc::procedure thisarg separator %this)
   
   (define (array-map/array o len proc t i::uint32 sep acc)
      (let loop ((i i)
		 (acc acc))
	 (if (<u32 i len)
	     (let ((pv (js-get-property-value o o i %this)))
		(if (js-absent? pv)
		    (loop (+u32 i 1) acc)
		    (let ((v (proc t pv (js-uint32-tointeger i) o %this)))
		       (loop (+u32 i 1)
			  (js-jsstring-append acc
			     (js-jsstring-append sep
				(js-tojsstring v %this)))))))
	     acc)))
   
   (define (vector-map o len::uint32 proc t sep)
      (with-access::JsArray o (vec ilen length)
	 (let ((v (& ""))
	       (l length))
	    (if (=u32 l #u32:0)
		v
		(let ((val (vector-ref vec 0)))
		   (let loop ((i #u32:1)
			      (acc (js-tojsstring (proc t val 0 o %this) %this)))
		      (if (or (>=u32 i ilen) (>=u32 i l))
			  (if (=u32 i len)
			      acc
			      (array-map/array o len proc t i sep acc))
			  (let* ((val (vector-ref vec (uint32->fixnum i)))
				 (v (proc t val (js-uint32-tointeger i) o %this)))
			     (loop (+u32 i 1)
				(js-jsstring-append acc
				   (js-jsstring-append sep
				      (js-tojsstring v %this))))))))))))

   (let ((sep (if (eq? separator (js-undefined))
		  (& ",")
		  (js-tojsstring separator %this))))
      (with-access::JsArray this (length)
	 (vector-map this length proc thisarg sep))))

;*---------------------------------------------------------------------*/
;*    js-array-maybe-map-join ...                                      */
;*---------------------------------------------------------------------*/
(define (js-array-maybe-map-join this proc thisarg separator %this cachem cachej)
   (if (and (js-array? this) (js-object-mode-plain? this))
       (js-array-map-join this proc thisarg separator %this)
       (js-array-maybe-join
	  (js-array-maybe-map-procedure this proc thisarg %this cachem)
	  separator %this cachej)))
   
;*---------------------------------------------------------------------*/
;*    js-array-prototype-fill ...                                      */
;*---------------------------------------------------------------------*/
(define (js-array-prototype-fill this::JsArray value start end %this)
   (let* ((len::uint32 (js-get-lengthu32 this %this))
	  (k (if (eq? start (js-undefined))
		 #u32:0
		 (let ((relstart (js-tointeger start %this)))
		    (if (< relstart 0)
			(let ((left (+ len relstart)))
			   (if (< left 0)
			       #u32:0
			       (js-touint32 left %this)))
			(minu32 (js-touint32 relstart %this) len)))))
	  (final (if (eq? end (js-undefined))
		     len
		     (let ((relend (js-tointeger end %this)))
			(if (< relend 0)
			    (let ((left (+ len relend)))
			       (if (< left 0)
				   #u32:0
				   (js-touint32 left %this)))
			    (minu32 (js-touint32 relend %this) len))))))
      (with-access::JsArray this (vec ilen length)
	 (cond
	    ((js-object-mode-inline? this)
	     (let loop ((i k))
		(if (<u32 i final)
		    (begin
		       (vector-set! vec (uint32->fixnum i) value)
		       (loop (+u32 i #u32:1)))
		    (when (>u32 i ilen)
		       (set! ilen i)))))
	    (else
	     (let loop ((i k))
		(if (<u32 i final)
		    (begin
		       (if (<u32 i ilen)
			   (vector-set! vec (uint32->fixnum i) value)    
			   (js-put! this i value #f %this))
		       (loop (+u32 i #u32:1)))
		    (when (>u32 i length)
		       (js-put-length! this (js-uint32-tointeger i) #f #f %this))))))
	 this)))

;*---------------------------------------------------------------------*/
;*    js-array-fill ...                                                */
;*---------------------------------------------------------------------*/
(define (js-array-fill this::JsArray value start end %this cache)
   (if (js-object-mode-plain? this)
       (js-array-prototype-fill this value start end %this)
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call3 %this
	     (js-get-name/cache this (& "fill") #f %this
		(or cache (js-pcache-ref js-array-pcache 4)))
	     this value start end))))

;*---------------------------------------------------------------------*/
;*    js-array-maybe-fill ...                                          */
;*---------------------------------------------------------------------*/
(define (js-array-maybe-fill this value start end %this cache)
   (if (js-array? this)
       (js-array-fill this value start end %this cache)
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call3 %this
	     (js-get-name/cache this (& "fill") #f %this
		(or cache (js-pcache-ref js-array-pcache 5)))
	     this value start end))))

;*---------------------------------------------------------------------*/
;*    js-array-prototype-foreach ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.18    */
;*---------------------------------------------------------------------*/
(define (js-array-prototype-foreach this proc t %this)
   
   (define (vector-foreach this o len::uint32 proc t i::uint32 %this)
      (if (js-object-mode-inline? o)
	  (with-access::JsArray o (vec ilen)
	     (let loop ((i i))
		(cond
		   ((>=u32 i ilen)
		    (js-undefined))
		   ((not (js-object-mode-inline? o))
		    (array-foreach this o len proc t i %this))
		   (else
		    (let ((v (vector-ref vec (uint32->fixnum i))))
		       (js-call3 %this proc t v (js-uint32-tointeger i) o)
		       (loop (+u32 i 1)))))))
	  (array-foreach this o len proc t i %this)))
   
   (define (array-foreach this o len proc t i::uint32 %this)
      (let loop ((i i))
	 (when (<u32 i len)
	    (let ((pv (js-get-property-value o o i %this)))
	       (unless (js-absent? pv)
		  (js-call3 %this proc t pv (uint32->fixnum i) o))
	       (loop (+u32 i 1))))))

   (array-prototype-iterator this proc t array-foreach vector-foreach %this)
   (js-undefined))
   
;*---------------------------------------------------------------------*/
;*    js-array-foreach ...                                             */
;*---------------------------------------------------------------------*/
(define (js-array-foreach this::JsArray proc thisarg %this cache)
   (if (js-object-mode-plain? this)
       (js-array-prototype-foreach this proc thisarg %this)
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call2 %this
	     (js-get-jsobject-name/cache this (& "forEach") #f %this
		(or cache (js-pcache-ref js-array-pcache 11)))
	     this proc thisarg))))

;*---------------------------------------------------------------------*/
;*    js-array-maybe-foreach ...                                       */
;*---------------------------------------------------------------------*/
(define (js-array-maybe-foreach this proc thisarg %this cache)
   (if (js-array? this)
       (js-array-foreach this proc thisarg %this cache)
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call2 %this
	     (js-get-name/cache this (& "forEach") #f %this
		(or cache (js-pcache-ref js-array-pcache 12)))
	     this proc thisarg))))

;*---------------------------------------------------------------------*/
;*    js-array-prototype-foreach-procedure ...                         */
;*---------------------------------------------------------------------*/
(define (js-array-prototype-foreach-procedure this::JsArray proc::procedure thisarg %this)
   
   (define (vector-foreach o len::uint32 proc thisarg i::uint32)
      [%assert-array! o "vector-foreach"]
      (with-access::JsArray o (vec ilen)
	 (let loop ((i i))
	    (cond
	       ((>=u32 i ilen)
		(js-undefined))
	       ((not (js-object-mode-inline? o))
		(array-foreach o len proc thisarg i))
	       (else
		(let ((v (vector-ref vec (uint32->fixnum i))))
		   (proc thisarg v (js-uint32-tointeger i) o %this)
		   (loop (+u32 i 1))))))))
   
   (define (array-foreach o len proc thisarg i::uint32)
      (let loop ((i i))
	 (when (<u32 i len)
	    (let ((pv (js-get-property-value o o i %this)))
	       (unless (js-absent? pv)
		  (proc thisarg pv (uint32->fixnum i) o %this))
	       (loop (+u32 i 1))))))

   (with-access::JsArray this (length)
      (vector-foreach this length proc thisarg #u32:0)))

;*---------------------------------------------------------------------*/
;*    js-array-foreach-procedure ...                                   */
;*---------------------------------------------------------------------*/
(define (js-array-foreach-procedure this::JsArray proc thisarg %this cache)
   (if (js-object-mode-plain? this)
       (js-array-prototype-foreach-procedure this proc thisarg %this)
       (let ((jsproc (js-make-function %this
			(lambda (_this x y z) (proc _this x y z %this))
			(js-function-arity 3 0)
			(js-function-info :name "forEachProc" :len 3)
			:constrsize 0
			:alloc js-object-alloc)))
	  (js-array-foreach this jsproc thisarg %this cache))))

;*---------------------------------------------------------------------*/
;*    js-array-maybe-foreach-procedure ...                             */
;*---------------------------------------------------------------------*/
(define (js-array-maybe-foreach-procedure this proc thisarg %this cache)
   (if (js-array? this)
       (js-array-foreach-procedure this proc thisarg %this cache)
       (let ((jsproc (js-make-function %this
			(lambda (_this x y z) (proc _this x y z %this))
			(js-function-arity 3 0)
			(js-function-info :name "forEachProc" :len 3)
			:constrsize 0
			:alloc js-object-alloc)))
	  (with-access::JsGlobalObject %this (js-array-pcache)
	     (js-call2 %this
		(js-get-name/cache this (& "forEach") #f %this
		   (or cache (js-pcache-ref js-array-pcache 12)))
		this jsproc thisarg)))))

;*---------------------------------------------------------------------*/
;*    js-array-prototype-map ...                                       */
;*    ------------------------------------------------------------     */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.19    */
;*---------------------------------------------------------------------*/
(define (js-array-prototype-map this proc thisarg %this)
   
   (define (array-map/array this o len proc thisarg i::uint32 a::JsArray %this)
      (let loop ((i i))
	 (if (<u32 i len)
	     (let ((pv (js-get-property-value o o i %this)))
		(if (js-absent? pv)
		    (with-access::JsArray a (ilen)
		       (when (js-object-mode-inline? a)
			  (js-object-mode-inline-set! a #f)
			  (if (>u32 i 0)
			      (set! ilen (-u32 i #u32:1))
			      (set! ilen #u32:0))))
		    (let ((v (js-call3 %this proc thisarg pv
				(js-uint32-tointeger i) o)))
		       (with-access::JsArray a (vec)
			  (if (<u32 i (fixnum->uint32 (vector-length vec)))
			      (vector-set! vec (uint32->fixnum i) v)
			      (js-define-own-property a
				 (js-uint32-tointeger i)
				 (instantiate::JsValueDescriptor
				    (name (number->string i))
				    (value v)
				    (enumerable #t)
				    (configurable #t)
				    (writable #t))
				 #f %this)))))
		(loop (+u32 i 1)))
	     a)))
   
   (define (vector-map this o len::uint32 proc thisarg i::uint32 %this)
      (with-access::JsArray o (vec ilen length)
	 (let ((v (js-create-vector (vector-length vec)))
	       (l length))
	    (let loop ((i i))
	       (cond
		  ((or (>=u32 i ilen) (>=u32 i l))
		   (let ((a (js-species->jsarray this v %this)))
		      (if (=u32 i len)
			  (with-access::JsArray a (length ilen)
			     (set! length len)
			     (set! ilen len)
			     a)
			  ;; the array has been uninlined by the callback
			  (array-map/array this o len proc thisarg i a %this))))
		  (else
		   (let ((val (vector-ref vec (uint32->fixnum i))))
		      (vector-set! v (uint32->fixnum i)
			 ((@ js-call3 __hopscript_public) %this proc thisarg val
			    (js-uint32-tointeger i) o))
		      (loop (+u32 i 1)))))))))
   
   (define (array-map this o len proc thisarg i::uint32 %this)
      (let ((a (js-array-construct/length %this (js-array-alloc %this)
		  (js-uint32-tointeger len))))
	 (array-map/array this o len proc thisarg i a %this)))
   
   (array-prototype-iterator this proc thisarg array-map vector-map %this))
   
;*---------------------------------------------------------------------*/
;*    js-array-map ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-array-map this::JsArray proc thisarg %this cache)
   (if (js-object-mode-plain? this)
       (js-array-prototype-map this proc thisarg %this)
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call2 %this
	     (js-get-jsobject-name/cache this (& "map") #f %this
		(or cache (js-pcache-ref js-array-pcache 11)))
	     this proc thisarg))))

;*---------------------------------------------------------------------*/
;*    js-array-maybe-map ...                                           */
;*---------------------------------------------------------------------*/
(define (js-array-maybe-map this proc thisarg %this cache)
   (if (js-array? this)
       (js-array-map this proc thisarg %this cache)
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call2 %this
	     (js-get-name/cache this (& "map") #f %this
		(or cache (js-pcache-ref js-array-pcache 12)))
	     this proc thisarg))))

;*---------------------------------------------------------------------*/
;*    js-array-prototype-map-procedure ...                             */
;*---------------------------------------------------------------------*/
(define (js-array-prototype-map-procedure this::JsArray proc::procedure thisarg %this)
   
   (define (array-map/array o len proc thisarg i::uint32 a::JsArray)
      (let loop ((i i))
	 (if (<u32 i len)
	     (let ((pv (js-get-property-value o o i %this)))
		(if (js-absent? pv)
		    (with-access::JsArray a (ilen)
		       (when (js-object-mode-inline? a)
			  (js-object-mode-inline-set! a #f)
			  (if (>u32 i 0)
			      (set! ilen (-u32 i #u32:1))
			      (set! ilen #u32:0))))
		    (let ((v (proc thisarg pv (js-uint32-tointeger i) o %this)))
		       (with-access::JsArray a (vec)
			  (if (<u32 i (fixnum->uint32 (vector-length vec)))
			      (vector-set! vec (uint32->fixnum i) v)
			      (js-define-own-property a
				 (js-uint32-tointeger i)
				 (instantiate::JsValueDescriptor
				    (name (number->string i))
				    (value v)
				    (enumerable #t)
				    (configurable #t)
				    (writable #t))
				 #f %this)))))
		(loop (+u32 i 1)))
	     a)))
   
   (define (vector-map o len::uint32 proc thisarg i::uint32)
      (with-access::JsArray o (vec ilen length)
	 (let ((v (js-create-vector (vector-length vec)))
	       (l length))
	    (let loop ((i i))
	       (cond
		  ((or (>=u32 i ilen) (>=u32 i l))
		   (let ((a (js-species->jsarray this v %this)))
		      (if (=u32 i len)
			  (with-access::JsArray a (length ilen)
			     (set! length len)
			     (set! ilen len)
			     a)
			  ;; the array has been uninlined by the callback
			  (array-map/array o len proc thisarg i a))))
		  (else
		   (let ((val (vector-ref vec (uint32->fixnum i))))
		      (vector-set! v (uint32->fixnum i)
			 (proc thisarg val (js-uint32-tointeger i) o %this))
		      (loop (+u32 i 1)))))))))
   
   (with-access::JsArray this (length)
      (vector-map this length proc thisarg #u32:0)))

;*---------------------------------------------------------------------*/
;*    js-array-map-procedure ...                                       */
;*---------------------------------------------------------------------*/
(define (js-array-map-procedure this::JsArray proc thisarg %this cache)
   (if (js-object-mode-plain? this)
       (js-array-prototype-map-procedure this proc thisarg %this)
       (let ((jsproc (js-make-function %this
			(lambda (_this x y z) (proc _this x y z %this))
			(js-function-arity 3 0)
			(js-function-info :name "mapProc" :len 3)
			:constrsize 0
			:alloc js-object-alloc)))
	  (js-array-map this jsproc thisarg %this cache))))

;*---------------------------------------------------------------------*/
;*    js-array-maybe-map-procedure ...                                 */
;*---------------------------------------------------------------------*/
(define (js-array-maybe-map-procedure this proc thisarg %this cache)
   (if (js-array? this)
       (js-array-map-procedure this proc thisarg %this cache)
       (let ((jsproc (js-make-function %this
			(lambda (_this x y z) (proc _this x y z %this))
			(js-function-arity 3 0)
			(js-function-info :name "mapProc" :len 3)
			:constrsize 0
			:alloc js-object-alloc)))
	  (with-access::JsGlobalObject %this (js-array-pcache)
	     (js-call2 %this
		(js-get-name/cache this (& "map") #f %this
		   (or cache (js-pcache-ref js-array-pcache 11)))
		this jsproc thisarg)))))

;*---------------------------------------------------------------------*/
;*    js-array-prototype-push ...                                      */
;*    -------------------------------------------------------------    */
;*    This is the method initial bound in Array.prototype.             */
;*---------------------------------------------------------------------*/
(define (js-array-prototype-push o::JsArray item %this::JsGlobalObject)
   (with-access::JsArray o (length ilen vec)
      (let ((n length))
	 (cond
	    ((not (array-extensible? o))
	     (js-raise-type-error %this
		"Can't add property ~a: object is not extensible" length))
	    ((and (=u32 n ilen)
		  (<u32 ilen (fixnum->uint32 (vector-length vec)))
		  (not (js-has-fixnum-property o (uint32->fixnum n) %this)))
	     (let ((idx (+u32 n 1)))
		(vector-set! vec (uint32->fixnum n) item)
		(set! ilen idx)
		(set! length idx)
		(js-uint32-tointeger idx)))
	    ((and (=fx (vector-length vec) 0)
		  (=uint32 n #u32:0)
		  (not (js-has-fixnum-property o 0 %this)))
	     (set! vec (js-create-vector (DEFAULT-EMPTY-ARRAY-SIZE)))
	     (set! ilen #u32:1)
	     (set! length #u32:1)
	     (vector-set! vec 0 item)
	     1)
	    (else
	     (if (<u32 length #u32:4294967295)
		 (js-array-put! o (uint32->fixnum n) item #f %this)
		 (js-put! o n item #f %this))
	     (if (=u32 (+u32 n #u32:1) #u32:0)
		 (js-raise-range-error %this
		    "Illegal length: ~s"
		    (js-tostring #l4294967296 %this))
		 (js-uint32-tointeger (+u32 #u32:1 n))))))))

;*---------------------------------------------------------------------*/
;*    js-array-push ...                                                */
;*---------------------------------------------------------------------*/
(define (js-array-push o::JsArray item %this::JsGlobalObject cache)
   (if (js-object-mode-plain? o)
       (js-array-prototype-push o item %this)
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call1 %this
	     (js-get-jsobject-name/cache o (& "push") #f %this
		(or cache (js-pcache-ref js-array-pcache 6)))
	     o item))))

;*---------------------------------------------------------------------*/
;*    js-array-maybe-push ...                                          */
;*---------------------------------------------------------------------*/
(define (js-array-maybe-push this item %this cache)
   (if (js-array? this)
       (js-array-push this item %this cache)
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call1 %this
	     (js-get-name/cache this (& "push") #f %this
		(or cache (js-pcache-ref js-array-pcache 7)))
	     this item))))

;*---------------------------------------------------------------------*/
;*    js-array-prototype-pop ...                                       */
;*---------------------------------------------------------------------*/
(define (js-array-prototype-pop o %this)
   (with-access::JsArray o (length ilen vec)
      (let ((len::uint32 (js-get-lengthu32 o %this)))
	 (cond
	    ((not (array-shrinkable? o))
	     (js-raise-type-error %this
		"Can't remove property ~a: length is read-only" length))
	    ((=u32 len #u32:0)
	     (js-put-length! o 0 #f #f %this)
	     (js-undefined))
	    ((=u32 length ilen)
	     (let* ((idx (-u32 len #u32:1))
		    (el (vector-ref vec (uint32->fixnum (-u32 ilen 1)))))
		(vector-set! vec (uint32->fixnum (-u32 ilen 1)) (js-absent))
		(set! length idx)
		(set! ilen idx)
		el))
	    (else
	     (let* ((idx (-u32 len #u32:1))
		    (el (js-get o (js-uint32-tointeger idx) %this)))
		(js-delete! o idx #t %this)
		(js-put-length! o (js-uint32-tointeger idx) #f #f %this)
		el))))))

;*---------------------------------------------------------------------*/
;*    js-array-pop ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-array-pop o::JsArray %this cache)
   (if (js-object-mode-plain? o)
       (js-array-prototype-pop o %this)
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call0 %this
	     (js-get-jsobject-name/cache o (& "pop") #f %this
		(or cache (js-pcache-ref js-array-pcache 8)))
	     o))))

;*---------------------------------------------------------------------*/
;*    js-array-maybe-pop ...                                           */
;*---------------------------------------------------------------------*/
(define (js-array-maybe-pop this %this cache)
   (if (js-array? this)
       (js-array-pop this %this cache)
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call0 %this
	     (js-get-name/cache this (& "pop") #f %this
		(or cache (js-pcache-ref js-array-pcache 9)))
	     this))))

;*---------------------------------------------------------------------*/
;*    js-array-prototype-slice ...                                     */
;*---------------------------------------------------------------------*/
(define (js-array-prototype-slice this::obj start end %this::JsGlobalObject)
   (if (and (js-array? this)
	    (js-object-mode-plain? this)
	    (js-object-mode-inline? this)
	    (fixnum? start))
       (cond
	  ((fixnum? end)
	   (js-array-inlined-slice2 this start end %this))
	  ((eq? end (js-undefined))
	   (with-access::JsArray this (ilen)
	      (js-array-inlined-slice2 this start (uint32->fixnum ilen) %this)))
	  (else
	   (js-array-slice this start end %this)))
       (js-array-slice this start end %this)))

;*---------------------------------------------------------------------*/
;*    js-array-slice ...                                               */
;*---------------------------------------------------------------------*/
(define (js-array-slice this::obj start end %this::JsGlobalObject)

   (define (vector-slice/vec! o val k::long final::long vec::vector)
      (let ((arr (js-species->jsarray this vec %this))
	    (len (vector-length vec)))
	 (let ((i (-fx len 1)))
	    (cond
	       ((=fx i -1)
		arr)
	       ((=fx i (-fx len 1))
		arr)
	       (else
		(js-put-length! arr i #f #f %this)
		arr)))))
   
   (define (vector-slice! o val::vector k::long final::long)
      (let ((vec (vector-copy val k final)))
	 (vector-slice/vec! o val k final vec)))
   
   (define (u8vector-slice! o val::u8vector k::long final::long)
      (let ((vec (js-create-vector (-fx final k))))
	 (let loop ((i k)
		    (j 0))
	    (if (=fx i final)
		(vector-slice/vec! o val k final vec)
		(begin
		   (vector-set! vec j
		      (uint8->fixnum (u8vector-ref val i)))
		   (loop (+fx i 1) (+fx j 1)))))))
   
   (define (string-slice! o val::bstring k::long final::long)
      (let ((vec ($create-vector (-fx final k))))
	 (let loop ((i k)
		    (j 0))
	    (if (=fx i final)
		(vector-slice/vec! o val k final vec)
		(begin
		   (vector-set! vec j
		      (char->integer (string-ref-ur val i)))
		   (loop (+fx i 1) (+fx j 1)))))))
   
   (define (array-copy! o len::long arr k::obj final::obj)
      (let loop ((i len))
	 (cond
	    ((= k final)
	     (js-put-length! arr i #f #f %this)
	     arr)
	    ((eq? (js-get-property o (js-toname k %this) %this) (js-undefined))
	     (set! k (+ 1 k))
	     (loop (+fx i 1)))
	    (else
	     (js-put! arr i (js-get o k %this) #f %this)
	     (set! k (+ 1 k))
	     (loop (+fx i 1))))))
   
   (define (array-slice! o k::obj final::obj)
      (let ((arr (js-array-construct/length %this (js-array-alloc  %this)
		    (- final k))))
	 (array-copy! o 0 arr k final)))

   (let* ((o (js-toobject %this this))
	  (len (js-uint32-tointeger (js-get-lengthu32 o %this)))
	  (relstart (js-tointeger start %this))
	  (k (if (< relstart 0) (max (+ len relstart) 0) (min relstart len)))
	  (relend (if (eq? end (js-undefined)) len (js-tointeger end %this)))
	  (final (if (< relend 0) (max (+ len relend) 0) (min relend len))))
      (cond
	 ((<= final k)
	  (js-empty-vector->jsarray %this))
	 ((isa? o JsTypedArray)
	  (with-access::JsTypedArray o (%data byteoffset)
	     (cond
		((string? %data)
		 (let* ((offset (uint32->fixnum byteoffset))
			(start (+fx offset (->fixnum k)))
			(end (+fx offset final))
			(vlen (string-length %data)))
		    (cond
		       ((<= end vlen)
			(string-slice! o %data start end))
		       ((>fx vlen 0)
			(let* ((arr (string-slice! o %data start vlen))
			       (vlen (->fixnum (js-get-length arr %this))))
			   (array-copy! o vlen arr (- len vlen) end)))
		       (else
			(array-slice! o start end)))))
		((u8vector? %data)
		 (let* ((offset (uint32->fixnum byteoffset))
			(start (+fx offset (->fixnum k)))
			(end (+fx offset final))
			(vlen (u8vector-length %data)))
		    (cond
		       ((<= end vlen)
			(u8vector-slice! o %data start end))
		       ((>fx vlen 0)
			(let* ((arr (u8vector-slice! o %data start vlen))
			       (vlen (->fixnum (js-get-length arr %this))))
			   (array-copy! o vlen arr (- len vlen) end)))
		       (else
			(array-slice! o start end)))))
		(else
		 (array-slice! o k final)))))
	 ((not (js-array? o))
	  (if (and (isa? o JsArguments) (js-object-mode-inline? o))
	      (with-access::JsArguments o (vec)
		 (let ((vlen (vector-length vec)))
		    (cond
		       ((<= final vlen)
			(vector-slice! o vec (->fixnum k) (->fixnum final)))
		       ((>fx vlen 0)
			(let* ((arr (vector-slice! o vec (->fixnum k) vlen))
			       (vlen (->fixnum (js-get-length arr %this))))
			   (array-copy! o vlen arr (- len vlen) final)))
		       (else
			(array-slice! o k final)))))
	      (array-slice! o k final)))
	 (else
	  (with-access::JsArray o (vec ilen)
	     (let ((vlen (uint32->fixnum ilen)))
		(cond
		   ((<= final vlen)
		    (vector-slice! o vec (->fixnum k) (->fixnum final)))
		   ((>fx vlen 0)
		    (let* ((arr (vector-slice! o vec (->fixnum k) vlen))
			   (vlen (->fixnum (js-get-length arr %this))))
		       (array-copy! o vlen arr (- len vlen) final)))
		   (else
		    (array-slice! o k final)))))))))
   
;*---------------------------------------------------------------------*/
;*    js-array-maybe-slice0 ...                                        */
;*---------------------------------------------------------------------*/
(define (js-array-maybe-slice0 this %this::JsGlobalObject cache)
   (if (and (js-array? this)
	    (js-object-mode-plain? this)
	    (js-object-mode-inline? this))
       (with-access::JsArray this (ilen vec)
	  (let ((o (js-array-species-create %this this (uint32->fixnum ilen))))
	     (with-access::JsArray o ((vdst vec) (idst ilen))
		(vector-copy! vdst 0 vec 0 (uint32->fixnum ilen))
		(set! idst ilen))
	     o))
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call0 %this
	     (js-get-name/cache this (& "slice") #f %this
		(or cache (js-pcache-ref js-array-pcache 17)))
	     this))))

;*---------------------------------------------------------------------*/
;*    js-array-maybe-slice1 ...                                        */
;*---------------------------------------------------------------------*/
(define (js-array-maybe-slice1 this start %this::JsGlobalObject cache)
   (cond
      ((and (js-array? this)
	    (js-object-mode-plain? this)
	    (js-object-mode-inline? this)
	    (fixnum? start))
       (with-access::JsArray this (ilen)
	  (js-array-inlined-slice2 this start (uint32->fixnum ilen) %this)))
      ((js-jsstring? this)
       (js-jsstring-slice this start (js-jsstring-lengthfx this) %this))
      (else
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call1 %this
	     (js-get-name/cache this (& "slice") #f %this
		(or cache (js-pcache-ref js-array-pcache 17)))
	     this
	     start)))))

;*---------------------------------------------------------------------*/
;*    js-array-maybe-slice2 ...                                        */
;*---------------------------------------------------------------------*/
(define (js-array-maybe-slice2 this start end %this::JsGlobalObject cache)
   (cond
      ((and (js-array? this)
	    (js-object-mode-plain? this)
	    (js-object-mode-inline? this)
	    (fixnum? start)
	    (fixnum? end))
       (js-array-inlined-slice2 this start end %this))
      ((js-jsstring? this)
       (js-jsstring-slice this start end %this))
      (else
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call2 %this
	     (js-get-name/cache this (& "slice") #f %this
		(or cache (js-pcache-ref js-array-pcache 17)))
	     this start end)))))

;*---------------------------------------------------------------------*/
;*    js-array-inlined-slice2 ...                                      */
;*---------------------------------------------------------------------*/
(define (js-array-inlined-slice2 this start end %this::JsGlobalObject)
   (with-access::JsArray this (ilen vec)
      (let* ((len (uint32->fixnum ilen))
	     (relstart start)
	     (k (if (<fx relstart 0)
		    (maxfx (+fx len relstart) 0)
		    (minfx relstart len)))
	     (relend end)
	     (final (if (<fx relend 0)
			(maxfx (+fx len relend) 0)
			(minfx relend len))))
	 (cond
	    ((<=fx final k)
	     (js-empty-vector->jsarray %this))
	    ((<=u32 (fixnum->uint32 final) ilen)
	     (with-access::JsArray this (ilen vec)
		(let* ((l (-fx final k))
		       (o (js-array-species-create %this this l)))
		   (with-access::JsArray o ((vdst vec) (idst ilen))
		      (vector-copy! vdst 0 vec k final)
		      (set! idst l))
		   o)))
	    (else
	     (js-array-slice this start end %this))))))

;*---------------------------------------------------------------------*/
;*    js-array-maybe-shift0 ...                                        */
;*---------------------------------------------------------------------*/
(define (js-array-maybe-shift0 this %this cache)
   (if (and (js-array? this)
	    (js-object-mode-plain? this)
	    (js-object-mode-inline? this))
       (with-access::JsArray this (vec ilen length)
	  (cond
	     ((=u32 ilen 0)
	      (js-undefined))
	     (($jsobject-vector-inline? this)
	      ;; fast path for shifting an array. when the the array
	      ;; is builtin (the JsArray object contains the Scheme vector)
	      ;; instead of shifting inside the Scheme array, we merely
	      ;; shift the inner pointer to the Scheme array
	      (let ((first ($js-jsarray-shift-builtin this))
		    (len (-u32 ilen 1)))
		 (set! length len)
		 (set! ilen len)
		 first))
	     (else
	      (with-access::JsArray this (vec length ilen)
		 (let ((first (vector-ref vec 0))
		       (nlen (-u32 ilen #u32:1))
		       (vlen (vector-length vec)))
		    (vector-copy! vec 0 vec 1)
		    (vector-set! vec (-fx vlen 1) (js-absent))
		    (set! length nlen)
		    (set! ilen (-u32 ilen 1))
		    first)))))
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call0 %this
	     (js-get-name/cache this (& "shift") #f %this
		(or cache (js-pcache-ref js-array-pcache 17)))
	     this))))

;*---------------------------------------------------------------------*/
;*    js-array-prototype-sort ...                                      */
;*---------------------------------------------------------------------*/
(define (js-array-prototype-sort this::obj comparefn %this::JsGlobalObject)
   
   (define (default-compare x y)
      (let ((nothasj (js-absent? x))
	    (nothask (js-absent? y)))
	 (cond
	    (nothasj nothask)
	    (nothask #t)
	    ((eq? x (js-undefined)) (eq? y (js-undefined)))
	    ((eq? y (js-undefined)) #t)
	    ((and (integer? x) (integer? y)) (< x y))
	    (else (string<? (js-tostring x %this) (js-tostring y %this))))))
   
   (define (make-compare comparefn)
      (lambda (x y)
	 (let ((nothasj (js-absent? x))
	       (nothask (js-absent? y)))
	    (cond
	       (nothasj nothask)
	       (nothask #t)
	       ((eq? x (js-undefined)) (eq? y (js-undefined)))
	       ((eq? y (js-undefined)) #t)
	       (else (<= (js-tointeger (js-call2 %this comparefn (js-undefined) x y) %this) 0))))))
   
   (define (get-compare comparefn)
      (cond
	 ((eq? comparefn (js-undefined))
	  default-compare)
	 ((not (js-procedure? comparefn))
	  (js-raise-type-error %this
	     "sort: argument not a function ~s" comparefn))
	 (else
	  (with-access::JsProcedure comparefn (proc)
	     (make-compare comparefn)))))
   
   (define (vector-sort this cmp)
      (with-access::JsArray this (vec)
	 ($sort-vector vec cmp)
	 this))
   
   (define (partition arr cmp left right pivotindex)
      (let ((pivotvalue (js-get arr pivotindex %this)))
	 (js-put! arr pivotindex (js-get arr right %this) #f %this)
	 (js-put! arr right pivotvalue #f %this)
	 (let loop ((i left)
		    (s left))
	    (if (< i right)
		(let ((vi (js-get arr i %this)))
		   (if (cmp vi pivotvalue)
		       (begin
			  (unless (= i s)
			     (let ((vi (js-get arr i %this)))
				(js-put! arr i (js-get arr s %this) #f %this)
				(js-put! arr s vi #f %this)))
			  (loop (+ i 1) (+ s 1)))
		       (loop (+ i 1) s)))
		(let ((si (js-get arr s %this)))
		   (js-put! arr s (js-get arr right %this) #f %this)
		   (js-put! arr right si #f %this)
		   s)))))
   
   
   (define (quicksort arr cmp left right)
      ;; http://en.wikipedia.org/wiki/Quicksort
      (when (< left right)
	 (let ((pivotindex (+ left
			      (inexact->exact (round (/ (- right left) 2))))))
	    (let ((pivotnewindex (partition arr cmp left right pivotindex)))
	       (quicksort arr cmp left (- pivotnewindex 1))
	       (quicksort arr cmp (+ pivotnewindex 1) right)))))
   
   (define (array-sort arr cmp)
      (let ((len (js-uint32-tointeger (js-get-lengthu32 arr %this))))
	 (unless (< len 2)
	    (quicksort arr cmp 0 (- len 1)))
	 arr))
   
   (let ((o this))
      (if (not (js-array? this))
	  (array-sort this (get-compare comparefn))
	  (with-access::JsArray this (vec)
	     (cond
		((js-object-mode-inline? this)
		 (vector-sort this (get-compare comparefn)))
		((=u32 (js-get-lengthu32 o %this) #u32:0)
		 this)
		(else
		 (array-sort this (get-compare comparefn))))))))

;*---------------------------------------------------------------------*/
;*    js-array-sort ...                                                */
;*---------------------------------------------------------------------*/
(define (js-array-sort this::JsArray comparefn %this cache)
   (if (js-object-mode-plain? this)
       (js-array-prototype-sort this comparefn %this)
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call1 %this
	     (js-get-jsobject-name/cache this (& "sort") #f %this
		(or cache (js-pcache-ref js-array-pcache 18)))
	     this comparefn))))

;*---------------------------------------------------------------------*/
;*    js-array-maybe-sort ...                                          */
;*---------------------------------------------------------------------*/
(define (js-array-maybe-sort this::obj comparefn %this cache)
   (if (js-array? this)
       (js-array-sort this comparefn %this cache)
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call1 %this
	     (js-get-name/cache this (& "sort") #f %this
		(or cache (js-pcache-ref js-array-pcache 18)))
	     this comparefn))))

;*---------------------------------------------------------------------*/
;*    js-array-prototype-indexof ...                                   */
;*    -------------------------------------------------------------    */
;*    The THIS argument is intentionally not required to be a JsArray  */
;*---------------------------------------------------------------------*/
(define (js-array-prototype-indexof this::JsObject el indx %this)
   
   (define (vector-indexof::int arr vec k::int len::int)
      (let loop ((k k))
	 (cond
	    ((>=fx k len)
	     -1)
	    ((js-strict-equal? (vector-ref vec k) el)
	     k)
	    (else
	     (loop (+fx k 1))))))
   
   (define (vector-holey-indexof::int arr vec k::int len::int)
      (let loop ((k k))
	 (cond
	    ((>=fx k len)
	     -1)
	    ((js-strict-equal? (vector-ref vec k) el)
	     k)
	    ((and (js-absent? (vector-ref vec k))
		  (let ((name (js-toname k %this)))
		     (and (js-has-property arr name %this)
			  (js-strict-equal? (js-get arr name %this) el))))
	     k)
	    (else
	     (loop (+fx k 1))))))
   
   (define (array-indexof::int arr k::uint32 len::uint32)
      (let ((k (js-uint32-tointeger k))
	    (len (js-uint32-tointeger len)))
	 (let loop ((k k))
	    (cond
	       ((>= k len)
		-1)
	       ((let ((name (js-toname k %this)))
		   (and (js-has-property arr name %this)
			(js-strict-equal? (js-get arr name %this) el)))
		k)
	       (else
		(loop (+ k 1)))))))
   
   (let* ((len::uint32 (js-get-lengthu32 this %this))
	  (n (js-tointeger indx %this)))
      (if (<=uint32 len n)
	  -1
	  (let ((k (if (< n 0)
		       (let ((absn (abs n)))
			  (if (<=uint32 len absn)
			      #u32:0
			      (-u32 len (->uint32 absn))))
		       (->uint32 n))))
	     (cond
		((not (js-array? this))
		 (array-indexof this k len))
		((js-object-mode-inline? this)
		 (with-access::JsArray this (vec ilen)
		    (vector-indexof this vec (uint32->fixnum k)
		       (uint32->fixnum ilen))))
		((js-object-mode-holey? this)
		 (with-access::JsArray this (vec ilen)
		    (vector-holey-indexof this vec (uint32->fixnum k)
		       (vector-length vec))))
		(else
		 (array-indexof this k len)))))))

;*---------------------------------------------------------------------*/
;*    js-array-indexof ...                                             */
;*---------------------------------------------------------------------*/
(define (js-array-indexof o::JsArray el indx %this cache)
   (if (js-object-mode-plain? o)
       (js-array-prototype-indexof o el indx %this)
       (with-access::JsGlobalObject %this (js-array-pcache)
	  (js-call2 %this
	     (js-get-jsobject-name/cache o (& "indexOf") #f %this
		(or cache (js-pcache-ref js-array-pcache 10)))
	     o el indx))))

;*---------------------------------------------------------------------*/
;*    js-iterator-to-array ...                                         */
;*    -------------------------------------------------------------    */
;*    This function is used when destructuring a binding.              */
;*---------------------------------------------------------------------*/
(define (js-iterator-to-array value size %this)

   (define (return it res)
      (let ((ret (js-get it (& "return") %this)))
	 (if (js-procedure? ret)
	     (js-call0 %this ret it)
	     (js-undefined)))
      res)

   (if (js-array? value)
       value
       (with-access::JsGlobalObject %this (js-symbol-iterator)
	  (let ((proc (js-get value js-symbol-iterator %this)))
	     (if (js-procedure? proc)
		 (let* ((vec (make-vector size (js-absent)))
			(res (js-vector->jsarray vec %this))
			(it (js-call0 %this proc value)))
		    (let loop ((i 0))
		       (let* ((n (js-get it (& "next") %this))
			      (v (js-call0 %this n it))
			      (done (js-get v (& "done") %this)))
			  (cond
			     ((=fx i size)
			      (return it res))
			     (done
			      (with-access::JsArray res (ilen length)
				 (set! ilen (fixnum->uint32 i))
				 (set! length (fixnum->uint32 i))
				 res))
			     (else
			      (let ((value (js-get v (& "value") %this)))
				 (vector-set! vec i value)
				 (loop (+fx i 1))))))))
		 (js-raise-type-error %this
		    "Invalid attempt to destructure non-iterable instance"
		    value))))))

;*---------------------------------------------------------------------*/
;*    js-jsobject->jsarray ::JsArray ...                               */
;*---------------------------------------------------------------------*/
(define-method (js-jsobject->jsarray o::JsArray %this)
   o)

;*---------------------------------------------------------------------*/
;*    js-call-with-stack-vector ...                                    */
;*    -------------------------------------------------------------    */
;*    Overriden by a macro in array.sch                                */
;*---------------------------------------------------------------------*/
(define (js-call-with-stack-vector vec proc)
   (proc vec))
   
;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
