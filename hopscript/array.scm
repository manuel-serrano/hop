;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/array.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:41:39 2013                          */
;*    Last change :  Fri Jan 26 06:10:57 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript arrays                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_array
   
   (library hop)
   
   (include "../nodejs/nodejs_debug.sch"
	    "stringliteral.sch")
   
   (extern ($js-make-jsarray::JsArray (::int ::JsConstructMap ::obj ::byte)
	      "bgl_make_jsarray"))
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_number
	   __hopscript_worker
	   __hopscript_symbol
	   __hopscript_string
	   __hopscript_generator)
   
   (export (js-init-array! ::JsGlobalObject)
	   (inline js-array-mark::long)
	   *JS-ARRAY-MARK*
	   (inline js-array?::bool ::obj)
	   (inline js-array-length::uint32 ::JsArray)
	   (inline js-array-update-length!::long ::JsArray ::long)
	   
	   (inline js-array-vec::vector ::JsArray)
	   (inline js-array-ilen::uint32 ::JsArray)
	   
	   (js-array-ref ::JsArray ::obj ::JsGlobalObject)
	   (js-array-ref-ur ::JsArray ::long ::JsGlobalObject)
	   (inline js-array-inl-ref ::JsArray ::obj
	      ::vector ::uint32 ::obj ::JsGlobalObject)
	   (js-array-index-ref ::JsArray ::uint32 ::JsGlobalObject)
	   (inline js-array-index-inl-ref ::JsArray ::uint32
	      ::vector ::uint32 ::obj ::JsGlobalObject)
	   (inline js-array-fixnum-ref ::JsArray ::long ::JsGlobalObject)
	   (inline js-array-fixnum-inl-ref ::JsArray ::long
	      ::vector ::uint32 ::obj ::JsGlobalObject)
	   
	   (js-array-set! ::JsArray idx ::obj ::bool ::JsGlobalObject)
	   (inline js-array-inl-set! ::JsArray ::obj ::obj
	      ::vector ::uint32 ::obj ::bool ::JsGlobalObject)
	   (js-array-index-set! ::JsArray ::uint32 ::obj ::bool ::JsGlobalObject)
	   (inline js-array-index-inl-set! ::JsArray ::uint32 ::obj
	      ::vector ::uint32 ::obj ::bool ::JsGlobalObject)
	   (inline js-array-fixnum-set! ::JsArray ::long ::obj
	      ::bool ::JsGlobalObject)
	   (inline js-array-fixnum-inl-set! ::JsArray ::long ::obj
	      ::vector ::uint32 ::obj ::bool ::JsGlobalObject)
	   
	   (js-array-set-ur! ::JsArray ::long ::obj ::bool ::JsGlobalObject)
	   (js-vector->jsarray::JsArray ::vector ::JsGlobalObject)
	   (js-vector->sparse-jsarray::JsArray ::vector ::JsGlobalObject)
	   
	   (js-array-alloc::JsArray ::JsGlobalObject)
	   (js-array-construct::JsArray ::JsGlobalObject ::JsArray ::obj)
	   (js-array-construct/length::JsArray ::JsGlobalObject ::JsArray ::obj)
	   (jsarray->list::pair-nil ::JsArray ::JsGlobalObject)
	   (jsarray->vector::vector ::JsArray ::JsGlobalObject)
	   (js-array-push ::JsArray ::obj ::JsGlobalObject)
	   (js-array-maybe-push ::obj ::obj ::JsGlobalObject)
	   (js-array-pop ::JsArray ::JsGlobalObject)
	   (js-array-maybe-pop ::obj ::JsGlobalObject)
	   (js-array-fill ::JsArray ::obj ::obj ::obj ::JsGlobalObject)
	   (js-array-maybe-fill ::obj ::obj ::obj ::obj ::JsGlobalObject)
	   (js-array-comprehension ::JsGlobalObject ::obj ::procedure
	      ::obj ::pair ::bstring ::bstring ::pair))
   
   (cond-expand
      (bigloo-c
       (export
	  (inline js-empty-vector->jsarray::JsArray ::JsGlobalObject)
	  (inline DEFAULT-EMPTY-ARRAY-SIZE::long)))
      (else
       (export (js-empty-vector->jsarray::JsArray ::JsGlobalObject)))))

;*---------------------------------------------------------------------*/
;*    js-toindex ...                                                   */
;*---------------------------------------------------------------------*/
(define-macro (js-toindex p)
   (cond-expand
      (bint30
       `(if (and (fixnum? ,p) (>=fx ,p 0))
	    (fixnum->uint32 ,p)
	    ((@ js-toindex  __hopscript_public) ,p)))
      ((or bint61 64)
       `(if (and (fixnum? ,p) (>=fx ,p 0) (<fx ,p (-fx (bit-lsh 1 32) 1)))
	    (fixnum->uint32 ,p)
	    ((@ js-toindex  __hopscript_public) ,p)))
      (else
       ((@ js-toindex  __hopscript_public) ,p))))
       
;*---------------------------------------------------------------------*/
;*    global parameters                                                */
;*---------------------------------------------------------------------*/
(define-inline (DEFAULT-EMPTY-ARRAY-SIZE) 8)

(define-inline (MAX-EXPANDABLE-ARRAY-SIZE::uint32)
   (bit-lshu32 #u32:1 20))
(define-inline (MAX-EXPANDABLE-ARRAY-SIZE/2::uint32)
      (/u32 (MAX-EXPANDABLE-ARRAY-SIZE) #u32:2))
(define-inline (MAX-EXPANDABLE-ARRAY-SIZE/8::uint32)
   (/u32 (MAX-EXPANDABLE-ARRAY-SIZE) #u32:8))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-begin!)

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsArray ...                                  */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsArray
   (lambda (o)
      (jsarray->vector o (js-initial-global-object)))
   (lambda (o %this)
      (js-vector->jsarray o (or %this (js-initial-global-object)))))

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
		  (set! nilen ilen))
	       ;; donate the value of the array
	       (js-for-in obj
		  (lambda (k)
		     (js-put! nobj k
			(js-donate (js-get obj k %_this) worker %_this)
			#f %this))
		  %this)
	       nobj)))))
	    
;*---------------------------------------------------------------------*/
;*    xml-unpack ::JsArray ...                                         */
;*---------------------------------------------------------------------*/
(define-method (xml-unpack obj::JsArray)
   
   (define (subvector->list vec alen::long)
      (if (=fx alen 0)
	  '()
	  (let loop ((i (-fx alen 1))
		     (acc '()))
	     (if (=fx i 0)
		 (cons (vector-ref-ur vec i) acc)
		 (loop (-fx i 1) (cons (vector-ref-ur vec i) acc))))))
   
   (with-access::JsArray obj (vec length)
      (if (js-array-full-inlined? obj)
	  (subvector->list vec (uint32->fixnum length))
	  (let* ((%this (js-initial-global-object))
		 (len::uint32 length))
	     (let loop ((i::uint32 #u32:0))
		(cond
		   ((=u32 i len)
		    '())
		   ((js-has-property obj (js-toname i %this) %this)
		    (cons (js-get obj i %this) (loop (+u32 i #u32:1))))
		   (else
		    (loop (+u32 i #u32:1)))))))))

;*---------------------------------------------------------------------*/
;*    xml-body-element ::JsArray ...                                   */
;*---------------------------------------------------------------------*/
(define-method (xml-body-element obj::JsArray)
   (xml-unpack obj))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsArray ...                                    */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsArray op compile isexpr)
   (let* ((%this (js-initial-global-object))
	  (len::uint32 (js-array-length o)))
      (if (=u32 len #u32:0)
	  (display "sc_vector2array([])" op)
	  (begin
	     (display "sc_vector2array([" op)
	     (hop->javascript (js-get o (js-toname 0 %this) %this) op compile isexpr)
	     (let loop ((i #u32:1))
		(if (=u32 i len)
		    (display "])" op)
		    (begin
		       (display "," op)
		       (when (js-has-property o (js-toname i %this) %this)
			  (hop->javascript (js-get o i %this)
			     op compile isexpr))
		       (loop (+u32 i #u32:1)))))))))

;*---------------------------------------------------------------------*/
;*    xml-write-attribute ::JsArray ...                                */
;*---------------------------------------------------------------------*/
(define-method (xml-write-attribute o::JsArray id op backend)
   (let ((v (xml-unpack o)))
      (when (pair? v)
	 (display (keyword->string! id) op)
	 (display "='" op)
	 (display (xml-attribute-encode (car v)) op)
	 (let loop ((v (cdr v)))
	    (when (pair? v)
	       (display " " op)
	       (display (xml-attribute-encode (car v)) op)
	       (loop (cdr v))))
	 (display "'" op))))

;*---------------------------------------------------------------------*/
;*    jsarray->list ...                                                */
;*---------------------------------------------------------------------*/
(define (jsarray->list o::JsArray %this)
   (let* ((%this (js-initial-global-object))
	  (len::uint32 (js-array-length o)))
      (if (=u32 len (fixnum->uint32 0))
	  '()
	  (let loop ((i #u32:0))
	     (cond
		((=u32 i len)
		 '())
		((js-has-property o (js-toname i %this) %this)
		 (cons (cons (js-get o i %this) i) (loop (+u32 i #u32:1))))
		(else
		 (loop (+u32 i #u32:1))))))))

;*---------------------------------------------------------------------*/
;*    jsarray->vector ...                                              */
;*---------------------------------------------------------------------*/
(define (jsarray->vector o::JsArray %this)
   (let* ((%this (js-initial-global-object))
	  (len::uint32 (js-array-length o)))
      (if (=u32 len #u32:0)
	  '#()
	  (let ((res (make-vector (uint32->fixnum len) (js-undefined))))
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
   (with-access::JsGlobalObject %this (__proto__ js-array js-array-prototype
					 js-function)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 ;; builtin prototype
	 (set! js-array-prototype
	    (instantiate::JsArray
	       (vec '#())
	       (__proto__ __proto__)
	       (properties (list
			      ;; cannot be defined with js-bind! because
			      ;; of bootstrap specificities
			      (instantiate::JsValueDescriptor
				 (name 'length)
				 (value 0)
				 (configurable #f)
				 (writable #t))))))

	 ;; DO NOT REMOVE !
	 ;; when array are ready for caching this should replace
	 ;; the old definition of js-array-prototype
	 ;; (set! js-array-prototype
	    ;; (instantiate::JsArray
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
	 (set! js-array
	    (js-make-function %this (%js-array %this) 1 'Array
	       :__proto__ js-function-prototype
	       :prototype js-array-prototype
	       :alloc (lambda (ctor) (js-array-alloc-ctor ctor %this))
	       :construct (lambda (this . is)
			     (js-array-construct %this this is))))
	 
	 ;; other properties of the Array constructor
	 ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.5.1
	 (js-bind! %this js-array 'isArray
	    :value (js-make-function %this
		      (lambda (this arg) (isa? arg JsArray))
		      1 'isArray)
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
			  (not (isa? mapfn JsFunction)))
		  (js-raise-type-error %this
		     "Array.from: when provided, the second argument must be a function"
		     mapfn))
	       ;; 5. b. If thisArg was supplied, let T be thisArg;
	       ;; else let T be undefined.
	       ;;  10. Let lenValue be Get(items, "length").
	       ;; 11. Let len be ToLength(lenValue).
	       (let ((len (js-get-length items #f %this)))
		  ;; 13. If IsConstructor(C) is true, then
		  ;; 13. a. Let A be the result of calling the [[Construct]]
		  ;;     internal method of C with an argument list containing
		  ;;     the single item len.
		  ;; 14. a. Else, Let A be ArrayCreate(len).
		  (let ((A (if (isa? C JsFunction)
			       (js-toobject %this (js-new1 %this C len))
			       (js-vector->jsarray (make-vector len) %this))))
		     ;; 16. Let k be 0.
		     ;; 17. Repeat, while k < len… (also steps a - h)
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
   
	 (js-bind! %this js-array 'from
	    :value (js-make-function %this array-from
		      0 'from
		      :prototype (js-undefined))
	    :enumerable #f
	    :hidden-class #t)
	 
	 ;; init the prototype properties
	 (init-builtin-array-prototype! %this js-array js-array-prototype)
	 
	 ;; bind Array in the global object
	 (js-bind! %this %this 'Array
	    :configurable #f :enumerable #f :value js-array
	    :hidden-class #t)

	 js-array)))

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
;*    js-array? ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (js-array? obj)
   (isa? obj JsArray))

;*---------------------------------------------------------------------*/
;*    js-array-length ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (js-array-length arr::JsArray)
   (with-access::JsArray arr (length)
      length))

;*---------------------------------------------------------------------*/
;*    js-get-length ::JsArray ...                                      */
;*---------------------------------------------------------------------*/
(define-method (js-get-length o::JsArray cache %this)
   (with-access::JsArray o (length)
      (uint32->integer length)))

;*---------------------------------------------------------------------*/
;*    js-array-inlined? ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-array-inlined? arr::JsArray)
   (with-access::JsArray arr (vec length)
      (or (>fx (vector-length vec) 0) (=u32 length #u32:0))))

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
		 (with-access::JsArray o (vec length ilen)
		    (and (<=u32 ilen (fixnum->uint32 (vector-length vec)))
			 (<=u32 ilen length))))
	      ,v))
       o))

;*---------------------------------------------------------------------*/
;*    js-array-find-length-property ...                                */
;*---------------------------------------------------------------------*/
(define (js-array-find-length-property arr::JsArray)
   (with-access::JsArray arr (properties)
      (when (pair? properties)
	 (with-access::JsPropertyDescriptor (car properties) (name)
	    (when (eq? name 'length)
	       (car properties))))))

;*---------------------------------------------------------------------*/
;*    js-array-update-length-property! ...                             */
;*---------------------------------------------------------------------*/
(define (js-array-update-length-property! arr::JsArray)
   
   (define (add-length-property! arr::JsArray)
      (with-access::JsArray arr (properties)
	 (let ((prop (instantiate::JsValueDescriptor
			(name 'length)
			(value (uint32->integer (js-array-length arr)))
			(configurable #f)
			(writable #t))))
	    (set! properties (cons prop properties))
	    prop)))
   
   (let ((desc (js-array-find-length-property arr)))
      (if desc
	  (with-access::JsValueDescriptor desc (value name)
	     (set! value (uint32->integer (js-array-length arr)))
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
		(vector-set-ur! new i (vector-ref-ur vec i))
		(loop (+fx i 1)))))))
	 
;*---------------------------------------------------------------------*/
;*    js-array-ref ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-array-ref arr::JsArray idx %this)
   (if (cond-expand
	  ((or bint30 bint32)
	   (and (fixnum? idx) (>=fx idx 0)))
	  (else
	   (and (fixnum? idx) (>=fx idx 0) (<=fx idx (-fx (bit-lsh 1 32) 2)))))
       (js-array-index-ref arr (fixnum->uint32 idx) %this)
       (js-get arr idx %this)))

;*---------------------------------------------------------------------*/
;*    js-array-inl-ref ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (js-array-inl-ref arr::JsArray idx::obj
		  avec::vector alen::uint32 mark::obj %this::JsGlobalObject)
   (if (and (fixnum? idx)
	    (>=fx idx 0)
 	    (<u32 (fixnum->uint32 idx) alen)
	    (eq? mark (js-array-mark)))
       (vector-ref-ur avec idx)
       (js-array-ref arr (fixnum->uint32 idx) %this)))
   
;*---------------------------------------------------------------------*/
;*    js-array-index-ref ...                                           */
;*---------------------------------------------------------------------*/
(define (js-array-index-ref arr::JsArray idx::uint32 %this)
   (with-access::JsArray arr (vec ilen)
      (cond
	 ((<u32 idx ilen)
	  (vector-ref-ur vec (uint32->fixnum idx)))
	 (else
	  (js-get arr (uint32->integer idx) %this)))))

;*---------------------------------------------------------------------*/
;*    js-array-index-inl-ref ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (js-array-index-inl-ref arr::JsArray idx::uint32
		  avec::vector alen::uint32 mark::obj %this::JsGlobalObject)
   (if (and (<u32 idx alen) (eq? mark (js-array-mark)))
       (vector-ref-ur avec (uint32->fixnum idx))
       (js-array-index-ref arr idx %this)))
   
;*---------------------------------------------------------------------*/
;*    js-array-fixnum-ref ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-array-fixnum-ref arr::JsArray idx::long %this)
   (with-access::JsArray arr (vec ilen)
      (cond
	 ((and (>=fx idx 0) (<u32 (fixnum->uint32 idx) ilen))
	  (vector-ref-ur vec idx))
	 (else
	  (js-get arr (uint32->integer idx) %this)))))
   
;*---------------------------------------------------------------------*/
;*    js-array-fixnum-inl-ref ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (js-array-fixnum-inl-ref arr::JsArray idx::long
		  avec::vector alen::uint32 mark::obj %this::JsGlobalObject)
   (if (and (>=fx idx 0)
	    (<u32 (fixnum->uint32 idx) alen)
	    (eq? mark (js-array-mark)))
       (vector-ref-ur avec idx)
       (js-array-ref arr (fixnum->uint32 idx) %this)))
   
;*---------------------------------------------------------------------*/
;*    js-array-ref-ur ...                                              */
;*---------------------------------------------------------------------*/
(define (js-array-ref-ur arr::JsArray idx::long %this)
   [assert (idx) (>=fx idx 0)]
   (with-access::JsArray arr (vec ilen)
      (cond
	 ((and (>=fx idx 0) (<u32 (fixnum->uint32 idx) ilen))
	  (vector-ref-ur vec idx))
	 ((<fx idx (vector-length vec))
	  (js-get arr idx %this))
	 ((cond-expand
	     ((or bint30 bint32)
	      (expandable-array arr (fixnum->uint32 idx)
		 (fixnum->uint32 (vector-length vec))))
	     (else
	      (and (<fx idx (-fx (bit-lsh 1 32) 1))
		   (expandable-array arr (fixnum->uint32 idx)
		      (fixnum->uint32 (vector-length vec))))))
	  =>
	  (lambda (len)
	     (let ((nlen (uint32->fixnum len)))
		(set! vec (copy-vector-fill! vec nlen (js-undefined))))
	     (js-get arr idx %this)))
	 (else
	  (js-get arr idx %this)))))
   
;*---------------------------------------------------------------------*/
;*    js-array-set! ...                                                */
;*---------------------------------------------------------------------*/
(define (js-array-set! arr::JsArray idx val throw %this)
   (if (and (fixnum? idx) (>=fx idx 0))
       (js-array-set-ur! arr idx val throw %this)
       (js-array-put! arr idx val throw %this)))

;*---------------------------------------------------------------------*/
;*    js-array-inl-set! ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-array-inl-set! arr::JsArray idx::obj val
		  avec::vector alen::uint32 mark::obj
		  throw::bool %this::JsGlobalObject)
   (if (and (fixnum? idx)
	    (>=fx idx 0)
	    (<u32 (fixnum->uint32 idx) alen)
	    (eq? mark (js-array-mark)))
       (vector-set-ur! avec idx val)
       (js-array-index-set! arr (fixnum->uint32 idx) val throw %this)))
    
;*---------------------------------------------------------------------*/
;*    js-array-index-set! ...                                          */
;*---------------------------------------------------------------------*/
(define (js-array-index-set! arr::JsArray idx::uint32 val throw %this)
   (with-access::JsArray arr (vec ilen)
      (cond
	 ((<u32 idx ilen)
	  (vector-set-ur! vec (uint32->fixnum idx) val)
	  val)
	 (else
	  (js-array-set-ur! arr (uint32->fixnum idx) val throw %this)))))
   
;*---------------------------------------------------------------------*/
;*    js-array-index-inl-set! ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (js-array-index-inl-set! arr::JsArray idx::uint32 val
		  avec::vector alen::uint32 mark::obj throw %this::JsGlobalObject)
   (if (and (<u32 idx alen) (eq? mark (js-array-mark)))
       (vector-set-ur! avec (uint32->fixnum idx) val)
       (js-array-index-set! arr idx val throw %this)))
   
;*---------------------------------------------------------------------*/
;*    js-array-fixnum-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (js-array-fixnum-set! arr::JsArray idx::long val throw %this)
   (with-access::JsArray arr (vec ilen)
      (cond
	 ((and (>=fx idx 0) (<u32 (fixnum->uint32 idx) ilen))
	  (vector-set-ur! vec idx val)
	  val)
	 (else
	  (js-array-set-ur! arr idx val throw %this)))))
   
;*---------------------------------------------------------------------*/
;*    js-array-fixnum-inl-set! ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (js-array-fixnum-inl-set! arr::JsArray idx::long val
		  avec::vector alen::uint32 mark::obj
		  throw::bool %this::JsGlobalObject)
   (if (and (>=fx idx 0)
	    (<u32 (fixnum->uint32 idx) alen)
	    (eq? mark (js-array-mark)))
       (vector-set-ur! avec idx val)
       (js-array-index-set! arr (fixnum->uint32 idx) val throw %this)))
   
;*---------------------------------------------------------------------*/
;*    js-array-set-ur! ...                                             */
;*---------------------------------------------------------------------*/
(define (js-array-set-ur! arr::JsArray idx::long val throw::bool %this)
   (with-access::JsArray arr (vec ilen length)
      (cond
	 ((<u32 (fixnum->uint32 idx) ilen)
	  (vector-set-ur! vec idx val)
	  val)
	 ((<fx idx (vector-length vec))
	  (with-access::JsArray arr (length)
	     (cond
		((not (array-extensible? arr))
		 (if throw
		     (js-raise-type-error %this
			"Can't add property ~a, object not extensible" idx)
		     val))
		((and (=u32 (fixnum->uint32 idx) ilen)
		      (js-array-full-inlined? arr))
		 (vector-set-ur! vec idx val)
		 (let ((nilen (+u32 ilen #u32:1)))
		    (set! ilen nilen)
		    (when (>=u32 (fixnum->uint32 idx) length)
		       (set! length nilen)))
		 val)
		(else
		 (js-array-put! arr idx val throw %this)))))
	 (else
	  (js-array-put! arr idx val throw %this)))))
   
;*---------------------------------------------------------------------*/
;*    init-builtin-array-prototype! ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.3       */
;*---------------------------------------------------------------------*/
(define (init-builtin-array-prototype! %this js-array js-array-prototype)
   
   ;; constructor
   (js-bind! %this js-array-prototype 'constructor
      :value js-array :enumerable #f
      :hidden-class #t)
   
   ;; tostring
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.2
   (define (array-prototype-tostring this::obj)
      (let* ((o (js-toobject %this this))
	     (func (js-get this 'join %this)))
	 (if (isa? func JsFunction)
	     (js-call1 %this func this (js-undefined))
	     (js-tojsstring this %this))))
   
   (js-bind! %this js-array-prototype 'toString
      :value (js-make-function %this array-prototype-tostring 0 'toString
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
		   (js-call0 %this (js-get obj 'toLocaleString %this) obj)
		   %this))))
      
      (let* ((o (js-toobject %this this))
	     (lenval::uint32 (js-touint32 (js-get-length o #f %this) %this)))
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
   
   (js-bind! %this js-array-prototype 'toLocaleString
      :value (js-make-function %this array-prototype-tolocalestring 0 'toLocaleString
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; concat
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.4
   (define (array-prototype-concat this::obj . l)
      
      (define (copy-array-slow target tstart src sstart send)
	 ;; slow copy, elements by elements
	 (let loop ((i sstart)
		    (j tstart))
	    (if (= i send)
		j
		(begin
		   (js-put! target j (js-get src i %this) #f %this)
		   (loop (+ i 1) (+ j 1))))))
      
      (define (copy-array src dst i)
	 (if (and (js-array-full-inlined? src)
		  (js-array-full-inlined? dst))
	     (with-access::JsArray src ((vsrc vec))
		(with-access::JsArray dst ((vdst vec) ilen)
		   ;; try to use a vector copy
		   (if (and (>fx (vector-length vdst) 0)
			    (>fx (vector-length vsrc) 0))
		       (let* ((lsrc (vector-length vsrc))
			      (slen (js-get-length src #f %this))
			      (alen (minfx slen lsrc)))
			  ;; fast vector-copy
			  (vector-copy! vdst i vsrc 0 alen)
			  (set! ilen (fixnum->uint32 (+fx i alen)))
			  (if (> slen lsrc)
			      (copy-array-slow dst (+fx i lsrc) src lsrc slen)
			      (+fx i alen)))
		       ;; slow copy
		       (copy-array-slow dst i src 0 (js-get-length src #f %this)))))
	     (copy-array-slow dst i src 0 (js-get-length src #f %this))))
      
      (let* ((l (cons (js-toobject %this this) l))
	     (new-len (let loop ((l l)
				 (len 0))
			 (cond
			    ((null? l)
			     len)
			    ((isa? (car l) JsArray)
			     (loop (cdr l)
				(js+ len (js-get-length (car l) #f %this) %this)))
			    (else
			     (loop (cdr l) (+ 1 len))))))
	     (arr (with-access::JsGlobalObject %this (js-array)
		     (js-array-construct/length %this
			(js-array-alloc %this) new-len))))
	 ;; fill the vector
	 (let loop ((l l)
		    (i 0))
	    (cond
	       ((null? l)
		arr)
	       ((isa? (car l) JsArray)
		(loop (cdr l) (copy-array (car l) arr i)))
	       (else
		(js-put! arr i (car l) #f %this)
		(loop (cdr l) (+fx 1 i)))))))
   
   (js-bind! %this js-array-prototype 'concat
      :value (js-make-function %this array-prototype-concat 1 'concat
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

      (let* ((o (js-toobject %this this))
	     (lenval::uint32 (js-touint32 (js-get-length o #f %this) %this))
	     (sep (if (eq? separator (js-undefined))
		      ","
		      (js-tostring separator %this))))
	 (if (=u32 lenval #u32:0)
	     (js-ascii->jsstring "")
	     (let* ((el0 (el->string (js-get o 0 %this))))
		(let loop ((r (list el0))
			   (i #u32:1))
		   (if (=u32 i lenval)
		       (js-stringlist->jsstring (reverse! r))
		       (loop (cons* (el->string (js-get o i %this)) sep r)
			  (+u32 i #u32:1))))))))
   
   (js-bind! %this js-array-prototype 'join
      :value (js-make-function %this array-prototype-join 1 'join
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; pop
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.6
   (define (array-prototype-pop this::obj)
      (let* ((o (js-toobject %this this))
	     (len::uint32 (js-touint32 (js-get-length o #f %this) %this)))
	 (cond
	    ((=u32 len #u32:0)
	     (js-put-length! o 0 #f #f %this)
	     (js-undefined))
	    (else
	     (let* ((indx (-u32 len #u32:1))
		    (el (js-get o (uint32->integer indx) %this)))
		(js-delete! o indx #t %this)
		(js-put-length! o (uint32->integer indx) #f #f %this)
		el)))))
   
   (js-bind! %this js-array-prototype 'pop
      :value (js-make-function %this array-prototype-pop 0 'pop
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; push
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.7
   (define (array-prototype-push this::obj . items)
      (if (not (isa? this JsArray))
	  (let ((o (js-toobject %this this)))
	     (let ((n (uint32->integer
			 (js-touint32 (js-get-length o #f %this) %this))))
		(for-each (lambda (item)
			     (js-put! o n item #f %this)
			     (set! n (+ 1 n)))
		   items)
		(js-put-length! o n #f #f %this)
		n))
	  (with-access::JsArray this (length)
	     (let ((n::uint32 length))
		(for-each (lambda (item)
			     (js-array-index-set! this n item #t %this)
			     (set! n (+u32 n #u32:1))
			     (when (=u32 n #u32:0)
				(js-raise-range-error %this
				   "Illegal length: ~s"
				   (js-tostring #l4294967295 %this))))
		   items)
		(uint32->integer n)))))
   
   (js-bind! %this js-array-prototype 'push
      :value (js-make-function %this array-prototype-push 1 'push
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
	 (with-access::JsArray o (properties ilen)
	    (let* ((len::uint32 (js-touint32 (js-get-length o #f %this) %this))
		   (len/2::uint32 (/u32 len #u32:2)))
	       (let loop ((i #u32:0))
		  (cond
		     ((=u32 i len/2)
		      o)
		     ((js-has-property o (js-toname i %this) %this)
		      (let* ((t (js-get o (uint32->fixnum i) %this))
			     (ni (+u32 i (fixnum->uint32 1)))
			     (rni (uint32->integer (-u32 len ni))))
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
			     (rni (uint32->integer (-u32 len ni))))
			 (if (js-has-property o (js-toname rni %this) %this)
			     (begin
				(js-put! o (uint32->fixnum i)
				   (js-get o rni %this) #f %this))
			     (begin
				(js-delete! o (uint32->fixnum i) #t %this)))
			 (js-delete! o rni #t %this)
			 (loop ni))))))))
      
      (let ((o (js-toobject %this this)))
	 (if (isa? o JsArray)
	     (with-access::JsArray o (vec)
		(if (js-array-full-inlined? o)
		    ;; fast path
		    (with-access::JsArray o (ilen)
		       (vector-reverse! vec (uint32->fixnum ilen))
		       o)
		    (array-reverse! o)))
	     (array-reverse! o))))
   
   (js-bind! %this js-array-prototype 'reverse
      :value (js-make-function %this array-prototype-reverse 0 'reverse
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
	       (vector-set! vec (-fx vlen 1) (js-undefined))
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
		  ((eq? (js-get-property o (js-toname i %this) %this) (js-undefined))
		   (js-delete! o (-fx i 1) #t %this)
		   (loop (+fx i 1)))
		  (else
		   (let ((v (js-get o i %this)))
		      (js-put! o (-fx i 1) v #f %this)
		      (loop (+fx i 1))))))))
      
      (let ((o (js-toobject %this this)))
	 (if (isa? o JsArray)
	     (with-access::JsArray o (vec length)
		(cond
		   ((=u32 length #u32:0)
		    (js-undefined))
		   ((js-array-full-inlined? o)
		    ;; ilen guard is needed when shifting array
		    ;; with prototype fields
		    (vector-shift! o length))
		   (else
		    (array-shift! o length))))
	     (let ((len (js-touint32 (js-get-length o #f %this) %this)))
		(cond
		   ((=u32 len #u32:0)
		    (js-put-length! o 0 #f #f %this)
		    (js-undefined))
		   (else
		    (array-shift! o len)))))))

   (js-bind! %this js-array-prototype 'shift
      :value (js-make-function %this array-prototype-shift 0 'shift
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; slice
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.10
   (define (array-prototype-slice this::obj start end)

      (define (vector-slice/vec! o val k::long final::long vec::vector)
	 (let ((arr (js-vector->jsarray vec %this))
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
	 (let ((vec (make-vector (-fx final k))))
	    (let loop ((i k)
		       (j 0))
	       (if (=fx i final)
		   (vector-slice/vec! o val k final vec)
		   (begin
		      (vector-set-ur! vec j
			 (uint8->fixnum (u8vector-ref val i)))
		      (loop (+fx i 1) (+fx j 1)))))))

      (define (string-slice! o val::bstring k::long final::long)
	 (let ((vec ($create-vector (-fx final k))))
	    (let loop ((i k)
		       (j 0))
	       (if (=fx i final)
		   (vector-slice/vec! o val k final vec)
		   (begin
		      (vector-set-ur! vec j
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
	     (len (uint32->integer (js-touint32 (js-get-length o #f %this) %this)))
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
				  (vlen (->fixnum (js-get-length arr #f %this))))
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
				  (vlen (->fixnum (js-get-length arr #f %this))))
			      (array-copy! o vlen arr (- len vlen) end)))
			  (else
			   (array-slice! o start end)))))
		   (else
		    (array-slice! o k final)))))
	    ((not (isa? o JsArray))
	     (array-slice! o k final))
	    (else
	     (with-access::JsArray o (vec ilen)
		(let ((vlen (uint32->fixnum ilen)))
		   (cond
		      ((<= final vlen)
		       (vector-slice! o vec (->fixnum k) (->fixnum final)))
		      ((>fx vlen 0)
		       (let* ((arr (vector-slice! o vec (->fixnum k) vlen))
			      (vlen (->fixnum (js-get-length arr #f %this))))
			  (array-copy! o vlen arr (- len vlen) final)))
		      (else
		       (array-slice! o k final)))))))))
      
   (js-bind! %this js-array-prototype 'slice
      :value (js-make-function %this array-prototype-slice 2 'slice
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; sort
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.11
   (define (array-prototype-sort this::obj comparefn)

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
	    ((not (isa? comparefn JsFunction))
	     (js-raise-type-error %this
		"sort: argument not a function ~s" comparefn))
	    (else
	     (with-access::JsFunction comparefn (proc)
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
	 (let ((len (uint32->integer
		       (js-touint32 (js-get-length arr #f %this) %this))))
	    (unless (< len 2)
	       (quicksort arr cmp 0 (- len 1)))
	    arr))

      (let ((o (js-toobject %this this)))
	 (if (not (isa? this JsArray))
	     (array-sort this (get-compare comparefn))
	     (with-access::JsArray this (vec)
		(cond
		   ((js-array-full-inlined? this)
		    (vector-sort this (get-compare comparefn)))
		   ((=u32 (js-touint32 (js-get-length o #f %this) %this) #u32:0)
		    this)
		   (else
		    (array-sort this (get-compare comparefn))))))))

   (js-bind! %this js-array-prototype 'sort
      :value (js-make-function %this array-prototype-sort 1 'sort
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; splice
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.12
   (define (array-prototype-splice this::obj start deletecount . items)

      (define (vector-splice this len actualstart actualdeletecount)
	 (with-access::JsArray this (vec ilen)
	    (let* ((alen (vector-length vec))
		   (litems (length items))
		   (rlen (+fx actualstart actualdeletecount))
		   (nlen (+fx len (-fx litems actualdeletecount)))
		   (cstart (+fx actualstart actualdeletecount))
		   (vres (make-vector actualdeletecount))
		   (res (js-vector->jsarray vres %this)))
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
	     (len (uint32->integer (js-touint32 (js-get-length o #f %this) %this)))
	     (actualstart (if (< relstart 0) (max (+ len relstart) 0) (min relstart len)))
	     (actualdeletecount (min (max (js-tointeger deletecount %this) 0)
				   (- len actualstart))))
	 (if (not (isa? this JsArray))
	     (array-splice this len actualstart actualdeletecount)
	     (with-access::JsArray this (vec)
		(cond
		   ((js-array-full-inlined? o)
		    (vector-splice this len
		       (->fixnum actualstart) (->fixnum actualdeletecount)))
		   (else
		    (array-splice this len actualstart actualdeletecount)))))))

   (js-bind! %this js-array-prototype 'splice
      :value (js-make-function %this array-prototype-splice 2 'splice
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
		  (set! vec nvec)
		  (js-put-length! arr (uint32->fixnum nlen) #f #f %this)
		  (with-access::JsArray arr (length)
		     (set! ilen length))
		  (uint32->fixnum nlen)))))
      
      (define (array-unshift arr len::uint32)
	 (let ((rest (array-get-elements arr 0 (uint32->integer len) %this))
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
	 (if (isa? this JsArray)
	     (with-access::JsArray this (length)
		(cond
		   ((null? items)
		    (uint32->integer length))
		   ((js-array-full-inlined? this)
		    (vector-unshift this))
		   (else
		    (array-unshift this length))))
	     (let ((len::uint32 (js-touint32 (js-get-length o #f %this) %this)))
		(if (null? items)
		    (let ((nlen (uint32->integer len)))
		       ;; override the length as the conversion touin32
		       ;; might have changed it
		       (js-put-length! o nlen #f #f %this)
		       nlen)
		    (array-unshift this len))))))

   (js-bind! %this js-array-prototype 'unshift
      :value (js-make-function %this array-prototype-unshift 1 'unshift
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; indexOf
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.14
   (define (array-prototype-indexof this::obj el . indx)

      (define (vector-indexof::int arr vec k::int len::int)
	 (let loop ((k k))
	    (cond
	       ((>=fx k len)
		-1)
	       ((js-strict-equal? (vector-ref-ur vec k) el)
		k)
	       (else
		(loop (+fx k 1))))))

      (define (array-indexof::int arr k::uint32 len::uint32)
	 (let ((k (uint32->integer k))
	       (len (uint32->integer len)))
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

      (let* ((o (js-toobject %this this))
	     (len::uint32 (js-touint32 (js-get-length o #f %this) %this)))
	 (if (=u32 len #u32:0)
	     -1
	     (let ((n (if (pair? indx) (js-tointeger (car indx) %this) 0)))
		(if (<=uint32 len n)
		    -1
		    (let ((k (if (< n 0)
				 (let ((absn (abs n)))
				    (if (<=uint32 len absn)
					#u32:0
					(-u32 len (->uint32 absn))))
				 (->uint32 n))))
		       (if (isa? o JsArray)
			   (with-access::JsArray o (vec ilen)
			      (if (js-array-full-inlined? o)
				  (vector-indexof o vec (uint32->fixnum k)
				     (uint32->fixnum ilen))
				  (array-indexof this k len)))
			   (array-indexof o k len))))))))

   (js-bind! %this js-array-prototype 'indexOf
      :value (js-make-function %this array-prototype-indexof 1 'indexOf
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
      
      (define (array-lastindexof::int arr k::uint32)
	 (let loop ((k k))
	    (cond
	       ((let ((name (js-toname k %this)))
		   (and (js-has-property arr name %this)
			(js-strict-equal? (js-get arr name %this) el)))
		(uint32->integer k))
	       ((=u32 k #u32:0)
		-1)
	       (else
		(loop (-u32 k #u32:1))))))
      
      (define (lastindexof::int o::JsObject k::uint32)
	 (if (isa? o JsArray)
	     (with-access::JsArray o (vec ilen)
		(if (and (js-array-full-inlined? o) (<u32 k ilen))
		    (vector-lastindexof o vec
		       (minfx (uint32->fixnum k)
			  (uint32->fixnum (-u32 ilen #u32:1))))
		    (array-lastindexof o k)))
	     (array-lastindexof o k)))
      
      (let* ((o (js-toobject %this this))
	     (len::uint32 (js-touint32 (js-get-length o #f %this) %this)))
	 (if (=u32 len #u32:0)
	     -1
	     (let ((n (if (pair? indx)
			  (js-tointeger (car indx) %this)
			  (uint32->integer (-u32 len #u32:1)))))
		(if (< n 0)
		    (let ((absn (abs n)))
		       (if (>uint32 len absn)
			   (lastindexof o (-u32 len (->uint32 (abs n))))
			   -1))
		    (if (>=uint32 (-u32 len #u32:1) n)
			(lastindexof o (->uint32 n))
			(lastindexof o (-u32 len #u32:1))))))))

   (js-bind! %this js-array-prototype 'lastIndexOf
      :value (js-make-function %this array-prototype-lastindexof 1 'lastIndexOf
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; every
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.16
   (define (array-prototype-every this::obj proc t)

      (define (test-val proc t v i::uint32 o)
	 (js-totest (js-call3 %this proc t v (uint32->integer i) o)))

      (define (vector-every o len::uint32 proc t i::uint32)
	 (with-access::JsArray o (vec ilen)
	    (let loop ((i i))
	       (cond
		  ((>=u32 i ilen)
		   (or (js-array-full-inlined? o) (array-every o len proc t i)))
		  (else
		   (let ((v (u32vref vec i)))
		      (cond
			 ((test-val proc t v i o)
			  (loop (+u32 i 1)))
			 (else
			  #f))))))))

      (define (array-every o len::uint32 proc t i::uint32)
	 (let loop ((i i))
	    (if (>=u32 i len)
		#t
		(let* ((n (js-toname i %this))
		       (pv (js-get-property-value o o n %this)))
		   (cond
		      ((js-absent? pv)
		       (loop (+u32 i 1)))
		      ((test-val proc t pv i o)
		       (loop (+u32 i 1)))
		      (else
		       #f))))))

      (array-prototype-iterator %this this proc t array-every vector-every))
   
   (js-bind! %this js-array-prototype 'every
      :value (js-make-function %this array-prototype-every 1 'every
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; some
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.17
   (define (array-prototype-some this::obj proc t)

      (define (test-val proc t v i::uint32 o)
	 (js-totest (js-call3 %this proc t v (uint32->integer i) o)))

      (define (vector-some o len::uint32 proc t i::uint32)
	 (with-access::JsArray o (vec ilen)
	    (let loop ((i i))
	       (cond
		  ((>=u32 i ilen)
		   (unless (js-array-full-inlined? o)
		      (array-some o len proc t i)))
		  (else
		   (let ((v (vector-ref vec (uint32->fixnum i))))
		      (cond
			 ((test-val proc t v i o)
			  #t)
			 (else
			  (loop (+u32 i 1))))))))))

      (define (array-some o len proc t i::uint32)
	 (let loop ((i i))
	    (if (>=u32 i len)
		#f
		(let ((pv (js-get-property-value o o (js-toname i %this) %this)))
		   (cond
		      ((js-absent? pv)
		       (loop (+u32 i 1)))
		      ((test-val proc t pv i o)
		       #t)
		      (else
		       (loop (+u32 i 1))))))))

      (array-prototype-iterator %this this proc t array-some vector-some))

   (js-bind! %this js-array-prototype 'some
      :value (js-make-function %this
		array-prototype-some 1 'some
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; forEach
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.18
   (define (array-prototype-foreach this::obj proc t)

      (define (vector-foreach o len::uint32 proc t i::uint32)
	 [%assert-array! o "vector-foreach"]
	 (if (js-array-full-inlined? o)
	     (with-access::JsArray o (vec ilen)
		(let loop ((i i))
		   (cond
		      ((>=u32 i ilen)
		       (js-undefined))
		      ((not (js-array-full-inlined? o))
		       (array-foreach o len proc t i))
		      (else
		       (let ((v (vector-ref vec (uint32->fixnum i))))
			  (js-call3 %this proc t v (uint32->integer i) o)
			  (loop (+u32 i 1)))))))
	     (array-foreach o len proc t i)))

      (define (array-foreach o len proc t i::uint32)
	 (let loop ((i i))
	    (when (<u32 i len)
	       (let ((pv (js-get-property-value o o
			    (js-toname i %this) %this)))
		  (unless (js-absent? pv)
		     (js-call3 %this proc t pv (uint32->fixnum i) o))
		  (loop (+u32 i 1))))))

      (array-prototype-iterator %this this proc t array-foreach vector-foreach)
      (js-undefined))

   (js-bind! %this js-array-prototype 'forEach
      :value (js-make-function %this array-prototype-foreach 1 'forEach
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; map
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.19
   (define (make-array-prototype-map %this::JsGlobalObject)
      (lambda (this::obj proc t)

	 (define (array-map/array o len proc t i::uint32 a::JsArray)
	    (let loop ((i i))
	       (if (<u32 i len)
		   (let ((pv (js-get-property-value o o (js-toname i %this) %this)))
		      (unless (js-absent? pv)
			 (js-put! a i
			    (js-call3 %this proc t pv (uint32->integer i) o)
			    #f %this))
		      (loop (+u32 i 1)))
		   a)))

	 (define (vector-map o len::uint32 proc::JsFunction t i::uint32)
	    (with-access::JsArray o (vec ilen length)
	       (let ((v (make-vector (vector-length vec) (js-undefined))))
		  (let loop ((i i))
		     (cond
			((>=u32 i ilen)
			 (let ((a (js-vector->jsarray v %this)))
			    (if (=u32 i len)
				(with-access::JsArray a (length ilen)
				   (set! length len)
				   (set! ilen len)
				   a)
				;; the array has been uninlined by the callback
				(array-map/array o len proc t i a))))
			(else
			 (let ((val (vector-ref-ur vec (uint32->fixnum i))))
			    (vector-set-ur! v (uint32->fixnum i)
			       (js-call3 %this proc t val
				  (uint32->integer i) o))
			    (loop (+u32 i 1)))))))))

	 (define (array-map o len proc t i::uint32)
	    (let ((a (js-array-construct/length %this (js-array-alloc %this)
			(uint32->integer len))))
	       (array-map/array o len proc t i a)))

	 (array-prototype-iterator %this this proc t array-map vector-map)))

   (js-bind! %this js-array-prototype 'map
      :value (js-make-function %this
		(make-array-prototype-map %this) 1 'map
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; filter
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.20
   (define (array-prototype-filter this::obj proc t)

      (define (array-filter/array o len proc t i::uint32 j::uint32 a)
	 (let loop ((i i)
		    (j::uint32 j))
	    (if (<u32 i len)
		(let ((pv (js-get-property-value o o (js-toname i %this) %this)))
		   (if (js-absent? pv)
		       (loop (+u32 i 1) j)
		       (let ((v pv)
			     (nj (js-toname j %this)))
			  (if (js-totest (js-call3 %this proc t v
					    (uint32->integer i) o))
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
      
      (define (vector-filter o len::uint32 proc t i::uint32)
	 [%assert-array! o "vector-filter"]
	 (with-access::JsArray o (vec ilen length)
	    (if (js-array-full-inlined? o)
		(let ((v (make-vector (vector-length vec) (js-undefined))))
		   (let loop ((i i)
			      (j 0))
		      (cond
			 ((>=u32 i ilen)
			  (let ((a (js-vector->jsarray v %this)))
			     (with-access::JsArray a (length ilen)
				(set! length j)
				(set! ilen j)
				(if (js-array-full-inlined? o)
				    a
				    (array-filter/array o len proc t i j a)))))
			 (else
			  (let ((val (vector-ref-ur vec (uint32->fixnum i))))
			     (cond
				((js-totest (js-call3 %this proc t val
					       (uint32->integer i) o))
				 (vector-set-ur! v (uint32->fixnum j) val)
				 (loop (+u32 i 1) (+u32 j 1)))
				(else
				 (loop (+u32 i 1) j))))))))
		(array-filter o len proc t i))))

      (define (array-filter o len proc t i::uint32)
	 (let ((a (js-vector->jsarray '#() %this)))
	    (array-filter/array o len proc t i 0 a)))

      (array-prototype-iterator %this this proc t array-filter vector-filter))

   (js-bind! %this js-array-prototype 'filter
      :value (js-make-function %this
		array-prototype-filter 1 'filter
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)


   ;; find
   ;; http://www.ecma-international.org/ecma-262/7.0/#sec-indexed-collections#sec-array.prototype.find
   (define (array-prototype-find this::obj proc t)

      (define (vector-find o::JsArray len::uint32 proc::JsFunction t i::uint32)
	 (with-access::JsArray o (vec ilen)
	    (let loop ((i i))
	       (cond
		  ((>=u32 i ilen)
		   (unless (js-array-full-inlined? o)
		      (array-find o len proc t i)))
		  (else
		   (let ((v (vector-ref-ur vec (uint32->fixnum i))))
		      (cond
			 ((js-totest (js-call3 %this proc t v (uint32->integer i) o))
			  v)
			 (else
			  (loop (+u32 i 1))))))))))

      (define (array-find o::JsArray len::uint32 proc::JsFunction t i::uint32)
	 (let loop ((i i))
	    (if (>=u32 i len)
		(js-undefined)
		(let* ((pv (js-get-property-value o o (js-toname i %this) %this))
		       (v (if (js-absent? pv) (js-undefined) pv)))
		   (if (js-totest (js-call3 %this proc t v (uint32->integer i) o))
		       v
		       (loop (+u32 i 1)))))))

      (array-prototype-iterator %this this proc t array-find vector-find))

   (js-bind! %this js-array-prototype 'find
      :value (js-make-function %this
		array-prototype-find 1 'find
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
		(let ((pv (js-get-property-value o o (js-toname i %this) %this)))
		   (if (js-absent? pv)
		       (loop (+u32 i #u32:1) acc)
		       (let ((v pv))
			  (loop (+u32 i #u32:1)
			     (js-call4 %this proc (js-undefined) acc v
				(uint32->integer i) o)))))
		acc)))
      
      (let* ((o (js-toobject %this this))
	     (len::uint32 (js-touint32 (js-get-length o #f %this) %this)))
	 (if (not (isa? proc JsFunction))
	     (js-raise-type-error %this "Not a procedure ~s" proc)
	     ;; find the accumulator init value
	     (if (null? init)
		 (let loop ((i #u32:0))
		    (if (<u32 i len)
			(let ((pv (js-get-property-value o o (js-toname i %this) %this)))
			   (if (js-absent? pv)
			       (loop (+u32 i #u32:1))
			       (reduce/accumulator o len (+u32 i #u32:1) pv)))
			(js-raise-type-error %this
			   "reduce: cannot find accumulator ~s" this)))
		 (reduce/accumulator o len #u32:0 (car init))))))
   
   (js-bind! %this js-array-prototype 'reduce
      :value (js-make-function %this
		array-prototype-reduce 1 'reduce
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; reduceRight
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.22
   (define (array-prototype-reduceright this::obj proc . init)
      
      (define (reduce/accumulator o len::uint32 i::uint32 accumulator)
	 (let loop ((i i)
		    (acc accumulator))
	    (let ((pv (js-get-property-value o o (js-toname i %this) %this)))
	       (if (<u32 i len)
		   (if (js-absent? pv)
		       (loop (-u32 i #u32:1) acc)
		       (let* ((v pv)
			      (acc (js-call4 %this proc (js-undefined) acc v
				      (uint32->integer i) o)))
			  (loop (-u32 i #u32:1) acc)))
		   acc))))
      
      (let* ((o (js-toobject %this this))
	     (len::uint32 (js-touint32 (js-get-length o #f %this) %this)))
	 (if (not (isa? proc JsFunction))
	     (js-raise-type-error %this "Not a procedure ~s" proc)
	     ;; find the accumulator init value
	     (if (null? init)
		 (let loop ((k (-u32 len #u32:1)))
		    (if (<u32 k len)
			(let ((pv (js-get-property-value o o (js-toname k %this) %this)))
			   (if (js-absent? pv)
			       (loop (-u32 k #u32:1))
			       (let ((v pv))
				  (reduce/accumulator o len (-u32 k #u32:1) v))))
			(js-raise-type-error %this
			   "reduce: cannot find accumulator ~s" this)))
		 (reduce/accumulator o len (-u32 len #u32:1) (car init))))))
   
   (js-bind! %this js-array-prototype 'reduceRight
      :value (js-make-function %this array-prototype-reduceright 1 'reduceRight
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; iterator
   ;; http://www.ecma-international.org/ecma-262/6.0/#sec-22.1.3.30
   (define (array-prototype-array-values this::obj)
      (js-make-iterator this %this))
   
   (with-access::JsGlobalObject %this (js-symbol-iterator)
      (js-bind! %this js-array-prototype js-symbol-iterator
	 :value (js-make-function %this array-prototype-array-values
		   0 '@@iterator
		   :prototype (js-undefined))
	 :enumerable #f
	 :hidden-class #t))

   ;; fill
   ;; http://www.ecma-international.org/ecma-262/6.0/#sec-array.prototype.fill
   (define (array-prototype-fill this::obj value start end)
      (js-array-maybe-fill this value start end %this))
   
   (js-bind! %this js-array-prototype 'fill
      :value (js-make-function %this array-prototype-fill 1 'fill
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)   

   ;; arrayComprehension
   ;; http://wiki.ecmascript.org/doku.php?id=harmony:array_comprehensions
   ;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Array_comprehensions
   (define (array-prototype-comprehension iterables::pair fun test _names
	      _astp _aste _astd)

      (define (prod2 l1 l2)
	 (append-map (lambda (a)
			(map (lambda (b) (list a b)) l2))
	    l1))

      (define (prod l)
	 (cond
	    ((null? l)
	     '())
	    ((null? (cdr l))
	     (car l))
	    ((null? (cddr l))
	     (prod2 (car l) (cadr l)))
	    (else
	     (let ((r (prod (cdr l))))
		(append-map (lambda (a) (map (lambda (r) (cons a r)) r))
		   (car l))))))

      
      (define (iterables->lists iterables)
	 
	 (define jsid (js-make-function %this (lambda (this n) n) 1 "id"))
	 
	 (map (lambda (el)
		 (if (isa? el JsArray)
		     (vector->list (jsarray->vector el %this))
		     (let* ((jsmap (js-get el 'map %this))
			    (arr (js-call1 %this jsmap el jsid)))
			(vector->list (jsarray->vector el %this)))))
	    iterables))

      (define (fast-comprehension)
	 (let ((this (car iterables)))
	    (if (eq? test #t)
		(let ((jsmap (js-get this 'map %this)))
		   (js-call1 %this jsmap this fun))
		(let* ((jsfilter (js-get this 'filter %this))
		       (arr (js-call1 %this jsfilter this test))
		       (jsmap (js-get arr 'map %this)))
		   (js-call1 %this jsmap arr fun)))))

      (if (null? (cdr iterables))
	  ;; fast path, only one array
	  (fast-comprehension)
	  ;; full path, multiple iterables
	  (let* ((this (car iterables))
		 (lsts (prod (iterables->lists iterables)))
		 (res (if (eq? test #t)
			  (map (lambda (l)
				  (js-apply %this fun this l))
			     lsts)
			  (filter-map (lambda (l)
					 (let ((t (js-apply %this test this l)))
					    (when (js-totest t)
					       (js-apply %this fun this l))))
			     lsts))))
	     (js-vector->jsarray (list->vector res) %this))))

   (js-bind! %this js-array-prototype 'comprehension
      :value (js-make-function %this array-prototype-comprehension 6
		'comprehension
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t))

;*---------------------------------------------------------------------*/
;*    %js-array ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.1       */
;*---------------------------------------------------------------------*/
(define (%js-array %this::JsGlobalObject)
   (lambda (this . items)
      (with-access::JsGlobalObject %this (js-array)
	 (js-array-construct %this (js-array-alloc %this) items))))

;*---------------------------------------------------------------------*/
;*    js-array-alloc ...                                               */
;*---------------------------------------------------------------------*/
(define (js-array-alloc::JsArray %this)
   (with-access::JsGlobalObject %this (js-array-prototype)
      (instantiate::JsArray
	 (cmap (js-not-a-cmap))
	 (__proto__ js-array-prototype))))

;*---------------------------------------------------------------------*/
;*    js-array-alloc ...                                               */
;*---------------------------------------------------------------------*/
(define (js-array-alloc-ctor::JsArray constructor::JsFunction %this)
   (with-access::JsGlobalObject %this (js-array js-array-prototype)
      (let ((proto (if (eq? constructor js-array)
		       js-array-prototype
		       (js-get constructor 'prototype %this))))
	 (instantiate::JsArray
	    (cmap (js-not-a-cmap))
	    (__proto__ proto)))))

;*---------------------------------------------------------------------*/
;*    js-array-construct ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.2.1     */
;*---------------------------------------------------------------------*/
(define (js-array-construct/length %this::JsGlobalObject this::JsArray len)

   (define (array-set! v::vector iln::uint32 ulen::uint32)
      (with-access::JsArray this (vec ilen length)
	 (set! length ulen)
	 (set! ilen iln)
	 (set! vec v))
      this)

   (cond
      ((not (=uint32 (js-touint32 len %this) len))
       (js-raise-range-error %this "index out of range ~a" len))
      ((<= len (bit-lsh 1 16))
       ;; MS CARE: the max boundary for a concrete vector
       ;; is pure heuristic. This should be confirmed by
       ;; actual tests
       (let* ((len (->fixnum len))
	      (vec (make-vector len (js-undefined))))
	  (array-set! vec #u32:0 (fixnum->uint32 len))))
      (else
       (array-set! (make-vector 10 (js-undefined)) #u32:0
	  (if (fixnum? len) (fixnum->uint32 len) (flonum->uint32 len))))))

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
      ((and (number? (car items-or-len)) (null? (cdr items-or-len)))
       (js-array-construct/length %this this (car items-or-len)))
      (else
       (let* ((vec (list->vector items-or-len))
	      (len (vector-length vec)))
	  (array-set! vec (fixnum->uint32 len) (fixnum->uint32 len))))))

;*---------------------------------------------------------------------*/
;*    js-vector->jsarray ...                                           */
;*---------------------------------------------------------------------*/
(define (js-vector->jsarray::JsArray vec::vector %this::JsGlobalObject)
   ;; MS 23 feb 2017
   (let* ((len (vector-length vec)))
      (with-access::JsGlobalObject %this (js-array-prototype)
	 (instantiate::JsArray
	    (__proto__ js-array-prototype)
	    (length (fixnum->uint32 len))
	    (ilen (fixnum->uint32 len))
	    (vec vec)))))

;*---------------------------------------------------------------------*/
;*    js-vector->sparse-jsarray ...                                    */
;*---------------------------------------------------------------------*/
(define (js-vector->sparse-jsarray vec::vector %this::JsGlobalObject)
   (let ((arr (js-vector->jsarray vec %this)))
      (let loop ((i (-fx (vector-length vec) 1)))
	 (if (=fx i -1)
	     (begin
		(js-object-mode-inline-set! arr #f)
		arr)
	     (begin
		(when (js-absent? (vector-ref-ur vec i))
		   (js-delete! arr i #f %this))
		(loop (-fx i 1)))))))
	     
;*---------------------------------------------------------------------*/
;*    js-empty-vector->jsarray ...                                     */
;*---------------------------------------------------------------------*/
(cond-expand
   (bigloo-c
    (define-inline (js-empty-vector->jsarray::JsArray %this::JsGlobalObject)
       (let ((mode (js-object-default-mode)))
	  (with-access::JsGlobalObject %this (js-array-prototype)
	     ($js-make-jsarray (DEFAULT-EMPTY-ARRAY-SIZE) (js-not-a-cmap)
		js-array-prototype mode)))))
   (else
    (define (js-empty-vector->jsarray::JsArray %this::JsGlobalObject)
       (let ((mode (js-object-default-mode)))
	  (with-access::JsGlobalObject %this (js-array-prototype)
	     (let ((vec (make-vector (DEFAULT-EMPTY-ARRAY-SIZE) (js-undefined))))
		(instantiate::JsArray
		   (__proto__ js-array-prototype)
		   (length #u32:0)
		   (ilen #u32:0)
		   (vec vec))))))))

;*---------------------------------------------------------------------*/
;*    js-properties-name ::JsArray ...                                 */
;*---------------------------------------------------------------------*/
(define-method (js-properties-name obj::JsArray enump %this)
   (js-array-update-length-property! obj)
   (with-access::JsArray obj (vec ilen)
      (let loop ((i (-fx (uint32->fixnum ilen) 1))
		 (acc '()))
	 (if (=fx i -1)
	     (vector-append (apply vector acc) (call-next-method))
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
	    ((eq? p 'length) #t)
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
	     (if (eq? p 'length)
		 (js-array-update-length-property! o)
		 (call-next-method)))
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
	    ;; MS: 23 feb 2017
	    ((not (js-isindex? i))
	     (if (eq? p 'length)
		 (uint32->integer length)
		 (call-next-method)))
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-get ::JsArray ...                                             */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsArray p %this)
   (with-access::JsArray o (vec ilen length)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((<u32 i ilen)
	     (u32vref vec i))
	    ;; MS: 23 feb 2017
	    ((not (js-isindex? i))
	     (if (eq? p 'length)
		 (uint32->integer length)
		 (call-next-method)))
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-get-name/cache-miss ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-get-name/cache-miss o::JsArray p::obj cache::JsPropertyCache throw %this)
   (with-access::JsArray o (vec ilen length)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((<u32 i ilen)
	     (u32vref vec i))
	    ;; MS: 23 feb 2017
	    ((not (js-isindex? i))
	     (if (eq? p 'length)
		 (uint32->integer length)
		 (call-next-method)))
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
;*    js-array-vector-properties ...                                   */
;*    -------------------------------------------------------------    */
;*    Returns the subset of the array properties which are stored      */
;*    in its inline vector.                                            */
;*---------------------------------------------------------------------*/
(define (js-array-vector-properties::pair-nil obj::JsArray %this offset::uint32)
   (with-access::JsArray obj (vec ilen)
      (if (<u32 offset ilen)
	  (let loop ((i (-fx (uint32->fixnum ilen) 1))
		     (acc '()))
	     (if (<fx i (uint32->fixnum offset))
		 acc
		 (let ((v (vector-ref vec i)))
		    (let ((desc (instantiate::JsValueDescriptor
				   (name (js-toname i %this))
				   (value v)
				   (writable #t)
				   (enumerable #t)
				   (configurable #t))))
		       (loop (-fx i 1) (cons desc acc))))))
	  '())))

;*---------------------------------------------------------------------*/
;*    uninline-array! ...                                              */
;*---------------------------------------------------------------------*/
(define (uninline-array! arr::JsArray %this::JsGlobalObject
	   #!optional (offset #u32:0))
   ;; this function switches from a fast inlined array representation
   ;; to a slow inefficient object representation
   (with-access::JsArray arr (vec ilen properties)
      (js-object-mode-inline-set! arr #f)
      (js-array-update-length-property! arr)
      (when (>fx (vector-length vec) 0)
	 (let ((plen (car properties)))
	    (set! properties
	       (cons plen
		  (append! (js-array-vector-properties arr %this offset)
		     (cdr properties)))))
	 (set! ilen offset)
	 (set! *JS-ARRAY-MARK* (+fx 1 *JS-ARRAY-MARK*)))
      arr))

;*---------------------------------------------------------------------*/
;*    reinline-array! ...                                              */
;*    -------------------------------------------------------------    */
;*    When the inline portion of an array is expanded it might         */
;*    reach a non inline property that after expansion can be          */
;*    included in the inline part. This is handled by this function.   */
;*---------------------------------------------------------------------*/
(define (reinline-array! o::JsArray nilen %this)
   (with-access::JsArray o (vec ilen length cmap properties)
      (js-object-mode-inline-set! o #t)
      (let ((len (minu32 (fixnum->uint32 (vector-length vec)) length)))
	 (let loop ((i nilen))
	    (if (<u32 i len)
		(let ((d (js-get-own-property o (js-toname i %this) %this)))
		   (if (js-array-inline-value-descriptor? d)
		       (if (not (eq? cmap (js-not-a-cmap)))
			   (error "reinilne-array!" "array cmap not implemented" i)
			   (begin
			      (set! properties (remq! d properties))
			      (with-access::JsValueDescriptor d (value)
				 (vector-set! vec (uint32->fixnum i) value)
				 (loop (+u32 i 1)))))
		       (set! ilen i)))
		(set! ilen i))))))

;*---------------------------------------------------------------------*/
;*    js-put! ::JsArray ...                                            */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::JsArray p v throw %this)
   (js-array-put! o p v throw %this))
   
;*---------------------------------------------------------------------*/
;*    js-array-put! ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.5       */
;*---------------------------------------------------------------------*/
(define (js-array-put! o::JsArray p v throw %this)
   
   (define (aput! o::JsArray p::obj v)
      (if (not (js-can-put o p %this))
	  ;; 1
	  (if throw
	      (js-raise-type-error %this
		 (if (js-object-mode-extensible? o)
		     "Can't add property ~a"
		     "Can't add property ~a, object not extensible")
		 p)
	      (js-undefined))
	  (let ((owndesc (js-get-own-property o p %this)))
	     ;; 2
	     (if (js-is-data-descriptor? owndesc)
		 ;; 3
		 (if (eq? p 'length)
		     (let ((newdesc (duplicate::JsValueDescriptor owndesc
				       (value v))))
			(js-array-update-length-property! o)
			(js-define-own-property o p newdesc throw %this))
		     (with-access::JsValueDescriptor owndesc ((valuedesc value))			
			(set! valuedesc v)
			(js-define-own-property o p owndesc throw %this)))
		 (let ((desc (js-get-property o p %this)))
		    ;; 4
		    (if (js-is-accessor-descriptor? desc)
			;; 5
			(with-access::JsAccessorDescriptor desc ((setter set))
			   (if (isa? setter JsFunction)
			       (js-call1 %this setter o v)
			       (js-undefined)))
			(let ((newdesc (instantiate::JsValueDescriptor
					  (name p)
					  (value v)
					  (writable #t)
					  (enumerable #t)
					  (configurable #t))))
			   ;; 6
			   (js-define-own-property o p newdesc throw %this)))))
	     v)))
   
   (with-access::JsArray o (vec ilen)
      (let ((idx::uint32 (js-toindex p)))
	 (cond
	    ((<u32 idx ilen)
	     (vector-set-ur! vec (uint32->fixnum idx) v)
	     v)
	    ((<u32 idx (fixnum->uint32 (vector-length vec)))
	     (with-access::JsArray o (length)
		(cond
		   ((not (array-extensible? o))
		    (if throw
			(js-raise-type-error %this
			   "Can't add property ~a, object is not extensible" p)
			v))
		   ((and (=u32 idx ilen) (js-array-full-inlined? o))
		    (vector-set-ur! vec (uint32->fixnum idx) v)
		    (let ((nilen (+u32 ilen #u32:1)))
		       (set! ilen nilen)
		       (when (>=u32 idx length)
			  (set! length nilen)))
		    v)
		   (else
		    (aput! o (js-toname p %this) v)))))
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
   (js-array-put! o 'length v throw %this)
   (with-access::JsArray o (length ilen properties)
      (let ((len (->uint32 v)))
	 (set! length len)
	 (when (<u32 len ilen) (set! ilen len))
	 (%assert-array! o "js-put-length!"))))

;*---------------------------------------------------------------------*/
;*    js-delete! ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (js-delete! o::JsArray p throw %this)
   (with-access::JsArray o (vec properties length ilen)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((and (=uint32 (+u32 i #u32:1) ilen) (js-isindex? i))
	     (u32vset! vec i (js-undefined))
	     (set! ilen i)
	     (js-object-mode-inline-set! o #f)
	     #t)
	    ((<u32 i ilen)
	     (unless (js-object-mode-frozen? o)
		(uninline-array! o %this i)
		(call-next-method)))
	    ((or (eq? p 'length) (eq? (js-toname p %this) 'length))
	     #f)
	    (else
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
   (when (js-array-inlined? vec)
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
	 (with-access::JsObject o (cmap properties __proto__)
	    (append (if (not (eq? cmap (js-not-a-cmap)))
			(with-access::JsConstructMap cmap (names)
			   (vector->list names))
			(map (lambda (d)
				(with-access::JsPropertyDescriptor d (name)
				   name))
			   properties))
	       (if (isa? __proto__ JsObject)
		   (loop __proto__)
		   '())))))
   
   (define (sym->string n)
      (cond
	 ((symbol? n)
 	  (symbol->string! n))
	 ((isa? n JsSymbolLiteral)
	  (with-access::JsSymbolLiteral n (val) val))
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
			  (vector-set-ur! vec i (js-undefined))
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
					 (set! value (uint32->integer (+u32 num #u32:1)))
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
      (with-access::JsArray a (length ilen properties)
	 (let ((old (car properties)))
	    (let ((r (js-define-own-property% a 'length
			newlendesc throw %this)))
	       (when r
		  (with-access::JsValueDescriptor (car properties) (value)
		     (with-handler
			exception-notify
			(let ((ulen (->uint32 value)))
			   (set! length ulen)
			   (cond
			      ((<u32 ulen ilen)
			       (set! ilen ulen))
			      ((>u32 ulen ilen)
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
			 (set! value (uint32->integer newlen)))
		      (if (>=uint32 newlen oldlen)
			  ;; 3.f
			  (js-define-own-length% a newlendesc throw %this)
			  (if (not owritable)
			      ;; 3.g
			      (reject "property read-only \"~a\"")
			      ;; 3.h
			      (let* ((inlp (js-array-full-inlined? a))
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
						     (name 'length)
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
	 (unless (isa? get JsFunction) (set! get (js-undefined)))
	 (unless (isa? set JsFunction) (set! set (js-undefined)))))
   
   (define (js-default-array-generic-property! desc)
      (cond
	 ((isa? desc JsDataDescriptor) (js-default-array-data-property! desc))
	 ((isa? desc JsAccessorDescriptor) (js-default-array-accessor-property! desc))
	 (else (js-default-array-property! desc))))
   
   (define (js-define-own-property-array a p::uint32 desc throw)
      (with-access::JsArray a (vec ilen length)
	 [assert (p) (<u32 p (fixnum->uint32 (vector-length vec)))]
	 (cond
	    ((and (>u32 p ilen) (not (js-object-mode-extensible? a)))
	     (let ((r (js-define-own-property% a
			 (js-toname p %this) desc #f %this)))
		(unless (js-array-inline-value-descriptor? desc)
		   (uninline-array! a %this))
		r))
	    ((isa? desc JsValueDescriptor)
	     (with-access::JsValueDescriptor desc (value)
		(if (js-array-inline-value-descriptor? desc)
		    (cond
		       ((<u32 p ilen)
			(u32vset! vec p value))
		       ((=u32 p ilen)
			(u32vset! vec p value)
			(reinline-array! a (+u32 ilen #u32:1) %this)
			#t)
		       (else
			;; MS: 22 feb 2017
			;; (uninline-array! a %this)
			(js-object-mode-inline-set! a #f)
			(js-define-own-property% a (js-toname p %this) desc #f %this)))
		    (begin
		       (uninline-array! a %this)
		       (js-define-own-property% a (js-toname p %this) desc #f %this)))))
	    ((isa? desc JsAccessorDescriptor)
	     (uninline-array! a %this)
	     (js-define-own-property% a (js-toname p %this) desc #f %this))
	    (else
	     (uninline-array! a %this)
	     (js-define-own-property% a (js-toname p %this) desc #f %this)))))

   (if (eq? p 'length)
       ;; 3
       (define-own-property-length (js-get-own-property a 'length %this))
       (let ((index::uint32 (js-toindex p)))
	  (if (js-isindex? index)
	      ;; 4
	      (with-access::JsArray a (vec ilen length)
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
					(let ((olen (vector-length vec))
					      (nlen (uint32->fixnum len)))
					   ;; MS CARE
;* 					      (set! ilen len)          */
					   (set! vec (copy-vector-fill! vec nlen (js-undefined))))
					(js-define-own-property-array
					   a index desc #f)))
				    (else
				     ;; slow access
				     (uninline-array! a %this)
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
				     (js-define-own-property a 'length
					oldlendesc #f %this)))
			       ;; 4.f
			       #t))))))
	      ;; 5
	      (call-next-method)))))
   
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
(define (array-prototype-iterator %this::JsGlobalObject
	   this proc t
	   array-iterator::procedure vector-iterator::procedure)
   ;; length must be evaluated before checking the function
   ;; see ch15/15.4/15.4.4/15.4.4.16/15.4.4.16-4-8.js
   (let ((o (js-toobject %this this)))
      (cond
	 ((not (isa? o JsArray))
	  (let ((len (js-touint32 (js-get-length o #f %this) %this)))
	     (if (not (isa? proc JsFunction))
		 (js-raise-type-error %this "Not a procedure ~s" proc)
		 (array-iterator o len proc t #u32:0))))
	 (else
	  [%assert-array! o "array-prototype-iterator"]
	  (if (not (isa? proc JsFunction))
	      (js-raise-type-error %this "Not a procedure ~s" proc)
	      (with-access::JsArray o (length vec ilen)
		 (if (js-array-inlined? o)
		     (vector-iterator o length proc t #u32:0)
		     (array-iterator o length proc t #u32:0))))))))

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
;*    js-prevent-extensions ::JsArray ...                              */
;*---------------------------------------------------------------------*/
(define-method (js-prevent-extensions o::JsArray)
   (with-access::JsArray o (vec length)
      (when (<u32 length (fixnum->uint32 (vector-length vec)))
	 (vector-shrink! vec (uint32->fixnum length)))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    js-for-in ::JsArray ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-for-in o::JsArray proc %this)
   (with-access::JsArray o (vec ilen)
      (if (js-array-inlined? o)
	  (let ((len ilen))
	     (let loop ((i #u32:0))
		(if (<u32 i len)
		    (begin
		       (proc (js-integer->jsstring (uint32->fixnum i)))
		       (loop (+u32 i #u32:1)))
		    (call-next-method))))
	  (call-next-method))))
  
;*---------------------------------------------------------------------*/
;*    js-array-push ...                                                */
;*---------------------------------------------------------------------*/
(define (js-array-push o::JsArray item %this::JsGlobalObject)
   (with-access::JsArray o (length ilen vec)
      (let ((n length))
	 (cond
	    ((not (array-extensible? o))
	     (js-raise-type-error %this
		"Can't add property ~a: object is not extensible" length))
	    ((and (=u32 n ilen)
		  (<u32 ilen (fixnum->uint32 (vector-length vec))))
	     (let ((idx (+u32 n 1)))
		(vector-set-ur! vec (uint32->fixnum n) item)
		(set! ilen idx)
		(set! length idx)
		;; ms: 22 feb 2017
		;; (js-array-update-length! o (+fx (uint32->fixnum n) 1))
		(uint32->integer idx)))
	    ((=fx (vector-length vec) 0)
	     (set! vec (make-vector (DEFAULT-EMPTY-ARRAY-SIZE) (js-undefined)))
	     (set! ilen #u32:1)
	     (set! length #u32:1)
	     (vector-set-ur! vec 0 item)
	     1)
	    (else
	     (if (<u32 length #u32:4294967295)
		 (js-array-put! o (uint32->fixnum n) item #f %this)
		 (js-put! o n item #f %this))
	     (if (=u32 (+u32 n #u32:1) #u32:0)
		 (js-raise-range-error %this
		    "Illegal length: ~s"
		    (js-tostring #l4294967296 %this))
		 (uint32->integer (+u32 #u32:1 n))))))))

;*---------------------------------------------------------------------*/
;*    js-array-maybe-push ...                                          */
;*---------------------------------------------------------------------*/
(define (js-array-maybe-push this item %this)
   (if (isa? this JsArray)
       (js-array-push this item %this)
       (js-call1 %this (js-get this 'push %this) this item)))

;*---------------------------------------------------------------------*/
;*    js-array-pop ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-array-pop o %this)
   (with-access::JsArray o (length ilen vec)
      (let ((len::uint32 (js-touint32 (js-get-length o #f %this) %this)))
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
		(vector-set! vec (uint32->fixnum (-u32 ilen 1)) (js-undefined))
		(set! length idx)
		(set! ilen idx)
		;; ms: 22 feb 2017
		;;(js-put-length! o (uint32->integer idx) #f #f %this)
		el))
	    (else
	     (let* ((idx (-u32 len #u32:1))
		    (el (js-get o (uint32->integer idx) %this)))
		(js-delete! o idx #t %this)
		(js-put-length! o (uint32->integer idx) #f #f %this)
		el))))))

;*---------------------------------------------------------------------*/
;*    js-array-maybe-pop ...                                           */
;*---------------------------------------------------------------------*/
(define (js-array-maybe-pop this %this)
   (if (isa? this JsArray)
       (js-array-pop this %this)
       (js-call0 %this (js-get this 'pop %this) this)))

;*---------------------------------------------------------------------*/
;*    js-array-fill ...                                                */
;*---------------------------------------------------------------------*/
(define (js-array-fill this::JsArray value start end %this)
   (let* ((len::uint32 (js-touint32 (js-get-length this #f %this) %this))
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
	 (if (js-array-full-inlined? this)
	     (let loop ((i k))
		(if (<u32 i final)
		    (begin
		       (vector-set! vec (uint32->fixnum i) value)
		       (loop (+u32 i #u32:1)))
		    (when (>u32 i ilen)
		       (set! ilen i))))
	     (let loop ((i k))
		(if (<u32 i final)
		    (begin
		       (if (<u32 i ilen)
			   (vector-set! vec (uint32->fixnum i) value)    
			   (js-put! this i value #f %this))
		       (loop (+u32 i #u32:1)))
		    (when (>u32 i length)
		       (js-put-length! this (uint32->integer i) #f #f %this)))))
	 this)))
   
;*---------------------------------------------------------------------*/
;*    js-array-maybe-fill ...                                          */
;*---------------------------------------------------------------------*/
(define (js-array-maybe-fill this value start end %this)
   (if (isa? this JsArray)
       (js-array-fill this value start end %this)
       (js-call3 %this (js-get this 'fill %this) this value start end)))

;*---------------------------------------------------------------------*/
;*    js-array-comprehension ...                                       */
;*---------------------------------------------------------------------*/
(define (js-array-comprehension %this iterables fun test _names _astp _aste _astd)
   (let ((jscomp (js-get (car iterables) 'comprehension %this))
	 (len (length iterables)))
      (js-call6 %this jscomp iterables
	 (js-make-function %this fun len "comprehension-expr")
	 (if (eq? test #t)
	     #t
	     (js-make-function %this test len "comprehension-test"))
	 _names _astp _aste _astd)))
	
;*---------------------------------------------------------------------*/
;*    JsStringLiteral end                                              */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-end!)
