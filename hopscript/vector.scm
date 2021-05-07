;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/vector.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May  7 09:59:09 2021                          */
;*    Last change :  Fri May  7 17:15:38 2021 (serrano)                */
;*    Copyright   :  2021 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HopScript vectors.                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_vector
   
   (library hop)
   
   (include "../nodejs/nodejs_debug.sch"
	    "types.sch"
	    "stringliteral.sch"
	    "property.sch"
	    "index.sch")
   
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
	   __hopscript_names
	   __hopscript_array)

   (export (js-init-vector! ::JsGlobalObject)
	   (inline js-vector-alloc ::uint32 ::JsGlobalObject)
	   (js-vector-new1 ::obj ::JsGlobalObject)
	   (inline js-vector-length::uint32 ::JsArray)
	   (js-vector-get ::JsArray ::obj ::JsGlobalObject)
	   (js-vector-put! ::JsArray ::obj ::obj ::JsGlobalObject)
	   (inline js-vector-ref ::JsArray ::obj ::JsGlobalObject)
	   (inline js-vector-set! ::JsArray ::obj ::obj ::JsGlobalObject)
	   (inline js-vector-index-ref ::JsArray ::uint32 ::JsGlobalObject)
	   (inline js-vector-index-set! ::JsArray ::uint32 ::obj ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    js-init-vector! ...                                              */
;*---------------------------------------------------------------------*/
(define (js-init-vector! %this)
   (with-access::JsGlobalObject %this (js-vector js-vector-prototype
					 js-function js-array-cmap
					 js-array-prototype
					 js-array-pcache)
   
      ;; local constant strings
      (unless (vector? __js_strings) (set! __js_strings (&init!)))
      
      ;; builtin prototype
      (set! js-vector-prototype (js-vector-alloc #u32:0 %this))
      (js-object-mode-enumerable-set! js-vector-prototype #f)
      (js-object-mode-hasnumeralprop-set! js-vector-prototype #f)
      (js-object-proto-set! js-vector-prototype js-array-prototype)

      (js-bind! %this js-vector-prototype (& "ref")
	 :value (js-make-function %this
		   (lambda (this index)
		      (if (js-vector? this)
			  (js-vector-ref this index %this)
			  (js-raise-type-error %this
			     "Not a vector ~s" (typeof this))))
		   (js-function-arity 1 0)
		   (js-function-info :name "ref" :len 1)))
      (js-bind! %this js-vector-prototype (& "set")
	 :value (js-make-function %this
		   (lambda (this index val)
		      (if (js-vector? this)
			  (js-vector-set! this index val %this)
			  (js-raise-type-error %this
			     "Not a vector ~s" (typeof this))))
		   (js-function-arity 1 0)
		   (js-function-info :name "ref" :len 1)))
		   
      ;; no possible modification to vector prototype
      (js-seal js-vector-prototype %this)
      (js-freeze js-vector-prototype %this)
   
      ;; create the vector object constructor
      (let ((proc (%js-vector %this)))
	 (set! js-vector
	    (js-make-function %this proc
	       (js-function-arity proc)
	       (js-function-info :name  "Vector" :len 1)
	       :__proto__ (js-object-proto js-function)
	       :prototype js-vector-prototype
	       :alloc js-vector-alloc-ctor)))

      ;; other properties of the Vector constructor
      (js-bind! %this js-vector (& "isVector")
	 :value (js-make-function %this
		   (lambda (this arg)
		      (js-vector? arg))
		   (js-function-arity 1 0)
		   (js-function-info :name "isVector" :len 1))
	 :writable #t
	 :enumerable #f
	 :hidden-class #t)

      ;; bind Vector in the global object
      (js-bind! %this %this (& "Vector")
	 :configurable #f :enumerable #f :value js-vector
	 :hidden-class #t)
      
      js-vector))

;*---------------------------------------------------------------------*/
;*    js-vector-length ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (js-vector-length this::JsArray)
   (with-access::JsArray this (ilen)
      ilen))

;*---------------------------------------------------------------------*/
;*    js-vector-get ...                                                */
;*---------------------------------------------------------------------*/
(define (js-vector-get this::JsArray prop %this)
   (with-access::JsArray this (ilen)
      (let ((i::uint32 (js-toindex prop)))
	 (cond
	    ((<u32 i ilen)
	     (js-vector-inline-ref this (uint32->fixnum i)))
	    ((eq? prop (& "length"))
	     (js-uint32-tointeger ilen))
	    ((not (js-isindex? i))
	     (js-get-jsobject this this (js-toname prop %this) %this))
	    (else
	     (js-vector-index-ref this i %this))))))
		  
;*---------------------------------------------------------------------*/
;*    js-vector-put! ...                                               */
;*---------------------------------------------------------------------*/
(define (js-vector-put! this::JsArray prop val %this)
   (with-access::JsArray this (ilen)
      (let ((i::uint32 (js-toindex prop)))
	 (cond
	    ((<u32 i ilen)
	     (js-vector-inline-set! this (uint32->fixnum i) val))
	    ((not (js-isindex? i))
	     (js-array-put! this prop val #t %this))
	    (else
	     (js-vector-index-set! this i val %this))))))
		  
;*---------------------------------------------------------------------*/
;*    js-vector-ref ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-vector-ref this::JsArray index %this)
   (let ((idx::uint32 (js-touint32 index %this)))
      (with-access::JsArray this (ilen)
	 (if (<u32 idx ilen)
	     (js-vector-inline-ref this (uint32->fixnum idx))
	     (js-raise-range-error %this
		"vector reference, out of range" index)))))

;*---------------------------------------------------------------------*/
;*    js-vector-set! ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-vector-set! this::JsArray index val %this)
   (let ((idx::uint32 (js-touint32 index %this)))
      (with-access::JsArray this (ilen)
	 (if (<u32 idx ilen)
	     (js-vector-inline-set! this (uint32->fixnum idx) val)
	     (js-raise-range-error %this
		"vector assignment, out of range" index)))))

;*---------------------------------------------------------------------*/
;*    js-vector-index-ref ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-vector-index-ref this::JsArray idx %this)
   (with-access::JsArray this (ilen)
      (if (<u32 idx ilen)
	  (js-vector-inline-ref this (uint32->fixnum idx))
	  (js-raise-range-error %this
	     "vector reference, out of range" idx))))

;*---------------------------------------------------------------------*/
;*    js-vector-index-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (js-vector-index-set! this::JsArray idx val %this)
   (with-access::JsArray this (ilen)
      (if (<u32 idx ilen)
	  (js-vector-inline-set! this (uint32->fixnum idx) val)
	  (js-raise-range-error %this
	     "vector assignment, out of range" idx))))

;*---------------------------------------------------------------------*/
;*    js-vector-alloc-ctor ...                                         */
;*    -------------------------------------------------------------    */
;*    This is a dummy implementation of vector constructor.            */
;*    Vector are always allocated in the JS constructor because        */
;*    they need there fixed size for being allocated.                  */
;*---------------------------------------------------------------------*/
(define (js-vector-alloc-ctor %this constructor::JsFunction)
   (with-access::JsGlobalObject %this (js-new-target js-vector)
      (set! js-new-target js-vector))
   (js-undefined))

;*---------------------------------------------------------------------*/
;*    js-vector-alloc ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (js-vector-alloc len::uint32 %this::JsGlobalObject)
   (cond-expand
      ((and bigloo-c (not devel) (not debug))
       (with-access::JsGlobalObject %this (js-vector-prototype js-array-cmap)
	  (let ((a ($js-make-jsarray (uint32->fixnum len) len 
		      js-array-cmap
		      js-vector-prototype
		      (js-undefined) (js-vector-default-mode))))
	     (with-access::JsArray a (ilen)
		(set! ilen len))
	     a)))
      (else
       (with-access::JsGlobalObject %this (js-vector-prototype)
	  (let* ((this (js-array-alloc %this))
		 (v (make-vector (uint32->fixnum len) (js-undefined))))
	     (js-object-mode-set! this (js-vector-default-mode))
	     (with-access::JsArray this (vec ilen length)
		(js-object-proto-set! this js-vector-prototype)
		(set! length len)
		(set! ilen len)
		(set! vec v))
	     this)))))
  
;*---------------------------------------------------------------------*/
;*    %js-vector ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.1       */
;*---------------------------------------------------------------------*/
(define (%js-vector %this::JsGlobalObject)
   (lambda (this len)
      (with-access::JsGlobalObject %this (js-new-target js-vector)
	 (if (eq? js-new-target (js-undefined))
	     (js-raise-type-error %this
		"Vector can only be used as a constructor" js-vector)
	     (begin
		(set! js-new-target (js-undefined))
		(js-vector-new1 len %this))))))

;*---------------------------------------------------------------------*/
;*    js-vector-new1 ...                                               */
;*---------------------------------------------------------------------*/
(define (js-vector-new1 len %this)
   (cond
      ((not (fixnum? len))
       (js-raise-type-error %this
	  "Wrong vector length, not an integer" len))
      ((<u32 (fixnum->uint32 len) #u32:0)
       (js-raise-range-error %this
	  "Wrong vector length, out of range" len))
      (else
       (js-vector-alloc (fixnum->uint32 len) %this))))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
