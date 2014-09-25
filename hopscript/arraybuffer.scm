;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/arraybuffer.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 13 08:07:32 2014                          */
;*    Last change :  Thu Sep 25 08:52:41 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript ArrayBuffer                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_arraybuffer

   (library hop)

   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_number
	   __hopscript_worker)

   (export (js-init-arraybuffer! ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsArray ...                                  */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsArrayBuffer
   (lambda (o)
      (call-with-output-string
	 (lambda (op)
	    (obj->javascript-expr o op))))
   (lambda (s)
      (call-with-input-string s
	 javascript->obj)))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsArray ...                                    */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsArrayBuffer op compile isexpr)
   (tprint "TODO"))

;*---------------------------------------------------------------------*/
;*    js-init-arraybuffer! ...                                         */
;*---------------------------------------------------------------------*/
(define (js-init-arraybuffer! %this)
   (with-access::JsGlobalObject %this (__proto__ js-function js-object)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 ;; builtin ArrayBuffer prototype
	 (define js-arraybuffer-prototype
	    (instantiate::JsObject
	       (__proto__ __proto__)))

	 (define (%js-arraybuffer this . items)
	    (apply js-arraybuffer-construct 
	       (js-arraybuffer-alloc js-arraybuffer %this)
	       items))

	 (define js-arraybuffer
	    (js-make-function %this %js-arraybuffer 1 'ArrayBuffer
	       :__proto__ js-function-prototype
	       :prototype js-arraybuffer-prototype
	       :alloc (lambda (ctor) (js-arraybuffer-alloc ctor %this))
	       :construct (lambda (this . items)
			     (apply js-arraybuffer-construct this items))))

	 (define (js-arraybuffer-alloc constructor::JsFunction %this)
	    (instantiate::JsArrayBuffer
	       (cmap #f)
	       (__proto__ (js-get constructor 'prototype %this))))

	 (define (js-arraybuffer-construct this::JsArrayBuffer . items)
	    (with-access::JsArrayBuffer this (data)
	       (when (pair? items)
		  (let ((i (car items)))
		     (let* ((n (js-touint32 i %this))
			    (f (uint32->fixnum n)))
			(set! data (make-u8vector f))
			
			;; byteLength
			(js-bind! %this this 'byteLength
			   :value f
			   :configurable #f
			   :writable #f
			   :enumerable #t)
			
			;; slice
			(define (arraybuffer-slice this::JsArrayBuffer beg end)
			   (with-access::JsArrayBuffer this (data)
			      (let* ((l (u8vector-length data))
				     (b (if (eq? beg (js-undefined))
					    (js-raise-error %this
					       "Wrong number of arguments."
					       #f)
					    (->fixnum (js-tointeger beg %this))))
				     (e (if (eq? end (js-undefined))
					    l
					    (->fixnum (js-tointeger end %this)))))
				 (when (< b 0) (set! b (+ l b)))
				 (when (< e 0) (set! e (+ l e)))
				 (set! b (min b l))
				 (set! e (min e l))
				 (let* ((l (max (min (- e b) l) 0))
					(new (%js-arraybuffer %this l)))
				    (with-access::JsArrayBuffer new ((dst data))
				       (let loop ((i 0))
					  (when (<fx i l)
					     (u8vector-set! dst i
						(u8vector-ref data (+fx i b)))
					     (loop (+fx i 1))))
				       new)))))
			
			(js-bind! %this this 'slice
			   :value (js-make-function %this
				     arraybuffer-slice 2 'slice)
			   :configurable #f
			   :writable #t
			   :enumerable #t))))
	       
	       this))

	 ;; bind the ArrayBuffer in the global object
	 (js-bind! %this %this 'ArrayBuffer
	    :configurable #f :enumerable #f :value js-arraybuffer)

	 js-arraybuffer)))

;*---------------------------------------------------------------------*/
;*    js-property-names ::JsArray ...                                  */
;*---------------------------------------------------------------------*/
(define-method (js-property-names obj::JsArrayBuffer enump %this)
   (let ((len (js-arraybuffer-length obj)))
      (vector-append (apply vector (iota len)) (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-has-property ::JsArrayBuffer ...                              */
;*---------------------------------------------------------------------*/
(define-method (js-has-property o::JsArrayBuffer p)
   (let ((index::uint32 (js-toindex p)))
      (if (js-isindex? index)
	  (if (<=u32 (fixnum->uint32 (js-arraybuffer-length o)) index)
	      (call-next-method)
	      #t)
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-get-property ::JsArrayBuffer ...                              */
;*---------------------------------------------------------------------*/
(define-method (js-get-property o::JsArrayBuffer p %this)
   (let ((index::uint32 (js-toindex p)))
      (if (js-isindex? index)
	  (if (<=u32 (fixnum->uint32 (js-arraybuffer-length o)) index)
	      (call-next-method)
	      (instantiate::JsValueDescriptor
		 (name (js-toname index %this))
		 (value (js-arraybuffer-ref o (uint32->fixnum index)))
		 (writable #t)
		 (enumerable #t)
		 (configurable #t)))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-get-property-value ::JsArrayBuffer ...                        */
;*---------------------------------------------------------------------*/
(define-method (js-get-property-value o::JsArrayBuffer p %this)
   (js-get o p %this))

;*---------------------------------------------------------------------*/
;*    js-get ::JsArrayBuffer ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsArrayBuffer p %this)
   (let ((i::uint32 (js-toindex p)))
      (cond
	 ((not (js-isindex? i))
	  (call-next-method))
	 ((<uint32 i (js-arraybuffer-length o))
	  (js-arraybuffer-ref o (uint32->fixnum i)))
	 (else
	  (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    js-put! ::JsArrayBuffer ...                                      */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::JsArrayBuffer p v throw %this)
   
   (define (js-put-array! o::JsArrayBuffer p::obj v)
      (if (not (js-can-put o p %this))
	  ;; 1
	  (js-undefined)
	  (let ((owndesc (js-get-own-property o p %this)))
	     ;; 2
	     (if (js-is-data-descriptor? owndesc)
		 ;; 3
		 (with-access::JsValueDescriptor owndesc ((valuedesc value))
		    (set! valuedesc v)
		    (js-define-own-property o p owndesc throw %this))
		 (multiple-value-bind (desc owner)
		    (js-get-property o p %this)
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
   
   (let ((i::uint32 (js-toindex p)))
      (cond
	 ((not (js-isindex? i))
	  (js-put-array! o (js-toname p %this) v))
	 ((<u32 i (fixnum->uint32 (js-arraybuffer-length o)))
	  (js-arraybuffer-set! o (uint32->fixnum i)
	     (->fixnum (js-tointeger v %this)))
	  v)
	 (else
	  (js-put-array! o (js-toname p %this) v)))))

;*---------------------------------------------------------------------*/
;*    js-delete! ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (js-delete! o::JsArrayBuffer p throw %this)
   (let ((i::uint32 (js-toindex p)))
      (cond
	 ((not (js-isindex? i))
	  (call-next-method))
	 ((<uint32 i (js-arraybuffer-length o))
	  #t)
	 (else
	  (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property o::JsArrayBuffer p %this::JsGlobalObject)
   (with-access::JsArrayBuffer o (frozen)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((not (js-isindex? i))
	     (call-next-method))
	    ((<uint32 i (js-arraybuffer-length o))
	     (instantiate::JsValueDescriptor
		(name (js-toname p %this))
		(value (js-arraybuffer-ref o (uint32->fixnum i)))
		(enumerable #t)
		(writable (not frozen))
		(configurable (not frozen))))
	    (else
	     (call-next-method))))))
