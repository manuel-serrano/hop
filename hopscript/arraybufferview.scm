;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/arraybufferview.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun 18 07:29:16 2014                          */
;*    Last change :  Fri Feb 16 08:46:41 2018 (serrano)                */
;*    Copyright   :  2014-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript ArrayBufferView              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_arraybufferview

   (library hop)

   (include "types.sch" "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_number
	   __hopscript_worker
	   __hopscript_arraybuffer)

   (export (js-init-arraybufferview! ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsArrayBuffer ...                            */
;*---------------------------------------------------------------------*/
(define (arraybufferview-serializer o::JsArrayBufferView)
   (with-access::JsArrayBufferView o (%data) %data))

(define-macro (arraybufferview-unserializer type bpe)
   `(lambda (o %this)
       (let ((this (or %this (js-initial-global-object))))
	  (with-access::JsGlobalObject this (js-arraybuffer js-int8array)
	     (let ((abuf (instantiateJsArrayBuffer
			    (__proto__ (js-get js-arraybuffer 'prototype this))
			    (data o))))
		(,(symbol-append 'instantiate:: type)
		 (__proto__ (js-get js-int8array 'prototype this))
		 (%data o)
		 (bpe 1)
		 (length (u8vector-length o))
		 (byteoffset 0)
		 (buffer abuf)))))))

(register-class-serialization! JsInt8Array
   arraybufferview-serializer
   (arraybufferview-unserializer JsInt8Array 1))
(register-class-serialization! JsUint8Array
   arraybufferview-serializer
   (arraybufferview-unserializer JsUint8Array 1))

(register-class-serialization! JsInt16Array
   arraybufferview-serializer
   (arraybufferview-unserializer JsInt16Array 2))
(register-class-serialization! JsUint16Array
   arraybufferview-serializer
   (arraybufferview-unserializer JsUint16Array 2))

(register-class-serialization! JsInt32Array
   arraybufferview-serializer
   (arraybufferview-unserializer JsInt32Array 4))
(register-class-serialization! JsUint32Array
   arraybufferview-serializer
   (arraybufferview-unserializer JsUint32Array 4))

(register-class-serialization! JsFloat32Array
   arraybufferview-serializer
   (arraybufferview-unserializer JsFloat32Array 4))

(register-class-serialization! JsFloat64Array
   arraybufferview-serializer
   (arraybufferview-unserializer JsFloat64Array 8))

(register-class-serialization! JsDataView
   arraybufferview-serializer
   (lambda (o %this)
      (let ((this (or %this (js-initial-global-object))))
	 (with-access::JsGlobalObject this (js-arraybuffer js-int8array)
	    (let ((abuf (instantiateJsArrayBuffer
			   (__proto__ (js-get js-arraybuffer 'prototype this))
			   (data o))))
	       (instantiateJsDataView
		  (__proto__ (js-get js-int8array 'prototype this))
		  (%data o)
		  (byteoffset 0)
		  (buffer abuf)))))))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsDataView ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsDataView worker %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-arraybuffer)
	 (with-access::JsDataView obj (%data buffer frozen byteoffset)
	    (let ((nbuffer (js-donate buffer worker %_this)))
	       (instantiateJsDataView
		  (__proto__ (js-get js-arraybuffer 'prototype %this))
		  (frozen frozen)
		  (buffer nbuffer)
		  (%data (with-access::JsArrayBuffer nbuffer (data) data))
		  (byteoffset byteoffset)))))))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsTypedArray ...                                     */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsTypedArray worker %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-arraybuffer)
	 (with-access::JsTypedArray obj (%data buffer frozen byteoffset bpe length)
	    (let ((nbuffer (js-donate buffer worker %_this))
		  (obj (class-constructor (object-class obj))))
	       (with-access::JsTypedArray obj (__proto__ frozen buffer
						 %data byteoffset bpe length)
		  (set! __proto__ (js-get js-arraybuffer 'prototype %this))
 		  (set! frozen frozen)
		  (set! buffer nbuffer)
		  (set! %data (with-access::JsArrayBuffer nbuffer (data) data))
		  (set! byteoffset byteoffset)
		  (set! bpe bpe)
		  (set! length length)
		  obj))))))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsDataView ...                                 */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsDataView op compile isexpr)
   (with-access::JsDataView o (frozen byteoffset buffer)
      (display "hop_buffer( \"JsJsDataView\", " op)
      (display (if frozen "true" "false") op)
      (display ", " op)
      (display byteoffset op)
      (display ", " op)
      (hop->javascript buffer op compile isexpr)
      (display ")" op)))

;*---------------------------------------------------------------------*/
;*    javascript-buffer->arraybufferview ...                           */
;*    -------------------------------------------------------------    */
;*    See __hopscript_arraybuffer                                      */
;*---------------------------------------------------------------------*/
(define (javascript-buffer->arraybufferview name args %this)
   (with-access::JsArrayBuffer (caddr args) (data)
      (let ((buf (instantiateJsDataView
		    (frozen (car args))
		    (byteoffset (fixnum->uint32 (cadr args)))
		    (buffer (caddr args))
		    (%data data))))
	 (js-put! buf 'length (u8vector-length data) #f %this)
	 buf)))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsTypedArray ...                               */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsTypedArray op compile isexpr)
   (with-access::JsTypedArray o (frozen byteoffset length bpe buffer)
      (display "hop_buffer( \"" op)
      (display (class-name (object-class o)) op)
      (display "\", " op)
      (display (if frozen "true" "false") op)
      (display ", " op)
      (display byteoffset op)
      (display ", " op)
      (display length op)
      (display ", " op)
      (display bpe op)
      (display ", " op)
      (hop->javascript buffer op compile isexpr)
      (display ")" op)))

;*---------------------------------------------------------------------*/
;*    js-typedarray-ref ::JsInt8Array ...                              */
;*---------------------------------------------------------------------*/
(define (js-i8array-ref buf::u8vector i::int)
   (int8->fixnum (uint8->int8 (u8vector-ref buf i))))

(define (js-i8array-set! buf::u8vector i::int v::obj %this::JsGlobalObject)
   (let ((val (js-tointeger v %this)))
      ($u8vector-set! buf i
	 (fixnum->int8 (if (flonum? val) (flonum->fixnum val) val)))))

(define-method (js-typedarray-ref o::JsInt8Array) js-i8array-ref)
(define-method (js-typedarray-set! o::JsInt8Array) js-i8array-set!)

;*---------------------------------------------------------------------*/
;*    js-typedarray-set! ::JsUint8Array ...                            */
;*---------------------------------------------------------------------*/
(define (js-u8array-ref  buf::u8vector i::int)
   (uint8->fixnum (u8vector-ref buf i)))

(define (js-u8array-set! buf::u8vector i::int v::obj %this::JsGlobalObject)
   (let ((val (js-tointeger v %this)))
      ($u8vector-set! buf i
	 (fixnum->uint8 (if (flonum? val) (flonum->fixnum val) val)))))

(define-method (js-typedarray-ref o::JsUint8Array) js-u8array-ref)
(define-method (js-typedarray-set! o::JsUint8Array) js-u8array-set!)

;*---------------------------------------------------------------------*/
;*    js-typedarray-set! ::JsUint8ClampedArray ...                     */
;*---------------------------------------------------------------------*/
(define (js-u8clampledarray-ref  buf::u8vector i::int)
   (uint8->fixnum (u8vector-ref buf i)))

(define (js-u8clampledarray-set! buf::u8vector i::int v::obj %this::JsGlobalObject)
   (let ((val (js-tointeger v %this)))
      (let ((n (if (flonum? val) (flonum->fixnum val) val)))
	 (cond
	    ((<fx n 0) ($u8vector-set! buf i 0))
	    ((>fx n 255) ($u8vector-set! buf i 255))
	    (else ($u8vector-set! buf i (fixnum->uint8 n)))))))

(define-method (js-typedarray-ref o::JsUint8ClampedArray)
   js-u8clampledarray-ref)

(define-method (js-typedarray-set! o::JsUint8ClampedArray)
   js-u8clampledarray-set!)

;*---------------------------------------------------------------------*/
;*    js-typedarray-set! ::JsInt16Array ...                            */
;*---------------------------------------------------------------------*/
(define (js-i16array-ref buf::u8vector i::int)
   (int16->fixnum ($s16/u8vector-ref buf (*fx 2 i))))

(define (js-i16array-set! buf::u8vector i::int v::obj %this::JsGlobalObject)
   (let ((val (js-tointeger v %this)))
      ($s16/u8vector-set! buf (*fx 2 i)
	 (fixnum->int16 (if (flonum? val) (flonum->fixnum val) val)))))

(define-method (js-typedarray-ref o::JsInt16Array) js-i16array-ref)
(define-method (js-typedarray-set! o::JsInt16Array) js-i16array-set!)

;*---------------------------------------------------------------------*/
;*    js-typedarray-set! ::JsUint16Array ...                           */
;*---------------------------------------------------------------------*/
(define (js-u16array-ref buf::u8vector i::int)
   (uint16->fixnum ($u16/u8vector-ref buf (*fx 2 i))))

(define (js-u16array-set! buf::u8vector i::int v::obj %this::JsGlobalObject)
   (let ((val (js-tointeger v %this)))
      ($u16/u8vector-set! buf (*fx 2 i)
	 (fixnum->uint16 (if (flonum? val) (flonum->fixnum val) val)))))

(define-method (js-typedarray-ref o::JsUint16Array) js-u16array-ref)
(define-method (js-typedarray-set! o::JsUint16Array) js-u16array-set!)

;*---------------------------------------------------------------------*/
;*    js-typedarray-set! ::JsInt32Array ...                            */
;*---------------------------------------------------------------------*/
(define (js-i32array-ref buf::u8vector i::int)
   (cond-expand
      (bint61
       (int32->fixnum ($s32/u8vector-ref buf (*fx 4 i))))
      (else
       (let ((v::int32 ($s32/u8vector-ref buf (*fx 4 i))))
	  (if (or (>s32 v (bit-lshs32 #s32:1 28))
		  (<s32 v (negs32 (bit-lshs32 #s32:1 28))))
	      (fixnum->flonum (int32->fixnum v))
	      (int32->fixnum v))))))

(define (js-i32array-set! buf::u8vector i::int v::obj %this::JsGlobalObject)
   (let ((val (js-tointeger v %this)))
      ($s32/u8vector-set! buf (*fx 4 i)
	 (fixnum->int32 (if (flonum? val) (flonum->fixnum val) val)))))

(define-method (js-typedarray-ref o::JsInt32Array) js-i32array-ref)
(define-method (js-typedarray-set! o::JsInt32Array) js-i32array-set!)

;*---------------------------------------------------------------------*/
;*    js-typedarray-set! ::JsUint32Array ...                           */
;*---------------------------------------------------------------------*/
(define (js-u32array-ref buf::u8vector i::int)
   (cond-expand
      (bint61
       (uint32->fixnum ($s32/u8vector-ref buf (*fx 4 i))))
      (else
       (let ((v::uint32 ($s32/u8vector-ref buf (*fx 4 i))))
	  (if (>u32 v (bit-lshu32 #u32:1 29))
	      (uint32->flonum ($s32/u8vector-ref buf (*fx 4 i)))
	      (uint32->fixnum ($s32/u8vector-ref buf (*fx 4 i))))))))

(define (js-u32array-set! buf::u8vector i::int v::obj %this::JsGlobalObject)
   (let ((val (js-tointeger v %this)))
      ($s32/u8vector-set! buf (*fx 4 i)
	 (if (flonum? val) (flonum->uint32 val) (fixnum->uint32 val)))))

(define-method (js-typedarray-ref o::JsUint32Array) js-u32array-ref)
(define-method (js-typedarray-set! o::JsUint32Array) js-u32array-set!)

;*---------------------------------------------------------------------*/
;*    js-typedarray-set! ::JsFloat32Array ...                          */
;*---------------------------------------------------------------------*/
(define (js-f32array-ref buf::u8vector i::int)
   ($f32/u8vector-ref buf (*fx 4 i)))

(define (js-f32array-set! buf::u8vector i::int v::obj %this::JsGlobalObject)
   (let ((val (js-tonumber v %this)))
      ($f32/u8vector-set! buf (*fx 4 i)
	 (if (fixnum? val) (fixnum->flonum val) val))))

(define-method (js-typedarray-ref o::JsFloat32Array) js-f32array-ref)
(define-method (js-typedarray-set! o::JsFloat32Array) js-f32array-set!)

;*---------------------------------------------------------------------*/
;*    js-typedarray-set! ::JsFloat64Array ...                          */
;*---------------------------------------------------------------------*/
(define (js-f64array-ref buf::u8vector i::int)
   ($f64/u8vector-ref buf (*fx 8 i)))

(define (js-f64array-set! buf::u8vector i::int v::obj %this::JsGlobalObject)
   (let ((val (js-tonumber v %this)))
      ($f64/u8vector-set! buf (*fx 8 i)
	 (if (fixnum? val) (fixnum->flonum val) val))))

(define-method (js-typedarray-ref o::JsFloat64Array) js-f64array-ref)
(define-method (js-typedarray-set! o::JsFloat64Array) js-f64array-set!)

;*---------------------------------------------------------------------*/
;*    js-init-arraybufferview! ...                                     */
;*---------------------------------------------------------------------*/
(define (js-init-arraybufferview! %this)
   (with-access::JsGlobalObject %this (js-int8array)
      (set! js-int8array (js-init-typedarray! %this 'Int8Array 1)))
   (with-access::JsGlobalObject %this (js-uint8array)
      (set! js-uint8array (js-init-typedarray! %this 'Uint8Array 1)))
   (with-access::JsGlobalObject %this (js-uint8clampedarray)
      (set! js-uint8clampedarray (js-init-typedarray! %this 'Uint8ClampedArray 1)))
   (with-access::JsGlobalObject %this (js-int16array)
      (set! js-int16array (js-init-typedarray! %this 'Int16Array 2)))
   (with-access::JsGlobalObject %this (js-uint16array)
      (set! js-uint16array (js-init-typedarray! %this 'Uint16Array 2)))
   (with-access::JsGlobalObject %this (js-int32array)
      (set! js-int32array (js-init-typedarray! %this 'Int32Array 4)))
   (with-access::JsGlobalObject %this (js-uint32array)
      (set! js-uint32array (js-init-typedarray! %this 'Uint32Array 4)))
   (with-access::JsGlobalObject %this (js-float32array)
      (set! js-float32array (js-init-typedarray! %this 'Float32Array 4)))
   (with-access::JsGlobalObject %this (js-float64array)
      (set! js-float64array (js-init-typedarray! %this 'Float64Array 8)))
   (with-access::JsGlobalObject %this (js-dataview)
      (set! js-dataview (js-init-dataview! %this))))

;*---------------------------------------------------------------------*/
;*    js-init-typedarray! ...                                          */
;*---------------------------------------------------------------------*/
(define (js-init-typedarray! %this name::symbol bp::int)
   (with-access::JsGlobalObject %this (__proto__ js-function js-object)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 ;; builtin ArrayBufferview prototype
	 (define js-typedarray-prototype
	    (instantiateJsObject
	       (__proto__ __proto__)))

	 (define (js-create-from-arraybuffer this::JsTypedArray
		    buf::JsArrayBuffer off::uint32 len::uint32)
	    (with-access::JsTypedArray this (buffer %data length byteoffset bpe)
	       (with-access::JsArrayBuffer buf (data)
		  (let ((vlen (u8vector-length data)))
		     (set! buffer buf)
		     (set! %data data)
		     (set! byteoffset off)
		     (set! length len)
		     ;; buffer
		     (js-bind! %this this 'buffer
			:value buffer
			:configurable #f
			:writable #f
			:enumerable #t
			:hidden-class #t)
		     
		     ;; BYTES_PER_ELEMENT
		     (js-bind! %this this 'BYTES_PER_ELEMENT
			:value (uint32->fixnum bpe)
			:configurable #f
			:writable #f
			:enumerable #t
			:hidden-class #t)

		     ;; length
		     (js-bind! %this this 'length
			:value (uint32->fixnum len)
			:configurable #f
			:writable #f
			:enumerable #t
			:hidden-class #t)
		     
		     ;; byteLength
		     (js-bind! %this this 'byteLength
			:value (uint32->fixnum (*u32 bpe length))
			:configurable #f
			:writable #f
			:enumerable #t
			:hidden-class #t)
		     
		     ;; byteOffset
		     (js-bind! %this this 'byteOffset
			:value (uint32->fixnum off)
			:configurable #f
			:writable #f
			:enumerable #t
			:hidden-class #t)

		     ;; set
		     (js-bind! %this this 'set
			:value (js-make-function %this js-set 2 "set")
			:configurable #t
			:writable #t
			:enumerable #t
			:hidden-class #t)

		     ;; get
		     (js-bind! %this this 'get
			:value (js-make-function %this
				  (lambda (this num)
				     (js-get this num %this))
				  1 "get")
			:configurable #t
			:writable #t
			:enumerable #t
			:hidden-class #t)
		     
		     ;; subarray
		     (js-bind! %this this 'subarray
			:value (js-make-function %this js-subarray 2 "subarray")
			:configurable #t
			:writable #t
			:enumerable #t
			:hidden-class #t)))
	       this))
	 
	 (define (js-typedarray-construct this::JsTypedArray items)
	    (cond
	       ((null? items)
		(js-create-from-arraybuffer this
		   (js-new %this (js-get %this 'ArrayBuffer %this))
		   #u32:0 #u32:0))
	       ((number? (car items))
		(cond
		   ((< (car items) 0)
		    (js-raise-range-error %this
		       "ArrayBufferView size is not a small enough positive integer"
		       (car items)))
		   ((and (flonum? (car items))
			 (>=fl (*fl (fixnum->flonum bp) (car items))
			    1073741823.0))
		    (js-raise-range-error %this
		       "ArrayBufferView size is too large"
		       (car items)))
		   ((and (>fx bp 1) (>=fx (car items) (/fx 1073741823 bp)))
		    (js-raise-range-error %this
		       "ArrayBufferView size is too large"
		       (car items)))
		   (else
		    (let ((len (js-touint32 (car items) %this)))
		       (js-create-from-arraybuffer this
			  (js-new %this (js-get %this 'ArrayBuffer %this)
			     (uint32->fixnum (*u32 (fixnum->uint32 bp) len)))
			  #u32:0 len)))))
	       ((isa? (car items) JsArrayBuffer)
		(with-access::JsArrayBuffer (car items) (data)
		   (let ((len (u8vector-length data)))
		      (cond
			 ((or (null? (cdr items)) (not (integer? (cadr items))))
			  (if (=fx (remainder len bp) 0)
			      (js-create-from-arraybuffer this
				 (car items)
				 #u32:0 (fixnum->uint32 len))
			      (js-raise-range-error %this
				 "Byte offset / length is not aligned ~a"
				 (car items))))
			 ((or (null? (cddr items))
			      (not (integer? (caddr items))))
			  (let ((off (->fixnum
					(js-tointeger (cadr items) %this))))
			     (cond
				((not
				    (and (=fx (remainder off bp) 0)
					 (=fx (remainder len bp) 0)))
				 (js-raise-range-error %this
				    "Byte offset / lenght is not aligned ~a"
				    (cadr items)))
				((<fx off 0)
				 (js-raise-range-error %this
				    "Byte offset out of range ~a"
				    (cadr items)))
				(else
				 (js-create-from-arraybuffer this
				    (car items)
				    (fixnum->uint32 off)
				    (fixnum->uint32
				       (-fx (/fx len bp) off)))))))
			 (else
			  (let ((off (->fixnum
					(js-tointeger (cadr items) %this)))
				(l (->fixnum
				      (js-tointeger (caddr items) %this))))
			     (cond
				((not (=fx (remainder off bp) 0))
				 (js-raise-range-error %this
				    "Byte offset / lenght is not aligned ~a"
				    (cadr items)))
				((or (>fx (*fx (-fx l off) bp) len) (<fx l 0))
				 (js-raise-range-error %this
				    "Length is out of range ~a"
				    l))
				((<fx off 0)
				 (js-raise-range-error %this
				    "Byte offset out of range ~a"
				    (cadr items)))
				(else
				 (js-create-from-arraybuffer this
				    (car items)
				    (fixnum->uint32 off)
				    (fixnum->uint32 l))))))))))
	       ((or (isa? (car items) JsArray) (isa? (car items) JsTypedArray))
		(let ((len (js-get (car items) 'length %this)))
		   (let* ((arr (js-typedarray-construct this (list len)))
			  (vset (js-typedarray-set! arr)))
		      (with-access::JsTypedArray arr (buffer)
			 (with-access::JsArrayBuffer buffer (data)
			    (let loop ((i 0))
			       (if (<fx i len)
				   (let ((v (js-get (car items) i %this)))
				      (unless (eq? v (js-absent))
					 (vset data i v %this))
				      (loop (+fx i 1)))))))
		      arr)))
	       (else
		(js-typedarray-construct this '()))))

	 (define (%js-typedarray this . items)
	    (js-typedarray-construct 
	       (js-typedarray-alloc js-typedarray %this)
	       items))

	 (define (js-typedarray-alloc constructor::JsFunction %this)
	    (let ((o (allocate-instance (symbol-append 'Js name))))
	       (with-access::JsTypedArray o (cmap bpe __proto__ elements)
		  (js-object-properties-set! o '())
		  (js-object-mode-set! o (js-object-default-mode))
		  (js-object-mode-extensible-set! o #t)
		  (set! cmap (js-not-a-cmap))
		  (set! bpe (fixnum->uint32 bp))
		  (set! elements '#())
		  (set! __proto__ (js-get constructor 'prototype %this)))
	       o))
	 
	 (define js-typedarray
	    (js-make-function %this %js-typedarray 1 name
	       :__proto__ js-function-prototype
	       :prototype js-typedarray-prototype
	       :alloc (lambda (ctor) (js-typedarray-alloc ctor %this))
	       :construct (lambda (this . items)
			     (js-typedarray-construct this items))))

	 (define (js-set this::JsTypedArray array offset)
	    (let ((off (if (eq? offset (js-undefined))
			   #u32:0
			   (js-touint32 offset %this))))
	       (cond
		  ((isa? array JsTypedArray)
		   (with-access::JsTypedArray this ((toff byteoffset)
						    (tlength length)
						    (tbpe bp)
						    (tbuffer buffer))
		      (with-access::JsArrayBuffer tbuffer ((target data))
			 (with-access::JsTypedArray array ((sbuffer buffer)
							   (soff byteoffset)
							   (slength length))
			    (with-access::JsArrayBuffer sbuffer ((source data))
			       (let ((tstart (+u32 (*u32 (fixnum->uint32 bp) off) toff)))
				  (cond
				     ((>=u32 tstart tlength)
				      (js-raise-range-error %this
					 "Offset out of range ~a"
					 tstart))
				     ((>u32 slength (+u32 tstart tlength))
				      (format
					 "Offset/length out of range ~a/~a ~~a"
					 offset slength))
				     (else
				      (u8vector-copy! target
					 (uint32->fixnum tstart)
					 source
					 (uint32->fixnum soff)
					 (uint32->fixnum
					    (-u32 (+u32 soff slength) 1)))))))))))
		  ((isa? array JsArray)
		   (with-access::JsTypedArray this ((toff byteoffset)
						    (tlength length)
						    (tbpe bp)
						    (tbuffer buffer))
		      (with-access::JsArrayBuffer tbuffer ((target data))
			 (let ((tstart (+u32 (*u32 (fixnum->uint32 bp) off) toff))
			       (slength (js-get array 'length %this)))
			    (cond
			       ((>=u32 tstart tlength)
				(js-raise-range-error %this
				   "Offset out of range ~a"
				   tstart))
			       ((>fx slength
				   (uint32->fixnum (+u32 tstart tlength)))
				(format
				   "Offset/length out of range ~a/~a ~~a"
				   offset slength))
			       (else
				(let ((ioff (uint32->fixnum off))
				      (vset (js-typedarray-set! this)))
				   (let loop ((i 0))
				      (when (<fx i slength)
					 (let ((o (js-get array i %this)))
					    (unless (eq? o (js-absent))
					       (vset target (+fx i ioff) o %this))
					    (loop (+fx i 1))))))))))))
		  (else
		   (js-undefined)))))

	 (define (js-subarray this::JsTypedArray beg end)
	    (with-access::JsTypedArray this (byteoffset bpe length buffer)
	       (let ((beg (+u32 (/u32 byteoffset bpe) (js-touint32 beg %this))))
		  (cond
		     ((<u32 beg #u32:0) (set! beg #u32:0))
		     ((>u32 beg length) (set! beg (-u32 length bpe))))
		  (let ((len (if (eq? end (js-undefined))
				 length
				 (let ((l (js-touint32 end %this)))
				    (cond
				       ((>u32 l length) length)
				       ((<u32 l beg) 0)
				       (else (-u32 l beg)))))))
		     (%js-typedarray (js-undefined) buffer
			(uint32->fixnum beg) (uint32->fixnum len))))))

	 ;; bind the Typedarray in the global object
	 (js-bind! %this %this name
	    :configurable #f :enumerable #f :value js-typedarray
	    :hidden-class #t)
	 
	 js-typedarray)))
	 
;*---------------------------------------------------------------------*/
;*    js-properties-names ::JsTypedArray ...                           */
;*---------------------------------------------------------------------*/
(define-method (js-properties-names::vector obj::JsTypedArray enump %this)
   (with-access::JsTypedArray obj (length)
      (let ((len (uint32->fixnum length)))
	 (append! (map js-integer->name (iota len))
	    (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    js-has-property ::JsTypedArray ...                               */
;*---------------------------------------------------------------------*/
(define-method (js-has-property o::JsTypedArray p %this)
   (with-access::JsTypedArray o (length)
      (let ((index::uint32 (js-toindex p)))
	 (if (js-isindex? index)
	     (if (<=u32 length index)
		 (call-next-method)
		 #t)
	     (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property ::JsTypedArray ...                           */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property o::JsTypedArray p %this::JsGlobalObject)
   (with-access::JsTypedArray o (vref byteoffset bpe length buffer frozen)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((not (js-isindex? i))
	     (call-next-method))
	    ((<uint32 i length)
	     (let ((vref (js-typedarray-ref o)))
		(with-access::JsArrayBuffer buffer (data)
		   (instantiate::JsValueDescriptor
		      (name (js-toname p %this))
		      (value (vref data
				(uint32->fixnum (+u32 (/u32 byteoffset bpe) i))))
		      (enumerable #t)
		      (writable (not frozen))
		      (configurable (not frozen))))))
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-get-typedarray ...                                            */
;*---------------------------------------------------------------------*/
(define (js-get-typedarray o::JsTypedArray p %this)
   (with-access::JsTypedArray o (buffer vref data byteoffset length bpe)
      (let ((i::uint32 (js-toindex p)))
	 (when (and (js-isindex? i) (<uint32 i length))
	    (let ((vref (js-typedarray-ref o)))
	       (with-access::JsArrayBuffer buffer (data)
		  (vref data (uint32->fixnum (+u32 (/u32 byteoffset bpe) i)))))))))

;*---------------------------------------------------------------------*/
;*    *optimize-length* ...                                            */
;*    -------------------------------------------------------------    */
;*    It is difficult to optimize arraybufferview as they are used     */
;*    as the base class for JsFastBuffer that used non uint32          */
;*    length (only for detecting errors).                              */
;*---------------------------------------------------------------------*/
(define *optimize-length* #f)
   
;*---------------------------------------------------------------------*/
;*    js-get-property-value ::JsTypedArray ...                         */
;*    -------------------------------------------------------------    */
;*    This method is optional. It could be removed without changing    */
;*    the programs behaviors. It merely optimizes access to arrays.    */
;*---------------------------------------------------------------------*/
(define-method (js-get-property-value o::JsTypedArray base p %this)
   (if (symbol? p)
       (if (and *optimize-length* (eq? p 'length))
	   (with-access::JsTypedArray o (length)
	      (if (=u32 length #u32:0)
		  (call-next-method)
		  (uint32->fixnum length)))
	   (call-next-method))
       (or (js-get-typedarray o p %this) (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-get ::JsTypedArray ...                                        */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsTypedArray p %this)
   (if (symbol? p)
       (if (and *optimize-length* (eq? p 'length))
	   (with-access::JsTypedArray o (length)
	      (if (=u32 length #u32:0)
		  (call-next-method)
		  (uint32->fixnum length)))
	   (call-next-method))
       (or (js-get-typedarray o p %this) (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-get-length ::JsTypedArray ...                                 */
;*---------------------------------------------------------------------*/
(define-method (js-get-length o::JsTypedArray %this #!optional cache)
   (with-access::JsTypedArray o (length)
      (if (or (not *optimize-length*) (=u32 length #u32:0))
	  (call-next-method)
	  (uint32->fixnum length))))

;*---------------------------------------------------------------------*/
;*    js-get-lengthu32 ::JsTypedArray ...                              */
;*---------------------------------------------------------------------*/
(define-method (js-get-lengthu32 o::JsTypedArray %this #!optional cache)
   (with-access::JsTypedArray o (length)
      (if (or (not *optimize-length*) (=u32 length #u32:0))
	  (call-next-method)
	  length)))

;*---------------------------------------------------------------------*/
;*    js-object-get-name/cache-miss ::JsTypedArray ...                 */
;*---------------------------------------------------------------------*/
(define-method (js-object-get-name/cache-miss o::JsTypedArray p
		  throw::bool %this::JsGlobalObject
		  cache::JsPropertyCache
		  #!optional (point -1) (cspecs '()))
   (if (and *optimize-length* (eq? p 'length))
       (with-access::JsTypedArray o (length)
	  (if (=u32 length #u32:0)
	      (uint32->fixnum length)
	      (call-next-method)))
       (call-next-method)))

;*---------------------------------------------------------------------*/
;*    js-put! ::JsTypedArray ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::JsTypedArray p v throw %this)
   
   (define (js-put-array! o::JsTypedArray p::obj v)
      (cond
	 ((not (js-can-put o p %this))
	  ;; 1
	  (js-undefined))
	 ((and *optimize-length* (eq? p 'length))
	  ;; 1b, specific to TypedArray where length is not a true property
	  (with-access::JsTypedArray o (length buffer)
	     (when (eq? buffer (class-nil JsArrayBuffer))
		;; only correct length are optimized (for preserving
		;; bad argument errors)
		(if (and (> v 0) (< v (-fx (bit-lsh 1 31) 1)))
		    (set! length (fixnum->uint32 (->fixnum v)))
		    (begin
		       (set! length #u32:0)
		       (call-next-method))))
	     v))
	 (else
	  (let ((owndesc (js-get-own-property o p %this)))
	     ;; 2
	     (if (js-is-data-descriptor? owndesc)
		 ;; 3
		 (with-access::JsValueDescriptor owndesc ((valuedesc value))
		    (set! valuedesc v)
		    (js-define-own-property o p owndesc throw %this))
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
	     v))))
   
   (with-access::JsTypedArray o (buffer length byteoffset bpe conv)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((not (js-isindex? i))
	     (js-put-array! o (js-toname p %this) v))
	    ((<u32 i length)
	     (let ((vset (js-typedarray-set! o)))
		(with-access::JsArrayBuffer buffer (data)
		   (vset data (uint32->fixnum (+u32 (/u32 byteoffset bpe) i))
		      v %this)))
	     v)
	    (else
	     (js-put-array! o (js-toname p %this) v))))))

;*---------------------------------------------------------------------*/
;*    js-delete! ::JsTypedArray ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-delete! o::JsTypedArray p throw %this)
   (with-access::JsTypedArray o (length)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((not (js-isindex? i))
	     (call-next-method))
	    ((<uint32 i length)
	     #t)
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-init-dataview! ...                                            */
;*---------------------------------------------------------------------*/
(define (js-init-dataview! %this)
   (with-access::JsGlobalObject %this (__proto__ js-function js-object)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))

	 ;; host endianess
	 (define host-lendian
	    (eq? (bigloo-config 'endianess) 'little-endian))

	 (define buf (make-u8vector 8))
	 
	 ;; builtin DataView prototype
	 (define js-dataview-prototype
	    (instantiateJsObject
	       (__proto__ __proto__)))
	 
	 (define (js-create-from-arraybuffer this::JsDataView
		    buf::JsArrayBuffer off::uint32 len::uint32)
	    (with-access::JsDataView this (buffer %data byteoffset)
	       (with-access::JsArrayBuffer buf (data)
		  (let ((vlen (u8vector-length data)))
		     (set! buffer buf)
		     (set! %data data)
		     (set! byteoffset off)
		     ;; buffer
		     (js-bind! %this this 'buffer
			:value buffer
			:configurable #f
			:writable #f
			:enumerable #t
			:hidden-class #t)
		     
		     ;; byteLength
		     (js-bind! %this this 'byteLength
			:value vlen
			:configurable #f
			:writable #f
			:enumerable #t
			:hidden-class #t)
		     
		     ;; byteOffset
		     (js-bind! %this this 'byteOffset
			:value (uint32->fixnum off)
			:configurable #f
			:writable #f
			:enumerable #t
			:hidden-class #t)
		     
		     ;; Int8
		     (js-bind! %this this 'getInt8
			:value (js-make-function %this js-getInt8 2 "getInt8")
			:configurable #t
			:writable #t
			:enumerable #t
			:hidden-class #t)
		     (js-bind! %this this 'setInt8
			:value (js-make-function %this js-setInt8 3 "setInt8")
			:configurable #t
			:writable #t
			:enumerable #t
			:hidden-class #t)
		     
		     ;; Uint8
		     (js-bind! %this this 'getUint8
			:value (js-make-function %this js-getInt8 2 "getUint8")
			:configurable #t
			:writable #t
			:enumerable #t
			:hidden-class #t)
		     (js-bind! %this this 'setUint8
			:value (js-make-function %this js-setInt8 3 "setUint8")
			:configurable #t
			:writable #t
			:enumerable #t
			:hidden-class #t)
		     
		     ;; Int16
		     (js-bind! %this this 'getInt16
			:value (js-make-function %this js-getInt16 2 "getInt16")
			:configurable #t
			:writable #t
			:enumerable #t
			:hidden-class #t)
		     (js-bind! %this this 'setInt16
			:value (js-make-function %this js-setInt16 3 "setInt16")
			:configurable #t
			:writable #t
			:enumerable #t
			:hidden-class #t)
		     
		     ;; Uint16
		     (js-bind! %this this 'getUint16
			:value (js-make-function %this js-getUint16 2 "getUint16")
			:configurable #t
			:writable #t
			:enumerable #t
			:hidden-class #t)
		     (js-bind! %this this 'setUint16
			:value (js-make-function %this js-setInt16 3 "setUint16")
			:configurable #t
			:writable #t
			:enumerable #t
			:hidden-class #t)
		     
		     ;; Int32
		     (js-bind! %this this 'getInt32
			:value (js-make-function %this js-getInt32 2 "getInt32")
			:configurable #t
			:writable #t
			:enumerable #t
			:hidden-class #t)
		     (js-bind! %this this 'setInt32
			:value (js-make-function %this js-setInt32 3 "setInt32")
			:configurable #t
			:writable #t
			:enumerable #t
			:hidden-class #t)
		     
		     ;; Uint32
		     (js-bind! %this this 'getUint32
			:value (js-make-function %this js-getUint32 2 "getUint32")
			:configurable #t
			:writable #t
			:enumerable #t
			:hidden-class #t)
		     (js-bind! %this this 'setUint32
			:value (js-make-function %this js-setInt32 3 "setUint32")
			:configurable #t
			:writable #t
			:enumerable #t
			:hidden-class #t)
		     
		     ;; Float32
		     (js-bind! %this this 'getFloat32
			:value (js-make-function %this js-getFloat32 2 "getFloat32")
			:configurable #t
			:writable #t
			:enumerable #t
			:hidden-class #t)
		     (js-bind! %this this 'setFloat32
			:value (js-make-function %this js-setFloat32 3 "setFloat32")
			:configurable #t
			:writable #t
			:enumerable #t
			:hidden-class #t)
		     
		     ;; Float64
		     (js-bind! %this this 'getFloat64
			:value (js-make-function %this js-getFloat64 2 "getFloat64")
			:configurable #t
			:writable #t
			:enumerable #t
			:hidden-class #t)
		     (js-bind! %this this 'setFloat64
			:value (js-make-function %this js-setFloat64 3 "setFloat64")
			:configurable #t
			:writable #t
			:enumerable #t
			:hidden-class #t)
		     
		     ))
	       this))
	 
	 (define (js-dataview-construct this::JsDataView items)
	    (cond
	       ((null? items)
		(js-raise-error %this "Wrong number of argument" 0))
	       ((isa? (car items) JsArrayBuffer)
		(with-access::JsArrayBuffer (car items) (data)
		   (let ((len (u8vector-length data)))
		      (cond
			 ((or (null? (cdr items)) (not (integer? (cadr items))))
			  (js-create-from-arraybuffer this
			     (car items)
			     #u32:0 (fixnum->uint32 len)))
			 ((or (null? (cddr items))
			      (not (integer? (caddr items))))
			  (let ((off (->fixnum
					(js-tointeger (cadr items) %this))))
			     (cond
				((<fx off 0)
				 (js-raise-range-error %this
				    "Byte offset out of range ~a"
				    (cadr items)))
				(else
				 (js-create-from-arraybuffer this
				    (car items)
				    (fixnum->uint32 off)
				    (fixnum->uint32 (-fx len off)))))))
			 (else
			  (let ((off (->fixnum
					(js-tointeger (cadr items) %this)))
				(l (->fixnum
				      (js-tointeger (caddr items) %this))))
			     (cond
				((or (>fx (-fx l off) len) (<fx l 0))
				 (js-raise-range-error %this
				    "Length is out of range ~a"
				    l))
				((<fx off 0)
				 (js-raise-range-error %this
				    "Byte offset out of range ~a"
				    (cadr items)))
				(else
				 (js-create-from-arraybuffer this
				    (car items)
				    (fixnum->uint32 off)
				    (fixnum->uint32 l))))))))))
	       (else
		(js-raise-type-error %this
		   "Object must be an ArrayBuffer" (car items)))))
	 
	 (define (%js-dataview this . items)
	    (js-dataview-construct 
	       (js-dataview-alloc js-dataview %this)
	       items))
	 
	 (define (js-dataview-alloc constructor::JsFunction %this)
	    (instantiateJsDataView
	       (cmap (js-not-a-cmap))
	       (__proto__ (js-get constructor 'prototype %this))))
	 
	 (define js-dataview
	    (js-make-function %this %js-dataview 1 'DataView
	       :__proto__ js-function-prototype
	       :prototype js-dataview-prototype
	       :alloc (lambda (ctor) (js-dataview-alloc ctor %this))
	       :construct (lambda (this . items)
			     (js-dataview-construct this items))))

	 (define (js-dataview-get this::JsDataView offset lendian get)
	    (let ((off (uint32->fixnum (js-touint32 offset %this))))
	       (with-access::JsDataView this (buffer %data)
		  (let ((len (u8vector-length %data)))
		     (cond
			((<fx off 0)
			 (js-get this (js-toname offset %this) %this))
			((>=fx off len)
			 (js-undefined))
			(else
			 (get %data off lendian)))))))

	 (define (js-dataview-set this::JsDataView offset value lendian set)
	    (let ((off (uint32->fixnum (js-touint32 offset %this))))
	       (with-access::JsDataView this (buffer %data)
		  (let ((len (u8vector-length %data)))
		     (cond
			((<fx off 0)
			 (js-get this (js-toname offset %this) %this))
			((>=fx off len)
			 (js-undefined))
			(else
			 (set %data off (->fixnum (js-tointeger value %this))
			    lendian)))))))
	 
	 (define (js-getInt8 this::JsDataView offset)
	    (js-dataview-get this offset #t
	       (lambda (vec off _)
		  (int8->fixnum (uint8->int8 (u8vector-ref vec off))))))
	 
	 (define (js-setInt8 this::JsDataView offset value)
	    (js-dataview-set this offset value #t
	       (lambda (vec off value _)
		  (u8vector-set! vec off (int8->uint8 (fixnum->int8 value))))))
	 
	 (define (js-getUint8 this::JsDataView offset)
	    (js-dataview-get this offset #t
	       (lambda (vec off _)
		  (uint8->fixnum (u8vector-ref vec off)))))
	 
	 (define (js-setUint8 this::JsDataView offset value)
	    (js-dataview-set this offset value #t
	       (lambda (vec off value _)
		  (u8vector-set! vec off (fixnum->uint8 value)))))

	 (define (js-get16 this::JsDataView offset lendian add)
	    (js-dataview-get this offset (js-totest lendian)
	       (lambda (vec off lendian)
		  (let ((b0 (u8vector-ref vec off))
			(b1 (u8vector-ref vec (+fx off 1))))
		     (if lendian (add b1 b0) (add b0 b1))))))
	 
	 (define (js-getInt16 this::JsDataView offset lendian)
	    (define (add b0 b1)
	       (+fx (bit-lsh (int8->fixnum (uint8->int8 b0)) 8)
		  (uint8->fixnum b1)))
	    (js-get16 this offset lendian add))
	  
	 (define (js-getUint16 this::JsDataView offset lendian)
	    (define (add b0 b1)
	       (+fx (bit-lsh (uint8->fixnum b0) 8) (uint8->fixnum b1)))
	    (js-get16 this offset lendian add))
	 
	 (define (js-setInt16 this::JsDataView offset value lendian)
	    (js-dataview-set this offset value (js-totest lendian)
	       (lambda (vec off value lendian)
		  (if (eq? host-lendian lendian)
		      ($s16/u8vector-set! vec off (fixnum->int16 value))
		      (let ((b0 (bit-and (bit-rsh value 8) #xff))
			    (b1 (bit-and value #xff)))
			 (if lendian
			     (begin
				(u8vector-set! vec off b1)
				(u8vector-set! vec (+fx off 1) b0))
			     (begin
				(u8vector-set! vec off b0)
				(u8vector-set! vec (+fx off 1) b1))))))))
	 
	 (define (js-get32 this::JsDataView offset lendian add)
	    (js-dataview-get this offset (js-totest lendian)
	       (lambda (vec off lendian)
		  (let ((b0 (u8vector-ref vec off))
			(b1 (u8vector-ref vec (+fx off 1)))
			(b2 (u8vector-ref vec (+fx off 2)))
			(b3 (u8vector-ref vec (+fx off 3))))
		     (if lendian (add b3 b2 b1 b0) (add b0 b1 b2 b3))))))
	 
	 (define (js-getInt32 this::JsDataView offset lendian)
	    (define (add b0 b1 b2 b3)
	       (+fx (bit-lsh (int8->fixnum (uint8->int8 b0)) 24)
		  (+fx (bit-lsh (uint8->fixnum b1) 16)
		     (+fx (bit-lsh (uint8->fixnum b2) 8)
			(uint8->fixnum b3)))))
	    (js-get32 this offset lendian add))

	 (define (js-getUint32 this::JsDataView offset lendian)
	    (define (add b0 b1 b2 b3)
	       (+fx (bit-lsh (uint8->fixnum b0) 24)
		  (+fx (bit-lsh (uint8->fixnum b1) 16)
		     (+fx (bit-lsh (uint8->fixnum b2) 8)
			(uint8->fixnum b3)))))
	    (js-get32 this offset lendian add))
	 
	 (define (js-setInt32 this::JsDataView offset value lendian)
	    (js-dataview-set this offset value (js-totest lendian)
	       (lambda (vec off value lendian)
		  (if (eq? host-lendian lendian)
		      ($s32/u8vector-set! vec off (fixnum->int32 value))
		      (let ((b0 (bit-and (bit-rsh value 24) #xff))
			    (b1 (bit-and (bit-rsh value 16) #xff))
			    (b2 (bit-and (bit-rsh value 8) #xff))
			    (b3 (bit-and value #xff)))
			 (if lendian
			     (begin
				(u8vector-set! vec off b3)
				(u8vector-set! vec (+fx off 1) b2)
				(u8vector-set! vec (+fx off 2) b1)
				(u8vector-set! vec (+fx off 3) b0))
			     (begin
				(u8vector-set! vec off b0)
				(u8vector-set! vec (+fx off 1) b1)
				(u8vector-set! vec (+fx off 2) b2)
				(u8vector-set! vec (+fx off 3) b3))))))))

	 (define (js-getFloat32 this::JsDataView offset lendian)
	    (with-access::JsDataView this (buffer %data)
	       (let ((len (u8vector-length %data))
		     (lendian (js-totest lendian)))
		  (cond
		     ((<fx offset 0)
		      (js-get this (js-toname offset %this) %this))
		     ((>=fx offset len)
		      (js-undefined))
		     ((eq? host-lendian lendian)
		      ($f32/u8vector-ref %data (->fixnum offset)))
		     (else
		      (u8vector-set! buf 0 (u8vector-ref %data (+fx offset 3)))
		      (u8vector-set! buf 1 (u8vector-ref %data (+fx offset 2)))
		      (u8vector-set! buf 2 (u8vector-ref %data (+fx offset 1)))
		      (u8vector-set! buf 3 (u8vector-ref %data (+fx offset 0)))
		      ($f32/u8vector-ref buf 0))))))

	 (define (js-setFloat32 this::JsDataView offset value lendian)
	    (with-access::JsDataView this (buffer %data)
	       (let* ((len (u8vector-length %data))
		      (lendian (js-totest lendian))
		      (value (js-tonumber value %this))
		      (v (if (flonum? value) value (fixnum->flonum value))))
		  (cond
		     ((<fx offset 0)
		      (js-get this (js-toname offset %this) %this))
		     ((>=fx offset len)
		      (js-undefined))
		     ((eq? host-lendian lendian)
		      ($f32/u8vector-set! %data (->fixnum offset) v))
		     (else
		      ($f32/u8vector-set! buf 0 v)
		      (u8vector-set! %data offset (u8vector-ref buf 3))
		      (u8vector-set! %data (+fx offset 1) (u8vector-ref buf 2))
		      (u8vector-set! %data (+fx offset 2) (u8vector-ref buf 1))
		      (u8vector-set! %data (+fx offset 3) (u8vector-ref buf 0)))))))
		      
	 (define (js-getFloat64 this::JsDataView offset lendian)
	    (with-access::JsDataView this (buffer %data)
	       (let ((len (u8vector-length %data))
		     (lendian (js-totest lendian))
		     (offset (js-touint32 offset %this)))
		  (cond
		     ((not (js-isindex? offset))
		      (js-raise-range-error %this "getFloat64: Index out of range" offset))
		     ((>=u32 offset (fixnum->uint32 len))
		      (js-undefined))
		     ((eq? host-lendian lendian)
		      ($f64/u8vector-ref %data (uint32->fixnum offset)))
		     (else
		      (let ((offset (uint32->fixnum offset)))
			 (u8vector-set! buf 0 (u8vector-ref %data (+fx offset 7)))
			 (u8vector-set! buf 1 (u8vector-ref %data (+fx offset 6)))
			 (u8vector-set! buf 2 (u8vector-ref %data (+fx offset 5)))
			 (u8vector-set! buf 3 (u8vector-ref %data (+fx offset 4)))
			 (u8vector-set! buf 4 (u8vector-ref %data (+fx offset 3)))
			 (u8vector-set! buf 5 (u8vector-ref %data (+fx offset 2)))
			 (u8vector-set! buf 6 (u8vector-ref %data (+fx offset 1)))
			 (u8vector-set! buf 7 (u8vector-ref %data (+fx offset 0)))
			 ($f64/u8vector-ref buf 0)))))))

	 (define (js-setFloat64 this::JsDataView offset value lendian)
	    (with-access::JsDataView this (buffer %data)
	       (let ((len (u8vector-length %data))
		     (lendian (js-totest lendian))
		     (value (js-tonumber value %this))
		     (v (if (flonum? value) value (fixnum->flonum value)))
		     (offset (js-touint32 offset %this)))
		  (cond
		     ((not (js-isindex? offset))
		      (js-raise-range-error %this "setFloat64: Index out of range" offset))
		     ((>=u32 offset (fixnum->uint32 len))
		      (js-undefined))
		     ((eq? host-lendian lendian)
		      ($f64/u8vector-set! %data (uint32->fixnum offset) v))
		     (else
		      ($f64/u8vector-set! buf 0 v)
		      (let ((offset (uint32->fixnum offset)))
			 (u8vector-set! %data (+fx offset 0) (u8vector-ref buf 7))
			 (u8vector-set! %data (+fx offset 1) (u8vector-ref buf 6))
			 (u8vector-set! %data (+fx offset 2) (u8vector-ref buf 5))
			 (u8vector-set! %data (+fx offset 3) (u8vector-ref buf 4))
			 (u8vector-set! %data (+fx offset 4) (u8vector-ref buf 3))
			 (u8vector-set! %data (+fx offset 5) (u8vector-ref buf 2))
			 (u8vector-set! %data (+fx offset 6) (u8vector-ref buf 1))
			 (u8vector-set! %data (+fx offset 7) (u8vector-ref buf 0))))))))
	 
	 ;; bind the Dataview in the global object
	 (js-bind! %this %this 'DataView
	    :configurable #f :enumerable #f :value js-dataview
	    :hidden-class #t)
	 
	 js-dataview)))
	 
;*---------------------------------------------------------------------*/
;*    js-for-of ::JsTypedArray ...                                     */
;*---------------------------------------------------------------------*/
(define-method (js-for-of o::JsTypedArray proc close %this)
   (with-access::JsGlobalObject %this (js-symbol-iterator)
      (let ((fun (js-get o js-symbol-iterator %this)))
	 (if (isa? fun JsFunction)
	     (js-for-of-iterator (js-call0 %this fun o) o proc close %this)
	     (with-access::JsTypedArray o (length %data)
		(let ((vref (js-typedarray-ref o)))
		   (let loop ((i #u32:0))
		      (when (<u32 i length)
			 (proc (vref %data (uint32->fixnum i)))
			 (loop (+u32 i 1))))))))))
