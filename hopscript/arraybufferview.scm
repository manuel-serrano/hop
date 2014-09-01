;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/arraybufferview.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun 18 07:29:16 2014                          */
;*    Last change :  Sat Aug 30 19:49:20 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript ArrayBufferView              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_arraybufferview

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

   (export (js-init-arraybufferview! ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    js-init-arraybufferview! ...                                     */
;*---------------------------------------------------------------------*/
(define (js-init-arraybufferview! %this)
   ;; Int8Array
   (js-init-typedarray! %this 'Int8Array
      1
      (lambda (buf o)
	 (uint8->fixnum (uint8->int8 (u8vector-ref buf o))))
      (lambda (buf o v)
	 (let ((val (js-tointeger v %this)))
	    ($u8vector-set! buf o
	       (fixnum->int8 (if (flonum? val) (flonum->fixnum val) val))))))
   ;; Uint8Array
   (js-init-typedarray! %this 'Uint8Array
      1
      (lambda (buf o)
	 (uint8->fixnum (u8vector-ref buf o)))
      (lambda (buf o v)
	 (let ((val (js-tointeger v %this)))
	    ($u8vector-set! buf o
	       (fixnum->uint8 (if (flonum? val) (flonum->fixnum val) val))))))
   ;; Int16Array
   (js-init-typedarray! %this 'Int16Array
      2
      (lambda (buf o)
	 (int16->fixnum ($s16/u8vector-ref buf o)))
      (lambda (buf o v)
	 (let ((val (js-tointeger v %this)))
	    ($s16/u8vector-set! buf o
	       (fixnum->int16 (if (flonum? val) (flonum->fixnum val) val))))))
   ;; Uint16Array
   (js-init-typedarray! %this 'Uint16Array
      2
      (lambda (buf o)
	 (uint16->fixnum ($s16/u8vector-ref buf o)))
      (lambda (buf o v)
	 (let ((val (js-tointeger v %this)))
	    ($u16/u8vector-set! buf o
	       (fixnum->uint16 (if (flonum? val) (flonum->fixnum val) val))))))
   ;; Int32Array
   (js-init-typedarray! %this 'Int32Array
      4
      (lambda (buf o)
	 (cond-expand
	    (bint61
	     (int32->fixnum ($s32/u8vector-ref buf o)))
	    (else
	     (let ((v::int32 ($s32/u8vector-ref buf o)))
		(if (or (>s32 v (bit-lshs32 #s32:1 28))
			(<s32 v (negs32 (bit-lshs32 #s32:1 28))))
		    (fixnum->flonum (int32->fixnum v))
		    (int32->fixnum v))))))
      (lambda (buf o v)
	 (let ((val (js-tointeger v %this)))
	    ($s32/u8vector-set! buf o
	       (fixnum->int32 (if (flonum? val) (flonum->fixnum val) val))))))
   ;; Uint32Array
   (js-init-typedarray! %this 'Uint32Array
      4
      (lambda (buf o)
	 (cond-expand
	    (bint61
	     (uint32->fixnum ($s32/u8vector-ref buf o)))
	    (else
	     (let ((v::uint32 ($s32/u8vector-ref buf o)))
		(if (>u32 v (bit-lshu32 #u32:1 29))
		    (uint32->flonum ($s32/u8vector-ref buf o))
		    (uint32->fixnum ($s32/u8vector-ref buf o)))))))
      (lambda (buf o v)
	 (let ((val (js-tointeger v %this)))
	    ($s32/u8vector-set! buf o
	       (if (flonum? val) (flonum->uint32 val) (fixnum->uint32 val))))))
   ;; Float32Array
   (js-init-typedarray! %this 'Float32Array
      4
      (lambda (buf o)
	 ($f32/u8vector-ref buf o))
      (lambda (buf o v)
	 (let ((val (js-tonumber v %this)))
	    ($f32/u8vector-set! buf o
	       (if (fixnum? val) (fixnum->flonum val) val)))))
   ;; Float64Array
   (js-init-typedarray! %this 'Float64Array
      8
      (lambda (buf o)
	 ($f64/u8vector-ref buf o))
      (lambda (buf o v)
	 (let ((val (js-tonumber v %this)))
	    ($f64/u8vector-set! buf o
	       (if (fixnum? val) (fixnum->flonum val) val)))))
   ;; DataView
   (js-init-dataview! %this))

;*---------------------------------------------------------------------*/
;*    js-init-typedarray! ...                                          */
;*---------------------------------------------------------------------*/
(define (js-init-typedarray! %this name::symbol bpe::int vref::procedure vset::procedure)
   (with-access::JsGlobalObject %this (__proto__ js-function js-object)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 ;; builtin ArrayBufferview prototype
	 (define js-typedarray-prototype
	    (instantiate::JsObject
	       (__proto__ __proto__)))

	 (define (js-create-from-arraybuffer this::JsTypedArray
		    buf::JsArrayBuffer off::uint32 len::uint32)
	    (with-access::JsTypedArray this (buffer %vec length byteoffset bpe)
	       (with-access::JsArrayBuffer buf (vec)
		  (let ((vlen (vector-length vec)))
		     (set! buffer buf)
		     (set! %vec vec)
		     (set! byteoffset off)
		     (set! length len)
		     ;; buffer
		     (js-bind! %this this 'buffer
			:value buffer
			:configurable #f
			:writable #f
			:enumerable #t)
		     
		     ;; BYTES_PER_ELEMENT
		     (js-bind! %this this 'BYTES_PER_ELEMENT
			:value (uint32->fixnum bpe)
			:configurable #f
			:writable #f
			:enumerable #t)
		     
		     ;; length
		     (js-bind! %this this 'length
			:value (uint32->fixnum length)
			:configurable #f
			:writable #f
			:enumerable #t)
		     
		     ;; byteLength
		     (js-bind! %this this 'byteLength
			:value (uint32->fixnum (*u32 bpe length))
			:configurable #f
			:writable #f
			:enumerable #t)
		     
		     ;; byteOffset
		     (js-bind! %this this 'byteOffset
			:value (uint32->fixnum off)
			:configurable #f
			:writable #f
			:enumerable #t)

		     ;; set
		     (js-bind! %this this 'set
			:value (js-make-function %this js-set 2 "set")
			:configurable #t
			:writable #t
			:enumerable #t)

		     ;; get
		     (js-bind! %this this 'get
			:value (js-make-function %this
				  (lambda (this num)
				     (js-get this num %this))
				  1 "get")
			:configurable #t
			:writable #t
			:enumerable #t)
		     
		     ;; subarray
		     (js-bind! %this this 'subarray
			:value (js-make-function %this js-subarray 2 "subarray")
			:configurable #t
			:writable #t
			:enumerable #t)))
	       this))
	 
	 (define (js-typedarray-construct this::JsTypedArray items)
	    (cond
	       ((null? items)
		(js-create-from-arraybuffer this
		   (js-new %this (js-get %this 'ArrayBuffer %this))
		   #u32:0 #u32:0))
	       ((number? (car items))
		(if (< (car items) 0)
		    (js-raise-range-error %this
		       "ArrayBufferView size is not a small enough positive integer"
		       (car items))
		    (let ((len (js-touint32 (car items) %this)))
		       (js-create-from-arraybuffer this
			  (js-new %this (js-get %this 'ArrayBuffer %this)
			     (uint32->fixnum (*u32 bpe len)))
			  #u32:0 len))))
	       ((isa? (car items) JsArrayBuffer)
		(with-access::JsArrayBuffer (car items) (vec)
		   (let ((len (u8vector-length vec)))
		      (cond
			 ((or (null? (cdr items)) (not (integer? (cadr items))))
			  (if (=fx (remainder len bpe) 0)
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
				    (and (=fx (remainder off bpe) 0)
					 (=fx (remainder len bpe) 0)))
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
				       (-fx (/fx len bpe) off)))))))
			 (else
			  (let ((off (->fixnum
					(js-tointeger (cadr items) %this)))
				(l (->fixnum
				      (js-tointeger (caddr items) %this))))
			     (cond
				((not (=fx (remainder off bpe) 0))
				 (js-raise-range-error %this
				    "Byte offset / lenght is not aligned ~a"
				    (cadr items)))
				((or (>fx (*fx (-fx l off) bpe) len) (<fx l 0))
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
	       ((isa? (car items) JsArray)
		(let ((len (js-get (car items) 'length %this)))
		   (let ((arr (js-typedarray-construct this (list len))))
		      (with-access::JsTypedArray arr (buffer vset)
			 (with-access::JsArrayBuffer buffer (vec)
			    (let loop ((i 0))
			       (if (<fx i len)
				   (let ((v (js-get (car items) i %this)))
				      (unless (eq? v (js-absent))
					 (vset vec i v))
				      (loop (+fx i 1)))))))
		      arr)))
	       (else
		(js-typedarray-construct this '()))))

	 (define (%js-typedarray this . items)
	    (js-typedarray-construct 
	       (js-typedarray-alloc js-typedarray %this)
	       items))
	 
	 (define (js-typedarray-alloc constructor::JsFunction %this)
	    (instantiate::JsTypedArray
	       (cmap #f)
	       (bpe bpe)
	       (vref vref)
	       (vset vset)
	       (__proto__ (js-get constructor 'prototype %this))))
	 
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
						    (tbpe bpe)
						    (tbuffer buffer))
		      (with-access::JsArrayBuffer tbuffer ((target vec))
			 (with-access::JsTypedArray array ((sbuffer buffer)
							   (soff byteoffset)
							   (slength length))
			    (with-access::JsArrayBuffer sbuffer ((source vec))
			       (let ((tstart (+u32 (*u32 bpe off) toff)))
				  (cond
				     ((or (>=u32 tstart tlength)
					  (<u32 tstart 0))
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
						    (tbpe bpe)
						    (tbuffer buffer))
		      (with-access::JsArrayBuffer tbuffer ((target vec))
			 (let ((tstart (+u32 (*u32 bpe off) toff))
			       (slength (js-get array 'length %this)))
			    (cond
			       ((or (>=u32 tstart tlength) (<u32 tstart 0))
				(js-raise-range-error %this
				   "Offset out of range ~a"
				   tstart))
			       ((>fx slength
				   (uint32->fixnum (+u32 tstart tlength)))
				(format
				   "Offset/length out of range ~a/~a ~~a"
				   offset slength))
			       (else
				(let ((ioff (uint32->fixnum off)))
				   (let loop ((i 0))
				      (when (<fx i slength)
					 (let ((o (js-get array i %this)))
					    (unless (eq? o (js-absent))
					       (vset target (+fx i ioff) o))
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
	    :configurable #f :enumerable #f :value js-typedarray)
	 
	 js-typedarray)))
	 
;*---------------------------------------------------------------------*/
;*    js-property-names ::JsTypedArray ...                             */
;*---------------------------------------------------------------------*/
(define-method (js-property-names obj::JsTypedArray enump %this)
   (with-access::JsTypedArray obj (length)
      (let ((len (uint32->fixnum length)))
	 (vector-append (apply vector (iota len)) (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    js-get ::JsTypedArray ...                                        */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsTypedArray p %this)
   (with-access::JsTypedArray o (buffer length vref byteoffset)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((not (js-isindex? i))
	     (call-next-method))
	    ((<uint32 i length)
	     (with-access::JsArrayBuffer buffer (vec)
		(vref vec (uint32->fixnum (+u32 byteoffset i)))))
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-put! ::JsTypedArray ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::JsTypedArray p v throw %this)
   
   (define (js-put-array! o::JsTypedArray p::obj v)
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
   
   (with-access::JsTypedArray o (buffer length vset byteoffset conv)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((not (js-isindex? i))
	     (js-put-array! o (js-toname p %this) v))
	    ((<u32 i length)
	     (with-access::JsArrayBuffer buffer (vec)
		(vset vec (uint32->fixnum (+u32 byteoffset i)) v))
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
;*    js-get-own-property ::JsTypedArray ...                           */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property o::JsTypedArray p %this::JsGlobalObject)
   (with-access::JsTypedArray o (vref byteoffset length buffer frozen)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((not (js-isindex? i))
	     (call-next-method))
	    ((<uint32 i length)
	     (with-access::JsArrayBuffer buffer (vec)
		(instantiate::JsValueDescriptor
		   (name (js-toname p %this))
		   (value (vref vec (uint32->fixnum (+u32 byteoffset i))))
		   (enumerable #t)
		   (writable (not frozen))
		   (configurable (not frozen)))))
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
	    (instantiate::JsObject
	       (__proto__ __proto__)))
	 
	 (define (js-create-from-arraybuffer this::JsDataView
		    buf::JsArrayBuffer off::uint32 len::uint32)
	    (with-access::JsDataView this (buffer %vec byteoffset)
	       (with-access::JsArrayBuffer buf (vec)
		  (let ((vlen (u8vector-length vec)))
		     (set! buffer buf)
		     (set! %vec vec)
		     (set! byteoffset off)
		     ;; buffer
		     (js-bind! %this this 'buffer
			:value buffer
			:configurable #f
			:writable #f
			:enumerable #t)
		     
		     ;; byteLength
		     (js-bind! %this this 'byteLength
			:value vlen
			:configurable #f
			:writable #f
			:enumerable #t)
		     
		     ;; byteOffset
		     (js-bind! %this this 'byteOffset
			:value (uint32->fixnum off)
			:configurable #f
			:writable #f
			:enumerable #t)
		     
		     ;; Int8
		     (js-bind! %this this 'getInt8
			:value (js-make-function %this js-getInt8 1 "getInt8")
			:configurable #t
			:writable #t
			:enumerable #t)
		     (js-bind! %this this 'setInt8
			:value (js-make-function %this js-setInt8 2 "setInt8")
			:configurable #t
			:writable #t
			:enumerable #t)
		     
		     ;; Uint8
		     (js-bind! %this this 'getUint8
			:value (js-make-function %this js-getInt8 1 "getUint8")
			:configurable #t
			:writable #t
			:enumerable #t)
		     (js-bind! %this this 'setUint8
			:value (js-make-function %this js-setInt8 2 "setUint8")
			:configurable #t
			:writable #t
			:enumerable #t)
		     
		     ;; Int16
		     (js-bind! %this this 'getInt16
			:value (js-make-function %this js-getInt16 1 "getInt16")
			:configurable #t
			:writable #t
			:enumerable #t)
		     (js-bind! %this this 'setInt16
			:value (js-make-function %this js-setInt16 2 "setInt16")
			:configurable #t
			:writable #t
			:enumerable #t)
		     
		     ;; Uint16
		     (js-bind! %this this 'getUint16
			:value (js-make-function %this js-getUint16 1 "getUint16")
			:configurable #t
			:writable #t
			:enumerable #t)
		     (js-bind! %this this 'setUint16
			:value (js-make-function %this js-setInt16 2 "setUint16")
			:configurable #t
			:writable #t
			:enumerable #t)
		     
		     ;; Int32
		     (js-bind! %this this 'getInt32
			:value (js-make-function %this js-getInt32 1 "getInt32")
			:configurable #t
			:writable #t
			:enumerable #t)
		     (js-bind! %this this 'setInt32
			:value (js-make-function %this js-setInt32 2 "setInt32")
			:configurable #t
			:writable #t
			:enumerable #t)
		     
		     ;; Uint32
		     (js-bind! %this this 'getUint32
			:value (js-make-function %this js-getUint32 1 "getUint32")
			:configurable #t
			:writable #t
			:enumerable #t)
		     (js-bind! %this this 'setUint32
			:value (js-make-function %this js-setInt32 2 "setUint32")
			:configurable #t
			:writable #t
			:enumerable #t)
		     
		     ;; Float32
		     (js-bind! %this this 'getFloat32
			:value (js-make-function %this js-getFloat32 1 "getFloat32")
			:configurable #t
			:writable #t
			:enumerable #t)
		     (js-bind! %this this 'setFloat32
			:value (js-make-function %this js-setFloat32 2 "setFloat32")
			:configurable #t
			:writable #t
			:enumerable #t)
		     
		     ;; Float64
		     (js-bind! %this this 'getFloat64
			:value (js-make-function %this js-getFloat64 1 "getFloat64")
			:configurable #t
			:writable #t
			:enumerable #t)
		     (js-bind! %this this 'setFloat64
			:value (js-make-function %this js-setFloat64 2 "setFloat64")
			:configurable #t
			:writable #t
			:enumerable #t)
		     
		     ))
	       this))
	 
	 (define (js-dataview-construct this::JsDataView items)
	    (cond
	       ((null? items)
		(js-raise-error %this "Wrong number of argument" 0))
	       ((isa? (car items) JsArrayBuffer)
		(with-access::JsArrayBuffer (car items) (vec)
		   (let ((len (u8vector-length vec)))
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
	    (instantiate::JsDataView
	       (cmap #f)
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
	       (with-access::JsDataView this (buffer %vec)
		  (with-access::JsArrayBuffer buffer (vec)
		     (unless (eq? %vec vec)
			(error "js-dataview-get" "%vec != vec" this)))
		  (let ((len (u8vector-length %vec)))
		     (cond
			((<fx off 0)
			 (js-get this (js-toname offset %this) %this))
			((>=fx off len)
			 (js-undefined))
			(else
			 (get %vec off lendian)))))))

	 (define (js-dataview-set this::JsDataView offset value lendian set)
	    (let ((off (uint32->fixnum (js-touint32 offset %this))))
	       (with-access::JsDataView this (buffer %vec)
		  (with-access::JsArrayBuffer buffer (vec)
		     (unless (eq? %vec vec)
			(error "js-dataview-set" "%vec != vec" this)))
		  (let ((len (u8vector-length %vec)))
		     (cond
			((<fx off 0)
			 (js-get this (js-toname offset %this) %this))
			((>=fx off len)
			 (js-undefined))
			(else
			 (set %vec off (->fixnum (js-tointeger value %this))
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
		      ($s16/u8vector-set! vec (/fx off 2) value)
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
		      ($s32/u8vector-set! vec (/fx off 4) value)
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
	    (with-access::JsDataView this (buffer %vec)
	       (with-access::JsArrayBuffer buffer (vec)
		  (unless (eq? %vec vec)
		     (error "js-getFloat32" "%vec != vec" this)))
	       (let ((len (u8vector-length %vec))
		     (lendian (js-totest lendian)))
		  (cond
		     ((<fx offset 0)
		      (js-get this (js-toname offset %this) %this))
		     ((>=fx offset len)
		      (js-undefined))
		     ((eq? host-lendian lendian)
		      ($f32/u8vector-ref %vec (/fx offset 4)))
		     (else
		      (u8vector-set! buf 0 (u8vector-ref %vec (+fx offset 3)))
		      (u8vector-set! buf 1 (u8vector-ref %vec (+fx offset 2)))
		      (u8vector-set! buf 2 (u8vector-ref %vec (+fx offset 1)))
		      (u8vector-set! buf 3 (u8vector-ref %vec (+fx offset 0)))
		      ($f32/u8vector-ref buf 0))))))

	 (define (js-setFloat32 this::JsDataView offset value lendian)
	    (with-access::JsDataView this (buffer %vec)
	       (with-access::JsArrayBuffer buffer (vec)
		  (unless (eq? %vec vec)
		     (error "js-setFloat32" "%vec != vec" this)))
	       (let* ((len (u8vector-length %vec))
		      (lendian (js-totest lendian))
		      (value (js-tonumber value %this))
		      (v (if (flonum? value) value (fixnum->flonum value))))
		  (cond
		     ((<fx offset 0)
		      (js-get this (js-toname offset %this) %this))
		     ((>=fx offset len)
		      (js-undefined))
		     ((eq? host-lendian lendian)
		      ($f32/u8vector-set! %vec (/fx offset 4) v))
		     (else
		      ($f32/u8vector-set! buf 0 v)
		      (u8vector-set! %vec offset (u8vector-ref buf 3))
		      (u8vector-set! %vec (+fx offset 1) (u8vector-ref buf 2))
		      (u8vector-set! %vec (+fx offset 2) (u8vector-ref buf 1))
		      (u8vector-set! %vec (+fx offset 3) (u8vector-ref buf 0)))))))
		      
	 (define (js-getFloat64 this::JsDataView offset lendian)
	    (with-access::JsDataView this (buffer %vec)
	       (with-access::JsArrayBuffer buffer (vec)
		  (unless (eq? %vec vec)
		     (error "js-getFloat64" "%vec != vec" this)))
	       (let ((len (u8vector-length %vec))
		     (lendian (js-totest lendian)))
		  (cond
		     ((<fx offset 0)
		      (js-get this (js-toname offset %this) %this))
		     ((>=fx offset len)
		      (js-undefined))
		     ((eq? host-lendian lendian)
		      ($f64/u8vector-ref %vec (/fx offset 8)))
		     (else
		      (u8vector-set! buf 0 (u8vector-ref %vec (+fx offset 7)))
		      (u8vector-set! buf 1 (u8vector-ref %vec (+fx offset 6)))
		      (u8vector-set! buf 2 (u8vector-ref %vec (+fx offset 5)))
		      (u8vector-set! buf 3 (u8vector-ref %vec (+fx offset 4)))
		      (u8vector-set! buf 4 (u8vector-ref %vec (+fx offset 3)))
		      (u8vector-set! buf 5 (u8vector-ref %vec (+fx offset 2)))
		      (u8vector-set! buf 6 (u8vector-ref %vec (+fx offset 1)))
		      (u8vector-set! buf 7 (u8vector-ref %vec offset))
		      ($f64/u8vector-ref buf 0))))))

	 (define (js-setFloat64 this::JsDataView offset value lendian)
	    (with-access::JsDataView this (buffer %vec)
	       (with-access::JsArrayBuffer buffer (vec)
		  (unless (eq? %vec vec)
		     (error "js-setFloat64" "%vec != vec" this)))
	       (let ((len (u8vector-length %vec))
		     (lendian (js-totest lendian))
		     (value (js-tonumber value %this))
		     (v (if (flonum? value) value (fixnum->flonum value))))
		  (cond
		     ((<fx offset 0)
		      (js-get this (js-toname offset %this) %this))
		     ((>=fx offset len)
		      (js-undefined))
		     ((eq? host-lendian lendian)
		      ($f64/u8vector-set! %vec (/fx offset 8) v))
		     (else
		      ($f64/u8vector-set! buf 0 v)
		      (u8vector-set! %vec offset (u8vector-ref buf 7))
		      (u8vector-set! %vec (+fx offset 1) (u8vector-ref buf 6))
		      (u8vector-set! %vec (+fx offset 2) (u8vector-ref buf 5))
		      (u8vector-set! %vec (+fx offset 3) (u8vector-ref buf 4))
		      (u8vector-set! %vec (+fx offset 4) (u8vector-ref buf 3))
		      (u8vector-set! %vec (+fx offset 5) (u8vector-ref buf 2))
		      (u8vector-set! %vec (+fx offset 6) (u8vector-ref buf 1))
		      (u8vector-set! %vec (+fx offset 7) (u8vector-ref buf 0)))))))
	 
	 ;; bind the Dataview in the global object
	 (js-bind! %this %this 'DataView
	    :configurable #f :enumerable #f :value js-dataview)
	 
	 js-dataview)))
	 
