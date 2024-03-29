;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/arraybuffer.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 13 08:07:32 2014                          */
;*    Last change :  Thu Aug 19 11:27:37 2021 (serrano)                */
;*    Copyright   :  2014-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript ArrayBuffer                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_arraybuffer

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
	   __hopscript_lib
	   __hopscript_number
	   __hopscript_worker
	   __hopscript_array)

   (export (js-init-arraybuffer! ::JsGlobalObject)
	   (js-u8vector->jsarraybuffer ::u8vector ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsArrayBuffer ...                            */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsArrayBuffer
   (lambda (o ctx)
      (with-access::JsArrayBuffer o (data) data))
   (lambda (o ctx)
      (if (isa? ctx JsGlobalObject)
	  (js-u8vector->jsarraybuffer o ctx)
	  (error "string->obj ::JsArrayBuffer" "Not a JavaScript context" o))))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsArrayBuffer ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsArrayBuffer worker %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-arraybuffer)
	 (with-access::JsArrayBuffer obj (data frozen)
	    (let* ((ndata data)
		   (nobj (instantiateJsArrayBuffer
			    (__proto__ (js-get js-arraybuffer (& "prototype") %this))
			    (frozen frozen)
			    (data ndata))))
	       (set! data '#u8())
	       (js-for-in obj
		  (lambda (k %this)
		     (js-put! nobj k
			(js-donate (js-get obj k %_this) worker %this)
			#f %this))
		  %this)
	       nobj)))))
	    
;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsArray ...                                    */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsArrayBuffer op compile isexpr ctx)
   
   (define chars
      '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f))
   
   (with-access::JsArrayBuffer o (data frozen)
      (display "hop_buffer( \"JsArrayBuffer\", " op)
      (display (if frozen "true" "false") op)
      (display ", \"" op)
      (let loop ((i 0))
	 (when (<fx i (u8vector-length data))
	    (let ((v (uint8->fixnum (u8vector-ref data i))))
	       (write-char (vector-ref chars (bit-rsh v 8)) op)
	       (write-char (vector-ref chars (bit-and v #xf)) op))
	    (loop (+fx i 1))))
      (display "\")" op)))

;*---------------------------------------------------------------------*/
;*    javascript-buffer->arraybuffer ...                               */
;*---------------------------------------------------------------------*/
(define (javascript-buffer->arraybuffer name args %this)
   (with-access::JsGlobalObject %this (js-arraybuffer)
      (let* ((u8v (hexstring->u8vector (cadr args)))
	     (buf (instantiateJsArrayBuffer
		     (__proto__ (js-get js-arraybuffer (& "prototype") %this))
		     (frozen (car args))
		     (data u8v))))
	 (js-put! buf (& "byteLength") (u8vector-length u8v) #f %this)
	 buf)))
      
;*---------------------------------------------------------------------*/
;*    hexstring->u8vector ...                                          */
;*---------------------------------------------------------------------*/
(define (hexstring->u8vector str::bstring)
   
   (define (err c)
      (error "hexstring->arraybuffer" "Illegal character" c))
   
   (define (char-alpha c)
      (cond
         ((char>=? c #\a)
          (if (char<=? c #\f)
	      (+fx 10 (-fx (char->integer c) (char->integer #\a)))
	      (err c)))
         ((char>=? c #\A)
          (if (char<=? c #\F)
	      (+fx 10 (-fx  (char->integer c) (char->integer #\A)))
	      (err c)))
         ((char>=? c #\0)
          (if (char<=? c #\9)
	      (-fx (char->integer c) (char->integer #\0))
	      (err c)))
         (else
	  (err c))))
   
   (define (hex2 str j)
      (let ((n1 (char-alpha (string-ref-ur str j)))
	    (n2 (char-alpha (string-ref-ur str (+fx j 1)))))
	 (+fx (*fx n1 16) n2)))
   
   (let* ((len (string-length str))
	  (u8vec (make-u8vector (/fx len 2))))
      (let loop ((i 0))
	 (if (<fx i len)
	     (begin
		(u8vector-set! u8vec (/fx i 2) (hex2 str i))
		(loop (+fx i 2)))
	     u8vec))))

;*---------------------------------------------------------------------*/
;*    js-init-arraybuffer! ...                                         */
;*---------------------------------------------------------------------*/
(define (js-init-arraybuffer! %this)
   (unless (vector? __js_strings) (set! __js_strings (&init!)))
   (with-access::JsGlobalObject %this (js-function js-object
					 js-arraybuffer)
      
      ;; builtin ArrayBuffer prototype
      (define js-arraybuffer-prototype
	 (instantiateJsObject
	    (__proto__ (js-object-proto %this))
	    (elements ($create-vector 1))))
      
      (define (%js-arraybuffer this . items)
	 (if (eq? (js-new-target-pop! %this) (js-undefined))
	     (js-arraybuffer-construct
		(js-arraybuffer-alloc-sans-new-target %this js-arraybuffer)
		items)
	     (js-arraybuffer-construct this items)))
      
      (define (js-arraybuffer-alloc-sans-new-target %this ctor::JsFunction)
	 (instantiateJsArrayBuffer
	    (cmap (js-not-a-cmap))
	    (__proto__ (if (eq? ctor js-arraybuffer)
			   js-arraybuffer-prototype
			   (js-get ctor (& "prototype") %this)))))
      
      (define (js-arraybuffer-alloc %this constructor::JsFunction)
	 (with-access::JsGlobalObject %this (js-new-target)
	    (set! js-new-target constructor))
	 (js-arraybuffer-alloc-sans-new-target %this constructor))
      
      (define (js-arraybuffer-construct this::JsArrayBuffer items)
	 (with-access::JsArrayBuffer this (data)
	    (when (pair? items)
	       (let ((i (car items))
		     (f 0))
		  ;; data
		  (cond
		     ((u8vector? i)
		      (set! f (u8vector-length i))
		      (set! data i))
		     ((and (flonum? i) (>=fl i 1073741823.0))
		      (js-raise-range-error %this
			 "ArrayBufferView size is too large"
			 i))
		     (else
		      (let ((n (js-touint32 i %this)))
			 (set! f (uint32->fixnum n))
			 (set! data (make-u8vector f)))))
		  
		  ;; byteLength
		  (js-bind! %this this (& "byteLength")
		     :value f
		     :configurable #f
		     :writable #f
		     :enumerable #t
		     :hidden-class #t)
		  
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
				  (new (%js-arraybuffer (js-undefined) l)))
			      (with-access::JsArrayBuffer new ((dst data))
				 (let loop ((i 0))
				    (when (<fx i l)
				       (u8vector-set! dst i
					  (u8vector-ref data (+fx i b)))
				       (loop (+fx i 1))))
				 new)))))
		  
		  (js-bind! %this this (& "slice")
		     :value (js-make-function %this arraybuffer-slice
			       (js-function-arity arraybuffer-slice)
			       (js-function-info :name "slice" :len 2))
		     :configurable #f
		     :writable #t
		     :enumerable #t
		     :hidden-class #t)))
	    
	    this))
      
      (set! js-arraybuffer
	 (js-make-function %this %js-arraybuffer
	    (js-function-arity %js-arraybuffer)
	    (js-function-info :name "ArrayBuffer" :len 1)
	    :__proto__ (js-object-proto js-function)
	    :prototype js-arraybuffer-prototype
	    :alloc js-arraybuffer-alloc))
      
      ;; bind the ArrayBuffer in the global object
      (js-bind! %this %this (& "ArrayBuffer")
	 :configurable #f :enumerable #f :value js-arraybuffer
	 :hidden-class #t)
      
      js-arraybuffer))

;*---------------------------------------------------------------------*/
;*    js-properties-names ::JsArrayBuffer ...                          */
;*---------------------------------------------------------------------*/
(define-method (js-properties-names obj::JsArrayBuffer enump %this)
   (let ((len (js-arraybuffer-length obj)))
      (append! (map js-integer->jsstring (iota len))
	 (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-ownkeys ::JsArrayBuffer ...                                   */
;*---------------------------------------------------------------------*/
(define-method (js-ownkeys obj::JsArrayBuffer %this)
   (js-vector->jsarray (js-properties-name obj #t %this) %this))

;*---------------------------------------------------------------------*/
;*    js-has-property ::JsArrayBuffer ...                              */
;*---------------------------------------------------------------------*/
(define-method (js-has-property o::JsArrayBuffer p %this)
   (let ((i::uint32 (js-toindex p)))
      (cond
	 ((not (js-isindex? i))
	  (call-next-method))
	 ((<uint32 i (fixnum->uint32 (js-arraybuffer-length o)))
	  #t)
	 (else
	  (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    js-has-own-property ::JsArrayBuffer ...                          */
;*---------------------------------------------------------------------*/
(define-method (js-has-own-property o::JsArrayBuffer p %this::JsGlobalObject)
   (not (eq? (js-get-own-property o p %this) (js-undefined))))

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

;*---------------------------------------------------------------------*/
;*    js-get-own-property-descriptor ...                               */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property-descriptor o::JsArrayBuffer p %this::JsGlobalObject)
   (with-access::JsArrayBuffer o (frozen)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((not (js-isindex? i))
	     (call-next-method))
	    ((<uint32 i (js-arraybuffer-length o))
	     (js-property-descriptor %this #t
		:value (js-arraybuffer-ref o (uint32->fixnum i))
		:enumerable #t
		:writable (not frozen)
		:configurable (not frozen)))
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-get-arraybuffer ...                                           */
;*---------------------------------------------------------------------*/
(define (js-get-arraybuffer o p %this)
   (let ((i::uint32 (js-toindex p)))
      (when (and (js-isindex? i) (<uint32 i (js-arraybuffer-length o)))
	 (js-arraybuffer-ref o (uint32->fixnum i)))))
   
;*---------------------------------------------------------------------*/
;*    js-get-property-value ::JsArrayBuffer ...                        */
;*    -------------------------------------------------------------    */
;*    This method is optional. It could be removed without changing    */
;*    the programs behaviors. It merely optimizes access to strings.   */
;*---------------------------------------------------------------------*/
(define-method (js-get-property-value o::JsArrayBuffer base p %this)
   (or (js-get-arraybuffer o p %this) (call-next-method)))

;*---------------------------------------------------------------------*/
;*    js-get ::JsArrayBuffer ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsArrayBuffer p %this)
   (or (js-get-arraybuffer o p %this) (call-next-method)))

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
			   (if (js-procedure? setter)
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
	     (uint32->fixnum (bit-andu32 (js-touint32 v %this) #u32:255)))
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
;*    js-u8vector->jsarraybuffer ...                                   */
;*---------------------------------------------------------------------*/
(define (js-u8vector->jsarraybuffer o::u8vector %this)
   #t)

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
