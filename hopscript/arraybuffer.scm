;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/arraybuffer.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 13 08:07:32 2014                          */
;*    Last change :  Wed Oct 25 16:18:38 2017 (serrano)                */
;*    Copyright   :  2014-17 Manuel Serrano                            */
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
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_number
	   __hopscript_worker)

   (export (js-init-arraybuffer! ::JsGlobalObject)
	   (js-u8vector->jsarraybuffer ::u8vector ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsArrayBuffer ...                            */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsArrayBuffer
   (lambda (o)
      (with-access::JsArrayBuffer o (data) data))
   (lambda (o %this)
      (js-u8vector->jsarraybuffer o (or %this (js-initial-global-object)))))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsArrayBuffer ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsArrayBuffer worker %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-arraybuffer)
	 (with-access::JsArrayBuffer obj (data frozen)
	    (let* ((ndata data)
		   (nobj (instantiate-JsArrayBuffer
			    (__proto__ (js-get js-arraybuffer 'prototype %this))
			    (frozen frozen)
			    (data ndata))))
	       (set! data '#u8())
	       (js-for-in obj
		  (lambda (k)
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
(define-method (hop->javascript o::JsArrayBuffer op compile isexpr)
   
   (define chars
      '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f))
   
   (with-access::JsArrayBuffer o (data frozen)
      (display "hop_buffer( \"JsArrayBuffer\", " op)
      (display (if frozen "true" "false") op)
      (display ", \"" op)
      (let loop ((i 0))
	 (when (<fx i (u8vector-length data))
	    (let ((v (uint8->fixnum (u8vector-ref data i))))
	       (write-char (vector-ref-ur chars (bit-rsh v 8)) op)
	       (write-char (vector-ref-ur chars (bit-and v #xf)) op))
	    (loop (+fx i 1))))
      (display "\")" op)))

;*---------------------------------------------------------------------*/
;*    javascript-buffer->arraybuffer ...                               */
;*---------------------------------------------------------------------*/
(define (javascript-buffer->arraybuffer name args %this)
   (with-access::JsGlobalObject %this (js-arraybuffer)
      (let* ((u8v (hexstring->u8vector (cadr args)))
	     (buf (instantiate-JsArrayBuffer
		     (__proto__ (js-get js-arraybuffer 'prototype %this))
		     (frozen (car args))
		     (data u8v))))
	 (js-put! buf 'byteLength (u8vector-length u8v) #f %this)
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
   (with-access::JsGlobalObject %this (__proto__ js-function js-object
					 js-arraybuffer)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 ;; builtin ArrayBuffer prototype
	 (define js-arraybuffer-prototype
	    (instantiate-JsObject
	       (__proto__ __proto__)))

	 (define (%js-arraybuffer this . items)
	    (apply js-arraybuffer-construct 
	       (js-arraybuffer-alloc js-arraybuffer %this)
	       items))

	 (set! js-arraybuffer
	    (js-make-function %this %js-arraybuffer 1 'ArrayBuffer
	       :__proto__ js-function-prototype
	       :prototype js-arraybuffer-prototype
	       :alloc (lambda (ctor) (js-arraybuffer-alloc ctor %this))
	       :construct (lambda (this . items)
			     (apply js-arraybuffer-construct this items))))

	 (define (js-arraybuffer-alloc constructor::JsFunction %this)
	    (instantiate-JsArrayBuffer
	       (cmap (js-not-a-cmap))
	       (__proto__ (js-get constructor 'prototype %this))))

	 (define (js-arraybuffer-construct this::JsArrayBuffer . items)
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
		     (js-bind! %this this 'byteLength
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
			:enumerable #t
			:hidden-class #t)))
	       
	       this))

	 ;; bind the ArrayBuffer in the global object
	 (js-bind! %this %this 'ArrayBuffer
	    :configurable #f :enumerable #f :value js-arraybuffer
	    :hidden-class #t)

	 js-arraybuffer)))

;*---------------------------------------------------------------------*/
;*    js-properties-name ::JsArray ...                                 */
;*---------------------------------------------------------------------*/
(define-method (js-properties-name obj::JsArrayBuffer enump %this)
   (let ((len (js-arraybuffer-length obj)))
      (vector-append (apply vector (map js-integer->jsstring (iota len)))
	 (call-next-method))))

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
