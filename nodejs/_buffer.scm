;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/nodejs/_buffer.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Aug 30 06:52:06 2014                          */
;*    Last change :  Tue Feb 21 09:45:59 2023 (serrano)                */
;*    Copyright   :  2014-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native native bindings                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__buffer

   (include "nodejs_debug.sch" "nodejs_types.sch")
   (include "../hopscript/stringthread.sch")

   (library hopscript hop)

   (import  __nodejs_process
	    __nodejs_require)

   (import  (__nodejs_buffer "| echo \"(module __nodejs_buffer (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\""))

   (static  (class Slab
	       (%this::JsGlobalObject read-only)
	       (js-slowbuffer::JsObject read-only)
	       (slowbuffer (default #f))
	       (offset::long (default 0))
	       (lastoffset::long (default 0))
	       (slice (default #f))))
   
   (export  (class JsSlowBuffer::JsArrayBuffer)

	    (class JsFastBuffer::JsTypedArray)

	    (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)
	    (blit-string-ascii-clamp! ::bstring ::long ::bstring ::long ::long)
	    (string->ucs2-string ::bstring ::long ::long)
	    (string-utf8-normalize-utf16 ::bstring ::long ::long)
	    (8bits-encode-utf8 ::bstring ::long ::long)
	    (js-string->jsslowbuffer ::bstring ::JsGlobalObject)
	    (js-string->jsfastbuffer ::bstring ::JsGlobalObject)
	    (js-jsslowbuffer->string::bstring ::JsSlowBuffer)
	    (js-jsfastbuffer->string::bstring ::JsFastBuffer)
	    (process-buffer ::JsGlobalObject ::JsObject)
	    (make-slowbuffer ::JsGlobalObject)
	    (make-slab-allocator ::JsGlobalObject ::JsObject)
	    (slab-allocate ::object ::obj ::long)
	    (slab-shrink! ::obj ::obj ::long ::long)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    js-arraybuffer-length ::JsSlowBuffer ...                         */
;*---------------------------------------------------------------------*/
(define-method (js-arraybuffer-length o::JsSlowBuffer)
   (with-access::JsSlowBuffer o (data)
      (string-length data)))

;*---------------------------------------------------------------------*/
;*    js-get-length ::JsSlowBuffer ...                                 */
;*---------------------------------------------------------------------*/
(define-method (js-get-length o::JsSlowBuffer %this #!optional cache)
   (with-access::JsSlowBuffer o (data)
      (string-length data)))

;*---------------------------------------------------------------------*/
;*    js-init-buffer! ...                                              */
;*---------------------------------------------------------------------*/
(define (js-init-buffer! %this)
   (unless (vector? __js_strings) (set! __js_strings (&init!)))
   ;; create buffer cmap
   (js-init-buffer-cmap! %this))

;*---------------------------------------------------------------------*/
;*    js-init-buffer-cmap! ...                                         */
;*---------------------------------------------------------------------*/
(define (js-init-buffer-cmap! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-buffer-cmap js-slowbuffer-cmap)
      (set! js-buffer-cmap (js-strings->cmap '#("length" "offset" "parent")))
      (set! js-slowbuffer-cmap (js-strings->cmap '#("length")))))

;*---------------------------------------------------------------------*/
;*    buffer-parser ...                                                */
;*---------------------------------------------------------------------*/
(define buffer-parser
   (let ((env (getenv "NODE_DEBUG")))
      (cond
	 ((not (string? env)) 0)
	 ((string-contains env "_buffer") 2)
	 (else 0))))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsArrayBuffer ...                            */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsSlowBuffer
   jsslowbuffer->u8vector
   js-u8vector->jsslowbuffer)

(register-class-serialization! JsFastBuffer
   jsfastbuffer->u8vector
   js-u8vector->jsfastbuffer)

;*---------------------------------------------------------------------*/
;*    string-int-set! ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (string-int-set! str::bstring i::long n::int)
   (string-set! str i (integer->char n)))

;*---------------------------------------------------------------------*/
;*    string-in-set-ur! ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (string-in-set-ur! str::bstring i::long n::int)
   (string-set-ur! str i (integer->char n)))

;*---------------------------------------------------------------------*/
;*    js-typedarray-ref ::JsFastBuffer ...                             */
;*---------------------------------------------------------------------*/
(define-method (js-typedarray-ref o::JsFastBuffer)
   (lambda (buf i)
      (if (string? buf)
	  (char->integer (string-ref buf i))
	  (uint8->fixnum (u8vector-ref buf i)))))

;*---------------------------------------------------------------------*/
;*    js-typedarray-set! ...                                           */
;*---------------------------------------------------------------------*/
(define-method (js-typedarray-set! o::JsFastBuffer)
   (lambda (buf i v %this)
      (let* ((w (bit-andu32 #u32:255 (js-touint32 v %this)))
	     (n (uint32->fixnum w)))
	 (if (string? buf)
	     (string-int-set! buf i n)
	     (u8vector-set! buf i (fixnum->uint8 n))))))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsTypeArray ...                                */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsSlowBuffer op compile isexpr ctx)
   (with-access::JsArrayBuffer o (data)
      (display "hop_buffer(\"JsSlowBuffer\", \"" op)
      (display (string-hex-extern data) op)
      (display "\")" op)))

;*---------------------------------------------------------------------*/
;*    js-string->jsslowbuffer ...                                      */
;*---------------------------------------------------------------------*/
(define (js-string->jsslowbuffer str %this)
   (with-access::JsGlobalObject %this (js-slowbuffer-proto js-slowbuffer-cmap)
      (instantiateJsSlowBuffer
	 (cmap js-slowbuffer-cmap)
	 (__proto__ js-slowbuffer-proto)
	 (elements (vector (string-length str)))
	 (data str))))

;*---------------------------------------------------------------------*/
;*    js-string->jsfastbuffer ...                                      */
;*---------------------------------------------------------------------*/
(define (js-string->jsfastbuffer str %this)
   (with-access::JsGlobalObject %this (js-buffer-proto js-buffer-cmap)
      (let ((slowbuffer (js-string->jsslowbuffer str %this))
	    (len (string-length str)))
	 (with-access::JsSlowBuffer slowbuffer (data)
	    (instantiateJsFastBuffer
	       (cmap js-buffer-cmap)
	       (__proto__ js-buffer-proto)
	       (elements (vector len 0 slowbuffer))
	       (%data data)
	       (frozen #f)
	       (buffer slowbuffer)
	       (byteoffset (fixnum->uint32 0))
	       (length (fixnum->uint32 len))
	       (bpe 1))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring->jsfastbuffer ...                                    */
;*---------------------------------------------------------------------*/
(define (js-jsstring->jsfastbuffer obj enc %this)
   (if (and (isa? obj JsStringLiteralASCII)
	    (memq obj '(#unspecified utf8 ascii latin1)))
       (js-string->jsfastbuffer (string-copy (js-jsstring->string obj)) %this)
       (let* ((data (js-jsstring->string obj))
	      (str (cond
		      ((or (eq? enc (& "utf8")) (eq? enc (js-undefined)))
		       (multiple-value-bind (str enc)
			  (utf8-normalize-utf16 data #t 0 (string-length data))
			  str))
		      ((eq? enc (& "binary"))
		       (8bits-encode-utf8 data 0 (string-length data)))
		      ((eq? enc (& "ascii"))
		       (string-copy data))
		      ((eq? enc (& "hex"))
		       (string-hex-intern data))
		      ((eq? enc (& "ucs2"))
		       (ucs2-string->utf8-string
			  (string->ucs2-string data 0 (string-length data))))
		      ((eq? enc (& "latin1"))
		       (iso-latin->utf8 data))
		      ((eq? enc (& "base64"))
		       (let ((ip (open-input-string! data 0 (string-length data)))
			     (op (open-output-string)))
			  (base64-encode-port ip op 0)
			  (close-output-port op)))
		      (else
		       (js-raise-type-error %this "Bad encoding" enc)))))
	  (js-string->jsfastbuffer str %this))))

;*---------------------------------------------------------------------*/
;*    js-array->jsfastbuffer ...                                       */
;*---------------------------------------------------------------------*/
(define (js-array->jsfastbuffer arr %this)
   (with-access::JsGlobalObject %this (js-buffer-proto)
      (let* ((len::uint32 (js-array-length arr))
	     (str (make-string (uint32->fixnum len))))
	 (let loop ((i #u32:0))
	    (unless (=u32 i len)
	       (if (js-has-property arr (js-toname i %this) %this)
		   (string-set! str (uint32->fixnum i)
		      (integer->char
			 (js-tointeger (js-get arr i %this) %this)))
		   (string-set! str (uint32->fixnum i) #a000))
	       (loop (+u32 i #u32:1))))
	 (js-string->jsfastbuffer str %this))))

;*---------------------------------------------------------------------*/
;*    js-arraybuffer->jsfastbuffer ...                                 */
;*---------------------------------------------------------------------*/
(define (js-arraybuffer->jsfastbuffer arr offset len %this)
   (with-access::JsGlobalObject %this (js-buffer-proto js-slowbuffer-proto
					 js-slowbuffer-cmap js-buffer-cmap)
      (with-access::JsArrayBuffer arr (data)
	 (let* ((len::uint32 (if (eq? len (js-undefined))
				 (fixnum->uint32 (u8vector-length data))
				 (js-touint32 len %this)))
		(offset::uint32 (if (eq? offset (js-undefined))
				    #u32:0
				    (js-touint32 offset %this)))
		(slowbuffer (instantiateJsSlowBuffer
			       (__proto__ js-slowbuffer-proto)
			       (cmap js-slowbuffer-cmap)
			       (elements (vector (uint32->fixnum len)))
			       (data data))))
	    (with-access::JsSlowBuffer slowbuffer (data)
	       (instantiateJsFastBuffer
		  (cmap js-buffer-cmap)
		  (__proto__ js-buffer-proto)
		  (elements (vector (uint32->fixnum len) 0 slowbuffer))
		  (%data data)
		  (frozen #f)
		  (buffer slowbuffer)
		  (byteoffset offset)
		  (length len)
		  (bpe 1)))))))

;*---------------------------------------------------------------------*/
;*    js-buffer->jsfastbuffer ...                                      */
;*---------------------------------------------------------------------*/
(define (js-buffer->jsfastbuffer arr %this)
   
   (define (u8vector-copy v)
      (let* ((l (u8vector-length v))
	     (n (make-u8vector l)))
	 (u8vector-copy! n 0 v 0 l)
	 n))
   
   (with-access::JsGlobalObject %this (js-buffer-proto js-slowbuffer-proto
					 js-buffer-cmap js-slowbuffer-cmap)
      (with-access::JsFastBuffer arr (buffer %data)
	 (with-access::JsSlowBuffer buffer (data)
	    (let* ((len::uint32 (fixnum->uint32
				   (if (string? %data)
				       (string-length %data)
				       (u8vector-length %data))))
		   (data (if (string? %data)
			     (string-copy %data)
			     (u8vector-copy %data)))
		   (slowbuffer (instantiateJsSlowBuffer
				  (cmap js-slowbuffer-cmap)
				  (__proto__ js-slowbuffer-proto)
				  (elements (vector (uint32->fixnum len)))
				  (data data))))
	       (instantiateJsFastBuffer
		  (cmap js-buffer-cmap)
		  (__proto__ js-buffer-proto)
		  (elements (vector (uint32->fixnum len) 0 slowbuffer))
		  (%data data)
		  (frozen #f)
		  (buffer slowbuffer)
		  (byteoffset #u32:0)
		  (length len)
		  (bpe 1)))))))

;*---------------------------------------------------------------------*/
;*    js-object->jsfastbuffer ...                                      */
;*---------------------------------------------------------------------*/
(define (js-object->jsfastbuffer this obj offset-or-enc len %this)
   (with-access::JsGlobalObject %this (js-buffer-proto js-slowbuffer-proto)
      (let ((vof (js-valueof obj %this)))
	 (if (eq? vof obj)
	     (let ((prim (js-toprimitive-for-string obj %this)))
		(js-jsstring->jsfastbuffer prim offset-or-enc %this))
	     (js-obj->jsfastbuffer this vof offset-or-enc len %this)))))

;*---------------------------------------------------------------------*/
;*    js-obj->jsfastbuffer ...                                         */
;*---------------------------------------------------------------------*/
(define (js-obj->jsfastbuffer this obj offset-or-enc len %this)
   (cond
      ((js-jsstring? obj)
       (js-jsstring->jsfastbuffer obj offset-or-enc %this))
      ((js-array? obj)
       (js-array->jsfastbuffer obj %this))
      ((isa? obj JsArrayBuffer)
       (js-arraybuffer->jsfastbuffer obj offset-or-enc len %this))
      ((isa? obj JsFastBuffer)
       (js-buffer->jsfastbuffer obj %this))
      ((isa? obj JsObject)
       (js-object->jsfastbuffer this obj offset-or-enc len %this))
      (else
       (js-undefined))))

;*---------------------------------------------------------------------*/
;*    js-jsslowbuffer->string ...                                      */
;*---------------------------------------------------------------------*/
(define (js-jsslowbuffer->string buf::JsSlowBuffer)
   (with-access::JsSlowBuffer buf (data)
      data))

;*---------------------------------------------------------------------*/
;*    js-jsfastbuffer->string ...                                      */
;*---------------------------------------------------------------------*/
(define (js-jsfastbuffer->string buf::JsFastBuffer)
   (with-access::JsFastBuffer buf (%data byteoffset length)
      (let ((start (uint32->fixnum byteoffset))
	    (len (uint32->fixnum length)))
	 (substring %data start (+fx start len)))))

;*---------------------------------------------------------------------*/
;*    jsfastbuffer->u8vector ...                                       */
;*---------------------------------------------------------------------*/
(define (jsfastbuffer->u8vector o::JsFastBuffer)
   (with-access::JsFastBuffer o (%data byteoffset length)
      (let* ((fxlen (uint32->fixnum length))
	     (buf (make-u8vector fxlen))
	     (off (uint32->fixnum byteoffset)))
	 (let loop ((i 0))
	    (if (=fx i fxlen)
		buf
		(begin
		   (u8vector-set! buf i
		      (fixnum->uint8
			 (char->integer (string-ref %data (+fx i off)))))
		   (loop (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    jsslowbuffer->u8vector ...                                       */
;*---------------------------------------------------------------------*/
(define (jsslowbuffer->u8vector o::JsSlowBuffer)
   (with-access::JsSlowBuffer o (data byteoffset)
      (let* ((fxlen (string-length data))
	     (buf (make-u8vector fxlen)))
	 (let loop ((i 0))
	    (if (=fx i fxlen)
		buf
		(begin
		   (u8vector-set! buf i
		      (fixnum->uint8
			 (char->integer (string-ref data i))))
		   (loop (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    js-u8vector->jsslowbuffer ...                                    */
;*---------------------------------------------------------------------*/
(define (js-u8vector->jsslowbuffer buf %this)
   (let ((str (make-string (u8vector-length buf))))
      (let loop ((i (-fx (u8vector-length buf) 1)))
	 (if (=fx i -1)
	     (js-string->jsslowbuffer str %this)
	     (begin
		(string-int-set! str i
		   (uint8->fixnum (u8vector-ref buf i)))
		(loop (-fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    js-u8vector->jsfastbuffer ...                                    */
;*---------------------------------------------------------------------*/
(define (js-u8vector->jsfastbuffer buf %this)
   (let ((str (make-string (u8vector-length buf))))
      (let loop ((i (-fx (u8vector-length buf) 1)))
	 (if (=fx i -1)
	     (js-string->jsfastbuffer str %this)
	     (begin
		(string-int-set! str i
		   (uint8->fixnum (u8vector-ref buf i)))
		(loop (-fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    js-arraybuffer-length ::JsSlowBuffer ...                         */
;*---------------------------------------------------------------------*/
(define-method (js-arraybuffer-length o::JsSlowBuffer)
   (with-access::JsSlowBuffer o (data)
      (string-length data)))

;*---------------------------------------------------------------------*/
;*    js-arraybuffer-ref ::JsSlowBuffer ...                            */
;*---------------------------------------------------------------------*/
(define-method (js-arraybuffer-ref o::JsSlowBuffer index)
   (with-access::JsSlowBuffer o (data)
      (char->integer (string-ref-ur data index))))

;*---------------------------------------------------------------------*/
;*    js-arraybuffer-set! ::JsSlowBuffer ...                           */
;*---------------------------------------------------------------------*/
(define-method (js-arraybuffer-set! o::JsSlowBuffer index val)
   (with-access::JsSlowBuffer o (data)
      (string-in-set-ur! data index val)))

;*---------------------------------------------------------------------*/
;*    js-put-length! ::JsFastBuffer ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-put-length! o::JsFastBuffer v::obj throw::bool cache %this::JsGlobalObject)
   (with-access::JsTypedArray o (length buffer elements)
      (if (eq? buffer (class-nil JsArrayBuffer))
	  (begin
	     (set! length (fixnum->uint32 (->fixnum v)))
	     (vector-set! elements 0 v))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-put! ::JsFastBuffer ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::JsFastBuffer p v throw %this)
   (with-access::JsTypedArray o (length buffer elements)
      (if (and (eq? p (& "length")) (eq? buffer (class-nil JsArrayBuffer)))
	  (begin
	     (set! length (fixnum->uint32 (->fixnum v)))
	     (vector-set! elements 0 v))
	  (call-next-method))))
   
;*---------------------------------------------------------------------*/
;*    js-buffer-constr ...                                             */
;*---------------------------------------------------------------------*/
(define (js-buffer-constr proto %this)
   (with-access::JsGlobalObject %this (js-buffer-proto js-slowbuffer-proto
					 js-slowbuffer-cmap js-buffer-cmap)
      (instantiateJsFastBuffer
	 (cmap js-buffer-cmap)
	 (__proto__ proto)
	 (elements (vector 0 0 (js-undefined)))
	 (bpe #u32:1))))

;*---------------------------------------------------------------------*/
;*    hopscript ...                                                    */
;*    -------------------------------------------------------------    */
;*    Nodejs Buffers are special objects which cannot be implemented   */
;*    in JavaScript. The __nodejs__buffer module backs up the          */
;*    __nodejs_buffer module which contains the Nodejs JavaScript      */
;*    part of the implementation. This current module modifies the     */
;*    allocation function of the JavaScript Buffer constructor in      */
;*    order not to allocate a JsObject but a JsTypedArray.             */
;*---------------------------------------------------------------------*/
(define (hopscript %this this %scope %module)
   (js-init-buffer! %this)
   ((@ hopscript __nodejs_buffer) %this this %scope %module)
   
   (with-access::JsGlobalObject %this (js-buffer-proto js-slowbuffer-proto js-nodejs-pcache)
      (let* ((exp (js-get-jsobject-name/cache %module (& "exports") #f %this
		     (js-pcache-ref js-nodejs-pcache 0)))
	     (buf (js-get-jsobject-name/cache exp (& "Buffer") #f %this
		     (js-pcache-ref js-nodejs-pcache 1)))
	     (proto (js-get-jsobject-name/cache buf (& "prototype") #f %this
		       (js-pcache-ref js-nodejs-pcache 2))))
	 
	 (with-access::JsGlobalObject %this (js-buffer-proto js-slowbuffer-proto)
	    (set! js-buffer-proto proto))
	 
	 (with-access::JsFunction buf (alloc)
	    (set! alloc
	       (lambda (%this ctor)
		  ;; see makeFastBuffer below for the complete JavaScript
		  ;; land JsTypedArray initialization
		  (js-buffer-constr proto %this))))
	 
	 exp)))

;*---------------------------------------------------------------------*/
;*    utf8-substring-length ...                                        */
;*    -------------------------------------------------------------    */
;*    Return the number of characters of an UTF8 string.               */
;*---------------------------------------------------------------------*/
(define (utf8-substring-length str::bstring len)
   (let ((len (minfx (string-length str) len)))
      (let loop ((r 0)
		 (l 0))
	 (if (=fx r len)
	     l
	     (loop (+fx r (utf8-char-size (string-ref str r))) (+fx l 1))))))

;*---------------------------------------------------------------------*/
;*    string-force-ascii! ...                                          */
;*---------------------------------------------------------------------*/
(define (string-force-ascii! str::bstring)
   (let ((len (string-length str)))
      (let loop ((i 0))
	 (if (=fx i len)
	     str
	     (let ((n (char->integer (string-ref str i))))
		(when (>fx n 127)
		   (string-set! str i (integer->char (bit-and n #x7f))))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    string-utf8-normalize-utf16                                      */
;*---------------------------------------------------------------------*/
(define (string-utf8-normalize-utf16 str::bstring start end)
   (multiple-value-bind (str enc)
      (utf8-normalize-utf16 str #t start end)
      (if (eq? enc 'ascii)
	  (js-ascii->jsstring str)
	  (js-utf8->jsstring str))))

;*---------------------------------------------------------------------*/
;*    8bits-encode-utf8 ...                                            */
;*---------------------------------------------------------------------*/
(define (8bits-encode-utf8 str::bstring start end)
   (let ((s (substring str start end)))
      (if (utf8-string? str #t)
	  s
	  (iso-latin->utf8! s))))

;*---------------------------------------------------------------------*/
;*    utf8-string-get ...                                              */
;*    -------------------------------------------------------------    */
;*    Decode the UTF8 character start at index i, b0 is s[ i ].        */
;*    Returns the UCS2 encoding of that character.                     */
;*---------------------------------------------------------------------*/
(define (utf8-string-get string::bstring i b0)
   (let loop ((byte (char->integer b0))
	      (ucs2 (char->integer b0))
	      (bits 6)
	      (i i))
      (if (=fx (bit-and byte #x40) 0)
	  (let ((ucs2 (bit-and ucs2 (-fx (bit-lsh 1 bits) 1))))
	     (if (>=fx ucs2 #x10000)
		 (-fx ucs2 #x10000)
		 ucs2))
	  (let ((next (char->integer (string-ref string (+fx i 1)))))
	     (loop (bit-lsh byte 1)
		(+fx (bit-lsh ucs2 6) (bit-and next #x3f))
		(+fx bits 5)
		(+fx i 1))))))

;*---------------------------------------------------------------------*/
;*    blit-string-ascii-clamp! ...                                     */
;*---------------------------------------------------------------------*/
(define (blit-string-ascii-clamp! string1::bstring o1 string2::bstring o2 len)
   (let loop ((i 0))
      (when (<fx i len)
         (let ((n (char->integer (string-ref-ur string1 (+fx o1 i)))))
            (string-set-ur! string2 (+fx o2 i) (integer->char (bit-and n 127)))
            (loop (+fx i 1))))))

;*---------------------------------------------------------------------*/
;*    blit-string-ascii-decode! ...                                    */
;*---------------------------------------------------------------------*/
(define (blit-string-ascii-decode! string1::bstring o1 string2::bstring o2 len)
   (let ((len1 (string-length string1)))
      (let loop ((i o1)
		 (j 0)
		 (l 0))
	 (if (and (<fx j len) (<fx i len1))
	     (let ((n (string-ref-ur string1 (+fx o1 i))))
		(if (char>? n #a127)
		    ;; an utf8 string
		    (let ((sz (utf8-char-size n))
			  (ucs2 (utf8-string-get string1 (+fx o1 i) n)))
		       (string-in-set-ur! string2 (+fx o2 j)
			  (bit-and ucs2 255))
		       (loop (+fx i sz) (+fx j 1) (+fx l 1)))
		    (begin
		       (string-set-ur! string2 (+fx o2 j) n)
		       (loop (+fx i 1) (+fx j 1) (+fx l 1)))))
	     l))))

;*---------------------------------------------------------------------*/
;*    blit-string-binary-decode! ...                                   */
;*---------------------------------------------------------------------*/
(define (blit-string-binary-decode! string1::bstring o1 string2::bstring o2 len)
   (blit-string-ascii-decode! string1 o1 string2 o2 len))

;*---------------------------------------------------------------------*/
;*    blit-string-binary! ...                                          */
;*---------------------------------------------------------------------*/
(define (blit-string-binary! string1::bstring o1 string2::bstring o2 len)
   (let ((len1 (string-length string1)))
      (let loop ((i o1)
		 (j 0)
		 (l 0))
	 (if (and (<fx j len) (<fx i len1))
	     (let ((n (string-ref-ur string1 (+fx o1 i))))
		(string-set-ur! string2 (+fx o2 j) n)
		(loop (+fx i 1) (+fx j 1) (+fx l 1)))
	     l))))

;*---------------------------------------------------------------------*/
;*    blit-string-utf8! ...                                            */
;*---------------------------------------------------------------------*/
(define (blit-string-utf8! string1::bstring o1 string2::bstring o2 len)
   (if (ascii-string? string1)
       (begin
	  (blit-string! string1 o1 string2 o2 len)
	  (values len len))
       (let ((len1 (string-length string1)))
	  (let loop ((i o1)
		     (j 0)
		     (l 0)
		     (c 0))
	     (if (and (<fx j len) (<fx i len1))
		 (let ((n (string-ref-ur string1 (+fx o1 i))))
		    (if (char>? n #a127)
			;; an utf8 string
			(if (char=? n #a248)
			    ;; #xf8 is Bigloo special encoding, which must
			    ;; be replace with the unicode replacement character
			    (if (<=fx (+fx j 3) len)
				(let ((k (+fx o2 j)))
				   (blit-string! "\xef\xbf\xbd" 0
				      string2 k 3)
				   (loop (+fx i 4) (+fx j 3) (+fx l 3) (+fx c 1)))
				(values l n))
			    (let ((sz (utf8-char-size n)))
			       (if (<=fx (+fx j sz) len)
				   (let ((k (+fx o2 j)))
				      (blit-string! string1 (+fx o1 i)
					 string2 k sz)
				      (loop (+fx i sz) (+fx j sz) (+fx l sz) (+fx c 1)))
				   (values l n))))
			(begin
			   (string-set-ur! string2 (+fx o2 j) n)
			   (loop (+fx i 1) (+fx j 1) (+fx l 1) (+fx c 1)))))
		 (values l c))))))

;*---------------------------------------------------------------------*/
;*    blit-string-ucs2! ...                                            */
;*    -------------------------------------------------------------    */
;*    Little endian representation of UCS2.                            */
;*---------------------------------------------------------------------*/
(define (blit-string-ucs2! string1::ucs2string o1 string2 o2 len)
   (let loop ((i 0))
      (if (<fx i (/fx len 2))
	  (let ((c (ucs2->integer (ucs2-string-ref string1 i))))
	     (string-int-set! string2 (+fx o2 (*fx i 2))
		(bit-and c 255))
	     (string-int-set! string2 (+fx o2 (+fx 1 (*fx i 2)))
		(bit-rsh c 8))
	     (loop (+fx i 1)))
	  (*fx i 2))))

;*---------------------------------------------------------------------*/
;*    blit-string-hex! ...                                             */
;*    -------------------------------------------------------------    */
;*    Little endian hexadecimal representation                         */
;*---------------------------------------------------------------------*/
(define (blit-string-hex! string1::ucs2string o1 string2 o2 len)
   
   (define (integer->hex n)
      (string-ref "0123456789abcdef" n))
   
   (let loop ((i 0))
      (if (<fx i (/fx len 2))
	  (let* ((c (ucs2->integer (ucs2-string-ref string1 i)))
		 (n0 (bit-and c 255))
		 (n1 (bit-rsh c 8))
		 (j (*fx i 4)))
	     (string-set! string2 (+fx o2 j)
		(integer->hex (bit-and n0 8)))
	     (string-set! string2 (+fx o2 (+fx j 1))
		(integer->hex (bit-rsh n0 8)))
	     (string-set! string2 (+fx o2 (+fx j 2))
		(integer->hex (bit-and n1 8)))
	     (string-set! string2 (+fx o2 (+fx j 3))
		(integer->hex (bit-rsh n1 8)))
	     (loop (+fx i 1)))
	  (*fx i 4))))

;*---------------------------------------------------------------------*/
;*    string->ucs2-string ...                                          */
;*    -------------------------------------------------------------    */
;*    Convert a LE 8bits strings into an equivalent UCS2 string.       */
;*---------------------------------------------------------------------*/
(define (string->ucs2-string string::bstring start end)
   (let* ((len (*fx (/fx (-fx end start) 2) 2))
	  (res (make-ucs2-string (/fx len 2))))
      (let loop ((i 0))
	 (if (=fx i len)
	     res
	     (let* ((j (+fx start i))
		    (c0 (char->integer (string-ref string j)))
		    (c1 (char->integer (string-ref string (+fx j 1))))
		    (ucs2 (+fx (bit-lsh c1 8) c0)))
		(ucs2-string-set! res (/fx i 2) (integer->ucs2 ucs2))
		(loop (+fx i 2)))))))
   
;*---------------------------------------------------------------------*/
;*    make-slowbuffer ...                                              */
;*---------------------------------------------------------------------*/
(define (make-slowbuffer %this::JsGlobalObject)
   
   (define __js_strings_execute_first
      (js-init-buffer! %this))
   
   (define slowbuffer-proto
      (with-access::JsGlobalObject %this (js-slowbuffer-proto js-object)
	 (unless js-slowbuffer-proto
	    (set! js-slowbuffer-proto
	       (with-access::JsFunction js-object (constrmap prototype)
		  (js-make-jsobject 25
		     (duplicate::JsConstructMap constrmap (%id (gencmapid)))
		     prototype))))
	 js-slowbuffer-proto))
   
   (define (slowbuffer-constr this a0)
      (let loop ((a0 a0))
	 (cond
	    ((integer? a0)
	     (let ((n::double (exact->inexact a0)))
		(cond
		   ((>fl n (exptfl 2. 32.))
		    (js-raise-type-error %this "Bad argument" a0))
		   ((>=fl n 1073741823.0) ;; #x3fffffff + 1
		    (js-raise-range-error %this "length (~s) > kMaxLength" a0))
		   (else
		    (with-access::JsGlobalObject %this (js-object js-slowbuffer-proto js-slowbuffer-cmap)
		       (let ((this (instantiateJsSlowBuffer
				      (cmap js-slowbuffer-cmap)
				      (__proto__ js-slowbuffer-proto)
				      (elements (vector (->fixnum a0)))
				      (data ($make-string/wo-fill (->fixnum a0))))))
			  ;; length
			  (js-bind! %this this (& "length")
			     :value a0
			     :configurable #f
			     :writable #f
			     :enumerable #t)
			  this))))))
	    ((string? a0)
	     (with-access::JsGlobalObject %this (js-object
						   js-slowbuffer-proto
						   js-slowbuffer-cmap)
		(let* ((data a0)
		       (this (instantiateJsSlowBuffer
				(cmap js-slowbuffer-cmap)
				(__proto__ js-slowbuffer-proto)
				(elements (vector (string-length data)))
				(data data))))
		   ;; length
		   (js-bind! %this this (& "length")
		      :value (string-length data)
		      :configurable #f
		      :writable #f
		      :enumerable #t)
		   this)))
	    ((js-jsstring? a0)
	     (with-access::JsGlobalObject %this (js-object
						   js-slowbuffer-proto
						   js-slowbuffer-cmap)
		(let* ((data (js-jsstring->string a0))
		       (this (instantiateJsSlowBuffer
				(cmap js-slowbuffer-cmap)
				(__proto__ js-slowbuffer-proto)
				(elements (vector (string-length data)))
				(data data))))
		   ;; length
		   (js-bind! %this this (& "length")
		      :value (string-length data)
		      :configurable #f
		      :writable #f
		      :enumerable #t)
		   this)))
	    ((isa? a0 JsSlowBuffer)
	     (with-access::JsSlowBuffer a0 (data)
		;; MS: 25 nov 2014, should the length be set to
		;; (string-length data) or (js-get a0 'length %this)?
		(loop data)))
	    (else
	     (error "buffer" "Illegal constructor call" a0)))))
   
   (define js-slowbuffer
      (js-make-function %this slowbuffer-constr
	 (js-function-arity slowbuffer-constr)
	 (js-function-info :name "SlowBuffer" :len 1)
	 :alloc (lambda (%this o) #unspecified)
	 :size 10
	 :prototype slowbuffer-proto
	 :shared-cmap #f))
   
   (define (check-offset data::bstring offset fxoffset sizeof action)
      ;; the error messages (case sensitive) are imposed by node_buffer.cc
      (cond
	 ((or (not (integer? offset)) (< offset 0) (<fx fxoffset 0))
	  (js-raise-type-error %this "offset is not uint" offset))
	 ((and (flonum? offset) (>fl offset (exptfl 2. 31.)))
	  (js-raise-type-error %this "Trying to ~a beyond buffer length"
	     (js-string->jsstring action)))
	 ((>fx (->fixnum offset) (-fx (string-length data) sizeof))
	  (js-raise-range-error %this
	     "Trying to ~a beyond buffer length"
	     (js-string->jsstring action)))))
   
   (define (byte-ref str i)
      (fixnum->uint8 (char->integer (string-ref-ur str i))))
   
   (define (byte-set! str i v)
      (string-int-set! str i (uint8->fixnum v)))
   
   (define (get-data obj)
      (cond
	 ((isa? obj JsSlowBuffer)
	  (with-access::JsSlowBuffer obj (data) data))
	 ((isa? obj JsTypedArray)
	  (with-access::JsTypedArray obj (%data) %data))
	 (else
	  (js-raise-type-error obj "not a buffer" #f))))
   
   (define (read-float this offset noassert le)
      (let ((data (get-data this))
	    (i (->fixnum offset)))
	 (unless (js-totest noassert) (check-offset data offset i 4 "read"))
	 (let ((buf (make-u8vector 4)))
	    (if le
		(begin
		   (u8vector-set! buf 0 (byte-ref data i))
		   (u8vector-set! buf 1 (byte-ref data (+fx i 1)))
		   (u8vector-set! buf 2 (byte-ref data (+fx i 2)))
		   (u8vector-set! buf 3 (byte-ref data (+fx i 3))))
		(begin
		   (u8vector-set! buf 3 (byte-ref data i))
		   (u8vector-set! buf 2 (byte-ref data (+fx i 1)))
		   (u8vector-set! buf 1 (byte-ref data (+fx i 2)))
		   (u8vector-set! buf 0 (byte-ref data (+fx i 3)))))
	    ($f32/u8vector-ref buf 0))))
   
   (define (read-double this offset noassert le)
      (let ((data (get-data this))
	    (i (->fixnum offset)))
	 (unless (js-totest noassert) (check-offset data offset i 8 "read"))
	 (let ((buf (make-u8vector 8)))
	    (if le
		(begin
		   (u8vector-set! buf 0 (byte-ref data i))
		   (u8vector-set! buf 1 (byte-ref data (+fx i 1)))
		   (u8vector-set! buf 2 (byte-ref data (+fx i 2)))
		   (u8vector-set! buf 3 (byte-ref data (+fx i 3)))
		   (u8vector-set! buf 4 (byte-ref data (+fx i 4)))
		   (u8vector-set! buf 5 (byte-ref data (+fx i 5)))
		   (u8vector-set! buf 6 (byte-ref data (+fx i 6)))
		   (u8vector-set! buf 7 (byte-ref data (+fx i 7))))
		(begin
		   (u8vector-set! buf 7 (byte-ref data i))
		   (u8vector-set! buf 6 (byte-ref data (+fx i 1)))
		   (u8vector-set! buf 5 (byte-ref data (+fx i 2)))
		   (u8vector-set! buf 4 (byte-ref data (+fx i 3)))
		   (u8vector-set! buf 3 (byte-ref data (+fx i 4)))
		   (u8vector-set! buf 2 (byte-ref data (+fx i 5)))
		   (u8vector-set! buf 1 (byte-ref data (+fx i 6)))
		   (u8vector-set! buf 0 (byte-ref data (+fx i 7)))))
	    ($f64/u8vector-ref buf 0))))
   
   (define (write-float this value offset noassert le)
      (let ((data (get-data this))
	    (i (->fixnum offset)))
	 (unless (js-totest noassert) (check-offset data offset i 4 "write"))
	 (let ((buf (make-u8vector 4))
	       (val (->flonum (js-tonumber value %this))))
	    ($f32/u8vector-set! buf 0 val)
	    (if le
		(begin
		   (byte-set! data i (u8vector-ref buf 0))
		   (byte-set! data (+fx 1 i) (u8vector-ref buf 1))
		   (byte-set! data (+fx 2 i) (u8vector-ref buf 2))
		   (byte-set! data (+fx 3 i) (u8vector-ref buf 3)))
		(begin
		   (byte-set! data i (u8vector-ref buf 3))
		   (byte-set! data (+fx 1 i) (u8vector-ref buf 2))
		   (byte-set! data (+fx 2 i) (u8vector-ref buf 1))
		   (byte-set! data (+fx 3 i) (u8vector-ref buf 0)))))))
   
   (define (write-double this value offset noassert le)
      (let ((data (get-data this))
	    (i (->fixnum offset)))
	 (unless (js-totest noassert) (check-offset data offset i 8 "write"))
	 (let ((buf (make-u8vector 8))
	       (val (->flonum (js-tonumber value %this))))
	    ($f64/u8vector-set! buf 0 val)
	    (if le
		(begin
		   (byte-set! data (+fx 0 i) (u8vector-ref buf 0))
		   (byte-set! data (+fx 1 i) (u8vector-ref buf 1))
		   (byte-set! data (+fx 2 i) (u8vector-ref buf 2))
		   (byte-set! data (+fx 3 i) (u8vector-ref buf 3))
		   (byte-set! data (+fx 4 i) (u8vector-ref buf 4))
		   (byte-set! data (+fx 5 i) (u8vector-ref buf 5))
		   (byte-set! data (+fx 6 i) (u8vector-ref buf 6))
		   (byte-set! data (+fx 7 i) (u8vector-ref buf 7)))
		(begin
		   (byte-set! data (+fx 0 i) (u8vector-ref buf 7))
		   (byte-set! data (+fx 1 i) (u8vector-ref buf 6))
		   (byte-set! data (+fx 2 i) (u8vector-ref buf 5))
		   (byte-set! data (+fx 3 i) (u8vector-ref buf 4))
		   (byte-set! data (+fx 4 i) (u8vector-ref buf 3))
		   (byte-set! data (+fx 5 i) (u8vector-ref buf 2))
		   (byte-set! data (+fx 6 i) (u8vector-ref buf 1))
		   (byte-set! data (+fx 7 i) (u8vector-ref buf 0)))))))

   (define (js-buffer-fill this value start end)
      (let* ((sdata (if (isa? this JsSlowBuffer)
			(with-access::JsSlowBuffer this (data) data)
			(with-access::JsTypedArray this (%data) %data)))
	     (slength (string-length sdata))
	     (start (if (eq? start (js-undefined))
			0
			(->fixnum start)))
	     (end (if (eq? end (js-undefined))
		      slength
		      (->fixnum end)))
	     (val (js-tointeger value %this)))
	 (cond
	    ((<fx end start)
	     (js-raise-range-error %this
		"sourceEnd < sourceStart" #f))
	    ((=fx end start)
	     0)
	    ((>=fx start slength)
	     (js-raise-range-error %this
		"sourceStart out of bounds" #f))
	    ((>fx end slength)
	     (js-raise-range-error %this
		"sourceEnd out of bounds" #f))
	    (else
	     (let ((v (integer->char
			 (uint8->fixnum
			    (fixnum->uint8
			       (if (flonum? val)
				   (flonum->fixnum val)
				   val))))))
		(let loop ((i start))
		   (when (<fx i end)
		      (string-int-set! sdata i (char->integer v))
		      (loop (+fx i 1))))
		(js-undefined))))))
   
   ;; binarySlice
   (js-bind! %this slowbuffer-proto (& "binarySlice")
      :value (js-make-function %this
		(lambda (this::JsSlowBuffer start end)
		   (with-access::JsSlowBuffer this (data)
		      (js-string->jsstring (8bits-encode-utf8 data start end))))
		(js-function-arity 2 0)
		(js-function-info :name "binarySlice" :len 2))
      :writable #t :enumerable #t :configurable #t :hidden-class #t)
   
   ;; utf8Slice
   (js-bind! %this slowbuffer-proto (& "utf8Slice")
      :value (js-make-function %this
		(lambda (this::JsSlowBuffer start end)
		   (with-access::JsSlowBuffer this (data)
		      (string-utf8-normalize-utf16 data start end)))
		(js-function-arity 2 0)
		(js-function-info :name "utf8Slice" :len 2))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)
   
   
   ;; asciiSlice
   (js-bind! %this slowbuffer-proto (& "asciiSlice")
      :value (js-make-function %this
		(lambda (this::JsSlowBuffer start end)
		   (with-access::JsSlowBuffer this (data)
		      (let* ((len (-fx (->fixnum end) (->fixnum start)))
			     (string (make-string len)))
			 (when (>fx len 0)
			    (blit-string-ascii-clamp! data start string 0 len))
			 (js-ascii->jsstring string))))
		(js-function-arity 2 0)
		(js-function-info :name "asciiSlice" :len 2))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   ;; base64Slice
   (js-bind! %this slowbuffer-proto (& "base64Slice")
      :value (js-make-function %this
		(lambda (this::JsSlowBuffer start end)
		   (with-access::JsSlowBuffer this (data)
		      (let ((ip (open-input-string! data start end))
			    (op (open-output-string)))
			 (base64-encode-port ip op 0)
			 (js-ascii->jsstring (close-output-port op)))))
		(js-function-arity 2 0)
		(js-function-info :name "base64Slice" :len 2))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   
   ;; ucs2Slice
   (js-bind! %this slowbuffer-proto (& "ucs2Slice")
      :value (js-make-function %this
		(lambda (this::JsSlowBuffer start end)
		   (with-access::JsSlowBuffer this (data)
		      (js-utf8->jsstring
			 (ucs2-string->utf8-string (string->ucs2-string data start end)))))
		(js-function-arity 2 0)
		(js-function-info :name "ucs2Slice" :len 2))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   ;; hexSlice
   (js-bind! %this slowbuffer-proto (& "hexSlice")
      :value (js-make-function %this
		(lambda (this::JsSlowBuffer start end)
		   (with-access::JsSlowBuffer this (data)
		      (js-ascii->jsstring (string-hex-extern data start end))))
		(js-function-arity 2 0)
		(js-function-info :name "hexSlice" :len 2))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   ;; latin1Slice
   (js-bind! %this slowbuffer-proto (& "latin1Slice")
      :value (js-make-function %this
		(lambda (this::JsSlowBuffer start end)
		   (with-access::JsSlowBuffer this (data)
		      (js-utf8->jsstring
			 (if (and (=fx start 0) (=fx end (string-length data)))
			     (iso-latin->utf8 data)
			     (iso-latin->utf8! (substring data start end))))))
		(js-function-arity 2 0)
		(js-function-info :name "latin1Slice" :len 2))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   ;; _charsWritten is described at
   ;; http://nodejs.org/api/buffer.html#buffer_buf_write_string_offset_length_encoding
   (js-bind! %this slowbuffer-proto (& "binaryWrite")
      :value (js-make-function %this
		(lambda (this::JsSlowBuffer string::obj offset length)
		   (if (js-jsstring? string)
		       (with-access::JsSlowBuffer this (data)
			  (let ((n (maxfx 0
				      (minfx (js-jsstring-lengthfx string)
					 (minfx length
					    (-fx (string-length data) offset))))))
			     (if (>fx n 0)
				 (let ((l (if (isa? string JsStringLiteral8BITS)
					      (blit-string-binary!
						 (js-jsstring->string string) 0 data offset n)
					      (blit-string-binary-decode!
						 (js-jsstring->string string) 0 data offset n))))
				    (js-put! js-slowbuffer (& "_charsWritten") l #t %this)
				    l)
				 n)))
		       (js-raise-type-error %this "not a string" string)))
		(js-function-arity 3 0)
		(js-function-info :name "binaryWrite" :len 3))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   (js-bind! %this slowbuffer-proto (& "utf8Write")
      :value (js-make-function %this
		(lambda (this::JsSlowBuffer string::obj offset length)
		   (if (js-jsstring? string)
		       (with-access::JsSlowBuffer this (data)
			  (let ((n (maxfx 0
				      (minfx (js-jsstring-lengthfx string)
					 (minfx length
					    (-fx (string-length data) offset))))))
			     (if (>fx n 0)
				 (multiple-value-bind (m c)
				    (blit-string-utf8!
				       (js-jsstring->string string) 0 data offset n)
				    (js-put! js-slowbuffer (& "_charsWritten") c #t %this)
				    m)
				 (begin
				    (js-put! js-slowbuffer (& "_charsWritten") n #t %this)
				    n))))
		       (js-raise-type-error %this "not a string" string)))
		(js-function-arity 3 0)
		(js-function-info :name "utf8Write" :len 3))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   (js-bind! %this slowbuffer-proto (& "asciiWrite")
      :value (js-make-function %this
		(lambda (this::JsSlowBuffer string::obj offset length)
		   (if (js-jsstring? string)
		       (with-access::JsSlowBuffer this (data)
			  (let ((n (maxfx 0
				      (minfx (js-jsstring-lengthfx string)
					 (minfx length
					    (-fx (string-length data) offset))))))
			     (if (>fx n 0)
				 (let ((l (blit-string-ascii-decode!
					     (js-jsstring->string string) 0 data offset n)))
				    (js-put! js-slowbuffer (& "_charsWritten") l #t %this)
				    l)
				 n)))
		       (js-raise-type-error %this "not a string" string)))
		(js-function-arity 3 0)
		(js-function-info :name "asciiWrite" :len 3))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   (js-bind! %this slowbuffer-proto (& "base64Write")
      :value (js-make-function %this
		(lambda (this::JsSlowBuffer string::obj offset length)
		   (if (js-jsstring? string)
		       (with-access::JsSlowBuffer this (data)
			  (let ((ip (open-input-string! (js-jsstring->string string)))
				(op (open-output-string)))
			     (base64-decode-port ip op #t)
			     (close-input-port ip)
			     (let* ((s (close-output-port op))
				    (n (maxfx 0
					  (minfx (string-length s)
					     (minfx length
						(-fx (string-length data) offset))))))
				(when (>fx n 0)
				   (blit-string! s 0 data offset n))
				(js-put! js-slowbuffer (& "_charsWritten") n #t %this)
				n)))
		       (js-raise-type-error %this "not a string" string)))
		(js-function-arity 3 0)
		(js-function-info :name "base64Write" :len 3))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   (js-bind! %this slowbuffer-proto (& "ucs2Write")
      :value (js-make-function %this
		(lambda (this::JsSlowBuffer string::obj offset length)
		   (if (js-jsstring? string)
		       (with-access::JsSlowBuffer this (data)
			  (let* ((s (utf8-string->ucs2-string (js-jsstring->string string)))
				 (n (maxfx 0
				       (minfx (*fx 2 (ucs2-string-length s))
					  (minfx length
					     (-fx (string-length data) offset))))))
			     (if (>fx n 0)
				 (let ((l (blit-string-ucs2! s 0 data offset n)))
				    (js-put! js-slowbuffer (& "_charsWritten") (/fx l 2)
				       #t %this)
				    l)
				 0)))
		       (js-raise-type-error %this "not a string" string)))
		(js-function-arity 3 0)
		(js-function-info :name "ucs2Write" :len 3))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   (js-bind! %this slowbuffer-proto (& "hexWrite")
      :value (js-make-function %this
		(lambda (this::JsSlowBuffer string::obj offset length)
		   (if (js-jsstring? string)
		       (with-access::JsSlowBuffer this (data)
			  (let* ((s (string-hex-intern (js-jsstring->string string)))
				 (n (maxfx 0
				       (minfx (*fx 2 (string-length s))
					  (minfx length
					     (-fx (string-length data) offset))))))
			     (if (>fx n 0)
				 (begin
				    (blit-string! s 0 data offset n)
				    (js-put! js-slowbuffer (& "_charsWritten") (*fx n 2) #t %this)
				    n)
				 0)))
		       (js-raise-type-error %this "not a string" string)))
		(js-function-arity 3 0)
		(js-function-info :name "hexWrite" :len 3))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   (js-bind! %this slowbuffer-proto (& "latin1Write")
      :value (js-make-function %this
		(lambda (this::JsSlowBuffer string::obj offset length)
		   (if (js-jsstring? string)
		       (with-access::JsSlowBuffer this (data)
			  (let* ((s (utf8->iso-latin (js-jsstring->string string)))
				 (n (maxfx 0
				       (minfx (string-length s)
					  (minfx length
					     (-fx (string-length data) offset))))))
			     (if (>fx n 0)
				 (begin
				    (blit-string! s 0 data offset n)
				    (js-put! js-slowbuffer (& "_charsWritten") n #t %this)
				    n)
				 0)))
		       (js-raise-type-error %this "not a string" string)))
		(js-function-arity 3 0)
		(js-function-info :name "latin1Write" :len 3))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   (js-bind! %this slowbuffer-proto (& "copy")
      :value (js-make-function %this
		(lambda (this target tstart sstart send)
		   (let* ((sdata (if (isa? this JsSlowBuffer)
				     (with-access::JsSlowBuffer this (data) data)
				     (with-access::JsTypedArray this (%data) %data)))
			  (tdata (if (isa? target JsSlowBuffer)
				     (with-access::JsSlowBuffer target (data) data)
				     (with-access::JsTypedArray target (%data) %data)))
			  (tlength (string-length tdata))
			  (slength (string-length sdata))
			  (tstart (if (eq? tstart (js-undefined))
				      0
				      (->fixnum tstart)))
			  (sstart (if (eq? sstart (js-undefined))
				      0
				      (->fixnum sstart)))
			  (send (if (eq? send (js-undefined))
				    slength
				    (->fixnum send))))
		      (cond
			 ((<fx send sstart)
			  (js-raise-range-error %this
			     "sourceEnd < sourceStart" #f))
			 ((=fx send sstart)
			  0)
			 ((>=fx tstart tlength)
			  (js-raise-range-error %this
			     "targetStart out of bounds" #f))
			 ((>=fx sstart slength)
			  (js-raise-range-error %this
			     "sourceStart out of bounds" #f))
			 ((>fx send slength)
			  (js-raise-range-error %this
			     "sourceEnd out of bounds" #f))
			 (else
			  (let ((tocopy (minfx
					   (minfx (-fx send sstart)
					      (-fx tlength tstart))
					   (-fx slength sstart))))
			     (when (>fx tocopy 0)
				(blit-string! sdata sstart tdata tstart tocopy))
			     tocopy)))))
		(js-function-arity 4 0)
		(js-function-info :name "copy" :len 4))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   (js-bind! %this slowbuffer-proto (& "fill")
      :value (js-make-function %this
		js-buffer-fill
		(js-function-arity 3 0)
		(js-function-info :name "fill" :len 3))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   (js-bind! %this slowbuffer-proto (& "readFloatLE")
      :value (js-make-function %this
		(lambda (this offset noassert)
		   (read-float this offset noassert #t))
		(js-function-arity 2 0)
		(js-function-info :name "readFloatLE" :len 2))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   (js-bind! %this slowbuffer-proto (& "readFloatBE")
      :value (js-make-function %this
		(lambda (this offset noassert)
		   (read-float this offset noassert #f))
		(js-function-arity 2 0)
		(js-function-info :name "readFloatBE" :len 2))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   (js-bind! %this slowbuffer-proto (& "readDoubleLE")
      :value (js-make-function %this
		(lambda (this offset noassert)
		   (read-double this offset noassert #t))
		(js-function-arity 2 0)
		(js-function-info :name "readDoubleLE" :len 2))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   (js-bind! %this slowbuffer-proto (& "readDoubleBE")
      :value (js-make-function %this
		(lambda (this offset noassert)
		   (read-double this offset noassert #f))
		(js-function-arity 2 0)
		(js-function-info :name "readDoubleBE" :len 2))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)
   
   (js-bind! %this slowbuffer-proto (& "writeFloatLE")
      :value (js-make-function %this
		(lambda (this value offset noassert)
		   (write-float this value offset noassert #t))
		(js-function-arity 3 0)
		(js-function-info :name "writeFloatLE" :len 3))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   (js-bind! %this slowbuffer-proto (& "writeFloatBE")
      :value (js-make-function %this
		(lambda (this value offset noassert)
		   (write-float this value offset noassert #f))
		(js-function-arity 3 0)
		(js-function-info :name "writeFloatBE" :len 3))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   (js-bind! %this slowbuffer-proto (& "writeDoubleLE")
      :value (js-make-function %this
		(lambda (this value offset noassert)
		   (write-double this value offset noassert #t))
		(js-function-arity 3 0)
		(js-function-info :name "writeDoubleLE" :len 3))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   (js-bind! %this slowbuffer-proto (& "writeDoubleBE")
      :value (js-make-function %this
		(lambda (this value offset noassert)
		   (write-double this value offset noassert #f))
		(js-function-arity 3 0)
		(js-function-info :name "writeDoubleBE" :len 3))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)
   
   (js-bind! %this js-slowbuffer (& "byteLength")
      :value (js-make-function %this
		(lambda (this string encoding)
		   (if (eq? encoding (js-undefined))
		       (js-jsstring-lengthfx string)
		       (let ((enc (js-tostring encoding %this)))
			  (cond
			     ((or (string-ci=? enc "utf8")
				  (string-ci=? enc "utf-8"))
			      (js-jsstring-lengthfx string))
			     ((or (string-ci=? enc "ucs2")
				  (string-ci=? enc "ucs-2")
				  (string-ci=? enc "utf16le")
				  (string-ci=? enc "utf-16le"))
			      (*fx (utf8-codeunit-length (js-jsstring->string string)) 2))
			     ((string-ci=? enc "hex")
			      (/fx (utf8-codeunit-length (js-jsstring->string string)) 2))
			     ((string-ci=? enc "base64")
			      (string-length (base64-decode (js-jsstring->string string) #t)))
			     ((string=? enc "latin1")
			      (string-length (js-jsstring->string string)))
			     ((or (string-ci=? enc "ascii")
				  (string=? enc "buffer"))
			      (utf8-string-length (js-jsstring->string string)))
			     ((string=? enc "binary")
			      (if (isa? string JsStringLiteral8BITS)
				  (string-length (js-jsstring->string string))
				  (utf8-string-length (js-jsstring->string string))))
			     (else
			      (utf8-string-length (js-jsstring->string string)))))))
		(js-function-arity 2 0)
		(js-function-info :name "byteLength" :len 2))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   (js-bind! %this js-slowbuffer (& "makeFastBuffer")
      :value (js-make-function %this
		(lambda (this sbuf buf offset len)
		   ;; this function is called by the JavaScript Buffer
		   ;; constructor, which is defined in the buffer.js module
		   (with-access::JsSlowBuffer sbuf (data)
		      (with-access::JsTypedArray buf (buffer length byteoffset %data)
			 (cond
			    ((< offset 0)
			     (js-raise-range-error %this "wrong offset (~s)" offset))
			    ((> offset (string-length data))
			     (js-raise-range-error %this "wrong offset (~s)" offset))
			    ((> len (string-length data))
			     (js-raise-range-error %this "wrong length (~s)" len)))
			 (set! %data data)
			 (set! length (fixnum->uint32 (->fixnum len)))
			 (set! byteoffset (fixnum->uint32 (->fixnum offset)))
			 ;; we can now override the length field of the fast buffer
			 ;; as we know it is in range
			 (js-put! buf (& "length") (->fixnum len) #f %this)
			 (set! buffer sbuf))))
		(js-function-arity 4 0)
		(js-function-info :name "makeFastBuffer" :len 4))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   (js-bind! %this js-slowbuffer (& "alloc")
      :value (js-make-function %this 
		(lambda (this size fill encoding)
		   (with-access::JsGlobalObject %this (js-buffer-proto)
		      (let* ((len (uint32->fixnum (js-touint32 size %this)))
			     (str (make-string len
				     (if (fixnum? fill)
					 (integer->char fill)
					 0)))
			     (buf (js-string->jsfastbuffer str %this)))
			 (unless (or (eq? fill (js-undefined)) (fixnum? fill))
			    (js-buffer-fill buf fill 0 len))
			 buf)))
		(js-function-arity 3 0)
		(js-function-info :name "alloc" :len 3))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)
      
   (js-bind! %this js-slowbuffer (& "from")
      :value (js-make-function %this 
		(lambda (this obj offset-or-enc len)
		   (js-obj->jsfastbuffer this obj offset-or-enc len %this))
		(js-function-arity 3 0)
		(js-function-info :name "from" :len 3))
      :writable #t :enumerable #t :configurable #t :hidden-class #f)

   js-slowbuffer)
   
;*---------------------------------------------------------------------*/
;*    ->fixnum ...                                                     */
;*---------------------------------------------------------------------*/
(define (->fixnum r)
   (cond
      ((fixnum? r) r)
      ((flonum? r) (flonum->fixnum r))
      ((elong? r) (elong->fixnum r))
      ((llong? r) (llong->fixnum r))
      (else (error "->fixnum" (format "Illegal number (~a)" (typeof r)) r))))

;*---------------------------------------------------------------------*/
;*    ->flonum ...                                                     */
;*---------------------------------------------------------------------*/
(define (->flonum r)
   (cond
      ((flonum? r) r)
      ((fixnum? r) (fixnum->flonum r))
      ((elong? r) (elong->flonum r))
      ((llong? r) (llong->flonum r))
      (else (error "->flonum" (format "Illegal number (~a)" (typeof r)) r))))

;*---------------------------------------------------------------------*/
;*    process-buffer ...                                               */
;*---------------------------------------------------------------------*/
(define (process-buffer %this::JsGlobalObject slowbuffer)
   (js-alist->jsobject
      `((SlowBuffer . ,slowbuffer))
      %this))

;*---------------------------------------------------------------------*/
;*    SLAB-SIZE ...                                                    */
;*---------------------------------------------------------------------*/
(define (SLAB-SIZE) (*fx 1024 1024))

;*---------------------------------------------------------------------*/
;*    make-slab-allocator ...                                          */
;*---------------------------------------------------------------------*/
(define (make-slab-allocator %this::JsGlobalObject js-slowbuffer)
   (instantiate::Slab
      (%this %this)
      (js-slowbuffer js-slowbuffer)))

;*---------------------------------------------------------------------*/
;*    roundup ...                                                      */
;*---------------------------------------------------------------------*/
(define (roundup a b)
   (let ((r (remainderfx a b)))
      (if (=fx r 0)
	  a
	  (-fx (+fx a b) r))))

;*---------------------------------------------------------------------*/
;*    slab-allocate ...                                                */
;*    -------------------------------------------------------------    */
;*    See src/slab_allocator.cc in nodejs                              */
;*---------------------------------------------------------------------*/
(define (slab-allocate slab obj size)
   (with-trace 'nodejs-slab "slab-allocate"
      (trace-item "obj=" (typeof obj) " size=" size)
      (with-access::Slab slab (%this js-slowbuffer slowbuffer slice
				 offset lastoffset)
	 (if (not slowbuffer)
	     (let* ((rsize (roundup (max size (SLAB-SIZE)) 8192))
		    (buf (js-new1 %this js-slowbuffer rsize)))
		(set! slowbuffer buf)
		(set! slice (js-get buf (& "slice") %this))
		(set! offset size)
		(with-access::JsSlowBuffer buf (data)
		   (trace-item "INIT size=" size " rsize=" rsize
		      " -> free offset=" offset " lastoffset=" lastoffset
		      " data=" (string-length data))
		   (values buf data 0)))
	     (with-access::JsSlowBuffer slowbuffer (data)
		;; slowbuffer data are implemented as strings
		(let* ((sz (string-length data)))
		   (if (>fx (+ offset size) sz)
		       ;; not enough space, new buffer required
		       (let* ((rsize (roundup (max size sz) 16))
			      (buf::JsSlowBuffer (js-new1 %this js-slowbuffer rsize)))
			  (set! slowbuffer buf)
			  (set! offset size)
			  (trace-item
			     "NEW size=" size " size_=" rsize
			     " -> next offset=" offset)
			  (with-access::JsSlowBuffer buf (data)
			     (values buf data 0)))
		       (let ((loff offset))
			  (set! offset (+fx offset size))
			  (set! lastoffset loff)
			  (trace-item "FILL size=" size " sz=" sz
			     " -> next offset=" offset)
			  (values slowbuffer data loff)))))))))

;*---------------------------------------------------------------------*/
;*    slab-shrink! ...                                                 */
;*---------------------------------------------------------------------*/
(define (slab-shrink! slab buf off size)
   (with-trace 'nodejs-slab "slab-shrink!"
      (with-access::Slab slab (lastoffset slowbuffer %this offset)
	 (trace-item "off=" off " size=" size
	    " old-offset=" offset " last-offset=" lastoffset)
	 (when (and (eq? slowbuffer buf) (=fx off lastoffset))
	    (set! offset (+fx lastoffset (roundup size 16))))
	 (trace-item "->offset=" offset)
	 (if (>fx size 0)
	     (with-access::Slab slab (js-slowbuffer)
		(js-new %this js-slowbuffer buf))
	     (js-undefined)))))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)

