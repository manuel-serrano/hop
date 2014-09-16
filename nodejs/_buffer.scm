;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_buffer.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Aug 30 06:52:06 2014                          */
;*    Last change :  Sun Sep 14 12:59:04 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Native native bindings                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__buffer

   (include "nodejs_debug.sch")
   
   (library hopscript)

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
   
   (export  (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)
	    (process-buffer ::JsGlobalObject ::JsObject)
	    (make-slowbuffer ::JsGlobalObject)
	    (make-slab-allocator ::JsGlobalObject ::JsObject)
	    (slab-allocate ::object ::obj ::long)
	    (slab-shrink! ::obj ::long ::long)))

;*---------------------------------------------------------------------*/
;*    hopscript ...                                                    */
;*    -------------------------------------------------------------    */
;*    Nodejs Buffers are special objects which cannot be implemented   */
;*    in JavaScript. The __nodejs__buffer module backs up the          */
;*    __nodejs_buffer module which contains the Nodejs JavaScript      */
;*    part of the implementation. This current module modify the       */
;*    allocation function of the JavaScript Buffer constructor in      */
;*    order not to allocate a JsObject but a JsTypedArray.             */
;*---------------------------------------------------------------------*/
(define (hopscript %this this %scope %module)
   (let* ((mod (nodejs-init-core "buffer" (@ hopscript __nodejs_buffer) %this))
	  (exp (js-get mod 'exports %this))
	  (buf (js-get exp 'Buffer %this))
	  (proto (js-get buf 'prototype %this)))
      (with-access::JsFunction buf (alloc)
	 (set! alloc
	    (lambda (ctor)
	       ;; see makeFastBuffer below for the complete JavaScript
	       ;; land JsTypedArray initialization
	       (instantiate::JsTypedArray
		  (cmap #f)
		  (bpe #u32:1)
		  (vref (lambda (buf o)
			   (char->integer (string-ref buf o))))
		  (vset (lambda (buf o v)
			   (let ((val (js-tointeger v %this)))
			      (string-set! buf o
				 (integer->char
				    (if (flonum? val)
					(flonum->fixnum val)
					val))))))
		  (__proto__ proto)))))
      mod))

;*---------------------------------------------------------------------*/
;*    utf8-substring-length ...                                        */
;*    -------------------------------------------------------------    */
;*    Return the number of characters of an UTF8 string.               */
;*---------------------------------------------------------------------*/
(define (utf8-substring-length str len)
   (let ((len (minfx (string-length str) len)))
      (let loop ((r 0)
		 (l 0))
	 (if (=fx r len)
	     l
	     (loop (+fx r (utf8-char-size (string-ref str r))) (+fx l 1))))))

;*---------------------------------------------------------------------*/
;*    string-force-ascii! ...                                          */
;*---------------------------------------------------------------------*/
(define (string-force-ascii! str)
   (let ((len (string-length str)))
      (let loop ((i 0))
	 (if (=fx i len)
	     str
	     (let ((n (char->integer (string-ref str i))))
		(when (<fx n 0)
		   (string-set! str i (integer->char (bit-and n #x7f))))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    make-slowbuffer ...                                              */
;*---------------------------------------------------------------------*/
(define (make-slowbuffer %this::JsGlobalObject)
   
   (define (not-implemented name)
      (js-make-function %this
	 (lambda (this . l)
	    (error "buffer" "buffer binding not implemented" name))
	 0 name))
   
   (define slowbuffer-proto
      (with-access::JsGlobalObject %this (js-object)
	 (js-new %this js-object)))
   
   (define (slowbuffer-constr this a0)
      (let loop ((a0 a0))
	 (cond
	    ((integer? a0)
	     (with-access::JsGlobalObject %this (js-object)
		(let ((this (instantiate::JsArrayBuffer
			       (__proto__ slowbuffer-proto)
			       (vec (make-string a0)))))
		   ;; length
		   (js-bind! %this this 'length
		      :value a0
		      :configurable #f
		      :writable #f
		      :enumerable #t)
		   this)))
	    ((string? a0)
	     (with-access::JsGlobalObject %this (js-object)
		(let ((this (instantiate::JsArrayBuffer
			       (__proto__ slowbuffer-proto)
			       (vec a0))))
		   ;; length
		   (js-bind! %this this 'length
		      :value (string-length a0)
		      :configurable #f
		      :writable #f
		      :enumerable #t)
		   this)))
	    ((isa? a0 JsArrayBuffer)
	     (with-access::JsArrayBuffer a0 (vec)
		(loop vec)))
	    (else
	     (error "buffer" "Illegal constructor call" a0)))))
   
   (define js-slowbuffer
      (js-make-function %this slowbuffer-constr 1 "SlowBuffer"
	 :alloc (lambda (o) #unspecified)
	 :construct slowbuffer-constr
	 :prototype slowbuffer-proto))

   ;; asciiSlice
   (js-put! slowbuffer-proto 'asciiSlice
      (js-make-function %this
	 (lambda (this::JsArrayBuffer start end)
	    (with-access::JsArrayBuffer this (vec)
	       (let* ((len (-fx end start))
		      (string (make-string len)))
		  (blit-string! vec start string 0 len)
		  (string-force-ascii! string))))
	 3 "asciiSlice")
      #f %this)

   ;; utf8Slice
   (js-put! slowbuffer-proto 'utf8Slice
      (js-make-function %this
	 (lambda (this::JsArrayBuffer start end)
	    (with-access::JsArrayBuffer this (vec)
	       (let* ((len (-fx end start))
		      (string (make-string len)))
		  (blit-string! vec start string 0 len)
		  string)))
	 3 "utf8Slice")
      #f %this)
   
   ;; _charsWritten is described at
   ;; http://nodejs.org/api/buffer.html#buffer_buf_write_string_offset_length_encoding
   (js-put! slowbuffer-proto 'utf8Write
      (js-make-function %this
	 (lambda (this::JsArrayBuffer string::bstring offset length)
	    (js-put! js-slowbuffer '_charsWritten
	       (utf8-substring-length string length) #t %this)
	    (with-access::JsArrayBuffer this (vec)
	       (blit-string! string 0 vec offset length))
	    length)
	 1 "utf8Write")
      #f %this)

   (js-put! slowbuffer-proto 'asciiWrite
      (js-make-function %this
	 (lambda (this::JsArrayBuffer string::bstring offset length)
	    (js-put! js-slowbuffer '_charsWritten
	       (minfx length (string-length string)) #t %this)
	    (with-access::JsArrayBuffer this (vec)
	       (blit-string! string 0 vec offset length))
	    length)
	 1 "AsciiWrite")
      #f %this)
   
   (js-put! js-slowbuffer 'byteLength
      (js-make-function %this
	 (lambda (this string encoding)
	    (cond
	       ((or (eq? encoding (js-undefined))
		    (string=? encoding "utf8")
		    (string=? encoding "ascii"))
		(string-length string))
	       (else
		(error "buffer" "byteLength encoding not implemented"
		   encoding))))
	 1 "byteLength")
      #t %this)
   
   (js-put! js-slowbuffer 'makeFastBuffer
      (js-make-function %this
	 (lambda (this sbuf buf offset len)
	    ;; this function is called by the JavaScript Buffer constructor,
	    ;; which is defined in the buffer.js module
	    (with-access::JsArrayBuffer sbuf (vec)
	       (with-access::JsTypedArray buf (buffer length byteoffset %vec)
		  (set! %vec vec)
		  (set! length (fixnum->uint32 len))
		  (set! byteoffset (fixnum->uint32 offset))
		  (set! buffer sbuf))))
	 4 "makeFastBuffer")
      #t %this)
   
   (for-each (lambda (fun)
		(js-put! slowbuffer-proto fun (not-implemented fun) #f %this))
      '(hexSlice binarySlice base64Slice ucs2Slice
	hexWrite binaryWrite base64Write ucs2Write))

   js-slowbuffer)
   
   
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

(define count 0)
(define debug 1)

;*---------------------------------------------------------------------*/
;*    slab-allocate ...                                                */
;*    -------------------------------------------------------------    */
;*    See src/slab_allocator.cc in nodejs                              */
;*---------------------------------------------------------------------*/
(define (slab-allocate slab obj size)
   (set! count (+fx count 1))
   (with-access::Slab slab (%this js-slowbuffer slowbuffer slice
			      offset lastoffset)
      (if (not slowbuffer)
	  (let* ((rsize (roundup (max size (SLAB-SIZE)) 8192))
		 (buf (js-new1 %this js-slowbuffer rsize)))
	     (set! slowbuffer buf)
	     (set! slice (js-get buf 'slice %this))
	     (set! offset size)
	     (with-access::JsArrayBuffer buf (vec)
		(when (>fx debug 0)
		   (tprint "SLAB-ALLOC.1(" count ") size=" size
		      " size_=" rsize " -> offset=" offset))
		(values buf vec 0)))
	  (with-access::JsArrayBuffer slowbuffer (vec)
	     ;; slowbuffer vectors are implemented as strings
	     (let* ((sz (string-length vec)))
		(if (>fx (+ offset size) sz)
		    ;; not enough space, new buffer required
		    (let* ((rsize (roundup (max size sz) 16))
			   (buf::JsArrayBuffer (js-new1 %this js-slowbuffer rsize)))
		       (set! slowbuffer buf)
		       (set! offset size)
		       (when (>fx debug 0)
			  (tprint "SLAB-ALLOC.2(" count ") size=" size
			     " size_=" rsize " -> offset=" offset))
		       (with-access::JsArrayBuffer buf (vec)
			  (values buf vec 0)))
		    (let ((loff offset))
		       (set! offset (+fx offset size))
		       (set! lastoffset loff)
		       (when (>fx debug 0)
			  (tprint "SLAB-ALLOC.3(" count ") size=" size
			     " sz=" sz " -> offset=" offset))
		       (values slowbuffer vec loff))))))))

;*---------------------------------------------------------------------*/
;*    slab-shrink! ...                                                 */
;*---------------------------------------------------------------------*/
(define (slab-shrink! slab off size)
   (with-access::Slab slab (lastoffset js-slowbuffer slowbuffer %this offset)
      (when (=fx off lastoffset)
	 (set! offset (+fx lastoffset (roundup size 16))))
      (when (>fx debug 0)
	 (tprint "SLAB-SHRINK(" count ") size=" size " -> offset=" offset))
      (if (>fx size 0)
	  (js-new %this js-slowbuffer slowbuffer)
	  (js-undefined))))
