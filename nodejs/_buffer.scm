;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_buffer.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Aug 30 06:52:06 2014                          */
;*    Last change :  Sun Aug 31 18:38:07 2014 (serrano)                */
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
   
   (export  (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)
	    (process-buffer ::JsGlobalObject ::JsObject)
	    (make-slowbuffer ::JsGlobalObject)
	    (make-slab-allocate ::JsGlobalObject ::JsObject)))

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
   
   (define (slowbuffer-constr this len)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((this (instantiate::JsArrayBuffer
			(__proto__ slowbuffer-proto)
			(vec (make-string len)))))
	    ;; length
	    (js-bind! %this this 'length
	       :value len
	       :configurable #f
	       :writable #f
	       :enumerable #t)
	    this)))
   
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

1   (js-put! slowbuffer-proto 'asciiWrite
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
;*    make-slab-allocate ...                                           */
;*---------------------------------------------------------------------*/
(define (make-slab-allocate %this::JsGlobalObject slowbuffer)
   (lambda (obj size)
      (let ((slowbuf::JsArrayBuffer (js-new1 %this slowbuffer size))
	    (offset 0))
	 (with-access::JsArrayBuffer slowbuf (vec)
	    (values slowbuf vec offset)))))
