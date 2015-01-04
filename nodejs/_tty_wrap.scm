;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_tty_wrap.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Oct 19 07:19:20 2014                          */
;*    Last change :  Thu Jan  1 19:39:17 2015 (serrano)                */
;*    Copyright   :  2014-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Nodejs TTY bindings                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__tty-wrap

   (library hopscript)

   (import  __nodejs_uv
	    __nodejs_process
	    __nodejs__buffer
	    __nodejs__stream-wrap)

   (export (process-tty-wrap ::WorkerHopThread ::JsGlobalObject ::JsProcess
	      ::obj ::JsObject)))

;*---------------------------------------------------------------------*/
;*    process-tty-wrap ...                                             */
;*---------------------------------------------------------------------*/
(define (process-tty-wrap %worker %this process::JsProcess slab slowbuffer::JsObject)
   
   (define (not-implemented name)
      (js-make-function %this
	 (lambda (this . l)
	    (error "tty_wrap" "binding not implemented" name))
	 0 name))
   
   (define (create-tty-proto)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((obj (js-new %this js-object)))
	    
	    (js-put! obj 'close
	       (js-make-function %this
		  (lambda (this cb)
		     (nodejs-close %worker %this this cb))
		  1 "close")
	       #f %this)
	    
	    (js-put! obj 'unref
	       (js-make-function %this
		  (lambda (this)
		     (with-access::JsHandle this (handle)
			(nodejs-unref handle %worker)))
		  0 "unref")
	       #f %this)
	    
	    (js-put! obj 'readStart
	       (js-make-function %this
		  (lambda (this)
		     (stream-read-start %worker %this process slab this))
		  0 "readStart")
	       #f %this)
	    
	    (js-put! obj 'readStop
	       (js-make-function %this
		  (lambda (this)
		     (stream-read-stop %worker %this this))
		  0 "readStop")
	       #f %this)
	    
	    (js-put! obj 'writeBuffer
	       (js-make-function %this
		  (lambda (this buffer)
		     (stream-write-buffer %worker %this this buffer))
		  1 "writeBuffer")
	       #f %this)
	    
	    (js-put! obj 'writeAsciiString
	       (js-make-function %this
		  (lambda (this string handle)
		     (stream-write-string %worker %this this
			(js-string->string string) 0 (js-string-length string)
			"ascii" #f handle))
		  2 "writeAsciiString")
	       #f %this)
	    
	    (js-put! obj 'writeUtf8String
	       (js-make-function %this
		  (lambda (this string handle)
		     (stream-write-string %worker %this this
			(js-string->string string) 0 (js-string-length string)
			"utf8" #f handle))
		  2 "writeUtf8String")
	       #f %this)
	    
	    (js-put! obj 'writeUcs2String
	       (js-make-function %this
		  (lambda (this string handle)
		     (let* ((ucs2string (utf8-string->ucs2-string string))
			    (buffer (ucs2-string->buffer ucs2string)))
			(stream-write-string %worker %this this
			   (js-string->string string) 0 (js-string-length string)
			   "ascii" #f handle)))
		  2 "writeUcs2String")
	       #f %this)
	    
	    (js-put! obj 'getWindowSize
	       (js-make-function %this
		  (lambda (this)
		     (with-access::JsHandle this (handle)
			(nodejs-tty-get-window-size %worker %this handle)))
		  0 "getWindowSize")
	       #f %this)
	    
	    (js-put! obj 'setRawMode
	       (js-make-function %this
		  (lambda (this)
		     (with-access::JsHandle this (handle)
			(nodejs-tty-set-raw-mode handle)))
		  0 "setRawMode")
	       #f %this)
	    
	    obj)))
   
   (define (get-tty-proto)
      (with-access::JsProcess process (tty-proto)
	 (unless tty-proto
	    (set! tty-proto (create-tty-proto)))
	 tty-proto))
   
   (define (tty-wrap hdl)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((obj (instantiate::JsHandle
		       (handle hdl)
		       (__proto__ (get-tty-proto)))))
	    (js-bind! %this obj 'fd
	       :get (js-make-function %this
		       (lambda (this)
			  (nodejs-stream-fd hdl))
		       0 'GetFD)
	       :writable #f :configurable #f)
	    obj)))
   
   (define (TTY this fd readable)
      (tty-wrap (nodejs-tty-handle %worker fd readable)))
   
   (with-access::JsGlobalObject %this (js-object)
      (with-access::JsProcess process (js-tty)
	 (let ((obj (js-new %this js-object)))
	    (set! js-tty
	       (js-make-function %this TTY 0 "TTY"
		  :alloc (lambda (o) #unspecified)
		  :prototype (get-tty-proto)
		  :construct TTY))
	    (js-put! obj 'isTTY
	       (js-make-function %this
		  (lambda (this fd)
		     (nodejs-istty %worker %this fd))
		  1 'isTTY)
	       #f %this)
	    
	    (js-put! obj 'guessHandleType
	       (js-make-function %this
		  (lambda (this fd)
		     (nodejs-guess-handle-type
			%worker %this fd))
		  1 'guessHandleType)
	       #f %this)
	    
	    (js-put! obj 'TTY js-tty #f %this)
	    
	    obj))))
   

