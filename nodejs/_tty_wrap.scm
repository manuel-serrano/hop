;*=====================================================================*/
;*    /tmp/HOPNEW/hop/nodejs/_tty_wrap.scm                             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Oct 19 07:19:20 2014                          */
;*    Last change :  Sun Feb 23 15:08:27 2020 (serrano)                */
;*    Copyright   :  2014-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Nodejs TTY bindings                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__tty-wrap

   (include "../hopscript/stringthread.sch")
   
   (library hopscript)

   (include "nodejs_types.sch")
   
   (import  __nodejs_uv
	    __nodejs_process
	    __nodejs__buffer
	    __nodejs__stream-wrap)

   (export (process-tty-wrap ::WorkerHopThread ::JsGlobalObject ::JsProcess
	      ::obj ::JsObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    process-tty-wrap ...                                             */
;*---------------------------------------------------------------------*/
(define (process-tty-wrap %worker %this process::JsProcess slab slowbuffer::JsObject)
   
   (define (create-tty-proto)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((obj (js-new0 %this js-object)))
	    (js-put! obj (& "close")
	       (js-make-function %this
		  (lambda (this cb)
		     (nodejs-close %worker %this process this cb))
		  (js-function-arity 1 0)
		  (js-function-info :name "close" :len 1))
	       #f %this)
	    
	    (js-put! obj (& "unref")
	       (js-make-function %this
		  (lambda (this)
		     (with-access::JsHandle this (handle)
			(nodejs-unref handle %worker)))
		  (js-function-arity 0 0)
		  (js-function-info :name "unref" :len 0))
	       #f %this)
	    
	    (js-put! obj (& "readStart")
	       (js-make-function %this
		  (lambda (this)
		     (stream-read-start %worker %this process slab this))
		  (js-function-arity 0 0)
		  (js-function-info :name "readStart" :len 0))
	       #f %this)
	    
	    (js-put! obj (& "readStop")
	       (js-make-function %this
		  (lambda (this)
		     (stream-read-stop %worker %this this))
		  (js-function-arity 0 0)
		  (js-function-info :name "readStop" :len 0))
	       #f %this)
	    
	    (js-put! obj (& "writeBuffer")
	       (js-make-function %this
		  (lambda (this buffer)
		     (stream-write-buffer %worker %this process this buffer))
		  (js-function-arity 1 0)
		  (js-function-info :name "writeBuffer" :len 1))
	       #f %this)
	    
	    (js-put! obj (& "writeAsciiString")
	       (js-make-function %this
		  (lambda (this string handle)
		     (stream-write-string %worker %this process this
			(js-jsstring->string string) 0 (js-jsstring-lengthfx string)
			"ascii" #f handle))
		  (js-function-arity 2 0)
		  (js-function-info :name "writeAsciiString" :len 2))
	       #f %this)
	    
	    (js-put! obj (& "writeUtf8String")
	       (js-make-function %this
		  (lambda (this string handle)
		     (stream-write-string %worker %this process this
			(js-jsstring->string string) 0 (js-jsstring-lengthfx string)
			"utf8" #f handle))
		  (js-function-arity 2 0)
		  (js-function-info :name "writeUtf8String" :len 2))
	       #f %this)
	    
	    (js-put! obj (& "writeUcs2String")
	       (js-make-function %this
		  (lambda (this string handle)
		     (let* ((ucs2string (utf8-string->ucs2-string string))
			    (buffer (ucs2-string->buffer ucs2string)))
			(stream-write-string %worker %this process this
			   (js-jsstring->string string) 0 (js-jsstring-lengthfx string)
			   "ascii" #f handle)))
		  (js-function-arity 2 0)
		  (js-function-info :name "writeUcs2String" :len 2))
	       #f %this)
	    
	    (js-put! obj (& "getWindowSize")
	       (js-make-function %this
		  (lambda (this)
		     (with-access::JsHandle this (handle)
			(nodejs-tty-get-window-size %worker %this handle)))
		  (js-function-arity 0 0)
		  (js-function-info :name "getWindowSize" :len 0))
	       #f %this)
	    
	    (js-put! obj (& "setRawMode")
	       (js-make-function %this
		  (lambda (this)
		     (with-access::JsHandle this (handle)
			(nodejs-tty-set-raw-mode handle)))
		  (js-function-arity 0 0)
		  (js-function-info :name "setRawMode" :len 0))
	       #f %this)
	    
	    obj)))
   
   (define (get-tty-proto)
      (with-access::JsProcess process (tty-proto)
	 (unless tty-proto
	    (set! tty-proto (create-tty-proto)))
	 tty-proto))
   
   (define (tty-wrap hdl)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((obj (instantiateJsHandle
		       (handle hdl)
		       (__proto__ (get-tty-proto))
		       (cmap (instantiate::JsConstructMap))
		       (elements ($create-vector 1)))))
	    (js-bind! %this obj (& "fd")
	       :get (js-make-function %this
		       (lambda (this)
			  (nodejs-stream-fd %worker hdl))
		       (js-function-arity 0 0)
		       (js-function-info :name "GetFD" :len 0))
	       :writable #f :configurable #f)
	    obj)))
   
   (define (TTY this fd readable)
      (tty-wrap (nodejs-tty-handle %worker fd readable)))

   (set! __js_strings (&init!))
   (with-access::JsGlobalObject %this (js-object)
      (with-access::JsProcess process (js-tty)
	 (let ((obj (js-new %this js-object)))
	    (set! js-tty
	       (js-make-function %this TTY
		  (js-function-arity TTY)
		  (js-function-info :name "TTY" :len 0)
		  :alloc (lambda (%this o) #unspecified)
		  :prototype (get-tty-proto)))
	    (js-put! obj (& "isTTY")
	       (js-make-function %this
		  (lambda (this fd)
		     (nodejs-istty %worker %this fd))
		  (js-function-arity 1 0)
		  (js-function-info :name "isTTY" :len 1))
	       #f %this)
	    
	    (js-put! obj (& "guessHandleType")
	       (js-make-function %this
		  (lambda (this fd)
		     (nodejs-guess-handle-type
			%worker %this fd))
		  (js-function-arity 1 0)
		  (js-function-info :name "guessHandleType" :len 1))
	       #f %this)
	    
	    (js-put! obj (& "TTY") js-tty #f %this)
	    
	    obj))))
   

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)

