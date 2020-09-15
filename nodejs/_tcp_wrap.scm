;*=====================================================================*/
;*    /tmp/HOPNEW/hop/nodejs/_tcp_wrap.scm                             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Oct 19 07:19:20 2014                          */
;*    Last change :  Sun Feb 23 15:53:49 2020 (serrano)                */
;*    Copyright   :  2014-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Nodejs TCP bindings                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__tcp-wrap

   (include "../hopscript/stringthread.sch")
   
   (library hopscript)

   (import  __nodejs_uv
	    __nodejs_process
	    __nodejs__buffer
	    __nodejs__stream-wrap)

   (include "nodejs_async.sch" "nodejs_types.sch")
   
   (export (process-tcp-wrap ::WorkerHopThread ::JsGlobalObject 
	      ::JsProcess ::obj ::JsObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    process-tcp-wrap ...                                             */
;*---------------------------------------------------------------------*/
(define (process-tcp-wrap %worker %this process::JsProcess slab slowbuffer::JsObject)
   
   (define (->fixnum n)
      (cond
	 ((fixnum? n) n)
	 ((flonum? n) (flonum->fixnum n))
	 (else 0)))
   
   (define reqs '())
   
   (define (connect family)
      (with-access::JsGlobalObject %this (js-object)
	 (lambda (this host port callback)
	    (with-access::JsHandle this (handle)
	       (let ((req (js-new %this js-object)))
		  (set! reqs (cons req reqs))
		  (nodejs-tcp-connect %worker %this handle host
		     (->fixnum (js-tointeger port %this)) family
		     (lambda (status handle)
			(when (<fx status 0)
			   (js-put! process (& "_errno")
			      (nodejs-err-name status) #f %this))
			(let ((oncomp (js-get req (& "oncomplete") %this)))
			   (!js-callback5 "connect" %worker %this oncomp
			      req status this req #t #t)
			   (set! reqs (remq req reqs))
			   (js-undefined))))
		  req)))))

   (define (create-tcp-proto)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((obj (js-new %this js-object)))
	    
	    (js-put! obj (& "close")
	       (js-make-function %this
		  (lambda (this cb)
		     (nodejs-close %worker %this process this cb))
		  (js-function-arity 1 0)
		  (js-function-info :name "close" :len 1))
	       #f %this)
	    
	    (js-put! obj (& "ref")
	       (js-make-function %this
		  (lambda (this)
		     (with-access::JsHandle this (handle)
			(nodejs-ref handle %worker)))
		  (js-function-arity 0 0)
		  (js-function-info :name "ref" :len 0))
	       #f %this)
	    
	    (js-put! obj (& "unref")
	       (js-make-function %this
		  (lambda (this)
		     (with-access::JsHandle this (handle)
			(nodejs-unref handle %worker)))
		  (js-function-arity 0 0)
		  (js-function-info :name "unref" :len 0))
	       #f %this)
	    
	    (js-put! obj (& "connect")
	       (js-make-function %this
		  (connect 4)
		  (js-function-arity 3 0)
		  (js-function-info :name "connect" :len 3))
	       #f %this)
	    
	    (js-put! obj (& "connect6")
	       (js-make-function %this
		  (connect 6)
		  (js-function-arity 3 0)
		  (js-function-info :name "connect" :len 3))
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
	    
	    (js-put! obj (& "setNoDelay")
	       (js-make-function %this
		  (lambda (this val)
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-nodelay handle (js-totest val))))
		  (js-function-arity 1 0)
		  (js-function-info :name "setNoDelay" :len 1))
	       #f %this)
	    
	    (js-put! obj (& "setKeepAlive")
	       (js-make-function %this
		  (lambda (this val tmt)
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-keepalive handle (js-totest val)
			   (->fixnum (js-tointeger tmt %this)))))
		  (js-function-arity 2 0)
		  (js-function-info :name "setKeepAlive" :len 2))
	       #f %this)
	    
	    (js-put! obj (& "setSimultaneousAccepts")
	       (js-make-function %this
		  (lambda (this val)
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-simultaneous-accepts
			   handle (js-totest val))))
		  (js-function-arity 1 0)
		  (js-function-info :name "setSimultaneousAccepts" :len 1))
	       #f %this)
	    
	    (js-put! obj (& "getsockname")
	       (js-make-function %this
		  (lambda (this val)
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-getsockname %this handle)))
		  (js-function-arity 1 0)
		  (js-function-info :name "getsockname" :len 1))
	       #f %this)
	    
	    (js-put! obj (& "getpeername")
	       (js-make-function %this
		  (lambda (this val)
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-getpeername %this handle)))
		  (js-function-arity 1 0)
		  (js-function-info :name "getpeername" :len 1))
	       #f %this)
	    
	    (js-put! obj (& "shutdown")
	       (js-make-function %this
		  (lambda (this)
		     (stream-shutdown %worker %this process this))
		  (js-function-arity 0 0)
		  (js-function-info :name "shutdown" :len 1))
	       #f %this)
	    
	    (js-put! obj (& "open")
	       (js-make-function %this
		  (lambda (this fd)
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-open %worker %this handle fd)))
		  (js-function-arity 1 0)
		  (js-function-info :name "open" :len 1))
	       #f %this)
	    
	    (js-put! obj (& "bind")
	       (js-make-function %this
		  (lambda (this addr port)
		     (with-access::JsHandle this (handle)
			(let ((p (->fixnum (js-tointeger port %this))))
			   (nodejs-tcp-bind %this process handle addr p 4))))
		  (js-function-arity 2 0)
		  (js-function-info :name "bind" :len 2))
	       #f %this)
	    
	    (js-put! obj (& "bind6")
	       (js-make-function %this
		  (lambda (this addr port)
		     (with-access::JsHandle this (handle)
			(let ((p (->fixnum (js-tointeger port %this))))
			   (nodejs-tcp-bind %this process handle addr p 6))))
		  (js-function-arity 2 0)
		  (js-function-info :name "bind6" :len 2))
	       #f %this)
	    
	    (js-put! obj (& "listen")
	       (js-make-function %this
		  (lambda (this backlog)
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-listen %worker %this process this handle
			   (->fixnum (js-tointeger backlog %this))
			   tcp-wrap)))
		  (js-function-arity 1 0)
		  (js-function-info :name "listen" :len 1))
	       #f %this)
	    
	    obj)))
   
   (define (get-tcp-proto)
      (with-access::JsProcess process (tcp-proto)
	 (unless tcp-proto
	    (set! tcp-proto (create-tcp-proto)))
	 tcp-proto))

   (define (tcp-wrap hdl)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((obj (instantiateJsHandle
		       (handle hdl)
		       (cmap (instantiate::JsConstructMap))
		       (__proto__ (get-tcp-proto))
		       (elements ($create-vector 2)))))
	    (js-bind! %this obj (& "fd")
	       :get (js-make-function %this
		       (lambda (this)
			  (nodejs-stream-fd %worker hdl))
		       (js-function-arity 0 0)
		       (js-function-info :name "getGD" :len 0))
	       :writable #f :configurable #f)
	    (js-put! obj (& "writeQueueSize")
	       (nodejs-stream-write-queue-size hdl) #f %this)
	    obj)))
   
   (define (TCP this)
      (tcp-wrap (nodejs-tcp-handle %worker)))

   (set! __js_strings (&init!))
   
   (with-access::JsGlobalObject %this (js-object)
      (with-access::JsProcess process (js-tcp)
	 (let ((obj (js-new %this js-object)))
	    (set! js-tcp
	       (js-make-function %this TCP
		  (js-function-arity 0 0)
		  (js-function-info :name "TCP" :len 0)
		  :alloc (lambda (%this o) #unspecified)
		  :prototype (get-tcp-proto)))
	    (js-put! obj (& "TCP") js-tcp #f %this)
	    obj))))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)

