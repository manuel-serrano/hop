;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/nodejs/_tcp_wrap.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Oct 19 07:19:20 2014                          */
;*    Last change :  Wed Oct 25 17:28:38 2017 (serrano)                */
;*    Copyright   :  2014-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Nodejs TCP bindings                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__tcp-wrap

   (library hopscript)

   (import  __nodejs_uv
	    __nodejs_process
	    __nodejs__buffer
	    __nodejs__stream-wrap)

   (include "nodejs_async.sch" "nodejs_types.sch")
   
   (export (process-tcp-wrap ::WorkerHopThread ::JsGlobalObject 
	      ::JsProcess ::obj ::JsObject)))

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
			   (js-put! process '_errno
			      (nodejs-err-name status) #f %this))
			(let ((oncomp (js-get req 'oncomplete %this)))
			   (!js-callback5 "connect" %worker %this oncomp
			      req status this req #t #t)
			   (set! reqs (remq req reqs))
			   (js-undefined))))
		  req)))))

   (define (create-tcp-proto)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((obj (js-new %this js-object)))
	    
	    (js-put! obj 'close
	       (js-make-function %this
		  (lambda (this cb)
		     (nodejs-close %worker %this process this cb))
		  1 "close")
	       #f %this)
	    
	    (js-put! obj 'ref
	       (js-make-function %this
		  (lambda (this)
		     (with-access::JsHandle this (handle)
			(nodejs-ref handle %worker)))
		  0 "ref")
	       #f %this)
	    
	    (js-put! obj 'unref
	       (js-make-function %this
		  (lambda (this)
		     (with-access::JsHandle this (handle)
			(nodejs-unref handle %worker)))
		  0 "unref")
	       #f %this)
	    
	    (js-put! obj 'connect
	       (js-make-function %this
		  (connect 4)
		  3 "connect")
	       #f %this)
	    
	    (js-put! obj 'connect6
	       (js-make-function %this
		  (connect 6)
		  3 "connect")
	       #f %this)
	    
	    (js-put! obj 'writeBuffer
	       (js-make-function %this
		  (lambda (this buffer)
		     (stream-write-buffer %worker %this process this buffer))
		  1 "writeBuffer")
	       #f %this)
	    
	    (js-put! obj 'writeAsciiString
	       (js-make-function %this
		  (lambda (this string handle)
		     (stream-write-string %worker %this process this
			(js-jsstring->string string) 0 (js-jsstring-lengthfx string)
			"ascii" #f handle))
		  2 "writeAsciiString")
	       #f %this)
	    
	    (js-put! obj 'writeUtf8String
	       (js-make-function %this
		  (lambda (this string handle)
		     (stream-write-string %worker %this process this
			(js-jsstring->string string) 0 (js-jsstring-lengthfx string)
			"utf8" #f handle))
		  2 "writeUtf8String")
	       #f %this)
	    
	    (js-put! obj 'writeUcs2String
	       (js-make-function %this
		  (lambda (this string handle)
		     (let* ((ucs2string (utf8-string->ucs2-string string))
			    (buffer (ucs2-string->buffer ucs2string)))
			(stream-write-string %worker %this process this
			   (js-jsstring->string string) 0 (js-jsstring-lengthfx string)
			   "ascii" #f handle)))
		  2 "writeUcs2String")
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
	    
	    (js-put! obj 'setNoDelay
	       (js-make-function %this
		  (lambda (this val)
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-nodelay handle (js-totest val))))
		  1 "setNoDelay")
	       #f %this)
	    
	    (js-put! obj 'setKeepAlive
	       (js-make-function %this
		  (lambda (this val tmt)
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-keepalive handle (js-totest val)
			   (->fixnum (js-tointeger tmt %this)))))
		  2 "setKeepAlive")
	       #f %this)
	    
	    (js-put! obj 'setSimultaneousAccepts
	       (js-make-function %this
		  (lambda (this val)
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-simultaneous-accepts
			   handle (js-totest val))))
		  1 "setSimultaneousAccepts")
	       #f %this)
	    
	    (js-put! obj 'getsockname
	       (js-make-function %this
		  (lambda (this val)
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-getsockname %this handle)))
		  1 "getsockname")
	       #f %this)
	    
	    (js-put! obj 'getpeername
	       (js-make-function %this
		  (lambda (this val)
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-getpeername %this handle)))
		  1 "getpeername")
	       #f %this)
	    
	    (js-put! obj 'shutdown
	       (js-make-function %this
		  (lambda (this)
		     (stream-shutdown %worker %this process this))
		  1 "shutdown")
	       #f %this)
	    
	    (js-put! obj 'open
	       (js-make-function %this
		  (lambda (this fd)
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-open %worker %this handle fd)))
		  2 "open")
	       #f %this)
	    
	    (js-put! obj 'bind
	       (js-make-function %this
		  (lambda (this addr port)
		     (with-access::JsHandle this (handle)
			(let ((p (->fixnum (js-tointeger port %this))))
			   (nodejs-tcp-bind %this process handle addr p 4))))
		  2 "bind")
	       #f %this)
	    
	    (js-put! obj 'bind6
	       (js-make-function %this
		  (lambda (this addr port)
		     (with-access::JsHandle this (handle)
			(let ((p (->fixnum (js-tointeger port %this))))
			   (nodejs-tcp-bind %this process handle addr p 6))))
		  2 "bind6")
	       #f %this)
	    
	    (js-put! obj 'listen
	       (js-make-function %this
		  (lambda (this backlog)
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-listen %worker %this process this handle
			   (->fixnum (js-tointeger backlog %this))
			   tcp-wrap)))
		  1 "listen")
	       #f %this)
	    
	    obj)))
   
   (define (get-tcp-proto)
      (with-access::JsProcess process (tcp-proto)
	 (unless tcp-proto
	    (set! tcp-proto (create-tcp-proto)))
	 tcp-proto))

   (define (tcp-wrap hdl)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((obj (instantiate-JsHandle
		       (handle hdl)
		       (__proto__ (get-tcp-proto)))))
	    (js-bind! %this obj 'fd
	       :get (js-make-function %this
		       (lambda (this)
			  (nodejs-stream-fd %worker hdl))
		       0 'getGD)
	       :writable #f :configurable #f)
	    (js-put! obj 'writeQueueSize
	       (nodejs-stream-write-queue-size hdl) #f %this)
	    obj)))
   
   (define (TCP this)
      (tcp-wrap (nodejs-tcp-handle %worker)))
   
   (with-access::JsGlobalObject %this (js-object)
      (with-access::JsProcess process (js-tcp)
	 (let ((obj (js-new %this js-object)))
	    (set! js-tcp
	       (js-make-function %this TCP 0 "TCP"
		  :alloc (lambda (o) #unspecified)
		  :prototype (get-tcp-proto)
		  :construct TCP))
	    (js-put! obj 'TCP js-tcp #f %this)
	    obj))))
