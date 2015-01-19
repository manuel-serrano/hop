;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_pipe_wrap.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Oct 19 07:19:20 2014                          */
;*    Last change :  Sat Jan 17 08:50:43 2015 (serrano)                */
;*    Copyright   :  2014-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Nodejs PIPE bindings                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__pipe-wrap

   (library hopscript)

   (import  __nodejs_uv
	    __nodejs_process
	    __nodejs__buffer
	    __nodejs__stream-wrap)

   (export (process-pipe-wrap ::WorkerHopThread ::JsGlobalObject ::JsProcess ::obj)))

;*---------------------------------------------------------------------*/
;*    process-pipe-wrap ...                                            */
;*---------------------------------------------------------------------*/
(define (process-pipe-wrap %worker %this process slab)

   (define (->fixnum n)
      (cond
	 ((fixnum? n) n)
	 ((flonum? n) (flonum->fixnum n))
	 (else 0)))
   
   (define pipe-prototype
      (with-access::JsGlobalObject %this (js-object)
	 (js-new %this js-object)))
   
   ;; close
   (js-put! pipe-prototype 'close
      (js-make-function %this
		  (lambda (this cb)
		     (nodejs-close %worker %this this cb))
		  1 "close")
      #f %this)

   ;; unref
   (js-put! pipe-prototype 'unref
      (js-make-function %this
		  (lambda (this)
		     (with-access::JsHandle this (handle)
			(nodejs-unref handle %worker)))
		  0 "unref")
      #f %this)

   ;; ref
   (js-put! pipe-prototype 'ref
      (js-make-function %this
	 (lambda (this)
	    (with-access::JsHandle this (handle)
	       (nodejs-ref handle %worker)))
	 0 "ref")
      #f %this)

   ;; readStart
   (js-put! pipe-prototype 'readStart
      (js-make-function %this
	 (lambda (this)
	    (stream-read-start %worker %this process slab this))
	 0 "readStart")
      #f %this)
   
   ;; readStop
   (js-put! pipe-prototype 'readStop
      (js-make-function %this
	 (lambda (this)
	    (stream-read-stop %worker %this this))
	 0 "readStop")
      #f %this)
   
   ;; shutdown
   (js-put! pipe-prototype 'shutdown
      (js-make-function %this 
	 (lambda (this)
	    (stream-shutdown %worker %this process this))
	 0 'shutdown)
      #f %this)
   
   ;; writeBuffer
   (js-put! pipe-prototype 'writeBuffer
      (js-make-function %this
	 (lambda (this buffer)
	    (stream-write-buffer %worker %this this buffer))
	 1 "writeBuffer")
      #f %this)
   
   ;; writeAsciiString
   (js-put! pipe-prototype 'writeAsciiString
      (js-make-function %this 
	 (lambda (this string handle)
	    (stream-write-string %worker %this this
	       (js-jsstring->string string) 0 (js-jsstring-length string)
	       "ascii" #f handle))
	 2 'writeAsciiString)
      #f %this)
   
   ;; writeUtf8String
   (js-put! pipe-prototype 'writeUtf8String
      (js-make-function %this
	 (lambda (this string handle)
	    (stream-write-string %worker %this this
	       (js-jsstring->string string) 0 (js-jsstring-length string)
	       "utf8" #f handle))
	 2 "writeUtf8String")
      #f %this)
   
   ;; writeUcs2String
   (js-put! pipe-prototype 'writeUcs2String
      (js-make-function %this
	 (lambda (this string handle)
	    (let* ((ucs2string (utf8-string->ucs2-string string))
		   (buffer (ucs2-string->buffer ucs2string)))
	       (stream-write-string %worker %this this
		  (js-jsstring->string string) 0 (js-jsstring-length string)
		  "ascii" #f handle)))
	 2 "writeUcs2String")
      #f %this)
   
   ;; bind
   (js-put! pipe-prototype 'bind
      (js-make-function %this 
	 (lambda (this name)
	    (with-access::JsHandle this (handle)
	       (nodejs-pipe-bind %this process handle name)))
	 0 'bind)
      #f %this)
   
   ;; listen
   (js-put! pipe-prototype 'listen
      (js-make-function %this
		  (lambda (this backlog)
		     (tprint "UNTESTED, example needed")
		     (with-access::JsHandle this (handle)
			(nodejs-pipe-listen %worker %this process this handle
			   (->fixnum (js-tointeger backlog %this)))))
		  1 "listen")
      #f %this)
   
   ;; connect
   (js-put! pipe-prototype 'connect
      (js-make-function %this 
	 (lambda (this name callback)
	    (with-access::JsGlobalObject %this (js-object)
	       (with-access::JsHandle this (handle)
		  (let ((req (js-new %this js-object)))
		     (nodejs-pipe-connect %worker %this handle name
			(lambda (status handle)
			   (when (<fx status 0)
			      (js-put! process '_errno
				 (nodejs-err-name status) #f %this))
			   (let ((oncomp (js-get req 'oncomplete %this)))
			      (js-call5 %this oncomp req status
				 this req #t #t)
			      (js-undefined))))
		     req))))
	 2 'connect)
      #f %this)
   
   ;; open
   (js-put! pipe-prototype 'open
      (js-make-function %this 
	 (lambda (this fd)
	    (with-access::JsHandle this (handle)
	       (nodejs-pipe-open %worker %this handle fd)))
	 1 'open)
      #f %this)

   ;; pipe
   (define (pipe . val)
      (with-access::JsGlobalObject %this (js-object)
	 (let* ((hdl (nodejs-new-pipe %worker (and (pair? val) (car val))))
		(obj (instantiate::JsHandle
			(handle hdl)
			(__proto__ pipe-prototype))))
	    ;; fd
	    (js-bind! %this obj 'fd
	       :get (js-make-function %this
		       (lambda (this)
			  (with-access::JsHandle this (handle)
			     (nodejs-stream-fd handle)))
		       0 'getGD)
	       :writable #f :configurable #f)
	    ;; writeQueueSize
	    (js-put! obj 'writeQueueSize
	       (nodejs-stream-write-queue-size hdl) #f %this)
	    obj)))
   
   (with-access::JsGlobalObject %this (js-object)
      (with-access::JsProcess process (js-pipe)
	 (let ((obj (js-new %this js-object)))
	    (set! js-pipe
	       (js-make-function %this
		  (lambda (this . args) #unspecified) 1 'Pipe
		  :construct pipe
		  :prototype pipe-prototype
		  :alloc (lambda (o) #unspecified)))
	    (js-put! obj 'Pipe js-pipe #t %this)
	    obj))))
