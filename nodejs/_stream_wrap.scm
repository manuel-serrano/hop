;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_stream_wrap.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Oct 20 12:31:24 2014                          */
;*    Last change :  Thu Oct 23 09:53:35 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Common stream functions                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__stream-wrap

   (library hopscript)

   (import  __nodejs_uv
	    __nodejs_process
	    __nodejs__buffer)

   (export (stream-shutdown ::WorkerHopThread ::JsGlobalObject
	      ::JsObject ::JsHandle)
	   (stream-write-buffer ::WorkerHopThread ::JsGlobalObject
	      ::JsObject ::obj)
	   (stream-write-string ::WorkerHopThread ::JsGlobalObject
	      ::JsHandle ::bstring ::long ::long ::obj ::obj)
	   (stream-read-start ::WorkerHopThread ::JsGlobalObject ::JsProcess
	      ::obj ::JsHandle)
	   (stream-read-stop ::WorkerHopThread ::JsGlobalObject
	      ::JsHandle)
	   (ucs2-string->buffer ::ucs2string)))

;*---------------------------------------------------------------------*/
;*    stream-shutdown ...                                              */
;*---------------------------------------------------------------------*/
(define (stream-shutdown %worker %this process this::JsHandle)
   (with-access::JsGlobalObject %this (js-object)
      (with-access::JsHandle this (handle)
	 (let* ((req (js-new %this js-object))
		(res (nodejs-stream-shutdown %worker %this handle
			(lambda (status handle)
			   (when (<fx status 0)
			      (js-put! process '_errno
				 (nodejs-err-name status) #f %this))
			   (let ((oncomp (js-get req 'oncomplete %this)))
			      (js-call3 %this oncomp req status this req))))))
	    (when (=fx res 0)
	       req)))))

;*---------------------------------------------------------------------*/
;*    stream-write-buffer ...                                          */
;*---------------------------------------------------------------------*/
(define (stream-write-buffer %worker %this this buffer)
   (with-access::JsTypedArray buffer (%data byteoffset length)
      (stream-write-string %worker %this this
	 %data
	 (uint32->fixnum byteoffset)
	 (uint32->fixnum length)
	 "ascii" #f)))

;*---------------------------------------------------------------------*/
;*    stream-write-string ...                                          */
;*---------------------------------------------------------------------*/
(define (stream-write-string %worker %this this::JsHandle
	   string::bstring offset::long len::long
	   encoding callback)
   (with-access::JsGlobalObject %this (js-object)
      (let ((ipc #f))
	 (if ipc
	     (error "stream-write-string" "IPC Not implemented yet" this)
	     (let ((req (js-new %this js-object)))
		(js-put! req 'bytes len #f %this)
		(with-access::JsHandle this (handle)
		   (js-put! this 'writeQueueSize
		      (nodejs-stream-write-queue-size handle) #f %this)
		   (nodejs-stream-write %worker %this handle
		      string offset len
		      (lambda (status)
			 (let ((oncomp (js-get req 'oncomplete %this)))
			    (js-call3 %this oncomp req status this req)
			    (js-put! this 'writeQueueSize
			       (nodejs-stream-write-queue-size handle) #f %this)
			    (js-undefined)))))
		req)))))

;*---------------------------------------------------------------------*/
;*    ucs2-string->buffer ...                                          */
;*---------------------------------------------------------------------*/
(define (ucs2-string->buffer ucs2string)
   (let* ((len (ucs2-string-length ucs2string))
	  (buf (make-string (*fx len 2))))
      (let loop ((i 0))
	 (if (=fx i len)
	     buf
	     (let* ((u (ucs2-string-ref ucs2string i))
		    (n (ucs2->integer u))
		    (i2 (*fx i 2)))
		(string-set! buf i2 (integer->char (bit-rsh n 8)))
		(string-set! buf (+fx 1 i2) (integer->char (bit-and n #x255)))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    stream-read-start ...                                            */
;*---------------------------------------------------------------------*/
(define (stream-read-start %worker %this process slab this)
   (with-access::JsHandle this (handle)
      (nodejs-stream-read-start %worker %this handle
	 (lambda (obj size) (slab-allocate slab obj size))
	 (lambda (status buf offset len)
	    (with-trace 'nodejs-buffer "read-start-cb"
	       (trace-item "status=" status " buf=" (typeof buf)
		  " offset=" offset " len=" len)
	       (cond
		  ((eof-object? status)
		   ;; eof
		   (js-put! process '_errno "EOF" #f %this)
		   (let ((onread (js-get this 'onread %this)))
		      (js-call3 %this onread this
			 (js-undefined) (js-undefined) +nan.0)))
		  ((not status)
		   ;; read error
		   (slab-shrink! slab buf offset 0)
		   (js-put! process '_errno (nodejs-err-name len) #f %this)
		   (let ((onread (js-get this 'onread %this)))
		      (js-call0 %this onread this)))
		  ((=fx len 0)
		   ;; nothing read
		   (slab-shrink! slab buf offset 0))
		  (else
		   ;; characters read
		   (let ((b (slab-shrink! slab buf offset len)))
		      (let ((onread (js-get this 'onread %this)))
			 (js-call3 %this onread this b offset len)
			 (js-undefined))))))))))

;*---------------------------------------------------------------------*/
;*    stream-read-stop ...                                             */
;*---------------------------------------------------------------------*/
(define (stream-read-stop %worker %this this)
   (with-access::JsHandle this (handle)
      (nodejs-stream-read-stop %worker %this handle)))
		
