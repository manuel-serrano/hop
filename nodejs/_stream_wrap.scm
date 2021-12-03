;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/nodejs/_stream_wrap.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Oct 20 12:31:24 2014                          */
;*    Last change :  Sat Aug 28 06:29:52 2021 (serrano)                */
;*    Copyright   :  2014-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Common stream functions                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__stream-wrap

   (include "../hopscript/stringthread.sch")
   
   (library hopscript)

   (include "nodejs_async.sch")
   
   (import  __nodejs_uv
	    __nodejs_process
	    __nodejs__buffer)

   (export (stream-shutdown ::WorkerHopThread ::JsGlobalObject
	      ::JsObject ::JsHandle)
	   (stream-write-buffer ::WorkerHopThread ::JsGlobalObject ::JsProcess
	      ::JsObject ::obj)
	   (stream-write-string ::WorkerHopThread ::JsGlobalObject ::JsProcess
	      ::JsHandle ::bstring ::long ::long ::obj ::obj ::obj)
	   (stream-read-start ::WorkerHopThread ::JsGlobalObject ::JsProcess
	      ::obj ::JsHandle)
	   (stream-read-stop ::WorkerHopThread ::JsGlobalObject
	      ::JsHandle)
	   (ucs2-string->buffer ::ucs2string)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    js-init-stream-wrap! ...                                         */
;*---------------------------------------------------------------------*/
(define (js-init-stream-wrap! %this)
   (unless (vector? __js_strings)
      (set! __js_strings (&init!))))

;*---------------------------------------------------------------------*/
;*    stream-shutdown ...                                              */
;*---------------------------------------------------------------------*/
(define (stream-shutdown %worker %this process this::JsHandle)
   (js-init-stream-wrap! %this)
   (with-access::JsGlobalObject %this (js-object)
      (with-access::JsHandle this (handle reqs)
	 (let* ((req (js-new %this js-object))
		(res (nodejs-stream-shutdown %worker %this handle
			(lambda (status handle)
			   (when (<fx status 0)
			      (js-put! process (& "_errno")
				 (nodejs-err-name status) #f %this))
			   (set! reqs (remq req reqs))
			   (let ((oncomp (js-get req (& "oncomplete") %this)))
			      (!js-callback3 "shutdown" %worker %this
				 oncomp req status this req)
			      '(js-worker-tick %worker))))))
	    (set! reqs (cons req reqs))
	    (when (=fx res 0)
	       req)))))

;*---------------------------------------------------------------------*/
;*    stream-write-buffer ...                                          */
;*---------------------------------------------------------------------*/
(define (stream-write-buffer %worker %this process this buffer)
   (with-access::JsTypedArray buffer (%data byteoffset length)
      (stream-write-string %worker %this process this
	 %data
	 (uint32->fixnum byteoffset)
	 (uint32->fixnum length)
	 "ascii" #f #f)))

;*---------------------------------------------------------------------*/
;*    stream-write-string ...                                          */
;*---------------------------------------------------------------------*/
(define (stream-write-string %worker %this process this::JsHandle
	   string::bstring offset::long len::long
	   encoding callback sendhandle)
   (js-init-stream-wrap! %this)
   (with-access::JsGlobalObject %this (js-object js-nodejs-pcache)
      (with-access::JsHandle this (handle reqs)
	 (let ((req (js-new %this js-object)))
	    (set! reqs (cons req reqs))
	    (with-access::JsProcess process (using-domains)
	       (if using-domains
		   (let ((dom (js-get-jsobject-name/cache process (& "domain") #f %this
				 (js-pcache-ref js-nodejs-pcache 3))))
		      (js-put-jsobject-name/cache! req (& "domain") dom #f %this
			 (js-pcache-ref js-nodejs-pcache 4)))
		   (js-put-jsobject-name/cache! req (& "domain") (js-null) #f %this
		      (js-pcache-ref js-nodejs-pcache 5))))
	    (js-put-jsobject-name/cache! req (& "bytes") len #f %this
	       (js-pcache-ref js-nodejs-pcache 6))
	    (with-access::JsHandle this (handle)
	       (set! reqs (remq req reqs))
	       (let ((cb (lambda (status)
			    (js-put-jsobject-name/cache! this (& "writeQueueSize")
			       (nodejs-stream-write-queue-size handle) #f %this
			       (js-pcache-ref js-nodejs-pcache 7))
			    (when (<fx status 0)
			       (js-put! process (& "_errno")
				  (nodejs-err-name status) #f %this))
			    (let ((oncomp (js-get-jsobject-name/cache req
					     (& "oncomplete") #f %this
					     (js-pcache-ref js-nodejs-pcache 8))))
			       (js-call3 %this oncomp req status this req)
			       '(js-worker-tick %worker)
			       (js-undefined)))))
		  (if (nodejs-pipe-ipc? handle)
		      (if (isa? sendhandle JsHandle)
			  (with-access::JsHandle sendhandle ((shdl handle))
			     (begin
				(js-put-jsobject-name/cache! req (& "handle")
				   sendhandle #f %this
				   (js-pcache-ref js-nodejs-pcache 9))
				(nodejs-stream-write2 %worker %this handle
				   string offset len shdl cb)))
			  (nodejs-stream-write2 %worker %this handle
			     string offset len #f cb))
		      (nodejs-stream-write %worker %this handle
			 string offset len cb)))
	       (js-put-jsobject-name/cache! this (& "writeQueueSize")
		  (nodejs-stream-write-queue-size handle) #f %this
		  (js-pcache-ref js-nodejs-pcache 10)))
	    req))))

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
   (with-trace 'nodejs-buffer "read-start"
      (js-init-stream-wrap! %this)
      (with-access::JsHandle this (handle)
	 (nodejs-stream-read-start %worker %this process handle
	    (lambda (obj size) (slab-allocate slab obj size))
	    (lambda (status buf offset len pending-type)
	       (with-trace 'nodejs-buffer "read-start-cb"
		  (trace-item "status=" status " buf=" (typeof buf)
		     " offset=" offset " len=" len)
		  (cond
		     ((eof-object? status)
		      ;; eof
		      (js-put! process (& "_errno") (js-string->jsstring "EOF")
			 #f %this)
		      (let ((onread (js-get this (& "onread") %this)))
			 (!js-callback3 "read-start" %worker %this onread this
			    (js-undefined) (js-undefined) (js-undefined))))
		     ((not status)
		      ;; read error
		      (slab-shrink! slab buf offset 0)
		      (js-put! process (& "_errno") (nodejs-err-name len) #f %this)
		      (let ((onread (js-get this (& "onread") %this)))
			 (!js-callback0 "read-start" %worker %this onread this)))
		     ((=fx len 0)
		      ;; nothing read
		      (slab-shrink! slab buf offset 0))
		     (else
		      ;; characters read
		      (let* ((b (slab-shrink! slab buf offset len))
			     (onread (js-get this (& "onread") %this)))
			 (if (and (nodejs-pipe-ipc? handle) pending-type)
			     (!js-callback4 "read-start" %worker %this
				onread this b offset len
				(nodejs-pipe-accept %worker %this this
				   pending-type))
			     (!js-callback3 "read-start" %worker %this
				onread this b offset len))
			 (js-undefined))))))))))

;*---------------------------------------------------------------------*/
;*    stream-read-stop ...                                             */
;*---------------------------------------------------------------------*/
(define (stream-read-stop %worker %this this)
   (with-access::JsHandle this (handle)
      (nodejs-stream-read-stop %worker %this handle)))
		
;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)

