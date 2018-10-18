;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_udp_wrap.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Oct 19 07:19:20 2014                          */
;*    Last change :  Mon Jul  6 14:06:39 2015 (serrano)                */
;*    Copyright   :  2014-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Nodejs UDP bindings                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__udp-wrap

   (library hopscript)

   (include "nodejs_async.sch")
   
   (import  __nodejs_uv
	    __nodejs_process
	    __nodejs__buffer
	    __nodejs__stream-wrap)

   (export (process-udp-wrap ::WorkerHopThread ::JsGlobalObject 
	      ::JsProcess ::obj ::JsObject)))

;*---------------------------------------------------------------------*/
;*    process-udp-wrap ...                                             */
;*---------------------------------------------------------------------*/
(define (process-udp-wrap %worker %this process::JsProcess slab slowbuffer::JsObject)
   
   (define (not-implemented name)
      (js-make-function %this
	 (lambda (this . l)
	    (error "udp_wrap" "binding not implemented" name))
	 0 (symbol->string name)))

   (define (create-udp-proto)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((obj (js-new %this js-object)))
	    
	    (js-put! obj 'bind
	       (js-make-function %this
		  (lambda (this addr port flags)
		     (with-access::JsHandle this (handle)
			(let ((p (->fixnum (js-tointeger port %this))))
			   (nodejs-udp-bind %this process handle addr p 4
			      (if (eq? flags (js-undefined)) 0 flags)))))
		  3 "bind")
	       #f %this)
	    
	    (js-put! obj 'send
	       (js-make-function %this
		  (lambda (this buffer offset length port address callback)
		     (udp-send %worker process %this this
			buffer offset length port address 4 callback))
		  6 "send")
	       #f %this)
	    
	    (js-put! obj 'bind6
	       (js-make-function %this
		  (lambda (this addr port flags)
		     (with-access::JsHandle this (handle)
			(let ((p (->fixnum (js-tointeger port %this))))
			   (nodejs-udp-bind %this process handle addr p 6
			      (if (eq? flags (js-undefined)) 0 flags)))))
		  3 "bind6")
	       #f %this)
	    
	    (js-put! obj 'send6
	       (js-make-function %this
		  (lambda (this buffer offset length port address callback)
		     (udp-send %worker process %this this 
			buffer offset length port address 6 callback))
		  5 "send6")
	       #f %this)
	    
	    (js-put! obj 'close
	       (js-make-function %this
		  (lambda (this cb)
		     (nodejs-close %worker %this process this cb))
		  1 "close")
	       #f %this)
	    
	    (js-put! obj 'recvStart
	       (js-make-function %this
		  (lambda (this)
		     (udp-recv-start %worker %this process slab this))
		  0 "recvStart")
	       #f %this)
	    
	    (js-put! obj 'recvStop
	       (js-make-function %this
		  (lambda (this)
		     (with-access::JsHandle this (handle)
			(nodejs-udp-recv-stop handle)))
		  1 "recvStop")
	       #f %this)
	    
	    (js-put! obj 'getsockname
	       (js-make-function %this
		  (lambda (this val)
		     (with-access::JsHandle this (handle)
			(nodejs-udp-getsockname %this handle)))
		  1 "getsockname")
	       #f %this)
	    
	    (js-put! obj 'addMembership
	       (js-make-function %this
		  (lambda (this addr iface)
		     (with-access::JsHandle this (handle)
			(nodejs-udp-set-membership handle
			   (js-tostring addr %this)
			   (unless (eq? iface (js-undefined))
			      (js-tostring iface %this))
			   'join-group)))
		  3 "addMembership")
	       #f %this)
	    
	    (js-put! obj 'dropMembership
	       (js-make-function %this
		  (lambda (this addr iface)
		     (with-access::JsHandle this (handle)
			(nodejs-udp-set-membership handle
			   (js-tostring addr %this)
			   (unless (eq? iface (js-undefined))
			      (js-tostring iface %this))
			   'leave-group)))
		  3 "dropMembership")
	       #f %this)
	    
	    (js-put! obj 'setMulticastTTL
	       (js-make-function %this
		  (lambda (this ttl)
		     (with-access::JsHandle this (handle)
			(nodejs-udp-set-multicast-ttl handle
			   (->fixnum (js-tointeger ttl %this) ))))
		  2 "setMulticastTTL")
	       #f %this)
	    
	    (js-put! obj 'setMulticastLoopback
	       (js-make-function %this
		  (lambda (this on)
		     (with-access::JsHandle this (handle)
			(nodejs-udp-set-multicast-loop handle
			   (js-toboolean on))))
		  2 "setMulticastLoopback")
	       #f %this)
	    
	    (js-put! obj 'setBroadcast
	       (js-make-function %this
		  (lambda (this on)
		     (with-access::JsHandle this (handle)
			(nodejs-udp-set-broadcast handle
			   (js-toboolean on))))
		  2 "setBroadcast")
	       #f %this)
	    
	    (js-put! obj 'setTTL
	       (js-make-function %this
		  (lambda (this ttl)
		     (with-access::JsHandle this (handle)
			(nodejs-udp-set-ttl handle
			   (->fixnum (js-tointeger ttl %this) ))))
		  2 "setTTL")
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
	    
	    obj)))
   
   (define (get-udp-proto)
      (with-access::JsProcess process (udp-proto)
	 (unless udp-proto
	    (set! udp-proto (create-udp-proto)))
	 udp-proto))
   
   (define (udp-wrap hdl)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((obj (instantiate::JsHandle
		       (handle hdl)
		       (__proto__ (get-udp-proto)))))
	    (js-bind! %this obj 'fd
	       :get (js-make-function %this
		       (lambda (this)
			  (nodejs-stream-fd %worker hdl))
		       0 'GetFD)
	       :writable #f :configurable #f)
	    obj)))
   
   (define (UDP this)
      (udp-wrap (nodejs-udp-handle %worker)))
   
   (with-access::JsGlobalObject %this (js-object)
      (with-access::JsProcess process (js-udp)
	 (let ((obj (js-new %this js-object)))
	    (set! js-udp
	       (js-make-function %this UDP 0 "UDP"
		  :alloc (lambda (o) #unspecified)
		  :prototype (get-udp-proto)
		  :construct UDP))
	    (js-put! obj 'UDP js-udp #f %this)
	    obj))))

;*---------------------------------------------------------------------*/
;*    ->fixnum ...                                                     */
;*---------------------------------------------------------------------*/
(define (->fixnum n)
   (cond
      ((fixnum? n) n)
      ((flonum? n) (flonum->fixnum n))
      (else 0)))

;*---------------------------------------------------------------------*/
;*    udp-send ...                                                     */
;*    -------------------------------------------------------------    */
;*    See STREAM-WRITE-STRING                                          */
;*---------------------------------------------------------------------*/
(define (udp-send %worker process %this this::JsHandle
	   buffer::JsTypedArray offset length port address family callback)
   (with-access::JsGlobalObject %this (js-object)
      (let ((off (->fixnum (js-tointeger offset %this)))
	    (len (->fixnum (js-tointeger length %this)))
	    (port (->fixnum (js-tointeger port %this)))
	    (req (js-new %this js-object)))
	 (js-put! req 'buffer buffer #f %this)
	 (with-access::JsHandle this (handle)
	    (with-access::JsTypedArray buffer (%data byteoffset length)
	       (let ((r (nodejs-udp-send %worker %this handle %data
			   (+fx off (uint32->fixnum byteoffset))
			   len
			   port (js-tostring address %this) family
			   (lambda (status)
			      (with-trace 'nodejs-udp "send-cb"
				 (trace-item "status=" status
				    " req=" (typeof req) " this=" (typeof this)
				    " buffer=" (typeof buffer))
				 (let ((oncomp (js-get req 'oncomplete %this)))
				    ;; MS: nodejs set the buffer into the req
				    ;; object using a setHiddenValue method
				    ;; that I don't understand
				    (trace-item "oncomp=" (typeof oncomp))
				    (unless (eq? oncomp (js-undefined))
				       (!js-callback4 'ude-send %worker %this
					  oncomp req status this req buffer))
				    (js-undefined)))))))
		  (if (=fx r 0)
		      req
		      (begin
			 (js-put! process '_errno (nodejs-err-name r) #f %this)
			 (js-undefined)))))))))

;*---------------------------------------------------------------------*/
;*    udp-recv-start ...                                               */
;*---------------------------------------------------------------------*/
(define (udp-recv-start %worker %this process slab this)
   (with-access::JsHandle this (handle)
      (let ((r (nodejs-udp-recv-start %worker %this handle
		  (lambda (obj size) (slab-allocate slab obj size))
		  (lambda (status buf offset len addr)
		     (with-trace 'nodejs-udp "recv-start-cb"
			(trace-item "status=" status " buf=" (typeof buf)
			   " offset=" offset " len=" len)
			(cond
			   ((=fx len 0)
			    ;; nothing read
			    (slab-shrink! slab buf offset 0))
			   ((not status)
			    ;; read error
			    (slab-shrink! slab buf offset 0)
			    (js-put! process '_errno (nodejs-err-name len) #f %this)
			    (let ((onmsg (js-get this 'onmessage %this)))
			       (!js-callback0 "recv-start" %worker %this
				  onmsg this)))
			   (else
			    ;; characters read
			    (let ((b (slab-shrink! slab buf offset len)))
			       (let ((onmsg (js-get this 'onmessage %this)))
				  (!js-callback5 "recv-start" %worker %this
				     onmsg this this b offset len
				     (js-alist->jsobject addr %this))
				  (js-undefined))))))))))
	 (if (=fx r 0)
	     #t
	     (begin
		(js-put! process '_errno (nodejs-err-name r) #f %this)
		#f)))))
		

