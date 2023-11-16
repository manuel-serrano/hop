;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/nodejs/_udp_wrap.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Oct 19 07:19:20 2014                          */
;*    Last change :  Sun May  7 14:11:41 2023 (serrano)                */
;*    Copyright   :  2014-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Nodejs UDP bindings                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__udp-wrap

   (include "../hopscript/stringthread.sch")
   
   (library hopscript)

   (include "nodejs_types.sch")
   
   (import  __nodejs_uv
	    __nodejs_process
	    __nodejs__buffer
	    __nodejs__stream-wrap)

   (export (process-udp-wrap ::WorkerHopThread ::JsGlobalObject ::JsProcess)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    process-udp-wrap ...                                             */
;*---------------------------------------------------------------------*/
(define (process-udp-wrap %worker %this process::JsProcess)
   
   (define (check-fail %this process r)
      (unless (=fx r 0)
	 (js-put! process (& "_errno") (nodejs-err-name r) #f %this))
      r)

   (define (create-udp-proto)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((obj (js-new %this js-object)))
	    
	    (js-put! obj (& "bind")
	       (js-make-function %this
		  (lambda (this addr port flags)
		     (with-access::JsHandle this (handle)
			(let ((p (->fixnum (js-tointeger port %this))))
			   (nodejs-udp-bind %this process handle addr p 4
			      (if (eq? flags (js-undefined)) 0 flags)))))
		  (js-function-arity 3 0)
		  (js-function-info :name "bind" :len 3))
	       #f %this)
	    
	    (js-put! obj (& "send")
	       (js-make-function %this
		  (lambda (this buffer offset length port address callback)
		     (udp-send %worker process %this this
			buffer offset length port address 4 callback))
		  (js-function-arity 6 0)
		  (js-function-info :name "send" :len 6))
	       #f %this)
	    
	    (js-put! obj (& "bind6")
	       (js-make-function %this
		  (lambda (this addr port flags)
		     (with-access::JsHandle this (handle)
			(let ((p (->fixnum (js-tointeger port %this))))
			   (nodejs-udp-bind %this process handle addr p 6
			      (if (eq? flags (js-undefined)) 0 flags)))))
		  (js-function-arity 3 0)
		  (js-function-info :name "bind6" :len 3))
	       #f %this)
	    
	    (js-put! obj (& "send6")
	       (js-make-function %this
		  (lambda (this buffer offset length port address callback)
		     (udp-send %worker process %this this 
			buffer offset length port address 6 callback))
		  (js-function-arity 6 0)
		  (js-function-info :name "send6" :len 6))
	       #f %this)
	    
	    (js-put! obj (& "close")
	       (js-make-function %this
		  (lambda (this cb)
		     (nodejs-close %worker %this process this cb))
		  (js-function-arity 1 0)
		  (js-function-info :name "close" :len 1))
	       #f %this)
	    
	    (js-put! obj (& "recvStart")
	       (js-make-function %this
		  (lambda (this)
		     (udp-recv-start %worker %this process this))
		  (js-function-arity 0 0)
		  (js-function-info :name "recvStart" :len 0))
	       #f %this)
	    
	    (js-put! obj (& "recvStop")
	       (js-make-function %this
		  (lambda (this)
		     (with-access::JsHandle this (handle)
			(nodejs-udp-recv-stop handle)))
		  (js-function-arity 0 0)
		  (js-function-info :name "recvStop" :len 0))
	       #f %this)
	    
	    (js-put! obj (& "getsockname")
	       (js-make-function %this
		  (lambda (this val)
		     (with-access::JsHandle this (handle)
			(nodejs-udp-getsockname %this handle)))
		  (js-function-arity 1 0)
		  (js-function-info :name "getsockname" :len 1))
	       #f %this)
	    
	    (js-put! obj (& "addMembership")
	       (js-make-function %this
		  (lambda (this addr iface)
		     (with-access::JsHandle this (handle)
			(check-fail %this process
			   (nodejs-udp-set-membership handle
			      (js-tostring addr %this)
			      (unless (eq? iface (js-undefined))
				 (js-tostring iface %this))
			      'join-group))))
		  (js-function-arity 2 0)
		  (js-function-info :name "addMembership" :len 2))
	       #f %this)
	    
	    (js-put! obj (& "dropMembership")
	       (js-make-function %this
		  (lambda (this addr iface)
		     (with-access::JsHandle this (handle)
			(nodejs-udp-set-membership handle
			   (js-tostring addr %this)
			   (unless (eq? iface (js-undefined))
			      (js-tostring iface %this))
			   'leave-group)))
		  (js-function-arity 2 0)
		  (js-function-info :name "dropMembership" :len 2))
	       #f %this)
	    
	    (js-put! obj (& "setMulticastTTL")
	       (js-make-function %this
		  (lambda (this ttl)
		     (with-access::JsHandle this (handle)
			(nodejs-udp-set-multicast-ttl handle
			   (->fixnum (js-tointeger ttl %this) ))))
		  (js-function-arity 1 0)
		  (js-function-info :name "setMulticastTTL" :len 1))
	       #f %this)
	    
	    (js-put! obj (& "setMulticastLoopback")
	       (js-make-function %this
		  (lambda (this on)
		     (with-access::JsHandle this (handle)
			(nodejs-udp-set-multicast-loop handle
			   (js-toboolean on))))
		  (js-function-arity 1 0)
		  (js-function-info :name "setMulticastLoopback" :len 1))
	       #f %this)
	    
	    (js-put! obj (& "setBroadcast")
	       (js-make-function %this
		  (lambda (this on)
		     (with-access::JsHandle this (handle)
			(nodejs-udp-set-broadcast handle
			   (js-toboolean on))))
		  (js-function-arity 1 0)
		  (js-function-info :name "setBroadcast" :len 1))
	       #f %this)
	    
	    (js-put! obj (& "setTTL")
	       (js-make-function %this
		  (lambda (this ttl)
		     (with-access::JsHandle this (handle)
			(nodejs-udp-set-ttl handle
			   (->fixnum (js-tointeger ttl %this) ))))
		  (js-function-arity 1 0)
		  (js-function-info :name "setTTL" :len 1))
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
	    
	    obj)))
   
   (define (get-udp-proto)
      (with-access::JsProcess process (udp-proto)
	 (unless udp-proto
	    (set! udp-proto (create-udp-proto)))
	 udp-proto))
   
   (define (udp-wrap hdl)
      (with-access::JsGlobalObject %this (js-object js-udp-cmap)
	 (with-access::JsProcess process (js-udp)
	    (unless js-udp-cmap
	       (set! js-udp-cmap (js-make-jsconstructmap :ctor js-udp)))
	    (with-access::JsFunction js-udp (constrsize)
	       (let ((obj (instantiateJsHandle
			     (handle hdl)
			     (__proto__ (get-udp-proto))
			     (cmap js-udp-cmap)
			     (elements ($create-vector constrsize)))))
		  (js-bind! %this obj (& "fd")
		     :get (js-make-function %this
			     (lambda (this)
				(nodejs-stream-fd %worker hdl))
			     (js-function-arity 0 0)
			     (js-function-info :name "GetFD" :len 0))
		     :writable #f :configurable #f)
		  obj)))))
   
   (define (UDP this)
      (udp-wrap (nodejs-udp-handle %worker)))

   (set! __js_strings (&init!))
   
   (with-access::JsGlobalObject %this (js-object)
      (with-access::JsProcess process (js-udp)
	 (let ((obj (js-new %this js-object)))
	    (set! js-udp
	       (js-make-function %this UDP
		  (js-function-arity 0 0)
		  (js-function-info :name "UDP" :len 0)
		  :alloc (lambda (%this o) #unspecified)
		  :prototype (get-udp-proto)))
	    (js-put! obj (& "UDP") js-udp #f %this)
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
	 (js-put! req (& "buffer") buffer #f %this)
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
				 (let ((oncomp (js-get req (& "oncomplete") %this)))
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
			 (js-put! process (& "_errno") (nodejs-err-name r) #f %this)
			 (js-undefined)))))))))

;*---------------------------------------------------------------------*/
;*    udp-recv-start ...                                               */
;*---------------------------------------------------------------------*/
(define (udp-recv-start %worker %this process this)
   (with-access::JsHandle this (handle)
      (with-access::JsProcess process (alloc slab)
	 (let ((r (nodejs-udp-recv-start %worker %this handle
		     alloc
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
			       (js-put! process (& "_errno") (nodejs-err-name len)
				  #f %this)
			       (let ((onmsg (js-get this (& "onmessage") %this)))
				  (!js-callback0 "recv-start" %worker %this
				     onmsg this)))
			      (else
			       ;; characters read
			       (let ((b (slab-shrink! slab buf offset len)))
				  (let ((onmsg (js-get this (& "onmessage") %this)))
				     (!js-callback5 "recv-start" %worker %this
					onmsg this this b offset len
					(js-alist->jsobject addr %this))
				     (js-undefined))))))))))
	    (if (=fx r 0)
		#t
		(begin
		   (js-put! process (& "_errno") (nodejs-err-name r) #f %this)
		   #f))))))
		

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)

