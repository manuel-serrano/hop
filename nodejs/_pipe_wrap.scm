;*=====================================================================*/
;*    /tmp/HOPNEW/hop/nodejs/_pipe_wrap.scm                            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Oct 19 07:19:20 2014                          */
;*    Last change :  Sun Feb 23 15:07:58 2020 (serrano)                */
;*    Copyright   :  2014-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Nodejs PIPE bindings                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__pipe-wrap

   (include "../hopscript/stringthread.sch")
   
   (library hopscript)

   (import  __nodejs_uv
	    __nodejs_process
	    __nodejs__buffer
	    __nodejs__stream-wrap)

   (include "nodejs_types.sch" "nodejs_async.sch")
   
   (export (process-pipe-wrap ::WorkerHopThread ::JsGlobalObject ::JsProcess ::obj)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

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

   (set! __js_strings (&init!))
   
   ;; close
   (js-put! pipe-prototype (& "close")
      (js-make-function %this
		  (lambda (this cb)
		     (nodejs-close %worker %this process this cb))
		  (js-function-arity 1 0)
		  (js-function-info :name "close" :len 1))
      #f %this)

   ;; unref
   (js-put! pipe-prototype (& "unref")
      (js-make-function %this
		  (lambda (this)
		     (with-access::JsHandle this (handle)
			(nodejs-unref handle %worker)))
		  (js-function-arity 0 0)
		  (js-function-info :name "unref" :len 0))
      #f %this)

   ;; ref
   (js-put! pipe-prototype (& "ref")
      (js-make-function %this
	 (lambda (this)
	    (with-access::JsHandle this (handle)
	       (nodejs-ref handle %worker)))
	 (js-function-arity 0 0)
	 (js-function-info :name "ref" :len 0))
      #f %this)

   ;; readStart
   (js-put! pipe-prototype (& "readStart")
      (js-make-function %this
	 (lambda (this)
	    (stream-read-start %worker %this process slab this))
	 (js-function-arity 0 0)
	 (js-function-info :name "readStart" :len 0))
      #f %this)
   
   ;; readStop
   (js-put! pipe-prototype (& "readStop")
      (js-make-function %this
	 (lambda (this)
	    (stream-read-stop %worker %this this))
	 (js-function-arity 0 0)
	 (js-function-info :name "readStop" :len 0))
      #f %this)
   
   ;; shutdown
   (js-put! pipe-prototype (& "shutdown")
      (js-make-function %this 
	 (lambda (this)
	    (stream-shutdown %worker %this process this))
	 (js-function-arity 0 0)
	 (js-function-info :name "shutdown" :len 0))
      #f %this)
   
   ;; writeBuffer
   (js-put! pipe-prototype (& "writeBuffer")
      (js-make-function %this
	 (lambda (this buffer)
	    (stream-write-buffer %worker %this process this buffer))
	 (js-function-arity 1 0)
	 (js-function-info :name "writeBuffer" :len 1))
      #f %this)
   
   ;; writeAsciiString
   (js-put! pipe-prototype (& "writeAsciiString")
      (js-make-function %this 
	 (lambda (this string handle)
	    (stream-write-string %worker %this process this
	       (js-jsstring->string string) 0 (js-jsstring-lengthfx string)
	       "ascii" #f handle))
	 (js-function-arity 2 0)
	 (js-function-info :name "writeAsciiString" :len 2))
      #f %this)
   
   ;; writeUtf8String
   (js-put! pipe-prototype (& "writeUtf8String")
      (js-make-function %this
	 (lambda (this string handle)
	    (stream-write-string %worker %this process this
	       (js-jsstring->string string) 0 (js-jsstring-lengthfx string)
	       "utf8" #f handle))
	 (js-function-arity 2 0)
	 (js-function-info :name "writeUtf8String" :len 2))
      #f %this)
   
   ;; writeUcs2String
   (js-put! pipe-prototype (& "writeUcs2String")
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
   
   ;; bind
   (js-put! pipe-prototype (& "bind")
      (js-make-function %this 
	 (lambda (this name)
	    (with-access::JsHandle this (handle)
	       (nodejs-pipe-bind %this process handle name)))
	 (js-function-arity 1 0)
	 (js-function-info :name "bind" :len 1))
      #f %this)
   
   ;; listen
   (js-put! pipe-prototype (& "listen")
      (js-make-function %this
		  (lambda (this backlog)
		     (with-access::JsHandle this (handle)
			(nodejs-pipe-listen %worker %this process this handle
			   (->fixnum (js-tointeger backlog %this)))))
		  (js-function-arity 1 0)
		  (js-function-info :name "listen" :len 1))
      #f %this)
   
   ;; connect
   (js-put! pipe-prototype (& "connect")
      (js-make-function %this 
	 (lambda (this name callback)
	    (with-access::JsGlobalObject %this (js-object)
	       (with-access::JsHandle this (handle)
		  (let ((req (js-new %this js-object)))
		     (nodejs-pipe-connect %worker %this handle name
			(lambda (status handle)
			   (when (<fx status 0)
			      (js-put! process (& "_errno")
				 (nodejs-err-name status) #f %this))
			   (let ((oncomp (js-get req (& "oncomplete") %this)))
			      (!js-callback5 'connect %worker %this
				 oncomp req status this req #t #t)
			      (js-undefined))))
		     req))))
	 (js-function-arity 2 0)
	 (js-function-info :name "connect" :len 2))
      #f %this)
   
   ;; open
   (js-put! pipe-prototype (& "open")
      (js-make-function %this 
	 (lambda (this fd)
	    (with-access::JsHandle this (handle)
	       (nodejs-pipe-open %worker %this handle fd)))
	 (js-function-arity 1 0)
	 (js-function-info :name "open" :len 1))
      #f %this)

   ;; pipe
   (define (pipe this #!optional val)
      (with-access::JsGlobalObject %this (js-object js-new-target)
	 (if (eq? js-new-target (js-undefined))
	     (js-raise-type-error %this
		"Pipe can only be used as a constructor" this)
	     (set! js-new-target (js-undefined)))
	 (let* ((hdl (nodejs-new-pipe %worker val))
		(obj (instantiateJsHandle
			(handle hdl)
			(__proto__ pipe-prototype)
			(cmap (instantiate::JsConstructMap))
			(elements ($create-vector 1)))))
	    ;; fd
	    (js-bind! %this obj (& "fd")
	       :get (js-make-function %this
		       (lambda (this)
			  (with-access::JsHandle this (handle)
			     (nodejs-stream-fd %worker handle)))
		       (js-function-arity 0 0)
		       (js-function-info :name "getGD" :len 0))
	       :writable #f :configurable #f)
	    ;; writeQueueSize
	    (js-put! obj (& "writeQueueSize")
	       (nodejs-stream-write-queue-size hdl) #f %this)
	    obj)))
   
   (with-access::JsGlobalObject %this (js-object)
      (with-access::JsProcess process (js-pipe)
	 (let ((obj (js-new %this js-object)))
	    (set! js-pipe
	       (js-make-function %this pipe
		  (js-function-arity 0 1 'optional)
		  (js-function-info :name "Pipe" :len 1)
		  :prototype pipe-prototype
		  :alloc js-no-alloc/new-target))
	    (js-put! obj (& "Pipe") js-pipe #t %this)
	    obj))))
;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)

