;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/worker.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr  3 11:39:41 2014                          */
;*    Last change :  Wed May 25 08:52:13 2016 (serrano)                */
;*    Copyright   :  2014-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript worker threads.              */
;*    -------------------------------------------------------------    */
;*    Not part of ECMAScript 5.                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_worker
   
   (library web hop js2scheme)
   
   (include "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_function
	   __hopscript_error
	   __hopscript_array)

   (static (class WorkerException::&exception
	      exn))
   
   (export (js-init-worker! ::JsGlobalObject)
	   (js-worker-construct ::JsGlobalObject ::procedure)
	   
	   (js-init-main-worker!::WorkerHopThread ::JsGlobalObject ::bool ::procedure)
	   (js-current-worker::WorkerHopThread)

	   (js-worker-load::procedure)
	   (js-worker-load-set! ::procedure)

	   (generic js-worker-loop ::object)
	   (generic js-worker-tick ::object)
	   (generic js-worker-exception-handler ::object ::obj ::int)
	   (generic js-worker-exec ::object ::bstring ::procedure)
	   (generic js-worker-push-thunk! ::object ::bstring ::procedure)
	   (generic js-worker-alive? ::object)
	   
	   (generic js-worker-terminate! ::object ::obj)
	   (generic js-worker-self-terminate! ::JsWorker ::obj)
	   (generic js-worker-post-slave-message ::JsWorker ::obj)
	   (generic js-worker-post-master-message ::JsWorker ::obj)
	   (generic js-worker-add-handler! ::object ::JsFunction)
	   (generic js-worker-remove-handler! ::object ::JsFunction)))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-begin!)

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsWorker ...                                 */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsWorker
   (lambda (o)
      (js-raise-type-error (js-initial-global-object)
	 "[[SerializeTypeError]] ~a" o))
   (lambda (o) o))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsWorker ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-donate o::JsWorker worker %this)
   (js-undefined))
		  
;*---------------------------------------------------------------------*/
;*    js-valueof ::JsWorker ...                                        */
;*---------------------------------------------------------------------*/
(define-method (js-valueof this::JsWorker %this)
   (with-access::JsWorker this (thread)
      thread))

;*---------------------------------------------------------------------*/
;*    js-init-worker! ...                                              */
;*---------------------------------------------------------------------*/
(define (js-init-worker! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (__proto__ js-worker js-worker-prototype
					 js-function)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 (define (%js-worker %this)
	    (with-access::JsGlobalObject %this (js-worker)
	       (lambda (this proc)
		  (js-new %this js-worker proc))))

	 ;; first, create the builtin prototype
	 (set! js-worker-prototype
	    (instantiate::JsWorker
	       (__proto__ __proto__)
	       (extensible #t)))
	 
	 ;; then, Create a HopScript worker object constructor
	 (set! js-worker
	    (js-make-function %this (%js-worker %this) 2 'Worker
	       :__proto__ js-function-prototype
	       :prototype js-worker-prototype
	       :construct (js-worker-construct %this (js-worker-load))))

	 ;; prototype properties
	 (init-builtin-worker-prototype! %this js-worker js-worker-prototype)
	 
	 ;; bind Worker in the global object
	 (js-bind! %this %this 'Worker
	    :configurable #f :enumerable #f :value js-worker)
	 js-worker)))

;*---------------------------------------------------------------------*/
;*    js-worker-construct ...                                          */
;*---------------------------------------------------------------------*/
(define (js-worker-construct %this loader)
   
   (define (remove-subworker! parent thread)
      (with-access::WorkerHopThread parent (mutex condv subworkers)
	 (synchronize mutex
	    (set! subworkers (delete! thread subworkers))
	    (condition-variable-signal! condv))))
   
   (define (add-subworker! parent thread)
      (with-access::WorkerHopThread parent (mutex subworkers)
	 (synchronize mutex
	    (set! subworkers (cons thread subworkers)))))
   
   (define (bind-worker-methods! %this scope worker)
      ;; postMessage
      (js-bind! %this scope 'postMessage
	 :value (js-make-function %this
		   (lambda (this data)
		      (js-worker-post-slave-message worker data))
		   1 'postMessage)
	 :writable #f
	 :configurable #f
	 :enumerable #f)
      
      ;; close
      (js-bind! %this scope 'close
	 :value (js-make-function %this
		   (lambda (this)
		      (js-worker-self-terminate! worker #f))
		   0 'close)
	 :writable #f
	 :configurable #f
	 :enumerable #f)
      
      (with-access::JsWorker worker (thread)
	 ;; onmessage
	 (js-bind! %this %this 'onmessage
	    :get (js-make-function %this
		    (lambda (this)
		       (with-access::WorkerHopThread thread (onmessage)
			  onmessage))
		    0 'onmessage)
	    :set (js-make-function %this
		    (lambda (this v)
		       (with-access::WorkerHopThread thread (onmessage keep-alive)
			  (set! keep-alive #t)
			  (set! onmessage v)))
		    1 'onmessage)
	    :configurable #f
	    :writable #t
	    :enumerable #t)
	 
;* 	 ;; onexit                                                     */
;* 	 (js-bind! %this %this 'onexit                                 */
;* 	    :get (js-make-function %this                               */
;* 		    (lambda (this)                                     */
;* 		       (with-access::WorkerHopThread thread (onexit)   */
;* 			  onexit))                                     */
;* 		    0 'onexit)                                         */
;* 	    :set (js-make-function %this                               */
;* 		    (lambda (this v)                                   */
;* 		       (with-access::WorkerHopThread thread (onexit)   */
;* 			  (set! onexit v)))                            */
;* 		    1 'onexit)                                         */
;* 	    :writable #t                                               */
;* 	    :configurable #t                                           */
;* 	    :enumerable #t)                                            */
	 ))
   
   (lambda (_ src)
      (with-access::JsGlobalObject %this (js-worker js-worker-prototype js-object)
	 (letrec* ((parent (js-current-worker))
		   (this (%global-constructor))
		   (source (js-tostring src %this))
		   (thunk (lambda ()
			     (js-put! this 'module
				(js-get %this 'module %this) #f this)
			     (loader source thread this)))
		   (thread (instantiate::WorkerHopThread
			      (parent parent)
			      (tqueue (list (cons "init" thunk)))
			      (%this this)
			      (keep-alive #f)
			      (module-cache (js-new0 %this js-object))
			      (body (lambda ()
				       (js-worker-loop thread)))
			      (cleanup (lambda (thread)
					  (when (isa? parent WorkerHopThread)
					     (remove-subworker! parent thread)))))))

	    ;; add the worker to the parent list
	    (when (isa? parent WorkerHopThread)
	       (add-subworker! parent thread))
	    
	    ;; start the worker thread
	    (thread-start! thread)
	       
	    ;; create the worker object
	    (let ((worker (instantiate::JsWorker
			     (__proto__ js-worker-prototype)
			     (extensible #t)
			     (thread thread))))
	       (with-access::WorkerHopThread thread (prehook)
		  (set! prehook
		     (lambda (%this this scope mod)
			(bind-worker-methods! %this scope worker))))
	       
	       ;; master onmessage and onexit
	       (let ((onmessage (js-undefined))
		     (onexit (js-undefined)))
		  (js-bind! %this worker 'onmessage
		     :get (js-make-function %this
			     (lambda (this) onmessage)
			     0 'onmessage)
		     :set (js-make-function %this
			     (lambda (this v)
				(set! onmessage v)
				(add-event-listener! this "message"
				   (lambda (this e)
				      (js-call1 %this v this e))))
			     2 'onmessage)
		     :configurable #t
		     :enumerable #t)
		  (js-bind! %this worker 'onexit
		     :get (js-make-function %this
			     (lambda (this) onexit)
			     0 'onexit)
		     :set (js-make-function %this
			     (lambda (this v)
				(set! onexit v)
				(add-event-listener! this "exit"
				   (lambda (this e)
				      (js-call1 %this v this e))))
			     2 'onexit)
		     :configurable #t
		     :enumerable #t))
	       
	       ;; return the newly created worker
	       worker)))))

;*---------------------------------------------------------------------*/
;*    init-builtin-worker-prototype! ...                               */
;*---------------------------------------------------------------------*/
(define (init-builtin-worker-prototype! %this js-worker obj)
   ;; prototype fields
   (js-bind! %this obj 'constructor
      :value js-worker
      :enumerable #f)
   ;; toString
   (js-bind! %this obj 'toString
      :value (js-make-function %this
		(lambda (this) (js-string->jsstring "[object Worker]"))
		0 'toString)
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; postMessage
   (js-bind! %this obj 'postMessage
      :value (js-make-function %this
		(lambda (this::JsWorker data)
		   (with-access::JsWorker this (thread)
		      (js-worker-post-master-message this data)))
		1 'postMessage)
      :writable #f
      :configurable #t
      :enumerable #f)
   ;; terminate
   (js-bind! %this obj 'terminate
      :value (js-make-function %this
		(lambda (this::JsWorker)
		   (with-access::JsWorker this (thread)
		      (js-worker-terminate! thread #f)))
		1 'terminate)
      :writable #f
      :enumerable #t
      :configurable #f))

;*---------------------------------------------------------------------*/
;*    js-worker-self-terminate ::WorkerHopThread ...                   */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-self-terminate! worker::JsWorker pred)
   (with-access::JsWorker worker (thread)
      (with-access::WorkerHopThread thread (subworkers)
	 (for-each (lambda (w)
		      (js-worker-terminate! w pred))
	    subworkers))
      (js-worker-terminate! thread pred)))
   
;*---------------------------------------------------------------------*/
;*    js-worker-post-slave-message ::WorkerHopThread ...               */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-post-slave-message worker::JsWorker data)
   (with-access::JsWorker worker (thread)
      (with-access::WorkerHopThread thread (parent listeners %this)
	 (when (isa? parent WorkerHopThread)
	    (let ((e (instantiate::MessageEvent
			(name "message")
			(target worker)
			(data (js-donate data parent %this)))))
	       (js-worker-push-thunk! parent "post-slave-message"
		  (lambda ()
		     (apply-listeners listeners e))))))))

;*---------------------------------------------------------------------*/
;*    js-worker-post-master-message ::WorkerHopThread ...              */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-post-master-message this::JsWorker data)
   (with-access::JsWorker this (thread)
      (with-access::WorkerHopThread thread (onmessage %this)
	 (let ((e (instantiate::MessageEvent
		     (name "message")
		     (target this)
		     (data (js-donate data thread %this)))))
	    (js-worker-push-thunk! thread "post-master-message"
	       (lambda ()
		  (when (isa? onmessage JsFunction)
		     (js-call1 %this onmessage this e))))))))

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::JsWorker ...                               */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! obj::JsWorker evt proc . capture)
   (cond
      ((string=? evt "message")
       (with-access::JsWorker obj (thread)
	  (with-access::WorkerHopThread thread (mutex listeners)
	     (synchronize mutex
		(set! listeners
		   (cons (lambda (e) (proc obj e)) listeners))))))
      ((string=? evt "exit")
       (with-access::JsWorker obj (thread)
	  (with-access::WorkerHopThread thread (mutex exitlisteners)
	     (synchronize mutex
		(set! exitlisteners
		   (cons (lambda (e) (proc obj e)) exitlisteners))))))
      (else
       (call-next-method))))

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::WorkerHopThread ...                        */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! obj::WorkerHopThread evt proc . capture)
   (if (string=? evt "message")
       (with-access::WorkerHopThread obj (mutex listeners)
	  (synchronize mutex
	     (set! listeners
		(cons (lambda (e) (proc obj e)) listeners))))
       (call-next-method)))

;*---------------------------------------------------------------------*/
;*    remove-event-listener! ::WorkerHopThread ...                     */
;*---------------------------------------------------------------------*/
(define-method (remove-event-listener! obj::WorkerHopThread evt proc . capture)
   #t)

;*---------------------------------------------------------------------*/
;*    default-worker-load ...                                          */
;*---------------------------------------------------------------------*/
(define (default-worker-load filename worker %this::JsGlobalObject)
   (loading-file-set! filename)
   (tprint "DEFAULT-WORKER-LOAD " filename)
   (let ((exprs (call-with-input-file filename
		   (lambda (in) (j2s-compile in :main #f :%this %this)))))
      (let ((m (eval-module))
	    (jsmodule #f))
	 (unwind-protect
	    ;; eval the compile module in the current environment
	    (for-each (lambda (e) (set! jsmodule (eval e))) exprs)
	    ;; restore the previous module
	    (eval-module-set! m)))))

;*---------------------------------------------------------------------*/
;*    js-worker-load ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter js-worker-load default-worker-load)

;*---------------------------------------------------------------------*/
;*    js-worker-add-handler! ::object ...                              */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-add-handler! obj::object fun)
   (with-access::WorkerHopThread obj (handlers)
      (set! handlers (cons fun handlers))))

;*---------------------------------------------------------------------*/
;*    js-worker-remove-handler! ::object ...                           */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-remove-handler! obj::object fun)
   (with-access::WorkerHopThread obj (handlers)
      (set! handlers (remq! fun handlers))))

;*---------------------------------------------------------------------*/
;*    %worker ...                                                      */
;*---------------------------------------------------------------------*/
(define %worker #f)
(define %global-constructor js-new-global-object)

;*---------------------------------------------------------------------*/
;*    js-current-worker ...                                            */
;*---------------------------------------------------------------------*/
(define (js-current-worker::WorkerHopThread)
   (let ((th (current-thread)))
      (if (isa? th WorkerHopThread)
	  th
	  %worker)))

;*---------------------------------------------------------------------*/
;*    js-init-main-worker! ...                                         */
;*    -------------------------------------------------------------    */
;*    Start the initial WorkerHopThread                                */
;*---------------------------------------------------------------------*/
(define (js-init-main-worker! %this::JsGlobalObject keep-alive ctor)
   (unless %worker
      (set! %global-constructor ctor)
      (with-access::JsGlobalObject %this (js-object)
	 (set! %worker
	    (instantiate::WorkerHopThread
	       (name "%worker")
	       (%this %this)
	       (onexit #f)
	       (keep-alive keep-alive)
	       (module-cache (js-new0 %this js-object))
	       (body (lambda () (js-worker-loop %worker)))))))
   %worker)

;*---------------------------------------------------------------------*/
;*    js-worker-exception-handler ...                                  */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-exception-handler th::object exn errval)
   (with-access::WorkerHopThread th (handlers %this %process keep-alive)
      (if (pair? handlers)
	  (let loop ((handlers (reverse handlers)))
	     (cond
		((null? handlers)
		 (exception-notify exn)
		 errval)
		((js-totest (js-call1 %this (car handlers) %process exn))
		 0)
		(else
		 (loop (cdr handlers)))))
	  (begin
	     (exception-notify exn)
	     errval))))

;*---------------------------------------------------------------------*/
;*    js-worker-tick ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-tick th::object)
   (with-access::WorkerHopThread th (%loop %process %retval
				       tqueue subworkers call
				       state mutex condv alivep)
      (tprint "THIS CODE SHOULD NOT BE EXECUTED (never tested)")
      (let loop ()
	 (let ((nthunk (synchronize mutex
			  (let liip ()
			     (cond
				((pair? tqueue)
				 (let ((nthunk (car tqueue)))
				    (set! tqueue (cdr tqueue))
				    nthunk))
				((and (eq? state 'terminated)
				      (or (not alivep) (not (alivep)))
				      (null? subworkers))
				 #f)
				(else
				 (condition-variable-wait! condv mutex)
				 (liip)))))))
	    (when (pair? nthunk)
	       (with-trace 'hopscript-worker (car nthunk)
		  (call (cdr nthunk)))
	       (loop))))))
   
;*---------------------------------------------------------------------*/
;*    js-worker-loop ...                                               */
;*    -------------------------------------------------------------    */
;*    This code is used only when HopScript is run without LIBUV.      */
;*    Otherwise, the LIBUV binding (see nodejs directory),             */
;*    overwrites this definition.                                      */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-loop th::object)
   (with-access::WorkerHopThread th (state subworkers name %this call mutex condv)
      (synchronize mutex
	 (condition-variable-broadcast! condv)
	 (tprint "THIS CODE SHOULD NOT BE EXECUTED")
	 ;; install the signal handler for that thread
	 (signal sigsegv
	    (lambda (x)
	       (js-raise-range-error %this
		  "Maximum call stack size exceeded"
		  #f)))
	 ;; loop unless terminated
	 (with-handler
	    (lambda (exn)
	       (js-worker-exception-handler th exn 1))
	    (unwind-protect
	       (js-worker-tick th)
	       (set! state 'terminated))
	    #t))))

;*---------------------------------------------------------------------*/
;*    js-worker-exec ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-exec th::object name::bstring thunk::procedure)
   (if (and (eq? (current-thread) th)
	    (with-access::WorkerHopThread th (tqueue)
	       (null? tqueue)))
       (thunk)
       (let ((response #f)
	     (mutex (make-mutex))
	     (condv (make-condition-variable)))
	  (synchronize mutex
	     (js-worker-push-thunk! th name
		(lambda ()
		   (set! response
		      (with-handler
			 (lambda (e)
			    (instantiate::WorkerException
			       (exn e)))
			 (thunk)))
		   (synchronize mutex
		      (condition-variable-signal! condv))))
	     (condition-variable-wait! condv mutex)
	     (if (isa? response WorkerException)
		 (with-access::WorkerException response (exn)
		    (raise exn))
		 response)))))

;*---------------------------------------------------------------------*/
;*    js-worker-push-thunk! ::object ...                               */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-push-thunk! th::object name::bstring thunk::procedure)
   (tprint "THIS CODE SHOULD NOT BE EXECUTED")
   (with-access::WorkerHopThread th (mutex condv tqueue)
      (synchronize mutex
	 (with-trace 'hopscript-worker "js-worker-push-thunk"
	    (trace-item "name=" name)
	    (trace-item "queue=" (map car tqueue)))
	 (set! tqueue (append! tqueue (list (cons name thunk))))
	 (condition-variable-signal! condv))))

;*---------------------------------------------------------------------*/
;*    js-worker-alive? ::object ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-alive? th::object)
   (tprint "THIS CODE SHOULD NOT BE EXECUTED")
   (with-access::WorkerHopThread th (tqueue state)
      (and (not (eq? state 'terminated)) (pair? tqueue))))

;*---------------------------------------------------------------------*/
;*    js-worker-terminate! ::WorkerHopThread ...                       */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-terminate! th::object pred)
   (tprint "THIS CODE SHOULD NOT BE EXECUTED")
   (with-access::WorkerHopThread th (state mutex condv alivep)
      (set! alivep pred)
      (synchronize mutex
	 (set! state 'terminated)
	 (condition-variable-signal! condv))))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral end                                              */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-end!)

