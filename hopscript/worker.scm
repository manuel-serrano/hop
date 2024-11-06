;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/worker.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr  3 11:39:41 2014                          */
;*    Last change :  Wed Nov  6 07:54:28 2024 (serrano)                */
;*    Copyright   :  2014-24 Manuel Serrano                            */
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
   
   (include "types.sch" "stringliteral.sch" "function.sch")
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_object
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_lib
	   __hopscript_function
	   __hopscript_error
	   __hopscript_array)
   
   (static (class WorkerException::&exception
	      exn))
   
   (export (js-init-worker! ::JsGlobalObject)
	   (js-worker-construct ::JsGlobalObject ::procedure)
	   
 	   (js-main-worker!::WorkerHopThread ::bstring ::bstring ::bool ::procedure ::procedure #!key (autostart #t))
 	   (js-main-no-worker!::WorkerHopThread ::bstring ::bstring ::bool ::procedure ::procedure)
	   (js-current-worker::WorkerHopThread)
	   (js-main-worker?::bool ::WorkerHopThread)
	   
	   (js-worker-load::procedure)
	   (js-worker-load-set! ::procedure)

	   (generic js-worker-start! worker::WorkerHopThread)
	   (generic js-worker-loop ::object ::procedure)
	   (generic js-worker-tick ::object)
	   (generic js-worker-exception-handler ::object ::obj ::int)
	   (generic js-worker-exec ::object ::bstring ::procedure)
	   (generic js-worker-exec-promise ::object ::bstring ::procedure ::procedure ::procedure)
	   (generic js-worker-exec-throws ::object ::bstring ::procedure)
	   (generic js-worker-run ::object ::bstring ::procedure)
	   (generic js-worker-run-throws ::object ::bstring ::procedure)
	   (generic js-worker-push! ::object ::bstring ::procedure)
	   (generic js-worker-alive? ::object)
	   
	   (generic js-worker-terminate! ::object ::obj)
	   (generic js-worker-self-terminate! ::JsWorker ::obj)
	   (generic js-worker-post-slave-message ::JsWorker ::obj)
	   (generic js-worker-post-master-message ::JsWorker ::obj)
	   (generic js-worker-add-handler! ::object ::JsFunction)
	   (generic js-worker-remove-handler! ::object ::JsFunction)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsWorker ...                                 */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsWorker
   (lambda (o ctx)
      (if (isa? ctx JsGlobalObject)
	  (js-raise-type-error ctx "[[SerializeTypeError]] ~a" o)
	  (error "obj->string" "Not a JavaScript context" ctx)))
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
   (with-access::JsGlobalObject %this (js-worker js-worker-prototype
					 js-function)
      
      ;; create the builder
      (define %js-worker
	 (js-worker-construct %this (js-worker-load)))
      
      ;; local constant strings
      (unless (vector? __js_strings) (set! __js_strings (&init!)))
      
      ;; create the builtin prototype
      (set! js-worker-prototype
	 (instantiateJsWorker
	    (__proto__ (js-object-proto %this))))

      ;; then, Create a HopScript worker object constructor
      (set! js-worker
	 (js-make-function %this %js-worker
	    (js-function-arity 1 0)
	    (js-function-info :name "Worker" :len 2)
	    :__proto__ (js-object-proto js-function)
	    :prototype js-worker-prototype
	    :alloc js-no-alloc))
      
      ;; prototype properties
      (init-builtin-worker-prototype! %this js-worker js-worker-prototype)
      
      ;; bind Worker in the global object
      (js-bind! %this %this (& "Worker")
	 :configurable #f :enumerable #f :value js-worker
	 :hidden-class #t)
      js-worker))

;*---------------------------------------------------------------------*/
;*    js-worker-construct ...                                          */
;*---------------------------------------------------------------------*/
(define (js-worker-construct %this loader)
   
   (define (remove-subworker! parent thread)
      (with-access::WorkerHopThread parent (mutex condv subworkers)
	 (synchronize mutex
	    (set! subworkers (delete! thread subworkers)))
	 (js-worker-push! parent "worker cleanup"
	    (lambda (%this)
	       'tick))))
   
   (define (add-subworker! parent thread)
      (with-access::WorkerHopThread parent (mutex subworkers)
	 (synchronize mutex
	    (set! subworkers (cons thread subworkers)))))
   
   (define (bind-worker-methods! %this scope worker)
      ;; postMessage
      (js-bind! %this scope (& "postMessage")
	 :value (js-make-function %this
		   (lambda (this data)
		      (js-worker-post-slave-message worker data))
		   (js-function-arity 1 0)
		   (js-function-info :name "postMessage" :len 1))
	 :writable #f
	 :configurable #f
	 :enumerable #f
	 :hidden-class #t)
      
      ;; close
      (js-bind! %this scope (& "close")
	 :value (js-make-function %this
		   (lambda (this)
		      (js-worker-self-terminate! worker #f))
		   (js-function-arity 0 0)
		   (js-function-info :name "close" :len 0))
	 :writable #f
	 :configurable #f
	 :enumerable #f
	 :hidden-class #t)
      
      (with-access::JsWorker worker (thread)
	 ;; onmessage
	 (js-bind! %this %this (& "onmessage")
	    :get (js-make-function %this
		    (lambda (this)
		       (with-access::WorkerHopThread thread (onmessage)
			  onmessage))
		    (js-function-arity 0 0)
		    (js-function-info :name "onmessage.set" :len 0))
	    :set (js-make-function %this
		    (lambda (this v)
		       (with-access::WorkerHopThread thread (onmessage keep-alive)
			  (set! keep-alive #t)
			  (set! onmessage v)))
		    (js-function-arity 1 0)
		    (js-function-info :name "onmessage.get" :len 1))
	    :configurable #f
	    :writable #t
	    :enumerable #t
	    :hidden-class #t)))

   (define (onexit th)
      (with-access::WorkerHopThread th (keep-alive parent exitlisteners)
	 (when (pair? exitlisteners)
	    (js-worker-push! parent "slave-terminate"
	       (lambda (%this)
		  (let ((e (instantiate::MessageEvent
			      (name "exit")
			      (target (js-undefined))
			      (data (js-undefined)))))
		     (apply-listeners exitlisteners e)))))))

   (lambda (_ src)
      (with-access::JsGlobalObject %this (js-worker-prototype js-object)
	 (letrec* ((parent (js-current-worker))
		   (source (js-tostring src %this))
		   (setup (lambda ()
			     ($js-init-worker-jsalloc)
			     (let ((this (%global-constructor
					    :name (string-append source "_w"))))
				;; store the worker in global object
				(with-access::JsGlobalObject this (worker js-object)
				   (set! worker thread)
				   (with-access::WorkerHopThread thread (%this module-cache)
				      (set! %this this)
				      (set! module-cache (js-new0 this js-object)))))))
		   (proc (lambda (thread)
			    (with-access::WorkerHopThread thread (%this)
			       (loader source thread %this))))
		   (mutex (make-mutex))
		   (condv (make-condition-variable))
		   (thname (gensym (string-append "WWorker@"
				    (prefix (basename (js-jsstring->string src))))))
		   (thread (instantiate::WorkerHopThread
			      (name thname)
			      (parent parent)
			      (mutex (if (isa? parent WorkerHopThread)
					 (with-access::WorkerHopThread parent (mutex)
					    mutex)
					 (make-mutex)))
			      (onexit (js-make-function %this
					 (lambda (this process retval)
					    (onexit thread))
					 (js-function-arity 2 0)
					 (js-function-info :name "onexit" :len 2)))
			      (keep-alive #f)
			      (resolver (when (isa? parent WorkerHopThread)
					   (with-access::WorkerHopThread parent (resolver)
					      resolver)))
			      (body (lambda ()
				       (setup)
				       (synchronize mutex
					  (condition-variable-broadcast! condv))
				       (js-worker-loop thread proc)))
			      (cleanup (lambda (thread)
					  (when (isa? parent WorkerHopThread)
					     (remove-subworker! parent thread)))))))
	    
	    ;; add the worker to the parent list
	    (when (isa? parent WorkerHopThread)
	       (add-subworker! parent thread))

	    ;; create the worker object
	    (let ((worker (instantiateJsWorker
			     (__proto__ js-worker-prototype)
			     (thread thread))))
	       (with-access::WorkerHopThread thread (prehook)
		  (set! prehook
		     (lambda (%this this scope mod)
			(bind-worker-methods! %this scope worker))))

	       ;; master onmessage and onexit
	       (let ((onmessage (js-undefined))
		     (onerror (js-undefined))
		     (onexit (js-undefined)))
		  (js-bind! %this worker (& "onmessage")
		     :get (js-make-function %this
			     (lambda (this) onmessage)
			     (js-function-arity 0 0)
			     (js-function-info :name "onmessage.get" :len 0))
		     :set (js-make-function %this
			     (lambda (this v)
				(set! onmessage v)
				(add-event-listener! this "message"
				   (lambda (this e)
				      (js-call1 %this v this e))))
			     (js-function-arity 1 0)
			     (js-function-info :name "onmessage.set" :len 1))
		     :configurable #t
		     :enumerable #t
		     :hidden-class #t)
		  (js-bind! %this worker (& "onerror")
		     :get (js-make-function %this
			     (lambda (this) onerror)
			     (js-function-arity 0 0)
			     (js-function-info :name "onerror.get" :len 0))
		     :set (js-make-function %this
			     (lambda (this v)
				(set! onerror v)
				(add-event-listener! this "error"
				   (lambda (this e)
				      (js-call0 %this v this))))
			     (js-function-arity 1 0)
			     (js-function-info :name "onerror.set" :len 1))
		     :configurable #t
		     :enumerable #t
		     :hidden-class #t)
		  (js-bind! %this worker (& "onexit")
		     :get (js-make-function %this
			     (lambda (this) onexit)
			     (js-function-arity 0 0)
			     (js-function-info :name "onexit.get" :len 0))
		     :set (js-make-function %this
			     (lambda (this v)
				(set! onexit v)
				(add-event-listener! this "exit"
				   (lambda (this e)
				      (js-call1 %this v this e))))
			     (js-function-arity 1 0)
			     (js-function-info :name "onexit.set" :len 1))
		     :configurable #t
		     :enumerable #t
		     :hidden-class #t))

	       ;; prepare the worker start
	       (js-worker-push! (js-current-worker)  "worker"
		  (lambda (%this)
		     (synchronize mutex
			(thread-start! thread))))
	       
	       ;; return the newly created worker
	       worker)))))

;*---------------------------------------------------------------------*/
;*    init-builtin-worker-prototype! ...                               */
;*---------------------------------------------------------------------*/
(define (init-builtin-worker-prototype! %this js-worker obj)
   ;; prototype fields
   (js-bind! %this obj (& "constructor")
      :value js-worker
      :enumerable #f
      :hidden-class #t)
   ;; toString
   (js-bind! %this obj (& "toString")
      :value (js-make-function %this
		(lambda (this) (js-string->jsstring "[object Worker]"))
		(js-function-arity 0 0)
		(js-function-info :name "toString" :len 0))
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #t)
   ;; postMessage
   (js-bind! %this obj (& "postMessage")
      :value (js-make-function %this
		(lambda (this::JsWorker data)
		   (with-access::JsWorker this (thread)
		      (js-worker-post-master-message this data)))
		(js-function-arity 1 0)
		(js-function-info :name "postMessage" :len 1))
      :writable #f
      :configurable #t
      :enumerable #f
      :hidden-class #t)
   ;; terminate
   (js-bind! %this obj (& "terminate")
      :value (js-make-function %this
		(lambda (this::JsWorker)
		   (with-access::JsWorker this (thread)
		      (js-worker-terminate! thread #f)))
		(js-function-arity 0 0)
		(js-function-info :name "terminate" :len 0))
      :writable #f
      :enumerable #t
      :configurable #f
      :hidden-class #t))

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
;*    -------------------------------------------------------------    */
;*    slave posts a message to its parent.                             */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-post-slave-message worker::JsWorker data)
   (with-access::JsWorker worker (thread listener)
      ;; listener is the parent message listener, although stored in the worker
      (with-access::WorkerHopThread thread (parent %this listeners)
	 (when (isa? parent WorkerHopThread)
	    (with-access::WorkerHopThread parent (%this)
	       (js-worker-push! parent "post-slave-message"
		  (lambda (%this)
		     (let ((e (instantiate::MessageEvent
				 (name "message")
				 (target worker)
				 (data (js-donate data parent %this)))))
			(apply-listeners listeners e)))))))))

;*---------------------------------------------------------------------*/
;*    js-worker-thread-post-slave-error ...                            */
;*---------------------------------------------------------------------*/
(define (js-worker-thread-post-slave-error thread::WorkerHopThread data)
   (with-handler
      exception-notify 
      (with-access::WorkerHopThread thread (parent errorlisteners %this mutex)
	 (when (isa? parent WorkerHopThread)
	    (with-access::WorkerHopThread parent (%this)
	       (js-worker-push! parent "post-slave-message"
		  (lambda (%this)
		     (let ((e (instantiate::MessageEvent
				 (name "error")
				 (target parent)
				 (data (js-donate data parent %this)))))
			(when (pair? errorlisteners)
			   (apply-listeners errorlisteners e)))))
	       (exception-notify data))))))

;*---------------------------------------------------------------------*/
;*    js-worker-post-master-message ::WorkerHopThread ...              */
;*    -------------------------------------------------------------    */
;*    Master posts a message to a worker.                              */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-post-master-message this::JsWorker data)
   (with-access::JsWorker this (thread)
      (with-access::WorkerHopThread thread (onmessage %this)
	 (js-worker-push! thread "post-master-message"
	    (lambda (%this)
	       (when (js-procedure? onmessage)
		  (let ((e (instantiate::MessageEvent
			      (name "message")
			      (target this)
			      (data (js-donate data thread %this)))))
		     (js-call1 %this onmessage this e))))))))

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::JsWorker ...                               */
;*    -------------------------------------------------------------    */
;*    Add a listener to a master.                                      */
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
      ((string=? evt "error")
       (with-access::JsWorker obj (thread)
	  (with-access::WorkerHopThread thread (mutex errorlisteners)
	     (synchronize mutex
		(set! errorlisteners
		   (cons (lambda (e) (proc obj e)) errorlisteners))))))
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
   (let ((exprs (call-with-input-file filename
		   (lambda (in)
		      (j2s-compile in
			 :driver-name "default-worker-load"
			 :main #f
			 :%this %this)))))
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
(define %global #f)
(define %module #f)
(define %global-constructor js-new-global-object)

;*---------------------------------------------------------------------*/
;*    js-main-worker? ...                                              */
;*    -------------------------------------------------------------    */
;*    Returns #t iff worker is the main worker.                        */
;*---------------------------------------------------------------------*/
(define (js-main-worker? w)
   (eq? w %worker))

;*---------------------------------------------------------------------*/
;*    js-current-worker ...                                            */
;*---------------------------------------------------------------------*/
(define (js-current-worker::WorkerHopThread)
   (let ((th (current-thread)))
      (if (isa? th WorkerHopThread)
	  th
	  %worker)))

;*---------------------------------------------------------------------*/
;*    js-main-worker! ...                                              */
;*    -------------------------------------------------------------    */
;*    Start the initial WorkerHopThread                                */
;*---------------------------------------------------------------------*/
(define (js-main-worker! name path keep-alive ctor ctormod #!key (autostart #t))

   (define (setup-worker! %worker)
      (set! %global (ctor :name name))
      (with-access::JsGlobalObject %global (js-object worker)
	 (set! worker %worker)
	 (set! %module (ctormod (basename path) path %worker %global))
	 (with-access::WorkerHopThread %worker (%this module-cache %loop)
	    ;; module-cache is used in src/main to check
	    ;; where the worker is running or not
	    (set! module-cache (js-new0 %this js-object))
	    (set! %this %global)
	    (js-put! module-cache
	       (js-string->jsstring path) %module #f %this))))

   (unless %worker
      ($js-init-jsalloc (js-object-default-mode))
      (set! %global-constructor ctor)
      (let ((mutex (make-mutex))
	    (startv (make-condition-variable))
	    (condv (make-condition-variable)))
	 (synchronize mutex
	    (set! %worker
	       (instantiate::WorkerHopThread
		  (name (string-append "%worker@" name))
		  (onexit #f)
		  (keep-alive keep-alive)
		  (mutex mutex)
		  (condv condv)
		  (body (lambda ()
			   (setup-worker! %worker)
			   (synchronize mutex
			      (condition-variable-broadcast! startv)
			      (unless autostart
				 (condition-variable-wait! condv mutex)))
			   (js-worker-loop %worker (lambda (th) th))))))
	    (thread-start-joinable! %worker)
	    (condition-variable-wait! startv mutex))))
   
   (values %worker %global %module))

;*---------------------------------------------------------------------*/
;*    js-main-no-worker! ...                                           */
;*    -------------------------------------------------------------    */
;*    Initialize the main worker in js-no-worker mode                  */
;*---------------------------------------------------------------------*/
(define (js-main-no-worker! name path keep-alive ctor ctormod)
   
   (define (setup-worker! %worker)
      (set! %global (ctor :name name))
      (with-access::JsGlobalObject %global (js-object worker)
	 (set! worker %worker)
	 (set! %module (ctormod (basename path) path %worker %global))
	 (with-access::WorkerHopThread %worker (%this module-cache)
	    ;; module-cache is used in src/main to check
	    ;; where the worker is running or not
	    (set! module-cache (js-new0 %this js-object))
	    (set! %this %global)
	    (js-put! module-cache (js-string->jsstring path) %module #f %this))))
   
   (unless %worker
      ($js-init-jsalloc (js-object-default-mode))
      (set! %global-constructor ctor)
      (set! %worker
	 (instantiate::WorkerHopThread
	    (name (string-append "%worker@" name))
	    (onexit #f)
	    (keep-alive keep-alive)
	    (body (lambda ()
		     (error "js-main-no-worker"
			"Cannot execute main worker in --js-no-worker mode"
			#f)))))
      (setup-worker! %worker))
   
   (values %worker %global %module))

;*---------------------------------------------------------------------*/
;*    js-worker-exception-handler ...                                  */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-exception-handler th::object exn errval)
   (with-access::WorkerHopThread th (handlers %this %process keep-alive parent %exn)
      (if (pair? handlers)
	  (let loop ((handlers (reverse handlers)))
	     (cond
		((null? handlers)
		 (cond
		    (parent
		     (js-worker-thread-post-slave-error th exn))
		    ((isa? exn &exception)
		     (exception-notify exn))
		    ((isa? exn object)
		     (exception-notify exn))
		    ((eq? %exn #unspecified)
		     (exception-notify exn))
		    (else
		     (exception-notify %exn)))
		 errval)
		((js-totest (js-call1 %this (car handlers) %process exn))
		 0)
		(else
		 (loop (cdr handlers)))))
	  (begin
	     (exception-notify exn)
	     errval))))

;*---------------------------------------------------------------------*/
;*    js-worker-start! ::WorkerHopThread ...                           */
;*    -------------------------------------------------------------    */
;*    To be used when the worker has been created with :autostart #f   */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-start! worker::WorkerHopThread)
   (with-access::WorkerHopThread worker (condv mutex)
       (synchronize mutex
	  (condition-variable-broadcast! condv))))

;*---------------------------------------------------------------------*/
;*    js-worker-loop ...                                               */
;*    -------------------------------------------------------------    */
;*    This code is used only when HopScript is run without LIBUV.      */
;*    Otherwise, the LIBUV binding (see nodejs directory),             */
;*    overwrites this definition.                                      */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-loop th::object proc::procedure))

;*---------------------------------------------------------------------*/
;*    js-worker-tick ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-tick th::object))

;*---------------------------------------------------------------------*/
;*    js-worker-exec ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-exec th::object name::bstring proc::procedure))

;*---------------------------------------------------------------------*/
;*    js-worker-exec-promise ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-exec-promise th::object name::bstring proc::procedure res::procedure rej::procedure))

;*---------------------------------------------------------------------*/
;*    js-worker-exec-throws ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-exec-throws th::object name::bstring proc::procedure))

;*---------------------------------------------------------------------*/
;*    js-worker-run ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-run th::object name::bstring proc::procedure)
   (if (eq? (current-thread) th)
       (with-access::WorkerHopThread th (%this %loop mutex condv)
	  (proc %this))
       (js-worker-exec th name proc)))

;*---------------------------------------------------------------------*/
;*    js-worker-run-throws ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-run-throws th::object name::bstring proc::procedure)
   (if (eq? (current-thread) th)
       (with-access::WorkerHopThread th (%this %loop mutex condv)
	  (proc %this))
       (js-worker-exec-throws th name proc)))

;*---------------------------------------------------------------------*/
;*    js-worker-push! ::object ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-push! th::object name::bstring proc::procedure))

;*---------------------------------------------------------------------*/
;*    js-worker-alive? ::object ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-alive? th::object))

;*---------------------------------------------------------------------*/
;*    js-worker-terminate! ::WorkerHopThread ...                       */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-terminate! th::object pred))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
