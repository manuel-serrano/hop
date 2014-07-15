;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/worker.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr  3 11:39:41 2014                          */
;*    Last change :  Sun Jul 13 06:52:37 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
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
   
   (export (class MessageEvent::event
	      data::obj)
	   
	   (class WorkerHopThread::hopthread
	      (mutex::mutex read-only (default (make-mutex)))
	      (condv::condvar read-only (default (make-condition-variable)))
	      (tqueue::pair-nil (default '()))
	      (listeners::pair-nil (default '()))
	      (onmessage::obj (default (js-undefined)))
	      (%this::JsGlobalObject read-only)
	      (%process (default #f))
	      (state::symbol (default 'init))
	      (module-table::obj (default (make-hashtable)))
	      (module-mutex::obj (default (make-mutex)))
	      (parent::obj (default #f))
	      (subworkers::pair-nil (default '())))

	   (js-current-worker::WorkerHopThread)
	   (js-init-worker! ::JsGlobalObject)
	   (js-init-main-worker!::WorkerHopThread ::JsGlobalObject)

	   (js-worker-load::procedure)
	   (js-worker-load-set! ::procedure)

	   (generic js-worker-thread-loop ::WorkerHopThread)
	   (generic js-worker-exec ::WorkerHopThread ::procedure)
	   (generic js-worker-push-thunk! ::WorkerHopThread ::procedure)
	   (generic js-worker-terminate! ::WorkerHopThread)
	   (generic js-worker-post-slave-message ::JsWorker ::obj)
	   (generic js-worker-post-master-message ::JsWorker ::obj)))

;*---------------------------------------------------------------------*/
;*    %main ...                                                        */
;*---------------------------------------------------------------------*/
(define %main #f)

;*---------------------------------------------------------------------*/
;*    js-current-worker ...                                            */
;*---------------------------------------------------------------------*/
(define (js-current-worker::WorkerHopThread)
   (let ((th (current-thread)))
      (if (isa? th WorkerHopThread)
	  th
	  %main)))

;*---------------------------------------------------------------------*/
;*    js-init-main-worker! ...                                         */
;*    -------------------------------------------------------------    */
;*    Start the initial WorkerHopThread                                */
;*---------------------------------------------------------------------*/
(define (js-init-main-worker! %this::JsGlobalObject)
   (unless %main
      (set! %main
	 (instantiate::WorkerHopThread
	    (name "main")
	    (%this %this)
	    (body (lambda () (js-worker-thread-loop %main))))))
   %main)
   
;*---------------------------------------------------------------------*/
;*    js-init-worker! ...                                              */
;*---------------------------------------------------------------------*/
(define (js-init-worker! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (__proto__ js-worker js-worker-prototype
					 js-function)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 ;; first, create the builtin prototype
	 (set! js-worker-prototype
	    (instantiate::JsWorker
	       (__proto__ __proto__)
	       (extensible #t)))
	 
	 ;; then, Create a HopScript worker object constructor
	 (set! js-worker
	    (js-make-function %this (%js-worker %this) 2 'JsWorker
	       :__proto__ js-function-prototype
	       :prototype js-worker-prototype
	       :construct (js-worker-construct %this)))

	 ;; prototype properties
	 (init-builtin-worker-prototype! %this js-worker js-worker-prototype)
	 ;; bind Worker in the global object
	 (js-bind! %this %this 'Worker
	    :configurable #f :enumerable #f :value js-worker)
	 js-worker)))

;*---------------------------------------------------------------------*/
;*    js-worker-load ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter js-worker-load default-worker-load)

;*---------------------------------------------------------------------*/
;*    %js-worker ...                                                   */
;*---------------------------------------------------------------------*/
(define (%js-worker %this)
   (with-access::JsGlobalObject %this (js-worker)
      (lambda (this proc)
	 (js-new %this js-worker proc))))

;*---------------------------------------------------------------------*/
;*    js-worker-construct ...                                          */
;*---------------------------------------------------------------------*/
(define (js-worker-construct %this)

   (define (remove-subworker! parent thread)
      (with-access::WorkerHopThread parent (mutex condv subworkers)
	 (synchronize mutex
	    (set! subworkers (delete! thread subworkers))
	    (condition-variable-signal! condv))))

   (define (add-subworker! parent thread)
      (with-access::WorkerHopThread parent (mutex subworkers)
	 (synchronize mutex
	    (set! subworkers (cons thread subworkers)))))
   
   (lambda (_ src)
      (letrec* ((parent (js-current-worker))
		(this (js-new-global-object))
		(source (js-tostring src %this))
		(thunk (lambda ()
			  (js-put! this 'module (js-get %this 'module %this) #f this)
			  ((js-worker-load) source thread this)))
		(thread (instantiate::WorkerHopThread
			   (parent parent)
			   (tqueue (list thunk))
			   (%this this)
			   (body (lambda ()
				    (js-worker-thread-loop thread)))
			   (cleanup (lambda (thread)
				       (when (isa? parent WorkerHopThread)
					  (remove-subworker! parent thread)))))))

	 ;; add the worker to the parent list
	 (when (isa? parent WorkerHopThread)
	    (add-subworker! parent thread))

	 ;; create the worker object
	 (with-access::JsGlobalObject %this (js-worker js-worker-prototype)
	    (let ((worker (instantiate::JsWorker
			     (__proto__ js-worker-prototype)
			     (extensible #t)
			     (thread thread))))
	       
	       ;; master onmessage
	       (let ((onmessage (js-undefined)))
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
		     :enumerable #t))

	       ;; postMessage
	       (js-bind! this this 'postMessage
		  :value (js-make-function this
			    (lambda (this data)
			       (js-worker-post-slave-message worker data))
			    1 'postMessage)
		  :writable #f
		  :configurable #f
		  :enumerable #f)

	       ;; onmessage
	       (js-bind! this this 'onmessage
		  :get (js-make-function this
			  (lambda (this)
			     (with-access::WorkerHopThread thread (onmessage)
				onmessage))
			  0 'onmessage)
		  :set (js-make-function this
			  (lambda (this v)
			     (with-access::WorkerHopThread thread (onmessage)
				(set! onmessage v)))
			  2 'onmessage)
		  :configurable #t
		  :writable #f
		  :enumerable #t)
	       
	       ;; start the worker thread
	       (thread-start! thread)

	       ;; return the newly created worker
	       worker)))))

;*---------------------------------------------------------------------*/
;*    js-valueof ::JsWorker ...                                        */
;*---------------------------------------------------------------------*/
(define-method (js-valueof this::JsWorker %this)
   (with-access::JsWorker this (thread)
      thread))
   
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
		(lambda (this) "[object Worker]")
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
		      (js-worker-terminate! thread)))
		1 'terminate)
      :writable #f
      :enumerable #t
      :configurable #f))

;*---------------------------------------------------------------------*/
;*    js-worker-thread-loop ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-thread-loop th::WorkerHopThread)
   (with-access::WorkerHopThread th (mutex condv tqueue state subworkers name)
      ;; loop unless terminated
      (let loop ()
	 (let ((thunk (synchronize mutex
			 (let liip ()
			    (cond
			       ((pair? tqueue)
				(let ((thunk (car tqueue)))
				   (set! tqueue (cdr tqueue))
				   thunk))
			       ((and (eq? state 'terminated)
				     (null? subworkers))
				#f)
			       (else
				(condition-variable-wait! condv mutex)
				(liip)))))))
	    (when (procedure? thunk)
	       (thunk)
	       (loop))))))

;*---------------------------------------------------------------------*/
;*    js-worker-exec ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-exec th::WorkerHopThread thunk::procedure)
   (if (isa? (current-thread) WorkerHopThread)
       (thunk)
       (let ((response #f)
	     (mutex (make-mutex))
	     (condv (make-condition-variable)))
	  (synchronize mutex
	     (js-worker-push-thunk! th
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
;*    js-worker-push-thunk! ::WorkerHopThread ...                      */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-push-thunk! th::WorkerHopThread thunk::procedure)
   (with-access::WorkerHopThread th (mutex condv tqueue)
      (synchronize mutex
	 (set! tqueue (append! tqueue (list thunk)))
	 (condition-variable-signal! condv))))

;*---------------------------------------------------------------------*/
;*    js-worker-terminate! ::WorkerHopThread ...                       */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-terminate! th::WorkerHopThread)
   (with-access::WorkerHopThread th (state mutex condv)
      (synchronize mutex
	 (set! state 'terminated)
	 (condition-variable-signal! condv))))

;*---------------------------------------------------------------------*/
;*    js-worker-post-slave-message ::WorkerHopThread ...               */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-post-slave-message worker::JsWorker data)
   (with-access::JsWorker worker (thread)
      (with-access::WorkerHopThread thread (parent listeners)
	 (when (isa? parent WorkerHopThread)
	    (let ((e (instantiate::MessageEvent
			(name "message")
			(target worker)
			(data data))))
	       (js-worker-push-thunk! parent
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
		     (data data))))
	    (js-worker-push-thunk! thread
	       (lambda ()
		  (when (isa? onmessage JsFunction)
		     (js-call1 %this onmessage this e))))))))

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::WorkerHopThread ...                        */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! obj::JsWorker event proc . capture)
   (if (string=? event "message")
       (with-access::JsWorker obj (thread)
	  (with-access::WorkerHopThread thread (mutex listeners)
	     (synchronize mutex
		(set! listeners
		   (cons (lambda (e) (proc obj e)) listeners)))))
       (call-next-method)))

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::WorkerHopThread ...                        */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! obj::WorkerHopThread event proc . capture)
   (if (string=? event "message")
       (with-access::WorkerHopThread obj (mutex listeners)
	  (synchronize mutex
	     (set! listeners
		(cons (lambda (e) (proc obj e)) listeners))))
       (call-next-method)))

;*---------------------------------------------------------------------*/
;*    remove-event-listener! ::WorkerHopThread ...                     */
;*---------------------------------------------------------------------*/
(define-method (remove-event-listener! obj::WorkerHopThread event proc . capture)
   #t)

;*---------------------------------------------------------------------*/
;*    default-worker-load ...                                          */
;*---------------------------------------------------------------------*/
(define (default-worker-load filename worker %this::JsGlobalObject)
   (loading-file-set! filename)
   (let ((exprs (call-with-input-file filename
		   (lambda (in) (j2s-compile in :main #f :%this %this)))))
      (let ((m (eval-module))
	    (jsmodule #f))
	 (unwind-protect
	    ;; eval the compile module in the current environment
	    (for-each (lambda (e) (set! jsmodule (eval! e))) exprs)
	    ;; restore the previous module
	    (eval-module-set! m)))))

   
