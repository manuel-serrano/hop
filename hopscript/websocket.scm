;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/websocket.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 15 05:51:37 2014                          */
;*    Last change :  Mon Feb 13 17:11:07 2023 (serrano)                */
;*    Copyright   :  2014-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop WebSockets                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_websocket

   (include "types.sch" "stringliteral.sch")
   
   (library web hop js2scheme)
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_object
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_function
	   __hopscript_error
	   __hopscript_array
	   __hopscript_worker
	   __hopscript_stringliteral
	   __hopscript_promise
	   __hopscript_lib)
   
   (export (class JsWebSocket::JsObject
	      (worker read-only)
	      (ws::websocket read-only)
	      (sendqueue::pair-nil (default '()))
	      recvqueue::cell)
	   
	   (class JsWebSocketClient::JsObject
	      (%mutex::mutex read-only (default (make-mutex)))
	      (state::long (default 0))
	      (wss::JsWebSocketServer read-only)
	      socket::obj 
	      (onmessages::pair-nil (default '()))
	      (oncloses::pair-nil (default '()))
	      (onerrors::pair-nil (default '())))
	   
	   (class JsWebSocketServer::JsObject
	      (downp::bool (default #f))
	      (state::long (default 0))
	      (worker read-only)
	      (svc::procedure read-only)
	      (conns::pair-nil (default '()))
	      (closes::pair-nil (default '())))
	   
	   (class JsWebSocketEvent::websocket-event))
	   
   (export (js-init-websocket! ::JsGlobalObject)
	   (js-websocket-post ::JsObject ::JsHopFrame ::int)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsWebSocket ...                              */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsWebSocket
   (lambda (o ctx)
      (if (isa? ctx JsGlobalObject)
	  (js-raise-type-error ctx "[[SerializeTypeError]] ~a" o)
	  (error "obj->string ::JsWebWocket" "Not a JavaScript context" ctx)))
   (lambda (o) o))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsWebSocketClient ...                        */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsWebSocketClient
   (lambda (o ctx)
      (if (isa? ctx JsGlobalObject)
	  (js-raise-type-error ctx "[[SerializeTypeError]] ~a" o)
	  (error "obj->string ::JsWebWocketClient" "Not a JavaScript context" ctx)))
   (lambda (o) o))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsWebSocketServer ...                        */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsWebSocketServer
   (lambda (o ctx)
      (if (isa? ctx JsGlobalObject)
	  (js-raise-type-error ctx "[[SerializeTypeError]] ~a" o)
	  (error "obj->string ::JsWebWocketServer" "Not a JavaScript context" ctx)))
   (lambda (o) o))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsWebSocket ...                                      */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsWebSocket worker::WorkerHopThread %this)
   (js-undefined))
   
;*---------------------------------------------------------------------*/
;*    js-donate ::JsWebSocketClient ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsWebSocketClient worker::WorkerHopThread %this)
   (js-undefined))
   
;*---------------------------------------------------------------------*/
;*    js-donate ::JsWebSocketServer ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsWebSocketServer worker::WorkerHopThread %this)
   (js-undefined))
   
;*---------------------------------------------------------------------*/
;*    add-event-listener! ::JsWebSocket ...                            */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! this::JsWebSocket name proc . l)
   (with-access::JsWebSocket this (ws)
      (add-event-listener! ws name
	 (lambda (evt)
	    (proc (duplicate::websocket-event evt
		     (target this)))))))

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::JsWebSocketServer ...                      */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! this::JsWebSocketServer name proc . l)
   (cond
      ((string=? name "connection")
       (with-access::JsWebSocketServer this (conns)
	  (set! conns (cons proc conns))))
      ((string=? name "close")
       (with-access::JsWebSocketServer this (closes)
	  (set! closes (cons proc closes))))))

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::JsWebSocketClient ...                      */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! this::JsWebSocketClient name proc . l)
   (when (procedure? proc)
      (cond
	 ((string=? name "message")
	  (with-access::JsWebSocketClient this (onmessages)
	     (set! onmessages (cons proc onmessages))))
	 ((string=? name "close")
	  (with-access::JsWebSocketClient this (oncloses)
	     (set! oncloses (cons proc oncloses))))
	 ((string=? name "error")
	  (with-access::JsWebSocketClient this (onerrors)
	     (set! onerrors (cons proc onerrors)))))))

;*---------------------------------------------------------------------*/
;*    js-websocket-states                                              */
;*---------------------------------------------------------------------*/
(define (js-websocket-state-connecting) 0)
(define (js-websocket-state-open) 1)
(define (js-websocket-state-closing) 2)
(define (js-websocket-state-closed) 3)
(define (js-websocket-state-error) -1)

;*---------------------------------------------------------------------*/
;*    ws-json-parser ...                                               */
;*---------------------------------------------------------------------*/
(define (ws-json-parser ip ctx %this)
   (let ((j (json-parser ip ctx)))
      (if (js-array? j)
	  (jsarray->list j %this)
	  j)))

;*---------------------------------------------------------------------*/
;*    js-init-websocket! ...                                           */
;*---------------------------------------------------------------------*/
(define (js-init-websocket! %this::JsGlobalObject)

   (unless (vector? __js_strings) (set! __js_strings (&init!)))
   
   (with-access::JsGlobalObject %this (js-object js-function)
      
      (define js-websocket-prototype
	 (instantiateJsObject
	    (__proto__ (js-object-proto %this))
	    (elements ($create-vector 5))))
      
      (define js-websocket-server-prototype
	 (instantiateJsObject
	    (__proto__ (js-object-proto %this))
	    (elements ($create-vector 2))))
      
      (define js-websocket-client-prototype
	 (instantiateJsObject
	    (__proto__ (js-object-proto %this))
	    (elements ($create-vector 5))))
      
      (define (js-websocket-construct o url options)
	 (with-access::JsGlobalObject %this (js-object)
	    (let* ((worker (js-current-worker))
		   (protocol (cond
				((string? options)
				 options)
				((js-jsstring? options)
				 (js-jsstring->string options))
				((js-array? options)
				 (let ((join (js-get options (& "join") %this)))
				    (js-jsstring->string 
				       (js-call1 %this join options
					  (js-ascii->jsstring ", ")))))
				((js-object? options)
				 (js-jsstring->string
				    (js-get options (& "protocol") %this)))))
		   (url (js-tostring url %this))
		   (queue (make-cell '()))
		   (ws (instantiate::websocket
			  (url (cond
				  ((string-prefix? "ws://" url)
				   (string-append "http://"
				      (substring url 5)))
				  ((string-prefix? "wss://" url)
				   (string-append "https://"
				      (substring url 6)))
				  (else
				   url)))
			  (protocol protocol)
			  (onbinary (lambda (data)
				       (with-handler
					  (lambda (e)
					     (exception-notify e))
					  (wss-onbinary data queue worker))))))
		   (obj (instantiateJsWebSocket
			   (__proto__ js-websocket-prototype)
			   (worker worker)
			   (recvqueue queue)
			   (ws ws))))
	       ;; listeners
	       (for-each (lambda (act)
			    (bind-websocket-listener! %this obj act))
		  (list
		     (cons "onmessage" #f)
		     (cons "onopen" #f)
		     (cons "onclose" #f)
		     (cons "onerror" #f)))
	       ;; connect the socket to the server
	       (js-worker-push! worker "ws-listener"
		  (lambda (%this)
		     (with-handler
			(lambda (e)
			   (with-access::&error e (msg)
			      (with-access::websocket ws (onerrors)
				 (let* ((msg (js-string->jsstring msg))
					(evt (instantiate::JsWebSocketEvent
						(name "error")
						(target ws)
						(data msg)
						(value msg))))
				    (js-worker-push! worker
				       "wesbsocket-client"
				       (lambda (%this)
					  (apply-listeners onerrors evt)))))))
			(websocket-connect! ws))))
	       obj)))
      
      (define (wss-onconnect wss)
	 (lambda (resp)
	    (with-access::http-response-websocket resp (request)
	       (with-access::http-request request (socket)
		  (let  ((ws (instantiateJsWebSocketClient
				(socket socket)
				(wss wss)
				(__proto__ js-websocket-client-prototype))))
		     ;; trigger an onconnect event
		     (with-access::JsWebSocketServer wss (conns worker)
			(js-worker-push! worker "wss-onconnect"
			   (lambda (%this)
			      ;; listeners
			      (for-each (lambda (act)
					   (bind-websocket-client-listener! %this
					      ws act))
				 (list (cons "onmessage" #f)
				    (cons "onclose" #f)
				    (cons "onerror" #f)))
			      (let ((evt (instantiate::server-event
					    (name "connection")
					    (target wss)
					    (value ws)))
				    (id (gensym)))
				 (apply-listeners conns evt)
				 ;; start the client-server-loop
				 ;; on JavaScript completion
				 (with-access::JsWebSocketServer wss (worker)
				    (js-worker-push! worker "wss-onconnect"
				       (lambda (%this)
					  (websocket-server-read-loop
					     %this request wss ws id))))))))
		     ;; returns the new client socket
		     ws)))))
      
      (define (wss-onbinary payload recvqueue worker)
	 (match-case (message-split payload 5)
	    ((?protocol ?id ?status ?content-type ?obj)
	     (let* ((pcell (assq (string->integer id) (cell-ref recvqueue)))
		    (val (hop-http-decode-value obj
			    (string->symbol content-type) %this
			    :response-type 'arraybuffer
			    :x-javascript-parser x-javascript-parser
			    :json-parser (lambda (ip ctx)
					    (ws-json-parser ip ctx %this))))
		    (status (string->integer status)))
		(when (pair? pcell)
		   (cell-set! recvqueue (remq! pcell (cell-ref recvqueue)))
		   (let ((handler (cdr pcell)))
		      ;; see hopscript/service.scm
		      (if (and (>=fx status 100) (<=fx status 299))
			  (cond
			     ((isa? handler JsPromise)
			      (js-worker-push! worker "ws-listener"
				 (lambda (%this)
				    (js-promise-async handler
				       (lambda (%this)
					  (js-promise-resolve handler val))))))
			     ((and (pair? handler) (procedure? (car handler)))
			      (js-worker-push! worker "ws-listener"
				 (lambda (%this)
				    ((car handler) val))))
			     ((and (pair? handler) (condition-variable? (car handler)))
			      (synchronize (cdr handler)
				 (let ((cv (car handler)))
				    (set-car! handler val)
				    (set-cdr! handler #f)
				    (condition-variable-signal! cv)))))
			  (cond
			     ((isa? handler JsPromise)
			      (js-worker-push! worker "ws-listener"
				 (lambda (%this)
				    (js-promise-async handler
				       (lambda (%this)
					  (js-promise-reject handler val))))))
			     ((and (pair? handler) (procedure? (cdr handler)))
			      (js-worker-push! worker "ws-listener"
				 (lambda (%this)
				    ((cdr handler) val))))
			     ((and (pair? handler) (condition-variable? (car handler)))
			      (synchronize (cdr handler)
				 (let ((cv (car handler)))
				    (set-car! handler #f)
				    (set-cdr! handler val)
				    (condition-variable-signal! cv))))))))))))
      
      (define (js->hop x)
	 (if (js-jsstring? x)
	     (js-jsstring->string x)
	     (js-raise-type-error %this "WebSocketServer: cannot convert ~s" x)))
      
      (define (js-websocket-server-construct this opt)
	 (letrec* ((path (cond
			    ((string? opt)
			     opt)
			    ((js-jsstring? opt)
			     (js-jsstring->string opt))
			    ((js-object? opt)
			     (js->hop (js-get opt (& "path") %this)))
			    (else
			     (js->hop opt))))
		   (proto (if (js-object? opt)
			      (let ((proto (js-get opt (& "protocol") %this)))
				 (cond
				    ((string? proto)
				     (list proto))
				    ((js-jsstring? proto)
				     (list (js-jsstring->string proto)))
				    ((js-array? proto)
				     (map js-jsstring->string
					(jsarray->list proto %this)))))))
		   (svc (service :name path ()
			   (let ((req (current-request)))
			      (websocket-server-response req 0
				 (wss-onconnect wss) proto))))
		   (wss (instantiateJsWebSocketServer
			   (state (js-websocket-state-open))
			   (worker (js-current-worker))
			   (__proto__ js-websocket-server-prototype)
			   (svc svc))))
	    ;; listeners
	    (for-each (lambda (act)
			 (bind-websocket-server-listener! %this wss act))
	       (list (cons "onconnection" #f)
		  (cons "onclose" #f)))
	    wss))
      
      ;; prototypes properties
      (init-builtin-websocket-prototype!
	 %this js-websocket-prototype)
      (init-builtin-websocket-server-prototype!
	 %this js-websocket-server-prototype)
      (init-builtin-websocket-client-prototype!
	 %this js-websocket-client-prototype)
      
      ;; two constructors
      (define js-websocket
	 (js-make-function %this
	    js-websocket-construct
	    (js-function-arity js-websocket-construct)
	    (js-function-info :name "WebSocket" :len 2)
	    :__proto__ (js-object-proto js-function)
	    :size 4
	    :alloc js-no-alloc
	    :shared-cmap #f))
      
      (define js-websocket-server
	 (js-make-function %this
	    js-websocket-server-construct
	    (js-function-arity js-websocket-server-construct)
	    (js-function-info :name "WebSocketServer" :len 1)
	    :__proto__ (js-object-proto js-function)
	    :alloc js-no-alloc
	    :shared-cmap #f))
      
      (js-bind! %this %this (& "WebSocket")
	 :configurable #f :enumerable #f :value js-websocket
	 :hidden-class #t)
      (js-bind! %this %this (& "WebSocketServer")
	 :configurable #f :enumerable #f :value js-websocket-server
	 :hidden-class #t)
      
      (js-bind! %this js-websocket (& "CONNECTING")
	 :configurable #f :enumerable #f
	 :value (js-websocket-state-connecting)
	 :hidden-class #t)
      (js-bind! %this js-websocket (& "OPEN")
	 :configurable #f :enumerable #f
	 :value (js-websocket-state-open)
	 :hidden-class #f)
      (js-bind! %this js-websocket (& "CLOSING")
	 :configurable #f :enumerable #f
	 :value (js-websocket-state-closing)
	 :hidden-class #f)
      (js-bind! %this js-websocket (& "CLOSED")
	 :configurable #f :enumerable #f
	 :value (js-websocket-state-closed)
	 :hidden-class #f)
      
      (js-undefined)))

;*---------------------------------------------------------------------*/
;*    init-builtin-websocket-prototype! ...                            */
;*---------------------------------------------------------------------*/
(define (init-builtin-websocket-prototype! %this obj)
   ;; readyState
   (js-bind! %this obj (& "readyState")
      :get (js-make-function %this
	      (lambda (this)
		 (with-access::JsWebSocket this (ws)
		    (with-access::websocket ws (state)
		       (case state
			  ((connecting) (js-websocket-state-connecting))
			  ((open) (js-websocket-state-open))
			  ((closing) (js-websocket-state-closing))
			  ((closed) (js-websocket-state-closed))
			  (else (js-websocket-state-error))))))
	      (js-function-arity 0 0)
	      (js-function-info :name "readyState.get" :len 0))
      :hidden-class #t)
   ;; url
   (js-bind! %this obj (& "url")
      :get (js-make-function %this
	      (lambda (this)
		 (with-access::JsWebSocket this (ws)
		    (with-access::websocket ws (url)
		       (cond
			  ((string-prefix? "http://" url)
			   (js-stringlist->jsstring
			      (list "ws://" (substring url 7))))
			  ((string-prefix? "https://" url)
			   (js-stringlist->jsstring
			      (list "wss://" (substring url 8))))
			  (else
			   (js-string->jsstring url))))))
	      (js-function-arity 0 0)
	      (js-function-info :name "url.get" :len 0))
      :hidden-class #t)
   ;; send
   (js-bind! %this obj (& "send")
      :value (js-make-function %this
		(lambda (this value)
		   (with-access::JsWebSocket this (ws)
		      (with-access::websocket ws (%socket %mutex)
			 (synchronize %mutex
			    (when (socket? %socket)
			       (websocket-send %socket
				  (js-tostring value %this)))))))
		(js-function-arity 1 0)
		(js-function-info :name "send" :len 1))
      :hidden-class #t)
   ;; close
   (js-bind! %this obj (& "close")
      :value (js-make-function %this
		(lambda (this)
		   (with-access::JsWebSocket this (ws)
		      (when (isa? ws websocket)
			 (websocket-close ws))))
		(js-function-arity 0 0)
		(js-function-info :name "close" :len 0))
      :hidden-class #t)
   ;; addEventListener
   (js-bind! %this obj (& "addEventListener")
      :value (js-make-function %this
		(lambda (this message proc)
		   (add-event-listener! this (js-tostring message %this) proc))
		(js-function-arity 2 0)
		(js-function-info :name "addEventListener" :len 2))
      :hidden-class #t)
   obj)

;*---------------------------------------------------------------------*/
;*    proc->listener ...                                               */
;*---------------------------------------------------------------------*/
(define (proc->listener worker %this proc::procedure this)
   (lambda (evt)
      (js-worker-push! worker "ws-listener"
	 (lambda (%this)
	    (js-call1-procedure proc this evt)))))

;*---------------------------------------------------------------------*/
;*    action->listener ...                                             */
;*---------------------------------------------------------------------*/
(define (action->listener worker %this action::pair this)
   (lambda (evt)
      (let ((g (gensym)))
	 (js-worker-push! worker "ws-listener"
	    (lambda (%this)
	       (when (js-procedure? (cdr action))
		  (js-call1-jsprocedure %this (cdr action) this evt)))))))

;*---------------------------------------------------------------------*/
;*    bind-websocket-listener! ...                                     */
;*---------------------------------------------------------------------*/
(define (bind-websocket-listener! %this obj action)
   (js-bind! %this obj (js-ascii-name->jsstring (car action))
      :get (js-make-function %this
              (lambda (this) (cdr action))
	      (js-function-arity 0 0)
	      (js-function-info :name (car action) :len 0))
      :set (js-make-function %this
	      (lambda (this proc)
		 (set-cdr! action proc)
		 (let ((name (substring (car action) 2)))
		    (with-access::JsWebSocket this (worker)
		       (add-event-listener! this
			     name (action->listener worker %this action this)))))
	      (js-function-arity 1 0)
	      (js-function-info :name (car action) :len 1))
      :hidden-class #t))

;*---------------------------------------------------------------------*/
;*    bind-websocket-server-listener! ...                              */
;*---------------------------------------------------------------------*/
(define (bind-websocket-server-listener! %this obj action)
   (js-bind! %this obj (js-ascii-name->jsstring (car action))
      :get (js-make-function %this
              (lambda (this) (cdr action))
	      (js-function-arity 0 0)
	      (js-function-info :name (car action) :len 0))
      :set (js-make-function %this
              (lambda (this proc)
                 (set-cdr! action proc)
                 (let ((name (substring (car action) 2)))
		    (with-access::JsWebSocketServer this (worker)
		       (add-event-listener! this
			     name (action->listener worker %this action this)))))
	      (js-function-arity 1 0)
	      (js-function-info :name (car action) :len 1))
      :hidden-class #t))

;*---------------------------------------------------------------------*/
;*    bind-websocket-client-listener! ...                              */
;*---------------------------------------------------------------------*/
(define (bind-websocket-client-listener! %this obj action)
   (js-bind! %this obj (js-ascii-name->jsstring (car action))
      :get (js-make-function %this
              (lambda (this) (cdr action))
	      (js-function-arity 0 0)
	      (js-function-info :name (car action) :len 0))
      :set (js-make-function %this
              (lambda (this proc)
                 (set-cdr! action proc)
                 (let ((name (substring (car action) 2)))
		    (with-access::JsWebSocketClient this (wss)
		       (with-access::JsWebSocketServer wss (worker)
			  (add-event-listener! this
				name (action->listener worker %this action this))))))
	      (js-function-arity 1 0)
	      (js-function-info :name (car action) :len 1))
      :hidden-class #t))

;*---------------------------------------------------------------------*/
;*    init-builtin-websocket-server-prototype! ...                     */
;*---------------------------------------------------------------------*/
(define (init-builtin-websocket-server-prototype! %this obj)
   ;; close
   (js-bind! %this obj (& "close")
      :value (js-make-function %this
		(lambda (this)
		   (with-access::JsWebSocketServer this (svc state closes worker downp)
		      (unless downp
			 (set! downp #t)
			 (set! state (js-websocket-state-closed))
			 (unregister-service! (procedure-attr svc))
			 (when (pair? closes)
			    ;; invoke the onclose listener
			    (let ((evt (instantiate::server-event
					  (name "connection")
					  (target this)
					  (value this))))
			       (js-worker-push! worker "wss-onclose"
				  (lambda (%this)
				     (apply-listeners closes evt))))))))
		(js-function-arity 0 0)
		(js-function-info :name "close" :len 0))
      :hidden-class #t)
   ;; addEventListner
   (js-bind! %this obj (& "addEventListener")
      :value (js-make-function %this
		(lambda (this message proc)
		   (with-access::JsWebSocketServer this (worker)
		      (add-event-listener! this (js-tostring message %this)
			 (proc->listener worker %this proc this))))
		(js-function-arity 2 0)
		(js-function-info :name "addEventListener" :len 2))
      :hidden-class #t)
   obj)

;*---------------------------------------------------------------------*/
;*    websocket-server-read-loop ...                                   */
;*---------------------------------------------------------------------*/
(define (websocket-server-read-loop %this req wss ws::JsWebSocketClient id)
   
   (define (onmessage frame)
      (with-access::JsWebSocketClient ws (onmessages)
	 (with-access::JsWebSocketServer wss (worker)
	    (let* ((val (js-string->jsstring frame))
		   (evt (instantiate::JsWebSocketEvent
			   (name "message")
			   (target ws)
			   (data val)
			   (value val))))
	       (js-worker-push! worker
		  "wesbsocket-client"
		  (lambda (%this)
		     (apply-listeners onmessages evt)))))))
   
   (define (onclose)
      (with-access::JsWebSocketClient ws (oncloses socket state %mutex)
	 (synchronize %mutex
	    (when socket
	       (socket-shutdown socket)
	       (set! socket #f)
	       (set! state (js-websocket-state-closed))
	       (with-access::JsWebSocketServer wss (worker)
		  (let ((evt (instantiate::JsWebSocketEvent
				(name "close")
				(target ws)
				(data (js-undefined))
				(value (js-undefined)))))
		     (js-worker-push! worker
			"wesbsocket-client"
			(lambda (%this)
			   (apply-listeners oncloses evt)))))))))
   
   (define (onerror)
      (with-access::JsWebSocketClient ws (onerrors socket state %mutex)
	 (synchronize %mutex
	    (when socket
	       (socket-shutdown socket)
	       (set! socket #f)
	       (set! state (js-websocket-state-closed))
	       (with-access::JsWebSocketServer wss (worker)
		  (let ((evt (instantiate::JsWebSocketEvent
				(name "error")
				(target ws)
				(data (js-undefined))
				(value (js-undefined)))))
		     (js-worker-push! worker
			"wesbsocket-client"
			(lambda (%this)
			   (apply-listeners onerrors evt)))))))))
   
   (define (eof-error? e)
      (or (isa? e &io-closed-error) (isa? e &io-read-error)))

   (define (invoke-websocket-service frame::JsHopFrame)
      (with-access::JsHopFrame frame (path args)
	 (let ((svc (get-service path)))
	    (service-invoke svc req args))))

   (define (decode-ws-message content-type data)
      (let ((val (hop-http-decode-value data
		    (string->symbol content-type)
		    %this
		    :response-type 'arraybuffer
		    :json-parser (lambda (ip ctx) (ws-json-parser ip ctx %this))
		    :x-javascript-parser x-javascript-parser)))
	 (cond
	    ((isa? val JsHopFrame)
	     val)
	    ((js-object? val)
	     (instantiateJsHopFrame
		(%this %this)
		(args (map! cdr
			 (js-jsobject->alist (js-get val (& "args") %this) %this)))
		(srv (js-get val (& "srv") %this))
		(options (js-get val (& "options") %this))
		(header (js-get val (& "header") %this))
		(path (string-append
			 (hop-service-base) "/" (js-get val (& "path") %this)))))
	    ((pair? val)
	     (instantiateJsHopFrame
		(%this %this)
		(args (cdr val))
		(header '())
		(path (string-append (hop-service-base) "/" (car val)))))
	    (else
	     (bigloo-type-error "websocket" "JsHopFrame or JsObject" val)))))

   (define (jsheader->header hd header)
      (if (js-object? hd)
	  (append (js-jsobject->alist hd %this) header)
	  header))

   (define (PoST-message msg::bstring socket req::http-request %this)
      (match-case (message-split msg 4)
	 ((?protocol ?id ?content-type ?data)
	  (let* ((frame (decode-ws-message content-type data))
		 (id (string->integer id))
		 (nreq (with-access::http-request req (header)
			  (with-access::JsHopFrame frame ((hd header) path)
			     (duplicate::http-request req
				(id id)
				(scheme 'ws)
				(abspath path)
				(header (jsheader->header hd header))
				(query path)))))
		 (resp (invoke-websocket-service frame)))
	     (http-ws-response resp nreq socket content-type 200 '() %this)))
	 (else
	  (error "PoST-message" "bad message" (format "~s" msg))
	  #f)))

   (define (is-proto? proto::bstring data::bstring)
      (string-prefix? proto data))
   
   (with-access::http-request req (socket)
      (thread-start!
	 (instantiate::hopthread
	    (body (lambda ()
		     ;; now the connection is established remove all
		     ;; connection/read timeouts.
		     (let ((in (socket-input socket)))
			(input-port-timeout-set! in 0))
		     (with-access::JsWebSocketClient ws (state)
			(set! state (js-websocket-state-open)))
		     ;; start reading the frames
		     (with-handler
			(lambda (e)
			   (when (>= (bigloo-debug) 1)
			      (exception-notify e))
			   (if (eof-error? e)
			       (onclose)
			       (onerror)))
			(let loop ()
			   (multiple-value-bind (opcode payload)
			      (websocket-read socket)
			      (case (bit-and opcode #xf)
				 ((1)
				  ;; text message
				  (cond
				     ((string? payload)
				      (onmessage payload)
				      (loop))
				     ((eof-object? payload)
				      (onclose))
				     (else
				      (onerror))))
				 ((2)
				  (cond
				     ((is-proto? "PoST:" payload)
				      (PoST-message payload socket req %this)
				      (loop))
				     (else
				      (onerror))))
				 ((0)
				  (onclose))
				 (else
				  (onerror))))))))))
      ws))

;*---------------------------------------------------------------------*/
;*    init-builtin-websocket-client-prototype! ...                     */
;*---------------------------------------------------------------------*/
(define (init-builtin-websocket-client-prototype! %this obj)
   ;; readyState
   (js-bind! %this obj (& "readyState")
      :get (js-make-function %this
	      (lambda (this)
		 (with-access::JsWebSocketClient this (socket state)
		    state))
	      (js-function-arity 0 0)
	      (js-function-info :name "readyState.get" :len 0))
      :hidden-class #t)
   ;; close
   (js-bind! %this obj (& "close")
      :value (js-make-function %this
		(lambda (this)
		   (with-access::JsWebSocketClient this (socket oncloses wss state %mutex)
		      (synchronize %mutex
			 (when (socket? socket)
			    (socket-shutdown socket)
			    (set! socket #f)
			    (set! state (js-websocket-state-closed))
			    (with-access::JsWebSocketServer wss (worker)
			       (js-worker-push! worker "ws-onclose"
				  (lambda (%this)
				     (when (pair? oncloses)
					;; invoke the onclose listener
					(let ((evt (instantiate::server-event
						      (name "connection")
						      (target this)
						      (value this))))
					   (apply-listeners oncloses evt))))))))))
		(js-function-arity 0 0)
		(js-function-info :name "close" :len 0))
      :hidden-class #t)
   ;; client
   (js-bind! %this obj (& "socket")
      :get (js-make-function %this
	      (lambda (this)
		 (with-access::JsWebSocketClient this (socket)
		    socket))
	      (js-function-arity 0 0)
	      (js-function-info :name "socket" :len 0))
      :hidden-class #t)
   ;; send
   (js-bind! %this obj (& "send")
      :value (js-make-function %this
		(lambda (this value)
		   (with-access::JsWebSocketClient this (socket %mutex)
		      (synchronize %mutex
			 (when socket
			    (websocket-send socket
			       (js-tostring value %this) :mask #f)))))
		(js-function-arity 1 0)
		(js-function-info :name "send" :len 1))
      :hidden-class #t)
   ;; addEventListner
   (js-bind! %this obj (& "addEventListener")
      :value (js-make-function %this
		(lambda (this message proc)
		   (with-access::JsWebSocketClient this (wss)
		      (with-access::JsWebSocketServer wss (worker)
			 (let ((action (js-tostring message %this)))
			    (add-event-listener! this action
			       (proc->listener worker %this proc this))))))
		(js-function-arity 2 0)
		(js-function-info :name "addEventListener" :len 2))
      :hidden-class #t)
   obj)

;*---------------------------------------------------------------------*/
;*    http-encode-value ...                                            */
;*---------------------------------------------------------------------*/
(define (http-encode-value obj content-type::bstring request %this)
   ;; see runtime/http-response.scm
   (cond
      ((or (string-prefix? "application/x-hop" content-type)
	   (string-prefix? "application/x-frame-hop" content-type))
       ;; fast path, bigloo serialization
       (obj->string obj %this))
      ((or (string-prefix? "application/json" content-type)
	   (string-prefix? "application/x-frame-json" content-type))
       ;; json encoding
       (call-with-output-string
	  (lambda (p) (obj->json obj p %this))))
      ((string-prefix? "text/plain" content-type)
       obj)
      (else
       ;; no other encoding supported
       (error "http-encode-value" "unsupported content-type" content-type))))

;*---------------------------------------------------------------------*/
;*    ws-response-socket ...                                           */
;*---------------------------------------------------------------------*/
(define (ws-response-socket obj req::http-request socket::socket
 	   content-type status header %this)
   (with-access::http-request req (id)
      (let ((data (http-encode-value obj content-type req %this)))
	 (websocket-send-binary socket
	    (string-append "PoST:"
	       (integer->string id)
	       ":" (integer->string status)
	       ":" content-type
	       ":" data)
	    :mask #f))))

;*---------------------------------------------------------------------*/
;*    http-ws-response ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (http-ws-response resp req::http-request socket::socket
		   content-type status header %this)
   (ws-response-socket resp req socket content-type status header %this))

;*---------------------------------------------------------------------*/
;*    http-ws-response ::JsPromise ...                                 */
;*---------------------------------------------------------------------*/
(define-method (http-ws-response resp::JsPromise req socket
		  content-type status header %this)
   (with-access::JsPromise resp (worker)
      (with-access::WorkerHopThread worker (%this)
	 (js-promise-then-catch %this resp 
	    (js-make-function %this
	       (lambda (this result)
		  (js-promise-async resp
		     (lambda (%this)
			(http-ws-response result req socket
			   content-type 200 header %this))))
	       (js-function-arity 1 0)
	       (js-function-info :name "reply" :len 1))
	    (js-make-function %this
	       (lambda (this rej)
		  (js-promise-async resp
		     (lambda (%this)
			(http-ws-response rej req socket
			   content-type 500 header %this))))
	       (js-function-arity 1 0)
	       (js-function-info :name "reject" :len 1))
	    resp))))

;*---------------------------------------------------------------------*/
;*    http-ws-response ::%http-response ...                            */
;*---------------------------------------------------------------------*/
(define-method (http-ws-response resp::%http-response req socket
		   content-type status header %this)
   (ws-response-socket (format "Method not implemented ~a" (typeof resp))
      req socket content-type 500 '() %this))

;*---------------------------------------------------------------------*/
;*    start-line->status ...                                           */
;*---------------------------------------------------------------------*/
(define (start-line->status::int obj::%http-response-server)
   (with-access::%http-response-server obj (start-line)
      (let ((m (pregexp-match "^[^ ]+[ \t]*([0-9]+)" start-line)))
	 (if m (string->integer (cadr m)) 200))))

;*---------------------------------------------------------------------*/
;*    http-ws-response ::http-response-string ...                      */
;*---------------------------------------------------------------------*/
(define-method (http-ws-response resp::http-response-string req socket
		   content-type status header %this)
   (with-access::http-response-string resp (body header content-type)
      (ws-response-socket body req socket
	 (or content-type "text/plain")
	 (start-line->status resp)
	 header
	 %this)))

;*---------------------------------------------------------------------*/
;*    http-ws-response ::http-response-async ...                       */
;*---------------------------------------------------------------------*/
(define-method (http-ws-response resp::http-response-async req socket
		  content-type status header %this)
   (with-access::http-response-async resp (async content-type header ctx)
      (async (lambda (obj)
		(http-ws-response resp req socket
		   (or content-type "text/plain")
		   (start-line->status obj)
		   header
		   ctx)))))

;*---------------------------------------------------------------------*/
;*    http-ws-response ::http-response-hop ...                         */
;*---------------------------------------------------------------------*/
(define-method (http-ws-response obj::http-response-hop req socket
		  content-type status header %this)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    message-split ...                                                */
;*---------------------------------------------------------------------*/
(define (message-split str num)
   (let loop ((n num)
	      (i 0)
	      (res '()))
      (if (=fx n 1)
	  (reverse! (cons (substring str i) res))
	  (let ((j (string-index str #\: i)))
	     (if (not j)
		 '()
		 (loop (-fx n 1)
		    (+fx j 1)
		    (cons (substring str i j) res)))))))

;*---------------------------------------------------------------------*/
;*    js-websocket-post ...                                            */
;*---------------------------------------------------------------------*/
(define (js-websocket-post srv::JsObject this::JsHopFrame id::int)
	 
   (with-access::JsWebSocket srv (ws sendqueue recvqueue worker)

      (define (abort-post entry)
	 (let ((handler (cdr entry))
	       (reason (js-ascii->jsstring "connection closed")))
	    (cond
	       ((isa? handler JsPromise)
		(js-worker-push! worker "ws-listener"
		   (lambda (%this)
		      (js-promise-async handler
			 (lambda (%this)
			    (js-promise-reject handler reason))))))
	       ((procedure? (cdr handler))
		(js-worker-push! worker "ws-listener"
		   (lambda (%this)
		      ((cdr handler) reason)))))))
      
      (with-access::websocket ws (%socket onopens oncloses)
	 (let* ((data (obj->string this))
		(msg (string-append "PoST:"
			(integer->string id)
			":application/x-hop:" data)))
	    (cond
	       ((socket? %socket)
		(websocket-send-binary %socket msg :mask #f))
	       ((pair? sendqueue)
		(set! sendqueue (cons msg sendqueue)))
	       (else
		(set! sendqueue (list msg))
		;; use onopens and oncloses directly to hide
		;; the handlers from JS
		(set! onopens
		   (cons
		      (lambda (evt)
			 (for-each (lambda (msg)
				      (websocket-send-binary %socket msg
					 :mask #f))
			    (reverse! sendqueue)))
		      onopens))
		(set! oncloses
		   (cons
		      (lambda (evt) (for-each abort-post recvqueue))
		      oncloses))))))))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
