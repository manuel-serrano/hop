;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/websocket.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 15 05:51:37 2014                          */
;*    Last change :  Wed Aug 27 15:35:05 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Hop WebSockets                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_websocket

   (library web hop js2scheme)
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_function
	   __hopscript_error
	   __hopscript_array
	   __hopscript_worker)
   
   (export (class JsWebSocket::JsObject
	      (ws::websocket read-only))
	   
	   (class JsWebSocketClient::JsObject
	      (socket::socket read-only)
	      (onmessages::pair-nil (default '())))
	   
	   (class JsWebSocketServer::JsObject
	      (worker read-only)
	      (svc::procedure read-only)
	      (conns::pair-nil (default '())))
	   
	   (class JsWebSocketEvent::websocket-event
	      (data read-only)))
	   
   (export (js-init-websocket! ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::JsWebSocket ...                            */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! this::JsWebSocket name proc . l)
   (with-access::JsWebSocket this (ws)
      (add-event-listener! ws name proc)))

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::JsWebSOcketServer ...                      */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! this::JsWebSocketServer name proc . l)
   (cond
      ((string=? name "connection")
       (with-access::JsWebSocketServer this (conns)
	  (set! conns (cons proc conns))))))

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::JsWebSocketClient ...                      */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! this::JsWebSocketClient name proc . l)
   (cond
      ((string=? name "message")
       (with-access::JsWebSocketClient this (onmessages)
	  (set! onmessages (cons proc onmessages))))))

;*---------------------------------------------------------------------*/
;*    js-init-websocket! ...                                           */
;*---------------------------------------------------------------------*/
(define (js-init-websocket! %this::JsGlobalObject)
   
   (with-access::JsGlobalObject %this (__proto__ js-object js-function)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 (define js-websocket-prototype
	    (instantiate::JsObject
	       (__proto__ __proto__)
	       (extensible #t)))
	 
	 (define js-websocket-server-prototype
	    (instantiate::JsObject
	       (__proto__ __proto__)
	       (extensible #t)))
	 
	 (define js-websocket-client-prototype
	    (instantiate::JsObject
	       (__proto__ __proto__)
	       (extensible #t)))

	 (define (js-websocket-construct o url options)
	    (with-access::JsGlobalObject %this (js-object)
	       (let* ((protocol (cond
				   ((string? options)
				    options)
				   ((isa? options JsArray)
				    (let ((join (js-get options 'join %this)))
				       (js-call1 %this join options ", ")))
				   ((isa? options JsObject)
				    (js-get options 'protocol %this))))
		      (url (js-tostring url %this))
		      (ws (instantiate::websocket
			     (url (pregexp-replace "ws://" url "http://"))
			     (protocol protocol)))
		      (obj (instantiate::JsWebSocket
			      (__proto__ js-websocket-prototype)
			      (ws ws))))
		  (websocket-connect! ws)
		  obj)))

	 (define (wss-onconnect wss)
	    (lambda (resp)
	       (with-access::http-response-websocket resp (request)
		  (let ((mutex (make-mutex)))
		     (synchronize mutex
			(let* ((ws (websocket-client %this
				      js-websocket-client-prototype
				      request wss mutex))
			       (evt (instantiate::server-event
				       (name "connection")
				       (target wss)
				       (value ws))))
			   (with-access::JsWebSocketServer wss (conns worker)
			      (js-worker-push-thunk! worker
				 (lambda ()
				    (apply-listeners conns evt))))))))))
	 
	 (define (js-websocket-server-construct this opt)
	    (letrec* ((path (if (string? opt) opt (js-get opt 'path %this)))
		      (proto (if (isa? opt JsObject)
				 (let ((proto (js-get opt 'protocol %this)))
				    (cond
				       ((string? proto)
					proto)
				       ((isa? proto JsArray)
					(jsarray->list proto %this))))))
		      (svc (service :name path ()
			      (let ((req (current-request)))
				 (websocket-server-response req 0
				    (wss-onconnect wss) proto))))
		      (wss (instantiate::JsWebSocketServer
			      (worker (js-current-worker))
			      (__proto__ js-websocket-server-prototype)
			      (svc svc))))
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
	       (lambda (this url options)
		  (js-new %this js-websocket url options))
	       2 'WebSocket
	       :__proto__ js-function-prototype
	       :construct js-websocket-construct))
	 
	 (define js-websocket-server
	    (js-make-function %this
	       (lambda (this path)
		  (js-new %this js-websocket-server path))
	       1 'WebSocketServer
	       :__proto__ js-function-prototype
	       :construct js-websocket-server-construct))
	 
	 (js-bind! %this %this 'WebSocket
	    :configurable #f :enumerable #f :value js-websocket)
	 (js-bind! %this %this 'WebSocketServer
	    :configurable #f :enumerable #f :value js-websocket-server)

	 (js-undefined))))

;*---------------------------------------------------------------------*/
;*    init-builtin-websocket-prototype! ...                            */
;*---------------------------------------------------------------------*/
(define (init-builtin-websocket-prototype! %this obj)
   ;; readyState
   (js-bind! %this obj 'readyState
      :get (js-make-function %this
	      (lambda (this)
		 (with-access::JsWebSocket this (ws)
		    (with-access::websocket ws (state)
		       (symbol->string state))))
	      0 'readyState))
   ;; url
   (js-bind! %this obj 'url
      :get (js-make-function %this
	      (lambda (this)
		 (with-access::JsWebSocket this (ws)
		    (with-access::websocket ws (url)
		       (pregexp-replace "http://" url "ws://"))))
	      0 'url))
   ;; send
   (js-bind! %this obj 'send
      :value (js-make-function %this
		(lambda (this value)
		   (with-access::JsWebSocket this (ws)
		      (with-access::websocket ws (%socket)
			 (websocket-send %socket value))))
		1 'send))
   ;; addEventListener
   (js-bind! %this obj 'addEventListener
      :value (js-make-function %this
		(lambda (this message proc)
		   (add-event-listener! this (js-tostring message %this) proc))
		2 'addEventListener))
   ;; listeners
   (for-each (lambda (act) (bind-listener! %this obj act))
      (list
	 (cons 'onmessage #f)
	 (cons 'onopen #f)
	 (cons 'onclose #f)
	 (cons 'onerror #f)))
   obj)

;*---------------------------------------------------------------------*/
;*    proc->listener ...                                               */
;*---------------------------------------------------------------------*/
(define (proc->listener %this proc this)
   (lambda (evt)
      (js-call1 %this proc this evt)))

;*---------------------------------------------------------------------*/
;*    bind-listener! ...                                               */
;*---------------------------------------------------------------------*/
(define (bind-listener! %this obj action)
   (js-bind! %this obj (car action)
      :get (js-make-function %this
              (lambda (this) (cdr action))
              0 (car action))
      :set (js-make-function %this
              (lambda (this proc)
                 (set-cdr! action proc)
                 (let ((name (substring (symbol->string! (car action)) 2)))
                    (add-event-listener! this
                          name (proc->listener %this proc this))))
              1 (car action))))

;*---------------------------------------------------------------------*/
;*    init-builtin-websocket-server-prototype! ...                     */
;*---------------------------------------------------------------------*/
(define (init-builtin-websocket-server-prototype! %this obj)
   ;; addEventListner
   (js-bind! %this obj 'addEventListener
      :value (js-make-function %this
		(lambda (this message proc)
		   (add-event-listener! this
			 (js-tostring message %this)
		      (proc->listener %this proc this)))
		2 'addEventListener))
   ;; listeners
   (for-each (lambda (act) (bind-listener! %this obj act))
      (list (cons 'onconnection #f)))
   obj)

;*---------------------------------------------------------------------*/
;*    websocket-client ...                                             */
;*---------------------------------------------------------------------*/
(define (websocket-client %this proto req wss mutex)
   (with-access::http-request req (socket)
      (let ((ws (instantiate::JsWebSocketClient
		   (socket socket)
		   (__proto__ proto))))
	 (thread-start!
	    (instantiate::hopthread
	       (body (lambda ()
			;; now the connection is established remove all
			;; connection/read timeouts.
			(let ((in (socket-input socket)))
			   (input-port-timeout-set! in 0))
			;; start reading the frames
			(synchronize mutex
			   (let loop ((frame (websocket-read socket)))
			      (when frame
				 (with-access::JsWebSocketClient ws (onmessages)
				    (with-access::JsWebSocketServer wss (worker svc)
				       (let ((evt (instantiate::JsWebSocketEvent
						     (name "message")
						     (target ws)
						     (data frame)
						     (value frame))))
					  (js-worker-push-thunk! worker
					     (lambda ()
						(apply-listeners onmessages evt)))))))
			      (loop (websocket-read socket))))))))
	 ws)))

;*---------------------------------------------------------------------*/
;*    init-builtin-websocket-client-prototype! ...                     */
;*---------------------------------------------------------------------*/
(define (init-builtin-websocket-client-prototype! %this obj)
   ;; client
   (js-bind! %this obj 'socket
      :get (js-make-function %this
	      (lambda (this)
		 (with-access::JsWebSocketClient this (socket)
		    socket))
	      0 'socket))
   ;; send
   (js-bind! %this obj 'send
      :value (js-make-function %this
		(lambda (this value)
		   (with-access::JsWebSocketClient this (socket)
		      (websocket-send socket value)))
		1 'send))
   ;; addEventListner
   (js-bind! %this obj 'addEventListener
      :value (js-make-function %this
		(lambda (this message proc)
		   (let ((action (js-tostring message %this)))
		      (add-event-listener! this action
			 (proc->listener %this proc this))))
		2 'addEventListener))
   ;; listeners
   (for-each (lambda (act) (bind-listener! %this obj act))
      (list (cons 'onmessage #f)))
   obj)

