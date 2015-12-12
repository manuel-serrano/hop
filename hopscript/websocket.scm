;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/websocket.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 15 05:51:37 2014                          */
;*    Last change :  Sat Dec 12 13:23:22 2015 (serrano)                */
;*    Copyright   :  2014-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop WebSockets                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_websocket

   (include "stringliteral.sch")
   
   (library web hop js2scheme)
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_function
	   __hopscript_error
	   __hopscript_array
	   __hopscript_worker
	   __hopscript_stringliteral)
   
   (export (class JsWebSocket::JsObject
	      (worker read-only)
	      (ws::websocket read-only))
	   
	   (class JsWebSocketClient::JsObject
	      (%mutex::mutex read-only (default (make-mutex)))
	      (state::symbol (default 'connecting))
	      (wss::JsWebSocketServer read-only)
	      socket::obj 
	      (onmessages::pair-nil (default '()))
	      (oncloses::pair-nil (default '()))
	      (onerrors::pair-nil (default '())))
	   
	   (class JsWebSocketServer::JsObject
	      (state::symbol (default 'init))
	      (worker read-only)
	      (svc::procedure read-only)
	      (conns::pair-nil (default '()))
	      (closes::pair-nil (default '())))
	   
	   (class JsWebSocketEvent::websocket-event))
	   
   (export (js-init-websocket! ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-begin!)

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsWebSocket ...                              */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsWebSocket
   (lambda (o)
      (js-raise-type-error (js-initial-global-object)
	 "[[SerializeTypeError]] ~a" o))
   (lambda (o) o))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsWebSocketClient ...                        */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsWebSocketClient
   (lambda (o)
      (js-raise-type-error (js-initial-global-object)
	 "[[SerializeTypeError]] ~a" o))
   (lambda (o) o))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsWebSocketServer ...                        */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsWebSocketServer
   (lambda (o)
      (js-raise-type-error (js-initial-global-object)
	 "[[SerializeTypeError]] ~a" o))
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
   (cond
      ((string=? name "message")
       (with-access::JsWebSocketClient this (onmessages)
	  (set! onmessages (cons proc onmessages))))
      ((string=? name "close")
       (with-access::JsWebSocketClient this (oncloses)
	  (set! oncloses (cons proc oncloses))))
      ((string=? name "error")
       (with-access::JsWebSocketClient this (onerrors)
	  (set! onerrors (cons proc onerrors))))))

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
	       (let* ((worker (js-current-worker))
		      (protocol (cond
				   ((string? options)
				    options)
				   ((js-jsstring? options)
				    (js-jsstring->string options))
				   ((isa? options JsArray)
				    (let ((join (js-get options 'join %this)))
				       (js-jsstring->string 
					  (js-call1 %this join options
					     (js-string->jsstring ", ")))))
				   ((isa? options JsObject)
				    (js-jsstring->string
				       (js-get options 'protocol %this)))))
		      (url (js-tostring url %this))
		      (ws (instantiate::websocket
			     (url (cond
				     ((string-prefix? "ws://" url)
				      (string-append "http://" (substring url 5)))
				     ((string-prefix? "wss://" url)
				      (string-append "https://" (substring url 6)))
				     (else
				      url)))
			     (protocol protocol)))
		      (obj (instantiate::JsWebSocket
			      (__proto__ js-websocket-prototype)
			      (worker worker)
			      (ws ws))))
		  ;; listeners
		  (for-each (lambda (act)
			       (bind-websocket-listener! %this obj act))
		     (list
			(cons 'onmessage #f)
			(cons 'onopen #f)
			(cons 'onclose #f)
			(cons 'onerror #f)))
		  ;; connect the socket to the server
		  (js-worker-push-thunk! worker "ws-listener"
		     (lambda ()
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
				       (js-worker-push-thunk! worker
					  "wesbsocket-client"
					  (lambda ()
					     (apply-listeners onerrors evt)))))))
			   (websocket-connect! ws))))
		  obj)))

	 (define (wss-onconnect wss)
	    (lambda (resp)
	       (with-access::http-response-websocket resp (request)
		  (with-access::http-request request (socket)
		     (let  ((ws (instantiate::JsWebSocketClient
				   (socket socket)
				   (wss wss)
				   (__proto__ js-websocket-client-prototype))))
			;; listeners
			(for-each (lambda (act)
				     (bind-websocket-client-listener! %this
					ws act))
			   (list (cons 'onmessage #f)
			      (cons 'onclose #f)))
			(let ((evt (instantiate::server-event
				      (name "connection")
				      (target wss)
				      (value ws)))
			      (id (gensym)))
			   ;; trigger an onconnect event
			   (with-access::JsWebSocketServer wss (conns worker)
			      (js-worker-push-thunk! worker "wss-onconnect"
				 (lambda ()
				    (apply-listeners conns evt)
				    ;; start the client-server-loop
				    ;; on JavaScript completion
				    (with-access::JsWebSocketServer wss (worker)
				       (js-worker-push-thunk! worker "wss-onconnect"
					  (lambda ()
					     (websocket-server-read-loop
						%this request wss ws id))))))))
			;; returns the new client socket
			ws)))))
	 
	 (define (js->hop x)
	    (if (js-jsstring? x)
		(js-jsstring->string x)
		x))
	 
	 (define (js-websocket-server-construct this opt)
	    (letrec* ((path (cond
			       ((string? opt)
				opt)
			       ((js-jsstring? opt)
				(js-jsstring->string opt))
			       (else
 				(js->hop (js-get opt 'path %this)))))
		      (proto (if (isa? opt JsObject)
				 (let ((proto (js-get opt 'protocol %this)))
				    (cond
				       ((string? proto)
					(list proto))
				       ((js-jsstring? proto)
					(list (js-jsstring->string proto)))
				       ((isa? proto JsArray)
					(map (lambda (el)
						(js-jsstring->string (car el)))
					   (jsarray->list proto %this)))))))
		      (svc (service :name path ()
			      (let ((req (current-request)))
				 (websocket-server-response req 0
				    (wss-onconnect wss) proto))))
		      (wss (instantiate::JsWebSocketServer
			      (state 'up)
			      (worker (js-current-worker))
			      (__proto__ js-websocket-server-prototype)
			      (svc svc))))
	       ;; listeners
	       (for-each (lambda (act)
			    (bind-websocket-server-listener! %this wss act))
		  (list (cons 'onconnection #f)
		     (cons 'onclose #f)))
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
		       (js-string->jsstring (symbol->string state)))))
	      0 'readyState))
   ;; url
   (js-bind! %this obj 'url
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
	      0 'url))
   ;; send
   (js-bind! %this obj 'send
      :value (js-make-function %this
		(lambda (this value)
		   (with-access::JsWebSocket this (ws)
		      (with-access::websocket ws (%socket %mutex)
			 (synchronize %mutex
			    (when (socket? %socket)
			       (websocket-send %socket
				  (js-tostring value %this)))))))
		1 'send))
   ;; close
   (js-bind! %this obj 'close
      :value (js-make-function %this
		(lambda (this)
		   (with-access::JsWebSocket this (ws)
		      (when (isa? ws websocket)
			 (websocket-close ws))))
		0 'close))
   ;; addEventListener
   (js-bind! %this obj 'addEventListener
      :value (js-make-function %this
		(lambda (this message proc)
		   (add-event-listener! this (js-tostring message %this) proc))
		2 'addEventListener))
   obj)

;*---------------------------------------------------------------------*/
;*    proc->listener ...                                               */
;*---------------------------------------------------------------------*/
(define (proc->listener worker %this proc::procedure this)
   (lambda (evt)
      (js-worker-push-thunk! worker "ws-listener"
	 (lambda ()
	    (js-call1 %this proc this evt)))))

;*---------------------------------------------------------------------*/
;*    action->listener ...                                             */
;*---------------------------------------------------------------------*/
(define (action->listener worker %this action::pair this)
   (lambda (evt)
      (let ((g (gensym)))
	 (js-worker-push-thunk! worker "ws-listener"
	    (lambda ()
	       (when (isa? (cdr action) JsFunction)
		  (js-call1 %this (cdr action) this evt)))))))

;*---------------------------------------------------------------------*/
;*    bind-websocket-listener! ...                                     */
;*---------------------------------------------------------------------*/
(define (bind-websocket-listener! %this obj action)
   (js-bind! %this obj (car action) 
      :get (js-make-function %this
              (lambda (this) (cdr action))
              0 (car action))
      :set (js-make-function %this
              (lambda (this proc)
                 (set-cdr! action proc)
                 (let ((name (substring (symbol->string! (car action)) 2)))
		    (with-access::JsWebSocket this (worker)
		       (add-event-listener! this
			     name (action->listener worker %this action this)))))
              1 (car action))))

;*---------------------------------------------------------------------*/
;*    bind-websocket-server-listener! ...                              */
;*---------------------------------------------------------------------*/
(define (bind-websocket-server-listener! %this obj action)
   (js-bind! %this obj (car action) 
      :get (js-make-function %this
              (lambda (this) (cdr action))
              0 (car action))
      :set (js-make-function %this
              (lambda (this proc)
                 (set-cdr! action proc)
                 (let ((name (substring (symbol->string! (car action)) 2)))
		    (with-access::JsWebSocketServer this (worker)
		       (add-event-listener! this
			     name (action->listener worker %this action this)))))
              1 (car action))))

;*---------------------------------------------------------------------*/
;*    bind-websocket-client-listener! ...                              */
;*---------------------------------------------------------------------*/
(define (bind-websocket-client-listener! %this obj action)
   (js-bind! %this obj (car action) 
      :get (js-make-function %this
              (lambda (this) (cdr action))
              0 (car action))
      :set (js-make-function %this
              (lambda (this proc)
                 (set-cdr! action proc)
                 (let ((name (substring (symbol->string! (car action)) 2)))
		    (with-access::JsWebSocketClient this (wss)
		       (with-access::JsWebSocketServer wss (worker)
			  (add-event-listener! this
				name (action->listener worker %this action this))))))
              1 (car action))))

;*---------------------------------------------------------------------*/
;*    init-builtin-websocket-server-prototype! ...                     */
;*---------------------------------------------------------------------*/
(define (init-builtin-websocket-server-prototype! %this obj)
   ;; close
   (js-bind! %this obj 'close
      :value (js-make-function %this
		(lambda (this)
		   (with-access::JsWebSocketServer this (svc state closes worker)
		      (unless (eq? state 'down)
			 (set! state 'down)
			 (unregister-service! (procedure-attr svc))
			 (when (pair? closes)
			    ;; invoke the onclose listener
			    (let ((evt (instantiate::server-event
					  (name "connection")
					  (target this)
					  (value this))))
			       (js-worker-push-thunk! worker "wss-onclose"
				  (lambda ()
				     (apply-listeners closes evt))))))))
		0 'close))
   ;; addEventListner
   (js-bind! %this obj 'addEventListener
      :value (js-make-function %this
		(lambda (this message proc)
		   (with-access::JsWebSocketServer this (worker)
		      (add-event-listener! this (js-tostring message %this)
			 (proc->listener worker %this proc this))))
		2 'addEventListener))
   obj)

(define _frame 0)
(define (frame++)
   (set! _frame (+fx 1 _frame))
   _frame)


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
	       (js-worker-push-thunk! worker
		  "wesbsocket-client"
		  (lambda ()
		     (apply-listeners onmessages evt)))))))
   
   (define (onclose)
      (with-access::JsWebSocketClient ws (oncloses socket state %mutex)
	 (synchronize %mutex
	    (when socket
	       (socket-shutdown socket)
	       (set! socket #f)
	       (set! state 'closed)
	       (with-access::JsWebSocketServer wss (worker)
		  (let ((evt (instantiate::JsWebSocketEvent
				(name "close")
				(target ws)
				(data (js-undefined))
				(value (js-undefined)))))
		     (js-worker-push-thunk! worker
			"wesbsocket-client"
			(lambda ()
			   (apply-listeners oncloses evt)))))))))

   (define (onerror)
      (with-access::JsWebSocketClient ws (onerrors socket state %mutex)
	 (synchronize %mutex
	    (when socket
	       (socket-shutdown socket)
	       (set! socket #f)
	       (set! state 'closed)
	       (with-access::JsWebSocketServer wss (worker)
		  (let ((evt (instantiate::JsWebSocketEvent
				(name "error")
				(target ws)
				(data (js-undefined))
				(value (js-undefined)))))
		     (js-worker-push-thunk! worker
			"wesbsocket-client"
			(lambda ()
			   (apply-listeners onerrors evt)))))))))

   (define (eof-error? e)
      (cond-expand
	 (bigloo4.2a
	  (or (isa? e &io-closed-error)
	      (isa? e &io-read-error)
	      (and (isa? e &error)
		   (with-access::&error e (msg)
		      (string=? msg
			 "Can't read on a closed input port")))))
	 (else
	  (or (isa? e &io-closed-error)
	      (isa? e &io-read-error)))))
   
   (with-access::http-request req (socket)
      (thread-start!
	 (instantiate::hopthread
	    (body (lambda ()
		     ;; now the connection is established remove all
		     ;; connection/read timeouts.
		     (let ((in (socket-input socket)))
			(input-port-timeout-set! in 0))
		     (with-access::JsWebSocketClient ws (state)
			(set! state 'open))
		     ;; start reading the frames
		     (with-handler
			(lambda (e)
			   (if (eof-error? e)
			       (onclose)
			       (onerror)))
			(let loop ()
			   (let ((frame (websocket-read socket)))
			      (cond
				 ((string? frame)
				  (onmessage frame)
				  (loop))
				 ((eof-object? frame)
				  (onclose))
				 (else
				  (onerror))))))))))
      ws))

;*---------------------------------------------------------------------*/
;*    init-builtin-websocket-client-prototype! ...                     */
;*---------------------------------------------------------------------*/
(define (init-builtin-websocket-client-prototype! %this obj)
   ;; readyState
   (js-bind! %this obj 'readyState
      :get (js-make-function %this
	      (lambda (this)
		 (with-access::JsWebSocketClient this (socket state)
		    (js-string->jsstring (symbol->string state))))
	      0 'readyState))
   ;; close
   (js-bind! %this obj 'close
      :value (js-make-function %this
		(lambda (this)
		   (with-access::JsWebSocketClient this (socket oncloses wss state %mutex)
		      (synchronize %mutex
			 (when (socket? socket)
			    (socket-shutdown socket)
			    (set! socket #f)
			    (set! state 'closed)
			    (with-access::JsWebSocketServer wss (worker)
			       (js-worker-push-thunk! worker "ws-onclose"
				  (lambda ()
				     (when (pair? oncloses)
					;; invoke the onclose listener
					(let ((evt (instantiate::server-event
						      (name "connection")
						      (target this)
						      (value this))))
					   (apply-listeners oncloses evt))))))))))
		0 'close))
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
		   (with-access::JsWebSocketClient this (socket %mutex)
		      (synchronize %mutex
			 (when socket
			    (websocket-send socket
			       (js-tostring value %this) :mask #f)))))
		1 'send))
   ;; addEventListner
   (js-bind! %this obj 'addEventListener
      :value (js-make-function %this
		(lambda (this message proc)
		   (with-access::JsWebSocketClient this (wss)
		      (with-access::JsWebSocketServer wss (worker)
			 (let ((action (js-tostring message %this)))
			    (add-event-listener! this action
			       (proc->listener worker %this proc this))))))
		2 'addEventListener))
   obj)

;*---------------------------------------------------------------------*/
;*    JsStringLiteral end                                              */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-end!)
