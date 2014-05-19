;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_hop_ws.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 15 05:51:37 2014                          */
;*    Last change :  Mon May 19 15:55:41 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Hop WebSockets                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__hop-ws

   (library hop hopscript)

   (static (class JsWebSocket::JsObject
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
	   
   (export (hopjs-websocket ::JsGlobalObject)
	   (hopjs-websocket-server ::JsGlobalObject)))

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
;*    hopjs-websocket ...                                              */
;*---------------------------------------------------------------------*/
(define (hopjs-websocket %this)
   (lambda (this url)
      (with-access::JsGlobalObject %this (js-object)
	 (let* ((ws (instantiate::websocket (url url)))
		(obj (instantiate::JsWebSocket
			(ws ws)))
		(url (js-tostring url %this)))
	    (websocket-connect! ws)
	    ;; prototype
	    (with-access::JsObject obj (__proto__)
	       (set! __proto__ (hopjs-websocket-prototype %this)))
	    obj))))
   
;*---------------------------------------------------------------------*/
;*    hopjs-websocket-prototype ...                                    */
;*---------------------------------------------------------------------*/
(define (hopjs-websocket-prototype %this)
   (with-access::JsGlobalObject %this (js-object js-others)
      (let ((old (assq 'js-websocket-prototype js-others)))
	 (or old
	     (let ((obj (js-new %this js-object)))
		;; readyState
		(js-bind! %this obj 'readyState
		   :get (js-make-function %this
			   (lambda (this)
			      (with-access::JsWebSocket this (ws)
				 (with-access::websocket ws (state)
				    (symbol->string state))))
			   0 "readyState"))
		;; url
		(js-bind! %this obj 'url
		   :get (js-make-function %this
			   (lambda (this)
			      (with-access::JsWebSocket this (ws)
				 (with-access::websocket ws (url)
				    url)))
			   0 "url"))
		;; send
		(js-bind! %this obj 'send
		   :value (js-make-function %this
			     (lambda (this value)
				(with-access::JsWebSocket this (ws)
				   (with-access::websocket ws (%socket)
				      (websocket-send %socket value))))
			     1 "send"))
		;; addEventListner
		(js-bind! %this obj 'addEventListener
		   :value (js-make-function %this
			     (lambda (this message proc)
				(add-event-listener! this
				      (js-tostring message %this) proc))
			     2 "addEventListener"))
		;; listeners
		(for-each (lambda (act) (bind-listener! %this obj act))
		   (list
		      (cons 'onmessage #f)
		      (cons 'onopen #f)
		      (cons 'onclose #f)
		      (cons 'onerror #f)))
		(set! js-others
		   (cons (cons 'js-websocket-prototype obj) js-others))
		obj)))))

;*---------------------------------------------------------------------*/
;*    hopjs-websocket-server ...                                       */
;*---------------------------------------------------------------------*/
(define (hopjs-websocket-server %this)
   (lambda (this opt)
      (letrec* ((path (if (string? opt) opt (js-get opt 'path %this)))
		(svc (service :name path ()
			(let ((req (current-request)))
			   (websocket-server-response req 0
			      (hopjs-onconnect wss %this)))))
		(wss (instantiate::JsWebSocketServer
			(worker (js-current-worker))
			(svc svc))))
	 (with-access::JsGlobalObject %this (js-object)
	    ;; prototype
	    (with-access::JsObject wss (__proto__)
	       (set! __proto__ (hopjs-websocket-server-prototype %this)))
	    wss))))

;*---------------------------------------------------------------------*/
;*    proc->listener ...                                               */
;*---------------------------------------------------------------------*/
(define (proc->listener %this proc this)
   (lambda (evt)
      (js-call1 %this proc this evt)))

;*---------------------------------------------------------------------*/
;*    hopjs-onconnect ...                                              */
;*---------------------------------------------------------------------*/
(define (hopjs-onconnect wss %this)
   (lambda (resp)
      (with-access::http-response-websocket resp (request)
	 (let ((mutex (make-mutex)))
	    (synchronize mutex
	       (let* ((ws (hopjs-websocket-client %this request wss mutex))
		      (evt (instantiate::server-event
			      (name "connection")
			      (target wss)
			      (value ws))))
		  (with-access::JsWebSocketServer wss (conns worker)
		     (js-worker-push-thunk! worker
			(lambda ()
			   (apply-listeners conns evt))))))))))

;*---------------------------------------------------------------------*/
;*    hopjs-websocket-server-prototype ...                             */
;*---------------------------------------------------------------------*/
(define (hopjs-websocket-server-prototype %this)
   (with-access::JsGlobalObject %this (js-object js-others)
      (let ((old (assq 'js-websocket-server-prototype js-others)))
	 (or old
	     (let ((obj (js-new %this js-object)))
		;; addEventListner
		(js-bind! %this obj 'addEventListener
		   :value (js-make-function %this
			     (lambda (this message proc)
				(add-event-listener! this
				      (js-tostring message %this)
				   (proc->listener %this proc this)))
			     2 "addEventListener"))
		;; listeners
		(for-each (lambda (act) (bind-listener! %this obj act))
		   (list (cons 'onconnection #f)))
		(set! js-others
		   (cons (cons 'js-websocket-server-prototype obj) js-others))
		obj)))))

;*---------------------------------------------------------------------*/
;*    hopjs-websocket-client ...                                       */
;*---------------------------------------------------------------------*/
(define (hopjs-websocket-client %this req wss mutex)
   (with-access::http-request req (socket)
      (let ((ws (instantiate::JsWebSocketClient
		   (socket socket)
		   (__proto__ (hopjs-websocket-client-prototype %this)))))
	 (thread-start!
	    (instantiate::hopthread
	       (body (lambda ()
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
						(apply-listeners onmessages evt)))))))))))))
	 ws)))

;*---------------------------------------------------------------------*/
;*    hopjs-websocket-client-prototype ...                             */
;*---------------------------------------------------------------------*/
(define (hopjs-websocket-client-prototype %this)
   (with-access::JsGlobalObject %this (js-object js-others)
      (let ((old (assq 'js-websocket-client-prototype js-others)))
	 (or old
	     (let ((obj (js-new %this js-object)))
		;; client
		(js-bind! %this obj 'socket
		   :get (js-make-function %this
			   (lambda (this)
			      (with-access::JsWebSocketClient this (socket)
				 socket))
			   0 "socket"))
		;; send
		(js-bind! %this obj 'send
		   :value (js-make-function %this
			     (lambda (this value)
				(with-access::JsWebSocketClient this (socket)
				   (websocket-send socket value)))
			     1 "send"))
		(set! js-others
		   (cons (cons 'js-websocket-client-prototype obj) js-others))
		;; addEventListner
		(js-bind! %this obj 'addEventListener
		   :value (js-make-function %this
			     (lambda (this message proc)
				(let ((action (js-tostring message %this)))
				   (add-event-listener! this action
				      (proc->listener %this proc this))))
			     2 "addEventListener"))
		;; listeners
		(for-each (lambda (act) (bind-listener! %this obj act))
		   (list (cons 'onmessage #f)))
		obj)))))

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
