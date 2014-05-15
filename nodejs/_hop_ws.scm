;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_hop_ws.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 15 05:51:37 2014                          */
;*    Last change :  Thu May 15 09:09:39 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Hop WebSockets                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__hop-ws

   (library hopscript hop)

   (export (hopjs-websocket ::JsGlobalObject)
	   (hopjs-websocket-server ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    hopjs-websocket ...                                              */
;*---------------------------------------------------------------------*/
(define (hopjs-websocket %this)
   (lambda (this url)
      (with-access::JsGlobalObject %this (js-object)
	 (let* ((obj (js-new %this js-object))
		(url (js-tostring url %this))
		(ws (instantiate::websocket (url url))))
	    (websocket-connect! ws)
	    ;; prototype
	    (with-access::JsObject obj (__proto__)
	       (set! __proto__ (hopjs-websocket-prototype %this)))
	    ;; web socket fields
	    (js-put! obj '%websocket ws #f %this)
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
			      (let ((ws (js-get this '%websocket %this)))
				 (with-access::websocket ws (state)
				    (symbol->string state))))
			   0 "readyState"))
		;; url
		(js-bind! %this obj 'url
		   :get (js-make-function %this
			   (lambda (this)
			      (let ((ws (js-get this '%websocket %this)))
				 (with-access::websocket ws (url)
				    url)))
			   0 "url"))
		;; send
		(js-bind! %this obj 'send
		   :value (js-make-function %this
			     (lambda (this value)
				(let ((ws (js-get this '%websocket %this)))
				   (websocket-send ws value)))
			     1 "send"))
		;; addEventListner
		(js-bind! %this obj 'addEventListener
		   :value (js-make-function %this
			     (lambda (this message proc)
				(let ((ws (js-get this '%websocket %this)))
				   (add-event-listener! ws
					 (js-tostring message %this)
				      (lambda (evt)
					 (js-call1 %this proc this evt)))))
			     2 "addEventListener"))
		(set! js-others
		   (cons (cons 'js-websocket-prototype obj) js-others))
		obj)))))

;*---------------------------------------------------------------------*/
;*    hopjs-websocket-server ...                                       */
;*---------------------------------------------------------------------*/
(define (hopjs-websocket-server %this)
   (lambda (this opt)
      (let* ((path (if (string? opt) opt (js-get opt 'path %this)))
	     (svc (service :name path () (tprint "GLIP"))))
	 (with-access::JsGlobalObject %this (js-object)
	    (let ((obj (js-new %this js-object)))
	       (js-put! obj '%service svc #f %this)
	       obj)))))

