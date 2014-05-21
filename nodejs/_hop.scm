;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_hop.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr 18 06:41:05 2014                          */
;*    Last change :  Wed May 21 12:19:09 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Hop binding                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__hop

   (library hopscript hop)

;*    (import __nodejs__hop-ws)                                        */
   
   (export (hopjs-process-hop ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    define-js ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (define-js name arity proc . opt)
   `(cons ',name
       (js-make-function %this ,proc ,arity ,(symbol->string name) ,@opt)))
		 
;*---------------------------------------------------------------------*/
;*    hopjs-process-hop ...                                            */
;*---------------------------------------------------------------------*/
(define (hopjs-process-hop %this)
   (with-access::JsGlobalObject %this (js-object)
      (alist->jsobject
	 
	 (list
	    ;; misc
	    (define-js exit 1
	       (lambda (this code)
		  (exit (js-tointeger code %this))))
	    
	    ;; requests
	    (define-js currentRequest 0
	       (lambda (this) (current-request)))
	    
	    (define-js withURL 3
	       (lambda (this url success opt)
		  (hopjs-with-url url success opt %this)))
	    
	    (define-js withHOP 3
	       (lambda (this svc success opt)
		  (hopjs-with-hop svc success opt %this)))
	    
	    ;; charset
	    (define-js charsetConvert 3
	       (lambda (this text from to)
		  (hopjs-charset-convert this text from to %this)))
	    
	    (define-js charset 0
	       (lambda (this) (hop-charset)))
	    
	    (define-js charsetSet 1
	       (lambda (this cs)
		  (hop-charset-set! (string->symbol (js-tostring cs %this)))))
	    
	    ;; responses
	    (define-js HTTPResponseHop 2
	       (lambda (this obj req)
		  (hopjs-response-hop this obj req %this)))
	    
	    (define-js HTTPResponseFile 2
	       (lambda (this file req)
		  (hopjs-response-file this file req %this)))
	    
	    (define-js HTTPResponseAuthentication 2
	       (lambda (this msg req)
		  (hopjs-response-authentication this msg req %this)))
	    
	    (define-js HTTPResponseAsync 1
	       (lambda (this proc req)
		  (hopjs-response-async this proc req %this)))
	    
	    ;; events
	    (define-js signal 2
	       (lambda (this name v)
		  (hop-event-signal! (js-tostring name %this) v)))
	    
	    (define-js broadcast 2
	       (lambda (this name v)
		  (hop-event-broadcast! (js-tostring name %this) v)))
	    
	    ;; lib
	    (define-js parseWebColor 1
	       (lambda (this color)
		  (hopjs-parse-web-color color %this)))
	    
	    (define-js makeWebColor 3 
	       (lambda (this r g b)
		  (make-hex-color r g b)))

	    ))))
;* 	    ;; websocket                                               */
;* 	    (define-js WebSocket 1                                     */
;* 	       (lambda (this) this)                                    */
;* 	       :construct (hopjs-websocket %this))                     */
;* 	    (define-js WebSocketServer 1                               */
;* 	       (lambda (this) this)                                    */
;* 	       :construct (hopjs-websocket-server %this))))))          */

;*---------------------------------------------------------------------*/
;*    hopjs-with-url ...                                               */
;*---------------------------------------------------------------------*/
(define (hopjs-with-url url success opt %this)
   
   (define (parse-json in)
      (js-json-parser in (js-undefined) %this))
   
   (let ((url (js-tostring url %this))
	 (fail #f)
	 (timeout 0)
	 (method "GET"))
      (unless (eq? opt (js-undefined))
	 (let ((f (js-get opt 'fail %this))
	       (t (js-get opt 'timeout %this))
	       (m (js-get opt 'method %this)))
	    (when (isa? f JsFunction)
	       (set! fail (lambda (x) (js-call1 %this f %this x))))
	    (unless (eq? t (js-undefined))
	       (set! timeout (js-tointeger t %this)))
	    (unless (eq? m (js-undefined))
	       (set! method (js-tostring m %this)))))
      (with-url url
	 (if (isa? success JsFunction)
	     (lambda (x) (js-call1 %this success %this x))
	     (lambda (x) x))
	 :parse-json parse-json
	 :fail fail 
	 :timeout timeout
	 :method method)))

;*---------------------------------------------------------------------*/
;*    hopjs-with-hop ...                                               */
;*---------------------------------------------------------------------*/
(define (hopjs-with-hop svc success opt %this)
   
   (define (parse-json in)
      (js-json-parser in (js-undefined) %this))
   
   (let ((host "localhost")
	 (port 8080)
	 (user #f)
	 (password #f)
	 (authorization #f)
	 (fail #f))
      (unless (eq? opt (js-undefined))
	 (let ((h (js-get opt 'host %this))
	       (p (js-get opt 'port %this))
	       (u (js-get opt 'user %this))
	       (w (js-get opt 'password %this))
	       (a (js-get opt 'authorization %this))
	       (f (js-get opt 'fail %this)))
	    (unless (eq? h (js-undefined))
	       (set! host (js-tostring h %this)))
	    (unless (eq? p (js-undefined))
	       (set! port (js-tointeger p %this)))
	    (unless (eq? u (js-undefined))
	       (set! user u))
	    (unless (eq? w (js-undefined))
	       (set! password (js-tostring w %this)))
	    (unless (eq? a (js-undefined))
	       (set! authorization (js-tostring a %this)))
	    (when (isa? f JsFunction)
	       (set! fail (lambda (x) (js-call1 %this f %this x))))))
      (with-hop-remote svc
	 (if (isa? success JsFunction)
	     (lambda (x) (js-call1 %this success %this x))
	     (lambda (x) x))
	 fail
	 :host host :port port 
	 :user user :password password :authorization authorization)))

;*---------------------------------------------------------------------*/
;*    get/default ...                                                  */
;*---------------------------------------------------------------------*/
(define (get/default obj key this def)
   (let ((v (js-get obj key this)))
      (if (eq? v (js-undefined)) def v)))

;*---------------------------------------------------------------------*/
;*    hopjs-response-hop ...                                           */
;*---------------------------------------------------------------------*/
(define (hopjs-response-hop this obj req %this)
   (if (isa? req JsObject)
       (instantiate::http-response-hop
	  (backend (get/default req 'backend %this (hop-xml-backend)))
	  (request (get/default req 'currentRequest %this (current-request)))
	  (start-line (get/default req 'startLine %this "HTTP/1.1 200 Ok"))
	  (content-type (get/default req 'contenttype %this #f))
	  (charset (get/default req 'charset %this (hop-charset)))
	  (value obj))
       (instantiate::http-response-hop
	  (backend (hop-xml-backend))
	  (request (if (eq? req (js-undefined)) (current-request) req))
	  (value obj))))

;*---------------------------------------------------------------------*/
;*    hopjs-response-file ...                                          */
;*---------------------------------------------------------------------*/
(define (hopjs-response-file this file req %this)
   (if (isa? req JsObject)
       (instantiate::http-response-file
	  (request (get/default req 'currentRequest %this (current-request)))
	  (charset (get/default req 'charset %this (hop-charset)))
	  (content-type (get/default req 'contenttype %this #f))
	  (file (js-tostring file %this)))
       (instantiate::http-response-file
	  (request (if (eq? req (js-undefined)) (current-request) req))
	  (file (js-tostring file %this)))))

;*---------------------------------------------------------------------*/
;*    hopjs-response-authentication ...                                */
;*---------------------------------------------------------------------*/
(define (hopjs-response-authentication this msg req %this)
   (user-access-denied
      (if (eq? req (js-undefined)) (current-request) req)
      (js-tostring msg %this)))

;*---------------------------------------------------------------------*/
;*    hopjs-response-async ...                                         */
;*---------------------------------------------------------------------*/
(define (hopjs-response-async this proc req %this)
   
   (define (async-proc req)
      (lambda (k)
	 (with-handler
	    (lambda (e)
	       (cond
		  ((isa? e JsError) (exception-notify e))
		  ((isa? e &error) (error-notify e)))
	       #f)
	    (js-call1 %this proc %this
	       (js-make-function %this
		  (lambda (this resp)
		     (k (scheme->response resp req)))
		  1 "reply")))))
   
   (if (isa? req JsObject)
       (let ((req (get/default req 'currentRequest %this (current-request))))
	  (instantiate::http-response-async
	     (request req)
	     (charset (get/default req 'charset %this (hop-charset)))
	     (content-type (get/default req 'contenttype %this #f))
	     (async (async-proc req))))
       (let ((req (if (eq? req (js-undefined)) (current-request) req)))
	  (instantiate::http-response-async
	     (request req)
	     (async (async-proc req))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-web-color ...                                        */
;*---------------------------------------------------------------------*/
(define (hopjs-parse-web-color color %this)
   (with-access::JsGlobalObject %this (js-object)
      (multiple-value-bind (r g b)
	 (parse-web-color (js-tostring color %this))
	 (let ((obj (js-new %this js-object)))
	    (js-put! obj 'red r #f %this)
	    (js-put! obj 'green g #f %this)
	    (js-put! obj 'blue b #f %this)
	    obj))))

;*---------------------------------------------------------------------*/
;*    hopjs-charset-convert ...                                        */
;*---------------------------------------------------------------------*/
(define (hopjs-charset-convert this text from to %this)
   (let ((from (js-tostring from %this))
         (to (js-tostring to %this)))
      (charset-convert text
         (if (string? from) (string->symbol from) (hop-locale))
         (if (string? to) (string->symbol to) (hop-charset)))))
   
