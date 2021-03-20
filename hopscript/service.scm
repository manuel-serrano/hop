;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/service.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct 17 08:19:20 2013                          */
;*    Last change :  Wed Apr  8 08:15:07 2020 (serrano)                */
;*    Copyright   :  2013-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript service implementation                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_service

   (library hop js2scheme)

   (include "types.sch"
	    "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_object
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_property
	   __hopscript_function
	   __hopscript_worker
	   __hopscript_json
	   __hopscript_lib
	   __hopscript_array
	   __hopscript_promise
	   __hopscript_websocket)

   (export (js-init-service! ::JsGlobalObject)
	   (js-make-hopframe ::JsGlobalObject ::obj ::obj ::obj)
	   (js-create-service::JsService ::JsGlobalObject ::obj ::obj ::obj ::bool ::bool ::WorkerHopThread)
	   (js-make-service::JsService ::JsGlobalObject ::procedure ::bstring ::bool ::bool ::int ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsService ...                                */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsService
   (lambda (o)
      (with-access::JsService o (svc)
	 (if svc
	     (with-access::hop-service svc (path resource)
		(vector path resource))
	     (vector #t #t))))
   (lambda (o ctx)
      (let* ((path (vector-ref o 0))
	     (svcp (lambda (this . args)
		      (js-make-hopframe ctx this path args)))
	     (hopsvc (cond
			((service-exists? path)
			 (get-service path))
			(else
			 (instantiate::hop-service
			    (id (string->symbol path))
			    (wid (let ((i (string-index path #\?)))
				    (string->symbol
				       (if i (substring path 0 i) path))))
			    (args '())
			    (proc (lambda l l))
			    (javascript "")
			    (path path)
			    (resource (vector-ref o 1)))))))
	 (js-make-service ctx svcp
	    (basename path)
	    #f #f -1 (js-current-worker)
	    hopsvc))))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsHopFrame ...                               */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsHopFrame
   (lambda (o)
      (with-access::JsHopFrame o (srv path args options header path)
	 (vector (unless (isa? srv JsWebSocket) srv) path args options header)))
   (lambda (o ctx)
      (if (and (vector? o) (=fx (vector-length o) 5))
	  (with-access::JsGlobalObject ctx (js-hopframe-prototype)
	     (instantiateJsHopFrame
		(__proto__ js-hopframe-prototype)
		(%this ctx)
		(srv (vector-ref o 0))
		(path (vector-ref o 1))
		(args (vector-ref o 2))
		(options (vector-ref o 3))
		(header (vector-ref o 4))))
	 (error "HopFrame" "wrong frame" o))))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsServer ...                                 */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsServer
   (lambda (o ctx)
      (if (isa? ctx JsGlobalObject)
	  (with-access::JsServer o (obj)
	     (with-access::server obj (host port ssl authorization version)
		(vector host port ssl
		   (if (string? authorization) authorization #f)
		   version
		   (js-jsobject->plist o ctx))))
	  (error "obj->string ::JsServer" "Not a JavaScript context" ctx)))
   (lambda (o ctx)
      (cond
	 ((not (isa? ctx JsGlobalObject))
	  (error "string->obj ::JsServer" "Not a JavaScript context" ctx))
	 ((and (vector? o) (=fx (vector-length o) 6))
	  (with-access::JsGlobalObject ctx (js-server-prototype)
	     (let ((srv (instantiateJsServer
			   (__proto__  js-server-prototype)
			   (obj (instantiate::server
				   (host (vector-ref o 0))
				   (port (vector-ref o 1))
				   (ssl (vector-ref o 2))
				   (ctx ctx)
				   (authorization (vector-ref o 3))
				   (version (vector-ref o 4)))))))
		(let loop ((rest (vector-ref o 5)))
		   (if (null? rest)
		       srv
		       (begin
			  (js-put! srv (js-keyword->jsstring (car rest))
			     (js-obj->jsobject (cadr rest) ctx)
			     #f ctx)
			  (loop (cddr rest))))))))
	 (else
	  (error "JsServer" "wrong server" o)))))

;*---------------------------------------------------------------------*/
;*    js-tostring ::JsHopFrame ...                                     */
;*---------------------------------------------------------------------*/
(define-method (js-tostring obj::JsHopFrame %this)
   (hopframe->string obj %this))

;*---------------------------------------------------------------------*/
;*    xml-primitive-value ::JsHopFrame ...                             */
;*---------------------------------------------------------------------*/
(define-method (xml-primitive-value o::JsHopFrame ctx)
   (with-access::JsHopFrame o (path)
      (hopframe->string o ctx)))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsService ...                                        */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsService worker::WorkerHopThread %_this)

   (define (relative-path path)
      (substring path (string-length (hop-service-base))))
   
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsService obj (svc info arity)
	 (with-access::hop-service svc (path)
	    (let* ((path (relative-path path))
		   (proc (js-make-function %this
			    (lambda (this) (js-undefined))
			    arity
			    (js-function-info :name (vector-ref info 0)
			       :len (vector-ref info 1))
			    :prototype #f
			    :__proto__ #f
			    :strict 'strict
			    :minlen -1
			    :alloc js-not-a-constructor-alloc
			    :constrsize 3
			    :method #f))
		   (nobj (js-create-service %this proc path #f #f #t worker)))
	       (js-for-in obj
		  (lambda (k %this)
		     (js-put! nobj (js-donate k worker %_this)
			(js-donate (js-get obj k %_this) worker %_this)
			#f %this))
		  %this)
	       nobj)))))
   
;*---------------------------------------------------------------------*/
;*    xml-unpack ::JsService ...                                       */
;*---------------------------------------------------------------------*/
(define-method (xml-unpack o::JsService ctx)
   o)

;*---------------------------------------------------------------------*/
;*    xml-attribute-encode ::JsHopFrame ...                            */
;*---------------------------------------------------------------------*/
(define-method (xml-attribute-encode o::JsHopFrame)
   (with-access::JsHopFrame o (%this)
      (hopframe->string o %this)))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsService ...                                  */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsService op compile isexpr ctx)
   (with-access::JsService o (svc)
      (compile svc op)))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsHopFrame ...                                 */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsHopFrame op compile isexpr ctx)
   (display "hop_url_encoded_to_obj('" op)
   (display (url-path-encode (obj->string o 'hop-client)) op)
   (display "')" op))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsServer ...                                   */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsServer op compile isexpr ctx)
   (display "hop_url_encoded_to_obj('" op)
   (display (url-path-encode (obj->string o ctx)) op)
   (display "')" op))

;*---------------------------------------------------------------------*/
;*    hop-register-value ::object ...                                  */
;*---------------------------------------------------------------------*/
(define-method (hop-register-value o::object register::procedure)
   #t)

;*---------------------------------------------------------------------*/
;*    json->obj ::JsObject ...                                         */
;*---------------------------------------------------------------------*/
(define-method (json->obj %this::JsObject ip::input-port)
   (js-json-parser ip #f #f #t %this))

;*---------------------------------------------------------------------*/
;*    post-multipart->obj ::JsGlobalObject ...                         */
;*---------------------------------------------------------------------*/
(define-method (post-multipart->obj %this::JsGlobalObject val enc)
   (cond
      ((string=? enc "string") (js-string->jsstring val))
      ((string=? enc "integer") (js-string->number val %this))
      ((string=? enc "keyword") (js-string->jsstring val))
      ((string=? enc "file") val)
      (else (string->obj val #f %this))))

;*---------------------------------------------------------------------*/
;*    j2s-js-literal ::JsService ...                                   */
;*---------------------------------------------------------------------*/
(define-method (j2s-js-literal o::JsService ctx)
   (with-access::JsService o (svc)
      (with-access::hop-service svc (path)
	 (format "HopService( '~a', undefined )" path))))

;*---------------------------------------------------------------------*/
;*    js-init-service! ...                                             */
;*---------------------------------------------------------------------*/
(define (js-init-service! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-function
					 js-service-pcache
					 js-service-prototype
					 js-hopframe-prototype)
      (let ((js-function-prototype (js-object-proto js-function)))

	 ;; local constant strings initialization
	 (unless (vector? __js_strings) (set! __js_strings (&init!)))

	 ;; service pcache
	 (set! js-service-pcache
	    ((@ js-make-pcache-table __hopscript_property) 1 "service"))
	 
	 ;; service prototype
	 (with-access::JsGlobalObject %this (__proto__)
	    (set! js-service-prototype
	       (instantiateJsService
		  (__proto__ js-function-prototype)
		  (worker (class-nil WorkerHopThread))
		  (alloc js-not-a-constructor-alloc)
		  (prototype (js-object-proto %this))
		  (info (js-function-info :name "" :len -1))
		  (procedure list)
		  (svc #f))))

	 (js-bind! %this js-service-prototype (& "name")
	    :value (js-ascii->jsstring "service")
	    :writable #f
	    :configurable #f
	    :enumerable #f
	    :hidden-class #t)
	 (js-bind! %this js-service-prototype (& "resource")
	    :value (js-make-function %this
		      (lambda (this file)
			 (js-string->jsstring
			    (service-resource this (js-jsstring->string file))))
		      (js-function-arity 1 0)
		      (js-function-info :name "resource" :len 1))
	    :writable #t
	    :configurable #t
	    :enumerable #f
	    :hidden-class #t)
	 (js-bind! %this js-service-prototype (& "unregister")
	    :value (js-make-function %this
		      (lambda (this)
			 (when (isa? this JsService)
			    (with-access::JsService this (svc)
			       (unregister-service! svc)))
			 (js-undefined))
		      (js-function-arity 0 0)
		      (js-function-info :name "unregister" :len 0))
	    :writable #t
	    :configurable #t
	    :enumerable #f
	    :hidden-class #t)
	 
	 (js-bind! %this js-service-prototype (& "timeout")
	    :get (js-make-function %this
		    (lambda (this)
		       (with-access::JsService this (svc)
			  (with-access::hop-service svc (timeout)
			     timeout)))
		    (js-function-arity 0 0)
		    (js-function-info :name "timeout" :len 0))
	    :hidden-class #t)
	 
	 (js-bind! %this js-service-prototype (& "ttl")
	    :get (js-make-function %this
		    (lambda (this)
		       (with-access::JsService this (svc)
			  (with-access::hop-service svc (ttl)
			     ttl)))
		    (js-function-arity 0 0)
		    (js-function-info :name "ttl.get" :len 0))
	    :set (js-make-function %this
		    (lambda (this v)
		       (with-access::JsService this (svc)
			  (with-access::hop-service svc (ttl)
			     (set! ttl (js-tointeger v %this)))))
		    (js-function-arity 1 0)
		    (js-function-info :name "ttl.set" :len 1))
	    :hidden-class #t)
	 
	 (js-bind! %this js-service-prototype (& "addURL")
	    :value (js-make-function %this
		      (lambda (this url)
			 (service-add-url! this url %this))
		      (js-function-arity 1 0)
		      (js-function-info :name "addURL" :len 1))
	    :hidden-class #t)
	 
	 (js-bind! %this js-service-prototype (& "removeURL")
	    :value (js-make-function %this
		      (lambda (this url)
			 (service-remove-url! this url %this))
		      (js-function-arity 1 0)
		      (js-function-info :name "removeURL" :len 1))
	    :hidden-class #t)
	 
	 (js-bind! %this js-service-prototype (& "getURLs")
	    :value (js-make-function %this
		      (lambda (this)
			 (service-get-urls this %this))
		      (js-function-arity 0 0)
		      (js-function-info :name "getURLs" :len 0))
	    :hidden-class #t)
	 
	 ;; HopFrame prototype and constructor
	 (set! js-hopframe-prototype
	    (instantiateJsObject
	       (__proto__ (js-object-proto %this))
	       (elements ($create-vector 8))))
	 
	 (js-bind! %this js-hopframe-prototype (& "post")
	    :value (js-make-function %this
		      (lambda (this::JsHopFrame success fail-or-opt)
			 (with-access::JsHopFrame this (args)
			    (post this success fail-or-opt %this #t)))
		      (js-function-arity 2 0)
		      (js-function-info :name "post" :len 2))
	    :hidden-class #t)
	 (js-bind! %this js-hopframe-prototype (& "postSync")
	    :value (js-make-function %this
		      (lambda (this::JsHopFrame opt)
			 (with-access::JsHopFrame this (args)
			    (post this #f opt %this #f)))
		      (js-function-arity 1 0)
		      (js-function-info :name "postSync" :len 2))
	    :hidden-class #t)
	 (js-bind! %this js-hopframe-prototype (& "call")
	    :value (js-make-function %this
		      (lambda (this::JsHopFrame req opt)
			 (with-access::JsHopFrame this (path args)
			    (let ((svc (get-service path)))
			       (with-access::hop-service svc (proc)
				  (apply proc req args)))))
		      (js-function-arity 2 0)
		      (js-function-info :name "call" :len 2))
	    :hidden-class #t)
	 (js-bind! %this js-hopframe-prototype (& "toString")
	    :value (js-make-function %this
		      (lambda (this::JsHopFrame)
			 (js-string->jsstring (hopframe->string this %this)))
		      (js-function-arity 0 0)
		      (js-function-info :name "toString" :len 0))
	    :hidden-class #t)
	 (js-bind! %this js-hopframe-prototype (& "inspect")
	    :value (js-make-function %this
		      (lambda (this::JsHopFrame)
			 (js-string->jsstring (hopframe->string this %this)))
		      (js-function-arity 0 0)
		      (js-function-info :name "inspect" :len 0))
	    :hidden-class #t)
	 (js-bind! %this js-hopframe-prototype (& "getHeader")
	    :value (js-make-function %this
		      (lambda (this::JsHopFrame)
			 (with-access::JsHopFrame this (header)
			    header))
		      (js-function-arity 0 0)
		      (js-function-info :name "getHeader" :len 0))
	    :hidden-class #t)
	 (js-bind! %this js-hopframe-prototype (& "setHeader")
	    :value (js-make-function %this
		      (lambda (this::JsHopFrame hd)
			 (with-access::JsHopFrame this (header)
			    (set! header hd)
			    this))
		      (js-function-arity 1 0)
		      (js-function-info :name "setHeader" :len 1))
	    :hidden-class #t)
	 (js-bind! %this js-hopframe-prototype (& "getOptions")
	    :value (js-make-function %this
		      (lambda (this::JsHopFrame opts)
			 opts)
		      (js-function-arity 1 0)
		      (js-function-info :name "getOptions" :len 1))
	    :hidden-class #t)
	 (js-bind! %this js-hopframe-prototype (& "setOptions")
	    :value (js-make-function %this
		      (lambda (this::JsHopFrame opts)
			 (with-access::JsHopFrame this (options)
			    (set! options opts)
			    this))
		      (js-function-arity 1 0)
		      (js-function-info :name "setOptions" :len 1))
	    :hidden-class #t)

	 (define (%js-service this proc path)
	    (js-create-service %this proc
	       (unless (eq? path (js-undefined))
		  (js-tostring path %this))
	       #f #t #f (js-current-worker)))

	 (define (%js-hopframe this path args)
	    (js-make-hopframe %this this path args))
	 
	 (letrec ((js-service (js-make-function %this
				 %js-service
				 (js-function-arity %js-service)
				 (js-function-info :name "Service" :len 2)
				 :__proto__ js-function-prototype
				 :prototype js-service-prototype
				 :size 5
				 :alloc js-no-alloc))
		  (js-hopframe (js-make-function %this
				  %js-hopframe
				  (js-function-arity %js-hopframe)
				  (js-function-info :name "HopFrame" :len 1)
				  :__proto__ js-function-prototype
				  :prototype js-hopframe-prototype
				  :alloc js-no-alloc)))
	    (js-bind! %this %this (& "Service")
	       :configurable #f :enumerable #f :value js-service
	       :hidden-class #t)
	    (js-bind! %this %this (& "HopFrame")
	       :configurable #f :enumerable #f :value js-hopframe
	       :hidden-class #t)
	    
	    (js-bind! %this js-service (& "exists")
	       :configurable #f :enumerable #f
	       :value (js-make-function %this
			 (lambda (this svc)
			    (service-exists? (js-tostring svc %this)))
			 (js-function-arity 1 0)
			 (js-function-info :name "exists" :len 1))
	       :hidden-class #t)
	    (js-bind! %this js-service (& "getService")
	       :configurable #f :enumerable #f
	       :value (js-make-function %this
			 (lambda (this svc)
			    (let* ((name (js-tostring svc %this))
				   (svc (get-service-from-name name)))
			       (js-make-service %this
				  (lambda (this . args)
				     (with-access::hop-service svc (path)
					(js-make-hopframe %this this path args)))
				  name #f #f -1 (js-current-worker) svc)))
			 (js-function-arity 1 0)
			 (js-function-info :name "getService" :len 1))
	       :hidden-class #t)
	    (js-bind! %this js-service (& "getServiceFromPath")
	       :configurable #f :enumerable #f
	       :value (js-make-function %this
			 (lambda (this svc)
			    (let* ((name (js-tostring svc %this))
				   (svc (get-service name)))
			       (js-make-service %this
				  (lambda (this . args)
				     (with-access::hop-service svc (path)
					(js-make-hopframe %this this path args)))
				  name #f #f -1 (js-current-worker) svc)))
			 (js-function-arity 1 0)
			 (js-function-info :name "getService" :len 1))
	       :hidden-class #t)
	    
	    (js-bind! %this js-service (& "allowURL")
	       :configurable #f :enumerable #f
	       :value (js-make-function %this
			 (lambda (this url)
			    (cond
			       ((not (hop-filters-open?))
				(js-raise-type-error %this
				   "allowURL must called from within the hoprc.js file"
				   url))
			       ((not (memq service-url-filter (hop-filters)))
				(set! *url-redirect* (create-hashtable))
				(hop-filter-add! service-url-filter)
				(add-service-allow-url! url))
			       (else
				(add-service-allow-url! url))))
			 (js-function-arity 1 0)
			 (js-function-info :name "allowURL" :len 1))
	       :hidden-class #t))
	 
	 (js-undefined))))

;*---------------------------------------------------------------------*/
;*    js-make-hopframe ...                                             */
;*---------------------------------------------------------------------*/
(define (js-make-hopframe %this::JsGlobalObject srv path args)
   (with-access::JsGlobalObject %this (js-hopframe-prototype js-object js-server)
      (instantiateJsHopFrame
	 (__proto__ js-hopframe-prototype)
	 (%this %this)
	 (srv srv)
	 (path path)
	 (args args))))

;*---------------------------------------------------------------------*/
;*    hopframe->string ...                                             */
;*---------------------------------------------------------------------*/
(define (hopframe->string::bstring frame::JsHopFrame %this)
   (with-access::JsHopFrame frame (srv path args)
      (let ((sans-srv (if (pair? args)
			  (hop-apply-url path args %this)
			  path)))
	 (if (isa? srv JsServer)
	     (with-access::JsServer srv (obj)
		(with-access::server obj (ssl host port authorization)
		   (let ((scheme (if ssl "https" "http")))
		      (if authorization
			  (format "~a://~a:~a:~a~a" scheme
			     authorization host port sans-srv)
			  (format "~a://~a:~a~a" scheme
			     host port sans-srv)))))
	     sans-srv))))

;*---------------------------------------------------------------------*/
;*    post ...                                                         */
;*---------------------------------------------------------------------*/
(define (post this::JsHopFrame success fail-or-opt %this async)
   
   (define (multipart-form-arg val)
      (cond
	 ((string? val)
	  `("string" ,val "hop-encoding: string"))
	 ((js-jsstring? val)
	  `("string" ,(js-jsstring->string val) "hop-encoding: string"))
	 ((integer? val)
	  `("integer" ,val "hop-encoding: integer"))
	 ((keyword? val)
	  `("keyword" ,(keyword->string val) "hop-encoding: keyword"))
	 (else
	  `("hop" ,(obj->string val %this) "hop-encoding: hop"))))
   
   (define (scheme->js val)
      (js-obj->jsobject val %this))
   
   (define (js-get-string opt key)
      (let ((v (js-get opt key %this)))
	 (unless (eq? v (js-undefined))
	    (js-tostring v %this))))
   
   (define (post-request-thread name %this callback fail scheme host port auth)
      (with-access::JsHopFrame this (path args header options)
	 (let ((user (js-get-string options (& "user")))
	       (password (js-get-string options (& "password")))
	       (header (when header (js-jsobject->alist header %this)))
	       (args (map multipart-form-arg args))
	       (receiver (lambda (ctx thunk)
			    (with-access::JsGlobalObject ctx (worker)
			       (js-worker-exec worker path #t thunk)))))
	    (thread-start!
	       (instantiate::hopthread
		  (name name)
		  (body (lambda ()
			   (with-handler
			      (lambda (e)
				 (exception-notify e)
				 (with-access::JsGlobalObject %this (worker)
				    (js-worker-exec worker path #t
				       (lambda ()
					  (fail e)))))
			      (with-hop-remote path callback fail
				 :scheme scheme
				 :host host :port port 
				 :user user :password password
				 :authorization auth
				 :header header
				 :ctx %this
				 :receiver receiver
				 :json-parser json-parser
				 :x-javascript-parser x-javascript-parser
				 :connection-timeout (hop-connection-timeout)
				 :args args))))))
	    (js-undefined))))
   
   (define (post-server-promise this %this host port auth scheme)
      (with-access::JsGlobalObject %this (js-promise)
	 (letrec* ((callback (lambda (x)
				(js-promise-async p
				   (lambda ()
				      (js-promise-resolve p x)))))
		   (fail (lambda (x)
			    (js-promise-async p
			       (lambda ()
				  (js-promise-reject p x)))))
		   (p (js-new %this js-promise
			 (js-make-function %this
			    (lambda (_ resolve reject)
			       (post-request-thread "post-server-promise"
				  %this callback fail scheme host port auth))
			    (js-function-arity 2 0)
			    (js-function-info :name "executor" :len 2)))))
	    p)))
   
   (define (post-server-async this success failure %this host port auth scheme)
      (with-access::JsHopFrame this (path)
	 (with-access::JsGlobalObject %this (worker)
	    (let ((callback (when (js-procedure? success)
			       (lambda (x)
				  (js-call1 %this success %this x))))
		  (fail (if (js-procedure? failure)
			    (lambda (obj)
			       (js-call1 %this failure %this obj))
			    exception-notify)))
	       (post-request-thread "post-async"
		  %this callback fail scheme host port auth)))))
   
   (define (post-server-sync this %this host port auth scheme)
      (with-access::JsHopFrame this (path args header options)
	 (let ((receiver (lambda (ctx thunk)
			    (with-access::JsGlobalObject ctx (worker)
			       (js-worker-exec worker path #t thunk)))))
	    (with-hop-remote path (lambda (x) x) #f
	       :scheme scheme
	       :host host :port port 
	       :user (js-get-string options (& "user"))
	       :password (js-get-string options (& "password"))
	       :authorization auth
	       :header (when header (js-jsobject->alist header %this))
	       :ctx %this
	       :receiver receiver
	       :json-parser json-parser
	       :x-javascript-parser x-javascript-parser
	       :args (map multipart-form-arg args)))))
   
   (define (post-server this success failure %this async host port auth scheme)
      (cond
	 ((not async)
	  (post-server-sync this %this host port auth scheme))
	 ((or (js-procedure? success) (js-procedure? failure))
	  (post-server-async this success failure %this host port auth scheme))
	 (else
	  (post-server-promise this %this host port auth scheme))))
   
   (define (post-websocket-promise this::JsHopFrame srv::JsWebSocket)
      (with-access::JsWebSocket srv (recvqueue)
	 (with-access::JsGlobalObject %this (js-promise)
	    (let ((frameid (get-frame-id)))
	       (letrec ((p (js-new %this js-promise
			      (js-make-function %this
				 (lambda (_ resolve reject)
				    (js-websocket-post srv this frameid))
				 (js-function-arity 2 0)
				 (js-function-info :name "executor" :len 2)))))
		  (cell-set! recvqueue
		     (cons (cons frameid p) (cell-ref recvqueue)))
		  p)))))
   
   (define (post-websocket-async this::JsHopFrame srv::JsWebSocket
	      success failure)
      (with-access::JsHopFrame this (path)
	 (let ((callback (when (js-procedure? success)
			    (lambda (x)
			       (js-worker-push-thunk! (js-current-worker) path
				  (lambda ()
				     (js-call1 %this success %this x))))))
	       (fail (when (js-procedure? failure)
			(lambda (obj)
			   (js-worker-push-thunk! (js-current-worker) path
			      (lambda ()
				 (js-call1 %this failure %this obj)))))))
	    (with-access::JsWebSocket srv (ws recvqueue)
	       (with-access::websocket ws (%mutex)
		  (let ((frameid (get-frame-id)))
		     (js-websocket-post srv this frameid)
		     (cell-set! recvqueue
			(cons (cons frameid (cons callback fail))
			   (cell-ref recvqueue)))))))))

   (define (post-websocket-sync this::JsHopFrame srv::JsWebSocket)
      (with-access::JsHopFrame this (path)
	 (with-access::JsWebSocket srv (ws recvqueue)
	    (with-access::websocket ws (%mutex %socket)
	       (if (socket? %socket)
		   (let* ((frameid (get-frame-id))
			  (cv (make-condition-variable))
			  (cell (cons cv %mutex)))
		      (js-websocket-post srv this frameid)
		      (cell-set! recvqueue
			 (cons (cons frameid cell) (cell-ref recvqueue)))
		      (condition-variable-wait! cv %mutex)
		      (or (car cell) (raise (cdr cell))))
		   (js-raise-type-error %this "not connected ~s" srv))))))
   
   (define (post-websocket this::JsHopFrame success failure %this async srv::JsWebSocket)
      (with-access::JsWebSocket srv (ws)
	 (with-access::websocket ws (%mutex)
	    (synchronize %mutex
	       (cond
		  ((not async)
		   (post-websocket-sync this srv))
		  ((or (js-procedure? success) (js-procedure? failure))
		   (post-websocket-async this srv success failure))
		  (else
		   (post-websocket-promise this srv)))))))

   (with-access::JsHopFrame this (srv)
      (cond
	 ((isa? srv JsServer)
	  (with-access::JsServer srv (obj)
	     (with-access::server obj (host port authorization ssl)
		(post-server this success fail-or-opt %this async
		   host port authorization (if ssl 'https 'http)))))
	 ((isa? srv JsWebSocket)
	  (post-websocket this success fail-or-opt %this async srv))
	 ((or (js-procedure? fail-or-opt) (not (js-object? fail-or-opt)))
	  (post-server this success fail-or-opt %this async
	     "localhost" (hop-default-port) #f (hop-default-scheme)))
	 (else
	  (with-access::JsHopFrame this (path args)
	     (post-options-deprecated path (map multipart-form-arg args)
		success fail-or-opt %this async))))))

;*---------------------------------------------------------------------*/
;*    get-frame-id ...                                                 */
;*---------------------------------------------------------------------*/
(define (get-frame-id)
   (let ((v frame-id))
      (set! frame-id (+fx 1 frame-id))
      v))

(define frame-id 0)

;*---------------------------------------------------------------------*/
;*    post-options-deprecated ...                                      */
;*---------------------------------------------------------------------*/
(define (post-options-deprecated svc::bstring args success opt %this async)
   (let ((host "localhost")
	 (port (hop-default-port))
	 (user #f)
	 (password #f)
	 (authorization #f)
	 (fail #f)
	 (failjs #f)
	 (asynchronous async)
	 (header #f)
	 (scheme 'http)
	 (worker (js-current-worker)))
      (cond
	 ((js-procedure? opt)
	  (set! fail
	     (if asynchronous
		 (lambda (obj)
		    (js-worker-push-thunk! worker svc
		       (lambda ()
			  (js-call1 %this opt %this obj))))
		 (lambda (obj)
		    (js-call1 %this opt %this obj)))))
	 ((not (eq? opt (js-undefined)))
	  (let* ((v (js-get opt (& "server") %this))
		 (o (if (eq? v (js-undefined)) opt v))
		 (a (js-get o (& "authorization") %this))
		 (h (js-get o (& "host") %this))
		 (p (js-get o (& "port") %this))
		 (u (js-get opt (& "user") %this))
		 (w (js-get opt (& "password") %this))
		 (f (js-get opt (& "fail") %this))
		 (y (js-get opt (& "asynchronous") %this))
		 (s (js-get opt (& "scheme") %this))
		 (c (js-get opt (& "ssl") %this))
		 (r (js-get opt (& "header") %this)))
	     (unless (eq? h (js-undefined))
		(set! host (js-tostring h %this)))
	     (unless (eq? p (js-undefined))
		(set! port (js-tointeger p %this)))
	     (unless (eq? u (js-undefined))
		(set! user (js-tostring u %this)))
	     (unless (eq? w (js-undefined))
		(set! password (js-tostring w %this)))
	     (unless (eq? a (js-undefined))
		(set! authorization (js-tostring a %this)))
	     (unless (js-totest y)
		(when (js-in? opt (& "asynchronous") %this)
		   (set! asynchronous #f)))
	     (when (js-totest c)
		(set! scheme 'https))
	     (unless (eq? s (js-undefined))
		(set! scheme (string->symbol (js-tostring s %this))))
	     (when (js-procedure? f)
		(set! failjs f)
		(set! fail
		   (lambda (obj)
		      (if asynchronous
			  (js-worker-push-thunk! worker svc
			     (lambda ()
				(js-call1 %this f %this obj)))
			  (js-call1 %this f %this obj)))))
	     (when (js-object? r)
		(set! header (js-jsobject->alist r %this))))))

      (define (scheme->js val)
	 (js-obj->jsobject val %this))
      
      (define (post-request callback)
	 (let ((receiver (lambda (ctx thunk)
			    (with-access::JsGlobalObject ctx (worker)
			       (js-worker-exec worker svc #t thunk)))))
	    (with-hop-remote svc callback fail
	       :scheme scheme
	       :host host :port port 
	       :user user :password password :authorization authorization
	       :header header
	       :ctx %this
	       :receiver receiver
	       :json-parser json-parser
	       :x-javascript-parser x-javascript-parser
	       :args args)))

      (define (spawn-thread)
	 (thread-start!
	    (instantiate::hopthread
	       (name "spawn-thread-deprecated")
	       (body (lambda ()
			(with-handler
			   (lambda (e)
			      (exception-notify e)
			      (if failjs
				  (js-worker-push-thunk! worker svc
				     (lambda ()
					(js-call1 %this failjs %this e)))
				  (begin
				     (exception-notify e)
				     (display "This error has been notified because no error\n" (current-error-port))
				     (display "handler was provided for the post invocation.\n" (current-error-port)))))
			   (post-request
			      (lambda (x)
				 (js-call1 %this success %this
				    (scheme->js x)))))))))
	 (js-undefined))

      (define (spawn-promise)
	 (with-access::JsGlobalObject %this (js-promise)
	    (js-new %this js-promise
	       (js-make-function %this
		  (lambda (this resolve reject)
		     (set! fail
			(lambda (obj)
			   (js-call1 %this reject %this obj)))
		     (thread-start!
			(instantiate::hopthread
			   (name "spawn-promise-deprecated")
			   (body (lambda ()
				    (with-handler
				       (lambda (e)
					  (js-call1 %this reject %this e))
				       (post-request
					  (lambda (x)
					     (js-call1 %this resolve %this
						(scheme->js x))))))))))
		  (js-function-arity 2 0)
		  (js-function-info :name "executor" :len 2)))))

      (if asynchronous
	  (if (js-procedure? success)
	      (spawn-thread)
	      (spawn-promise))
	  (post-request
	     (if (js-procedure? success)
		 (lambda (x) (js-call1 %this success %this (scheme->js x)))
		 scheme->js)))))      

;*---------------------------------------------------------------------*/
;*    js-create-service ...                                            */
;*    -------------------------------------------------------------    */
;*    This function is called when the HopScript constructor           */
;*    Service is directly invoked from user programs. The role         */
;*    of this function is to build an internal hop-service that        */
;*    merely calls the HopScript function passed as argument.          */
;*    The complexity of this function comes when the optional          */
;*    "args" arguments is an object, in which case, the function       */
;*    builds a service with optional named arguments.                  */
;*---------------------------------------------------------------------*/
(define (js-create-service %this::JsGlobalObject proc path loc register import worker::WorkerHopThread)
   
   (define (source::bstring proc)
      (if (js-function? proc)
	  (or (js-function-path proc) (pwd))
	  (pwd)))
   
   (define (fix-args len)
      (map (lambda (i)
	      (string->symbol (format "a~a" i)))
	 (iota len)))

   (define (service-debug id::symbol proc)
      (if (>fx (bigloo-debug) 0)
	  (lambda ()
	     (js-service/debug id loc proc))
	  proc))

   (define (js-service-parse-request svc req)
      (with-access::http-request req (path abspath)
	 (let ((args (service-parse-request svc req)))
	    (if (and (null? args)
		     (=fx (string-length path) (string-length abspath)))
		(js-literal->jsobject '#() '#() %this)
		(js-obj->jsobject args %this)))))

   (when (and (eq? proc (js-undefined)) (not (eq? path (js-undefined))))
      (set! path (js-tostring proc %this)))
   
   (let* ((path (or path (gen-service-url :public #t)))
	  (hoppath (make-hop-url-name path))
	  (src (source proc)))
      (multiple-value-bind (id wid)
	 (service-path->ids path)
	 (letrec* ((svcp (lambda (this . vals)
			    (with-access::JsService svcjs (svc)
			       (with-access::hop-service svc (path)
				  (js-make-hopframe %this this path vals)))))
		   (svcjs (js-make-service %this svcp (symbol->string! id)
			     register import
			     (js-function-arity svcp) worker
			     (instantiate::hop-service
				(ctx %this)
				(proc (if (js-procedure? proc)
					  (lambda (this . args)
					     (js-apply %this proc this args))
					  (lambda (this . args)
					     (js-undefined))))
				(handler (lambda (svc req)
					    (js-worker-exec worker
					       (symbol->string! id) #f
					       (service-debug id
						  (lambda ()
						     (service-invoke svc req
							(js-service-parse-request svc req)))))))
				(javascript "HopService( ~s, ~s )")
				(path hoppath)
				(id id)
				(wid wid)
				(args (fix-args (js-get proc (& "length") %this)))
				(resource (dirname src))
				(source src)))))
	    svcjs))))

;*---------------------------------------------------------------------*/
;*    service-path->ids ...                                            */
;*---------------------------------------------------------------------*/
(define (service-path->ids path)
   (let* ((id (string->symbol path))
	  (wid (let ((j (string-index path #\/)))
		  (if j (string->symbol (substring path 0 j)) id))))
      (values id wid)))

;*---------------------------------------------------------------------*/
;*    service-pack-cgi-arguments ...                                   */
;*---------------------------------------------------------------------*/
(define-method (service-pack-cgi-arguments ctx::JsGlobalObject svc vals)
   (with-access::JsGlobalObject ctx (js-object worker)
      (with-access::hop-service svc (args id)
	 (cond
	    ((null? vals)
	     '())
	    ((and (pair? args) (eq? (car args) #!key))
	     ;; old dsssl protocol (<=rc7)
	     args)
	    (else
	     ;; hop-encoding:hop, new varargs protocol
	     (let ((obj (js-new0 ctx js-object))
		   (vecks '()))
		;; first step
		(for-each (lambda (arg)
			     (let ((k (js-string->name (car arg)))
				   (val (js-string->jsstring (cdr arg))))
				(cond
				   ((not (js-in? obj k ctx))
				    (js-put! obj k val #f ctx))
				   (else
				    (let ((old (js-get obj k ctx)))
				       (if (pair? old)
					   (set-cdr! (last-pair old)  val)
					   (begin
					      (set! vecks (cons k vecks))
					      (js-put! obj k (list val) #f ctx))))))))
		   vals)
		;; second step, patch the multi-files arguments
		(for-each (lambda (k)
			     (let ((old (js-get obj k ctx)))
				(js-put! obj k
				   (js-vector->jsarray
				      (list->vector (reverse! old))
				      ctx)
				   #f ctx)))
		   vecks)
		obj))))))
   
;*---------------------------------------------------------------------*/
;*    js-make-service ...                                              */
;*---------------------------------------------------------------------*/
(define (js-make-service %this proc name register import arity worker svc)
   
   (define (default-service)
      (instantiate::hop-service
	 (id (string->symbol name))
	 (wid (let ((i (string-index name #\?)))
		 (string->symbol
		    (if i (substring name 0 i) name))))
	 (args '())
	 (proc (lambda l l))
	 (javascript "")
	 (path (hop-service-base))
	 (resource "/")))
   
   (define (set-service-path! o p)
      (with-access::JsService o (svc)
	 (when svc
	    (when register (unregister-service! svc))
	    (with-access::hop-service svc (path id wid)
	       (set! path p)
	       (when (string=? (dirname p) (hop-service-base))
		  (let ((apath (substring path
				  (+fx 1 (string-length (hop-service-base))))))
		     (multiple-value-bind (i w)
			(service-path->ids apath)
			(set! id i)
			(set! wid w)))))
	    (when register (register-service! svc)))))
   
   ;; register only if there is an implementation
   (when svc
      (when (and register (>=fx (hop-default-port) 0))
	 (register-service! svc))
      (unless import
	 (with-access::WorkerHopThread worker (services)
	    (set! services (cons svc services)))))
   
   (define (get-path o)
      (with-access::JsService o (svc)
	 (when svc
	    (with-access::hop-service svc (path)
	       (js-string->jsstring path)))))
   
   (define (set-path o v)
      (set-service-path! o (js-tostring v %this))
      v)
   
   (define (get-name o)
      (with-access::JsService o (svc)
	 (when svc
	    (with-access::hop-service svc (id)
	       (js-string->jsstring
		  (symbol->string! id))))))
   
   (define (set-name o v)
      (set-service-path! o
	 (make-file-name (hop-service-base) (js-tostring v %this)))
      v)
   
   (with-access::JsGlobalObject %this (js-service-prototype)
      (instantiateJsService
	 (procedure proc)
	 (info (js-function-info :name name :len arity))
	 (arity (js-function-arity proc))
	 (worker worker)
	 (prototype (js-object-proto %this))
	 (__proto__ js-service-prototype)
	 (alloc js-not-a-constructor-alloc)
	 (svc (or svc (default-service)))
	 (elements (vector
			(instantiate::JsValueDescriptor
			   (name (& "length"))
			   (value 0))
			(instantiate::JsAccessorDescriptor
			   (name (& "path"))
			   (get (js-make-function %this get-path
				   (js-function-arity get-path)
				   (js-function-info :name "path" :len 1)))
			   (set (js-make-function %this set-path
				   (js-function-arity set-path)
				   (js-function-info :name "path" :len 2)))
			   (%get get-path)
			   (%set set-path))
			(instantiate::JsAccessorDescriptor
			   (name (& "name"))
			   (get (js-make-function %this get-name
				   (js-function-arity get-name)
				   (js-function-info :name "name" :len 1)))
			   (set (js-make-function %this set-name
				   (js-function-arity set-name)
				   (js-function-info :name "name" :len 2)))
			   (%get get-name)
			   (%set set-name)))))))

;*---------------------------------------------------------------------*/
;*    service? ::JsService ...                                         */
;*---------------------------------------------------------------------*/
(define-method (service? obj::JsService)
   #t)

;*---------------------------------------------------------------------*/
;*    service->hop-service ::JsService ...                             */
;*---------------------------------------------------------------------*/
(define-method (service->hop-service obj::JsService)
   (with-access::JsService obj (svc)
      svc))

;*---------------------------------------------------------------------*/
;*    js-tostring ::hop-service ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-tostring obj::hop-service %this)
   (with-access::hop-service obj (path)
      (format "[Service ~a]" path)))

;*---------------------------------------------------------------------*/
;*    *allow-urls* ...                                                 */
;*---------------------------------------------------------------------*/
(define *allow-urls* '())
(define *url-redirect* #f)

;*---------------------------------------------------------------------*/
;*    add-service-allow-url! ...                                       */
;*---------------------------------------------------------------------*/
(define (add-service-allow-url! url)
   (set! *allow-urls* (cons (js-jsstring->string url) *allow-urls*)))

;*---------------------------------------------------------------------*/
;*    service-add-url! ...                                             */
;*---------------------------------------------------------------------*/
(define (service-add-url! svc url %this)
   (let ((u (js-jsstring->string url)))
      (cond
	 ((not (isa? svc JsService))
	  (js-raise-type-error %this
	     "Not a service ~s" (js-tostring svc %this)))
	 ((not (member u *allow-urls*))
	  (js-raise-type-error %this
	     "URL not allowed for redirection ~s (see Service.allowURL)" u))
	 ((string-prefix? (hop-service-base) u)
	  (js-raise-type-error %this
	     (format "Illegal URL prefix ~~s (must not be a prefix of ~s)"
		(hop-service-base))
	     u))
	 (else
	  (with-access::JsService svc (svc)
	     (with-access::hop-service svc (path)
		(let ((old (hashtable-get *url-redirect* u)))
		   (when old
		      (js-raise-type-error %this
			 (format "URL ~s already bound to ~~s" u) old))
		   (hashtable-put! *url-redirect* u path))))))))

;*---------------------------------------------------------------------*/
;*    service-remove-url! ...                                          */
;*---------------------------------------------------------------------*/
(define (service-remove-url! svc url %this)
   (let ((u (js-jsstring->string url)))
      (cond
	 ((not (isa? svc JsService))
	  (js-raise-type-error %this
	     "Not a service ~s" (js-tostring svc %this)))
	 ((not (member u *allow-urls*))
	  (js-raise-type-error %this
	     "URL not allowed for redirection ~s (see Service.allowURL)" u))
	 (else
	  (with-access::JsService svc (svc)
	     (with-access::hop-service svc (path)
		(let ((old (hashtable-get *url-redirect* u)))
		   (cond
		      ((not old)
		       (js-raise-type-error %this "unbound URL ~s" u))
		      ((not (string=? path old))
		       (js-raise-type-error %this "URL ~s bound to another service" u))
		      (else
		       (hashtable-remove! *url-redirect* u))))))))))

;*---------------------------------------------------------------------*/
;*    service-get-urls ...                                             */
;*---------------------------------------------------------------------*/
(define (service-get-urls svc %this)
   (if (isa? svc JsService)
       (with-access::JsService svc (svc)
	  (with-access::hop-service svc (path)
	     (let ((res '()))
		(hashtable-for-each *url-redirect*
		   (lambda (k v)
		      (when (string=? v path)
			 (set! res (cons (js-string->jsstring k) res)))))
		(js-vector->jsarray (list->vector res) %this))))
       (js-raise-type-error %this
	  "Not a service ~s" (js-tostring svc %this))))

;*---------------------------------------------------------------------*/
;*    service-url-filter ...                                           */
;*---------------------------------------------------------------------*/
(define (service-url-filter req::http-request)
   (when (isa? req http-server-request)
      (with-access::http-server-request req (abspath path)
	 (let ((alias (hashtable-get *url-redirect* abspath)))
	    (when alias (set! abspath alias))))
      req))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
