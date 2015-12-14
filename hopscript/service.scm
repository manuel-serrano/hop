;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/service.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct 17 08:19:20 2013                          */
;*    Last change :  Mon Dec 14 07:29:36 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript service implementation                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_service

   (library hop)

   (include "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_rts
	   __hopscript_object
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_property
	   __hopscript_function
	   __hopscript_worker
	   __hopscript_json
	   __hopscript_lib
	   __hopscript_array)

   (export (js-init-service! ::JsGlobalObject)
	   (js-make-hopframe ::JsGlobalObject ::obj ::obj ::obj)
	   (js-create-service::JsService ::JsGlobalObject ::obj ::obj ::obj ::bool ::bool ::WorkerHopThread)
	   (js-make-service::JsService ::JsGlobalObject ::procedure ::obj ::bool ::bool ::int ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-begin!)

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
	     (svcjs (js-make-service ctx svcp
		       (js-string->jsstring (basename path))
		       #f #f -1 (js-current-worker)
		       (instantiate::hop-service
			  (id (string->symbol path))
			  (wid (let ((i (string-index path #\?)))
				  (string->symbol
				     (if i (substring path 0 i) path))))
			  (args '())
			  (proc (lambda l l))
			  (javascript "")
			  (path path)
			  (resource (vector-ref o 1))))))
	 svcjs)))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsHopFrame ...                               */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsHopFrame
   (lambda (o)
      (with-access::JsHopFrame o (srv path args options header path)
	 (vector srv path args options header)))
   (lambda (o ctx)
      (if (and (vector? o) (=fx (vector-length o) 4))
	  (with-access::JsGlobalObject ctx (js-hopframe-prototype)
	     (instantiate::JsHopFrame
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
   (lambda (o)
      (with-access::JsServer o (obj)
	 (with-access::server obj (host port ssl authorization version)
	    (vector host port ssl
	       (if (string? authorization) authorization #f)
	       version
	       (js-jsobject->plist o (js-initial-global-object))))))
   (lambda (o ctx)
      (if (and (vector? o) (=fx (vector-length o) 6))
	  (let ((srv (instantiate::JsServer
			(obj (instantiate::server
				(host (vector-ref o 0))
				(port (vector-ref o 1))
				(ssl (vector-ref o 2))
				(authorization (vector-ref o 3))
				(version (vector-ref o 4)))))))
	     (let loop ((rest (vector-ref o 5)))
		(if (null? rest)
		    srv
		    (begin
		       (js-put! srv (keyword->symbol (car rest))
			  (js-obj->jsobject (cadr rest) ctx)
			  #f ctx)
		       (loop (cddr rest))))))
	  (error "JsServer" "wrong server" o))))

;*---------------------------------------------------------------------*/
;*    js-tostring ::JsHopFrame ...                                     */
;*---------------------------------------------------------------------*/
(define-method (js-tostring o::JsHopFrame %this)
   (with-access::JsHopFrame o (args path)
      path))

;*---------------------------------------------------------------------*/
;*    xml-primitive-value ::JsHopFrame ...                             */
;*---------------------------------------------------------------------*/
(define-method (xml-primitive-value o::JsHopFrame)
   (with-access::JsHopFrame o (path)
      path))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsService ...                                        */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsService worker::WorkerHopThread %_this)
   (with-access::WorkerHopThread worker (%this)
      (let* ((proc (lambda (this . args)
		      (with-access::JsService obj (svc)
			 (with-access::hop-service svc (path)
			    (js-make-hopframe %this this path args)))))
	     (nobj (duplicate::JsService obj
		      (procedure proc)
		      (properties '()))))
	 (js-for-in obj
	    (lambda (k)
	       (js-put! nobj k
		  (js-donate (js-get obj k %_this) worker %this)
		  #f %this))
	    %this)
	 nobj)))
   
;*---------------------------------------------------------------------*/
;*    xml-unpack ::JsService ...                                       */
;*---------------------------------------------------------------------*/
(define-method (xml-unpack o::JsService)
   #f)

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
(define-method (hop->javascript o::JsService op compile isexpr)
   (with-access::JsService o (svc)
      (compile svc op)))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsHopFrame ...                                 */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsHopFrame op compile isexpr)
   (display "hop_url_encoded_to_obj('" op)
   (display (url-path-encode (obj->string o 'hop-client)) op)
   (display "')" op))
;*       (display "new HopFrame( " op)                                 */
;*       (hop->javascript srv op compile isexpr)                       */
;*       (display ",\"" op)                                            */
;*       (display path op)                                             */
;*       (display "\"," op)                                            */
;*       (display "[ " op)                                             */
;*       (when (pair? args)                                            */
;* 	 (let loop ((args args))                                       */
;* 	    (hop->javascript (car args) op compile isexpr)             */
;* 	    (when (pair? (cdr args))                                   */
;* 	       (display "," op)                                        */
;* 	       (loop (cdr args)))))                                    */
;*       (display "], " op)                                            */
;*       (display "false, " op)                                        */
;*       (display "false " op)                                         */
;*       (display ")" op)))                                            */

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsServer ...                                   */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsServer op compile isexpr)
   (display "hop_url_encoded_to_obj('" op)
   (display (url-path-encode (obj->string o 'hop-client)) op)
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
      ((string=? enc "file") (js-string->jsstring val))
      ((string=? enc "integer") (string->integer val))
      ((string=? enc "keyword") (string->keyword val))
      (else (string->obj val #f %this))))

;*---------------------------------------------------------------------*/
;*    js-init-service! ...                                             */
;*---------------------------------------------------------------------*/
(define (js-init-service! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (__proto__ js-function
					 js-service-prototype
					 js-hopframe-prototype)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 ;; service prototype
	 (set! js-service-prototype
	    (instantiate::JsService
	       (__proto__ js-function-prototype)
	       (worker (class-nil WorkerHopThread))
	       (name "service")
	       (alloc (lambda (_) #unspecified))
	       (construct (lambda (constructor args)
			     (js-raise-type-error %this "not a constructor ~s"
				js-function-prototype)))
	       (len -1)
	       (procedure list)
	       (svc #f)
	       (extensible #t)))
	 
	 (js-bind! %this js-service-prototype 'resource
	    :value (js-make-function %this
		      (lambda (this file)
			 (js-string->jsstring
			    (service-resource this (js-jsstring->string file))))
		      1 'resource)
	    :writable #t
	    :configurable #t
	    :enumerable #f)
	 (js-bind! %this js-service-prototype 'unregister
	    :value (js-make-function %this
		      (lambda (this)
			 (when (isa? this JsService)
			    (with-access::JsService this (svc)
			       (unregister-service! svc)))
			 (js-undefined))
		      0 'unregister)
	    :writable #t
	    :configurable #t
	    :enumerable #f)
	 
	 (js-bind! %this js-service-prototype 'timeout
	    :get (js-make-function %this
		    (lambda (this)
		       (with-access::JsService this (svc)
			  (with-access::hop-service svc (timeout)
			     timeout)))
		    0 'timeout))

	 (js-bind! %this js-service-prototype 'ttl
	    :get (js-make-function %this
		    (lambda (this)
		       (with-access::JsService this (svc)
			  (with-access::hop-service svc (ttl)
			     ttl)))
		    0 'ttl)
	    :set (js-make-function %this
		    (lambda (this v)
		       (with-access::JsService this (svc)
			  (with-access::hop-service svc (ttl)
			     (set! ttl (js-tointeger v %this)))))
		    0 'ttl))
	 
	 ;; HopFrame prototype and constructor
	 (set! js-hopframe-prototype
	    (instantiate::JsObject
	       (__proto__ __proto__)
	       (extensible #t)))
	 
	 (js-bind! %this js-hopframe-prototype 'post
	    :value (js-make-function %this
		      (lambda (this::JsHopFrame success fail-or-opt)
			 (with-access::JsHopFrame this (url args)
			    (post this success fail-or-opt %this #t)))
		      3 'post))
	 (js-bind! %this js-hopframe-prototype 'postSync
	    :value (js-make-function %this
		      (lambda (this::JsHopFrame opt)
			 (with-access::JsHopFrame this (url args)
			    (post this #f opt %this #f)))
		      2 'postSync))
	 (js-bind! %this js-hopframe-prototype 'toString
	    :value (js-make-function %this
		      (lambda (this::JsHopFrame)
			 (js-string->jsstring (hopframe->string this %this)))
		      0 'toString))
	 (js-bind! %this js-hopframe-prototype 'inspect
	    :value (js-make-function %this
		      (lambda (this::JsHopFrame)
			 (js-string->jsstring (hopframe->string this %this)))
		      0 'inspect))
	 (js-bind! %this js-hopframe-prototype 'getHeader
	    :value (js-make-function %this
		      (lambda (this::JsHopFrame hd)
			 (with-access::JsHopFrame this (header)
			    header))
		      0 'getHeader))
	 (js-bind! %this js-hopframe-prototype 'setHeader
	    :value (js-make-function %this
		      (lambda (this::JsHopFrame hd)
			 (with-access::JsHopFrame this (header)
			    (set! header hd)
			    this))
		      1 'setHeader))
	 (js-bind! %this js-hopframe-prototype 'getOptions
	    :value (js-make-function %this
		      (lambda (this::JsHopFrame opts)
			 opts)
		      0 'getOptions))
	 (js-bind! %this js-hopframe-prototype 'setOptions
	    :value (js-make-function %this
		      (lambda (this::JsHopFrame opts)
			 (with-access::JsHopFrame this (options)
			    (set! options opts)
			    this))
		      1 'setOptions))

	 (letrec ((js-service (js-make-function %this
				 (lambda (this proc path)
				    (js-new %this js-service proc path))
				 3 'Service
				 :__proto__ js-function-prototype
				 :prototype js-service-prototype
				 :construct (lambda (this proc path)
					       (js-create-service %this proc
						  (unless (eq? path (js-undefined))
						     (js-tostring path %this))
						  #f #t #f (js-current-worker)))))
		  (js-hopframe (js-make-function %this
				  (lambda (this url args)
				     (js-new %this js-hopframe url args))
				  1 'HopFrame
				  :__proto__ js-function-prototype
				  :prototype js-hopframe-prototype
				  :construct (lambda (this path args)
						(js-make-hopframe %this 
						   this path args)))))
	    (js-bind! %this %this 'Service
	       :configurable #f :enumerable #f :value js-service)
	    (js-bind! %this %this 'HopFrame
	       :configurable #f :enumerable #f :value js-hopframe)
	    
	    (js-bind! %this js-service 'exists
	       :configurable #f :enumerable #f
	       :value (js-make-function %this
			 (lambda (this svc)
			    (if (service-exists? (js-tostring svc %this)) #t #f))
			 1 'exists)))

	 (js-undefined))))

;*---------------------------------------------------------------------*/
;*    js-make-hopframe ...                                             */
;*---------------------------------------------------------------------*/
(define (js-make-hopframe %this::JsGlobalObject srv path args)
   (with-access::JsGlobalObject %this (js-hopframe-prototype js-object js-server)
      (instantiate::JsHopFrame
	 (__proto__ js-hopframe-prototype)
	 (%this %this)
	 (srv (when (isa? srv JsServer) srv))
	 (path path)
	 (args args))))

;*    (define (url-frame)                                              */
;*       (with-access::JsGlobalObject %this (js-hopframe-prototype)    */
;* 	 (instantiate::JsHopFrame                                      */
;* 	    (__proto__ js-hopframe-prototype)                          */
;* 	    (%this %this)                                              */
;* 	    (srv srv)                                                  */
;* 	    (url url)                                                  */
;* 	    (args args))))                                             */
;*                                                                     */
;*    (define (multipart-frame)                                        */
;*       (with-access::JsGlobalObject %this (js-hopframe-prototype)    */
;* 	 (instantiate::JsHopFrame                                      */
;* 	    (%this %this)                                              */
;* 	    (srv srv)                                                  */
;* 	    (args (unless (eq? args (js-undefined))                    */
;* 		     (map (lambda (val)                                */
;* 			     (cond                                     */
;* 				((isa? val JsStringLiteral)            */
;* 				 `("string" ,(js-jsstring->string val) */
;* 				     "hop-encoding: string"))          */
;* 				((integer? val)                        */
;* 				 `("integer" ,val                      */
;* 				     "hop-encoding: integer"))         */
;* 				((keyword? val)                        */
;* 				 `("keyword" ,(keyword->string val)    */
;* 				     "hop-encoding: keyword"))         */
;* 				((string? val)                         */
;* 				 (error "js-make-hopframe"             */
;* 				    "Illegal string" val))             */
;* 				(else                                  */
;* 				 `("hop" ,(obj->string val 'hop-to-hop) */
;* 				     "hop-encoding: hop"))))           */
;* 			args)))                                        */
;* 	    (url url)                                                  */
;* 	    (__proto__ js-hopframe-prototype))))                       */
;*                                                                     */
;*    (cond                                                            */
;*       ((null? args)                                                 */
;*        (url-frame))                                                 */
;*       ((and (null? (cdr args))                                      */
;* 	    (string? (car args))                                       */
;* 	    (<fx (string-length (car args)) 80))                       */
;*        (url-frame))                                                 */
;*       ((every integer? args)                                        */
;*        (url-frame))                                                 */
;*       ((keyword? (car args))                                        */
;*        ;; scheme call                                               */
;*        (js-string->jsstring (hop-apply-url url args)))              */
;*       (else                                                         */
;*        (multipart-frame))))                                         */
;*                                                                     */

;*---------------------------------------------------------------------*/
;*    hopframe->string ...                                             */
;*---------------------------------------------------------------------*/
(define (hopframe->string::bstring frame::JsHopFrame %this)
   (with-access::JsHopFrame frame (srv path args)
      (let ((sans-srv (if (pair? args)
			  (hop-apply-url path args)
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
	 ((isa? val JsStringLiteral)
	  `("string" ,(js-jsstring->string val) "hop-encoding: string"))
	 ((integer? val)
	  `("integer" ,val "hop-encoding: integer"))
	 ((keyword? val)
	  `("keyword" ,(keyword->string val) "hop-encoding: keyword"))
	 ((string? val)
	  (error "js-make-hopframe" "Illegal string" val))
	 (else
	  `("hop" ,(obj->string val 'hop-to-hop) "hop-encoding: hop"))))

   (define (scheme->js val)
      (if (string? val)
	  (js-string->jsstring val)
	  val))

   (define (js-get-string opt key)
      (let ((v (js-get opt key %this)))
	 (unless (eq? v (js-undefined))
	    (js-tostring v %this))))
   
   (define (post-request callback fail scheme host port user password auth)
      (with-access::JsHopFrame this (path args header options)
	 (with-hop-remote path callback fail
	    :scheme scheme
	    :host host :port port 
	    :user (js-get-string options 'user)
	    :password (js-get-string options 'password)
	    :authorization auth
	    :header (when header (js-jsobject->alist header %this))
	    :ctx %this
	    :json-parser (lambda (ip ctx) (js-json-parser ip #f #f #f %this))
	    :args (map multipart-form-arg args))))

   (define (post-server-promise this %this host port auth scheme)
      (with-access::JsGlobalObject %this (js-promise)
	 (js-new %this js-promise
	    (js-make-function %this
	       (lambda (this resolve reject)
		  (thread-start!
		     (instantiate::hopthread
			(body (lambda ()
				 (with-handler
				    (lambda (e)
				       (js-call1 %this reject %this e))
				    (post-request
				       (lambda (x)
					  (js-call1 %this resolve %this
					     (scheme->js x)))
				       (lambda (x)
					  (js-call1 %this reject %this x))
				       scheme host port
				       user password auth)))))))
	       2 "executor"))))
   
   (define (post-server-async this success failure %this host port auth scheme)
      (with-access::JsHopFrame this (path)
	 (let ((callback (when (isa? success JsFunction)
			    (lambda (x)
			       (js-worker-push-thunk! (js-current-worker) path
				  (lambda ()
				     (js-call1 %this success %this
					(scheme->js x)))))))
	       (fail (when (isa? failure JsFunction)
			(lambda (obj)
			   (js-worker-push-thunk! (js-current-worker) path
			      (lambda ()
				 (js-call1 %this failure %this obj)))))))
	    (thread-start!
	       (instantiate::hopthread
		  (body (lambda ()
			   (with-handler
			      (or fail exception-notify)
			      (post-request
				 callback fail
				 scheme host port
				 user password auth))))))
	    (js-undefined))))
   
   (define (post-server-sync this %this host port auth scheme)
      (with-access::JsHopFrame this (path args header options)
	 (with-hop-remote path scheme->js #f
	    :scheme scheme
	    :host host :port port 
	    :user (js-get-string options 'user)
	    :password (js-get-string options 'password)
	    :authorization auth
	    :header (when header (js-jsobject->alist header %this))
	    :ctx %this
	    :json-parser (lambda (ip ctx) (js-json-parser ip #f #f #f %this))
	    :args (map multipart-form-arg args))))

   (define (post-server this success failure %this async host port auth scheme)
      (cond
	 ((not async)
	  (post-server-sync this %this host port auth scheme))
	 ((or (isa? success JsFunction) (isa? failure JsFunction))
	  (post-server-async this success failure %this host port auth scheme))
	 (else
	  (post-server-promise this %this host port auth scheme))))
   
   (with-access::JsHopFrame this (srv)
      (cond
	 ((isa? srv JsServer)
	  (with-access::JsServer srv (obj)
	     (with-access::server obj (host port authorization ssl)
		(post-server this success fail-or-opt %this async
		   host port authorization (if ssl 'https 'http)))))
	 ((or (isa? fail-or-opt JsFunction) (not (isa? fail-or-opt JsObject)))
	  (post-server this success fail-or-opt %this async
	     "localhost" (hop-port) #f 'http))
	 (else
	  (with-access::JsHopFrame this (path args)
	     (post-options-deprecated path (map multipart-form-arg args)
		success fail-or-opt %this async))))))

;*---------------------------------------------------------------------*/
;*    post-options-deprecated ...                                      */
;*---------------------------------------------------------------------*/
(define (post-options-deprecated svc::bstring args success opt %this async)
   
   (let ((host "localhost")
	 (port (hop-port))
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
	 ((isa? opt JsFunction)
	  (set! fail
	     (if asynchronous
		 (lambda (obj)
		    (js-call1 %this opt %this obj))
		 (lambda (obj)
		    (js-worker-push-thunk! worker svc
		       (lambda ()
			  (js-call1 %this opt %this obj)))))))
	 ((not (eq? opt (js-undefined)))
	  (let* ((v (js-get opt 'server %this))
		 (o (if (eq? v (js-undefined)) opt v))
		 (a (js-get o 'authorization %this))
		 (h (js-get o 'host %this))
		 (p (js-get o 'port %this))
		 (u (js-get opt 'user %this))
		 (w (js-get opt 'password %this))
		 (f (js-get opt 'fail %this))
		 (y (js-get opt 'asynchronous %this))
		 (s (js-get opt 'scheme %this))
		 (c (js-get opt 'ssl %this))
		 (r (js-get opt 'header %this)))
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
		(when (js-in? %this 'asynchronous opt)
		   (set! asynchronous #f)))
	     (when (js-totest c)
		(set! scheme 'https))
	     (unless (eq? s (js-undefined))
		(set! scheme (string->symbol (js-tostring s %this))))
	     (when (isa? f JsFunction)
		(set! failjs f)
		(set! fail
		   (lambda (obj)
		      (if asynchronous
			  (js-worker-push-thunk! worker svc
			     (lambda ()
				(js-call1 %this f %this obj)))
			  (js-call1 %this f %this obj)))))
	     (when (isa? r JsObject)
		(set! header (js-jsobject->alist r %this))))))

      (define (scheme->js val)
	 ;; a string might be received on internal server error
	 (if (string? val)
	     (js-string->jsstring val)
	     val))
      
      (define (post-request callback)
	 (with-hop-remote svc callback fail
	    :scheme scheme
	    :host host :port port 
	    :user user :password password :authorization authorization
	    :header header
	    :ctx %this
	    :json-parser (lambda (ip ctx) (js-json-parser ip #f #f #f %this))
	    :args args))

      (define (spawn-thread)
	 (thread-start!
	    (instantiate::hopthread
	       (body (lambda ()
			(with-handler
			   (lambda (e)
			      (if failjs
				  (js-worker-push-thunk! worker svc
				     (lambda ()
					(js-call1 %this failjs %this e)))
				  (exception-notify e)))
			   (post-request
			      (lambda (x)
				 (js-worker-push-thunk! worker svc
				    (lambda ()
				       (js-call1 %this success %this
					  (scheme->js x)))))))))))
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
			   (body (lambda ()
				    (with-handler
				       (lambda (e)
					  (js-call1 %this reject %this e))
				       (post-request
					  (lambda (x)
					     (js-call1 %this resolve %this
						(scheme->js x))))))))))
		  2 "executor"))))

      (if asynchronous
	  (if (isa? success JsFunction)
	      (spawn-thread)
	      (spawn-promise))
	  (post-request
	     (if (isa? success JsFunction)
		 (lambda (x) (js-call1 %this success %this (scheme->js x)))
		 scheme->js)))))      

;*---------------------------------------------------------------------*/
;*    js-create-service ...                                            */
;*    -------------------------------------------------------------    */
;*    This function is called when the HopScript constructor           */
;*    Service is directly invoked from user programs. The role         */
;*    of this function is to build an internal hop-service that        */
;*    simply calls the HopScript function passed as argument.          */
;*    The complexity of this function comes when the optional          */
;*    "args" arguments is an object, in which case, the function       */
;*    builds a service with optional named arguments.                  */
;*---------------------------------------------------------------------*/
(define (js-create-service %this::JsGlobalObject proc path loc register import worker::WorkerHopThread)
   
   (define (source::bstring proc)
      (if (isa? proc JsFunction)
	  (with-access::JsFunction proc (src)
	     (match-case src
		(((at ?path ?-) . ?-) path)
		(else (pwd))))
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

   (when (and (eq? proc (js-undefined)) (not (eq? path (js-undefined))))
      (set! path (js-tostring proc %this)))
   
   (let* ((path (or path (gen-service-url :public #t)))
	  (hoppath (make-hop-url-name path))
	  (src (source proc)))
      (multiple-value-bind (id wid)
	 (service-path->ids path)
	 (letrec* ((svcp (lambda (this . args)
			    (with-access::JsService svcjs (svc)
			       (with-access::hop-service svc (path)
				  (js-make-hopframe %this this path args)))))
		   (svcjs (js-make-service %this svcp id register import
			     -1 worker
			     (instantiate::hop-service
				(ctx %this)
				(proc (if (isa? proc JsFunction)
					  (lambda (this . args)
					     (map! (lambda (a)
						      (js-obj->jsobject a %this))
						args)
					     (js-worker-exec worker
						(symbol->string! id)
						(service-debug id
						   (lambda ()
						      (js-apply %this proc this args)))))
					  (lambda (this . args)
					     (js-undefined))))
				(javascript "HopService( ~s, ~s )")
				(path hoppath)
				(id id)
				(wid wid)
				(args (fix-args (js-get proc 'length %this)))
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
   (with-access::JsGlobalObject ctx (js-object)
      (with-access::hop-service svc (args)
	 (cond
	    ((null? vals)
	     '())
	    ((and (pair? args) (eq? (car args) #!key))
	     ;; old dsssl protocol (<=rc7)
	     args)
	    (else
	     ;; new varargs protocol
	     (let ((obj (js-new0 ctx js-object)))
		(for-each (lambda (arg)
			     (let ((k (car arg))
				   (val (js-string->jsstring (cdr arg))))
				(cond
				   ((not (js-in? ctx k obj))
				    (js-put! obj k val #f ctx))
				   (else
				    (error "service-pack-cgi-arguments"
				       "not implemented"
				       arg)))))
		   vals)
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
      (when register (register-service! svc))
      (unless import
	 (with-access::WorkerHopThread worker (services)
	    (set! services (cons svc services)))))
   
   (with-access::JsGlobalObject %this (js-service-prototype)
      (instantiate::JsService
	 (procedure proc)
	 (len arity)
	 (arity arity)
	 (worker worker)
	 (__proto__ js-service-prototype)
	 (name (if (symbol? name) (symbol->string! name) ""))
	 (alloc (lambda (_)
		   (js-raise-type-error %this
		      "service not a constructor" #f)))
	 (construct (lambda (_ arg)
		       (js-raise-type-error %this
			  "service not a constructor" arg)))
	 (properties (list
			(instantiate::JsValueDescriptor
			   (name 'length)
			   (value 0))
			(instantiate::JsAccessorDescriptor
			   (name 'path)
			   (get (js-make-function %this
				   (lambda (o)
				      (with-access::JsService o (svc)
					 (when svc
					    (with-access::hop-service svc (path)
					       (js-string->jsstring path)))))
				   1 'path))
			   (set (js-make-function %this
				   (lambda (o v)
				      (set-service-path! o
					 (js-tostring v %this))
				      v)
				   2 'path)))
			(instantiate::JsAccessorDescriptor
			   (name 'name)
			   (get (js-make-function %this
				   (lambda (o)
				      (with-access::JsService o (svc)
					 (when svc
					    (with-access::hop-service svc (id)
					       (js-string->jsstring
						  (symbol->string! id))))))
				   1 'name))
			   (set (js-make-function %this
				   (lambda (o v)
				      (set-service-path! o
					 (make-file-name (hop-service-base)
					    (js-tostring v %this)))
				      v)
				   2 'name)))))
	 (svc (or svc (default-service))))))

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
;*    JsStringLiteral end                                              */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-end!)

