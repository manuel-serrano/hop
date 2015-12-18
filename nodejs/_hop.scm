;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_hop.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr 18 06:41:05 2014                          */
;*    Last change :  Fri Dec 18 07:56:54 2015 (serrano)                */
;*    Copyright   :  2014-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop binding                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__hop

   (library hopscript hop)

   (import  __nodejs_uv
	    __nodejs__buffer)

   (static (class JsUrlFrame::JsObject
	      (%this read-only)
	      (args read-only (default #f))
	      (url read-only)))

   (export (nodejs-modules-directory::bstring)
	   (nodejs-modules-directory-set! ::bstring)
	   (hopjs-process-hop ::WorkerHopThread ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    nodejs-modules-directory ...                                     */
;*---------------------------------------------------------------------*/
(define-parameter nodejs-modules-directory
   (make-file-path (hop-lib-directory) "hop" (hop-version) "node_modules"))

;*---------------------------------------------------------------------*/
;*    define-js ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (define-js name arity proc . opt)
   `(cons ',name
       (js-make-function %this ,proc ,arity ,(symbol->string name) ,@opt)))
		 
;*---------------------------------------------------------------------*/
;*    bind-param! ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (bind-param! %this exports type id accessor . mutator)
   `(js-bind! ,%this ,exports ',id
       :get (js-make-function %this
	       (lambda (this)
		  ,(case type
		      ((string)
		       `(js-string->jsstring (,accessor)))
		      ((symbol)
		       `(js-string->jsstring (symbol->string (,accessor))))
		      ((string-array)
		       `(let ((v (map js-string->jsstring (,accessor))))
			   (js-vector->jsarray (list->vector v) %this)))
		      ((bool obj integer)
		       `(,accessor))
		      (else
		       (error "bind-param!" "unsupported type" type))))
	       0 ',id)
       :set ,(when (pair? mutator)
	       `(js-make-function %this
		   (lambda (o v)
		      ,(case type
			  ((string)
			   `(,(car mutator)
			     (js-tostring v %this)))
			  ((symbol)
			   `(,(car mutator)
			     (string->symbol (js-tostring v %this))))
			  ((integer)
			   `(,(car mutator)
			     (js-tointeger v %this)))
			  ((bool)
			   `(,(car mutator)
			     (js-totest v)))
			  ((string-array)
			   `(,(car mutator)
			     (map! (lambda (s) (js-tostring s %this))
				(jsarray->list v %this))))
			  (else
			   `(,(car mutator) v))))
		   1 ',id))
       :writable ,(pair? mutator)
       :enumerable #t))
   
;*---------------------------------------------------------------------*/
;*    hopjs-process-hop ...                                            */
;*---------------------------------------------------------------------*/
(define (hopjs-process-hop %worker %this)
   (with-access::JsGlobalObject %this (js-object js-function-prototype 
					 __proto__)
      
      (define js-urlframe-prototype
	 (instantiate::JsObject
	    (__proto__ __proto__)
	    (extensible #t)))
      
      (define js-webservice
	 (js-make-function %this
	    (lambda (this url args)
	       (js-new %this js-webservice url args))
	    2 'webservice
	    :__proto__ js-function-prototype
	    :prototype js-urlframe-prototype
	    :construct (lambda (this url args)
			  (js-make-urlframe %this
			     js-urlframe-prototype
			     url args))))

      (define js-server-prototype
	 (instantiate::JsObject
	    (__proto__ __proto__)
	    (extensible #t)))
      
      (define js-server
	 (js-make-function %this
	    (lambda (this host port auth ssl)
	       (js-new %this host port auth))
	    4 'Server
	    :__proto__ js-function-prototype
	    :prototype js-server-prototype
	    :construct (lambda (this host port auth ssl)
			  (instantiate::JsServer
			     (__proto__ js-server-prototype)
			     (data '())
			     (obj (instantiate::server
				     (ssl (js-toboolean ssl))
				     (host (if (eq? host (js-undefined))
					       (hop-server-hostname)
					       (js-tostring host %this)))
				     (port (if (eq? port (js-undefined))
					       (hop-port)
					       (js-tointeger port %this)))
				     (authorization (when (js-totest auth)
						       (js-tostring auth %this)))))))))
      
      (js-bind! %this js-urlframe-prototype 'post
	 :value (js-make-function %this
		   (lambda (this::JsUrlFrame success opt)
		      (post-url this success opt %this #f))
		   2 'post))
      (js-bind! %this js-urlframe-prototype 'postSync
	 :value (js-make-function %this
		   (lambda (this::JsUrlFrame opt)
		      (post-url this #f opt %this #t))
		   1 'postSync))
      (js-bind! %this js-urlframe-prototype 'toString
	 :value (js-make-function %this
		   (lambda (this::JsUrlFrame)
		      (js-string->jsstring (urlframe->string this %this)))
		   0 'toString))

      (js-bind! %this js-server-prototype 'addEventListener
	 :value (js-make-function %this
		   (lambda (this::JsServer event proc . capture)
		      (with-access::JsServer this (obj data)
			 (let ((f (lambda (evt)
				     (js-call1 %this proc this evt))))
			    (set! data (cons (cons proc f) data))
			    (when (isa? obj server)
			       (add-event-listener! obj
				     (js-tostring event %this) f)))))
		   3 'addEventListener)
	 :enumerable #f)
      (js-bind! %this js-server-prototype 'removeEventListener
	 :value (js-make-function %this
		   (lambda (this::JsServer event proc . capture)
		      (with-access::JsServer this (obj data)
			 (when (isa? obj server)
			    (let ((f (assq proc data)))
			       (when (pair? f)
				  (remove-event-listener! server
					(js-tostring event %this) (cdr f)))))))
		   3 'removeEventListener)
	 :enumerable #f)
      (js-bind! %this js-server-prototype 'port
	 :get (js-make-function %this
		 (lambda (this::JsServer)
		    (with-access::JsServer this (obj)
		       (when (isa? obj server)
			  (with-access::server obj (port)
			     port))))
		 0 'port)
	 :writable #f)
      (js-bind! %this js-server-prototype 'host
	 :get (js-make-function %this
		 (lambda (this::JsServer)
		    (with-access::JsServer this (obj)
		       (when (isa? obj server)
			  (with-access::server obj (host)
			     (js-string->jsstring host)))))
		 0 'host)
	 :writable #f)
      (js-bind! %this js-server-prototype 'authorization
	 :get (js-make-function %this
		 (lambda (this::JsServer)
		    (with-access::JsServer this (obj)
		       (when (isa? obj server)
			  (with-access::server obj (authorization)
			     (when (string? authorization)
				(js-string->jsstring authorization))))))
		 0 'authorization)
	 :writable #f)
      (js-bind! %this js-server-prototype 'ssl
	 :get (js-make-function %this
		 (lambda (this::JsServer)
		    (with-access::JsServer this (obj)
		       (when (isa? obj server)
			  (with-access::server obj (ssl)
			     ssl))))
		 0 'ssl)
	 :writable #f)
				
      (with-access::JsGlobalObject %this (js-object)
	 (js-alist->jsobject
	    (list
	       ;; info
	       `(version . ,(hop-version))
	       `(hostname . ,(js-string->jsstring (hostname)))
	       `(modulesDir . ,(js-string->jsstring (nodejs-modules-directory)))
	       (define-js port 0
		  (lambda (this)
		     (hop-port)))
	       
	       ;; requests
	       (define-js webService 1
		  (lambda (this base)
		     (let ((name (string->symbol (js-jsstring->string base))))
			(js-make-function %this
			   (lambda (this args)
			      (js-new %this js-webservice base args))
			   1 name))))
	       
	       ;; charset
	       (define-js charsetConvert 3
		  (lambda (this text from to)
		     (js-string->jsstring
			(hopjs-charset-convert this text from to %this))))
	       
	       ;; responses
	       (define-js HTTPResponseHop 2
		  (lambda (this obj req)
		     (hopjs-response-hop this obj req %this)))
	       
	       (define-js HTTPResponseXml 2
		  (lambda (this obj req)
		     (hopjs-response-xml this obj req %this)))
	       
	       (define-js HTTPResponseFile 2
		  (lambda (this file req)
		     (hopjs-response-file this file req %this)))
	       
	       (define-js HTTPResponseString 2
		  (lambda (this str req)
		     (hopjs-response-string this str req %this)))
	       
	       (define-js HTTPResponseAuthentication 2
		  (lambda (this msg req)
		     (hopjs-response-authentication this msg req %this)))
	       
	       (define-js HTTPResponseProxy 2
		  (lambda (this proc req)
		     (hopjs-response-proxy this proc req %this)))
	       
	       (define-js HTTPResponseAsync 2
		  (lambda (this proc req)
		     (hopjs-response-async this proc req %this %worker)))
	       
	       ;; events
	       (define-js signal 2
		  (lambda (this name v)
		     (hop-event-signal! (js-tostring name %this) v)))

	       (define-js broadcast 2
		  (lambda (this name v)
		     (hop-event-broadcast! (js-tostring name %this) v)))

	       `(Server . ,js-server)
	       
	       ;; XML
	       (define-js compileXML 3
		  (lambda (this xml ofile backend)
		     (let ((be (hop-get-xml-backend
				  (if (eq? backend (js-undefined))
				      'html5
				      (string->symbol
					 (js-tostring backend %this))))))
			(if (eq? ofile (js-undefined))
			    (js-string->jsstring
			       (call-with-output-string
				  (lambda (op)
				     (xml-write xml op be))))
			    (call-with-output-file (js-tostring ofile %this)
			       (lambda (op)
				  (xml-write xml op be)
				  (js-undefined)))))))

	       (define-js createElement 1
		  (lambda (this tag)
		     (instantiate::xml-element
			(tag (string->symbol (js-tostring tag %this)))
			(id (xml-make-id))
			(attributes '())
			(body '()))))
	       
	       ;; lib
	       (define-js encodeURIComponent 1
		  (lambda (this path)
		     (js-string->jsstring
			(url-path-encode (js-tostring path %this)))))
	       
	       (define-js encodeHTML 1
		  (lambda (this path)
		     (js-string->jsstring
			(html-string-encode (js-tostring path %this)))))
	       
	       (define-js md5sum 1
		  (lambda (this path)
		     (js-string->jsstring
			(md5sum-string (js-tostring path %this)))))
	       
	       (define-js sha1sum 1
		  (lambda (this path)
		     (js-string->jsstring
			(sha1sum-string (js-tostring path %this)))))
	       
	       ;; Lists
	       (define-js List -1
		  (lambda (this . l)
		     l))
	       
	       (define-js Cons 2
		  (lambda (this car cdr)
		     (cons car cdr))))
	    %this))))

;*---------------------------------------------------------------------*/
;*    js-make-urlframe ...                                             */
;*---------------------------------------------------------------------*/
(define (js-make-urlframe %this::JsGlobalObject js-urlframe-prototype url args)
   (instantiate::JsUrlFrame
      (%this %this)
      (url url)
      (args (if (eq? args (js-undefined))
		'()
		(map! (lambda (v)
			 (if (keyword? v)
			     v
			     (js-tostring v %this)))
		   (js-jsobject->plist args %this))))
      (__proto__ js-urlframe-prototype)))

;*---------------------------------------------------------------------*/
;*    urlframe->string ...                                             */
;*---------------------------------------------------------------------*/
(define (urlframe->string::bstring frame::JsUrlFrame %this)
   (with-access::JsUrlFrame frame (url args)
      (hop-apply-nice-url (js-jsstring->string url) args)))
   
;*---------------------------------------------------------------------*/
;*    post-url ...                                                     */
;*---------------------------------------------------------------------*/
(define (post-url frame::JsUrlFrame success opt %this force-sync)
   
   (let ((host "localhost")
	 (port #f)
	 (authorization #f)
	 (fail #f)
	 (asynchronous (not force-sync))
	 (header #f)
	 (timeout 0)
	 (method 'GET)
	 (scheme "http"))
      (cond
	 ((isa? opt JsFunction)
	  (set! fail
	     (lambda (xhr)
		(with-access::xml-http-request xhr (header)
		   (js-call1 %this opt %this
		      (js-alist->jsobject header %this))))))
	 ((not (eq? opt (js-undefined)))
	  (let ((h (js-get opt 'host %this))
		(p (js-get opt 'port %this))
		(a (js-get opt 'authorization %this))
		(f (js-get opt 'fail %this))
		(y (js-get opt 'asynchronous %this))
		(s (js-get opt 'scheme %this))
		(c (js-get opt 'ssl %this))
		(t (js-get opt 'timeout %this))
		(m (js-get opt 'method %this))
		(r (js-get opt 'header %this)))
	     (unless (eq? h (js-undefined))
		(set! host (js-tostring h %this)))
	     (unless (eq? p (js-undefined))
		(set! port (js-tointeger p %this)))
	     (unless (eq? a (js-undefined))
		(set! authorization (js-tostring a %this)))
	     (unless (js-totest y)
		(when (js-in? %this 'asynchronous opt)
		   (set! asynchronous #f)))
	     (when (js-totest c)
		(set! scheme "https"))
	     (unless (eq? s (js-undefined))
		(set! scheme (js-tostring s %this)))
	     (unless (eq? t (js-undefined))
		(set! timeout (js-tointeger t %this)))
	     (unless (eq? m (js-undefined))
		(set! method (string->symbol (js-tostring m %this))))
	     (when (isa? f JsFunction)
		(set! fail
		   (lambda (xhr)
		      (with-access::xml-http-request xhr (header)
			 (js-call1 %this f %this
			    (js-alist->jsobject header %this))))))
	     (when (isa? r JsObject)
		(set! header (js-jsobject->alist r %this))))))

      (define (url-base url)
	 (if (string-index url #\:)
	     url
	     (string-append scheme "://" host
		(if port (format ":~a" port) "")
		url)))
		      
      (define (post callback)
	 (with-access::JsUrlFrame frame (url args)
	    (with-url (hop-apply-nice-url
			 (url-base (js-jsstring->string url)) args)
	       callback
	       :fail fail
	       :method method
	       :timeout timeout
	       :authorization authorization
	       :json-parser (lambda (ip ctx) (js-json-parser ip #f #f #f %this))
	       :ctx %this
	       :header header)))
      
      (define (scheme->js val)
	 (js-obj->jsobject val %this))
      
      (if asynchronous
	  (begin
	     (thread-start!
		(instantiate::hopthread
		   (body (lambda ()
			    (post
			       (if (isa? success JsFunction)
				   (lambda (x)
				      (js-worker-exec (js-current-worker) "post"
					 (lambda ()
					    (js-call1 %this success %this
					       (scheme->js x)))))
				   scheme->js))))))
	     (js-undefined))
	  (post 
	     (if (isa? success JsFunction)
		 (lambda (x) (js-call1 %this success %this (scheme->js x)))
		 scheme->js)))))

;*---------------------------------------------------------------------*/
;*    hopjs-with-url ...                                               */
;*---------------------------------------------------------------------*/
(define (hopjs-with-url url success opt %this)
   
   (define (js-javascript->obj obj)
      (js-obj->jsobject obj %this))
   
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
	     (lambda (x) (js-call1 %this success %this (js-javascript->obj x)))
	     (lambda (x) x))
	 :fail fail 
	 :timeout timeout
	 :ctx %this
	 :json-parser (lambda (ip ctx) (js-json-parser ip #f #f #f %this))
	 :method (string->symbol method))))

;*---------------------------------------------------------------------*/
;*    get/default ...                                                  */
;*---------------------------------------------------------------------*/
(define (get/default obj key this def)
   (let ((v (js-get obj key this)))
      (cond
	 ((eq? v (js-undefined)) def)
	 ((js-jsstring? v) (js-jsstring->string v))
	 ((isa? v JsObject) (js-jsobject->alist v this))
	 (else v))))

;*---------------------------------------------------------------------*/
;*    get/list ...                                                     */
;*---------------------------------------------------------------------*/
(define (get/list::pair-nil obj key this def)
   (let ((v::obj (js-get obj key this)))
      (cond
	 ((eq? v (js-undefined)) def)
	 ((isa? v JsObject) (js-jsobject->alist v this))
	 (else def))))

;*---------------------------------------------------------------------*/
;*    hopjs-response-hop ...                                           */
;*---------------------------------------------------------------------*/
(define (hopjs-response-hop this obj req %this)
   (if (isa? req JsObject)
       (instantiate::http-response-hop
	  (backend (get/default req 'backend %this (hop-xml-backend)))
	  (start-line (get/default req 'startLine %this "HTTP/1.1 200 Ok"))
	  (content-type (get/default req 'contentType %this "application/x-json-hop"))
	  (charset (get/default req 'charset %this (hop-charset)))
	  (header (get/list req 'header %this '((Cache-Control: . "no-cache") (Pragma: . "no-cache"))))
	  (value obj))
       (instantiate::http-response-hop
	  (backend (hop-xml-backend))
	  (content-type "application/x-json-hop")
	  (value obj))))

;*---------------------------------------------------------------------*/
;*    hopjs-response-xml ...                                           */
;*---------------------------------------------------------------------*/
(define (hopjs-response-xml this obj req %this)
   (if (isa? req JsObject)
       (instantiate::http-response-xml
	  (backend (get/default req 'backend %this (hop-xml-backend)))
	  (start-line (get/default req 'startLine %this "HTTP/1.1 200 Ok"))
	  (content-type (get/default req 'contentType %this "application/x-javascript"))
	  (charset (get/default req 'charset %this (hop-charset)))
	  (header (get/list req 'header %this '((Cache-Control: . "no-cache") (Pragma: . "no-cache"))))
	  (xml obj))
       (instantiate::http-response-xml
	  (backend (hop-xml-backend))
	  (charset (hop-charset))
	  (content-type "text/html")
	  (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
	  (xml obj))))

;*---------------------------------------------------------------------*/
;*    hopjs-response-file ...                                          */
;*---------------------------------------------------------------------*/
(define (hopjs-response-file this file req %this)
   (if (isa? req JsObject)
       (instantiate::http-response-file
	  (charset (get/default req 'charset %this (hop-charset)))
	  (content-type (get/default req 'contentType %this #f))
	  (header (get/list req 'header %this '()))
	  (file (js-tostring file %this)))
       (instantiate::http-response-file
	  (file (js-tostring file %this)))))

;*---------------------------------------------------------------------*/
;*    hopjs-response-string ...                                        */
;*---------------------------------------------------------------------*/
(define (hopjs-response-string this string req %this)
   (if (isa? req JsObject)
       (instantiate::http-response-string
	  (charset (get/default req 'charset %this (hop-charset)))
	  (content-type (get/default req 'contentType %this #f))
	  (start-line (get/default req 'startLine %this "HTTP/1.1 200 Ok"))
	  (header (get/list req 'header %this '()))
	  (body (js-tostring string %this)))
       (instantiate::http-response-string
	  (body (js-tostring string %this)))))

;*---------------------------------------------------------------------*/
;*    hopjs-response-authentication ...                                */
;*---------------------------------------------------------------------*/
(define (hopjs-response-authentication this msg req %this)
   (let ((req (if (isa? req http-request) req (instantiate::http-request))))
      (access-denied req (js-tostring msg %this))))

;*---------------------------------------------------------------------*/
;*    hopjs-response-proxy ...                                         */
;*---------------------------------------------------------------------*/
(define (hopjs-response-proxy this url req %this)
   (let ((url (js-tostring url %this)))
      (multiple-value-bind (scheme uinfo host port abspath)
	 (http-url-parse url)
	 (instantiate::http-response-remote
	    (host host)
	    (port port)
	    (path abspath)
	    (header `((Host: . ,host)))))))

;*---------------------------------------------------------------------*/
;*    hopjs-response-async ...                                         */
;*---------------------------------------------------------------------*/
(define (hopjs-response-async this proc req %this %worker)
   
   (define (async-proc req)
      (if (isa? req http-request)
	  (lambda (k)
	     (js-worker-push-thunk! %worker "hopjs-response-async"
		(lambda ()
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
			    1 "reply" :src 'builtin))))))
	  (js-raise-type-error %this "not a request" req)))
   
   (if (isa? req JsObject)
       (let ((req (get/default req 'currentRequest %this #f)))
	  (instantiate::http-response-async
	     (charset (get/default req 'charset %this (hop-charset)))
	     (content-type (get/default req 'contentType %this #f))
	     (header (get/list req 'header %this '()))
	     (async (async-proc req))))
       (instantiate::http-response-async
	  (async (async-proc req)))))

;*---------------------------------------------------------------------*/
;*    hopjs-charset-convert ...                                        */
;*---------------------------------------------------------------------*/
(define (hopjs-charset-convert this text from to %this)
   (let ((from (js-tostring from %this))
         (to (js-tostring to %this)))
      (charset-convert (js-tostring text %this)
	 (if (string? from) (string->symbol from) (hop-locale))
	 (if (string? to) (string->symbol to) (hop-charset)))))
   
