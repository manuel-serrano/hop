;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_hop.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr 18 06:41:05 2014                          */
;*    Last change :  Wed Jan 14 09:36:08 2015 (serrano)                */
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

   (export (hopjs-process-hop ::WorkerHopThread ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    define-js ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (define-js name arity proc . opt)
   `(cons ',name
       (js-make-function %this ,proc ,arity ,(symbol->string name) ,@opt)))
		 
;*---------------------------------------------------------------------*/
;*    hopjs-process-hop ...                                            */
;*---------------------------------------------------------------------*/
(define (hopjs-process-hop %worker %this)
   (with-access::JsGlobalObject %this (js-object)
      (js-alist->jsobject
	 
	 (list
	    ;;configure
	    `(shareDir . ,(hop-share-directory))
	    `(binDir . ,(hop-bin-directory))
 	    `(libDir . ,(hop-lib-directory))
 	    `(contribsDir . ,(hop-contribs-directory))
 	    `(webletsDir . ,(hop-weblets-directory))

	    (define-js debug 0
	       (lambda (this) (bigloo-debug)))
	    (define-js debugSet 1
	       (lambda (this v) (bigloo-debug-set! (js-tointeger v %this))))
		
	    ;; misc
	    (define-js srcDir 0
	       (lambda (this)
		  (string->js-string (the-loading-dir))))
	    
	    (define-js srcFile 0
	       (lambda (this code)
		  (string->js-string (the-loading-file))))

	    (define-js currentThread 0
	       (lambda (this) (current-thread)))
	    
	    ;; info
	    `(port . ,(hop-port))
	    `(hostname . ,(string->js-string (hostname)))
	    `(version . ,(hop-version))
	       
	    ;; requests
	    (define-js withURL 3
	       (lambda (this url success opt)
		  (hopjs-with-url url success opt %this)))
	    
	    ;; charset
	    (define-js charsetConvert 3
	       (lambda (this text from to)
		  (string->js-string
		     (hopjs-charset-convert this text from to %this))))
	    
	    (define-js charset 0
	       (lambda (this)
		  (string->js-string (symbol->string! (hop-charset)))))
	    
	    (define-js charsetSet 1
	       (lambda (this cs)
		  (hop-charset-set! (string->symbol (js-tostring cs %this)))))
	    
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
	    
	    (define-js HTTPResponseAsync 1
	       (lambda (this proc req)
		  (hopjs-response-async this proc req %this %worker)))
	    
	    ;; events
	    (define-js signal 2
	       (lambda (this name v)
		  (hop-event-signal! (js-tostring name %this) v)))
	    
	    (define-js broadcast 2
	       (lambda (this name v)
		  (hop-event-broadcast! (js-tostring name %this) v)))

	    ;; xml
	    (define-js xmlCompile 3
	       (lambda (this xml ofile backend)
		  (let ((be (hop-get-xml-backend
			       (if (eq? backend (js-undefined))
				   'html5
				   (string->symbol
				      (js-tostring backend %this))))))
		     (if (eq? ofile (js-undefined))
			 (string->js-string
			    (call-with-output-string
			       (lambda (op)
				  (xml-write xml op be))))
			 (call-with-output-file (js-tostring ofile %this)
			    (lambda (op)
			       (xml-write xml op be)
			       (js-undefined)))))))
	    
	    ;; lib
	    (define-js parseWebColor 1
	       (lambda (this color)
		  (hopjs-parse-web-color (js-tostring color %this) %this)))
	    
	    (define-js makeWebColor 3 
	       (lambda (this r g b)
		  (string->js-string (make-hex-color r g b))))

	    (define-js encodeURIComponent 1
	       (lambda (this path)
		  (string->js-string
		     (url-path-encode (js-tostring path %this)))))

	    (define-js md5sum 1
	       (lambda (this path)
		  (string->js-string
		     (md5sum-string (js-tostring path %this)))))

	    (define-js sha1sum 1
	       (lambda (this path)
		  (string->js-string
		     (sha1sum-string (js-tostring path %this)))))

	    ;; Lists
	    (define-js List -1
	       (lambda (this . l)
		  l))

	    (define-js Cons 2
	       (lambda (this car cdr)
		  (cons car cdr)))
	    )
	 %this)))

;*---------------------------------------------------------------------*/
;*    hopjs-with-url ...                                               */
;*---------------------------------------------------------------------*/
(define (hopjs-with-url url success opt %this)
   
   (define (js-string->obj obj)
      (string->obj obj
	 (lambda (o)
	    (if (string? o) (js-javascript->obj o) o))))
   
   (define (js-javascript->obj obj)
      (javascript->obj obj %this))

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
	 :string->obj js-string->obj
	 :javascript->obj js-javascript->obj
	 :string->string string->js-string
	 :fail fail 
	 :timeout timeout
	 :method (string->symbol method))))

;*---------------------------------------------------------------------*/
;*    get/default ...                                                  */
;*---------------------------------------------------------------------*/
(define (get/default::obj obj key this def)
   (let ((v (js-get obj key this)))
      (cond
	 ((eq? v (js-undefined)) def)
	 ((js-string? v) (js-string->string v))
	 (else v))))

;*---------------------------------------------------------------------*/
;*    hopjs-response-hop ...                                           */
;*---------------------------------------------------------------------*/
(define (hopjs-response-hop this obj req %this)
   (if (isa? req JsObject)
       (instantiate::http-response-hop
	  (backend (get/default req 'backend %this (hop-xml-backend)))
	  (start-line (get/default req 'startLine %this "HTTP/1.1 200 Ok"))
	  (content-type (get/default req 'contentType %this "application/x-javascript"))
	  (charset (get/default req 'charset %this (hop-charset)))
	  (value obj))
       (instantiate::http-response-hop
	  (backend (hop-xml-backend))
	  (content-type "application/x-javascript")
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
	  (header (get/default req 'header %this '((Cache-Control: . "no-cache") (Pragma: . "no-cache"))))
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
	     (js-worker-exec %worker "hopjs-response-async"
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
			    1 "reply"))))))
	  (js-raise-type-error %this "not a request" req)))
   
   (if (isa? req JsObject)
       (let ((req (get/default req 'currentRequest %this #f)))
	  (instantiate::http-response-async
	     (charset (get/default req 'charset %this (hop-charset)))
	     (content-type (get/default req 'contentType %this #f))
	     (async (async-proc req))))
       (instantiate::http-response-async
	  (async (async-proc req)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-web-color ...                                        */
;*---------------------------------------------------------------------*/
(define (hopjs-parse-web-color color %this)
   (with-access::JsGlobalObject %this (js-object)
      (multiple-value-bind (r g b)
	 (parse-web-color color)
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
      (charset-convert (js-tostring text %this)
	 (if (string? from) (string->symbol from) (hop-locale))
	 (if (string? to) (string->symbol to) (hop-charset)))))
   
