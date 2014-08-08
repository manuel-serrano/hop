;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_http.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug  7 06:23:37 2014                          */
;*    Last change :  Fri Aug  8 07:00:40 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HTTP bindings                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__http
   
   (library hopscript)

   (import  __nodejs_uv)

   (static (class JsHttpParser::JsObject
	      (buffer (default #f))))

   (export (process-http-parser ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    process-http-parser ...                                          */
;*---------------------------------------------------------------------*/
(define (process-http-parser %this)
   
   (define (not-implemented name)
      (js-make-function %this
	 (lambda (this . l)
	    (error "crypto" "binding not implemented" name))
	 0 name))

   (define (exn msg . args)
      (with-access::JsGlobalObject %this (js-object js-error)
	 (raise
	    (js-new %this js-error (apply format msg args)))))
   
   (define http-parser-proto
      (with-access::JsGlobalObject %this (js-object)
	 (let ((proto (js-new %this js-object)))
	    (js-put! proto 'reinitialize
	       (js-make-function %this
		  (lambda (this kind)
		     (if (not (or (eq? kind 0) (eq? kind 1)))
			 (exn "Argument must be HTTPParser.REQUEST or HTTPParser.RESPONSE")
			 (with-access::JsHttpParser this (buffer)
			    (set! buffer #f)
			    (js-undefined))))
		  1 'reinitialize)
	       #f %this)
	    (js-put! proto 'execute
	       (js-make-function %this
		  (lambda (this buf offset length)
		     (let ((off (->fixnum (js-tointeger offset %this)))
			   (len (->fixnum (js-tointeger length %this))))
			(with-access::JsHttpParser this (buffer)
			   (when buffer
			      (exn "Already parsing a buffer"))
			   (unless (string? buf)
			      (exn "Argument should be a buffer ~a" (typeof buf)))
			   (when (> off (string-length buf))
			      (exn "Offset is out of bounds"))
			   (when (> (+fx off len) (string-length buf))
			      (exn "off + len > buffer.length"))
			   (set! buffer buf)
			   (http-parser-execute %this this buf off len)
			   (set! buffer #f)
			   (js-undefined))))
		  1 'execute)
	       #f %this)
	    proto)))
   
   (define http-proto
      (let ((proto (with-access::JsGlobalObject %this (js-object)
		      (js-new %this js-object))))
	 (js-put! proto 'REQUEST 0 #f %this)
	 (js-put! proto 'RESPONSE 1  #f %this)
	 proto))
   
   (define (http-parser this kind)
      (with-access::JsGlobalObject %this (js-object)
	 (instantiate::JsHttpParser
	    (__proto__ http-parser-proto))))
   
   (let ((http (js-make-function %this http-parser 1 "HTTPParser"
		  :alloc (lambda (o) #unspecified)
		  :construct http-parser
		  :prototype http-parser-proto)))
      (with-access::JsObject http (__proto__)
	 (set! __proto__ http-proto))
      (js-alist->jsobject
	 `((HTTPParser . ,http))
	 %this)))

;*---------------------------------------------------------------------*/
;*    ->fixnum ...                                                     */
;*---------------------------------------------------------------------*/
(define (->fixnum n)
   (cond
      ((fixnum? n) n)
      ((flonum? n) (flonum->fixnum n))
      (else 0)))

;*---------------------------------------------------------------------*/
;*    http-parser-execute ...                                          */
;*---------------------------------------------------------------------*/
(define (http-parser-execute %this parser buffer off len)
   (let* ((ip (open-input-string buffer off))
	  (obj (read/rp http-line-grammar ip %this parser)))
      ;; don't unwind-protect as it is unimportant not to close the port
      (close-input-port ip)
      obj))

;*---------------------------------------------------------------------*/
;*    http-line-grammar ...                                            */
;*---------------------------------------------------------------------*/
(define http-line-grammar
   (regular-grammar ((SP #\Space)
		     (CRLF "\r\n")
		     %this parser)
      ((: (+ (in ("AZ"))) SP)
       ;; HTTP request
       (http-parse-request (the-substring 0 -1) (the-port) %this parser))
      ((: "HTTP/" (: (+ (in ("09"))) #\. (+ (in ("09")))) SP)
       ;; HTTP response
       (let ((len (the-length)))
	  (http-parse-response "http" (the-substring 5 (-fx len 1))
	     (the-port) %this parser)))
      ((: "HTTPS/" (: (+ (in ("09"))) #\. (+ (in ("09")))) SP)
       ;; HTTPS response
       (let ((len (the-length)))
	  (http-parse-response "https" (the-substring 6 (-fx len 1))
	     (the-port) %this parser)))
      ((: (out #\< SP) (+ (out SP)) SP)
       ;; Illegal (parsed) requests
       (tprint "FAILURE TODO..."))
      (else
       (let ((o (the-failure)))
	  (tprint "FAILURE TODO...")))))

;*---------------------------------------------------------------------*/
;*    http-parse-request ...                                           */
;*---------------------------------------------------------------------*/
(define (http-parse-request method pi::input-port %this parser)
   (with-trace 3 "http-parse-method-request"
      (let (scheme host port path http-version userinfo)
	 (multiple-value-bind (s u h p a)
	    (http-url-parse pi)
	    (set! scheme (string->symbol s))
	    (set! host h)
	    (set! port p)
	    (set! path a)
	    (set! userinfo u)
	    (set! http-version (read/rp http-version-grammar pi))
	    (trace-item "http=" http-version " scheme=" s " user=" u
	       " host=" h " port=" p " path=[" a "]"))
	 (multiple-value-bind (header actual-host actual-port cl te auth pauth co)
	    (http-parse-header pi #f)
	    (with-access::JsGlobalObject %this (js-object)
	       (let* ((i (string-index path #\?))
		      (query #f)
		      (abspath (cond
				  ((not i)
				   ;; file name canonicalization is needed
				   ;; for authentication
				   (file-name-canonicalize! (url-decode! path)))
				  ((>fx i 0)
				   (let ((l (string-length path)))
				      (set! query (substring path (+fx i 1) l)))
				   (let ((p (url-decode! (substring path 0 i))))
				      (file-name-canonicalize! p)))
				  (else
				   (let ((l (string-length path)))
				      (set! query (substring path 1 l)))   
				   "/")))
		      (minfo (js-new %this js-object)))
		  (js-put! minfo 'method method #f %this)
		  (let ((i (string-index http-version #\.)))
		     (when i
			(js-put! minfo 'versionMajor
			   (string->integer (substring http-version 0 i))
			   #f %this)
			(js-put! minfo 'versionMinor
			   (string->integer (substring http-version (+fx i 1)))
			   #f %this)))
		  (js-put! minfo 'versionminor http-version #f %this)
		  (js-put! minfo 'shouldkeepAlive (eq? http-version 'HTTP/1.1) #f %this)
		  (js-put! minfo 'upgrade (memq 'upgrade header) #f %this)
		  (let ((oncomp (js-get parser 'onHeadersComplete %this)))
		     (js-call1 %this oncomp parser minfo))))))))

;*---------------------------------------------------------------------*/
;*    http-parse-response ...                                          */
;*---------------------------------------------------------------------*/
(define (http-parse-response protocol http-version pi::input-port %this parser)
   (with-access::JsGlobalObject %this (js-object)
      (let ((minfo (js-new %this js-object)))
	 (let ((status (read pi)))
	    (if (not (integer? status))
		(tprint "BAD STATUS")
		;; parse the start line
		(let ((msg (read-line pi)))
		   (js-put! minfo 'statusCode status #f %this)
		   (let ((i (string-index http-version #\.)))
		      (when i
			 (js-put! minfo 'versionMajor
			    (string->integer (substring http-version 0 i))
			    #f %this)
			 (js-put! minfo 'versionMinor
			    (string->integer (substring http-version (+fx i 1)))
			    #f %this)))
		   ;; parse the header
		   (multiple-value-bind (headers actual-host actual-port cl
					   tenc auth pauth co)
		      (http-parse-header pi #f)
		      (js-put! minfo 'upgrade (memq 'upgrade headers) #f %this)
		      (js-put! minfo 'headers
			 (js-vector->jsarray
			    (list->vector
			       (append-map
				  (lambda (p)
				     (let ((key (keyword->string! (car p)))
					   (val (if (symbol? (cdr p))
						    (symbol->string! (cdr p))
						    (cdr p))))
					(list key val)))
				  headers))
			    %this)
			 #f %this))
		   (let ((onhdcomp (js-get parser 'onHeadersComplete %this)))
		      (js-call1 %this onhdcomp parser minfo))
		   ;; body
		   
		   (let ((onbody (js-get parser 'onBody %this)))
		      (js-call1 %this onbody parser minfo))
		   (let ((onmsgcomp (js-get parser 'onMessageComplete %this)))
		      (js-call1 %this onmsgcomp parser minfo))))))))
	  
;*---------------------------------------------------------------------*/
;*    http-version-grammar ...                                         */
;*---------------------------------------------------------------------*/
(define http-version-grammar
   (regular-grammar ((DIGIT (in ("09")))
		     (SP (+ #\Space)))
      (SP
       (ignore))
      ((: "HTTP/" (+ DIGIT) "." (+ DIGIT) "\n")
       (the-subsymbol 0 -1))
      ((: "HTTP/" (+ DIGIT) "." (+ DIGIT) "\r\n")
       (the-subsymbol 0 -2))
      (else
       (tprint "FAILURE VERSION TODO"))))

;*---------------------------------------------------------------------*/
;*    http-sp-grammar ...                                              */
;*---------------------------------------------------------------------*/
(define http-sp-grammar
   (regular-grammar ((SP #\Space))
      (SP 'sp)
      (else (tprint "FAILURE SP TODO"))))


   
