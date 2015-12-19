;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/hop.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 25 15:30:55 2004                          */
;*    Last change :  Thu Dec 17 05:46:52 2015 (serrano)                */
;*    Copyright   :  2004-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP engine.                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop

   (library web)

   (cond-expand
      (enable-ssl (library ssl)))
   
   (include "verbose.sch")
   
   (import  __hop_param
	    __hop_types
	    __hop_misc
	    __hop_user
	    __hop_service
	    __hop_http-response
	    __hop_js-comp
	    __hop_json
	    __hop_xml-types
	    __hop_http-error
	    __hop_http-lib
	    __hop_weblets
	    __hop_xml)
   
   (with    __hop_event)
   
   (export  (generic thread-request ::obj)
	    (generic thread-request-set! ::obj ::obj)
	    (anonymous-request::http-request)
	    (request-get::obj ::http-request ::symbol)
	    (request->response::%http-response ::http-request ::obj)
	    (with-url ::bstring ::obj
		      #!key
		      fail
		      (header '())
		      (authorization #f)
		      (connection 'keep-alive)
		      (timeout 0)
		      (method 'GET)
		      (json-parser (lambda (ip ctx) (javascript->obj ip)))
		      (ctx #f)
		      body)
	    (with-hop-remote path success failure
			     #!key
			     (host "localhost")
			     (port (hop-port))
			     (abspath #f)
			     (user #f)
			     (password #f)
			     (authorization #f)
			     (header '())
			     (anim #f)
			     (scheme 'http)
			     (ctx #f)
			     (json-parser (lambda (ip ctx) (javascript->obj ip)))
			     args)
	    (generic with-hop-local obj success fail authorization header)
	    (hop-get-file::obj ::bstring ::obj)
	    (open-input-https-socket ::bstring bufinfo timeout)))

;*---------------------------------------------------------------------*/
;*    *anonymous-request* ...                                          */
;*---------------------------------------------------------------------*/
(define *anonymous-request* #f)

;*---------------------------------------------------------------------*/
;*    anonymous-request ...                                            */
;*---------------------------------------------------------------------*/
(define (anonymous-request)
   (unless (isa? *anonymous-request* http-request)
      (set! *anonymous-request*
	    (instantiate::http-server-request
	       (http 'HTTP/1.0)
	       (connection 'close))))
   *anonymous-request*)

;*---------------------------------------------------------------------*/
;*    *current-request* ...                                            */
;*---------------------------------------------------------------------*/
(define *current-request* #f)

;*---------------------------------------------------------------------*/
;*    thread-request ::obj ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (thread-request th)
   *current-request*)

;*---------------------------------------------------------------------*/
;*    thread-request-set! ::obj ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (thread-request-set! th::obj req)
   (set! *current-request* req))

;*---------------------------------------------------------------------*/
;*    current-request ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (current-request)
   (thread-request (current-thread)))

;*---------------------------------------------------------------------*/
;*    current-request-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (current-request-set! thread req)
   (thread-request-set! thread req))

;*---------------------------------------------------------------------*/
;*    request-get ...                                                  */
;*---------------------------------------------------------------------*/
(define (request-get req key)
   (let loop ()
      (cond
	 ((isa? req http-server-request+)
	  (with-access::http-server-request+ req (%env)
	     (let ((c (assq key %env)))
		(if (not (pair? c))
		    #unspecified
		    (cdr c)))))
	 ((isa? req http-server-request)
	  (widen!::http-server-request+ req
	     (%env (request-env-parse req)))
	  (loop))
	 (else
	  #unspecified))))

;*---------------------------------------------------------------------*/
;*    request-env-parse ...                                            */
;*---------------------------------------------------------------------*/
(define (request-env-parse req)
   (with-access::http-request req (header)
      (let ((env (http-header-field header hop-env:)))
	 (if (string? env)
	     (string->obj (url-decode env))
	     '()))))

;*---------------------------------------------------------------------*/
;*    request->response ...                                            */
;*    -------------------------------------------------------------    */
;*    This function assumes that (HOP-FILTERS) returns a read-only     */
;*    immutable data structure. In other words, it assumes that no     */
;*    other thread can change the list (HOP-FILTERS) in the            */
;*    background. Because of this assumption, no lock is needed in     */
;*    this function.                                                   */
;*---------------------------------------------------------------------*/
(define (request->response req::http-request thread)
   (let loop ((m req)
	      (filters (hop-filters)))
      (if (null? filters)
	  (with-access::http-request m (content-length method path host port
					  header userinfo scheme http)
	     (if (or (not (isa? req http-proxy-request))
		     (not (hop-enable-proxying)))
		 (instantiate::http-response-abort)
		 (let ((n (instantiate::http-response-proxy
			     (scheme scheme)
			     (method method)
			     (host host)
			     (port port)
			     (path path)
			     (userinfo userinfo)
			     (http http)
			     (header header)
			     (bodyp (not (eq? method 'HEAD)))
			     (content-length content-length)
			     (remote-timeout (hop-read-timeout))
			     (connection-timeout (hop-connection-timeout)))))
		    (hop-run-hook (hop-http-response-proxy-hooks) m n))))
	  (let ((n ((cdar filters) m)))
	     (cond
		((isa? n %http-response)
		 (hop-run-hook (hop-http-response-server-hooks) m n))
		((eq? n m)
		 (loop m (cdr filters)))
		((isa? n http-request)
		 (loop n (cdr filters)))
		((eq? n 'hop-resume)
		 (loop m (hop-filters)))
		(else
		 (loop m (cdr filters))))))))

;*---------------------------------------------------------------------*/
;*    hop-run-hook ...                                                 */
;*---------------------------------------------------------------------*/
(define (hop-run-hook hooks m rp)
   (let loop ((hooks hooks)
	      (rp rp))
      (if (null? hooks)
	  rp
	  (loop (cdr hooks) ((car hooks) m rp)))))

;*---------------------------------------------------------------------*/
;*    header-content-type ...                                          */
;*---------------------------------------------------------------------*/
(define (header-content-type header)
   (let ((cell (assq content-type: header)))
      (when (and (pair? cell) (string? (cdr cell)))
	 (let* ((str (cdr cell))
		(i (string-index str #\;)))
	    (if i
		(string->symbol (substring str 0 i))
		(string->symbol str))))))

;*---------------------------------------------------------------------*/
;*    byte-array->string ...                                           */
;*---------------------------------------------------------------------*/
(define (byte-array->string v)
   (let* ((len (vector-length v))
	  (s (make-string len)))
      (let loop ((i 0))
	 (when (<fx i len)
	    (string-set! s i (integer->char (vector-ref v i)))
	    (loop (+fx i 1))))
      s))

;*---------------------------------------------------------------------*/
;*    make-http-callback ...                                           */
;*---------------------------------------------------------------------*/
(define (make-http-callback url req success fail user-or-auth ctx responsetype json-parser)
   
   (define (http-callback-decode ctype clength p ctx)
      (case ctype
	 ((application/x-hop)
	  (let* ((chars (read-chars clength p))
		 (s (if (eq? responsetype 'arraybuffer)
			chars
			(string-hex-intern chars))))
	     (string->obj s #f ctx)))
	 ((application/x-url-hop)
	  (string->obj
	     (url-decode
		(if (>elong clength #e0)
		    (read-chars clength p)
		    (read-string p)))
	     #f ctx))
	 ((application/x-json-hop)
	  (string->obj
	     (byte-array->string 
		(javascript->obj
		   (if (>elong clength #e0)
		       (read-chars clength p)
		       (read-string p))))
	     #f ctx))
	 ((application/json)
	  (json-parser p ctx))
	 ((application/x-javascript)
	  (json-parser p ctx))
	 ((text/html application/xhtml+xml)
	  (car (last-pair (parse-html p (elong->fixnum clength)))))
	 (else
	  (read-string p))))
   
   (define (default-error-handling status header p)
      (if (procedure? fail)
	  (fail (instantiate::xml-http-request
		   (status status)
		   (header header)
		   (input-port p)))
	  (raise
	     (instantiate::&error
		(proc url)
		(msg (format "Illegal status `~a'" status))
		(obj (when (input-port? p) (read-string p)))))))
   
   (lambda (p status header clength tenc)
      (with-trace 'with-hop "make-http-callback"
	 (trace-item "status=" status " content-length=" clength)
	 (when (and (input-port? p) (>elong clength #e0))
	    (input-port-fill-barrier-set! p (elong->fixnum clength)))
	 (case status
	    ((200)
	     (trace-item "ctype=" (header-content-type header))
	     (trace-item "header=" header)
	     ;; see hop-json-mime-type and hop-bigloo-mime-type
	     (let ((obj (http-callback-decode (header-content-type header)
			   clength p ctx)))
		(success obj)))
	    ((201 204 304)
	     ;; no message body
	     (success (instantiate::xml-http-request
			 (status status)
			 (header header)
			 (input-port p))))
	    ((401 407)
	     (if (procedure? fail)
		 (fail (instantiate::xml-http-request
			  (status status)
			  (header header)
			  (input-port p)))
		 (raise
		    (instantiate::&hop-authentication-error
		       (proc url)
		       (msg "Authentication requested by remote server")
		       (obj user-or-auth)))))
	    ((500)
	     (cond
		((assq :hop-error header)
		 =>
		 (lambda (c)
		    (if (cdr c)
			(let ((val (string->obj (url-decode (cdr c)) ctx)))
			   (if (procedure? fail)
			       (fail val)
			       (raise
				  (instantiate::&error
				     (proc url)
				     (msg (format "Illegal status `~a'" status))
				     (obj (when (input-port? p) (read-string p)))))))
			(default-error-handling status header p))))
		(else
		 (default-error-handling status header p))))
	    (else
	     (default-error-handling status header p))))))

;*---------------------------------------------------------------------*/
;*    hop-to-hop-id ...                                                */
;*---------------------------------------------------------------------*/
(define hop-to-hop-id -1)

;*---------------------------------------------------------------------*/
;*    scheme-default-port ...                                          */
;*---------------------------------------------------------------------*/
(define (scheme-default-port scheme)
   (case scheme
      ((https) 443)
      (else 80)))
   
;*---------------------------------------------------------------------*/
;*    with-url  ...                                                    */
;*---------------------------------------------------------------------*/
(define (with-url url success
		  #!key fail
		  (header '())
		  (authorization #f)
		  (timeout 0)
		  (method 'GET)
		  (connection 'keep-alive)
		  (json-parser (lambda (ip ctx) (javascript->obj ip)))
		  (ctx #f)
		  body)
   (set! hop-to-hop-id (-fx hop-to-hop-id 1))
   (hop-verb 1 (hop-color hop-to-hop-id hop-to-hop-id " WITH-URL")
	     ": " url "\n")
   (with-trace 'with-hop "with-url"
      (trace-item "url=" url)
      (trace-item "header=" header)
      (cond
	 ((and (procedure? fail) (not (correct-arity? fail 1)))
	  (error "with-url" "Illegal fail handler" fail))
	 ((and (procedure? success) (not (correct-arity? success 1)))
	  (error "with-url" "Illegal success handler" success))
	 (else
	  (multiple-value-bind (scheme userinfo host port path)
	     (url-parse url)
	     (cond
		((or (string=? scheme "file")
		     (string=? scheme "string")
		     (string=? scheme "| ")
		     (string=? scheme "pipe")
		     (string=? scheme "gzip")
		     (string=? scheme "/resource/"))
		 (let ((p (open-input-file url)))
		    (if (input-port? p)
			(let ((s (read-string p))
			      (suc (if (procedure? success)
				       success
				       (lambda (x) x))))
			   (close-input-port p)
			   (suc s))
			(if (procedure? fail)
			    (fail url)
			    (raise (instantiate::&io-error
				      (proc "with-url")
				      (msg "Cannot open url")
				      (obj url)))))))
		(else
		 (let* ((s (string->symbol scheme))
			(r (instantiate::http-server-request
			      (scheme s)
			      (id hop-to-hop-id)
			      (userinfo userinfo)
			      (host (or host path))
			      (port (or port (scheme-default-port s)))
			      (header (cons `(connection: . ,connection) header))
			      (connection-timeout timeout)
			      (connection connection)
			      (timeout timeout)
			      (method method)
			      (authorization authorization)
			      (path (if host path "/"))))
			(suc (if (procedure? success) success (lambda (x) x)))
			(hdl (make-http-callback url r suc fail #f ctx 'plain json-parser)))
		    (trace-item "remote path=" path)
		    (http-send-request r hdl :body body)))))))))

;*---------------------------------------------------------------------*/
;*    with-hop-remote ...                                              */
;*---------------------------------------------------------------------*/
(define (with-hop-remote path success fail
	   #!key
	   (host "localhost")
	   (port (hop-port))
	   (abspath #f)
	   (user #f)
	   (password #f)
	   (header '())
	   (authorization #f)
	   (anim #f)
	   (scheme 'http)
	   (json-parser (lambda (ip ctx) (javascript->obj ip)))
	   (ctx #f)
	   args)
   (set! hop-to-hop-id (-fx hop-to-hop-id 1))
   (hop-verb 1 (hop-color hop-to-hop-id hop-to-hop-id " WITH-HOP")
      " " scheme "://" host ":" port
      (if (and (=fx (hop-verbose) 1) (>fx (string-length path) 80))
	  (string-append (substring path 0 80) "...")
	  path)
      "\n")
   (with-trace 'with-hop "with-hop-remote"
      (trace-item "host=" host " port=" port " path=" path " abspath=" abspath)
      (trace-item "authorization=" authorization)
      (cond
	 ((and (procedure? fail) (not (correct-arity? fail 1)))
	  (error "with-hop" "Illegal fail handler" fail))
	 ((and (procedure? success) (not (correct-arity? success 1)))
	  (error "with-hop" "Illegal success handler" success))
	 (else
	  (let* ((req (instantiate::http-server-request
			 (userinfo (when (and (string? user) (string? password))
				      (string-append user ":" password)))
			 (id hop-to-hop-id)
			 (host host)
			 (port port)
			 (connection 'close)
			 (header (cons*
				    '(hop-serialize: . "x-hop")
				    '(hop-responsetype: . "arraybuffer")
				    '(hop-client: . "hop")
				    header))
			 (method (if (pair? args) 'post 'put))
			 (authorization authorization)
			 (scheme scheme)
			 (path (or abspath path))))
		 (suc (or success (lambda (x) x)))
		 (hdl (make-http-callback path req suc fail
			 (or user authorization) ctx 'arraybuffer
			 json-parser)))
	     (trace-item "remote path=" path)
	     (http-send-request req hdl :args args))))))

;*---------------------------------------------------------------------*/
;*    fail-or-raise ...                                                */
;*---------------------------------------------------------------------*/
(define (fail-or-raise fail exc)
   (if (procedure? fail)
       (fail exc)
       (raise exc)))

;*---------------------------------------------------------------------*/
;*    with-hop-local ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (with-hop-local obj success fail auth header)
   (when (procedure? success) (success obj)))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-proxy ...                         */
;*    -------------------------------------------------------------    */
;*    This method is used for imported services. These services        */
;*    are called locally but they are still remote.                    */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-proxy success fail auth header)
   (with-access::http-response-proxy obj (path host port)
      (with-hop-remote path success fail
	 :host host :port port :authorization auth :header header)))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-autoload ...                      */
;*    -------------------------------------------------------------    */
;*    This method is used for imported local services (without body).  */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-autoload success fail auth header)
   (with-access::http-response-autoload obj (request)
      (with-access::http-request request (path)
	 (let ((rep (service-filter request)))
	    (if (isa? rep %http-response)
		(with-hop-local rep success fail auth header)
		(error "with-hop" "Bad auto-loaded local service" path))))))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::xml ...                                         */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::xml success fail auth header)
   (when (procedure? success) (success obj)))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-authentication ...                */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local o::http-response-authentication success fail aut header)
   (when (procedure? success)
      (success (instantiate::&error
		  (proc "with-hop")
		  (msg "Authentication required")
		  (obj o)))))
   
;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-string ...                        */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-string success fail auth header)
   (when (procedure? success)
      (with-access::http-response-string obj (body)
	 (success body))))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-hop ...                           */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-hop success fail auth header)
   (when (procedure? success)
      (with-access::http-response-hop obj (value)
	 (success value))))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-xml ...                           */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-xml success fail auth header)
   (when (procedure? success)
      (with-access::http-response-xml obj (xml)
	 (success xml))))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-procedure ...                     */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-procedure success fail auth header)
   (when (procedure? success)
      (with-access::http-response-procedure obj (proc)
	 (success (with-output-to-string proc)))))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-file ...                          */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-file success fail auth header)
   (with-access::http-response-file obj (file)
      (let ((pf (open-input-file file)))
	 (if (not (input-port? pf))
	     (fail-or-raise
		fail
		(if (not (file-exists? file))
		    (instantiate::&io-file-not-found-error
		       (proc "with-hop")
		       (msg "File not found")
		       (obj file))
		    (instantiate::&io-port-error
		       (proc "with-hop")
		       (msg "Cannot open file")
		       (obj file))))
	     (unwind-protect
		(when (procedure? success) (success (read-string pf)))
		(close-input-port pf))))))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-cgi ...                           */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-cgi success fail auth header)
   (fail-or-raise
    fail
    (instantiate::&error
       (proc "with-response")
       (msg "Illegal response")
       (obj obj))))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-put ...                           */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-put success fail auth header)
   (fail-or-raise
    fail
    (instantiate::&error
       (proc "with-response")
       (msg "Illegal response")
       (obj obj))))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-filter ...                        */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-filter success fail auth header)
   (fail-or-raise
    fail 
    (instantiate::&error
       (proc "with-response")
       (msg "Illegal response")
       (obj obj))))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-persistent ...                    */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local o::http-response-persistent success fail auth header)
   (when (procedure? success) (success o)))

;*---------------------------------------------------------------------*/
;*    hop-get-file ...                                                 */
;*    -------------------------------------------------------------    */
;*    This function is used by Hop to ask itself files. That is, when  */
;*    a weblet has to read a file as it was provided by the            */
;*    broker it can't simply open it because the broker may have       */
;*    defined filters that tells how to server that very file.         */
;*    The function HOP-GET-FILE simulates a user request to            */
;*    the broker.                                                      */
;*    This function has been first added for implementing the          */
;*    :inline option of HEAD, LINK, and SCRIPT markups. Since version  */
;*    1.9.0, it is only used in the SCRIPT markup.                     */
;*---------------------------------------------------------------------*/
(define (hop-get-file path thread)
   (let* ((reqi (current-request))
	  (req (instantiate::http-server-request
		  (path path)))
	  (rep (request->response req thread)))
      (cond
	 ((isa? rep http-response-file)
	  (with-access::http-response-file rep (file)
	     (let ((p (open-input-file file)))
		(if (input-port? p)
		    (unwind-protect
		       (read-string p)
		       (close-input-port p))
		    #f))))
	 ((isa? rep http-response-procedure)
	  (let ((p (open-output-string)))
	     (with-access::http-response-procedure rep (proc)
		(unwind-protect
		   (proc p)
		   (close-output-port p)))))
	 ((isa? rep http-response-string)
	  (with-access::http-response-string rep (body)
	     body))
	 (else
	  #f))))
   
;*---------------------------------------------------------------------*/
;*    open-input-https-socket ...                                      */
;*---------------------------------------------------------------------*/
(define (open-input-https-socket string bufinfo timeout)
   
   (define (parser ip status-code header clen tenc)
      (when (and (>=fx status-code 200) (<=fx status-code 299))
	 (cond
	    ((not (input-port? ip))
	     (open-input-string ""))
	    ((and (elong? clen) (>elong clen 0))
	     (input-port-fill-barrier-set! ip (elong->fixnum clen))
	     ($input-port-length-set! ip clen)
	     ip)
	    (else
	     ip))))

   (cond-expand
      (enable-ssl
       (multiple-value-bind (protocol login host port abspath)
	  (url-sans-protocol-parse string "https")
	  (let loop ((ip #f)
		     (header `((user-agent: ,(hop-user-agent))
			       (Connection: keep-alive))))
	     (let* ((sock (http :socket
			     (make-ssl-client-socket host port
				:timeout timeout)
			     :protocol 'https
			     :host host
			     :port port
			     :login login
			     :path abspath
			     :timeout timeout
			     :header header))
		    (op (socket-output sock)))
		(if (input-port? ip)
		    ;; the socket has been re-opened for seek, reuse
		    ;; the user input-port
		    (input-port-clone! ip (socket-input sock))
		    (set! ip (socket-input sock)))
		(input-port-close-hook-set! ip
		   (lambda (ip)
		      (close-output-port op)
		      (socket-close sock)))
		(input-port-seek-set! ip
		   (lambda (ip offset)
		      (socket-close sock)
		      (let ((r (string-append "bytes=" (integer->string offset) "-")))
			 (loop ip `((range: ,r)
				    (user-agent: "Mozilla/5.0"))))))
		(with-handler
		   (lambda (e)
		      (socket-close sock)
		      (when (isa? e &http-redirection)
			 (with-access::&http-redirection e (url)
			    (open-input-file url bufinfo))))
		   (http-parse-response ip op parser))))))
      (else
       (error "open-input-https-socket" "ssl disable" string))))
