;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/http-response.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 25 14:15:42 2004                          */
;*    Last change :  Thu Sep  6 18:02:34 2007 (serrano)                */
;*    Copyright   :  2004-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HTTP response                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_http-response

   (library web)
   
   (include "http-lib.sch")
   
   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_misc
	    __hop_xml
	    __hop_hop-extra
	    __hop_http-lib
	    __hop_http-error
	    __hop_http-filter
	    __hop_js-lib
	    __hop_user)

   (export  (generic http-response::symbol ::%http-response ::socket)
	    (generic scheme->response ::obj ::http-request)
	    (http-response-void)
	    (http-send-request ::http-request ::procedure)
	    (response-is-xml?::bool ::obj)))

;*---------------------------------------------------------------------*/
;*    http-response ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (http-response::symbol r::%http-response socket))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-authentication ...                 */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-authentication socket)
   (with-trace 3 'http-response::http-response-authentication
      (with-access::http-response-authentication r (header content-type body server timeout request start-line)
	 (let ((p (socket-output socket))
	       (connection (http-request-connection request)))
	    (when (>fx timeout 0)
	       (output-timeout-set! p timeout))
	    (http-write-line p start-line)
	    (http-write-header p header)
	    (http-write-line p "Connection: " connection)
	    (when content-type
	       (http-write-line p "Content-Type: " content-type))
	    (when server
	       (http-write-line p "Server: " server))
	    (when (string? body)
	       (http-write-line p "Content-Length: " (string-length body)))
	    (http-write-line p)
	    (when (string? body) (display body p))
	    (flush-output-port p)
	    connection))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-string ...                         */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-string socket)
   (with-trace 3 'http-response::http-response-string
      (with-access::http-response-string r (start-line header content-type server content-length bodyp body char-encoding timeout request)
	 (let* ((p (socket-output socket))
		(ce (or char-encoding (hop-char-encoding)))
		(s body)
		(connection (http-request-connection request)))
	    (when (>fx timeout 0)
	       (output-timeout-set! p timeout))
	    (http-write-line p start-line)
	    (http-write-header p header)
	    (http-write-line p "Content-Length: " (string-length s))
	    (http-write-line p "Connection: " connection)
	    (when content-type
	       (http-write-line p "Content-Type: " content-type))
	    (when server
	       (http-write-line p "Server: " server))
	    (http-write-line p)
	    (when bodyp (display s p))
	    (flush-output-port p)
	    connection))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-js ...                             */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-js socket)
   (with-trace 3 'http-response::http-response-js
      (with-access::http-response-js r (start-line header content-type server content-length value bodyp timeout request)
	 (let ((p (socket-output socket))
	       (connection (http-request-connection request)))
	    (when (>fx timeout 0)
	       (output-timeout-set! p timeout))
	    (http-write-line p "HTTP/1.1 200 Ok")
	    (http-write-header p header)
	    (if (>elong content-length #e0)
		(http-write-line p "Content-Length: " content-length)
		(set! connection 'close))
	    (http-write-line p "Connection: " connection)
	    (when content-type
	       (http-write-line p "Content-Type: " content-type))
	    (when server
	       (http-write-line p "Server: " server))
	    ;; The case of the header names depends on the browse
	    ;; by doubling the initial h, we eliminate the need for
	    ;; testing twice the header in the client (see hop_is_http_json,
	    ;; defined in hop-autoconf.js.in).
	    (http-write-line p "Hhopjson: true")
	    (http-write-line p)
	    ;; the body
	    (with-trace 4 'http-response-js
	       (when bodyp (display (hop->json value) p)))
	    (flush-output-port p)
	    connection))))
      
;*---------------------------------------------------------------------*/
;*    http-response ::http-response-hop ...                            */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-hop socket)
   (with-trace 3 'http-response::http-response-hop
      (with-access::http-response-hop r (start-line header content-type server content-length xml char-encoding bodyp timeout request backend)
	 (let ((p (socket-output socket))
	       (connection (http-request-connection request)))
	    (when (>fx timeout 0)
	       (output-timeout-set! p timeout))
	    (http-write-line p start-line)
	    (http-write-header p header)
	    (if (>elong content-length #e0)
		(http-write-line p "Content-Length: " content-length)
		(set! connection 'close))
	    (http-write-line p "Connection: " connection)
	    (when content-type
	       (http-write-line p "Content-Type: " content-type))
	    (when server
	       (http-write-line p "Server: " server))
	    (http-write-line p)
	    ;; the body
	    (with-trace 4 'http-response-hop
	       (when bodyp
		  (xml-write xml
			     p
			     (or char-encoding (hop-char-encoding))
			     backend)))
	    (flush-output-port p)
	    connection))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-procedure ...                      */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-procedure socket)
   (with-trace 3 'http-response::http-response-procedure
      (with-access::http-response-procedure r (start-line header content-type server content-length proc bodyp timeout request)
	 (let ((p (socket-output socket))
	       (connection (http-request-connection request)))
	    (when (>fx timeout 0)
	       (output-timeout-set! p timeout))
	    (http-write-line p start-line)
	    (http-write-header p header)
	    (if (>elong content-length #e0)
		(http-write-line p "Content-Length: " content-length)
		(set! connection 'close))
	    (http-write-line p "Connection: " connection)
	    (when content-type
	       (http-write-line p "Content-Type: " content-type))
	    (when server
	       (http-write-line p "Server: " server))
	    (http-write-line p)
	    ;; the body
	    (with-trace 4 'http-response-procedure
	       (when bodyp (proc p)))
	    (flush-output-port p)
	    connection))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-file ...                           */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-file socket)
   (with-trace 3 'http-response::http-response-file
      (with-access::http-response-file r (start-line header content-type server file bodyp request timeout)
	 (if (authorized-path? request file)
	     ;; In C the file is never read with RGC so it can
	     ;; be open with a tiny buffer
	     (let ((p (socket-output socket))
		   (pf (cond-expand
			  (bigloo-c (open-input-file file 1))
			  (else (open-input-file file))))
		   (connection (http-request-connection request)))
		(cond
		   ((not (input-port? pf))
		    (let ((rep (if (file-exists? file)
				   (http-permission-denied file)
				   (http-file-not-found file))))
		       (http-response rep socket)))
		   ((directory? file)
		    (let ((rep (response-directory r file)))
		       (http-response rep socket)))
		   (else
		    (when (>fx timeout 0)
		       (output-timeout-set! p timeout))
		    (http-write-line p start-line)
		    (http-write-header p header)
		    (http-write-line p "Connection: " connection)
		    (when content-type
		       (http-write-line p "Content-Type: " content-type))
		    (when server
		       (http-write-line p "Server: " server))
		    (http-write-line p "Content-Length: " (file-size file))
		    (http-write-line p)
		    ;; the body
		    (with-trace 4 'http-response-file
		       (when bodyp
			  (unwind-protect
			     (send-chars pf p)
			     (close-input-port pf))))
		    (flush-output-port p)
		    connection)))
	     (http-response (user-access-denied request) socket)))))

;*---------------------------------------------------------------------*/
;*    response-directory ...                                           */
;*---------------------------------------------------------------------*/
(define (response-directory rep dir)
   (instantiate::http-response-hop
      (backend (hop-xml-backend))
      (content-type (xml-backend-mime-type (hop-xml-backend)))
      (char-encoding (request-encoding (%http-response-request rep)))
      (request (%http-response-request rep))
      (xml (if (%http-response-bodyp rep)
	       (<HTML>
		  (<HEAD> (<TITLE> dir))
		  (<BODY>
		     (<H1> dir)
		     (<PRE> (map (lambda (f)
				    (let ((path (make-file-name dir f)))
				       (<A> :href path
					 (if (directory? path)
					     (string-append f "/\n")
					     (string-append f "\n")))))
				 (sort (directory->list dir) string<?)))))
	       '()))))

;*---------------------------------------------------------------------*/
;*    hop-cgi-env ...                                                  */
;*---------------------------------------------------------------------*/
(define (hop-cgi-env socket r path qstr method)
   (with-access::%http-response r (request header content-type)
      (let* ((auth (http-header-field header authorization:))
	     (auth-type (if (and (string? auth) (substring=? auth "Basic" 5))
			    "Basic"
			    ""))
	     (auth-user (if (string=? auth-type "")
			    ""
			    (let ((vs (http-header-field-values
				       (substring auth 6 (string-length auth)))))
			       (if (pair? vs)
				   (caar vs)
				   ""))))
	     (rident (http-header-field header user-agent:)))
	 (cons* env: (format "AUTH_TYPE=")
		env: (format "CONTENT_LENGTH=~a"
			     (if (http-response-cgi? r)
				 (http-response-cgi-content-length r)
				 ""))
		env: (format "CONTENT_TYPE=~a" content-type)
		env: "GATEWAY_INTERFACE=CGI/1.2"
		env: (format "PATH_INFO=~a" path)
		env: (format "PATH_TRANSLATED=~a" path)
		env: (format "QUERY_STRING=~a" qstr)
		env: (format "REMOTE_ADDR=~a" (socket-host-address socket))
		env: (format "REMOTE_HOST=~a" (socket-hostname socket))
		env: (format "REMOTE_IDENT=~a" rident)
		env: (format "REMOTE_USER=~a" auth-user)
		env: (format "REQUEST_METHOD=~a" method)
		env: (format "SCRIPT_NAME=~a" path)
		env: (format "SERVER_NAME=~a" (http-request-host request))
		env: (format "SERVER_PORT=~a" (hop-port))
		env: (format "SERVER_PROTOCOL=HTTP/1.1")
		env: (format "SERVER_SOFTWARE=~a~a" (hop-name) (hop-version))
		(apply append
		       (map (lambda (h)
			       (let* ((k (car h))
				      (v (cdr h))
				      (ks (string-replace!
					   (string-upcase! (keyword->string k))
					   #\-
					   #\_)))
				  (list env: (format "HTTP_~s=~s" ks v))))
			    header))))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-cgi ...                            */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-cgi socket)
   (with-trace 3 'http-response::http-response-cgi
      (with-access::http-response-cgi r (start-line header content-type server cgibin bodyp request timeout)
	 (if (authorized-path? request cgibin)
	     (let ((p (socket-output socket)))
		(when (>fx timeout 0)
		   (output-timeout-set! p timeout))
		(http-write-line p start-line)
		(http-write-header p header)
		(http-write-line p "Connection: close")
		(when content-type
		   (http-write-line p "Content-Type: " content-type))
		(when server
		   (http-write-line p "Server: " server))
		(http-write-line p)
		;; the body
		(with-trace 4 'http-response-cgi-process
		   (let* ((pi (socket-input socket))
			  (cl (http-response-cgi-content-length r))
			  (body (read-chars (elong->fixnum cl) pi))
			  (env (hop-cgi-env socket r cgibin "" 'POST))
			  (proc (apply run-process cgibin output: pipe: input: pipe: env)))
		      (fprint (process-input-port proc) body "\r\n")
		      (close-output-port (process-input-port proc))
		      ;; parse the cgi acknowledge
		      (http-parse-header (process-output-port proc)
					 (process-input-port proc))
		      ;; send the result of the request
		      (when bodyp
			 (send-chars (process-output-port proc) p)
			 (close-input-port (process-output-port proc)))
		      (flush-output-port p)
		      'close)))
	     (http-response (user-access-denied request) socket)))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-put ...                            */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-put socket)
   (with-trace 3 'http-response::http-response-put
      (with-access::http-response-put r (start-line header content-type server uri bodyp timeout request)
	 (let ((l (string-length uri)))
	    (let loop ((i 0))
	       (cond
		  ((=fx i l)
		   (http-response
		    (instantiate::http-response-string
		       (request request)
		       (start-line "HTTP/1.0 400 Bad Request")
		       (body (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html><body>Bad request ~a</body></html>" uri))
		       (timeout timeout))
		    socket))
		  ((char=? (string-ref uri i) #\?)
		   (let ((cmd (substring uri 0 i))
			 (args (substring uri (+fx i 1) l))
			 (p (socket-output socket)))
		      (when (>fx timeout 0)
			 (output-timeout-set! p timeout))
		      (http-write-line p start-line)
		      (http-write-header p header)
		      (http-write-line p "Connection: close")
		      (when content-type
			 (http-write-line p "Content-Type: " content-type))
		      (when server
			 (http-write-line p "Server: " server))
		      (http-write-line p)
		      ;; the body
		      (with-trace 4 'http-response-put-process
			 (let* ((pi (socket-input socket))
				(env (hop-cgi-env socket r cmd args 'GET))
				(proc (apply run-process cmd
					     output: pipe: input: pipe: env)))
			    (fprint (process-input-port proc) args "\r\n")
			    (close-output-port (process-input-port proc))
			    ;; parse the cgi acknowledge
			    (http-parse-header (process-output-port proc)
					       (process-input-port proc))
			    ;; send the result of the request
			    (when bodyp
			       (send-chars (process-output-port proc) p)
			       (close-input-port (process-output-port proc)))
			    (flush-output-port p)
			    'close))))
		  (else
		   (loop (+fx i 1)))))))))
    
;*---------------------------------------------------------------------*/
;*    http-response ::http-response-abort ...                          */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-abort socket)
   (with-trace 3 'http-response::http-response-abort
      'http-response-abort))
      
;*---------------------------------------------------------------------*/
;*    http-response ::http-response-filter ...                         */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-filter socket)
   (with-trace 3 'http-response::http-response-filter
      (http-filter (http-response-filter-response r) r socket)))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-persistent ...                     */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-persistent socket)
   'persistent)

;*---------------------------------------------------------------------*/
;*    request-encoding ...                                             */
;*---------------------------------------------------------------------*/
(define (request-encoding req)
   (or (http-request-char-encoding req) (hop-char-encoding)))

;*---------------------------------------------------------------------*/
;*    response-is-xml? ...                                             */
;*---------------------------------------------------------------------*/
(define (response-is-xml? p)
   (cond
      ((not (pair? p))
       (xml? p))
      ((xml? (car p))
       #t)
      ((pair? (car p))
       (and (list? p)
	    (or (response-is-xml? (car p)) (response-is-xml? (cdr p)))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    scheme->response ::obj ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (scheme->response obj::obj req)
   (cond
      ((string? obj)
       (instantiate::http-response-string
	  (char-encoding (request-encoding req))
	  (request req)
	  (bodyp (not (eq? (http-request-method req) 'HEAD)))
	  (body obj)))
      ((response-is-xml? obj)
       (instantiate::http-response-hop
	  (backend (hop-xml-backend))
	  (content-type (xml-backend-mime-type (hop-xml-backend)))
	  (char-encoding (request-encoding req))
	  (request req)
	  (bodyp (not (eq? (http-request-method req) 'HEAD)))
	  (xml obj)))
      (else
       (instantiate::http-response-js
	  (backend (hop-xml-backend))
	  (request req)
	  (value obj)))))

;*---------------------------------------------------------------------*/
;*    scheme->response ::%http-response ...                            */
;*---------------------------------------------------------------------*/
(define-method (scheme->response obj::%http-response req)
   obj)

;*---------------------------------------------------------------------*/
;*    scheme->response ::xml ...                                       */
;*---------------------------------------------------------------------*/
(define-method (scheme->response obj::xml req)
   (instantiate::http-response-hop
      (request req)
      (backend (hop-xml-backend))
      (content-type (xml-backend-mime-type (hop-xml-backend)))
      (char-encoding (request-encoding req))
      (bodyp (not (eq? (http-request-method req) 'HEAD)))
      (xml obj)))

;*---------------------------------------------------------------------*/
;*    http-response-void ...                                           */
;*---------------------------------------------------------------------*/
(define (http-response-void)
   (instantiate::http-response-string
      (request (instantiate::http-request))
      (start-line "HTTP/1.0 204 No Content")))

;*---------------------------------------------------------------------*/
;*    http-send-request ...                                            */
;*---------------------------------------------------------------------*/
(define (http-send-request req::http-request proc::procedure)
   (with-access::http-request req (scheme method path (httpv http) host port header socket userinfo authorization timeout)
      (let ((ssl (eq? scheme 'https)))
	 (let loop ((host host)
		    (port port)
		    (user userinfo)
		    (path path))
	    (let* ((sock (if (and (not ssl) (hop-proxy))
			     (make-proxy-socket (hop-proxy) timeout)
			     (make-client-socket/timeout host port
							 timeout req ssl)))
		   (out (socket-output sock))
		   (in (socket-input sock)))
	       (when (> timeout 0)
		  (output-port-timeout-set! out timeout)
		  (input-port-timeout-set! in timeout))
	       (http :in in :out out
		  :protocol scheme :method method :http-version httpv
		  :host host :port port :path path :header header
		  :authorization authorization
		  :login user
		  :body socket
		  :proxy (hop-proxy))
	       (with-handler
		  (lambda (e)
		     (if (&http-redirection? e)
			 (multiple-value-bind (rhost rport ruser rpath)
			    (redirect e)
			    (loop (or rhost host)
				  (or rport port)
				  (or ruser user)
				  (if rhost
				      (make-file-name (dirname path) rpath)
				      rpath)))
			 (raise e)))
		  (unwind-protect
		     (http-parse-response in out proc)
		     (socket-close sock))))))))
   
;*---------------------------------------------------------------------*/
;*    redirect ...                                                     */
;*---------------------------------------------------------------------*/
(define (redirect e)
   (let* ((url (&http-redirection-url e))
	  (ip (&http-redirection-port e)))
      (multiple-value-bind (rproto ruser rhost rport rpath)
	 (url-parse url)
	 (cond
	    ((not rproto)
	     (values #f #f #f rpath))
	    ((not (string? rhost))
	     (raise
	      (instantiate::&io-malformed-url-error
		 (proc 'http-send-request)
		 (msg "Illegal host")
		 (obj url))))
	    (else
	     (values rhost rport ruser rpath))))))

;*---------------------------------------------------------------------*/
;*    make-proxy-socket ...                                            */
;*---------------------------------------------------------------------*/
(define (make-proxy-socket proxy timeout)
   (let ((i (string-index proxy #\:)))
      (if (and (fixnum? i) (>fx i 0))
	  (let* ((proxy (substring proxy 0 i))
		 (len (string-length proxy))
		 (port (string->integer (substring proxy (+fx i 1) len))))
	     (make-client-socket proxy port :timeout timeout))
	  (make-client-socket proxy 80 :timeout timeout))))
