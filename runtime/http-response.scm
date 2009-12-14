;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/http-response.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 25 14:15:42 2004                          */
;*    Last change :  Mon Dec 14 05:27:39 2009 (serrano)                */
;*    Copyright   :  2004-09 Manuel Serrano                            */
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
	    __hop_charset
	    __hop_hop-extra
	    __hop_http-lib
	    __hop_http-error
	    __hop_http-filter
	    __hop_js-lib
	    __hop_user
	    __hop_cache
	    __hop_security)

   (export  (generic http-response::symbol ::%http-response ::socket)
	    (generic scheme->response ::obj ::http-request)
	    (http-response-void ::http-request)
	    (http-send-request ::http-request ::procedure)))

;*---------------------------------------------------------------------*/
;*    http-response ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (http-response::symbol r::%http-response socket))

;*---------------------------------------------------------------------*/
;*    http-write-content-type ...                                      */
;*---------------------------------------------------------------------*/
(define (http-write-content-type p content-type charset)
   (let ((ctype (or content-type (hop-default-mime-type))))
      (if charset
	  (http-write-line p "Content-type: " ctype "; charset=" charset)
	  (http-write-line p "Content-type: " ctype))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-authentication ...                 */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-authentication socket)
   (with-trace 3 'http-response::http-response-authentication
      (with-access::http-response-authentication r (header content-type body server timeout request start-line)
	 (let ((p (socket-output socket))
	       (connection (http-request-connection request)))
	    (when (>=fx timeout 0) (output-timeout-set! p timeout))
	    (http-write-line-string p start-line)
	    (http-write-header p header)
	    (http-write-line p "Connection: " connection)
	    (http-write-content-type p content-type #f)
	    (when server
	       (http-write-line-string p "Server: " server))
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
      (with-access::http-response-string r (start-line
					    header
					    content-type charset
					    server content-length
					    bodyp body
					    timeout request)
	 (let* ((p (socket-output socket))
		(s body)
		(connection (http-request-connection request))
		(l (string-length s))
		(clen (if (=elong content-length #e-1)
			  l
			  content-length)))
	    (when (>=fx timeout 0) (output-timeout-set! p timeout))
	    (http-write-line-string p start-line)
	    (http-write-header p header)
	    (if (>= clen 0)
		(http-write-line p "Content-Length: " clen)
		(set! connection 'close))
	    (http-write-line p "Connection: " connection)
	    (http-write-content-type p content-type charset)
	    (when server (http-write-line-string p "Server: " server))
	    (http-write-line p)
	    (when bodyp (display s p))
	    (flush-output-port p)
	    connection))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-js ...                             */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-js socket)
   (with-trace 3 'http-response::http-response-js
      (with-access::http-response-js r (start-line
					header
					content-type charset
					server content-length value
					bodyp timeout request)
	 (let ((p (socket-output socket))
	       (connection (http-request-connection request)))
	    (when (>=fx timeout 0) (output-timeout-set! p timeout))
	    (http-write-line-string p start-line)
	    (http-write-header p header)
	    (if (>elong content-length #e0)
		(http-write-line p "Content-Length: " content-length)
		(set! connection 'close))
	    (http-write-line p "Connection: " connection)
	    (http-write-content-type p (or content-type (hop-json-mime-type)) charset)
	    (when server
	       (http-write-line-string p "Server: " server))
	    (http-write-line p)
	    ;; the body
	    (with-trace 4 'http-response-js
	       (when bodyp (display (hop->json value #t #f) p)))
	    (flush-output-port p)
	    connection))))

;*---------------------------------------------------------------------*/
;*    chunked-flush-hook ...                                           */
;*---------------------------------------------------------------------*/
(define (chunked-flush-hook port size)
   (let ((buf (output-port-flush-buffer port)))
      (if (string? buf)
	  (let ((letter "0123456789abcdef"))
	     (string-set! buf 0 #\return)
	     (string-set! buf 1 #\newline)
	     (let loop ((size size)
			(i 2))
		(if (>fx size 0)
		    (let ((d (string-ref letter (remainderfx size 16))))
		       (string-set! buf i d)
		       (loop (/fx size 16) (+fx i 1)))
		    ;; swap the string
		    (let loop ((j 2)
			       (k (-fx i 1)))
		       (if (>=fx j k)
			   (begin
			      (string-set! buf i #\return)
			      (string-set! buf (+fx i 1) #\newline)
			      (+fx i 2))
			   (let ((c0 (string-ref buf j))
				 (c1 (string-ref buf k)))
			      (string-set! buf j c1)
			      (string-set! buf k c0)
			      (loop (+fx j 1) (-fx k 1))))))))
	  (string-append "\r\n" (integer->string size 16) "\r\n"))))
   
;*---------------------------------------------------------------------*/
;*    http-response ::http-response-hop ...                            */
;*    -------------------------------------------------------------    */
;*    Since Bigloo3.1c hop response are transmitted as chunked         */
;*    response to allow HOP to use keep-alive connections              */
;*    for dynamic responses.                                           */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-hop socket)
   (with-trace 3 'http-response::http-response-hop
      (if (<fx (hop-security) 2)
	  (http-response-hop-unsecure r socket)
	  (http-response-hop-secure r socket))))

;*---------------------------------------------------------------------*/
;*    http-response-hop-unsecure ...                                   */
;*---------------------------------------------------------------------*/
(define (http-response-hop-unsecure r socket)
   (with-trace 3 'http-response-hop-unsecure
      (with-access::http-response-hop r (request
					 start-line header
					 content-type charset server backend
					 content-length
					 xml bodyp timeout)
	 (let ((connection (http-request-connection request))
	       (p (socket-output socket))
	       (clen content-length)
	       (chunked (eq? (http-request-http request) 'HTTP/1.1)))
	    (output-timeout-set! p timeout)
	    (http-write-line-string p start-line)
	    (http-write-header p header)
	    (cond
	       ((>elong clen #e0)
		(http-write-line p "Content-Length: " clen)
		(set! chunked #f))
	       ((not chunked)
		(set! connection 'close))
	       ((eq? connection 'close)
		(set! chunked #f))
	       (else
		(http-write-line p "Transfer-Encoding: chunked")))
	    (http-write-line p "Connection: " connection)
	    (let ((ctype (or content-type (xml-backend-mime-type backend))))
	       (http-write-content-type p ctype charset))
	    (when server
	       (http-write-line-string p "Server: " server))
	    (http-write-line-string p "Hhop: true")
	    (if chunked
		(begin
		   (flush-output-port p)
		   (output-port-flush-hook-set! p chunked-flush-hook))
		(http-write-line p))
	    ;; the body
	    (with-trace 4 'http-response-hop
	       (when bodyp
		  (xml-write xml p backend)))
	    (flush-output-port p)
	    ;; for chunked, write the last 0 chunk
	    (when chunked
	       (output-port-flush-hook-set! p #unspecified)
	       (http-write-line p "\r\n0\r\n")
	       (flush-output-port p))
	    connection))))

;*---------------------------------------------------------------------*/
;*    http-response-hop-secure ...                                     */
;*---------------------------------------------------------------------*/
(define (http-response-hop-secure r socket)
   (with-trace 3 'http-response-hop-unsecure
      (with-access::http-response-hop r (request
					 start-line header
					 content-type charset server backend
					 content-length
					 xml bodyp timeout)
	 (let ((connection (http-request-connection request))
	       (p (socket-output socket))
	       (str (security-manager r)))
	    (output-timeout-set! p timeout)
	    (http-write-line-string p start-line)
	    (http-write-header p header)
	    (http-write-line p "Content-Length: " (string-length str))
	    (http-write-line p "Connection: " connection)
	    (let ((ctype (or content-type (xml-backend-mime-type backend))))
	       (http-write-content-type p ctype charset))
	    (when server
	       (http-write-line-string p "Server: " server))
	    (http-write-line-string p "Hhop: true")
	    (http-write-line p "hop-security: " (hop-security))
	    (http-write-line p)
	    ;; the body
	    (with-trace 4 'http-response-hop
	       (when bodyp
		  (display str p)))
	    (flush-output-port p)
	    connection))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-procedure ...                      */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-procedure socket)
   (with-trace 3 'http-response::http-response-procedure
      (with-access::http-response-procedure r (start-line header content-type charset server content-length proc bodyp timeout request)
	 (let ((p (socket-output socket))
	       (connection (if (>elong content-length #e0)
			       (http-request-connection request)
			       'close)))
	    (when (>=fx timeout 0) (output-timeout-set! p timeout))
	    (http-write-line-string p start-line)
	    (http-write-header p header)
	    (if (>elong content-length #e0)
		(http-write-line p "Content-Length: " content-length)
		(set! connection 'close))
	    (http-write-line p "Connection: " connection)
	    (http-write-content-type p content-type charset)
	    (when server
	       (http-write-line-string p "Server: " server))
	    (http-write-line p)
	    (flush-output-port p)
	    ;; the body
	    (with-trace 4 'http-response-procedure
	       (when bodyp
		  (proc p)
		  (flush-output-port p)))
	    connection))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-raw ...                            */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-raw socket)
   (with-trace 3 'http-response::http-response-raw
      (with-access::http-response-raw r (bodyp proc timeout request connection)
	 (let ((p (socket-output socket)))
	    (when (>=fx timeout 0) (output-timeout-set! p timeout))
	    (when bodyp
	       (proc p)
	       (flush-output-port p))
	    (if (symbol? connection)
		connection
		(http-request-connection request))))))

;*---------------------------------------------------------------------*/
;*    http-response-range ...                                          */
;*---------------------------------------------------------------------*/
(define (http-response-range range r::http-response-file socket::socket)
   
   (define (response-error status-line request socket header)
      (http-response
       (instantiate::http-response-error
	  (request request)
	  (start-line status-line)
	  (charset (hop-locale))
	  (header header))
       socket))

   (define (response-header conn beg end size p r)
      (with-access::http-response-file r (start-line header content-type charset server request)
	 ;; partial content header
	 (display (http-request-http request) p)
	 (http-write-line-string p " 206 Partial Content")
	 (http-write-header p header)
	 (http-write-line p "Connection: " conn)
	 (http-write-content-type p content-type charset)
	 (when server (http-write-line-string p "Server: " server))
	 (let ((dt (date->rfc2822-date (current-date))))
	    (http-write-line p "Date: " dt)
	    (http-write-line p "Last-Modified: " dt))
	 ;; content-length
	 (display "Content-Length: " p)
	 (display-elong (+elong #e1 (-elong end beg)) p)
	 (http-write-line p)
	 ;; content-range
	 (display "Content-Range: bytes " p)
	 (display-elong beg p)
	 (display "-" p)
	 (display-elong end p)
	 (display "/" p)
	 (display-elong size p)
	 (http-write-line p)
	 ;; close the header
	 (http-write-line p)))
   
   (with-access::http-response-file r (start-line file bodyp request timeout)
      (let ((size (file-size file)))
	 (if (>=elong size #e0)
	     (with-handler
		(lambda (e)
		   (exception-notify e)
		   (response-error "HTTP/1.0 400 Bad Request" request socket '()))
		(multiple-value-bind (beg end)
		   (http-parse-range range)
		   (let* ((connection (http-request-connection request))
			  (p (socket-output socket)))
		      ;; fill the default arguments
		      (unless end (set! end (-elong size #e1)))
		      (unless beg (set! beg (-elong size end)))
		      (when (>=elong end size)
			 (set! end (-elong size 1)))
		      ;; check the correctness
		      (unless (and (>=elong beg 0) (<=elong beg end))
			 (response-error
			  "HTTP/1.0 416 Requested range not satisfiable"
			  request
			  socket
			  '((content-range: . "*"))))
		      (output-timeout-set! p timeout)
		      ;; the header
		      (response-header connection beg end size p r)
		      ;; capture dumping
		      (let ((cp (hop-capture-port)))
			 (when (output-port? cp)
			    (display "----------------------------------------------------------\n" cp)
			    (fprintf cp "http://~a:~a~a\n\n"
				     (http-request-host request)
				     (http-request-port request)
				     (http-request-path request))
			    (response-header connection beg end size cp r)
			    (flush-output-port cp)))
		      ;; the body
		      (with-trace 4 'http-response-file
			 (when bodyp
			    (send-file file p (+elong #e1 (-elong end beg)) beg)
			    (flush-output-port p)))
		      connection)))
	     (http-response (http-file-not-found file) socket)))))

;*---------------------------------------------------------------------*/
;*    http-response-regular-file ...                                   */
;*---------------------------------------------------------------------*/
(define (http-response-regular-file r::http-response-file socket::socket)
   (with-access::http-response-file r (start-line header content-type charset server file bodyp request timeout)
      (let ((size (file-size file)))
	 (if (>=elong size #e0)
	     (let ((connection (http-request-connection request))
		   (p (socket-output socket)))
		(output-timeout-set! p timeout)
		(http-write-line-string p start-line)
		(http-write-header p header)
		(http-write-line p "Connection: " connection)
		(http-write-content-type p content-type charset)
		(when server (http-write-line-string p "Server: " server))
		(unless (eq? connection 'close)
		   (display "Content-Length: " p)
		   (display-elong size p)
		   (http-write-line p))
		(http-write-line p)
		;; the body
		(with-trace 4 'http-response-file
		   (when bodyp (send-file file p size #e-1)))
		(flush-output-port p)
		connection)
	     (http-response (http-file-not-found file) socket)))))

;*---------------------------------------------------------------------*/
;*    directory->response ...                                          */
;*---------------------------------------------------------------------*/
(define (directory->response rep dir)
   (instantiate::http-response-hop
      (backend (hop-xml-backend))
      (content-type (xml-backend-mime-type (hop-xml-backend)))
      (charset (hop-charset))
      (request (%http-response-request rep))
      (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
      (xml (if (%http-response-bodyp rep)
	       (<HTML>
		  (<HEAD> (<TITLE> dir))
		  (<BODY>
		     (<H1> dir)
		     (let ((cvt (charset-converter (hop-locale) (hop-charset))))
			(<PRE> (map (lambda (f)
				       (let* ((fe (cvt f))
					      (path (make-file-name dir fe)))
					  (<A> :href path
					     (if (directory? path)
						 (string-append f "/\n")
						 (string-append f "\n")))))
				    (sort (directory->list dir) string<?))))))
	       '()))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-file ...                           */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-file socket)
   (with-trace 3 'http-response::http-response-file
      (with-access::http-response-file r (start-line header file request)
	 (if (authorized-path? request file)
	     (cond
		((directory? file)
		 (http-response (directory->response r file) socket))
		((http-header-field (http-request-header request) range:)
		 =>
		 (lambda (range)
		    (http-response-range range r socket)))
		(else
		 (http-response-regular-file r socket)))
	     (http-response (user-access-denied request) socket)))))

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
      (with-access::http-response-cgi r (start-line header content-type charset server cgibin bodyp request timeout)
	 (if (authorized-path? request cgibin)
	     (let ((p (socket-output socket)))
		(when (>=fx timeout 0) (output-timeout-set! p timeout))
		(http-write-line-string p start-line)
		(http-write-header p header)
		(http-write-line p "Connection: close")
		(http-write-content-type p content-type charset)
		(when server
		   (http-write-line-string p "Server: " server))
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
      (with-access::http-response-put r (start-line header content-type charset server uri bodyp timeout request)
	 (let ((l (string-length uri)))
	    (let loop ((i 0))
	       (cond
		  ((=fx i l)
		   (http-response
		    (instantiate::http-response-string
		       (request request)
		       (start-line "HTTP/1.0 400 Bad Request")
		       (charset (hop-locale))
		       (body (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html><body>Bad request ~a</body></html>" uri))
		       (timeout timeout))
		    socket))
		  ((char=? (string-ref uri i) #\?)
		   (let ((cmd (substring uri 0 i))
			 (args (substring uri (+fx i 1) l))
			 (p (socket-output socket)))
		      (when (>=fx timeout 0) (output-timeout-set! p timeout))
		      (http-write-line-string p start-line)
		      (http-write-header p header)
		      (http-write-line p "Connection: close")
		      (http-write-content-type p content-type charset)
		      (when server
			 (http-write-line-string p "Server: " server))
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
   (with-trace 3 'http-response::http-response-persistent
      (with-access::http-response-persistent r (body)
	 (when (string? body)
	    (let ((p (socket-output socket)))
	       (display body p)
	       (flush-output-port p))))
      'persistent))

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
	  (charset (hop-charset))
	  (request req)
	  (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
	  (bodyp (not (eq? (http-request-method req) 'HEAD)))
	  (body obj)))
      (else
       (instantiate::http-response-js
	  (backend (hop-xml-backend))
	  (content-type (hop-json-mime-type))
	  (charset (hop-charset))
	  (request req)
	  (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
	  (bodyp (not (eq? (http-request-method req) 'HEAD)))
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
      (charset (hop-charset))
      (bodyp (not (eq? (http-request-method req) 'HEAD)))
      (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
      (xml obj)))

;*---------------------------------------------------------------------*/
;*    scheme->response ::xml-tilde ...                                 */
;*---------------------------------------------------------------------*/
(define-method (scheme->response obj::xml-tilde req)
   (instantiate::http-response-string
      (content-type (hop-json-mime-type))
      (charset (hop-charset))
      (request req)
      (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
      (bodyp (not (eq? (http-request-method req) 'HEAD)))
      (body (xml-tilde->expression obj))))

;*---------------------------------------------------------------------*/
;*    scheme->response ::xml-http-request ...                          */
;*    -------------------------------------------------------------    */
;*    This is used to forward the response of a remote WITH-HOP        */
;*    to clients.                                                      */
;*---------------------------------------------------------------------*/
(define-method (scheme->response obj::xml-http-request req)
   (instantiate::http-response-procedure
      (request req)
      (start-line (format "HTTP/1.1 ~a" (xml-http-request-status obj)))
      (header (xml-http-request-header obj))
      (proc (lambda (op)
	       (display (read-string (xml-http-request-input-port obj)) op)))))
   
;*---------------------------------------------------------------------*/
;*    http-response-void ...                                           */
;*---------------------------------------------------------------------*/
(define (http-response-void req)
   (instantiate::http-response-string
      (request req)
      (charset (hop-locale))
      (start-line "HTTP/1.0 204 No Content")))

;*---------------------------------------------------------------------*/
;*    http-send-request ...                                            */
;*---------------------------------------------------------------------*/
(define (http-send-request req::http-request proc::procedure)
   (with-trace 3 'http-send-request
      (with-access::http-request req (scheme method path (httpv http) host port header socket userinfo timeout)
	 (let ((ssl (eq? scheme 'https)))
	    (let loop ((host host)
		       (port port)
		       (user userinfo)
		       (path path))
	       (trace-item "scheme=" scheme " host=" host " port=" port)
	       (trace-item "method=" method " path=" path)
	       (let* ((sock (if (and (not ssl) (hop-use-proxy))
				(make-proxy-socket (hop-use-proxy) timeout)
				(make-client-socket/timeout host port
							    timeout req ssl)))
		      (out (socket-output sock))
		      (in (socket-input sock)))
		  (when (> timeout 0)
		     (output-port-timeout-set! out timeout)
		     (input-port-timeout-set! in timeout))
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
		     (http :in in :out out
			:protocol scheme :method method :http-version httpv
			:host host :port port :path path :header header
			:authorization (if (and (http-server-request? req)
						(http-server-request-authorization req))
					   (http-server-request-authorization req)
					   (let ((auth (assq :authorization header)))
					      (when (pair? auth) (cdr auth))))
			:timeout timeout
			:login user
			:body socket
			:proxy (hop-use-proxy))
		     (unwind-protect
			(http-parse-response in out proc)
			(socket-close sock)))))))))
   
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
