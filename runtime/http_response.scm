;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/runtime/http_response.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 25 14:15:42 2004                          */
;*    Last change :  Sat Jun 17 16:15:28 2017 (serrano)                */
;*    Copyright   :  2004-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HTTP response                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_http-response

   (library web)
   
   (include "http_lib.sch")
   
   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_misc
	    __hop_xml-types
	    __hop_xml
	    __hop_html-base
	    __hop_html-head
	    __hop_charset
	    __hop_http-lib
	    __hop_http-error
	    __hop_http-filter
	    __hop_js-comp
	    __hop_json
	    __hop_user
	    __hop_cache
	    __hop_security)

   (export  (generic http-response::symbol ::%http-response ::obj ::socket)
	    (generic scheme->response ::obj ::http-request)
	    (http-send-request ::http-request ::procedure #!key body args)
	    (chunked-flush-hook port size)))

;*---------------------------------------------------------------------*/
;*    http-response ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (http-response::symbol r::%http-response request socket))

;*---------------------------------------------------------------------*/
;*    http-write-content-type ...                                      */
;*---------------------------------------------------------------------*/
(define (http-write-content-type p content-type charset)
   (let ((ctype (or content-type "text/plain")))
      (if charset
	  (http-write-line p "Content-type: " ctype "; charset=" charset)
	  (http-write-line p "Content-type: " ctype))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-string ...                         */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-string request socket)
   (with-trace 'hop-response "http-response::http-response-string"
      (with-access::http-response-string r (start-line
					    header
					    content-type charset
					    server content-length
					    bodyp body
					    timeout)
	 (let* ((p (socket-output socket))
		(body body)
		(connection (with-access::http-request request (connection)
			       connection))
		(clen (if (=elong content-length #e-1)
			  (string-length body)
			  content-length)))
	    (when (>=fx timeout 0) (output-timeout-set! p timeout))
	    (http-write-line-string p start-line)
	    (http-write-header p header)
	    (if (>= clen 0)
		(http-write-line p "Content-Length: " clen)
		(set! connection 'close))
	    (http-write-line p "Connection: " connection)
	    (http-write-content-type p content-type charset)
	    (http-write-line-string p "Server: " server)
	    (http-write-line p)
	    (when bodyp (display body p))
	    (flush-output-port p)
	    connection))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-hop ...                            */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-hop request socket)

   (define (serialize val)
      (obj->string val (request-type request)))
   
   (define (arraybuffer-request? request)
      (with-access::http-request request (header)
	 (let ((c (assq hop-responsetype: header)))
	    (and (pair? c) (string=? (cdr c) "arraybuffer")))))
   
   (define (request-type request)
      (with-access::http-request request (header)
	 (let ((c (assq hop-client: header)))
	    (if (and (pair? c) (string=? (cdr c) "hop"))
		'hop-to-hop
		'hop-client))))
   
   (define (response-x-hop value conn p)
      (with-access::http-request request (connection)
	 (let* ((str (serialize value))
		(rep (if (arraybuffer-request? request)
			 str
			 (string-hex-extern str))))
	    (http-write-line p "Content-Length: " (string-length rep))
	    (http-write-line p "Connection: " conn)
	    (http-write-line p)
	    (display-string rep p))))

   (define (response-x-javascript value p padding)
      (http-write-line p "Connection: close")
      (http-write-line p)
      (when padding (display padding p))
      (display "(" p)
      (obj->javascript-expr value p)
      (display ")" p))

   (define (response-x-url-hop value conn p)
      (let ((s (url-path-encode (serialize value))))
	 (http-write-line p "Content-Length: " (string-length s))
	 (http-write-line p "Connection: " conn)
	 (http-write-line p)
	 (display s p)))

   (define (response-x-json-hop value p)
      (http-write-line p "Connection: close")
      (http-write-line p)
      (byte-array->json (serialize value) p))
   
   (define (response-json value p padding)
      (http-write-line p "Connection: close")
      (http-write-line p)
      (if padding
	  (begin
	     (display padding p)
	     (display "(" p)
	     (obj->json value p)
	     (display ")" p))
	  (obj->json value p)))

   (with-trace 'hop-response "http-response::http-response-hop"
      (with-access::http-response-hop r (start-line
					   header
					   (ctype content-type)
					   charset
					   server content-length
					   value padding
					   bodyp 
					   timeout)
	 (with-access::http-request request (connection (headerreq header))
	    (let ((p (socket-output socket))
		  (conn connection))
	       (when (>=fx timeout 0) (output-timeout-set! p timeout))
	       (http-write-line-string p start-line)
	       (http-write-header p header)
	       (http-write-line p "Cache-Control: no-cache")
	       (http-write-content-type p ctype charset)
	       (http-write-line-string p "Server: " server)
	       (when bodyp
		  ;; select the transfer method according to the content-type
		  (cond
		     ((not (string? ctype))
		      (error "http-response"
			 "No serialization method specified"
			 ctype))
		     ((string-prefix? "application/x-hop" ctype)
		      ;; fast path, bigloo serialization
		      (response-x-hop value conn p))
		     ((string-prefix? "application/x-javascript" ctype)
		      ;; standard javascript serialization
		      (set! conn 'close)
		      (response-x-javascript value p padding))
		     ((string-prefix? "application/x-url-hop" ctype)
		      ;; fast path, bigloo serialization
		      (response-x-url-hop value conn p))
		     ((string-prefix? "application/x-json-hop" ctype)
		      (set! conn 'close)
		      (response-x-json-hop value p))
		     ((string-prefix? "application/json" ctype)
		      ;; json encoding
		      (response-json value p padding))
		     (else
		      (error "http-response"
			 (format "Unsupported serialization method \"~a\""
			    ctype)
			 request))))
	       (flush-output-port p)
	       conn)))))

;*---------------------------------------------------------------------*/
;*    chunked-flush-hook ...                                           */
;*---------------------------------------------------------------------*/
(define (chunked-flush-hook port size)
   (when (>fx size 0)
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
	     (string-append "\r\n" (integer->string size 16) "\r\n")))))
   
;*---------------------------------------------------------------------*/
;*    http-response ::http-response-xml ...                            */
;*    -------------------------------------------------------------    */
;*    Since Bigloo3.1c hop responses are transmitted as chunked        */
;*    responses which allows Hop to use keep-alive connections for     */
;*    dynamic responses.                                               */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-xml request socket)
   (with-trace 'hop-response "http-response-xml"
      (with-access::http-response-xml r (start-line header
					 content-type charset server
					 xml bodyp timeout
					 backend)
	 (let ((connection (with-access::http-request request (connection)
			      connection))
	       (p (socket-output socket))
	       (chunked (eq? (with-access::http-request request (http) http)
			   'HTTP/1.1)))
	    (output-timeout-set! p timeout)
	    (http-write-line-string p start-line)
	    (http-write-header p header)
	    (cond
	       ((not chunked)
		(set! connection 'close))
	       ((eq? connection 'close)
		(set! chunked #f))
	       (else
		(http-write-line p "Transfer-Encoding: chunked")))
	    (http-write-line p "Connection: " connection)
	    (with-access::xml-backend backend (mime-type)
	       (let ((ctype (or content-type mime-type)))
		  (http-write-content-type p ctype charset)))
	    (http-write-line-string p "Server: " server)
	    (if chunked
		(begin
		   (flush-output-port p)
		   (output-port-flush-hook-set! p chunked-flush-hook))
		(http-write-line p))
	    ;; the body
	    (with-trace 'hop-respponse "http-response-xml"
	       (when bodyp
		  (with-access::xml-backend backend (security)
		     (if (isa? security security-manager)
			 (with-access::security-manager security (xml-sanitize)
			    (let ((xmls (xml-sanitize xml backend request)))
			       (xml-write xmls p backend)))
			 (xml-write xml p backend)))))
	    (flush-output-port p)
	    ;; for chunked, write the last 0 chunk
	    (when chunked
	       (output-port-flush-hook-set! p #unspecified)
	       (http-write-line p "\r\n0\r\n")
	       (flush-output-port p))
	    connection))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-procedure ...                      */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-procedure request socket)
   (with-trace 'hop-response "http-response::http-response-procedure"
      (with-access::http-response-procedure r (start-line header content-type
						 charset server content-length
						 proc bodyp
						 timeout)
	 (let ((p (socket-output socket))
	       (connection (if (>elong content-length #e0)
			       (with-access::http-request request (connection)
				  connection)
			       'close)))
	    (when (>=fx timeout 0) (output-timeout-set! p timeout))
	    (http-write-line-string p start-line)
	    (http-write-header p header)
	    (if (>elong content-length #e0)
		(http-write-line p "Content-Length: " content-length)
		(set! connection 'close))
	    (http-write-line p "Connection: " connection)
	    (http-write-content-type p content-type charset)
	    (http-write-line-string p "Server: " server)
	    (http-write-line p)
	    (flush-output-port p)
	    ;; the body
	    (with-trace 'hop-response "http-response-procedure"
	       (when bodyp
		  (proc p)
		  (flush-output-port p)))
	    connection))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-raw ...                            */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-raw request socket)
   (with-trace 'hop-response "http-response::http-response-raw"
      (with-access::http-response-raw r (bodyp proc timeout connection)
	 (let ((p (socket-output socket)))
	    (when (>=fx timeout 0) (output-timeout-set! p timeout))
	    (when bodyp
	       (proc p)
	       (flush-output-port p))
	    (if (symbol? connection)
		connection
		(with-access::http-request request (connection)
		   connection))))))

;*---------------------------------------------------------------------*/
;*    http-response-range ...                                          */
;*---------------------------------------------------------------------*/
(define (http-response-range range r::http-response-file request socket::socket)
   
   (define (response-error status-line request socket header)
      (http-response
       (instantiate::http-response-string
	  (start-line status-line)
	  (charset (hop-locale))
	  (header header))
       request
       socket))

   (define (response-header conn beg end size p r file)
      (with-access::http-response-file r (start-line header
					    content-type charset
					    server)
	 (with-access::http-request request (http)
	    ;; partial content header
	    (display http p)
	    (http-write-line-string p " 206 Partial Content")
	    (http-write-header p header)
	    (http-write-line p "Connection: " conn)
	    (http-write-content-type p content-type charset)
	    (http-write-line-string p "Server: " server)
	    (let ((dtc (date->rfc2822-date (current-date))))
	       ;; don't need to explicitly display the last-modified
	       ;; header entry because it is contained in the
	       ;; pre-computed header response
	       (http-write-line p "Date: " dtc))
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
	    (http-write-line p))))
   
   (with-access::http-response-file r (start-line file bodyp timeout)
      (let ((size (file-size file)))
	 (if (>=elong size #e0)
	     (with-handler
		(lambda (e)
		   (exception-notify e)
		   (response-error "HTTP/1.0 400 Bad Request" request socket '()))
		(multiple-value-bind (beg end)
		   (http-parse-range range)
		   (with-access::http-request request (connection host port path)
		      (let ((p (socket-output socket)))
			 ;; fill the default arguments
			 (unless end (set! end (-elong size #e1)))
			 (unless beg (set! beg (-elong size end)))
			 (when (>elong end size) (set! end (-elong size 1)))
			 ;; check the correctness
			 (unless (and (>=elong beg 0) (<=elong beg end))
			    (response-error
			       "HTTP/1.0 416 Requested range not satisfiable"
			       request
			       socket
			       '((content-range: . "*"))))
			 (output-timeout-set! p timeout)
			 ;; the header
			 (response-header connection beg end size p r file)
			 ;; capture dumping
			 (let ((cp (hop-capture-port)))
			    (when (output-port? cp)
			       (display "----------------------------------------------------------\n" cp)
			       (fprintf cp "http://~a:~a~a\n\n" host port path)
			       (response-header connection beg end size cp r file)
			       (flush-output-port cp)))
			 ;; the body
			 (with-trace 'hop-response "http-response-file"
			    (when bodyp
			       (send-file file p (+elong #e1 (-elong end beg)) beg)
			       (flush-output-port p)))
			 connection))))
	     (http-response (http-file-not-found file) request socket)))))

;*---------------------------------------------------------------------*/
;*    http-response-regular-file ...                                   */
;*---------------------------------------------------------------------*/
(define (http-response-regular-file r::http-response-file request socket::socket)
   (with-access::http-response-file r (start-line header
					 content-type charset
					 server file bodyp
					 size offset
					 timeout)
      (let ((size (if (=elong size #e-1)
		      (let ((fs (file-size file)))
			 (if (<=elong offset 0)
			     fs
			     (-elong fs offset))))))
	 (if (>=elong size #e0)
	     (with-access::http-request request (connection)
		(let ((p (socket-output socket)))
		   (output-timeout-set! p timeout)
		   (http-write-line-string p start-line)
		   (http-write-header p header)
		   (http-write-line p "Connection: " connection)
		   (http-write-content-type p content-type charset)
		   (http-write-line-string p "Server: " server)
		   (unless (eq? connection 'close)
		      (display "Content-Length: " p)
		      (display-elong size p)
		      (http-write-line p))
		   (http-write-line p)
		   (flush-output-port p)
		   ;; the body
		   (when bodyp (send-file file p size offset))
		   (flush-output-port p)
		   connection))
	     (http-response (http-file-not-found file) request socket)))))

;*---------------------------------------------------------------------*/
;*    directory->response ...                                          */
;*---------------------------------------------------------------------*/
(define (directory->response rep dir)
   (with-access::%http-response rep (bodyp)
      (instantiate::http-response-xml
	 (backend (hop-xml-backend))
	 (content-type (with-access::xml-backend (hop-xml-backend) (mime-type) mime-type))
	 (charset (hop-charset))
	 (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
	 (xml (if bodyp
		  (<HTML>
		     (<HEAD> (<TITLE> dir))
		     (<BODY>
			(<H1> dir)
			(let ((cvt (charset-converter
				      (hop-locale) (hop-charset))))
			   (<PRE>
			      (unless (string=? dir "/")
				 (<A> :href
				    (if (char=? (string-ref dir (-fx (string-length dir) 1)) (file-separator))
					(string-append (dirname (substring dir 0 (-fx (string-length dir) 1)))
					   "/")
					(dirname dir))
				    "..\n"))
			      (map! (lambda (f)
				       (let* ((fe (cvt f))
					      (path (make-file-name dir fe)))
					  (if (directory? path)
					      (<A> :href (string-append path "/")
						 (string-append f "/\n"))
					      (<A> :href path
						 (string-append f "\n")))))
				 (sort (directory->list dir) string<?))))))
		  '())))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-file ...                           */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-file request socket)
   (with-trace 'hop-response "http-response::http-response-file"
      (with-access::http-response-file r (start-line header file)
	 (with-access::http-request request (header)
	    (cond
	       ((directory? file)
		(http-response (directory->response r file) request socket))
	       ((http-header-field header range:)
		=>
		(lambda (range) (http-response-range range r request socket)))
	       (else
		(http-response-regular-file r request socket)))))))

;*---------------------------------------------------------------------*/
;*    hop-cgi-env ...                                                  */
;*---------------------------------------------------------------------*/
(define (hop-cgi-env socket r::%http-response request::http-request
	      path::bstring  qstr method)
   (with-access::%http-response r (header content-type)
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
			     (if (isa? r http-response-cgi)
				 (with-access::http-response-cgi r (content-length)
				    content-length)
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
		env: (format "SERVER_NAME=~a" (with-access::http-request request (host) host))
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
(define-method (http-response r::http-response-cgi request socket)
   (with-trace 'hop-response "http-response::http-response-cgi"
      (with-access::http-response-cgi r (start-line header
					   content-type charset
					   server cgibin
					   bodyp
					   timeout content-length)
	 (if (authorized-path? request cgibin)
	     (let ((p (socket-output socket)))
		(when (>=fx timeout 0) (output-timeout-set! p timeout))
		(http-write-line-string p start-line)
		(http-write-header p header)
		(http-write-line p "Connection: close")
		(http-write-content-type p content-type charset)
		(http-write-line-string p "Server: " server)
		(http-write-line p)
		;; the body
		(with-trace 'hop-response "http-response-cgi-process"
		   (let* ((pi (socket-input socket))
			  (cl content-length)
			  (body (read-chars (elong->fixnum cl) pi))
			  (env (hop-cgi-env socket r request cgibin "" 'POST))
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
	     (http-response (access-denied request) request socket)))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-put ...                            */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-put request socket)
   (with-trace 'hop-response "http-response::http-response-put"
      (with-access::http-response-put r (start-line header
					   content-type charset
					   server uri bodyp
					   timeout)
	 (let ((l (string-length uri)))
	    (let loop ((i 0))
	       (cond
		  ((=fx i l)
		   (http-response
		    (instantiate::http-response-string
		       (start-line "HTTP/1.0 400 Bad Request")
		       (charset (hop-locale))
		       (body (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html><body>Bad request ~a</body></html>" uri))
		       (timeout timeout))
		    request
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
		      (http-write-line-string p "Server: " server)
		      (http-write-line p)
		      ;; the body
		      (with-trace 'hop-response "http-response-put-process"
			 (let* ((pi (socket-input socket))
				(env (hop-cgi-env socket r request cmd args 'GET))
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
(define-method (http-response r::http-response-abort request socket)
   (with-trace 'hop-response "http-response::http-response-abort"
      'abort))
      
;*---------------------------------------------------------------------*/
;*    http-response ::http-response-filter ...                         */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-filter request socket)
   (with-trace 'hop-response "http-response::http-response-filter"
      (with-access::http-response-filter r (response)
	 (http-filter response r request socket))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-persistent ...                     */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-persistent request socket)
   (with-trace 'hop-response "http-response::http-response-persistent"
      (with-access::http-response-persistent r (body)
	 (when (string? body)
	    (let ((p (socket-output socket)))
	       (display body p)
	       (flush-output-port p)))
	 'persistent)))

;*---------------------------------------------------------------------*/
;*    response-is-xml? ...                                             */
;*---------------------------------------------------------------------*/
(define (response-is-xml? p)
   (cond
      ((not (pair? p))
       (isa? p xml))
      ((isa? (car p) xml)
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
   (with-access::http-request req (method header)
      (cond
	 ((string? obj)
	  (instantiate::http-response-string
	     (charset (hop-charset))
	     (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
	     (bodyp (not (eq? method 'HEAD)))
	     (body obj)))
	 (else
	  (let* ((c (assq hop-serialize: header))
		 (ctype (cond
			   ((or (not (pair? c)) (not (string? (cdr c))))
			    "application/x-javascript")
			   ((string=? (cdr c) "x-hop")
			    "application/x-hop")
			   ((string=? (cdr c) "javascript")
			    "application/x-javascript")
			   ((string=? (cdr c) "url-arraybuffer")
			    "application/x-url-hop")
			   ((string=? (cdr c) "json-byte-array")
			    "application/x-json")
			   ((string=? (cdr c) "json")
			    "application/json")
			   (else
			    "application/x-javascript"))))
	     (instantiate::http-response-hop
		(backend (hop-xml-backend-secure))
		(content-type ctype)
		(charset (hop-charset))
		(header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
		(bodyp (not (eq? method 'HEAD)))
		(value obj)))))))

;*---------------------------------------------------------------------*/
;*    scheme->response ::%http-response ...                            */
;*---------------------------------------------------------------------*/
(define-method (scheme->response obj::%http-response req)
   obj)

;*---------------------------------------------------------------------*/
;*    scheme->response ::xml ...                                       */
;*---------------------------------------------------------------------*/
(define-method (scheme->response obj::xml req)
   (with-access::http-request req (method)
      (let ((be (hop-xml-backend-secure)))
	 (with-access::xml-backend be (mime-type)
	    (instantiate::http-response-xml
	       (backend be)
	       (content-type mime-type)
	       (charset (hop-charset))
	       (bodyp (not (eq? method 'HEAD)))
	       (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
	       (xml obj))))))

;*---------------------------------------------------------------------*/
;*    scheme->response ::xml-tilde ...                                 */
;*---------------------------------------------------------------------*/
(define-method (scheme->response obj::xml-tilde req)
   (with-access::http-request req (method)
      (instantiate::http-response-string
	 (content-type (hop-mime-type))
	 (charset (hop-charset))
	 (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
	 (bodyp (not (eq? method 'HEAD)))
	 (body (xml-tilde->expression obj)))))

;*---------------------------------------------------------------------*/
;*    scheme->response ::xml-http-request ...                          */
;*    -------------------------------------------------------------    */
;*    This is used to forward the response of a remote WITH-HOP        */
;*    to clients.                                                      */
;*---------------------------------------------------------------------*/
(define-method (scheme->response obj::xml-http-request req)
   (with-access::xml-http-request obj (status input-port header)
      (instantiate::http-response-procedure
	 (start-line (format "HTTP/1.1 ~a" status))
	 (header header)
	 (proc (lambda (op) (display (read-string input-port) op))))))
   
;*---------------------------------------------------------------------*/
;*    http-send-request ...                                            */
;*---------------------------------------------------------------------*/
(define (http-send-request req::http-request proc::procedure #!key args body)
   (with-trace 'hop-response "http-send-request"
      (with-access::http-request req (scheme method path (httpv http) host port header socket userinfo timeout connection-timeout connection)
	 (let ((ssl (eq? scheme 'https)))
	    (let loop ((host host)
		       (port port)
		       (user userinfo)
		       (path path))
	       (trace-item "scheme=" scheme " host=" host " port=" port)
	       (trace-item "method=" method " path=" path " user=" user)
	       (let* ((sock (if (and (not ssl) (hop-use-proxy))
				(make-proxy-socket
				   (hop-use-proxy) connection-timeout)
				(make-client-socket/timeout
				   host port connection-timeout req ssl)))
		      (out (socket-output sock))
		      (in (socket-input sock)))
		  (socket-option-set! sock SO_REUSEADDR: #t)
		  (when (> timeout 0)
		     (output-timeout-set! out timeout)
		     (input-timeout-set! in timeout))
		  (with-handler
		     (lambda (e)
			(if (isa? e &http-redirection)
			    (multiple-value-bind (rhost rport ruser rpath)
			       (redirect e)
			       (loop (or rhost host)
				  (or rport port)
				  (or ruser user)
				  (cond
				     (rhost rpath)
				     ((string-prefix? "/" rpath)
				      rpath)
				     (else
				      (make-file-name (dirname path) rpath)))))
			    (raise e)))
		     (set! header (cons (cons connection: connection) header))
		     (let ((auth (if (and (isa? req http-server-request)
					  (with-access::http-server-request req (authorization)
					     authorization))
				     (with-access::http-server-request req (authorization)
					authorization)
				     (let ((auth (assq :authorization header)))
					(when (pair? auth) (cdr auth)))))
			   (ctype (let ((ct (assq :content-type header)))
				     (when (pair? ct)
					(string->symbol (cdr ct))))))
			(http :in in :out out
			   :protocol scheme :method method :http-version httpv
			   :host host :port port :path path :header header
			   :content-type (or ctype (when args 'multipart/form-data))
			   :authorization auth
			   :timeout timeout
			   :login user
			   :args args
			   :body body
			   :proxy (hop-use-proxy))
			(when (eq? connection 'close)
			   (close-output-port out))
			(unwind-protect
			   (http-parse-response in out proc)
			   (socket-close sock))))))))))
   
;*---------------------------------------------------------------------*/
;*    redirect ...                                                     */
;*---------------------------------------------------------------------*/
(define (redirect e::&http-redirection)
   (with-access::&http-redirection e (url (ip port))
      (multiple-value-bind (rproto ruser rhost rport rpath)
	 (url-parse url)
	 (cond
	    ((string=? rproto "file")
	     (values #f #f #f rpath))
	    (else
	     (values rhost rport ruser rpath))))))

;*---------------------------------------------------------------------*/
;*    make-proxy-socket ...                                            */
;*---------------------------------------------------------------------*/
(define (make-proxy-socket proxy timeout)
   (let ((i (string-index proxy #\:))
	 (tmt (micro-seconds timeout)))
      (if (and (fixnum? i) (>fx i 0))
	  (let* ((len (string-length proxy))
		 (proxy (substring proxy 0 i))
		 (port (string->integer (substring proxy (+fx i 1) len))))
	     (make-client-socket proxy port :timeout tmt))
	  (make-client-socket proxy 80 :timeout tmt))))

