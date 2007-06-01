;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/http-response.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 25 14:15:42 2004                          */
;*    Last change :  Fri Jun  1 07:33:40 2007 (serrano)                */
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
	    (make-unchunks ::input-port)
	    (response-chunks ::input-port ::output-port)
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
      (with-access::http-response-authentication r (header content-type body server timeout request)
	 (let ((p (socket-output socket))
	       (connection (http-request-connection request)))
	    (when (>fx timeout 0)
	       (output-timeout-set! p timeout))
	    (http-write-line p "HTTP/1.0 401 Unauthorized")
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
		(s (if (eq? ce 'UTF-8)
		       (iso-latin->utf8! body)
		       body))
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
	    (http-write-line p "hop-json: true")
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
		      (http-read-header (process-output-port proc)
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
			    (http-read-header (process-output-port proc)
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
       (or (response-is-xml? (car p))
	   (response-is-xml? (cdr p))))
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
   (with-trace 3 'http-send-request
      (with-access::http-request req (scheme method path http host port header socket userinfo authorization timeout)
	 (let ((ssl (eq? scheme 'https)))
	    (let loop ((host host)
		       (port port)
		       (userinfo userinfo)
		       (path path))
	       (let* ((sock (make-client-socket/timeout host port
							timeout req ssl))
		      (rp (socket-output sock))
		      (ip (socket-input sock)))
		  (when (> timeout 0)
		     (output-port-timeout-set! rp timeout)
		     (input-port-timeout-set! ip timeout))
		  (trace-item "sock=" sock)
		  (unwind-protect
		     (begin
			;; the header
			(http-write-line rp method " " path " " http)
			(when host (http-write-line rp "Host: " host))
			(http-write-header rp header)
			(cond
			   (userinfo
			    (http-write-line rp "Authorization: Basic "
					     (base64-encode userinfo)))
			   (authorization
			    (http-write-line rp "Authorization: "
					     authorization)))
			(http-write-line rp)
			;; the content of the request
			(cond
			   ((string? socket)
			    (display socket rp))
			   ((input-port? socket)
			    (send-chars socket rp)))
			(flush-output-port rp)
			(multiple-value-bind (_1 status-code _2)
			   (http-parse-status-line ip)
			   (multiple-value-bind (header _1 _2 cl te _3 _4 _5)
			      (http-read-header ip rp)
			      (case status-code
				 ((204 304)
				  ;; no message body
				  (proc status-code cl #f))
				 ((301 307)
				  ;; redirection
				  (let ((loc (assq location: header)))
				     (if (not (pair? loc))
					 (raise
					  (instantiate::&io-malformed-url-error
					     (proc 'http-sedn-request)
					     (msg "No URL for redirection!")
					     (obj ip)))
					 (multiple-value-bind (_
							       redirect-uinfo
							       redirect-host
							       redirect-port
							       redirect-path)
					    (url-parse (cdr loc))
					    (loop redirect-host
						  redirect-port
						  redirect-uinfo
						  redirect-path)))))
				 (else
				  (if (not (eq? te 'chunked))
				      (proc status-code cl ip)
				      (let ((ip2 (open-input-procedure
						  (make-unchunks ip))))
					 (unwind-protect
					    (proc status-code cl ip2)
					    (begin
					       (close-input-port ip2)
					       (close-input-port ip))))))))))
		     (socket-close sock))))))))

;*---------------------------------------------------------------------*/
;*    *chunk-size-grammar* ...                                         */
;*---------------------------------------------------------------------*/
(define *chunk-size-grammar*
   (regular-grammar ((SZ (+ xdigit))
		     (BLANK (in " \t"))
		     (CRLF "\r\n")
		     op)
      ((: SZ (* BLANK) #\;)
       (when op (display (the-string) op))
       (let ((sz (string->integer
		  (the-substring 0 (-fx (the-length) 1))
		  16)))
	  (read/rp (regular-grammar ((CRLF "\r\n"))
		      ((: (+ (or (+ (out "\r")) (+ (: "\r" (out "\n"))))) CRLF)
		       (trace-item "chunk-extension: " (the-string))
		       (when op (display (the-string) op)))
		      (else
		       (raise (instantiate::&io-parse-error
				 (proc 'chunks)
				 (msg "Illegal character")
				 (obj (http-parse-error-message
				       (the-failure)
				       (the-port)))))))
		   (the-port))
	  sz))
      ((: SZ (* BLANK) CRLF)
       (when op (display (the-string) op))
       (let ((l (the-length)))
	  (string->integer (the-substring 0 (-fx l 2)) 16)))
      (else
       (let* ((c1 (the-failure))
	      (c2 (read-char (the-port)))
	      (c3 (read-char (the-port)))
	      (c4 (read-char (the-port)))
	      (c5 (read-char (the-port))))
	  (raise (instantiate::&io-parse-error
		    (proc 'chunks)
		    (msg "Illegal chunk size")
		    (obj (string-for-read (string c1 c2 c3 c4 c5)))))))))

;*---------------------------------------------------------------------*/
;*    response-chunks ...                                              */
;*---------------------------------------------------------------------*/
(define (response-chunks ip::input-port op::output-port)
   (with-trace 4 'response-chunks
      (let loop ()
	 (let ((sz (read/rp *chunk-size-grammar* ip op)))
	    (trace-item "chunk-size: " sz " " ip " " op)
	    (if (>fx sz 0)
		;; a regular chunk
		(begin
		   (let loop ((sz sz))
		      (when (>fx sz 0)
			 (let ((s (send-chars ip op sz)))
			    (when (>fx s 0)
			       (loop (-fx sz s))))))
		   (let ((s (http-read-crlf ip)))
		      (trace-item "# " (string-for-read s))
		      (display s op)
		      (loop)))
		;; the last chunk starting with an optional trailer
		(let loop ()
		   (let ((l (http-read-line ip)))
		      (trace-item "# " (string-for-read l))
		      (display l op)
		      (if (>fx (string-length l) 2)
			  (begin
			     (trace-item "trailer: " (string-for-read l))
			     (loop))
			  (flush-output-port op)))))))))

;*---------------------------------------------------------------------*/
;*    make-unchunks ...                                                */
;*---------------------------------------------------------------------*/
(define (make-unchunks ip::input-port)
   (let* ((state 'size)
	  (sz 0)
	  (bufsz 512)
	  (buffer (make-string bufsz #a000)))
      (lambda ()
	 (let loop ()
	    (case state
	       ((eof)
		#f)
	       ((trailer)
		(let ((l (http-read-line ip)))
		   (when (<=fx (string-length l) 2)
		      (set! state 'eof))
		   l))
	       ((chunk)
		(cond
		   ((=fx sz 0)
		    (http-read-crlf ip)
		    (set! state 'size)
		    (loop))
		   ((<fx sz bufsz)
		    (let ((s (read-chars sz ip)))
		       (set! sz (-fx sz (string-length s)))
		       s))
		   (else
		    (let ((s (read-chars! buffer bufsz ip)))
		       (set! sz (-fx sz s))
		       (if (=fx s bufsz)
			   buffer
			   (substring buffer 0 s))))))
	       (else
		(set! sz (read/rp *chunk-size-grammar* ip #f))
		(if (>fx sz 0)
		    ;; a regular chunk
		    (begin
		       (set! state 'chunk)
		       (loop))
		    ;; the last chunk starting with an optional trailer
		    (begin
		       (set! state 'trailer)
		       (loop)))))))))
