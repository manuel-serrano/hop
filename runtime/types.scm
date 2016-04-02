;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/runtime/types.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:55:24 2004                          */
;*    Last change :  Sat Apr  2 07:23:52 2016 (serrano)                */
;*    Copyright   :  2004-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP's classes                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_types
   
   (import __hop_param
	   __hop_xml-types)
   
   (export (class user
	      (mutex::mutex read-only (default (make-mutex)))
	      (name::bstring read-only)
	      (uuid::obj read-only (default #unspecified))
	      (groups::pair-nil read-only (default '()))
	      (password::bstring read-only)
	      (services read-only)
	      (files read-only)
	      (directories read-only)
	      (preferences-filename::obj read-only)
	      (preferences::pair-nil (default '()))
	      (data::obj (default #unspecified))
	      (authentication::symbol read-only (default 'basic)))
	   
	   (class &hop-method-error::&io-parse-error)
	   (class &hop-autoload-error::&io-error)
	   (class &hop-security-error::&error)
	   (class &hop-authentication-error::&error)
	   (class &hop-injection-error::&hop-security-error)
	   
           (abstract-class %http-message
	      (seconds::elong read-only (default (current-seconds)))
	      (socket (default #f))
	      (header::pair-nil (default '()))
	      (content-length::elong read-only (default #e-1))
	      (charset (default #f))
	      (timeout::int (default 0)))
	   
	   (class http-request::%http-message
	      (%user (default #f))
	      (id::int read-only (default -1))
	      (transfer-encoding (default #f))
	      (http::symbol (default 'HTTP/1.1))
	      (host::bstring (default "localhost"))
	      (path::bstring (default "/dummy"))
	      (userinfo read-only (default #f))
	      (scheme::symbol (default 'http))
	      (port::int (default 80))
	      (method::symbol read-only (default 'GET))
	      (abspath::bstring (default ""))
	      (query::obj (default #f))
	      (connection::symbol (default 'keep-alive))
	      (authorization (default #f))
	      (connection-timeout::int (default 0)))
	   
	   (final-class http-server-request::http-request
	      (service::obj (default #unspecified)))
	   
	   (wide-class http-server-request+::http-server-request
	      (%env (default #f)))
	   
	   (class http-proxy-request::http-request)
	   
	   (class xml-http-request
	      (status::int read-only)
	      (header::pair-nil read-only (default '()))
	      (input-port read-only))
	   
	   (abstract-class %http-response::%http-message
	      (content-type::obj (default #f))
	      (bodyp::bool read-only (default #t)))
	   
	   (class http-response-abort::%http-response)
	   
	   (class http-response-proxy::%http-response
	      (http::symbol read-only (default 'HTTP/1.1))
	      (host::bstring read-only (default "localhost"))
	      (scheme::symbol read-only (default 'http))
 	      (port::int read-only (default 80))
	      (method::symbol read-only (default 'GET))
	      (path::bstring read-only)
	      (userinfo read-only (default #f))
	      (remote-timeout read-only (default (hop-read-timeout)))
	      (connection-timeout read-only (default (hop-connection-timeout))))

	   ;; http-response-remote is a weblet backward compatibiilty type
	   (class http-response-remote::http-response-proxy)
	   
	   (class http-response-proxy-websocket::http-response-proxy
	      (request::http-request read-only))
	   
	   (class http-response-filter::%http-response
	      (response::%http-response read-only)
	      (statusf::procedure (default (lambda (x) x)))
	      (headerf::procedure (default (lambda (x) x)))
	      bodyf::procedure)
	   
	   (abstract-class %http-response-server::%http-response
	      (server::bstring (default (hop-server-name)))
 	      (start-line::bstring read-only (default "HTTP/1.1 200 Ok")))
	   
	   (class http-response-autoload::%http-response-server
	      (request::http-request read-only))
	   
	   (class http-response-xml::%http-response-server
	      (backend read-only)
	      (xml read-only))
	   
	   (class http-response-hop::%http-response-server
	      (backend read-only)
	      (padding::obj (default #f))
	      (value::obj read-only))
	   
	   (class http-response-procedure::%http-response-server
	      (proc::procedure read-only))
	   
	   (class http-response-raw::%http-response-server
	      (connection::obj read-only (default #f))
	      (proc::procedure read-only))
	   
	   (class http-response-file::%http-response-server
	      (file::bstring read-only))
	   
	   (class http-response-shoutcast::http-response-file)
	   
	   (class http-response-string::%http-response-server
	      (body::bstring read-only (default "")))
	   
	   (class http-response-authentication::http-response-string)
	   
	   (class http-response-websocket::%http-response-server
	      (request::http-request read-only)
	      (connection::symbol read-only (default 'Upgrade))
	      (origin::obj read-only (default #f))
	      (location::obj read-only (default #f))
	      (protocol::obj read-only)
	      (sec read-only (default #f))
	      (accept read-only (default #f))
	      (onconnect read-only (default #f)))
	   
	   (class http-response-cgi::%http-response-server
	      (cgibin::bstring read-only))
	   
	   (class http-response-persistent::%http-response
	      (request::http-request read-only)
	      (body (default #f)))
	   
	   (class http-response-async::%http-response
	      (async::procedure read-only))
	   
	   (class http-response-chunked::%http-response
	      (body (default #f)))
	   
	   (class http-response-put::%http-response-server
	      (uri::bstring read-only))
	   
	   (class hop-service
	      ;; the service identifier (e.g., doc/example)
	      id::symbol
	      ;; the weblet identifier (e.g., doc)
	      wid::symbol
	      ;; the path associated with the service
	      path::bstring
	      ;; the service formals
	      (args::obj read-only)
	      ;; the user procedure associated
	      (proc::procedure read-only (info '(serialize: #f)))
	      ;; the JS code calling that service
	      (javascript::bstring read-only)
	      ;; a time stamp
	      (creation::elong read-only (default (current-seconds)))
	      ;; a timeout in second
	      (timeout::long read-only (default -1))
	      ;; the number of times the service might be called
	      (ttl::long (default -1))
	      ;; the resource directory (that contains the source file)
	      (resource::obj (default #f))
	      ;; the source file
	      (source::obj read-only (default #f))
	      ;; ctx (i.e., %this for JavaScript)
	      (ctx::obj read-only (default #f)))))

;*---------------------------------------------------------------------*/
;*    object-display ...                                               */
;*---------------------------------------------------------------------*/
(define-method (object-display o::http-request . port)
   (with-output-to-port (if (null? port) (current-output-port) (car port))
      (lambda ()
	 (with-access::http-request o (method scheme host port path http)
	    (display* "#<" (class-name (object-class o)) ": "
		      method " "
		      scheme "://" host ":" port (string-for-read path))
	    (when (string? http)
	       (display " ")
	       (display http))
	    (display ">")))))
    
;*---------------------------------------------------------------------*/
;*    object-write ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (object-write o::http-request . port)
   (with-output-to-port (if (null? port) (current-output-port) (car port))
      (lambda ()
	 (with-access::http-request o (method http scheme host port path)
	    (print "method: " method)
	    (print "http  : " http)
	    (print "scheme: " scheme)
	    (print "host  : " host)
	    (print "port  : " port)
	    (print "path  : " path)))))

;*---------------------------------------------------------------------*/
;*    object-display ...                                               */
;*---------------------------------------------------------------------*/
(define-method (object-display o::hop-service . port)
   (with-output-to-port (if (null? port) (current-output-port) (car port))
      (lambda ()
	 (with-access::hop-service o (id)
	    (display* "#<hop-service: " id ">")))))
    
;*---------------------------------------------------------------------*/
;*    object-write ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (object-write o::hop-service . port)
   (with-output-to-port (if (null? port) (current-output-port) (car port))
      (lambda ()
	 (with-access::hop-service o (id path args)
	    (print "id  : " id)
	    (print "path: " path)
	    (print "args: " args)))))

;*---------------------------------------------------------------------*/
;*    exception-notify ::&hop-autoload-error ...                       */
;*---------------------------------------------------------------------*/
(define-method (exception-notify exc::&hop-autoload-error)
   (with-access::&hop-autoload-error exc (proc obj msg)
      (fprintf (current-error-port) "~a: cannot autoload ~s\n" proc msg)
      (exception-notify obj)))
