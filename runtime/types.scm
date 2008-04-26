;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/runtime/types.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:55:24 2004                          */
;*    Last change :  Sat Apr 26 08:19:43 2008 (serrano)                */
;*    Copyright   :  2004-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP's classes                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_types
   
   (import __hop_param)

   (use    __hop_xml)
   
   (export (class user
	       (name::bstring read-only)
	       (groups::pair-nil read-only (default '()))
	       (password::bstring read-only)
	       (services read-only)
	       (directories read-only)
	       (preferences-filename::obj read-only)
	       (preferences::pair-nil (default '()))
	       (data::obj (default #unspecified)))

	   (class &hop-method-error::&io-parse-error)
	   
           (abstract-class %http-message
	      (date::date read-only (default (current-date)))
	      (socket (default #f))
	      (header::pair-nil (default '()))
	      (content-length::elong read-only (default #e-1))
	      (charset (default #f))
	      (timeout::int (default -1)))
	   
	   (class http-request::%http-message
	      (user::user read-only (default (user-nil)))
	      (id::int read-only (default -1))
	      (localhostp::bool (default #f))
	      (localclientp::bool (default #f))
	      (proxyp::bool read-only (default #f))
	      (hook::procedure (default (lambda (rep) rep)))
	      (transfer-encoding (default #f))
	      (authorization (default #f))
	      (proxy-authorization (default #f))
	      (http::bstring (default "HTTP/1.1"))
	      (host::bstring (default "localhost"))
	      (path::bstring (default "/dummy"))
	      (userinfo read-only (default #f))
	      (scheme::symbol (default 'http))
	      (port::bint (default 80))
	      (method::symbol read-only (default 'GET))
	      (encoded-path::bstring (default ""))
	      (connection::symbol (default 'keep-alive))
	      (service::obj (default #unspecified))
	      (%env (default #f)))

	   (class xml-http-request
	      (status::int read-only)
	      (header::pair-nil read-only (default '()))
	      (input-port read-only))
	   
	   (abstract-class %http-response::%http-message
	      (content-type::bstring (default (hop-default-mime-type)))
	      (request::http-request (default (http-request-nil)))
	      (bodyp::bool read-only (default #t)))

	   (class http-response-abort::%http-response)
	   
	   (class http-response-remote::%http-response
	      (http::bstring read-only (default "http"))
	      (host::bstring read-only (default "localhost"))
	      (scheme::symbol read-only (default '?))
 	      (port::bint read-only (default 80))
	      (method::symbol read-only (default 'GET))
	      (path::bstring read-only)
	      (userinfo read-only (default #f))
	      (encoded-path::bstring read-only)
	      (remote-timeout read-only (default #f))
	      (connection-timeout read-only (default #f)))

	   (class http-response-filter::%http-response
	      (response::%http-response read-only)
	      (statusf::procedure (default (lambda (x) x)))
	      (headerf::procedure (default (lambda (x) x)))
	      bodyf::procedure)

	   (abstract-class %http-response-local::%http-response
	      (server::bstring (default (hop-server-name)))
 	      (start-line::bstring read-only (default "HTTP/1.1 200 Ok")))

	   (class http-response-authentication::%http-response-local
	      (body read-only (default #f)))

	   (class http-response-hop::%http-response-local
	      (backend read-only (default (hop-xml-backend)))
	      (force-content-length read-only (default #f))
	      (xml read-only))
	   
	   (class http-response-js::%http-response-local
	      (backend read-only (default (hop-xml-backend)))
	      (value::obj read-only))
	   
	   (class http-response-procedure::%http-response-local
	      (proc::procedure read-only))
	   
	   (class http-response-raw::%http-response-local
	      (connection::obj read-only (default #f))
	      (proc::procedure read-only))

	   (class http-response-file::%http-response-local
	      (file::bstring read-only))
	   
	   (class http-response-shoutcast::http-response-file)
	   
	   (class http-response-string::%http-response-local
	      (body::bstring read-only (default "")))

	   (class http-response-error::http-response-string)

	   (class http-response-cgi::%http-response-local
	      (cgibin::bstring read-only))

	   (class http-response-persistent::%http-response)
	   
	   (class http-response-put::%http-response-local
	      (uri::bstring read-only))

	   (class hop-service
	      ;; the service identifier (e.g., doc/example)
	      (id::symbol read-only)
	      ;; the weblet identifier (e.g., doc)
	      (wid::symbol read-only)
	      ;; the path associated with the service
	      (path::bstring read-only)
	      ;; the service formals
	      (args::obj read-only)
	      ;; the implementation body
	      (%exec::procedure read-only)
	      ;; the user procedure associated
	      (proc::procedure read-only)
	      ;; the JS code calling that service
	      (javascript::bstring read-only)
	      ;; a time stamp
	      (creation::elong read-only)
	      ;; a timeout in second
	      (timeout::long read-only (default -1))
	      ;; the number of times the service might be called
	      (ttl::long (default -1))
	      ;; the resource directory (that contains the source file)
	      (resource::obj (default #f))
	      ;; the source file
	      (source::obj read-only (default #f)))

	   (http-request-hook-add! ::http-request ::procedure)))
   
;*---------------------------------------------------------------------*/
;*    object-display ...                                               */
;*---------------------------------------------------------------------*/
(define-method (object-display o::http-request . port)
   (with-output-to-port (if (null? port) (current-output-port) (car port))
      (lambda ()
	 (with-access::http-request o (method scheme host port path http)
	    (display* "#<http-request: " method " "
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
;*    http-request-hook-add! ...                                       */
;*---------------------------------------------------------------------*/
(define (http-request-hook-add! req h)
   (with-access::http-request req (hook)
      (let ((old-hook hook))
	 (set! hook (lambda (rep)
		       (let* ((rep2 (h rep))
			      (res (if (%http-response? rep2)
				       rep2
				       rep)))
			  (old-hook res)))))))
