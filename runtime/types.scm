;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/types.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:55:24 2004                          */
;*    Last change :  Sun Jul 15 16:14:57 2007 (serrano)                */
;*    Copyright   :  2004-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP's classes                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_types
   
   (option  (set! *dlopen-init* "bgl_dload_init_s_hop"))

   (import __hop_param)

   (use    __hop_xml)
   
   (export (class user
	       (name::bstring read-only)
	       (groups::pair-nil read-only (default '()))
	       (password::bstring read-only)
	       (services read-only)
	       (directories read-only)
	       (data::obj (default #unspecified)))

	   (class &hop-method-error::&io-parse-error)
	   
           (abstract-class %http-message
	      (date::date read-only (default (current-date)))
	      (socket (default #f))
	      (header::pair-nil (default '()))
	      (content-length::elong read-only (default #e-1))
	      (char-encoding (default #f))
	      (timeout::int (default -1)))
	   
	   (class http-request::%http-message
	      (user (default #f))
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
	   
	   (abstract-class %http-response::%http-message
	      (content-type::bstring (default (hop-default-mime-type)))
	      (request::http-request (default (instantiate::http-request)))
	      (bodyp::bool read-only (default #t)))

	   (class http-response-abort::%http-response)
	   
	   (class http-response-remote::%http-response
	      (http::bstring read-only)
	      (host::bstring read-only)
	      (scheme::symbol read-only)
 	      (port::bint read-only)
	      (method::symbol read-only)
	      (path::bstring read-only)
	      (userinfo read-only)
	      (encoded-path::bstring read-only)
	      (remote-timeout read-only)
	      (connection-timeout read-only))

	   (class http-response-filter::%http-response
	      (response::%http-response read-only)
	      (statusf::procedure (default (lambda (x) x)))
	      (headerf::procedure (default (lambda (x) x)))
	      bodyf::procedure)

	   (class http-response-authentication::%http-response
	      (server::bstring (default (hop-server-name)))
	      (body read-only (default #f)))

	   (abstract-class %http-response-local::%http-response
	      (server::bstring (default (hop-server-name)))
 	      (start-line::bstring read-only (default "HTTP/1.1 200 Ok")))

	   (class http-response-hop::%http-response-local
	      (backend read-only (default (hop-xml-backend)))
	      (xml read-only))
	   
	   (class http-response-js::%http-response-local
	      (backend read-only (default (hop-xml-backend)))
	      (value::obj read-only))
	   
	   (class http-response-procedure::%http-response-local
	      (proc::procedure read-only))
	   
	   (class http-response-file::%http-response-local
	      (file::bstring read-only))
	   
	   (class http-response-shoutcast::http-response-file)
	   
	   (class http-response-string::%http-response-local
	      (body::bstring read-only (default "")))

	   (class http-response-cgi::%http-response-local
	      (cgibin::bstring read-only))

	   (class http-response-persistent::%http-response)
	   
	   (class http-response-put::%http-response-local
	      (uri::bstring read-only))

	   (class http-response-webdav::%http-response-local
	      (xml read-only))

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
