;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/types.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:55:24 2004                          */
;*    Last change :  Tue Jun  6 17:58:32 2006 (serrano)                */
;*    Copyright   :  2004-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP's classes                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_types
   
   (option  (set! *dlopen-init* #t))

   (import __hop_param)
   
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
	      (char-encoding (default #f)))
	   
	   (class http-request::%http-message
	      (user (default #f))
	      (id::int read-only (default -1))
	      (localhostp::bool (default #f))
	      (localclientp::bool (default #f))
	      (hook::procedure (default (lambda (rep) rep)))
	      (transfer-encoding (default #f))
	      (authorization (default #f))
	      (proxy-authorization (default #f))
	      (http::bstring (default "HTTP/1.1"))
	      host::bstring
	      path::bstring
	      (userinfo read-only (default #f))
	      (scheme::bstring (default "http"))
	      (port::bint (default 80))
	      (method::symbol read-only (default 'GET))
	      (encoded-path::bstring (default ""))
	      (timeout::int read-only (default -1)))
	   
	   (abstract-class %http-response::%http-message
	      (content-type::bstring (default "text/html"))
	      (request::obj (default #unspecified))
	      (bodyp::bool read-only (default #t))
	      (timeout::int (default -1)))

	   (class http-response-abort::%http-response)
	   
	   (class http-response-remote::%http-response
	      (http::bstring read-only)
	      (host::bstring read-only)
	      (scheme::bstring read-only)
 	      (port::bint read-only)
	      (method::symbol read-only)
	      (path::bstring read-only)
	      (userinfo read-only)
	      (encoded-path::bstring read-only)
	      (remote-timeout read-only (default #f)))

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
	      (xml read-only))
	   
	   (class http-response-procedure::%http-response-local
	      (proc::procedure read-only))
	   
	   (class http-response-file::%http-response-local
	      (file::bstring read-only))
	   
	   (class http-response-shoutcast::http-response-file)
	   
	   (class http-response-string::%http-response-local
	      (body::bstring read-only (default "")))

	   ;; this class is obsolete. it should no longer be used
	   (class http-response-obj::%http-response-local
	      (body::obj read-only))
	   
	   (class http-response-js::%http-response-local
	      (body::obj read-only))
	   
	   (class http-response-cgi::%http-response-local
	      (cgibin::bstring read-only))

	   (class http-response-persistent::%http-response)
	   
	   (class http-response-put::%http-response-local
	      (uri::bstring read-only))

	   (class hop-service
	      (id::symbol read-only)
	      (path::bstring read-only)
	      (args::obj read-only)
	      (%exec::procedure read-only)
	      (proc::procedure read-only)
	      (javascript::bstring read-only)
	      (expiration read-only (default #f)))

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
