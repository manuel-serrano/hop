;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/http/types.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May 14 09:04:28 2024                          */
;*    Last change :  Wed May 15 12:04:57 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HTTP public types                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module ...                                                   */
;*---------------------------------------------------------------------*/
(module __http_types
   
   (export (abstract-class %http-message
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
	   
	   (class http-proxy-request::http-request)
	   
	   (abstract-class %http-response::%http-message
	      (content-type::obj (default #f))
	      (bodyp::bool read-only (default #t)))
	   
	   (class http-response-proxy::%http-response
	      (http::symbol read-only (default 'HTTP/1.1))
	      (host::bstring read-only (default "localhost"))
	      (scheme::symbol read-only (default 'http))
 	      (port::int read-only (default 80))
	      (method::symbol read-only (default 'GET))
	      (path::bstring read-only)
	      (userinfo read-only (default #f))
	      (remote-timeout read-only)
	      (connection-timeout read-only))
	   
	   (abstract-class %http-response-server::%http-response
	      (server (default #f))
 	      (start-line::bstring (default "HTTP/1.1 200 Ok")))
	   
	   (class http-response-string::%http-response-server
	      (body::bstring read-only (default "")))
	   
	   (class http-response-file::%http-response-server
	      (file::bstring read-only)
	      (size::elong (default #e-1))
	      (offset::elong (default #e-1))
	      (connection (default #f)))
	   
	   (class http-response-async::%http-response
	      (async::procedure read-only)
	      (ctx::obj (default 'hop)))
	   
	   (class http-response-abort::%http-response)
	   
	   (class http-response-error::%http-response
	      (err::obj read-only))

	   (class &io-parse-method-error::&io-parse-error)
	   
	   (generic http-response::symbol ::%http-response ::obj ::socket)))

;*---------------------------------------------------------------------*/
;*    http-response ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (http-response::symbol r::%http-response request socket))

