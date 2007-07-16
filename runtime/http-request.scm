;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/http-request.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:55:24 2004                          */
;*    Last change :  Mon Jul 16 08:02:13 2007 (serrano)                */
;*    Copyright   :  2004-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HTTP request management                                      */
;*    -------------------------------------------------------------    */
;*    HTTP/1.1 is defined in the RFC 2616.                             */
;*    Authentication is defined in the RFC 2617.                       */
;*    Cookie is defined in the RFC 2109.                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_http-request

   (library web)
   
   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_http-lib
	    __hop_user
	    __hop_misc)
   
   (export  (http-parse-request::http-request ::socket ::int ::int)))
	   
;*---------------------------------------------------------------------*/
;*    parse-error ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-error proc msg obj)
   (raise (instantiate::&io-parse-error
	     (obj obj)
	     (proc proc)
	     (msg msg))))

;*---------------------------------------------------------------------*/
;*    http-parse-request ...                                           */
;*---------------------------------------------------------------------*/
(define (http-parse-request sock id timeout)
   (let ((port (socket-input sock))
	 (out (socket-output sock)))
      (input-timeout-set! port timeout)
      (let* ((req (read/rp request-line-grammar port id out))
	     (localc (string=? (socket-local-address sock)
			       (socket-host-address sock)))
	     (localh (or (not (hop-enable-proxing))
			 (not (http-request-proxyp req))
			 (is-local? (http-request-host req))
			 (let ((ip (host (http-request-host req))))
			    (or (string=? ip (socket-local-address sock))
				(string=? ip (hop-server-hostip)))))))
	 (input-timeout-set! port 0)
	 (with-access::http-request req (socket localclientp localhostp user userinfo)
	    (set! socket sock)
	    (set! localclientp localc)
	    (set! localhostp localh)
	    (when (and (not user) localhostp)
	       (set! user (if (string? userinfo)
			      (or (find-authenticated-user userinfo)
				  (anonymous-user))
			      (anonymous-user))))
	    req))))

;*---------------------------------------------------------------------*/
;*    request-line-grammar ...                                         */
;*---------------------------------------------------------------------*/
(define request-line-grammar
   (regular-grammar ((SP #\Space)
		     (CRLF "\r\n")
		     id
		     out)
      ((: "GET" SP)
       (http-parse-method-request 'GET (the-port) out id))
      ((: "HOP" SP)
       (http-parse-method-request 'HOP (the-port) out id))
      ((: "HEAD" SP)
       (http-parse-method-request 'HEAD (the-port) out id))
      ((: "POST" SP)
       (http-parse-method-request 'POST (the-port) out id))
      ((: "PUT" SP)
       (http-parse-method-request 'PUT (the-port) out id))
      ((: "TRACE" SP)
       (http-parse-method-request 'TRACE (the-port) out id))
      ((: "OPTIONS" SP)
       (http-parse-method-request 'OPTIONS (the-port) out id))
      ((: "PROPFIND" SP)
       (http-parse-method-request 'PROPFIND (the-port) out id))
      ((: "PROPPATCH" SP)
       (http-parse-method-request 'PROPPATCH (the-port) out id))
      ((: "MKCOL" SP)
       (http-parse-method-request 'MKCOL (the-port) out id))
      ((: "DELETE" SP)
       (http-parse-method-request 'DELETE (the-port) out id))
      ((: "COPY" SP)
       (http-parse-method-request 'COPY (the-port) out id))
      ((: (+ (in ("AZaz"))) SP)
       (raise (instantiate::&hop-method-error
		 (proc 'request-line-grammar)
		 (msg "Method not implemented")
		 (obj (the-string)))))
      (else
       (parse-error 'request-line-grammar "Illegal character" (the-failure)))))

;*---------------------------------------------------------------------*/
;*    http-parse-method-request ...                                    */
;*---------------------------------------------------------------------*/
(define (http-parse-method-request method pi::input-port po::output-port id)
   (with-trace 3 'http-parse-request
      (let (scheme hostname port abspath http-version userinfo)
	 (let ((pi2 (if (or #t (>=fx (bigloo-debug) 3))
			(let ((line (http-read-line pi)))
			   (trace-item method " " (string-for-read line))
			   (open-input-string line))
			pi)))
	    (multiple-value-bind (s u h p a)
	       (http-url-parse pi2)
	       (trace-item "scheme=" s " user=" u
			   " hostname=" h " port=" p " abspath=[" a "]")
	       (set! scheme (string->symbol s))
	       (set! hostname h)
	       (set! port p)
	       (set! abspath a)
	       (set! userinfo u)
	       (read/rp http-sp-grammar pi2)
	       (set! http-version (read/rp http-version-grammar pi2))
	       (http-read-crlf pi2)
	       (if (input-string-port? pi2)
		   (close-input-port pi2))))
	 (multiple-value-bind (header actual-host actual-port cl te auth pauth co)
	    (http-read-header pi po)
	    (let ((cabspath (http-file-name-canonicalize! abspath))
		  (connection (or co
				  (if (string<? http-version "HTTP/1.1")
				      'close
				      'keep-alive))))
	       (instantiate::http-request
		  (id id)
		  (method method)
		  (http http-version)
		  (scheme scheme)
		  (proxyp (string? hostname))
		  (path (xml-string-decode cabspath))
		  (userinfo userinfo)
		  (encoded-path cabspath)
		  (header header)
		  (port (or actual-port port (hop-port)))
		  (host (or actual-host hostname "localhost"))
		  (content-length cl)
		  (transfer-encoding te)
		  (authorization auth)
		  (proxy-authorization pauth)
		  (connection connection)
		  (user (or (and (string? auth)
				 (or (find-authenticated-user auth)
				     (anonymous-user)))
			    (and (string? pauth)
				 (or (find-authenticated-user pauth)
				     (anonymous-user)))
			    (anonymous-user)))))))))

;*---------------------------------------------------------------------*/
;*    http-file-name-canonicalize! ...                                 */
;*---------------------------------------------------------------------*/
(define (http-file-name-canonicalize! path)
   (let ((len (string-length path)))
      (let loop ((i 0))
	 (cond
	    ((=fx i len)
	     (file-name-canonicalize! path))
	    ((char=? (string-ref path i) #\?)
	     (string-append (file-name-canonicalize! (substring path 0 i))
			    (substring path i len)))
	    (else
	     (loop (+fx i 1)))))))
   
;*---------------------------------------------------------------------*/
;*    http-version-grammar ...                                         */
;*---------------------------------------------------------------------*/
(define http-version-grammar
   (regular-grammar ((DIGIT (in ("09"))))
      ((: "HTTP/" (+ DIGIT) "." (+ DIGIT))
       (the-string))
      (else
       (parse-error 'http-version-grammar "Illegal character" (the-failure)))))

;*---------------------------------------------------------------------*/
;*    http-sp-grammar ...                                              */
;*---------------------------------------------------------------------*/
(define http-sp-grammar
   (regular-grammar ((SP #\Space))
      (SP
       'sp)
      (else
       (parse-error 'sp-grammar "Illegal character" (the-failure)))))
      
