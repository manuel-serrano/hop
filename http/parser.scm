;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/http/parser.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:55:24 2004                          */
;*    Last change :  Thu Jun  6 07:46:54 2024 (serrano)                */
;*    Copyright   :  2004-24 Manuel Serrano                            */
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
(module __http_parser

   (library web)

   (import  __http_types)
   
   (export  (http-parse-request::http-request ::socket ::int)))


;*---------------------------------------------------------------------*/
;*    parse-error ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-error proc msg obj socket port)
   (let ((o (if (eof-object? obj)
		obj
		(let ((l (read-line port)))
		   (if (eof-object? l)
		       obj
		       (format "{~a}~a" obj (string-for-read l)))))))
      (raise (instantiate::&io-parse-error
		(obj o)
		(proc (format "request-line-grammar(~a)"
			 (socket-hostname socket)))
		(msg msg)))))

;*---------------------------------------------------------------------*/
;*    http-parse-request ...                                           */
;*---------------------------------------------------------------------*/
(define (http-parse-request sock id)
   (let ((port (socket-input sock)))
      (let ((req (read/rp request-line-grammar port id sock)))
	 (with-access::http-request req (socket)
	    (set! socket sock)
	    req))))

;*---------------------------------------------------------------------*/
;*    request-eof ...                                                  */
;*---------------------------------------------------------------------*/
(define request-eof-exception
   (instantiate::&io-parse-error
      (obj beof)
      (proc "request-line-grammar")
      (msg "Illegal premature end-of-file")))

;*---------------------------------------------------------------------*/
;*    request-line-grammar ...                                         */
;*---------------------------------------------------------------------*/
(define request-line-grammar
   (regular-grammar ((SP #\Space)
		     (CRLF "\r\n")
		     id
		     socket)
      ((: (+ (in ("AZ"))) SP)
       ;; HTTP requests
       (http-parse-method-request (the-subsymbol 0 -1) (the-port) socket id))
      ((: "<" (+ (in "policyferqust" #\-)) (* SP) "/>" #a000)
       ;; Flash authentication requests
       (http-parse-policy-file-request id (the-string) (the-port)))
      ((: (out #\< SP) (+ (out SP)) SP)
       ;; Illegal (parsed) requests
       (let ((s (if (>fx (the-length) 10)
		    (string-append (the-substring 0 10) "...")
		    (the-string))))
	  (raise (instantiate::&io-parse-method-error
		    (proc (format "request-line-grammar(~a)"
			     (socket-hostname socket)))
		    (msg "Method not implemented")
		    (obj (string-for-read s))))))
      (else
       (let ((o (the-failure)))
	  (if (eof-object? o)
	      (raise request-eof-exception)
	      (parse-error "request-line-grammar" "Illegal method"
		 o socket (the-port)))))))

;*---------------------------------------------------------------------*/
;*    http-parse-error-msg ...                                         */
;*---------------------------------------------------------------------*/
(define (http-parse-error-msg c port)
   (if (char? c)
       (let ((line (http-read-line port)))
	  (string-for-read
	     (string-append "{" (string c) "}"
		(if (string? line) (string-for-read line) ""))))
       c))

;*---------------------------------------------------------------------*/
;*    http-parse-method-request ...                                    */
;*---------------------------------------------------------------------*/
(define (http-parse-method-request method pi::input-port socket::socket id)
   (with-trace 3 "http-parse-method-request"
      (let (scheme host port path http-version userinfo)
	 (multiple-value-bind (s u h p a)
	    (http-url-parse pi)
	    (set! scheme (string->symbol s))
	    (set! host h)
	    (set! port p)
	    (set! path a)
	    (set! userinfo u)
	    (set! http-version (read/rp http-version-grammar pi socket))
	    (trace-item "http=" http-version " scheme=" s " user=" u
	       " host=" h " port=" p " path=[" a "]"))
	 (multiple-value-bind (header actual-host actual-port cl te auth pauth co)
	    (http-parse-header pi (socket-output socket))
	    (let* ((i (string-index path #\?))
		   (query #f)
                   (abspath (cond
                               ((not i)
				;; file name canonicalization is needed
				;; for authentication
				(let* ((dpath (uri-decode-component! path))
				       (i (string-index dpath #\?)))
				   (if i
				       (begin
					  (set! query (substring dpath (+fx i 1)))
					  (file-name-canonicalize!
					     (substring dpath 0 i)))
				       (file-name-canonicalize! dpath))))
                               ((>fx i 0)
                                (let ((l (string-length path)))
                                   (set! query (substring path (+fx i 1) l)))
                                (let ((p (url-decode! (substring path 0 i))))
                                   (file-name-canonicalize! p)))
                               (else
                                (let ((l (string-length path)))
                                   (set! query (substring path 1 l)))   
                                "/")))
		   (connection (or co
				   (if (eq? http-version 'HTTP/1.1)
				       'keep-alive
				       'close))))
	       (trace-item "abspath=" abspath
		  " query=" query
		  " connection=" connection
		  " content-length=" cl
		  " header=" header)
	       (let ((ip (input-port-name pi))
		     (port (or actual-port port 80)))
		  (if (string? host)
		      (instantiate::http-proxy-request
			 (id id)
			 (method method)
			 (http http-version)
			 (scheme scheme)
			 (userinfo userinfo)
			 (path path)
			 (abspath abspath)
			 (query query)
			 (header header)
			 (port port)
			 (host (or actual-host host))
			 (content-length cl)
			 (transfer-encoding te)
			 (authorization pauth)
			 (connection connection))
		      (instantiate::http-server-request
			 (id id)
			 (method method)
			 (http http-version)
			 (scheme scheme)
			 (userinfo userinfo)
			 (path path)
			 (abspath abspath)
			 (query query)
			 (header header)
			 (port port)
			 (host (or actual-host (hostname)))
			 (content-length cl)
			 (transfer-encoding te)
			 (authorization auth)
			 (connection connection)))))))))

;*---------------------------------------------------------------------*/
;*    http-parse-policy-file-request ...                               */
;*---------------------------------------------------------------------*/
(define (http-parse-policy-file-request id string port)
   (if (substring-at? string "<policy-file-request" 0)
       ;; This request is emitted by Flash plugins >= 9.0.115.
       ;; This plugins are buggous because they should seek for
       ;; the policy file using the /hop/public/server-event/policy-file.
       ;; In the meantime, Hop also handles the <policy-file-request/>.
       (instantiate::http-server-request
	  (id id)
	  (method 'FLASH-POLICY-FILE)
	  (http 'HTTP/0.0)
	  (scheme 'policy-file-request)
	  (path "<policy-file-request/>")
	  (abspath "<policy-file-request/>")
	  (header '())
	  (port port)
	  (host "localhost")
	  (content-length 10))
       (raise (instantiate::&io-parse-method-error
		 (proc "http-parse-policy-file-request")
		 (msg "policy file request not understood")
		 (obj string)))))

;*---------------------------------------------------------------------*/
;*    http-version-grammar ...                                         */
;*---------------------------------------------------------------------*/
(define http-version-grammar
   (regular-grammar ((DIGIT (in ("09")))
		     (SP (+ #\Space))
		     socket)
      (SP
       (ignore))
      ((: "HTTP/" (+ DIGIT) "." (+ DIGIT) "\n")
       (the-subsymbol 0 -1))
      ((: "HTTP/" (+ DIGIT) "." (+ DIGIT) "\r\n")
       (the-subsymbol 0 -2))
      (else
       (parse-error "http-version-grammar" "Illegal character"
	  (the-failure) socket (the-port)))))
      
