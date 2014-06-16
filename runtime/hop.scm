;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/hop.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 25 15:30:55 2004                          */
;*    Last change :  Thu Jun 12 15:42:03 2014 (serrano)                */
;*    Copyright   :  2004-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP engine.                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop

   (library web)

   (include "verbose.sch")
   
   (import  __hop_param
	    __hop_types
	    __hop_misc
	    __hop_user
	    __hop_service
	    __hop_http-response
	    __hop_js-comp
	    __hop_json
	    __hop_xml-types
	    __hop_http-error
	    __hop_http-lib
	    __hop_weblets
	    __hop_xml)
   
   (with    __hop_event)
   
   (export  (generic thread-request ::obj)
	    (generic thread-request-set! ::obj ::obj)
	    (inline current-request::obj)
	    (inline current-request-set! ::thread ::obj)
	    (anonymous-request::http-request)
	    (request-get::obj ::symbol)
	    (request->response::%http-response ::http-request ::obj)
	    (with-url ::bstring ::obj
		      #!key
		      fail
		      (header '())
		      (timeout 0)
		      (method 'GET)
		      body
		      parse-json)
	    (with-hop-remote path success failure
			     #!key
			     (host "localhost")
			     (port (hop-port))
			     (abspath #f)
			     (user #f)
			     (password #f)
			     (authorization #f)
			     (anim #f)
			     parse-json)
	    (generic with-hop-local obj success fail authorization)
	    (hop-get-file::obj ::bstring ::obj)))

;*---------------------------------------------------------------------*/
;*    *anonymous-request* ...                                          */
;*---------------------------------------------------------------------*/
(define *anonymous-request* #f)

;*---------------------------------------------------------------------*/
;*    anonymous-request ...                                            */
;*---------------------------------------------------------------------*/
(define (anonymous-request)
   (unless (isa? *anonymous-request* http-request)
      (set! *anonymous-request*
	    (instantiate::http-server-request
	       (http 'HTTP/1.0)
	       (connection 'close)
	       (user (anonymous-user)))))
   *anonymous-request*)

;*---------------------------------------------------------------------*/
;*    *current-request* ...                                            */
;*---------------------------------------------------------------------*/
(define *current-request* #f)

;*---------------------------------------------------------------------*/
;*    thread-request ::obj ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (thread-request th)
   *current-request*)

;*---------------------------------------------------------------------*/
;*    thread-request-set! ::obj ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (thread-request-set! th::obj req)
   (set! *current-request* req))

;*---------------------------------------------------------------------*/
;*    current-request ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (current-request)
   (thread-request (current-thread)))

;*---------------------------------------------------------------------*/
;*    current-request-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (current-request-set! thread req)
   (thread-request-set! thread req))

;*---------------------------------------------------------------------*/
;*    request-get ...                                                  */
;*---------------------------------------------------------------------*/
(define (request-get key)
   (let ((req (current-request)))
      (let loop ()
	 (cond
	    ((isa? req http-server-request+)
	     (with-access::http-server-request+ req (%env)
		(let ((c (assq key %env)))
		   (if (not (pair? c))
		       #unspecified
		       (cdr c)))))
	    ((isa? req http-server-request)
	     (widen!::http-server-request+ req
		(%env (request-env-parse req)))
	     (loop))
	    (else
	     #unspecified)))))

;*---------------------------------------------------------------------*/
;*    request-env-parse ...                                            */
;*---------------------------------------------------------------------*/
(define (request-env-parse req)
   (with-access::http-request req (header)
      (let ((env (http-header-field header hop-env:)))
	 (if (string? env)
	     (string->obj (url-decode env))
	     '()))))

;*---------------------------------------------------------------------*/
;*    request->response ...                                            */
;*    -------------------------------------------------------------    */
;*    This function assumes that (HOP-FILTERS) returns a read-only     */
;*    immutable data structure. In other words, it assumes that no     */
;*    other thread can change the list (HOP-FILTERS) in the            */
;*    background. Because of this assumption, no lock is needed in     */
;*    this function.                                                   */
;*---------------------------------------------------------------------*/
(define (request->response req::http-request thread)
   (let loop ((m req)
	      (filters (hop-filters)))
      (if (null? filters)
	  (with-access::http-request m (content-length method path host port
					  user header userinfo scheme http)
	     (if (or (not (isa? req http-proxy-request))
		     (not (hop-enable-proxing)))
		 (hop-request-hook m (http-file-not-found path))
		 (let* ((n (instantiate::http-response-proxy
			      (scheme scheme)
			      (method method)
			      (host host)
			      (port port)
			      (path path)
			      (userinfo userinfo)
			      (http http)
			      (header header)
			      (bodyp (not (eq? method 'HEAD)))
			      (content-length content-length)
			      (request m)
			      (remote-timeout (hop-read-timeout))
			      (connection-timeout (hop-connection-timeout))))
			(r (hop-run-hook (hop-http-response-proxy-hooks) m n)))
		    (hop-request-hook m r))))
	  (let ((n ((cdar filters) m)))
	     (cond
		((isa? n %http-response)
		 (let ((r (hop-run-hook (hop-http-response-server-hooks) m n)))
		    (hop-request-hook m r)))
		((eq? n m)
		 (loop m (cdr filters)))
		((isa? n http-request)
		 (current-request-set! thread n)
		 (loop n (cdr filters)))
		((eq? n 'hop-resume)
		 (loop m (hop-filters)))
		(else
		 (loop m (cdr filters))))))))

;*---------------------------------------------------------------------*/
;*    hop-run-hooks ...                                                */
;*---------------------------------------------------------------------*/
(define (hop-run-hook hooks m rp)
   (let loop ((hooks hooks)
	      (rp rp))
      (if (null? hooks)
	  rp
	  (loop (cdr hooks) ((car hooks) m rp)))))

;*---------------------------------------------------------------------*/
;*    hop-request-hook ...                                             */
;*    -------------------------------------------------------------    */
;*    Execute the request hook and set the response's request field.   */
;*---------------------------------------------------------------------*/
(define (hop-request-hook::%http-response req rep)
   (cond
      ((not (isa? req http-request))
       (error "hop-request-hook" "Illegal request" req))
      ((not (isa? rep %http-response))
       (error "hop-request-hook" "Illegal response" rep))
      (else
       (with-access::http-request req (hook)
	  (let* ((rep2 (hook rep))
		 (res (if (isa? rep2 %http-response) rep2 rep)))
	     (with-access::%http-response res (request)
		(set! request req))
	     res)))))

;*---------------------------------------------------------------------*/
;*    header-content-type ...                                          */
;*---------------------------------------------------------------------*/
(define (header-content-type header)
   (let ((cell (assq content-type: header)))
      (when (and (pair? cell) (string? (cdr cell)))
	 (let* ((str (cdr cell))
		(i (string-index str #\;)))
	    (if i
		(string->symbol (substring str 0 i))
		(string->symbol str))))))

;*---------------------------------------------------------------------*/
;*    byte-array->string ...                                           */
;*---------------------------------------------------------------------*/
(define (byte-array->string v)
   (let* ((len (vector-length v))
	  (s (make-string len)))
      (let loop ((i 0))
	 (when (<fx i len)
	    (string-set! s i (integer->char (vector-ref v i)))
	    (loop (+fx i 1))))
      s))

;*---------------------------------------------------------------------*/
;*    make-http-callback ...                                           */
;*---------------------------------------------------------------------*/
(define (make-http-callback proc::symbol req success fail parse-json)
   (lambda (p status header clength tenc)
      (when (and (input-port? p) (>elong clength #e0))
	 (input-port-fill-barrier-set! p (elong->fixnum clength)))
      (case status
	 ((200)
	  ;; see hop-json-mime-type and hop-bigloo-mime-type
	  (let ((obj (case (header-content-type header)
			((application/x-hop)
			 (string->obj (read-chars clength p)))
			((application/x-url-hop)
			 (string->obj (url-decode (read-chars clength p))))
			((application/x-json-hop)
			 (string->obj
			    (byte-array->string 
			       (javascript->obj (read-chars clength p)))))
			((application/json)
			 ((or parse-json javascript->obj) p))
			((application/x-javascript)
			 (javascript->obj p))
			((text/html application/xhtml+xml)
			 (car (last-pair (parse-html p (elong->fixnum clength)))))
			(else
			 (read-string p)))))
	     (success obj)))
	 ((201 204 304)
	  ;; no message body
	  (success (instantiate::xml-http-request
		      (status status)
		      (header header)
		      (input-port p))))
	 ((401 407)
	  (if (procedure? fail)
	      (fail (instantiate::xml-http-request
		       (status status)
		       (header header)
		       (input-port p)))
	      (raise (user-access-denied req))))
	 (else
	  (if (procedure? fail)
	      (fail (instantiate::xml-http-request
		       (status status)
		       (header header)
		       (input-port p)))
	      (raise
		 (instantiate::&error
		    (proc proc)
		    (msg (format "Illegal status `~a'" status))
		    (obj (when (input-port? p) (read-string p))))))))))

;*---------------------------------------------------------------------*/
;*    hop-to-hop-id ...                                                */
;*---------------------------------------------------------------------*/
(define hop-to-hop-id -1)

;*---------------------------------------------------------------------*/
;*    scheme-default-port ...                                          */
;*---------------------------------------------------------------------*/
(define (scheme-default-port scheme)
   (case scheme
      ((https) 443)
      (else 80)))
   
;*---------------------------------------------------------------------*/
;*    with-url  ...                                                    */
;*---------------------------------------------------------------------*/
(define (with-url url success #!key fail (header '()) (timeout 0) (method 'GET)
		  parse-json body)
   (set! hop-to-hop-id (-fx hop-to-hop-id 1))
   (hop-verb 1 (hop-color hop-to-hop-id hop-to-hop-id " WITH-URL")
	     ": " url "\n")
   (with-trace 2 "with-url"
      (trace-item "url=" url)
      (trace-item "header=" header)
      (cond
	 ((and (procedure? fail) (not (correct-arity? fail 1)))
	  (error "with-url" "Illegal fail handler" fail))
	 ((and (procedure? success) (not (correct-arity? success 1)))
	  (error "with-url" "Illegal success handler" success))
	 (else
	  (multiple-value-bind (scheme userinfo host port path)
	     (url-parse url)
	     (cond
		((or (string=? scheme "file")
		     (string=? scheme "string")
		     (string=? scheme "| ")
		     (string=? scheme "pipe")
		     (string=? scheme "gzip")
		     (string=? scheme "/resource/"))
		 (let ((p (open-input-file url)))
		    (if (input-port? p)
			(let ((s (read-string p))
			      (suc (if (procedure? success)
				       success
				       (lambda (x) x))))
			   (close-input-port p)
			   (suc s))
			(if (procedure? fail)
			    (fail url)
			    (raise (instantiate::&io-error
				      (proc "with-url")
				      (msg "Cannot open url")
				      (obj url)))))))
		(else
		 (let* ((s (string->symbol scheme))
			(r (instantiate::http-server-request
			      (user (class-nil user))
			      (scheme s)
			      (id hop-to-hop-id)
			      (userinfo userinfo)
			      (host (or host path))
			      (port (or port (scheme-default-port s)))
			      (header header)
			      (connection-timeout timeout)
			      (timeout timeout)
			      (method method)
			      (path (if host path "/"))))
			(suc (if (procedure? success) success (lambda (x) x)))
			(hdl (make-http-callback 'with-url r suc fail
				parse-json)))
		    (trace-item "remote path=" path)
		    (http-send-request r hdl body)))))))))

;*---------------------------------------------------------------------*/
;*    with-hop-remote ...                                              */
;*---------------------------------------------------------------------*/
(define (with-hop-remote path success fail
	   #!key
	   (host "localhost")
	   (port (hop-port))
	   (abspath #f)
	   (user #f)
	   (password #f)
	   (authorization #f)
	   (anim #f)
	   parse-json)
   (set! hop-to-hop-id (-fx hop-to-hop-id 1))
   (hop-verb 1 (hop-color hop-to-hop-id hop-to-hop-id " WITH-HOP")
      ": " path "\n")
   (with-trace 2 "with-hop"
      (trace-item "host=" host " port=" port " path=" path " abspath=" abspath)
      (trace-item "authorization=" authorization)
      (cond
	 ((and (procedure? fail) (not (correct-arity? fail 1)))
	  (error "with-hop" "Illegal fail handler" fail))
	 ((and (procedure? success) (not (correct-arity? success 1)))
	  (error "with-hop" "Illegal success handler" success))
	 (else
	  (let* ((req (instantiate::http-server-request
			 (userinfo (when (and (string? user) (string? password))
				      (string-append user ":" password)))
			 (user (anonymous-user))
			 (id hop-to-hop-id)
			 (host host)
			 (port port)
			 (connection 'close)
			 (header '((hop-serialize: . "arraybuffer")))
			 (authorization authorization)
			 (path (or abspath path))))
		 (suc (or success (lambda (x) x)))
		 (hdl (make-http-callback 'with-hop req suc fail parse-json)))
	     (trace-item "remote path=" path)
	     (http-send-request req hdl))))))

;*---------------------------------------------------------------------*/
;*    fail-or-raise ...                                                */
;*---------------------------------------------------------------------*/
(define (fail-or-raise fail exc)
   (if (procedure? fail)
       (fail exc)
       (raise exc)))

;*---------------------------------------------------------------------*/
;*    with-hop-local ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (with-hop-local obj success fail auth)
   (when (procedure? success) (success obj)))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-proxy ...                         */
;*    -------------------------------------------------------------    */
;*    This method is used for imported services. These services        */
;*    are called locally but they are still remote.                    */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-proxy success fail auth)
   (with-access::http-response-proxy obj (path host port)
      (with-hop-remote path success fail
	 :host host :port port :authorization auth)))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-autoload ...                      */
;*    -------------------------------------------------------------    */
;*    This method is used for imported local services (without body).  */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-autoload success fail auth)
   (with-access::http-response-autoload obj (request)
      (with-access::http-request request (path)
	 (let ((rep (service-filter request)))
	    (if (isa? rep %http-response)
		(with-hop-local rep success fail auth)
		(error "with-hop" "Bad auto-loaded local service" path))))))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::xml ...                                         */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::xml success fail auth)
   (when (procedure? success) (success obj)))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-authentication ...                */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local o::http-response-authentication success fail aut)
   (when (procedure? success)
      (success (instantiate::&error
		  (proc "with-hop")
		  (msg "Authentication required")
		  (obj o)))))
   
;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-string ...                        */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-string success fail auth)
   (when (procedure? success)
      (with-access::http-response-string obj (body)
	 (success body))))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-hop ...                           */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-hop success fail auth)
   (when (procedure? success)
      (with-access::http-response-hop obj (value)
	 (success value))))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-xml ...                           */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-xml success fail auth)
   (when (procedure? success)
      (with-access::http-response-xml obj (xml)
	 (success xml))))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-procedure ...                     */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-procedure success fail auth)
   (when (procedure? success)
      (with-access::http-response-procedure obj (proc)
	 (success (with-output-to-string proc)))))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-file ...                          */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-file success fail auth)
   (with-access::http-response-file obj (file)
      (let ((pf (open-input-file file)))
	 (if (not (input-port? pf))
	     (fail-or-raise
		fail
		(if (not (file-exists? file))
		    (instantiate::&io-file-not-found-error
		       (proc "with-hop")
		       (msg "File not found")
		       (obj file))
		    (instantiate::&io-port-error
		       (proc "with-hop")
		       (msg "Cannot open file")
		       (obj file))))
	     (unwind-protect
		(when (procedure? success) (success (read-string pf)))
		(close-input-port pf))))))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-cgi ...                           */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-cgi success fail auth)
   (fail-or-raise
    fail
    (instantiate::&error
       (proc "with-response")
       (msg "Illegal response")
       (obj obj))))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-put ...                           */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-put success fail auth)
   (fail-or-raise
    fail
    (instantiate::&error
       (proc "with-response")
       (msg "Illegal response")
       (obj obj))))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-filter ...                        */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local obj::http-response-filter success fail auth)
   (fail-or-raise
    fail 
    (instantiate::&error
       (proc "with-response")
       (msg "Illegal response")
       (obj obj))))

;*---------------------------------------------------------------------*/
;*    with-hop-local ::http-response-persistent ...                    */
;*---------------------------------------------------------------------*/
(define-method (with-hop-local o::http-response-persistent success fail auth)
   (when (procedure? success) (success o)))

;*---------------------------------------------------------------------*/
;*    hop-get-file ...                                                 */
;*    -------------------------------------------------------------    */
;*    This function is used by Hop to ask itself files. That is, when  */
;*    a weblet has to read a file as it was provided by the            */
;*    broker it can't simply open it because the broker may have       */
;*    defined filters that tells how to server that very file.         */
;*    The function HOP-GET-FILE simulates a user request to            */
;*    the broker.                                                      */
;*    This function has been first added for implementing the          */
;*    :inline option of HEAD, LINK, and SCRIPT markups. Since version  */
;*    1.9.0, it is only used in the SCRIPT markup.                     */
;*---------------------------------------------------------------------*/
(define (hop-get-file path thread)
   (let* ((reqi (current-request))
	  (req (instantiate::http-server-request
		  (localclientp #t)
		  (lanclientp #t)
		  (user (if (isa? reqi http-request)
			    (with-access::http-request reqi (user) user)
			    (anonymous-user)))
		  (path path)))
	  (rep (request->response req thread)))
      (cond
	 ((isa? rep http-response-file)
	  (with-access::http-response-file rep (file)
	     (let ((p (open-input-file file)))
		(if (input-port? p)
		    (unwind-protect
		       (read-string p)
		       (close-input-port p))
		    #f))))
	 ((isa? rep http-response-procedure)
	  (let ((p (open-output-string)))
	     (with-access::http-response-procedure rep (proc)
		(unwind-protect
		   (proc p)
		   (close-output-port p)))))
	 ((isa? rep http-response-string)
	  (with-access::http-response-string rep (body)
	     body))
	 (else
	  #f))))
   
