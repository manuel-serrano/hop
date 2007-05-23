;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/hop.scm                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 25 15:30:55 2004                          */
;*    Last change :  Wed May 23 15:02:09 2007 (serrano)                */
;*    Copyright   :  2004-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP engine.                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop

   (library web)
   
   (import  __hop_param
	    __hop_types
	    __hop_misc
	    __hop_thread
	    __hop_user
	    __hop_service
	    __hop_http-response
	    __hop_js-lib
	    __hop_xml
	    __hop_http-error
	    __hop_http-lib)
   
   (with    __hop_hop-notepad
	    __hop_hop-inline
	    __hop_hop-paned
	    __hop_hop-slider
	    __hop_hop-tabslider
	    __hop_hop-tree
	    __hop_hop-extra
	    __hop_hop-foldlist
	    __hop_event)
   
   (export  (current-request::obj)
	    (request-get::obj ::symbol)
	    (request->response::%http-response ::http-request)
	    (with-url ::bstring ::obj #!key fail (header '()))
	    (with-remote-host ::bstring ::hop-service ::pair-nil ::obj ::obj)
	    (generic with-hop-response obj proc fail)
	    (hop-get-file::obj ::bstring)))

;*---------------------------------------------------------------------*/
;*    current-request ...                                              */
;*---------------------------------------------------------------------*/
(define (current-request)
   (let ((d (thread-data (hop-current-thread))))
      (if (http-request? d)
	  d
	  #f)))

;*---------------------------------------------------------------------*/
;*    request-get ...                                                  */
;*---------------------------------------------------------------------*/
(define (request-get key)
   (let ((req (current-request)))
      (if req
	  (with-access::http-request req (%env)
	     (unless %env (set! %env (request-env-parse req)))
	     (let ((c (assq key %env)))
		(if (not (pair? c))
		    #unspecified
		    (cdr c))))
	  #unspecified)))

;*---------------------------------------------------------------------*/
;*    request-env-parse ...                                            */
;*---------------------------------------------------------------------*/
(define (request-env-parse req)
   (with-access::http-request req (header)
      (let ((env (http-header-field header hop-share:)))
	 (if (string? env)
	     (string->obj (xml-string-decode env))
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
(define (request->response req::http-request)
   (let loop ((m req)
	      (filters (hop-filters)))
      (if (null? filters)
	  (with-access::http-request m (content-length method path)
	     (let* ((n (if (hop-enable-proxing)
			   (instantiate::http-response-remote
			      (scheme (http-request-scheme m))
			      (method (http-request-method m))
			      (host (http-request-host m))
			      (port (http-request-port m))
			      (path (http-request-path m))
			      (userinfo (http-request-userinfo m))
			      (encoded-path (http-request-encoded-path m))
			      (http (http-request-http m))
			      (header (http-request-header m))
			      (bodyp (not (eq? method 'HEAD)))
			      (content-length content-length)
			      (request req)
			      (remote-timeout (hop-read-timeout))
			      (connection-timeout (hop-read-timeout)))
			   (http-file-not-found path)))
		    (r (hop-run-hook (hop-http-response-remote-hooks) m n)))
		(hop-request-hook m r)))
	  (let ((n ((cdar filters) m)))
	     (cond
		((eq? n 'hop-resume)
		 (loop m (hop-filters)))
		((%http-response? n)
		 (let ((r (hop-run-hook (hop-http-response-local-hooks) m n)))
		    (hop-request-hook m r)))
		((http-request? n)
		 (loop n (cdr filters)))
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
      ((not (http-request? req))
       (error 'hop-request-hook "Illegal request" req))
      ((not (%http-response? rep))
       (error 'hop-request-hook "Illegal response" rep))
      (else
       (let* ((rep2 ((http-request-hook req) rep))
	      (res (if (%http-response? rep2) rep2 rep)))
	  (%http-response-request-set! res req)
	  res))))

;*---------------------------------------------------------------------*/
;*    make-http-callback ...                                           */
;*---------------------------------------------------------------------*/
(define (make-http-callback proc::symbol req success fail)
   (lambda (status clength p)
      (when (>elong clength #e0)
	 (input-port-fill-barrier-set! p (elong->fixnum clength)))
      (case status
	 ((200)
	  (success (read-string p)))
	 ((201)
	  (success (json->hop p)))
	 ((202)
	  (success (string->obj (read p))))
	 ((401 407)
	  (if (procedure? fail)
	      (fail status p)
	      (raise (user-access-denied req))))
	 (else
	  (if (procedure? fail)
	      (fail status p)
	      (raise
	       (instantiate::&error
		  (proc proc)
		  (msg (format "Illegal status `~a'" status))
		  (obj (read-string p)))))))))

;*---------------------------------------------------------------------*/
;*    hop-to-hop-id ...                                                */
;*---------------------------------------------------------------------*/
(define hop-to-hop-id -1)

;*---------------------------------------------------------------------*/
;*    with-url  ...                                                    */
;*---------------------------------------------------------------------*/
(define (with-url url success #!key fail (header '()))
   (set! hop-to-hop-id (-fx hop-to-hop-id 1))
   (hop-verb 1 (hop-color hop-to-hop-id hop-to-hop-id " WITH-URL")
	     ": " url "\n")
   (with-trace 2 'with-url
      (trace-item "url=" url)
      (trace-item "header=" header)
      (cond
	 ((and (procedure? fail) (not (correct-arity? fail 2)))
	  (error 'with-url "Illegal fail handler" fail))
	 ((and (procedure? success) (not (correct-arity? success 1)))
	  (error 'with-url "Illegal success handler" success))
	 (else
	  (multiple-value-bind (scheme userinfo host port path)
	     (url-parse url)
	     (let ((r (instantiate::http-request
			 (scheme (string->symbol scheme))
			 (id hop-to-hop-id)
			 (userinfo userinfo)
			 (host host)
			 (port port)
			 (header header)
			 (path path)))
		   (suc (if (procedure? success) success (lambda (x) x))))
		(trace-item "remote path=" path)
		(http-send-request
		 r (make-http-callback 'with-url r suc fail))))))))
   
;*---------------------------------------------------------------------*/
;*    with-remote-host ...                                             */
;*---------------------------------------------------------------------*/
(define (with-remote-host url service args success fail)
   (set! hop-to-hop-id (-fx hop-to-hop-id 1))
   (hop-verb 1 (hop-color hop-to-hop-id hop-to-hop-id " WITH-HOP")
	     ": " url ":" (hop-service-id service) "\n")
   (with-trace 2 'with-hop
      (trace-item "url=" url " service=" (hop-service-id service))
      (cond
	 ((and (procedure? fail) (not (correct-arity? fail 2)))
	  (error 'with-hop "Illegal fail handler" fail))
	 ((and (procedure? success) (not (correct-arity? success 1)))
	  (error 'with-hop "Illegal success handler" success))
	 (else
	  (multiple-value-bind (_ userinfo host port path)
	     (url-parse url)
	     (let ((suc (if (procedure? success) success (lambda (x) x))))
		(if (and (is-local? host) (=fx port (hop-port)))
		    (with-hop-response (apply (hop-service-proc service) args)
				       suc fail)
		    (let* ((path (apply make-hop-service-url service args))
			   (r (instantiate::http-request
				 (id hop-to-hop-id)
				 (userinfo userinfo)
				 (host host)
				 (port port)
				 (path path))))
		       (trace-item "remote path=" path)
		       (http-send-request
			r (make-http-callback 'with-hop r suc fail))))))))))

;*---------------------------------------------------------------------*/
;*    with-hop-response ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (with-hop-response obj success fail)
   (if (response-is-xml? obj)
       (with-hop-response-xml obj #f success (hop-xml-backend))
       (success obj)))

;*---------------------------------------------------------------------*/
;*    with-hop-response-xml ...                                        */
;*---------------------------------------------------------------------*/
(define (with-hop-response-xml obj encoding success backend)
   (let ((s (with-output-to-string
	       (lambda ()
		  (xml-write obj
			     (current-output-port)
			     (or encoding (hop-char-encoding))
			     backend)))))
      (success s)))

;*---------------------------------------------------------------------*/
;*    with-hop-response ::http-response-authentication ...             */
;*---------------------------------------------------------------------*/
(define-method (with-hop-response o::http-response-authentication success fail)
   (fail (instantiate::&error
	       (proc 'with-hop)
	       (msg "Authentication required")
	       (obj o))))
   
;*---------------------------------------------------------------------*/
;*    with-hop-response ::http-response-string ...                     */
;*---------------------------------------------------------------------*/
(define-method (with-hop-response obj::http-response-string success fail)
   (success (http-response-string-body obj)))

;*---------------------------------------------------------------------*/
;*    with-hop-response ::http-response-js ...                         */
;*---------------------------------------------------------------------*/
(define-method (with-hop-response obj::http-response-js success fail)
   (success (http-response-js-value obj)))

;*---------------------------------------------------------------------*/
;*    with-hop-response ::http-response-hop ...                        */
;*---------------------------------------------------------------------*/
(define-method (with-hop-response obj::http-response-hop success fail)
   (with-hop-response-xml (http-response-hop-xml obj)
			  (http-response-hop-char-encoding obj)
			  success
			  (http-response-hop-backend obj)))

;*---------------------------------------------------------------------*/
;*    with-hop-response ::http-response-procedure ...                  */
;*---------------------------------------------------------------------*/
(define-method (with-hop-response obj::http-response-procedure success fail)
   (success (with-output-to-string (http-response-procedure-proc obj))))

;*---------------------------------------------------------------------*/
;*    with-hop-response ::http-response-file ...                       */
;*---------------------------------------------------------------------*/
(define-method (with-hop-response obj::http-response-file success fail)
   (let* ((f (http-response-file-file obj))
	  (pf (open-input-file f)))
      (if (not (input-port? pf))
	  (fail
	   (if (not (file-exists? f))
	       (instantiate::&io-file-not-found-error
		  (proc 'with-hop)
		  (msg "File not found")
		  (obj f))
	       (instantiate::&io-port-error
		  (proc 'with-hop)
		  (msg "Cannot open file")
		  (obj f))))
	  (unwind-protect
	     (success (read-string pf))
	     (close-input-port pf)))))

;*---------------------------------------------------------------------*/
;*    with-hop-response ::http-response-cgi ...                        */
;*---------------------------------------------------------------------*/
(define-method (with-hop-response obj::http-response-cgi success fail)
   (fail (instantiate::&error
	    (proc 'with-response)
	    (msg "Illegal response")
	    (obj obj))))

;*---------------------------------------------------------------------*/
;*    with-hop-response ::http-response-put ...                        */
;*---------------------------------------------------------------------*/
(define-method (with-hop-response obj::http-response-put success fail)
   (fail (instantiate::&error
	    (proc 'with-response)
	    (msg "Illegal response")
	    (obj obj))))

;*---------------------------------------------------------------------*/
;*    with-hop-response ::http-response-remote ...                     */
;*---------------------------------------------------------------------*/
(define-method (with-hop-response obj::http-response-remote success fail)
   (with-access::http-response-remote obj (host port path userinfo)
      (let ((req (instantiate::http-request
		    (userinfo userinfo)
		    (host host)
		    (port port)
		    (path path))))
	 (http-send-request req
			    (lambda (status clength p)
			       (case status
				  ((200)
				   (success (read-string p)))
				  ((401 407)
				   (fail (instantiate::&error
					    (proc 'with-hop)
					    (msg "Access denied")
					    (obj req))))
				  (else
				   (fail (instantiate::&error
					    (proc 'with-hop)
					    (msg (format "Illegal status ~a"
							 status))
					    (obj (read-string )))))))))))

;*---------------------------------------------------------------------*/
;*    with-hop-response ::http-response-filter ...                     */
;*---------------------------------------------------------------------*/
(define-method (with-hop-response obj::http-response-filter success fail)
   (fail (instantiate::&error
	    (proc 'with-response)
	    (msg "Illegal response")
	    (obj obj))))

;*---------------------------------------------------------------------*/
;*    with-hop-response ::http-response-persistent ...                 */
;*---------------------------------------------------------------------*/
(define-method (with-hop-response o::http-response-persistent success fail)
   (success o))
   
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
;*    :inline option of HEAD, LINK, and SCRIPT markups.                */
;*---------------------------------------------------------------------*/
(define (hop-get-file path)
   (let* ((req (instantiate::http-request
		  (localhostp #t)
		  (localclientp #t)
		  (path path)))
	  (rep (request->response req)))
      (cond
	 ((http-response-file? rep)
	  (with-access::http-response-file rep (file)
	     (let ((p (open-input-file file)))
		(if (input-port? p)
		    (unwind-protect
		       (read-string p)
		       (close-input-port p))
		    #f))))
	 ((http-response-procedure? rep)
	  (let ((p (open-output-string)))
	     (with-access::http-response-procedure rep (proc)
		(proc p)
		(close-output-port p))))
	 (else
	  #f))))
   
