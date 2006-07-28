;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/hop.scm                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 25 15:30:55 2004                          */
;*    Last change :  Fri Jul 28 14:59:35 2006 (serrano)                */
;*    Copyright   :  2004-06 Manuel Serrano                            */
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
	    __hop_http-error)
   
   (with    __hop_hop-notepad
	    __hop_hop-inline
	    __hop_hop-paned
	    __hop_hop-slider
	    __hop_hop-tabslider
	    __hop_hop-tree
	    __hop_hop-extra
	    __hop_hop-foldlist
	    __hop_event)
   
   (export  (the-current-request::obj)
	    (hop::%http-response ::http-request)
	    (hop-to-hop ::bstring ::int ::obj ::hop-service . ::obj)
	    (with-url ::bstring ::procedure #!key (fail raise) (header '()))
	    (with-remote-host ::bstring ::hop-service ::pair-nil ::procedure ::procedure)
	    (generic with-hop-response obj proc fail)))

;*---------------------------------------------------------------------*/
;*    the-current-request ...                                          */
;*---------------------------------------------------------------------*/
(define (the-current-request)
   (let ((d (thread-data (hop-current-thread))))
      (if (http-request? d)
	  d
	  #f)))

;*---------------------------------------------------------------------*/
;*    hop ...                                                          */
;*---------------------------------------------------------------------*/
(define (hop req::http-request)
   (let loop ((m req)
	      (filters (hop-filters)))
      (if (null? filters)
	  (with-access::http-request m (content-length method path)
	     (let* ((n (if (hop-enable-proxing)
			   (instantiate::http-response-remote
			      (method (http-request-method m))
			      (host (http-request-host m))
			      (port (http-request-port m))
			      (path (http-request-path m))
			      (userinfo (http-request-userinfo m))
			      (encoded-path (http-request-encoded-path m))
			      (http (http-request-http m))
			      (header (http-request-header m))
			      (scheme (http-request-scheme m))
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
		 (mutex-lock! (hop-filter-mutex))
		 (let ((tail (cdr filters)))
		    (mutex-unlock! (hop-filter-mutex))
		    (loop n tail)))
		(else
		 (mutex-lock! (hop-filter-mutex))
		 (let ((tail (cdr filters)))
		    (mutex-unlock! (hop-filter-mutex))
		    (loop m tail))))))))

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
;*    hop-to-hop-id ...                                                */
;*---------------------------------------------------------------------*/
(define hop-to-hop-id -1)

;*---------------------------------------------------------------------*/
;*    hop-to-hop ...                                                   */
;*    -------------------------------------------------------------    */
;*    This form is obsolete. It should not be used. It will be         */
;*    removed from the runtime one day. It is subsumed by WITH-HOP     */
;*    (see service.sch).                                               */
;*---------------------------------------------------------------------*/
(define (hop-to-hop host port userinfo service . opts)
   (set! hop-to-hop-id (-fx hop-to-hop-id 1))
   (hop-verb 1 (hop-color hop-to-hop-id hop-to-hop-id " HOP-TO-HOP")
	     ": " host ":" port " " (hop-service-id service) " " opts
	     "\n")
   (with-trace 2 'hop-to-hop
      (trace-item "host=" host " port=" port " service=" (hop-service-id service) " opts=" opts)
      (if (or (not (string? host)) (is-local? host))
	  (let ((resp (apply (hop-service-proc service) opts)))
	     (if (http-response-obj? resp)
		 (http-response-obj-body resp)
		 #unspecified))
	  (let* ((path (apply make-hop-service-url service opts))
		 (req (instantiate::http-request
			 (id hop-to-hop-id)
			 (userinfo userinfo)
			 (host host)
			 (port port)
			 (path path))))
	     (trace-item "remote path=" path)
	     (http-send-request req
				(lambda (status clength p)
				   (case status
				      ((200)
				       (read p))
				      ((401 407)
				       (user-access-denied req))
				      (else
				       (error 'hop-to-hop
					      (format "Illegal status `~a'"
						      status)
					      (read-string p))))))))))

;*---------------------------------------------------------------------*/
;*    make-http-callback ...                                           */
;*---------------------------------------------------------------------*/
(define (make-http-callback req success fail)
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
	  (fail (user-access-denied req)))
	 (else
	  (fail
	   (instantiate::&error
	      (proc 'wih-hop)
	      (msg (format "Illegal status `~a'" status))
	      (obj (read p))))))))

;*---------------------------------------------------------------------*/
;*    with-url ...                                                     */
;*---------------------------------------------------------------------*/
(define (with-url url success #!key (fail raise) (header '()))
   (set! hop-to-hop-id (-fx hop-to-hop-id 1))
   (hop-verb 1 (hop-color hop-to-hop-id hop-to-hop-id " WITH-URL")
	     ": " url "\n")
   (with-trace 2 'with-url
      (trace-item "url=" url)
      (trace-item "header=" header)
      (multiple-value-bind (_ userinfo host port path)
	 (url-parse url)
	 (let ((r (instantiate::http-request
		       (id hop-to-hop-id)
		       (userinfo userinfo)
		       (host host)
		       (port port)
		       (header header)
		       (path path))))
	    (trace-item "remote path=" path)
	    (http-send-request r (make-http-callback r success fail))))))
   
;*---------------------------------------------------------------------*/
;*    with-remote-host ...                                             */
;*---------------------------------------------------------------------*/
(define (with-remote-host url service args success fail)
   (set! hop-to-hop-id (-fx hop-to-hop-id 1))
   (hop-verb 1 (hop-color hop-to-hop-id hop-to-hop-id " WITH-HOP")
	     ": " url ":" (hop-service-id service) "\n")
   (with-trace 2 'with-hop
      (trace-item "url=" url " service=" (hop-service-id service))
      (multiple-value-bind (_ userinfo host port path)
	 (url-parse url)
	 (if (and (is-local? host) (=fx port (hop-port)))
	     (with-hop-response (apply (hop-service-proc service) args)
				success fail)
	     (let* ((path (apply make-hop-service-url service args))
		    (r (instantiate::http-request
			  (id hop-to-hop-id)
			  (userinfo userinfo)
			  (host host)
			  (port port)
			  (path path))))
		(trace-item "remote path=" path)
		(http-send-request r (make-http-callback r success fail)))))))

;*---------------------------------------------------------------------*/
;*    with-hop-response ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (with-hop-response obj success fail)
   (if (response-is-xml? obj)
       (with-hop-response-xml obj #f success)
       (success obj)))

;*---------------------------------------------------------------------*/
;*    with-hop-response-xml ...                                        */
;*---------------------------------------------------------------------*/
(define (with-hop-response-xml obj encoding success)
   (let ((s (with-output-to-string
	       (lambda ()
		  (xml-write obj
			     (current-output-port)
			     (or encoding (hop-char-encoding)))))))
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
			  success))

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
   
