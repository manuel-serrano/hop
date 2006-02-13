;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/hop.scm                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 25 15:30:55 2004                          */
;*    Last change :  Mon Feb 13 17:51:00 2006 (serrano)                */
;*    Copyright   :  2004-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Handling HTTP requests.                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop

   (import  __hop_param
	    __hop_types
	    __hop_misc
	    __hop_thread
	    __hop_user
	    __hop_service
	    __hop_http-response)
   
   (with    __hop_html-notepad
	    __hop_html-inline
	    __hop_html-paned
	    __hop_html-slider
	    __hop_html-tabslider
	    __hop_html-tree
	    __hop_html-extra
	    __hop_event)
   
   (export  (the-current-request::obj)
	    (hop::%http-response ::http-request)
	    (hop-to-hop ::bstring ::int ::obj ::hop-service . ::obj)))

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
	  (with-access::http-request m (content-length method)
	     (let* ((rp (instantiate::http-response-remote
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
			   (timeout (hop-connection-timeout))))
		    (rp2 (hop-run-hook
			  (hop-http-response-remote-hooks) m rp)))
		(hop-request-hook m rp2)))
	  (let ((n ((cdar filters) m)))
	     (cond
		((%http-response? n)
		 (let ((rp2 (hop-run-hook
			     (hop-http-response-local-hooks) m n)))
		    (hop-request-hook m rp2)))
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
	      (res (if (%http-response? rep2)
		       rep2
		       rep)))
	  (%http-response-request-set! res req)
	  res))))

;*---------------------------------------------------------------------*/
;*    hop-to-hop-id ...                                                */
;*---------------------------------------------------------------------*/
(define hop-to-hop-id -1)

;*---------------------------------------------------------------------*/
;*    hop-to-hop ...                                                   */
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


