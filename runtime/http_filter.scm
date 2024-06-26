;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/runtime/http_filter.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 24 13:19:41 2006                          */
;*    Last change :  Tue May 14 12:40:25 2024 (serrano)                */
;*    Copyright   :  2006-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HTTP response filtering                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_http-filter

   (include "http_utils.sch"
            "verbose.sch")

   (library web http)
   
   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_misc
	    __hop_xml-types
	    __hop_http-utils
	    __hop_http-error
	    __hop_http-response
	    __hop_http-proxy)
	    
   (export  (generic http-filter::symbol ::%http-response ::http-response-filter ::obj ::socket)))

;*---------------------------------------------------------------------*/
;*    http-filter ::%http-response ...                                 */
;*---------------------------------------------------------------------*/
(define-generic (http-filter::symbol r::%http-response f request socket)
   (http-response r request socket))

;*---------------------------------------------------------------------*/
;*    http-filter ::http-response-proxy ...                            */
;*---------------------------------------------------------------------*/
(define-method (http-filter r::http-response-proxy f request socket)
   (with-access::http-response-proxy r (scheme
					  host port
					  header content-length
					  #;request
					  timeout)
      (trace-item "remotehost=" host
		  " remoteport=" port
		  " timeout=" timeout)
      (let* ((ssl (eq? scheme 'https))
	     (host (or (hop-use-proxy-host) host))
	     (port (or (hop-use-proxy-port) port))
	     (rsock (make-client-socket/timeout host port timeout request ssl))
	     (rp (socket-output rsock))
	     (sp (socket-input socket)))
	 (hop-verb 5
		   (hop-color request request " FILTER")
		   " " host ":" port "\n")
	 (unwind-protect
	    (begin
	       ;; the header
	       (with-trace 4 "http-response-header"
		  (http-write-line rp (response-proxy-start-line r))
		  (http-write-header rp (http-filter-proxy-header header))
		  (http-write-line rp)
		  ;; the content of the request
		  (when (>elong content-length #e0)
		     (trace-item "content-length=" content-length)
		     (send-chars sp rp content-length))
		  (flush-output-port rp))
	       ;; the body
	       (with-access::http-response-filter f (statusf headerf bodyf)
		  (with-trace 4 "http-response-body"
		     (let* ((ip (socket-input rsock))
			    (op (socket-output socket))
			    (statusf statusf)
			    (headerf headerf)
			    (bodyf bodyf)
			    (sl (http-parse-status-line ip)))
			(multiple-value-bind (http-version status-code phrase)
			   sl
			   (let ((fs (statusf
					(format "~a ~a ~a"
					   http-version
					   status-code
					   phrase))))
			      (display fs op)
			      (multiple-value-bind (header _1 _2 cl te _3 _4 _5)
				 (http-parse-header ip op)
				 (when (eq? te 'chunked)
				    (let ((c (assq transfer-encoding: header)))
				       (set! header (remq! c header))))
				 (let ((fh (headerf header)))
				    (http-write-header op fh)
				    (http-write-line op)
				    (cond
				       ((or (=fx status-code 204)
					    (=fx status-code 304))
					;; no message body
					#unspecified)
				       (else
					(if (eq? te 'chunked)
					    (let ((ip2 (http-chunks->port ip)))
					       (unwind-protect
						  (bodyf ip2 op sl header cl)
						  (close-input-port ip2)))
					    (bodyf ip op sl header cl))))))))))))
	    (socket-close rsock))))
   'close)

;*---------------------------------------------------------------------*/
;*    http-filter ::http-response-filter ...                           */
;*    -------------------------------------------------------------    */
;*    Compose two filters                                              */
;*---------------------------------------------------------------------*/
(define-method (http-filter r::http-response-filter f request socket)
   (with-access::http-response-filter r ((sr statusf)
					 (hr headerf)
					 (br bodyf)
					 (rr response)
					 (rq request))
      (with-access::http-response-filter f ((sf statusf)
					    (hf headerf)
					    (bf bodyf))
	 (let ((f (instantiate::http-response-filter
		     (response rr)
		     #;(request rq)
		     (statusf (lambda (s)
				 (sf (sr s))))
		     (headerf (lambda (h)
				 (hf (hr h))))
		     (bodyf (lambda (ip op status header cl)
			       (let ((buf (open-output-string)))
				  (with-handler
				     (lambda (e)
					(close-output-port buf)
					(raise e))
				     (br ip buf status header cl))
				  (let* ((s (close-output-port buf))
					 (ip (open-input-string s)))
				     (unwind-protect
					(bf ip op status header cl)
					(close-input-port ip)))))))))
	    (http-response f request socket)))))

				  
			    
		   
   
