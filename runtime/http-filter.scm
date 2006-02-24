;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/http-filter.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 24 13:19:41 2006                          */
;*    Last change :  Fri Feb 24 15:40:42 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HTTP response filtering                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_http-filter

   (include "http-lib.sch")
   
   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_misc
	    __hop_xml
	    __hop_http-lib
	    __hop_http-error
	    __hop_http-response)
	    
   (export  (generic http-filter ::%http-response ::http-response-filter ::socket)))

;*---------------------------------------------------------------------*/
;*    http-filter ::%http-response ...                                 */
;*---------------------------------------------------------------------*/
(define-generic (http-filter r::%http-response f socket)
   (http-response r socket))

;*---------------------------------------------------------------------*/
;*    http-filter ::http-response-remote ...                           */
;*---------------------------------------------------------------------*/
(define-method (http-filter r::http-response-remote f socket)
   (with-access::http-response-remote r (host port header content-length timeout request)
      (trace-item "remotehost=" host
		  " remoteport=" port
		  " timeout=" timeout)
      (let* ((host (or (hop-proxy-host) host))
	     (port (or (hop-proxy-port) port))
	     (rsock (make-client-socket/timeout host port timeout request))
	     (rp (socket-output rsock))
	     (sp (socket-input socket)))
	 (hop-verb 4
		   (hop-color request request " FILTER")
		   " " host ":" port "\n")
	 (unwind-protect
	    (begin
	       ;; the header
	       (with-trace 4 'http-response-header
		  (http-write-line rp (response-remote-start-line r))
		  (http-write-header rp (http-filter-proxy-header header))
		  (http-write-line rp)
		  ;; the content of the request
		  (when (>elong content-length #e0)
		     (trace-item "content-length=" content-length)
		     (send-chars sp rp content-length))
		  (flush-output-port rp))
	       ;; the body
	       (with-trace 4 'http-response-body
		  (let* ((ip (socket-input rsock))
			 (op (socket-output socket))
			 (statusf (http-response-filter-statusf f))
			 (headerf (http-response-filter-headerf f))
			 (bodyf (http-response-filter-bodyf f)))
		     (multiple-value-bind (http-version status-code phrase)
			(http-parse-status-line ip)
			;; WARNING: phrase contains its terminal \r\n hence
			;; it must be displayed with regular scheme writer,
			;; not HTTP-WRITE-LINE!
			(trace-item "# " http-version " " status-code " "
				    (string-for-read phrase))
			(let ((fs (statusf
				   (format "~a ~a ~a"
					   http-version
					   status-code
					   phrase))))
			   (display fs op)
			   (multiple-value-bind (header _1 _2 cl te _3 _4)
			      (http-read-header ip)
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
					 (let ((ip2 (open-input-procedure
						     (make-unchunks ip))))
					    (unwind-protect
					       (bodyf ip2 op fs fh cl)
					       (close-input-port ip2)))
					 (bodyf ip op fs fh cl)))))))))))
	    (socket-close rsock)))))

;*---------------------------------------------------------------------*/
;*    http-filter ::http-response-filter ...                           */
;*    -------------------------------------------------------------    */
;*    Compose two filters                                              */
;*---------------------------------------------------------------------*/
(define-method (http-filter r::http-response-filter f socket)
   (with-access::http-response-filter r ((sr statusf)
					 (hr headerf)
					 (br bodyf)
					 (rr response))
      (with-access::http-response-filter f ((sf statusf)
					    (hf headerf)
					    (bf bodyf))
	 (let ((f (instantiate::http-response-filter
		     (response rr)
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
	    (http-response f socket)))))

				  
			    
		   
   
