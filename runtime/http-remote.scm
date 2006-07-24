;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/http-remote.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 23 15:46:32 2006                          */
;*    Last change :  Mon Jul 24 14:08:40 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The HTTP remote response                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_http-remote

   (include "http-lib.sch")
   
   (import  __hop_param
	    __hop_types
	    __hop_misc
	    __hop_http-lib
	    __hop_http-response
	    __hop_user)

   (static  (class connection
	       (socket::socket read-only)
	       (port::int read-only)
	       (id::elong read-only)
	       (locked::bool (default #f))))
   
   (export  (response-remote-start-line ::http-response-remote)))

;*---------------------------------------------------------------------*/
;*    response-remote-start-line ...                                   */
;*---------------------------------------------------------------------*/
(define (response-remote-start-line resp)	    
   (with-access::http-response-remote resp (scheme host port encoded-path http method userinfo)
      (let ((p (if (string? userinfo)
		   (string-append userinfo "@" encoded-path)
		   encoded-path)))
	 (cond
	    ((not (hop-proxy))
	     (format "~a ~a ~a" method p http))
	    ((not hostname)
	     (format "~a ~a://~a ~a" method scheme p http))
	    ((not port)
	     (format "~a ~a://~a~a ~a" method scheme host p http))
	    (else
	     (format "~a ~a://~a:~a~a ~a" method scheme host port p http))))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-remote ...                         */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-remote socket)
   (with-trace 3 'http-response::http-response-remote
      (with-access::http-response-remote r (host port header content-length timeout request timeout remote-timeout)
	 (trace-item "remotehost=" host
		     " remoteport=" port
		     " remote-timeout=" remote-timeout)
	 (let* ((host (or (hop-proxy-host) host))
		(port (or (hop-proxy-port) port))
		(remote (remote-get-socket host port remote-timeout request))
		(rp (connection-output remote))
		(sp (socket-input socket)))
	    (hop-verb 2
		      (hop-color request request " REMOTE")
		      " " host ":" port "\n")
	    (with-handler
	       (lambda (e)
		  (remote-close! remote)
		  (raise e))
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
		     (let* ((ip (connection-input remote))
			    (op (socket-output socket)))
			(when (>fx timeout 0)
			   (input-port-timeout-set! ip timeout)
			   (output-port-timeout-set! op timeout))
			(multiple-value-bind (http-version status-code phrase)
			   (http-parse-status-line ip)
			   ;; WARNING: phrase contains its terminal \r\n hence
			   ;; it must be displayed with regular scheme writer,
			   ;; not HTTP-WRITE-LINE!
			   (trace-item "# " http-version " " status-code " "
				       (string-for-read phrase))
			   (display http-version op)
			   (display " " op)
			   (display status-code op)
			   (display " " op)
			   (display phrase op)
			   (multiple-value-bind (header _1 _2 cl te _3 _4 connection)
			      (http-read-header ip)
			      (http-write-header op header)
			      (http-write-line op)
			      (case status-code
				 ((204 304)
				  ;; no message body
				  #unspecified)
				 (else
				  (if (not (eq? te 'chunked))
				      (unless (=elong cl #e0)
					 (send-chars ip op cl))
				      (response-chunks ip op))))
			      ;; what to do with the remote connection
			      (if (or (eq? connection 'close)
				      (not (hop-enable-keep-alive)))
				  (remote-close! remote)
				  (remote-keep-alive! remote))
			      ;; return to the main hop loop
			      (let ((s (http-header-field
					(http-request-header request)
					proxy-connection:)))
				 (cond
				    ((string? s)
				     (string->symbol s))
				    (else
				     'close)))))))))))))

;*---------------------------------------------------------------------*/
;*    *remote-lock* ...                                                */
;*---------------------------------------------------------------------*/
(define *remote-lock* (make-mutex "remote"))

;*---------------------------------------------------------------------*/
;*    *connection-table* ...                                           */
;*---------------------------------------------------------------------*/
(define *connection-table* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    *remote-id* ...                                                  */
;*---------------------------------------------------------------------*/
(define *remote-id* #e0)

;*---------------------------------------------------------------------*/
;*    remote-get-socket ...                                            */
;*    -------------------------------------------------------------    */
;*    This function is in charge of keeping alive remote connection.   */
;*    When a new connection with a remote host is up to be opened,     */
;*    it first checks if it happens to still have an old connection    */
;*    with that host.                                                  */
;*---------------------------------------------------------------------*/
(define (remote-get-socket host port timeout msg)
   (define (purge-table!)
      (cond
	 ((=elong *remote-id* #e0)
	  ;; flush out all elements because *remote-id* has overflowed
	  (hashtable-filter! *connection-table* (lambda (x) #f)))
	 ((=fx (hashtable-size *connection-table*)
	       (hop-max-remote-keep-alive-connection))
	  ;; the table contains the max number of permited elements
	  (hashtable-filter!
	   *connection-table*
	   (lambda (c)
	      (<= (-elong *remote-id* (connection-id c))
		  (hop-max-remote-keep-alive-connection)))))))
   (define (register-new-connection!)
      (set! *remote-id* (+elong *remote-id* #e1))
      (let* ((socket (make-client-socket/timeout host port timeout msg))
	     (connection (instantiate::connection
			    (socket socket)
			    (port port)
			    (id *remote-id*))))
;* 	 (hashtable-put! *connection-table* host connection)           */
	 connection))
   (define (get-connection)
      (let ((old (hashtable-get *connection-table* host)))
	 (if (and (connection? old)
		  (input-port? (socket-input (connection-socket old))))
	     (trace-item "old=" old " port=" (connection-port old))
	     (trace-item "old=" old))
	 (if old
	     (cond
		((connection-locked old)
		 (register-new-connection!))
		((or (not (input-port? (socket-input (connection-socket old))))
		     (not (=fx (connection-port old) port)))
		 (hashtable-remove! *connection-table* old)
		 (register-new-connection!))
		(else
		 old))
	     (register-new-connection!))))
   (with-trace 4 'remote-get-socket
      (mutex-lock! *remote-lock*)
      (let ((conn (get-connection)))
	 (connection-locked-set! conn #t)
	 (mutex-unlock! *remote-lock*)
	 conn)))
   
;*---------------------------------------------------------------------*/
;*    remote-keep-alive! ...                                           */
;*---------------------------------------------------------------------*/
(define (remote-keep-alive! connection)
   (mutex-lock! *remote-lock*)
;*    (connection-locked-set! connection #f)                           */
   (socket-close (connection-socket connection))
   (mutex-unlock! *remote-lock*))

;*---------------------------------------------------------------------*/
;*    remote-close! ...                                                */
;*---------------------------------------------------------------------*/
(define (remote-close! connection)
   (mutex-lock! *remote-lock*)
   (socket-close (connection-socket connection))
;*    (hashtable-remove! *connection-table* connection)                */
   (mutex-unlock! *remote-lock*))

;*---------------------------------------------------------------------*/
;*    remote-down? ...                                                 */
;*---------------------------------------------------------------------*/
(define (remote-down? connection)
   (socket-down? (connection-socket connection)))

;*---------------------------------------------------------------------*/
;*    connection-output ...                                            */
;*---------------------------------------------------------------------*/
(define (connection-output connection)
   (socket-output (connection-socket connection)))

;*---------------------------------------------------------------------*/
;*    connection-input ...                                             */
;*---------------------------------------------------------------------*/
(define (connection-input connection)
   (socket-input (connection-socket connection)))
