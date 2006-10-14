;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/http-remote.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 23 15:46:32 2006                          */
;*    Last change :  Sat Oct 14 16:02:00 2006 (serrano)                */
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
	    __hop_user
	    __hop_http-error)

   (static  (class connection
	       (socket::socket read-only)
	       (host::bstring read-only)
	       (port::int read-only)
	       (id::elong (default -1))
	       request-id::obj
	       (keep-alive?::bool (default #f))
	       (locked::bool (default #f))
	       (register?::bool read-only)))
   
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
      (let loop ()
	 (with-access::http-response-remote r (host port header content-length remote-timeout request connection-timeout)
	    (trace-item "remotehost=" host
			" remoteport=" port
			" connection-timeout=" connection-timeout)
	    (let* ((host (or (hop-proxy-host) host))
		   (port (or (hop-proxy-port) port))
		   (remote (remote-get-socket host port connection-timeout request))
		   (rp (connection-output remote))
		   (sp (socket-input socket)))
	       ;; verb
	       (if (connection-keep-alive? remote)
		   (hop-verb 2
			     (hop-color request request " REMOTE.ka")
			     " (alive since "
			     (connection-request-id remote)
			     ") " host ":" port "\n")
		   (hop-verb 2
			     (hop-color request request " REMOTE")
			     " " host ":" port "\n"))
	       (when (>fx remote-timeout 0)
		  (output-timeout-set! rp remote-timeout))
	       (with-handler
		  (lambda (e)
		     (connection-close! remote)
		     (raise e))
		  ;; the header and the request
		  (with-trace 4 'http-response-header
		     (trace-item "start-line: " (response-remote-start-line r))
		     (http-write-line rp (response-remote-start-line r))
		     ;; if a char is ready and is eof, it means that the
		     ;; connection is closed
		     (if (connection-down? remote)
			 (begin
			    (connection-close! remote)
			    (loop))
			 (begin
			    (http-write-header
			     rp (http-filter-proxy-header header))
			    (when (hop-enable-keep-alive)
			       (let ((h (if (hop-proxy-host)
					    "proxy-connection: keep-alive"
					    "connection: keep-alive")))
				  (http-write-line rp h)))
			    (http-write-line rp)
			    ;; the content of the request
			    (when (>elong content-length #e0)
			       (trace-item "content-length=" content-length)
			       (send-chars sp rp content-length))
			    (flush-output-port rp)
			    (remote-body r socket remote))))))))))

;*---------------------------------------------------------------------*/
;*    remote-body ...                                                  */
;*---------------------------------------------------------------------*/
(define (remote-body r::http-response-remote socket remote::connection)
   (with-access::http-response-remote r (host port remote-timeout timeout request)
      ;; the body
      (with-trace 4 'http-response-body
	 (let ((op (socket-output socket))
	       (ip (connection-input remote))
	       (wstart #f))
	    (when (>fx remote-timeout 0)
	       (input-timeout-set! ip remote-timeout))
	    (when (>fx timeout 0)
	       (output-timeout-set! op timeout))
	    (with-handler
	       (lambda (e)
		  (cond
		     (wstart
		      ;; If we have alread send characters to the client
		      ;; it is no longer possible to answer resource
		      ;; unavailable so we raise the error
		      (raise e))
		     ((connection-keep-alive? remote)
		      ;; This is a keep-alive connection that is likely
		      ;; to have been closed by the remote server, we retry
		      ;; with a fresh connection
		      (connection-close! remote)
		      (hop-verb 2
				(hop-color request request " RESET.ka")
				" (alive since "
				(connection-request-id remote)
				") " host ":" port "\n")
		      (http-response r socket))
		     (else
		      ;; This is an unrecoverable error
		      (http-response (http-remote-error host e) socket))))
	       (multiple-value-bind (http-version status-code phrase)
		  (http-parse-status-line ip)
		  (multiple-value-bind (header _1 _2 cl te _3 _4 connection)
		     (http-read-header ip)
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
			 (connection-close! remote)
			 (connection-keep-alive! remote))
		     ;; return to the main hop loop
		     (let ((s (http-header-field
			       (http-request-header request)
			       proxy-connection:)))
			(cond
			   ((string? s)
			    (string->symbol s))
			   (else
			    'close))))))))))

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
(define (remote-get-socket host port timeout request)
   (define (purge-table!)
      (with-trace 5 'purge-table!
	 (trace-item "before purge, table-length="
		     (hashtable-size *connection-table*))
	 (hashtable-filter! *connection-table*
			    (lambda (k c)
			       (if (connection-locked c)
				   #t
				   (begin
				      (socket-close (connection-socket c))
				      #f))))
	 (trace-item "after purge, table-length="
		     (hashtable-size *connection-table*))))
   (define (get-new-connection! register)
      (with-trace 4 'register-new-connection
	 ;; unclock before establishing the connection
	 (mutex-unlock! *remote-lock*)
	 (let ((socket (make-client-socket/timeout host port timeout request)))
	    ;; relock the whole stuff
	    (mutex-lock! *remote-lock*)
	    (set! *remote-id* (+elong *remote-id* #e1))
	    (let ((connection (instantiate::connection
				 (socket socket)
				 (host host)
				 (port port)
				 (request-id (http-request-id request))
				 (locked #t)
				 (id *remote-id*)
				 (register? register))))
	       (when register
		  (hashtable-put! *connection-table* host connection)
		  (trace-item "table-length="
			      (hashtable-size *connection-table*))
		  (when (> (hashtable-size *connection-table*)
			   (hop-max-remote-keep-alive-connection))
		     (purge-table!)))
	       (mutex-unlock! *remote-lock*)
	       connection))))
   (define (get-connection)
      (with-trace 4 'get-connection
	 (trace-item "host=" host)
	 (mutex-lock! *remote-lock*)
	 (let ((old (hashtable-get *connection-table* host)))
	    (if (connection? old)
		(if (and (not (connection-locked old))
			 (=fx (connection-port old) port))
		    (begin
		       ;; re-use the connection
		       (connection-keep-alive?-set! old #t)
		       (connection-locked-set! old #t)
		       (mutex-unlock! *remote-lock*)
		       old)
		    ;; we have a collision in the hashtable, we
		    ;; cannot store the new connection
		    (get-new-connection! #f))
		;; create a new connection and registers it
		(get-new-connection! #t)))))
   (get-connection))
   
;*---------------------------------------------------------------------*/
;*    connection-keep-alive! ...                                       */
;*---------------------------------------------------------------------*/
(define (connection-keep-alive! connection)
   (mutex-lock! *remote-lock*)
   (if (connection-register? connection)
       (connection-locked-set! connection #f)
       (socket-close (connection-socket connection)))
   (mutex-unlock! *remote-lock*))

;*---------------------------------------------------------------------*/
;*    connection-close! ...                                            */
;*---------------------------------------------------------------------*/
(define (connection-close! connection)
   (mutex-lock! *remote-lock*)
   (socket-close (connection-socket connection))
   (hashtable-remove! *connection-table* (connection-host connection))
   (mutex-unlock! *remote-lock*))

;*---------------------------------------------------------------------*/
;*    connection-down? ...                                             */
;*---------------------------------------------------------------------*/
(define (connection-down? connection)
   (let ((ip (connection-input connection)))
      (when (char-ready? ip)
	 (with-handler
	    (lambda (e) #t)
	    (let ((c (read-char ip)))
	       (if (eof-object? c)
		   #t
		   (let ((msg (http-parse-error-message c ip)))
		      (raise
		       (instantiate::&io-parse-error
			  (obj ip)
			  (proc 'http-response)
			  (msg (format "Illegal character: ~a" msg)))))))))))

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
