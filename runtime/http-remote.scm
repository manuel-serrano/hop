;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/http-remote.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 23 15:46:32 2006                          */
;*    Last change :  Fri Aug 10 08:38:57 2007 (serrano)                */
;*    Copyright   :  2006-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HTTP remote response                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_http-remote

   (include "http-lib.sch")

   (library web)
   
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
	       request-id::obj
	       (keep-alive?::bool (default #f))
	       (intable?::bool (default #f))
	       (locked?::bool (default #f))
	       (closed?::bool (default #f))
	       (wstart?::bool (default #f))
	       date::elong))
   
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
	 (with-access::http-response-remote r (scheme host port header content-length remote-timeout request connection-timeout)
	    (trace-item "remotehost=" host
			" remoteport=" port
			" connection-timeout=" connection-timeout)
	    (let* ((host (or (hop-proxy-host) host))
		   (port (or (hop-proxy-port) port))
		   (ssl (eq? scheme 'https))
		   (remote (remote-get-socket host port connection-timeout request ssl))
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
		  (output-timeout-set! rp remote-timeout)
		  (input-timeout-set! (connection-input remote) remote-timeout))
	       ;; this unwind-protect is just here for debugging
	       ;; when the keep-alive remote connection is complete,
	       ;; remove the unwind-protected block.
	       (unwind-protect
		  (with-handler
		     (lambda (e)
			(error-notify e)
			(connection-close! remote)
			(cond
			   ((connection-wstart? remote)
			    #;(tprint "wstart...")
			    ;; If we have already send characters to the client
			    ;; it is no longer possible to answer resource
			    ;; unavailable so we raise the error.
			    ;; There is no need to close the connection because
			    ;; the next handler closes it.
			    (raise e))
			   ((connection-keep-alive? remote)
			    #;(tprint "keep-alive...")
			    ;; This is a keep-alive connection that is likely
			    ;; to have been closed by the remote server, we
			    ;; retry with a fresh connection
			    (hop-verb 2
				      (hop-color request request " RESET.ka")
				      " (alive since "
				      (connection-request-id remote)
				      ") " host ":" port "\n")
			    (http-response r socket))
			   (else
			    ;; This is an unrecoverable error
			    (http-response (http-remote-error host e) socket))))
		     ;; the header and the request
		     (with-trace 4 'http-response-header
			(trace-item "start-line: "
				    (response-remote-start-line r))
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
			       (when (hop-enable-remote-keep-alive)
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
			       (remote-body r socket remote)))))
		  ;; debug
		  (unless (or (connection-intable? remote)
			      (connection-closed? remote))
		     (tprint "connection not either closed or in table (http-response.scm)"
			     remote)
		     (connection-close! remote))))))))

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
	    (when (>fx timeout 0)
	       (output-timeout-set! op timeout))
	    (multiple-value-bind (http-version status-code phrase)
	       (http-parse-status-line ip)
	       (multiple-value-bind (header _1 _2 cl te _3 _4 connection)
		  (http-parse-header ip (connection-output remote))
		  ;; WARNING: phrase contains its terminal \r\n hence
		  ;; it must be displayed with regular scheme writer,
		  ;; not HTTP-WRITE-LINE!
		  (trace-item "# " http-version " " status-code " "
			      (string-for-read phrase))
		  (trace-item "content-length=" cl)
		  (trace-item "transfer-encoding=" te)
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
		  ;; what to do with the remote connection. if the
		  ;; status code is not 200, we always close the connection
		  ;; in order to avoid, in particular, re-direct problems.
		  (if (or (not (=fx status-code 200))
			  (eq? connection 'close)
			  (not (hop-enable-remote-keep-alive)))
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
			 'close)))))))))

;*---------------------------------------------------------------------*/
;*    *remote-lock* ...                                                */
;*---------------------------------------------------------------------*/
(define *remote-lock* (make-mutex "remote"))

;*---------------------------------------------------------------------*/
;*    *connection-table* ...                                           */
;*---------------------------------------------------------------------*/
(define *connection-table* (make-hashtable))
(define *connection-number* 0)
(define *connection-open* 0)
(define *connection-close* 0)
(define *connection-pending* '())

;*---------------------------------------------------------------------*/
;*    connection-table-get ...                                         */
;*---------------------------------------------------------------------*/
(define (connection-table-get host)
   (let ((lst (hashtable-get *connection-table* host)))
      (when (pair? lst)
	 (car lst))))
   
;*---------------------------------------------------------------------*/
;*    connection-timeout? ...                                          */
;*---------------------------------------------------------------------*/
(define (connection-timeout? c now)
   (with-access::connection c (date)
      (let ((age (*fx 1000 (elong->fixnum (-elong now date)))))
	 (>=fx age (hop-remote-keep-alive-timeout)))))

;*---------------------------------------------------------------------*/
;*    cleanup-connection-table! ...                                    */
;*    -------------------------------------------------------------    */
;*    Filters out useless keep-alive connections.                      */
;*---------------------------------------------------------------------*/
(define (cleanup-connection-table!)
   (with-trace 5 'cleanup-connection-table!
      (let* ((now (current-seconds))
	     (num 0)
	     (orows (hashtable->list *connection-table*))
	     (nrows (map (lambda (row)
			    (filter (lambda (c)
				       (with-access::connection c
					     (socket locked? intable? closed?)
					  (cond
					     (locked?
					      (set! num (+fx 1 num))
					      #t)
					     ((connection-timeout? c now)
					      (socket-close socket)
					      (set! intable? #f)
					      (set! closed? #t)
					      #f)
					     (else
					      (set! num (+fx 1 num))
					      #t))))
				    row))
			 orows)))
	 (for-each (lambda (orow nrow)
		      (let ((host (connection-host (car orow))))
			 (unless (eq? orow nrow)
			    (hashtable-remove! *connection-table* host)
			    (when (pair? nrow)
			       (hashtable-put! *connection-table* host nrow)))))
		   orows nrows)
	 (trace-item " table-length: " *connection-number* " -> " num)
	 (set! *connection-number* num))))
   
;*---------------------------------------------------------------------*/
;*    remote-get-socket ...                                            */
;*    -------------------------------------------------------------    */
;*    This function is in charge of keeping alive remote connections.  */
;*    When a new connection with a remote host is up to be opened,     */
;*    it first checks if it happens to still have an old connection    */
;*    with that host.                                                  */
;*---------------------------------------------------------------------*/
(define (remote-get-socket host port timeout request ssl)
   
   (define (get-new-connection!)
      (with-trace 4 'get-new-connection
	 (let ((s (make-client-socket/timeout host port timeout request ssl)))
	    (instantiate::connection
	       (socket s)
	       (host host)
	       (port port)
	       (request-id (http-request-id request))
	       (date (current-seconds))))))

   (define (register! connection)
      (mutex-lock! *remote-lock*)
      (set! *connection-open* (+fx 1 *connection-open*))
      (set! *connection-pending* (cons connection *connection-pending*))
      (mutex-unlock! *remote-lock*)
      connection)
   
   (define (get-connection)
      (with-trace 4 'get-connection
	 (trace-item "host=" host)
	 (if (not (hop-enable-remote-keep-alive))
	     (get-new-connection!)
	     (begin
		(mutex-lock! *remote-lock*)
		(let ((old (connection-table-get host)))
		   (when (connection? old)
		      (trace-item "old-connection=yes")
		      (trace-item "old-connection locked="
				  (connection-locked? old))
		      (trace-item "old-connection port="
				  (connection-port old)))
		   (if (connection? old)
		       (cond
			  ((or (connection-locked? old)
			       (not (=fx (connection-port old) port)))
			   ;; the connection is locked or used on another port
			   (mutex-unlock! *remote-lock*)
			   (register! (get-new-connection!)))
			  ((connection-timeout? old (current-seconds))
			   ;; the connection is too old, closed it
			   (connection-close-sans-lock! old)
			   ;; release the lock only when the old connection
			   ;; has been closed and properly removed
			   (mutex-unlock! *remote-lock*)
			   (register! (get-new-connection!)))
			  (else
			   ;; re-use the connection
			   (connection-keep-alive?-set! old #t)
			   ;; allocate a little extra amount of time
			   ;; for the connection
			   (connection-date-set! old (current-seconds))
			   (connection-locked?-set! old #t)
			   (connection-wstart?-set! old #f)
			   (mutex-unlock! *remote-lock*)
			   old))
		       ;; create a new connection and registers it
		       (begin
			  (mutex-unlock! *remote-lock*)
			  (register! (get-new-connection!)))))))))
   
   (get-connection))
   
;*---------------------------------------------------------------------*/
;*    connection-keep-alive! ...                                       */
;*    -------------------------------------------------------------    */
;*    We keep alive connections iff:                                   */
;*      - the number of current connections does not exceed the user   */
;*        parameter HOP-MAX-REMOTE-KEEP-ALIVE-CONNECTION               */
;*      - the number of alive connections does not exceed the user     */
;*        parameter HOP-MAX-REMOTE-KEEP-ALIVE-CONNECTION               */
;*      - the number of alive connections to the host does not exceed  */
;*        the user parameter HOP-MAX-REMOTE-KEEP-ALIVE-CONNECTION      */
;*        divided by an arbitrary threshold.                           */
;*---------------------------------------------------------------------*/
(define (connection-keep-alive! connection)
   (mutex-lock! *remote-lock*)
   (cond
      ((>=fx *connection-number* (hop-max-remote-keep-alive-connection))
       ;; no room available, cleanup the table
       (connection-close-sans-lock! connection)
       (cleanup-connection-table!))
      ((>= (-fx *connection-open* (+fx *connection-close* *connection-number*))
	   (hop-max-remote-keep-alive-connection))
       ;; too many uncompleted connections
       (connection-close-sans-lock! connection)
       (set! *connection-pending*
	     (filter! (lambda (c)
			 (if (and (connection-down? c)
				  (not (connection-closed? c)))
			     (begin
				(connection-close-sans-lock! c)
				#f)
			     #t))
		      *connection-pending*))
       #unspecified)
      ((>=fx *connection-number* (hop-max-remote-keep-alive-connection))
       ;; no room available, cleanup the table
       (connection-close-sans-lock! connection)
       (cleanup-connection-table!))
      (else
       ;; store the connection only if room is available on the table
       (with-access::connection connection (host locked? intable?)
	  (let ((l (hashtable-get *connection-table* host)))
	     (cond
		((not (pair? l))
		 (hashtable-put! *connection-table* host (list connection))
		 (set! *connection-number* (+fx 1 *connection-number*))
		 (set! locked? #f)
		 (set! intable? #t))
		((memq connection l)
		 (set! locked? #f))
		((<fx (length l) (/ (hop-max-remote-keep-alive-connection) 10))
		 (append! (last-pair l) (list connection))
		 (set! *connection-number* (+fx 1 *connection-number*))
		 (set! locked? #f)
		 (set! intable? #t))
		(else
		 (connection-close-sans-lock! connection)))))))
   (mutex-unlock! *remote-lock*))

;*---------------------------------------------------------------------*/
;*    connection-close-sans-lock! ...                                  */
;*---------------------------------------------------------------------*/
(define (connection-close-sans-lock! connection::connection)
   (with-access::connection connection (host socket intable? closed?)
      (unless closed?
	 (set! *connection-close* (+fx 1 *connection-close*))
	 (set! *connection-pending* (remq! connection *connection-pending*))
	 (socket-close socket)
	 (set! closed? #t)
	 (when intable?
	    (let ((l (remq! connection (hashtable-get *connection-table* host))))
	       (set! intable? #f)
	       (set! *connection-number* (-fx *connection-number* 1))
	       (if (null? l)
		   (hashtable-remove! *connection-table* host)
		   (hashtable-put! *connection-table* host l)))))))
   
;*---------------------------------------------------------------------*/
;*    connection-close! ...                                            */
;*---------------------------------------------------------------------*/
(define (connection-close! connection::connection)
   (mutex-lock! *remote-lock*)
   (connection-close-sans-lock! connection)
   (mutex-unlock! *remote-lock*))

;*---------------------------------------------------------------------*/
;*    connection-down? ...                                             */
;*---------------------------------------------------------------------*/
(define (connection-down? connection::connection)
   (let ((ip (connection-input connection)))
      (or (not (input-port? ip))
	  (when (char-ready? ip)
	     (with-handler
		(lambda (e) #t)
		(let ((c (read-char ip)))
		   (if (eof-object? c)
		       #t
		       (let ((m (http-parse-error-message c ip)))
			  (raise
			   (instantiate::&io-parse-error
			      (obj ip)
			      (proc 'http-response)
			      (msg (format "Illegal character: ~a" m))))))))))))

;*---------------------------------------------------------------------*/
;*    connection-output ...                                            */
;*---------------------------------------------------------------------*/
(define (connection-output connection::connection)
   (socket-output (connection-socket connection)))

;*---------------------------------------------------------------------*/
;*    connection-input ...                                             */
;*---------------------------------------------------------------------*/
(define (connection-input connection::connection)
   (socket-input (connection-socket connection)))
