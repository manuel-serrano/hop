;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/runtime/event.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 27 05:45:08 2005                          */
;*    Last change :  Tue Apr 20 13:54:07 2010 (serrano)                */
;*    Copyright   :  2005-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The implementation of server events                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_event

   (library web)
   
   (include "xml.sch"
	    "service.sch"
	    "param.sch")

   (import  __hop_param
	    __hop_types
	    __hop_user
	    __hop_xml
	    __hop_hop-extra
	    __hop_misc
	    __hop_hop
	    __hop_http-response
	    __hop_js-lib
	    __hop_cgi
	    __hop_read
	    __hop_service
	    __hop_http-response
	    __hop_http-error)

   (static  (class http-response-event::%http-response-local
	       (name::bstring read-only))
	    
	    (class ajax-connection
	       (mutex::mutex read-only (default (make-mutex)))
	       (req (default #f))
	       key::symbol
	       (buffers::pair-nil (default '()))
	       (marktime::elong (default (current-seconds)))
	       (pingtime::elong (default #e0)))
	    
	    (class buffer
	       (buffer-init)
	       (size::int read-only (default (hop-event-buffer-size)))
	       (mutex::mutex read-only (default (make-mutex)))
	       (cells::pair-nil (default '()))
	       (head::pair-nil (default '()))
	       (tail::pair-nil (default '()))))
	    
   (export  (hop-event-init! ::obj)
	    (hop-event-tables)
	    (hop-event-signal! ::bstring ::obj)
	    (hop-event-broadcast! ::bstring ::obj)
	    (hop-event-client-ready? ::bstring)
	    (hop-event-policy-file ::http-request)))

;*---------------------------------------------------------------------*/
;*    *event-mutex* ...                                                */
;*---------------------------------------------------------------------*/
(define *event-mutex* (make-mutex "hop-event"))

;*---------------------------------------------------------------------*/
;*    hop-multipart-key ...                                            */
;*---------------------------------------------------------------------*/
(define hop-multipart-key "hop-multipart")

;*---------------------------------------------------------------------*/
;*    event services ...                                               */
;*---------------------------------------------------------------------*/
(define *port-service* #f)
(define *websocket-service* #f)
(define *register-service* #f)
(define *unregister-service* #f)
(define *init-service* #f)
(define *policy-file-service #f)
(define *close-service* #f)
(define *client-key* 0)
(define *default-request* (instantiate::http-server-request (user (user-nil))))
(define *ping* (symbol->string (gensym 'ping)))
(define *clients-number* 0)

;*---------------------------------------------------------------------*/
;*    object-display ::ajax-connection ...                             */
;*---------------------------------------------------------------------*/
(define-method (object-display obj::ajax-connection . port)
   (let ((p (if (pair? port) (car port) (current-output-port))))
      (with-access::ajax-connection obj (key req)
	 (display key p)
	 (display " " p)
	 (if (http-request? req)
	     (let ((sock (http-request-socket req)))
		(display (socket-hostname sock) p)
		(display ":" p)
		(display (socket-port-number sock) p))
	     "unattached"))))

;*---------------------------------------------------------------------*/
;*    buffer-init ...                                                  */
;*---------------------------------------------------------------------*/
(define (buffer-init buf)
   (with-access::buffer buf (size head tail cells)
      (set! cells (map! (lambda (i) (cons #f #unspecified)) (iota size)))
      (set! head cells)
      (set! tail cells)))

;*---------------------------------------------------------------------*/
;*    buffer-empty? ...                                                */
;*---------------------------------------------------------------------*/
(define (buffer-empty? buf)
   (with-access::buffer buf (head mutex)
      (mutex-lock! mutex)
      (let ((r (not (car (car head)))))
	 (mutex-unlock! mutex)
	 r)))

;*---------------------------------------------------------------------*/
;*    buffer-push! ...                                                 */
;*---------------------------------------------------------------------*/
(define (buffer-push! buf val)
   (with-access::buffer buf (size head tail cells mutex)
      (mutex-lock! mutex)
      (if (pair? tail)
	  ;; we have free cells
	  (let ((cell (car tail)))
	     (set! tail (cdr tail))
	     (set-car! cell #t)
	     (set-cdr! cell val))
	  ;; we dont have free cells, we remove the first one
	  (let* ((l head)
		 (cell (car l)))
	     (set! head (cdr head))
	     (set-cdr! l '())
	     (set-cdr! cell val)
	     (set-cdr! (last-pair head) l)
	     (set! tail '())))
      (mutex-unlock! mutex)))

;*---------------------------------------------------------------------*/
;*    buffer-pop! ...                                                  */
;*---------------------------------------------------------------------*/
(define (buffer-pop! buf)
   (with-access::buffer buf (size head tail cells mutex)
      (mutex-lock! mutex)
      (let ((cell (car head)))
	 (if (car cell)
	     (let ((val (cdr cell)))
		(set-car! cell #f)
		(let ((l head))
		   (set! head (cdr head))
		   (set-cdr! l '())
		   (if (pair? tail)
		       (set-cdr! (last-pair tail) l)
		       (begin
			  (set-cdr! (last-pair head) l)
			  (set! tail l))))
		(mutex-unlock! mutex)
		val)
	     (begin
		(mutex-unlock! mutex)
		#f)))))
   
;*---------------------------------------------------------------------*/
;*    buffer-pop-all! ...                                              */
;*    -------------------------------------------------------------    */
;*    Contrary to buffer-pop!, buffer-pop-all! assumes that the        */
;*    mutex is already acquired.                                       */
;*---------------------------------------------------------------------*/
(define (buffer-pop-all! buf)
   (with-access::buffer buf (size head tail cells)
      (let loop ((res '())
		 (last (last-pair head)))
	 (let ((cell (car head)))
	    (if (car cell)
		(let ((val (cdr cell)))
		   (set-car! cell #f)
		   (let ((l head))
		      (set! head (cdr head))
		      (set-cdr! l '())
		      (set-cdr! last l)
		      (loop (cons val res) l)))
		(reverse! res))))))

;*---------------------------------------------------------------------*/
;*    buffer-get-all ...                                               */
;*---------------------------------------------------------------------*/
(define (buffer-get-all buf)
   (with-access::buffer buf (size head tail cells mutex)
      (mutex-lock! mutex)
      (let loop ((res '()))
	 (let ((cell (car head)))
	    (if (car cell)
		(let ((val (cdr cell)))
		   (let ((l head))
		      (set! head (cdr head))
		      (loop (cons val res))))
		(begin
		   (mutex-unlock! mutex)
		   (reverse! res)))))))

;*---------------------------------------------------------------------*/
;*    *ajax-connection-key-table* ...                                  */
;*---------------------------------------------------------------------*/
(define *ajax-connection-key-table* (make-hashtable))
(define *ajax-connection-name-table* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    ajax-find-connection-by-key ...                                  */
;*---------------------------------------------------------------------*/
(define (ajax-find-connection-by-key key)
   (hashtable-get *ajax-connection-key-table* key))

;*---------------------------------------------------------------------*/
;*    ajax-find-connections-by-name ...                                */
;*---------------------------------------------------------------------*/
(define (ajax-find-connections-by-name name)
   (or (hashtable-get *ajax-connection-name-table* name) '()))

;*---------------------------------------------------------------------*/
;*    ajax-connection-event-pop! ...                                   */
;*---------------------------------------------------------------------*/
(define (ajax-connection-event-pop! conn name)
   (with-access::ajax-connection conn (buffers)
      (let ((cell (assoc name buffers)))
	 (when (pair? cell)
	    (buffer-pop! (cdr cell))))))

;*---------------------------------------------------------------------*/
;*    ajax-connection-event-pop-all! ...                               */
;*---------------------------------------------------------------------*/
(define (ajax-connection-event-pop-all! conn)
   (with-access::ajax-connection conn (buffers)
      (filter-map (lambda (buffer)
		     (let ((vals (buffer-pop-all! (cdr buffer))))
			(when (pair? vals) (cons (car buffer) vals))))
		  buffers)))

;*---------------------------------------------------------------------*/
;*    ajax-connection-event-push! ...                                  */
;*    -------------------------------------------------------------    */
;*    This function assumes that the event mutex is acquired.          */
;*---------------------------------------------------------------------*/
(define (ajax-connection-event-push! conn name value)
   [assert (conn) (not (symbol? (mutex-state (ajax-connection-mutex conn))))]
   (with-access::ajax-connection conn (buffers mutex buffers)
      (let ((cell (assoc name buffers)))
	 (when (pair? cell)
	    (buffer-push! (cdr cell) value)))))

;*---------------------------------------------------------------------*/
;*    ajax-connection-abandon-for! ...                                 */
;*---------------------------------------------------------------------*/
(define (ajax-connection-abandon-for! conn nreq)
   (with-access::ajax-connection conn (req mutex)
      (when (http-request? req)
	 (ajax-signal-value req (scheme->response '() req)))
      (set! req nreq)))

;*---------------------------------------------------------------------*/
;*    ajax-connection-add-event! ...                                   */
;*---------------------------------------------------------------------*/
(define (ajax-connection-add-event! conn name)
   (with-access::ajax-connection conn (mutex buffers)
      (let ((cell (assoc name buffers)))
	 (unless (pair? cell)
	    (hashtable-update! *ajax-connection-name-table*
			       name
			       (lambda (l) (cons conn l))
			       (list conn))
	    (set! buffers (cons (cons name (instantiate::buffer)) buffers))))))

;*---------------------------------------------------------------------*/
;*    ajax-connection-remove-event! ...                                */
;*---------------------------------------------------------------------*/
(define (ajax-connection-remove-event! conn name)
   (with-access::ajax-connection conn (mutex buffers)
      (mutex-lock! mutex)
      (let ((cell (assoc name buffers)))
	 (when (pair? cell)
	    (set! buffers (remq! cell buffers))
	    (hashtable-update! *ajax-connection-name-table*
			       name
			       (lambda (l)
				  (remq! conn l))
			       '())))
      (mutex-unlock! mutex)))

;*---------------------------------------------------------------------*/
;*    ajax-register-new-connection! ...                                */
;*---------------------------------------------------------------------*/
(define (ajax-register-new-connection! req key)
   (let ((conn (instantiate::ajax-connection
		  (key key)
		  (req req))))
      (hashtable-put! *ajax-connection-key-table* key conn)))


;*---------------------------------------------------------------------*/
;*    websocket-register-new-connection! ...                           */
;*---------------------------------------------------------------------*/
(define (websocket-register-new-connection! req key)
   
   (define (get-header header key default)
      (let ((c (assq key header)))
	 (if (pair? c)
	     (cdr c)
	     default)))
   
   (define (websocket-server-location host)
      (format "ws://~a/~a/server-event/websocket?key=~a"
	      (or host (format "~a:~a" (hostname) (hop-port)))
	      (hop-initial-weblet)
	      key))
   
   ;; register the websocket
   (with-lock *event-mutex*
      (lambda ()
	 (set! *websocket-request-list*
	       (cons (cons (string->symbol key) req)
		     *websocket-request-list*))))

   (tprint "websocket-register-new-connection: " key)
   
   (with-access::http-request req (header connection socket)
      (let ((host (get-header header host: #f)))
	 (instantiate::http-response-websocket
	    (start-line "HTTP/1.1 101 Web Socket Protocol Handshake")
	    (location (websocket-server-location host))
	    (origin (get-header header origin: "localhost"))
	    (protocol (get-header header WebSocket-Protocol: #f))
	    (connection 'Upgrade)))))

;*---------------------------------------------------------------------*/
;*    hop-event-init! ...                                              */
;*    -------------------------------------------------------------    */
;*    Flash >= 9.0.115 enforces security policies using "policy files" */
;*    which are described at:                                          */
;*        http://www.adobe.com/devnet/flashplayer/articles/            */
;*          fplayer9_security.html                                     */
;*---------------------------------------------------------------------*/
(define (hop-event-init! port)
   (with-lock *event-mutex*
      (lambda ()
	 (when (=fx *client-key* 0)
	    (set! *client-key* (elong->fixnum (current-seconds)))
	    
	    (set! *websocket-service*
		  (service :name "server-event/websocket" (#!key key)
		     (websocket-register-new-connection! (current-request) key)))
	    
	    (set! *port-service*
		  (service :name "server-event/info" ()
		     (let* ((hd (http-request-header (current-request)))
			    (host (assq host: hd))
			    (key (get-server-event-key (or port 0))))
			(tprint "server-info key=" key)
			(if (pair? host)
			    (let ((s (string-split (cdr host) ":")))
			       (vector (car s) port key))
			    (vector (hostname) port key)))))
	    
	    (set! *init-service*
		  (service :name "server-event/init" (#!key key)
		     (with-lock *event-mutex*
			(lambda ()
			   (let ((req (current-request)))
			      ;; read the Flash's ending zero byte
			      (read-byte
			       (socket-input (http-request-socket req)))
			      (set! *flash-request-list*
				    (cons (list (string->symbol key)
						req
						(current-seconds))
					  *flash-request-list*))
			      ;; increments the number of connected clients
			      (set! *clients-number* (+fx *clients-number* 1))
			      (instantiate::http-response-event
				 (request req)
				 (name key)))))))

	    (set! *policy-file-service
		  (service :name "server-event/policy-file" (#!key port key)
		     (instantiate::http-response-string
			(request (current-request))
			(content-type "application/xml")
			(body (hop-event-policy port)))))

	    (set! *close-service*
		  (service :name "server-event/close" (#!key key)
		     (with-lock *event-mutex*
			(lambda ()
			   (let ((key (string->symbol key)))
			      (set! *clients-number* (-fx *clients-number* 1))
			      (set! *flash-request-list*
				    (filter! (lambda (e)
						(not (eq? (car e) key)))
					     *flash-request-list*)))))))

	    (set! *unregister-service*
		  (service :name "server-event/unregister" (#!key event key)
		     (server-event-unregister event key)))
	    
	    (set! *register-service*
		  (service :name "server-event/register" (#!key event key mode)
		     (server-event-register event key mode)))))))

;*---------------------------------------------------------------------*/
;*    dump-ajax-table ...                                              */
;*---------------------------------------------------------------------*/
(define (dump-ajax-table table)
   (hashtable-map table
		  (lambda (k conn)
		     (with-access::ajax-connection conn (key marktime buffers)
			(cons (cons key (seconds->date marktime))
			      (map (lambda (buf)
				      (cons (car buf)
					    (buffer-get-all (cdr buf))))
				   buffers))))))

;*---------------------------------------------------------------------*/
;*    hop-event-tables ...                                             */
;*    -------------------------------------------------------------    */
;*    Dump the content of the event table for debug purposes.          */
;*---------------------------------------------------------------------*/
(define (hop-event-tables)
   (with-lock *event-mutex*
      (lambda ()
	 `((*clients-number* ,*clients-number*)
	   (*flash-request-list* ,*flash-request-list*)
	   (*flash-socket-table* ,*flash-socket-table*)
	   (*multipart-request-list* ,*multipart-request-list*)
	   (*multipart-socket-table* ,*multipart-socket-table*)
	   (*websocket-request-list* ,*websocket-request-list*)
	   (*websocket-socket-table* ,*websocket-socket-table*)
	   (*ajax-connection-key-table* ,(dump-ajax-table *ajax-connection-key-table*))))))

;*---------------------------------------------------------------------*/
;*    server-event-gc ...                                              */
;*---------------------------------------------------------------------*/
(define (server-event-gc)

   (define (ajax-connection-expired? conn)
      (with-access::ajax-connection conn (pingtime)
	 (when (>elong pingtime #e0)
	    (>fx (elong->fixnum (-elong (current-seconds) pingtime))
		 (/fx (hop-connection-timeout) 1000)))))
		 
   (define (ajax-connection-ping? conn)
      (with-access::ajax-connection conn (req marktime)
	 (when req
	    (>elong (-elong (current-seconds) marktime)
		    (hop-event-keep-alive)))))

   (define (ajax-connection-ping! conn)
      (with-access::ajax-connection conn (req mutex pingtime)
	 (set! pingtime (current-seconds))
	 (when (http-request? req)
	    (ajax-signal-value req (scheme->response '() req)))))
   
   (define (server-event-ajax-gc)
      ;; cleanup the ping-ed connections that did not respond
      (hashtable-for-each *ajax-connection-key-table*
			  (lambda (key conn)
			     (when (ajax-connection-expired? conn)
				(tprint "*** ajax collecting: " conn)
				(set! *clients-number* (-fx *clients-number* 1))
				(with-access::ajax-connection conn (buffers)
				   (for-each (lambda (buffer)
						(ajax-connection-remove-event!
						 conn (car buffer)))
					     buffers))
				(hashtable-remove! *ajax-connection-key-table*
						   key))))
      ;; ping the old connection
      (hashtable-for-each *ajax-connection-key-table*
			  (lambda (k conn)
			     (when (ajax-connection-ping? conn)
				(tprint "*** ping: " conn)
				(ajax-connection-ping! conn)))))

   (define (flash-connection-ping? e)
      (>elong (-elong (current-seconds) (caddr e)) (hop-event-keep-alive)))
   
   (define (server-event-flash-gc)
      ;; not implemented yet
      (for-each (lambda (e)
		   (when (flash-connection-ping? e)
		      (flash-signal-value (cadr e) *ping* #unspecified)))
		*flash-request-list*))
   
   (server-event-ajax-gc)
   (server-event-flash-gc))

;*---------------------------------------------------------------------*/
;*    server-event-register ...                                        */
;*---------------------------------------------------------------------*/
(define (server-event-register event key mode)
   
   (define (ajax-register-event! req name key)
;*       (tprint "***** ajax-register-event, name=" name " key=" key)  */
      (let ((conn (ajax-find-connection-by-key key)))
	 (if conn
	     (with-lock (ajax-connection-mutex conn)
		(lambda ()
		   ;; mark the conn for protecting it from being collected
		   (ajax-connection-marktime-set! conn (current-seconds))
		   (ajax-connection-pingtime-set! conn #e0)
		   (unless (string=? name "")
		      ;; this is not an automatic re-registry
		      (ajax-connection-add-event! conn name))
		   (let ((vals (ajax-connection-event-pop-all! conn)))
		      (if (pair? vals)
			  (scheme->response vals req)
			  (begin
			     (ajax-connection-abandon-for! conn req)
			     (instantiate::http-response-persistent
				(request req)))))))
	     (let ((conn (ajax-register-new-connection! req key)))
		;; increment the number of connected client
		(set! *clients-number* (+fx 1 *clients-number*))
		(ajax-connection-add-event! conn name)
		(instantiate::http-response-persistent
		   (request req))))))
   
   (define (flash-register-event! req name)
;*       (tprint "***** flash-register-event, name=" name)             */
      (hashtable-update! *flash-socket-table*
			 name
			 (lambda (l) (cons req l))
			 (list req))
      (instantiate::http-response-string
	 (request req)))

   (define (multipart-register-event! req name)
;*       (tprint "***** multipart-register-event, name=" name)         */
      (hashtable-update! *multipart-socket-table*
			 name
			 (lambda (l) (cons req l))
			 (list req))
      (instantiate::http-response-persistent
	 (body (format "HTTP/1.1 200 Ok\r\nContent-type: multipart/x-mixed-replace; boundary=\"~a\"\n\n--~a\nContent-type: text/xml\n\n<r name='~a'></r>\n--~a\n"
		       hop-multipart-key hop-multipart-key name hop-multipart-key))
	 (request req)))

   (define (websocket-register-event! req name key)
;*       (tprint "***** websocket-register-event, name=" name " key=" key) */
;*       (tprint " list=" *websocket-request-list*)                    */
      (let ((c (assq key *websocket-request-list*)))
	 (if (pair? c)
	     (let ((req (cdr c)))
		(hashtable-update! *websocket-socket-table*
				   name
				   (lambda (l) (cons req l))
				   (list req))
		(instantiate::http-response-string
		   (request req)))
	     (error 'server-event-register "Illegal websocket entry" key))))

   (with-lock *event-mutex*
      (lambda ()
	 (if (<fx *clients-number* (hop-event-max-clients))
	     (let ((req (current-request))
		   (key (string->symbol key)))
		;; set an output timeout on the socket
		(output-timeout-set! (socket-output (http-request-socket req))
				     (hop-connection-timeout))
		;; register the client
		(let ((r (cond
			    ((string=? mode "xhr-multipart")
			     (multipart-register-event! req event))
			    ((string=? mode "websocket")
			     (websocket-register-event! req event key))
			    ((string=? mode "flash")
			     (let ((req (cadr (assq key *flash-request-list*))))
				(flash-register-event! req event)))
			    (else
			     (ajax-register-event! req event key)))))
		   ;; cleanup the current connections
		   (server-event-gc)
		   r))
	     (http-service-unavailable event)))))

;*---------------------------------------------------------------------*/
;*    server-event-unregister ...                                      */
;*---------------------------------------------------------------------*/
(define (server-event-unregister event key)
   
   (define (unregister-ajax-event! name key)
      (let ((conn (ajax-find-connection-by-key key)))
	 (when (ajax-connection? conn)
	    (ajax-connection-remove-event! conn name))))
   
   (define (unregister-flash-event! event key)
      (let ((c (assq (string->symbol key) *flash-request-list*)))
	 (when (pair? c)
	    (let ((req (cadr c)))
	       (hashtable-update! *flash-socket-table*
				  event
				  (lambda (l) (delete! req l))
				  '())
	       ;; Ping the client to check it still exists. If the client
	       ;; no longer exists, an error will be raised and the client
	       ;; will be removed from the tables.
	       (flash-signal-value req *ping* #unspecified)))))

   (define (unregister-multipart-event! event key)
      (let ((c (assq (string->symbol key) *multipart-request-list*)))
	 (when (pair? c)
	    (let ((req (cadr c)))
	       (hashtable-update! *multipart-socket-table*
				  event
				  (lambda (l) (delete! req l))
				  '())
	       ;; Ping the client to check it still exists. If the client
	       ;; no longer exists, an error will be raised and the client
	       ;; will be removed from the tables.
	       (multipart-signal-value req (multipart-value *ping* #unspecified))))))

   (define (unregister-websocket-event! event key)
      (let ((c (assq (string->symbol key) *websocket-request-list*)))
	 (when (pair? c)
	    (let ((req (cadr c)))
	       (hashtable-update! *websocket-socket-table*
				  event
				  (lambda (l) (delete! req l))
				  '())
	       ;; Ping the client to check it still exists. If the client
	       ;; no longer exists, an error will be raised and the client
	       ;; will be removed from the tables.
	       (websocket-signal-value req (websocket-value *ping* #unspecified))))))
   
   (with-lock *event-mutex*
      (lambda ()
	 (unregister-websocket-event! event key)
	 (unregister-multipart-event! event key)
	 (unregister-ajax-event! event key)
	 (unregister-flash-event! event key)
	 #f)))

;*---------------------------------------------------------------------*/
;*    flash-close-request! ...                                         */
;*    -------------------------------------------------------------    */
;*    This assumes that the event-mutex has been acquired.             */
;*---------------------------------------------------------------------*/
(define (flash-close-request! req)
   ;; close the socket
   (socket-close (http-request-socket req))
   ;; remove the request from the *flash-request-list*
   (set! *flash-request-list*
	 (filter! (lambda (e) (not (eq? (cadr e) req))) *flash-request-list*))
   ;; remove the request from the *flash-socket-table*
   (hashtable-for-each *flash-socket-table*
		       (lambda (k l)
			  (hashtable-update! *flash-socket-table*
					     k
					     (lambda (l) (delete! req l))
					     '())))
   (hashtable-filter! *flash-socket-table* (lambda (k l) (pair? l))))
   
;*---------------------------------------------------------------------*/
;*    multipart-close-request! ...                                     */
;*    -------------------------------------------------------------    */
;*    This assumes that the event-mutex has been acquired.             */
;*---------------------------------------------------------------------*/
(define (multipart-close-request! req)
   ;; close the socket
   (socket-close (http-request-socket req))
   ;; remove the request from the *multipart-request-list*
   (set! *multipart-request-list*
	 (filter! (lambda (e) (not (eq? (cadr e) req)))
		  *multipart-request-list*))
   ;; remove the request from the *multipart-socket-table*
   (hashtable-for-each *multipart-socket-table*
		       (lambda (k l)
			  (hashtable-update! *multipart-socket-table*
					     k
					     (lambda (l) (delete! req l))
					     '())))
   (hashtable-filter! *multipart-socket-table* (lambda (k l) (pair? l))))
   
;*---------------------------------------------------------------------*/
;*    websocket-close-request! ...                                     */
;*    -------------------------------------------------------------    */
;*    This assumes that the event-mutex has been acquired.             */
;*---------------------------------------------------------------------*/
(define (websocket-close-request! req)
   ;; close the socket
   (socket-close (http-request-socket req))
   ;; remove the request from the *websocket-request-list*
   (set! *websocket-request-list*
	 (filter! (lambda (e) (not (eq? (cdr e) req)))
		  *websocket-request-list*))
   ;; remove the request from the *websocket-socket-table*
   (hashtable-for-each *websocket-socket-table*
		       (lambda (k l)
			  (hashtable-update! *websocket-socket-table*
					     k
					     (lambda (l) (delete! req l))
					     '())))
   (hashtable-filter! *websocket-socket-table* (lambda (k l) (pair? l))))
   
;*---------------------------------------------------------------------*/
;*    hop-event-client-ready? ...                                      */
;*---------------------------------------------------------------------*/
(define (hop-event-client-ready? name)
   
   (define (ajax-event-client-ready? name)
      (pair? (ajax-find-connections-by-name name)))
   
   (define (multipart-event-client-ready? name)
      (let ((l (hashtable-get *multipart-socket-table* name)))
	 (and (pair? l)
	      (any? (lambda (req)
		       (let ((s (http-request-socket req)))
			  (not (socket-down? s))))
		    l))))
   
   (define (websocket-event-client-ready? name)
      (let ((l (hashtable-get *websocket-socket-table* name)))
	 (and (pair? l)
	      (any? (lambda (req)
		       (let ((s (http-request-socket req)))
			  (not (socket-down? s))))
		    l))))
   
   (define (flash-event-client-ready? name)
      (let ((l (hashtable-get *flash-socket-table* name)))
	 (and (pair? l)
	      (any? (lambda (req)
		       (let ((s (http-request-socket req)))
			  (not (socket-down? s))))
		    l))))
   
   (or (multipart-event-client-ready? name)
       (websocket-event-client-ready? name)
       (ajax-event-client-ready? name)
       (flash-event-client-ready? name)))

;*---------------------------------------------------------------------*/
;*    *flash-socket-table* ...                                         */
;*---------------------------------------------------------------------*/
(define *flash-socket-table* (make-hashtable))
(define *flash-request-list* '())

;*---------------------------------------------------------------------*/
;*    *multipart-socket-table*                                         */
;*---------------------------------------------------------------------*/
(define *multipart-socket-table* (make-hashtable))
(define *multipart-request-list* '())

;*---------------------------------------------------------------------*/
;*    *websocket-request-table*                                        */
;*---------------------------------------------------------------------*/
(define *websocket-socket-table* (make-hashtable))
(define *websocket-request-list* '())

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-event ...                          */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-event socket)
   (let ((p (socket-output socket)))
      (fprintf p "<acknowledge name='~a'/>" (http-response-event-name r))
      (display #a000 p)
      (flush-output-port p)
      'persistent))

;*---------------------------------------------------------------------*/
;*    get-server-event-key ...                                         */
;*---------------------------------------------------------------------*/
(define (get-server-event-key port)
   (mutex-lock! *event-mutex*)
   (set! *client-key* (+fx 1 *client-key*))
   (let ((key (format "~a:~a://~a" (hostname) port *client-key*)))
      (mutex-unlock! *event-mutex*)
      key))

;*---------------------------------------------------------------------*/
;*    ajax-signal-value ...                                            */
;*---------------------------------------------------------------------*/
(define (ajax-signal-value req resp)
   (let ((s (http-request-socket req)))
      (with-handler
	 (lambda (e)
	    (tprint "AJAX EVENT ERROR: " e)
	    (unless (&io-error? e) (raise e))
	    #f)
	 (http-response resp s)
	 (socket-close s)
	 #t)))

;*---------------------------------------------------------------------*/
;*    flash-signal-value ...                                           */
;*---------------------------------------------------------------------*/
(define (flash-signal-value req name value)
   (let* ((s (http-request-socket req))
	  (p (socket-output s)))
      (with-handler
	 (lambda (e)
	    (tprint "FLASH EVENT ERROR: " e " thread=" (current-thread))
	    (if (&io-error? e)
		(begin
		   (set! *clients-number* (-fx *clients-number* 1))
		   (flash-close-request! req))
		(raise e)))
	 (begin
	    (let ((p (current-error-port)))
	       (fprintf p "<event name='~a'>" name)
	       (display value p)
	       (display "</event>\n" p))
	    (fprintf p "<event name='~a'>" name)
	    (display value p)
	    (display "</event>\n" p)
	    (display #a000 p)
	    (flush-output-port p)))))

;*---------------------------------------------------------------------*/
;*    multipart-signal-value ...                                       */
;*---------------------------------------------------------------------*/
(define (multipart-signal-value req value)
   (let* ((s (http-request-socket req))
	  (p (socket-output s)))
      (with-handler
	 (lambda (e)
	    (tprint "MULTIPART EVENT ERROR: " e " thread=" (current-thread))
	    (if (&io-error? e)
		(begin
		   (set! *clients-number* (-fx *clients-number* 1))
		   (multipart-close-request! req))
		(raise e)))
	 (begin
	    (fprintf p "Content-type: text/xml\n\n")
	    (display value p)
	    (fprintf p "\n--~a\n" hop-multipart-key p)
	    (flush-output-port p)))))

;*---------------------------------------------------------------------*/
;*    websocket-signal-value ...                                       */
;*---------------------------------------------------------------------*/
(define (websocket-signal-value req value)
   (let* ((s (http-request-socket req))
	  (p (socket-output s)))
      (with-handler
	 (lambda (e)
	    (tprint "WEBSOCKET EVENT ERROR: " e " thread=" (current-thread)
		    " req=" req " value=" value)
	    (if (&io-error? e)
		(begin
		   (set! *clients-number* (-fx *clients-number* 1))
		   (websocket-close-request! req))
		(raise e)))
	 (begin
	    (display #a000 p)
	    (display value p)
	    (display #a255 p)
	    (flush-output-port p)))))

;*---------------------------------------------------------------------*/
;*    enveloppe-value ...                                              */
;*---------------------------------------------------------------------*/
(define (enveloppe-value name value)
   (cond
      ((xml? value)
       (format "<x name='~a'>~a</x>" name (xml->string value (hop-xml-backend))))
      ((string? value)
       (format "<s name='~a'>~a</s>" name value))
      ((integer? value)
       (format "<i name='~a'>~a</i>" name value))
      ((real? value)
       (format "<f name='~a'>~a</f>" name value))
      (else
       (let ((op (open-output-string)))
	  (fprintf op "<j name='~a'><![CDATA[" name)
	  (obj->javascript value op #f)
	  (display "]]></j>" op)
	  (close-output-port op)))))

;*---------------------------------------------------------------------*/
;*    multipart-value ...                                              */
;*---------------------------------------------------------------------*/
(define (multipart-value name value)
   (enveloppe-value name value))
   
;*---------------------------------------------------------------------*/
;*    websocket-value ...                                              */
;*---------------------------------------------------------------------*/
(define (websocket-value name value)
   (enveloppe-value name value))
   
;*---------------------------------------------------------------------*/
;*    json-make-signal-value ...                                       */
;*---------------------------------------------------------------------*/
(define (json-make-signal-value value)
   (cond
      ((xml? value)
       (xml->string value (hop-xml-backend)))
      ((or (string? value) (number? value))
       value)
      (else
       (let ((op (open-output-string)))
	  (display "<json><![CDATA[" op)
	  (obj->javascript value op #f)
	  (display "]]></json>" op)
	  (close-output-port op)))))

;*---------------------------------------------------------------------*/
;*    for-each-socket ...                                              */
;*---------------------------------------------------------------------*/
(define (for-each-socket table name proc)
   (let ((r #f))
      (hashtable-update! table
			 name
			 (lambda (l)
			    (set! r (proc l))
			    l)
			 '())
      r))

;*---------------------------------------------------------------------*/
;*    hop-signal-id ...                                                */
;*---------------------------------------------------------------------*/
(define hop-signal-id 0)

;*---------------------------------------------------------------------*/
;*    hop-event-signal! ...                                            */
;*---------------------------------------------------------------------*/
(define (hop-event-signal! name value)
   
   (define (ajax-event-signal! name value)
      (let ((conns (ajax-find-connections-by-name name)))
	 (let loop ((l conns))
	    (if (pair? l)
		(with-access::ajax-connection (car l) (req mutex)
		   (mutex-lock! mutex)
		   (if (http-request? req)
		       (let* ((r req)
			      (val (unwind-protect
				      (scheme->response
				       (list (list name value)) req)
				      (begin
					 (set! req #f)
					 (mutex-unlock! mutex)))))
			  (tprint "AJAX SIGNAL: " name)
			  (or (ajax-signal-value r val)
			      (loop (cdr l))))
		       (begin
			  (mutex-unlock! mutex)
			  (loop (cdr l)))))
		(when (pair? conns)
		   (tprint "PUSHING: " value)
		   (let* ((conn (car conns))
			  (mutex (ajax-connection-mutex conn)))
		      (mutex-lock! mutex)
		      (ajax-connection-event-push! conn name value)
		      (mutex-unlock! mutex)
		      (tprint "PUSHING: done"))
		   #t)))))
   
   (define (flash-event-signal! name value)
      (for-each-socket
       *flash-socket-table*
       name
       (lambda (l)
	  (when (pair? l)
	     (let ((val (json-make-signal-value value)))
		(flash-signal-value (car l) name val)
		#t)))))

   (define (multipart-event-signal! name value)
      (for-each-socket
       *multipart-socket-table*
       name
       (lambda (l)
	  (when (pair? l)
	     (let ((val (multipart-value name value)))
		(multipart-signal-value (car l) val)
		#t)))))

   (define (websocket-event-signal! name value)
      (for-each-socket
       *websocket-socket-table*
       name
       (lambda (l)
	  (when (pair? l)
	     (let ((val (websocket-value name value)))
		(websocket-signal-value (car l) val)
		#t)))))

   (set! hop-signal-id (-fx hop-signal-id 1))
   (hop-verb 2 (hop-color hop-signal-id hop-signal-id " SIGNAL")
	     ": " name)
   (hop-verb 3 " value=" (with-output-to-string (lambda () (write value))))
   (hop-verb 2 "\n")
   (mutex-lock! *event-mutex*)
   (unwind-protect
      (case (random 4)
	 ((0)
	  (unless (multipart-event-signal! name value)
	     (unless (websocket-event-signal! name value)
		(unless (flash-event-signal! name value)
		   (ajax-event-signal! name value)))))
	 ((1)
	  (unless (flash-event-signal! name value)
	     (unless (ajax-event-signal! name value)
		(unless (multipart-event-signal! name value)
		   (websocket-event-signal! name value)))))
	 ((2)
	  (unless (ajax-event-signal! name value)
	     (unless (multipart-event-signal! name value)
		(unless (websocket-event-signal! name value)
		   (flash-event-signal! name value)))))
	 ((3)
	  (unless (websocket-event-signal! name value)
	     (unless (flash-event-signal! name value)
		(unless (ajax-event-signal! name value)
		   (multipart-event-signal! name value))))))
      (mutex-unlock! *event-mutex*))
	 
   #unspecified)

;*---------------------------------------------------------------------*/
;*    hop-broadcast-id ...                                             */
;*---------------------------------------------------------------------*/
(define hop-broadcast-id 0)

;*---------------------------------------------------------------------*/
;*    hop-event-broadcast! ...                                         */
;*---------------------------------------------------------------------*/
(define (hop-event-broadcast! name value)
   
   (define (ajax-event-broadcast! name value)
      (for-each (lambda (conn)
		   (with-access::ajax-connection conn (req mutex)
		      (with-lock mutex
			 (lambda ()
			    (if (http-request? req)
				(let ((val (scheme->response
					    (list (list name value)) req)))
				   (tprint ">>> hop-event-broadcast, ajax broadcast: " name " "
					   (http-request-socket req))
				   (ajax-signal-value req val)
				   (set! req #f))
				(ajax-connection-event-push! conn name value))))))
		(ajax-find-connections-by-name name)))
   
   (define (flash-event-broadcast! name value)
      (for-each-socket
       *flash-socket-table*
       name
       (lambda (l)
	  (when (pair? l)
	     (let ((val (json-make-signal-value value)))
		(for-each (lambda (req)
			     (flash-signal-value req name val))
			  l))))))

   (define (websocket-event-broadcast! name value)
      (for-each-socket
       *websocket-socket-table*
       name
       (lambda (l)
	  (when (pair? l)
	     (let ((val (websocket-value name value)))
		(for-each (lambda (req)
			     (websocket-signal-value req val))
			  l))))))
       
   (define (multipart-event-broadcast! name value)
      (for-each-socket
       *multipart-socket-table*
       name
       (lambda (l)
	  (when (pair? l)
	     (let ((val (multipart-value name value)))
		(for-each (lambda (req)
			     (multipart-signal-value req val))
			  l))))))

   (set! hop-broadcast-id (-fx hop-broadcast-id 1))
   (hop-verb 2 (hop-color hop-broadcast-id hop-broadcast-id " BROADCAST")
	     ": " name)
   (hop-verb 3 " value=" (with-output-to-string (lambda () (write-circle value))))
   (hop-verb 2 "\n")
   (mutex-lock! *event-mutex*)
   (unwind-protect
      (begin
	 (ajax-event-broadcast! name value)
	 (websocket-event-broadcast! name value)
	 (multipart-event-broadcast! name value)
	 (flash-event-broadcast! name value))
      (mutex-unlock! *event-mutex*))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    hop-event-policy ...                                             */
;*---------------------------------------------------------------------*/
(define (hop-event-policy port)
   (format "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<cross-domain-policy xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"http://www.adobe.com/xml/schemas/PolicyFileSocket.xsd\">
<allow-access-from domain=\"*\" to-ports=\"~a\" />
</cross-domain-policy>" port))

;*---------------------------------------------------------------------*/
;*    hop-event-policy-file ...                                        */
;*---------------------------------------------------------------------*/
(define (hop-event-policy-file req)
   (instantiate::http-response-raw
      (request req)
      (proc (lambda (p)
	       (display (hop-event-policy (http-request-port req)) p)
	       (write-char #a000 p)))))
