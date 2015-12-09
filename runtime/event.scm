;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/event.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 27 05:45:08 2005                          */
;*    Last change :  Wed Dec  9 15:15:40 2015 (serrano)                */
;*    Copyright   :  2005-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The implementation of server events                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_event

   (library web)

   (cond-expand
      (enable-ssl
       (library ssl)))

   (include "thread.sch")

   (include "xml.sch"
	    "service.sch"
	    "verbose.sch"
	    "param.sch")

   (import  __hop_configure
	    __hop_param
	    __hop_thread
	    __hop_types
	    __hop_user
	    __hop_xml-types
	    __hop_xml
	    __hop_html-head
	    __hop_misc
	    __hop_hop
	    __hop_http-response
	    __hop_json
	    __hop_js-comp
	    __hop_read
	    __hop_service
	    __hop_http-response
	    __hop_http-error
	    __hop_websocket)

   (static  (class http-response-event::%http-response-server
	       (name::bstring read-only))
	    
	    (class ajax-connection
	       (mutex::mutex read-only (default (make-mutex)))
	       (req (default #f))
	       key::symbol
	       (buffers::pair-nil (default '()))
	       (marktime::elong (default (current-seconds)))
	       (pingtime::elong (default #e0))
	       (padding::obj (default #f)))

	    ;; buffers for ajax server push
	    (class ajax-buffer
	       (buffer-init)
	       (size::int read-only (default (hop-event-buffer-size)))
	       (mutex::mutex read-only (default (make-mutex)))
	       (cells::pair-nil (default '()))
	       (head::pair-nil (default '()))
	       (tail::pair-nil (default '())))

	    ;; hopsocket for server-to-server listeners
	    (class hopsocket
	       (mutex::mutex read-only (default (make-mutex)))
	       (key::bstring read-only)
	       (socket::socket read-only)
	       (thread::thread read-only)
	       (listeners::pair-nil (default '()))))
	    
   (export  (class event
	       (name::bstring read-only)
	       (target::obj read-only)
	       (stopped::bool (default #f))
	       (value::obj (default #unspecified)))

	    (class websocket-event::event
	       (data::obj (default #unspecified)))

	    (class server-event::websocket-event)

	    (class server
	       (host::bstring read-only)
	       (port::int read-only)
	       (ssl::bool (default #f))
	       (authorization read-only (default #f))
	       (version::bstring read-only (default (hop-version)))
	       (listeners::pair-nil (default '()))
	       (%websocket (default #f))
	       (%key (default #f)))
	    
	    (generic server-init! ::server)
	    
	    (generic add-event-listener! ::obj ::obj ::procedure . l)
	    (generic remove-event-listener! ::obj ::obj ::procedure . l)
	    (generic stop-event-propagation ::event ::bool)

	    (apply-listeners ::obj ::event)
	    
	    (hop-event-init!)
	    (hop-event-tables)
	    (hop-event-signal! ::bstring ::obj)
	    (hop-event-broadcast! ::bstring ::obj)
	    (hop-event-client-ready? ::bstring)
	    (hop-event-policy-file ::http-request)
	    (hop-event-info-service::procedure)
	    (hop-event-register-service::procedure)
	    (hop-event-unregister-service::procedure)))

;*---------------------------------------------------------------------*/
;*    debug ...                                                        */
;*---------------------------------------------------------------------*/
(define debug-ajax #f)
(define debug-ajax-buffer #f)
(define debug-multipart #f)
(define debug-flash #f)

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::obj ...                                    */
;*    -------------------------------------------------------------    */
;*    This generic function is for the time being not used in the Hop  */
;*    server but it acts as a placeholder for future implementations   */
;*    that might need to signal asynchronously event to the server.    */
;*---------------------------------------------------------------------*/
(define-generic (add-event-listener! obj::obj event proc . capture)
   (if (and (string? event) (string? obj))
       (add-server-listener! obj event proc capture)
       (error "add-event-listener!"
	  (format "Illegal ~s listener" event) obj)))

(define-generic (remove-event-listener! obj::obj event proc . capture)
   (if (and (string? event) (string? obj))
       (remove-server-listener! obj event proc capture)
       (error "remove-event-listener!"
	  (format "Illegal ~s listener" event) obj)))

(define-generic (stop-event-propagation event::event default::bool)
   (with-access::event event (stopped)
      (set! stopped #t)))

;*---------------------------------------------------------------------*/
;*    envelope-re ...                                                  */
;*---------------------------------------------------------------------*/
(define envelope-re
   (pregexp "^<([rsxifj]) name='([^']+)'>((?:.|[\n])*)</[rsxifj]>$" ))
(define cdata-re
   (pregexp "^<!\\[CDATA\\[((?:.|[\n])*)\\]\\]>$"))

;*---------------------------------------------------------------------*/
;*    server-init! ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (server-init! srv::server)
   
   (define (message->event k id text)
      (case (string-ref k 0)
	 ((#\i)
	  (let ((val (string->integer text)))
	     (instantiate::server-event
		(target server)
		(name id)
		(value val)
		(data val))))
	 ((#\f)
	  (let ((val (string->real text)))
	     (instantiate::server-event
		(target server)
		(name id)
		(value val)
		(data val))))
	 ((#\x)
	  (let ((val (call-with-input-string text
			(lambda (p)
			   (car
			      (last-pair
				 (parse-html p (string-length text))))))))
	     (instantiate::server-event
		(target server)
		(name id)
		(value val)
		(data val))))
	 ((#\s)
	  (let ((val (url-decode text)))
	     (instantiate::server-event
		(target server)
		(name id)
		(value val)
		(data val))))
	 ((#\j)
	  (let ((val (string->obj
			(url-decode (cadr (pregexp-match cdata-re text))))))
	     (instantiate::server-event
		(target server)
		(name id)
		(value val)
		(data val))))))
   
   (define (parse-message v)
      (with-access::websocket-event v (data)
	 (let ((m (pregexp-match envelope-re data)))
	    (when (pair? m)
	       (let ((k (cadr m))
		     (id (caddr m))
		     (text (cadddr m)))
		  (with-access::server srv (listeners)
		     (let ((ltns (filter-map (lambda (l)
						(when (string=? (car l) id)
						   (cdr l)))
				    listeners)))
			(when (pair? ltns)
			   (let ((event (message->event k id text)))
			      (apply-listeners ltns event))))))))))
   
   (with-access::server srv (host port ssl authorization %websocket %key)
      (unless %websocket
	 (with-hop (string-append (hop-service-base) "/public/server-event/info")
	    :host host :port port
	    :authorization authorization
	    :ssl ssl
	    :sync #t
	    (lambda (v)
	       (let* ((key (vector-ref v 2))
		      (url (format "~a://~a:~a~a/public/server-event/websocket?key=~a"
			      (if ssl "wss" "ws") host port (hop-service-base)
			      key)))
		  (set! %key key)
		  (set! %websocket
		     (instantiate::websocket
			(url url)
			(authorization authorization)
			(onmessages (list parse-message))))
		  (websocket-connect! %websocket)))))))

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::server ...                                 */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! obj::server event proc . capture)
   (server-init! obj)
   (with-access::server obj (host port ssl authorization %key listeners)
      (with-hop (format "~a/public/server-event/register?event=~a&key=~a&mode=websocket"
		   (hop-service-base) event %key)
	 :host host :port port
	 :authorization authorization
	 :ssl ssl
	 (lambda (v)
	    (synchronize *event-mutex*
	       (set! listeners (cons (cons event proc) listeners)))))))

;*---------------------------------------------------------------------*/
;*    remove-event-listener! ::server ...                              */
;*---------------------------------------------------------------------*/
(define-method (remove-event-listener! obj::server event proc . capture)
   (with-access::server obj (listeners)
      (synchronize *event-mutex*
	 (set! listeners
	    (filter (lambda (l)
		       (not (and (eq? (cdr l) proc) (string=? (car l) event))))
	       listeners)))))

;*---------------------------------------------------------------------*/
;*    *event-mutex* ...                                                */
;*---------------------------------------------------------------------*/
(define *event-mutex* (make-mutex "hop-event"))
(define *listener-mutex* (make-mutex "hop-listener"))

;*---------------------------------------------------------------------*/
;*    hop-multipart-key ...                                            */
;*---------------------------------------------------------------------*/
(define hop-multipart-key "hop-multipart")

;*---------------------------------------------------------------------*/
;*    event services ...                                               */
;*---------------------------------------------------------------------*/
(define *port-service* #f)
(define *websocket-service* #t)
(define *register-service* #f)
(define *unregister-service* #f)
(define *init-service* #f)
(define *policy-file-service #f)
(define *close-service* #f)
(define *client-key* 0)
(define *default-request* (instantiate::http-server-request))
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
	 (if (isa? req http-request)
	     (with-access::http-request req (socket)
		(display (socket-hostname socket) p)
		(display ":" p)
		(display (socket-port-number socket) p))
	     "unattached"))))

;*---------------------------------------------------------------------*/
;*    buffer-init ...                                                  */
;*---------------------------------------------------------------------*/
(define (buffer-init buf)
   (with-access::ajax-buffer buf (size head tail cells)
      (set! cells (map! (lambda (i) (cons #f #unspecified)) (iota size)))
      (set! head cells)
      (set! tail cells)))

;*---------------------------------------------------------------------*/
;*    buffer-empty? ...                                                */
;*---------------------------------------------------------------------*/
(define (buffer-empty? buf)
   (with-access::ajax-buffer buf (head mutex)
      (synchronize mutex
	 (not (car (car head))))))

;*---------------------------------------------------------------------*/
;*    buffer-length ...                                                */
;*    -------------------------------------------------------------    */
;*    Assumes mutex acquired.                                          */
;*---------------------------------------------------------------------*/
(define (buffer-length buf)
   (with-access::ajax-buffer buf (head tail cells mutex)
      (let loop ((i 0)
		 (h head))
	 (if (and (pair? h) (caar h))
	     (loop (+fx i 1) (cdr h))
	     i))))
   
;*---------------------------------------------------------------------*/
;*    buffer-push! ...                                                 */
;*---------------------------------------------------------------------*/
(define (buffer-push! buf val)
   (with-access::ajax-buffer buf (head tail cells mutex)
      (synchronize mutex
	 (when debug-ajax-buffer
	    (tprint ">>> buffer-push: len=" (buffer-length buf)
	       " len(cell)=" (length cells)))
	 (if (pair? tail)
	     ;; we have free cells
	     (let ((cell (car tail)))
		(set! tail (cdr tail))
		(set-car! cell #t)
		(set-cdr! cell val))
	     ;; we dont have free cells, we remove the first one
	     (let ((l head)
		   (cell (car head)))
		(set! head (cdr head))
		(set-cdr! l '())
		(set-cdr! cell val)
		(set-cdr! (last-pair head) l)))
	 (when debug-ajax-buffer
	    (tprint "<<< buffer-push: len=" (buffer-length buf)
	       " len(head)=" (length head))))))

;*---------------------------------------------------------------------*/
;*    buffer-pop! ...                                                  */
;*---------------------------------------------------------------------*/
(define (buffer-pop! buf)
   (with-access::ajax-buffer buf (head tail cells mutex)
      (synchronize mutex
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
		   (when debug-ajax-buffer
		      (tprint ">>> buffer-pop: len=" (buffer-length buf)))
		   val)
		(begin
		   (when debug-ajax-buffer
		      (tprint ">>> buffer-pop: len=" 0))
		   #f))))))
   
;*---------------------------------------------------------------------*/
;*    buffer-pop-all! ...                                              */
;*    -------------------------------------------------------------    */
;*    Contrary to buffer-pop!, buffer-pop-all! assumes that the        */
;*    mutex is already acquired.                                       */
;*---------------------------------------------------------------------*/
(define (buffer-pop-all! buf)
   (with-access::ajax-buffer buf (head tail cells)
      (when debug-ajax-buffer
	 (tprint ">>> buffer-pop-all!..." (buffer-length buf)
	    " len(head)=" (length head)))
      (let loop ((res '())
		 (l head))
	 (if (and (pair? l) (caar l))
	     (let ((c (car l)))
		(set-car! c #f)
		(loop (cons (cdr c) res) (cdr l)))
	     (begin
		(set! tail head)
		(when debug-ajax-buffer
		   (tprint "<<< buffer-pop-all len(head)=" (length head)))
		(reverse! res))))))

;*---------------------------------------------------------------------*/
;*    buffer-get-all ...                                               */
;*    -------------------------------------------------------------    */
;*    Debug function                                                   */
;*---------------------------------------------------------------------*/
(define (buffer-get-all buf)
   (with-access::ajax-buffer buf (head tail cells)
      (let loop ((res '())
		 (l head))
	 (if (and (pair? l) (caar l))
	     (let ((c (car l)))
		(loop (cons (cdr c) res) (cdr l)))
	     (reverse! res)))))

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
   [assert (conn)
      (with-access::ajax-connection conn (mutex)
	 (not (symbol? (mutex-state mutex))))]
   (with-access::ajax-connection conn (buffers mutex buffers)
      (let ((cell (assoc name buffers)))
	 (when debug-ajax
	    (tprint ">>>    hop-event-broadcast, ajax.4 push name=" name
	       " cell=" (typeof cell)))
	 (when (pair? cell)
	    (buffer-push! (cdr cell) value)))))

;*---------------------------------------------------------------------*/
;*    ajax-connection-abandon-for! ...                                 */
;*---------------------------------------------------------------------*/
(define (ajax-connection-abandon-for! conn nreq)
   (with-access::ajax-connection conn (req mutex padding)
      (when debug-ajax
	 (tprint "!!! AJAX-CONNECTION-ABANDON: req=" req))
      (when (isa? req http-request)
	 (ajax-signal-value req (scheme->response '() req)))
      (set! req nreq)))

;*---------------------------------------------------------------------*/
;*    ajax-connection-add-event! ...                                   */
;*---------------------------------------------------------------------*/
(define (ajax-connection-add-event! conn name)
   (with-access::ajax-connection conn (mutex buffers)
      (let ((cell (assoc name buffers)))
	 (unless (pair? cell)
	    (when debug-ajax
	       (tprint "ajax-connection-add-event, create buffer for name=" name))
	    (hashtable-update! *ajax-connection-name-table*
	       name
	       (lambda (l) (cons conn l))
	       (list conn))
	    (set! buffers
	       (cons (cons name (instantiate::ajax-buffer)) buffers))))))

;*---------------------------------------------------------------------*/
;*    ajax-connection-remove-event! ...                                */
;*---------------------------------------------------------------------*/
(define (ajax-connection-remove-event! conn name)
   (when debug-ajax
      (tprint "!!! ajax-connection-remove-event name=" name))
   (with-access::ajax-connection conn (mutex buffers)
      (synchronize mutex
	 (let ((cell (assoc name buffers)))
	    (when (pair? cell)
	       (set! buffers (remq! cell buffers))
	       (hashtable-update! *ajax-connection-name-table*
		  name
		  (lambda (l) (remq! conn l))
		  '()))))))

;*---------------------------------------------------------------------*/
;*    ajax-register-new-connection! ...                                */
;*---------------------------------------------------------------------*/
(define (ajax-register-new-connection! req key padding)
   (when debug-ajax
      (tprint "AJAX-REGISTER-NEW-CONNECTION req=" req))
   (let ((conn (instantiate::ajax-connection
		  (key key)
		  (req req)
		  (padding padding))))
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
   
   (with-access::http-request req (header connection socket)
      (let ((host (get-header header host: #f))
	    (version (get-header header sec-websocket-version: "-1")))
	 ;; see http_response.scm for the source code that actually sends
	 ;; the bytes of the response to the client.
	 (with-trace 'event "ws-register-new-connection"
	    (trace-item "protocol-version=" version)
	    (let ((resp (websocket-server-response req key)))
	       ;; register the websocket
	       (synchronize *event-mutex*
		  (set! *websocket-response-list*
		     (cons (cons (string->symbol key) resp)
			*websocket-response-list*))
		  (trace-item "key=" key)
		  (trace-item "socket=" socket )
		  (trace-item "connected clients=" *websocket-response-list*))
	       resp)))))

;*---------------------------------------------------------------------*/
;*    hop-event-init! ...                                              */
;*    -------------------------------------------------------------    */
;*    Flash >= 9.0.115 enforces security policies using "policy files" */
;*    which are described at:                                          */
;*        http://www.adobe.com/devnet/flashplayer/articles/            */
;*          fplayer9_security.html                                     */
;*---------------------------------------------------------------------*/
(define (hop-event-init!)
   (synchronize *event-mutex*
      (when (=fx *client-key* 0)
	 (set! *client-key* (elong->fixnum (current-seconds)))
	 
	 (set! *websocket-service*
	    (service :name "public/server-event/websocket"
	       :id server-event
	       :timeout 0
	       (#!key key)
	       (with-trace 'event "start websocket service"
		  (trace-item "key=" key)
		  (let ((req (current-request)))
		     (with-access::http-request req (header)
			(if (websocket-proxy-request? header)
			    (websocket-proxy-response req)
			    (websocket-register-new-connection! req key)))))))
	 
	 (set! *port-service*
	    (service :name "public/server-event/info"
	       :id server-event
	       :timeout 0
	       ()
	       (let* ((req (current-request))
		      (hd (with-access::http-request req (header) header))
		      (host (assq host: hd))
		      (key (get-server-event-key req))
		      (sock (with-access::http-request req (socket) socket))
		      (port (with-access::http-request req (port) port))
		      (ssl (cond-expand
			      (enable-ssl (ssl-socket? sock))
			      (else #f))))
		  (if (pair? host)
		      (let ((s (string-split (cdr host) ":")))
			 (vector (car s) port key ssl))
		      (with-access::http-request req (host)
			 (vector host port key ssl))))))
	 
	 (set! *init-service*
	    (service :name "public/server-event/init"
	       :id server-event
	       :timeout 0
	       (#!key key)
	       (synchronize *event-mutex*
		  (lambda ()
		     (let ((req (current-request)))
			;; read the Flash's ending zero byte
			(read-byte
			   (socket-input
			      (with-access::http-request req (socket) socket)))
			(set! *flash-request-list*
			   (cons (list (string->symbol key)
				    req
				    (current-seconds))
			      *flash-request-list*))
			;; increments the number of connected clients
			(set! *clients-number* (+fx *clients-number* 1))
			(instantiate::http-response-event
			   (name key)))))))
	 
	 (set! *policy-file-service
	    (service :name "public/server-event/policy-file"
	       :id server-event
	       :timeout 0
	       (#!key port key)
	       (instantiate::http-response-string
		  (content-type "application/xml")
		  (body (hop-event-policy port)))))
	 
	 (set! *close-service*
	    (service :name "public/server-event/close" :timeout 0 (#!key key)
	       :id server-event
	       (synchronize *event-mutex*
		  (lambda ()
		     (let ((key (string->symbol key)))
			(set! *clients-number* (-fx *clients-number* 1))
			(set! *flash-request-list*
			   (filter! (lambda (e)
				       (not (eq? (car e) key)))
			      *flash-request-list*)))))))
	 
	 (set! *unregister-service*
	    (service :name "public/server-event/unregister"
	       :id server-event
	       :timeout 0
	       (#!key event key)
	       (server-event-unregister event key)))
	 
	 (set! *register-service*
	    (service :name "public/server-event/register"
	       :id server-event
	       :timeout 0
	       (#!key event key mode padding)
	       (server-event-register event key mode padding (current-request)))))))

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
   (synchronize *event-mutex*
      `((*clients-number* ,*clients-number*)
	(*flash-request-list* ,*flash-request-list*)
	(*flash-socket-table* ,*flash-socket-table*)
	(*multipart-request-list* ,*multipart-request-list*)
	(*multipart-socket-table* ,*multipart-socket-table*)
	(*websocket-response-list* ,*websocket-response-list*)
	(*websocket-socket-table* ,*websocket-socket-table*)
	(*ajax-connection-key-table* ,(dump-ajax-table *ajax-connection-key-table*)))))

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
	 (when (isa? req http-request)
	    (ajax-signal-value req (scheme->response '() req)))))
   
   (define (server-event-ajax-gc)
      ;; cleanup the ping-ed connections that did not respond
      (hashtable-for-each *ajax-connection-key-table*
			  (lambda (key conn)
			     (when (ajax-connection-expired? conn)
				(when debug-ajax
				   (tprint "*** ajax collecting: " conn))
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
				(when debug-ajax
				   (tprint "*** ping: " conn))
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
(define (server-event-register event key mode padding req)
   
   (define (ajax-register-event! req key name padding)
      (with-trace 'event "ajax-register-event!"
	 (let ((conn (ajax-find-connection-by-key key)))
	    (when debug-ajax
	       (tprint " ajax-register-connection name=" name " conn=" conn))
	    (if conn
		(with-access::ajax-connection conn (marktime pingtime mutex)
		   (synchronize mutex
		      ;; mark the conn for protecting it from being collected
		      (set! marktime (current-seconds))
		      (set! pingtime #e0)
		      (unless (string=? name "")
			 ;; this is not an automatic re-registry
			 (ajax-connection-add-event! conn name))
		      (let ((vals (ajax-connection-event-pop-all! conn)))
			 (when debug-ajax
			    (tprint "   ajax-register-event! ajax vals=" vals))
			 (if (pair? vals)
			     (let ((val (padding-response
					   (scheme->response vals req)
					   padding)))
				(when debug-ajax
				   (tprint "   ajax padding=" padding))
				val)
			     (with-access::ajax-connection conn ((p padding))
				(ajax-connection-abandon-for! conn req)
				(set! p padding)
				(instantiate::http-response-persistent
				   (request req)))))))
		(let ((conn (ajax-register-new-connection! req key padding)))
		   ;; increment the number of connected client
		   (set! *clients-number* (+fx 1 *clients-number*))
		   (ajax-connection-add-event! conn name)
		   (instantiate::http-response-persistent
		      (request req)))))))
   
   (define (flash-register-event! req key name)
      (with-trace 'event "flash-register-event!"
	 (let ((req (cadr (assq key *flash-request-list*))))
	    (hashtable-update! *flash-socket-table*
			       name
			       (lambda (l) (cons req l))
			       (list req))
	    (instantiate::http-response-string))))

   (define (multipart-register-event! req key name)
      (with-trace 'event "multipart-register-event!"
	 (let ((content (format "--~a\nContent-type: text/xml\n\n<r name='~a'></r>\n--~a\n"
				hop-multipart-key name hop-multipart-key))
	       (c (assq key *multipart-request-list*)))
	    (if (pair? c)
		(let ((oreq (cdr c)))
		   ;; we already have a connection for that
		   ;; client, we simply re-use it
		   (hashtable-update! *multipart-socket-table*
				      name
				      (lambda (l) (cons oreq l))
				      (list oreq))
		   ;; we must response as a multipart response because
		   ;; that's the way we have configured the xhr
		   ;; on the client side but we must close the connection
		   (instantiate::http-response-string
		      (header '((Transfer-Encoding: . "chunked")))
		      (content-length #e-2)
		      (content-type (format "multipart/x-mixed-replace; boundary=\"~a\"" hop-multipart-key))
		      (body (format "~a\r\n~a" (integer->string (string-length content) 16) content))))
		;; we create a new connection for that client
		;; multipart never sends a complete answer. In order to
		;; traverse proxy, we use a chunked response
		(with-access::http-request req (socket)
		   (output-port-flush-hook-set!
		      (socket-output socket)
		    (lambda (port size)
		       (output-port-flush-hook-set! port chunked-flush-hook)
		       ""))
		   (set! *multipart-request-list*
			 (cons (cons key req)
			       *multipart-request-list*))
		   (hashtable-update! *multipart-socket-table*
				      name
				      (lambda (l) (cons req l))
				      (list req))
		   (instantiate::http-response-persistent
		      (request req)
		      (body (format "HTTP/1.1 200 Ok\r\nTransfer-Encoding: chunked\r\nContent-type: multipart/x-mixed-replace; boundary=\"~a\"\r\n\r\n~a\r\n~a"
				    hop-multipart-key
				    (integer->string (string-length content) 16)
				    content))))))))
   
   (define (websocket-register-event! req key name)
      (with-trace 'event "ws-register-event!"
	 (let ((c (assq key *websocket-response-list*)))
	    (trace-item "key=" key)
	    (trace-item "name=" name)
	    (trace-item "c=" c)
	    (if (pair? c)
		(let ((resp (cdr c)))
		   (hashtable-update! *websocket-socket-table*
		      name
		      (lambda (l) (cons resp l))
		      (list resp))
		   (instantiate::http-response-string))
		(error "server-event-register" "Illegal websocket entry" key)))))

   (with-trace 'event "ws-register-event!"
      (synchronize *event-mutex*
	 (trace-item "event=" event)
	 (trace-item "key=" key)
	 (trace-item "mode=" mode)
	 (trace-item "padding=" padding)
	 (if (<fx *clients-number* (hop-event-max-clients))
	     (let ((key (string->symbol key)))
		;; set an output timeout on the socket
		(with-access::http-request req (socket) 
		   (output-timeout-set!
		      (socket-output socket) (hop-connection-timeout)))
		;; register the client
		(let ((r (cond
			    ((string=? mode "xhr-multipart")
			     (multipart-register-event! req key event))
			    ((string=? mode "websocket")
			     (websocket-register-event! req key event))
			    ((string=? mode "flash")
			     (flash-register-event! req key event))
			    (else
			     (ajax-register-event! req key event padding)))))
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
	 (when (isa? conn ajax-connection)
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
	       (multipart-signal req
		  (multipart-value *ping* #unspecified))))))

   (define (unregister-websocket-event! event key)
      (let ((c (assq (string->symbol key) *websocket-response-list*)))
	 (when (pair? c)
	    (let ((resp (cdr c)))
	       (hashtable-update! *websocket-socket-table*
		  event
		  (lambda (l) (delete! resp l))
		  '())
	       ;; Ping the client to check if it still exists. If the client
	       ;; no longer exists, an error will be raised and the client
	       ;; will be removed from the tables.
	       (websocket-signal resp
		  (websocket-value *ping* #unspecified))))))
   
   (synchronize *event-mutex*
      (unregister-websocket-event! event key)
      (unregister-multipart-event! event key)
      (unregister-ajax-event! event key)
      (unregister-flash-event! event key)
      #f))

;*---------------------------------------------------------------------*/
;*    flash-close-request! ...                                         */
;*    -------------------------------------------------------------    */
;*    This assumes that the event-mutex has been acquired.             */
;*---------------------------------------------------------------------*/
(define (flash-close-request! req)
   ;; close the socket
   (with-access::http-request req (socket)
      (socket-shutdown socket))
   (set! *clients-number* (-fx *clients-number* 1))
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
   (with-access::http-request req (socket)
      (socket-shutdown socket))
   (set! *clients-number* (-fx *clients-number* 1))
   ;; remove the request from the *multipart-request-list*
   (set! *multipart-request-list*
	 (filter! (lambda (e) (not (eq? (cdr e) req)))
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
(define (websocket-close-request! resp::http-response-websocket)
   (with-access::http-response-websocket resp ((req request))
      ;; close the socket
      (with-access::http-request req (socket)
	 (socket-close socket)
	 (with-trace 'event "ws-close-request!"
	    (trace-item "socket=" socket)))
      ;; decrement the current number of connected clients
      (set! *clients-number* (-fx *clients-number* 1))
      ;; remove the request from the *websocket-response-list*
      (set! *websocket-response-list*
	 (filter! (lambda (e)
		     (not (eq? (cdr e) resp)))
	    *websocket-response-list*))
      ;; remove the response from the *websocket-socket-table*
      (hashtable-for-each *websocket-socket-table*
	 (lambda (k l)
	    (hashtable-update! *websocket-socket-table*
	       k
	       (lambda (l) (delete! resp l))
	       '())))
      (hashtable-filter! *websocket-socket-table* (lambda (k l) (pair? l)))))
   
;*---------------------------------------------------------------------*/
;*    hop-event-client-ready? ...                                      */
;*---------------------------------------------------------------------*/
(define (hop-event-client-ready? name)
   
   (define (ajax-event-client-ready? name)
      (pair? (ajax-find-connections-by-name name)))
   
   (define (multipart-event-client-ready? name)
      (let ((l (hashtable-get *multipart-socket-table* name)))
	 (and (pair? l)
	      (any (lambda (req)
		      (with-access::http-request req (socket)
			 (not (socket-down? socket))))
		    l))))
   
   (define (websocket-event-client-ready? name)
      (let ((l (hashtable-get *websocket-socket-table* name)))
	 (and (pair? l)
	      (any (lambda (req)
		      (with-access::http-request req (socket)
			 (not (socket-down? socket))))
		    l))))
   
   (define (flash-event-client-ready? name)
      (let ((l (hashtable-get *flash-socket-table* name)))
	 (and (pair? l)
	      (any (lambda (req)
		      (with-access::http-request req (socket)
			 (not (socket-down? socket))))
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
(define *websocket-response-list* '())

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-event ...                          */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-event request socket)
   (let ((p (socket-output socket)))
      (with-access::http-response-event r (name)
	 (fprintf p "<acknowledge name='~a'/>" name))
      (display #a000 p)
      (flush-output-port p)
      'persistent))

;*---------------------------------------------------------------------*/
;*    get-server-event-key ...                                         */
;*---------------------------------------------------------------------*/
(define (get-server-event-key req::http-request)
   (synchronize *event-mutex*
      (set! *client-key* (+fx 1 *client-key*))
      (with-access::http-request req (host port)
	 (format "~a:~a://~a" host port *client-key*))))

;*---------------------------------------------------------------------*/
;*    ajax-signal-value ...                                            */
;*---------------------------------------------------------------------*/
(define (ajax-signal-value request resp)
   (with-access::http-request request (socket)
      (with-handler
	 (lambda (e)
	    (when debug-ajax
	       (tprint "AJAX EVENT ERROR: " e))
	    (unless (isa? e &io-error) (raise e))
	    #f)
	 (http-response resp request socket)
	 (socket-shutdown socket)
	 #t)))

;*---------------------------------------------------------------------*/
;*    flash-signal-value ...                                           */
;*---------------------------------------------------------------------*/
(define (flash-signal-value req name value)
   (with-access::http-request req (socket)
      (let ((p (socket-output socket)))
	 (with-handler
	    (lambda (e)
	       (when debug-flash
		  (tprint "FLASH EVENT ERROR: " e " thread=" (current-thread)))
	       (if (isa? e &io-error)
		   (flash-close-request! req)
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
	       (flush-output-port p))))))

;*---------------------------------------------------------------------*/
;*    multipart-signal ...                                             */
;*---------------------------------------------------------------------*/
(define (multipart-signal req vstr::bstring)
   (with-access::http-request req (socket)
      (let ((p (socket-output socket)))
	 (with-handler
	    (lambda (e)
	       (when debug-multipart
		  (tprint "MULTIPART EVENT ERROR: "
		     e " thread=" (current-thread)))
	       (if (isa? e &io-error)
		   (multipart-close-request! req)
		   (raise e)))
	    (begin
	       (fprintf p "Content-type: text/xml\n\n")
	       (display-string vstr p)
	       (fprintf p "\n--~a\n" hop-multipart-key p)
	       (flush-output-port p))))))

;*---------------------------------------------------------------------*/
;*    websocket-signal ...                                             */
;*---------------------------------------------------------------------*/
(define (websocket-signal resp::http-response-websocket vstr::bstring)
   
   (define (hixie-signal-value vstr socket)
      (let ((p (socket-output socket)))
	 (display #a000 p)
	 (display-string vstr p)
	 (display #a255 p)
	 (flush-output-port p)))
   
   (define (hybi-signal-value vstr socket)
      (let ((p (socket-output socket)))
	 (let ((l (string-length vstr)))
	    ;; FIN=1, OPCODE=x1 (text)
	    (display (integer->char #x81) p)
	    (cond
	       ((<=fx l 125)
		;; 1 byte length
		(display (integer->char l) p))
	       ((<=fx l 65535)
		;; 2 bytes length
		(display #a126 p)
		(display (integer->char (bit-rsh l 8)) p)
		(display (integer->char (bit-and l #xff)) p))
	       (else
		;; 4 bytes length
		(display #a127 p)
		(display "\000\000\000\000" p)
		(display (integer->char (bit-rsh l 24)) p)
		(display (integer->char (bit-and (bit-rsh l 16) #xff)) p)
		(display (integer->char (bit-and (bit-rsh l 8) #xff)) p)
		(display (integer->char (bit-and l #xff)) p)))
	    ;; payload data
	    (display-string vstr p)
	    (flush-output-port p))))
   
   (with-access::http-response-websocket resp ((req request))
      (with-access::http-request req (socket)
	 (with-handler
	    (lambda (e)
	       (with-trace 'event "ws-signal-error"
		  (trace-item "err=" e)
		  (trace-item "socket=" socket)
		  (trace-item "thread=" (current-thread))
		  (trace-item "req=" req )
		  (trace-item "vlen=" (string-length vstr))
		  (if (isa? e &io-error)
		      (begin
			 (with-trace 'event "ws-error-close"
			    (websocket-close-request! resp))
			 #f)
		      (raise e))))
	    (with-access::http-response-websocket resp (accept)
	       (with-trace 'event "ws-signal"
		  (trace-item "resp=" resp)
		  (if accept
		      (hybi-signal-value vstr socket)
		      (hixie-signal-value vstr socket))))))))

;*---------------------------------------------------------------------*/
;*    envelope-value ...                                               */
;*---------------------------------------------------------------------*/
(define (envelope-value::bstring name value)
   (cond
      ((isa? value xml)
       (format "<x name='~a'>~a</x>" name (xml->string value (hop-xml-backend))))
      ((string? value)
       (format "<s name='~a'>~a</s>" name (url-path-encode value)))
      ((integer? value)
       (format "<i name='~a'>~a</i>" name value))
      ((real? value)
       (format "<f name='~a'>~a</f>" name value))
      (else
       (let ((op (open-output-string)))
	  (fprintf op "<j name='~a'><![CDATA[" name)
	  (display (url-path-encode (obj->string value 'hop-client)) op)
	  (display "]]></j>" op)
	  (close-output-port op)))))

;*---------------------------------------------------------------------*/
;*    multipart-value ...                                              */
;*---------------------------------------------------------------------*/
(define (multipart-value::bstring name value)
   (envelope-value name value))
   
;*---------------------------------------------------------------------*/
;*    websocket-value ...                                              */
;*---------------------------------------------------------------------*/
(define (websocket-value::bstring name value)
   (envelope-value name value))
   
;*---------------------------------------------------------------------*/
;*    js-make-signal-value ...                                         */
;*---------------------------------------------------------------------*/
(define (js-make-signal-value value)
   (cond
      ((isa? value xml)
       (xml->string value (hop-xml-backend)))
      ((or (string? value) (number? value))
       value)
      (else
       (let ((op (open-output-string)))
	  (display "<javascript>[CDATA[" op)
	  (display (url-path-encode (obj->string value 'hop-client)) op)
	  (display "]]></javascript>" op)
	  (close-output-port op)))))

;*---------------------------------------------------------------------*/
;*    for-each-socket ...                                              */
;*---------------------------------------------------------------------*/
(define (for-each-socket table name proc)
   (let ((l (hashtable-get table name)))
      (when l
	 (proc l))))

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
		(with-access::ajax-connection (car l) (req mutex padding)
		   (unless (synchronize mutex
			      (when (isa? req http-request)
				 (let* ((r req)
					(val (unwind-protect
						(padding-response
						   (scheme->response
						      (list (list name value)) req)
						   padding)
						(begin
						   (set! req #f)
						   (mutex-unlock! mutex)))))
				    (ajax-signal-value r val))))
		      (loop (cdr l))))
		(when (pair? conns)
		   (when debug-ajax
		      (tprint "PUSHING: " name " value=" value))
		   (let ((conn (car conns)))
		      (with-access::ajax-connection conn (mutex)
			 (synchronize mutex
			    (ajax-connection-event-push! conn name value)))
		      (when debug-ajax
			 (tprint "PUSHING: done")))
		   #t)))))
   
   (define (flash-event-signal! name value)
      (for-each-socket
       *flash-socket-table*
       name
       (lambda (l)
	  (when (pair? l)
	     (let ((val (js-make-signal-value value)))
		(flash-signal-value (car l) name val)
		#t)))))

   (define (multipart-event-signal! name value)
      (for-each-socket
       *multipart-socket-table*
       name
       (lambda (l)
	  (when (pair? l)
	     (let ((val (multipart-value name value)))
		(when debug-multipart
		   (tprint "MULTIPART SIGNAL: " name))
		(multipart-signal (car l) val)
		#t)))))

   (define (websocket-event-signal! name value)
      (for-each-socket
       *websocket-socket-table*
       name
       (lambda (l)
	  (when (pair? l)
	     (let ((val (websocket-value name value)))
		(websocket-signal (car l) val)
		#t)))))

   (set! hop-signal-id (-fx hop-signal-id 1))
   (hop-verb 2 (hop-color hop-signal-id hop-signal-id " SIGNAL")
	     ": " name)
   (hop-verb 3 " value=" (with-output-to-string (lambda () (write value))))
   (hop-verb 2 "\n")
   (synchronize *event-mutex*
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
		   (multipart-event-signal! name value)))))))
	 
   #unspecified)

;*---------------------------------------------------------------------*/
;*    hop-event-broadcast! ...                                         */
;*---------------------------------------------------------------------*/
(define (hop-event-broadcast! name value)
   
   (define (ajax-event-broadcast! name value)
      (when debug-ajax
	 (tprint ">>> hop-event-broadcast, ajax broadcast: " name))
      (for-each (lambda (conn)
		   (when debug-ajax
		      (tprint ">>>    hop-event-broadcast, ajax.2 conn=" conn))
		   (with-access::ajax-connection conn (req mutex padding)
		      (synchronize mutex
			 (when debug-ajax
			    (tprint ">>>    hop-event-broadcast, ajax.3 req="
			       req))
			 (if (isa? req http-request)
			     (let ((val (padding-response
					   (scheme->response
					      (list (list name value)) req)
					   padding)))
				(ajax-signal-value req val)
				(set! req #f))
			     (ajax-connection-event-push! conn name value)))))
		(ajax-find-connections-by-name name)))
   
   (define (flash-event-broadcast! name value)
      (let ((val (js-make-signal-value value)))
	 (for-each-socket
	    *flash-socket-table*
	    name
	    (lambda (l)
	       (when (pair? l)
		  (for-each (lambda (req)
			       (flash-signal-value req name val))
		     l))))))

   (define (websocket-event-broadcast! name value)
      (with-trace 'event "ws-event-broadcast!"
	 (trace-item "name=" name)
	 (trace-item "value="
	    (if (or (string? value) (symbol? value) (number? value))
		value
		(typeof value)))
	 (let ((val (websocket-value name value)))
	    (for-each-socket
	       *websocket-socket-table*
	       name
	       (lambda (l)
		  (with-trace 'event "ws-event-broadcast"
		     (trace-item "name=" name)
		     (trace-item "# clients=" (length l))
		     (trace-item "cients="
			(map (lambda (resp)
				(with-access::http-response-websocket resp (request)
				   (with-access::http-request request (socket)
				      socket)))
			   l))
		     (when (pair? l)
			(for-each (lambda (resp)
				     (websocket-signal resp val))
			   l))))))))
       
   (define (multipart-event-broadcast! name value)
      (let ((val (multipart-value name value)))
	 (for-each-socket
	    *multipart-socket-table*
	    name
	    (lambda (l)
	       (when (pair? l)
		  (for-each (lambda (req)
			       (multipart-signal req val))
		     l))))))

   (synchronize *event-mutex*
      (ajax-event-broadcast! name value)
      (websocket-event-broadcast! name value)
      (multipart-event-broadcast! name value)
      (flash-event-broadcast! name value))
   
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
      (connection 'close)
      (proc (lambda (p)
	       (with-access::http-request req (port)
		  (display (hop-event-policy port) p))
	       (write-char #a000 p)))))

;*---------------------------------------------------------------------*/
;*    *server-listeners* ...                                           */
;*---------------------------------------------------------------------*/
(define *server-listeners*
   (make-hashtable 16))

;*---------------------------------------------------------------------*/
;*    add-server-listener! ...                                         */
;*---------------------------------------------------------------------*/
(define (add-server-listener! obj::bstring event::bstring proc::procedure capture)

   (define (parse-host host auth)
      (let ((l (string-split host ":")))
	 (if (null? (cdr l))
	     (values host 8080 #f)
	     (values (car l) (string->integer (cadr l)) auth))))
   
   (define (parse-authenticated-host str)
      (let ((s (string-split str "@")))
	 (if (null? (cdr s))
	     (parse-host str #f)
	     (parse-host (cadr s) (car s)))))

   (define (register-event! host port auth hs)
      (with-access::hopsocket hs (mutex key listeners key)
	 (with-hop (*register-service* :event obj :key key :mode "websocket")
	    :authorization auth
	    :host host :port port
	    (lambda (_)
	       (synchronize mutex
		  (set! listeners
		     (cons (cons obj proc) listeners)))))))

   (define (get-hopsocket! host port auth key)
      (let* ((sock (make-client-socket host port))
	     (path (format "~a/public/server-event/websocket?key=~a"
		      (hop-service-base)
		      key)))
	 (letrec* ((th (cond-expand
			  (enable-threads
			     (instantiate::hopthread
				(body (lambda () (hopsocket-loop hs)))))
			  (else
			   (error "add-server-listener!"
			      "server listener requires thread support"
			      #f))))
		   (hs (instantiate::hopsocket
			  (key key)
			  (socket sock)
			  (thread th))))
	    (hashtable-put! *server-listeners* event hs)
	    (http :socket sock
	       :host host
	       :port port
	       :path path
	       :authorization auth
	       :header `((origin ,(format "~a:~a" (hostname) (hop-port)))
			 (WebSocket-Protocol "ws")))
	    (let* ((in (socket-input sock))
		   (line (read-line in))
		   (upgrade (read-line in))
		   (conn (read-line in))
		   (origin (read-line in))
		   (location (read-line in))
		   (proto (read-line in)))
	       (unless (string=? proto "")
		  (read-line in)))
	    (thread-start! th)
	    hs)))

   (define (parse-websocket-envelope buf len)
      (cond
	 ((pregexp-match "<x name='([^']*)'>"buf)
	  => (lambda (m)
		(let* ((name (cadr m))
		       (xml (substring buf
			       (+fx (string-length name) 11)
			       (-fx len 4))))
		   (values name xml))))
	 ((pregexp-match "<s name='([^']*)'>" buf)
	  => (lambda (m)
		(let* ((name (cadr m))
		       (url (substring buf
			       (+fx (string-length name) 11)
			       (-fx len 4))))
		   (values name (url-decode url)))))
	 ((pregexp-match "<i name='([^']*)'>"buf)
	  => (lambda (m)
		(let* ((name (cadr m))
		       (int (substring buf
			       (+fx (string-length name) 11)
			       (-fx len 4))))
		   (values name (string->integer int)))))
	 ((pregexp-match "<f name='([^']*)'>" buf)
	  => (lambda (m)
		(let* ((name (cadr m))
		       (real (substring buf
				(+fx (string-length name) 11)
				(-fx len 4))))
		   (values name (string->real real)))))
	 ((pregexp-match "<j name='([^']*)'><![[]CDATA[[]" buf)
	  => (lambda (m)
		(let* ((name (cadr m))
		       (js (url-decode
			      (substring buf
				 (+fx (string-length name) 20)
				 (-fx  len 7)))))
		   (values name (string->obj js)))))
	 (else
	  (error "hopsocket" "Illegal websocket envelope" buf))))

   (define (read-websocket-event in)
      (let ((buf (make-string 128))
	    (len 128))
	 (let loop ((i 0))
	    (let ((c (read-byte in)))
	       (cond
		  ((=fx c 255)
		   (parse-websocket-envelope buf i))
		  ((=fx i len)
		   (let ((nbuf (make-string (*fx 2 len))))
		      (blit-string! buf 0 nbuf 0 len)
		      (set! buf nbuf)
		      (set! len (*fx 2 len))
		      (string-set! buf i (integer->char c))
		      (loop (+fx i 1))))
		  (else
		   (string-set! buf i (integer->char c))
		   (loop (+fx i 1))))))))

   (define (hopsocket-raise-event hs name value)
      (with-access::hopsocket hs (mutex listeners)
	 (let ((l (synchronize mutex
		     (filter (lambda (c) (string=? (car c) name))
			listeners))))
	    (when (pair? l)
	       (let ((e (instantiate::server-event
			   (name name)
			   (value value)
			   (target event))))
		  (let loop ((l l))
		     (when (pair? l)
			((cdar l) e)
			(with-access::event e (stopped)
			   (unless stopped
			      (loop (cdr l)))))))))))

   (define (hopsocket-loop hs)
      (with-access::hopsocket hs (socket)
	 (let ((in (socket-input socket)))
	    (let loop ()
	       (let ((o (read-byte in)))
		  (cond
		     ((eof-object? o)
		      ;; close silently
		      #f)
		     ((=fx o 0)
		      (multiple-value-bind (name value)
			 (read-websocket-event in)
			 (hopsocket-raise-event hs name value)
			 (loop)))
		     (else
		      (error "hopsocket"
			 "Illegal server response"
			 (format "{~a}~a" o (read-string in))))))))))
      
   (multiple-value-bind (host port auth)
      (parse-authenticated-host event)
      (let ((hs (synchronize *listener-mutex*
		   (hashtable-get *server-listeners* event))))
	 (if (isa? hs hopsocket)
	     (register-event! host port auth hs)
	     (with-hop (*port-service*)
		:host host :port port
		:authorization auth
		(lambda (v)
		   (let ((key (vector-ref v 2)))
		      (synchronize *listener-mutex*
			 (let ((hs (hashtable-get *server-listeners* event)))
			    (if (isa? hs hopsocket)
				(register-event! host port auth hs)
				(let ((hs (get-hopsocket! host port auth key)))
				   (register-event! host port auth hs))))))))))))

;*---------------------------------------------------------------------*/
;*    remove-server-listener! ...                                      */
;*---------------------------------------------------------------------*/
(define (remove-server-listener! obj event proc capture)
   (let ((hs (synchronize *listener-mutex*
		(hashtable-get *server-listeners* event))))
      (when (isa? hs hopsocket)
	 (with-access::hopsocket hs (mutex socket listeners)
	    (synchronize mutex
	       (set! listeners (filter! (lambda (l)
					   (not (eq? (car l) obj)))
				  listeners))
	       (when (null? listeners)
		  (socket-shutdown socket)
		  (synchronize *listener-mutex*
		     (hashtable-remove! *server-listeners* event))))))))

;*---------------------------------------------------------------------*/
;*    padding-response ...                                             */
;*---------------------------------------------------------------------*/
(define (padding-response rep padding)
   (when padding
      (with-access::http-response-hop rep ((p padding) content-type)
	 (set! p padding)
	 (set! content-type "application/x-javascript")))
   rep)

;*---------------------------------------------------------------------*/
;*    hop-event-info-service ...                                       */
;*---------------------------------------------------------------------*/
(define (hop-event-info-service)
   (hop-event-init!)
   *port-service*)

;*---------------------------------------------------------------------*/
;*    hop-event-register-service ...                                   */
;*---------------------------------------------------------------------*/
(define (hop-event-register-service)
   (hop-event-init!)
   *register-service*)
   
;*---------------------------------------------------------------------*/
;*    hop-event-unregister-service ...                                 */
;*---------------------------------------------------------------------*/
(define (hop-event-unregister-service)
   (hop-event-init!)
   *unregister-service*)
   
;*---------------------------------------------------------------------*/
;*    apply-listeners ...                                              */
;*---------------------------------------------------------------------*/
(define (apply-listeners listeners::obj e::event)
   (let loop ((l listeners))
      (when (pair? l)
	 ((car l) e)
	 (with-access::event e (stopped)
	    (unless stopped
	       (loop (cdr l)))))))

