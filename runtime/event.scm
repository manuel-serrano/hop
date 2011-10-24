;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/runtime/event.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 27 05:45:08 2005                          */
;*    Last change :  Tue Oct 18 20:15:48 2011 (serrano)                */
;*    Copyright   :  2005-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The implementation of server events                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_event

   (library web)
   
   (cond-expand
      (enable-threads
       (library pthread)))

   (include "xml.sch"
	    "service.sch"
	    "param.sch")

   (import  __hop_configure
	    __hop_param
	    __hop_types
	    __hop_user
	    __hop_xml-types
	    __hop_xml
	    __hop_html-head
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

	    (class server-event::event)
	    
	    (generic add-event-listener! ::obj ::obj ::procedure . l)
	    (generic remove-event-listener! ::obj ::obj ::procedure . l)
	    (generic stop-event-propagation ::event ::bool)

	    (hop-event-init! ::obj)
	    (hop-event-tables)
	    (hop-event-signal! ::bstring ::obj)
	    (hop-event-broadcast! ::bstring ::obj)
	    (hop-event-client-ready? ::bstring)
	    (hop-event-policy-file ::http-request)))

;*---------------------------------------------------------------------*/
;*    debug ...                                                        */
;*---------------------------------------------------------------------*/
(define debug-ajax #f)
(define debug-ajax-buffer #f)
(define debug-websocket #f)
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
       (error "add-event-listener!" "Illegal listener" obj)))

(define-generic (remove-event-listener! obj::obj event proc . capture)
   (if (and (string? event) (string? obj))
       (remove-server-listener! obj event proc capture)
       (error "remove-event-listener!" "Illegal listener" obj)))

(define-generic (stop-event-propagation event::event default::bool)
   (event-stopped-set! event #t))

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
   (with-access::ajax-buffer buf (size head tail cells)
      (set! cells (map! (lambda (i) (cons #f #unspecified)) (iota size)))
      (set! head cells)
      (set! tail cells)))

;*---------------------------------------------------------------------*/
;*    buffer-empty? ...                                                */
;*---------------------------------------------------------------------*/
(define (buffer-empty? buf)
   (with-access::ajax-buffer buf (head mutex)
      (mutex-lock! mutex)
      (let ((r (not (car (car head)))))
	 (mutex-unlock! mutex)
	 r)))

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
      (mutex-lock! mutex)
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
	    " len(head)=" (length head)))
      (mutex-unlock! mutex)))

;*---------------------------------------------------------------------*/
;*    buffer-pop! ...                                                  */
;*---------------------------------------------------------------------*/
(define (buffer-pop! buf)
   (with-access::ajax-buffer buf (head tail cells mutex)
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
		(when debug-ajax-buffer
		   (tprint ">>> buffer-pop: len=" (buffer-length buf)))
		(mutex-unlock! mutex)
		val)
	     (begin
		(when debug-ajax-buffer
		   (tprint ">>> buffer-pop: len=" 0))
		(mutex-unlock! mutex)
		#f)))))
   
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
   [assert (conn) (not (symbol? (mutex-state (ajax-connection-mutex conn))))]
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
	    (tprint "ajax-connection-add-event, create buffer for name=" name)
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
   (tprint "!!! ajax-connection-remove-event name=" name)
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
(define (ajax-register-new-connection! req key padding)
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
   
   (define (websocket-server-location host)
      (format "ws://~a/~a/server-event/websocket?key=~a"
	 (or host (format "~a:~a" (http-request-host req) (http-request-port req)))
	 (hop-initial-weblet)
	 key))
   
   (define (protocol76-key-value s)
      (let ((l (string-length s)))
	 (let loop ((i 0)
		    (n #l0)
		    (b #l0))
	    (if (=fx i l)
		(/llong n b)
		(let ((c (string-ref s i)))
		   (cond
		      ((char-numeric? c)
		       (let ((k (fixnum->llong
				   (-fx (char->integer c) (char->integer #\0)))))
			  (loop (+fx i 1) (+llong (*llong n #l10) k) b)))
		      ((char=? c #\space)
		       (loop (+fx i 1) n (+llong b #l1)))
		      (else
		       (loop (+fx i 1) n b))))))))
   
   (define (blit-string-int32-big-endian! n::llong buf::bstring o::int)
      (let ((b3 (llong->fixnum (bit-andllong n #l255)))
	    (b2 (llong->fixnum (bit-andllong (bit-rshllong n 8) #l255)))
	    (b1 (llong->fixnum (bit-andllong (bit-rshllong n 16) #l255)))
	    (b0 (llong->fixnum (bit-andllong (bit-urshllong n 24) #l255))))
	 (string-set! buf (+fx o 3) (integer->char b3))
	 (string-set! buf (+fx o 2) (integer->char b2))
	 (string-set! buf (+fx o 1) (integer->char b1))
	 (string-set! buf o (integer->char b0))))
   
   (define (webwocket-hixie-protocol-76 key1 key2 req)
      (when debug-websocket
	 (tprint "websocket-protocol76 " key1 " " key2))
      ;; Handshake known as draft-hixie-thewebsocketprotocol-76
      ;; see http://www.whatwg.org/specs/web-socket-protocol/
      ;; http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-03
      ;; for each key, compute the following:
      ;;   extract numerics (0-9) from value, and convert to int base 10.
      ;;   Divide by number of space characters in value!
      (let ((k1 (protocol76-key-value key1))
	    (k2 (protocol76-key-value key2))
	    (key3 (read-chars 8 (socket-input (http-request-socket req))))
	    (buf (make-string 16 #\0)))
	 (blit-string-int32-big-endian! k1 buf 0)
	 (blit-string-int32-big-endian! k2 buf 4)
	 (blit-string! key3 0 buf 8 8)
	 (string-hex-intern! (md5sum buf))))
   
   (define (websocket-sec-challenge req header)
      ;; newer websocket protocols includes a 3 keys challenge
      ;; we check which version we are asked.
      (let ((key1 (get-header header sec-websocket-key1: #f)))
	 (when key1
	    (let ((key2 (get-header header sec-websocket-key2: #f)))
	       (when key2
		  (webwocket-hixie-protocol-76 key1 key2 req))))))
   
   (define (websocket-hixie-protocol header read)
      (instantiate::http-response-websocket
	 (request req)
	 (start-line "HTTP/1.1 101 Web Socket Protocol Handshake")
	 (location (websocket-server-location host))
	 (origin (get-header header origin: "localhost"))
	 (protocol (get-header header WebSocket-Protocol: #f))
	 (connection 'Upgrade)
	 (sec (websocket-sec-challenge req header))))      
   
   (define (websocket-hybi-protocol header req)
      ;; http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-08
      (when debug-websocket
	 (tprint "websocket-hybi-protocol-08, header: " header))
      (let* ((key (get-header header sec-websocket-key: #f))
	     (i (string-index key #\space))
	     (pkey (if i (car (string-split key #\space)) key))
	     (guid "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
	     (akey (string-append pkey guid))
	     (skey (sha1sum-string akey)))
	 (instantiate::http-response-websocket
	    (request req)
	    (start-line "HTTP/1.1 101 Switching Protocols")
	    (connection 'Upgrade)
	    (protocol (get-header header WebSocket-Protocol: #f))
	    (accept (base64-encode (string-hex-intern! skey))))))
   
   (with-access::http-request req (header connection socket)
      (let ((host (get-header header host: #f))
	    (version (get-header header sec-websocket-version: #f)))
	 ;; see http_response.scm for the source code that actually sends
	 ;; the bytes of the response to the client.
	 (when debug-websocket
	    (tprint "websocket-register, protocol-version: " version))
	 (let ((resp (case (string->integer version)
			((7 8 9 10)
			 (websocket-hybi-protocol header req))
			(else
			 (websocket-hixie-protocol header req)))))
	    ;; register the websocket
	    (with-lock *event-mutex*
	       (lambda ()
		  (set! *websocket-response-list*
		     (cons (cons (string->symbol key) resp)
			*websocket-response-list*))))
	    resp))))

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
		  (service :name "server-event/websocket" :id server-event
		     (#!key key)
		     (when debug-websocket
			(tprint "register websocket key=" key))
		     (websocket-register-new-connection! (current-request) key)))
	    
	    (set! *port-service*
		  (service :name "server-event/info" :id server-event ()
		     (let* ((req (current-request))
			    (hd (http-request-header req))
			    (host (assq host: hd))
			    (key (get-server-event-key req))
			    (port (http-request-port req)))
			(if (pair? host)
			    (let ((s (string-split (cdr host) ":")))
			       (vector (car s) port key))
			    (vector (http-request-host req) port key)))))
	    
	    (set! *init-service*
		  (service :name "server-event/init" :id server-event
		     (#!key key)
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
		  (service :name "server-event/policy-file" :id server-event
		     (#!key port key)
		     (instantiate::http-response-string
			(request (current-request))
			(content-type "application/xml")
			(body (hop-event-policy port)))))

	    (set! *close-service*
		  (service :name "server-event/close" (#!key key)
		     :id server-event
		     (with-lock *event-mutex*
			(lambda ()
			   (let ((key (string->symbol key)))
			      (set! *clients-number* (-fx *clients-number* 1))
			      (set! *flash-request-list*
				    (filter! (lambda (e)
						(not (eq? (car e) key)))
					     *flash-request-list*)))))))

	    (set! *unregister-service*
		  (service :name "server-event/unregister" :id server-event
		     (#!key event key)
		     (server-event-unregister event key)))
	    
	    (set! *register-service*
		  (service :name "server-event/register" :id server-event
		     (#!key event key mode padding)
		     (server-event-register event key mode padding)))))))

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
	   (*websocket-response-list* ,*websocket-response-list*)
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
(define (server-event-register event key mode padding)
   
   (define (ajax-register-event! req key name padding)
      (with-trace 3 "ajax-register-event!"
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
			 (when debug-ajax
			    (tprint "   ajax-register-event! ajax vals=" vals))
			 (if (pair? vals)
			     (let ((val (scheme->response vals req)))
				(when padding
				   (when debug-ajax
				      (tprint "   ajax padding=" padding))
				   (http-response-js-padding-set! val padding))
				val)
			     (begin
				(ajax-connection-abandon-for! conn req)
				(ajax-connection-padding-set! conn padding)
				(instantiate::http-response-persistent
				   (request req)))))))
		(let ((conn (ajax-register-new-connection! req key padding)))
		   ;; increment the number of connected client
		   (set! *clients-number* (+fx 1 *clients-number*))
		   (ajax-connection-add-event! conn name)
		   (instantiate::http-response-persistent
		      (request req)))))))
   
   (define (flash-register-event! req key name)
      (with-trace 3 "flash-register-event!"
	 (let ((req (cadr (assq key *flash-request-list*))))
	    (hashtable-update! *flash-socket-table*
			       name
			       (lambda (l) (cons req l))
			       (list req))
	    (instantiate::http-response-string
	       (request req)))))

   (define (multipart-register-event! req key name)
      (with-trace 3 "multipart-register-event!"
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
		      (request req)
		      (header '((Transfer-Encoding: . "chunked")))
		      (content-length #e-2)
		      (content-type (format "multipart/x-mixed-replace; boundary=\"~a\"" hop-multipart-key))
		      (body (format "~a\r\n~a" (integer->string (string-length content) 16) content))))
		;; we create a new connection for that client
		;; multipart never sends a complete answer. In order to
		;; traverse proxy, we use a chunked response
		(begin
		   (output-port-flush-hook-set!
		    (socket-output (http-request-socket req))
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
      (with-trace 3 "websocket-register-event!"
	 (let ((c (assq key *websocket-response-list*)))
	    (when debug-websocket
	       (tprint "websocket-register-event key=" key " name=" name " c=" c))
	    (if (pair? c)
		(let ((resp (cdr c)))
		   (hashtable-update! *websocket-socket-table*
				      name
				      (lambda (l) (cons resp l))
				      (list resp))
		   (instantiate::http-response-string
		      (request (http-response-websocket-request resp))))
		(error "server-event-register" "Illegal websocket entry" key)))))

   (with-trace 2 "server-register-event!"
      (with-lock *event-mutex*
	 (lambda ()
	    (when (or debug-ajax debug-websocket debug-multipart debug-flash)
	       (tprint ">>> SERVER-EVENT-REGISTER: event=[" event "] key="
		  key " mode=" mode " padding=" padding))
	    (if (<fx *clients-number* (hop-event-max-clients))
		(let ((req (current-request))
		      (key (string->symbol key)))
		   (trace-item "event=" event " key=" key)
		   ;; set an output timeout on the socket
		   (output-timeout-set!
		    (socket-output (http-request-socket req))
		    (hop-connection-timeout))
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
		(http-service-unavailable event))))))

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
      (let ((c (assq (string->symbol key) *websocket-response-list*)))
	 (when (pair? c)
	    (let ((resp (cadr c)))
	       (hashtable-update! *websocket-socket-table*
				  event
				  (lambda (l) (delete! resp l))
				  '())
	       ;; Ping the client to check it still exists. If the client
	       ;; no longer exists, an error will be raised and the client
	       ;; will be removed from the tables.
	       (websocket-signal-value resp (websocket-value *ping* #unspecified))))))
   
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
   (let ((req (http-response-websocket-request resp)))
      ;; close the socket
      (socket-close (http-request-socket req))
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
(define *websocket-response-list* '())

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
(define (get-server-event-key req::http-request)
   (mutex-lock! *event-mutex*)
   (set! *client-key* (+fx 1 *client-key*))
   (let ((key (format "~a:~a://~a"
		      (http-request-host req)
		      (http-request-port req)
		      *client-key*)))
      (mutex-unlock! *event-mutex*)
      key))

;*---------------------------------------------------------------------*/
;*    ajax-signal-value ...                                            */
;*---------------------------------------------------------------------*/
(define (ajax-signal-value req resp)
   (let ((s (http-request-socket req)))
      (with-handler
	 (lambda (e)
	    (when debug-ajax
	       (tprint "AJAX EVENT ERROR: " e))
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
	    (when debug-flash
	       (tprint "FLASH EVENT ERROR: " e " thread=" (current-thread)))
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
	    (when debug-multipart
	       (tprint "MULTIPART EVENT ERROR: " e " thread=" (current-thread)))
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
(define (websocket-signal-value resp::http-response-websocket value)
   
   (define (hixie-signal-value value p)
      (display #a000 p)
      (display value p)
      (display #a255 p)
      (flush-output-port p))

   (define (hybi-signal-value value p)
      (let ((ps (open-output-string)))
	 (display value ps)
	 (let* ((s (close-output-port ps))
		(l (string-length s)))
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
	    (display s p)
	    (flush-output-port p))))
   
   (let* ((req (http-response-websocket-request resp))
	  (s (http-request-socket req))
	  (p (socket-output s)))
      (with-handler
	 (lambda (e)
	    (when debug-websocket
	       (tprint "WEBSOCKET EVENT ERROR: " e " thread=" (current-thread)
		  " req=" req " value=" value))
	    (if (&io-error? e)
		(begin
		   (set! *clients-number* (-fx *clients-number* 1))
		   (websocket-close-request! resp))
		(raise e)))
	 (with-access::http-response-websocket resp (accept)
	    (if accept
		(hybi-signal-value value p)
		(hixie-signal-value value p))))))

;*---------------------------------------------------------------------*/
;*    enveloppe-value ...                                              */
;*---------------------------------------------------------------------*/
(define (enveloppe-value name value)
   (cond
      ((xml? value)
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
		(with-access::ajax-connection (car l) (req mutex padding)
		   (mutex-lock! mutex)
		   (if (http-request? req)
		       (let* ((r req)
			      (val (unwind-protect
				      (scheme->response
				       (list (list name value)) req)
				      (begin
					 (set! req #f)
					 (mutex-unlock! mutex)))))
			  (http-response-js-padding-set! val padding)
			  (or (ajax-signal-value r val)
			      (loop (cdr l))))
		       (begin
			  (mutex-unlock! mutex)
			  (loop (cdr l)))))
		(when (pair? conns)
		   (when debug-ajax
		      (tprint "PUSHING: " name " value=" value))
		   (let* ((conn (car conns))
			  (mutex (ajax-connection-mutex conn)))
		      (mutex-lock! mutex)
		      (ajax-connection-event-push! conn name value)
		      (mutex-unlock! mutex)
		      (when debug-ajax
			 (tprint "PUSHING: done")))
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
		(when debug-multipart
		   (tprint "MULTIPART SIGNAL: " name))
		(multipart-signal-value (car l) val)
		#t)))))

   (define (websocket-event-signal! name value)
      (for-each-socket
       *websocket-socket-table*
       name
       (lambda (l)
	  (when (pair? l)
	     (let ((val (websocket-value name value)))
		(when debug-websocket
		   (tprint "websocket signal: " name))
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
		      (with-lock mutex
			 (lambda ()
			    (when debug-ajax
			       (tprint ">>>    hop-event-broadcast, ajax.3 req=" req))
			    (if (http-request? req)
				(let ((val (scheme->response
					    (list (list name value)) req)))
				   (http-response-js-padding-set! val padding)
				   (ajax-signal-value req val)
				   (set! req #f))
				(ajax-connection-event-push! conn name value))))))
		(ajax-find-connections-by-name name)))
   
   (define (flash-event-broadcast! name value)
      (let ((val (json-make-signal-value value)))
	 (for-each-socket
	    *flash-socket-table*
	    name
	    (lambda (l)
	       (when (pair? l)
		  (for-each (lambda (req)
			       (flash-signal-value req name val))
		     l))))))

   (define (websocket-event-broadcast! name value)
      (let ((val (websocket-value name value)))
	 (for-each-socket
	    *websocket-socket-table*
	    name
	    (lambda (l)
	       (when debug-websocket
		  (tprint "websocket-event-broadcast name=" name " l=" l))
	       (when (pair? l)
		  (for-each (lambda (resp)
			       (websocket-signal-value resp val))
		     l))))))
       
   (define (multipart-event-broadcast! name value)
      (let ((val (multipart-value name value)))
	 (for-each-socket
	    *multipart-socket-table*
	    name
	    (lambda (l)
	       (when (pair? l)
		  (for-each (lambda (req)
			       (multipart-signal-value req val))
		     l))))))

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
      (connection 'close)
      (proc (lambda (p)
	       (display (hop-event-policy (http-request-port req)) p)
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
	 (tprint ">>> REGISTER EVENT")
	 (with-hop (*register-service* :event obj :key key :mode "websocket")
	    :authorization auth
	    :host host :port port
	    (lambda (_)
	       (tprint "<<< REGISTER EVENT")
	       (with-lock mutex
		  (lambda ()
		     (set! listeners
			(cons (cons obj proc) listeners))))))))

   (define (get-hopsocket! host port auth key)
      (let* ((sock (make-client-socket host port))
	     (path (format "~a/server-event/websocket?key=~a"
		      (hop-service-base)
		      key)))
	 (letrec* ((th (cond-expand
			  (enable-threads
			     (instantiate::pthread
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

   (define (parse-websocket-enveloppe buf len)
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
		       (js (substring buf
			      (+fx (string-length name) 20)
			      (-fx  len 7))))
		   (values name (call-with-input-string js json->hop)))))
	 (else
	  (error "hopsocket" "Illegal websocket enveloppe" buf))))

   (define (read-websocket-event in)
      (let ((buf (make-string 128))
	    (len 128))
	 (let loop ((i 0))
	    (let ((c (read-byte in)))
	       (cond
		  ((=fx c 255)
		   (parse-websocket-enveloppe buf i))
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
	 (mutex-lock! mutex)
	 (let ((l (filter (lambda (c) (string=? (car c) name)) listeners)))
	    (mutex-unlock! mutex)
	    (when (pair? l)
	       (let ((e (instantiate::server-event
			   (name name)
			   (value value)
			   (target event))))
		  (let loop ((l l))
		     (when (pair? l)
			((cdar l) e)
			(unless (event-stopped e)
			   (loop (cdr l))))))))))

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
      (mutex-lock! *listener-mutex*)
      (let ((hs (hashtable-get *server-listeners* event)))
	 (tprint "ADD-SERVER-LISTENER obj=" obj " event=" event " -> HS=" hs)
	 (mutex-unlock! *listener-mutex*)
	 (if (hopsocket? hs)
	     (register-event! host port auth hs)
	     (with-hop (*port-service*)
		:host host :port port
		:authorization auth
		(lambda (v)
		   (let ((key (vector-ref v 2)))
		      (with-lock *listener-mutex*
			 (lambda ()
			    (let ((hs (hashtable-get *server-listeners* event)))
			       (if (hopsocket? hs)
				   (register-event! host port auth hs)
				   (let ((hs (get-hopsocket! host port auth key)))
				      (register-event! host port auth hs)))))))))))))

;*---------------------------------------------------------------------*/
;*    remove-server-listener! ...                                      */
;*---------------------------------------------------------------------*/
(define (remove-server-listener! obj event proc capture)
   (mutex-lock! *listener-mutex*)
      (let ((hs (hashtable-get *server-listeners* event)))
	 (mutex-unlock! *listener-mutex*)
	 (when (hopsocket? hs)
	    (with-access::hopsocket hs (mutex socket listeners)
	       (with-lock mutex
		  (lambda ()
		     (set! listeners (filter! (lambda (l)
						 (not (eq? (car l) obj)))
					listeners))
		     (when (null? listeners)
			(socket-close socket)
			(mutex-lock! *listener-mutex*)
			(hashtable-remove! *server-listeners* event)
			(mutex-unlock! *listener-mutex*))))))))
