;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/runtime/event.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 27 05:45:08 2005                          */
;*    Last change :  Fri Jun 14 14:57:28 2019 (serrano)                */
;*    Copyright   :  2005-20 Manuel Serrano                            */
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
	    __hop_websocket
	    __hop_watch)

   (static  (class http-response-event::%http-response-server
	       (name::bstring read-only)))

   (export  (class event
	       (name::bstring read-only)
	       (target::obj read-only)
	       (stopped::bool (default #f))
	       (value::obj (default #unspecified)))

	    (class websocket-event::event
	       (data::obj (default #unspecified)))

	    (class server-event::websocket-event)

	    (class server
	       (mutex::mutex read-only (default (make-mutex)))
	       (host::bstring read-only)
	       (port::int read-only)
	       (ssl::bool (default #f))
	       (authorization read-only (default #f))
	       (version::bstring read-only (default (hop-version)))
	       (listeners::pair-nil (default '()))
	       (ready-listeners::pair-nil (default '()))
	       (down-listeners::pair-nil (default '()))
	       (trigger::procedure read-only (default (lambda (f) (f))))
	       (ctx (default #f))
	       (%websocket (default #f))
	       (%key (default #f)))
	    
	    (generic server-init! ::server)
	    (generic server-reset! ::server)
	    
	    (generic add-event-listener! ::obj ::obj ::procedure . l)
	    (generic remove-event-listener! ::obj ::obj ::procedure . l)
	    (generic stop-event-propagation ::event ::bool)

	    (apply-listeners ::obj ::event)
	    
	    (hop-event-init!)
	    (hop-event-tables)
	    (hop-event-signal! ::bstring ::obj #!optional ctx)
	    (hop-event-broadcast! ::bstring ::obj #!optional ctx)
	    (hop-event-client-ready? ::bstring)
	    (hop-event-policy-file ::http-request)
	    (hop-event-info-service::procedure)
	    (hop-event-register-service::procedure)
	    (hop-event-unregister-service::procedure)))

;*---------------------------------------------------------------------*/
;*    debug ...                                                        */
;*---------------------------------------------------------------------*/
(define debug-multipart #f)

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::obj ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (add-event-listener! obj::obj event proc . capture)
   (cond
      ((and (string? event) (string? obj))
       (add-server-listener! obj event proc capture))
      ((eq? obj 'hop)
       (add-hop-listener! event proc capture))
      (else
       (error "add-event-listener!"
	  (format "Illegal ~s listener" event) obj))))

(define-generic (remove-event-listener! obj::obj event proc . capture)
   (cond
      ((and (string? event) (string? obj))
       (remove-server-listener! obj event proc capture))
      ((eq? obj 'hop)
       (remove-hop-listener! event proc capture))
      (else
       (error "remove-event-listener!"
	  (format "Illegal ~s listener" event) obj))))

(define-generic (stop-event-propagation event::event default::bool)
   (with-access::event event (stopped)
      (set! stopped #t)))

;*---------------------------------------------------------------------*/
;*    envelope-re ...                                                  */
;*---------------------------------------------------------------------*/
(define envelope-re
   (pregexp "^<([rsxifj]) name='([^']+)'>(.*)</[rsxifj]>$" 'MULTILINE))
(define cdata-re
   (pregexp "^<!\\[CDATA\\[(.*)\\]\\]>$" 'MULTILINE))

;*---------------------------------------------------------------------*/
;*    server-init! ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (server-init! srv::server)
   
   (define (message->event k id text)
      (case (string-ref k 0)
	 ((#\i)
	  (let ((val (string->integer text)))
	     (instantiate::server-event
		(target srv)
		(name id)
		(value val)
		(data val))))
	 ((#\f)
	  (let ((val (string->real text)))
	     (instantiate::server-event
		(target srv)
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
		(target srv)
		(name id)
		(value val)
		(data val))))
	 ((#\s)
	  (let ((val (url-decode text)))
	     (instantiate::server-event
		(target srv)
		(name id)
		(value val)
		(data val))))
	 ((#\j)
	  (with-access::server srv (ctx)
	     (let ((val (string->obj
			   (url-decode (cadr (pregexp-match cdata-re text)))
			   #f ctx)))
		(instantiate::server-event
		   (target srv)
		   (name id)
		   (value val)
		   (data val)))))
	 ((#\r)
	  (let ((val (url-decode text)))
	     (instantiate::server-event
		(target srv)
		(name "ready")
		(value val)
		(data val))))))

   (define (parse-message v)
      (with-access::websocket-event v (data)
	 (let ((m (pregexp-match envelope-re data)))
	    (when (pair? m)
	       (let* ((k (cadr m))
		      (id (caddr m))
		      (text (cadddr m)))
		  (if (char=? (string-ref k 0) #\r)
		      (with-access::server srv (ready-listeners trigger)
			 (trigger
			    (lambda ()
			       (apply-listeners ready-listeners
				  (message->event k id text)))))
		      (with-access::server srv (listeners trigger)
			 (let ((ltns (filter-map (lambda (l)
						    (when (string=? (car l) id)
						       (cdr l)))
					listeners)))
			    (when (pair? ltns)
			       (trigger
				  (lambda ()
				     (let ((event (message->event k id text)))
					(apply-listeners ltns event)))))))))))))

   (define (down e)
      (with-trace 'event "down@server-init!"
	 (trace-item "e=" e)
	 (with-access::server srv (down-listeners trigger)
	    (trigger
	       (lambda ()
		  (apply-listeners down-listeners e))))))

   (with-access::server srv (host port ssl authorization %websocket %key mutex)
      (synchronize mutex
	 (unless %websocket
	    (with-hop ((hop-event-info-service))
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
			   (oncloses (list down))
			   (onmessages (list parse-message))))
		     (websocket-connect! %websocket))))))))

;*---------------------------------------------------------------------*/
;*    server-reset! ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (server-reset! obj::server)
   (with-access::server obj (mutex)
      (synchronize mutex
	 (server-reset-sans-lock! obj))))

;*---------------------------------------------------------------------*/
;*    server-reset-sans-lock! ...                                      */
;*---------------------------------------------------------------------*/
(define (server-reset-sans-lock! obj::server)
   (with-access::server obj (%websocket %key host port)
      (websocket-close %websocket)
      (set! %websocket #f)
      (set! %key #f)))

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::server ...                                 */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! obj::server event proc . capture)
   (let loop ((armed #f))
      (server-init! obj)
      (with-access::server obj (mutex host port ssl authorization %key)
	 (cond
	    ((string=? event "ready")
	     (synchronize mutex
		(with-access::server obj (ready-listeners)
		   (set! ready-listeners (cons proc ready-listeners)))))
	    ((string=? event "down")
	     (synchronize mutex
		(with-access::server obj (down-listeners)
		   (set! down-listeners (cons proc down-listeners)))))
	    (else
	     (with-hop ((hop-event-register-service) :event event
			  :key %key :mode "websocket")
		:host host :port port
		:authorization authorization
		:ssl ssl
		(lambda (v)
		   (synchronize mutex
		      (with-access::server obj (listeners)
			 (set! listeners (cons (cons event proc) listeners)))))
		(lambda (exn)
		   (if (isa? exn xml-http-request)
		       (with-access::xml-http-request exn (status input-port header)
			  (if (and (not armed) (=fx status 500))
			      (let ((badkey (read-string input-port)))
				 (when (string=? (md5sum %key) badkey)
				    (server-reset! obj)
				    (loop #t)))
			      (tprint "error: " (read-string input-port))))
		       (exception-notify exn)))))))))

;*---------------------------------------------------------------------*/
;*    remove-event-listener! ::server ...                              */
;*---------------------------------------------------------------------*/
(define-method (remove-event-listener! obj::server event proc . capture)
   (with-access::server obj (listeners mutex host port ssl authorization %key)
      (let ((key %key))
	 (when (synchronize mutex
		  (let loop ((ls listeners))
		     (when (pair? ls)
			(let ((l (car ls)))
			   (if (and (eq? (cdr l) proc) (string=? (car l) event))
			       (begin
				  (set! listeners (remq! l listeners))
				  (when (null? listeners)
				     (server-reset-sans-lock! obj))
				  #t)
			       (loop (cdr ls)))))))
	    (when key
	       (with-hop ((hop-event-unregister-service) :event event :key key)
		  :host host :port port
		  :authorization authorization
		  :ssl ssl))))))

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
(define *info-service* #f)
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
	    (let ((resp (websocket-server-response req key :protocol '("json"))))
	       (watch-socket! socket
		  (lambda (s)
		     (with-trace 'event
			   "watch@websocket-register-new-connection!"
			(trace-item "CLOSING s=" s)
			(socket-close s)
			(notify-client! "disconnect" #f req
			   *disconnect-listeners*))))
	       (trace-item "resp=" (typeof resp))
	       ;; register the websocket
	       (synchronize *event-mutex*
		  (set! *websocket-response-list*
		     (cons (cons (string->symbol key) (cons resp req))
			*websocket-response-list*))
		  (trace-item "key=" key)
		  (trace-item "socket=" socket )
		  (trace-item "connected clients=" *websocket-response-list*))
	       resp)))))

;*---------------------------------------------------------------------*/
;*    hop-event-init! ...                                              */
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
		     (with-access::http-request req (header socket)
			(trace-item "req.socket=" socket)
			(if (websocket-proxy-request? header)
			    (websocket-proxy-response req)
			    (websocket-register-new-connection! req key)))))))
	 
	 (set! *info-service*
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
			(set! *clients-number* (-fx *clients-number* 1)))))))
	 
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
;*    hop-event-tables ...                                             */
;*    -------------------------------------------------------------    */
;*    Dump the content of the event table for debug purposes.          */
;*---------------------------------------------------------------------*/
(define (hop-event-tables)
   (synchronize *event-mutex*
      `((*clients-number* ,*clients-number*)
	(*multipart-request-list* ,*multipart-request-list*)
	(*multipart-socket-table* ,*multipart-socket-table*)
	(*websocket-response-list* ,*websocket-response-list*)
	(*websocket-socket-table* ,*websocket-socket-table*))))

;*---------------------------------------------------------------------*/
;*    server-event-register ...                                        */
;*---------------------------------------------------------------------*/
(define (server-event-register event key mode padding req)
   
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
      (with-trace 'event "websocket-register-event!"
	 (let ((c (assq key *websocket-response-list*)))
	    (trace-item "key=" key)
	    (trace-item "name=" name)
	    ;;(trace-item "c=" c)
	    (if (pair? c)
		(let ((resp (cadr c)))
		   (websocket-signal resp
		      (format "<r name='ready'>~a</r>"
			 (url-path-encode name)))
		   (hashtable-update! *websocket-socket-table*
		      name
		      (lambda (l) (cons resp l))
		      (list resp))
		   (instantiate::http-response-string))
		(begin
		   (trace-item "keylist=" *websocket-response-list*)
		   (instantiate::http-response-string
		      (start-line "HTTP/1.0 500 Illegal key")
		      (body (md5sum (symbol->string! key)))))))))

   (with-trace 'event "ws-register-event!"
      (synchronize *event-mutex*
	 (trace-item "event=" (url-decode event))
	 (trace-item "key=" key)
	 (trace-item "mode=" mode)
	 (trace-item "padding=" padding)
	 (if (<fx *clients-number* (hop-event-max-clients))
	     (if (string? key)
		 (let ((key (string->symbol key))
		       (event (url-decode! event)))
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
				(else
				 (error "register-event" "unsupported mode" mode)))))
		       (notify-client! "connect" event req
			  *connect-listeners*)
		       r))
		 (http-service-unavailable event))
	     (http-service-unavailable event)))))

;*---------------------------------------------------------------------*/
;*    server-event-unregister ...                                      */
;*---------------------------------------------------------------------*/
(define (server-event-unregister event key)
   
   
   
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
		  (envelope-value *ping* #unspecified #f))))))
   
   (define (remq-one! x y)
      (let laap ((x x)
		 (y y))
	 (cond
	    ((null? y) y)
	    ((eq? x (car y)) (cdr y))
	    (else (let loop ((prev y))
		     (cond ((null? (cdr prev))
			    y)
			   ((eq? (cadr prev) x)
			    (set-cdr! prev (cddr prev))
			    y)
			   (else (loop (cdr prev)))))))))
   
   (define (unregister-websocket-event! event key)
      (let ((c (assq (string->symbol key) *websocket-response-list*)))
	 (when (pair? c)
	    (let ((resp (cadr c))
		  (req (cddr c)))
	       (hashtable-update! *websocket-socket-table*
		  event
		  (lambda (l)
		     (notify-client! "disconnect" event req
			*disconnect-listeners*) 
		     (remq-one! resp l))
		  '())
	       ;; Ping the client to check if it still exists. If the client
	       ;; no longer exists, an error will be raised and the client
	       ;; will be removed from the tables.
	       (websocket-signal resp
		  (envelope-value *ping* #unspecified #f))))))
   
   (synchronize *event-mutex*
      (let ((event (url-decode! event)))
	 (when (and (string? key) event)
	    (unregister-websocket-event! event key)
	    (unregister-multipart-event! event key)
	    #f))))

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
		     (not (eq? (cadr e) resp)))
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
   
   (or (multipart-event-client-ready? name)
       (websocket-event-client-ready? name)))

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
      (with-trace 'event "hybi-signal-value"
	 (let ((p (socket-output socket)))
	    (trace-item "p=" p " closed=" (closed-output-port? p))
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
	       (flush-output-port p)))))
   
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
		  (trace-item "accept=" accept)
		  (if accept
		      (hybi-signal-value vstr socket)
		      (hixie-signal-value vstr socket))))))))

;*---------------------------------------------------------------------*/
;*    envelope-value ...                                               */
;*---------------------------------------------------------------------*/
(define (envelope-value::bstring name value ctx)
   (let ((n (url-path-encode name)))
      (cond
	 ((isa? value xml)
	  (format "<x name='~a'>~a</x>" n (xml->string value (hop-xml-backend))))
	 ((string? value)
	  (format "<s name='~a'>~a</s>" n (url-path-encode value)))
	 ((integer? value)
	  (format "<i name='~a'>~a</i>" n value))
	 ((real? value)
	  (format "<f name='~a'>~a</f>" n value))
	 (else
	  (let ((op (open-output-string)))
	     (fprintf op "<j name='~a'><![CDATA[" n)
	     ;; ICI utiliser ctx
	     (display (url-path-encode (obj->string value (or ctx 'hop-client))) op)
	     (display "]]></j>" op)
	     (close-output-port op))))))

;*---------------------------------------------------------------------*/
;*    envelope-json-value ...                                          */
;*---------------------------------------------------------------------*/
(define (envelope-json-value name value ctx)
   (let ((n (url-path-encode name)))
      (let ((op (open-output-string)))
	 (fprintf op "<o name='~a'><![CDATA[" n)
	 (let ((json (call-with-output-string
			(lambda (op)
			   (obj->json value op ctx)))))
	    (display (url-path-encode json) op)
	    (display "]]></o>" op)
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
(define (hop-event-signal! name value #!optional ctx)
   
   (define (multipart-event-signal! name value)
      (for-each-socket
       *multipart-socket-table*
       name
       (lambda (l)
	  (when (pair? l)
	     (let ((val (envelope-value name value ctx)))
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
	       (with-access::http-response-websocket (car l) (protocol)
		  (let ((val (if (and (string? protocol)
				      (string=? protocol "json"))
				 (envelope-json-value name value ctx)
				 (envelope-value name value ctx))))
		     (websocket-signal (car l) val)
		     #t))))))

   (set! hop-signal-id (-fx hop-signal-id 1))
   (hop-verb 2 (hop-color hop-signal-id hop-signal-id " SIGNAL")
	     ": " name)
   (hop-verb 3 " value=" (with-output-to-string (lambda () (write value))))
   (hop-verb 2 "\n")
   (synchronize *event-mutex*
      (case (random 2)
	 ((0)
	  (unless (multipart-event-signal! name value)
	     (websocket-event-signal! name value)))
	 ((3)
	  (unless (websocket-event-signal! name value)
	     (multipart-event-signal! name value)))))
	 
   #unspecified)

;*---------------------------------------------------------------------*/
;*    hop-event-broadcast! ...                                         */
;*---------------------------------------------------------------------*/
(define (hop-event-broadcast! name value #!optional ctx)
   
   (define jsvalue #f)
   (define jsonvalue #f)
   
   (define (hopjs-value name value)
      (unless jsvalue (set! jsvalue (envelope-value name value ctx)))
      jsvalue)
      
   (define (json-value name value)
      (unless jsonvalue (set! jsonvalue (envelope-json-value name value ctx)))
      jsonvalue)
      
   (define (websocket-event-broadcast! name value)
      (with-trace 'event "ws-event-broadcast!"
	 (trace-item "name=" name)
	 (trace-item "value="
	    (if (or (string? value) (symbol? value) (number? value))
		value
		(typeof value)))
	 (trace-item "ws-table=" (hashtable-key-list *websocket-socket-table*))
	 (for-each-socket *websocket-socket-table*
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
				  (with-access::http-response-websocket resp (protocol)
				     (websocket-signal resp
					(if (and (string? protocol)
						 (string=? protocol "json"))
					    (json-value name value)
					    (hopjs-value name value)))))
			l)))))))
       
   (define (multipart-event-broadcast! name value)
      (let ((val (envelope-value name value ctx)))
	 (for-each-socket
	    *multipart-socket-table*
	    name
	    (lambda (l)
	       (when (pair? l)
		  (for-each (lambda (req)
			       (multipart-signal req val))
		     l))))))

   (synchronize *event-mutex*
      (websocket-event-broadcast! name value)
      (multipart-event-broadcast! name value))
   
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
(define (add-server-listener! event::bstring srv::bstring proc::procedure capture)

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

   (let ((olds (synchronize *listener-mutex*
		  (hashtable-get *server-listeners* srv))))
      (if (isa? olds server)
	  (add-event-listener! olds event proc capture)
	  (multiple-value-bind (host port auth)
	     (parse-authenticated-host srv)
	     (let ((news (instantiate::server
			    (host host)
			    (port port)
			    (ssl #f)
			    (authorization auth))))
		(synchronize *listener-mutex*
		   (hashtable-put! *server-listeners* srv news))
		(add-event-listener! news event proc capture))))))

;*---------------------------------------------------------------------*/
;*    remove-server-listener! ...                                      */
;*---------------------------------------------------------------------*/
(define (remove-server-listener! event srv proc capture)
   (let ((olds (synchronize *listener-mutex*
		  (hashtable-get *server-listeners* srv))))
      (when (isa? olds server)
	 (apply remove-event-listener! olds event proc capture))))

;*---------------------------------------------------------------------*/
;*    *connect-listeners* ...                                          */
;*---------------------------------------------------------------------*/
(define *connect-listeners* '())
(define *disconnect-listeners* '())

;*---------------------------------------------------------------------*/
;*    add-hop-listener! ...                                            */
;*---------------------------------------------------------------------*/
(define (add-hop-listener! event::bstring proc::procedure capture)
   (cond
      ((string=? event "connect")
       (synchronize *listener-mutex*
	  (set! *connect-listeners*
	     (cons proc *connect-listeners*))))
      ((string=? event "disconnect")
       (synchronize *listener-mutex*
	  (set! *disconnect-listeners*
	     (cons proc *disconnect-listeners*))))))

;*---------------------------------------------------------------------*/
;*    remove-hop-listener! ...                                         */
;*---------------------------------------------------------------------*/
(define (remove-hop-listener! event::bstring proc::procedure capture)
   (cond
      ((string=? event "connect")
       (synchronize *listener-mutex*
	  (set! *connect-listeners*
	     (delete! proc *connect-listeners*))))
      ((string=? event "disconnect")
       (synchronize *listener-mutex*
	  (set! *disconnect-listeners*
	     (delete! proc *disconnect-listeners*))))))

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
   *info-service*)

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

;*---------------------------------------------------------------------*/
;*    notify-client! ...                                               */
;*---------------------------------------------------------------------*/
(define (notify-client! event id req ltns)
   (when (pair? ltns)
      (let ((e (instantiate::server-event
		  (target req)
		  (name event)
		  (value id)
		  (data id))))
	 (apply-listeners ltns e))))
