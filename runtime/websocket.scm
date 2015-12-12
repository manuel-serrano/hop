;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/websocket.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 15 07:21:08 2012                          */
;*    Last change :  Sat Dec 12 13:32:46 2015 (serrano)                */
;*    Copyright   :  2012-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop WebSocket server-side tools                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_websocket
   
   (include "http_lib.sch"
	    "service.sch"
            "verbose.sch"
	    "thread.sch")

   (library web)

   (import  __hop_configure
	    __hop_thread
	    __hop_param
	    __hop_types
	    __hop_user
	    __hop_xml-types
	    __hop_xml
	    __hop_html-head
	    __hop_misc
	    __hop_hop
	    __hop_http-lib
	    __hop_http-response
	    __hop_json
	    __hop_read
	    __hop_service
	    __hop_http-response
	    __hop_http-error
	    __hop_event)
   
   (static (class websocket-frame
              (final?::bool read-only (default #t))
              (kind::symbol read-only (default 'text))
              (payload read-only)))

   (export (class websocket
	      (%mutex read-only (default (make-mutex "websocket")))
	      (%condvar read-only (default (make-condition-variable)))
	      (%socket (default #f))
	      (url::bstring read-only)
	      (onopens::pair-nil (default '()))
	      (onerrors::pair-nil (default '()))
	      (oncloses::pair-nil (default '()))
	      (onmessages::pair-nil (default '()))
	      (authorization (default #f))
	      (state::symbol (default 'connecting))
	      (version read-only (default 'hybi))
	      (protocol::obj read-only (default #f)))
	   
	   (class ws-server
	      (ws-server-init!)
	      (%mutex read-only (default (make-mutex)))
	      (%websocket (default #f))
	      (%key (default #f))
	      (host::bstring read-only (default "localhost"))
	      (port::int read-only (default 80))
	      (authorization (default #f))
	      (onmessages::pair-nil (default '())))
	   
	   (websocket-proxy-request? ::pair-nil)
	   (websocket-proxy-connect! ::bstring ::int ::http-request)
	   (websocket-proxy-response::http-response-proxy-websocket ::http-request)
	   (websocket-server-response ::http-request key #!optional onconnect protocol)

	   (websocket-connect! ::websocket)
	   (websocket-close ::websocket)
	   (websocket-send-text ::socket ::bstring #!key (mask #t) (final #t))
	   (websocket-send ::socket ::obj #!key (mask #t) (final #t))

	   (websocket-read ::socket)))

;*---------------------------------------------------------------------*/
;*    websocket-debug ...                                              */
;*---------------------------------------------------------------------*/
(define (websocket-debug)
   ;; don't forget to compile this module with -g to
   ;; enable websockets debugging
   #t)

(define (websocket-debug-level)
   (if (websocket-debug) 1 1000000))

;*---------------------------------------------------------------------*/
;*    websocket-proxy-tunnel-count ...                                 */
;*---------------------------------------------------------------------*/
(define websocket-proxy-tunnel-count 0)

;*---------------------------------------------------------------------*/
;*    *connect-host-table* ...                                         */
;*---------------------------------------------------------------------*/
(define *connect-host-table* (create-hashtable))

;*---------------------------------------------------------------------*/
;*    connect-mutex ...                                                */
;*---------------------------------------------------------------------*/
(define connect-mutex (make-mutex 'connect))

;*---------------------------------------------------------------------*/
;*    get-header ...                                                   */
;*---------------------------------------------------------------------*/
(define (get-header header key default)
   (let ((c (assq key header)))
      (if (pair? c)
	  (cdr c)
	  default)))

;*---------------------------------------------------------------------*/
;*    websocket-proxy-connect! ...                                     */
;*---------------------------------------------------------------------*/
(define (websocket-proxy-connect! host port req)
   (if (<fx websocket-proxy-tunnel-count (hop-max-websocket-proxy-tunnel))
       (begin
	  (with-access::http-request req ((req-host host) (req-port port) socket)
	     (tprint "*** WEBSOCKET-PROX-CONNECT req=" req
		" req-host=" req-host " req-port=" req-port
		" req-ip=" (socket-host-address socket) " "
		(ipv4->elong (socket-host-address socket)))
	     (tprint "*** dest-host=" host " dest-port=" port))
	  (synchronize connect-mutex
	     (hashtable-put! *connect-host-table*
		(format "~a:~a" host port) #t))
	  (websocket-connect-tunnel host port req)
	  (instantiate::http-response-persistent
	     (request req)
	     (body "200 Connection established\r\n")))
       (instantiate::http-response-abort)))
   
;*---------------------------------------------------------------------*/
;*    websocket-proxy-request? ...                                     */
;*---------------------------------------------------------------------*/
(define (websocket-proxy-request? header)
   (when (string=? (get-header header upgrade: "") "websocket")
      (let ((host (get-header header host: #f))
	    (port (get-header header port: 80)))
	 (synchronize connect-mutex
	    (hashtable-get *connect-host-table* (format "~a:~a" host port))))))

;*---------------------------------------------------------------------*/
;*    websocket-proxy-response ...                                     */
;*---------------------------------------------------------------------*/
(define (websocket-proxy-response req)
   (with-access::http-request req (content-length method path host port
				     user header userinfo scheme http)
      (instantiate::http-response-proxy-websocket
	 (scheme scheme)
	 (method method)
	 (host host)
	 (port port)
	 (path path)
	 (userinfo userinfo)
	 (http http)
	 (header header)
	 (content-length content-length)
	 (request req)
	 (remote-timeout (hop-read-timeout))
	 (connection-timeout (hop-connection-timeout)))))

;*---------------------------------------------------------------------*/
;*    websocket-hybi-accept ...                                        */
;*---------------------------------------------------------------------*/
(define (websocket-hybi-accept key)
   (let* ((i (string-index key #\space))
	  (pkey (if i (car (string-split key #\space)) key))
	  (guid "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
	  (akey (string-append pkey guid))
	  (skey (sha1sum-string akey)))
      (base64-encode (string-hex-intern! skey))))

;*---------------------------------------------------------------------*/
;*    websocket-server-response ...                                    */
;*---------------------------------------------------------------------*/
(define (websocket-server-response req key #!optional onconnect protocol)
   
   (define (websocket-server-location host)
      (with-access::http-request req ((h host) (p port))
	 (format "ws://~a/~a/public/server-event/websocket?key=~a"
	    (or host (format "~a:~a" h p))
	    (hop-initial-weblet)
	    key)))
   
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
      (with-trace 'websocket "websocket-protocol76"
	 (trace-item "key1=" key1)
	 (trace-item "key2=" key2)
	 ;; Handshake known as draft-hixie-thewebsocketprotocol-76
	 ;; see http://www.whatwg.org/specs/web-socket-protocol/
	 ;; http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-03
	 ;; for each key, compute the following:
	 ;;   extract numerics (0-9) from value, and convert to int base 10.
	 ;;   Divide by number of space characters in value!
	 (with-access::http-request req (socket) 
	    (let ((k1 (protocol76-key-value key1))
		  (k2 (protocol76-key-value key2))
		  (key3 (read-chars 8 (socket-input socket)))
		  (buf (make-string 16 #\0)))
	       (blit-string-int32-big-endian! k1 buf 0)
	       (blit-string-int32-big-endian! k2 buf 4)
	       (blit-string! key3 0 buf 8 8)
	       (string-hex-intern! (md5sum buf))))))

   (define (websocket-subprotocol srvproto clientproto)
      ;; Subprotocols described at
      ;; http://tools.ietf.org/html/rfc6455#section-11.3.4
      (cond
	 ((not srvproto)
	  ;; the client will notice the error when it receives the response
	  #f)
	 ((not clientproto)
	  ;; MS: 2 aug 2014, I'm not sure what to do in this case
	  ;; see http://tools.ietf.org/html/rfc6455#section-4.2.2
	  #f)
	 (else
	  (find (lambda (s) (member s srvproto))
	     (string-split clientproto ", ")))))
      
   (define (websocket-sec-challenge req header)
      ;; newer websocket protocols includes a 3 keys challenge
      ;; we check which version we are asked.
      (let ((key1 (get-header header sec-websocket-key1: #f)))
	 (when key1
	    (let ((key2 (get-header header sec-websocket-key2: #f)))
	       (when key2
		  (webwocket-hixie-protocol-76 key1 key2 req))))))
   
   (define (websocket-hixie-protocol header subprotocol read)
      (instantiate::http-response-websocket
	 (request req)
	 (start-line "HTTP/1.1 101 Web Socket Protocol Handshake")
	 (location (websocket-server-location (get-header header host: #f)))
	 (origin (get-header header origin: "localhost"))
	 (protocol subprotocol)
	 (connection 'Upgrade)
	 (sec (websocket-sec-challenge req header))
	 (onconnect onconnect)))
   
   (define (websocket-hybi-protocol header subprotocol req)
      ;; http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-08
      (with-trace 'websocket "websocket-hybi-protocol"
	 (trace-item "header=" header " subprotocol=" subprotocol)
	 (let ((key (get-header header sec-websocket-key: #f)))
	    (instantiate::http-response-websocket
	       (request req)
	       (start-line "HTTP/1.1 101 Switching Protocols")
	       (connection 'Upgrade)
	       (protocol subprotocol)
	       (accept (websocket-hybi-accept key))
	       (onconnect onconnect)))))
   
   (with-access::http-request req (header connection socket)
      (let* ((host (get-header header host: #f))
	     (version (get-header header sec-websocket-version: "-1"))
	     (clientprotocol (get-header header sec-websocket-protocol: #f))
	     (subprotocol (websocket-subprotocol protocol clientprotocol)))
	 ;; see http_response.scm for the source code that actually sends
	 ;; the bytes of the response to the client.
	 (with-trace 'websocket "ws-register"
	    (trace-item "version="
	       version)
	    (trace-item "origin="
	       (get-header header origin: "localhost"))
	    (trace-item "Sec-WebSocket-Protocol="
	       (get-header header sec-websocket-protocol: #f))
	    (let ((v (string->integer version)))
	       (cond
		  ((and (>=fx v 7) (<=fx v 25))
		   (websocket-hybi-protocol header subprotocol req))
		  (else
		   (websocket-hixie-protocol header subprotocol req))))))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-websocket ...                      */
;*    -------------------------------------------------------------    */
;*    websocket are described at:                                      */
;*    http://tools.ietf.org/html/draft-hixie-thewebsocketprotocol      */
;*                                                                     */
;*    They impose a strict ordering and case for the reply. Thus Hop   */
;*    uses a dedicated class instead of a generic response-string.     */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-websocket request socket)
   (with-trace 'websocket "http-response::http-response-websocket"
      (with-access::http-response-websocket r (start-line
					       connection
					       origin
					       location
					       timeout
					       protocol
					       sec
					       accept
					       onconnect)
	 (trace-item "sec=" sec " protocol=" protocol " accept=" accept)
	 (let ((p (socket-output socket)))
	    (when (>=fx timeout 0) (output-timeout-set! p timeout))
	    (http-write-line-string p start-line)
	    (http-write-line-string p "Upgrade: WebSocket")
	    (http-write-line p "Connection: " connection)
	    (cond
	       (accept
		  (http-write-line p "Sec-WebSocket-Accept: " accept)
		  (when protocol
		     (http-write-line p "Sec-WebSocket-Protocol: " protocol))
		  (http-write-line p))
	       (sec
		  (http-write-line p "Sec-WebSocket-Origin: " origin)
		  (http-write-line p "Sec-WebSocket-Location: " location)
		  (when protocol
		     (http-write-line p "Sec-WebSocket-Protocol: " protocol))
		  (http-write-line p)
		  (display sec p))
	       (else
		(http-write-line p "WebSocket-Origin: " origin)
		(http-write-line p "WebSocket-Location: " location)
		(when protocol
		   (http-write-line p "Sec-WebSocket-Protocol: " protocol))
		(http-write-line p)))
	    ;; run the onconnection listener before flushing the output
	    (when (procedure? onconnect) (onconnect r))
	    ;; the server is up and running, notify the client
	    (flush-output-port p)
	    ;; set the socket in non blocking mode to prevent
	    ;; down connections to block all event broadcasts
	    (output-timeout-set! p 5000)
	    ;; detach the input and output buffer as they belong
	    ;; to the thread
	    (socket-buffers-detach! socket)
	    'persistent))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-proxy-websocket ...                */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-proxy-websocket request socket)
   (with-trace 'websocket "http-response::http-response-proxy-websocket"
      (cond-expand
	 (enable-threads
	    (thread-start!
	       (instantiate::hopthread
		  (body
		     (lambda () (websocket-tunnel r socket)))))
	    'persistent)
	 (else
	  (error "http-response"
	     "websocket tunnel requires thread support"
	     #f)))))

;*---------------------------------------------------------------------*/
;*    websocket-connect-tunnel ...                                     */
;*    -------------------------------------------------------------    */
;*    Used after a connect request                                     */
;*---------------------------------------------------------------------*/
(define (websocket-connect-tunnel host port req)
   ;; increment the number of active proxy tunnel 
   (set! websocket-proxy-tunnel-count (+fx websocket-proxy-tunnel-count 1))
   (with-access::http-request req (socket timeout)
      (let* ((rsocket (make-client-socket/timeout host port timeout req #f))
	     (ip (socket-input socket))
	     (op (socket-output socket))
	     (rop (socket-output rsocket))
	     (rip (socket-input rsocket)))
	 (socket-timeout-set! socket 0 0)
	 (socket-timeout-set! rsocket 0 0)
	 (letrec ((th1 (instantiate::hopthread
			  (name (format "websocket tunnel (client) %s:%d"
				   host port))
			  (body (lambda ()
				   (let loop ()
				      (let ((c (read-char ip)))
					 (if (eof-object? c)
					     (begin
						(socket-shutdown socket)
						(socket-close rsocket)
						(set! websocket-proxy-tunnel-count
						   (-fx websocket-proxy-tunnel-count 1))
						(tprint "WEBSOCKET-CONNECT-TUNNEL ABORT.1..")
						(thread-terminate! th2))
					     (begin
						(display c rop)
						(flush-output-port rop)
						(loop)))))))))
		  (th2 (instantiate::hopthread
			  (name (format "websocket tunnel (remote) %s:%d"
				   host port))
			  (body (lambda ()
				   (let loop ()
				      (let ((c (read-char rip)))
					 (if (eof-object? c)
					     (begin
						(socket-close rsocket)
						(socket-shutdown socket)
						(tprint "WEBSOCKET-CONNECT-TUNNEL ABORT.2..")
						(thread-terminate! th1))
					     (begin
						(display c rop)
						(flush-output-port op)
						(loop))))))))))
	    (thread-start! th1)
	    (thread-start! th2)))))

;*---------------------------------------------------------------------*/
;*    websocket-tunnel ...                                             */
;*---------------------------------------------------------------------*/
(define (websocket-tunnel resp socket)
   (with-trace 'websocket "ws-tunnel"
      (with-access::http-response-proxy-websocket resp (request remote-timeout)
	 (with-access::http-request request (header connection-timeout path timeout)
	    (let* ((host (get-header header host: "localhost"))
		   (i (string-index host #\:))
		   (name (if i (substring host 0 i) host))
		   (port (if i (string->integer (substring host (+fx i 1))) 80))
		   (rsocket (make-client-socket/timeout name port timeout request #f))
		   (rop (socket-output rsocket))
		   (rip (socket-input rsocket))
		   (op (socket-output socket)))
	       (socket-timeout-set! rsocket 0 0)
	       (socket-timeout-set! socket 0 0)
	       (trace-item name ":" port " " path)
	       (http-write-line rop (format "GET ~a HTTP/1.1" path))
	       (http-write-header rop
		  (cons "connection: Upgrade"
		     (http-filter-proxy-header header)))
	       (http-write-line rop)
	       (flush-output-port rop)
	       (let loop ()
		  (let ((c (read-char rip)))
		     (if (eof-object? c)
			 (socket-shutdown socket)
			 (begin
			    (display c op)
			    (flush-output-port op)
			    (loop))))))))))
	 
;*---------------------------------------------------------------------*/
;*    add-event-listener! ::websocket ...                              */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! ws::websocket evt proc . capture)
   (with-access::websocket ws (%mutex state)
      (synchronize %mutex
	 (cond
	    ((string=? evt "open")
	     (with-access::websocket ws (onopens)
		(when (eq? state 'open)
		   (let ((e (instantiate::websocket-event
			       (name "open")
			       (target ws)
			       (value ws))))
		      (proc e)))
		(set! onopens (cons proc onopens))))
	    ((string=? evt "message")
	     (with-access::websocket ws (onmessages %condvar)
		(condition-variable-signal! %condvar)
		(set! onmessages (cons proc onmessages))))
	    ((string=? evt "close")
	     (with-access::websocket ws (oncloses %condvar)
		(condition-variable-signal! %condvar)
		(set! oncloses (cons proc oncloses))))
	    ((string=? evt "error")
	     (with-access::websocket ws (onerrors %condvar)
		(condition-variable-signal! %condvar)
		(set! onerrors (cons proc onerrors))))))))

;*---------------------------------------------------------------------*/
;*    add-event-listener! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! ws::ws-server evt proc . capture)
   (with-access::ws-server ws (%mutex onmessages host port authorization %key %websocket)
      (unless (string=? evt "ready")
	  (with-hop ((hop-event-register-service) :event evt
		       :key %key :mode "websocket")
	     :host host :port port :authorization authorization
	     #f))
      (synchronize %mutex
	 (let ((old (assoc evt onmessages)))
	    (if (pair? old)
		(set-cdr! old (cons proc (cdr old)))
		(set! onmessages (cons (cons evt (list proc)) onmessages)))))
      (when (and (string=? evt "ready") %websocket)
	 (websocket-trigger-server-ready ws))))
   
;*---------------------------------------------------------------------*/
;*    ws-server-init! ...                                              */
;*---------------------------------------------------------------------*/
(define (ws-server-init! ws::ws-server)
   (with-access::ws-server ws (host port authorization %key %websocket)
      (with-hop ((hop-event-info-service))
	 :host host :port port
	 :sync #t
	 :authorization authorization
	 (lambda (v)
	    (if (vector? v)
		(let* ((key (vector-ref v 2))
		       (path (format "/~a/public/server-event/websocket?key=~a"
				(hop-initial-weblet) key))
		       (url (format "ws://~a:~a~a"
			       (vector-ref v 0)
			       (vector-ref v 1)
			       path))
		       (onmessage (lambda (e::event)
				     (websocket-envelope-parse ws e)))
		       (onopen (lambda (e::event)
				  (websocket-trigger-event ws "ready" ws)))
		       (onclose (lambda (e::event)
				   (websocket-trigger-event ws "down" ws)))
		       (webs (instantiate::websocket
				(url url)
				(authorization authorization)
				(oncloses (list onclose))
				(onopens (list onopen))
				(onmessages (list onmessage)))))
		   (set! %key key)
		   (set! %websocket webs)
		   (websocket-connect! webs))
		(raise
		   (instantiate::&io-error
		      (proc "websocket-hop-url")
		      (msg "illegal server info")
		      (obj v))))))))

;*---------------------------------------------------------------------*/
;*    websocket-connected? ...                                         */
;*---------------------------------------------------------------------*/
(define (websocket-connected? ws::websocket)
   (with-access::websocket ws (%socket)
      (socket? %socket)))

;*---------------------------------------------------------------------*/
;*    websocket-connect! ...                                           */
;*---------------------------------------------------------------------*/
(define (websocket-connect! ws::websocket)
   
   (define (close)
      (with-access::websocket ws (%mutex %socket oncloses state)
	 (synchronize %mutex
	    (unless (memq state '(closed closing))
	       (when (pair? oncloses)
		  (set! state 'closing)
		  (let ((se (instantiate::websocket-event
			       (name "close")
			       (target ws)
			       (value ws))))
		     (apply-listeners oncloses se)))
	       (set! state 'closed)
	       (when (socket? %socket)
		  (socket-shutdown %socket)
		  (set! %socket #f))))))
   
   (define (abort e)
      (with-access::websocket ws (%mutex %socket onerrors state)
	 (synchronize %mutex
	    (when (pair? onerrors)
	       (let ((se (instantiate::websocket-event
			    (name "close")
			    (target ws)
			    (data e)
			    (value e))))
		  (apply-listeners onerrors se)))
	    (when (socket? %socket)
	       (socket-shutdown %socket)
	       (set! state 'closed)
	       (set! %socket #f)))))
   
   (define (message val)
      (with-access::websocket ws (onmessages %mutex)
	 (let ((se (instantiate::websocket-event
		      (name "message")
		      (target ws)
		      (data val)
		      (value val))))
	    (synchronize %mutex
	       (apply-listeners onmessages se)))))
   
   (define (eof-error? e)
      (cond-expand
	 (bigloo4.2a
	  (or (isa? e &io-closed-error)
	      (isa? e &io-read-error)
	      (and (isa? e &error)
		   (with-access::&error e (msg)
		      (string=? msg
			 "Can't read on a closed input port")))))
	 (else
	  (or (isa? e &io-closed-error)
	      (isa? e &io-read-error)))))
   
   (with-access::websocket ws (%mutex %condvar %socket url authorization state onopens protocol
				 onmessages onerrors oncloses)
      (synchronize %mutex
	 (unless (websocket-connected? ws)
	    (multiple-value-bind (scheme userinfo host port path)
	       (url-parse url)
	       (let* ((sock (make-client-socket/timeout host port -1 0 (string=? scheme "https")))
		      (in (socket-input sock))
		      (out (socket-output sock)))
		  (set! %socket sock)
		  (let* ((uagent (format "Hop ~a" (hop-version)))
			 (origin (format "http://~a:~a" host port))
			 (sec-ws-key (base64-encode
					(format "ws~a"
					   (hop-login-cookie-crypt-key)))))
		     (http :in in :out out
			:method 'GET
			:http-version 'HTTP/1.1
			:host (format "~a:~a" host port)
			:path path
			:authorization authorization
			:header (append `((user-agent: . ,uagent)
					  (Connection: . Upgrade)
					  (Upgrade: . websocket)
					  (Origin: . ,origin)
					  (Sec-WebSocket-Key: . ,sec-ws-key)
					  (Sec-WebSocket-Version: . 13))
				   (if (string? protocol)
				       `((Sec-WebSocket-Protocol: . ,protocol))
				       '())))
		     (multiple-value-bind (http status line)
			(http-parse-status-line in)
			(if (=fx status 101)
			    (multiple-value-bind (header host port clen tenc auth pauth connection)
			       (http-parse-header in out)
			       (let ((accept (assq sec-websocket-accept: header)))
				  (when (and (pair? accept)
					     (string=?
						(websocket-hybi-accept sec-ws-key)
						(cdr accept)))
				     (set! state 'open)
				     (when (pair? onopens)
					(let ((se (instantiate::websocket-event
						     (name "open")
						     (target ws)
						     (value ws))))
					   (apply-listeners onopens se)))
				     (thread-start!
					(instantiate::hopthread
					   (body (lambda ()
						    (synchronize %mutex
						       (unless (or (pair? onmessages)
								   (pair? onerrors)
								   (pair? oncloses))
							  (condition-variable-wait! %condvar %mutex)))
						    (with-handler
						       (lambda (e)
							  (if (not %socket)
							      (close)
							      (begin
								 (exception-notify e)
								 (if (eof-error? e)
								     (close)
								     (abort e)))))
						       (let ((in (socket-input sock)))
							  (input-timeout-set! in 0)
							  (let loop ()
							     (let ((msg (websocket-read sock)))
								(cond
								   ((string? msg)
								    (message msg)
								    (if %socket (loop) (close)))
								   ((eof-object? msg)
								    (close))
								   (else
								    (abort msg))))))))))))))
			    (close))))))))))

;*---------------------------------------------------------------------*/
;*    websocket-close ...                                              */
;*---------------------------------------------------------------------*/
(define (websocket-close ws::websocket)
   (with-access::websocket ws (%socket)
      (when %socket
	 (socket-shutdown %socket)
	 (set! %socket #f))))

;*---------------------------------------------------------------------*/
;*    websocket-envelope-parse ...                                     */
;*---------------------------------------------------------------------*/
(define (websocket-envelope-parse ws::ws-server e::websocket-event)
   (with-access::websocket-event e (value)
      (let ((m (pregexp-match "^<([rsxifj]) name='([^']+)'>((?:.|[\n])*)</[rsxifj]>$" value)))
	 (if m
	     (let ((k (cadr m))
		   (id (caddr m))
		   (text (cadddr m)))
		(case (string-ref k 0)
		   ((#\i)
		    (websocket-trigger-event ws id (string->integer text)))
		   ((#\f)
		    (websocket-trigger-event ws id (string->real text)))
		   ((#\x)
		    (tprint "!!! WEBSOCKET TODO: " text))
		   ((#\j)
		    (let ((t (pregexp-match "^<!\\[CDATA\\[((?:.|[\n])*)\\]\\]>$" text)))
		       (if t
			   (websocket-trigger-event ws id
			      (javascript->obj (cadr t)))
			   (raise
			      (instantiate::&io-parse-error
				 (proc "websocket")
				 (msg "Illegal value")
				 (obj text))))))
		   ((#\r)
		    (websocket-trigger-server-ready ws))
		   (else
		    (raise
		       (instantiate::&io-parse-error
			  (proc "websocket")
			  (msg "Illegal message type")
			  (obj id))))))
	     (raise
		(instantiate::&io-parse-error
		   (proc "websocket")
		   (msg "Illegal message")
		   (obj value)))))))

;*---------------------------------------------------------------------*/
;*    websocket-trigger-event ...                                      */
;*---------------------------------------------------------------------*/
(define (websocket-trigger-event ws::ws-server id::bstring val::obj)
   (with-access::ws-server ws (onmessages)
      (let ((onmsg (assoc id onmessages)))
	 (when (and (pair? onmsg) (pair? (cdr onmsg)))
	    (let ((e (instantiate::event
			(name id)
			(target ws)
			(value val))))
	       (apply-listeners (cdr onmsg) e))))))

;*---------------------------------------------------------------------*/
;*    websocket-trigger-server-ready ...                               */
;*---------------------------------------------------------------------*/
(define (websocket-trigger-server-ready ws::ws-server)
   (with-access::ws-server ws (onreadys)
      (websocket-trigger-event ws "ready" ws)))

;*---------------------------------------------------------------------*/
;*    websocket-send-frame ...                                         */
;*    -------------------------------------------------------------    */
;*    Write FRAME to PORT (RFC 6455, Section 5.2.)                     */
;*    http://www.rfc-base.org/txt/rfc-6455.txt                         */
;*---------------------------------------------------------------------*/
(define (websocket-send-frame payload::bstring port::output-port
	   #!key (opcode 0) (final #t) (mask #t))
   (with-trace 'websocket "websocket-send-frame"
      (trace-item "opcode=" opcode " final=" final " mask=" mask)
      ;; byte 0, FIN + opcode
      (write-byte (if final (bit-or #x80 opcode) opcode) port)
      ;; byte 1, Mask + payload len (7)
      (let ((len (string-length payload)))
	 ;; the length
	 (cond
	    ((<=fx len 125)
	     ;; 1 byte length
	     (write-byte (if mask (bit-or #x80 len) len) port))
	    ((<=fx len 65535)
	     ;; 2 bytes length
	     (write-byte (if mask (bit-or #x80 126) 126) port)
	     (write-byte (quotientfx len 256) port)
	     (write-byte (remainderfx len 256) port))
	    (else
	     ;; 8 bytes length
	     (write-byte (if mask (bit-or #x80 127) 127) port)
	     (let loop ((i 0)
			(len len))
		(if (=fx i 8)
		    (write-byte len port)
		    (begin
		       (loop (+fx i 1) (quotientfx len 256))
		       (write-byte (remainderfx len 256) port))))))
	 (if mask
	     ;; masked payload data
	     (let ((key (vector (random 256) (random 256) (random 256) (random 256))))
		;; key
		(write-byte (vector-ref-ur key 0) port)
		(write-byte (vector-ref-ur key 1) port)
		(write-byte (vector-ref-ur key 2) port)
		(write-byte (vector-ref-ur key 3) port)
		;; payload
		(let loop ((i 0))
		   (when (<fx i len)
		      (let* ((end (-fx len i))
			     (stop (if (>=fx end 4) 4 end)))
			 (let liip ((j 0))
			    (if (<fx j stop)
				(let ((c (char->integer
					    (string-ref-ur payload (+fx i j))))
				      (k (vector-ref-ur key j)))
				   (write-byte (bit-xor c k) port)
				   (liip (+fx j 1)))
				(loop (+fx i 4))))))))
	     ;; unmasked payload data
	     (display-string payload port)))
      (flush-output-port port)))

;*---------------------------------------------------------------------*/
;*    websocket-send-text ...                                          */
;*    -------------------------------------------------------------    */
;*    Send TEXT over WS.                                               */
;*---------------------------------------------------------------------*/
(define (websocket-send-text sock::socket text::bstring #!key (mask #t) (final #t))
   (websocket-send-frame text (socket-output sock)
      :opcode 1 :final #t :mask mask))

;*---------------------------------------------------------------------*/
;*    websocket-send ...                                               */
;*---------------------------------------------------------------------*/
(define (websocket-send sock::socket obj #!key (mask #t) (final #t))
   (cond
      ((string? obj)
       (websocket-send-text sock obj :mask mask :final final))
      (else
       (error "websocket-send" "obj type not implemented" obj))))

;*---------------------------------------------------------------------*/
;*    websocket-read ...                                               */
;*---------------------------------------------------------------------*/
(define (websocket-read sock::socket)
   
   (define (read-int16 in)
      (let* ((bh (read-byte in))
	     (bl (read-byte in)))
	 (bit-or (bit-lsh bh 8) bl)))
   
   (define (read-int32 in)
      (let* ((wh (read-int16 in))
	     (wl (read-int16 in)))
	 (bit-or (bit-lsh wh 16) wl)))
   
   (define (check-byte b val)
      (and (integer? b) (=fx b val)))
   
   (define (read-playload-len l::int in::input-port)
      (cond
	 ((<=fx l 125)
	  l)
	 ((=fx l 126)
	  (read-int16 in))
	 ((=fx l 127)
	  (let* ((b0 (read-byte in))
		 (b1 (read-byte in))
		 (b2 (read-byte in))
		 (b3 (read-byte in)))
	     (if (check-byte b0 0)
		 (if (check-byte b1 0)
		     (if (check-byte b2 0)
			 (if (check-byte b3 0)
			     (read-int32 in)
			     b3)
			 b2)
		     b1)
		 b0)))
	 (else
	  #f)))

   (define (unmask payload key)
      (let ((len (string-length payload)))
	 (let loop ((i 0))
	    (when (<fx i len)
	       (let* ((end (-fx len i))
		      (stop (if (>=fx end 4) 4 end)))
		  (let liip ((j 0))
		     (if (<fx j stop)
			 (let ((c (char->integer
				     (string-ref-ur payload (+fx i j))))
			       (k (vector-ref-ur key j)))
			    (string-set-ur! payload (+fx i j)
			       (integer->char (bit-xor c k) ))
			    (liip (+fx j 1)))
			 (loop (+fx i 4))))))))
      payload)

   (let ((in (socket-input sock)))
      ;; MS: to be complete, binary, ping, etc. must be supported
      (let ((b (read-byte in)))
	 (if (check-byte b #x81)
	     (let ((s (read-byte in)))
		(if (integer? s)
		    (let ((len (read-playload-len (bit-and s #x7f) in)))
		       (cond
			  ((not len)
			   (if (eof-object? len)
			       len
			       (error "websocket-read" "badly formed frame" s)))
			  ((=fx (bit-and s #x80) 0)
			   ;; unmasked payload
			   (read-chars len in))
			  (else
			   ;; masked payload
			   (let* ((key0 (read-byte in))
				  (key1 (read-byte in))
				  (key2 (read-byte in))
				  (key3 (read-byte in)))
			      (if (integer? key0)
				  (if (integer? key1)
				      (if (integer? key2)
					  (if (integer? key3)
					      (let ((key (vector key0 key1 key2 key3))
						    (payload (read-chars len in)))
						 (if (string? payload)
						     (unmask payload key)
						     payload))
					      key3)
					  key2)
				      key1)
				  key0)))))
		    s))
	     b))))
