;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/runtime/websocket.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 15 07:21:08 2012                          */
;*    Last change :  Mon Feb 10 11:15:15 2014 (serrano)                */
;*    Copyright   :  2012-14 Manuel Serrano                            */
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
	    __hop_js-comp
	    __hop_cgi
	    __hop_read
	    __hop_service
	    __hop_http-response
	    __hop_http-error
	    __hop_event)
   
   (export (class websocket
	      (%mutex read-only (default (make-mutex)))
	      (%socket (default #f))
	      (url::bstring read-only)
	      (onopens::pair-nil (default '()))
	      (onerrors::pair-nil (default '()))
	      (oncloses::pair-nil (default '()))
	      (onmessages::pair-nil (default '()))
	      (authorization (default #f))
	      (state::symbol (default 'connecting))
	      (version read-only (default 'hybi)))

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
	   (websocket-proxy-connect! ::bstring ::int)
	   (websocket-proxy-response::http-response-proxy-websocket ::http-request)
	   (websocket-server-response header req)
	   (websocket-debug)
	   (websocket-debug-level::int)

	   (websocket-connect! ::websocket)))

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
(define (websocket-proxy-connect! host port)
   (if (and (hop-enable-proxing) (hop-enable-websocket-proxing))
       (begin
	  (synchronize connect-mutex
	     (hashtable-put! *connect-host-table* (format "~a:~a" host port) #t))
	  (instantiate::http-response-string
	     (body "Ok")))
       (instantiate::http-response-abort)))
   
;*---------------------------------------------------------------------*/
;*    websocket-proxy-request? ...                                     */
;*---------------------------------------------------------------------*/
(define (websocket-proxy-request? header)
   (when (string=? (get-header header upgrade: "") "websocket")
      (let ((host (get-header header host: #f)))
	 (synchronize connect-mutex
	    (hashtable-get *connect-host-table* host)))))

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
(define (websocket-server-response req key)
   
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
      (with-trace (+ (websocket-debug-level) 2) "websocket-protocol76"
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
	 (location (websocket-server-location (get-header header host: #f)))
	 (origin (get-header header origin: "localhost"))
	 (protocol (get-header header WebSocket-Protocol: #f))
	 (connection 'Upgrade)
	 (sec (websocket-sec-challenge req header))))      
   
   (define (websocket-hybi-protocol header req)
      ;; http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-08
      (with-trace (+ (websocket-debug-level) 3) "websocket-hybi-protocol"
	 (trace-item "header=" header)
	 (let ((key (get-header header sec-websocket-key: #f)))
	    (instantiate::http-response-websocket
	       (request req)
	       (start-line "HTTP/1.1 101 Switching Protocols")
	       (connection 'Upgrade)
	       (protocol (get-header header WebSocket-Protocol: #f))
	       (accept (websocket-hybi-accept key))))))
   
   (with-access::http-request req (header connection socket)
      (let ((host (get-header header host: #f))
	    (version (get-header header sec-websocket-version: "-1")))
	 ;; see http_response.scm for the source code that actually sends
	 ;; the bytes of the response to the client.
	 (with-trace (websocket-debug-level) "ws-register"
	    (trace-item "version="
	       version)
	    (trace-item "origin="
	       (get-header header origin: "localhost"))
	    (trace-item "WebSocket-Protocolo="
	       (get-header header WebSocket-Protocolo: #f))
	    (let ((v (string->integer version)))
	       (cond
		  ((and (>=fx v 7) (<=fx v 25))
		   (websocket-hybi-protocol header req))
		  (else
		   (websocket-hixie-protocol header req))))))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-websocket ...                      */
;*    -------------------------------------------------------------    */
;*    websocket are described at:                                      */
;*    http://tools.ietf.org/html/draft-hixie-thewebsocketprotocol      */
;*                                                                     */
;*    They impose a strict ordering and case for the reply. Thus Hop   */
;*    uses a dedicated class instead of a generic response-string.     */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-websocket socket)
   (with-trace 3 "http-response::http-response-websocket"
      (with-access::http-response-websocket r (start-line
					       connection
					       origin
					       location
					       timeout
					       protocol
					       sec
					       accept)
	 (let ((p (socket-output socket)))
	    (when (>=fx timeout 0) (output-timeout-set! p timeout))
	    (http-write-line-string p start-line)
	    (http-write-line-string p "Upgrade: WebSocket")
	    (http-write-line p "Connection: " connection)
	    (cond
	       (accept
		  (http-write-line p "Sec-WebSocket-Accept: " accept)
		  (when protocol
		     (http-write-line p "WebSocket-Protocol: " protocol))
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
		   (http-write-line p "WebSocket-Protocol: " protocol))
		(http-write-line p)))
	    (flush-output-port p)
	    ;; set the socket in non blocking mode to prevent
	    ;; down connections to block all event broadcasts
	    (output-timeout-set! p 5000)
	    'persistent))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-proxy-websocket ...                */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-proxy-websocket socket)
   (with-trace 3 "http-response::http-response-proxy-websocket"
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
;*    websocket-tunnel ...                                              */
;*---------------------------------------------------------------------*/
(define (websocket-tunnel resp socket)
   (with-trace (websocket-debug-level) "ws-tuner"
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
			 (socket-close socket)
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
	     (with-access::websocket ws (onmessages)
		(set! onmessages (cons proc onmessages))))
	    ((string=? evt "close")
	     (with-access::websocket ws (oncloses)
		(set! oncloses (cons proc oncloses))))
	    ((string=? evt "error")
	     (with-access::websocket ws (onerrors)
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
   
   (define (read-int16 in)
      (let ((bh (read-byte in))
	    (bl (read-byte in)))
	 (bit-or (bit-lsh bh 8) bl)))
   
   (define (read-int32 in)
      (let ((wh (read-int16 in))
	    (wl (read-int16 in)))
	 (bit-or (bit-lsh wh 16) wl)))
   
   (define (close)
      (with-access::websocket ws (%mutex %socket oncloses state)
	 (synchronize %mutex
	    (when (pair? oncloses)
	       (set! state 'closing)
	       (let ((se (instantiate::websocket-event
			    (name "close")
			    (target ws)
			    (value ws))))
		  (apply-listeners oncloses se)))
	    (set! state 'closed)
	    (socket-close %socket)
	    (set! %socket #f))))

   (define (abort)
      (with-access::websocket ws (%mutex %socket onerrors)
	 (synchronize %mutex
	    (when (pair? onerrors)
	       (let ((se (instantiate::websocket-event
			    (name "close")
			    (target ws)
			    (value ws))))
		  (apply-listeners onerrors se))))
	 (close)))

   (define (message val)
      (with-access::websocket ws (onmessages)
	 (let ((se (instantiate::websocket-event
		      (name "message")
		      (target ws)
		      (value val))))
	    (apply-listeners onmessages se))))

   (define (read-check-byte in val)
      (unless (=fx (read-byte in) val)
	 (abort)))

   (define (read-messages)
      (with-access::websocket ws (%socket)
	 (let ((in (socket-input %socket)))
	    (let loop ()
	       (read-check-byte in #x81)
	       (let ((s (read-byte in)))
		  (cond
		     ((<=fx s 125)
		      (message (read-chars s in)))
		     ((=fx s 126)
		      (message (read-chars (read-int16 in) in)))
		     ((=fx s 127)
		      (read-check-byte in 0)
		      (read-check-byte in 0)
		      (read-check-byte in 0)
		      (read-check-byte in 0)
		      (message (read-chars (read-int32 in) in)))
		     (else
		      (abort))))
	       (loop)))))
   
   (with-access::websocket ws (%mutex %socket url authorization state onopens)
      (synchronize %mutex
	 (unless (websocket-connected? ws)
	    (multiple-value-bind (scheme userinfo host port path)
	       (url-parse url)
	       (let* ((sock (make-client-socket/timeout host port -1 0 #f))
		      (in (socket-input sock))
		      (out (socket-output sock)))
		  (set! %socket sock)
		  (let* ((uagent (format "Hop ~a" (hop-version)))
			 (origin (format "http://~a:~a" host port))
			 (sec-ws-key (base64-encode
					(format "ws~a"
					   (hop-login-cookie-crypt-key)))))
		     (http :in in :out out
			:protocol "http"
			:method 'GET
			:http-version 'HTTP/1.1
			:host (format "~a:~a" host port)
			:path path
			:authorization authorization
			:header `((upgrade: . websocket)
				  (connection: . upgrade)
				  (origin: . ,origin)
				  (sec-websocket-key: . ,sec-ws-key)
				  (sec-websocket-version: . 13)
				  (user-agent: . ,uagent)))
		     (multiple-value-bind (http status line)
			(http-parse-status-line in)
			(when (=fx status 101)
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
				       (instantiate::pthread
					  (body (lambda ()
						   (unwind-protect
						      (with-handler
							 (lambda (e)
							    #f)
							 (read-messages))
						      (close))))))))))))))))))

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
			   (websocket-trigger-event ws id (javascript->obj (cadr t)))
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
