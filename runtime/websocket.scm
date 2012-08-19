;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/runtime/websocket.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 15 07:21:08 2012                          */
;*    Last change :  Sun Aug 19 07:32:57 2012 (serrano)                */
;*    Copyright   :  2012 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Hop WebSocket server-side tools                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_websocket

   (include "http_lib.sch"
            "verbose.sch")

   (library web
	    pthread)

   (import __hop_configure
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
	    __hop_http-error)
   
   (export (websocket-proxy-request? ::pair-nil)
	   (websocket-proxy-connect! ::bstring ::int)
	   (websocket-proxy-response::http-response-proxy-websocket ::http-request)
	   (websocket-server-response header req)))

;*---------------------------------------------------------------------*/
;*    debug-websocket ...                                              */
;*---------------------------------------------------------------------*/
(define debug-websocket #f)

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
	  (mutex-lock! connect-mutex)
	  (hashtable-put! *connect-host-table* (format "~a:~a" host port) #t)
	  (mutex-unlock! connect-mutex)
	  (instantiate::http-response-string
	     (body "Ok")))
       (instantiate::http-response-abort)))
   
;*---------------------------------------------------------------------*/
;*    websocket-proxy-request? ...                                     */
;*---------------------------------------------------------------------*/
(define (websocket-proxy-request? header)
   
   (when (string=? (get-header header upgrade: "") "websocket")
      (let ((host (get-header header host: #f)))
	 (mutex-lock! connect-mutex)
	 (let ((r (hashtable-get *connect-host-table* host)))
	    (mutex-unlock! connect-mutex)
	    r))))

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
      (when debug-websocket
	 (tprint "websocket-protocol76 key1=[" key1 "] key2=[" key2 "]"))
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
	    (string-hex-intern! (md5sum buf)))))
   
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
	    (version (get-header header sec-websocket-version: "-1")))
	 ;; see http_response.scm for the source code that actually sends
	 ;; the bytes of the response to the client.
	 (when debug-websocket
	    (tprint "websocket-register, protocol-version: " version))
	 (let ((v (string->integer version)))
	    (cond
	       ((and (>=fx v 7) (<=fx v 25))
		(websocket-hybi-protocol header req))
	       (else
		(websocket-hixie-protocol header req)))))))

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
	    'persistent))))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-proxy-websocket ...                */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-proxy-websocket socket)
   (with-trace 3 "http-response::http-response-proxy-websocket"
      (cond-expand
	 (enable-threads
	    (thread-start!
	       (instantiate::pthread
		  (body
		     (lambda () (websocket-tunel r socket)))))
	    'persistent)
	 (else
	  (error "http-response"
	     "websocket tunel requires thread support"
	     #f)))))

;*---------------------------------------------------------------------*/
;*    websocket-tunel ...                                              */
;*---------------------------------------------------------------------*/
(define (websocket-tunel resp socket)
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
	    (tprint ">>> starting webosocket-tunel..." name ":" port " " path)
	    (http-write-line rop (format "GET ~a HTTP/1.1" path))
	    (http-write-header rop (cons
				      "connection: Upgrade"
				      (http-filter-proxy-header header)))
	    (http-write-line rop)
	    (flush-output-port rop)
	    (let loop ()
	       (let ((c (read-char rip)))
		  (if (eof-object? c)
		      (begin
			 (tprint ">>> closing tunel..." name ":" port " " path)
			 (socket-close socket))
		      (begin
			 (display c op)
			 (flush-output-port op)
			 (loop)))))))))
	 
	    
