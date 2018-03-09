;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/src/init.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 17 13:55:11 2005                          */
;*    Last change :  Fri Mar  9 10:44:46 2018 (serrano)                */
;*    Copyright   :  2005-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop initialization (default filtering).                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_init

   (include "libraries.sch")

   (library hop)

   (import  hop_param)
   
   (export  (init-server-socket!)
	    (init-http!)
	    (init-webdav!)
	    (init-flash!)
	    (init-zeroconf!)))

;*---------------------------------------------------------------------*/
;*    init-server-socket! ...                                          */
;*    -------------------------------------------------------------    */
;*    Create the Hop server socket according to user options.          */
;*---------------------------------------------------------------------*/
(define (init-server-socket!)
   (when (socket-server? (hop-server-socket))
      (socket-close (hop-server-socket)))
   (with-handler
      (lambda (e)
	 (exception-notify e)
	 (fprint (current-error-port) "Cannot start Hop server, exiting...")
	 (exit 2))
      (if (hop-enable-https)
	  (cond-expand
	     (enable-ssl
	      (let ((cert (read-certificate (hop-https-cert)))
		    (pkey (read-private-key (hop-https-pkey))))
		 (hop-server-socket-set!
		    (make-ssl-server-socket (hop-port)
		       :name (hop-server-listen-addr)
		       :protocol (hop-https-protocol)
		       :cert cert :pkey pkey))))
	     (else
	      (error "hop" "SSL not supported by this version of Hop" #f)))
	  (hop-server-socket-set!
	     (make-server-socket (hop-port)
		:name (hop-server-listen-addr)
		:backlog (hop-somaxconn))))))

;*---------------------------------------------------------------------*/
;*    *http-method-handlers* ...                                       */
;*---------------------------------------------------------------------*/
(define *http-method-handlers* '())

;*---------------------------------------------------------------------*/
;*    http-find-method-handler ...                                     */
;*---------------------------------------------------------------------*/
(define (http-find-method-handler method)
   ;; make a special case for GET, the most frequent method to avoid
   ;; a useless lookup
   (if (eq? method 'GET)
       http-get
       (let ((c (assq method *http-method-handlers*)))
	  (when (pair? c)
	     (cdr c)))))

;*---------------------------------------------------------------------*/
;*    add-http-handler! ...                                            */
;*---------------------------------------------------------------------*/
(define (add-http-handler! method handler)
   (let ((tail (list (cons method handler))))
      (if (null? *http-method-handlers*)
	  (set! *http-method-handlers* tail)
	  (set-cdr! (last-pair *http-method-handlers*) tail))))

;*---------------------------------------------------------------------*/
;*    accept-gzip? ...                                                 */
;*---------------------------------------------------------------------*/
(define (accept-gzip? header)
   (let ((c (assq :accept-encoding header)))
      (and (pair? c)
	   (or (string-prefix? "gzip" (cdr c) 0)
	       (member "gzip" (string-split (cdr c) ","))))))

;*---------------------------------------------------------------------*/
;*    hop-gzipped-directory? ...                                       */
;*---------------------------------------------------------------------*/
(define (hop-gzipped-directory? abspath)
   (any (lambda (p)
	   (let ((l (string-length p)))
	      (and (>fx (string-length abspath) l)
		   (string-prefix? p abspath)
		   (char=? (string-ref abspath l) (file-separator)))))
      (hop-gzipped-directories)))
   
;*---------------------------------------------------------------------*/
;*    http-get ...                                                     */
;*---------------------------------------------------------------------*/
(define (http-get req)
   (with-access::http-request req (abspath query connection header)
      (cond
	 ((and (or (eq? connection 'upgrade)
		   (eq? connection '|keep-alive, upgrade|)
		   (eq? connection '|upgrade, keep-alive|))
	       (websocket-proxy-request? header))
	  (websocket-proxy-response req))
	 ((not (authorized-path? req abspath))
	  (access-denied req))
	 ((not (file-exists? abspath))
	  ;; an error
	  (http-get-file-not-found req))
	 (query
	  ;;; a file with query arguments
	  (http-get-file-query req))
	 ((is-suffix? abspath ".hop")
	  ;; hop source code
	  (http-get-hop req #t))
	 (else
	  ;; a regular file
	  (http-get-file/cache req)))))

;*---------------------------------------------------------------------*/
;*    http-head ...                                                    */
;*---------------------------------------------------------------------*/
(define (http-head req)
   (with-access::http-request req (abspath query)
      (cond
	 ((not (authorized-path? req abspath))
	  (access-denied req))
	 ((not (file-exists? abspath))
	  ;; an error
	  (http-get-file-not-found req))
	 (query
	  ;; a file with query arguments
	  (http-get-file-query req))
	 ((is-suffix? abspath ".hop")
	  ;; hop source code
	  (http-get-hop req #f))
	 (else
	  ;; a regular file
	  (let ((lm (date->rfc2822-date
		       (seconds->date (file-modification-time abspath)))))
	     (http-get-file req lm #f))))))

;*---------------------------------------------------------------------*/
;*    http-connect ...                                                 */
;*---------------------------------------------------------------------*/
(define (http-connect req)
   (with-access::http-request req (host port)
      (if (and (isa? req http-proxy-request)
	       (hop-enable-proxying)
	       (hop-enable-websocket-proxying)
	       (hop-proxy-ip-allowed? req)
	       (or (http-request-local? req) (hop-proxy-allow-remote-client)))
	  ;; okay for proxying connect response (probably used for websocket)
	  (websocket-proxy-connect! host port req)
	  ;; refused
	  (instantiate::http-response-abort))))

;*---------------------------------------------------------------------*/
;*    http-get-file-not-found ...                                      */
;*---------------------------------------------------------------------*/
(define (http-get-file-not-found req)
   (with-access::http-request req (abspath timeout connection)
      (cond
	 ((hop-service-path? abspath)
	  (http-service-not-found abspath req))
	 ((string=? abspath "/crossdomain.xml")
	  (set! connection 'close)
	  (let ((s (format "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE cross-domain-policy SYSTEM \"http://www.macromedia.com/xml/dtds/cross-domain-policy.dtd\">
<cross-domain-policy>
 <allow-access-from domain=\"*\" />
</cross-domain-policy>" (hop-port))))
	     (instantiate::http-response-string
		(timeout timeout)
		(content-type "application/xml")
		(body s))))
	 (else
	  (http-file-not-found abspath)))))

;*---------------------------------------------------------------------*/
;*    http-get-file-query ...                                          */
;*---------------------------------------------------------------------*/
(define (http-get-file-query req)

   (define (query-name query)
      (let* ((str (substring query 3))
	     (i (string-index str #\&)))
	 (if i (substring str 0 i) str)))
      
   (with-access::http-request req (abspath query method timeout header)
      (cond
	 ((string=? query (hop-scm-compile-suffix))
	  (clientc-response req abspath abspath (hop-scm-compile-suffix)))
	 ((string-prefix? "js=" query)
	  (clientc-response req abspath (query-name query) query))
	 ((string-prefix? "es=" query)
	  (let ((resp (clientc-response req abspath (query-name query) query)))
	     (with-access::%http-response resp (content-type)
		(set! content-type "application/javascript")
		resp)))
	 ((string=? query (hop-hss-compile-suffix))
	  (hss-response req abspath))
	 (else
	  (instantiate::http-response-file
	     (timeout timeout)
	     (charset (hop-locale))
	     (content-type (mime-type abspath "text/plain"))
	     (bodyp (eq? method 'GET))
	     (header '((Accept-Ranges: . "bytes")))
	     (file abspath))))))
   
;*---------------------------------------------------------------------*/
;*    http-get-hop ...                                                 */
;*---------------------------------------------------------------------*/
(define (http-get-hop req bodyp::bool)
   (with-access::http-request req (abspath timeout)
      (let ((rep (hop-load abspath)))
	 (cond
	    ((isa? rep %http-response)
	     rep)
	    ((isa? rep xml)
	     (instantiate::http-response-xml
		(backend (hop-xml-backend))
		(timeout timeout)
		(content-type (mime-type abspath "text/plain"))
		(charset (hop-charset))
		(bodyp bodyp)
		(header '((Cache-Control: . "no-cache")))
		(xml rep)))
	    (else
	     (let ((url (make-hop-url-name (prefix (basename abspath)))))
		(instantiate::http-response-string
		   (start-line "HTTP/1.0 307 Temporary Redirect")
		   (header (list (cons 'location: url))))))))))

;*---------------------------------------------------------------------*/
;*    get-memory-cache ...                                             */
;*---------------------------------------------------------------------*/
(define get-memory-cache #f)

;*---------------------------------------------------------------------*/
;*    init-get-cache ...                                               */
;*---------------------------------------------------------------------*/
(define (init-get-cache!)
   (set! get-memory-cache
	 (instantiate::cache-memory
	    (validity (lambda (ce _) (isa? ce cache-entry)))
	    (max-entries (hop-get-cache-size)))))

;*---------------------------------------------------------------------*/
;*    http-get-file/cache ...                                          */
;*    -------------------------------------------------------------    */
;*    The performance of this function is critical. In must not        */
;*    compute date unless needed.                                      */
;*---------------------------------------------------------------------*/
(define (http-get-file/cache req)
   (with-access::http-request req (abspath header)
      (let ((ce (cache-get get-memory-cache abspath))
	    (im (http-header-field header if-modified-since:)))
	 (if (and (not im) (isa? ce cache-entry))
	     ;; fast path, in memory cache
	     (with-access::cache-entry ce (value)
		value)
	     (let ((lm (date->rfc2822-date
			  (seconds->date (file-modification-time abspath)))))
		(cond
		   ((and im (string<=? lm im))
		    ;; not modified
		    (instantiate::http-response-string
		       (start-line "HTTP/1.1 304 Not Modified")
		       (content-type (mime-type (prefix abspath) "text/plain"))
		       (header `((Last-Modified: . ,lm)))
		       (charset (hop-locale))))
		   ((isa? ce cache-entry)
		    (with-access::cache-entry ce (value)
		       value))
		   (else
		    ;; a new entry
		    (let ((resp (http-get-file req lm #t)))
		       (cache-put! get-memory-cache abspath resp)
		       resp))))))))

;*---------------------------------------------------------------------*/
;*    http-get-file ...                                                */
;*    -------------------------------------------------------------    */
;*    The file exists and the request has already been authorized.     */
;*---------------------------------------------------------------------*/
(define (http-get-file req last-modified::bstring bodyp::bool)
   (with-access::http-request req (abspath query header method timeout)
      (cond
	 ((and (string-suffix? ".gz" abspath) (accept-gzip? header))
	  ;; send a gzipped file with a mime type corresponding
	  ;; to the ungzipped file
	  (instantiate::http-response-file
	     (timeout timeout)
	     (header `((Last-Modified: . ,last-modified)
		       (Content-Encoding: . "gzip")
		       (Accept-Ranges: . "bytes")))
	     (content-type (mime-type (prefix abspath) "text/plain"))
	     (charset (hop-locale))
	     (bodyp bodyp)
	     (file abspath)))
	 ((and (hop-gzipped-directory? abspath)
	       (file-exists? (string-append abspath ".gz"))
	       (accept-gzip? header))
	  ;; send a gzipped version of the file
	  (instantiate::http-response-file
	     (timeout timeout)
	     (header `((Last-Modified: . ,last-modified)
		       (Content-Encoding: . "gzip")
		       (Accept-Ranges: . "bytes")))
	     (content-type (mime-type abspath "text/plain"))
	     (charset (hop-locale))
	     (bodyp bodyp)
	     (file (string-append abspath ".gz"))))
	 ((http-header-field header icy-metadata:)
	  =>
	  (lambda (icy)
	     (instantiate::http-response-shoutcast
		(timeout -1)
		(start-line "ICY 200 OK")
		(header `((Last-Modified: . ,last-modified)
			  (Accept-Ranges: . "bytes")))
		(bodyp bodyp)
		(file abspath))))
	 (else
	  ;; send a regular file
	  (instantiate::http-response-file
	     (timeout timeout)
	     (header `((Last-Modified: . ,last-modified)
		       (Accept-Ranges: . "bytes")))
	     (content-type (mime-type abspath "text/plain"))
	     (charset (hop-locale))
	     (bodyp bodyp)
	     (file abspath))))))

;*---------------------------------------------------------------------*/
;*    webdav-options ...                                               */
;*---------------------------------------------------------------------*/
(define (webdav-options)
   '((dav: . 1)
     (allow: . "COPY, MOVE, PUT, PROPFIND, PROPPATCH, MKCOL, DELETE")))

;*---------------------------------------------------------------------*/
;*    add-options! ...                                                 */
;*---------------------------------------------------------------------*/
(define (add-options! opt1 opt2)
   (for-each (lambda (opt)
		(let* ((key (car opt))
		       (cell (assq key opt1)))
		   (if (pair? cell)
		       (set-cdr! cell (string-append (cdr cell) ", " (cdr opt)))
		       (set! opt1 (cons opt opt1)))))
	     opt2)
   opt1)

;*---------------------------------------------------------------------*/
;*    http-options ...                                                 */
;*---------------------------------------------------------------------*/
(define (http-options req)
   (let ((options '((allow: . "GET, HEAD, POST, PUT, OPTIONS"))))
      (when (hop-enable-webdav)
	 (set! options (add-options! options (webdav-options))))
      (instantiate::http-response-string
	 (charset (hop-locale))
	 (header options)
	 (bodyp #f))))

;*---------------------------------------------------------------------*/
;*    log-local-response ...                                           */
;*    -------------------------------------------------------------    */
;*    See "Apache Combined Log Format" at:                             */
;*      http://httpd.apache.org/docs/1.3/logs.html                     */
;*---------------------------------------------------------------------*/
(define (log-local-response port req resp)
   
   (define (two-digits n)
      (when (< n 10)
	 (display #\0 port))
      (display n port))
   
   (with-access::http-request req (socket host method abspath http header)
      ;; distant host address and user
      (fprintf port "~a - ~a "
	 (socket-host-address socket)
	 (with-access::user (http-request-user req) (name)
	    name))
      ;; date
      (display "[" port)
      (let* ((d   (current-date))
	     (tz  (if (= 1 (date-is-dst d))
		      (- (date-timezone d) 3600)
		      (date-timezone d))))
	 (two-digits (date-day d)) (display "/" port)
	 (display (month-aname (date-month d)) port) (display "/" port)
	 (display (date-year d) port) (display ":" port)
	 (two-digits (date-hour d)) (display ":" port)
	 (two-digits (date-minute d)) (display ":" port)
	 (two-digits (date-second d)) (display " " port)
	 (display (if (> tz 0) "+" "-") port)
	 (two-digits (quotient  (abs tz) 3600))
	 (two-digits (remainder (abs tz) 3600)))
      (display "] " port)
      ;; request
      (fprintf port "\"~a ~a ~a\" " method abspath http)
      ;; Return code
      (if (isa? resp %http-response-server)
	  (with-access::%http-response-server resp ((str start-line))
	     (let ((len (string-length str)))
		(let loop ((i 0)
			   (sp 0))
		   (cond
		      ((or (>=fx i len) (>=fx sp 2))
		       #f)
		      ((char=? (string-ref str i) #\space)
		       (loop (+fx i 1) (+fx sp 1)))
		      ((=fx sp 1)
		       (display (string-ref str i) port)
		       (loop (+fx i 1) sp))
		      (else
		       (loop (+fx i 1) sp))))))
	  (display 305 port))
      (display " " port)
      ;; content-length
      (with-access::%http-response resp ((hdrs header) content-length)
	 (cond
	    ((isa? resp http-response-file)
	     (with-access::http-response-file resp (file)
		(display (file-size file) port)))
	    ((>elong content-length #e0)
	     (display content-length port))
	    (else
	     (display "-" port))))
      ;; long version (add User-Agent and Referer)
      (when (>=fx (hop-log) 1)
        (let* ((agent0 (assq :user-agent header))
               (agent (if (pair? agent0)
                          (cdr agent0)
                          "-"))
               (referer0 (assq :referer header))
               (referer (if (pair? referer0)
                            (cdr referer0)
                            "-")))
          (fprintf port " ~s ~s" referer agent)))
      (newline port)
      (flush-output-port port)
      resp))

;*---------------------------------------------------------------------*/
;*    log-remote-response ...                                          */
;*---------------------------------------------------------------------*/
(define (log-remote-response port req resp)
   
   (define (two-digits n)
      (when (< n 10)
	 (display #\0 port))
      (display n port))
   
   (with-access::http-request req (socket host (p port) method abspath http header)
      ;; distant host address and user
      (fprintf port "~a - ~a "
	 (socket-host-address socket)
	 (with-access::user (http-request-user req) (name) name)) 
      ;; date
      (display "[" port)
      (let* ((d   (current-date))
	     (tz  (if (= 1 (date-is-dst d))
		      (- (date-timezone d) 3600)
		      (date-timezone d))))
	 (two-digits (date-day d)) (display "/" port)
	 (display (month-aname (date-month d)) port) (display "/" port)
	 (display (date-year d) port) (display ":" port)
	 (two-digits (date-hour d)) (display ":" port)
	 (two-digits (date-minute d)) (display ":" port)
	 (two-digits (date-second d)) (display " " port)
	 (display (if (> tz 0) "+" "-") port)
	 (two-digits (quotient  (abs tz) 3600))
	 (two-digits (remainder (abs tz) 3600)))
      (display "] " port)
      ;; request
      (fprintf port "\"~a http://~a:~a~a ~a\" " method host p abspath http)
      ;; Return code
      (display "305" port)
      (display " " port)
      ;; Content-length
      (display "-" port)
      ;; Long version (add User-Agent and Referer)
      (when (>=fx (hop-log) 1)
	 (let ((agent   (assq :user-agent header))
	       (referer (assq :referer header)))
	    (when (and (pair? agent) (pair? referer))
	       (fprintf port " ~s ~s" (cdr referer) (cdr agent)))))
      (newline port)
      (flush-output-port port)
      resp))

;*---------------------------------------------------------------------*/
;*    hop-proxy-ip-allowed? ...                                        */
;*---------------------------------------------------------------------*/
(define (hop-proxy-ip-allowed? req)
   (or (not (hop-proxy-ip-mask))
       (with-access::http-request req (socket)
	  (let ((ip (ipv4->elong (socket-host-address socket))))
	     (=elong (bit-andelong ip (hop-proxy-ip-mask-word)) ip)))))

;*---------------------------------------------------------------------*/
;*    init-webdav! ...                                                 */
;*---------------------------------------------------------------------*/
(define (init-webdav!)
   (add-http-handler! 'PROPFIND webdav-propfind)
   (add-http-handler! 'PROPPATCH webdav-proppatch)
   (add-http-handler! 'MKCOL webdav-mkcol)
   (add-http-handler! 'DELETE webdav-delete)
   (add-http-handler! 'COPY webdav-copy)
   (add-http-handler! 'MOVE webdav-move)
   (add-http-handler! 'PUT webdav-put)
   (add-http-handler! 'LOCK webdav-lock)
   (add-http-handler! 'UNLOCK webdav-unlock))

;*---------------------------------------------------------------------*/
;*    init-flash! ...                                                  */
;*---------------------------------------------------------------------*/
(define (init-flash!)
   (add-http-handler! 'FLASH-POLICY-FILE hop-event-policy-file))
   
;*---------------------------------------------------------------------*/
;*    init-http! ...                                                   */
;*---------------------------------------------------------------------*/
(define (init-http!)

   ;; prepare the get cache
   (init-get-cache!)
   
   ;; regular http handlers
   (add-http-handler! 'GET http-get)
   (add-http-handler! 'HEAD http-head)
   (add-http-handler! 'OPTIONS http-options)
   (add-http-handler! 'CONNECT http-connect)

   ;; local filter (Fallback local file filter)
   (hop-filter-add-always-last!
      (lambda (req)
	 (with-access::http-request req (method)
	    (cond
	       ((isa? req http-server-request)
		(let ((handler (http-find-method-handler method)))
		   (if (procedure? handler)
		       (handler req)
		       (http-method-error req))))
	       ((eq? method 'CONNECT)
		(http-connect req))))))
   
   ;; proxy hooks
   (hop-http-response-proxy-hook-add!
      (lambda (req resp)
	 (with-access::http-request req (socket)
	    (if (not socket)
		;; a inner request
		resp
		(let ((localclientp (http-request-local? req)))
		   (cond
		      ((and (not localclientp)
			    (or (not (hop-proxy-allow-remote-client))
				(not (hop-proxy-ip-allowed? req))))
		       (instantiate::http-response-abort))
		      ((and localclientp (not (hop-proxy-authentication)))
		       resp)
		      ((and (not localclientp)
			    (not (hop-proxy-remote-authentication))
			    (not (hop-proxy-authentication)))
		       resp)
		      (else
		       (with-access::http-request req (host port path header)
			  (let ((user (http-request-user req)))
			     (if (user-authorized-service? user 'proxy)
				 resp
				 (proxy-denied req user host)))))))))))
   
   ;; logging
   (when (output-port? (hop-log-file))
      (hop-http-response-server-hook-add!
       (lambda (req resp)
	  (log-local-response (hop-log-file) req resp)))
      (hop-http-response-proxy-hook-add!
       (lambda (req resp)
	  (log-remote-response (hop-log-file) req resp)))))

;*---------------------------------------------------------------------*/
;*    init-zeroconf! ...                                               */
;*---------------------------------------------------------------------*/
(define (init-zeroconf!)
   (add-event-listener! (zeroconf-backend) "onready"
      (lambda (o)
	 (let ((name "Hop")
	       (session (format "session=~a" (hop-session))))
	    ;; publish main Hop service
	    (zeroconf-publish! :name name
	       :type "_http._tcp"
	       :port (hop-port)
	       session
	       (format "version=~a" (hop-version))
	       (format "path=~a" (hop-service-base)))
	    ;; publish webdav service
	    (when (hop-enable-webdav)
	       (zeroconf-publish! :name name
		  :type "_webdav._tcp"
		  :port (hop-port)
		  session))
	    ;; publish hop available services
	    (for-each (lambda (wi)
			 (apply zeroconf-publish!
			    :name (format "~a" (cadr wi))
			    session
			    (cddr wi)))
	       (get-weblets-zeroconf)))))
   (zeroconf-start))
