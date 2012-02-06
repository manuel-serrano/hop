;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/src/init.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 17 13:55:11 2005                          */
;*    Last change :  Fri Feb  3 18:47:00 2012 (serrano)                */
;*    Copyright   :  2005-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop initialization (default filtering).                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_init

   (library hop)

   (import  hop_param)
   
   (export  (init-http!)
	    (init-webdav!)
	    (init-flash!)
	    (init-zeroconf!)))

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
   (any? (lambda (p)
	    (let ((l (string-length p)))
	       (and (>fx (string-length abspath) l)
		    (string-prefix? p abspath)
		    (char=? (string-ref abspath l) (file-separator)))))
	 (hop-gzipped-directories)))
   
;*---------------------------------------------------------------------*/
;*    http-get ...                                                     */
;*---------------------------------------------------------------------*/
(define (http-get req)
   (with-access::http-request req (abspath query)
      (cond
	 ((not (authorized-path? req abspath))
	  (user-access-denied req))
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
	  (user-access-denied req))
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
;*    http-get-file-not-found ...                                      */
;*---------------------------------------------------------------------*/
(define (http-get-file-not-found req)
   (with-access::http-request req (abspath timeout connection)
      (cond
	 ((hop-service-path? abspath)
	  (http-service-not-found abspath))
	 ((string=? abspath "/crossdomain.xml")
	  (set! connection 'close)
	  (let ((s (format "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE cross-domain-policy SYSTEM \"http://www.macromedia.com/xml/dtds/cross-domain-policy.dtd\">
<cross-domain-policy>
 <allow-access-from domain=\"*\" />
</cross-domain-policy>" (hop-port))))
	     (instantiate::http-response-string
		(request req)
		(timeout timeout)
		(content-type "application/xml")
		(body s))))
	 (else
	  (http-file-not-found abspath)))))

;*---------------------------------------------------------------------*/
;*    http-get-file-query ...                                          */
;*---------------------------------------------------------------------*/
(define (http-get-file-query req)
   (with-access::http-request req (abspath query method timeout header)
      (cond
	 ((string=? query (hop-scm-compile-suffix))
	  (clientc-response req abspath))
	 ((string=? query (hop-hss-compile-suffix))
	  (hss-response req abspath))
	 (else
	  (instantiate::http-response-file
	     (request req)
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
		(request req)
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
;*---------------------------------------------------------------------*/
(define (http-get-file/cache req)
   (with-access::http-request req (abspath header)
      (let ((ce (cache-get get-memory-cache abspath))
	    (im (http-header-field header if-modified-since:))
	    (lm (date->rfc2822-date
		   (seconds->date (file-modification-time abspath)))))
	 (cond
	    ((and (string? im) (string=? im lm))
	     ;; not modified
	     (instantiate::http-response-string
		(request req)
		(start-line "HTTP/1.1 304 Not Modified")
		(content-type (mime-type (prefix abspath) "text/plain"))
		(header `((Last-Modified: . ,lm)))
		(charset (hop-locale))))
	    ((isa? ce cache-entry)
	     ;; in memory cache
	     (with-access::cache-entry ce (value)
		value))
	    (else
	     ;; a new entry
	     (let ((resp (http-get-file req lm #t)))
		(cache-put! get-memory-cache abspath resp)
		resp))))))

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
	     (request req)
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
	     (request req)
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
		(request req)
		(timeout -1)
		(start-line "ICY 200 OK")
		(header `((Last-Modified: . ,last-modified)
			  (Accept-Ranges: . "bytes")))
		(bodyp bodyp)
		(file abspath))))
	 (else
	  ;; send a regular file
	  (instantiate::http-response-file
	     (request req)
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
	 (request req)
	 (charset (hop-locale))
	 (header options)
	 (bodyp #f))))

;*---------------------------------------------------------------------*/
;*    log-local-response ...                                           */
;*---------------------------------------------------------------------*/
(define (log-local-response port req resp)
   
   (define (two-digits n)
      (when (< n 10)
	 (display #\0 port))
      (display n port))
   
   (with-access::http-request req (socket host (u user) method abspath http header)
      ;; distant host address and user
      (fprintf port "~a - ~a "
	       (socket-host-address socket)
	       (if (isa? u user) (with-access::user u (name) name)) "-")
      ;; date
      (display "[" port)
      (let* ((d   (current-date))
	     (tz  (if (= 1 (date-is-dst d))
		      (- (date-timezone d) 3600)
		      (date-timezone d))))
	 (two-digits (date-day d)) (display "/" port)
	 (two-digits (date-month d)) (display "/" port)
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
      (if (isa? resp %http-response-local)
	  (with-access::%http-response-local resp ((str start-line))
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
      (when (>fx (hop-log) 1)
	 (let ((agent (assq :user-agent header))
	       (referer (assq :referer header)))
	    (when (and (pair? agent) (pair? referer))
	       (fprintf port " ~s ~s" (cdr referer) (cdr agent)))))
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
   
   (with-access::http-request req (socket host (p port) (u user) method abspath http header)
      ;; distant host address and user
      (fprintf port "~a - ~a "
	       (socket-host-address socket)
	       (if (isa? u user) (with-access::user u (name) name)) "-")
      ;; date
      (display "[" port)
      (let* ((d   (current-date))
	     (tz  (if (= 1 (date-is-dst d))
		      (- (date-timezone d) 3600)
		      (date-timezone d))))
	 (two-digits (date-day d)) (display "/" port)
	 (two-digits (date-month d)) (display "/" port)
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
      (when (>fx (hop-log) 1)
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

   ;; local filter (Fallback local file filter)
   (hop-filter-add-always-last!
    (lambda (req)
       (when (isa? req http-server-request)
	  (with-access::http-request req (method)
	     (let ((handler (http-find-method-handler method)))
		(if (procedure? handler)
		    (handler req)
		    (http-method-error req)))))))
   
   ;; remote hooks
   (hop-http-response-remote-hook-add!
    (lambda (req resp)
       (with-access::http-request req (localclientp)
	  (cond
	     ((and (not localclientp)
		   (or (not (hop-proxy-allow-remote-client))
		       (not (hop-proxy-ip-allowed? req))))
	      (instantiate::http-response-abort
		 (request req)))
	     ((and localclientp (not (hop-proxy-authentication)))
	      resp)
	     ((and (not localclientp)
		   (not (hop-proxy-remote-authentication))
		   (not (hop-proxy-authentication)))
	      resp)
	     (else
	      (with-access::http-request req (user host port path header)
		 (if (user-authorized-service? user 'proxy)
		     resp
		     (proxy-denied req user host))))))))
   
   ;; logging
   (when (output-port? (hop-log-file))
      (hop-http-response-local-hook-add!
       (lambda (req resp)
	  (log-local-response (hop-log-file) req resp)))
      (hop-http-response-remote-hook-add!
       (lambda (req resp)
	  (log-remote-response (hop-log-file) req resp)))))

;*---------------------------------------------------------------------*/
;*    init-zeroconf! ...                                               */
;*---------------------------------------------------------------------*/
(define (init-zeroconf!)
   (hop-zeroconf-start!)
   (let ((name "Hop"))
      ;; publish main Hop service
      (hop-zeroconf-publish! :name name
	 :type "_http._tcp"
	 :port (hop-port)
	 (format "version=~a" (hop-version))
	 (format "path=~a" (hop-service-base)))
      ;; publish webdav service
      (when (hop-enable-webdav)
	 (hop-zeroconf-publish! :name name
	    :type "_webdav._tcp"
	    :port (hop-port)))
      ;; publish hop available services
      (for-each (lambda (wi)
		   (apply hop-zeroconf-publish! :name (format "~a" (cadr wi))
		      (cddr wi)))
	 (get-weblets-zeroconf))))

