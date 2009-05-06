;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/src/init.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 17 13:55:11 2005                          */
;*    Last change :  Wed Mar 25 14:43:31 2009 (serrano)                */
;*    Copyright   :  2005-09 Manuel Serrano                            */
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
	    (init-flash!)))

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
	 ((not (file-exists? abspath))
	  ;; an error
	  (http-get-file-not-found req))
	 (query
	  ;; a file with query arguments
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
	  (http-get-file req #f)))))

;*---------------------------------------------------------------------*/
;*    http-get-file-not-found ...                                      */
;*---------------------------------------------------------------------*/
(define (http-get-file-not-found req)
   (with-access::http-request req (abspath timeout)
      (cond
	 ((hop-service-path? abspath)
	  (http-service-not-found abspath))
	 ((string=? abspath "/crossdomain.xml")
	  (http-request-connection-set! req 'close)
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
   (with-access::http-request req (abspath query method timeout)
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
	     (file abspath))))))
   
;*---------------------------------------------------------------------*/
;*    http-get-hop ...                                                 */
;*---------------------------------------------------------------------*/
(define (http-get-hop req bodyp::bool)
   (with-access::http-request req (abspath timeout)
      (let ((rep (hop-load abspath)))
	 (cond
	    ((%http-response? rep)
	     rep)
	    ((xml? rep)
	     (instantiate::http-response-hop
		(request req)
		(timeout timeout)
		(content-type (mime-type abspath (hop-default-mime-type)))
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
	    (validity (lambda (ce _) (cache-entry? ce)))
	    (max-entries (hop-get-cache-size)))))

;*---------------------------------------------------------------------*/
;*    http-get-file/cache ...                                          */
;*---------------------------------------------------------------------*/
(define (http-get-file/cache req)
   (with-access::http-request req (abspath)
      (let ((cache (cache-memory-get get-memory-cache abspath)))
	 (if (%http-response? cache)
	     cache
	     (let ((resp (http-get-file req #t)))
		(cache-put! get-memory-cache abspath resp)
		resp)))))

;*---------------------------------------------------------------------*/
;*    http-get-file ...                                                */
;*---------------------------------------------------------------------*/
(define (http-get-file req bodyp)
   (with-access::http-request req (abspath query header method timeout)
      (cond
	 ((pair? (assq 'icy-metadata: header))
	  (instantiate::http-response-shoutcast
	     (request req)
	     (timeout -1)
	     (start-line "ICY 200 OK")
	     (bodyp bodyp)
	     (file abspath)))
	 ((and (string-suffix? ".gz" abspath) (accept-gzip? header))
	  ;; send a gzipped file with a mime type corresponding
	  ;; to the ungzipped file
	  (instantiate::http-response-file
	     (request req)
	     (timeout timeout)
	     (content-type (mime-type (prefix abspath) "text/plain"))
	     (charset (hop-locale))
	     (header `((content-encoding: . "gzip")))
	     (bodyp bodyp)
	     (file abspath)))
	 ((and (hop-gzipped-directory? abspath)
	       (file-exists? (string-append abspath ".gz"))
	       (accept-gzip? header))
	  ;; send a gzipped version of the file
	  (instantiate::http-response-file
	     (request req)
	     (timeout timeout)
	     (content-type (mime-type abspath "text/plain"))
	     (charset (hop-locale))
	     (header `((content-encoding: . "gzip")))
	     (bodyp bodyp)
	     (file (string-append abspath ".gz"))))
	 (else
	  ;; send a regular file
	  (instantiate::http-response-file
	     (request req)
	     (timeout timeout)
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
   (let ((options '((allow: . "GET, HEAD, POST, OPTIONS"))))
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
   
   (with-access::http-request req (socket host user method abspath http header)
      ;; distant host address and user
      (fprintf port "~a - ~a "
	       (socket-host-address socket)
	       (if (user? user) (user-name user)) "-")
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
      (let* ((str (%http-response-local-start-line resp))
	     (len (string-length str)))
	 (let loop ((i  0)
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
		(loop (+fx i 1) sp)))))
      (display " " port)
      ;; content-length
      (let ((hdrs (%http-response-header resp)))
	 (cond
	    ((http-response-file? resp)
	     (display (file-size (http-response-file-file resp)) port))
	    ((>elong (%http-response-content-length resp) #e0)
	     (display (%http-response-content-length resp) port))
	    (else
	     (display "-" port))))
      ;; long version (add User-Agent and Referer)
      (when (>fx (hop-log) 1)
	 (let ((agent (assoc :user-agent header))
	       (referer (assoc :referer header)))
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
   
   (with-access::http-request req (socket host (p port) user method abspath http header)
      ;; distant host address and user
      (fprintf port "~a - ~a "
	       (socket-host-address socket)
	       (if (user? user) (user-name user)) "-")
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
	 (let ((agent   (assoc :user-agent header))
	       (referer (assoc :referer header)))
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
       (when (http-server-request? req)
	  (let ((handler (http-find-method-handler (http-request-method req))))
	     (if (procedure? handler)
		 (handler req)
		 (http-method-error req))))))
   
   ;; remote hooks
   (hop-http-response-remote-hook-add!
    (lambda (req resp)
       (cond
	  ((and (not (http-request-localclientp req))
		(or (not (hop-proxy-allow-remote-client))
		    (not (hop-proxy-ip-allowed? req))))
	   (instantiate::http-response-abort
	      (request req)))
	  ((and (http-request-localclientp req)
		(not (hop-proxy-authentication)))
	   resp)
	  ((and (not (http-request-localclientp req))
		(not (hop-proxy-remote-authentication))
		(not (hop-proxy-authentication)))
	   resp)
	  (else
	   (with-access::http-request req (user host port path header)
	      (if (user-authorized-service? user 'proxy)
		  resp
		  (proxy-denied req user host)))))))
   
   ;; logging
   (when (output-port? (hop-log-file))
      (hop-http-response-local-hook-add!
       (lambda (req resp)
	  (log-local-response (hop-log-file) req resp)))
      (hop-http-response-remote-hook-add!
       (lambda (req resp)
	  (log-remote-response (hop-log-file) req resp)))))
