;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/src/init.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 17 13:55:11 2005                          */
;*    Last change :  Sat Apr  5 07:00:06 2008 (serrano)                */
;*    Copyright   :  2005-08 Manuel Serrano                            */
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
;*    http-get ...                                                     */
;*---------------------------------------------------------------------*/
(define (http-get req)
   (with-access::http-request req (encoded-path method header)
      (cond
	 ((not (file-exists? encoded-path))
	  (let ((i (string-index encoded-path #\?)))
	     (cond
		((and (fixnum? i) (>fx i 0))
		 (let ((p (substring encoded-path 0 i))
		       (suf (substring encoded-path i (string-length encoded-path))))
		    (cond
		       ((string=? suf (hop-scm-compile-suffix))
			(scm-response req p))
		       ((string=? suf (hop-hss-compile-suffix))
			(hss-response req p))
		       ((file-exists? p)
			(instantiate::http-response-file
			   (request req)
			   (charset (hop-locale))
			   (content-type (mime-type p "text/plain"))
			   (bodyp (eq? method 'GET))
			   (file p)))
		       ((hop-service-path? p)
			(http-service-not-found p))
		       (else
			(http-file-not-found p)))))
		((hop-service-path? encoded-path)
		 (http-service-not-found encoded-path))
;* 			((string=? path "/crossdomain.xml")            */
;* 			 (tprint "======================== /crossdomain.xml") */
;* 			 (http-request-connection-set! req 'close)     */
;* 			 (let ((s (format "<?xml version=\"1.0\" encoding=\"UTF-8\"?> */
;* <!DOCTYPE cross-domain-policy SYSTEM \"http://www.macromedia.com/xml/dtds/cross-domain-policy.dtd\"> */
;* <cross-domain-policy>                                               */
;*  <allow-access-from domain=\"*\" />                                 */
;* </cross-domain-policy>" (hop-port))))                               */
;* 			    (tprint s)                                 */
;* 			    (instantiate::http-response-string         */
;* 			       (request req)                           */
;* 			       (content-type "application/xml")        */
;* 			       (body s))))                             */
		(else
		 (http-file-not-found encoded-path)))))
	 ((is-suffix? (http-request-encoded-path req) "hop")
	  (let ((rep (hop-load (http-request-encoded-path req))))
	     (cond
		((%http-response? rep)
		 rep)
		((xml? rep)
		 (instantiate::http-response-hop
		    (request req)
		    (content-type (mime-type encoded-path (hop-default-mime-type)))
		    (charset (hop-charset))
		    (bodyp (eq? method 'GET))
		    (header '((Cache-Control: . "no-cache")))
		    (xml rep)))
		(else
		 (let ((url (make-hop-url-name
			     (prefix
			      (basename
			       (http-request-encoded-path req))))))
		    (instantiate::http-response-string
		       (start-line "HTTP/1.0 307 Temporary Redirect")
		       (header (list (cons 'location: url)))))))))
	 ((pair? (assq 'icy-metadata: header))
	  (instantiate::http-response-shoutcast
	     (request req)
	     (start-line "ICY 200 OK")
	     (bodyp (eq? method 'GET))
	     (file encoded-path)))
	 (else
	  (cond
	     ((and (string-suffix? ".gz" encoded-path) (accept-gzip? header))
	      ;; send a gzipped file with a mime type corresponding
	      ;; to the ungzipped file
	      (instantiate::http-response-file
		 (request req)
		 (content-type (mime-type (prefix encoded-path) "text/plain"))
		 (charset (hop-locale))
		 (header `((content-encoding: . "gzip")))
		 (bodyp (eq? method 'GET))
		 (file encoded-path)))
	     ((and (file-exists? (string-append encoded-path ".gz"))
		   (member (dirname encoded-path) (hop-gzipped-directories))
		   (accept-gzip? header))
	      ;; send a gzipped version of the file
	      (instantiate::http-response-file
		 (request req)
		 (content-type (mime-type encoded-path "text/plain"))
		 (charset (hop-locale))
		 (header `((content-encoding: . "gzip")))
		 (bodyp (eq? method 'GET))
		 (file (string-append encoded-path ".gz"))))
	     (else
	      ;; send a regular file
	      (instantiate::http-response-file
		 (request req)
		 (content-type (mime-type encoded-path "text/plain"))
		 (charset (hop-locale))
		 (bodyp (eq? method 'GET))
		 (file encoded-path))))))))

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
;*    log-response ...                                                 */
;*---------------------------------------------------------------------*/
(define (log-response port req resp)
   (define (two-digits n)
      (when (< n 10)
	 (display #\0 port))
      (display n port))
   
   (with-access::http-request req (socket host user method encoded-path http header)
      ;; distant host address and user
      (fprintf port "~a - ~a "
	       (socket-host-address socket)
	       (or user "-"))
      ;; date
      (display "[" port)
      (let* ((d   (current-date))
	     (tz  (if (= 1 (date-is-dst d))
		      (- (date-timezone d) 3600)
		      (date-timezone d))))
	 (two-digits (date-day d))    (display "/" port)
	 (two-digits (date-month d))  (display "/" port)
	 (display (date-year d) port) (display ":" port)
	 (two-digits (date-hour d))   (display ":" port)
	 (two-digits (date-minute d)) (display ":" port)
	 (two-digits (date-second d)) (display " " port)
	 (display (if (> tz 0) "+" "-") port)
	 (two-digits (quotient  (abs tz) 3600))
	 (two-digits (remainder (abs tz) 3600)))
      (display "] " port)
      ;; request
      (fprintf port "\"~a ~a ~a\" " method encoded-path http)
      ;; Return code
      (let* ((str (%http-response-local-start-line resp))
	     (len (string-length str)))
	 (let Loop ((i  0)
		    (sp 0))
	    (cond
	       ((or (>= i len) (>= sp 2))
		#f)
	       ((char=? (string-ref str i) #\space)
		(Loop (+ i 1) (+ sp 1)))
	       ((= sp 1)
		(display (string-ref str i) port)
		(Loop (+ i 1) sp))
	       (else
		(Loop (+ i 1) sp)))))
      (display " " port)
      ;; Content-length
      (let ((hdrs (%http-response-local-header resp)))
	 (cond
	    ((http-response-file? resp)
	     (display (file-size (http-response-file-file resp)) port))
	    ((>elong (%http-response-content-length resp) #e0)
	     (display (%http-response-content-length resp) port))
	    (else
	     (display "-" port))))
      ;; Long version (add User-Agent and Referer)
      (let ((agent   (assoc :user-agent header))
	    (referer (assoc :referer header)))
	 (when (and (pair? agent) (pair? referer))
	    (fprintf port " ~s ~s" (cdr referer) (cdr agent))))
      (newline port)
      (flush-output-port port)))

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

   ;; regular http handlers
   (add-http-handler! 'GET http-get)
   (add-http-handler! 'HEAD http-get)
   (add-http-handler! 'OPTIONS http-options)
   
   ;; local filter (Fallback local file filter)
   (hop-filter-add-always-last!
    (lambda (req)
       (when (http-request-localhostp req)
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
	  (log-response (hop-log-file) req resp)))))
