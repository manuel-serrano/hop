;*=====================================================================*/
;*    serrano/prgm/project/hop/src/init.scm                            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 17 13:55:11 2005                          */
;*    Last change :  Sat Jul 21 05:20:41 2007 (serrano)                */
;*    Copyright   :  2005-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop initialization (default filtering).                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_init

   (library hop)
   
   (import  hop_param))

;*---------------------------------------------------------------------*/
;*    *hss-mutex* ...                                                  */
;*---------------------------------------------------------------------*/
(define *hss-mutex* (make-mutex 'hss))

;*---------------------------------------------------------------------*/
;*    hss-write ...                                                    */
;*---------------------------------------------------------------------*/
(define (hss-write hss p)
   (let loop ((hss hss))
      (if (string? hss)
	  (display hss p)
	  (for-each loop hss))))

;*---------------------------------------------------------------------*/
;*    hss-cache ...                                                    */
;*---------------------------------------------------------------------*/
(define hss-cache
   (instantiate::cache
      (path (make-file-path (hop-rc-directory)
			    "cache"
			    (string-append "hss-"
					   (integer->string (hop-port)))))
      (out (lambda (o p) (hss-write o p)))))

;*---------------------------------------------------------------------*/
;*    hss-response ...                                                 */
;*---------------------------------------------------------------------*/
(define (hss-response req)
   (with-access::http-request req (path method header)
      (with-lock *hss-mutex*
	 (lambda ()
	    (let ((cache (cache-get hss-cache path))
		  (mime (mime-type path "text/css")))
	       (if (string? cache)
		   (instantiate::http-response-file
		      (request req)
		      (content-type mime)
		      (bodyp (eq? method 'GET))
		      (file cache))
		   (let* ((hss (hop-load-hss path))
			  (cache (cache-put! hss-cache path hss)))
		      (if (string? cache)
			  (instantiate::http-response-file
			     (request req)
			     (content-type mime)
			     (bodyp (eq? method 'GET))
			     (file cache))
			  (instantiate::http-response-procedure
			     (request req)
			     (content-type mime)
			     (bodyp (eq? method 'GET))
			     (proc (lambda (p) (hss-write hss p))))))))))))

;*---------------------------------------------------------------------*/
;*    hop-filter ...                                                   */
;*    -------------------------------------------------------------    */
;*    Default local file filter.                                       */
;*---------------------------------------------------------------------*/
(hop-filter-add-always-last!
 (lambda (req)
    (when (http-request-localhostp req)
       (with-access::http-request req (path method header)
	  (case method
	     ((GET HEAD)
	      (cond
		 ((not (file-exists? path))
		  (let ((i (string-index path #\?)))
		     (cond
			((and (fixnum? i) (>fx i 0))
			 (let ((p (substring path 0 i)))
			    (cond
			       ((file-exists? p)
				(instantiate::http-response-file
				   (request req)
				   (content-type (mime-type p "text/plain"))
				   (bodyp (eq? method 'GET))
				   (file p)))
			       ((hop-service-path? p)
				(http-service-not-found p))
			       (else
				(http-file-not-found p)))))
			((hop-service-path? path)
			 (http-service-not-found path))
			(else
			 (http-file-not-found path)))))
		 ((is-suffix? (http-request-path req) "hop")
		  (let ((rep (hop-load (http-request-path req))))
		     (cond
			((%http-response? rep)
			 rep)
			((xml? rep)
			 (instantiate::http-response-hop
			    (request req)
			    (content-type (mime-type path "text/html"))
			    (bodyp (eq? method 'GET))
			    (xml rep)))
			(else
			 (http-warning
			  (format "File `~a' loaded but produced no result"
				  (http-request-path req)))))))
		 ((is-suffix? (http-request-path req) "hss")
		  (hss-response req))
		 ((pair? (assq 'icy-metadata: header))
		  (instantiate::http-response-shoutcast
		     (request req)
		     (start-line "ICY 200 OK")
		     (bodyp (eq? method 'GET))
		     (file path)))
		 (else
		  (instantiate::http-response-file
		     (request req)
		     (content-type (mime-type path "text/plain"))
		     (bodyp (eq? method 'GET))
		     (file path)))))
	     ((OPTIONS)
	      (if (hop-enable-webdav)
		  (instantiate::http-response-string
		     (request req)
		     (header `((dav: . 1)
			       (allow: . "GET, HEAD, POST, OPTIONS, COPY, MOVE, PUT, PROPFIND, PROPPATCH, MKCOL, DELETE")))
		     (bodyp #f))
		  (instantiate::http-response-string
		     (request req)
		     (header `((allow: . "GET, HEAD, POST, OPTIONS")))
		     (bodyp #f))))
	     ((PROPFIND)
	      (if (hop-enable-webdav)
		  (webdav-propfind req)
		  (http-bad-request 'propfind)))
	     ((PROPPATCH)
	      (if (hop-enable-webdav)
		  (webdav-proppatch req)
		  (http-bad-request 'proppatch)))
	     ((MKCOL)
	      (if (hop-enable-webdav)
		  (webdav-mkcol req)
		  (http-bad-request 'mkcol)))
	     ((DELETE)
	      (if (hop-enable-webdav)
		  (webdav-delete req)
		  (http-bad-request 'delete)))
	     ((COPY)
	      (if (hop-enable-webdav)
		  (webdav-copy req)
		  (http-bad-request 'copy)))
	     ((MOVE)
	      (if (hop-enable-webdav)
		  (webdav-move req)
		  (http-bad-request 'move)))
	     ((PUT)
	      (if (hop-enable-webdav)
		  (webdav-put req)
		  (http-bad-request 'put)))
	     ((LOCK)
	      (if (hop-enable-webdav)
		  (webdav-lock req)
		  (http-bad-request 'lock)))
	     ((UNLOCK)
	      (if (hop-enable-webdav)
		  (webdav-unlock req)
		  (http-bad-request 'unlock)))
	     (else
	      (http-method-error req)))))))

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
;*    proxy authentication ...                                         */
;*---------------------------------------------------------------------*/
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
	       (instantiate::http-response-string
		  (request req)
		  (start-line "HTTP/1.0 407 Proxy Authentication Required")
		  (header `((Proxy-Authenticate:
			     .
			     ,(format "Basic realm=\"Hop proxy (~a) authentication\""
				      host))))
		  (body "Protected Area! Authentication required."))))))))

;*---------------------------------------------------------------------*/
;*    server logging ...                                               */
;*---------------------------------------------------------------------*/
(when (output-port? (hop-log-file))
   (hop-http-response-local-hook-add!
    (lambda (req resp)
       (log-response (hop-log-file) req resp))))
