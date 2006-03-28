;*=====================================================================*/
;*    serrano/prgm/project/hop/src/init.scm                            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 17 13:55:11 2005                          */
;*    Last change :  Sun Mar 26 08:55:27 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
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
;*    hop alias ...                                                    */
;*---------------------------------------------------------------------*/
(hop-filter-add-always-first!
 (lambda (req)
    (with-access::http-request req (host port path)
       (when (is-local? host)
	  (set! host "localhost")
	  (set! port (hop-port))))))

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
		     (if (>fx i 0)
			 (let ((p (substring path 0 i)))
			    (if (file-exists? p)
				(instantiate::http-response-file
				   (content-type (mime-type p "text/plain"))
				   (bodyp (eq? method 'GET))
				   (file p))
				(http-file-not-found p)))
			 (http-file-not-found path))))
		 ((is-suffix? (http-request-path req) "hop")
		  (let ((rep (hop-load (http-request-path req))))
		     (cond
			((%http-response? rep)
			 rep)
			((xml? rep)
			 (instantiate::http-response-hop
			    (content-type (mime-type path "text/html"))
			    (bodyp (eq? method 'GET))
			    (xml rep)))
			(else
			 (http-warning
			  (format "File `~a' loaded but produced no result"
				  (http-request-path req)))))))
		 ((is-suffix? (http-request-path req) "hss")
		  (let ((hss (hop-load-hss (http-request-path req))))
		     (instantiate::http-response-hop
			(content-type (mime-type path "text/css"))
			(bodyp (eq? method 'GET))
			(xml hss))))
		 ((pair? (assq 'icy-metadata: header))
		  (instantiate::http-response-shoutcast
		     (start-line "ICY 200 OK")
		     (bodyp (eq? method 'GET))
		     (file path)))
		 (else
		  (instantiate::http-response-file
		     (content-type (mime-type path "text/plain"))
		     (bodyp (eq? method 'GET))
		     (file path)))))
	     ((HOPEVT HOP)
	      (http-file-not-found path))
	     (else
	      req))))))

;*---------------------------------------------------------------------*/
;*    proxy authentication ...                                         */
;*---------------------------------------------------------------------*/
(let ((host (hostname)))
   (hop-http-response-remote-hook-add!
    (lambda (req resp)
       (if (not (hop-proxy-authentication))
	   resp
	   (with-access::http-request req (user host port path header)
	      (if (or (not (users-added?)) (user? user))
		  resp
		  (instantiate::http-response-string
		     (start-line "HTTP/1.0 407 Proxy Authentication Required")
		     (header `((Proxy-Authenticate:
				.
				,(format "Basic realm=\"Hop proxy (~a) authentication\""
					 host))))
		     (body "Protected Area! Authentication required."))))))))

;*---------------------------------------------------------------------*/
;*    user authentication                                              */
;*---------------------------------------------------------------------*/
(hop-filter-add-always-first!
 (lambda (req)
    (with-access::http-request req (localhostp path method user)
       (when (and localhostp (users-added?))
	  (cond
	     ((not (file-exists? path))
	      req)
	     ((user-authorized-request? user req)
	      req)
	     (else
	      (user-access-denied req)))))))
 

