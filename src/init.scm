;*=====================================================================*/
;*    serrano/prgm/project/hop/src/init.scm                            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 17 13:55:11 2005                          */
;*    Last change :  Fri Jan 20 11:22:10 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop initialization (default filtering).                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_init

   (library hop))

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
			 (instantiate::http-response-file
			    (bodyp (eq? method 'GET))
			    (file (substring path 0 i)))
			 (http-file-not-found path))))
		 ((is-suffix? (http-request-path req) "hop")
		  (let ((rep (hop-load (http-request-path req))))
		     (cond
			((%http-response? rep)
			 rep)
			(else
			 (instantiate::http-response-hop
			    (bodyp (eq? method 'GET))
			    (content-type (mime-type path "text/html"))
			    (xml rep))))))
		 ((is-suffix? (http-request-path req) "hss")
		  (let ((hss (hop-load-hss (http-request-path req))))
		     (instantiate::http-response-hop
			(bodyp (eq? method 'GET))
			(content-type (mime-type path "text/css"))
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
	     (else
	      req))))))
