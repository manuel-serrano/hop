;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/hopreplay/log.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Apr 26 09:03:00 2008                          */
;*    Last change :  Sat Apr 26 15:56:13 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Parse log files                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hoprp_log
   
   (library web)
   
   (import  hoprp_param
	    hoprp_login)
   
   (export  (parse-log ::bstring)
	    
	    (class request
	       (host::bstring read-only)
	       (port::int read-only)
	       (path::bstring read-only)
	       (method::symbol read-only)
	       (header::pair-nil read-only (default '())))))

;*---------------------------------------------------------------------*/
;*    log-regexp ...                                                   */
;*---------------------------------------------------------------------*/
(define log-regexp
   (let ((ip-regexp "(?:[0-9]+[.]){3}[0-9]+")
	 (user-regexp "([^ ]+)")
	 (date-regexp "[[][^]]+[]]")
	 (req-regexp "\"(GET) ([^ ]+) HTTP/1.[01]\""))
      (string-append ip-regexp
		     " - "
		     user-regexp
		     " "
		     date-regexp
		     " "
		     req-regexp)))

;*---------------------------------------------------------------------*/
;*    parse-log ...                                                    */
;*---------------------------------------------------------------------*/
(define (parse-log file)
   (with-input-from-file file
      (lambda ()
	 (let loop ((res '()))
	    (let ((line (read-line)))
	       (if (eof-object? line)
		   (reverse! res)
		   (let ((m (pregexp-match log-regexp line)))
		      (match-case m
			 ((?- ?user ?me ?url)
			  (if (substring-at? url "http://" 0)
			      (loop (cons (proxy-request me url) res))
			      (loop (cons (remote-request me url) res))))
			 (else
			  (loop res))))))))))
				    
;*---------------------------------------------------------------------*/
;*    proxy-request ...                                                */
;*---------------------------------------------------------------------*/
(define (proxy-request met url)
   (multiple-value-bind (scheme uinfo host port path)
      (url-parse url)
      (let ((hd (if (and (not uinfo) (hoprp-login))
		    `((proxy-authorization: . ,(hoprp-login))
		      (host: . ,host))
		    `((host: . ,host)))))
	 (instantiate::request
	    (host host)
	    (port port)
	    (path url)
	    (header hd)
	    (method (string->symbol (string-downcase met)))))))

;*---------------------------------------------------------------------*/
;*    remote-request ...                                               */
;*---------------------------------------------------------------------*/
(define (remote-request me url)
   (let ((hd (if (hoprp-login)
		 `((authorization: . ,(hoprp-login)))
		 '())))
      (instantiate::request
	 (host (hoprp-host))
	 (port (hoprp-port))
	 (method (string->symbol (string-downcase me)))
	 (header hd)
	 (path url))))
   
   
