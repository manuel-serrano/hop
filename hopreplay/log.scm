;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/hopreplay/log.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Apr 26 09:03:00 2008                          */
;*    Last change :  Sat Apr 26 10:31:20 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Parse log files                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hoprp_log
   
   (library web)
   
   (import  hoprp_param)
   
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
			      (multiple-value-bind (scheme uinfo host port path)
				 (url-parse url)
				 (loop (cons (instantiate::request
						(host host)
						(port port)
						(path (format "http://~a:~a~a"
							      host port
							      path))
						(header '((host . ,host)))
						(method (string->symbol
							 (string-downcase me))))
					     res)))
			      (loop (cons (instantiate::request
					     (host (hoprp-host))
					     (port (hoprp-port))
					     (method (string->symbol
						      (string-downcase me)))
					     (path url))
					  res))))
			 (else
			  (loop res))))))))))
				    
					  
