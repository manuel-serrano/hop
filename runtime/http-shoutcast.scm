;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/runtime/http-shoutcast.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 25 14:15:42 2004                          */
;*    Last change :  Mon Apr 19 15:18:38 2010 (serrano)                */
;*    Copyright   :  2004-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HTTP response                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_http-shoutcast

   (include "http-lib.sch")
   
   (library multimedia)
   
   (import  __hop_param
	    __hop_types
	    __hop_http-lib
	    __hop_http-response
	    __hop_user
	    __hop_misc))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-shoutcast ...                      */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-shoutcast socket)
   (with-trace 3 'http-response::http-response-shoutcast
      (with-access::http-response-shoutcast r (request file)
	 (if (authorized-path? request file)
	     (shoutcast r socket)
	     (http-response (user-access-denied request) socket)))))

;*---------------------------------------------------------------------*/
;*    shoutcast ...                                                    */
;*---------------------------------------------------------------------*/
(define (shoutcast r::http-response-shoutcast socket)
   (with-access::http-response-file r (start-line header content-type file)
      (let ((p (socket-output socket))
	    (pf (open-input-file file)))
	 (if (not (input-port? pf))
	     (raise
	      (if (not (file-exists? file))
		  (instantiate::&io-file-not-found-error
		     (proc 'shoutcast)
		     (msg "File not found")
		     (obj file))
		  (instantiate::&io-port-error
		     (proc 'shoutcast)
		     (msg "File not found")
		     (obj file))))
	     (let* ((size (file-size file))
		    (psize #e10240)
		    (id3 (mp3-id3 file))
		    (name (icy-name id3 file))
		    (title (format "StreamTitle='~a';"
				   (icy-title id3 file)))
		    (l (string-length title))
		    (l16 (+fx (/fx l 16) 1))
		    (clen (+elong size
				  (+elong
				   (fixnum->elong (+fx 1 l))
				   (/elong size psize)))))
		;; remove the timeout
		(output-port-timeout-set! p 0)
		;; regular header
		(http-write-line p start-line)
		(http-write-header p header)
		(http-write-line p "Connection: close")
		(http-write-line p "Content-Length: " clen)
		(when content-type
		   (http-write-line p "Content-Type: " content-type))
		;; shoutcast headers
		(http-write-line p "icy-metaint: " psize)
		(http-write-line p "icy-name: " name)
		(http-write-line p)
		;; the body
		(with-trace 4 'http-response-shoutcast
		   (unwind-protect
		      (call-in-background
		       (lambda ()
			  (let ((pad (make-string (*fx l16 16) #a000)))
			     (blit-string! title 0 pad 0 l)
			     (send-chars-all pf p psize)
			     (display (integer->char l16) p)
			     (display pad p)
			     (let loop ((offset psize))
				(let ((len (-elong size offset)))
				   (when (>elong len #e0)
				      (let ((sz (minelong len psize)))
					 (send-chars-all pf p sz)
					 (display #a000 p)
					 (loop (+elong offset sz)))))))))
		      (begin
			 (close-input-port pf)
			 (flush-output-port p))))
		;; close the connection
		'close)))))

;*---------------------------------------------------------------------*/
;*    send-chars-all ...                                               */
;*---------------------------------------------------------------------*/
(define (send-chars-all pf::input-port p::output-port psize::elong)
   (let loop ((psize psize))
      (let ((psize (-elong psize (send-chars pf p psize))))
	 (when (>elong psize #e0)
	    (loop psize)))))

;*---------------------------------------------------------------------*/
;*    icy-name ...                                                     */
;*---------------------------------------------------------------------*/
(define (icy-name id3 file)
   (define (id3-icy-name id3)
      (with-access::id3 id3 (artist album)
	 (format "~a -- ~a" artist album)))
   (define (file-icy-name file)
      (let ((dir (dirname file)))
	 (if (string=? dir "/")
	     file
	     dir)))
   (if (id3? id3)
       (with-access::id3 id3 (artist album)
	  (if (not (string=? artist "unknown"))
	      (id3-icy-name id3)
	      (file-icy-name file)))
       (file-icy-name file)))

;*---------------------------------------------------------------------*/
;*    icy-title ...                                                    */
;*---------------------------------------------------------------------*/
(define (icy-title id3 file)
   (if (id3? id3)
       (with-access::id3 id3 (title track)
	  (if (not (string=? title "unknown"))
	      (format "~a [~a]" title track)
	      (basename file)))
       (basename file)))
   
