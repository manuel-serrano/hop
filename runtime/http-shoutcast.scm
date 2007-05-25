;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/http-shoutcast.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 25 14:15:42 2004                          */
;*    Last change :  Fri May 25 08:10:57 2007 (serrano)                */
;*    Copyright   :  2004-07 Manuel Serrano                            */
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
	    __hop_thread))

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
		  (make-&io-file-not-found-error #f #f
						 'http-response
						 "File not found"
						 file)
		  (make-&io-port-error #f #f
				       'http-response
				       "File not found"
				       file)))
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
			     (send-chars pf p psize)
			     (display (integer->char l16) p)
			     (display pad p)
			     (let loop ((offset psize))
				(when (<elong offset size)
				   (send-chars pf p psize)
				   (display #a000 p)
				   (loop (+elong offset psize)))))))
		      (begin
			 (close-input-port pf)
			 (flush-output-port p))))
		;; close the connection
		'close)))))

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
   
