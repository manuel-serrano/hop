;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/http-webdav.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 15 14:30:41 2007                          */
;*    Last change :  Mon Jul 16 09:44:05 2007 (serrano)                */
;*    Copyright   :  2007 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    WebDAV (server side) implementation                              */
;*    This module implements a WebDAV server as specified              */
;*    in the RFC2518. This RFC can be found at:                        */
;*      http://tools.ietf.org/html/rfc2518                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_http-webdav

   (library web)
   
   (include "http-lib.sch"
	    "xml.sch")
   
   (import  __hop_param
	    __hop_types
	    __hop_http-lib
	    __hop_http-response
	    __hop_user
	    __hop_misc
            __hop_xml
	    __hop_http-error)

   (export  (webdav-propfind ::http-request)
	    (webdav-mkcol ::http-request)
	    (webdav-delete ::http-request)
	    (webdav-copy ::http-request))

   (static  (class xml-webdav::xml-element)))

;*---------------------------------------------------------------------*/
;*    *webdav-backend* ...                                             */
;*---------------------------------------------------------------------*/
(define *webdav-backend*
   (instantiate::xml-backend
      (id 'webdav)
      (mime-type "text/xml")
      (doctype "")
      (html-attributes '())
      (header-format "<?xml version=\"1.0\" encoding=\"~a\"?>")
      (no-end-tags-elements '())
      ;; the meta-format contains the closing />
      (meta-format "/>")
      ;; support <foo/> abbreviations
      (abbrev-emptyp #t)))

;*---------------------------------------------------------------------*/
;*    define-xmldav ...                                                */
;*---------------------------------------------------------------------*/
(define-macro (define-xmldav M . rest)
   `(define-xml xml-element #t ,M :attributes ((xmlns:D . "DAV:")) ,@rest))
   
;*---------------------------------------------------------------------*/
;*    webdav elements                                                  */
;*---------------------------------------------------------------------*/
(define-xml xml-webdav #t <DAV> :attributes ((xmlns . "DAV:")))

(define-xmldav <DAV:MULTISTATUS> :markup D:multistatus)
(define-xmldav <DAV:RESPONSE> :markup D:response)
(define-xmldav <DAV:PROPSTAT> :markup D:propstat)
(define-xmldav <DAV:PROP> :markup D:prop)
(define-xmldav <DAV:RESOURCETYPE> :markup D:resourcetype)
(define-xmldav <DAV:COLLECTION> :markup D:collection)
(define-xmldav <DAV:CREATIONDATE> :markup D:creationdate)
(define-xmldav <DAV:GETCONTENTTYPE> :markup D:getcontenttype)
(define-xmldav <DAV:GETLASTMODIFIED> :markup D:getlastmodified)
(define-xmldav <DAV:GETCONTENTLENGTH> :markup D:getcontentlength)
(define-xmldav <DAV:STATUS> :markup D:status)
(define-xmldav <DAV:HREF> :markup D:href)

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-webdav ...                         */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-webdav socket)
   (with-trace 3 'http-response::http-response-webdav
      (with-access::http-response-webdav r (start-line
					    request timeout
					    content-type char-encoding
					    header xml)
	 (let* ((p (socket-output socket))
		(ce (or char-encoding (hop-char-encoding)))
		(sbody (let ((op (open-output-string)))
			  (xml-write xml op ce *webdav-backend*)
			  (close-output-port op)))
		(s (if (eq? ce 'UTF-8)
		       (iso-latin->utf8! sbody)
		       sbody))
		(connection (http-request-connection request)))
	    (when (>fx timeout 0) (output-timeout-set! p timeout))
	    (http-write-line p start-line)
	    (http-write-line p "Content-Type: text/xml"
			     (if (eq? char-encoding 'UTF-8)
				 "; charset=\"utf-8\""
				 ""))
	    (http-write-line p "Content-Length: " (string-length s))
	    (http-write-line p "Connection: " connection)
	    (http-write-header p header)
	    (http-write-line p)
	    (display s p)
	    (newline p)
	    (flush-output-port p)
	    connection))))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-webdav ...                                       */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-webdav p encoding backend)
   (with-access::xml-backend backend (header-format doctype html-attributes)
      (fprintf p header-format encoding)
      (newline p)
      (for-each (lambda (b)
		   (xml-write b p encoding backend))
		(xml-webdav-body obj))))

;*---------------------------------------------------------------------*/
;*    date->iso8601-date ...                                           */
;*---------------------------------------------------------------------*/
(define (date->iso8601-date date)
   (define (2digits num)
      (if (<fx num 10)
          (string #\0 (integer->char (+fx (char->integer #\0) num)))
          (integer->string num)))
   (define (date/timezone date timezone)
      (let* ((tz (/fx timezone 60))
	     (h (/fx tz 60))
	     (m (remainder tz 60)))
	 (format "~a-~a-~aT~a-~a-~aZ~a~a:~a"
		 (date-year date)
		 (2digits (date-month date))
		 (2digits (date-day date))
		 (2digits (date-hour date))
		 (2digits (date-minute date))
		 (2digits (date-second date))
		 (if (<fx tz 0) "+" "-")
		 (2digits (absfx h))
		 (2digits m))))
   (if (>fx (date-is-dst date) 0)
       (let* ((dateu (make-date :sec (date-second date)
			:min (date-minute date)
			:hour (date-hour date)
			:day (date-day date)
			:month (date-month date)
			:year (date-year date)
			:timezone 0))
	      (secu (date->seconds dateu))
	      (sec (date->seconds date)))
	  (date/timezone date (elong->fixnum (-elong secu sec))))
       (date/timezone date (date-timezone date))))

;*---------------------------------------------------------------------*/
;*    date->rfc2822-date ...                                           */
;*---------------------------------------------------------------------*/
(define (date->rfc2822-date date)
   (define (2digits num)
      (if (<fx num 10)
          (string #\0 (integer->char (+fx (char->integer #\0) num)))
          (integer->string num)))
   (define (date/timezone date timezone)
      (let* ((tz (/fx timezone 60))
	     (h (/fx tz 60))
	     (m (remainder tz 60)))
	 (format "~a, ~a ~a ~a ~a:~a:~a ~a~a~a"
		 (day-aname (date-wday date))
		 (date-day date)
		 (month-aname (date-month date))
		 (date-year date)
		 (2digits (date-hour date))
		 (2digits (date-minute date))
		 (2digits (date-second date))
		 (if (<fx tz 0) "+" "-")
		 (2digits (absfx h))
		 (2digits m))))
   (if (>fx (date-is-dst date) 0)
       (let* ((dateu (make-date :sec (date-second date)
			:min (date-minute date)
			:hour (date-hour date)
			:day (date-day date)
			:month (date-month date)
			:year (date-year date)
			:timezone 0))
	      (secu (date->seconds dateu))
	      (sec (date->seconds date)))
	  (date/timezone date (elong->fixnum (-elong secu sec))))
       (date/timezone date (date-timezone date))))

;*---------------------------------------------------------------------*/
;*    webdav-propfind ...                                              */
;*---------------------------------------------------------------------*/
(define (webdav-propfind req::http-request)
   (with-access::http-request req (char-encoding path user)
      (cond
	 ((not (authorized-service? req 'webdav))
	  (user-service-denied req user 'webdav))
	 ((not (authorized-path? req path))
	  (user-access-denied req))
	 (else
	  (instantiate::http-response-webdav
	     (request req)
	     (start-line "HTTP/1.1 207 Multi-Status")
	     (char-encoding (or char-encoding (hop-char-encoding)))
	     (xml (<DAV>
		     (<DAV:MULTISTATUS>
			(directory->dav path)))))))))

;*---------------------------------------------------------------------*/
;*    directory->dav ...                                               */
;*---------------------------------------------------------------------*/
(define (directory->dav dir)
   (if (not (directory? dir))
       (<DAV:RESPONSE>
	  (<DAV:HREF> dir)
	  (<DAV:PROPSTAT>
	     (<DAV:STATUS> "HTTP/1.1 404 Not Found")))
       (let ((files (directory->path-list dir)))
	  (map (lambda (p)
		  (<DAV:RESPONSE>
		     (<DAV:HREF> p)
		     (<DAV:PROPSTAT>
			(<DAV:PROP>
			   (<DAV:GETCONTENTLENGTH>
			      (if (directory? p) 0 (file-size p)))
			   (if (directory? p)
			       (<DAV:RESOURCETYPE> (<DAV:COLLECTION>))
			       (<DAV:RESOURCETYPE>))
			   (<DAV:GETCONTENTTYPE> "httpd/unix-directory")
			   (<DAV:CREATIONDATE>
			      (date->iso8601-date
			       (seconds->date
				(file-modification-time p))))
			   (<DAV:GETLASTMODIFIED>
			      (date->rfc2822-date
			       (seconds->date
				(file-modification-time p)))))
			(<DAV:STATUS> "HTTP/1.1 200 OK"))))
	       files))))
		       
;*---------------------------------------------------------------------*/
;*    webdav-mkcol ...                                                 */
;*---------------------------------------------------------------------*/
(define (webdav-mkcol req::http-request)
   (with-access::http-request req (char-encoding path content-length)
      (let* ((dir (dirname path))
	     (parent (dirname dir)))
	 (cond
	    ((not (authorized-service? req 'webdav-write))
	     (user-service-denied req user 'webdav-write))
	    ((not (authorized-path? req dir))
	     (user-access-denied req))
	    (else
	     (cond
		((not (directory? parent))
		 (instantiate::http-response-string
		    (request req)
		    (char-encoding (or char-encoding (hop-char-encoding)))
		    (start-line "HTTP/1.1 409 Conflict")))
		((directory? dir)
		 (instantiate::http-response-string
		    (request req)
		    (char-encoding (or char-encoding (hop-char-encoding)))
		    (start-line "HTTP/1.1 405 Not allowed")))
		((not (make-directory dir))
		 (instantiate::http-response-string
		    (request req)
		    (char-encoding (or char-encoding (hop-char-encoding)))
		    (start-line "HTTP/1.1 507 Insufficient Storage")))
		((>=elong content-length #e0)
		 (instantiate::http-response-string
		    (request req)
		    (char-encoding (or char-encoding (hop-char-encoding)))
		    (start-line "HTTP/1.1 415 Unsupported Media Type")))
		(else
		 (instantiate::http-response-string
		    (request req)
		    (char-encoding (or char-encoding (hop-char-encoding)))
		    (start-line "HTTP/1.1 201 Created")))))))))

;*---------------------------------------------------------------------*/
;*    webdav-delete ...                                                */
;*---------------------------------------------------------------------*/
(define (webdav-delete req::http-request)
   (with-access::http-request req (char-encoding path content-length header)
      (let ((depth (assq depth: header)))
	 (tprint "header: " header " depth: " depth)
	 (cond
	    ((not (file-exists? path))
	     (instantiate::http-response-string
		(request req)
		(char-encoding (or char-encoding (hop-char-encoding)))
		(start-line "HTTP/1.1 404 File Not Found")))
	    ((directory? path)
	     (if (and (pair? depth) (not (string=? (cadr depth) "infinity")))
		 (http-bad-request (format "Illegal depth: " (cadr depth)))
		 (if (rm-rf path)
		     (instantiate::http-response-string
			(request req)
			(char-encoding (or char-encoding (hop-char-encoding)))
			(start-line "HTTP/1.1 200 Ok"))
		     (instantiate::http-response-string
			(request req)
			(char-encoding (or char-encoding (hop-char-encoding)))
			(start-line "HTTP/1.1 424 Failed Dependency")))))
	    
	    (else
	     (if (delete-file path)
		 (instantiate::http-response-string
		    (request req)
		    (char-encoding (or char-encoding (hop-char-encoding)))
		    (start-line "HTTP/1.1 200 Ok"))
		 (instantiate::http-response-string
		    (request req)
		    (char-encoding (or char-encoding (hop-char-encoding)))
		    (start-line "HTTP/1.1 424 Failed Dependency"))))))))
   
;*---------------------------------------------------------------------*/
;*    rm-rf ...                                                        */
;*---------------------------------------------------------------------*/
(define (rm-rf path)
   (when (file-exists? path)
      (if (directory? path)
	  (let ((files (directory->list path)))
	     (when (every? (lambda (f) (rm-rf (make-file-name path f))) files)
		(delete-directory path)))
	  (delete-file path))))

;*---------------------------------------------------------------------*/
;*    webdav-copy ...                                                  */
;*---------------------------------------------------------------------*/
(define (webdav-copy req::http-request)
   (with-access::http-request req (char-encoding path content-length header)
      (tprint "header: " header)
      (instantiate::http-response-string
	 (request req)
	 (char-encoding (or char-encoding (hop-char-encoding)))
	 (start-line "HTTP/1.1 424 Failed Dependency"))))
   
