;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/http-webdav.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 15 14:30:41 2007                          */
;*    Last change :  Wed Jul 18 14:09:11 2007 (serrano)                */
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
	    (webdav-copy ::http-request)
	    (webdav-move ::http-request)
	    (webdav-put ::http-request)
	    (webdav-lock ::http-request)
	    (webdav-unlock ::http-request))

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
   `(define-xml xml-element #t ,M ,@rest))
   
;*---------------------------------------------------------------------*/
;*    webdav elements                                                  */
;*---------------------------------------------------------------------*/
(define-xml xml-webdav #t <DAV> :attributes ((xmlns . "DAV:")))

(define-xmldav <DAV:MULTISTATUS> :markup D:multistatus
   :attributes ((xmlns:D . "DAV:")))
(define-xmldav <DAV:RESPONSE> :markup D:response
   :attributes ((xmlns:D . "DAV:")))
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
	 (tprint "header=" (http-request-header request) " clen="
		 (http-request-content-length request))
	 (let* ((p (socket-output socket))
		(ce (or char-encoding (hop-char-encoding)))
		(sbody (let ((op (open-output-string)))
			  (xml-write xml op ce *webdav-backend*)
			  (close-output-port op)))
		(s (if (eq? ce 'UTF-8)
		       (iso-latin->utf8! sbody)
		       sbody))
		(connection (if (eq? (http-request-connection request)
				     'keep-alive)
				(http-request-connection request)
				'close)))
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
;*    cp-r ...                                                         */
;*---------------------------------------------------------------------*/
(define (cp-r src dest)
   (let loop ((src src)
	      (dest dest))
      (if (directory? src)
	  (let ((destdir (make-file-name dest (basename src))))
	     (cond
		((or (directory? destdir) (make-directory destdir))
		 (append-map (lambda (f)
				(loop f destdir))
			     (directory->path-list src)))
		(else
		 (list destdir))))
	  (let* ((target (make-file-name dest (basename src)))
		 (mod (file-mode src)))
	     (if (copy-file src target)
		 (begin
		    (chmod target mod)
		    '())
		 (list target))))))

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
	 (format "~a-~a-~aT~a-~a-~a ~a~a:~a"
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
;*    get-header ...                                                   */
;*---------------------------------------------------------------------*/
(define (get-header header key default)
   (let ((prop (assq key header)))
      (if (pair? prop)
	  (cdr prop)
	  default)))

;*---------------------------------------------------------------------*/
;*    webdav-propfind-all-properties ...                               */
;*---------------------------------------------------------------------*/
(define (webdav-propfind-all-properties)
   (list <GETCONTENTLENGTH>
	 <GETCONTENTTYPE>
	 <RESOURCETYPE>
	 <GETLASTMODIFIED>
	 <CREATIONDATE>))

;*---------------------------------------------------------------------*/
;*    parse-propfind-body ...                                          */
;*---------------------------------------------------------------------*/
(define (parse-propfind-body clen port)
   (bind-exit (return)
      (xml-parse port
		 (elong->fixnum clen)
		 (lambda (m attrs children)
		    (case m
		       ((prop)
			(return (filter (lambda (p)
					   (or (procedure? p)
					       (xml-element? p)))
					children)))
		       ((getcontentlength)
			<GETCONTENTLENGTH>)
		       ((getcontenttype)
			<GETCONTENTTYPE>)
		       ((resourcetype)
			<RESOURCETYPE>)
		       ((getlastmodified)
			<GETLASTMODIFIED>)
		       ((creationdate)
			<CREATIONDATE>)
		       (else
			(let ((xmlns (assq 'xmlns attrs)))
			   (when (and (pair? xmlns)
				      (string=? (cdr xmlns) "DAV:"))
			      (instantiate::xml-element
				 (markup (symbol-append '|D:| m))
				 (id (symbol->string (gensym)))
				 (body '()))))))))))

;*---------------------------------------------------------------------*/
;*    webdav-propfind ...                                              */
;*---------------------------------------------------------------------*/
(define (webdav-propfind req::http-request)
   (with-access::http-request req (char-encoding
				   content-length
				   socket
				   path user header)
      (cond
	 ((not (authorized-service? req 'webdav))
	  (user-service-denied req user 'webdav))
	 ((not (authorized-path? req path))
	  (user-access-denied req))
	 (else
	  (let ((depth (get-header header depth: "infinity"))
		(props (if (<=elong content-length 0)
			   (webdav-propfind-all-properties)
			   (parse-propfind-body
			    content-length
			    (socket-input socket)))))
	     (instantiate::http-response-webdav
		(request req)
		(start-line "HTTP/1.1 207 Multi-Status")
		(char-encoding (or char-encoding (hop-char-encoding)))
		(xml (<DAV>
			(<DAV:MULTISTATUS>
			   (directory->dav path depth props))))))))))

;*---------------------------------------------------------------------*/
;*    <GETCONTENTLENGTH> ...                                           */
;*---------------------------------------------------------------------*/
(define (<GETCONTENTLENGTH> p)
   (<DAV:GETCONTENTLENGTH> (if (directory? p) 0 (file-size p))))

;*---------------------------------------------------------------------*/
;*    <RESOURCETYPE> ...                                               */
;*---------------------------------------------------------------------*/
(define (<RESOURCETYPE> p)
   (if (directory? p)
       (<DAV:RESOURCETYPE> (<DAV:COLLECTION>))
       (<DAV:RESOURCETYPE>)))

;*---------------------------------------------------------------------*/
;*    <GETCONTENTTYPE> ...                                             */
;*---------------------------------------------------------------------*/
(define (<GETCONTENTTYPE> p)
   (if (directory? p)
       (<DAV:GETCONTENTTYPE> "httpd/unix-directory")
       (<DAV:GETCONTENTTYPE> "application/octet-stream")))

;*---------------------------------------------------------------------*/
;*    <CREATIONDATE> ...                                               */
;*---------------------------------------------------------------------*/
(define (<CREATIONDATE> p)
   (<DAV:CREATIONDATE>
      (date->iso8601-date
       (seconds->date
	(file-modification-time p)))))

;*---------------------------------------------------------------------*/
;*    <GETLASTMODIFIED> ...                                            */
;*---------------------------------------------------------------------*/
(define (<GETLASTMODIFIED> p)
   (<DAV:GETLASTMODIFIED>
      (date->rfc2822-date
       (seconds->date
	(file-modification-time p)))))

;*---------------------------------------------------------------------*/
;*    directory->dav ...                                               */
;*---------------------------------------------------------------------*/
(define (directory->dav path depth properties)
   
   (define (<RESPONSE> p)
      (<DAV:RESPONSE>
	 (<DAV:HREF> p)
	 (let loop ((okp '())
		    (errp '())
		    (props properties))
	    (if (null? props)
		(cond
		   ((and (pair? okp) (pair? errp))
		    (list 		    
		     (<DAV:PROPSTAT>
			(<DAV:PROP> (reverse! okp))
			(<DAV:STATUS> "HTTP/1.1 200 OK"))
		     (<DAV:PROPSTAT>
			(<DAV:PROP> (reverse! errp))
			(<DAV:STATUS> "HTTP/1.1 404 Not Found"))))
		   ((pair? okp)
		    (<DAV:PROPSTAT>
		       (<DAV:PROP> (reverse! okp))
		       (<DAV:STATUS> "HTTP/1.1 200 OK")))
		   ((pair? errp)
		    (<DAV:PROPSTAT>
		       (<DAV:PROP> (reverse! errp))
		       (<DAV:STATUS> "HTTP/1.1 404 Not Found")))
		   (else
		    (<DAV:PROPSTAT>
		       (<DAV:STATUS> "HTTP/1.1 200 OK"))))
		(cond
		   ((procedure? (car props))
		    (loop (cons ((car props) p) okp) errp (cdr props)))
		   ((xml-element? (car props))
		    (loop okp (cons (car props) errp) (cdr props)))
		   (else
		    (loop okp errp (cdr props))))))))

   (cond
      ((directory? path)
       (let* ((paths (directory->path-list path))
	      (files (if (string=? depth "infinity")
			 paths
			 (cons path paths))))
	  (map <RESPONSE> files)))
      ((file-exists? path)
       (<RESPONSE> path))
      (else
       (<DAV:RESPONSE>
	  (<DAV:HREF> path)
	  (<DAV:PROPSTAT>
	     (<DAV:STATUS> "HTTP/1.1 404 Not Found"))))))
		       
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
	 (cond
	    ((not (file-exists? path))
	     (instantiate::http-response-string
		(request req)
		(char-encoding (or char-encoding (hop-char-encoding)))
		(start-line "HTTP/1.1 404 File Not Found")))
	    ((directory? path)
	     (if (and (pair? depth) (not (string=? (cadr depth) "infinity")))
		 (http-bad-request (format "Illegal depth: ~a" (cadr depth)))
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
;*    webdav-copy ...                                                  */
;*---------------------------------------------------------------------*/
(define (webdav-copy req::http-request)
   
   (define (resp ce status)
      (instantiate::http-response-string
	 (request req)
	 (char-encoding ce)
	 (start-line status)))
   
   (define (cp-file ce overwrite src dst)
      (let* ((dst (if (directory? dst) (make-file-name dst (basename src)) dst))
	     (dir (dirname dst)))
	 (cond
	    ((not (directory? dir))
	     (resp ce "HTTP/1.1 409 Conflict"))
	    ((and (file-exists? dst) (string=? overwrite "F"))
	     (resp ce "HTTP/1.1 412 Precondition Failed"))
	    (else
	     (let ((status (if (file-exists? dst)
			       "HTTP/1.1 204 No Content"
			       "HTTP/1.1 201 Created"))
		   (mod (file-mode src)))
		(if (copy-file src dst)
		    (begin
		       (chmod dst mod)
		       (resp ce status))
		    (resp ce "HTTP/1.1 507 Insufficient Storage")))))))

   (define (cp-dir-response ce cp-res)
      (tprint "cp-res=" cp-res)
      (if (null? cp-res)
	  (resp ce "HTTP/1.1 201 Created")
	  (instantiate::http-response-webdav
	     (request req)
	     (start-line "HTTP/1.1 207 Multi-Status")
	     (char-encoding ce)
	     (xml (<DAV>
		     (<DAV:MULTISTATUS>
			(map (lambda (p)
				(<DAV:RESPONSE>
				   (<DAV:HREF> p)
				   (<DAV:STATUS> "HTTP/1.1 507 Insufficient Storage")))
			     cp-res)))))))

   (define (cp-r2 ce mod src dst)
      (if (make-directory dst)
	  (begin
	     (chmod dst mod)
	     (let ((cp-res (append-map (lambda (p) (cp-r p dst))
				       (directory->path-list src))))
		(cp-dir-response ce cp-res)))
	  (resp ce "HTTP/1.1 507 Insufficient Storage")))
   
   (define (cp-dir ce overwrite depth src dst)
      (let* ((mod (file-mode src))
	     (dir (if (directory? dst) dst (dirname dst)))
	     (dst (make-file-name dir (basename dst))))
	 (cond
	    ((not (directory? dir))
	     (resp ce "HTTP/1.1 409 Conflict"))
	    ((and (file-exists? dst) (string=? overwrite "F"))
	     (resp ce "HTTP/1.1 412 Precondition Failed"))
	    ((string=? depth "0")
	     ;; just create the directory
	     (cond
		((and (file-exists? dst) (directory? dst))
		 (resp ce "HTTP/1.1 204 No Content"))
		((make-directory dst)
		 (chmod dst mod)
		 (resp ce "HTTP/1.1 201 Created"))
		(else
		 (resp ce "HTTP/1.1 507 Insufficient Storage"))))
	    ((string=? depth "infinity")
	     ;; copy recursively the directory
	     (cond
		((not (file-exists? dst))
		 (cp-r2 ce mod src dst))
		((not (directory? dst))
		 (if (delete-file dst)
		     (cp-r2 ce mod src dst)
		     (resp ce "HTTP/1.1 424 Failed Dependency")))
		(else
		 (cp-dir-response ce (cp-r src dst)))))
	    (else
	     (http-bad-request (format "Illegal depth: ~a" (cadr depth)))))))
   
   (with-access::http-request req (header
				   path
				   char-encoding content-length
				   scheme host port)
      (let* ((destination (get-header header destination: #f))
	     (overwrite (get-header header overwrite: "T"))
	     (ce (or char-encoding (hop-char-encoding)))
	     (pref (format "://~a:~a" host port))
	     (i (string-index destination #\:))
	     (dest (when destination
		      (if (and i (substring-at? destination pref i))
			  (substring destination
				     (+ i (string-length pref))
				     (string-length destination))
			  destination))))
	 (cond
	    ((not dest)
	     (http-bad-request "Missing destination"))
	    ((not (file-exists? path))
	     (resp ce "HTTP/1.1 404 File Not Found"))
	    ((string=? path destination)
	     (resp ce "HTTP/1.1 403 Forbidden"))
	    ((directory? path)
	     (let ((depth (get-header header depth: "infinity")))
		(cp-dir ce overwrite depth path dest)))
	    (else
	     (cp-file ce overwrite path dest))))))

;*---------------------------------------------------------------------*/
;*    webdav-move ...                                                  */
;*---------------------------------------------------------------------*/
(define (webdav-move req::http-request)
   
   (define (resp ce status)
      (instantiate::http-response-string
	 (request req)
	 (char-encoding ce)
	 (start-line status)))
   
   (define (mv-file ce overwrite src dst)
      (let* ((dst (if (directory? dst)
		      (make-file-name dst (basename src))
		      dst))
	     (dir (dirname dst)))
	 (cond
	    ((not (directory? dir))
	     (resp ce "HTTP/1.1 409 Conflict"))
	    ((and (file-exists? dst) (string=? overwrite "F"))
	     (resp ce "HTTP/1.1 412 Precondition Failed"))
	    (else
	     (let ((status (if (file-exists? dst)
			       "HTTP/1.1 204 No Content"
			       "HTTP/1.1 201 Created")))
		(when (file-exists? dst) (delete-file dst))
		(if (rename-file src dst)
		    (resp ce status)
		    (resp ce "HTTP/1.1 507 Insufficient Storage")))))))
   
   (define (mv-dir ce overwrite depth src dst)
      (let* ((mod (file-mode src))
	     (dir (if (directory? dst) dst (dirname dst)))
	     (dst (make-file-name dir (basename dst))))
	 (cond
	    ((not (directory? dir))
	     (resp ce "HTTP/1.1 409 Conflict"))
	    ((and (file-exists? dst) (string=? overwrite "F"))
	     (resp ce "HTTP/1.1 412 Precondition Failed"))
	    (else
	     (when (file-exists? dst) (rm-rf dst))
	     (if (or (file-exists? dst) (not (rename-file src dst)))
		 (resp ce "HTTP/1.1 409 Conflict")
		 (resp ce "HTTP/1.1 204 No Content"))))))
   
   (with-access::http-request req (header
				   path
				   char-encoding content-length
				   scheme host port)
      (let* ((destination (get-header header destination: #f))
	     (overwrite (get-header header overwrite: "T"))
	     (ce (or char-encoding (hop-char-encoding)))
	     (pref (format "://~a:~a" host port))
	     (i (string-index destination #\:))
	     (dest (when destination
		      (if (and i (substring-at? destination pref i))
			  (substring destination
				     (+ i (string-length pref))
				     (string-length destination))
			  destination))))
	 (cond
	    ((not dest)
	     (http-bad-request "Missing destination"))
	    ((not (file-exists? path))
	     (resp ce "HTTP/1.1 404 File Not Found"))
	    ((string=? path destination)
	     (resp ce "HTTP/1.1 403 Forbidden"))
	    ((directory? path)
	     (let ((depth (get-header header depth: "infinity")))
		(if (and (string? depth) (string=? depth "infinity"))
		    (mv-dir ce overwrite depth path dest)
		    (http-bad-request (format "Illegal depth: ~a" depth)))))
	    (else
	     (mv-file ce overwrite path dest))))))

;*---------------------------------------------------------------------*/
;*    webdav-put ...                                                   */
;*---------------------------------------------------------------------*/
(define (webdav-put req::http-request)
   (with-access::http-request req (path content-length socket char-encoding)
      (let ((status (if (file-exists? path)
			"HTTP/1.1 204 No Content"
			"HTTP/1.1 201 Created"))
	    (p (open-output-file path))
	    (len (elong->fixnum content-length)))
	 (cond
	    ((not (output-port? p))
	     (instantiate::http-response-string
		(request req)
		(char-encoding (or char-encoding (hop-char-encoding)))
		(start-line (if (directory? (dirname path))
				"HTTP/1.1 507 Insufficient Storage"
				"HTTP/1.1 409 Conflict"))))
	    ((=fx len (send-chars (socket-input socket) p len))
	     (close-output-port p)
	     (instantiate::http-response-string
		(request req)
		(char-encoding (or char-encoding (hop-char-encoding)))
		(start-line status)))
	    (else
	     (close-output-port p)
	     (delete-file path)
	     (instantiate::http-response-string
		(request req)
		(char-encoding (or char-encoding (hop-char-encoding)))
		(start-line "HTTP/1.1 507 Insufficient Storage")))))))
   
;*---------------------------------------------------------------------*/
;*    webdav-lock ...                                                  */
;*---------------------------------------------------------------------*/
(define (webdav-lock req::http-request)
   (http-service-unavailable req))

;*---------------------------------------------------------------------*/
;*    webdav-unlock ...                                                */
;*---------------------------------------------------------------------*/
(define (webdav-unlock req::http-request)
   (http-service-unavailable req))
