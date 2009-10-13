;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/http-webdav.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 15 14:30:41 2007                          */
;*    Last change :  Tue Oct 13 09:02:22 2009 (serrano)                */
;*    Copyright   :  2007-09 Manuel Serrano                            */
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
	    __hop_http-error
	    __hop_priv)

   (export  (webdav-propfind ::http-request)
	    (webdav-proppatch ::http-request)
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
(define-xml xml-webdav #t <DAV> :attributes (:xmlns "DAV:"))
 
(define-xmldav <DAV:MULTISTATUS> :markup D:multistatus
   :attributes (:xmlns:D "DAV:"))
(define-xmldav <DAV:RESPONSE> :markup D:response
   :attributes (:xmlns:D "DAV:"))
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
;*    xml-write ::xml-webdav ...                                       */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-webdav p backend)
   (with-access::xml-backend backend (header-format doctype html-attributes)
      (fprintf p header-format (hop-charset))
      (newline p)
      (for-each (lambda (b)
		   (xml-write b p backend))
		(xml-webdav-body obj))))

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
		 :content-length
		 (elong->fixnum clen)
		 :procedure
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
			(let ((xmlns (plist-assq 'xmlns attrs)))
			   (when (and (pair? xmlns)
				      (string=? (cadr xmlns) "DAV:"))
			      (instantiate::xml-element
				 (markup (symbol-append '|D:| m))
				 (id (symbol->string (gensym)))
				 (body '()))))))))))

;*---------------------------------------------------------------------*/
;*    webdav-propfind ...                                              */
;*---------------------------------------------------------------------*/
(define (webdav-propfind req::http-request)
   (with-access::http-request req (content-length socket abspath user header)
      (cond
	 ((not (authorized-service? req 'webdav))
	  (user-service-denied req user 'webdav))
	 ((not (authorized-path? req abspath))
	  (user-access-denied req))
	 (else
	  (let ((depth (get-header header depth: "infinity"))
		(props (if (<=elong content-length 0)
			   (webdav-propfind-all-properties)
			   (parse-propfind-body
			    content-length (socket-input socket)))))
	     (instantiate::http-response-hop
		(request req)
		(start-line "HTTP/1.1 207 Multi-Status")
		(backend *webdav-backend*)
		(content-type (xml-backend-mime-type *webdav-backend*))
		(charset (hop-charset))
		(xml (<DAV>
			(<DAV:MULTISTATUS>
			   (directory->dav abspath depth props))))))))))

;*---------------------------------------------------------------------*/
;*    webdav-proppatch ...                                             */
;*---------------------------------------------------------------------*/
(define (webdav-proppatch req::http-request)
   (instantiate::http-response-string
      (request req)
      (charset (hop-locale))
      (start-line "HTTP/1.1 403 Forbidden")))

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
	 (<DAV:HREF> (url-path-encode p))
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
			(<DAV:STATUS> "HTTP/1.1 404 Not.1 Found"))))
		   ((pair? okp)
		    (<DAV:PROPSTAT>
		       (<DAV:PROP> (reverse! okp))
		       (<DAV:STATUS> "HTTP/1.1 200 OK")))
		   ((pair? errp)
		    (<DAV:PROPSTAT>
		       (<DAV:PROP> (reverse! errp))
		       (<DAV:STATUS> "HTTP/1.1 404 Not.2 Found")))
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
       (if (string=? depth "0")
	   (<RESPONSE> path)
	   (let* ((paths (directory->path-list path))
		  (files (if (string=? depth "infinity")
			     paths
			     (cons path paths))))
	      (map <RESPONSE> files))))
      ((file-exists? path)
       (<RESPONSE> path))
      (else
       (<DAV:RESPONSE>
	  (<DAV:HREF> (url-path-encode path))
	  (<DAV:PROPSTAT>
	     (<DAV:STATUS> "HTTP/1.1 404 Not Found"))))))
		       
;*---------------------------------------------------------------------*/
;*    webdav-mkcol ...                                                 */
;*---------------------------------------------------------------------*/
(define (webdav-mkcol req::http-request)
   (with-access::http-request req (abspath content-length)
      (let* ((dir (dirname abspath))
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
		    (charset (hop-locale))
		    (start-line "HTTP/1.1 409 Conflict")))
		((directory? dir)
		 (instantiate::http-response-string
		    (request req)
		    (charset (hop-locale))
		    (start-line "HTTP/1.1 405 Not allowed")))
		((not (make-directory dir))
		 (instantiate::http-response-string
		    (request req)
		    (charset (hop-locale))
		    (start-line "HTTP/1.1 507 Insufficient Storage")))
		((>=elong content-length #e0)
		 (instantiate::http-response-string
		    (request req)
		    (charset (hop-locale))
		    (start-line "HTTP/1.1 415 Unsupported Media Type")))
		(else
		 (instantiate::http-response-string
		    (request req)
		    (charset (hop-locale))
		    (start-line "HTTP/1.1 201 Created")))))))))

;*---------------------------------------------------------------------*/
;*    webdav-delete ...                                                */
;*---------------------------------------------------------------------*/
(define (webdav-delete req::http-request)
   (with-access::http-request req (abspath content-length header)
      (let ((depth (assq depth: header)))
	 (cond
	    ((not (file-exists? abspath))
	     (instantiate::http-response-string
		(request req)
		(charset (hop-locale))
		(start-line "HTTP/1.1 404 File Not Found")))
	    ((directory? abspath)
	     (if (and (pair? depth) (not (string=? (cadr depth) "infinity")))
		 (http-bad-request (format "Illegal depth: ~a" (cadr depth)))
		 (if (delete-path abspath)
		     (instantiate::http-response-string
			(request req)
			(charset (hop-locale))
			(start-line "HTTP/1.1 200 Ok"))
		     (instantiate::http-response-string
			(request req)
			(charset (hop-locale))
			(start-line "HTTP/1.1 424 Failed Dependency")))))
	    
	    (else
	     (if (delete-file abspath)
		 (instantiate::http-response-string
		    (request req)
		    (charset (hop-locale))
		    (start-line "HTTP/1.1 200 Ok"))
		 (instantiate::http-response-string
		    (request req)
		    (charset (hop-locale))
		    (start-line "HTTP/1.1 424 Failed Dependency"))))))))
   
;*---------------------------------------------------------------------*/
;*    webdav-copy ...                                                  */
;*---------------------------------------------------------------------*/
(define (webdav-copy req::http-request)
   
   (define (resp status)
      (instantiate::http-response-string
	 (request req)
	 (charset (hop-locale))
	 (start-line status)))
   
   (define (cp-file overwrite src dst)
      (let* ((dst (if (directory? dst) (make-file-name dst (basename src)) dst))
	     (dir (dirname dst)))
	 (cond
	    ((not (directory? dir))
	     (resp "HTTP/1.1 409 Conflict"))
	    ((and (file-exists? dst) (string=? overwrite "F"))
	     (resp "HTTP/1.1 412 Precondition Failed"))
	    (else
	     (let ((status (if (file-exists? dst)
			       "HTTP/1.1 204 No Content"
			       "HTTP/1.1 201 Created"))
		   (mod (file-mode src)))
		(if (copy-file src dst)
		    (begin
		       (chmod dst mod)
		       (resp status))
		    (resp "HTTP/1.1 507 Insufficient Storage")))))))

   (define (cp-dir-response cp-res)
      (if (null? cp-res)
	  (resp "HTTP/1.1 201 Created")
	  (instantiate::http-response-hop
	     (request req)
	     (start-line "HTTP/1.1 207 Multi-Status")
	     (backend *webdav-backend*)
	     (content-type (xml-backend-mime-type *webdav-backend*))
	     (charset (hop-locale))
	     (xml (<DAV>
		     (<DAV:MULTISTATUS>
			(map (lambda (p)
				(<DAV:RESPONSE>
				   (<DAV:HREF> (url-path-encode p))
				   (<DAV:STATUS> "HTTP/1.1 507 Insufficient Storage")))
			     cp-res)))))))

   (define (cp-r2 mod src dst)
      (if (make-directory dst)
	  (begin
	     (chmod dst mod)
	     (let ((cp-res (append-map (lambda (p) (cp-r p dst))
				       (directory->path-list src))))
		(cp-dir-response cp-res)))
	  (resp "HTTP/1.1 507 Insufficient Storage")))
   
   (define (cp-dir overwrite depth src dst)
      (let* ((mod (file-mode src))
	     (dir (if (directory? dst) dst (dirname dst)))
	     (dst (make-file-name dir (basename dst))))
	 (cond
	    ((not (directory? dir))
	     (resp "HTTP/1.1 409 Conflict"))
	    ((and (file-exists? dst) (string=? overwrite "F"))
	     (resp "HTTP/1.1 412 Precondition Failed"))
	    ((string=? depth "0")
	     ;; just create the directory
	     (cond
		((and (file-exists? dst) (directory? dst))
		 (resp "HTTP/1.1 204 No Content"))
		((make-directory dst)
		 (chmod dst mod)
		 (resp "HTTP/1.1 201 Created"))
		(else
		 (resp "HTTP/1.1 507 Insufficient Storage"))))
	    ((string=? depth "infinity")
	     ;; copy recursively the directory
	     (cond
		((not (file-exists? dst))
		 (cp-r2 mod src dst))
		((not (directory? dst))
		 (if (delete-file dst)
		     (cp-r2 mod src dst)
		     (resp "HTTP/1.1 424 Failed Dependency")))
		(else
		 (cp-dir-response (cp-r src dst)))))
	    (else
	     (http-bad-request (format "Illegal depth: ~a" (cadr depth)))))))
   
   (with-access::http-request req (header
				   abspath content-length
				   scheme host port user)
      (let* ((destination (get-header header destination: #f))
	     (overwrite (get-header header overwrite: "T"))
	     (i (string-index destination #\:))
	     (dest (when destination
		      (multiple-value-bind (_ _ dhost dport dabspath)
			 (url-parse destination)
			 (if (and (string? dhost)
				  (integer? dport)
				  (string=? dhost host)
				  (=fx dport port))
			     dabspath
			     destination)))))
	 (cond
	    ((not dest)
	     (http-bad-request "Missing destination"))
	    ((not (file-exists? abspath))
	     (resp "HTTP/1.1 404 File Not Found"))
	    ((string=? abspath destination)
	     (resp "HTTP/1.1 403 Forbidden"))
	    ((or (not (user-authorized-path? user (dirname dest)))
		 (not (user-authorized-path? user abspath)))
	     (user-access-denied req))
	    ((directory? abspath)
	     (let ((depth (get-header header depth: "infinity")))
		(cp-dir overwrite depth abspath dest)))
	    (else
	     (cp-file overwrite abspath dest))))))

;*---------------------------------------------------------------------*/
;*    webdav-move ...                                                  */
;*---------------------------------------------------------------------*/
(define (webdav-move req::http-request)
   
   (define (resp status)
      (instantiate::http-response-string
	 (request req)
	 (charset (hop-locale))
	 (start-line status)))
   
   (define (mv-file overwrite src dst)
      (let* ((dst (if (directory? dst)
		      (make-file-name dst (basename src))
		      dst))
	     (dir (dirname dst)))
	 (cond
	    ((not (directory? dir))
	     (resp "HTTP/1.1 409 Conflict"))
	    ((and (file-exists? dst) (string=? overwrite "F"))
	     (resp "HTTP/1.1 412 Precondition Failed"))
	    (else
	     (let ((status (if (file-exists? dst)
			       "HTTP/1.1 204 No Content"
			       "HTTP/1.1 201 Created")))
		(when (file-exists? dst) (delete-file dst))
		(if (rename-file src dst)
		    (resp status)
		    (resp "HTTP/1.1 507 Insufficient Storage")))))))
   
   (define (mv-dir overwrite depth src dst)
      (let* ((mod (file-mode src))
	     (dir (if (directory? dst) dst (dirname dst)))
	     (dst (make-file-name dir (basename dst))))
	 (cond
	    ((not (directory? dir))
	     (resp "HTTP/1.1 409 Conflict"))
	    ((and (file-exists? dst) (string=? overwrite "F"))
	     (resp "HTTP/1.1 412 Precondition Failed"))
	    (else
	     (when (file-exists? dst) (delete-path dst))
	     (if (or (file-exists? dst) (not (rename-file src dst)))
		 (resp "HTTP/1.1 409 Conflict")
		 (resp "HTTP/1.1 204 No Content"))))))
   
   (with-access::http-request req (header
				   abspath
				   content-length scheme host port user)
      (let* ((destination (get-header header destination: #f))
	     (overwrite (get-header header overwrite: "T"))
	     (i (string-index destination #\:))
	     (dest (when destination
		      (multiple-value-bind (_ _ dhost dport dabspath)
			 (url-parse destination)
			 (if (and (string? dhost)
				  (integer? dport)
				  (string=? dhost host)
				  (=fx dport port))
			     dabspath
			     destination)))))
	 (cond
	    ((not dest)
	     (http-bad-request "Missing destination"))
	    ((not (file-exists? abspath))
	     (resp "HTTP/1.1 404 File Not Found"))
	    ((string=? abspath destination)
	     (resp "HTTP/1.1 403 Forbidden"))
	    ((or (not (user-authorized-path? user dest))
		 (not (user-authorized-path? user abspath)))
	     (user-access-denied req))
	    ((directory? abspath)
	     (let ((depth (get-header header depth: "infinity")))
		(if (and (string? depth) (string=? depth "infinity"))
		    (mv-dir overwrite depth abspath dest)
		    (http-bad-request (format "Illegal depth: ~a" depth)))))
	    (else
	     (mv-file overwrite abspath dest))))))

;*---------------------------------------------------------------------*/
;*    webdav-put ...                                                   */
;*---------------------------------------------------------------------*/
(define (webdav-put req::http-request)
   (with-access::http-request req (abspath content-length socket)
      (let ((status (if (file-exists? abspath)
			"HTTP/1.1 204 No Content"
			"HTTP/1.1 201 Created"))
	    (p (open-output-file abspath))
	    (len (elong->fixnum content-length)))
	 (cond
	    ((not (output-port? p))
	     (instantiate::http-response-string
		(request req)
		(charset (hop-locale))
		(start-line (if (directory? (dirname abspath))
				"HTTP/1.1 507 Insufficient Storage"
				"HTTP/1.1 409 Conflict"))))
	    ((<=fx len (send-chars (socket-input socket) p len))
	     (instantiate::http-response-string
		(request req)
		(charset (hop-locale))
		(start-line status)))
	    (else
	     (close-output-port p)
	     (delete-file abspath)
	     (instantiate::http-response-string
		(request req)
		(charset (hop-locale))
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
