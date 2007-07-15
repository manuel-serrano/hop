;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/http-webdav.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 15 14:30:41 2007                          */
;*    Last change :  Sun Jul 15 16:59:27 2007 (serrano)                */
;*    Copyright   :  2007 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    WebDAV (server side) implementation                              */
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
            __hop_xml)

   (export  (webdav-propfind ::http-request))

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
      (meta-format "/>")))

;*---------------------------------------------------------------------*/
;*    webdav elements                                                  */
;*---------------------------------------------------------------------*/
(define-xml xml-webdav #t <DAV>)
(define-xml xml-element #t <DAV:MULTISTATUS> :markup multistatus)

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
;*    webdav-propfind ...                                              */
;*---------------------------------------------------------------------*/
(define (webdav-propfind req::http-request)
   (with-access::http-request req (char-encoding)
      (instantiate::http-response-webdav
	 (request req)
	 (start-line "HTTP/1.1 207 Multi-Status")
	 (xml (<DAV>
		 (<DAV:MULTISTATUS> :xmlns "DAV"))))))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-webdav ...                                       */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-webdav p encoding backend)
   (with-access::xml-backend backend (header-format doctype html-attributes)
      (fprintf p header-format encoding)
      (newline p)
      (display doctype p)
      (newline p)
      (for-each (lambda (b)
		   (xml-write b p encoding backend))
		(xml-webdav-body obj))))
