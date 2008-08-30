;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/runtime/hop-inline.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec 23 08:17:58 2005                          */
;*    Last change :  Sat Aug 30 18:53:05 2008 (serrano)                */
;*    Copyright   :  2005-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The implementation of the HOP inline markup.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-inline

   (library web)
   
   (include "xml.sch")

   (import  __hop_param
	    __hop_types
	    __hop_user
	    __hop_xml
	    __hop_dom
	    __hop_misc
	    __hop_js-lib
	    __hop_service
	    __hop_http-response
	    __hop_hop)

   (static  (class xml-inline-element::xml-element
	       (host::bstring read-only)
	       (port::int read-only)
	       (path::bstring read-only)
	       (eid read-only)
	       (userinfo read-only (default #f))
	       (authorization read-only (default #f))))
   
   (export  (<INLINE> . ::obj)))

;*---------------------------------------------------------------------*/
;*    <INLINE> ...                                                     */
;*---------------------------------------------------------------------*/
(define-xml-compound <INLINE> ((id #unspecified string)
			       (src #f string)
			       (early #t boolean))
   (cond
      ((not (string? src))
       (error '<INLINE> "Missing :src attribute" src))
      (else
       (multiple-value-bind (_ userinfo host port path)
	  (url-parse src)
	  (let* ((req (current-request))
		 (auth (and (http-server-request? req)
			    (http-server-request-authorization req))))
	     (if early
		 (xml-inline (or host (hostname))
			     (or port (hop-port))
			     path userinfo auth id)
		 (instantiate::xml-inline-element
		    (markup '_)
		    (eid id)
		    (body '())
		    (host host)
		    (port port)
		    (path path)
		    (userinfo userinfo)
		    (authorization auth))))))))
   
;*---------------------------------------------------------------------*/
;*    xml-write ::xml-inline-element ...                               */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-inline-element p backend)
   (with-access::xml-inline-element obj (host port path userinfo authorization eid)
      (xml-write (xml-inline host port path userinfo authorization eid)
		 p backend)))

;*---------------------------------------------------------------------*/
;*    xml-inline ...                                                   */
;*---------------------------------------------------------------------*/
(define (xml-inline host port path userinfo authorization eid)
   (define (filter-attr attr)
      (not (eq? (car attr) 'id)))
   (http-send-request
    (instantiate::http-server-request
       (host host)
       (port port)
       (path path)
       (user (user-nil))
       (userinfo userinfo)
       (authorization authorization))
    (lambda (p status header clength tenc)
       (let ((cl (cond
		    ((elong? clength)
		     (elong->fixnum clength))
		    ((integer? clength)
		     clength)
		    (else
		     -1))))
	  (if (=fx status 200)
	      (bind-exit (return)
		 (let ((res (instantiate::xml-document
			       (markup 'document)
			       (id (xml-make-id #unspecified 'DOCUMENT))
			       (body (html-parse
				      p
				      :content-length
				      cl
				      :procedure
				      (lambda (markup attr body)
					 (let* ((ia (assq 'id attr))
						(i (if (pair? ia)
						       (cdr ia)
						       (xml-make-id #unspecified markup)))
						(el (instantiate::xml-element
						       (markup markup)
						       (id i)
						       (attributes (filter! filter-attr attr))
						       (body body))))
					    (for-each (lambda (b)
							 (when (xml-element? b)
							    (xml-element-parent-set! b el)))
						      body)
					    (if (and (string? eid)
						     (string=? i eid))
						(return el)
						el))))))))
		    (and (not (string? eid)) res)))
	      (<DIV> "error"
		     (let ((po (open-output-string)))
			(send-chars p po cl)
			(close-output-port po))))))))
   
