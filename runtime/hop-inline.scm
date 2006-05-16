;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/hop-inline.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec 23 08:17:58 2005                          */
;*    Last change :  Fri May  5 16:55:47 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The implementation of the HOP inline markup.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-inline

   (library web)
   
   (include "compiler-macro.sch"
	    "xml.sch")

   (import  __hop_param
	    __hop_types
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
	  (let* ((req (the-current-request))
		 (auth (and (http-request? req)
			    (http-request-authorization req))))
	     (if early
		 (xml-inline host port path userinfo auth id)
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
(define-method (xml-write obj::xml-inline-element p encoding)
   (with-access::xml-inline-element obj (host port path userinfo authorization eid)
      (xml-write (xml-inline host port path userinfo authorization eid)
		 p encoding)))

;*---------------------------------------------------------------------*/
;*    xml-inline ...                                                   */
;*---------------------------------------------------------------------*/
(define (xml-inline host port path userinfo authorization eid)
   (http-send-request
    (instantiate::http-request
       (host host)
       (port port)
       (path path)
       (userinfo userinfo)
       (authorization authorization))
    (lambda (status clength p)
       (let ((cl (cond
		    ((elong? clength)
		     (elong->fixnum clength))
		    ((integer? clength)
		     clength)
		    (else
		     -1))))
	  (if (=fx status 200)
	      (bind-exit (return)
		 (instantiate::xml-document
		    (markup 'document)
		    (id (xml-make-id #unspecified 'DOCUMENT))
		    (body (html-parse
			   p cl
			   (lambda (markup attr body)
			      (let* ((ia (assq 'id attr))
				     (i (if (pair? ia)
					    (cdr ia)
					    (xml-make-id #unspecified markup)))
				     (el (instantiate::xml-element
					    (markup markup)
					    (id i)
					    (attributes attr)
					    (body body))))
				 (for-each (lambda (b)
					      (when (xml-element? b)
						 (xml-element-parent-set! b el)))
					   body)
				 (if (and (string? eid) (string=? i eid))
				     (return el)
				     el)))))))
	      (<DIV> "error"
		     (let ((po (open-output-string)))
			(send-chars p po cl)
			(close-output-port po))))))))
   
