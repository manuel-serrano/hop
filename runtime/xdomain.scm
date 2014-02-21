;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/runtime/xdomain.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May  6 11:54:10 2011                          */
;*    Last change :  Fri Feb 21 13:48:40 2014 (serrano)                */
;*    Copyright   :  2011-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop xdomain requests                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_xdomain

   (include "service.sch")
   
   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_misc
	    __hop_xml-types
	    __hop_xml
	    __hop_html-base
	    __hop_html-head
	    __hop_cgi
	    __hop_service
	    __hop_js-comp
	    __hop_read
	    __hop_hop
	    __hop_user
	    __hop_http-error
	    __hop_security)
   
   (export (init-hop-xdomain-service!)))

;*---------------------------------------------------------------------*/
;*    *xdomain-svc* ...                                                */
;*---------------------------------------------------------------------*/
(define *xdomain-svc* #unspecified)

;*---------------------------------------------------------------------*/
;*    init-hop-xdomain-service! ...                                    */
;*---------------------------------------------------------------------*/
(define (init-hop-xdomain-service!)
   (set! *xdomain-svc*
      (service :name "public/xdomain" ()
	 (instantiate::http-response-xml
	    (backend (hop-xml-backend))
	    (request (current-request))
	    (charset (hop-locale))
	    (xml (<HTML>
		    (<HEAD>)
		    (<BODY>
		       (sexp->xml-tilde
			  `(add-event-listener! window "message"
			      (lambda (e)
				 ((@ hop_send_request_xdomain js) e)))))))))))
