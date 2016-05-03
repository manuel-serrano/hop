;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/runtime/html_react.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 28 18:01:20 2016                          */
;*    Last change :  Thu May  5 06:12:28 2016 (serrano)                */
;*    Copyright   :  2016 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Dynamic nodes                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_html-react

   (library web)

   (include "param.sch"
	    "xml.sch")

   (import  __hop_types
	    __hop_mime
	    __hop_misc
	    __hop_priv
	    __hop_param
	    __hop_configure
	    __hop_charset
	    __hop_xml-types
	    __hop_xml
	    __hop_dom
	    __hop_hop
	    __hop_cache
	    __hop_user
	    __hop_security)

   (export  (<REACT> . ::obj)))

;*---------------------------------------------------------------------*/
;*    REACT ...                                                        */
;*---------------------------------------------------------------------*/
(define-tag <REACT> ((id #f)
		     (inline #f boolean)
		     (alt #f)
		     (src #unspecified)
		     (attributes)
		     body)
   (let ((id (xml-make-id)))
      (instantiate::xml-react
	 (tag 'react)
	 (body `(lambda ()
		   ,@(filter-map (lambda (node)
				    (when (isa? node xml-tilde)
				       (xml-tilde->sexp node)))
			body))))))
	       
;*---------------------------------------------------------------------*/
;*    xml-write ::xml-react ...                                        */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-react p backend)
   
   (define (react-init key parent thunk)
      (if (not parent)
	  (error "react" "illegal orphan react node" obj)
	  (with-access::xml-element parent ((pid id))
	     (cond
		((dom-next-sibling obj)
		 =>
		 (lambda (sibling)
		    (with-access::xml-element sibling ((sid id))
		       `((pragma "window.hop.reactInit") ,pid ,sid ,key ,thunk))))
		(else
		 `((pragma "window.hop.reactInit") ,pid ,#f ,key ,thunk))))))

   (with-access::xml-react obj (id body parent)
      (let ((expr (react-init id parent body)))
	 (xml-write (sexp->xml-tilde expr) p backend))))
   
   
