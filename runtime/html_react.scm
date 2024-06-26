;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/runtime/html_react.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 28 18:01:20 2016                          */
;*    Last change :  Tue May 14 12:49:45 2024 (serrano)                */
;*    Copyright   :  2016-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Dynamic nodes                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_html-react

   (library web http)

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
		     (%context #f)
		     (attributes)
		     body)
   (let ((nodes (filter-map (lambda (node)
			       (when (isa? node xml-tilde)
				  (xml-tilde->sexp node)))
		   body)))
      (if (null? nodes)
	  (error "<REACT>" "Illegal empty react node" body)
	  (let ((id (xml-make-id)))
	     (instantiate::xml-react
		(tag 'react)
		(id id)
		(%context %context)
		(body `(lambda () ,@nodes)))))))
	       
;*---------------------------------------------------------------------*/
;*    xml-write ::xml-react ...                                        */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-react p backend)
   
   (define (parent-id parent)
      (when (isa? parent xml-element)
	 (with-access::xml-element parent (id)
	    id)))
   
   (define (react-init key parent thunk)
      (if (not parent)
	  (error "react" "illegal orphan react node" obj)
	  (cond
	     ((dom-next-sibling obj)
	      =>
	      (lambda (sibling)
		 (cond
		    ((isa? sibling xml-element)
		     (with-access::xml-element sibling ((sid id))
			`((pragma "window.hop.reactNode")
			  ,thunk ,(parent-id parent) ,sid #f ,key)))
		    ((string? sibling)
		     `((pragma "window.hop.reactNode")
		       ,thunk ,(parent-id parent) ,#f ,(url-path-encode sibling) ,key))
		    (else
		     `((pragma "window.hop.reactNode")
		       ,thunk ,(parent-id parent) ,#f , #f ,key)))))
	     (else
	      `((pragma "window.hop.reactNode")
		,thunk ,(parent-id parent) ,#f , #f ,key)))))
   
   (with-access::xml-react obj (id body parent %context)
      (let ((expr (react-init id parent body)))
	 (xml-write (sexp->xml-tilde expr :%context %context) p backend))))
   
   
