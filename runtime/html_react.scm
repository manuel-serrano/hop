;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/runtime/html_react.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 28 18:01:20 2016                          */
;*    Last change :  Fri Apr 29 17:15:01 2016 (serrano)                */
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
	       
	 
   


