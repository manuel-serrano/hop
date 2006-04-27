;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/hop-paned.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug 18 10:01:02 2005                          */
;*    Last change :  Thu Apr 27 08:11:40 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of paned.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-paned

   (include "compiler-macro.sch"
	    "xml.sch")

   (import  __hop_param
	    __hop_types
	    __hop_xml
	    __hop_misc
	    __hop_js-lib
	    __hop_service)

   (static  (class html-paned::xml-element
	       (fraction read-only)
	       (style read-only (default #f))
	       (height read-only (default #f))
	       (orientation read-only (default 'horizontal))
	       (onresize read-only))

	    (class html-pan::xml-element))

   (export  (<PANED> . ::obj)
	    (<PAN> . ::obj)))
   
;*---------------------------------------------------------------------*/
;*    <PANED> ...                                                      */
;*---------------------------------------------------------------------*/
(define-xml-compound <PANED> ((id #unspecified string)
			      (fraction 0)
			      (onresize "")
			      (orientation 'vertical)
			      (style #f)
			      body)
   (cond
      ((null? body)
       (error '<PANED> "Illegal body, missing two pans" body))
      ((null? (cdr body))
       (error '<PANED> "Illegal body, missing one pan" body))
      ((not (memq orientation '(horizontal vertical)))
       (error '<PANED> "Illegal orientation" orientation))
      ((null? (cddr body))
       (instantiate::html-paned
	  (markup 'paned)
	  (id (xml-make-id id 'PANED))
	  (fraction fraction)
	  (orientation orientation)
	  (onresize onresize)
	  (style style)
	  (body body)))))

;*---------------------------------------------------------------------*/
;*    <PAN> ...                                                        */
;*---------------------------------------------------------------------*/
(define-xml-compound <PAN> ((id #unspecified string)
			    (attr)
			    body)
   (instantiate::html-pan
      (markup 'PAN)
      (id (xml-make-id id 'PAN))
      (attributes attr)
      (body body)))

;*---------------------------------------------------------------------*/
;*    xml-write ::html-paned ...                                       */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-paned p encoding)
   (let ((gid (symbol->string (gensym))))
      (with-access::html-paned obj (id fraction onresize body orientation style)
	 (fprintf p "<div class='hop-paned' id='~a'" gid)
	 (when style (fprintf p " style='~a'" style))
	 (display ">" p)
	 (xml-write (car body) p encoding)
	 (xml-write (cadr body) p encoding)
	 (display " <script language='JavaScript'>" p)
	 (fprint p
		 "hop_paned_onresize_set( "
		 (if (eq? orientation 'vertical)
		     "hop_make_vpaned( "
		     "hop_make_hpaned( ")
		 "document.getElementById( '" gid "' ),"
		 "'" id "', "
		 fraction ", "
		 "document.getElementById( '" (html-pan-id (car body)) "' ),"
		 "document.getElementById( '" (html-pan-id (cadr body)) "' ) )"
		 ", function() { " onresize " } )"))
      (display " </script>" p)
      (display "</span>" p)))
   
;*---------------------------------------------------------------------*/
;*    xml-write ::html-pan ...                                         */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-pan p encoding)
   (with-access::html-pan obj (id body)
      (fprintf p "<div id='~a' class='hop-pan' style='visibility: hidden'>" id)
      (xml-write body p encoding)
      (display "</div>" p)))
