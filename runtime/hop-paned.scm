;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/hop-paned.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug 18 10:01:02 2005                          */
;*    Last change :  Fri Jun  8 08:04:10 2007 (serrano)                */
;*    Copyright   :  2005-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of paned.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-paned

   (include "xml.sch")

   (import  __hop_param
	    __hop_types
	    __hop_xml
	    __hop_misc
	    __hop_js-lib
	    __hop_service)

   (static  (class html-paned::xml-element
	       (klass read-only)
	       (fraction read-only)
	       (style read-only (default #f))
	       (height read-only (default #f))
	       (orientation read-only (default 'vertical))
	       (onresize read-only))

	    (class html-pan::xml-element
	       (klass read-only)))

   (export  (<PANED> . ::obj)
	    (<PAN> . ::obj)))
   
;*---------------------------------------------------------------------*/
;*    <PANED> ...                                                      */
;*---------------------------------------------------------------------*/
(define-xml-compound <PANED> ((id #unspecified string)
			      (class #unspecified string)
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
	  (klass class)
	  (fraction fraction)
	  (orientation orientation)
	  (onresize onresize)
	  (style style)
	  (body body)))))

;*---------------------------------------------------------------------*/
;*    <PAN> ...                                                        */
;*---------------------------------------------------------------------*/
(define-xml-compound <PAN> ((id #unspecified string)
			    (class #unspecified string)
			    (attr)
			    body)
   (instantiate::html-pan
      (markup 'PAN)
      (id (xml-make-id id 'PAN))
      (klass class)
      (attributes attr)
      (body body)))

;*---------------------------------------------------------------------*/
;*    xml-write ::html-paned ...                                       */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-paned p encoding backend)
   (let ((gid (symbol->string (gensym))))
      (with-access::html-paned obj (id klass fraction onresize body orientation style)
	 (let ((cl (if (string? klass)
		       (string-append "hop-paned " klass)
		       "hop-paned")))
	    (fprintf p "<div class='~a' id='~a'" cl gid))
	 (when style (fprintf p " style='~a'" style))
	 (display ">" p)
	 (xml-write (car body) p encoding backend)
	 (xml-write (cadr body) p encoding backend)
	 (display " <script type='" p)
	 (display (hop-javascript-mime-type) p)
	 (display "'>" p)
	 (fprint p
		 "hop_paned_onresize_set( "
		 (if (eq? orientation 'vertical)
		     "hop_make_vpaned( "
		     "hop_make_hpaned( ")
		 "document.getElementById( '" gid "' ),"
		 "'" id "', '"
		 (if (eq? orientation 'vertical) "hop-vpaned" "hop-hpaned")
		 "', ")
	 (if (string? fraction)
	     (fprint p "\"" fraction "\"")
	     (display fraction p))
	 (fprint p 
		 ", "
		 "document.getElementById( '" (html-pan-id (car body)) "' ),"
		 "document.getElementById( '" (html-pan-id (cadr body)) "' ) )"
		 ", function() { "
		 (cond
		    ((xml-tilde? onresize)
		     (tilde->string onresize))
		    ((string? onresize)
		     onresize)
		    (else
		     ""))
		 " } )"))
      (display " </script>" p)
      (display "</span>" p)))
   
;*---------------------------------------------------------------------*/
;*    xml-write ::html-pan ...                                         */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-pan p encoding backend)
   (with-access::html-pan obj (id klass body)
      (display "<div id='" p)
      (display id p)
      (display "' class='hop-pan" p)
      (when (string? klass)
	 (display " " p)
	 (display klass p))
      (display "' style='visibility: hidden'>" p)
      (xml-write body p encoding backend)
      (display "</div>" p)))
