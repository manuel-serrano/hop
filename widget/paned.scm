;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/widget/paned.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug 18 10:01:02 2005                          */
;*    Last change :  Mon Feb  8 08:55:31 2010 (serrano)                */
;*    Copyright   :  2005-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of paned.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwidget-paned

   (library hop)

   (static  (class html-paned::xml-element
	       (klass read-only)
	       (fraction read-only)
	       (style read-only (default #f))
	       (onresize read-only)
	       (orientation::symbol read-only))

	    (class html-pan::xml-element
	       (klass read-only)))

   (export  (<PANED> . ::obj)
	    (<PAN> . ::obj)))
   
;*---------------------------------------------------------------------*/
;*    <PANED> ...                                                      */
;*    -------------------------------------------------------------    */
;*    See __hop_css for HSS types.                                     */
;*---------------------------------------------------------------------*/
(define-markup <PANED> ((id #unspecified string)
			(class #unspecified string)
			(fraction 30)
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
	  (onresize onresize)
	  (style style)
	  (orientation orientation)
	  (body body)))
      (else
       (error '<PANED> "Illegal body, too many pan" body))))

;*---------------------------------------------------------------------*/
;*    <PAN> ...                                                        */
;*---------------------------------------------------------------------*/
(define-markup <PAN> ((id #unspecified string)
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
;*    xml-write ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-paned p backend)
   (with-access::html-paned obj (id klass fraction onresize style body orientation)
      (fprintf p "<div hssclass='hop-paned' id='~a'" id)
      (when (string? klass) (fprintf p " class='~a'" klass))
      (when style (fprintf p " style='~a'" style))
      (display ">" p)
      (fprintf p "<div class='hop-paned-inner hop-paned-~a' id='~a-inner'>"
	       orientation id)
      (fprintf p "<div class='hop-paned-pan hop-paned-el1' id='~a-el1'>" id)
      (xml-write (car body) p backend)
      (fprint p "</div>")
      (fprintf p "<div class='hop-paned-cursor' id='~a-cursor'></div>" id)
      (fprintf p "<div class='hop-paned-pan hop-paned-el2' id='~a-el2'>" id)
      (xml-write (cadr body) p backend)
      (fprint p "</div>")
      (fprint p "</div>")
      (display " <script type='" p)
      (display (hop-javascript-mime-type) p)
      (display "'>" p)
      (fprintf p
	       "hop_init_paned_~a( ~s, ~a, function(event) { ~a } )"
	       orientation
	       id
	       (if (string? fraction)
		   (string-append "\"" fraction "\"")
		   fraction)
	       (cond
		  ((xml-tilde? onresize)
		   (xml-tilde->return onresize))
		  ((string? onresize)
		   onresize)
		  (else
		   "")))
      (display " </script></div>" p)))

;*---------------------------------------------------------------------*/
;*    xml-write ::html-pan ...                                         */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-pan p backend)
   (with-access::html-pan obj (id klass body attributes)
      (display "<div id='" p)
      (display id p)
      (display "' class='hop-pan" p)
      (when (string? klass)
	 (display " " p)
	 (display klass p))
      (display "' " p)
      (xml-write-attributes attributes p backend)
      (display ">" p)
      (xml-write body p backend)
      (display "</div>" p)))
