;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/html-tabslider.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Thu Aug 18 10:01:02 2005                          */
;*    Last change :  Tue Mar  7 12:52:35 2006 (eg)                     */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of TABSLIDER.                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_html-tabslider

   (include "compiler-macro.sch"
	    "xml.sch")

   (import  __hop_param
	    __hop_types
	    __hop_xml
	    __hop_misc
	    __hop_js-lib
	    __hop_service)

   (static  (class html-tabslider::xml-element
	       (width (default #f))
	       (height (default #f))
	       (index (default 0)))
	    (class html-tspage::xml-element)
	    (class html-tshead::xml-element))

   (export  (<TABSLIDER> . ::obj)
	    (<TSPAN> . ::obj)
	    (<TSHEAD> . ::obj)))
   
;*---------------------------------------------------------------------*/
;*    <TABSLIDER> ...                                                  */
;*---------------------------------------------------------------------*/
(define-xml-compound <TABSLIDER> ((id #unspecified string)
				  (width #f)
				  (height #f)
				  (index 0)
				  body)
   ;; Verify that the body is a list of <TSPAN>
   (for-each (lambda (x)
		(unless (and (xml-element? x)
			     (eq? (xml-element-markup x) 'tspan))
		   (error '<TABSLIDER> "Component is not a <TSPAN>" x)))
	     body)
   
   (instantiate::html-tabslider
      (markup 'tabslider)
      (id (xml-make-id id 'TABSLIDER))
      (width width)
      (height height)
      (index index)
      (body body)))

;*---------------------------------------------------------------------*/
;*    xml-write ::html-tabslider ...                                   */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-tabslider p encoding)
  (with-access::html-tabslider obj (id width height body index)
     (fprintf p "<div class='hop-tabslider' id='~a'" id)
     (when (or width height)
       (fprintf p " style=\"~a~a\""
		(if width  (format "width: ~a;" width) "")
		(if height (format "height: ~a;" height) "")))
     (display ">" p)
     (xml-write body p encoding)
     (display "</div>" p)
     (fprintf p
        "<script type='text/javascript'>hop_tabslider_init('~a', ~a)</script>"
	id index)))

;*---------------------------------------------------------------------*/
;*    <TSPAN> ...                                                      */
;*---------------------------------------------------------------------*/
(define-xml-compound <TSPAN> ((id #unspecified string)
			      body)
   ;; Check that body is well formed
   (unless (and (pair? body) (>= (length body) 2))
      (error '<TSPAGE> "Illegal body, at least two elements needed" body))
   
   (instantiate::html-tspage
      (markup 'tspan)
      (id (xml-make-id id 'TSPAN))
      (body (list (car body)
		  (apply <DIV> :class "hop-tabslider-content" (cdr body))))))

;*---------------------------------------------------------------------*/
;*    xml-write ::html-tspan ...                                       */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-tspage p encoding)
  (xml-write (html-tspage-body obj) p encoding))

;*---------------------------------------------------------------------*/
;*    <TSHEAD> ...                                                     */
;*---------------------------------------------------------------------*/
(define-xml-alias <TSHEAD> <SPAN> :class "hop-tabslider-head")
