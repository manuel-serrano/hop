;*=====================================================================*/
;*    serrano/prgm/project/hop/1.10.x/runtime/hop-slider.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug 18 10:01:02 2005                          */
;*    Last change :  Mon Oct 13 19:48:41 2008 (serrano)                */
;*    Copyright   :  2005-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of sliders.                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-slider

   (library web)
   
   (include "xml.sch")

   (import  __hop_param
	    __hop_types
	    __hop_xml
	    __hop_misc
	    __hop_js-lib
	    __hop_service)

   (static  (class html-slider::xml-element
	       (klass read-only)
	       (value read-only)
	       (min read-only)
	       (max read-only)
	       (step read-only)
	       (onchange read-only)
	       (caption read-only)))

   (export  (<SLIDER> . ::obj)))

;*---------------------------------------------------------------------*/
;*    <SLIDER> ...                                                     */
;*---------------------------------------------------------------------*/
(define-xml-compound <SLIDER> ((id #unspecified string)
			       (class #unspecified string)
			       (value 0)
			       (min 0)
			       (max 100)
			       (step 1)
			       (onchange #f)
			       (caption "top"))
   (instantiate::html-slider
      (markup 'slider)
      (klass (if (string? class)
		 (string-append "hop-slider " class)
		 "hop-slider"))
      (id (xml-make-id id 'SLIDER))
      (value value)
      (min min)
      (max max)
      (step step)
      (onchange onchange)
      (caption caption)
      (body '())))

;*---------------------------------------------------------------------*/
;*    valueof ...                                                      */
;*---------------------------------------------------------------------*/
(define (valueof attr obj)
   (cond
      ((integer? obj)
       obj)
      ((xml-tilde? obj)
       (xml-tilde->expression obj))
      (else
       (error 'SLIDER (format "Illegal ~a" attr) obj))))
       
;*---------------------------------------------------------------------*/
;*    xml-write ::html-slider ...                                      */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-slider p backend)
   (with-access::html-slider obj (id klass value min max step onchange caption)
      (let ((gid (gensym))
	    (oc (cond
		   ((xml-tilde? onchange)
		    (xml-tilde->return onchange))
		   ((string? onchange)
		    onchange)
		   (else
		    ""))))
	 (fprintf p "<span id='~a'>" gid)
	 (display " <script type='" p)
	 (display (hop-javascript-mime-type) p)
	 (display "'>" p)
	 (fprint p
		 "hop_slider_onchange_set( "
		 "hop_make_slider( "
		 "document.getElementById( '" gid "' ), "
		 "'" klass "', "
		 "'" id "', ")
 	 (xml-write-expression min p)
	 (display "," p)
	 (xml-write-expression max p)
	 (display "," p)
	 (xml-write-expression step p)
	 (display "," p)
	 (xml-write-expression value p)
	 (display "," p)
	 (xml-write-expression caption p)
	 (fprint p"), function() { " oc " } )"))
      (display " </script>" p)
      (display "</span>" p)))

