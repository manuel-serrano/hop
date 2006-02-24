;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/html-slider.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug 18 10:01:02 2005                          */
;*    Last change :  Thu Feb 23 02:38:02 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of sliders.                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_html-slider

   (include "compiler-macro.sch"
	    "xml.sch")

   (import  __hop_param
	    __hop_types
	    __hop_xml
	    __hop_misc
	    __hop_js-lib
	    __hop_service)

   (static  (class html-slider::xml-element
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
			       (value 0 integer)
			       (min 0 integer)
			       (max 100 integer)
			       (step 1 integer)
			       (onchange "")
			       (caption "top"))
   (instantiate::html-slider
      (markup 'slider)
      (id (xml-make-id id 'SLIDER))
      (value value)
      (min min)
      (max max)
      (step step)
      (onchange onchange)
      (caption caption)
      (body '())))
	    
;*---------------------------------------------------------------------*/
;*    xml-write ::html-slider ...                                      */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-slider p encoding)
   (let ((gid (gensym)))
      (fprintf p "<span id='~a'>" gid)
      (display " <script language='JavaScript'>" p)
      (with-access::html-slider obj (id value min max step onchange caption)
	 (fprint p
		 "hop_slider_onchange_set( "
		 "hop_make_slider( "
		 "document.getElementById( '" gid "' ), "
		 "'" id "', "
		 min ", " max ", " step ", "
		 value ", \"" caption "\" )"
		 ", function() { " onchange " } )"))
      (display " </script>" p)
      (display "</span>" p)))

