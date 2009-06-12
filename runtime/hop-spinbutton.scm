;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/hop-spinbutton.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 11 18:49:09 2009                          */
;*    Last change :  Fri Jun 12 19:24:35 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Spin Buttons server side implementation                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-spinbutton

   (include "xml.sch")

   (import  __hop_param
	    __hop_types
	    __hop_xml
	    __hop_hop-extra
	    __hop_misc
	    __hop_js-lib
	    __hop_service
	    __hop_js-lib)

   (export  (<SPINBUTTON> . ::obj)))

;*---------------------------------------------------------------------*/
;*    <SPINBUTTON> ...                                                 */
;*---------------------------------------------------------------------*/
(define-xml-compound <SPINBUTTON> ((id #unspecified string)
				   (min 0 integer)
				   (max 100 integer)
				   (value 0 integer)
				   (onchange #f)
				   (attrs)
				   body)
   
   (define (input-width)
      (let loop ((v ((@ maxfx __r4_numbers_6_5_fixnum) (abs min) (abs max)))
		 (w 0))
	 (if (>fx v 0)
	     (loop (/fx v 10) (+fx w 1))
	     (if (or (<fx min 0) (<fx max 0))
		 (+fx 1 w)
		 w))))
   
   (if (pair? body)
       (error '<SPINBUTTON> "arguments ignored" body)
       (let* ((id (xml-make-id id 'SPINBUTTON))
	      (w (input-width))
	      (body (list
		     (<TR>
			(<TD> :class "hop-spinbutton-value" :rowspan 2
			   (<INPUT> :class "hop-spinbutton-entry"
			      :id (string-append id "-entry")
			      :type 'text
			      :style (format "width: ~aem" w)
			      :onchange (format "hop_spinbutton_set(~s,this.value)" id)
			      :value (integer->string value)))
			(<TD> :class "hop-spinbutton-button hop-spinbutton-button-top"
			   :onmousedown (format "hop_spinbutton_inc(~s)" id)
			   (<DIV> "&#9650;")))
		     (<TR> 
			(<TD> :class "hop-spinbutton-button hop-spinbutton-button-bottom"
			   :onmousedown (format "hop_spinbutton_dec(~s)" id)
			   (<DIV> "&#9660;")))))
	      (init `(:value ,value
		      :minvalue ,min
		      :maxvalue ,max)))
	  (instantiate::xml-element
	     (markup 'table)
	     (id id)
	     (attributes `(:cellspacing  "0"
			   :cellpadding "0"
			   :hssclass "hop-spinbutton"
 			   :onchange ,onchange
			   ,@attrs))
	     (initializations init)
	     (body body)))))
