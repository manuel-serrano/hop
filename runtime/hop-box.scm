;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/hop-box.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr  3 17:41:27 2009                          */
;*    Last change :  Fri Apr  3 18:40:24 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Server side implementation of HOP boxes.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-box

   (include "xml.sch")

   (import  __hop_param
	    __hop_types
	    __hop_xml
	    __hop_hop-extra
	    __hop_misc
	    __hop_js-lib
	    __hop_service)

   (export  (<BOX> . ::obj)))

;*---------------------------------------------------------------------*/
;*    <BOX>                                                            */
;*---------------------------------------------------------------------*/
(define-xml-compound <BOX> ((id #unspecified string)
			    (class "" string)
			    (orientation 'horizontal symbol)
			    (attrs)
			    body)
   (<DIV> :class (format "__HSS_box ~a" class) :id (xml-make-id id 'box)
      (<SPAN> :class "hop_box_layout" "")
      (<TABLE> :class "hop_box_container" (<TR> (<TD> body)))))

   
