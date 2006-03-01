;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/html-folditem.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Wed Mar  1 11:23:29 2006                          */
;*    Last change :  Wed Mar  1 15:32:36 2006 (eg)                     */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of <FOLD-ITEM>.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_html-folditem

   (include "compiler-macro.sch"
	    "xml.sch")

   (import  __hop_param
	    __hop_xml)


   (export  (<FOLD-ITEM> . ::obj)))


;*---------------------------------------------------------------------*/
;*    <FOLD-ITEM> ...                                                  */
;*---------------------------------------------------------------------*/
(define-xml-compound <FOLD-ITEM> ((id #unspecified string)
				  (class #f)
				  (title #f)
				  (open-icon #f)
				  (closed-icon #f)
				  body)
  (let ((iditem (symbol->string (gensym "fi")))
	(open   (or open-icon
		    (make-file-name (hop-icons-directory)
				    "triangle-down.png")))
	(closed (or closed-icon
		    (make-file-name (hop-icons-directory)
				    "triangle-right.png")))
	(cls    (if class
		    (string-append "hop-fold-item " class)
		    "hop-fold-item")))
    (list
     (<UL> :class cls
	   (<LI> (<SPAN> :onclick (format "hop_fold_item_toggle(~s, ~s, ~s)"
					  iditem open closed)
			 title)
		 (<DIV> :id iditem :style "display:none" body)))
     (<SCRIPT> :type "text/javascript"
	       (format "hop_fold_item_icon_set(~s, ~s)" iditem closed)))))
