;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/html-edit.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Sat Apr  8 13:15:13 2006                          */
;*    Last change :  Mon Apr 10 22:59:15 2006 (eg)                     */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of <EDITOR>.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_html-editor

   (include "compiler-macro.sch"
	    "xml.sch")

   (import  __hop_param
	    __hop_xml)

   (export  (<EDITOR> . ::obj)))


(define *icons-dir* "/mnt/users/eg/Trash/editor-icons")


(define (make-toolbar cls width)
  (define (<SPACER>)
    (<TD> :width "6px"))

  (define (<SEPARATOR>)
    (<TD> :width "12px"  :align "center"
      (<IMG> :src (make-file-path *icons-dir* "separator.gif") :border 0)))

  (define (<FILER>)
    (<TD> "&nbsp;"))
  
  (define (<ICON> name tooltip image)
    (let ((icon1 (make-file-path *icons-dir* (string-append image ".gif")))
	  (icon2 (make-file-path *icons-dir* (string-append image "_on.gif")))
	  (cls1  "hop-edit-button")
	  (cls2  "hop-edit-button-over"))
      (<TD> :style "width: 22px"
	(<IMG> :src icon1 :class cls1 :border 0 :width "20px" :height "20px"
	       :title tooltip
	       :onmouseover (format "this.src= ~s; this.className=~s" icon2 cls2)
	       :onmouseout  (format "this.src= ~s; this.className=~s" icon1 cls1)))))

  (let ((bg (make-file-path *icons-dir* "background_silver.jpg")))
    (<DIV>
     (<TABLE> :cellpadding 0 :cellspacing 0 :border 0 :class "hop-edit-toolbars"
	      :width width
	 (<TR> :style (format "height: 26px; background-image: url(~s);" bg)
	  (<SPACER>)
	  (<ICON> "bold" "Bold" "bold")
	  (<ICON> "italic" "Italic" "italic")
	  (<ICON> "underline" "Underline" "underline")
	  (<ICON> "strikethrough" "Strike Through" "strikethrough")
	  (<SEPARATOR>)
	  (<ICON> "subscript" "Subscript" "subscript")
	  (<ICON> "superscript" "Superscript" "superscript")
	  (<SEPARATOR>)
	  (<ICON> "justifyleft"   "Left alignment" 	"justify_left")
	  (<ICON> "justifycenter" "Center alignment" 	"justify_center")
	  (<ICON> "justifyright"  "Right alignment"	"justify_right")
	  (<ICON> "justify"       "Justified"		"justify")
	  (<SEPARATOR>)
	  (<ICON> "unorderedlist" "Bullet" "list_unordered")
	  (<ICON> "orderedlist" "Numbering" "list_ordered")
	  (<SEPARATOR>)
	  (<ICON> "indent" "Increase indent" "indent_left")
	  (<ICON> "outdent" "Decrease indent" "indent_right")
	  (<FILER>))
	 (<TR> :style (format "height: 26px; background-image: url(~s);" bg)
	  (<SPACER>)
	  (<ICON> "forecolor" "Foreground" "forecolor")
	  (<ICON> "backcolor" "Background" "backcolor")
	  (<SEPARATOR>)
	  (<ICON> "cut" "Cut" "cut")
	  (<ICON> "copy" "Copy" "copy")
	  (<ICON> "paste" "Paste" "paste")
	  (<SEPARATOR>)
	  (<ICON> "undo" "Undo" "undo")
	  (<ICON> "redo" "Redo" "redo")
	  (<SEPARATOR>)
	  (<ICON> "inserttable" "Insert table" "insert_table")
	  (<ICON> "insertimage" "Insert image" "insert_picture")
	  (<ICON> "insertlink"  "Insert link" "insert_hyperlink")
	  (<SEPARATOR>)
	  (<ICON> "viewsource"  "Alternate view" "view_source")
	  (<SEPARATOR>)
	  (<ICON> "help" "Help" "help")
	  (<FILER>)  ;; Why do we need 2 of them here?
	  (<FILER>))))))

;*---------------------------------------------------------------------*/
;*    <EDITOR> ...                                                     */
;*---------------------------------------------------------------------*/
(define-xml-compound <EDITOR> ((id #unspecified string)
			       (class #f)
			       (width "100%")
			       (height "600px")
			       (name "")
			       body)
  (<DIV> :style "height:300; background: Ivory"
     (make-toolbar class width)))
		       