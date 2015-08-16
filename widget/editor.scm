;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/widget/editor.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Sat Apr  8 13:15:13 2006                          */
;*    Last change :  Sat Jun 20 07:51:58 2009 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of <EDITOR>.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwidget-editor

   (library hop)
   
   (export  (<EDITOR> . ::obj)))

;*---------------------------------------------------------------------*/
;*    Global parameters                                                */
;*---------------------------------------------------------------------*/
(define *icons-dir*
   (make-file-name (hop-share-directory) "editor"))
(define *popups-dir*
   *icons-dir*)

(define *styles*
   '(("Title 1" "h1") ("Title 2" "h2") ("Title 3" "h3")
     ("Title 4" "h4") ("Title 5" "h5") ("Title 6" "h6")
     ("Address" "address") ("Paragraph" "p")))

(define *fonts*
   '("Sans Serif" "Serif" "Monospace"
     "Arial" "Tahoma" "Verdana" "Courier New" "Georgia"
     "Times New Roman" "Impact" "Comic Sans MS"))

;*---------------------------------------------------------------------*/
;*    make-palette ...                                                 */
;*---------------------------------------------------------------------*/
(define (make-palette id)
   ;; Web safe palette (a variant of the 216 colors palette reduced to 27 ..)
   (let loop ((r #x33) (g #x33) (b #x33)
		       (res '()))
      (cond
	 ((> b #xff)
	  (loop r (+ g #x66) #x33 res))
	 ((> g #xff)
	  (loop (+ r #x66) #x33 #x33 res))
	 ((> r #xff)
	  (reverse res))
	 (else
	  (let ((col (format "rgb(~a,~a,~a)" r g b)))
	     (loop r g (+ b #x66)
		   (cons
		    (<DIV> :class "hop-edit-palette-item"
		       :style (string-append "background:" col)
		       :onclick (format "hop_edit_colorize(~s,~s)" id col))
		    res)))))))

;*---------------------------------------------------------------------*/
;*    make-toolbar ...                                                 */
;*---------------------------------------------------------------------*/
(define (make-toolbar id width)
   
   (define (<SPACER>)
      (<TD> :width "6px"))
   
   (define (<SEPARATOR>)
      (<TD> :width "12px"  :align "center"
	 (<IMG> :src (make-file-name *icons-dir* "ed-separator.png")
	    :border 0)))
   
   (define (<FILER>)
      (<TD> "&#160;"))
   
   (define (<ICON> name tooltip image)
      (let ((icon (make-file-name *icons-dir* (string-append image ".png"))))
	 (<TD> :style "width: 22px"
	    (<IMG> :src icon
	       :class "hop-edit-button"
	       :id (format "~a-but-~a" id name)
	       :border 0  :title tooltip
	       :onclick (format "hop_edit_action(~s, ~s)" id name)
	       :onmouseover "if (!hop_edit_src) className='hop-edit-button-over'"
	       :onmouseout  "if (!hop_edit_src) className='hop-edit-button'"))))
   
   
   (define (<STYLES>)
      (<TD> :width 50
	 (<SELECT> :id (string-append "hop-edit-styles-" id)
	    (map (lambda (x)
		    (<OPTION> :onclick (format "hop_edit_style_set(~s,~s)"
					       id (cadr x))
		       (car x)))
		 *styles*))))
   
   (define (<FONTS>)
      (<TD> :width 65
	 (<SELECT> :id (string-append "hop-edit-fonts-" id)
	    (map (lambda (x)
		    (<OPTION> :onclick (format "hop_edit_font_set(~s,~s)" id x) x))
		 *fonts*))))
   
   (define (<FONT-SIZES>)
      (<TD> :width 20
	 (<SELECT> :id (string-append "hop-edit-fs-" id)
	    (map (lambda (x)
		    (<OPTION> :onclick (format "hop_edit_fontsize_set(~s,~s)" id x) x))
		 '(1 2 3 4 5 6 7)))))
   
   (define (<PALETTE>)
      (let ((palette (string-append id "-palette")))
	 (<DIV> :class "hop-edit-palette" :id palette
	    :onclick "this.style.display = 'none'"
	    (<DIV> (make-palette id)))))
   
   (let ((bg (make-file-name *icons-dir* "ed-silver-bg.png")))
      (<DIV> :id (string-append id "-toolbars")
	 ;; The (not yet visible) color palette
	 (<PALETTE>)
	 ;; First line of the toolbar
	 (<TABLE> :cellpadding 0 :cellspacing 0 :class "hop-edit-toolbars"
	    :width width :style "border-bottom: 0px"
	    (<TR> :style (format "height: 26px; background-image: url(~s);" bg)
	       (<SPACER>)
	       (<ICON> "submit" "Submit" "ed-page-go")
	       (<ICON> "cancel" "Cancel" "ed-page")
	       (<SEPARATOR>)
	       (<ICON> "bold" "Bold" "ed-bold")
	       (<ICON> "italic" "Italic" "ed-italic")
	       (<ICON> "underline" "Underline" "ed-underline")
	       (<ICON> "strikethrough" "Strike Through" "ed-strikethrough")
	       (<SEPARATOR>)
	       (<ICON> "subscript" "Subscript" "ed-subscript")
	       (<ICON> "superscript" "Superscript" "ed-superscript")
	       (<SEPARATOR>)
	       (<ICON> "inserthorizontalrule" "Intert horizontal line" "ed-hrule")
	       (<SEPARATOR>)
	       (<ICON> "justifyleft" "Left alignment" "ed-justify-left")
	       (<ICON> "justifycenter" "Center alignment" "ed-justify-center")
	       (<ICON> "justifyright" "Right alignment" "ed-justify-right")
	       (<ICON> "justifyfull" "Justified" "ed-justify")
	       (<SEPARATOR>)
	       (<ICON> "insertunorderedlist" "Bullet" "ed-list-unordered")
	       (<ICON> "insertorderedlist" "Numbering" "ed-list-ordered")
	       (<SEPARATOR>)
	       (<ICON> "undo" "Undo" "ed-undo")
	       (<ICON> "redo" "Redo" "ed-redo")
	       (<SEPARATOR>)
	       (<ICON> "indent" "Increase indent" "ed-indent")
	       (<ICON> "outdent" "Decrease indent" "ed-unindent")
	       (<FILER>)))
	 
	 ;; Second line of the toolbar
	 (<TABLE> :cellpadding 0 :cellspacing 0 :width width 
	    :class "hop-edit-toolbars"
	    (<TR> :style (format "height: 26px; background-image: url(~s);" bg)
	       (<SPACER>)
	       (<ICON> "viewsource" "Alternate view" "ed-html")
	       (<SEPARATOR>)
	       (<STYLES>) (<SPACER>)
	       (<FONTS>) (<SPACER>)
	       (<FONT-SIZES>) (<SPACER>)
	       (<ICON> "forecolor" "Foreground" "ed-foreground")
	       (<ICON> "hilitecolor" "Background" "ed-background")
	       (<SEPARATOR>)
	       (<ICON> "insertlink"  "Insert link" "ed-link")
	       (<ICON> "unlink"	"Delete link" "ed-unlink")
	       (<SEPARATOR>)
	       (<ICON> "insertimage" "Insert image" "ed-image")
	       (<ICON> "inserttable" "Insert table" "ed-table")
	       (<FILER>))))))

;*---------------------------------------------------------------------*/
;*    <EDITOR> ...                                                     */
;*---------------------------------------------------------------------*/
(define-xml-compound <EDITOR> ((id #unspecified string)
			       (class #f)
			       (width "100%")
			       (height "600px")
			       (name "")
			       (onsubmit #f)
			       (oncancel #f)
			       body)
   (let ((id (xml-make-id id 'EDITOR)))
      (<DIV> :style (format "width: ~a" width)
	 :class "hop-editor"
	 ;; Create the toolbar
	 (make-toolbar id width)
	 ;; Create the (invisible) text area which can be used for forms
	 (<TEXTAREA> :style
	    (format "display: none; width: ~a; height: ~a; border:0;"
		    width height)
	    :id id :name name body)
	 ;; Create an iframe for rich text editing
	 (<IFRAME> :id (string-append "hop-edit" id) :frameborder 0
	    :onmouseover "hop_edit_in_iframe = true"
	    :onmouseout  "hop_edit_in_iframe = false"
	    :width width :height height)
	 (<SCRIPT> (format "hop_edit_init(~s, ~s, ~a, ~a)" id *popups-dir*
			   (hop->js-callback onsubmit)
			   (hop->js-callback oncancel))))))
