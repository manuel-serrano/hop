;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/html-foldlist.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Wed Mar  1 11:23:29 2006                          */
;*    Last change :  Thu Mar 23 14:15:24 2006 (eg)                     */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of <FL>. 		                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_html-foldlist

   (include "compiler-macro.sch"
	    "xml.sch")

   (import  __hop_param
	    __hop_xml)

   (static  (class html-foldlist::xml-element
		   (spacing (default 0))
		   (icono (default #f))
		   (iconc (default #f)))
	    (class html-flitem::xml-element
		   (open (default #f)))
	    (class html-flhead::xml-element))

   (export  (<FOLD-ITEM> . ::obj)
	    (<FL> . ::obj)
	    (<FLITEM> . ::obj)
	    (<FLHEAD> . ::obj)))


;*---------------------------------------------------------------------*/
;*    <FOLD-ITEM> ...                                                  */
;*---------------------------------------------------------------------*/
(define-xml-compound <FOLD-ITEM> ((id #unspecified string)
				  (class #f)
				  (title #f)
				  (icono #f)
				  (iconc #f)
				  body)
  (let ((iditem (symbol->string (gensym "fi")))
	(open   (or icono (make-file-name (hop-icons-directory)
					  "triangle-down.png")))
	(closed (or iconc (make-file-name (hop-icons-directory)
					  "triangle-right.png")))
	(cls    (if class
		    (string-append "hop-fold-item " class)
		    "hop-fold-item")))
    (list
    (<UL> :class cls :onclick "alert('XX')"
	   (<LI> (<SPAN> :onclick (format "hop_fold_item_toggle(~s, ~s, ~s)"
					  iditem open closed)
			 title)
		 (<DIV> :onclick " alert('inside'); return true"
			:id iditem :style "display:none" body)))
     (<SCRIPT> :type "text/javascript"
	       (format "hop_fold_item_icon_set(~s, ~s)" iditem closed)))))

;*---------------------------------------------------------------------*/
;*    <FL> ...                                                         */
;*---------------------------------------------------------------------*/
(define-xml-compound <FL> ((id #unspecified string)
			   (spacing 0)
			   (icono #f)
			   (iconc #f)
			   body)
  (let ((res (instantiate::html-foldlist
	         (markup 'fl)
		 (id (xml-make-id id 'FL))
		 (spacing spacing)
		 (icono icono)
		 (iconc iconc)
		 (body body))))
    ;; Verify that the body is a list of <FLITEM>s and fill their parent
    (for-each (lambda (x)
		(if (and (xml-element? x)
			 (eq? (xml-element-markup x) 'flitem))
		    (html-flitem-parent-set! x res)
		    (error '<FL> "Component is not a <FLITEM>" x)))
	      body)
    res))

;;;
;;;    xml-write ::html-foldlist ...
;;;
(define-method (xml-write obj::html-foldlist p encoding)
  (fprintf p "<table class='hop-fl' id='~a' cellpadding='0' cellspacing='~a'>"
	   (html-foldlist-id obj) (html-foldlist-spacing obj))
  (xml-write (html-foldlist-body obj) p encoding)
  (display "</table>" p))


;*---------------------------------------------------------------------*/
;*    <FLITEM> ...                                                     */
;*---------------------------------------------------------------------*/
(define-xml-compound <FLITEM> ((id #unspecified string)
			       (open #f)
			       body)
  (instantiate::html-flitem
     (markup 'flitem)
     (id (xml-make-id id 'FLITEM))
     (open open)
     (body body)))

;;;
;;;    xml-write ::html-flitem ...
;;;
(define-method (xml-write obj::html-flitem p encoding)
  (with-access::html-flitem obj (body id parent open)
     (let ((tmp    body)
	   (icono (or (html-foldlist-icono parent)
		      (make-file-name (hop-icons-directory)
				       "triangle-down.png")))
	   (iconc (or (html-foldlist-iconc parent)
		      (make-file-name (hop-icons-directory)
				      "triangle-right.png"))))
       (fprintf p "<tr onclick='hop_fold_item_toggle(~s,~s,~s)'>" id icono iconc)
       (fprintf p "<td><img id=~s src=~s></td><td width='100%'>"
		(string-append id "-img") (if open icono iconc))       
       (when (and (pair? tmp)
		  (xml-element? (car tmp))
		  (eq? (xml-element-markup (car tmp)) 'flhead))
	 (xml-write (car tmp) p encoding)
	 (set! tmp (cdr tmp)))
       (fprintf p "</td></tr><tr><td></td><td><div id='~a' style='display:~a'>"
		id (if open "block" "none"))
       (xml-write tmp p encoding)
       (display "</div></td></tr>" p))))
  
;*---------------------------------------------------------------------*/
;*    <FLHEAD> ...                                                     */
;*---------------------------------------------------------------------*/
(define-xml-compound <FLHEAD> ((id #unspecified string) body)
  (instantiate::html-flhead
     (markup 'flhead)
     (id (xml-make-id id 'FLHEAD))
     (body body)))

;;;
;;;    xml-write ::html-flhead ...
;;;
(define-method (xml-write obj::html-flhead p encoding)
  (display "<span>" p)
  (xml-write (html-flhead-body obj) p encoding)
  (display "</span>" p))

