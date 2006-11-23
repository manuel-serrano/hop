;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/hop-foldlist.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Wed Mar  1 11:23:29 2006                          */
;*    Last change :  Thu Nov 23 14:16:30 2006 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of <FL>. 		                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-foldlist

   (include "compiler-macro.sch"
	    "xml.sch")

   (import  __hop_param
	    __hop_xml
	    __hop_service
	    __hop_types
	    __hop_misc)

   (static  (class html-foldlist::xml-element
	       (cname::bstring read-only)
	       (stop::bool (default #t))
	       (spacing (default 0))
	       (icono (default #f))
	       (iconc (default #f)))
	    (class html-flitem::xml-element
	       (cname::bstring read-only)
	       (open (default #f)))
	    (class html-flhead::xml-element
	       (cname::bstring read-only)))

   (export  (<FL> . ::obj)
	    (<FLITEM> . ::obj)
	    (<FLHEAD> . ::obj)))

;*---------------------------------------------------------------------*/
;*    <FL> ...                                                         */
;*---------------------------------------------------------------------*/
(define-xml-compound <FL> ((id #unspecified string)
			   (class #unspecified string)
			   (spacing 0)
			   (icono #f)
			   (iconc #f)
			   body)
  (let ((res (instantiate::html-foldlist
	         (markup 'fl)
		 (cname (if (string? class)
			    (string-append "hop-fl " class)
			    "hop-fl"))
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
(define-method (xml-write obj::html-foldlist p encoding backend)
  (fprintf p "<table class='~a' id='~a' cellpadding='0' cellspacing='~a'>"
	   (html-foldlist-cname obj)
	   (html-foldlist-id obj) (html-foldlist-spacing obj))
  (xml-write (html-foldlist-body obj) p encoding backend)
  (display "</table>" p))


;*---------------------------------------------------------------------*/
;*    <FLITEM> ...                                                     */
;*---------------------------------------------------------------------*/
(define-xml-compound <FLITEM> ((id #unspecified string)
			       (class #unspecified string)
			       (open #f)
			       body)
  (instantiate::html-flitem
     (markup 'flitem)
     (cname (if (string? class)
		(string-append "hop-fl-item " class)
		"hop-fl-item"))
     (id (xml-make-id id 'FLITEM))
     (open open)
     (body body)))

;*---------------------------------------------------------------------*/
;*    xml-write ::html-flitem ...                                      */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-flitem p encoding backend)
   (with-access::html-flitem obj (body id parent open)
      (let ((tmp   body)
	    (icono (or (html-foldlist-icono parent)
		       (make-file-name (hop-icons-directory)
				       "triangle-down.png")))
	    (iconc (or (html-foldlist-iconc parent)
		       (make-file-name (hop-icons-directory)
				       "triangle-right.png"))))
	 (cond
	    ((not (pair? body))
	     (fprintf p "<tr onclick='hop_fold_item_toggle(~s,~s,~s) ~a'>"
		      id icono iconc
		      (if (html-foldlist-stop parent) ";hop_stop_propagation( event, true )" "")))
	    ((and (pair? body)
		  (xml-delay? (car body))
		  (null? (cdr body)))
	     (fprintf p
		      "<tr onclick='hop_fold_item_toggle_service(~s,~s,~s,~s) ~a'>"
		      id icono iconc
		      (hop-service-path
		       (procedure->service
			(xml-delay-thunk (car body))))
		      (if (html-foldlist-stop parent) ";hop_stop_propagation( event, true )" ""))
	     (set-car! body ""))
	    ((and (pair? body)
		  (pair? (cdr body))
		  (xml-delay? (cadr body))
		  (null? (cddr body)))
	     (fprintf p
		      "<tr onclick='hop_fold_item_toggle_service(~s,~s,~s,~s) ~a'>"
		      id icono iconc
		      (hop-service-path
		       (procedure->service
			(xml-delay-thunk (cadr body))))
		      (if (html-foldlist-stop parent) ";hop_stop_propagation( event, true )" ""))
	     (set-car! (cdr body) ""))
	    (else
	     (fprintf p "<tr onclick='hop_fold_item_toggle(~s,~s,~s) ~a'>"
		      id icono iconc
		      (if (html-foldlist-stop parent) ";hop_stop_propagation( event, true )" ""))))
	 (fprintf p "<td><img class='hop-fl-img' id=~s src=~s/></td><td width='100%' class='~a'>"
		  (string-append id "-img") (if open icono iconc)
		  (html-flitem-cname obj))
	 (when (and (pair? tmp)
		    (xml-element? (car tmp))
		    (eq? (xml-element-markup (car tmp)) 'flhead))
	    (xml-write (car tmp) p encoding backend)
	    (set! tmp (cdr tmp)))
	 (fprintf p "</td></tr><tr><td></td><td class='hop-fl-item-body'><div class='~a' id='~a' style='display:~a'>"
		  (html-flitem-cname obj) id (if open "block" "none"))
	 (xml-write tmp p encoding backend)
	 (display "</div></td></tr>" p))))
  
;*---------------------------------------------------------------------*/
;*    <FLHEAD> ...                                                     */
;*---------------------------------------------------------------------*/
(define-xml-compound <FLHEAD> ((id #unspecified string)
			       (class #unspecified string)
			       body)
  (instantiate::html-flhead
     (markup 'flhead)
     (cname (if (string? class)
		(string-append "hop-fl-head " class)
		"hop-fl-head"))
     (id (xml-make-id id 'FLHEAD))
     (body body)))

;;;
;;;    xml-write ::html-flhead ...
;;;
(define-method (xml-write obj::html-flhead p encoding backend)
  (display (format "<span class='~a'>" (html-flhead-cname obj)) p)
  (xml-write (html-flhead-body obj) p encoding backend)
  (display "</span>" p))

