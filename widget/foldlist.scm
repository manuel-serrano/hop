;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/widget/foldlist.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Wed Mar  1 11:23:29 2006                          */
;*    Last change :  Sat Nov 12 07:24:35 2011 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of <FL> markup.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwidget-foldlist

   (library hop)
   
   (static  (class html-foldlist::xml-element
	       (cname::bstring read-only)
	       (stop::bool (default #t))
	       (spacing (default 0))
	       (icono (default #f))
	       (iconc (default #f))
	       (history read-only (default #t)))
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
(define-tag <FL> ((id #unspecified string)
		  (class #unspecified string)
		  (spacing 0)
		  (icono #f)
		  (iconc #f)
		  (history #unspecified)
		  body)
   (let ((res (instantiate::html-foldlist
	         (tag 'fl)
		 (cname (if (string? class)
			    (string-append "hop-fl " class)
			    "hop-fl"))
		 (id (xml-make-id id 'FL))
		 (spacing spacing)
		 (icono icono)
		 (iconc iconc)
		 (history (if (boolean? history)
			      history
			      (not (eq? id #unspecified))))
		 (body body))))
      ;; Verify that the body is a list of <FLITEM>s and fill their parent
      (for-each (lambda (x)
		   (if (and (isa? x xml-element)
			    (with-access::xml-element x (tag)
			       (eq? tag 'flitem)))
		       (with-access::html-flitem x (parent)
			  (set! parent res))
		       (error "<FL>" "Component is not a <FLITEM>" x)))
		body)
      res))

;*---------------------------------------------------------------------*/
;*    xml-write ::html-foldlist ...                                    */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-foldlist p backend)
   (with-access::html-foldlist obj (cname id spacing body)
      (fprintf p "<table class='~a' id='~a' cellpadding='0' cellspacing='~a'>"
	 cname id spacing)
      (xml-write body p backend)
      (display "</table>" p)))

;*---------------------------------------------------------------------*/
;*    <FLITEM> ...                                                     */
;*---------------------------------------------------------------------*/
(define-tag <FLITEM> ((id #unspecified string)
		      (class #unspecified string)
		      (open #f)
		      body)
   (instantiate::html-flitem
      (tag 'flitem)
      (cname (if (string? class)
		 (string-append "hop-fl-item " class)
		 "hop-fl-item"))
      (id (xml-make-id id 'FLITEM))
      (open open)
      (body body)))

;*---------------------------------------------------------------------*/
;*    xml-write ::html-flitem ...                                      */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-flitem p backend)
   (with-access::html-flitem obj (body id parent open)
      (let ((tmp   body)
	    (icono (or (with-access::html-foldlist parent (icono) icono)
		       (make-file-path (hop-icons-directory)
				       "hop-foldlist"
				       "triangle-down.png")))
	    (iconc (or (with-access::html-foldlist parent (iconc) iconc)
		       (make-file-path (hop-icons-directory)
				       "hop-foldlist"
				       "triangle-right.png")))
	    (history (if (with-access::html-foldlist parent (history) history)
			 "true" "false")))
	 (cond
	    ((not (pair? body))
	     (fprintf p "<tr onclick='hop_fold_item_toggle(~s,~a) ~a'>"
		      id history
		      (with-access::html-foldlist parent (stop)
			 (if stop ";hop_stop_propagation( event, true )" ""))))
	    ((and (pair? body)
		  (isa? (car body) xml-delay)
		  (null? (cdr body)))
	     (fprintf p
		      "<tr onclick='hop_fold_item_toggle_service(~s,~a,~s) ~a'>"
		      id history
		      (with-access::xml-delay (car body) (thunk)
			 ((procedure->service thunk)))
		      (with-access::html-foldlist parent (stop)
			 (if stop ";hop_stop_propagation( event, true )" "")))
	     (set-car! body ""))
	    ((and (pair? body)
		  (pair? (cdr body))
		  (isa? (cadr body) xml-delay)
		  (null? (cddr body)))
	     (fprintf p
		      "<tr onclick='hop_fold_item_toggle_service(~s,~a,~s) ~a'>"
		      id history
		      (with-access::xml-delay (cadr body) (thunk)
			 ((procedure->service thunk)))
		      (with-access::html-foldlist parent (stop)
			 (if parent ";hop_stop_propagation( event, true )" "")))
	     (set-car! (cdr body) ""))
	    (else
	     (fprintf p "<tr onclick='hop_fold_item_toggle(~s,~a) ~a'>"
		      id history
		      (with-access::html-foldlist parent (stop)
			 (if stop ";hop_stop_propagation( event, true )" "")))))
	 (fprintf p "<td>
                       <span class='hop-fl-img ~a' id=~s></span>
                     </td><td class='~a'>"
		  (if open "hop-fl-img-open" "hop-fl-img-close") 
		  (string-append id "-img")
		  (with-access::html-flitem obj (cname) cname))
	 (when (and (pair? tmp)
		    (isa? (car tmp) xml-element)
		    (with-access::xml-element (car tmp) (tag)
		       (eq? tag 'flhead)))
	    (xml-write (car tmp) p backend)
	    (set! tmp (cdr tmp)))
	 (fprintf p "</td></tr><tr><td></td><td class='hop-fl-item-body'><div class='~a' id='~a' style='display:~a'>"
		  (with-access::html-flitem obj (cname) cname)
		  id (if open "block" "none"))
	 (xml-write tmp p backend)
	 (display "</div></td></tr>" p))))
  
;*---------------------------------------------------------------------*/
;*    <FLHEAD> ...                                                     */
;*---------------------------------------------------------------------*/
(define-tag <FLHEAD> ((id #unspecified string)
		      (class #unspecified string)
		      body)
   (instantiate::html-flhead
      (tag 'flhead)
      (cname (if (string? class)
		 (string-append "hop-fl-head " class)
		 "hop-fl-head"))
      (id (xml-make-id id 'FLHEAD))
      (body body)))

;*---------------------------------------------------------------------*/
;*    xml-write ::html-flhead ...                                      */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-flhead p backend)
   (with-access::html-flhead obj (cname body)
      (display (format "<span class='~a'>" cname) p)
      (xml-write body p backend)
      (display "</span>" p)))

