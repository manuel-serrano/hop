;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/hop-extra.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 14 05:36:34 2005                          */
;*    Last change :  Sat Nov 18 15:17:15 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Various HTML extensions                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-extra

   (include "compiler-macro.sch"
	    "xml.sch")

   (library web)

   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_misc
	    __hop_xml
	    __hop_css
	    __hop_js-lib
	    __hop_hop)

   (export  (<HEAD> . ::obj)
	    (head-parse args)
	    (<FOOT> . ::obj)
	    (<FOOT-BUTTON> . ::obj)
	    
	    (<TOOLTIP> . ::obj)
	    (<SORTTABLE> . ::obj)))

;*---------------------------------------------------------------------*/
;*    head-runtime-system ...                                          */
;*---------------------------------------------------------------------*/
(define head-runtime-system
   (cons (let ((p (make-file-name (hop-share-directory) "hop.css")))
	    (<LINK> :rel "stylesheet" :type "text/css"
	       :href p))
	 (map (lambda (f)
		 (let ((p (make-file-name (hop-share-directory) f)))
		    (<SCRIPT> :type (hop-configure-javascript-mime-type)
		       :src p)))
	      (hop-runtime-system))))

;*---------------------------------------------------------------------*/
;*    head-parse ...                                                   */
;*---------------------------------------------------------------------*/
(define (head-parse args)
   
   (define (jscript p)
      (<SCRIPT> :type (hop-javascript-mime-type) :src p))
   
   (define (favicon p)
      (<LINK> :rel "shortcut icon" :href p))
   
   (define (css p)
      (<LINK> :rel "stylesheet" :type "text/css" :href p))

   (define (incl f)
      (let ((js (let ((p (make-file-name (hop-share-directory)
					 (string-append f ".js"))))
		   (if (file-exists? p)
		       (jscript p)
		       (let ((p (make-file-name (hop-share-directory)
						(string-append f ".scm"))))
			  (when (file-exists? p)
			     (jscript p))))))
	    (css (let ((p (make-file-name (hop-share-directory)
					  (string-append f ".hss"))))
		    (if (file-exists? p)
			(css p)
			(let ((p (make-file-name (hop-share-directory)
						 (string-append f ".css"))))
			   (when (file-exists? p)
			      (css p)))))))

	 (if js
	     (if css
		 (list js css)
		 (list js))
	     (if css
		 (list css)
		 (error '<HEAD> "Can't find include file" f)))))
   
   (let loop ((a args)
	      (mode #f)
	      (rts #t)
	      (els '()))
      (cond
	 ((null? a)
	  (let ((body (reverse! els)))
	     (if rts
		 (append head-runtime-system body)
		 body)))
	 ((pair? (car a))
	  (loop (append (car a) (cdr a)) mode rts els))
	 ((null? (car a))
	  (loop (cdr a) mode rts els))
	 ((keyword? (car a))
	  (if (null? (cdr a))
	      (error '<HEAD> (format "Missing ~a value" (car a)) a)
	      (case (car a)
		 ((:css)
		  (if (string? (cadr a))
		      (loop (cddr a) :css rts (cons (css (cadr a)) els))
		      (error '<HEAD> "Illegal :css" (cadr a))))
		 ((:jscript)
		  (if (string? (cadr a))
		      (loop (cddr a) :jscript rts (cons (jscript (cadr a)) els))
		      (error '<HEAD> "Illegal :jscript" (cadr a))))
		 ((:favicon)
		  (if (string? (cadr a))
		      (loop (cddr a) #f rts (cons (favicon (cadr a)) els))
		      (error '<HEAD> "Illegal :favicon" (cadr a))))
		 ((:include)
		  (if (string? (cadr a))
		      (loop (cddr a) :include rts (append (incl (cadr a)) els))
		      (error '<HEAD> "Illegal :include" (cadr a))))
		 ((:rts)
		  (if (boolean? (cadr a))
		      (loop (cddr a) #f (cadr a) els)
		      (error '<HEAD> "Illegal :rts" (cadr a))))
		 ((:title)
		  (if (string? (cadr a))
		      (loop (cddr a) #f rts (cons (<TITLE> (cadr a)) els))
		      (error '<HEAD> "Illegal :title" (cadr a))))
		 ((:base)
		  (if (string? (cadr a))
		      (loop (cddr a) #f rts (cons (<BASE> :href (cadr a)) els))
		      (error '<HEAD> "Illegal :title" (cadr a))))
		 (else
		  (error '<HEAD>
			 (format "Unknown ~a argument" (car a))
			 (cadr a))))))
	 ((string? (car a))
	  (case mode
	     ((:css)
	      (loop (cdr a) mode rts (cons (css (car a)) els)))
	     ((:jscript)
	      (loop (cdr a) mode rts (cons (jscript (car a)) els)))
	     ((:include)
	      (loop (cdr a) mode rts (append (incl (car a)) els)))
	     (else
	      (loop (cdr a) #f rts (cons (car a) els)))))
	 ((not (car a))
	  (loop (cdr a) #f rts els))
	 (else
	  (loop (cdr a) #f rts (cons (car a) els))))))

;*---------------------------------------------------------------------*/
;*    <HEAD> ...                                                       */
;*---------------------------------------------------------------------*/
(define (<HEAD> . args)
   (let* ((body0 (head-parse args))
	  (ubase (filter (lambda (x)
			    (xml-markup-is? x 'base))
			 body0))
	  (body1 (if (pair? ubase)
		     (cons (car (last-pair ubase))
			   (filter! (lambda (x)
				       (not (xml-markup-is? x 'base)))
				    body0))
		     body0))
	  (body2 (if (not (xml-markup-is? (car body0) 'meta))
		     (let ((meta (<META> :http-equiv "Content-Type")))
			(cons meta body1))
		     body1)))
      (instantiate::xml-markup
	 (markup 'head)
	 (attributes '())
	 (body body2))))

;*---------------------------------------------------------------------*/
;*    <FOOT> ...                                                       */
;*---------------------------------------------------------------------*/
(define-xml-compound <FOOT> ((id #unspecified string)
			     (class "foot" string)
			     body)
   (<DIV>
      :id (xml-make-id id 'FOOT)
      :class class
      (<DIV>
	 :align "center"
	 :class "foot-buttons"
	 (<FOOT-BUTTON>
	    :href "http://hop.inria.fr"
	    :title "HOP home page"
	    :src "hop.png")
	 body)))

;*---------------------------------------------------------------------*/
;*    <FOOT-BUTTON> ...                                                */
;*---------------------------------------------------------------------*/
(define-xml-compound <FOOT-BUTTON> ((id #unspecified string)
				    (class "foot-button")
				    (href #f string)
				    (title #f string)
				    (path #f)
				    (src #f))
   (<A>
      :class class
      :href href
      :title title
      (<IMG> :alt title
	     :src (cond
		     ((string? path)
		      path)
		     ((string? src)
		      (format "~a/buttons/~a"
			      (url-encode (hop-share-directory))
			      src))
		     (else
		      (error '<FOOT-BUTTON> "Illegal source" src))))))
				     
;*---------------------------------------------------------------------*/
;*    <TOOLTIP> ...                                                    */
;*---------------------------------------------------------------------*/
(define-xml-compound <TOOLTIP> ((id #unspecified string)
				(onclick 0)
				(onmouseout 0)
				body)
   (<DIV> :id (xml-make-id id 'TOOLTIP)
	  :class "hoptooltip"
	  :onclick (format "~a; hop_tooltip_hide()" onclick)
	  :onmouseout (format "~a; hop_tooltip_hide()" onmouseout)
	  body))
	  
;*---------------------------------------------------------------------*/
;*    <SORTTABLE> ...                                                  */
;*---------------------------------------------------------------------*/
(define-xml-compound <SORTTABLE> ((id #unspecified string)
				  (attributes)
				  body)
   :hss-type "span.hop-sorttable table"
   (let ((i (xml-make-id))
	 (attr (map! (lambda (e)
			(list (symbol->keyword (car e)) (cdr e)))
		     attributes)))
      (<SPAN>
	 :id i 
	 :class "hop-sorttable"
	 (apply <TABLE> :id (if (string? id) id (xml-make-id 'SORTTABLE))
		(append! attr body))
	 (<SCRIPT> (format "hop_sorttable( '~a' )" i)))))
