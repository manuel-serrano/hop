;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/html-extra.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 14 05:36:34 2005                          */
;*    Last change :  Mon Mar 20 16:23:29 2006 (eg)                     */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Various HTML extensions                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_html-extra

   (include "compiler-macro.sch"
	    "xml.sch")

   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_misc
	    __hop_xml
	    __hop_css
	    __hop_js-lib
	    __hop_hop)

   (export  (<HOP-HEAD> . ::obj)
	    (<HOP-FOOT-LOGO> . ::obj)
	    
	    (<TOOLTIP> . ::obj)
	    (<SORTTABLE> . ::obj)))

;*---------------------------------------------------------------------*/
;*    hop-file ...                                                     */
;*---------------------------------------------------------------------*/
(define (hop-file path file)
   (let ((p (find-file/path file path)))
      (if (string? p)
	  p
	  (make-file-name (hop-share-directory) file))))

;*---------------------------------------------------------------------*/
;*    hop-css ...                                                      */
;*---------------------------------------------------------------------*/
(define (hop-css file dir)
   (<LINK>
      :rel "stylesheet"
      :type "text/css"
      :href (cond
	       ((= (string-length file) 0)
		(error '<HOP-HEAD> "Illegal css" file))
	       ((char=? (string-ref file 0) (file-separator))
		file)
	       ((substring-at? file "http://" 0)
		file)
	       (else
		(hop-file dir file)))))

;*---------------------------------------------------------------------*/
;*    hop-jscript ...                                                  */
;*---------------------------------------------------------------------*/
(define (hop-jscript file dir)
   (<SCRIPT>
      :type "text/javascript"
      :src (cond
	      ((= (string-length file) 0)
	       (error '<HOP-HEAD> "Illegal jscript" file))
	      ((char=? (string-ref file 0) (file-separator))
	       file)
	      ((substring-at? file "http://" 0)
	       file)
	      (else
	       (hop-file dir file)))))

;*---------------------------------------------------------------------*/
;*    <HOP-HEAD> ...                                                   */
;*---------------------------------------------------------------------*/
(define (<HOP-HEAD> . obj)
   (let* ((req (the-current-request))
	  (dir (if (http-request? req)
		   (list (dirname (http-request-path req)))
		   '()))
	  (css '())
	  (jscript '())
	  (favicon #f)
	  (mode #f))
      (let loop ((a obj))
	 (cond
	    ((null? a)
	     (cons
	      (<META> :http-equiv "Content-Type"
		      :content "text/html"
		      :charset (if (eq? (hop-char-encoding) 'UTF-8)
				   "UTF-8"
				   "ISO-8859-1"))
	      (let ((css (cons (<LINK> :rel "stylesheet"
				       :type "text/css"
				       :href (hop-file dir "hop.css"))
			       (map! (lambda (file) (hop-css file dir))
				     (reverse! css))))
		    (jscript (cons (<SCRIPT>
				      :type "text/javascript"
				      :src (hop-file dir "hop.js"))
				   (map! (lambda (file) (hop-jscript file dir))
					 (reverse! jscript)))))
		 (if favicon
		     (cons (<LINK> :rel "shortcut icon" :href favicon)
			   (append! css jscript))
		     (append! css jscript)))))
	    ((pair? (car a))
	     (loop (append (car a) (cdr a))))
	    ((null? (car a))
	     (loop (cdr a)))
	    ((keyword? (car a))
	     (if (null? (cdr a))
		 (error '<HOP-HEAD> (format "Missing ~a value" (car a)) a)
		 (case (car a)
		    ((:dir)
		     (set! mode :dir)
		     (if (string? (cadr a))
			 (set! dir (cons (cadr a) dir))
			 (error '<HOP-HEAD> "Illegal :dir" (cadr a))))
		    ((:css)
		     (set! mode :css)
		     (if (string? (cadr a))
			 (set! css (cons (cadr a) css))
			 (error '<HOP-HEAD> "Illegal :css" (cadr a))))
		    ((:jscript)
		     (set! mode :jscript)
		     (if (string? (cadr a))
			 (set! jscript (cons (cadr a) jscript))
			 (error '<HOP-HEAD> "Illegal :jscript" (cadr a))))
		    ((:favicon)
		     (set! mode #f)
		     (if (string? (cadr a))
			 (set! favicon (cadr a))
			 (error '<HOP-HEAD> "Illegal :favicon" (cadr a))))
		    (else
		     (error '<HOP-HEAD>
			    (format "Unknown ~a argument" (car a))
			    (cadr a)))))
	     (loop (cddr a)))
	    ((string? (car a))
	     (case mode
		((:dir)
		 (set! dir (cons (car a) dir)))
		((:css)
		 (set! css (cons (car a) css)))
		((:jscript)
		 (set! jscript (cons (car a) jscript))))
	     (loop (cdr a)))
	    (else
	     (error '<HOP-HEAD> (format "Illegal ~a argument" (car a)) a))))))

;*---------------------------------------------------------------------*/
;*    <HOP-FOOT-LOGO> ...                                              */
;*    -------------------------------------------------------------    */
;*    This do not use CSS because we want them to be correctly         */
;*    displayed even when no CSS is specified.                         */
;*---------------------------------------------------------------------*/
(define-xml-compound <HOP-FOOT-LOGO> ((id #unspecified string)
				      (img (<IMG> :inline #t
						  :src (make-file-path
							(hop-share-directory)
							"icons"
							"foot-logo.png")))
				      body)
   :hss-type "div.hop-foot-logo"
   (let ((r (if (null? body)
		(<SPAN> :style "font-size: 22px;
                                font-style: normal;
                                font-family: Futura_Poster, Blippo, Cooper, Eras-UltraBlk, RoostHeavy, Sinaloa, Arial, Verda, sans serif;
                                font-weight: bold;"
			(hop-name)
			(<SPAN> :style "font-size: 7px; font-stretch: ultra-condensed; font-family: sans serif; margin-right: 2px; margin-left: -1px; vertical-align: bottom"
				(hop-version)))
		body)))
      (<DIV> :style
	     (format "width: 84px; height: 27px;
                      overflow: hidden;
                      background-color: white;
                      border: 2px outset #777;
                      padding: 0; padding: 0;
                      background-image: url( '~a/icons/logo-bg.png' );"
		     (hop-share-directory))
	     :class "hop-foot-logo"
	     (if (not img)
		 r
		 (<TABLE> :style "width: 100%; font-size: x-small;"
			  :border-collapse "collapse" 
			  :border 0
			  :cellspacing 0
			  :cellpadding 0
			  (<TR>
			     (<TD> :align 'left img)
			     (<TD> :align 'center r)))))))
		   

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
