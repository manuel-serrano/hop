;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/html-extra.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 14 05:36:34 2005                          */
;*    Last change :  Fri Jan 20 09:17:53 2006 (serrano)                */
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
	    __hop_builtin
	    __hop_hop)

   (export  (<HOP-HEAD> . ::obj)
	    
	    (<HOP-MAILTO> . ::obj)
	    (<TOOLTIP> . ::obj)
	    (<SORTTABLE> . ::obj))
	    
   (eval    (export-exports)))

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
	  (favicon #f))
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
	    ((null? (cdr a))
	     (error '<HOP-HEAD> (format "Missing ~a value" (car a)) a))
	    (else
	     (case (car a)
		((:dir)
		 (set! dir (cons (cadr a) dir)))
		((:css)
		 (cond
		    ((and (pair? (cadr a)) (every? string? (cadr a)))
		     (set! css (append (reverse (cadr a)) css)))
		    ((string? (cadr a))
		     (set! css (cons (cadr a) css)))
		    (else
		     (error '<HOP-HEAD> "Illegal :css" (cadr a)))))
		((:jscript)
		 (cond
		    ((and (pair? (cadr a)) (every? string? (cadr a)))
		     (set! jscript (append (reverse (cadr a)) jscript)))
		    ((string? (cadr a))
		     (set! jscript (cons (cadr a) jscript)))
		    (else
		     (error '<HOP-HEAD> "Illegal :jscript" (cadr a)))))
		((:favicon)
		 (set! favicon (cadr a)))
		(else
		 (error '<HOP-HEAD>
			(format "Unknown ~a argument" (car a))
			(cadr a))))
	     (loop (cddr a)))))))

;*---------------------------------------------------------------------*/
;*    <HOP-MAILTO> ...                                                 */
;*---------------------------------------------------------------------*/
(define (<HOP-MAILTO> . args)
   (let* ((email (car args))
	  (oclick (format "hop( ~a, ~s )"
			  (scheme->javascript builtin/mailto)
			  email)))
      (<SPAN> :class "hopmailto" :onclick oclick email)))

;*---------------------------------------------------------------------*/
;*    <TOOLTIP> ...                                                    */
;*---------------------------------------------------------------------*/
(define-xml-compound TOOLTIP ((id #unspecified string)
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
(define-xml-compound SORTTABLE ((id #unspecified string)
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
