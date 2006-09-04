;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/hop-extra.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 14 05:36:34 2005                          */
;*    Last change :  Fri Aug 25 10:04:30 2006 (serrano)                */
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
		(error '<HEAD> "Illegal css" file))
	       ((char=? (string-ref file 0) #\/)
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
	       (error '<HEAD> "Illegal jscript" file))
	      ((char=? (string-ref file 0) #\/)
	       file)
	      ((substring-at? file "http://" 0)
	       file)
	      (else
	       (hop-file dir file)))))

;*---------------------------------------------------------------------*/
;*    head-parse ...                                                   */
;*---------------------------------------------------------------------*/
(define (head-parse args)
   (let* ((req (the-current-request))
;* 	  (dir (if (http-request? req)                                 */
;* 		   (list (dirname (http-request-path req)))            */
;* 		   '()))                                               */
	  (dir '())
	  (css '())
	  (jscript '())
	  (favicon #f)
	  (mode #f)
	  (rest '()))
      (let loop ((a args))
	 (cond
	    ((null? a)
	     (let ((css (map! (lambda (file) (hop-css file dir))
			      (reverse css)))
		   (jscript (map! (lambda (file) (hop-jscript file dir))
				  (reverse jscript))))
		(values dir
			(if favicon
			    (cons (<LINK> :rel "shortcut icon" :href favicon)
				  (append (append rest css) jscript))
			    (append rest (append css jscript))))))
	    ((pair? (car a))
	     (loop (append (car a) (cdr a))))
	    ((null? (car a))
	     (loop (cdr a)))
	    ((keyword? (car a))
	     (if (null? (cdr a))
		 (error '<HEAD> (format "Missing ~a value" (car a)) a)
		 (case (car a)
		    ((:dir)
		     (set! mode :dir)
		     (if (string? (cadr a))
			 (set! dir (cons (cadr a) dir))
			 (error '<HEAD> "Illegal :dir" (cadr a))))
		    ((:css)
		     (set! mode :css)
		     (if (string? (cadr a))
			 (set! css (cons (cadr a) css))
			 (error '<HEAD> "Illegal :css" (cadr a))))
		    ((:jscript)
		     (set! mode :jscript)
		     (if (string? (cadr a))
			 (set! jscript (cons (cadr a) jscript))
			 (error '<HEAD> "Illegal :jscript" (cadr a))))
		    ((:favicon)
		     (set! mode #f)
		     (if (string? (cadr a))
			 (set! favicon (cadr a))
			 (error '<HEAD> "Illegal :favicon" (cadr a))))
		    (else
		     (error '<HEAD>
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
		 (set! jscript (cons (car a) jscript)))
		(else
		 (set! rest (cons (car a) rest))))
	     (loop (cdr a)))
	    ((not (car a))
	     (loop (cdr a)))
	    (else
	     (set! rest (cons (car a) rest))
	     (loop (cdr a)))))))

;*---------------------------------------------------------------------*/
;*    <HEAD> ...                                                       */
;*---------------------------------------------------------------------*/
(define (<HEAD> . args)
   (multiple-value-bind (dir body)
      (head-parse args)
      (let* ((meta (<META> :http-equiv "Content-Type"))
	     (css (<LINK> :rel "stylesheet"
		     :type "text/css"
		     :href (hop-file dir "hop.css")))
	     (jscripts (list
			(<SCRIPT>
			   :type "text/javascript"
			   :src (hop-file dir "hop-autoconf.js"))
			(<SCRIPT>
			   :type "text/javascript"
			   :src (hop-file dir "hop.js"))))
	     (body (cons css  (append jscripts body)))
	     (body (if (memq (hop-xml-backend) '(html html-4.01))
		       (cons meta body)
		       body)))
	 (instantiate::xml-markup
	    (markup 'head)
	    (attributes '())
	    (body body)))))

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
