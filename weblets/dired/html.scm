;*=====================================================================*/
;*    serrano/prgm/project/hop/weblets/dired/html.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan  6 17:45:08 2005                          */
;*    Last change :  Wed Feb  8 07:38:37 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Dired HTML facilities                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    <DIRED-HEAD> ...                                                 */
;*---------------------------------------------------------------------*/
(define (<DIRED-HEAD> req dir)
   (with-access::http-request req (host port path header)
      (let ((css1 (make-file-name dir "dired.hss"))
	    (css2 (make-file-name dir ".dired.css"))
	    (css3 (make-file-name dir ".dired.hss")))
	 (<HEAD>
	  (<META> :http-equiv "Content-Type"
		  :content "text/html; charset=ISO-8859-1")
	  (<HOP-HEAD> :css "hop-tree.css" "hop-sorttable.css"
		      :jscript "hop-tree.js" "hop-sorttable.js")
	  (<LINK> :rel "stylesheet"
		  :type "text/css"
		  :href (format "~a/dired.hss" (dired-install-directory)))
	  (if (dired-mozilla-client? req)
	      (<LINK> :rel "stylesheet"
		      :type "text/css"
		      :href (format "~a/dired-moz.css" (dired-install-directory)))
	      "")
	  (if (file-exists? css1)
	      (<LINK> :rel "stylesheet" :type "text/css" :href css1)
	      "")
	  (if (file-exists? css2)
	      (<LINK> :rel "stylesheet" :type "text/css" :href css2)
	      "")
	  (if (file-exists? css3)
	      (<LINK> :rel "stylesheet" :type "text/css" :href css3)
	      "")
	  (<SCRIPT> :type "text/javascript"
		    :src (format "~a/dired.js" (dired-install-directory)))))))

;*---------------------------------------------------------------------*/
;*    <DIRED-DIR> ...                                                  */
;*---------------------------------------------------------------------*/
(define (<DIRED-DIR> req dir)
   (let ((sep (string (file-separator))))
      (with-access::http-request req (host port path)
	 (let loop ((l (file-name->list dir))
		    (acc "")
		    (res '()))
	    (if (pair? l)
		(let* ((nacc (string-append acc (car l) sep))
		       (url nacc)
		       (html (<TH> (<A> :href url (car l)) "/")))
		   (loop (cdr l) nacc (cons html res)))
		(<DIV> :class "directory"
		       (<TABLE> (<TR> (reverse! res)))))))))

;*---------------------------------------------------------------------*/
;*    <DIRED-BUTTON> ...                                               */
;*---------------------------------------------------------------------*/
(define (<DIRED-BUTTON> title onclick i1 . id)
   (let ((srcout (make-file-path (dired-install-directory) "icons" i1)))
      (<SPAN> :class "dired-button"
	      :id (if (pair? id) (car id) (xml-make-id 'DIRED-BUTTON))
	      :onclick onclick
	      (<IMG> :class "dired-button"
		     :alt srcout
		     :src srcout
		     :title title))))

