;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/hop-fileselector.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep 14 09:36:55 2006                          */
;*    Last change :  Fri Sep 15 19:56:21 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The HOP implement of server-side file selector.                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-fileselector

   (include "compiler-macro.sch"
	    "xml.sch"
	    "service.sch")

   (import  __hop_param
	    __hop_types
	    __hop_xml
	    __hop_misc
	    __hop_js-lib
	    __hop_service
	    __hop_cgi
	    __hop_hop-tree
	    __hop_user
	    __hop_hop)

   (export  (<FILESELECT> . ::obj)
	    (<FILEBROWSE> . ::obj)))
   
;*---------------------------------------------------------------------*/
;*    <FILESELECT> ...                                                 */
;*---------------------------------------------------------------------*/
(define-xml-compound <FILESELECT> ((id #unspecified string)
				   (class #unspecified string)
				   (size 10)
				   (value "" string)
				   (attributes)
				   body)
   (let ((svc (get-fileselect-service)))
      (apply <INPUT>
	     :id (xml-make-id id 'FILESELECT)
	     :class (if (string? class)
			(string-append "hop-fileselect " class)
			"hop-fileselect")
	     :type "text" :size size :value value
	     :onkeydown (format "hop_fileselect_keypress( ~a, this, event )"
				(hop-service-javascript svc))
	     (map (lambda (e) (list (symbol->keyword (car e)) (cdr e)))
		  attributes))))

;*---------------------------------------------------------------------*/
;*    *fileselect-service* ...                                         */
;*---------------------------------------------------------------------*/
(define *fileselect-service* #f)

;*---------------------------------------------------------------------*/
;*    auto-complete ...                                                */
;*---------------------------------------------------------------------*/
(define (auto-complete req path)
   (let ((dir (dirname path))
	 (base (basename path)))
      (if (and (file-exists? dir) (directory? dir) (authorized-path? req dir))
	  (list->vector 
	   (map! (lambda (s)
		    (let ((p (make-file-name dir s)))
		       (if (directory? p)
			   (make-file-name p "")
			   p)))
		 (sort (filter (lambda (s) (substring-at? s base 0))
			       (directory->list dir))
		       string<?)))
	  '#())))

;*---------------------------------------------------------------------*/
;*    get-fileselect-service ...                                       */
;*---------------------------------------------------------------------*/
(define (get-fileselect-service)
   (unless *fileselect-service*
      (set! *fileselect-service*
	    (service (d o) (auto-complete (the-current-request) d))))
   *fileselect-service*)

;*---------------------------------------------------------------------*/
;*    <FILEBROWSE> ...                                                 */
;*---------------------------------------------------------------------*/
(define-xml-compound <FILEBROWSE> ((id #unspecified string)
				   (class #unspecified string)
				   (size 10)
				   (value "" string)
				   (text "Browse" string)
				   (width 500 integer)
				   (height 400 integer)
				   (onselect #unspecified)
				   (attributes)
				   body)
   (let ((svc (get-filebrowser-service)))
      (<BUTTON>
	 :id (xml-make-id id 'FILESELECT)
	 :class (if (string? class)
		    (string-append "hop-filebrowse " class)
		    "hop-fileselect")
	 :value value
	 :onclick (format "hop_stop_propagation( event, false ); hop_filebrowse( ~a, this, ~a, ~a, event, ~a )"
			  (hop-service-javascript svc) 
			  width height
			  (cond
			     ((xml-tilde? onselect)
			      (format "function() { ~a }"
				      (xml-tilde-body onselect)))
			     ((string? onselect)
			      (format "function() { ~a }" onselect))
			     (else
			      "false")))
	 text)))

;*---------------------------------------------------------------------*/
;*    *filebrowser-service* ...                                        */
;*---------------------------------------------------------------------*/
(define *filebrowser-service* #f)

;*---------------------------------------------------------------------*/
;*    browse ...                                                       */
;*---------------------------------------------------------------------*/
(define (browse req ident base)
   (<DIV> :class "hop-filebrowse"
      (let loop ((dir base)
		 (open #t))
	 (<TREE>
	    :open open
	    :onselect (format "var o=document.getElementById( '~a' ); o.value = this.selection.value; o.onselect();" ident)
	    (<TRHEAD> (if open dir (basename dir)))
	    (<TRBODY>
	       (<DELAY>
		  (lambda ()
		     (map (lambda (f)
			     (let ((p (make-file-name dir f)))
				(if (directory? p)
				    (loop p #f)
				    (<TRLEAF> :value p f))))
			  (sort (directory->list dir) string<?)))))))))
   
;*---------------------------------------------------------------------*/
;*    get-filebrowser-service ...                                      */
;*---------------------------------------------------------------------*/
(define (get-filebrowser-service)
   (unless *filebrowser-service*
      (set! *filebrowser-service*
	    (service (ident value)
	       (browse (the-current-request) ident value))))
   *filebrowser-service*)
				      
