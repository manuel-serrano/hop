;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/hop-fileselector.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep 14 09:36:55 2006                          */
;*    Last change :  Sat Sep 16 18:20:01 2006 (serrano)                */
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
	    __hop_hop
	    __hop_css)

   (export  (<FILESELECT> . ::obj)
	    (<FILEBROWSE> . ::obj)
	    hop-filebrowse))
   
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
;*    <FILESELECT> ...                                                 */
;*---------------------------------------------------------------------*/
(define-xml-compound <FILESELECT> ((id #unspecified string)
				   (class #unspecified string)
				   (size 10)
				   (value "" string)
				   (attributes)
				   body)
   :hss-type "input.hop-fileselect"
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
;*    browse ...                                                       */
;*---------------------------------------------------------------------*/
(define (browse req window-id base path predicate multiselect onselect)
   (let ((tree-id (xml-make-id 'TREE))
	 (filt (lambda (p)
		  (and (authorized-path? req p) (predicate p)))))
      (<DIV> :class "hop-filebrowse"
	 (<DIV> :class "hop-filebrowse-tree"
	    (let loop ((dir base)
		       (root #t))
	       (<TREE>
		  :id (if root tree-id (xml-make-id 'SUBTREE))
		  :value dir
		  :open (substring-at? path dir 0)
		  :onselect (format "hop_iwindow_close( '~a' ); ~a"
				    window-id
				    (cond
				       ((string? onselect)
					onselect)
				       ((xml-tilde? onselect)
					(xml-tilde-body onselect))
				       (else
					"")))
		  :multiselect multiselect
		  (<TRHEAD>
		     (if root dir (basename dir)))
		  (<TRBODY> 
		     (<DELAY>
			(lambda ()
			   (let ((files (map! (lambda (f)
						 (make-file-name dir f))
					      (directory->list dir))))
			      (map! (lambda (p)
				       (if (directory? p)
					   (loop p #f)
					   (<TRLEAF> :value p (basename p))))
				    (sort (filter! filt files)
					  string<?)))))))))
	 (<DIV> :class "hop-filebrowse-buttons"
	    (<BUTTON> :onclick (format "hop_iwindow_close( '~a' )"
				       window-id
				       window-id)
	       "Cancel")
	    (<BUTTON> :onclick
	       (format "document.getElementById( '~a' ).onselect()" tree-id)
	       "Select")))))
   
;*---------------------------------------------------------------------*/
;*    *filebrowser-service* ...                                        */
;*---------------------------------------------------------------------*/
(define *filebrowser-service* #f)

;*---------------------------------------------------------------------*/
;*    default-filebrowser-filter-predicate ...                         */
;*---------------------------------------------------------------------*/
(define (default-filebrowser-filter-predicate p)
   (let ((f (basename p)))
      (or (=fx (string-length f) 0)
	  (not (char=? (string-ref f 0) #\.)))))

;*---------------------------------------------------------------------*/
;*    make-filebrowser-service ...                                     */
;*---------------------------------------------------------------------*/
(define (make-filebrowser-service predicate)
   (service (ident wident value path multi)
      (let ((onselect (format "var o=document.getElementById( '~a' ); o.value = this.selection.value; o.onselect();" ident)))
	 (browse (the-current-request) wident value path predicate multi onselect))))

;*---------------------------------------------------------------------*/
;*    get-default-filebrowser-service ...                              */
;*    -------------------------------------------------------------    */
;*    Default browser service. It uses the default browser filter      */
;*    procedure. Browsers that uses a different filter have to         */
;*    use a different service.                                         */
;*---------------------------------------------------------------------*/
(define (get-default-filebrowser-service)
   (unless *filebrowser-service*
      (set! *filebrowser-service*
	    (make-filebrowser-service default-filebrowser-filter-predicate)))
   *filebrowser-service*)

;*---------------------------------------------------------------------*/
;*    <FILEBROWSE> ...                                                 */
;*---------------------------------------------------------------------*/
(define-xml-compound <FILEBROWSE> ((id #unspecified string)
				   (class #unspecified string)
				   (title #unspecified string)
				   (value "" string)
				   (path "" string)
				   (text "Browse" string)
				   (width 500 integer)
				   (height 400 integer)
				   (onselect #unspecified)
				   (multiselect #f boolean)
				   (filter #unspecified procedure)
				   (attributes)
				   body)
   (let ((svc (if (eq? filter #unspecified)
		  (get-default-filebrowser-service)
		  (make-filebrowser-service filter)))
	 (id (xml-make-id id 'FILESELECT)))
      (<BUTTON>
	 :id id
	 :class (if (string? class)
		    (string-append "hop-filebrowse " class)
		    "hop-filebrowse")
	 :value value
	 :onclick (format "hop_stop_propagation( event, false ); this.onselect = ~a; hop_filebrowse( ~a, '~a', '~a', this.value, '~a', ~a, event.clientX, event.clientY, ~a, ~a )"
			  ;; service
			  (cond
			     ((xml-tilde? onselect)
			      (format "function() { ~a }"
				      (xml-tilde-body onselect)))
			     ((string? onselect)
			      (format "function() { ~a }" onselect))
			     (else
			      "function() { return false }"))
			  ;; service
			  (hop-service-javascript svc)
			  ;; title
			  (if (string? title) title value)
			  ;; button ident
			  id
			  ;; path
			  path
			  ;; multiselect
			  (if multiselect "true" "false")
			  ;; with and height
			  width height)
	 text)))

;*---------------------------------------------------------------------*/
;*    hop-filebrowse ...                                               */
;*---------------------------------------------------------------------*/
(define (hop-filebrowse #!key
			handler
			title
			value
			path
			(filter default-filebrowser-filter-predicate)
			(multiselect #f)
			(width 500)
			(height 400))
   (cond
      ((not (integer? width))
       (bigloo-type-error 'hop-filebrowse "integer" width))
      ((not (integer? height))
       (bigloo-type-error 'hop-filebrowse "integer" height))
      ((not (xml-tilde? handler))
       (bigloo-type-error 'hop-filebrowse "client code" handler))
      ((not (string? value))
       (bigloo-type-error 'hop-filebrowse "string" value))
      ((not (string? path))
       (bigloo-type-error 'hop-filebrowse "string" path))
      ((not (boolean? multiselect))
       (bigloo-type-error 'hop-filebrowse "boolean" multiselect))
      ((not (procedure? filter))
       (bigloo-type-error 'hop-filebrowse "procedure" filter)))
   (if (not (string? title)) (set! title value))
   (let* ((body (xml-tilde-body handler))
	  (len (string-length body))
	  (hdl (if (substring-at? body ";\n" (-fx len 2))
		   (substring body 0 (-fx len 2))
		   body))
	  (svc (service (ident wident value path multi)
		  (let ((onselect (format "~a( this.value )" hdl)))
		     (browse (the-current-request) wident value path
			     filter multiselect onselect)))))
      (format "hop_stop_propagation( event, false ); hop_filebrowse( ~a, '~a', '~a', '~a', '~a', ~a, 0, 0, ~a, ~a )"
	      ;; service
	      (hop-service-javascript svc)
	      ;; title
	      title
	      ;; ident
	      (xml-make-id 'FILEBROWSE)
	      ;; value
	      value
	      ;; path
	      path
	      ;; multiselect
	      (if multiselect "true" "false")
	      ;; width, height
	      width height)))
   
				      
