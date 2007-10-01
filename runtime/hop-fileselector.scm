;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/hop-fileselector.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep 14 09:36:55 2006                          */
;*    Last change :  Sat Sep 29 19:26:37 2007 (serrano)                */
;*    Copyright   :  2006-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP implement of server-side file selector.                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-fileselector

   (library web)
   
   (include "xml.sch"
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
	    __hop_css
	    __hop_read)

   (export  (<FILESELECT> . ::obj)
	    (<FILEBROWSE> . ::obj)
	    filebrowse))
   
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
	    (service (d o) (auto-complete (current-request) d))))
   *fileselect-service*)

;*---------------------------------------------------------------------*/
;*    <FILESELECT> ...                                                 */
;*---------------------------------------------------------------------*/
(define-xml-compound <FILESELECT> ((id #unspecified string)
				   (class #unspecified string)
				   (onchange #f)
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
	     :onkeydown (format "hop_fileselect_keypress( ~a, this, event, ~a )"
				(hop-service-javascript svc)
				(cond
				   ((string? onchange)
				    (format "function() { ~a }" onchange))
				   ((xml-tilde? onchange)
				    (format "function() { ~a }"
					    (xml-tilde-body onchange)))
				   (else
				    "function() { return false; }")))
	     (map (lambda (e) (list (symbol->keyword (car e)) (cdr e)))
		  attributes))))

;*---------------------------------------------------------------------*/
;*    webdav? ...                                                      */
;*---------------------------------------------------------------------*/
(define (webdav? path)
   (substring-at? path "http://" 0))

;*---------------------------------------------------------------------*/
;*    is-directory? ...                                                */
;*---------------------------------------------------------------------*/
(define (is-directory? p)
   (if (webdav? p)
       (webdav-directory? p)
       (directory? p)))

;*---------------------------------------------------------------------*/
;*    browse ...                                                       */
;*---------------------------------------------------------------------*/
(define (browse req::http-request
		window-id::bstring label::obj
		base::obj path::bstring
		predicate::procedure multiselect::bool onselect::bstring)
   (define (proc->string proc)
      (cond
	 ((string? proc)
	  proc)
	 ((xml-tilde? proc)
	  (xml-tilde-body proc))
	 (else
	  "")))
   (let ((filt (lambda (p) (and (authorized-path? req p) (predicate p))))
	 (tident (xml-make-id 'FILEBROWSER-TREE)))
      (<DIV> :class "hop-filebrowse"
	 (<DIV> :class "hop-filebrowse-tree"
	    (let loop ((dir (if (string? base) base path))
		       (label label)
		       (root #t))
	       (<TREE>
		  :id (if root tident (xml-make-id 'SUBTREE))
		  :value dir
		  :open (substring-at? path dir 0)
		  :onselect (format "hop_window_close( '~a' ); ~a"
				    window-id
				    (proc->string onselect))
		  :multiselect multiselect
		  (<TRHEAD>
		     (if label label (if root dir (basename dir))))
		  (<TRBODY> 
		     (<DELAY>
			(lambda ()
			   (let ((files (if (webdav? dir)
					    (webdav-directory->path-list dir)
					    (directory->path-list dir))))
			      (map! (lambda (p)
				       (if (is-directory? p)
					   (loop p (basename p) #f)
					   (<TRLEAF> :value p (basename p))))
				    (sort (filter! filt files)
					  string<?)))))))))
	 (<DIV> :class "hop-filebrowse-buttons"
	    (<BUTTON> :onclick (format "hop_window_close( '~a' )" window-id)
	       "Cancel")
	    (<BUTTON> :onclick
	       (format "document.getElementById( '~a' ).onselect()" tident)
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
   (service (ident wident label base path multi)
      (let ((onselect (format "var t=this; var o=document.getElementById( '~a' ); o.selection=hop_tree_selection(t); o.value=o.selection[0]; o.onselect();" ident)))
	 (browse (current-request)
		 wident #f base path predicate multi onselect))))

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
				   (label #unspecified string)
				   (value "" string)
				   (path "" string)
				   (text "Browse" string)
				   (width 500 integer)
				   (height 400 integer)
				   (onmousedown #f)
				   (onclick #unspecified)
				   (onselect #unspecified)
				   (multiselect #f boolean)
				   (filter #unspecified procedure)
				   (attributes)
				   body)
   (let ((svc (if (eq? filter #unspecified)
		  (get-default-filebrowser-service)
		  (make-filebrowser-service filter)))
	 (id (xml-make-id id 'FILEBROWSE)))
      (<BUTTON>
	 :id id
	 :class (if (string? class)
		    (string-append "hop-filebrowse " class)
		    "hop-filebrowse")
	 :value value
	 :onmousedown (or onmousedown "")
	 :onclick (format "~a; hop_stop_propagation( event, false ); this.onselect=~a; hop_filebrowse( ~a, '~a', '~a', ~a, this.value, '~a', ~a, hop_event_mouse_x( event ), hop_event_mouse_y( event ), ~a, ~a )"
			  ;; user onclick
			  (cond
			     ((xml-tilde? onclick)
			      (xml-tilde-body onclick))
			     ((string? onclick)
			      onclick)
			     (else
			      "false"))
			  ;; onselect
			  (cond
			     ((xml-tilde? onselect)
			      (format "function() { ~a }"
				      (xml-tilde-body onselect)))
			     ((string? onselect)
			      (format "function() { ~a }" onselect))
			     (else
			      "function() { return false; }"))
			  ;; service
			  (hop-service-javascript svc)
			  ;; title
			  (if (string? title) title value)
			  ;; button ident
			  id
			  ;; label
			  (if (string? label)
			      (string-append "\"" (string-for-read label) "\"")
			      "false")
			  ;; path
			  path
			  ;; multiselect
			  (if multiselect "true" "false")
			  ;; with and height
			  width height)
	 text)))

;*---------------------------------------------------------------------*/
;*    filebrowse ...                                                   */
;*---------------------------------------------------------------------*/
(define (filebrowse #!key
		    select
		    label
		    title
		    value
		    label
		    path
		    (filter default-filebrowser-filter-predicate)
		    (multiselect #f)
		    (width 500)
		    (height 400))
   (cond
      ((not (integer? width))
       (bigloo-type-error 'filebrowse "integer" width))
      ((not (integer? height))
       (bigloo-type-error 'filebrowse "integer" height))
      ((not (xml-tilde? select))
       (bigloo-type-error 'filebrowse "client code" select))
      ((not (string? value))
       (bigloo-type-error 'filebrowse "string" value))
      ((not (string? path))
       (bigloo-type-error 'filebrowse "string" path))
      ((not (boolean? multiselect))
       (bigloo-type-error 'filebrowse "boolean" multiselect))
      ((not (procedure? filter))
       (bigloo-type-error 'filebrowse "procedure" filter)))
   (if (not (string? title)) (set! title value))
   (let* ((body (xml-tilde-body select))
	  (len (string-length body))
	  (hdl (if (substring-at? body ";\n" (-fx len 2))
		   (substring body 0 (-fx len 2))
		   body))
	  (svc (service (ident wident label value path multi)
		  (let ((onselect (format "~a(hop_tree_selection(this))" hdl)))
		     (browse (current-request) wident label value path
			     filter multiselect onselect)))))
      (format "hop_filebrowse( ~a, '~a', '~a', ~a, '~a', '~a', ~a, 0, 0, ~a, ~a )"
	      ;; service
	      (hop-service-javascript svc)
	      ;; title
	      title
	      ;; ident
	      (xml-make-id 'FILEBROWSE)
	      ;; label
	      (if (string? label)
		  (string-append "\"" (string-for-read label) "\"")
		  "false")
	      ;; value
	      value
	      ;; path
	      path
	      ;; multiselect
	      (if multiselect "true" "false")
	      ;; width, height
	      width height)))
   
				      
