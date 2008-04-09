;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/runtime/hop-fileselector.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep 14 09:36:55 2006                          */
;*    Last change :  Wed Apr  2 09:22:45 2008 (serrano)                */
;*    Copyright   :  2006-08 Manuel Serrano                            */
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
	    __hop_hop-extra
	    __hop_misc
	    __hop_js-lib
	    __hop_service
	    __hop_cgi
	    __hop_hop-tree
	    __hop_user
	    __hop_hop
	    __hop_css
	    __hop_read)

   (export  (<FILEBROWSE> . ::obj)
	    filebrowse))

;*---------------------------------------------------------------------*/
;*    val->fun ...                                                     */
;*---------------------------------------------------------------------*/
(define (val->fun val)
   (cond
      ((string? val)
       (format "function() { return ~s }" (string-for-read val)))
      ((xml-tilde? val)
       (format "function() { return ~a }" (tilde->string val)))
      (else
       "function() { return false }")))
   
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
   (let ((filt (lambda (p)
		  (and (or (webdav? p) (authorized-path? req p))
		       (predicate p))))
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
		     (if label
			 (url-decode label)
			 (if root
			     (url-decode dir)
			     (url-decode (basename dir)))))
		  (<TRBODY> 
		     (<DELAY>
			(lambda ()
			   (let ((files (if (webdav? dir)
					    (webdav-directory->path-list dir)
					    (directory->path-list dir))))
			      (map! (lambda (p)
				       (if (is-directory? p)
					   (loop p (basename p) #f)
					   (<TRLEAF> :value p
					      (url-decode (basename p)))))
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
	 :onclick (format "~a; hop_stop_propagation( event, false ); this.onselect=~a; hop_filebrowse( ~a, '~a', '~a', ~a, function() { return document.getElementById( '~a' ).value;}, ~a, ~a, hop_event_mouse_x( event ), hop_event_mouse_y( event ), ~a, ~a )"
			  ;; user onclick
			  (cond
			     ((xml-tilde? onclick)
			      (xml-tilde-body onclick))
			     ((string? onclick)
			      onclick)
			     (else
			      "false"))
			  ;; onselect
			  (val->fun onselect)
			  ;; service
			  (hop-service-javascript svc)
			  ;; title
			  (if (string? title) title value)
			  ;; button ident
			  id
			  ;; label
			  (val->fun label)
			  ;; id
			  id
			  ;; path
			  (val->fun path)
			  ;; multiselect
			  (if multiselect "true" "false")
			  ;; with and height
			  width height)
	 text)))

;*---------------------------------------------------------------------*/
;*    filebrowse ...                                                   */
;*---------------------------------------------------------------------*/
(define (filebrowse #!key
		    id
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
      ((and (not (string? value)) (not (xml-tilde? value)))
       (error 'filebrowse "~ and string expected" value))
      ((and (not (string? path)) (not (xml-tilde? value)))
       (error 'filebrowse "~ and string expected" value))
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
      (format "hop_filebrowse( ~a, '~a', '~a', ~a, ~a, ~a, ~a, 0, 0, ~a, ~a )"
	      ;; service
	      (hop-service-javascript svc)
	      ;; title
	      title
	      ;; ident
	      (xml-make-id id 'FILEBROWSE)
	      ;; label
	      (val->fun label)
	      ;; value
	      (val->fun value)
	      ;; path
	      (val->fun path)
	      ;; multiselect
	      (if multiselect "true" "false")
	      ;; width, height
	      width height)))
   
				      
