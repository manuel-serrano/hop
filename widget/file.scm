;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/widget/file.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr  2 07:32:34 2008                          */
;*    Last change :  Fri Dec 17 08:19:22 2010 (serrano)                */
;*    Copyright   :  2008-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP of server-side file selectors and completion.            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwidget-file

   (library web hop)
   
   (import  __hopwidget-tree
	    __hopwidget-paned)

   (export  (init-hop-file-services!)
	    (url-completion ::http-request ::bstring)
	    (<FILECHOOSER> . ::obj)))

;*---------------------------------------------------------------------*/
;*    *inputurl-service* ...                                           */
;*    -------------------------------------------------------------    */
;*    Any change to the service name must be reflected in hop-dom.js   */
;*    and hop-file.js                                                  */
;*---------------------------------------------------------------------*/
(define *inputurl-service* #unspecified)
(define *filechooser-service* #unspecified)
(define *files-service* #unspecified)
(define *addplace-service* #unspecified)
(define *removeplace-service* #unspecified)
(define *toggle-service* #unspecified)

;*---------------------------------------------------------------------*/
;*    url-icon-path ...                                                */
;*---------------------------------------------------------------------*/
(define (url-icon-path icon)
   (make-file-path (hop-icons-directory) "hop-filechooser" icon))

;*---------------------------------------------------------------------*/
;*    init-hop-file-services! ...                                      */
;*---------------------------------------------------------------------*/
(define (init-hop-file-services!)
   (set! *inputurl-service*
	 (service :name "server-file/completion" (path)
	    (when (string? path)
	       (let ((path ((hop-charset->locale) path)))
		  (url-completion (current-request) path)))))
   (set! *filechooser-service*
	 (service :name "server-file/filechooser" (args)
	    (apply <FILECHOOSER> args)))
   (set! *files-service*
	 (service :name "server-file/files" (id url regexp showhidden)
	    (let ((url (if (string? url)
			   ((hop-charset->locale) url)
			   (pwd))))
	       (preference-store! 'filechooser/url url)
	       (list (<FILECHOOSER:FILES> id url regexp (not showhidden))
		     (<FILECHOOSER:PATH> id url)))))
   (set! *addplace-service*
	 (service :name "server-file/addplace" (id url)
	    (when (string? url)
	       (let ((url ((hop-charset->locale) url)))
		  (when (and (string? url) (is-directory? url))
		     (let ((old (preference-get 'filechooser/places :default '())))
			(unless (member url old)
			   (preference-store! 'filechooser/places (append! old (list url))))))))
	    (<FILECHOOSER:PLACES> id)))
   (set! *removeplace-service*
	 (service :name "server-file/removeplace" (id url)
	    (when (string? url)
	       (let ((old (preference-get 'filechooser/places)))
		  (when old
		     (preference-store!
		      'filechooser/places
		      (delete! ((hop-charset->locale) url) old)))))
	    (<FILECHOOSER:PLACES> id)))
   (set! *toggle-service*
	 (service :name "server-file/togglelocation" (id v)
	    (preference-store! 'filechooser/show-location v)
	    (location-classname v))))

;*---------------------------------------------------------------------*/
;*    webdav? ...                                                      */
;*---------------------------------------------------------------------*/
(define (webdav? path)
   (substring-at? path "http://" 0))

;*---------------------------------------------------------------------*/
;*    url-completion ...                                               */
;*---------------------------------------------------------------------*/
(define (url-completion req path)
   (if (webdav? path)
       (webdav-completion req path)
       (abspath-completion req path)))

;*---------------------------------------------------------------------*/
;*    abspath-completion ...                                           */
;*---------------------------------------------------------------------*/
(define (abspath-completion req path)
   (let ((dir (dirname path)))
      (if (and (file-exists? dir)
	       (directory? dir)
	       (authorized-path? req dir)
	       (>fx (string-length path) 0))
	  (let ((base (if (char=? (string-ref path (-fx (string-length path) 1))
				  (file-separator))
			  ""
			  (basename path))))
	     (list->vector
	      (sort (filter-map (lambda (s)
				   (when (substring-at? s base 0)
				      (let ((p (make-file-name dir s)))
					 (if (directory? p)
					     (make-file-name p "")
					     p))))
				(directory->list dir))
		    string<?)))
	  '#())))

;*---------------------------------------------------------------------*/
;*    webdav-completion ...                                            */
;*---------------------------------------------------------------------*/
(define (webdav-completion req path)
   (let ((dir (dirname path)))
      (if (and (webdav-file-exists? dir) (webdav-directory? dir))
	  (let ((base (if (char=? (string-ref path (-fx (string-length path) 1))
				  (file-separator))
			  ""
			  (basename path))))
	     (list->vector
	      (sort (filter-map (lambda (s)
				   (when (substring-at? s base 0)
				      (let ((p (make-file-name dir s)))
					 (if (directory? p)
					     (make-file-name p "")
					     p))))
				(webdav-directory->list dir))
		    string<?)))
	  '#())))

;*---------------------------------------------------------------------*/
;*    obj->proc ...                                                    */
;*---------------------------------------------------------------------*/
(define (obj->proc obj)
   (cond
      ((xml-tilde? obj)
       (format "function( event ) { ~a }" (xml-tilde->return obj)))
      ((string? obj)
       (format "function( event ) { ~a }" obj))))

;*---------------------------------------------------------------------*/
;*    <FILECHOOSER> ...                                                */
;*    -------------------------------------------------------------    */
;*    See __hop_css for HSS types.                                     */
;*---------------------------------------------------------------------*/
(define-markup <FILECHOOSER> ((id #unspecified string)
			      (class #unspecified string)
			      (filters '(("All Files" "^[^.].*")))
			      (hidden #t)
			      (url (preference-get 'filechooser/url :default (pwd)) string)
			      (onselect #f)
			      (onopen #f)
			      (oncancel #f)
			      (onrun #f)
			      body)
   (let ((id (xml-make-id id 'filechooser))
	 (regexp (if (null? filters) ".*" (cadar filters))))
     (<DIV> :id id
	 :class (if (string? class)
		    (string-append "filechooser " class)
		    "filechooser")
	 :onkeydown (secure-javascript-attr
		     (format "hop_filechooser_key( this, ~s )" id))
	 (<SCRIPT>
	    (format "hop_add_event_listener( window, 'ready', function( e ) { var el = document.getElementById( ~s ); ~a; ~a; ~a; ~a; }, false )"
		    id
		    (if onselect
			(format "el.select = ~a" (obj->proc onselect))
			"false")
		    (if onopen
			(format "el.open = ~a;" (obj->proc onopen))
			"false")
		    (if oncancel
			(format "el.cancel = ~a" (obj->proc oncancel))
			"false")
		    (if onrun
			(format "el.run = ~a" (obj->proc onrun))
			"false")))
	 (<IMG> :class "filechooser-drag"
	    :id (string-append id "-drag")
	    :src (url-icon-path "drag.png"))
	 (<DIV> :id (string-append id "-path")
	    (<FILECHOOSER:PATH> id url))
	 (<FILECHOOSER:LOCATION> url id)
	 (<PANED> :fraction 25
	    (<PAN>
	       :onmouseup (secure-javascript-attr
			   (format "hop_filechooser_end_drag( event, ~s )" id))
	       (<DIV> :id (string-append id "-places")
		  (<FILECHOOSER:PLACES> id)))
	    (<PAN>
	       (let ((files (<DIV> :id (string-append id "-files")
			       :class "filechooser-files"
			       (<FILECHOOSER:FILES> id url regexp hidden))))
		  (if (pair? body)
		      (<TABLE> :class "filechooser-body"
			 :cellspacing 0
			 :cellpadding 0
			 (<COLGROUP> (<COL>) (<COL> :width "0*"))
			 (<TR>
			    (<TD> files)
			    (<TD> :class "filechooser-body" body)))
		      files))))
	 (<DIV> (<FILECHOOSER:BUTTONS> id url filters regexp hidden))
	 (<DIV> (<FILECHOOSER:OKCANCEL> id)))))

;*---------------------------------------------------------------------*/
;*    location-classname ...                                           */
;*---------------------------------------------------------------------*/
(define (location-classname flag)
   (if flag
       "filechooser-button-selected filechooser-button filechooser-keyboard"
       "filechooser-button filechooser-keyboard"))

;*---------------------------------------------------------------------*/
;*    <FILECHOOSER:PATH> ...                                           */
;*---------------------------------------------------------------------*/
(define (<FILECHOOSER:PATH> id url)
   
   (define (<BUT> dir url id)
      (<SPAN> :class "filechooser-button filechooser-button-unselected"
	 :onclick (secure-javascript-attr
		   (format "hop_filechooser_button_push( this, ~s, ~s )" id url))
	 :onmousedown (secure-javascript-attr
		       (format "hop_filechooser_begin_drag( event, ~s, ~s )"
			      id url))
	 dir
	 (<SPAN> :style "display: none" (string (file-separator)))))
   
   (define (initial-url scheme user host port)
      (cond
	 ((string=? scheme "file")
	  "")
	 (user
	  (format "~a://~a@~a:~a" scheme user host port))
	 (else
	  (format "~a://~a:~a" scheme host port))))

   (if (>fx (string-length url) 0)
       (multiple-value-bind (scheme user host port abspath)
	  (url-parse url)
	  (let loop ((url (initial-url scheme user host port))
		     (dirs (file-name->list abspath))
		     (buts '()))
	     (if (pair? dirs)
		 (let ((dir ((hop-locale->charset) (car dirs))))
		    (let ((url (make-file-name url dir)))
		       (loop url
			     (cdr dirs)
			     (cons (<BUT> dir url id) buts))))
		 (<DIV> :class "filechooser-path"
		    (<SPAN> :class (location-classname
				    (preference-get 'filechooser/show-location))
		       :onclick (secure-javascript-attr
				 (format "hop_filechooser_toggle_location( this, ~s )"
					 id ))
		       "  ")
		    (reverse! buts)))))
       '()))

;*---------------------------------------------------------------------*/
;*    <FILECHOOSER:LOCATION> ...                                       */
;*---------------------------------------------------------------------*/
(define (<FILECHOOSER:LOCATION> url id)
   (<TABLE> :class "filechooser-location" :id (string-append id "-location")
      :style (if (preference-get 'filechooser/show-location)
		 "display:block"
		 "display:none")
      (<TR>
	 (<TH> "Location: ")
	 (<TD> (<INPUT> :type 'url
		  :id (string-append id "-location-input")
		  :class "filechooser-location"
		  :onkeypress (secure-javascript-attr
			       (format "hop_filechooser_location_keypress( this, event, ~s )"
				       id))
		  :value "")))))

;*---------------------------------------------------------------------*/
;*    <FILECHOOSER:PLACES> ...                                         */
;*---------------------------------------------------------------------*/
(define (<FILECHOOSER:PLACES> id)
   (<TABLE> :class "filechooser-places"
      :cellspacing 0 :cellpadding 0
      :onmouseup (secure-javascript-attr
		  (format "hop_filechooser_end_drag( event, ~s )" id))
      (<COLGROUP> (<COL> :width "0*"))
      (<TR> (<TH> :colspan 2 "Places"))
      ;; home and hdd
      (let ((path (getenv "HOME")))
	 (<TR> :ondblclick (secure-javascript-attr
			    (format "hop_filechooser_open( ~s, ~s )" id path))
	    :title path
	    (<TD> :class "filechooser-icon filechooser-home" (getenv "USER"))))
      (let ((path (make-file-name (getenv "HOME") "Desktop")))
	 (<TR> :ondblclick (secure-javascript-attr
			    (format "hop_filechooser_open( ~s, ~s )" id path))
	    :title path
	    (<TD> :class "filechooser-icon filechooser-desktop" "Desktop")))
      (let ((path (dirname (make-file-name "" "foo"))))
	 (<TR> :ondblclick (secure-javascript-attr
			    (format "hop_filechooser_open( ~s, ~s )" id path))
	    :title path
	    (<TD> :class "filechooser-icon filechooser-hdd" "File system")))
      (<TR> (<TD> :colspan 2 :class "filechooser-br"))
      ;; users directories
      (filter-map (lambda (ep)
		     (when (string? ep)
			(let ((p ((hop-locale->charset) ep)))
			   (<TR>
			      :onclick (secure-javascript-attr
					(format "hop_filechooser_select( this, event, ~s, ~s )" id p))
			      :ondblclick (secure-javascript-attr
					   (format "hop_filechooser_open( ~s, ~s )"
						   id p))
			      :title p
			      (<TD> :class "filechooser-icon filechooser-folder"
				 (basename p))))))
		  (preference-get 'filechooser/places :default '()))))

;*---------------------------------------------------------------------*/
;*    get-url-icon ...                                                 */
;*---------------------------------------------------------------------*/
(define (get-url-icon file default)
   (url-icon-path default))

;*---------------------------------------------------------------------*/
;*    is-directory? ...                                                */
;*---------------------------------------------------------------------*/
(define (is-directory? p)
   (if (webdav? p)
       (webdav-directory? p)
       (directory? p)))

;*---------------------------------------------------------------------*/
;*    2digits ...                                                      */
;*---------------------------------------------------------------------*/
(define (2digits num)
   (if (>=fx num 10)
       num
       (string #\0 (integer->char (+ (char->integer #\0) num)))))
	     
;*---------------------------------------------------------------------*/
;*    <FILECHOOSER:FILES> ...                                          */
;*    -------------------------------------------------------------    */
;*    URL is not encoded for http.                                     */
;*---------------------------------------------------------------------*/
(define (<FILECHOOSER:FILES> id url regexp hidden)
   (let ((odd #t)
	 (dnow (current-date))
	 (now (current-seconds)))
      
      (define (file-date path)
	 (let* ((sec (file-modification-time path))
		(dt (seconds->date sec)))
	    (cond
	       ((=fx (date-yday dt) (date-yday dnow))
		;; today date
		(format "Today at ~a:~a"
			(2digits (date-hour dt))
			(2digits (date-minute dt))))
	       ((or (=fx (+fx (date-yday dt) 1) (date-yday dnow))
		    (and (=fx (date-yday dnow) 1)
			 (=fx (+fx (date-year dt) 1) (date-year dnow))
			 (=fx (date-month dt) 12)
			 (=fx (date-day dt) 31)))
		;; yesterday date
		(format "Yesterday at ~a:~a"
			(2digits (date-hour dt))
			(2digits (date-minute dt))))
	       (else
		(format "~a ~a ~a" (2digits (date-day dt))
			(month-aname (date-month dt))
			(let ((y (date-year dt)))
			   (if (>= y 2000)
			       (2digits (remainder y 2000))
			       y)))))))
      
      (define (get-sorted-files url)
	 (sort (lambda (f1 f2)
		  (cond
		     ((is-directory? f1)
		      (or (not (is-directory? f2))
			  (<fx (string-natural-compare3 f1 f2) 0)))
		     ((is-directory? f2)
		      #f)
		     (else
		      (<fx (string-natural-compare3 f1 f2) 0))))
	       (filter (lambda (p)
			  (let ((b (basename p)))
			     (and (or (not (char=? (string-ref b 0) #\.))
				      (not hidden))
				  (or (is-directory? p)
				      (pregexp-match regexp (basename p))))))
		       (if (webdav? url)
			   (with-handler
			      (lambda (e)
				 (if (&io-error? e)
				     (begin
					(exception-notify e)
					'())
				     (raise e)))
			      (map! url-encode
				    (webdav-directory->path-list
				     (url-encode url))))
			   (directory->path-list url)))))
      
      (define (<tr> ep eid class prevep previd nextep nextid)
	 (let ((p ((hop-locale->charset) ep)))
	    (<TR> :class class 
	       :onclick (secure-javascript-attr
			 (format "hop_filechooser_select( this, event, ~s, ~s )" id p))
	       :ondblclick (secure-javascript-attr
			    (if (is-directory? p)
				(format "hop_filechooser_open( ~s, ~s )"
					id p)
				(format "hop_filechooser_ok( event, ~s )"
					id)))
	       :onmousedown (secure-javascript-attr
			     (format "hop_filechooser_begin_drag( event, ~s, ~s )"
				     id p))
	       (<TD> :class (if (is-directory? p)
				"filechooser-icon filechooser-folder"
				"filechooser-icon filechooser-file")
		  ;; we should use an input element to receive key events
		  #;(<INPUT> :value (url-decode (basename p))
		     :type 'text
		     :onkeydown (secure-javascript-attr
				 (format "hop_filechooser_key( this, event, ~s, ~s, ~s, ~s, ~s )"
					 id prevep previd nextep nextid)))
		  (<SPAN> (url-decode (basename p))))
	       (<TD> :class "filechooser-modified"
		  (file-date ep)))))
      
      (<TABLE> :class "filechooser-files" :cellspacing 0 :cellpadding 0
	 (<COLGROUP> (<COL>) (<COL> :width "0*"))
	 (<TR> (<TH> "Name") (<TH> :class "filechooser-modified" "Modified"))
	 (let ((epaths (get-sorted-files url)))
	    (if (null? epaths)
		'()
		(let loop ((epaths epaths)
			   (ids (map (lambda (ep) (xml-make-id 'path)) epaths))
			   (classes (let ((cla (list "odd" "even")))
				       (set-cdr! (last-pair cla) cla)
				       cla))
			   (pep "")
			   (pid ""))
		   (let ((ep (car epaths))
			 (id (car ids)))
		      (if (null? (cdr epaths))
			  (list (<tr> ep id (car classes) pep pid "" ""))
			  (cons (<tr> ep id (car classes) pep pid (cadr epaths) (cadr ids))
				(loop (cdr epaths) (cdr ids) (cdr classes) ep id))))))))))

;*---------------------------------------------------------------------*/
;*    <FILECHOOSER:BUTTONS> ...                                        */
;*---------------------------------------------------------------------*/
(define (<FILECHOOSER:BUTTONS> id url filters regexp hidden)
   (<TABLE> :class "filechooser-buttons"
      (<TR>
	 (<TD>
	    (<BUTTON> :class "filechooser-button-add"
	       :onclick (secure-javascript-attr
			 (format "hop_filechooser_add( ~s )" id))
	       "Add")
	    (<BUTTON> :class "filechooser-button-remove"
	       :onclick (secure-javascript-attr
			 (format "hop_filechooser_remove( ~s )" id))
	       "Remove"))
	 (<TD> :class "right"
	    (<SPAN> :class "filechooser-hidden"
	       (<INPUT> :type 'checkbox
		  :id (string-append id "-hidden")
		  :selected (not hidden)
		  :onchange (secure-javascript-attr
			     (format "hop_filechooser_filter( ~s, ~s )" id url)))
	       "Show Hidden Files")
	    (<SELECT> :class "filechooser-filters"
	       :id (string-append id "-filters")
	       :onchange (secure-javascript-attr
			  (format "hop_filechooser_filter( ~s, ~s )" id url))
	       (map (lambda (o)
		       (<OPTION> :value (cadr o)
			  :selected (string=? (cadr o) regexp)
			  (car o) " "))
		    filters))))))

;*---------------------------------------------------------------------*/
;*    <FILECHOOSER:OKCANCEL> ...                                       */
;*---------------------------------------------------------------------*/
(define (<FILECHOOSER:OKCANCEL> id)
   (<DIV> :class "filechooser-okcancel"
      (<BUTTON> :class "filechooser-button-cancel"
	 :onclick (secure-javascript-attr
		   (format "hop_filechooser_cancel( event, ~s )" id))
	 (<SPAN> :class "filechooser-button-cancel" " "))
      (<BUTTON> :class "filechooser-button-open"
	 :onclick (secure-javascript-attr
		   (format "hop_filechooser_ok( event, ~s )" id))
	 (<SPAN> :class "filechooser-button-open" " "))
      (<BUTTON> :class "filechooser-button-run"
	 :onclick (secure-javascript-attr
		   (format "hop_filechooser_run( event, ~s )" id))
	 (<SPAN> :class "filechooser-button-run" " "))))


