;*=====================================================================*/
;*    serrano/prgm/project/hop/weblets/dired/icons.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan  6 18:10:48 2005                          */
;*    Last change :  Mon Jan  9 14:36:14 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    directories displayed with icons                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    dired/rename ...                                                 */
;*---------------------------------------------------------------------*/
(define-service (dired/rename dir old new)
   (define (mv req header)
      (let* ((sep (string (file-separator)))
	     (o (string-append dir sep old))
	     (n (string-append dir sep new)))
	 (if (rename-file o n)
	     (instantiate::http-response-hop
		(header header)
		(xml (<HTML>
		      (<BODY>
		       (<SPAN> :class "dired-mv" new)))))
	     (http-internal-error 'rename "Can't rename file" o))))
   (dired-sysadmin (the-current-request) mv old))

;*---------------------------------------------------------------------*/
;*    dired/remove ...                                                 */
;*---------------------------------------------------------------------*/
(define-service (dired/remove path)
   (define (rm req header)
      (if (and (string? path) (file-exists? path))
	  (if (delete-path path)
	      (instantiate::http-response-string
		 (start-line "HTTP/1.0 400 Bad Request")
		 (body path))
	      (instantiate::http-response-string
		 (start-line "HTTP/1.0 200 Ok")
		 (body path)))
	  (instantiate::http-response-string
	     (start-line "HTTP/1.0 400 Bad Request")
	     (header header)
	     (body (if (string? path)
		       (format "Can't find find file ~s" path)
		       path)))))
   (dired-sysadmin (the-current-request) rm path))

;*---------------------------------------------------------------------*/
;*    dired/exec ...                                                   */
;*---------------------------------------------------------------------*/
(define-service (dired/exec bin arg)
   (let ((req (the-current-request)))
      (if (http-request-localclientp req)
	  (with-access::http-request req (user)
	     (cond
		((user-authorized-service? user 'exec)
		 (let* ((sys (string-append bin " " (string-escape arg #\space)))
			(res (system sys)))
		    (if (not (=fx res 0))
			(instantiate::http-response-string
			   (start-line "HTTP/1.0 400 Bad Request")
			   (body (format "Error: command failure ~s" sys)))
			(instantiate::http-response-string
			   (start-line "HTTP/1.0 200 Ok")
			   (body sys)))))
		(else
		 (user-access-denied req))))
	  (instantiate::http-response-string
	     (start-line "HTTP/1.0 400 Bad Request")
	     (body (format "Error: remote invokation from ~s disallowed"
			   (socket-hostname (http-request-socket req))))))))

;*---------------------------------------------------------------------*/
;*    dired/tgz ...                                                    */
;*---------------------------------------------------------------------*/
(define-service (dired/tgz path)
   (let ((len (string-length path)))
      (if (<=fx len 4)
	  (http-file-bad-request path)
	  (let ((apath (substring path 0 (-fx len 4))))
	     (if (file-exists? apath)
		 (instantiate::http-response-procedure
		    (content-type (mime-type "f.tgz" "application/octet-stream"))
		    (proc (lambda (p)
			     (let* ((pr (run-process
					 "tar"
					 "cfz"
					 "-"
					 apath
					 error: "/dev/null"
					 output: pipe:))
				    (pi (process-output-port pr)))
				(send-chars pi p)
				(close-input-port pi)))))
		 (http-file-bad-request path))))))

;*---------------------------------------------------------------------*/
;*    <DIRED-IMG-ONCLICK> ...                                          */
;*---------------------------------------------------------------------*/
(define (<DIRED-IMG-ONCLICK> src onclick alt)
   (<IMG> :class "link"
	  :alt alt
	  :src src
	  :onmouseover "this.style.border = \"1px solid black\";"
	  :onmouseout "this.style.border = \"1px solid transparent\";"
	  :onclick onclick))

;*---------------------------------------------------------------------*/
;*    <DIRED-IMG-CLICK> ...                                            */
;*---------------------------------------------------------------------*/
(define (<DIRED-IMG-CLICK> src click alt)
   (<DIRED-IMG-ONCLICK> src click alt))

;*---------------------------------------------------------------------*/
;*    <DIRED-IMG-LINK> ...                                             */
;*---------------------------------------------------------------------*/
(define (<DIRED-IMG-LINK> src url alt)
   (<DIRED-IMG-ONCLICK> src {location=$url} alt))

;*---------------------------------------------------------------------*/
;*    <DIRED-FILE-INFO> ...                                            */
;*---------------------------------------------------------------------*/
(define (<DIRED-FILE-INFO> file)
   (<TABLE>
    (<TR> (<TD> :align "left" "File size: ")
	  (let ((s (file-size file)))
	     (cond
		((>elong s (*elong #e1024 #e1024))
		 (<TD> :align "right"
		       (elong->string (/elong s (*elong #e1024 #e1024))) "m"))
		((> s 1024)
		 (<TD> :align "right"
		       (elong->string (/elong s #e1024)) "k"))
		(else
		 (<TD> :align "right" (elong->string s))))))
    (<TR> (<TD> :align "left" "Mod time: ")
	  (<TD> :align "right"
		(let ((dt (seconds->date
			   (file-modification-time file))))
		   (format "~a/~a/~a"
			   (date-day dt)
			   (date-month dt)
			   (date-year dt)))))
    (<TR> (<TD> :align "left" "Owner: ")
	  (<TD> :align "right" (file-uid file)))
    (<TR> (<TD> :align "left" "Perms: ")
	  (<TD> :align "right" (linux-mode (file-mode file))))))

;*---------------------------------------------------------------------*/
;*    <DIRED-ICON-ENTRY> ...                                           */
;*---------------------------------------------------------------------*/
(define (<DIRED-ICON-ENTRY> req dir class name ident icon text . buttons)
   (<DIV>
    :class "ientry"
    :id ident
    (<A> :name ident)
    (<TABLE>
     :class "ientry"
     :frame 'void :rules 'none
     (<TR>
      (<TD> :align "center"
	    (<DIV> :class "filename" (<DIRED-INPUT-RENAME> dir name))))
     (<TR>
      (<TD> :class class
	    :align "center"
	    (<DIV> :class "icon"
		   (if text
		       (<TABLE>
			(<TR> (<TD> :align "center" icon))
			(<TR> (<TD> :align "center" :class class
				    (<DIV> :class class text))))
		       (<TABLE>
			(<TR> (<TD> :align "center" icon)))))))
     (<TR>
      (<TD> :align "center"
	    (<DIV> :class "buttons"
		   (<TABLE>
		    :width "100%"
		    (<TR> (<TD> :align "left"
				(<DIRED-BUTTON-REMOVE> req dir name ident))
			  (<TD> :align "right"
				(if (pair? buttons)
				    (<TABLE>
				     (<COLGROUP> :width "200px")
				     (<TR>
				      (map (lambda (b)
					      (<TD> :align "right" b))
					   buttons)))
				    ""))))))))))

;*---------------------------------------------------------------------*/
;*    <DIRED-INPUT-RENAME> ...                                         */
;*---------------------------------------------------------------------*/
(define (<DIRED-INPUT-RENAME> dir name)
   (<INPUT>
      :class "filename"
      :onfocus "this.style.borderColor = \"black\";"
      :onblur "this.style.borderColor = \"transparent\";"
      :onkeyup {dired_rename($dired/rename($dir,$name,this.value),event,this)}
      :type 'text :value name))

;*---------------------------------------------------------------------*/
;*    <DIRED-BUTTON-REMOVE> ...                                        */
;*---------------------------------------------------------------------*/
(define (<DIRED-BUTTON-REMOVE> req dir name ident)
   (with-access::http-request req (host port)
      (let* ((path (xml-string-encode (make-file-name dir name)))
	     (oclick {dired_remove( $dired/remove( $path ), $name, $ident)}))
	 (<DIRED-BUTTON> "Remove" oclick "trash.png"))))

;*---------------------------------------------------------------------*/
;*    <DIRED-BUTTON-DOWNLOAD> ...                                      */
;*---------------------------------------------------------------------*/
(define (<DIRED-BUTTON-DOWNLOAD> req dir name)
   (with-access::http-request req (host port)
      (let* ((u (format "~a/~a" dir name))
	     (oclick {location = $u}))
	 (<DIRED-BUTTON> "Download" oclick "download.png"))))

;*---------------------------------------------------------------------*/
;*    <DIRED-BUTTON-TAR> ...                                           */
;*---------------------------------------------------------------------*/
(define (<DIRED-BUTTON-TAR> req dir name)
   (with-access::http-request req (host port)
      (let* ((path (string-append (make-file-name dir name) ".tgz"))
	     (oclick {location = $dired/tgz( $path )}))
	 (<DIRED-BUTTON> "Download" oclick "download.png"))))

;*---------------------------------------------------------------------*/
;*    dired-find-buttons ...                                           */
;*---------------------------------------------------------------------*/
(define (dired-find-buttons req dir name file)
   (let ((c (suffix-assoc-ci name *dired-commands*)))
      (if (not (pair? c))
	  '()
	  (let* ((k (if (http-request-localclientp req) 'local 'remote))
		 (apps (assq k (cdr c))))
	     (if (not apps)
		 '()
		 (map (lambda (a)
			 (let ((cmd (car a))
			       (img (cadr a)))
			    (<DIRED-BUTTON>
			     cmd
			     {hop( $dired/exec( $cmd, $file ), false )}
			     img)))
		      (cdr apps)))))))

;*---------------------------------------------------------------------*/
;*    dired-find-icon ...                                              */
;*---------------------------------------------------------------------*/
(define (dired-find-icon req dir name)
   (define (icon-name name)
      (let ((len (string-length name)))
	 (cond
	    ((=fx len 0)
	     "default")
	    ((member (string-ref name (-fx len 1)) '(#\~ #\#))
	     "backup")
	    (else
	     (let ((alias (suffix-assoc-ci name *dired-suffix-aliases*)))
		(if alias
		    (let loop ((name (cadr alias)))
		       (let ((alias (assoc name *dired-suffix-aliases*)))
			  (if (pair? alias)
			      (loop (cadr alias))
			      (string-append name ".png"))))
		    (string-append (string-downcase (suffix name)) ".png")))))))
   (let* ((icon (icon-name name))
	  (ifile (dired-app-icon icon)))
      (if (and (string? ifile) (file-exists? ifile))
	  ifile
	  (dired-app-icon "default.png"))))

;*---------------------------------------------------------------------*/
;*    dired-as-icon-entry ...                                          */
;*---------------------------------------------------------------------*/
(define (dired-as-icon-entry req dir name ident)
   (let loop ((filters *dired-icon-filters*))
      (if (null? filters)
	  (dired-icon-default-filter req dir name ident)
	  (let ((h ((car filters) req dir name ident)))
	     (if (xml? h)
		 h
		 (loop (cdr filters)))))))

;*---------------------------------------------------------------------*/
;*    dired-as-icon ...                                                */
;*---------------------------------------------------------------------*/
(define (dired-as-icon req dir hide sort order width height)
   (let* ((adir (dired-canonicalize-dirname dir))
	  (files (dired-get-files dir hide sort order))
	  (cols (/fx width 210))
	  (icons (map (lambda (f)
			 (<TD> (dired-as-icon-entry req adir f (xml-make-id 'icon))))
		      files)))
      (instantiate::http-response-hop
	 (xml (<HTML>
	       (<DIRED-HEAD> req adir)
	       (<BODY> :class "dired"
		       (<DIV> :class "header"
			      (<DIRED-DIR> req adir)
			      (<DIRED-CONFIG> req dir hide 'icon sort order))
		       (<A> :name "0")
		       (<DIV> :class "entries"
			      (<TABLE> (map <TR> (list-split! icons cols))))
		       (<DIV> :class "dired-jpeg-comment"
			      :id "dired-jpeg-comment")))))))

;*---------------------------------------------------------------------*/
;*    *dired-icon-filters* ...                                         */
;*---------------------------------------------------------------------*/
(define *dired-icon-filters* '())

;*---------------------------------------------------------------------*/
;*    dired-icon-filter-add! ...                                       */
;*---------------------------------------------------------------------*/
(define (dired-icon-filter-add! filter)
   (set! *dired-icon-filters* (cons filter *dired-icon-filters*)))

;*---------------------------------------------------------------------*/
;*    dired-icon-default-filter ...                                    */
;*---------------------------------------------------------------------*/
(define dired-icon-default-filter
   (lambda (req dir name ident)
      (with-access::http-request req (host port)
	 (let* ((img (dired-find-icon req dir name))
		(file (make-file-name dir name))
		(icon (<DIRED-IMG-LINK> img file name))
		(buttons (dired-find-buttons req dir name file))
		(info (<DIRED-FILE-INFO> file)))
	    (apply <DIRED-ICON-ENTRY> req dir "text" name ident icon info buttons)))))

;*---------------------------------------------------------------------*/
;*    directory filter ...                                             */
;*---------------------------------------------------------------------*/
(dired-icon-filter-add!
 (lambda (req dir name ident)
    (let ((path (string-append dir "/" name)))
       (when (directory? path)
	  (let ((src (dired-app-icon "directory.png")))
	     (<DIRED-ICON-ENTRY>
	      req
	      dir
	      "text"
	      name
	      ident
	      (<DIRED-IMG-CLICK> src {location=$path} name)
	      (<TABLE>
	       (<TR> (<TD> :align "left" "Entries: ")
		     (<TD> :align "right" (length (dired-directory->list path))))
	       (<TR> (<TD> :align "left" "Mod time: ")
		     (<TD> :align "right"
			   (let ((dt (seconds->date
				      (file-modification-time path))))
			      (format "~a/~a/~a"
				      (date-day dt)
				      (date-month dt)
				      (date-year dt)))))
	       (<TR> (<TD> :align "left" "Owner: ")
		     (<TD> :align "right" (file-uid path)))
	       (<TR> (<TD> :align "left" "Perms: ")
		     (<TD> :align "right" (linux-mode (file-mode path)))))
	      (<DIRED-BUTTON-TAR> req dir name)))))))

;*---------------------------------------------------------------------*/
;*    trashcan filter ...                                              */
;*---------------------------------------------------------------------*/
(dired-icon-filter-add!
 (lambda (req dir name ident)
    (let ((path (string-append dir (string (file-separator)) name)))
       (when (and (directory? path) (string-ci=? name "trashcan"))
	  (let* ((fs (dired-directory->list path))
		 (src (dired-app-icon
		       (if (null? fs) "trash-empty.png" "trash-full.png"))))
	     (<DIRED-ICON-ENTRY>
	      req
	      dir
	      "text"
	      name
	      ident
	      (<DIRED-IMG-LINK> src path name)
	      (<TABLE>
	       (<TR> (<TD> :align "left" "Entries: ")
		     (<TD> :align "right" (length (dired-directory->list path))))
	       (<TR> (<TD> :align "left" "Mod time: ")
		     (<TD> :align "right"
			   (let ((dt (seconds->date
				      (file-modification-time path))))
			      (format "~a/~a/~a"
				      (date-day dt)
				      (date-month dt)
				      (date-year dt)))))
	       (<TR> (<TD> :align "left" "Perms: ")
		     (<TD> :align "right" (linux-mode (file-mode path)))))
	      (<DIRED-BUTTON-TAR> req dir name)))))))

;*---------------------------------------------------------------------*/
;*    png/gif filter ...                                               */
;*---------------------------------------------------------------------*/
(dired-icon-filter-add!
 (lambda (req dir name ident) 
    (when (or (is-suffix-ci? name "png") (is-suffix? name "gif"))
       (let* ((url (format "~a/~a" dir name))
	      (buttons (dired-find-buttons req dir name url)))
	  (<DIRED-ICON-ENTRY>
	   req
	   dir
	   "text"
	   name
	   ident
	   (<DIRED-IMG-LINK> url url name)
	   (<DIRED-FILE-INFO> (make-file-name dir name))
	   buttons)))))

;*---------------------------------------------------------------------*/
;*    Plugins                                                          */
;*---------------------------------------------------------------------*/
(hop-load "dired/jpeg.scm")
(hop-load "dired/tarball.scm")
