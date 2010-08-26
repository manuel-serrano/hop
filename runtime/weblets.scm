;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/runtime/weblets.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Sat Jan 28 15:38:06 2006 (eg)                     */
;*    Last change :  Fri Aug 20 12:12:08 2010 (serrano)                */
;*    Copyright   :  2004-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Weblets Management                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_weblets

   (include "xml.sch")
   
   (import __hop_configure
	   __hop_param
	   __hop_types
	   __hop_xml
	   __hop_service
	   __hop_misc
	   __hop_read)
   
   (static  (abstract-class %autoload
	       (path::bstring read-only)
	       (pred::procedure read-only))

	    (class %autoload-file::%autoload
	       (loaded::bool (default #f))
	       (hooks::pair-nil read-only (default '()))
	       (mutex::mutex (default (make-mutex "autoload-file"))))

	    (class %autoload-incompatible::%autoload
	       (name::bstring read-only)
	       (info::pair-nil read-only)))
   
   (export  (find-weblets-in-directory ::bstring)
	    (reset-autoload!)
	    (get-autoload-directories::pair-nil)
	    (get-autoload-weblet-directories::pair-nil)
	    (hop-load-hz ::bstring)
	    (hop-load-weblet ::bstring)
	    (install-autoload-weblets! ::pair-nil)
	    (autoload-prefix::procedure ::bstring)
	    (autoload ::bstring ::procedure . hooks)
	    (autoload-filter ::http-request)
	    (autoload-force-load! ::bstring)))

;*---------------------------------------------------------------------*/
;*    find-weblets-in-directory ...                                    */
;*---------------------------------------------------------------------*/
(define (find-weblets-in-directory dir)
   (define (get-weblet-details dir name)
      (let* ((infos (get-weblet-info dir name))
	     (main (assoc 'main-file infos))
	     (prefix (make-file-name dir name))
	     (weblet (make-file-name prefix 
				     (if main
					 (cadr main)
					 (string-append name ".hop")))))
	 (when (file-exists? weblet)
	    `((weblet ,weblet) (prefix ,prefix) (name ,name) ,@infos))))
   (let loop ((files (directory->list dir))
	      (res '()))
      (if (null? files)
	  res
	  (let ((web (get-weblet-details dir (car files))))
	     (if web
		 (loop (cdr files) (cons web res))
		 (loop (cdr files) res))))))

;*---------------------------------------------------------------------*/
;*    get-weblet-info ...                                              */
;*---------------------------------------------------------------------*/
(define (get-weblet-info dir name)
   (let ((file (make-file-path dir name "etc" "weblet.info")))
      (if (file-exists? file)
	  (with-input-from-file file read)
	  (let ((f (make-file-path dir name "etc" (string-append name ".info"))))
	     (if (file-exists? f)
		 (with-input-from-file f read)
		 '())))))

;*---------------------------------------------------------------------*/
;*    *weblet-table* ...                                               */
;*---------------------------------------------------------------------*/
(define *weblet-table* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    *weblet-lock* ...                                                */
;*---------------------------------------------------------------------*/
(define *weblet-lock* (make-mutex "weblets"))

;*---------------------------------------------------------------------*/
;*    *weblet-autoload-dirs* ...                                       */
;*---------------------------------------------------------------------*/
(define *weblet-autoload-dirs* '())

;*---------------------------------------------------------------------*/
;*    reset-autoload! ...                                              */
;*---------------------------------------------------------------------*/
(define (reset-autoload!)
   (install-autoload-weblets! *weblet-autoload-dirs*))

;*---------------------------------------------------------------------*/
;*    get-autoload-directories ...                                     */
;*---------------------------------------------------------------------*/
(define (get-autoload-directories)
   *weblet-autoload-dirs*)

;*---------------------------------------------------------------------*/
;*    hop-load-hz ...                                                  */
;*---------------------------------------------------------------------*/
(define (hop-load-hz path)
   (let ((p (open-input-gzip-file path)))
      (unwind-protect
	 (let* ((tmp (make-file-name (os-tmp) "hop"))
		(file (car (untar p :directory tmp)))
		(base (substring file
				 (+fx (string-length tmp) 1)
				 (string-length file)))
		(dir (dirname base))
		(name (if (string=? dir ".") base dir))
		(src (make-file-path tmp name (string-append name ".hop"))))
	    (hop-load-weblet src))
	 (close-input-port p))))

;*---------------------------------------------------------------------*/
;*    hop-load-weblet ...                                              */
;*---------------------------------------------------------------------*/
(define (hop-load-weblet path)
   (let* ((dir (dirname path))
	  (name (basename (prefix path))))
      (if (file-exists? path)
	  (begin
	     (hop-load path)
	     (let* ((winfoname (make-file-path name "etc" "weblet.info"))
		    (winfo (if (file-exists? winfoname)
			       (with-input-from-file winfoname read)
			       '()))
		    (url (string-append "/hop/" name)))
		(install-weblet-dashboard! name dir winfo url)))
	  (error "hop-load-weblet" "Cannot find HOP source" path))))

;*---------------------------------------------------------------------*/
;*    install-weblet-dashboard! ...                                    */
;*---------------------------------------------------------------------*/
(define (install-weblet-dashboard! name dir winfo url)
   
   (define (add-dashboard-applet! name icon svc)
      (unless (pair? (assoc name (hop-dashboard-weblet-applets)))
	 (hop-dashboard-weblet-applets-set!
	  (cons (list name icon svc) (hop-dashboard-weblet-applets)))))

   (unless (member name (hop-dashboard-weblet-disabled-applets))
      ;; the user does not want of this weblet
      (let ((dashboard (assq 'dashboard winfo)))
	 ;; dashboard declaration
	 (if (pair? dashboard)
	     ;; a customized dashboard
	     (for-each (lambda (d)
			  (match-case d
			     ((?i ?svc)
			      (let ((p (make-file-path dir "etc" i)))
				 (add-dashboard-applet! dir p svc)))
			     ((and ?i (? string?))
			      (let* ((p (make-file-path dir "etc" i))
				     (svc (string-append url "/dashboard")))
				 (add-dashboard-applet! name i svc)))
			     (else
			      (warning 'autoload-weblets
				       "bad dashboard declaration"
				       d))))
		       (cdr dashboard))
	     ;; is there a dashboard icon for a regular an applet?
	     (let ((icon (make-file-path dir "etc" "dashboard.png")))
		(when (file-exists? icon)
		   (let ((svc (string-append url "/dashboard")))
		      (add-dashboard-applet! name icon svc))))))))

;*---------------------------------------------------------------------*/
;*    install-autoload-weblets! ...                                    */
;*---------------------------------------------------------------------*/
(define (install-autoload-weblets! dirs)

   (define (install-autoload-prefix path url)
      (hop-verb 4 (hop-color 1 "" "AUTOLOAD") " " path " for " url "\n")
      (autoload path (autoload-prefix url)))
   
   (define (warn name opath npath)
      (when (> (bigloo-warning) 1)
	 (warning name
		  (format
		   "autoload already installed on:\n  ~a\nignoring:\n  ~a"
		   opath
		   npath))))

   (define (hop-compatible? x)
      
      (define (cmpversion version cmp)
	 (or (not (pair? version))
	     (not (string? (cadr version)))
	     (string=? (cadr version) "")
	     (cmp (hop-version) (cadr version))))

      (and (cmpversion (assq 'minhop x) string>=?)
	   (cmpversion (assq 'maxhop x) string<=?)))

   (define (make-incompatible-url name x)
      (make-url-name (hop-service-base)
		     (format "~a/incompatible?minhop=~a&maxhop=~a"
			     name
			     (let ((c (assq 'minhop x)))
				(if (pair? c) (cadr c) "*"))
			     (let ((c (assq 'maxhop x)))
				(if (pair? c) (cadr c) "*")))))

   (define (warn-incompatible name x)
      (warning name
	       (format " -- Hop ~s incompatible with min: ~a, max: ~a"
		       (hop-version)
		       (let ((c (assq 'minhop x)))
			  (if (pair? c) (cadr c) "*"))
		       (let ((c (assq 'maxhop x)))
			  (if (pair? c) (cadr c) "*")))))
   
   (define (maybe-autoload x)
      (let ((cname (assq 'name x)))
	 (if (pair? cname)
	     (let* ((name (cadr cname))
		    (prefix (cadr (assq 'prefix x)))
		    (svc (let ((c (assq 'service x)))
			    (if (and (pair? c) (symbol? (cadr c)))
				(symbol->string (cadr c))
				name)))
		    (url (make-url-name (hop-service-base) svc))
		    (path (cadr (assq 'weblet x)))
		    (autopred (assq 'autoload x))
		    (rc (assq 'rc x))
		    (opath (hashtable-get *weblet-table* svc)))
		;; dashboard setup
		(install-weblet-dashboard! name prefix x url)
		;; rc setup
		(when (pair? rc) (eval (cadr rc)))
		;; autoload per say
		(cond
		   ((string? opath)
		    (warn name opath path))
		   ((not (hop-compatible? x))
		    (warn-incompatible name x)
		    (autoload-incompatible path (autoload-prefix url) name x))
		   ((pair? autopred)
		    (when (cadr autopred)
		       (hashtable-put! *weblet-table* name path)
		       (hop-verb 2 "Setting autoload " path " on "
				 (cadr autopred) "\n")
		       (autoload path (eval (cadr autopred)))))
		   (else
		    (hashtable-put! *weblet-table* name svc)
		    (install-autoload-prefix path url))))
	     (warning 'autoload-weblets
		      "Illegal weblet etc/weblet.info file"
		      x))))
   
   ;; since autoload are likely to be installed before the scheduler
   ;; starts, the lock above is unlikely to be useful.
   (with-lock *weblet-lock*
      (lambda ()
	 (set! *weblet-autoload-dirs* dirs)
	 (for-each (lambda (dir)
		      (for-each maybe-autoload (find-weblets-in-directory dir)))
		   dirs))))

;*---------------------------------------------------------------------*/
;*    autoload-prefix ...                                              */
;*    -------------------------------------------------------------    */
;*    Builds a predicate that matches iff the request path is a        */
;*    prefix of STRING.                                                */
;*---------------------------------------------------------------------*/
(define (autoload-prefix path)
   (let ((lp (string-length path)))
      (lambda (req)
	 (with-access::http-request req (abspath)
	    (and (substring-at? abspath path 0)
		 (let ((la (string-length abspath)))
		    (or (=fx la lp) (char=? (string-ref abspath lp) #\/))))))))

;*---------------------------------------------------------------------*/
;*    *autoload-mutex* ...                                             */
;*---------------------------------------------------------------------*/
(define *autoload-mutex* (make-mutex "autoload"))

;*---------------------------------------------------------------------*/
;*    *autoloads* ...                                                  */
;*---------------------------------------------------------------------*/
(define *autoloads* '())
(define *autoloads-loaded* '())

;*---------------------------------------------------------------------*/
;*    get-autoload-weblet-directories ...                              */
;*---------------------------------------------------------------------*/
(define (get-autoload-weblet-directories)
   (map (lambda (o)
	   (dirname (%autoload-path o)))
	*autoloads*))

;*---------------------------------------------------------------------*/
;*    autoload ...                                                     */
;*---------------------------------------------------------------------*/
(define (autoload file pred . hooks)
   (with-lock *autoload-mutex*
      (lambda ()
	 (let ((qfile (find-file/path file (hop-path))))
	    (if (not (and (string? qfile) (file-exists? qfile)))
		(error "autoload-add!" "Can't find autoload file" file)
		(let ((al (instantiate::%autoload-file
			     (path qfile)
			     (pred pred)
			     (hooks hooks))))
		   (set! *autoloads* (cons al *autoloads*))))))))

;*---------------------------------------------------------------------*/
;*    autoload-incompatible ...                                        */
;*---------------------------------------------------------------------*/
(define (autoload-incompatible file pred name info)
   (with-lock *autoload-mutex*
      (lambda ()
	 (let ((qfile (find-file/path file (hop-path))))
	    (if (not (and (string? qfile) (file-exists? qfile)))
		(error "autoload-add!" "Can't find autoload file" file)
		(let ((al (instantiate::%autoload-incompatible
			     (path qfile)
			     (pred pred)
			     (name name)
			     (info info))))
		   (set! *autoloads* (cons al *autoloads*))))))))

;*---------------------------------------------------------------------*/
;*    autoload-load! ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (autoload-load! a::%autoload req))

;*---------------------------------------------------------------------*/
;*    autoload-load! ::%autoload-file ...                              */
;*---------------------------------------------------------------------*/
(define-method (autoload-load! a::%autoload-file req)
   (with-access::%autoload-file a (path hooks loaded mutex)
      (mutex-lock! mutex)
      (unwind-protect
	 (unless loaded
	    (hop-verb 1 (hop-color req req " AUTOLOADING") ": " path "\n")
	    ;; load the autoloaded file
	    (with-handler
	       (lambda (e)
		  (exception-notify e)
		  (raise
		   (instantiate::&hop-autoload-error
		      (proc "autoload-load!")
		      (msg "Cannot autoload file")
		      (obj path))))
	       (hop-load-modified path))
	    ;; execute the hooks
	    (for-each (lambda (h) (h req)) hooks)
	    (set! loaded #t))
	 (mutex-unlock! mutex))))

;*---------------------------------------------------------------------*/
;*    autoload-load! ::%autoload-incompatible ...                      */
;*---------------------------------------------------------------------*/
(define-method (autoload-load! a::%autoload-incompatible req)
   (with-access::%autoload-incompatible a (name info)
      (let* ((min (assq 'minhop info))
	     (minhop (if (pair? min) (cadr min) "*"))
	     (max (assq 'max info))
	     (maxhop (if (pair? max) (cadr max) "*")))
	 (raise
	  (instantiate::&hop-autoload-error
	     (proc name)
	     (msg (format "Hop \"~a\" cannot run this weblet" (hop-version)))
	     (obj (format "min-hop: ~a, max-hop: ~a" minhop maxhop)))))))

;*---------------------------------------------------------------------*/
;*    autoload-filter ...                                              */
;*    -------------------------------------------------------------    */
;*    This filter is no longer registered as is. It is now invoked by  */
;*    the service-filter, when no service matches a HOP url.           */
;*---------------------------------------------------------------------*/
(define (autoload-filter req)
   (mutex-lock! *autoload-mutex*)
   (let loop ((al *autoloads*))
      (if (null? al)
	  (begin
	     (mutex-unlock! *autoload-mutex*)
	     #f)
	  (with-access::%autoload (car al) (pred)
	     (if (pred req)
		 (begin
		    (mutex-unlock! *autoload-mutex*)
		    ;; the autoload cannot be removed until read, otherwise
		    ;; parallel requests to the autoloaded service will raise
		    ;; a service not found error
		    (autoload-load! (car al) req)
		    ;; add all the file associated with the autoload in
		    ;; the service path table (see __hop_service).
		    (service-etc-path-table-fill! (%autoload-path (car al)))
		    ;; remove the autoaload (once loaded)
		    (mutex-lock! *autoload-mutex*)
		    (set! *autoloads* (remq! (car al) *autoloads*))
		    (set! *autoloads-loaded* (cons (car al) *autoloads-loaded*))
		    (mutex-unlock! *autoload-mutex*)
		    #t)
		 (loop (cdr al)))))))

;*---------------------------------------------------------------------*/
;*    autoload-loaded? ...                                             */
;*---------------------------------------------------------------------*/
(define (autoload-loaded? req)
   (mutex-lock! *autoload-mutex*)
   (let loop ((al *autoloads-loaded*))
      (cond
	 ((null? al)
	  (mutex-unlock! *autoload-mutex*)
	  #f)
	 (((%autoload-pred (car al)) req)
	  (mutex-unlock! *autoload-mutex*)
	  #t)
	 (else
	  (loop (cdr al))))))

;*---------------------------------------------------------------------*/
;*    autoload-force-load! ...                                         */
;*---------------------------------------------------------------------*/
(define (autoload-force-load! path)
   (let ((req (instantiate::http-server-request
		 (localclientp #t)
		 (port (hop-port))
		 (path path)
		 (abspath path))))
      (or (autoload-filter req) (autoload-loaded? req))))

