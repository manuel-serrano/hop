;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/weblets.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Sat Jan 28 15:38:06 2006 (eg)                     */
;*    Last change :  Fri Oct 26 17:34:56 2007 (serrano)                */
;*    Copyright   :  2004-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Weblets Management                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_weblets

   (include "xml.sch")
   
   (import __hop_param
	   __hop_types
	   __hop_xml
	   __hop_service
	   __hop_misc
	   __hop_read)
   
   (static  (class %autoload
	       (path::bstring read-only)
	       (pred::procedure read-only)
	       (hooks::pair-nil read-only (default '()))
	       (mutex::mutex (default (make-mutex)))
	       (loaded::bool (default #f))))
   
   (export  (find-weblets-in-directory ::string)
	    (reset-autoload!)
	    (get-autoload-directories::pair-nil)
	    (get-autoload-weblet-directories::pair-nil)
	    (install-autoload-weblets! ::pair-nil)
	    (autoload-prefix::procedure ::bstring)
	    (autoload ::bstring ::procedure . hooks)
	    (autoload-filter ::http-request)))

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
	    `((name ,name) (weblet ,weblet) (prefix ,prefix) ,@infos))))
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
;*    install-autoload-weblets! ...                                    */
;*---------------------------------------------------------------------*/
(define (install-autoload-weblets! dirs)
   
   (define (install-autoload-prefix path url)
      (hop-verb 2 "Setting autoload " path " on " url "\n")
      (autoload path (autoload-prefix url)))
   
   (define (is-service-base? req)
      (with-access::http-request req (path)
	 (let ((base (hop-service-base)))
	    (and (substring-at? base path 0)
		 (let ((lp (string-length path))
		       (lb (string-length base)))
		    (or (=fx lp lb)
			(and (=fx lp (+fx lb 1))
			     (char=? (string-ref path (-fx lp 1))
				     #\/))))))))
   
   (define (warn name opath npath)
      (when (> (bigloo-warning) 1)
	 (warning name
		  (format
		   "autoload already installed on:\n  ~a\nignoring:\n  ~a"
		   opath
		   npath))))

   (define (add-dashboard-applet! name icon svc)
      (hop-dashboard-weblet-applets-set!
       (cons (list name icon svc) (hop-dashboard-weblet-applets))))
   
   (define (maybe-autoload x)
      (let ((cname (assq 'name x)))
	 (if (pair? cname)
	     (let* ((name (cadr cname))
		    (prefix (cadr (assq 'prefix x)))
		    (url (make-url-name (hop-service-base) name))
		    (path (cadr (assq 'weblet x)))
		    (autopred (assq 'autoload x))
		    (rc (assq 'rc x))
		    (dashboard (assq 'dashboard x))
		    (opath (hashtable-get *weblet-table* name)))
		(if (pair? dashboard)
		    (for-each (lambda (d)
				 (match-case d
				    ((?i ?svc)
				     (let ((p (make-file-path prefix "etc" i)))
					(add-dashboard-applet! name p svc)))
				    ((and ?i (? string?))
				     (let ((p (make-file-path prefix "etc" i)))
					(add-dashboard-applet! name i "svc")))
				    (else
				     (warning 'autoload-weblets
					      "bad dashboard declaration"
					      d))))
			      (cdr dashboard))
		    (let ((icon (make-file-path prefix "etc" "dashboard.png")))
		       (when (file-exists? icon)
			  (add-dashboard-applet!
			   name
			   icon
			   (string-append url "/dashboard")))))
		(when (pair? rc) (eval (cadr rc)))
		(cond
		   ((string? opath)
		    (warn name opath path))
		   ((pair? autopred)
		    (when (cadr autopred)
		       (hashtable-put! *weblet-table* name path)
		       (hop-verb 2 "Setting autoload " path " on "
				 (cadr autopred) "\n")
		       (autoload path (eval (cadr autopred)))))
		   (else
		    (hashtable-put! *weblet-table* name path)
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
(define (autoload-prefix string)
   (let* ((p string)
	  (p/ (string-append string "/"))
	  (lp (string-length p)))
      (lambda (req)
	 (with-access::http-request req (path)
	    (let ((i (string-index path #\?))
		  (l (string-length path)))
	       (if (or (not i) (=fx i -1))
		   (and (substring-at? path p 0)
			(or (=fx l lp) (eq? (string-ref path lp) #\/)))
		   (and (>=fx i lp)
			(substring-at? path p 0 i)
			(or (=fx i lp)
			    (char=? (string-ref path lp) #\/)))))))))

;*---------------------------------------------------------------------*/
;*    *autoload-mutex* ...                                             */
;*---------------------------------------------------------------------*/
(define *autoload-mutex* (make-mutex))

;*---------------------------------------------------------------------*/
;*    *autoloads* ...                                                  */
;*---------------------------------------------------------------------*/
(define *autoloads* '())

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
		(error 'autoload-add! "Can't find autoload file" file)
		(let ((al (instantiate::%autoload
			     (path qfile)
			     (pred pred)
			     (hooks hooks))))
		   (set! *autoloads* (cons al *autoloads*))))))))

;*---------------------------------------------------------------------*/
;*    autoload-load! ...                                               */
;*---------------------------------------------------------------------*/
(define (autoload-load! req al)
   (with-access::%autoload al (path hooks loaded mutex)
      (mutex-lock! mutex)
      (unwind-protect
	 (unless loaded
	    (hop-verb 1 (hop-color req req " AUTOLOADING") ": " path "\n")
	    ;; load the autoloaded file
	    (hop-load-modified path)
	    ;; execute the hooks
	    (for-each (lambda (h) (h req)) hooks)
	    (set! loaded #t))
	 (mutex-unlock! mutex))))

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
		    (autoload-load! req (car al))
		    ;; add all the file associated with the autoload in
		    ;; the service path table (see __hop_service).
		    (service-etc-path-table-fill! (%autoload-path (car al)))
		    ;; remove the autoaload (once loaded)
		    (mutex-lock! *autoload-mutex*)
		    (set! *autoloads* (remq! (car al) *autoloads*))
		    (mutex-unlock! *autoload-mutex*)
		    #t)
		 (loop (cdr al)))))))
