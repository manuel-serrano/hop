;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/runtime/hz.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov 19 05:30:17 2007                          */
;*    Last change :  Tue Jun 12 07:09:45 2012 (serrano)                */
;*    Copyright   :  2007-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Functions for dealing with HZ packages.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hz

   (library web)
   
   (import  __hop_param
	    __hop_misc)

   (export (hz-package-filename? ::bstring)
	   (hz-package-name-parse ::bstring)
	   (hz-package-url-parse ::bstring)
	   (hz-package-info ::bstring)
	   (hz-cache-path ::bstring  #!key (dir (make-cache-name "api")))
	   (hz-download-to-cache ::bstring #!key (dir (make-cache-name "api")))
	   (hz-resolve-name ::bstring ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    hz-package-filename? ...                                         */
;*---------------------------------------------------------------------*/
(define (hz-package-filename? name)
   (string-suffix-ci? (hop-hz-package-suffix) name))

;*---------------------------------------------------------------------*/
;*    hz-package-sans-suffix ...                                       */
;*---------------------------------------------------------------------*/
(define (hz-package-sans-suffix name)
   (substring name
              0
              (-fx (string-length name)
                   (+fx 1 (string-length (hop-hz-package-suffix))))))

;*---------------------------------------------------------------------*/
;*    hz-package-name-parse-sans-url ...                               */
;*---------------------------------------------------------------------*/
(define (hz-package-name-parse-sans-url name)
   (let* ((n (hz-package-sans-suffix (basename name)))
	  (l (string-length n))
          (index (string-index-right n #\-))
          (vdot (string-index-right n #\.)))
      (cond
         ((not index)
          (error "hz-package-name-parse" "Illegal hz-package name" name))
         ((and (fixnum? vdot) (>fx vdot index))
          ;; a hz-package without release
          (let* ((version (substring n (+fx 1 index) l))
                 (base (substring n 0 index)))
	     (values base version)))
	 ((and (=fx index (-fx l 2)) (char=? (string-ref n (-fx l 1)) #\*))
	  (values (substring n 0 index) "*"))
         (else
          ;; a hz-package with release
          (let ((vindex (string-index-right n #\- (-fx index 1))))
             (if (not vindex)
                 (error "hz-package-name-parse" "Illegal hz-package name" name)
                 (let* ((version (substring n (+fx 1 vindex) (string-length n)))
                        (base (substring n 0 vindex)))
		    (values base version))))))))

;*---------------------------------------------------------------------*/
;*    hz-package-pattern->regexp ...                                   */
;*---------------------------------------------------------------------*/
(define (hz-package-pattern->regexp url)
   (multiple-value-bind (base version)
      (hz-package-name-parse-sans-url url)
      (cond
	 ((pregexp-match "([0-9]+)\\.([0-9]+)\\.[*]" version)
	  =>
	  (lambda (m) (format "~a-~a\\.~a\\..*" base (cadr m) (caddr m))))
	 ((pregexp-match "([0-9]+)\\.[*]" version)
	  =>
	  (lambda (m) (format "~a-~a\\..*" base (cadr m))))
	 ((string=? "*" version)
	  => 
	  (lambda (m) (format "~a-.+" base)))
	 (else
	  (pregexp-quote url)))))

;*---------------------------------------------------------------------*/
;*    hz-package-name-parse ...                                        */
;*    -------------------------------------------------------------    */
;*    Parses a hz-package file name and returns the base name, the     */
;*    version number, and release number.                              */
;*    The syntax of a hz-package name is:                              */
;*       .*-[0-9]+.[0-9]+.[0-9]+(-{pre,rc}?[0-9]+)?.hz                 */
;*---------------------------------------------------------------------*/
(define (hz-package-name-parse name)
   (unless (hz-package-filename? name)
      (error "hz-package-name-parse" "Illegal hz-package name" name))
   (let ((varg (string-index-right name "?&")))
      (if varg
	  (let ((name (substring name (+fx varg 1) (string-length name))))
	     (hz-package-name-parse name))
	  (hz-package-name-parse-sans-url name))))

;*---------------------------------------------------------------------*/
;*    hz-package-url-parse ...                                         */
;*---------------------------------------------------------------------*/
(define (hz-package-url-parse url)
   (multiple-value-bind (scheme userinfo host port path)
      (url-parse url)
      (hz-package-name-parse (basename path))))

;*---------------------------------------------------------------------*/
;*    hz-package-info ...                                              */
;*---------------------------------------------------------------------*/
(define (hz-package-info url)
   (multiple-value-bind (base version)
      (hz-package-url-parse url)
      (let ((info (make-file-path base "etc" "weblet.info"))
	    (ip (open-input-gzip-file url)))
	 (unwind-protect
	    (untar ip :file info)
	    (close-input-port ip)))))

;*---------------------------------------------------------------------*/
;*    abspath->filename ...                                            */
;*---------------------------------------------------------------------*/
(define (abspath->filename abspath)
   (let ((i (string-index abspath #\?)))
      (if i
	  (let ((j (string-index abspath #\= i)))
	     (if j
		 (substring abspath (+fx 1 j) (string-length abspath))
		 abspath))
	  abspath)))

;*---------------------------------------------------------------------*/
;*    download-url ...                                                 */
;*---------------------------------------------------------------------*/
(define (download-url url dir)
   (with-handler
      (lambda (e)
	 (delete-directory dir)
	 (error "hz" "Cannot find HZ package" url))
      (call-with-input-file url
	 (lambda (iport)
	    (make-directories dir)
	    (let* ((p (open-input-gzip-port iport)))
	       (unwind-protect
		  (untar p :directory dir)
		  (close-input-port iport)))))))

;*---------------------------------------------------------------------*/
;*    hz-server-resolve-name ...                                       */
;*---------------------------------------------------------------------*/
(define (hz-server-resolve-name url)
   (let ((u (string-append (hop-hz-server) "/hop/weblets/resolve?weblet=" url)))
      (call-with-input-file u
	 (lambda (p)
	    (let ((s (read-string p)))
	       (if (=fx (string-length s) 0)
		   (error "hz" "Cannot resolve name" url)
		   (basename s)))))))

;*---------------------------------------------------------------------*/
;*    hz-cache-path ...                                                */
;*---------------------------------------------------------------------*/
(define (hz-cache-path url #!key (dir (make-cache-name "api")))
   (let ((cache (hz-resolve-name url (list dir))))
      (when (directory? cache)
	 (multiple-value-bind (base version)
	    (hz-package-name-parse (basename url))
	    (let ((cachedir (make-file-name cache base)))
	       (when (directory? cachedir)
		  cachedir))))))

;*---------------------------------------------------------------------*/
;*    hz-download-to-cache ...                                         */
;*---------------------------------------------------------------------*/
(define (hz-download-to-cache url #!key (dir (make-cache-name "api")))
   (or (hz-cache-path url :dir dir)
       (multiple-value-bind (scheme _ host port abspath)
	  (url-parse url)
	  (let ((apath (abspath->filename abspath)))
	     (multiple-value-bind (base version)
		(hz-package-name-parse apath)
		(let* ((dest dir)
		       (dir (if host
				(make-file-name dest
				   (format "~a_~a~a"
				      host port
				      (prefix (basename apath))))
				(make-file-name dest (prefix (basename apath))))))
		   (cond
		      ((directory? dir)
		       (make-file-name dir base))
		      ((file-exists? url)
		       (download-url url dir)
		       (make-file-name dir base))
		      ((not (string=? scheme "*"))
		       (error "hz" "Cannot find module" url))
		      (else
		       (let* ((name (hz-server-resolve-name url))
			      (dir (make-file-name dest (prefix name)))
			      (url (string-append
				      (hop-hz-server)
				      "/hop/weblets/download?weblet=" name)))
			  (download-url url dir)
			  (make-file-name dir base))))))))))

;*---------------------------------------------------------------------*/
;*    hz-resolve-name ...                                              */
;*    -------------------------------------------------------------    */
;*    This function accepts as parameter a HZ specification and        */
;*    returns an actual local file name that contains that HZ          */
;*    package. This function may download from the web the package.    */
;*---------------------------------------------------------------------*/
(define (hz-resolve-name url path)
   (if (or (string-prefix? "http://" url)
	   (string-prefix? "https://" url))
       url
       (or ((hop-hz-resolver) url)
	   (let ((regexp (hz-package-pattern->regexp url)))
	      (or (find-val (lambda (p) (hz/repository regexp p)) path)
		  url)))))

;*---------------------------------------------------------------------*/
;*    hz/repository ...                                                */
;*---------------------------------------------------------------------*/
(define (hz/repository regexp dir)
   
   (define (find dir dir->files)
      (let ((files (sort (dir->files dir)
			 (lambda (f1 f2)
			    (>fx (string-natural-compare3 f1 f2) 0)))))
	 (find-val (lambda (f)
		      (when (pregexp-match regexp f)
			 (make-file-path dir f)))
		   files)))
   
   (cond
      ((not (string? dir))
       #f)
      ((directory? dir)
       (find dir directory->list))
      (else
       (multiple-value-bind (_ _ host _ _)
	  (url-parse dir)
	  (when (string? host)
	     (find dir webdav-directory->list))))))

;*---------------------------------------------------------------------*/
;*    find-val ...                                                     */
;*---------------------------------------------------------------------*/
(define (find-val pred lst)
   (when (pair? lst)
      (let ((v (pred (car lst))))
	 (or v (find-val pred (cdr lst))))))
      
       
