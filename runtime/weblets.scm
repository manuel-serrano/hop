;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/weblets.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Sat Jan 28 15:38:06 2006 (eg)                     */
;*    Last change :  Sun May 21 09:10:17 2006 (serrano)                */
;*    Copyright   :  2004-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Weblets Management                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_weblets

   (include "compiler-macro.sch"
	    "xml.sch")
   
   (import __hop_param
	   __hop_types
	   __hop_xml
	   __hop_service
	   __hop_misc)
   
   (export  (find-weblets-in-directory ::string)
	    (autoload-weblets ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    autoload-weblets ...                                             */
;*---------------------------------------------------------------------*/
(define (autoload-weblets dirs)
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
   (define (maybe-autoload x)
      (let ((url (make-file-name (hop-service-base) (cadr (assoc 'name x))))
	    (path (cadr (assq 'weblet x)))
	    (autopred (assq 'autoload x)))
	 (if (pair? autopred)
	     (begin
		(hop-verb 2 "Setting autoload " path " on "
			  (cadr autopred) "\n")
		(autoload path (eval (cadr autopred))))
	     (install-autoload-prefix path url))))
   (for-each (lambda (dir)
		(for-each maybe-autoload (find-weblets-in-directory dir)))
	     dirs))

;*---------------------------------------------------------------------*/
;*    find-weblets-in-directory ...                                    */
;*---------------------------------------------------------------------*/
(define (find-weblets-in-directory dir)
   
   (define (get-weblet-details dir name)
      (let* ((infos (get-weblet-infos dir name))
	     (main (assoc 'main-file infos))
	     (weblet (make-file-path dir
				     name
				     (if main
					 (cadr main)
					 (string-append name ".hop")))))
	 (when (file-exists? weblet)
	    `((name ,name) (weblet ,weblet) ,@infos))))
   
   (let Loop ((files (directory->list dir))
	      (res '()))
      (if (null? files)
	  res
	  (let ((web (get-weblet-details dir (car files))))
	     (if web
		 (Loop (cdr files) (cons web res))
		 (Loop (cdr files) res))))))

;*---------------------------------------------------------------------*/
;*    get-weblet-infos ...                                             */
;*---------------------------------------------------------------------*/
(define (get-weblet-infos dir name)
   (let ((file  (make-file-path dir name (string-append name ".info"))))
      (if (file-exists? file)
	  (with-input-from-file file read)
	  '())))
