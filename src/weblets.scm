;*=====================================================================*/
;*    serrano/prgm/project/hop/src/weblets.scm                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Sat Jan 28 15:38:06 2006 (eg)                     */
;*    Last change :  Wed Feb  8 08:52:04 2006 (serrano)                */
;*    Copyright   :  2004-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Weblets Management                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_weblets

   (library hop)

   (import hop_param)
   
   (export  (weblets-config-directory)
	    (get-weblet-infos ::string ::string)
	    (get-weblet-config ::string)
	    (get-weblet-config-value ::string ::symbol ::obj)
	    (find-weblets ::string)
	    (autoload-weblets))
   
   (eval    (export-exports)))

;; ----------------------------------------------------------------------
;; 	weblets-config-directory ...
;; ----------------------------------------------------------------------
(define (weblets-config-directory)
  (make-file-name (hop-rc-directory) "weblets-conf"))

;; ----------------------------------------------------------------------
;; 	get-weblet-infos ...
;; ----------------------------------------------------------------------
(define (get-weblet-infos dir name)
  (let ((file  (make-file-path dir name (string-append name ".info"))))
    (if (file-exists? file)
	(with-input-from-file file read)
	'((active #t)))))

;; ----------------------------------------------------------------------
;; 	get-weblet-config ...
;; ----------------------------------------------------------------------
(define (get-weblet-config name)
  (let ((file (make-file-name (weblets-config-directory)
			      (string-append name ".conf"))))
    (if (file-exists? file)
	(with-input-from-file file read)
	'())))

;; ----------------------------------------------------------------------
;; 	get-weblet-config-value ...
;; ----------------------------------------------------------------------
(define (get-weblet-config-value name key default)
  (let* ((conf (get-weblet-config name))
	 (v    (assoc key conf)))
    (if v (cadr v) default)))

;; ----------------------------------------------------------------------
;; 	find-weblets ...
;; ----------------------------------------------------------------------
(define (find-weblets dir)
  (define (get-weblet-details dir name)
    (let* ((conf   (get-weblet-config name))
	   (infos  (get-weblet-infos dir name))
	   (main   (assoc 'main-file infos))
	   (weblet (make-file-path dir
				   name
				   (if main
				       (cadr main)
				       (string-append name ".hop")))))
      (and (file-exists? weblet)
	   `((name ,name)
	     (weblet ,weblet)
	     ,@conf
	     ,@infos))))

  (let Loop ((files (directory->list dir))
	     (res '()))
    (if (null? files)
	res
	(let ((web (get-weblet-details dir (car files))))
	  (if web
	      (Loop (cdr files) (cons web res))
	      (Loop (cdr files) res))))))

;; ----------------------------------------------------------------------
;; 	autoload-weblets ...
;; ----------------------------------------------------------------------
(define (autoload-weblets)
  (define (maybe-autoload x)
    (let ((url    (make-file-name (hop-service-base) (cadr (assoc 'name x))))
	  (path   (cadr (assoc 'weblet x)))
	  (active (cadr (assoc 'active x))))
      (when active 
	(hop-verb 2 "Autoload " path " on " url "\n")
	(autoload path (autoload-prefix url)))))
    
  (for-each (lambda (dir)
	      (for-each maybe-autoload (find-weblets dir)))
	    (hop-autoload-directories)))

