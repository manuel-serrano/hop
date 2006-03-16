;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/weblets.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Sat Jan 28 15:38:06 2006 (eg)                     */
;*    Last change :  Thu Mar 16 08:38:37 2006 (serrano)                */
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

   
   (export  (weblets-config-directory)
	    (get-weblet-infos ::string ::string)
	    (get-weblet-config ::string)
	    (get-weblet-config-value ::string ::symbol ::obj)
	    (find-weblets ::string)
	    (autoload-weblets ::pair-nil)

	    (<WEBLET-ABOUT> . args)))

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
	'())))

;; ----------------------------------------------------------------------
;; 	get-weblet-config ...
;; ----------------------------------------------------------------------
(define (get-weblet-config name)
  (let ((file (make-file-name (weblets-config-directory)
			      (string-append name ".conf"))))
    (if (file-exists? file)
	(with-input-from-file file read)
	'((active #t)))))

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
(define (autoload-weblets dirs)
  (define (maybe-autoload x)
    (let ((url      (make-file-name (hop-service-base) (cadr (assoc 'name x))))
	  (path     (cadr (assq 'weblet x)))
	  (active   (cadr (assq 'active x)))
	  (autopred (assq 'autoload x)))
      (when active
	(if (pair? autopred)
	    (begin
	       (hop-verb 2 "Autoload " path " on " (cadr autopred) "\n")
	       (autoload path (eval (cadr autopred))))
	    (begin
	       (hop-verb 2 "Autoload " path " on " url "\n")
	       (autoload path (autoload-prefix url)))))))
  (for-each (lambda (dir)
	      (for-each maybe-autoload (find-weblets dir)))
	    dirs))

;; ----------------------------------------------------------------------
;; 	WEBLET-ABOUT ...
;; ----------------------------------------------------------------------
(define-xml-compound <WEBLET-ABOUT> ((id #unspecified string)
				     (title #f)
				     (subtitle #f)
				     (version #f)
				     (icon #f)
				     body)
   
   (cons* 
    (<TABLE> :width "100%" 
	     (<TR>
		(<TD> :valign "top" :align "left"
		      (<IMG> :src icon)
		      (<TD> :valign "top" :align "right"
			    (when title (<H2> title))
			    (when subtitle (<H3> subtitle))
			    (when version
			       (<H4> (format "(version ~A)" version)))))))
    body))
