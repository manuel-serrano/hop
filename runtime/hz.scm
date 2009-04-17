;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/hz.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov 19 05:30:17 2007                          */
;*    Last change :  Fri Apr 17 10:40:56 2009 (serrano)                */
;*    Copyright   :  2007-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Functions for dealing with HZ packages.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hz

   (import  __hop_param)

   (export (hz-package-filename? ::bstring)
	   (hz-package-name-parse ::bstring)
	   (hz-package-url-parse ::bstring)
	   (hz-package-info ::bstring)
	   (hz-download-to-cache ::bstring)))

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
          (index (string-index-right n #\-))
          (vdot (string-index-right n #\.)))
      (cond
         ((not index)
          (error 'hz-package-name-parse "Illegal hz-package name" name))
         ((and (fixnum? vdot) (>fx vdot index))
          ;; a hz-package without release
          (let* ((version (substring n (+fx 1 index) (string-length n)))
                 (base (substring n 0 index)))
	     (values base version)))
         (else
          ;; a hz-package with release
          (let ((vindex (string-index-right n #\- (-fx index 1))))
             (if (not vindex)
                 (error 'hz-package-name-parse "Illegal hz-package name" name)
                 (let* ((version (substring n (+fx 1 vindex) (string-length n)))
                        (base (substring n 0 vindex)))
		    (values base version))))))))

;*---------------------------------------------------------------------*/
;*    hz-package-name-parse ...                                        */
;*    -------------------------------------------------------------    */
;*    Parses a hz-package file name and returns the base name, the     */
;*    version number, and release number.                              */
;*    The syntax of a hz-package name is:                              */
;*       .*-[0-9]+.0-9]+.[0-9]+(-{pre,rc}?[0-9]+)?.hz                  */
;*---------------------------------------------------------------------*/
(define (hz-package-name-parse name)
   (unless (hz-package-filename? name)
      (error 'hz-package-name-parse "Illegal hz-package name" name))
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
;*    hz-download-to-cache ...                                         */
;*---------------------------------------------------------------------*/
(define (hz-download-to-cache url)
   (multiple-value-bind (_ _ host port abspath)
      (url-parse url)
      (multiple-value-bind (base version)
	 (hz-package-name-parse abspath)
	 (let* ((dest (make-file-path
		       (hop-rc-directory) "cache" (hop-api-cache)))
		(dir (if host
			 (make-file-name dest
					 (format "~a_~a~a"
						 host port
						 (prefix (basename abspath))))
			 (make-file-name dest (prefix (basename abspath)))))
		(url (if (string? (hop-hz-local-repository))
			 (let ((f (make-file-name (hop-hz-local-repository)
						  (basename abspath))))
			    (if (file-exists? f)
				f
				url))
			 url)))
	    (unless (directory? dir)
	       (call-with-input-file url
		  (lambda (iport)
		     (make-directories dir)
		     (let* ((p (open-input-gzip-port iport)))
			(unwind-protect
			   (untar p :directory dir)
			   (close-input-port iport))))))
	    (make-file-name dir base)))))
