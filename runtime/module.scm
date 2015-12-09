;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/module.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 26 09:29:33 2009                          */
;*    Last change :  Wed Dec  9 14:38:02 2015 (serrano)                */
;*    Copyright   :  2009-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP module resolver                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_module
   
   (import  __hop_misc
	    __hop_read
	    __hop_param
	    __hop_hz
	    __hop_types
	    __hop_clientc
	    __hop_weblets)
   
   (export  (hop-module-extension-handler ::obj)
	    (make-hop-module-resolver ::procedure)))

;*---------------------------------------------------------------------*/
;*    hop-module-extension-handler ...                                 */
;*---------------------------------------------------------------------*/
(define (hop-module-extension-handler exp)
   
   (define (find-location-dir exp)
      (when (epair? exp)
	 (match-case (cer exp)
	    ((at ?file . ?-) (dirname file)))))

   (match-case exp
      ((?- ?- . ?clauses)
       (let ((i (filter-map (lambda (c)
			       (match-case c
				  ((<TILDE> ??- :src (quote ?import) ??-) import)
				  (else #f)))
			    clauses)))
	  (if (pair? i)
	      (with-access::clientc (hop-clientc) (modulec)
		 (with-trace 'module "clientc-modulec"
		    (trace-item "i=" i)
		    (trace-item "dir=" (find-location-dir exp))
		    (modulec i (find-location-dir exp))))
	      i)))
      (else
       '())))

;*---------------------------------------------------------------------*/
;*    initial-resolver ...                                             */
;*---------------------------------------------------------------------*/
(define initial-resolver #f)

;*---------------------------------------------------------------------*/
;*    make-hop-module-resolver ...                                     */
;*---------------------------------------------------------------------*/
(define (make-hop-module-resolver resolver)
   (set! initial-resolver resolver)
   (lambda (module files abase)
      (with-trace 'module "hop-module-resolver"
	 (trace-item "module=" module)
	 (trace-item "files=" files)
	 (trace-item "abase=" abase)
	 (if (pair? files)
	     (hop-module-afile-resolver module
		(if (string? abase)
		    (map (lambda (f) (make-file-name abase f)) files)
		    files))
	     (let ((rfiles (resolver module files (find-afile abase))))
		(trace-item "rfiles=" rfiles " abase=" abase)
		(if (pair? rfiles)
		    (hop-module-afile-resolver module rfiles)
		    (let ((f (or (hop-module-path-resolver module ".")
				 (hop-module-path-resolver module abase))))
		       (if f (list f) '()))))))))

;*---------------------------------------------------------------------*/
;*    find-afile ...                                                   */
;*---------------------------------------------------------------------*/
(define (find-afile dir)
   (when (string? dir)
      (let loop ((dir dir))
	 (if (file-exists? (make-file-name dir ".afile"))
	     dir
	     (let ((parent (dirname dir)))
		(unless (string=? parent dir)
		   (if (string=? parent ".")
		       (when (file-exists? ".afile")
			  ".")
		       (find-afile parent))))))))

;*---------------------------------------------------------------------*/
;*    hop-module-path-resolver ...                                     */
;*---------------------------------------------------------------------*/
(define (hop-module-path-resolver module dir)
   (let* ((name (string-append (symbol->string module) ".hop"))
	  (file (if (eq? dir '*) name (make-file-name dir name))))
      (when (file-exists? file)
	  file)))

;*---------------------------------------------------------------------*/
;*    hop-module-afile-resolver ...                                    */
;*---------------------------------------------------------------------*/
(define (hop-module-afile-resolver module files)
   (apply append (filter-map (lambda (f) (url-resolver module f)) files)))
   
;*---------------------------------------------------------------------*/
;*    url-resolver ...                                                 */
;*---------------------------------------------------------------------*/
(define (url-resolver module url)
   (if (hz-package-filename? url)
       (hz-module-resolver module url)
       (list url)))

;*---------------------------------------------------------------------*/
;*    hz-module-resolver ...                                           */
;*---------------------------------------------------------------------*/
(define (hz-module-resolver module url)
   
   (define (resolve-in-dir dir)
      (with-trace 'module "resolve-in-dir"
	 (trace-item "module=" module)
	 (trace-item "url=" url)
	 (trace-item "dir=" dir)
	 (hop-load-afile dir)
	 (initial-resolver module '() dir)))
   
   (define (resolve-default)
      (let ((dir (hz-download-to-cache url (hop-hz-repositories))))
	 (if (directory? dir)
	     (resolve-in-dir dir)
	     '())))

   (with-trace 'module "hz-module-resolver"
      (trace-item "module=" module)
      (trace-item "url=" url)
      (cond
	 ((hz-local-weblet-path url (get-autoload-directories)) => resolve-in-dir)
	 ((hz-cache-path url) => resolve-in-dir)
	 (else (resolve-default)))))
