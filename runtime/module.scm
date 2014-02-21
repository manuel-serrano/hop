;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/runtime/module.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 26 09:29:33 2009                          */
;*    Last change :  Fri Feb 21 13:41:44 2014 (serrano)                */
;*    Copyright   :  2009-14 Manuel Serrano                            */
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
   (match-case exp
      ((?- ?- . ?clauses)
       (let ((i (filter-map (lambda (c)
			       (match-case c
				  ((<TILDE> ??- :src (quote ?import) ??-) import)
				  (else #f)))
			    clauses)))
	  (if (pair? i)
	      (with-access::clientc (hop-clientc) (modulec)
		 (modulec i))
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
      (with-trace 1 "hop-module-resolver"
	 (trace-item "module=" module)
	 (trace-item "files=" files)
	 (trace-item "abase=" abase)
	 (if (pair? files)
	     (hop-module-afile-resolver module files abase)
	     (let ((rfiles (resolver module files '*)))
		(let ((r (if (pair? rfiles)
			     (hop-module-afile-resolver module rfiles abase)
			     (hop-module-path-resolver module "."))))
		   (trace-item "result=" r)
		   r))))))

;*---------------------------------------------------------------------*/
;*    hop-module-path-resolver ...                                     */
;*---------------------------------------------------------------------*/
(define (hop-module-path-resolver module dir)
   (let* ((name (string-append (symbol->string module) ".hop"))
	  (file (make-file-name dir name)))
      (if (file-exists? file)
	  (list file)
	  '())))

;*---------------------------------------------------------------------*/
;*    hop-module-afile-resolver ...                                    */
;*---------------------------------------------------------------------*/
(define (hop-module-afile-resolver module files abase)
   (apply append (filter-map (lambda (f) (url-resolver module f abase)) files)))
   
;*---------------------------------------------------------------------*/
;*    url-resolver ...                                                 */
;*---------------------------------------------------------------------*/
(define (url-resolver module url abase)
   (if (hz-package-filename? url)
       (hz-module-resolver module url)
       (list url)))

;*---------------------------------------------------------------------*/
;*    hz-module-resolver ...                                           */
;*---------------------------------------------------------------------*/
(define (hz-module-resolver module url)
   
   (define (resolve-in-dir dir)
      (with-trace 1 "resolve-in-dir"
	 (trace-item "module=" module)
	 (trace-item "url=" url)
	 (trace-item "dir=" dir)
	 (let ((afile (make-file-path dir ".afile"))
	       (abase (module-abase)))
	    (when (file-exists? afile)
	       (hop-load-afile dir)
	       (module-abase-set! dir))
	    (unwind-protect
	       (initial-resolver module '() dir)
	       (module-abase-set! abase)))))
   
   (define (resolve-default)
      (let ((dir (hz-download-to-cache url (hop-hz-repositories))))
	 (if (directory? dir)
	     (resolve-in-dir dir)
	     '())))

   (with-trace 1 "hz-module-resolver"
      (trace-item "module=" module)
      (trace-item "url=" url)
      (cond
	 ((hz-local-weblet-path url (get-autoload-directories)) => resolve-in-dir)
	 ((hz-cache-path url) => resolve-in-dir)
	 (else (resolve-default)))))
