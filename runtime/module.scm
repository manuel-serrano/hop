;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/runtime/module.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 26 09:29:33 2009                          */
;*    Last change :  Wed Jan  4 08:14:37 2012 (serrano)                */
;*    Copyright   :  2009-12 Manuel Serrano                            */
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
	    __hop_clientc)
   
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
   (lambda (module abase)
      (let ((files (resolver module '*)))
	 (if (pair? files)
	     (hop-module-afile-resolver module files)
	     (hop-module-path-resolver module ".")))))

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
(define (hop-module-afile-resolver module files)
   (apply append (filter-map (lambda (f) (url-resolver f module)) files)))
   
;*---------------------------------------------------------------------*/
;*    url-resolver ...                                                 */
;*---------------------------------------------------------------------*/
(define (url-resolver url module)
   (if (hz-package-filename? url)
       (hz-module-resolver module url)
       (list url)))

;*---------------------------------------------------------------------*/
;*    hz-module-resolver ...                                           */
;*---------------------------------------------------------------------*/
(define (hz-module-resolver module url)
   
   (define (resolve-in-dir dir)
      (let ((afile (make-file-path dir ".afile"))
	    (abase (module-abase)))
	 (when (file-exists? afile)
	    (module-load-access-file afile)
	    (module-abase-set! dir))
	 (unwind-protect
	    (initial-resolver module dir)
	    (module-abase-set! abase))))
   
   (define (resolve-default)
      (let* ((url (hz-resolve-name url (hop-hz-repositories)))
	     (dir (hz-download-to-cache url)))
	 (if (directory? dir)
	     ;; resolve the module
	     (resolve-in-dir dir)
	     '())))
   
   ;; feed the cache
   (let ((cache (hz-cache-path url)))
      (if (and (string? cache) (directory? cache))
	  (resolve-in-dir cache)
	  (resolve-default))))
