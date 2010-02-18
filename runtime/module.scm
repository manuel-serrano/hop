;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/runtime/module.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 26 09:29:33 2009                          */
;*    Last change :  Thu Feb 18 09:07:44 2010 (serrano)                */
;*    Copyright   :  2009-10 Manuel Serrano                            */
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
				  ((<TILDE> ??- :src (quote ?import)) import)
				  (else #f)))
			    clauses)))
	  (if (pair? i)
	      ((clientc-modulec (hop-clientc)) i)
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
       (hz-resolver module url)
       (list url)))

;*---------------------------------------------------------------------*/
;*    hz-resolver ...                                                  */
;*---------------------------------------------------------------------*/
(define (hz-resolver module url)
   ;; feed the cache
   (let ((dir (hz-download-to-cache url)))
      ;; resolve the module
      (let ((afile (make-file-path dir ".afile")))
	 (when (file-exists? afile)
	    (module-load-access-file afile)
	    (module-abase-set! dir))
	 (initial-resolver module dir))))
		    

