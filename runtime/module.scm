;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/module.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 26 09:29:33 2009                          */
;*    Last change :  Fri Mar 27 14:10:52 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
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
       (let ((i (map (lambda (c)
			(match-case c
			   ((<TILDE> ??- :src (quote ?import)) import)
			   (else '())))
		     clauses)))
	  (if (pair? i)
	      ((clientc-modulec (hop-clientc)) i)
	      i)))
      (else
       '())))

;*---------------------------------------------------------------------*/
;*    make-hop-module-resolver ...                                     */
;*---------------------------------------------------------------------*/
(define (make-hop-module-resolver resolver)
   (lambda (module abase)
      (let ((files (resolver module abase)))
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
   (let ((dir (hop-module-feed-cache url (hop-module-cache))))
      ;; resolve the module
      (let ((afile (make-file-path dir ".afile")))
	 (when (file-exists? afile) (module-load-access-file afile))
	 ((bigloo-module-resolver) module dir))))

;*---------------------------------------------------------------------*/
;*    hop-module-feed-cache ...                                        */
;*---------------------------------------------------------------------*/
(define (hop-module-feed-cache url dest)
   (multiple-value-bind (_ _ host port abspath)
      (url-parse url)
      (multiple-value-bind (base version)
	 (hz-package-name-parse abspath)
	 (let ((dir (if host
			(make-file-name dest
					(format "~a_~a~a-~a"
						host port
						(prefix (basename abspath))))
			(make-file-name dest (prefix (basename abspath))))))
	    (unless (directory? dir)
	       (call-with-input-file url
		  (lambda (iport)
		     (make-directories dir)
		     (let* ((p (open-input-gzip-port iport)))
			(unwind-protect
			   (untar p :directory dir)
			   (close-input-port iport))))))
	    (make-file-name dir base)))))


