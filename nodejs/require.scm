;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/require.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 16 15:47:40 2013                          */
;*    Last change :  Fri Apr 18 08:06:33 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo Nodejs module implementation                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs_require

   (library hop hopscript js2scheme)

   (import __nodejs)

   (export (%nodejs-module::JsObject ::bstring ::bstring ::JsGlobalObject)
	   (nodejs-require ::bstring ::JsGlobalObject)
	   (nodejs-load ::bstring ::WorkerHopThread)))

;*---------------------------------------------------------------------*/
;*    %nodejs-module ...                                               */
;*---------------------------------------------------------------------*/
(define (%nodejs-module::JsObject id filename %this::JsGlobalObject)

   (define (nodejs-filename->paths::vector file::bstring)
      (if (char=? (string-ref file 0) #\/)
	  (let loop ((dir (dirname file))
		     (acc '()))
	     (if (string=? dir "/")
		 (list->vector
		    (reverse! (cons "/node_modules" acc)))
		 (loop (dirname dir)
		    (cons (make-file-name dir "node_modules") acc))))
	  '#()))

   (with-access::JsGlobalObject %this (js-object)
      (let ((m (js-new %this js-object)))
	 ;; id field
	 (js-put! m 'id id #f %this)
	 ;; exports
	 (js-put! m 'exports
	    (alist->jsobject `((prototype . ,(js-new %this js-object))))
	    #f %this)
	 ;; filename
	 (js-put! m 'filename filename #f %this)
	 ;; loaded
	 (js-put! m 'loaded #t #f %this)
	 ;; children
	 (js-put! m 'children '#() #f %this)
	 ;; paths
	 (js-put! m 'paths (nodejs-filename->paths filename) #f %this)
	 ;; reqgister the module in the current worker thread
	 (nodejs-cache-module-put! filename (js-current-worker) m)
	 ;; return the newly allocated module
	 m)))

;*---------------------------------------------------------------------*/
;*    nodejs-require ...                                               */
;*    -------------------------------------------------------------    */
;*    Require a nodejs module, load it if necessary or simply          */
;*    reuse the previously loaded module structure.                    */
;*---------------------------------------------------------------------*/
(define (nodejs-require name %this)
   (let ((worker (js-current-worker)))
      (if (core-module? name)
	  (js-get (nodejs-load-core-module name worker %this) 'exports %this)
	  (let ((abspath (nodejs-resolve name %this)))
	     (if (and (string? abspath) (file-exists? abspath))
		 (js-get (nodejs-load abspath worker) 'exports %this)
		 (with-access::JsGlobalObject %this (js-uri-error)
		    (js-raise
		       (js-new %this js-uri-error
			  (format "Cannot find module ~s" name)))))))))

;*---------------------------------------------------------------------*/
;*    nodejs-load ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-load filename worker::WorkerHopThread)
   
   (define (load-module %this)
      (loading-file-set! filename)
      (let ((expr (call-with-input-file filename
		     (lambda (in)
			(j2s-compile in :module-main #f)))))
	 ;; eval the compile module in the current environment
	 ((eval! expr) %this)))
   
   (with-access::WorkerHopThread worker (module-mutex module-table %this)
      (synchronize module-mutex
	 (or (hashtable-get module-table filename)
	     (load-module %this)))))

;*---------------------------------------------------------------------*/
;*    nodejs-resolve ...                                               */
;*    -------------------------------------------------------------    */
;*    Resolve the path name according to the current module path.      */
;*---------------------------------------------------------------------*/
(define (nodejs-resolve name::bstring %this::JsGlobalObject)
   
   (define (nodejs-resolve-package path)
      (let ((pkg (make-file-name path "package.json")))
	 (when (file-exists? pkg)
	    (call-with-input-file pkg
	       (lambda (in)
		  (let* ((o (js-json-parser in (js-undefined) %this))
			 (m (js-tostring (js-get o 'main %this) %this)))
		     (when (string? m)
			(make-file-name path m))))))))
   
   (define (suffix name)
      (if (string-suffix? ".js" name )
	  name
	  (string-append name ".js")))
   
   (define (package-or-file name)
      (if (directory? name)
	  (nodejs-resolve-package name)
	  (suffix name)))
   
   (cond
      ((string-null? name)
       #f)
      ((char=? (string-ref name 0) (file-separator))
       (package-or-file name))
      ((or (string-prefix? "./" name) (string-prefix? "../" name))
       (package-or-file (file-name-canonicalize! (make-file-path (pwd) name))))
      (else
       (let ((module (js-get %this 'module %this)))
	  (when (isa? module JsObject)
	     (find (lambda (dir)
		      (let ((path (make-file-name dir name)))
			 (if (directory? path)
			     (nodejs-resolve-package path)
			     (let ((src (suffix name)))
				(when (file-exists? src)
				   src)))))
		(vector->list (js-get module 'paths %this))))))))

;*---------------------------------------------------------------------*/
;*    nodejs-cache-module ...                                          */
;*---------------------------------------------------------------------*/
(define (nodejs-cache-module name worker)
   (with-access::WorkerHopThread worker (module-table module-mutex)
      (synchronize module-mutex
	 (hashtable-get module-table name))))
   
;*---------------------------------------------------------------------*/
;*    nodejs-cache-module-put! ...                                     */
;*---------------------------------------------------------------------*/
(define (nodejs-cache-module-put! name worker module)
   (with-access::WorkerHopThread worker (module-table module-mutex)
      (synchronize module-mutex
	 (hashtable-put! module-table name module)
	 module)))

;*---------------------------------------------------------------------*/
;*    nodejs-load-core-module ...                                      */
;*---------------------------------------------------------------------*/
(define (nodejs-load-core-module name worker %this)
   
   (define (nodejs-init-core-module name %this)
      (let ((c (assoc name (core-module-table))))
	 ((cdr c) %this)))
   
   (or (nodejs-cache-module name worker)
       (nodejs-init-core-module name %this)))

;*---------------------------------------------------------------------*/
;*    core-module? ...                                                 */
;*---------------------------------------------------------------------*/
(define (core-module? name)
   (assoc name (core-module-table)))

;*---------------------------------------------------------------------*/
;*    Bind the nodejs require function                                 */
;*---------------------------------------------------------------------*/
(js-worker-load-set! nodejs-require)

