;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/require.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 16 15:47:40 2013                          */
;*    Last change :  Wed May 28 19:31:16 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo Nodejs module implementation                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs_require

   (library hop hopscript js2scheme web)

   (import __nodejs
	   __nodejs_process)

   (export (%nodejs-module::JsObject ::bstring ::bstring ::JsGlobalObject)
	   (nodejs-require ::bstring ::WorkerHopThread ::JsGlobalObject ::procedure)
	   (nodejs-load ::bstring ::WorkerHopThread ::procedure)
	   (nodejs-compile-file ::bstring ::bstring ::bstring)
	   (nodejs-resolve-filename ::bstring ::pair-nil)
	   (nodejs-new-global-object::JsGlobalObject)
	   (nodejs-auto-require! ::WorkerHopThread ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    nodejs-compile-file ...                                          */
;*---------------------------------------------------------------------*/
(define (nodejs-compile-file ifile name ofile)
   (if (string=? ofile "-")
       (module->javascript ifile (current-output-port) (basename ifile) #f #f)
       (call-with-output-file ofile
	  (lambda (op)
	     (module->javascript ifile name op #f #f)))))

;*---------------------------------------------------------------------*/
;*    module->javascript ...                                           */
;*---------------------------------------------------------------------*/
(define (module->javascript filename id op compile isexpr)
   (fprintf op "hop_requires[ ~s ] = function() { " id)
   (display "var exports = {}; " op)
   (fprintf op "var module = { id: ~s, filename: ~s, loaded: true, exports: exports }\n" id filename)
   (call-with-input-file filename
      (lambda (in)
	 (for-each (lambda (exp) (display exp op))
	    (j2s-compile in
	       :driver (j2s-javascript-driver)))))
   (display "return exports;}\n" op))

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
   
   (define (module-init! m exports)
      ;; id field
      (js-put! m 'id id #f %this)
      ;; exports
      (js-put! m 'exports exports #f %this)
      ;; filename
      (js-put! m 'filename filename #f %this)
      ;; loaded
      (js-put! m 'loaded #t #f %this)
      ;; children
      (js-put! m 'children '#() #f %this)
      ;; paths
      (js-put! m 'paths (nodejs-filename->paths filename) #f %this))
   
   (with-trace 1 "%nodejs-module"
      (trace-item "id=" id)
      (trace-item "filename=" filename)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((m (js-new %this js-object))
	       (exports (js-new %this js-object)))
	    ;; module properties
	    (module-init! m exports)
	    ;; reqgister the module in the current worker thread
	    (nodejs-cache-module-put! filename (js-current-worker) m)
	    ;; global object exports
	    (js-put! %this 'exports exports #f %this)
	    ;; bind the module at once
	    (js-put! %this 'module m #f %this)
	    ;; return the newly allocated module
	    m))))

;*---------------------------------------------------------------------*/
;*    nodejs-new-global-object ...                                     */
;*---------------------------------------------------------------------*/
(define (nodejs-new-global-object)
   (nodejs-init-global-object! (js-new-global-object)))

;*---------------------------------------------------------------------*/
;*    nodejs-init-global-object! ...                                   */
;*---------------------------------------------------------------------*/
(define (nodejs-init-global-object! this)
   
   (define require
      (js-make-function this
	 (lambda (_ name)
	    (nodejs-require (js-tostring name this) (js-current-worker) this
	       nodejs-new-global-object))
	 1 "require"))

   ;; process
   (js-put! this 'process (%nodejs-process this) #f this)
   ;; require
   (js-put! this 'require require #f this)
   ;; the global object
   this)
   
;*---------------------------------------------------------------------*/
;*    nodejs-auto-require! ...                                         */
;*---------------------------------------------------------------------*/
(define (nodejs-auto-require! worker this)
   ;; console
   (js-put! this 'console (nodejs-require-core "console" worker this) #f this)
   ;; timers
   (nodejs-import! this (nodejs-require-core "timers" worker this))
   ;; buffer
   (nodejs-import! this (nodejs-require-core "buffer" worker this))
   ;; return the object
   this)

;*---------------------------------------------------------------------*/
;*    compile-mutex ...                                                */
;*---------------------------------------------------------------------*/
(define compile-mutex (make-mutex))
(define compile-table (make-hashtable))

;*---------------------------------------------------------------------*/
;*    nodejs-compile ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-compile filename)
   (synchronize compile-mutex
      (or (hashtable-get compile-table filename)
	  (let* ((mod (gensym))
		 (expr (call-with-input-file filename
			  (lambda (in)
			     (j2s-compile in
				:module-main #f
				:module-name (symbol->string mod)))))
		 (evmod (eval-module)))
	     (unwind-protect
		(begin
		   (for-each eval! expr)
		   (let ((mod (eval! 'hopscript)))
		      (hashtable-put! compile-table filename mod)
		      mod))
		(eval-module-set! evmod))))))

;*---------------------------------------------------------------------*/
;*    nodejs-load ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-load filename worker::WorkerHopThread new-global-object)
   
   (define (load-module)
      (let ((hopscript (nodejs-compile filename))
	    (this (nodejs-auto-require! worker (new-global-object))))
	 ;; create the module
	 (hopscript this)
	 ;; return the newly created module
	 (js-get this 'module this)))

   (with-trace 1 "nodejs-load"
      (trace-item "filename=" filename)
      (with-access::WorkerHopThread worker (module-mutex module-table)
	 (synchronize module-mutex
	    (or (hashtable-get module-table filename)
		(let ((mod (with-loading-file filename load-module)))
		   (hashtable-put! module-table filename mod)
		   mod))))))

;*---------------------------------------------------------------------*/
;*    nodejs-require ...                                               */
;*    -------------------------------------------------------------    */
;*    Require a nodejs module, load it if necessary or simply          */
;*    reuse the previously loaded module structure.                    */
;*---------------------------------------------------------------------*/
(define (nodejs-require name worker %this new-global-object)
   (with-trace 1 "nodejs-require"
      (trace-item "name=" name)
      (if (core-module? name)
	  (nodejs-require-core name worker %this)
	  (let ((abspath (nodejs-resolve name %this)))
	     (if (and (string? abspath) (file-exists? abspath))
		 (let ((mod (nodejs-load abspath worker new-global-object)))
		    (js-get mod 'exports %this))
		 (with-access::JsGlobalObject %this (js-uri-error)
		    (js-raise
		       (js-new %this js-uri-error
			  (format "Cannot find module ~s" name)))))))))

;*---------------------------------------------------------------------*/
;*    core-module? ...                                                 */
;*---------------------------------------------------------------------*/
(define (core-module? name)
   (assoc name (core-module-table)))

;*---------------------------------------------------------------------*/
;*    nodejs-require-core ...                                          */
;*    -------------------------------------------------------------    */
;*    Require a nodejs module, load it if necessary or simply          */
;*    reuse the previously loaded module structure.                    */
;*---------------------------------------------------------------------*/
(define (nodejs-require-core name worker %this)
   
   (define (nodejs-load-core-module name worker)
      
      (define (nodejs-init-core-module name)
	 (with-trace 2 "nodejs-init-core-module"
	    (trace-item "name=" name)
	    (let ((this (nodejs-new-global-object)))
	       (let ((c (assoc name (core-module-table))))
		  (let ((m ((cdr c) this)))
		     ;; complete the global object initialization
		     (nodejs-auto-require! worker this)
		     ;; return the module
		     m)))))
      
      (with-trace 1 "nodejs-load-core-module"
	 (trace-item "name=" name)
	 (trace-item "cache=" (nodejs-cache-module name worker))
	 (or (nodejs-cache-module name worker)
	     (nodejs-init-core-module name))))

   (with-trace 1 "nodejs-require"
      (trace-item "name=" name)
      (js-get (nodejs-load-core-module name worker) 'exports %this)))

;*---------------------------------------------------------------------*/
;*    nodejs-env-path ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-env-path)
   (let ((env (getenv "NODE_PATH")))
      (if (string? env)
	  (unix-path->list env)
	  '())))

;*---------------------------------------------------------------------*/
;*    nodejs-resolve-filename ...                                      */
;*---------------------------------------------------------------------*/
(define (nodejs-resolve-filename name::bstring path)
   
   (define (nodejs-resolve-package path)
      (let ((pkg (make-file-name path "package.json")))
	 (when (file-exists? pkg)
	    (call-with-input-file pkg
	       (lambda (ip)
		  (let* ((o (json-parse ip
			       :object-alloc (lambda () (list '#unspecified))
			       :object-set (lambda (o p val)
					      (set-cdr! o
						 (cons (cons p val) o)))
			       :object-return cdr))
			 (m (assq 'main o)))
		     (when (and (pair? m) (string? (cdr m)))
			(make-file-name path (cdr m)))))))))
   
   (define (suffix name)
      (if (string-suffix? ".js" name )
	  name
	  (string-append name ".js")))
   
   (define (package-or-file name)
      (if (directory? name)
	  (nodejs-resolve-package name)
	  (suffix name)))

   (define (resolve-in-dir dir)
      (cond
	 ((string-null? name)
	  #f)
	 ((char=? (string-ref name 0) (file-separator))
	  (package-or-file name))
	 ((or (string-prefix? "./" name) (string-prefix? "../" name))
	  (package-or-file (file-name-canonicalize! (make-file-path dir name))))
	 (else
	  #f)))

   (any (lambda (dir)
	   (let ((path (make-file-name dir name)))
	      (if (directory? path)
		  (nodejs-resolve-package path)
		  (let ((src (suffix path)))
		     (when (file-exists? src)
			src)))))
      path))

;*---------------------------------------------------------------------*/
;*    nodejs-resolve ...                                               */
;*    -------------------------------------------------------------    */
;*    Resolve the path name according to the current module path.      */
;*---------------------------------------------------------------------*/
(define (nodejs-resolve name::bstring %this::JsGlobalObject)
   (or (nodejs-resolve-filename name (cons (pwd) (nodejs-env-path)))
       (let ((module (js-get %this 'module %this)))
	  (when (isa? module JsObject)
	     (nodejs-resolve-filename name
		(append (vector->list (js-get module 'paths %this))
		   (nodejs-env-path)))))))

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
;*    nodejs-import ...                                                */
;*    -------------------------------------------------------------    */
;*    Bind the exported binding into a global object.                  */
;*---------------------------------------------------------------------*/
(define (nodejs-import! %this e)
   ;; bind all the exported functions in the global object 
   (js-for-in e
      (lambda (p)
	 (let ((k (string->symbol p)))
	    (js-put! %this k (js-get e k %this) #f %this)))
      %this))
   
;*---------------------------------------------------------------------*/
;*    Bind the nodejs require function                                 */
;*---------------------------------------------------------------------*/
(js-worker-load-set!
   (lambda (name worker this)
      (nodejs-require name worker this
	 (lambda () (nodejs-init-global-object! this)))))

