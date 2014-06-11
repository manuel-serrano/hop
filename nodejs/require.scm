;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/require.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 16 15:47:40 2013                          */
;*    Last change :  Wed Jun 11 19:46:17 2014 (serrano)                */
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
	   (nodejs-load ::bstring ::WorkerHopThread ::procedure)
	   (nodejs-compile-file ::bstring ::bstring ::bstring)
	   (nodejs-resolve ::bstring ::JsGlobalObject)
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
      (js-put! m 'children (js-vector->jsarray '#() %this) #f %this)
      ;; paths
      (js-put! m 'paths
	 (js-vector->jsarray (nodejs-filename->paths filename) %this)
	 #f %this))
   
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

   ;; require
   (define require
      (js-make-function this
	 (lambda (_ name)
	    (nodejs-require (js-tostring name this)
	       (js-current-worker) this nodejs-new-global-object))
	 1 "require"))

   ;; require.resolve
   (js-put! require 'resolve
      (js-make-function this
	 (lambda (_ name)
	    (let ((name (js-tostring name this)))
	       (if (core-module? name)
		   name
		   (nodejs-resolve name this))))
	 1 "resolve")
      #f this)
   
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
	     (let ((mod (nodejs-load abspath worker new-global-object)))
		(js-get mod 'exports %this))))))

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
;*    nodejs-resolve ...                                               */
;*    -------------------------------------------------------------    */
;*    Resolve the path name according to the current module path.      */
;*    -------------------------------------------------------------    */
;*    http://nodejs.org/api/modules.html#modules_all_together          */
;*---------------------------------------------------------------------*/
(define (nodejs-resolve name::bstring %this::JsGlobalObject)
   
   (define (resolve-file x)
      (if (and (file-exists? x) (not (directory? x)))
	  (file-name-canonicalize x)
	  (let ((js (string-append x ".js")))
	     (when (and (file-exists? js) (not (directory? js)))
		(file-name-canonicalize js)))))
   
   (define (resolve-package pkg)
      (call-with-input-file pkg
	 (lambda (ip)
	    (let* ((o (json-parse ip
			 :array-alloc list
			 :array-set (lambda (o i v) #f)
			 :array-return (lambda (o i) #f)
			 :object-alloc (lambda () (make-cell '()))
			 :object-set (lambda (o p val)
					(cell-set! o
					   (cons (cons p val)
					      (cell-ref o))))
			 :object-return (lambda (o) (cell-ref o))
			 :parse-error (lambda (msg token loc)
					 (js-raise-syntax-error
					    (js-new-global-object) msg ""))))
		   (m (assoc "main" o)))
	       (when (and (pair? m) (string? (cdr m)))
		  (cdr m))))))
   
   (define (resolve-directory x)
      (let ((json (make-file-name x "package.json")))
	 (or (and (file-exists? json)
		  (let ((m (resolve-package json)))
		     (resolve-file (make-file-name x m))))
	     (let ((p (make-file-name x "index.js")))
		(when (file-exists? p)
		   (file-name-canonicalize p))))))
   
   (define (resolve-file-or-directory x dir)
      (let ((file (make-file-name dir x)))
	 (or (resolve-file file)
	     (resolve-directory file))))
   
   (define (resolve-error x)
      (with-access::JsGlobalObject %this (js-uri-error)
	 (js-raise
	    (js-new %this js-uri-error
	       (format "Cannot find module ~s" name)))))
   
   (define (resolve-modules mod x start)
      (any (lambda (dir)
	      (resolve-file-or-directory x dir))
	 (node-modules-path mod start)))
   
   (define (node-modules-path mod start)
      (let ((paths (js-get mod 'paths %this)))
	 (cond
	    ((pair? paths)
	     (append paths nodejs-env-path))
	    ((isa? paths JsArray)
	     (with-access::JsArray paths (vec)
		(append (vector->list vec) nodejs-env-path)))
	    (else
	     nodejs-env-path))))
   
   (let* ((mod (js-get %this 'module %this))
	  (dir (dirname (js-get mod 'filename %this))))
      (if (or (string-prefix? "./" name)
	      (string-prefix? "../" name)
	      (string-prefix? "/" name))
	  (or (resolve-file-or-directory name dir)
	      (resolve-modules mod name (dirname dir))
	      (resolve-error name))
	  (or (resolve-modules mod name (dirname dir))
	      (resolve-error name)))))

;*---------------------------------------------------------------------*/
;*    nodejs-env-path ...                                              */
;*---------------------------------------------------------------------*/
(define nodejs-env-path
   (let ((home-path (let ((home (getenv "HOME")))
		       (if (string? home)
			   (list (make-file-name home ".node_modules")
			      (make-file-name home ".node_libraries"))
			   '()))))
      (let ((env (getenv "NODE_PATH")))
	 (if (string? env)
	     (append (unix-path->list env) home-path)
	     home-path))))

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
	 (lambda ()
	    ;; remove the temporary module set in the worker thunk
	    ;; (see worker.scm)
	    (js-delete! this 'module #f this)
	    (nodejs-init-global-object! this)))))

