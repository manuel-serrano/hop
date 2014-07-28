;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/require.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 16 15:47:40 2013                          */
;*    Last change :  Mon Jul 28 12:30:00 2014 (serrano)                */
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

   (export (nodejs-module::JsObject ::bstring ::bstring ::JsGlobalObject)
	   (nodejs-require ::JsGlobalObject ::JsObject)
	   (nodejs-require-core ::bstring ::WorkerHopThread ::JsGlobalObject)
	   (nodejs-load ::bstring ::WorkerHopThread)
	   (nodejs-import!  ::JsGlobalObject ::JsObject ::JsObject . bindings)
	   (nodejs-compile-file ::bstring ::bstring ::bstring)
	   (nodejs-resolve ::bstring ::JsGlobalObject ::obj)
	   (nodejs-new-global-object::JsGlobalObject)
	   (nodejs-new-scope-object ::JsGlobalObject)
	   (nodejs-eval ::JsGlobalObject ::JsObject)
	   (nodejs-function ::JsGlobalObject ::JsObject)
	   (nodejs-worker ::JsGlobalObject ::JsObject ::JsObject)))

;*---------------------------------------------------------------------*/
;*    nodejs-compile-file ...                                          */
;*---------------------------------------------------------------------*/
(define (nodejs-compile-file ifile name ofile)
   (let* ((srcmap (when (>fx (bigloo-debug) 0)
		     (string-append ofile ".map")))
	  (op (if (string=? ofile "-")
		  (current-output-port)
		  (open-output-file ofile)))
	  (tree (unwind-protect
		    (module->javascript ifile name op #f #f srcmap)
		    (unless (eq? op (current-output-port))
		       (close-output-port op)))))
      (when (>fx (bigloo-debug) 0)
	 (call-with-output-file srcmap
	    (lambda (p)
	       (generate-source-map tree ifile ofile p))))))

;*---------------------------------------------------------------------*/
;*    module->javascript ...                                           */
;*---------------------------------------------------------------------*/
(define (module->javascript filename id op compile isexpr srcmap)
   (let ((this (nodejs-new-global-object)))
      (fprintf op "hop_requires[ ~s ] = function() { " id)
      (display "var exports = {}; " op)
      (fprintf op "var module = { id: ~s, filename: ~s, loaded: true, exports: exports }; " id filename)
      (flush-output-port op)
      (let ((offset (output-port-position op)))
	 (call-with-input-file filename
	    (lambda (in)
	       (let ((tree (j2s-compile in
			      :%this this
			      :source filename
			      :resource (dirname filename)
			      :filename filename
			      :worker (js-current-worker)
			      :parser 'client-program
			      :driver (if (>fx (bigloo-debug) 0)
					  (j2s-javascript-debug-driver)
					  (j2s-javascript-driver)))))
		  (for-each (lambda (exp)
			       (unless (isa? exp J2SNode)
				  ;; skip node information, used for sourcemap
				  ;; generation
				  (display exp op)))
		     tree)
		  (display "return exports;}" op)
		  (when srcmap
		     (fprintf op "\n\nhop_source_mapping_url( ~s, \"~a\" );\n"
			filename srcmap)
		     (fprintf op "\n//# sourceMappingURL=~a\n" srcmap))
		  ;; first element of the tree is a position offset
		  ;; see sourcemap generation
		  (cons offset tree)))))))

;*---------------------------------------------------------------------*/
;*    nodejs-driver ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-driver)
   (if (> (bigloo-debug) 0) (j2s-debug-driver) (j2s-optim-driver)))

;*---------------------------------------------------------------------*/
;*    nodejs-module ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-module::JsObject id filename %this::JsGlobalObject)
   
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
      ;; parent
      (js-put! m 'parent #f #f %this)
      ;; children
      (js-put! m 'children (js-vector->jsarray '#() %this) #f %this)
      ;; paths
      (js-put! m 'paths
	 (js-vector->jsarray (nodejs-filename->paths filename) %this)
	 #f %this))

   (with-trace 1 "nodejs-module"
      (trace-item "id=" id)
      (trace-item "filename=" filename)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((m (js-new0 %this js-object))
	       (exports (js-new0 %this js-object)))
	    ;; module properties
	    (module-init! m exports)
	    ;; reqgister the module in the current worker thread
	    (nodejs-cache-module-put! filename (js-current-worker) m)
	    ;; return the newly allocated module
	    m))))

;*---------------------------------------------------------------------*/
;*    nodejs-require ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-require this::JsGlobalObject %module::JsObject)
   
   ;; require
   (define require
      (js-make-function this
	 (lambda (_ name)
	    (nodejs-require-module (js-tostring name this)
	       (js-current-worker) this %module))
	 1 "require"))

   ;; require.resolve
   (js-put! require 'resolve
      (js-make-function this
	 (lambda (_ name)
	    (let ((name (js-tostring name this)))
	       (if (core-module? name)
		   name
		   (nodejs-resolve name this %module))))
	 1 "resolve")
      #f this)

   require)

;*---------------------------------------------------------------------*/
;*    nodejs-new-global-object ...                                     */
;*---------------------------------------------------------------------*/
(define (nodejs-new-global-object)
   (js-new-global-object))

;*---------------------------------------------------------------------*/
;*    nodejs-new-scope-object ...                                      */
;*---------------------------------------------------------------------*/
(define (nodejs-new-scope-object global::JsGlobalObject)
   (let ((scope (duplicate::JsGlobalObject global
		   (cmap (instantiate::JsConstructMap))
		   (properties '())
		   (elements '#()))))
      (nodejs-import! global scope global)
      (hopscript-global-object-init! scope)
      scope))

;*---------------------------------------------------------------------*/
;*    hopscript-global-object-init! ...                                */
;*---------------------------------------------------------------------*/
(define (hopscript-global-object-init! scope)
   (js-put! scope 'HEAD (js-html-head scope) #f scope))

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
				:driver (nodejs-driver)
				:module-main #f
				:module-name (symbol->string mod)))))
		 (evmod (eval-module)))
	     (unwind-protect
		(begin
		   (for-each eval! expr)
		   (let ((hopscript (eval! 'hopscript)))
		      (hashtable-put! compile-table filename hopscript)
		      hopscript))
		(eval-module-set! evmod))))))

;*---------------------------------------------------------------------*/
;*    nodejs-load ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-load filename worker::WorkerHopThread)
   
   (define (load-module-js)
      (with-access::WorkerHopThread worker (%this)
	 (with-access::JsGlobalObject %this (js-object)
	    (let ((hopscript (nodejs-compile filename))
		  (this (js-new0 %this js-object))
		  (scope (nodejs-new-scope-object %this))
		  (mod (nodejs-module (basename filename) filename %this)))
	       ;; create the module
	       (hopscript %this this scope mod)
	       ;; return the newly created module
	       mod))))

   (define (load-module-hop)
      (with-access::WorkerHopThread worker (%this)
	 (with-access::JsGlobalObject %this (js-object)
	    (let ((evmod (hop-load filename :mode 'module))
		  (this (js-new0 %this js-object))
		  (scope (nodejs-new-scope-object %this))
		  (mod (nodejs-module (basename filename) filename %this)))
	       (when (evmodule? evmod)
		  (call-with-eval-module evmod
		     (lambda ()
			((eval! 'hopscript) %this this scope mod))))
	       ;; return the newly created module
	       mod))))

   (define (load-module-so)
      (with-access::WorkerHopThread worker (%this)
	 (with-access::JsGlobalObject %this (js-object)
	    (let ((init (dynamic-load filename))
		  (this (js-new0 %this js-object))
		  (scope (nodejs-new-scope-object %this))
		  (mod (nodejs-module (basename filename) filename %this)))
	       (when (procedure? init)
		  (init %this this scope mod))
	       ;; return the newly created module
	       mod))))

   (define (load-module)
      (cond
	 ((string-suffix? ".js" filename)
	  (load-module-js))
	 ((string-suffix? ".hop" filename)
	  (load-module-hop))
	 ((string-suffix? ".so" filename)
	  (load-module-so))
	 (else
	  (js-raise-error (js-new-global-object)
	     "Don't know how to load module"
	     filename))))

   (with-trace 1 "nodejs-load"
      (trace-item "filename=" filename)
      (with-access::WorkerHopThread worker (module-mutex module-table)
	 (synchronize module-mutex
	    (or (hashtable-get module-table filename)
		(let ((mod (with-loading-file filename load-module)))
		   (hashtable-put! module-table filename mod)
		   mod))))))

;*---------------------------------------------------------------------*/
;*    nodejs-require-module ...                                        */
;*    -------------------------------------------------------------    */
;*    Require a nodejs module, load it if necessary or simply          */
;*    reuse the previously loaded module structure.                    */
;*---------------------------------------------------------------------*/
(define (nodejs-require-module name worker %this %module)
   (with-trace 1 "nodejs-require-module"
      (trace-item "name=" name)
      (if (core-module? name)
	  (nodejs-require-core name worker %this)
	  (let* ((abspath (nodejs-resolve name %this %module))
		 (mod (nodejs-load abspath worker))
		 (children (js-get %module 'children %this))
		 (push (js-get children 'push %this)))
	     (js-call1 %this push children mod)
	     (when (eq? (js-get mod 'parent %this) (js-undefined))
		(js-put! mod 'parent %module #f %this))
	     (js-get mod 'exports %this)))))

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
	    (with-access::JsGlobalObject %this (js-object)
	       (let ((this (js-new0 %this js-object))
		     (scope (nodejs-new-scope-object %this))
		     (mod (nodejs-module name name %this)))
		  (let ((c (assoc name (core-module-table))))
		     ;; initialize the core module
		     ((cdr c) %this this scope mod)
		     ;; return the module
		     mod)))))
      
      (with-trace 1 "nodejs-load-core-module"
	 (trace-item "name=" name)
	 (trace-item "cache=" (nodejs-cache-module name worker))
	 (or (nodejs-cache-module name worker)
	     (nodejs-init-core-module name))))

   (with-trace 1 "nodejs-require-core"
      (trace-item "name=" name)
      (js-get (nodejs-load-core-module name worker) 'exports %this)))

;*---------------------------------------------------------------------*/
;*    nodejs-resolve ...                                               */
;*    -------------------------------------------------------------    */
;*    Resolve the path name according to the current module path.      */
;*    -------------------------------------------------------------    */
;*    http://nodejs.org/api/modules.html#modules_all_together          */
;*---------------------------------------------------------------------*/
(define (nodejs-resolve name::bstring %this::JsGlobalObject %module)
   
   (define (resolve-file x)
      (if (and (file-exists? x) (not (directory? x)))
	  (file-name-canonicalize x)
	  (let loop ((suffixes '("js" "hop" "so")))
	     (when (pair? suffixes)
		(let* ((suffix (car suffixes))
		       (src (string-append x ".js")))
		   (when (and (file-exists? src) (not (directory? src)))
		      (file-name-canonicalize src)))))))
   
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
   
   (let* ((mod %module)
	  (dir (dirname (js-get mod 'filename %this))))
      (cond
	 ((or (string-prefix? "./" name) (string-prefix? "../" name))
	  (or (resolve-file-or-directory name dir)
	      (resolve-modules mod name (dirname dir))
	      (resolve-error name)))
	 ((string-prefix? "/" name)
	  (or (resolve-file-or-directory name "/")
	      (resolve-modules mod name "/")
	      (resolve-error name)))
	 (else
	  (or (resolve-modules mod name (dirname dir))
	      (resolve-error name))))))

;*---------------------------------------------------------------------*/
;*    nodejs-env-path ...                                              */
;*---------------------------------------------------------------------*/
(define nodejs-env-path
   (let* ((sys-path (make-file-path (hop-lib-directory)
		       "hop" (hop-version) "node_modules"))
	  (home-path (let ((home (getenv "HOME")))
			(if (string? home)
			    (list (make-file-name home ".node_modules")
			       (make-file-name home ".node_libraries")
			       sys-path)
			    (list sys-path)))))
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
(define (nodejs-import! %this %scope e . bindings)
   
   (define (for-in obj::JsObject proc)
      
      (define (vfor-each proc vec)
	 (let ((len (vector-length vec)))
	    (let loop ((i 0))
	       (when (<fx i len)
		  (proc (vector-ref-ur vec i))
		  (loop (+fx i 1))))))
      
      (define (in-property p)
	 (when (isa? p JsPropertyDescriptor)
	    (with-access::JsPropertyDescriptor p (name)
	       (proc (symbol->string! name)))))
      
      (let loop ((o obj))
	 (with-access::JsObject o (cmap properties __proto__)
	    (if cmap
		(with-access::JsConstructMap cmap (descriptors)
		   (vfor-each in-property descriptors))
		(for-each in-property properties)))))
   
   ;; e start being undefined during the first steps of the rts boot
   (when (isa? e JsObject)
      ;; bind all the exported functions in the global object
      (if (null? bindings)
	  (for-in e
	     (lambda (p)
		(let ((k (string->symbol p)))
		   (js-put! %scope k (js-get e k %this) #f %this))))
	  (for-each (lambda (k)
		       (js-put! %scope k (js-get e k %this) #f %this))
	     bindings))))

;*---------------------------------------------------------------------*/
;*    nodejs-eval ...                                                  */
;*    -------------------------------------------------------------    */
;*    See js2scheme/header.scm                                         */
;*---------------------------------------------------------------------*/
(define (nodejs-eval %this scope)
   
   (define (js-eval this str)
      (if (not (string? str))
	  str
	  (call-with-input-string str
	     (lambda (ip)
		(%js-eval ip 'eval %this scope %this)))))

   (js-bind! %this scope 'eval
      :value (js-make-function %this js-eval 1 'eval :prototype (js-undefined))
      :configurable #f :enumerable #f))

;*---------------------------------------------------------------------*/
;*    nodejs-function ...                                              */
;*    -------------------------------------------------------------    */
;*    See js2scheme/header.scm                                         */
;*---------------------------------------------------------------------*/
(define (nodejs-function %this scope)

   (define (js-function-construct this . args)
      (if (null? args)
	  (js-make-function %this (lambda (this) (js-undefined))
	     0 "" :construct (lambda (_) (js-undefined)))
	  (let* ((len (length args))
		 (formals (take args (-fx len 1)))
		 (body (car (last-pair args)))
		 (fun (format "(function(~(,)) { ~a })"
			 (map (lambda (o) (js-tostring o %this)) formals)
			 (js-tostring body %this))))
	     (call-with-input-string fun
		(lambda (ip)
		   (%js-eval ip 'eval %this this scope))))))

   (define js-function
      (with-access::JsGlobalObject %this (js-function-prototype)
	 (js-make-function %this
	    js-function-construct 1 "Function"
	    :__proto__ js-function-prototype
	    :prototype js-function-prototype
	    :construct js-function-construct)))

   (js-bind! %this scope 'Function
      :value js-function
      :configurable #f :enumerable #f))


;*---------------------------------------------------------------------*/
;*    nodejs-worker ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-worker %this::JsGlobalObject scope::JsObject %module::JsObject)

   (define (loader filename worker this)
      (nodejs-require-module filename worker this %module))

   (define (%js-worker %this)
      (with-access::JsGlobalObject %this (js-worker)
	 (lambda (this proc)
	    (js-new %this js-worker proc))))
   
   (define js-worker
      (with-access::JsGlobalObject %this (js-function-prototype
					    js-worker-prototype)
	 (js-make-function %this (%js-worker %this) 2 'JsWorker
	    :__proto__ js-function-prototype
	    :prototype js-worker-prototype
	    :construct (js-worker-construct %this loader))))

   (js-bind! %this scope 'Worker
      :value js-worker
      :configurable #f :enumerable #f))
   
;*---------------------------------------------------------------------*/
;*    Bind the nodejs require function                                 */
;*---------------------------------------------------------------------*/
(js-worker-load-set!
   (lambda (filename worker this)
      (nodejs-require-module filename worker this
	 (nodejs-module (basename filename) filename this))))
