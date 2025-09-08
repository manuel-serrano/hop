;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/src/hopjs.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Mon Sep  8 07:52:13 2025 (serrano)                */
;*    Copyright   :  2004-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP entry point                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module main

   (library pthread http hop libuv js2scheme hopscript nodejs libbacktrace)
   (eval (library hop hopscript nodejs))
   
   (main main))

(cond-expand
   (enable-ssl (register-eval-srfi! 'enable-ssl)))
   
;*---------------------------------------------------------------------*/
;*    signal-init! ...                                                 */
;*---------------------------------------------------------------------*/
(define (signal-init!)
   (signal sigterm
      (lambda (n)
	 (unless (current-thread)
	    ((hop-sigterm-handler) n)))))

;*---------------------------------------------------------------------*/
;*    global configuration                                             */
;*---------------------------------------------------------------------*/
(define source #f)
(define exprs '())
(define options '())

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   (with-trace 'hopjs "main"
      
      ;; gc traces
      (let ((env (getenv "HOPTRACE")))
	 (when (string? env)
	    (when (string-contains env "hopscript:gc")
	       (cond-expand
		  (gc ($bgl-gc-verbose-set! #t))
		  (else #unspecified)))
	    (when (string-contains env "hopscript:cache")
	       (hop-profile-set! #t))))
      
      ;; catch critical signals
      (signal-init!)
      
      ;; set the library load path
      (bigloo-library-path-set! (hop-library-path))
      
      ;; default js configuration
      (hop-sofile-compile-policy-set! 'aot)
      (hopjs-standalone-set! #t)
      (hopscript-install-expanders!)
      
      ;; command line parsing
      (let* ((no (getenv "NODE_OPTIONS"))
	     (ho (getenv "HOP_OPTIONS"))
	     (argv (cdr args)))
	 (when (string? no)
	    (set! argv
	       (append (call-with-input-string no port->string-list) argv)))
	 (when (string? ho)
	    (set! argv
	       (append (call-with-input-string ho port->string-list) argv)))
	 (parse-args! argv))

      ;; no default client runtime (see nodejs-head@nodejs/require.scm)
      (hop-runtime-client-set! #f) 
      
      ;; debug traces
      (when (getenv "BIGLOOTRACE")
	 (bigloo-debug-set! (maxfx (bigloo-debug) 2)))
      
      (nodejs-command-line-set!
	 (cons (car (command-line)) options))
      
      ;; default dummy js compiler (for HTML tag embeded expressions)
      (init-clientc-compiler!
	 :expressionc (lambda l (error "clientc" "not a compiler - expressionc" l))
	 :modulec (lambda l (error "clientc" "not a compiler - modulec" l))
	 :macroe list
	 :sexp->precompiled (lambda (obj debug) (js-trivial-compiler obj))
	 :precompiled->JS-expression js-precompiled-compiler
	 :precompiled->JS-statement js-precompiled-compiler
	 :precompiled->JS-return js-precompiled-compiler
	 :precompiled->sexp js-precompiled-compiler
	 :precompiled-free-variables (lambda l (error "clientc" "not a compiler" l))
	 :jsc (lambda l (error "clientc" "not a compiler" l))
	 :jsonc (lambda l (error "clientc" "not a compiler" l))
	 :htmlc (lambda l (error "clientc" "not a compiler" l))
	 :valuec (lambda (obj p host-compiler host-context host-reigster loc)
		    (display (js-trivial-compiler obj) p)))
      
      ;; final configuration
      (when (hop-profile) (js-profile-init `(:server #t) #f #f ""))
      
      ;; disable JS global service declaration
      (js-service-support-set! #f)
      
      ;; js initialization
      (multiple-value-bind (%worker %global %module)
	 (js-main-worker! "main"
	    (format "hop-~a~a (~a)"
	       (hop-version) (hop-minor-version) (hop-build-tag))
	    (not source)
	    nodejs-new-global-object nodejs-new-module :autostart #f)
	 
	 ;; js loaders
	 (hop-loader-add! "js"
	    (lambda (path . test)
	       (js-worker-exec %worker "hop-loader"
		  (lambda (%this)
		     (nodejs-load path path  %worker %global %module
			:commonjs-export #t)))))
	 (hop-loader-add! "mjs"
	    (lambda (path . test)
	       (js-worker-exec %worker "hop-loader"
		  (lambda (%this)
		     (nodejs-load path path %worker %global %module
			:commonjs-export #t)))))
	 (hop-loader-add! "ast.json"
	    (lambda (path . test)
	       (js-worker-exec %worker "hop-loader"
		  (lambda (%this)
		     (nodejs-load path path %worker %global %module
			:commonjs-export #t)))))
	 ;; ts loader
	 (hop-loader-add! "ts"
	    (lambda (path . test)
	       (js-worker-exec %worker "hop-loader"
		  (lambda (%this)
		     (nodejs-load path path %worker %global %module :lang "ts"
			:commonjs-export #t)))))
	 
	 ;; install the command line loaders
	 (trace-item "loaders=" (length loaders))
	 (for-each (lambda (m) (nodejs-register-user-loader! %global m))
	    (nodejs-loaders))
	 
	 ;; start JS execution
	 (javascript-start-worker! %global %module %worker source)

	 ;; start main js loop
	 (with-access::WorkerHopThread %worker (mutex condv)
	    (synchronize mutex
	       (condition-variable-broadcast! condv)))
	 
	 (thread-join! %worker))))

;*---------------------------------------------------------------------*/
;*    js-precompiled-compiler ...                                      */
;*---------------------------------------------------------------------*/
(define (js-precompiled-compiler hs debug)
   ;; see hopscheme/hopscheme.scm
   (let ((obj (vector-ref hs 4)))
      (js-trivial-compiler obj)))
   
;*---------------------------------------------------------------------*/
;*    js-trivial-compiler ...                                          */
;*---------------------------------------------------------------------*/
(define (js-trivial-compiler obj)
   (cond
      ((eq? obj #unspecified) "undefined")
      ((number? obj) obj)
      ((string? obj) obj)
      ((boolean? obj) (if obj "true" "false"))
      ((null? obj) "null")
      ((js-jsstring? obj) (string-append "\"" (js-jsstring->string obj) "\""))
      (else (error "js-trivial-compiler"
	       (format "don't know how to compile (~a)" (typeof obj))
	       obj))))

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args! args)
   
   (define (usage args-parse-usage)
      (print "Usage: hopjs [options] ...")
      (print "       hopjs [options] file.js ...")
      (newline)
      (print "Options:")
      (args-parse-usage #f)
      (newline)
      (print "Shell Variables:")
      (print "   - HOPTRACE: hop internal trace [HOPTRACE=\"key1, key2, ...\"]")
      (print "      j2s:info, j2s:type, j2s:utype, j2s:hint, j2s:usage, j2s:key")
      (print "      j2s:dump, nodejs:compile, hopscript:cache, hopscript:hint")
      (print "      j2s:scope hopscript:gc")
      (print "   - HOPVERBOSE: an integer")
      (print "   - HOPCFLAGS: hopc compilation flags")
      (print "   - NODE_DEBUG: nodejs internal debugging [NODE_DEBUG=key]")
      (print "   - NODE_PATH: nodejs require path")
      (print "   - HOP_OPTIONS: additional command line options")
      (print "   - NODE_LOG: log file")
      (print "   - BIGLOOHEAP: Initial size of the heap (in Mbytes)")
      (print "   - BIGLOOTRACE: List of active traces")
      (print "   - BIGLOODEBUG: Bigloo debug level")
      (print "   - XDG_CACHE_HOME: Cache directory")
      (print "   - NODE_OPTIONS: Additional command line options")
      (newline)
      (print "Default configuration:")
      (print "   - rc-dir: " (hop-rc-directory)))
   
   (let ((d (getenv "BIGLOODEBUG")))
      (when (string? d)
	 (bigloo-debug-set! (string->integer d))))
   
   (let ((d (getenv "NODE_DEBUG")))
      (when (string? d)
	 (let ((l (filter integer? (map string->number (string-split d ",")))))
	    (when (pair? l)
	       (bigloo-debug-set! (car l))))))

   (bind-exit (stop)
      (args-parse args
	 (section "Misc")
	 ((("-h" "--help") (help "This message"))
	  (usage args-parse-usage)
	  (exit 0))
	 ((("-v" "--version") (help "print Hop.js version"))
	  (print (hop-version))
	  (exit 0))
	 ;; common options
	 (section "Common options")
	 (("--enable-source-maps" (help "Source Map V3 support for stack traces"))
	  (j2s-compile-options-set!
	     (cons* :source-map #t (j2s-compile-options))))
	 (("--loader" ?module (help "Specify the module to use a custom module loader"))
	  (nodejs-loaders-set! (cons module (nodejs-loaders))))
	 (("--experimental-loader=?module" (help "Specify the module to use a custom module loader"))
	  (nodejs-loaders-set! (cons module (nodejs-loaders))))
	 (("--no-warnings" (help "Silence all process warnings"))
	  (bigloo-warning-set! 0))
	 ;; specific options
	 (section "Hopjs specific options")
	 (("--clear-cache" (help "Clear all caches"))
	  (for-each (lambda (cache)
		       (when (directory? cache)
			  (delete-path cache)))
	     (list (make-cache-name)
		(hop-cache-directory))))
	 (("--compilation-cache" ?bool (help "Do not use so caches"))
	  (hop-cache-enable-set! (member bool '("yes" "on" "true" "enable"))))
	 (section "Hopc options")
	 (("-g?level" (help "Debug level"))
	  (hop-sofile-enable-set! #f)
	  (if (string=? level "")
	      (bigloo-debug-set! 1)
	      (bigloo-debug-set! (string->integer level))))
	 (("-O?level" (help "Optimization level (for newly compiled so files)"))
	  (cond
	     ((string=? level "")
	      (hop-hopc-flags-set! "-O --safe")
	      (j2s-compile-options-set!
		 (cons* :optim 1 (j2s-compile-options))))
	     ((string=? level "x")
	      (hop-hopc-flags-set! "-Ox")
	      (j2s-compile-options-set!
		 (cons* :optim 1000 :optim-ccall #t (j2s-compile-options))))
	     ((string=? level "s")
	      (j2s-compile-options-set!
		 (cons* :optim 2 :optim-size 1 (j2s-compile-options))))
	     ((string=? level "2s")
	      (j2s-compile-options-set!
		 (cons* :optim 2 :optim-size 2 (j2s-compile-options))))
	     (else
	      (hop-hopc-flags-set! "--safe")
	      (j2s-compile-options-set!
		 (cons* :optim level (j2s-compile-options))))))
	 (("--so-policy" ?policy (help "Sofile compile policy [none, aot, aot+, nte, nte1, nte+, pgo, ifexists]"))
	  (hop-sofile-compile-policy-set! (string->symbol policy))
	  (when (eq? (hop-sofile-compile-policy) 'none)
	     (hop-sofile-enable-set! #f)))
	 (("--log" ?file (help "Generate log file [filename, stdout, stderr]"))
	  (let ((p (cond
		      ((string=? file "stdout")
		       (current-output-port))
		      ((string=? file "stderr")
		       (current-error-port))
		      (else
		       (append-output-file file)))))
	     (unless p
		(error "hopjs" "Cannot open log file" file))
	     (hop-log-file-set! p)))
	 (else
	  (set! source else)
	  (set! options (append! options rest))
	  (stop #t)))))
   
;*---------------------------------------------------------------------*/
;*    javascript-start-worker! ...                                     */
;*---------------------------------------------------------------------*/
(define (javascript-start-worker! %global %module %worker source)
   (with-trace 'hopjs "javascript-start-worker!"
      (trace-item "source=" source)
      ;; hss extension
      (javascript-init-hss %worker %global)
      ;; push user expressions
      (when (pair? exprs)
	 (js-worker-push! %worker "cmdline"
	    (lambda (%this)
	       (for-each (lambda (expr)
			    (call-with-input-string (string-append expr "\n")
			       (lambda (ip)
				  (%js-eval ip 'eval %global
				     (js-undefined) %global))))
		  exprs))))
      (if source
	  ;; load the source file loading
	  (load-js-file source %global %module %worker)
	  ;; start the js repl
	  (begin
	     (signal sigint
		(lambda (n)
		   (thread-kill! %worker sigint)))
	     (js-worker-push! %worker "repl"
		(lambda (%this)
		   (display* "Welcome to Hop.js " (hop-version)
		      " (" (hop-sofile-compile-policy) ").")
		   (node-repl %global %worker)))))
      ;; start the javascript loop
      (with-access::WorkerHopThread %worker (mutex condv module-cache)
	 (synchronize mutex
	    ;; module-cache is #f until the worker is initialized and
	    ;; running (see hopscript/worker.scm)
	    (unless module-cache
	       (condition-variable-wait! condv mutex))))
      ;; return the worker for the main loop to join
      %worker))

;*---------------------------------------------------------------------*/
;*    javascript-init-hss ...                                          */
;*---------------------------------------------------------------------*/
(define (javascript-init-hss %worker %global)
   ;; force the module initialization
   (js-worker-push! %worker "hss"
      (lambda (%this)
	 (let ((mod (nodejs-new-module "hss" "hss" %worker %global))
	       (scope (nodejs-new-scope-object %global)))
	    (js-put! scope (& "module") mod #f scope)
	    (call-with-input-string "false"
	       (lambda (in)
		  (%js-eval in 'repl %global %global scope)))
	    (hop-hss-foreign-eval-set!
	       (lambda (ip)
		  (js-put! mod (& "filename")
		     (js-string->jsstring (input-port-name ip)) #f
		     %global)
		  (%js-eval-hss ip %global %worker scope)))))))

;*---------------------------------------------------------------------*/
;*    load-js-file ...                                                 */
;*---------------------------------------------------------------------*/
(define (load-js-file f %global %module %worker)
   
   (define (load-js-directory path)
      (let ((src (string-append (basename path) ".js")))
	 (if (file-exists? src)
	     (hop-load-weblet (make-file-name path src))
	     (error "hopjs" "Cannot find source file" path))))
   
   (define (load-package pkg)
      (call-with-input-file pkg
	 (lambda (ip)
	    (let* ((obj (javascript->obj ip))
		   (cmain (assq 'main obj)))
	       (when (pair? cmain)
		  (load-js-file
		     (make-file-name (dirname pkg) (cdr cmain))
		     %global %module %worker)
		  #t)))))
   
   (let ((path (cond
		  ((string-index f ":")
		   f)
		  ((and (>fx (string-length f) 0)
			(char=? (string-ref f 0) (file-separator)))
		   f)
		  (else
		   (file-name-canonicalize! (make-file-name (pwd) f))))))
      (cond
	 ((string-suffix? ".hz" path)
	  ;; this is a weblet
	  (hop-load-hz path))
	 ((directory? path)
	  ;; load a directory
	  (let ((pkg (make-file-name path "package.json")))
	     (cond
		((file-exists? pkg)
		 (load-package pkg))
		(else
		 (load-js-directory path)))))
	 ((or (string-suffix? ".js" path)
	      (string-suffix? ".mjs" path)
	      (string-suffix? ".ast.json" path))
	  ;; javascript
	  (when %worker
	     (with-access::WorkerHopThread %worker (%this prerun)
		(js-worker-push! %worker (format "nodejs-load(~a)" path)
		   (lambda (%this)
		      (with-handler
			 (lambda (e)
			    (raise e))
			 (nodejs-load-module path %worker %global %module :commonjs-export #t)))))))
	 ((string-suffix? ".ts" path)
	  ;; typescript
	  (when %worker
	     (with-access::WorkerHopThread %worker (%this prerun)
		(js-worker-push! %worker (format "nodejs-load(~a)" path)
		   (lambda (%this)
		      (nodejs-load-module path %worker %global %module :lang "ts" :commonjs-export #t))))))
	 ((string=? (basename path) "package.json")
	  (load-package path))
	 (else
	  ;; this is a plain file
	  (error "hopjs" "Unknown source file format" path)))))
