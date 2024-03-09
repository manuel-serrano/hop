;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/nodejs/require.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 16 15:47:40 2013                          */
;*    Last change :  Sat Mar  9 06:41:28 2024 (serrano)                */
;*    Copyright   :  2013-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo Nodejs module implementation                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs_require

   (include "../hopscript/stringthread.sch"
	    "nodejs.sch")
   
   (library hop hopscript js2scheme web)

   (import __nodejs
	   __nodejs__hop
	   __nodejs__process
	   __nodejs_syncg)

   (export (nodejs-hop-debug)
	   (nodejs-file-paths::JsObject ::JsStringLiteral ::JsGlobalObject)
	   (nodejs-new-module::JsObject ::bstring ::bstring ::WorkerHopThread ::JsGlobalObject)
	   (node-module-paths ::JsObject ::JsGlobalObject)
	   (nodejs-require ::WorkerHopThread ::JsGlobalObject ::JsObject ::bstring)
	   (nodejs-import-module::JsModule ::WorkerHopThread ::JsGlobalObject ::JsObject ::bstring ::long ::bool ::obj)
	   (nodejs-import-module-core::JsModule ::WorkerHopThread ::JsGlobalObject ::JsObject ::bstring ::long ::bool ::obj ::vector)
	   (nodejs-import-module-hop::JsModule ::WorkerHopThread ::JsGlobalObject ::JsObject ::bstring ::long ::bool ::obj ::vector)
	   (nodejs-import-module-dynamic::JsPromise ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::bstring ::bool ::obj)
	   (nodejs-import-meta::JsObject ::WorkerHopThread ::JsGlobalObject ::JsObject ::JsStringLiteral)
	   (nodejs-module-namespace::JsObject ::JsModule ::WorkerHopThread ::JsGlobalObject)
	   (nodejs-head ::WorkerHopThread ::JsGlobalObject ::JsObject ::JsObject)
	   (nodejs-script ::WorkerHopThread ::JsGlobalObject ::JsObject ::JsObject)
	   (nodejs-core-module ::bstring ::WorkerHopThread ::JsGlobalObject)
	   (nodejs-require-core ::bstring ::WorkerHopThread ::JsGlobalObject)
	   (nodejs-load-module path::bstring worker::WorkerHopThread
	      %this %module
	      #!key (lang "hopscript") compiler
	      config (commonjs-export #t) loc)
	   (nodejs-load src ::bstring ::WorkerHopThread ::obj ::obj 
	      #!key lang srcalias (commonjs-export #unspecified))
	   (nodejs-bind-export!  ::JsGlobalObject ::JsObject ::JsObject . bindings)
	   (nodejs-compile-abort-all!)
	   (nodejs-compile-client-file ::bstring ::bstring ::bstring ::bstring)
	   (nodejs-compile-json ::bstring ::bstring ::bstring ::bstring)
	   (nodejs-compile-html ::bstring ::bstring ::bstring ::bstring)
	   (nodejs-compile-pending::int)
	   (nodejs-compile-add-event-listener! ::bstring ::procedure ::bool)
	   (nodejs-compile-remove-event-listener! ::bstring ::procedure)
	   (nodejs-resolve ::bstring ::JsGlobalObject ::obj ::pair-nil ::symbol)
	   (nodejs-resolve-extend-path! ::pair-nil)
	   (nodejs-new-global-object::JsGlobalObject #!key name)
	   (nodejs-new-scope-object ::JsGlobalObject)
	   (nodejs-eval ::JsGlobalObject ::JsObject)
	   (nodejs-function ::JsGlobalObject ::JsObject)
	   (nodejs-worker ::JsGlobalObject ::JsObject ::JsObject)
	   (nodejs-plugins-toplevel-loader)
	   (nodejs-language-toplevel-loader)
	   (nodejs-language-notify-error ::bstring ::bstring)
	   (nodejs-register-user-loader! ::JsGlobalObject ::bstring)))

;*---------------------------------------------------------------------*/
;*    env-debug ...                                                    */
;*---------------------------------------------------------------------*/
(define env-debug
   (let ((env (getenv "HOP_DEBUG")))
      (if (string? env)
	  (string->integer env)
	  0)))

;*---------------------------------------------------------------------*/
;*    %env-push-trace ...                                              */
;*---------------------------------------------------------------------*/
(define-macro (%env-push-trace denv traceid val)
   (cond-expand
      ((and bigloo-c (not bigloo-saw)) `($env-push-trace ,denv ,traceid ,val))
      (else #unspecified)))

;*---------------------------------------------------------------------*/
;*    %env-pop-trace ...                                               */
;*---------------------------------------------------------------------*/
(define-macro (%env-pop-trace denv)
   (cond-expand
      ((and bigloo-c (not bigloo-saw)) `($env-pop-trace ,denv))
      (else #unspecified)))

;*---------------------------------------------------------------------*/
;*    nodejs-hop-debug ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-hop-debug)
   (max env-debug (bigloo-debug)))

;*---------------------------------------------------------------------*/
;*    &paths                                                           */
;*    -------------------------------------------------------------    */
;*    Because the loaders execute in another threads, they have        */
;*    to use the key property of the main thread.                      */
;*---------------------------------------------------------------------*/
(define-expander &&
   (lambda (x e)
      (e `(begin
	     '(tprint "& paths " (current-thread) " " ',(cer x))
	     (& "paths"))
	 e)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    require-mutex ...                                                */
;*---------------------------------------------------------------------*/
(define require-mutex (make-mutex))
(define require-init #f)

;*---------------------------------------------------------------------*/
;*    js-init-require! ...                                             */
;*---------------------------------------------------------------------*/
(define (js-init-require! %this)
   (unless (vector? __js_strings)
      (set! __js_strings (&init!)))
   (synchronize require-mutex
      (unless require-init
	 (set! require-init #t)
	 (when (memq (hop-sofile-compile-policy) '(nte nte1 nte+))
	    (nodejs-compile-workers-inits!))
	 (unless (or (and (<=fx (hop-port) -1) (<=fx (hop-ssl-port) -1))
		     *resolve-service*)
	    (set! *resolve-service* (nodejs-make-resolve-service %this))))))

;*---------------------------------------------------------------------*/
;*    builtin-language? ...                                            */
;*---------------------------------------------------------------------*/
(define (builtin-language? str)
   (member str
      '("hopscript" "html" "json" "hop" "ecmascript5"
	"ecmascript6" "ecmascript2017" "ecmascript2018"
	"ecmascript2019" "ecmascript2020" "ecmascript2021"
	"ecmascript2022" "typescript")))

;*---------------------------------------------------------------------*/
;*    default scope length                                             */
;*---------------------------------------------------------------------*/
(define (SCOPE-ELEMENTS-LENGTH) 128)

;*---------------------------------------------------------------------*/
;*    compile-mutex ...                                                */
;*---------------------------------------------------------------------*/
(define compile-mutex (make-mutex))

(define compile-pending 0)

(define compile-listeners-all '())
(define compile-listeners-start '())
(define compile-listeners-end '())

(define command-line-worker #f)
(define command-line-this #f)
(define command-line-module #f)

;*---------------------------------------------------------------------*/
;*    socompile ...                                                    */
;*---------------------------------------------------------------------*/
(define-struct socompile proc cmd src ksucc kfail abortp)
(define-struct soentry filename lang mtime wslave key)

;*---------------------------------------------------------------------*/
;*    nodejs-compile-client-file ...                                   */
;*    -------------------------------------------------------------    */
;*    This function implements the clientc jsc compiler                */
;*    (see src/parserargs.scm). It is used when a client, i.e., a      */
;*    browser requests a JavaScript module.                            */
;*    See alao runtime/clientc.scm                                     */
;*---------------------------------------------------------------------*/
(define (nodejs-compile-client-file ifile::bstring name::bstring ofile::bstring query)
   (let ((i (string-contains query "&lang=")))
      (multiple-value-bind (%ctxworker %ctxthis %ctxmodule)
	 (nodejs-command-line-dummy-module)
	 (if i
	     (let ((lang (substring query (+fx i 6))))
		(if (string=? lang "hopscript")
		    (nodejs-compile-file-client-hopscript ifile ifile
		       name ofile query #f %ctxworker %ctxthis %ctxmodule lang)
		    (nodejs-compile-client-file-lang ifile name ofile query
		       %ctxworker %ctxthis %ctxmodule lang)))
	     (let ((lang (if (string-suffix? ".ts" ifile)
			     "typescript"
			     "hopscript")))
		(nodejs-compile-file-client-hopscript ifile ifile name ofile query #f
		   %ctxworker %ctxthis %ctxmodule lang))))))

;*---------------------------------------------------------------------*/
;*    nodejs-compile-client-file-lang ...                              */
;*---------------------------------------------------------------------*/
(define (nodejs-compile-client-file-lang ifile name ofile query
	   %ctxworker %ctxthis %ctxmodule lang::bstring)

   (define (filename->file val lang %this)
      (let ((ifile (js-tostring val %this)))
	 (nodejs-compile-file-client-hopscript ifile ifile
	    name ofile query ifile %ctxworker %ctxthis %ctxmodule lang)))

   (define (ast->file val lang %this)
      (nodejs-compile-file-client-hopscript val ifile
	 name ofile query ifile %ctxworker %ctxthis %ctxmodule lang))

   (define (json->file val lang %this)
      (nodejs-compile-json
	 (string-append "string:" (js-jsstring->string val))
	 name ofile query))
   
   (define (value->file val lang %this)
      (with-access::JsGlobalObject %this (js-json)
	 (let* ((stringify (js-get js-json (& "stringify") %this))
		(str (js-call1 %this stringify (js-undefined) val)))
	    (json->file str lang %this))))

   (let ((worker (js-current-worker)))
      (js-worker-exec worker "nodejs-compile-file-lang"
	 (lambda (%this)
	    (with-access::JsGlobalObject %ctxthis (js-object js-symbol)
	       (let* ((exp (nodejs-require-module lang worker %ctxthis %ctxmodule))
		      (key (js-get js-symbol (& "compiler") %ctxthis))
		      (comp (js-get exp key %ctxthis)))
		  (if (js-procedure? comp)
		      (let ((obj (js-call1-jsprocedure %ctxthis comp (js-undefined)
				    (js-string->jsstring ifile))))
			 (when (js-object? obj)
			    (let* ((ty (js-tostring (js-get obj (& "type") %ctxthis) %ctxthis))
				   (val (js-get obj (& "value") %ctxthis))
				   (l (js-get obj (& "language") %ctxthis))
				   (lang (if (eq? l (js-undefined))
					     lang
					     (js-language->string l
						"nodejs-compile-client-file-lang"
						%ctxthis))))
			       (cond
				  ((string=? ty "filename")
				   (filename->file val lang %ctxthis))
				  ((string=? ty "json")
				   (json->file val lang %ctxthis))
				  ((string=? ty "value")
				   (value->file val lang %ctxthis))
				  ((string=? ty "error")
				   (raise val))
				  ((string=? ty "ast")
				   (ast->file val lang %ctxthis))
				  (else
				   (js-raise-error %ctxthis
				      "Wrong language compiler output"
				      lang))))))
		      (js-raise-error %ctxthis
			 (format "Wrong language (~s) object `~a'"
			    lang (typeof comp))
			 lang))))))))

;*---------------------------------------------------------------------*/
;*    nodejs-compile-file-client-hopscript ...                         */
;*---------------------------------------------------------------------*/
(define (nodejs-compile-file-client-hopscript obj ifile name ofile
	   query srcalias %ctxworker %ctxthis %ctxmodule lang::bstring)
   (let* ((srcmap (when (>fx (nodejs-hop-debug) 0)
		     (string-append ofile ".map")))
	  (op (if (string=? ofile "-")
		  (current-output-port)
		  (open-output-file ofile)))
	  (qr (cond
		 ((string-prefix? "js=" query) 'js)
		 ((string-prefix? "mjs=" query) 'mjs)
		 ((string-prefix? "es=" query) 'es)
		 (else 'js)))
	  (tree (unwind-protect
		   (module->javascript obj ifile
		      name op #f #f srcmap qr srcalias
		      %ctxworker %ctxthis %ctxmodule lang)
		   (unless (eq? op (current-output-port))
		      (close-output-port op)))))
      (when (>fx (nodejs-hop-debug) 0)
	 (call-with-output-file srcmap
	    (lambda (p)
	       (generate-source-map tree ifile ofile p))))))

;*---------------------------------------------------------------------*/
;*    nodejs-compile-json ...                                          */
;*---------------------------------------------------------------------*/
(define (nodejs-compile-json ifile::bstring name::bstring ofile::bstring query)
   
   (define (compile-json op)
      (let ((str (call-with-input-file ifile read-string)))
	 (if (string=? query "es")
	     (display str op)
	     (begin
		(fprintf op "hop[ '%requires' ][ ~s ] = function() {\n"
		   ifile)
		(fprintf op "var exports = ~a; var module = { id: ~s, filename: ~s, loaded: true, exports: exports, paths: [~a] };\nhop[ '%modules' ][ '~a' ] = module.exports;\nfunction require( url ) { return hop[ '%require' ]( url, module ) }\n return exports; }\n"
		   str
		   name ifile
		   (js-paths (nodejs-filename->paths ifile))
		   ifile)))))
   
   (if (string=? ofile "-")
       (compile-json (current-output-port))
       (call-with-output-file ofile compile-json)))

;*---------------------------------------------------------------------*/
;*    nodejs-compile-html ...                                          */
;*---------------------------------------------------------------------*/
(define (nodejs-compile-html ifile::bstring name::bstring ofile::bstring query)

   (define (compile-html filename)
      (call-with-input-file filename
	 (lambda (in)
	    (debug-compile-trace "nodejs-compile-html" filename)
	    (let ((tree (j2s-compile in
			   :filename filename
			   :parser 'client-program
			   :site 'client
			   :driver (j2s-javascript-driver)
			   :warning-global #f
			   :driver-name "j2s-javascript-driver"
			   :node-modules-directory (nodejs-node-modules-directory))))
	       (filter (lambda (exp)
			  (not (isa? exp J2SNode)))
		  tree)))))
   
   (define (compile-json op)
      (let ((strs (compile-html ifile)))
	 (if (string=? query "es")
	     (for-each (lambda (e) (display e op)) strs)
	     (begin
		(fprintf op "hop[ '%requires' ][ ~s ] = function() {\n"
		   ifile)
		(fprintf op "var exports = ~a; var module = { id: ~s, filename: ~s, loaded: true, exports: exports, paths: [~a] };\nhop[ '%modules' ][ '~a' ] = module.exports;\nfunction require( url ) { return hop[ '%require' ]( url, module ) }\n return exports; }\n"
		   (apply string-append strs)
		   name ifile
		   (js-paths (nodejs-filename->paths ifile))
		   ifile)))))
   
   (if (string=? ofile "-")
       (compile-json (current-output-port))
       (call-with-output-file ofile compile-json)))

;*---------------------------------------------------------------------*/
;*    nodejs-compile-pending ...                                       */
;*---------------------------------------------------------------------*/
(define (nodejs-compile-pending)
   (synchronize compile-mutex
      (+fx compile-pending (length socompile-queue))))

;*---------------------------------------------------------------------*/
;*    nodejs-compile-add-event-listener! ...                           */
;*---------------------------------------------------------------------*/
(define (nodejs-compile-add-event-listener! event proc capture)
   (synchronize compile-mutex
      (cond
	 ((string=? event "all")
	  (set! compile-listeners-all (cons proc compile-listeners-all))
	  (when (=fx compile-pending 0)
	     (let ((evt (instantiate::event
			   (name "all")
			   (target (js-undefined))
			   (value 0))))
		(apply-listeners compile-listeners-all evt))))
	 ((string=? event "end")
	  (set! compile-listeners-end (cons proc compile-listeners-end)))
	 ((string=? event "start")
	  (set! compile-listeners-start (cons proc compile-listeners-start)))))
   #f)

;*---------------------------------------------------------------------*/
;*    nodejs-compile-remove-event-listener! ...                        */
;*---------------------------------------------------------------------*/
(define (nodejs-compile-remove-event-listener! event proc)
   (synchronize compile-mutex
      (cond
	 ((string=? event "all")
	  (set! compile-listeners-all (remq! proc compile-listeners-all)))
	 ((string=? event "start")
	  (set! compile-listeners-start (remq! proc compile-listeners-start)))
	 ((string=? event "end")
	  (set! compile-listeners-end (remq! proc compile-listeners-end)))))
   #f)

;*---------------------------------------------------------------------*/
;*    hop-boot ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (hop-boot)
   (file->string "../share/hop-boot.js"))

;*---------------------------------------------------------------------*/
;*    module->javascript ...                                           */
;*---------------------------------------------------------------------*/
(define (module->javascript obj filename id op compile isexpr
	   srcmap query srcalias
	   %ctxworker %ctxthis::JsGlobalObject %ctxmodule::JsObject
	   lang::bstring)
   
   (define (init-dummy-module! this worker)
      (with-access::JsGlobalObject this (js-object)
	 (let ((mod (js-new0 this js-object))
	       (exp (js-new0 this js-object)))
	    (js-put! mod (& "exports") this #f this)
	    (js-put! mod (& "filename") (js-string->jsstring filename) #f this)
	    (js-put! this (& "global") this #f this)
	    (js-put! this (& "GLOBAL") this #f this)
	    (js-put! this (& "module") mod #f this)
	    (js-put! this (& "exports") exp #f this)
	    (js-put! this (& "__filename")
	       (js-string->jsstring filename) #f this)
	    (js-put! this (& "__dirname")
	       (js-string->jsstring (dirname filename)) #f this)
	    (js-put! this (& "require")
	       (nodejs-require worker this mod "hopscript") #f this)
	    (js-put! this (& "process")
	       (nodejs-process worker this) #f this)
	    (js-put! this (& "console")
	       (nodejs-require-core "console" worker this) #f this))))

   (define (hopscript->javascript obj filename header lang::bstring esplainp this worker)
      (debug-compile-trace "hopscript->javascript" filename)
      (j2s-compile obj
	 :%this this
	 :source filename
	 :resource (dirname filename)
	 :filename filename
	 :node-modules-directory (nodejs-node-modules-directory)
	 :worker worker
	 :header header
	 :verbose (if (>=fx (hop-verbose) 10) (-fx (hop-verbose) 10) 0)
	 :parser 'client-program
	 :es6-module-client (eq? query 'mjs)
	 :warning-global #f
	 :driver (if (or (<=fx (nodejs-hop-debug) 0) esplainp)
		     (j2s-javascript-driver)
		     (j2s-javascript-debug-driver))
	 :driver-name (if (or (<=fx (nodejs-hop-debug) 0) "esplainp")
			  "j2s-javascript-driver"
			  "j2s-javascript-debug-driver")
	 :language lang
	 :site 'client
	 :plugins-loader (make-plugins-loader %ctxthis %ctxmodule (js-current-worker))
	 :debug (if esplainp 0 (nodejs-hop-debug))))
   
   (define (compile obj header offset esplainp worker this)
      (let ((tree (hopscript->javascript obj filename
		     header lang esplainp this worker)))
	 (for-each (lambda (exp)
		      (unless (isa? exp J2SNode)
			 ;; skip node information, used
			 ;; for sourcemap generation
			 (display exp op)))
	    tree)
	 (when header
	    (display "\nreturn module.exports;}\n" op)
	    (when srcmap
	       (fprintf op "\n\nhop_source_mapping_url(~s, \"~a\");\n"
		  (or srcalias filename) srcmap)
	       (fprintf op "\n//# sourceMappingURL=~a\n" srcmap)))
	 ;; first element of the tree is a position offset
	 ;; see sourcemap generation
	 (cons offset tree)))

   (js-worker-exec %ctxworker "module->javascript"
      (lambda (%this)
	 (let ((esplainp (memq query '(mjs es ts))))
	    (init-dummy-module! %ctxthis %ctxworker)
	    (let ((header (unless esplainp
			     (format "var exports = {}; var module = { id: ~s, filename: ~s, loaded: true, exports: exports, paths: [~a] };\nhop[ '%modules' ][ '~a' ] = module.exports;\nfunction require( url ) { return hop[ '%require' ]( url, module ) }\n"
				id filename
				(js-paths (nodejs-filename->paths filename))
				filename))))
	       (when header
		  (fprintf op (hop-boot))
		  (fprintf op "hop[ '%requires' ][ ~s ] = function() {\n"
		     (or srcalias filename))
		  (flush-output-port op))
	       (let ((offset (output-port-position op)))
		  (if (string? obj)
		      (call-with-input-file filename
			 (lambda (in)
			    (compile in header offset esplainp
			       %ctxworker %ctxthis)))
		      (compile obj header offset esplainp
			 %ctxworker %ctxthis))))))))

;*---------------------------------------------------------------------*/
;*    nodejs-command-line-dummy-module ...                             */
;*    -------------------------------------------------------------    */
;*    This function creates a JS global object and a JS module that    */
;*    are used to load the file from the top-level, i.e., those        */
;*    files that are invoked on the command line or for client-side    */
;*    compilation.                                                     */
;*---------------------------------------------------------------------*/
(define (nodejs-command-line-dummy-module)
   (synchronize compile-mutex
      (unless command-line-worker
	 (multiple-value-bind (%worker %global %module)
	    (js-main-worker! "compile" (pwd)
	       #t nodejs-new-global-object nodejs-new-module)
	    (set! command-line-worker %worker)
	    (set! command-line-this %global)
	    (set! command-line-module %module)))
      (values command-line-worker command-line-this command-line-module)))

;*---------------------------------------------------------------------*/
;*    js-paths ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-paths vec)
   (let ((len (-fx (vector-length vec) 1)))
      (call-with-output-string
	 (lambda (p)
	    (let loop ((i 0))
	       (when (<=fx i len)
		  (display "'" p)
		  (display (vector-ref vec i) p)
		  (display "'" p)
		  (if (<fx i len) (display ", " p))
		  (loop (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    nodejs-driver ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-driver)
   (if (> (nodejs-hop-debug) 0) (j2s-debug-driver) (j2s-optim-driver)))

;*---------------------------------------------------------------------*/
;*    nodejs-file-paths ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-file-paths filename %this)
   (js-vector->jsarray (nodejs-filename->paths (js-tostring filename %this)) %this))

;*---------------------------------------------------------------------*/
;*    nodejs-file->paths ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-filename->paths::vector file::bstring)
   (cond
      ((string-null? file)
       (vector (pwd)))
      ((char=? (string-ref file 0) #\/)
       (let loop ((dir (dirname file))
		  (acc '()))
	  (cond
	     ((string=? dir "/")
	      (list->vector
		 (reverse! (cons (js-string->jsstring "/node_modules") acc))))
	     ((string-suffix? "/node_modules" dir)
	      (loop (dirname dir) acc))
	     (else
	      (loop (dirname dir)
		 (cons*
		    (js-string->jsstring dir)
		    (js-string->jsstring (make-file-name dir "node_modules"))
		    acc))))))
      (else
       (nodejs-filename->paths (file-name-canonicalize (make-file-name (pwd) file))))))

;*---------------------------------------------------------------------*/
;*    nodejs-new-module-sans-cache ...                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-new-module-sans-cache::JsObject id::bstring filename worker::WorkerHopThread %this::JsGlobalObject)

   (define (module-init! m)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((expo (js-new0 %this js-object)))
	    (with-access::JsObject expo (cmap)
	       (set! cmap (js-make-jsconstructmap :single #t)))
	    ;; id field
	    (js-put! m (& "id") (js-string->jsstring id) #f %this)
	    ;; exports
	    (js-put! m (& "exports") expo #f %this)
	    ;; filename
	    (js-put! m (& "filename") (js-string->jsstring filename) #f %this)
	    ;; loaded
	    (js-put! m (& "loaded") #f #f %this)
	    ;; parent
	    (js-put! m (& "parent") (js-null) #f %this)
	    ;; children
	    (js-put! m (& "children") (js-vector->jsarray '#() %this) #f %this)
	    ;; paths
	    (js-put! m (&& "paths")
	       (js-vector->jsarray (nodejs-filename->paths filename) %this)
	       #f %this))))

   (with-trace 'require (format "nodejs-new-module-sans-cache ~a ~a" id filename)
      (with-access::JsGlobalObject %this (js-object js-symbol-tostringtag js-initial-cmap)
	 (let ((m (instantiateJsModule
		     (cmap js-initial-cmap)
		     (__proto__ (js-object-proto %this)))))
	    (js-bind! %this m js-symbol-tostringtag
	       :value (& "Module")
	       :configurable #f :writable #f :enumerable #f)
	    ;; module properties
	    (module-init! m)
	    ;; register the module in the current worker thread
	    ;; return the newly allocated module
	    (trace-item "module=" (typeof m))
	    m))))
;*---------------------------------------------------------------------*/
;*    nodejs-new-module ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-new-module::JsObject id::bstring filename worker::WorkerHopThread %this::JsGlobalObject)
   (with-trace 'require (format "nodejs-new-module ~a ~a" id filename)
      (let ((m (nodejs-new-module-sans-cache id filename worker %this)))
	 ;; register the module in the current worker thread
	 (with-access::WorkerHopThread worker (module-cache)
	    (when (js-object? module-cache)
	       (js-put! module-cache
		  (js-string->jsstring filename) m #f %this)))
	 m)))

;*---------------------------------------------------------------------*/
;*    wrong-language ...                                               */
;*---------------------------------------------------------------------*/
(define (wrong-language lang tag this)
   (js-raise-error this
      (format "~s: Wrong language object (~s)" tag (typeof lang))
      lang))

;*---------------------------------------------------------------------*/
;*    js-language->string ...                                          */
;*---------------------------------------------------------------------*/
(define (js-language->string lang tag this)
   (cond
      ((string? lang) lang)
      ((js-jsstring? lang) (js-jsstring->string lang))
      (else (wrong-language lang tag this))))

;*---------------------------------------------------------------------*/
;*    language-compiler ...                                            */
;*---------------------------------------------------------------------*/
(define (language-compiler language lang this %module worker)
   (let loop ((lang lang))
      (cond
	 ((js-object? lang)
	  (with-access::JsGlobalObject this (js-symbol)
	     (let* ((key (js-get js-symbol (& "compiler") this))
		    (comp (js-get lang key this)))
		(if (js-procedure? comp)
		    comp
		    (wrong-language lang "language-compiler" this)))))
	 ((js-jsstring? lang)
	  (let ((str (js-jsstring->string lang)))
	     (unless (builtin-language? str)
		(let ((langmod (nodejs-require-module str
				  worker this %module)))
		   (loop langmod)))))
	 ((string? lang)
	  (let ((str lang))
	     (unless (builtin-language? str)
		(let ((langmod (nodejs-require-module str
				  worker this %module)))
		   (loop langmod)))))
	 ((not (builtin-language? language))
	  (let ((langmod (nodejs-require-module language
			    worker this %module)))
	     (loop langmod)))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    language-script ...                                              */
;*    -------------------------------------------------------------    */
;*    This is used by the SCRIPT HTML tag to insert per-langage        */
;*    header.                                                          */
;*---------------------------------------------------------------------*/
(define (language-script lang %this %module worker)
   (let loop ((lang lang))
      (cond
	 ((js-object? lang)
	  (with-access::JsGlobalObject %this (js-symbol)
	     (let* ((key (js-get js-symbol (& "script") %this))
		    (comp (js-get lang key %this)))
		(if (js-procedure? comp)
		    comp
		    (wrong-language lang "language-script" %this)))))
	 ((eq? lang (& "hopscript"))
	  #f)
	 ((js-jsstring? lang)
	  (let ((str (js-jsstring->string lang)))
	     (let ((langmod (nodejs-require-module str
			       worker %this %module)))
		(loop langmod))))
	 ((string? lang)
	  (let ((str lang))
	     (let ((langmod (nodejs-require-module str
			       worker %this %module)))
		(loop langmod))))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    nodejs-require ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-require worker::WorkerHopThread this::JsGlobalObject %module::JsObject language::bstring)

   ;; require
   (define require
      (js-make-function this
	 (lambda (_ name lang opt)
	    (nodejs-require-module (js-tostring name this)
	       (js-current-worker) this %module
	       (if (eq? lang (js-undefined))
		   language
		   (js-language->string lang
		      (format "nodejs-require (~s)" name)this))
	       (language-compiler language lang this %module
		  (js-current-worker))
	       opt))
	 (js-function-arity 3 0)
	 (js-function-info :name "require" :len 3)
	 :size 4))

   (js-init-require! this)
   
   ;; require.lang
   (js-bind! this require (& "lang")
      :get (js-make-function this
	      (lambda (_)
		 (js-string->jsstring language))
	      (js-function-arity 0 0)
	      (js-function-info :name "lang" :len 0))
      :set (js-make-function this
	      (lambda (_ val)
		 (set! language (js-language->string val "lang" this))
		 val)
	      (js-function-arity 1 0)
	      (js-function-info :name "lang" :len 1))
      :configurable #f :writable #t)
   
   ;; require.main
   (with-access::JsGlobalObject this (js-main js-object) 
      (js-bind! this require (& "main")
	 :get (js-make-function this (lambda (this) js-main)
		 (js-function-arity 0 0)
		 (js-function-info :name "main" :len 0))
	 :configurable #f :writable #f))
   
   ;; require.resolve
   (js-bind! this require (& "resolve")
      :value (js-make-function this
		(lambda (_ name opts)
		   (let ((name (js-tostring name this)))
		      (if (core-module? name)
			  (js-string->jsstring name)
			  (let ((paths (when (js-object? opts)
					  (js-get opts (&& "paths") this))))
			     (js-string->jsstring
				(nodejs-resolve name this %module
				   (if (js-array? paths)
				       (map! (lambda (v)
						(js-tostring v this))
					  (jsarray->list paths this))
				       (node-module-paths %module this))
				   'body))))))
		(js-function-arity 2 0)
		(js-function-info :name "resolve" :len 2))
      :configurable #t :writable #t :enumerable #t)

   ;; require.cache
   (with-access::WorkerHopThread worker (module-cache)
      (js-bind! this require (& "cache")
	 :get (js-make-function this
		 (lambda (this) module-cache)
		 (js-function-arity 0 0)
		 (js-function-info :name "cache" :len 0))
	 :set (js-make-function this
		 (lambda (this v)
		    ;; when setting require.cache, erase the compilation
		    ;; table for avoid out of sync errors
		    (set! module-cache v))
		 (js-function-arity 1 0)
		 (js-function-info :name "cache" :len 1))
	 :configurable #f))

   ;; module.require
   (js-bind! this %module (& "require")
      :value require
      :enumerable #f)

   require)

;*---------------------------------------------------------------------*/
;*    node-module-paths ...                                            */
;*---------------------------------------------------------------------*/
(define (node-module-paths mod %this)
   (let ((paths (js-get mod (&& "paths") %this)))
      (cond
	 ((pair? paths)
	  (append (map js-jsstring->string paths)) nodejs-env-path)
	 ((js-array? paths)
	  (with-access::JsArray paths (vec)
	     (append (map! js-jsstring->string (vector->list vec))
		nodejs-env-path)))
	 (else
	  nodejs-env-path))))

;*---------------------------------------------------------------------*/
;*    nodejs-env-path ...                                              */
;*---------------------------------------------------------------------*/
(define nodejs-env-path
   (let* ((sys-path (nodejs-modules-directory))
	  (home-path (let ((home (getenv "HOME")))
			(if (string? home)
			    (list (make-file-name home ".node_modules")
			       (make-file-name home ".node_libraries")
			       sys-path)
			    (list sys-path)))))
      (let ((env (getenv "NODE_PATH")))
	 (if (string? env)
	     (append (unix-path->list env)
		(cons (hop-weblets-directory)
		   home-path))
	     home-path))))

;*---------------------------------------------------------------------*/
;*    nodejs-import-module ...                                         */
;*    -------------------------------------------------------------    */
;*    ES6 module import.                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-import-module::JsModule worker::WorkerHopThread
	   %this::JsGlobalObject %module::JsObject
	   path::bstring checksum::long commonjs-export::bool loc)
   (let* ((respath (nodejs-resolve path %this %module (node-module-paths %module %this) 'import))
	  (mod (nodejs-load-module respath worker %this %module
		  :commonjs-export commonjs-export :loc loc)))
      (with-access::JsModule mod ((mc checksum))
	 (if (or (=fx checksum 0) (=fx checksum mc) (=fx mc 0))
	     mod
	     (js-raise-type-error/loc %this loc
		"corrupted module ~s"
		(js-get mod (& "filename") %this))))))

;*---------------------------------------------------------------------*/
;*    nodejs-import-module-core ...                                    */
;*    -------------------------------------------------------------    */
;*    Function used to (es6-)import bindings from a core module.       */
;*    As these modules do not contain any ES6 export declarations,     */
;*    this function extracts from MODULE.EXPORTS exported bindings.    */
;*---------------------------------------------------------------------*/
(define (nodejs-import-module-core::JsModule worker::WorkerHopThread
	   %this::JsGlobalObject %module::JsObject
	   path::bstring checksum::long commonjs-export::bool
	   loc symbols::vector)
   
   (define (import-var exports sym)
      (let* ((name (js-string->jsstring (symbol->string! sym)))
	     (v (js-get exports name %this)))
	 v))
   
   (let* ((mod (nodejs-import-module worker %this %module path
		  checksum commonjs-export loc))
	  (exports (js-get mod (& "exports") %this)))
      (duplicate::JsModule mod
	 (evars (vector-map (lambda (s) (import-var exports s)) symbols)))))
   
;*---------------------------------------------------------------------*/
;*    nodejs-import-module-hop ...                                     */
;*    -------------------------------------------------------------    */
;*    Function used to (es6-)import bindings from a Hop module.        */
;*---------------------------------------------------------------------*/
(define (nodejs-import-module-hop::JsModule worker::WorkerHopThread
	   %this::JsGlobalObject %module::JsObject
	   path::bstring checksum::long commonjs-export::bool
	   loc symbols::vector)
   
   (define (import-var mod sym sopath)
      (with-access::JsModule mod (%module)
	 (cond
	    ((evmodule? %module)
	     (call-with-eval-module %module
		(lambda ()
		   (let ((dyn::dynamic-env (current-dynamic-env))
			 (mod (evmodule-name %module)))
		      (let ()
			 (%env-push-trace dyn mod loc)
			 (let ((v (eval `(@ ,(car sym) ,mod))))
			    (%env-pop-trace dyn)
			    v))))))
	    ((string? %module)
	     (dynamic-load-symbol-get
		(dynamic-load-symbol sopath
		   (if (eq? (cdr sym) 'procedure)
		       (string-append (symbol->string (car sym)) "-env")
		       (symbol->string (car sym)))
		   %module))))))

   (let ((mod (nodejs-import-module worker %this %module path
		 checksum commonjs-export loc))
	 (sopath (hop-sofile-path path)))
      (with-access::JsModule mod (evars)
	 (set! evars
	    (vector-map! (lambda (s) (import-var mod s sopath)) symbols)))
      mod))

;*---------------------------------------------------------------------*/
;*    nodejs-import-module-dynamic ...                                 */
;*    -------------------------------------------------------------    */
;*    As of October 2018, this is still a mere proposal. See           */
;*    https://github.com/tc39/proposal-dynamic-import                  */
;*---------------------------------------------------------------------*/
(define (nodejs-import-module-dynamic::JsPromise worker::WorkerHopThread
	   %this::JsGlobalObject %module::JsObject name::obj
	   base::bstring commonjs-export::bool loc)
   (with-access::JsGlobalObject %this (js-promise)
      (let ((name (js-tostring name %this)))
	 (js-new1 %this js-promise
	    (js-make-function %this
	       (lambda (this resolve reject)
		  (with-handler
		     (lambda (exn)
			(js-call1 %this reject (js-undefined) exn))
		     (let* ((path (nodejs-resolve name %this %module
				     (node-module-paths %module %this) 'import))
			    (mod (nodejs-import-module worker %this %module
				    path 0 commonjs-export loc)))
			(js-call1 %this resolve (js-undefined)
			   (nodejs-module-namespace mod worker %this)))))
	       (js-function-arity 2 0)
	       (js-function-info :name "import" :len 2))))))

;*---------------------------------------------------------------------*/
;*    nodejs-import-meta ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-import-meta::JsObject worker::WorkerHopThread
	   %this::JsGlobalObject %module::JsObject url::JsStringLiteral)
   (let ((obj (instantiateJsObject
		 (__proto__ (js-null))
		 (elements ($create-vector 2)))))
      (js-bind! %this obj (& "url")
	 :value url)
      (js-bind! %this obj (& "vendor")
	 :value (js-string->jsstring "hop"))
      obj))

;*---------------------------------------------------------------------*/
;*    nodejs-module-namespace ...                                      */
;*---------------------------------------------------------------------*/
(define (nodejs-module-namespace::JsObject mod::JsModule worker::WorkerHopThread %this)
   
   (define (constant? n)
      (or (number? n) (boolean? n) (string? n) (js-jsstring? n)))

   (define (module-filename mod %this)
      (js-get mod (& "filename") %this))

   (define debug #f)
   
   (define (bind-export! modobj mod id export margin)
      (with-access::JsModule mod (evars imports redirects)
	 (let* ((idx (js-evar-info-index export))
		(redirect (js-evar-info-redirect export))
		(writable (js-evar-info-writable export)))
	    (when debug
	       (tprint margin "export="  export " "
		  (module-filename mod %this)))
	    (cond
	       ((null? idx)
		;; namespace export
		(when debug
		   (tprint margin "namespace..."))
		(let ((mod (vector-ref imports (car redirect))))
		   (bind-exports! modobj mod (string-append margin " "))))
	       ((pair? redirect)
		(when debug
		   (tprint margin "redirect..."))
		;; variable reexport
		(let loop ((idx idx)
			   (redirect redirect)
			   (mod mod))
		   (with-access::JsModule mod (imports exports)
		      (when debug
			 (tprint (string-append margin " ")
			    "idx=" idx " red=" redirect " "
			    (module-filename mod %this)))
		      (if (null? redirect)
			  (bind-export! modobj mod id
			     (vector-ref exports (car idx))
			     (string-append margin " "))
			  (let ((mod (vector-ref imports (car redirect))))
			     (with-access::JsModule mod (exports)
				(loop (cdr idx) (cdr redirect) mod)))))))
	       ((not writable)
		;; constant export
		(when debug
		   (tprint margin "bind.1 " id " " idx " " evars))
		(js-bind! %this modobj id
		   :value (vector-ref evars (car idx))
		   :configurable #f :writable #f))
	       (else
		;; variable export
		(when debug
		   (tprint margin "bind.2 " id))
		(js-bind! %this modobj id
		   :get (js-make-function %this
			   (lambda (this)
			      (vector-ref evars (car idx)))
			   (js-function-arity 0 0)
			   (js-function-info :name "get" :len 0))
		   :configurable #f :writable #f))))))

   (define (bind-exports! modobj mod margin)
      (with-access::JsModule mod (evars exports imports redirects)
	 (vector-for-each
	    (lambda (export)
	       (let ((id (js-evar-info-id export)))
		  (bind-export! modobj mod id export margin)))
	    exports)))
   
   (define (module-namespace mod::JsModule)
      (with-access::JsGlobalObject %this (js-symbol-tostringtag)
	 (with-access::JsModule mod (evars exports imports redirects)
	    (let ((modobj (instantiateJsObject
			     (__proto__ (js-object-proto %this)))))
	       (bind-exports! modobj mod "")
	       (js-bind! %this modobj js-symbol-tostringtag
		  :value (js-string->jsstring "Module")
		  :configurable #f :writable #f :enumerable #f)
	       (let ((commonjs (js-get mod (& "exports") %this)))
		  (with-access::JsModule mod (%module)
		     (when (isa? commonjs JsObject)
			(js-for-in commonjs
			   (lambda (id %this)
			      (js-bind! %this modobj id
				 :value (js-get commonjs id %this)
				 :configurable #t :writable #t))
			   %this))))
	       modobj))))
   
   (when debug
      (tprint "NS " (module-filename mod %this)))
   
   (with-access::JsModule mod (namespace)
      (or namespace
	  (let ((nm (module-namespace mod)))
	     (set! namespace nm)
	     nm))))

;*---------------------------------------------------------------------*/
;*    nodejs-head ...                                                  */
;*    -------------------------------------------------------------    */
;*    Per module version of js-html-script@__hopscript_public          */
;*    (see hopscript/public.scm).                                      */
;*---------------------------------------------------------------------*/
(define (nodejs-head worker::WorkerHopThread %this::JsGlobalObject %scope::JsObject %module)
   
   ;; head
   (define head
      (js-make-function %this
	 (lambda (this attrs . nodes)
	    (let ((rts (if (and (js-object? attrs)
				(js-has-property attrs (& "rts") %this))
			   (js-toboolean (js-get attrs (& "rts") %scope))
			   (hop-runtime-client))))
	       (apply <HEAD> :idiom "javascript" :%context %scope
		  (unless (eq? rts #f)
		     (<SCRIPT>
			(format "hop[ '%root' ] = ~s"
			   (dirname
			      (js-jsstring->string
				 (js-get %module (& "filename") %scope))))))
		  (when (js-object? attrs)
		     (js-object->keyword-arguments* attrs %this))
		  (filter (lambda (n)
			     (or (isa? n xml-tilde)
				 (isa? n xml-markup)
				 (isa? n xml-cdata)))
		     (xml-body nodes %this)))))
	 (js-function-arity 1 -1 'scheme)
	 (js-function-info :name "HEAD" :len -1)))
   head)

;*---------------------------------------------------------------------*/
;*    nodejs-script ...                                                */
;*    -------------------------------------------------------------    */
;*    Per module version of js-html-script@__hopscript_public          */
;*    (see hopscript/public.scm).                                      */
;*---------------------------------------------------------------------*/
(define (nodejs-script worker::WorkerHopThread %this::JsGlobalObject %scope::JsObject %module)
   
;*    (define (ast->string ast lang %this)                             */
;*       (j2s-compile ast                                              */
;* 	 :%this %this                                                  */
;* 	 :filename "ast"                                               */
;* 	 :node-modules-directory (nodejs-node-modules-directory)       */
;* 	 :worker worker                                                */
;* 	 :header '()                                                   */
;* 	 :verbose 0                                                    */
;* 	 :parser 'client-program                                       */
;* 	 :es6-module-client #t                                         */
;* 	 :warning-global #f                                            */
;* 	 :driver (j2s-javascript-driver)                               */
;* 	 :driver-name "j2s-javascript-driver"                          */
;* 	 :language lang                                                */
;* 	 :site 'client                                                 */
;* 	 :plugins-loader (make-plugins-loader %this %module worker)    */
;* 	 :debug 0)                                                     */
;*       )                                                             */
   
;*    (define (json->file val lang %this)                              */
;*       (nodejs-compile-json                                          */
;* 	 (string-append "string:" (js-jsstring->string val))           */
;* 	 name ofile query))                                            */
   
;*    (define (value->string val lang %this)                           */
;*       (with-access::JsGlobalObject %this (js-json)                  */
;* 	 (let* ((stringify (js-get js-json (& "stringify") %this))     */
;* 		(str (js-call1 %this stringify (js-undefined) val)))   */
;* 	    (json->string str lang %this))))                           */
   
   (define (script-language-header attrs nodes)
      (when (js-object? attrs)
	 (let ((lang (js-get attrs (& "lang") %this)))
	    (if (js-jsstring? lang)
		(let ((comp (language-script lang %this %module worker)))
		   (when (js-procedure? comp)
		      (let ((obj (js-call1-jsprocedure %this comp
				    (js-undefined)
				    attrs)))
			 (when (js-object? obj)
			    (let* ((ty (js-get obj (& "type") %this))
				   (val (js-get obj (& "value") %this)))
			       (cond
;* 			      ((string=? ty "ast")                     */
;* 			       (ast->string val lang %this))           */
				  ((eq? ty (& "string"))
				   (js-tostring val %this))
				  (else
				   (js-raise-error %this
				      "Wrong language compiler output format ~s"
				      lang))))))))
		(let ((ty (js-get attrs (& "type") %this)))
		   (when (eq? ty (& "hop"))
		      (js-put! attrs (& "type") (& "module") #f %this)
		      "import * as hop from '/hop/0/hop-client.mjs';"))))))
   
   (define (script-lang attrs nodes lang)
      (let* ((comp (language-script lang %this %module worker))
	     (hd (when (js-procedure? comp)
		    (let ((obj (js-call1-jsprocedure %this comp
				  (js-undefined)
				  attrs)))
		       (when (js-object? obj)
			  (let* ((ty (js-get obj (& "type") %this))
				 (val (js-get obj (& "value") %this)))
			     (cond
				((eq? ty (& "string"))
				 (js-tostring val %this))
				(else
				 (js-raise-error %this
				    "Wrong language compiler output format ~s"
				    lang)))))))))
	 (as-array %this
	    (apply <SCRIPT> :idiom "javascript"
	       :%context %scope :module %module
	       (js-object->keyword-arguments* attrs %this)
	       (if hd (cons hd nodes) nodes)))))
   
   (define (script-hop attrs nodes)
      (as-array %this
	 (apply <SCRIPT> :idiom "javascript"
	    :%context %scope :module %module
	    (js-object->keyword-arguments* attrs %this)
	    (cons* :type "module"
	       "import * as hop from '/hop/0/hop-client.mjs';"
	       nodes))))
   
   (define (as-array %this tmp)
      (if (pair? tmp)
	  (js-vector->jsarray (list->vector tmp) %this)
	  tmp))
   
   (define script
      (js-make-function %this
	 (lambda (this attrs . nodes)
	    (if (not (js-object? attrs))
		(as-array %this
		   (apply <SCRIPT> :idiom "javascript"
		      :%context %scope :module %module
		      nodes))
		(let ((lang (js-get attrs (& "lang") %this)))
		   (if (js-jsstring? lang)
		       (as-array %this (script-lang attrs nodes lang))
		       (let ((ty (js-get attrs (& "type") %this)))
			  (if (eq? ty (& "hop"))
			      (as-array %this (script-hop attrs nodes))
			      (as-array %this
				 (apply <SCRIPT> :idiom "javascript"
				    :%context %scope :module %module
				    (js-object->keyword-arguments* attrs %this)
				    nodes))))))))
	 (js-function-arity 1 -1 'scheme)
	 (js-function-info :name "SCRIPT" :len -1)))
   
   script)

;*---------------------------------------------------------------------*/
;*    *resolve-service* ...                                            */
;*---------------------------------------------------------------------*/
(define *resolve-service* #f)
(define *resolve-url-path* "public/require/resolve")

;*---------------------------------------------------------------------*/
;*    nodejs-new-global-object ...                                     */
;*---------------------------------------------------------------------*/
(define (nodejs-new-global-object #!key name)
   (let ((%this (js-new-global-object :size 256 :name name)))
      (js-init-require! %this)
      (with-access::JsGlobalObject %this (js-object js-nodejs-pcache)
	 ;; allocate the pcache for the nodejs modules
	 (set! js-nodejs-pcache
	    ((@ js-make-pcache-table __hopscript_property) 25 "nodejs"))
	 ;; mark object non-enumerable (i.e., it contains no enumerable
	 ;; property) in order to optimize for..in
	 (js-object-mode-enumerable-set! (js-object-proto %this) #f)
	 ;; bind the builtin hop functions
	 (js-init-hop-builtin! %this %this)
	 ;; bind the v8 builtin
	 (nodejs-init-v8-global-object! %this (js-object-proto %this))
	 (nodejs-init-v8-global-object-prototype! %this %this))
      %this))

;*---------------------------------------------------------------------*/
;*    nodejs-make-resolve-service ...                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-make-resolve-service %this::JsGlobalObject)
   (service :name *resolve-url-path* (name filename)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((mod (js-new0 %this js-object)))
	    ;; module properties
	    (js-put! mod (& "id") (js-string->jsstring filename) #f %this)
	    ;; filename
	    (js-put! mod (& "filename") (js-string->jsstring filename) #f %this)
	    ;; paths
	    (js-put! mod (&& "paths")
	       (js-vector->jsarray (nodejs-filename->paths filename) %this)
	       #f %this)
	    ;; the resolution
	    (nodejs-resolve name %this mod (node-module-paths mod %this) 'body)))))

;*---------------------------------------------------------------------*/
;*    nodejs-init-v8-global-object! ...                                */
;*---------------------------------------------------------------------*/
(define (nodejs-init-v8-global-object! %this::JsGlobalObject proto::JsObject)
   ;; global values
   (cond-expand
      (gc (js-bind! %this %this (& "gc")
	     :value (js-make-function %this
		       (lambda (this) (gc))
		       (js-function-arity 0 0)
		       (js-function-info :name "gc" :len 0))
	     :enumerable #f
	     :writable #t
	     :configurable #f)))
   ;; v8 compatibility (used by nodejs/lib)
   (js-bind! %this proto (& "__defineGetter__")
      :value (js-make-function %this
		(lambda (this name fun) (js-bind! %this this name :get fun))
		(js-function-arity 2 0)
		(js-function-info :name "__defineGetter__" :len 2))
      :enumerable #f
      :writable #t
      :configurable #f))

;*---------------------------------------------------------------------*/
;*    nodejs-init-v8-global-object-prototype! ...                      */
;*---------------------------------------------------------------------*/
(define (nodejs-init-v8-global-object-prototype! %this::JsGlobalObject proto)
   ;; Dtrace profiling
   (js-bind! %this proto (& "DTRACE_HTTP_SERVER_REQUEST")
      :value (js-make-function %this
		(lambda (this req socket)
		   (js-undefined))
		(js-function-arity 2 0)
		(js-function-info :name "DTRACE_HTTP_SERVER_REQUEST" :len 2))
      :enumerable #f :writable #f :configurable #f :hidden-class #f)
   (js-bind! %this proto (& "DTRACE_HTTP_SERVER_RESPONSE")
      :value (js-make-function %this
		(lambda (this req socket)
		   (js-undefined))
		(js-function-arity 2 0)
		(js-function-info :name "DTRACE_HTTP_SERVER_RESPONSE" :len 2))
      :enumerable #f :writable #f :configurable #f :hidden-class #f)
   (js-bind! %this proto (& "COUNTER_HTTP_SERVER_REQUEST")
      :value (js-make-function %this
		(lambda (this)
		   (js-undefined))
		(js-function-arity 0 0)
		(js-function-info :name "COUNTER_HTTP_SERVER_REQUEST" :len 0))
      :enumerable #f :writable #f :configurable #f :hidden-class #f)
   (js-bind! %this proto (& "COUNTER_HTTP_SERVER_RESPONSE")
      :value (js-make-function %this
		(lambda (this)
		   (js-undefined))
		(js-function-arity 0 0)
		(js-function-info :name "COUNTER_HTTP_SERVER_RESPONSE" :len 0))
      :enumerable #f :writable #f :configurable #f :hidden-class #f)
   (js-bind! %this proto (& "DTRACE_HTTP_CLIENT_RESPONSE")
      :value (js-make-function %this
		(lambda (this req socket)
		   (js-undefined))
		(js-function-arity 2 0)
		(js-function-info :name "DTRACE_HTTP_CLIENT_RESPONSE" :len 2))
      :enumerable #f :writable #f :configurable #f :hidden-class #f)
   (js-bind! %this proto (& "COUNTER_HTTP_CLIENT_REQUEST")
      :value (js-make-function %this
		(lambda (this)
		   (js-undefined))
		(js-function-arity 0 0)
		(js-function-info :name "COUNTER_HTTP_CLIENT_REQUEST" :len 0))
      :enumerable #f :writable #f :configurable #f :hidden-class #f)
   (js-bind! %this proto (& "DTRACE_HTTP_CLIENT_REQUEST")
      :value (js-make-function %this
		(lambda (this req socket)
		   (js-undefined))
		(js-function-arity 2 0)
		(js-function-info :name "DTRACE_HTTP_CLIENT_REQUEST" :len 2))
      :enumerable #f :writable #f :configurable #f :hidden-class #f)
   (js-bind! %this proto (& "COUNTER_HTTP_CLIENT_RESPONSE")
      :value (js-make-function %this
		(lambda (this)
		   (js-undefined))
		(js-function-arity 0 0)
		(js-function-info :name "COUNTER_HTTP_CLIENT_RESPONSE" :len 0))
      :enumerable #f :writable #f :configurable #f :hidden-class #f)
   (js-bind! %this proto (& "DTRACE_NET_STREAM_END")
      :value (js-make-function %this
		(lambda (this)
		   (js-undefined))
		(js-function-arity 0 0)
		(js-function-info :name "DTRACE_NET_STREAM_END" :len 0))
      :enumerable #f :writable #f :configurable #f :hidden-class #f)
   (js-bind! %this proto (& "DTRACE_NET_SOCKET_READ")
      :value (js-make-function %this
		(lambda (this)
		   (js-undefined))
		(js-function-arity 0 0)
		(js-function-info :name "DTRACE_NET_SOCKET_READ" :len 0))
      :enumerable #f :writable #f :configurable #f :hidden-class #f)
   (js-bind! %this proto (& "DTRACE_NET_SOCKET_WRITE")
      :value (js-make-function %this
		(lambda (this)
		   (js-undefined))
		(js-function-arity 0 0)
		(js-function-info :name "DTRACE_NET_SOCKET_WRITE" :len 0))
      :enumerable #f :writable #f :configurable #f :hidden-class #f)
   (js-bind! %this proto (& "DTRACE_NET_SERVER_CONNECTION")
      :value (js-make-function %this
		(lambda (this)
		   (js-undefined))
		(js-function-arity 0 0)
		(js-function-info :name "DTRACE_NET_SERVER_CONNECTION" :len 0))
      :enumerable #f :writable #f :configurable #f :hidden-class #f)
   (js-bind! %this proto (& "COUNTER_NET_SERVER_CONNECTION")
      :value (js-make-function %this
		(lambda (this)
		   (js-undefined))
		(js-function-arity 0 0)
		(js-function-info :name "COUNTER_NET_SERVER_CONNECTION" :len 0))
      :enumerable #f :writable #f :configurable #f :hidden-class #f)
   (js-bind! %this proto (& "COUNTER_NET_SERVER_CONNECTION_CLOSE")
      :value (js-make-function %this
		(lambda (this)
		   (js-undefined))
		(js-function-arity 0 0)
		(js-function-info :name "COUNTER_NET_SERVER_CONNECTION_CLOSE" :len 0))
      :enumerable #f :writable #f :configurable #f :hidden-class #f)
   %this)

;*---------------------------------------------------------------------*/
;*    nodejs-new-scope-object ...                                      */
;*---------------------------------------------------------------------*/
(define (nodejs-new-scope-object global::JsGlobalObject)
   (with-access::JsGlobalObject global (elements js-scope-cmap)
      (when (eq? js-scope-cmap (class-nil JsConstructMap))
	 (set! js-scope-cmap (js-make-jsconstructmap)))
      (let ((scope (duplicate::JsGlobalObject global
		      (cmap js-scope-cmap)
		      (elements ($create-vector (SCOPE-ELEMENTS-LENGTH))))))
	 (js-object-proto-set! scope global)
	 (js-object-mode-set! scope
	    (bit-andu32 (js-object-default-mode)
	       (bit-notu32 (JS-OBJECT-MODE-INLINE))))
	 (nodejs-scope-init! scope)
	 scope)))

;*---------------------------------------------------------------------*/
;*    nodejs-scope-init! ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-scope-init! scope)
   (js-bind! scope scope (& "head")
      :value (js-html-head scope)
      :enumerable #f :writable #f :configurable #f :hidden-class #f))

;*---------------------------------------------------------------------*/
;*    debug-compile-trace ...                                          */
;*---------------------------------------------------------------------*/
(define (debug-compile-trace tag filename #!optional target)
   (when (or (>=fx (nodejs-hop-debug) 2)
	     (string-contains (or (getenv "HOPTRACE") "") "nodejs:compile"))
      (display "compiling (" (current-error-port))
      (display tag (current-error-port))
      (display "): " (current-error-port))
      (display filename (current-error-port))
      (when target
	 (display " -> " (current-error-port))
	 (display target (current-error-port)))
      (newline (current-error-port))))

;*---------------------------------------------------------------------*/
;*    nodejs-compile ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-compile src filename::bstring
	   %ctxthis %ctxmodule
	   #!key lang worker-slave (commonjs-export #t) warning-global)
   
   (define (cache-path filename)
      (make-cache-name 
	 (string-append (string-replace (prefix filename) #\/ #\_)
	    (if (>fx (nodejs-hop-debug) 0) "-g.scm" ".scm"))))
   
   (define (load-cache filename)
      (when (hop-cache-enable)
	 (let ((path (cache-path filename)))
	    (when (and (file-exists? path)
		       (<elong (file-modification-time filename)
			  (file-modification-time path)))
	       (hop-verb 3 "using cache \"" (hop-color 7 "" path) "\"\n")
	       (call-with-input-file path
		  port->sexp-list)))))
   
   (define (store-cache filename expr)
      (when (hop-cache-enable)
	 (let ((path (cache-path filename)))
	    (with-handler
	       (lambda (e)
		  (when (>=fx (nodejs-hop-debug) 1)
		     (exception-notify e)
		     (with-output-to-port (current-error-port)
			(lambda ()
			   (print "\nThis error was raised because the JS compiler\n"
			      "is enable to store the generated file into its cache.\n"
			      "This error can be safely ignored but the file will have\n"
			      "to be recompiled at each run.\n")))))
	       (call-with-output-file path
		  (lambda (op)
		     (for-each (lambda (e) (pp e op)) expr))))))
      expr)
   
   (define (compile-file filename::bstring mod)
      (with-trace 'require "compile-file"
	 (trace-item "filename=" filename)
	 (or (load-cache filename)
	     (with-handler
		(lambda (e)
		   (when (>=fx (nodejs-hop-debug) 1)
		      (exception-notify e))
		   (raise e))
		(store-cache filename
		   (call-with-input-file filename
		      (lambda (in)
			 (debug-compile-trace "compile-file" filename)
			 (let ((m (open-mmap filename read: #t :write #f)))
			    (unwind-protect
			       (j2s-compile in
				  :driver (nodejs-driver)
				  :driver-name "nodejs-driver"
				  :filename filename
				  :source src
				  :language (or lang "hopscript")
				  :node-modules-directory (nodejs-node-modules-directory)
				  :mmap-src m
				  :module-main #f
				  :module-name (symbol->string mod)
				  :worker-slave worker-slave
				  :verbose (if (>=fx (hop-verbose) 10) (-fx (hop-verbose) 10) 0)
				  :plugins-loader (make-plugins-loader %ctxthis %ctxmodule (js-current-worker))
				  :commonjs-export commonjs-export
				  :es6-module-client #t
				  :warning-global warning-global
				  :debug (nodejs-hop-debug))
			       (close-mmap m))))))))))
   
   (define (compile-url url::bstring mod)
      (with-trace 'require "compile-url"
	 (trace-item "url=" url)
	 (trace-item "filename=" filename)
	 (call-with-input-file url
	    (lambda (in)
	       (debug-compile-trace "compile-url" filename)
	       (input-port-name-set! in url)
	       (j2s-compile in
		  :driver (nodejs-driver)
		  :driver-name "nodejs-driver"
		  :language (or lang "hopscript")
		  :filename filename
		  :source src
		  :node-modules-directory (nodejs-node-modules-directory)
		  :module-main #f
		  :module-name (symbol->string mod)
		  :worker-slave worker-slave
		  :verbose (if (>=fx (hop-verbose) 10) (-fx (hop-verbose) 10) 0)
		  :plugins-loader (make-plugins-loader %ctxthis %ctxmodule (js-current-worker))
		  :commonjs-export commonjs-export
		  :es6-module-client #t
		  :debug (nodejs-hop-debug))))))
   
   (define (compile-ast ast::J2SProgram mod)
      (with-trace 'require "compile-ast"
	 (with-access::J2SProgram ast (path)
	    (trace-item "ast=" path)
	    (debug-compile-trace "compile-ast" path)
	    (let ((m (when (file-exists? path)
			(open-mmap filename read: #t :write #f))))
	       (unwind-protect
		  (j2s-compile ast
		     :driver (nodejs-driver)
		     :driver-name "nodejs-driver"
		     :filename filename
		     :source src
		     :language (or lang "hopscript")
		     :node-modules-directory (nodejs-node-modules-directory)
		     :module-main #f
		     :mmap-src m
		     :module-name (symbol->string mod)
		     :worker-slave worker-slave
		     :verbose (if (>=fx (hop-verbose) 10) (-fx (hop-verbose) 10) 0)
		     :plugins-loader (make-plugins-loader %ctxthis %ctxmodule (js-current-worker))
		     :commonjs-export commonjs-export
		     :es6-module-client #t
		     :debug (nodejs-hop-debug))
		  (when (mmap? m)
		     (close-mmap m)))))))

   (define (compile-lang src::bstring mod lang)
      (let* ((comp (nodejs-language-toplevel-loader))
	     (obj (comp lang src '((module-main . #f))))
	     (ty (assq :type obj))
	     (val (assq :value obj)))
	 (cond
	    ((or (not (pair? ty)) (not (pair? val)))
	     (error lang "wrong status" obj))
	    ((equal? (cdr ty) "error")
	     (error lang "error" (cdr val)))
	    ((equal? (cdr ty) "json-error")
	     (nodejs-language-notify-error (cdr val) lang)
	     (error lang "aborting" src))
	    ((equal? (cdr ty) "filename")
	     (compile-file (cdr val) mod))
	    (else
	     (error lang "don't know what to do with" obj)))))
   
   (define (compile src mod lang)
      (with-trace 'require "compile"
	 (trace-item "src=" src)
	 (trace-item "lang=" lang)
	 (cond
	    ((isa? src J2SProgram)
	     (compile-ast src mod))
	    ((not (string? src))
	     (bigloo-type-error "nodejs-compile" "string or J2SProgram" src))
	    ((and (string? lang) (not (builtin-language? lang)))
	     (compile-lang src mod lang))
	    ((file-exists? filename)
	     (compile-file filename mod))
	    (else
	     (compile-url filename mod)))))
   
   (unless nodejs-debug-compile
      (set! nodejs-debug-compile
	 (if (string-contains (or (getenv "HOPTRACE") "") "nodejs:compile")
	     'yes
	     'no)))

   (synchronize compile-mutex
      (with-trace 'require "nodejs-compile"
	 (trace-item "filename=" filename)
	 (trace-item "lang=" lang)
	 (let ((key (if worker-slave
			(string-append filename "_w")
			filename))
	       (tgt #f))
	    (or (let* ((mod (gensym (string->symbol (basename filename))))
		       (expr (compile src mod lang))
		       (evmod (eval-module)))
		   (when (eq? nodejs-debug-compile 'yes)
		      (unless (directory? "/tmp/HOP")
			 (make-directory "/tmp/HOP"))
		      (set! tgt (make-file-name "/tmp/HOP"
				   (string-append
				      (string-replace (prefix filename) #\/ #\_)
				      ".scm")))
			 (call-with-output-file tgt
			    (lambda (op)
			       (for-each (lambda (e) (pp e op)) expr))))
		   (trace-item "thread=" (current-thread))
		   (trace-item "expr=" (format "~s" expr))
		   (unwind-protect
		      (begin
			 (hop-verb 2 "loading \"" (hop-color 7 "" filename) "\""
			    (if tgt (format " (~s)\n" tgt) "\n"))
			 ;; evaluate the module clause first
			 (eval! (car expr))
			 (let ((nexpr (map (lambda (x)
					      (eval `(expand ',x)))
					 (cdr expr))))
			    ;; the forms to be evaluated have to be expanded
			    ;; first in order to resolve the &begin! ... &end!
			    ;; construct
			    (for-each eval nexpr)
			    (eval! 'hopscript)))
		      (eval-module-set! evmod))))))))

;*---------------------------------------------------------------------*/
;*    nodejs-debug-compile ...                                         */
;*---------------------------------------------------------------------*/
(define nodejs-debug-compile #f)

;*---------------------------------------------------------------------*/
;*    hop-load-cache ...                                               */
;*---------------------------------------------------------------------*/
(define hop-load-cache (make-hashtable))
(define sofile-cache (make-hashtable))
(define soload-mutex (make-mutex))

(define socompile-mutex-debug #f)
(define socompile-mutex (make-mutex))
(define socompile-condv (make-condition-variable))
(define socompile-files '())
(define socompile-processes '())
(define socompile-ended #f)

(define socompile-worker-count 0)
(define socompile-queue '())
(define socompile-compiled '())
(define socompile-incompile '())

;*---------------------------------------------------------------------*/
;*    synchronize/debug ...                                            */
;*---------------------------------------------------------------------*/
(define-expander synchronize/debug
   (lambda (x e)
      (match-case x
	 ((synchronize/debug socompile-mutex . ?body)
	  (let ((nx `(synchronize ,(cadr x)
			(set! socompile-mutex-debug
			   (cons ',(cer x) (current-thread)))
			,@(cddr x))))
	     (e nx e)))
	 (else
	  (error "synchronize/debug" "bad form" x)))))

(define-expander condition-variable-wait!/debug
   (lambda (x e)
      (match-case x
	 ((condition-variable-wait!/debug ?condv socompile-mutex)
	  (let ((nx `(begin
			(condition-variable-wait! ,condv socompile-mutex)
			(set! socompile-mutex-debug
			   (cons ',(cer x) (current-thread))))))
	     (e nx e)))
	 (else
	  (error "condition-variable-wait!/debug" "bad form" x)))))

;*---------------------------------------------------------------------*/
;*    hop-dynamic-load ...                                             */
;*---------------------------------------------------------------------*/
(define (hop-dynamic-load sopath)
   (synchronize soload-mutex
      (let ((old (hashtable-get sofile-cache sopath)))
	 (if old
	     (values (car old) (cdr old))
	     (multiple-value-bind (proc mod)
		(dynamic-load sopath)
		(hop-verb 2 "loading \"" (hop-color 5 "" sopath) "\"\n")
		(let ((v (cond
			    ((procedure? proc)
			     proc)
			    ((dynamic-load-symbol sopath "hopscript-env" mod)
			     =>
			     (lambda (sym)
				(dynamic-load-symbol-get sym))))))
		   (hashtable-put! sofile-cache sopath (cons v mod))
		   (values v mod)))))))

;*---------------------------------------------------------------------*/
;*    nodejs-process-wait ...                                          */
;*---------------------------------------------------------------------*/
(define (nodejs-process-wait proc filename)
   (process-wait proc)
   (synchronize compile-mutex
      (when (pair? compile-listeners-end)
	 (let ((evt (instantiate::event
		       (name "end")
		       (target  filename)
		       (value (process-exit-status proc)))))
		       (apply-listeners compile-listeners-end evt)))
      (set! compile-pending (-fx compile-pending 1))
      (when (=fx compile-pending 0)
	 (when (pair? compile-listeners-all)
	    (let ((evt (instantiate::event
			  (name "all")
			  (target (js-undefined))
			  (value 0))))
	       (apply-listeners compile-listeners-all evt))))))

;*---------------------------------------------------------------------*/
;*    nodejs-compile-abort-all! ...                                    */
;*---------------------------------------------------------------------*/
(define (nodejs-compile-abort-all!)
   
   (define (socompile-wait p::struct status::bool)
      (let ((proc (socompile-proc p)))
	 (nodejs-process-wait proc (socompile-src p))
	 (if (and status (=fx (process-exit-status proc) 0))
	     ((socompile-ksucc p))
	     ((socompile-kfail p)))))

   (synchronize/debug socompile-mutex
      (for-each (lambda (p)
		   (define debug-abort #f)
		   (case (hop-sofile-compile-policy)
		      ((nte1)
		       (socompile-wait p #t)
		       (hop-sofile-compile-policy-set! 'abort))
		      ((nte+)
		       (socompile-wait p #t))
		      (else
		       (when debug-abort
			  (tprint "aborting " (socompile-proc p)
			     " " (socompile-cmd p)))
		       (hop-verb 3 (hop-color -1 -1 " ABORTING ") " "
			  (socompile-cmd p) "\n")
		       (socompile-abortp-set! p #t)
		       (process-kill (socompile-proc p))
		       ((socompile-kfail p))
		       (when debug-abort
			  (tprint "waiting/abort " (socompile-proc p)))
		       (when debug-abort
			  (tprint "killed " (socompile-proc p))))))
	 socompile-processes)
      (set! socompile-processes '())
      (set! socompile-files '())
      (set! socompile-ended #t)))

;*---------------------------------------------------------------------*/
;*    nodejs-compile-workers-inits! ...                                */
;*---------------------------------------------------------------------*/
(define (nodejs-compile-workers-inits!)
   
   (define (compile-worker e::struct)
      (let loop ()
	 (if (<fx socompile-worker-count (hop-sofile-max-workers))
	     (begin
		(set! socompile-worker-count
		   (+fx 1 socompile-worker-count))
		(set! socompile-queue
		   (remq! e socompile-queue))
		(set! socompile-incompile
		   (cons (soentry-key e) socompile-incompile))
		(thread-start! (make-compile-worker e)))
	     (begin
		(condition-variable-wait!/debug socompile-condv socompile-mutex)
		(loop)))))

   (register-exit-function!
      (lambda (status)
	 (nodejs-compile-abort-all!)
	 status))

   (thread-start!
      (instantiate::hopthread
	 (name "socompile-orchestrator")
	 (body (lambda ()
		  (thread-name-set! (current-thread) "socompile")
		  (with-handler
		     (lambda (e)
			(exception-notify e))
		     (synchronize/debug socompile-mutex
			(let loop ()
			   (let liip ()
			      (when (pair? socompile-queue)
				 (let ((e (nodejs-select-socompile
					     socompile-queue)))
				    (or (string? (find-new-sofile
						    (soentry-filename e)
						    (soentry-wslave e)))
					(compile-worker e))
				    (liip))))
			   (condition-variable-wait!/debug
			      socompile-condv socompile-mutex)
			   (loop)))))))))

;*---------------------------------------------------------------------*/
;*    register-socompile-process! ...                                  */
;*---------------------------------------------------------------------*/
(define (register-socompile-process! proc::process cmd::bstring src ksucc kfail)
   ;; socompile-mutex already locked
   (set! socompile-processes
      (cons (socompile proc cmd src ksucc kfail #f) socompile-processes))
   proc)

;*---------------------------------------------------------------------*/
;*    unregister-socomopile-process! ...                               */
;*---------------------------------------------------------------------*/
(define (unregister-socompile-process! proc)
   ;; socompile-mutex already locked
   (let ((el (find (lambda (s) (eq? (socompile-proc s) proc))
		socompile-processes)))
      (set! socompile-processes (delete! el socompile-processes))
      el))

;*---------------------------------------------------------------------*/
;*    soworker-name ...                                                */
;*---------------------------------------------------------------------*/
(define soworker-name
   (let ((count 0))
      (lambda ()
	 (set! count (+fx 1 count))
	 (string-append "socompile-worker-" (integer->string count)))))

;*---------------------------------------------------------------------*/
;*    make-compile-worker ...                                          */
;*---------------------------------------------------------------------*/
(define (make-compile-worker en::struct)
   (instantiate::hopthread
      (name (soworker-name))
      (body (lambda ()
	       (with-handler
		  (lambda (e)
		     (exception-notify e))
		  (with-trace 'sorequire "make-compile-worker"
		     (trace-item "thread=" (current-thread))
		     (trace-item "en=" en)
		     (nodejs-socompile (soentry-filename en)
			(soentry-filename en)
			(soentry-lang en)
			(soentry-wslave en))
		     (synchronize/debug socompile-mutex
			(set! socompile-worker-count
			   (-fx socompile-worker-count 1))
			(set! socompile-compiled
			   (cons (soentry-key en) socompile-compiled))
			(set! socompile-incompile
			   (remq! (soentry-key en) socompile-incompile))
			(condition-variable-broadcast! socompile-condv))))))))
   
;*---------------------------------------------------------------------*/
;*    nodejs-select-socompile ...                                      */
;*    -------------------------------------------------------------    */
;*    Select one entry amongst the compile queue. The current          */
;*    strategy is to select the last modified file.                    */
;*---------------------------------------------------------------------*/
(define (nodejs-select-socompile::struct queue::pair)
   ;; socompile-mutex is already acquired
   (let loop ((entries (cdr queue))
	      (entry (car queue)))
      (cond
	 ((null? entries)
	  entry)
	 ((<elong (soentry-mtime (car entries)) (soentry-mtime entry))
	  ;; select the oldest entry
	  (loop (cdr entries) (car entries)))
	 (else
	  (loop (cdr entries) entry)))))

;*---------------------------------------------------------------------*/
;*    nodejs-socompile-queue-push ...                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-socompile-queue-push filename lang worker-slave)
   (with-trace 'sorequire "nodejs-socompile-queue-push"
      (synchronize/debug socompile-mutex
	 (trace-item "filename=" filename)
	 (trace-item "workfer-slave=" worker-slave)
	 (trace-item "socompile-queue=" (map soentry-filename socompile-queue))
	 (trace-item "socompile-compiled=" socompile-compiled)
	 (let ((key (if worker-slave (string-append filename "_w") filename)))
	    (when (and (file-exists? filename)
		       (not (member key socompile-compiled))
		       (not (member key socompile-incompile))
		       (not (find (lambda (e)
				     (eq? (soentry-key e) key))
			       socompile-queue)))
	       (let ((en (soentry filename lang
			    (file-modification-time filename)
			    (if worker-slave #t #f) key)))
		  (set! socompile-queue (cons en socompile-queue)))))
	 (condition-variable-broadcast! socompile-condv))))

;*---------------------------------------------------------------------*/
;*    nodejs-socompile ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-socompile src::obj filename::bstring lang worker-slave::bool)
   
   (define (exec line::pair ksucc::procedure kfail::procedure)
      
      (let ((cmd (format "~( )" line))
	    (mutex (make-mutex "compilation"))
	    (estr #f)
	    (proc #f))
	 
	 (synchronize/debug socompile-mutex
	    (set! proc (unless socompile-ended
			  (register-socompile-process!
			     (apply run-process (car line)
				:wait #f
				error: pipe:
				output: "/dev/null"
				(cdr line))
			     cmd filename ksucc kfail)))
	    ;; error logger (mandatory to flush child stderr)
	    (when (process? proc)
	       (thread-start!
		  (instantiate::hopthread
		     (name "errlogger")
		     (body (lambda ()
			      (let ((err (process-error-port proc)))
				 (with-handler
				    (lambda (e) e)
				    (unwind-protect
				       (synchronize mutex
					  (set! estr (read-string err)))
				       (close-input-port err)))))))))
	    (with-handler
	       (lambda (e)
		  (fprint (current-error-port) "**** ERROR: compilation failed " filename)
		  (exception-notify e))
	       (when (process? proc)
		  (set! compile-pending (+fx compile-pending 1))
		  (when (pair? compile-listeners-start)
		     (let ((evt (instantiate::event
				   (name "start")
				   (target filename)
				   (value `(pending: ,compile-pending
					      command: ,cmd)))))
			(apply-listeners compile-listeners-start evt))))))
	 (nodejs-process-wait proc filename)
	 (synchronize mutex
	    (if (and socompile-ended (=fx (process-exit-status proc) 0))
		'ended
		(let ((soc (unregister-socompile-process! proc)))
		   (unless (=fx (process-exit-status proc) 0)
		      (if soc
			  (unless (socompile-abortp soc)
			     estr))))))))
   
   (define (dump-error cmd sopath msg)
      (fprint (current-error-port) msg "\n")
      (call-with-output-file (string-append sopath ".err")
	 (lambda (op)
	    (display cmd op)
	    (newline op)
	    (display msg op))))
   
   (define (make-ksucc sopath sopathtmp misctmp)
      (lambda ()
	 (let ((o (string-append (prefix sopathtmp) ".o")))
	    (when (file-exists? o) (delete-file o))
	    (cond
	       ((file-exists? sopathtmp)
		(rename-file sopathtmp sopath)
		(when (and misctmp (file-exists? misctmp)) (delete-file misctmp))
		sopath)
	       ((file-exists? sopath)
		sopath)))))
   
   (define (make-kfail sopath sopathtmp misctmp)
      (lambda ()
	 (let ((o (string-append (prefix sopathtmp) ".o"))
	       (c (string-append (prefix sopathtmp) ".c")))
	    (when (file-exists? o) (delete-file o))
	    (when (file-exists? c) (delete-file c)))
	 (when (file-exists? sopath) (delete-file sopath))
	 (when (file-exists? sopathtmp) (delete-file sopathtmp))
	 (when (and misctmp (file-exists? misctmp)) (delete-file misctmp))))

   (with-trace 'sorequire (format "nodejs-socompile ~a" filename)
      (let loop ()
	 (let ((tmp (synchronize/debug socompile-mutex
		       (cond
			  ((find-new-sofile filename worker-slave)
			   =>
			   (lambda (x) x))
			  ((member filename socompile-files)
			   (condition-variable-wait!/debug
			      socompile-condv socompile-mutex)
			   'loop)
			  (else
			   (set! socompile-files
			      (cons filename socompile-files))
			   'compile)))))
	    (trace-item "tmp=" tmp)
	    (cond
	       (socompile-ended #f)
	       ((string? tmp) tmp)
	       ((eq? tmp 'loop) (loop))
	       (else
		(with-handler
		   (lambda (e)
		      (exception-notify e)
		      (fprintf (current-error-port) "sofile ~s (~s) not generated."
			 (hop-sofile-path filename
			    :suffix (if worker-slave "_w" ""))
			 filename))
		   (let* ((sopath (hop-sofile-path filename
				     :suffix (if worker-slave "_w" "")))
			  (sopathtmp (make-file-name
					(dirname sopath)
					(string-append "#" (basename sopath))))
			  (astfile (when (isa? src J2SProgram)
				      (make-file-name (dirname sopath)
					 (string-append (basename filename) ".ast"))))
			  (cmd `(,(hop-hopc)
				 ;; bigloo
				 ,(format "--bigloo=~a" (hop-bigloo))
				 ;; verbosity
				 ,@(if (eq? nodejs-debug-compile 'yes)
				       (list (format "-v~a" (hop-verbose)))
				       '())
				 ;; source
				 ,@(cond
				      ((string? src)
				       (list src))
				      ((isa? src J2SProgram)
				       `(,filename "--ast-file" ,astfile))
				      (else
				       (error "nodejs-socompile"
					  (format "bad source format `~a'" (typeof src)) filename)))
				 ;; target
				 "-y" "--js-no-module-main" "-o" ,sopathtmp
				 ;; js plugins
				 "--js-plugins"
				 ;; profiling
				 ,@(if (hop-profile) '("--profile") '())
				 ;; worker
				 ,@(if worker-slave '("--js-worker-slave") '())
				 ;; debug
				 ,@(if (eq? nodejs-debug-compile 'yes)
				       `("-t" ,(make-file-name "/tmp/HOP"
						  (string-append
						     (prefix (basename filename))
						     ".scm")))
				       '())
				 ;; config
				 ,@(if (pair? (j2s-compile-options))
				       `("--js-config"
					   ,(string-for-read
					       (format "~s"
						  (j2s-compile-options))))
				       '())
				 ;; other options
				 ,@(call-with-input-string (hop-hopc-flags)
				      port->string-list)))
			  (ksucc (make-ksucc sopath sopathtmp astfile))
			  (kfail (make-kfail sopath sopathtmp astfile)))
		      (make-directories (dirname sopath))
		      (when astfile
			 (call-with-output-file astfile
			    (lambda (out)
			       (display (obj->string src) out))))
		      (trace-item "sopath=" sopath)
		      (trace-item "sopathtmp=" sopathtmp)
		      (trace-item "cmd=" cmd)
		      (synchronize-global
			 (make-file-name
			    (dirname (hop-sofile-path "hop.lock"))
			    "hop.lock")
			 (lambda ()
			    ;; check if the file has already been compiled while
			    ;; we were waiting for the lock
			    (let ((msg (unless (find-new-sofile filename worker-slave worker-slave)
					  (debug-compile-trace "nodejs-socompile" filename sopath)
					  (hop-verb 2 (hop-color -2 -2 " COMPILE") " "
					     (format "~( )\n"
						(map (lambda (s)
							(if (string-index s #\space)
							    (string-append "\"" s "\"")
							    s))
						   cmd)))
					  (exec cmd ksucc kfail))))
			       (trace-item "msg=" msg)
			       (unwind-protect
				  (cond
				     ((not msg)
				      ;; compilation succeeded
				      (ksucc))
				     ((string? msg)
				      ;; compilation failed
				      (kfail)
				      (hop-verb 1
					 (hop-color -1 -1 " COMPILE-ERROR")
					 " " cmd "\n" msg)
				      (dump-error cmd sopath msg)
				      'error))
				  (synchronize/debug socompile-mutex
				     (unless socompile-ended
					(set! socompile-files
					   (delete! filename socompile-files))
					(condition-variable-broadcast!
					   socompile-condv)))))))))))))))

;*---------------------------------------------------------------------*/
;*    find-new-sofile ...                                              */
;*---------------------------------------------------------------------*/
(define (find-new-sofile filename::bstring worker-slave #!optional enable-cache)
   (with-trace 'require "find-new-sofile"
      (trace-item "filename=" filename)
      (when (or (hop-cache-enable) enable-cache)
	 (let ((sopath (hop-find-sofile filename
			  :suffix (if worker-slave "_w" ""))))
	    (trace-item "sopath=" sopath)
	    (if (string? sopath)
		(begin
		   (trace-item "file.mtime=" (file-modification-time sopath))
		   (trace-item "sofi.mtime=" (file-modification-time filename))
		   (when (>= (file-modification-time sopath)
			    (file-modification-time filename))
		      sopath))
		sopath)))))

;*---------------------------------------------------------------------*/
;*    nodejs-load ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-load src filename worker::WorkerHopThread %ctxthis %ctxmodule
	   #!key lang srcalias (commonjs-export #unspecified))
   
   (define (loadso-or-compile filename lang worker-slave)
      (with-trace 'require "loadso-or-compile"
	 (trace-item "filename=" filename " lang=" lang
	    " slave=" (if worker-slave #t #f))
	 (let loop ((sopath (find-new-sofile filename worker-slave)))
	    (trace-item "sopath=" sopath)
	    (cond
	       ((and (string? sopath) (hop-sofile-enable))
		(multiple-value-bind (p _)
		   (hop-dynamic-load sopath)
		   (if (and (procedure? p) (=fx (procedure-arity p) 4))
		       p
		       (js-raise-error %ctxthis
			  (format "Wrong compiled file format ~s" sopath)
			  sopath))))
	       ((or (not (symbol? sopath)) (not (eq? sopath 'error)))
		(case (hop-sofile-compile-policy)
		   ((aot)
		    (if (hop-sofile-enable)
			(loop (nodejs-socompile src filename lang worker-slave))
			(nodejs-compile src filename %ctxthis %ctxmodule
			   :lang lang :commonjs-export commonjs-export
			   :worker-slave worker-slave)))
		   ((nte nte1 nte+)
		    (when (hop-sofile-enable)
		       (nodejs-socompile-queue-push filename lang worker-slave))
		    (nodejs-compile src filename %ctxthis %ctxmodule
		       :lang lang :commonjs-export commonjs-export
		       :worker-slave worker-slave))
		   (else
		    (nodejs-compile src filename %ctxthis %ctxmodule
		       :lang lang :commonjs-export commonjs-export
		       :worker-slave worker-slave))))
	       (else
		(nodejs-compile src filename %ctxthis %ctxmodule
		   :lang lang :commonjs-export commonjs-export
		   :worker-slave worker-slave))))))
   
   (define (load-module-js filename lang)
      (with-trace 'require "require@load-module-js"
	 (with-access::WorkerHopThread worker (%this prehook parent)
	    (with-access::JsGlobalObject %this (js-object js-main)
	       (let ((hopscript (loadso-or-compile filename lang parent))
		     (this (js-new0 %this js-object))
		     (scope (nodejs-new-scope-object %this))
		     (mod (nodejs-new-module (if js-main filename ".")
			     (or srcalias filename) worker %this)))
		  (js-put! scope (& "module") mod #f %this)
		  ;; prehooking
		  (when (procedure? prehook)
		     (prehook %this this scope mod))
		  ;; main module
		  (when (eq? js-main (js-null)) (set! js-main mod))
		  ;; create the module
		  (with-handler
		     (lambda (e)
			(with-access::WorkerHopThread worker (module-cache %this)
			   (js-delete! module-cache
			      (js-string->jsstring filename) #f %this))
			(raise e))
		     (hopscript %this this scope mod))
		  ;; set the loaded property
		  (js-put! mod (& "loaded") #t #f %this)
		  ;; return the newly created module
		  (trace-item "mod=" (typeof mod) " filename=" filename)
		  mod)))))
   
   (define (load-module-html)
      (with-trace 'require "require@load-module-html"
	 (with-access::WorkerHopThread worker (%this prehook)
	    (with-access::JsGlobalObject %this (js-object js-main)
	       (let ((hopscript (nodejs-compile filename filename
				   %ctxthis %ctxmodule
				   :lang lang
				   :commonjs-export #f
				   :warning-global #f))
		     (this (js-new0 %this js-object))
		     (scope (nodejs-new-scope-object %this))
		     (mod (nodejs-new-module (if js-main filename ".")
			     (or srcalias filename) worker %this)))
		  ;; prehooking
		  (when (procedure? prehook)
		     (prehook %this this scope mod))
		  ;; main module
		  (when (eq? js-main (js-null)) (set! js-main mod))
		  ;; create the module
		  (with-handler
		     (lambda (e)
			(with-access::WorkerHopThread worker (module-cache %this)
			   (js-delete! module-cache
			      (js-string->jsstring filename) #f %this))
			(raise e))
		     ;; exports the HTML value
		     (js-put! mod (& "exports")
			(hopscript %this this scope mod)
			#f %this))
		  ;; return the newly created module
		  (trace-item "mod=" (typeof mod))
		  mod)))))
   
   (define (hop-eval filename ws)
      (let ((old (hashtable-get hop-load-cache filename)))
	 (if old
	     (values (car old) (cdr old))
	     (let ((v (hop-load filename :mode 'module :worker-slave ws)))
		(cond
		   ((procedure? v)
		    (hashtable-put! hop-load-cache filename (cons v #f))
		    (values v #f))
		   ((evmodule? v)
		    (let ((init (call-with-eval-module v
				   (lambda ()
				      (eval! 'hopscript)))))
		       (hashtable-put! hop-load-cache filename (cons init v))
		       (values init v)))
		   (else
		    (values #f #f)))))))
   
   (define (read-module-name filename)
      (call-with-input-file filename
	 (lambda (proc)
	    (match-case (read proc)
	       ((module ?mod . ?-) mod)
	       (else #f)))))
   
   (define (compiled-module filename)
      (let ((mod (read-module-name filename)))
	 (when mod
	    (eval-find-module mod))))
   
   (define (hop-load/cache filename worker-slave)
      (let loop ((sopath (find-new-sofile filename worker-slave)))
	 (cond
	    ((compiled-module filename)
	     =>
	     (lambda (mod)
		(call-with-eval-module mod
		   (lambda () (eval 'hopscript)))))
	    ((and (string? sopath) (hop-sofile-enable))
	     (hop-dynamic-load sopath))
	    ((or (not (symbol? sopath)) (not (eq? sopath 'error)))
	     (case (hop-sofile-compile-policy)
		((aot)
		 (if (hop-sofile-enable)
		     (loop (nodejs-socompile src filename lang worker-slave))
		     (hop-eval filename worker-slave)))
		((nte nte1 nte+)
		 (when (hop-sofile-enable)
		    (nodejs-socompile-queue-push filename lang worker-slave))
		 (hop-eval filename worker-slave))
		(else
		 (hop-eval filename worker-slave))))
	    (else
	     (when (and (memq (hop-sofile-compile-policy) '(nte1 nte+))
			(hop-sofile-enable))
		(nodejs-socompile-queue-push filename lang worker-slave))
	     (hop-eval filename worker-slave)))))
   
   (define (load-module-hop)
      (with-access::WorkerHopThread worker (%this parent)
	 (with-access::JsGlobalObject %this (js-object)
	    (multiple-value-bind (init module)
	       (hop-load/cache filename parent)
	       (let ((this (js-new0 %this js-object))
		     (scope (nodejs-new-scope-object %this))
		     (mod (nodejs-new-module filename
			     (or srcalias filename) worker %this)))
		  (when module
		     (with-access::JsModule mod (%module)
			(set! %module module)))
		  (when (and (procedure? init) (=fx (procedure-arity init) 4))
		     (init %this this scope mod))
		  mod)))))
   
   (define (load-module-so)
      (with-access::WorkerHopThread worker (%this)
	 (with-access::JsGlobalObject %this (js-object)
	    (multiple-value-bind (init _)
	       (hop-dynamic-load filename)
	       (let ((this (js-new0 %this js-object))
		     (scope (nodejs-new-scope-object %this))
		     (mod (nodejs-new-module filename
			     (or srcalias filename) worker %this)))
		  (when (procedure? init)
		     (init %this this scope mod))
		  mod)))))
   
   (define (not-found filename)
      (js-raise-error %ctxthis
	 (format "don't know how to load module ~s" filename)
	 filename))
   
   (define (mime-html? url)
      (multiple-value-bind (scheme userinfo host port path)
	 (url-parse url)
	 (let ((r (instantiate::http-server-request
		     (scheme (string->symbol scheme))
		     (userinfo userinfo)
		     (host (or host path))
		     (port (or port 80))
		     (connection-timeout (hop-connection-timeout))
		     (connection 'close)
		     (method 'HEAD)
		     (path (if host path "/")))))
	    (http-send-request r
	       (lambda (p status header clength tenc)
		  (let ((ct (assq :content-type header)))
		     (when (pair? ct)
			(string-prefix? "text/html"
			   (cdr ct)))))))))
   
   (define (worker-compiler-available? %this filename)
      (with-access::JsGlobalObject %this (js-object js-symbol)
	 (let* ((langmode (nodejs-require-module lang worker
			     %this %ctxmodule))
		(key (js-get js-symbol (& "compiler") %this))
		(comp (js-get langmode key %this)))
	    (isa? comp JsFunction))))
   
   (define (compiler-available? filename)
      (if (eq? worker (current-thread))
	  (with-access::JsGlobalObject %ctxthis (js-object js-symbol)
	     (worker-compiler-available? %ctxthis filename))
	  (js-worker-exec-throws worker "language-loader"
	     (lambda (%this)
		(worker-compiler-available? %ctxthis filename)))))
   
   (define (builtin-load-module filename lang)
      (cond
	 ((or (string-suffix? ".js" filename) (string-suffix? ".mjs" filename))
	  (load-module-js filename lang))
	 ((string-suffix? ".ts" filename)
	  (load-module-js filename "typescript"))
	 ((string-suffix? ".ast.json" filename)
	  (load-module-js filename lang))
	 ((string-suffix? ".html" filename)
	  (load-module-html))
	 ((string-suffix? ".hop" filename)
	  (load-module-hop))
	 ((string-suffix? ".so" filename)
	  (load-module-so))
	 ((string-suffix? ".node" filename)
	  (load-module-so))
	 ((string-suffix? ".dylib" filename)
	  (load-module-so))
	 ((or (string-prefix? "http://" filename)
	      (string-prefix? "https://" filename))
	  (cond
	     ((string=? lang "html")
	      (load-module-html))
	     ((mime-html? filename)
	      (load-module-html))
	     (else
	      (load-module-js filename lang))))
	 ((not (string-index (basename filename) #\.))
	  (load-module-js filename lang))
	 ((compiler-available? filename)
	  (load-module-js filename lang))
	 (else
	  (not-found filename))))
   
   (define (url-to-path str)
      (let ((path (if (string-prefix? "file://" str)
		      (substring str 7)
		      str)))
	 (if (file-exists? path)
	     path
	     (js-raise-uri-error %ctxthis "load error (~s)" filename))))
   
   (define (loader-load-module)
      (if (null? loader-loaders)
	  (builtin-load-module filename lang)
	  (multiple-value-bind (resolve reject)
	     (js-worker-exec-promise loader-worker "resolve"
		(lambda (this)
		   (with-access::WorkerHopThread loader-worker (%this)
		      (let ((ctx (js-alist->jsobject `((parent . ,(js-undefined))) %this)))
			 (let loop ((loaders loader-loaders)
				    (url (js-string->jsstring filename))
				    (ctx ctx))
			    (if (null? loaders)
				(js-alist->jsobject `((source . ,(js-jsstring->string url))) %this)
				(js-call3 this (car loaders) (js-undefined)
				   url ctx 
				   (js-make-function this
				      (lambda (this specifier context)
					 (loop (cdr loaders) specifier context))
				      (js-function-arity 2 0)
				      (js-function-info :name "loader" :len 2))))))))
		(lambda (res)
		   (with-access::WorkerHopThread loader-worker (%this)
		      (js-get res (& "source") %this)))
		(lambda (rej)
		   rej))
	     (if (js-jsstring? resolve)
		 (builtin-load-module
		    (url-to-path (js-jsstring->string resolve))
		    #f)
		 (js-raise-uri-error %ctxthis "loader error (~s)" filename)))))
   
   (with-trace 'require (format "nodejs-load ~a" filename)
      (trace-item "lang=" lang)
      (when (eq? commonjs-export #unspecified)
	 (let ((cj (memq :commonjs-export (j2s-compile-options))))
	    (if (and (pair? cj) (pair? (cdr cj)))
		(set! commonjs-export (cadr cj))
		(set! commonjs-export #t))))
      (with-loading-file filename
		  (if (eq? worker loader-worker)
		      (lambda () (builtin-load-module filename lang))
		      loader-load-module))))

;*---------------------------------------------------------------------*/
;*    nodejs-require-module ...                                        */
;*    -------------------------------------------------------------    */
;*    Require a nodejs module, load it if necessary or simply          */
;*    reuse the previously loaded module structure.                    */
;*---------------------------------------------------------------------*/
(define (nodejs-require-module name::bstring worker::WorkerHopThread
	   %this %module #!optional (lang "hopscript") compiler copts)
   (with-trace 'require (format "nodejs-require-module ~a" name)
      (let* ((path (nodejs-resolve name %this %module (node-module-paths %module %this) 'body))
	     (mod (nodejs-load-module path worker %this %module
		     :lang lang :compiler compiler :config copts))
	     (exports (js-get mod (& "exports") %this)))
	 (trace-item "exports=" (typeof exports))
	 exports)))

;*---------------------------------------------------------------------*/
;*    nodejs-load-module ...                                           */
;*    -------------------------------------------------------------    */
;*    Require a nodejs module, load it if necessary or simply          */
;*    reuse the previously loaded module structure.                    */
;*---------------------------------------------------------------------*/
(define (nodejs-load-module path::bstring worker::WorkerHopThread
	   %this %module
	   #!key (lang "hopscript") compiler
	   config (commonjs-export #t) loc)
   
   (define (load-json path)
      (let ((mod (nodejs-new-module path path worker %this))
	    (json (call-with-input-file path
		     (lambda (ip)
			(js-json-parser ip #f #f #f %this)))))
	 (js-put! mod (& "exports") json #f %this)
	 mod))
   
   (define (load-module src path worker %this %module lang compiler srcalias)
      (cond
	 ((core-module? path)
	  (nodejs-core-module path worker %this))
	 ((js-procedure? compiler)
	  (let ((obj (js-call2-jsprocedure %this compiler (js-undefined)
			path (if (pair? config) config '()))))
	     (when (js-object? obj)
		(let ((ty (js-tostring (js-get obj (& "type") %this) %this))
		      (val (js-get obj (& "value") %this))
		      (langc (js-get obj (& "language") %this)))
		   (cond
		      ((string=? ty "filename")
		       (load-module (js-tostring val %this)
			  (js-tostring val %this)
			  worker %this %module
			  (if (eq? langc (js-undefined))
			      lang
			      (js-language->string langc "load-module" %this))
			  #f
			  path))
		      ((string=? ty "ast")
		       (load-module val path
			  worker %this %module
			  (if (eq? langc (js-undefined))
			      lang
			      (js-language->string langc "load-module" %this))
			  #f
			  path))
		      ((string=? ty "value")
		       (let ((mod (nodejs-new-module path path worker %this)))
			  (js-put! mod (& "exports") val #f %this)
			  mod))
		      ((string=? ty "json")
		       (let ((mod (nodejs-new-module path path worker %this)))
			  (js-put! mod (& "exports") val #f %this)
			  mod))
		      (else
		       val))))))
	 ((or (eq? lang "json") (string-suffix? ".json" path))
	  (load-json path))
	 ((eq? lang 'json)
	  (error "nodejs-load-module" "internal error, lang should be a string"
	     lang))
	 (else
	  (let ((mod (nodejs-load src path worker %this %module
			:lang lang :srcalias srcalias
			:commonjs-export commonjs-export)))
	     (unless (js-get mod (& "parent") %this)
		;; parent and children
		(let* ((children (js-get %module (& "children") %this))
		       (push (js-get children (& "push") %this)))
		   (js-call1 %this push children mod)
		   (js-put! mod (& "parent") %module #f %this)))
	     mod))))

   (with-trace 'require (format "nodejs-load-module ~a" path)
      (with-access::WorkerHopThread worker (module-cache)
	 (let ((mod (js-get-property-value module-cache module-cache
		       (js-string->jsstring path) %this)))
	    (trace-item "path=" path)
	    (trace-item "mod=" (if (eq? mod (js-absent)) 'absent (typeof mod)))
	    (if (eq? mod (js-absent))
		(let ((env (current-dynamic-env)))
		   (let ()
		      (%env-push-trace env path loc)
		      (let ((v (load-module path path worker %this %module lang compiler #f)))
			 (%env-pop-trace env)
			 v)))
		mod)))))

;*---------------------------------------------------------------------*/
;*    core-module? ...                                                 */
;*---------------------------------------------------------------------*/
(define (core-module? name)
   (assoc name (core-module-table)))

;*---------------------------------------------------------------------*/
;*    nodejs-core-module ...                                           */
;*    -------------------------------------------------------------    */
;*    Get a core module object. This function is used during bootstrap */
;*    by NODEJS-PROCESS to get the console core module.                */
;*---------------------------------------------------------------------*/
(define (nodejs-core-module name::bstring worker %this)
   
   (define (nodejs-init-core name worker %this)
      (with-trace 'require (format "nodejs-init-core ~a" name)
	 (trace-item "gencmapid=" (gencmapid))
	 (with-access::JsGlobalObject %this (js-object)
	    (let* ((p (assoc name (core-module-table)))
		   (init (if (pair? p)
			     (cdr p)
			     (error "nodejs-code-module"
				"Cannot find core module"
				name)))
		   (this (js-new0 %this js-object))
		   (scope (nodejs-new-scope-object %this))
		   (mod (nodejs-new-module name name worker %this)))
	       ;; initialize the core module
	       (with-trace 'require (format "nodejs-init-core.init ~a" name)
		  (init %this this scope mod))
	       ;; return the module
	       (trace-item "mod=" (typeof mod))
	       (trace-item "gencmapid=" (gencmapid))
	       mod))))

   (with-trace 'require (format "nodejs-core-module ~a" name)
      (nodejs-init-core name worker %this)))

;*---------------------------------------------------------------------*/
;*    nodejs-require-core ...                                          */
;*    -------------------------------------------------------------    */
;*    Require a nodejs module, load it if necessary or simply          */
;*    reuse the previously loaded module structure.                    */
;*---------------------------------------------------------------------*/
(define (nodejs-require-core name::bstring worker %this)
   (with-trace 'require (format "nodejs-require-core ~a" name)
      (with-access::WorkerHopThread worker (module-cache)
	 (let ((mod (js-get-property-value module-cache module-cache
		       (js-string->jsstring name) %this)))
	    (when (eq? mod (js-absent))
	       (set! mod (nodejs-core-module name worker %this)))
	    (js-get mod (& "exports") %this)))))

;*---------------------------------------------------------------------*/
;*    nodejs-resolve ...                                               */
;*    -------------------------------------------------------------    */
;*    Resolve the path name according to the current module path.      */
;*    -------------------------------------------------------------    */
;*    http://nodejs.org/api/modules.html#modules_all_together          */
;*---------------------------------------------------------------------*/
(define (nodejs-resolve name::bstring %this::JsGlobalObject %module paths::pair-nil mode::symbol)
   (with-trace 'require "nodejs-resolve"
      (trace-item "name=" name)
      (trace-item "%module=" (typeof %module))
      (trace-item "thread=" (current-thread))
      (with-access::JsGlobalObject %this (worker)
	 (if (eq? worker loader-worker)
	     (builtin-resolve name %this %module paths mode)
	     (loader-resolve name %this %module paths mode)))))

;*---------------------------------------------------------------------*/
;*    loader-resolve ...                                               */
;*    -------------------------------------------------------------    */
;*    See nodejs-register-user-loader!                                 */
;*---------------------------------------------------------------------*/
(define (loader-resolve name::bstring %this::JsGlobalObject %module paths::pair-nil mode::symbol)
   (if (null? loader-resolvers)
       (builtin-resolve name %this %module paths mode)
       (multiple-value-bind (resolve reject)
	  (js-worker-exec-promise loader-worker "resolve"
	     (lambda (this)
		(with-access::WorkerHopThread loader-worker (%this)
		   (let ((ctx (js-alist->jsobject `((parent . ,(js-undefined))) %this)))
		      (let loop ((resolvers loader-resolvers)
				 (name (js-string->jsstring name))
				 (ctx ctx))
			 (if (null? resolvers)
			     (builtin-resolve (js-tostring name %this) %this %module paths mode)
			     (js-call3 this (car resolvers) (js-undefined)
				name ctx 
				(js-make-function this
				   (lambda (this specifier context)
				      (loop (cdr resolvers) specifier context))
				   (js-function-arity 2 0)
				   (js-function-info :name "resolver" :len 2))))))))
	     (lambda (res) res)
	     (lambda (rej) rej))
	  (if (symbol? reject)
	      resolve
	      (js-raise-uri-error %this "resolve error (~s)" name)))))

;*---------------------------------------------------------------------*/
;*    json-table ...                                                   */
;*---------------------------------------------------------------------*/
(define json-table (make-hashtable))

;*---------------------------------------------------------------------*/
;*    load-package-json ...                                            */
;*---------------------------------------------------------------------*/
(define (load-package-json file %this)
   (let ((old (hashtable-get json-table file)))
      (if old
	  old
	  (call-with-input-file file
	     (lambda (ip)
		(let ((o (json-parse ip
			    :array-alloc (lambda ()
					    (make-cell '()))
			    :array-set (lambda (a i val)
					  (cell-set! a (cons val (cell-ref a))))
			    :array-return (lambda (a i)
					     (reverse! (cell-ref a)))
			    :object-alloc (lambda () (make-cell '()))
			    :object-set (lambda (o p val)
					   (cell-set! o
					      (cons (cons p val)
						 (cell-ref o))))
			    :object-return (lambda (o) (cell-ref o))
			    :parse-error (lambda (msg obj loc)
					    (js-raise-syntax-error/loc %this
					       loc msg obj)))))
		   (hashtable-put! json-table file o)
		   o))))))

;*---------------------------------------------------------------------*/
;*    builtin-resolve ...                                              */
;*    -------------------------------------------------------------    */
;*    Resolve the path name according to the current module path.      */
;*    -------------------------------------------------------------    */
;*    http://nodejs.org/api/modules.html#modules_all_together          */
;*---------------------------------------------------------------------*/
(define (builtin-resolve name::bstring %this::JsGlobalObject %module paths::pair-nil mode::symbol)
   
   (define (resolve-file x)
      (cond
	 ((and (file-exists? x) (not (directory? x)))
	  (if (string-suffix? ".hz" x)
	      (resolve-hz x)
	      (file-name-canonicalize x)))
	 ((string-suffix? ".hz" x)
	  (resolve-autoload-hz x))
	 (else
	  (let loop ((sufs '(".js" ".mjs" ".hop" ".ts" ".so" ".json" ".node" ".hss" ".css")))
	     (when (pair? sufs)
		(let* ((suffix (car sufs))
		       (src (string-append x suffix)))
		   (if (and (file-exists? src) (not (directory? src)))
		       (file-name-canonicalize src)
		       (loop (cdr sufs)))))))))
   
   (define (resolve-autoload-hz hz)
      (with-trace 'require "nodejs-resolve.resolve-autoload-hz"
	 (trace-item "hz=" hz)
	 (cond
	    ((hz-local-weblet-path hz (get-autoload-directories))
	     => resolve-directory)
	    ((hz-cache-path hz)
	     => resolve-directory)
	    (else
	     (let ((dir (hz-download-to-cache hz (hop-hz-repositories))))
		(if (directory? dir)
		    (resolve-directory dir)
		    #f))))))
   
   (define (resolve-hz hz)
      (with-trace 'require "nodejs-resolve.resolve-hz"
	 (trace-item "hz=" hz)
	 (let ((dir (hz-download-to-cache hz (hop-hz-repositories))))
	    (trace-item "dir=" dir)
	    (if (directory? dir)
		(resolve-directory dir)
		#f))))
   
   (define (resolve-package-exports file)
      (with-trace 'require "resolve-package-exports"
	 (trace-item "file=" file)
	 (let* ((dir (dirname file))
		(json (make-file-name dir "package.json")))
	    (when (file-exists? json)
	       (let* ((o (load-package-json json %this))
		      (e (assoc "exports" o)))
		  (trace-item "m=" e)
		  (when (pair? e)
		     (let ((c (assoc (string-append "./" (basename file))
				 (cdr e))))
			(when (pair? c)
			   (make-file-path dir (cdr c))))))))))

   (define (resolve-package pkg dir)
      (with-trace 'require "resolve-package"
	 (trace-item "pkg=" pkg)
	 (trace-item "dir=" dir)
	 (let* ((o (load-package-json pkg %this))
		(m (cond
		      ((eq? mode 'head) (assoc "client" o))
		      ((assoc "server" o) => (lambda (m) m))
		      (else (assoc "main" o)))))
	    (trace-item "m=" m)
	    (if (pair? m)
		(cdr m)
		(let ((idx (make-file-name dir "index.js")))
		   (when (file-exists? idx)
		      idx))))))
   
   (define (resolve-directory x)
      (with-trace 'require "nodejs-resolve.resolve-directory"
	 (trace-item "x=" x)
	 (let ((json (make-file-name x "package.json")))
	    (or (and (file-exists? json)
		     (let* ((m (resolve-package json x)))
			(cond
			   ((pair? m)
			    (cons name
			       (map (lambda (m)
				       (resolve-file-or-directory m x))
				  m)))
			   ((string? m)
			    (resolve-file-or-directory m x))
			   (else
			    #f))))
		(let ((p (make-file-name x "index.js")))
		   (when (file-exists? p)
		      (file-name-canonicalize p)))))))
   
   (define (resolve-file-or-directory x dir)
      (let ((file (make-file-name dir x)))
	 (or (resolve-package-exports file)
	     (resolve-file file)
	     (resolve-directory file))))
   
   (define (resolve-error x dir)
      (with-access::JsGlobalObject %this (js-uri-error)
	 (let ((exn (js-new %this js-uri-error
		       (js-string->jsstring
			  (format "cannot find module ~s in ~s" name dir))
		       7)))
	    (js-put! exn (& "code") (js-string->jsstring "MODULE_NOT_FOUND")
	       #f %this)
	    (js-raise exn))))

   (define (resolve-modules mod x paths)
      (with-trace 'require "resolve-modules"
	 (trace-item "x=" x)
	 (trace-item "paths=" paths)
	 (any (lambda (dir)
		 (resolve-file-or-directory x dir))
	    paths)))
   
   (with-trace 'require "builtin-resolve"
      (trace-item "name=" name)
      (trace-item "%module=" (typeof %module))
      (trace-item "thread=" (current-thread))
      (let* ((mod %module)
	     (filename (js-jsstring->string (js-get mod (& "filename") %this)))
	     (dir (dirname filename)))
	 (trace-item "dir=" dir)
	 (trace-item "paths=" (let ((paths (js-get mod (&& "paths") %this)))
				 (if (js-array? paths)
				     (jsarray->vector paths %this)
				     paths)))
	 (let loop ((name name))
	    (cond
	       ((core-module? name)
		(if (eq? mode 'import)
		    (let ((namem (string-append name ".mod")))
		       ;; MS 21feb23, once all the nodejs core modules are
		       ;; supporting ES6 module, this test should be removed
		       (if (core-module? namem)
			   namem
			   name))
		    name))
	       ((string-prefix? "node:" name)
		(loop (substring name 5)))
	       ((or (string-prefix? "http://" name)
		    (string-prefix? "https://" name))
		name)
	       ((or (string-prefix? "http://" dir)
		    (string-prefix? "https://" dir))
		(multiple-value-bind (scheme uinfo host port path)
		   (url-parse filename)
		   (let ((abspath (hop-apply-url
				     (string-append "/hop/" *resolve-url-path*)
				     (list name path)
				     %this)))
		      (with-hop-remote abspath
			 (lambda (x)
			    (if uinfo
				(format "~a://~a:~a~@a~a" scheme host port uinfo x)
				(format "~a://~a:~a~a" scheme host port x)))
			 (lambda (x) #f)
			 :host host
			 :port port))))
;* 	       ((string-prefix? "@hop/" name)                          */
;* 		(if (string=? "@hop/hop" name)                         */
;* 		    "hop.mod"                                          */
;* 		    (or (resolve-file-or-directory                     */
;* 			   (substring name 5)                          */
;* 			   (nodejs-node-modules-directory))            */
;* 			(resolve-modules mod name)                     */
;* 			(resolve-error name dir))))                    */
	       ((string-prefix? "hop:" name)
		(if (string=? "hop:hop" name)
		    "hop.mod"
		    (or (resolve-file-or-directory
			   (substring name 4)
			   (nodejs-node-modules-directory))
			(resolve-modules mod name paths)
			(resolve-error name dir))))
	       ((string-prefix? "hop:" name)
 		(or (resolve-file-or-directory
		       (substring name 4)
		       (nodejs-node-modules-directory))
		    (resolve-error name dir)))
	       ((or (string-prefix? "./" name)
		    (string-prefix? "../" name)
		    (string=? ".." name))
		(or (resolve-file-or-directory name dir)
		    (resolve-modules mod name paths)
		    (resolve-error name dir)))
	       ((string-prefix? "/" name)
		(or (resolve-file-or-directory name "/")
		    (resolve-modules mod name paths)
		    (resolve-error name dir)))
	       ((string-suffix? ".hz" name)
		(or (resolve-hz name)
		    (resolve-modules mod name paths)
		    (resolve-error name dir)))
	       (else
		(or (resolve-modules mod name paths)
		    (resolve-error name dir))))))))

;*---------------------------------------------------------------------*/
;*    nodejs-resolve-extend-path! ...                                  */
;*    -------------------------------------------------------------    */
;*    This function is used by hop to extend the require search        */
;*    path                                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-resolve-extend-path! path)
   (set! nodejs-env-path (append nodejs-env-path path)))

;*---------------------------------------------------------------------*/
;*    nodejs-bind-export! ...                                          */
;*    -------------------------------------------------------------    */
;*    Bind the exported bindings into a global object.                 */
;*---------------------------------------------------------------------*/
(define (nodejs-bind-export! %this %scope e . bindings)
   
   (define (for-in obj::JsObject proc)
      
      (define (vfor-each proc vec)
         (let ((len (vector-length vec)))
            (let loop ((i 0))
               (when (<fx i len)
                  (proc (vector-ref-ur vec i))
                  (loop (+fx i 1))))))
      
      (define (in-mapped-property n)
	 (proc (property-name n)))
      
      (define (in-property p)
         (when (isa? p JsPropertyDescriptor)
            (with-access::JsPropertyDescriptor p (name)
               (proc name))))
      
      (with-access::JsObject obj (cmap)
         (if (js-object-mapped? obj)
             (with-access::JsConstructMap cmap (props)
                (vfor-each in-mapped-property props))
	     (begin
		(vector-for-each in-property (js-object-inline-elements obj))
		(vector-for-each in-property (js-object-noinline-elements obj))))))
   
   ;; e start being undefined during the first steps of the rts boot
   (when (js-object? e)
      ;; bind all the exported functions in the global object
      (if (null? bindings)
          (for-in e
             (lambda (k)
                (js-bind! %this %scope k
                   :value (js-get e k %this)
                   :writable #t :enumerable #f :configurable #f
                   :hidden-class #f)))
          (for-each (lambda (k)
                       (js-bind! %this %scope k
                          :get (js-make-function %this
                                  (lambda (o)
                                     (js-get e k %this))
                                  (js-function-arity 0 0)
				  (js-function-info :name "get" :len 0))
                          :hidden-class #f))
             bindings)))

   e)

;*---------------------------------------------------------------------*/
;*    nodejs-eval ...                                                  */
;*    -------------------------------------------------------------    */
;*    See js2scheme/header.scm                                         */
;*    -------------------------------------------------------------    */
;*    tests:                                                           */
;*      ch7/7.2/S7.2_A1.1_T2.js                                        */
;*      ch7/7.3/S7.3_A4_T1.js                                          */
;*      ch8/8.7/8,7.2/8.7.2-1-s.js                                     */
;*      ch10/10.4/10/4.3/10.4.3-1-19-s.js                              */
;*      ch11/11.1/11.1.5/11.1.5_6-3-1.js                               */
;*---------------------------------------------------------------------*/
(define (nodejs-eval %this scope)
   
   (define (js-eval this str)
      (if (not (js-jsstring? str))
	  str
	  (call-with-input-string (js-jsstring->string str)
	     (lambda (ip)
		(%js-eval ip 'eval %this %this scope)))))

   (js-bind! %this scope (& "eval")
      :value (js-make-function %this js-eval
		(js-function-arity 1 0)
		(js-function-info :name "eval" :len 1)
		:prototype (js-undefined))
      :configurable #f :enumerable #f))

;*---------------------------------------------------------------------*/
;*    current-loc ...                                                  */
;*---------------------------------------------------------------------*/
(define-expander current-loc
   (lambda (x e)
      (when (epair? x) `',(cer x))))
	 
;*---------------------------------------------------------------------*/
;*    nodejs-function ...                                              */
;*    -------------------------------------------------------------    */
;*    See js2scheme/header.scm                                         */
;*---------------------------------------------------------------------*/
(define (nodejs-function %this scope)

   (define (js-function-construct this . args)
      (if (null? args)
	  (js-make-function %this (lambda (this) (js-undefined))
	     (js-function-arity 0 0)
	     (js-function-info :name "" :len 0)
	     :alloc js-object-alloc)
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
	 (js-make-function %this js-function-construct
	    (js-function-arity js-function-construct)
	    (js-function-info :name "Function" :len 1)
	    :__proto__ js-function-prototype
	    :prototype js-function-prototype
	    :alloc js-no-alloc)))

   (js-bind! %this scope (& "Function")
      :value js-function
      :configurable #f :enumerable #f :hidden-class #f))

;*---------------------------------------------------------------------*/
;*    nodejs-worker ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-worker %this::JsGlobalObject scope::JsObject %module::JsObject)

   (define parentfile
      (js-tostring (js-get %module (& "filename") %this) %this))
   
   (define (loader filename worker this)
      (let ((parent (nodejs-new-module (basename filename)
		       parentfile worker this)))
	 (if (string? filename)
	     (nodejs-require-module filename worker this parent)
	     (js-raise-error %this
		(format "cannot load worker module ~a" filename)
		filename))))

   (define %js-worker
      (js-worker-construct %this loader))

   (define js-worker
      (with-access::JsGlobalObject %this (js-function-prototype
					    js-worker-prototype)
	 (js-make-function %this %js-worker
	    (js-function-arity %js-worker)
	    (js-function-info :name "JsWorker" :len 1)
	    :__proto__ js-function-prototype
	    :prototype js-worker-prototype
	    :alloc js-no-alloc)))

   (js-bind! %this scope (& "Worker") :value js-worker
      :configurable #f :enumerable #f)

   js-worker)

;*---------------------------------------------------------------------*/
;*    make-plugins-loader ...                                          */
;*---------------------------------------------------------------------*/
(define (make-plugins-loader %ctxthis %ctxmodule worker)
   (when (and (isa? %ctxthis JsGlobalObject) (isa? %ctxmodule JsObject))
      (lambda (lang file conf)
	 (js-worker-run-throws worker "plugins-loader"
	    (lambda (%this)
	       (with-access::JsGlobalObject %ctxthis (js-object js-symbol)
		  (let* ((filemod (nodejs-new-module-sans-cache file file
				     worker %ctxthis))
			 (langmod (nodejs-require-module lang worker
				      %ctxthis filemod))
			 (key (js-get js-symbol (& "compiler") %ctxthis))
			 (parser (let ((o (js-get langmod key %ctxthis)))
				    (when (js-object? o)
				       (js-get o (& "parser") %ctxthis)))))
		     (if (js-object? parser)
			 (let ((ps (js-get parser (& "plugins") %ctxthis)))
			    (if (pair? ps)
				(map (lambda (p)
					(if (procedure? (cdr p))
					    (cons (car p)
					       (lambda (tok decl conf ctrl)
						  (js-worker-run-throws worker
						     "plugins"
						     (lambda (%this)
							((cdr p) tok decl conf ctrl)))))
					    p))
				   ps)
				'()))
			 '()))))))))

;*---------------------------------------------------------------------*/
;*    make-language-loader ...                                         */
;*---------------------------------------------------------------------*/
(define (make-language-loader %ctxthis %ctxmodule worker)
   
   (define (language-loader lang file conf)
      (with-access::JsGlobalObject %ctxthis (js-object js-symbol)
	 (let* ((langmod (nodejs-require-module lang worker
			    %ctxthis %ctxmodule))
		(key (js-get js-symbol (& "compiler") %ctxthis))
		(comp (js-get langmod key %ctxthis)))
	    (js-jsobject->alist
	       (js-call2 %ctxthis comp (js-undefined)
		  (js-string->jsstring file)
		  (js-alist->jsobject conf %ctxthis))
	       %ctxthis))))
   
   (when (and (isa? %ctxthis JsGlobalObject) (isa? %ctxmodule JsObject))
      (lambda (lang file conf)
	 (if (eq? (current-thread) worker)
	     (language-loader lang file conf)
	     (js-worker-exec-throws worker "language-loader"
		(lambda (%this)
		   (language-loader lang file conf)))))))

;*---------------------------------------------------------------------*/
;*    nodejs-plugins-toplevel-loader ...                               */
;*    -------------------------------------------------------------    */
;*    This function is intended to be used by external tools only      */
;*    (e.g., hopc).                                                    */
;*---------------------------------------------------------------------*/
(define (nodejs-plugins-toplevel-loader)
   (multiple-value-bind (worker this module)
      (js-main-worker! "plugin" "plugin" #t
	 nodejs-new-global-object nodejs-new-module)
      (make-plugins-loader this module worker)))

;*---------------------------------------------------------------------*/
;*    nodejs-language-toplevel-loader ...                              */
;*    -------------------------------------------------------------    */
;*    This function is intended to be used by external tools only      */
;*    (e.g., hopc).                                                    */
;*---------------------------------------------------------------------*/
(define (nodejs-language-toplevel-loader)
   (multiple-value-bind (worker this module)
      (js-main-worker! "language" "language" #t
	 nodejs-new-global-object nodejs-new-module)
      (make-language-loader this module worker)))

;*---------------------------------------------------------------------*/
;*    nodejs-language-notify-error ...                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-language-notify-error str proc)

   (define (parse ip)
      (json-parse ip
	 :array-alloc (lambda () (make-cell '()))
	 :array-set (lambda (a i val)
		       (cell-set! a (cons val (cell-ref a))))
	 :array-return (lambda (a i)
			  (reverse! (cell-ref a)))
	 :object-alloc (lambda ()
			  (make-cell '()))
	 :object-set (lambda (o p val)
			(cell-set! o
			   (cons (cons (string->symbol p) val)
			      (cell-ref o))))
	 :object-return (lambda (o)
			   (reverse! (cell-ref o)))
	 :parse-error (lambda (msg fname loc)
			 (error/location proc msg str fname loc))))

   (let ((errs (call-with-input-string str parse)))
      (for-each (lambda (err)
		   (match-case err
		      (((error . ?msg) (at (file . ?filename) (loc . ?loc)))
		       (error-notify/location
			  (instantiate::&error
			     (proc (format "hopc:~a" proc))
			     (msg msg)
			     (obj '%no-error-obj))
			  filename loc))
		      (((error . ?msg))
		       (error-notify
			  (instantiate::&error
			     (proc (format "hopc:~a" proc))
			     (msg msg)
			     (obj '%no-error-obj))))
		      (else
		       (error-notify
			  (instantiate::&error
			     (proc (format "hopc:~a" proc))
			     (msg "compilation error")
			     (obj err))))))
	 errs)))

;*---------------------------------------------------------------------*/
;*    loader-worker ...                                                */
;*---------------------------------------------------------------------*/
(define loader-worker #f)
(define loader-mutex (make-mutex))
(define loader-condv (make-condition-variable))
(define loader-module #f)
(define loader-resolvers '())
(define loader-loaders '())

;*---------------------------------------------------------------------*/
;*    js-loader-worker ...                                             */
;*---------------------------------------------------------------------*/
(define (js-loader-worker)
   
   (define (setup-worker! w)
      (let ((global (nodejs-new-global-object :name "loader"))
	    (path (pwd)))
	 (with-access::JsGlobalObject global (js-object worker js-function-sans-prototype-cmap)
	    (set! worker w)
	    (set! loader-module (nodejs-new-module (basename path) path w global))
	    (with-access::WorkerHopThread worker (%this module-cache %loop)
	       ;; module-cache is used in src/main to check
	       ;; where the worker is running or not
	       (set! module-cache (js-new0 %this js-object))
	       (set! %this global)
	       (js-put! module-cache (js-string->jsstring path)
		  loader-module #f global)))))
   
   (letrec ((worker (instantiate::WorkerHopThread
		       (name "loader")
		       (onexit #f)
		       (keep-alive #t)
		       (body (lambda ()
				(setup-worker! worker)
				(synchronize loader-mutex
				   (condition-variable-broadcast! loader-condv))
				(js-worker-loop worker (lambda (th) th)))))))
      (thread-start! worker)
      (condition-variable-wait! loader-condv loader-mutex)
      worker))

;*---------------------------------------------------------------------*/
;*    load-loader ...                                                  */
;*---------------------------------------------------------------------*/
(define (load-loader this path)
   (with-access::WorkerHopThread loader-worker (%this)
      (let* ((mod (nodejs-load-module path loader-worker %this loader-module
		     :commonjs-export #f))
	     (res (nodejs-module-namespace mod loader-worker %this)))
	 (let ((resolve (js-get res (& "resolve") %this))
	       (load (js-get res (& "load") %this)))
	    (cons resolve load)))))
      
;*---------------------------------------------------------------------*/
;*    nodejs-register-user-loader! ...                                 */
;*    -------------------------------------------------------------    */
;*    This implements the Nodejs module API as described at:           */
;*      https://nodejs.org/docs/latest/api/module.html                 */
;*---------------------------------------------------------------------*/
(define (nodejs-register-user-loader! %this module)
   (synchronize loader-mutex
      (unless loader-worker
	 (set! loader-worker (js-loader-worker))))
   (let ((res (js-worker-exec-throws loader-worker "load"
		 (lambda (this)
		    (load-loader this module)))))
      (synchronize loader-mutex
	 (when (car res)
	    (set! loader-resolvers (append loader-resolvers (list (car res)))))
	 (when (cdr res)
	    (set! loader-loaders (append loader-loaders (list (cdr res))))))
      #unspecified))
   
;*---------------------------------------------------------------------*/
;*    Bind the nodejs require functions                                */
;*---------------------------------------------------------------------*/
(js-worker-load-set!
   (lambda (filename worker this)
      (let ((mod (nodejs-new-module (basename filename) filename worker this)))
	 (nodejs-require-module filename worker this mod))))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
