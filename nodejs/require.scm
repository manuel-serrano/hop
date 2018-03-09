;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/nodejs/require.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 16 15:47:40 2013                          */
;*    Last change :  Fri Mar  9 07:25:03 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo Nodejs module implementation                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs_require

   (library hop hopscript js2scheme web)

   (import __nodejs
	   __nodejs__hop
	   __nodejs_process
	   __nodejs_syncg)

   (export (nodejs-module::JsObject ::bstring ::bstring ::WorkerHopThread ::JsGlobalObject)
	   (nodejs-require ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj)
	   (nodejs-head ::WorkerHopThread ::JsGlobalObject ::JsObject ::JsObject)
	   (nodejs-script ::WorkerHopThread ::JsGlobalObject ::JsObject ::JsObject)
	   (nodejs-core-module ::bstring ::WorkerHopThread ::JsGlobalObject)
	   (nodejs-require-core ::bstring ::WorkerHopThread ::JsGlobalObject)
	   (nodejs-load ::bstring ::WorkerHopThread #!optional lang srcalias)
	   (nodejs-import!  ::JsGlobalObject ::JsObject ::JsObject . bindings)
	   (nodejs-compile-file ::bstring ::bstring ::bstring ::bstring)
	   (nodejs-compile-json ::bstring ::bstring ::bstring ::bstring)
	   (nodejs-compile-html ::bstring ::bstring ::bstring ::bstring)
	   (nodejs-compile-pending::int)
	   (nodejs-compile-add-event-listener! ::bstring ::procedure ::bool)
	   (nodejs-compile-remove-event-listener! ::bstring ::procedure)
	   (nodejs-resolve ::bstring ::JsGlobalObject ::obj ::symbol)
	   (nodejs-resolve-extend-path! ::pair-nil)
	   (nodejs-new-global-object::JsGlobalObject)
	   (nodejs-new-scope-object ::JsGlobalObject)
	   (nodejs-eval ::JsGlobalObject ::JsObject)
	   (nodejs-function ::JsGlobalObject ::JsObject)
	   (nodejs-worker ::JsGlobalObject ::JsObject ::JsObject)))

;;(define-macro (bigloo-debug) 0)

;*---------------------------------------------------------------------*/
;*    compile-mutex ...                                                */
;*---------------------------------------------------------------------*/
(define compile-mutex (make-mutex))
(define compile-table (make-hashtable))

(define compile-pending 0)

(define compile-listeners-all '())
(define compile-listeners-start '())
(define compile-listeners-end '())

;*---------------------------------------------------------------------*/
;*    socompile ...                                                    */
;*---------------------------------------------------------------------*/
(define-struct socompile proc cmd src ksucc kfail abortp)

;*---------------------------------------------------------------------*/
;*    nodejs-compile-file ...                                          */
;*---------------------------------------------------------------------*/
(define (nodejs-compile-file ifile::bstring name::bstring ofile::bstring query)
   (let* ((srcmap (when (>fx (bigloo-debug) 0)
		     (string-append ofile ".map")))
	  (op (if (string=? ofile "-")
		  (current-output-port)
		  (open-output-file ofile)))
	  (tree (unwind-protect
		   (module->javascript ifile name op #f #f srcmap query)
		   (unless (eq? op (current-output-port))
		      (close-output-port op)))))
      (when (>fx (bigloo-debug) 0)
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
	    (debug-compile-trace filename)
	    (let ((tree (j2s-compile in
			   :filename filename
			   :parser 'client-program
			   :site 'client
			   :driver (j2s-javascript-driver)
			   :driver-name "j2s-javascript-driver")))
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
(define (module->javascript filename::bstring id op compile isexpr srcmap query)

   (define (init-dummy-module! this worker)
      (with-access::JsGlobalObject this (js-object)
	 (let ((mod (js-new0 this js-object))
	       (exp (js-new0 this js-object)))
	    (js-put! mod 'exports this #f this)
	    (js-put! mod 'filename (js-string->jsstring filename) #f this)
	    (js-put! this 'global this #f this)
	    (js-put! this 'GLOBAL this #f this)
	    (js-put! this 'module mod #f this)
	    (js-put! this 'exports exp #f this)
	    (js-put! this '__filename
	       (js-string->jsstring filename) #f this)
	    (js-put! this '__dirname
	       (js-string->jsstring (dirname filename)) #f this)
	    (js-put! this 'require
	       (nodejs-require worker this mod (js-string->jsstring "hopscript")
		  #f this)
	    (js-put! this 'process
	       (nodejs-process worker this) #f this)
	    (js-put! this 'console
	       (nodejs-require-core "console" worker this) #f this))))

   (let ((this (nodejs-new-global-object))
	 (worker (js-current-worker))
	 (esplainp (string=? query "es")))
      (init-dummy-module! this worker)
      (let ((header (unless esplainp
		       (format "var exports = {}; var module = { id: ~s, filename: ~s, loaded: true, exports: exports, paths: [~a] };\nhop[ '%modules' ][ '~a' ] = module.exports;\nfunction require( url ) { return hop[ '%require' ]( url, module ) }\n"
			  id filename
			  (js-paths (nodejs-filename->paths filename))
			  filename))))
	 (when header
	    (fprintf op (hop-boot))
	    (fprintf op "hop[ '%requires' ][ ~s ] = function() {\n" filename)
	    (flush-output-port op))
	 (let ((offset (output-port-position op)))
	    (call-with-input-file filename
	       (lambda (in)
		  (debug-compile-trace filename)
		  (let ((tree (j2s-compile in
				 :%this this
				 :source filename
				 :resource (dirname filename)
				 :filename filename
				 :worker worker
				 :header header
				 :verbose (if (>=fx (bigloo-debug) 3)
					      (hop-verbose)
					      0)
				 :parser 'client-program
				 :driver (if (or (<=fx (bigloo-debug) 0)
						 esplainp)
					     (j2s-javascript-driver)
					     (j2s-javascript-debug-driver))
				 :driver-name (if (or (<=fx (bigloo-debug) 0)
						      "esplainp")
						  "j2s-javascript-driver"
						  "j2s-javascript-debug-driver")
				 :site 'client
				 :debug (if esplainp 0 (bigloo-debug)))))
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
			      filename srcmap)
			   (fprintf op "\n//# sourceMappingURL=~a\n" srcmap)))
		     ;; first element of the tree is a position offset
		     ;; see sourcemap generation
		     (cons offset tree))))))))

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
   (if (> (bigloo-debug) 0) (j2s-debug-driver) (j2s-optim-driver)))

;*---------------------------------------------------------------------*/
;*    nodejs-file->paths ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-filename->paths::vector file::bstring)
   (if (char=? (string-ref file 0) #\/)
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
		 (cons
		    (js-string->jsstring (make-file-name dir "node_modules"))
		    acc)))))
       '#()))

;*---------------------------------------------------------------------*/
;*    nodejs-module ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-module::JsObject id filename worker::WorkerHopThread %this::JsGlobalObject)

   (define (module-init! m)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((expo (js-new0 %this js-object)))
	    (with-access::JsObject expo (cmap)
	       (set! cmap (instantiate::JsConstructMap (single #t))))
	    ;; id field
	    (js-put! m 'id (js-string->jsstring id) #f %this)
	    ;; exports
	    (js-put! m 'exports expo #f %this)
	    ;; filename
	    (js-put! m 'filename (js-string->jsstring filename) #f %this)
	    ;; loaded
	    (js-put! m 'loaded #f #f %this)
	    ;; parent
	    (js-put! m 'parent (js-null) #f %this)
	    ;; children
	    (js-put! m 'children (js-vector->jsarray '#() %this) #f %this)
	    ;; paths
	    (js-put! m 'paths
	       (js-vector->jsarray (nodejs-filename->paths filename) %this)
	       #f %this))))

   (with-trace 'require (format "nodejs-module ~a ~a" id filename)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((m (js-new0 %this js-object)))
	    ;; module properties
	    (module-init! m)
	    ;; register the module in the current worker thread
	    (with-access::WorkerHopThread worker (module-cache)
	       (js-put! module-cache filename m #f %this))
	    ;; return the newly allocated module
	    (trace-item "module=" (typeof m))
	    m))))

;*---------------------------------------------------------------------*/
;*    nodejs-require ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-require worker::WorkerHopThread this::JsGlobalObject %module::JsObject language)

   ;; require
   (define require
      (js-make-function this
	 (lambda (_ name lang)
	    (nodejs-require-module (js-tostring name this)
	       (js-current-worker) this %module
	       (cond
		  ((eq? lang (js-undefined))
		   language)
		  ((isa? lang JsObject)
		   (with-access::JsGlobalObject this (js-symbol)
		      (let* ((key (js-get js-symbol 'compiler %this))
			     (comp (js-get lang key %this)))
			 (if (isa? comp JsFunction)
			     comp
			     (js-raise-error (js-new-global-object)
				"Wrong language object"
				lang)))))
		  ((js-jsstring? lang)
		   (let ((mod (nodejs-require worker this 
		   
		  (else
		   (string->symbol (js-tostring lang this))))))
	 2 "require"))

   ;; require.main
   (with-access::JsGlobalObject this (js-main js-object) 
      (js-bind! this require 'main
	 :get (js-make-function this (lambda (this) js-main) 0 'main)
	 :configurable #f :writable #f))
   
   ;; require.resolve
   (js-put! require 'resolve
      (js-make-function this
	 (lambda (_ name)
	    (let ((name (js-tostring name this)))
	       (if (core-module? name)
		   (js-string->jsstring name)
		   (js-string->jsstring (nodejs-resolve name this %module 'body)))))
	 1 "resolve")
      #f this)

   ;; require.cache
   (with-access::WorkerHopThread worker (module-cache)
      (js-bind! this require 'cache
	 :get (js-make-function this
		 (lambda (this) module-cache)
		 0 'cache)
	 :set (js-make-function this
		 (lambda (this v)
		    ;; when setting require.cache, erase the compilation
		    ;; table for avoid out of sync errors
		    (synchronize compile-mutex
		       (set! compile-table (make-hashtable)))
		    (set! module-cache v))
		 1 'cache)
	 :configurable #f))

   ;; module.require
   (js-bind! this %module 'require
      :value require
      :enumerable #f)

   ;; require.language
   (let ((lang (js-string->jsstring (symbol->string language))))
      (js-put! require 'language lang #f this))
   
   require)

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
	    (let ((rts (if (isa? attrs JsObject)
			   (js-get attrs 'rts %scope)
			   #t)))
	       (apply <HEAD> :idiom "javascript" :context %scope
		  (unless (eq? rts #f)
		     (<SCRIPT>
			(format "hop[ '%root' ] = ~s"
			   (dirname
			      (js-jsstring->string (js-get %module 'filename %scope))))))
		  (when (isa? attrs JsObject)
		     (js-object->keyword-arguments* attrs %this))
		  (filter (lambda (n)
			     (or (isa? n xml-tilde)
				 (isa? n xml-markup)
				 (isa? n xml-cdata)))
		     (xml-body nodes)))))
	 -1 "HEAD"))

   head)

;*---------------------------------------------------------------------*/
;*    nodejs-script ...                                                */
;*    -------------------------------------------------------------    */
;*    Per module version of js-html-script@__hopscript_public          */
;*    (see hopscript/public.scm).                                      */
;*---------------------------------------------------------------------*/
(define (nodejs-script worker::WorkerHopThread %this::JsGlobalObject %scope::JsObject %module)
   
   ;; script
   (define script
      (js-make-function %this
	 (lambda (this attrs . nodes)
	    (let ((tmp (apply <SCRIPT> :idiom "javascript" :context %scope
			  (when (isa? attrs JsObject)
			     (js-object->keyword-arguments* attrs %this))
			  nodes)))
	       (if (pair? tmp)
		   (js-vector->jsarray (list->vector tmp) %this)
		   tmp)))
	 -1 "SCRIPT"))
   
   script)

;*---------------------------------------------------------------------*/
;*    *resolve-service* ...                                            */
;*---------------------------------------------------------------------*/
(define *resolve-service* #f)
(define *resolve-this* #f)
(define *resolve-url-path* "public/require/resolve")

;*---------------------------------------------------------------------*/
;*    nodejs-new-global-object ...                                     */
;*---------------------------------------------------------------------*/
(define (nodejs-new-global-object)
   (unless (or *resolve-service* (<fx (hop-port) -1))
      (when (memq (hop-sofile-compile-policy) '(nte nte1 nte+))
	 (nodejs-compile-workers-inits!))
      (set! *resolve-service*
	 (service :name *resolve-url-path* (name filename)
	    (unless *resolve-this*
	       (set! *resolve-this* (js-new-global-object)))
	    (let ((this *resolve-this*))
	       (with-access::JsGlobalObject this (js-object)
		  (let ((m (js-new0 this js-object)))
		     ;; module properties
		     (js-put! m 'id (js-string->jsstring filename) #f this)
		     ;; filename
		     (js-put! m 'filename (js-string->jsstring filename) #f this)
		     ;; paths
		     (js-put! m 'paths
			(js-vector->jsarray (nodejs-filename->paths filename) this)
			#f this)
		     ;; the resolution
		     (nodejs-resolve name this m 'body)))))))
   (let ((obj (js-new-global-object)))
      (nodejs-v8-global-object-init! obj)
      obj))

;*---------------------------------------------------------------------*/
;*    nodejs-v8-global-object-init! ...                                */
;*---------------------------------------------------------------------*/
(define (nodejs-v8-global-object-init! %this::JsGlobalObject)
   ;; v8 compatibility (used by nodejs/lib)
   (with-access::JsGlobalObject %this (js-object)
      (let ((proto (js-get js-object 'prototype %this)))
	 (js-bind! %this proto '__defineGetter__
	    :value (js-make-function %this
		      (lambda (this name fun)
			 (js-bind! %this this
			    (string->symbol (js-jsstring->string name))
			    :get fun))
		      2 "__defineGetter__")
	    :enumerable #f
	    :writable #t
	    :configurable #f)
	 ;; Dtrace profiling
	 (js-bind! %this proto 'DTRACE_HTTP_SERVER_REQUEST
	    :value (js-make-function %this
		      (lambda (this req socket)
			 (js-undefined))
		      2 "DTRACE_HTTP_SERVER_REQUEST")
	    :enumerable #f :writable #f :configurable #f :hidden-class #f)
	 (js-bind! %this proto 'DTRACE_HTTP_SERVER_RESPONSE
	    :value (js-make-function %this
		      (lambda (this req socket)
			 (js-undefined))
		      2 "DTRACE_HTTP_SERVER_RESPONSE")
	    :enumerable #f :writable #f :configurable #f :hidden-class #f)
	 (js-bind! %this proto 'COUNTER_HTTP_SERVER_REQUEST
	    :value (js-make-function %this
		      (lambda (this)
			 (js-undefined))
		      0 "COUNTER_HTTP_SERVER_REQUEST")
	    :enumerable #f :writable #f :configurable #f :hidden-class #f)
	 (js-bind! %this proto 'COUNTER_HTTP_SERVER_RESPONSE
	    :value (js-make-function %this
		      (lambda (this)
			 (js-undefined))
		      0 "COUNTER_HTTP_SERVER_RESPONSE")
	    :enumerable #f :writable #f :configurable #f :hidden-class #f)
	 (js-bind! %this proto 'DTRACE_HTTP_CLIENT_RESPONSE
	    :value (js-make-function %this
		      (lambda (this req socket)
			 (js-undefined))
		      2 "DTRACE_HTTP_CLIENT_RESPONSE")
	    :enumerable #f :writable #f :configurable #f :hidden-class #f)
	 (js-bind! %this proto 'COUNTER_HTTP_CLIENT_REQUEST
	    :value (js-make-function %this
		      (lambda (this)
			 (js-undefined))
		      0 "COUNTER_HTTP_CLIENT_REQUEST")
	    :enumerable #f :writable #f :configurable #f :hidden-class #f)
	 (js-bind! %this proto 'DTRACE_HTTP_CLIENT_REQUEST
	    :value (js-make-function %this
		      (lambda (this req socket)
			 (js-undefined))
		      2 "DTRACE_HTTP_CLIENT_REQUEST")
	    :enumerable #f :writable #f :configurable #f :hidden-class #f)
	 (js-bind! %this proto 'COUNTER_HTTP_CLIENT_RESPONSE
	    :value (js-make-function %this
		      (lambda (this)
			 (js-undefined))
		      0 "COUNTER_HTTP_CLIENT_RESPONSE")
	    :enumerable #f :writable #f :configurable #f :hidden-class #f)
	 (js-bind! %this proto 'DTRACE_NET_STREAM_END
	    :value (js-make-function %this
		      (lambda (this)
			 (js-undefined))
		      0 "DTRACE_NET_STREAM_END")
	    :enumerable #f :writable #f :configurable #f :hidden-class #f)
	 (js-bind! %this proto 'DTRACE_NET_SOCKET_READ
	    :value (js-make-function %this
		      (lambda (this)
			 (js-undefined))
		      0 "DTRACE_NET_SOCKET_READ")
	    :enumerable #f :writable #f :configurable #f :hidden-class #f)
	 (js-bind! %this proto 'DTRACE_NET_SOCKET_WRITE
	    :value (js-make-function %this
		      (lambda (this)
			 (js-undefined))
		      0 "DTRACE_NET_SOCKET_WRITE")
	    :enumerable #f :writable #f :configurable #f :hidden-class #f)
	 (js-bind! %this proto 'DTRACE_NET_SERVER_CONNECTION
	    :value (js-make-function %this
		      (lambda (this)
			 (js-undefined))
		      0 "DTRACE_NET_SERVER_CONNECTION")
	    :enumerable #f :writable #f :configurable #f :hidden-class #f)
	 (js-bind! %this proto 'COUNTER_NET_SERVER_CONNECTION
	    :value (js-make-function %this
		      (lambda (this)
			 (js-undefined))
		      0 "COUNTER_NET_SERVER_CONNECTION")
	    :enumerable #f :writable #f :configurable #f :hidden-class #f)
	 (js-bind! %this proto 'COUNTER_NET_SERVER_CONNECTION_CLOSE
	    :value (js-make-function %this
		      (lambda (this)
			 (js-undefined))
		      0 "COUNTER_NET_SERVER_CONNECTION_CLOSE")
	    :enumerable #f :writable #f :configurable #f :hidden-class #f)))
   %this)

;*---------------------------------------------------------------------*/
;*    nodejs-new-scope-object ...                                      */
;*---------------------------------------------------------------------*/
(define (nodejs-new-scope-object global::JsGlobalObject)
   (let ((scope (duplicate::JsGlobalObject global
		   (cmap (instantiate::JsConstructMap))
		   (__proto__ global)
		   (elements '#()))))
      (js-object-properties-set! scope '())
      (js-object-mode-set! scope (js-object-default-mode))
      (nodejs-import! global scope global)
      (hopscript-global-object-init! scope)
      scope))

;*---------------------------------------------------------------------*/
;*    hopscript-global-object-init! ...                                */
;*---------------------------------------------------------------------*/
(define (hopscript-global-object-init! scope)
   (js-put! scope 'HEAD (js-html-head scope) #f scope))

;*---------------------------------------------------------------------*/
;*    debug-compile-trace ...                                          */
;*---------------------------------------------------------------------*/
(define (debug-compile-trace filename)
   (when (>=fx (bigloo-debug) 4)
      (display "** " (current-error-port))
      (display filename (current-error-port))
      (newline (current-error-port))))

;*---------------------------------------------------------------------*/
;*    nodejs-compile ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-compile filename::bstring #!key lang worker-slave srcalias)
   
   (define (compile-file filename::bstring mod)
      (with-trace 'require "compile-file"
	 (trace-item "filename=" filename)
	 (call-with-input-file filename
	    (lambda (in)
	       (debug-compile-trace filename)
	       (let ((m (open-mmap filename read: #t :write #f)))
		  (unwind-protect
		     (j2s-compile in
			:driver (nodejs-driver)
			:driver-name "nodejs-driver"
			:filename (or srcalias filename)
			:language (or lang 'hopscript)
			:mmap-src m
			:module-main #f
			:module-name (symbol->string mod)
			:worker-slave worker-slave
			:verbose (if (>=fx (bigloo-debug) 3) (hop-verbose) 0)
			:debug (bigloo-debug))
		     (close-mmap m)))))))
   
   (define (compile-url url::bstring mod)
      (with-trace 'require "compile-url"
	 (trace-item "url=" url)
	 (trace-item "filename=" filename)
	 (call-with-input-file url
	    (lambda (in)
	       (debug-compile-trace filename)
	       (input-port-name-set! in url)
	       (j2s-compile in
		  :driver (nodejs-driver)
		  :driver-name "nodejs-driver"
		  :language (or lang 'hopscript)
		  :filename (or filename srcalias)
		  :module-main #f
		  :module-name (symbol->string mod)
		  :worker-slave worker-slave
		  :verbose (if (>=fx (bigloo-debug) 3) (hop-verbose) 0)
		  :debug (bigloo-debug))))))
   
   (define (compile filename::bstring mod)
      (if (file-exists? filename)
	  (compile-file filename mod)
	  (compile-url filename mod)))

   (unless nodejs-debug-compile
      (set! nodejs-debug-compile
	 (if (string-contains (or (getenv "HOPTRACE") "") "nodejs:compile")
	     'yes
	     'no)))

   (synchronize compile-mutex
      (with-trace 'require "nodejs-compile"
	 (trace-item "filename=" filename)
	 (or (hashtable-get compile-table filename)
	     (let* ((mod (gensym))
		    (expr (compile filename mod))
		    (evmod (eval-module)))
		(when (eq? nodejs-debug-compile 'yes)
		   (unless (directory? "/tmp/HOP")
		      (make-directory "/tmp/HOP"))
		   (tprint "nodejs-compile " filename
		      " -> " (make-file-name "/tmp/HOP" (string-replace filename #\/ #\_)))
		   (call-with-output-file
			 (make-file-name "/tmp/HOP" (string-replace filename #\/ #\_))
		      (lambda (op)
			 (pp expr op))))
		(trace-item "expr=" (format "~s" expr))
		(unwind-protect
		   (begin
		      (eval '(module g139933
			      (library hop hopscript js2scheme nodejs)
			      (export
				 (hopscript
				    ::JsGlobalObject
				    ::JsObject
				    ::JsObject
				    ::JsObject))))
		      (for-each eval expr)
		      (let ((hopscript (eval! 'hopscript)))
			 (hashtable-put! compile-table filename hopscript)
			 hopscript))
		   (eval-module-set! evmod)))))))

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
;*    hop-dynamic-load ...                                             */
;*---------------------------------------------------------------------*/
(define (hop-dynamic-load sopath)
   (synchronize soload-mutex
      (let ((old (hashtable-get sofile-cache sopath)))
	 (if old
	     old
	     (let ((v (dynamic-load sopath)))
		(hashtable-put! sofile-cache sopath v)
		v)))))

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
;*    nodejs-compile-workers-inits! ...                                */
;*---------------------------------------------------------------------*/
(define (nodejs-compile-workers-inits!)
   
   (define (compile-worker e::pair)
      (let loop ()
	 (if (<fx socompile-worker-count (hop-sofile-max-workers))
	     (begin
		(set! socompile-worker-count (+fx 1 socompile-worker-count))
		(set! socompile-queue (remq! e socompile-queue))
		(set! socompile-incompile (cons (car e) socompile-incompile))
		(thread-start! (make-compile-worker e)))
	     (begin
		(condition-variable-wait! socompile-condv socompile-mutex)
		(loop)))))
   
   (define (socompile-wait p::struct status::bool)
      (let ((proc (socompile-proc p)))
	 (nodejs-process-wait proc (socompile-src p))
	 (if (and status (=fx (process-exit-status proc) 0))
	     ((socompile-ksucc p))
	     ((socompile-kfail p)))))

   (register-exit-function!
      (lambda (status)
	 (synchronize socompile-mutex
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
			     (when debug-abort
				(tprint "waiting/abort " (socompile-proc p)))
			     (socompile-wait p #f)
			     (when debug-abort
				(tprint "killed " (socompile-proc p))))))
	       socompile-processes)
	    (set! socompile-processes '())
	    (set! socompile-files '())
	    (set! socompile-ended #t)
	    status)))

   (thread-start!
      (instantiate::hopthread
	 (name "socompile-orchestrator")
	 (body (lambda ()
		  (with-handler
		     (lambda (e)
			(exception-notify e))
		     (synchronize socompile-mutex
			(let loop ()
			   (let liip ()
			      (when (pair? socompile-queue)
				 (let ((e (nodejs-select-socompile socompile-queue)))
				    (or (hop-find-sofile (car e))
					(compile-worker e))
				    (liip))))
			   (condition-variable-wait!
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
	 (string-append "socompiler-worker-" (integer->string count)))))

;*---------------------------------------------------------------------*/
;*    make-compile-worker ...                                          */
;*---------------------------------------------------------------------*/
(define (make-compile-worker e::pair)
   (instantiate::hopthread
      (name (soworker-name))
      (body (lambda ()
	       (with-handler
		  (lambda (e)
		     (exception-notify e))
		  (with-trace 'sorequire "make-compile-worker"
		     (trace-item "thread=" (current-thread))
		     (trace-item "e=" e)
		     (nodejs-socompile (car e) (cdr e))
		     (synchronize socompile-mutex
			(set! socompile-worker-count
			   (-fx socompile-worker-count 1))
			(set! socompile-compiled
			   (cons (car e) socompile-compiled))
			(set! socompile-incompile
			   (remq! (car e) socompile-incompile))
			(condition-variable-broadcast! socompile-condv))))))))
   
;*---------------------------------------------------------------------*/
;*    nodejs-select-socompile ...                                      */
;*    -------------------------------------------------------------    */
;*    Select one entry amongst the compile queue. The current          */
;*    strategy is to select the last modified file.                    */
;*---------------------------------------------------------------------*/
(define (nodejs-select-socompile queue::pair)
   ;; socompile-mutex is already acquired
   (let loop ((entries (cdr queue))
	      (entry (car queue)))
      (cond
	 ((null? entries)
	  entry)
	 ((<elong (cer (car entries)) (cer entry))
	  ;; select the oldest entry
	  (loop (cdr entries) (car entries)))
	 (else
	  (loop (cdr entries) entry)))))

;*---------------------------------------------------------------------*/
;*    nodejs-socompile-queue-push ...                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-socompile-queue-push filename lang)
   (with-trace 'sorequire "nodejs-socompile-queue-push"
      (synchronize socompile-mutex
	 (trace-item "filename=" filename)
	 (trace-item "socompile-queue=" (map car socompile-queue))
	 (trace-item "socompile-compiled=" socompile-compiled)
	 (when (and (file-exists? filename)
		    (not (member filename socompile-compiled))
		    (not (member filename socompile-incompile))
		    (not (assoc filename socompile-queue)))
	    (set! socompile-queue
	       (cons (econs filename lang (file-modification-time filename))
		  socompile-queue)))
	 (condition-variable-broadcast! socompile-condv))))

;*---------------------------------------------------------------------*/
;*    nodejs-socompile ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-socompile filename lang)
   
   (define (exec line::pair ksucc::procedure kfail::procedure)
      
      (let* ((cmd (format "~( )" line))
	     (proc (synchronize socompile-mutex
		      (unless socompile-ended
			 (register-socompile-process!
			    (apply run-process (car line)
			       :wait #f
			       error: pipe:
			       output: "/dev/null"
			       (cdr line))
			    cmd filename ksucc kfail))))
	     (mutex (make-mutex "compilation"))
	     (estr #f))
	 ;; verbose
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
	       (synchronize socompile-mutex
		  (set! compile-pending (+fx compile-pending 1))
		  (when (pair? compile-listeners-start)
		     (let ((evt (instantiate::event
				   (name "start")
				   (target filename)
				   (value `(pending: ,compile-pending
					      command: ,cmd)))))
			(apply-listeners compile-listeners-start evt))))
	       (nodejs-process-wait proc filename)
	       (synchronize mutex
		  (if (and socompile-ended (=fx (process-exit-status proc) 0))
		      'ended
		      (let ((soc (unregister-socompile-process! proc)))
			 (unless (=fx (process-exit-status proc) 0)
			    (if soc
				(unless (socompile-abortp soc)
				   estr))))))))))
   
   (define (dump-error cmd sopath msg)
      (call-with-output-file (string-append sopath ".err")
	 (lambda (op)
	    (display cmd op)
	    (newline op)
	    (display msg op))))
   
   (define (make-ksucc sopath sopathtmp)
      (lambda ()
	 (let ((o (string-append (prefix sopathtmp) ".o")))
	    (when (file-exists? o) (delete-file o))
	    (rename-file sopathtmp sopath)
	    sopath)))
   
   (define (make-kfail sopath sopathtmp)
      (lambda ()
	 (let ((o (string-append (prefix sopathtmp) ".o"))
	       (c (string-append (prefix sopathtmp) ".c")))
	    (when (file-exists? o) (delete-file o))
	    (when (file-exists? c) (delete-file c)))
	 (when (file-exists? sopath) (delete-file sopath))
	 (when (file-exists? sopathtmp) (delete-file sopathtmp))))
   
   (with-trace 'sorequire (format "nodejs-socompile ~a" filename)
      (let loop ()
	 (let ((tmp (synchronize socompile-mutex
		       (cond
			  ((hop-find-sofile filename)
			   =>
			   (lambda (x) x))
			  ((member filename socompile-files)
			   (condition-variable-wait!
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
		   exception-notify
		   (let* ((sopath (hop-sofile-path filename))
			  (sopathtmp (make-file-name
					(dirname sopath)
					(string-append "#" (basename sopath))))
			  (cmd `(,(hop-hopc)
				  ;; bigloo
				  ,(format "--bigloo=~a" (hop-bigloo))
				  ;; verbosity
				  ,@(if (= (hop-verbose) 0)
					(if (eq? nodejs-debug-compile 'yes) '("-v4") '())
				       (list (format "-v~a" (hop-verbose))))
				  ;; source
				  ,filename
				  ;; target
				  "-y" "--js-no-module-main" "-o" ,sopathtmp
				  ;; profiling
				  ,@(if (hop-profile) '("--profile") '())
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
				  ,@(call-with-input-string (hop-hopc-flags) port->string-list)))
			  (ksucc (make-ksucc sopath sopathtmp))
			  (kfail (make-kfail sopath sopathtmp)))
		      (make-directories (dirname sopath))
		      (trace-item "sopath=" sopath)
		      (trace-item "sopathtmp=" sopathtmp)
		      (trace-item "cmd=" cmd)
		      (hop-verb 3 (hop-color -2 -2 " COMPILE") " "
			 (format "~( )\n"
			    (map (lambda (s)
				    (if (string-index s #\space)
					(string-append "\"" s "\"")
					s))
			       cmd)))
		      (synchronize-global
			 (make-file-name
			    (dirname (hop-sofile-path "hop.lock"))
			    "hop.lock")
			 (lambda ()
			    (let ((msg (exec cmd ksucc kfail)))
			       (trace-item "msg=" msg)
			       (unwind-protect
				  (cond
				     ((not msg)
				      ;; compilation succeeded
				      (ksucc))
				     ((string? msg)
				      ;; compilation failed
				      (kfail)
				      (hop-verb 3
					 (hop-color -1 -1 " COMPILE-ERROR")
					 " " cmd "\n")
				      (dump-error cmd sopath msg)
				      'error))
				  (synchronize socompile-mutex
				     (unless socompile-ended
					(set! socompile-files
					   (delete! filename socompile-files))
					(condition-variable-broadcast!
					   socompile-condv)))))))))))))))

;*---------------------------------------------------------------------*/
;*    nodejs-load ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-load filename worker::WorkerHopThread #!optional lang srcalias)

   (define (loadso-or-compile filename lang worker-slave)
      (if worker-slave
	  (nodejs-compile filename :lang lang :worker-slave #t :srcalias srcalias)
	  (let loop ((sopath (hop-find-sofile filename)))
	     (cond
		((string? sopath)
		 (let ((p (hop-dynamic-load sopath)))
		    (if (and (procedure? p) (=fx (procedure-arity p) 4))
			p
			(js-raise-error (js-new-global-object)
			   (format "Wrong compiled file format ~s" sopath)
			   sopath))))
		((and (not (eq? sopath 'error)) (hop-sofile-enable))
		 (case (hop-sofile-compile-policy)
		    ((aot)
		     (loop (nodejs-socompile filename lang)))
		    ((nte nte1 nte+)
		     (nodejs-socompile-queue-push filename lang)
		     (nodejs-compile filename :lang lang :srcalias srcalias))
		    (else
		     (nodejs-compile filename :lang lang :srcalias srcalias))))
		(else
		 (when (memq (hop-sofile-compile-policy) '(nte1 nte+))
		    (nodejs-socompile-queue-push filename lang))
		 (nodejs-compile filename :lang lang :srcalias srcalias))))))
   
   (define (load-module-js)
      (with-trace 'require "require@load-module-js"
	 (with-access::WorkerHopThread worker (%this prehook parent)
	    (with-access::JsGlobalObject %this (js-object js-main)
	       (let ((hopscript (loadso-or-compile filename lang parent))
		     (this (js-new0 %this js-object))
		     (scope (nodejs-new-scope-object %this))
		     (mod (nodejs-module (if js-main filename ".")
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
			   (js-delete! module-cache filename #f %this))
			(raise e))
		     (hopscript %this this scope mod))
		  ;; set the loaded property
		  (js-put! mod 'loaded #t #f %this)
		  ;; return the newly created module
		  (trace-item "mod=" (typeof mod))
		  mod)))))

   (define (load-module-html)
      (with-trace 'require "require@load-module-html"
	 (with-access::WorkerHopThread worker (%this prehook)
	    (with-access::JsGlobalObject %this (js-object js-main)
	       (let ((hopscript (nodejs-compile filename :lang lang))
		     (this (js-new0 %this js-object))
		     (scope (nodejs-new-scope-object %this))
		     (mod (nodejs-module (if js-main filename ".")
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
			   (js-delete! module-cache filename #f %this))
			(raise e))
		     ;; exports the HTML value
		     (js-put! mod 'exports (hopscript %this this scope mod)
			#f %this))
		  ;; return the newly created module
		  (trace-item "mod=" (typeof mod))
		  mod)))))
   
   (define (hop-load/cache filename)
      (let ((old (hashtable-get hop-load-cache filename)))
	 (unless old
	    (set! old (hop-load filename :mode 'module))
	    (hashtable-put! hop-load-cache filename old))
	 old))
   
   (define (load-module-hop)
      (with-access::WorkerHopThread worker (%this)
	 (with-access::JsGlobalObject %this (js-object)
	    (let ((evmod-or-init (hop-load/cache filename))
		  (this (js-new0 %this js-object))
		  (scope (nodejs-new-scope-object %this))
		  (mod (nodejs-module filename
			  (or srcalias filename) worker %this)))
	       (cond
		  ((and (procedure? evmod-or-init)
			(=fx (procedure-arity evmod-or-init) 4))
		   (evmod-or-init %this this scope mod))
		  ((evmodule? evmod-or-init)
		   (call-with-eval-module evmod-or-init
		      (lambda ()
			 ((eval! 'hopscript) %this this scope mod)))))
	       ;; return the newly created module
	       mod))))
   
   (define (load-module-so)
      (with-access::WorkerHopThread worker (%this)
	 (with-access::JsGlobalObject %this (js-object)
	    (let ((init (hop-dynamic-load filename))
		  (this (js-new0 %this js-object))
		  (scope (nodejs-new-scope-object %this))
		  (mod (nodejs-module filename
			  (or srcalias filename) worker %this)))
	       (when (procedure? init)
		  (init %this this scope mod))
	       ;; return the newly created module
	       mod))))
   
   (define (not-found filename)
      (js-raise-error (js-new-global-object)
	 (format "Don't know how to load module ~s" filename)
	 filename))

   (define (load-module)
      (cond
	 ((string-suffix? ".js" filename)
	  (load-module-js))
	 ((string-suffix? ".html" filename)
	  (load-module-html))
	 ((string-suffix? ".hop" filename)
	  (load-module-hop))
	 ((string-suffix? ".so" filename)
	  (load-module-so))
	 ((string-suffix? ".dylib" filename)
	  (load-module-so))
	 ((or (string-prefix? "http://" filename)
	      (string-prefix? "https://" filename))
	  (case lang
	     ((html) (load-module-html))
	     (else (load-module-js))))
	 ((not (string-index (basename filename) #\.))
	  (load-module-js))
	 (else
	  (not-found filename))))

   (with-trace 'require (format "nodejs-load ~a" filename)
      (with-loading-file filename load-module)))

;*---------------------------------------------------------------------*/
;*    nodejs-require-module ...                                        */
;*    -------------------------------------------------------------    */
;*    Require a nodejs module, load it if necessary or simply          */
;*    reuse the previously loaded module structure.                    */
;*---------------------------------------------------------------------*/
(define (nodejs-require-module name::bstring worker::WorkerHopThread %this
	   %module #!optional lang)
   
   (define (load-json path)
      (let ((mod (nodejs-module path path worker %this))
	    (json (call-with-input-file path
		     (lambda (ip)
			(js-json-parser ip #f #f #f %this)))))
	 (js-put! mod 'exports json #f %this)
	 mod))
   
   (define (load-module path worker %this %module lang srcalias)
      (cond
	 ((core-module? path)
	  (nodejs-core-module path worker %this))
	 ((isa? lang JsFunction)
	  (let ((obj (js-call1 %this lang (js-undefined) path)))
	     (when (isa? obj JsObject)
		(let ((ty (js-tostring (js-get obj 'type %this) %this))
		      (val (js-get obj 'value %this))
		      (lang (js-get obj 'language %this)))
		   (cond
		      ((string=? ty "filename")
		       (load-module (js-tostring val %this)
			  worker %this %module
			  (if (eq? lang (js-undefined))
			      'hopscript
			      (string->symbol (js-tostring lang %this)))
			  path))
		      (else
		       val))))))
	 ((string-suffix? ".json" path)
	  (load-json path))
	 (else
	  (let ((mod (nodejs-load path worker lang srcalias)))
	     (unless (js-get mod 'parent %this)
		;; parent and children
		(let* ((children (js-get %module 'children %this))
		       (push (js-get children 'push %this)))
		   (js-call1 %this push children mod)
		   (js-put! mod 'parent %module #f %this)))
	     mod))))

   (with-trace 'require (format "nodejs-require-module ~a" name)
      (with-access::WorkerHopThread worker (module-cache)
	 (let* ((path (nodejs-resolve name %this %module 'body))
		(mod (js-get-property-value module-cache module-cache path %this)))
	    (trace-item "path=" path)
	    (trace-item "mod=" (if (eq? mod (js-absent)) 'absent (typeof mod)))
	    (if (eq? mod (js-absent))
		(let ((mod (load-module path worker %this %module lang #f)))
		   (js-get mod 'exports %this))
		(let ((exports (js-get mod 'exports %this)))
		   (trace-item "exports=" (typeof exports))
		   exports))))))

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
	    (let ((init (cdr (assoc name (core-module-table))))
		  (this (js-new0 %this js-object))
		  (scope (nodejs-new-scope-object %this))
		  (mod (nodejs-module name name worker %this)))
	       ;; initialize the core module
	       (with-trace 'require (format "nodejs-init-core.init ~a" name)
		  (init %this this scope mod))
	       ;; return the module
	       (trace-item "mod=" (typeof mod))
	       (trace-item "gencmapid=" (gencmapid))
	       mod))))
   
   (with-trace 'require (format "nodejs-core-module ~a" name)
      (with-access::WorkerHopThread worker (module-cache)
	 (let ((m (js-get-property-value module-cache module-cache name %this)))
	    (if (eq? m (js-absent))
		(nodejs-init-core name worker %this)
		m)))))

;*---------------------------------------------------------------------*/
;*    nodejs-require-core ...                                          */
;*    -------------------------------------------------------------    */
;*    Require a nodejs module, load it if necessary or simply          */
;*    reuse the previously loaded module structure.                    */
;*---------------------------------------------------------------------*/
(define (nodejs-require-core name::bstring worker %this)
   (with-trace 'require (format "nodejs-require-core ~a" name)
      (let ((e (js-get (nodejs-core-module name worker %this) 'exports %this)))
	 (with-access::JsObject e (cmap)
	    (with-access::JsConstructMap cmap (%id props)
	       (trace-item name " cmap.id=" %id " cmap.props=" props)
	       e)))))

;*---------------------------------------------------------------------*/
;*    nodejs-resolve ...                                               */
;*    -------------------------------------------------------------    */
;*    Resolve the path name according to the current module path.      */
;*    -------------------------------------------------------------    */
;*    http://nodejs.org/api/modules.html#modules_all_together          */
;*---------------------------------------------------------------------*/
(define (nodejs-resolve name::bstring %this::JsGlobalObject %module mode)
   
   (define (resolve-file x)
      (cond
	 ((and (file-exists? x) (not (directory? x)))
	  (if (string-suffix? ".hz" x)
	      (resolve-hz x)
	      (file-name-canonicalize x)))
	 ((string-suffix? ".hz" x)
	  (resolve-autoload-hz x))
	 (else
	  (let loop ((suffixes '(".js" ".hop" ".so" ".json" ".hss" ".css")))
	     (when (pair? suffixes)
		(let* ((suffix (car suffixes))
		       (src (string-append x suffix)))
		   (if (and (file-exists? src) (not (directory? src)))
		       (file-name-canonicalize src)
		       (loop (cdr suffixes)))))))))

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
   
   (define (resolve-package pkg dir)
      (call-with-input-file pkg
	 (lambda (ip)
	    (let* ((o (json-parse ip
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
			 :parse-error (lambda (msg path loc)
					 (js-raise-syntax-error/loc %this
					    `(at ,path ,loc)
					    msg path))))
		   (m (assoc (if (eq? mode 'head) "client" "main") o)))
	       (if (pair? m)
		   (cdr m)
		   (let ((idx (make-file-name dir "index.js")))
		      (when (file-exists? idx)
			 idx)))))))
   
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
	 (or (resolve-file file)
	     (resolve-directory file))))
   
   (define (resolve-error x)
      (with-access::JsGlobalObject %this (js-uri-error)
	 (let ((exn (js-new %this js-uri-error
		       (js-string->jsstring (format "Cannot find module ~s" name))
		       7)))
	    (js-put! exn 'code (js-string->jsstring "MODULE_NOT_FOUND")
	       #f %this)
	    (js-raise exn))))
   
   (define (resolve-modules mod x)
      (any (lambda (dir)
	      (resolve-file-or-directory x dir))
	 (node-modules-path mod)))
   
   (define (node-modules-path mod)
      (let ((paths (js-get mod 'paths %this)))
	 (cond
	    ((pair? paths)
	     (append (map js-jsstring->string paths) nodejs-env-path))
	    ((isa? paths JsArray)
	     (with-access::JsArray paths (vec)
		(append (map! js-jsstring->string (vector->list vec))
		   nodejs-env-path)))
	    (else
	     nodejs-env-path))))
   
   (with-trace 'require "nodejs-resolve"
      (let* ((mod %module)
	     (filename (js-jsstring->string (js-get mod 'filename %this)))
	     (dir (dirname filename)))
	 (trace-item "name=" name)
	 (trace-item "dir=" dir)
	 (trace-item "paths=" (let ((paths (js-get mod 'paths %this)))
				(if (isa? paths JsArray)
				    (jsarray->vector paths %this)
				    paths)))
	 (cond
	    ((core-module? name)
	     name)
	    ((or (string-prefix? "http://" name)
		 (string-prefix? "https://" name))
	     name)
	    ((or (string-prefix? "http://" dir)
		 (string-prefix? "https://" dir))
	     (multiple-value-bind (scheme uinfo host port path)
		(url-parse filename)
		(let* ((paths (js-get mod 'paths %this))
		       (abspath (hop-apply-url
				   (string-append "/hop/" *resolve-url-path*)
				   (list name path))))
		   (with-hop-remote abspath
		      (lambda (x)
			 (if uinfo
			     (format "~a://~a:~a~@a~a" scheme host port uinfo x)
			     (format "~a://~a:~a~a" scheme host port x)))
		      (lambda (x) #f)
		      :host host
		      :port port))))
	    ((or (string-prefix? "./" name) (string-prefix? "../" name))
	     (or (resolve-file-or-directory name dir)
		 (resolve-modules mod name)
		 (resolve-error name)))
	    ((string-prefix? "/" name)
	     (or (resolve-file-or-directory name "/")
		 (resolve-modules mod name)
		 (resolve-error name)))
	    ((string-suffix? ".hz" name)
	     (or (resolve-hz name)
		 (resolve-modules mod name)
		 (resolve-error name)))
	    (else
	     (or (resolve-modules mod name)
		 (resolve-error name)))))))

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
;*    nodejs-resolve-extend-path! ...                                  */
;*    -------------------------------------------------------------    */
;*    This function is used by hop to extend the require search        */
;*    path                                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-resolve-extend-path! path)
   (set! nodejs-env-path (append nodejs-env-path path)))

;*---------------------------------------------------------------------*/
;*    nodejs-import! ...                                               */
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
      
      (define (in-mapped-property n)
	 (when (symbol? (property-name n)) (proc (property-name n))))
      
      (define (in-property p)
	 (when (isa? p JsPropertyDescriptor)
	    (with-access::JsPropertyDescriptor p (name)
	       (proc name))))
      
      (let loop ((o obj))
	 (with-access::JsObject o (cmap __proto__)
	    (if (not (eq? cmap (js-not-a-cmap)))
		(with-access::JsConstructMap cmap (props)
		   (vfor-each in-mapped-property props))
		(for-each in-property (js-object-properties o))))))
   
   ;; e start being undefined during the first steps of the rts boot
   (when (isa? e JsObject)
      ;; bind all the exported functions in the global object
      (if (null? bindings)
	  (for-in e
	     (lambda (k)
		(js-bind! %this %scope k
		   :value (js-get e k %this)
		   :writable #t :enumerable #f :configurable #f
		   :hidden-class #f)))
		;;(js-put! %scope k  #f %this)
	  (for-each (lambda (k)
		       (js-bind! %this %scope k
			  :get (js-make-function %this
				  (lambda (o)
				     (js-get e k %this))
				  0 'get)
			  :hidden-class #f))
	     bindings))))

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

   (js-bind! %this scope 'eval
      :value (js-make-function %this js-eval 1 'eval :prototype (js-undefined))
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
	    :src (cons (current-loc) "Function() { /* require.scm */ }")
	    :__proto__ js-function-prototype
	    :prototype js-function-prototype
	    :construct js-function-construct)))

   (js-bind! %this scope 'Function
      :value js-function
      :configurable #f :enumerable #f :hidden-class #f))


;*---------------------------------------------------------------------*/
;*    nodejs-worker ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-worker %this::JsGlobalObject scope::JsObject %module::JsObject)

   (define (loader filename worker this)
      (if (string? filename)
	  (nodejs-require-module filename worker this %module)
	  (js-raise-error %this
	     (format "Cannot load worker module ~a" filename)
	     filename)))

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
	 (nodejs-module (basename filename) filename worker this))))
