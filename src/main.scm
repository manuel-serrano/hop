;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/src/main.scm                        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Thu May 11 07:57:54 2023 (serrano)                */
;*    Copyright   :  2004-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP entry point                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module main

   (include "libraries.sch")
   
   (import  hop_parseargs
	    hop_param
	    hop_init
	    hop_accept
	    hop_scheduler
	    hop_scheduler-nothread
	    hop_scheduler-queue
	    hop_scheduler-one-to-one
	    hop_scheduler-pool
	    hop_scheduler-accept-many)

   (cond-expand
      ((library libbacktrace)
       (library libbacktrace)))
   
   (cond-expand
      (hop-library
       (extern (export main "hop_main"))
       (export (main x)))
      (boot-from-java
       ;; java name is bigloo.hop.main.main (args)
       (export (main::int args::pair-nil)))
      (else (main main))))

;*---------------------------------------------------------------------*/
;*    signal-init! ...                                                 */
;*---------------------------------------------------------------------*/
(define (signal-init!)
   (cond-expand
      (enable-threads #unspecified)
      (else (signal sigpipe (lambda (n) #unspecified))))
   (signal sigterm
      (lambda (n)
	 (unless (current-thread)
	    ((hop-sigterm-handler) n)))))

;*---------------------------------------------------------------------*/
;*    js initialization ...                                            */
;*---------------------------------------------------------------------*/
(define jsmutex (make-mutex))
(define jscondv (make-condition-variable))
(define jsinit #f)

(define lock (make-spinlock))

;*---------------------------------------------------------------------*/
;*    jsctx ...                                                        */
;*---------------------------------------------------------------------*/
(define-struct jsctx global module worker)

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   ;; gc traces
   (let ((env (getenv "HOPTRACE")))
      (when (and (string? env) (string-contains env "hopscript:gc"))
	 (cond-expand
	    (gc ($bgl-gc-verbose-set! #t))
	    (else #unspecified))))
   ;; catch critical signals
   (signal-init!)
   ;; set the Hop cond-expand identification
   (for-each register-srfi! (cons 'hop-server (hop-srfis)))
   ;; set the library load path
   (bigloo-library-path-set! (hop-library-path))
   ;; define the Hop macros
   (hop-install-expanders!)
   ;; setup the hop readers
   (bigloo-load-reader-set! hop-read)
   (bigloo-load-module-set!
      (lambda (f mod)
	 (hop-load-modified f :abase #t :afile #f :module mod)))
   ;; setup the hop module resolvers
   (bigloo-module-extension-handler-set!
      hop-module-extension-handler)
   (bigloo-module-resolver-set!
      (make-hop-module-resolver (bigloo-module-resolver)))
   (let ((jsctx #f))
      ;; parse the command line
      (multiple-value-bind (files exprs exprsjs)
	 (parse-args args)
	 ;; extent the require search path to the Hop autoload directories
	 (nodejs-resolve-extend-path! (hop-autoload-directories))
	 ;; install the builtin filters
	 (hop-filter-add! service-filter)
	 (hop-init args files exprs)
	 ;; adjust the actual hop-port before executing RC file
	 (if (hop-javascript)
	     (set! jsctx
		(javascript-init
		   (if (pair? files) (car files) ".")
		   args exprsjs))
	     (users-close!))
	 ;; when debugging, init the debugger runtime
	 (hop-debug-init! (hop-client-output-port))
	 ;; prepare the regular http handling
	 (init-http!)
	 (when (hop-enable-webdav) (init-webdav!))
	 ;; close filter installation
	 (unless (hop-javascript)
	    (hop-filters-close!))
	 ;; https file handling
	 (cond-expand
	    (enable-ssl
	     (input-port-protocol-set! "https://" open-input-https-socket)))
	 ;; start zeroconf
	 (when (hop-enable-zeroconf) (init-zeroconf!))
	 ;; create the scheduler (unless the rc file has already created one)
	 (unless (or (isa? (hop-scheduler) scheduler) (not (hop-run-server)))
	    (hop-scheduler-set! (make-hop-scheduler)))
	 ;; start the hop scheduler loop (i.e. the hop main loop)
	 (with-handler
	    (lambda (e)
	       (exception-notify e)
	       (fprint (current-error-port)
		  "An error has occurred in the Hop main loop, exiting...")
	       (exit 1))
	    (let ((serv (hop-server-socket))
		  (servs (hop-server-ssl-socket)))
	       ;; ready to now say hello
	       (hello-world)
	       ;; when needed, start the HOP repl
	       (when (eq? (hop-enable-repl) 'scm)
		  (hop-repl (hop-scheduler)))
	       ;; when needed, start a loop for server events
	       (hop-event-init!)
	       (cond
		  ((hop-run-server)
		   ;; preload all the forced services
		   (for-each (lambda (svc)
				(let* ((path (string-append (hop-service-base)
						"/" svc))
				       (req (instantiate::http-server-request
					       (path path)
					       (abspath path)
					       (port (hop-default-port))
					       (connection 'close))))
				   (with-handler
				      (lambda (err)
					 (exception-notify err)
					 (fprintf (current-error-port)
					    "*** WARNING: Service \"~a\" cannot be pre-loaded.\n" svc))
				      (service-filter req))))
		      (hop-preload-services))
		   ;; close the filters, users, and security
		   (security-close!)
		   ;; wait for js init
		   (when jsctx
		      (users-close!)
		      (hop-filters-close!)
		      (javascript-load-files files exprsjs jsctx)
		      (synchronize jsmutex
			 (set! jsinit #t)
			 (condition-variable-broadcast! jscondv)))
		   ;; start zeroconf
		   (when (hop-enable-zeroconf) (init-zeroconf!))
		   ;; start the main loop
		   (cond
		      ((and serv servs)
		       (cond-expand
			  (enable-threads 
			   (thread-start!
			      (instantiate::hopthread
				 (body (lambda ()
					  (scheduler-accept-loop
					     (make-hop-scheduler)
					     servs #t)))))
			   (scheduler-accept-loop (hop-scheduler) serv #t))
			  (else
			   (error "hop"
			      "Thread support missing for running both http and https servers"
			      servs))))
		      (serv
		       (scheduler-accept-loop (hop-scheduler) serv #t))
		      (servs
		       (scheduler-accept-loop (hop-scheduler) servs #t)))
		   (when jsctx
		      (thread-join! (jsctx-worker jsctx))))
		  (jsctx
		   (users-close!)
		   (hop-filters-close!)
		   (javascript-load-files files exprsjs jsctx)
		   ;; start zeroconf
		   (when (hop-enable-zeroconf) (init-zeroconf!))
		   (synchronize jsmutex
		      (set! jsinit #t)
		      (condition-variable-broadcast! jscondv))
		   (if (thread-join! (jsctx-worker jsctx)) 0 1))
		  (else
		   (sleep 2000)
		   0)))))))

;*---------------------------------------------------------------------*/
;*    hop-init ...                                                     */
;*---------------------------------------------------------------------*/
(define (hop-init args files exprs)
   ;; preload the command-line files
   (when (pair? files)
      (let ((req (instantiate::http-server-request
		    (host "localhost")
		    (port (hop-default-port)))))
	 ;; set a dummy request
	 (thread-request-set! #unspecified req)
	 ;; preload the user files
	 (for-each (lambda (f) (load-command-line-weblet f #f #f #f)) files)
	 ;; unset the dummy request
	 (thread-request-set! #unspecified #unspecified)))
   ;; evaluate the user expressions
   (for-each (lambda (expr)
		(call-with-input-string expr
		   (lambda (ip)
		      (let ((sexp (hop-read ip)))
			 (with-handler
			    (lambda (e)
			       (if (isa? e &eval-warning)
				   (begin
				      (warning-notify e)
				      #unspecified)
				   (raise e)))
			    (eval sexp))))))
      exprs))

;*---------------------------------------------------------------------*/
;*    javascript-init ...                                              */
;*---------------------------------------------------------------------*/
(define (javascript-init name args exprs)
   ;; install the hopscript expanders
   (hopscript-install-expanders!)
   (multiple-value-bind (%worker %global %module)
      (js-main-worker! "main"
	 (format "hop-~a~a (~a)" (hop-version) (hop-minor-version) (hop-build-tag))
	 (or (hop-run-server) (eq? (hop-enable-repl) 'js))
	 nodejs-new-global-object nodejs-new-module)
      ;; js loader
      (hop-loader-add! "js"
	 (lambda (path . test)
	    (js-worker-exec %worker "hop-loader"
	       (lambda (%this)
		  (nodejs-load path path  %worker %global %module :commonjs-export #t)))))
      (hop-loader-add! "mjs"
	 (lambda (path . test)
	    (js-worker-exec %worker "hop-loader"
	       (lambda (%this)
		  (nodejs-load path path %worker %global %module :commonjs-export #t)))))
      (hop-loader-add! "ast.json"
	 (lambda (path . test)
	    (js-worker-exec %worker "hop-loader"
	       (lambda (%this)
		  (nodejs-load path path %worker %global %module :commonjs-export #t)))))
      ;; ts loader
      (hop-loader-add! "ts"
	 (lambda (path . test)
	    (js-worker-exec %worker "hop-loader"
	       (lambda (%this)
		  (nodejs-load path path %worker %global %module :lang "ts" :commonjs-export #t)))))
      ;; profiling
      (when (hop-profile)
	 (js-profile-init `(:server #t) #f #f))
      ;; rc.js file
      (if (string? (hop-rc-loaded))
	  (let ((rcmutex (make-mutex))
		(rccondv (make-condition-variable)))
	     (synchronize rcmutex
		(javascript-rc %global %module %worker rcmutex rccondv)
		(javascript-init-main-loop exprs %global %module %worker)
		(condition-variable-wait! rccondv rcmutex)
		(jsctx %global %module %worker)))
	  (begin
	     (javascript-init-main-loop exprs %global %module %worker)
	     (jsctx %global %module %worker)))))

;*---------------------------------------------------------------------*/
;*    javascript-init-main-loop ...                                    */
;*---------------------------------------------------------------------*/
(define (javascript-init-main-loop exprs %global %module %worker)
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
   ;; close user registration
   (js-worker-push! %worker "jsinit"
      (lambda (%this)
	 (synchronize jsmutex
	    (unless jsinit
	       (condition-variable-wait! jscondv jsmutex)))))
   ;; start the JS repl loop
   (when (eq? (hop-enable-repl) 'js)
      (js-worker-push! %worker "repl"
	 (lambda (%this)
	    (hopscript-repl (hop-scheduler) %global %worker))))
   ;; start the javascript loop
   (with-access::WorkerHopThread %worker (mutex condv module-cache)
      (synchronize mutex
	 ;; module-cache is #f until the worker is initialized and
	 ;; running (see hopscript/worker.scm)
	 (unless module-cache
	    (condition-variable-wait! condv mutex))))
   ;; return the worker for the main loop to join
   %worker)

;*---------------------------------------------------------------------*/
;*    javascript-rc ...                                                */
;*    -------------------------------------------------------------    */
;*    Load the hoprc.js in a sandboxed environment.                    */
;*---------------------------------------------------------------------*/
(define (javascript-rc %global %module %worker rcmutex rccondv)
   
   (define (load-rc path)
      (hop-rc-file-set! path)
      ;; behave as if rc file is not loaded yet
      (hop-rc-loaded! #f)
      ;; set the preferred language
      (hop-preferred-language-set! "hopscript")
      ;; force the module initialization
      (js-worker-push! %worker "rc"
	 (lambda (%this)
	    (let ((path (if (and (>fx (string-length path) 0)
				 (char=? (string-ref path 0) (file-separator)))
			    path
			    (file-name-canonicalize!
			       (make-file-name (pwd) path)))))
	       (let ((oldload (hop-rc-loaded)))
		  (hop-rc-loaded! #f)
		  (unwind-protect
		     ;;(nodejs-load path path %worker %global %module :commonjs-export #t)
		     (nodejs-load-module path %worker %global %module :commonjs-export #t)
		     (begin
			(hop-rc-loaded! oldload)
			(synchronize rcmutex
			   (condition-variable-broadcast! rccondv)))))))))
   
   (let ((path (string-append (prefix (hop-rc-loaded)) ".js")))
      (if (file-exists? path)
	  (load-rc path)
	  (let ((path (string-append (prefix (hop-rc-file)) ".js")))
	     (if (file-exists? path)
		 (load-rc path)
		 (js-worker-push! %worker "init"
		    (lambda (%this)
		       (synchronize rcmutex
			  (condition-variable-broadcast! rccondv)))))))))

;*---------------------------------------------------------------------*/
;*    javascript-load-files ...                                        */
;*---------------------------------------------------------------------*/
(define (javascript-load-files files exprs jsctx)
   ;; preload the command-line files
   (when (pair? files)
      (let ((%global (jsctx-global jsctx))
	    (%module (jsctx-module jsctx))
	    (%worker (jsctx-worker jsctx)))
	 (let ((req (instantiate::http-server-request
		       (host "localhost")
		       (port (hop-default-port)))))
	    ;; set a dummy request
	    (thread-request-set! #unspecified req)
	    ;; preload the user files
	    (for-each (lambda (f)
			 (load-command-line-weblet f %global %module %worker))
	       files)
	    ;; unset the dummy request
	    (thread-request-set! #unspecified #unspecified)))))

;*---------------------------------------------------------------------*/
;*    javascript-init-hss ...                                          */
;*---------------------------------------------------------------------*/
(define (javascript-init-hss %worker %global)
   (let ((exp (call-with-input-string "false"
		 (lambda (in)
		    (j2s-compile in :driver (j2s-plain-driver)
		       :driver-name "j2s-plain-driver"
		       :parser 'repl
		       :verbose 0
		       :hopscript-header #f
		       :filename "javascript-init-hss")))))
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
		     (js-worker-exec %worker "hss"
			(lambda (%this)
			   (js-put! mod (& "filename")
			      (js-string->jsstring (input-port-name ip)) #f
			      %global)
			   (%js-eval-hss ip %global %worker scope))))))))))

;*---------------------------------------------------------------------*/
;*    make-hop-scheduler ...                                           */
;*---------------------------------------------------------------------*/
(define (make-hop-scheduler)
   (cond-expand
      (enable-threads
       (case (hop-scheduling)
	  ((nothread)
	   (instantiate::nothread-scheduler))
	  ((queue)
	   (instantiate::queue-scheduler
	      (size (hop-max-threads))
	      (onready hop-acknowledge)))
	  ((one-to-one)
	   (instantiate::one-to-one-scheduler
	      (size (hop-max-threads))
	      (onready hop-acknowledge)))
	  ((pool)
	   (instantiate::pool-scheduler
	      (size (hop-max-threads))
	      (onready hop-acknowledge)))
	  ((accept-many)
	   (instantiate::accept-many-scheduler
	      (size (hop-max-threads))
	      (onready hop-acknowledge)))
	  (else
	   (error "hop" "Unknown scheduling policy" (hop-scheduling)))))
      (else
       (unless (eq? (hop-scheduling) 'nothread)
	  (warning "hop" "Threads disabled, forcing \"nothread\" scheduler."))
       (instantiate::nothread-scheduler))))

;*---------------------------------------------------------------------*/
;*    load-command-line-weblet ...                                     */
;*---------------------------------------------------------------------*/
(define (load-command-line-weblet f %global %module %worker)

   (define (load-hop-directory path)
      (let ((src (string-append (basename path) ".hop")))
	 (when (file-exists? src)
	    (hop-load-weblet (make-file-name path src)))))

   (define (load-js-directory path)
      (let ((src (string-append (basename path) ".js")))
	 (if (file-exists? src)
	     (hop-load-weblet (make-file-name path src))
	     (error "hop" "Cannot find source file" path))))

   (define (load-package pkg)
      (call-with-input-file pkg
	 (lambda (ip)
	    (let* ((obj (javascript->obj ip))
		   (cmain (assq 'main obj)))
	       (when (pair? cmain)
		  (load-command-line-weblet
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
		((not %worker)
		 (load-hop-directory path))
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
		      ;(nodejs-load path path %worker %global %module :commonjs-export #t)
		      (nodejs-load-module path %worker %global %module :commonjs-export #t))))))
	 ((string-suffix? ".mjs" path)
	  ;; javascript
	  (when %worker
	     (with-access::WorkerHopThread %worker (%this prerun)
		(js-worker-push! %worker (format "nodejs-load(~a)" path)
		   (lambda (%this)
		      ;(nodejs-load path path %worker %global %module :commonjs-export #f)
		      (nodejs-load-module path %worker %global %module :commonjs-export #f))))))
	 ((string-suffix? ".ts" path)
	  ;; typescript
	  (when %worker
	     (with-access::WorkerHopThread %worker (%this prerun)
		(js-worker-push! %worker (format "nodejs-load(~a)" path)
		   (lambda (%this)
		      ;(nodejs-load path path %worker  %global %module :lang "ts" :commonjs-export #t)
		      (nodejs-load-module path %worker %global %module :lang "ts" :commonjs-export #t))))))
	 ((string=? (basename path) "package.json")
	  (load-package path))
	 (else
	  ;; this is a plain file
	  (unless %worker
	     (hop-load-weblet path))))))

;*---------------------------------------------------------------------*/
;*    hop-repl ...                                                     */
;*---------------------------------------------------------------------*/
(define (hop-repl scd)
   (cond
      ((<=fx (hop-max-threads) 1)
       (error "hop-repl"
	  "not enough threads to start a REPL (see --threads-max option)"
	  (hop-max-threads)))
      ((isa? scd scheduler)
       (with-access::scheduler scd (size)
	  (if (<=fx size 1)
	      (error "hop-repl"
		 "HOP REPL cannot be spawned without multi-threading"
		 scd)
	      (spawn0 scd (stage-repl repl)))))
      (else
       (thread-start!
	  (instantiate::hopthread
	     (body (lambda ()
		      (stage-repl repl))))))))

;*---------------------------------------------------------------------*/
;*    hopscript-repl ...                                               */
;*---------------------------------------------------------------------*/
(define (hopscript-repl scd %global %worker)
   (cond
      ((<=fx (hop-max-threads) 1)
       (error "hop-repl"
	  "not enough threads to start a REPL (see --threads-max option)"
	  (hop-max-threads)))
      ((isa? scd scheduler)
       (with-access::scheduler scd (size)
	  (if (<=fx size 1)
	      (error "hop-repl"
		 "HOP REPL cannot be spawned without multi-threading"
		 scd)
	      (multiple-value-bind (%worker %global %module)
		 (js-main-worker! "repl" (pwd) #f
		    nodejs-new-global-object nodejs-new-module)
		 (js-worker-exec %worker "repl"
		    (lambda (%this)
		       (repljs %global %worker)))))))
      (else
       (repljs %global %worker))))

;*---------------------------------------------------------------------*/
;*    stage-repl ...                                                   */
;*---------------------------------------------------------------------*/
(define (stage-repl repl)
   (lambda (scd thread)
      (debug-thread-info-set! thread "stage-repl")
      (hop-verb 1 "Entering repl...\n")
      (begin (repl) (exit 0))))

;*---------------------------------------------------------------------*/
;*    hop-acknowledge ...                                              */
;*---------------------------------------------------------------------*/
(define (hop-acknowledge)

   (define (parse-host host)
      (cond
	 ((pregexp-match "([^:]+):([0-9]+)" host)
	  =>
	  (lambda (m)
	     (values (cadr m) (string->integer (caddr m)))))
	 ((pregexp-match "[0-9]+" host)
	  =>
	  (lambda (m)
	     (values "127.0.01" (string->integer host))))
	 (else
	  (values host (+fx (hop-port) 1)))))

   (when (hop-acknowledge-host)
      (multiple-value-bind (host port)
	 (parse-host (hop-acknowledge-host))
	 (with-handler
	    (lambda (e)
	       (exception-notify e)
	       #f)
	    (let* ((sock (make-client-socket host port :timeout 2000))
		   (port (socket-output sock)))
	       (hop-verb 1 (hop-color 4 0 " ACK ") (hop-acknowledge-host) "\n")
	       (display "hop" port)
	       (flush-output-port port)
	       (socket-close sock))))))
		     
