;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/src/main.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Sun Feb 14 14:15:49 2016 (serrano)                */
;*    Copyright   :  2004-16 Manuel Serrano                            */
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
	    ((hop-sigterm-handler) n))))
   (when (<fx (bigloo-debug) 3)
      (signal sigsegv
	 (lambda (n)
	    (fprint (current-error-port) "Segmentation violation")
	    (display-trace-stack (get-trace-stack) (current-error-port))
	    (exit 2)))))

;*---------------------------------------------------------------------*/
;*    js initialization ...                                            */
;*---------------------------------------------------------------------*/
(define jsmutex (make-mutex))
(define jscondv (make-condition-variable))
(define jsinit #f)

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
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
   (let ((jsworker #f))
      ;; parse the command line
      (multiple-value-bind (files exprs exprsjs)
	 (parse-args args)
	 ;; extent the require search path to the Hop autoload directories
	 (nodejs-resolve-extend-path! (hop-autoload-directories))
	 ;; install the builtin filters
	 (hop-filter-add! service-filter)
	 (hop-init args files exprs)
	 ;; js rc load
	 (if (hop-javascript)
	     (set! jsworker (javascript-init args files exprsjs))
	     (users-close!))
	 ;; when debugging, init the debugger runtime
	 (hop-debug-init! (hop-client-output-port))
	 ;; prepare the regular http handling
	 (init-http!)
	 (when (hop-enable-webdav) (init-webdav!))
	 (when (hop-enable-fast-server-event) (init-flash!))
	 ;; close filter installation
	 (hop-filters-close!)
	 ;; https file handling
	 (cond-expand
	    (enable-ssl
	     (input-port-protocol-set! "https://" open-input-https-socket)))
	 ;; start zeroconf
	 (when (hop-enable-zeroconf) (init-zeroconf!))
	 ;; create the scheduler (unless the rc file has already created one)
	 (unless (isa? (hop-scheduler) scheduler)
	    (set-scheduler!))
	 ;; start the hop scheduler loop (i.e. the hop main loop)
	 (with-handler
	    (lambda (e)
	       (exception-notify e)
	       (fprint (current-error-port)
		  "An error has occurred in the Hop main loop, exiting...")
	       (exit 1))
	    (let ((serv (hop-server-socket)))
	       ;; adjust the actual hop-port
	       (hop-port-set! (socket-port-number serv))
	       (hop-fast-server-event-port-set! (socket-port-number serv))
	       ;; ready to now say hello
	       (hello-world)
	       ;; tune the server socket
	       (socket-option-set! serv :TCP_NODELAY #t)
	       ;; start the job (a la cron background tasks) scheduler
	       (when (hop-enable-jobs)
		  (job-start-scheduler!))
	       ;; when needed, start the HOP repl
	       (when (eq? (hop-enable-repl) 'scm)
		  (hop-repl (hop-scheduler)))
	       ;; when needed, start a loop for server events
	       (hop-event-server (hop-scheduler))
	       (when (hop-run-server)
		  ;; preload all the forced services
		  (for-each (lambda (svc)
			       (let* ((path (string-append (hop-service-base)
					       "/" svc))
				      (req (instantiate::http-server-request
					      (path path)
					      (abspath path)
					      (port (hop-port))
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
		  (when jsworker
		     (synchronize jsmutex
			(unless jsinit
			   (condition-variable-wait! jscondv jsmutex))))
		  ;; start the main loop
		  (scheduler-accept-loop (hop-scheduler) serv #t))
	       (if jsworker
		   (if (thread-join! jsworker) 0 1)
		   0))))))

;*---------------------------------------------------------------------*/
;*    hop-init ...                                                     */
;*---------------------------------------------------------------------*/
(define (hop-init args files exprs)
   ;; preload the command-line files
   (when (pair? files)
      (let ((req (instantiate::http-server-request
		    (host "localhost")
		    (port (hop-port)))))
	 ;; set a dummy request
	 (thread-request-set! #unspecified req)
	 ;; preload the user files
	 (for-each (lambda (f) (load-command-line-weblet f #f)) files)
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
(define (javascript-init args files exprs)
   (let* ((%global (nodejs-new-global-object))
	  (%worker (js-init-main-worker! %global
		      ;; keep-alive
		      (or (hop-run-server) (eq? (hop-enable-repl) 'js))
		      nodejs-new-global-object)))
      ;; js loader
      (hop-loader-add! "js" (lambda (path . test) (nodejs-load path %worker)))
      ;; rc.js file
      (when (hop-rc-loaded?) (javascript-rc %worker %global))
      ;; hss extension
      (when (hop-javascript) (javascript-init-hss %worker %global))
      ;; create the repl JS module
      (let ((path (file-name-canonicalize!
		     (make-file-name (pwd) (car args)))))
	 (nodejs-module "repl" path %worker %global))
      ;; push the user expressions
      (when (pair? exprs)
	 (js-worker-push-thunk! %worker "cmdline"
	    (lambda ()
	       (for-each (lambda (expr)
			    (call-with-input-string (string-append expr "\n")
			       (lambda (ip)
				  (%js-eval ip 'eval %global
				     (js-undefined) %global))))
		  exprs))))
      ;; close user registration
      (js-worker-push-thunk! %worker "cmdline"
	 (lambda ()
	    (users-close!)
	    (synchronize jsmutex
	       (set! jsinit #t)
	       (condition-variable-signal! jscondv))))
      ;; preload the command-line files
      (when (pair? files)
	 (let ((req (instantiate::http-server-request
		       (host "localhost")
		       (port (hop-port)))))
	    ;; set a dummy request
	    (thread-request-set! #unspecified req)
	    ;; preload the user files
	    (for-each (lambda (f) (load-command-line-weblet f %worker)) files)
	    ;; unset the dummy request
	    (thread-request-set! #unspecified #unspecified)))
      ;; start the javascript loop
      (hop-hopscript-worker (hop-scheduler) %global %worker)
      ;; start the JS repl loop
      (when (eq? (hop-enable-repl) 'js)
	 (hopscript-repl (hop-scheduler) %global %worker))
      ;; return the worker for the main loop to join
      %worker))

;*---------------------------------------------------------------------*/
;*    javascript-rc ...                                                */
;*    -------------------------------------------------------------    */
;*    Load the hoprc.js in a sandboxed environment.                    */
;*---------------------------------------------------------------------*/
(define (javascript-rc %worker %global)
   
   (define (load-rc path)
      ;; set the preferred language
      (hop-preferred-language-set! "hopscript")
      ;; force the module initialization
      (js-worker-push-thunk! %worker "hss"
	 (lambda ()
	    (let ((path (if (and (>fx (string-length path) 0)
				 (char=? (string-ref path 0) (file-separator)))
			    path
			    (file-name-canonicalize!
			       (make-file-name (pwd) path)))))
	       (nodejs-load path %worker)))))

   (let ((path (string-append (prefix (hop-rc-loaded)) ".js")))
      (if (file-exists? path)
	  (load-rc path)
	  (let ((path (string-append (prefix (hop-rc-file)) ".js")))
	     (when (file-exists? path)
		(load-rc path))))))

;*---------------------------------------------------------------------*/
;*    javascript-init-hss ...                                          */
;*---------------------------------------------------------------------*/
(define (javascript-init-hss %worker %global)
   (let ((mod (nodejs-module "hss" "hss" %worker %global))
	 (scope (nodejs-new-scope-object %global))
	 (exp (call-with-input-string "false"
		 (lambda (in)
		    (j2s-compile in :driver (j2s-plain-driver)
		       :parser 'repl
		       :filename "repl.js")))))
      ;; force the module initialization
      (js-worker-push-thunk! %worker "hss"
	 (lambda ()
	    ((eval! exp) %global %global scope mod)
	    (hop-hss-foreign-eval-set!
	       (lambda (ip)
		  (js-put! mod 'filename
		     (js-string->jsstring (input-port-name ip)) #f
		     %global)
		  (%js-eval-hss ip %global %worker scope)))))))
   
;*---------------------------------------------------------------------*/
;*    set-scheduler! ...                                               */
;*---------------------------------------------------------------------*/
(define (set-scheduler!)
   (cond-expand
      (enable-threads
       (case (hop-scheduling)
	  ((nothread)
	   (hop-scheduler-set!
	      (instantiate::nothread-scheduler)))
	  ((queue)
	   (hop-scheduler-set!
	      (instantiate::queue-scheduler
		 (size (hop-max-threads)))))
	  ((one-to-one)
	   (hop-scheduler-set!
	      (instantiate::one-to-one-scheduler
		 (size (hop-max-threads)))))
	  ((pool)
	   (hop-scheduler-set!
	      (instantiate::pool-scheduler
		 (size (hop-max-threads)))))
	  ((accept-many)
	   (hop-scheduler-set!
	      (instantiate::accept-many-scheduler
		 (size (hop-max-threads)))))
	  (else
	   (error "hop" "Unknown scheduling policy" (hop-scheduling)))))
      (else
       (unless (eq? (hop-scheduling) 'nothread)
	  (warning "hop" "Threads disabled, forcing \"nothread\" scheduler."))
       (hop-scheduler-set! (instantiate::nothread-scheduler)))))

;*---------------------------------------------------------------------*/
;*    load-command-line-weblet ...                                     */
;*---------------------------------------------------------------------*/
(define (load-command-line-weblet f %worker)

   (define (load-hop-directory path)
      (let ((src (string-append (basename path) ".hop")))
	 (when (file-exists? src)
	    (hop-load-weblet (make-file-name path src)))))

   (define (load-js-directory path)
      (let ((src (string-append (basename path) ".js")))
	 (when (file-exists? src)
	    (hop-load-weblet (make-file-name path src)))))

   (define (load-package pkg)
      (call-with-input-file pkg
	 (lambda (ip)
	    (let* ((obj (javascript->obj ip))
		   (cmain (assq 'main obj)))
	       (when (pair? cmain)
		  (load-command-line-weblet
		     (make-file-name (dirname pkg) (cdr cmain)) %worker)
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
	 ((string-suffix? ".js" path)
	  ;; javascript
	  (when %worker
	     (with-access::WorkerHopThread %worker (%this prerun)
		(js-worker-push-thunk! %worker "nodejs-load"
		   (lambda ()
		      (nodejs-load path %worker))))))
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
   (if (>fx (hop-max-threads) 1)
       (with-access::scheduler scd (size)
	  (if (<=fx size 1)
	      (error "hop-repl"
		 "HOP REPL cannot be spawned without multi-threading"
		 scd)
	      (spawn0 scd (stage-repl repl))))
       (error "hop-repl"
	  "not enough threads to start a REPL (see --threads-max option)"
	  (hop-max-threads))))

;*---------------------------------------------------------------------*/
;*    hopscript-repl ...                                               */
;*---------------------------------------------------------------------*/
(define (hopscript-repl scd %global %worker)
   (if (>fx (hop-max-threads) 1)
       (if (isa? scd scheduler)
	   (with-access::scheduler scd (size)
	      (if (<=fx size 1)
		  (error "hop-repl"
		     "HOP REPL cannot be spawned without multi-threading"
		     scd)
		  (spawn0 scd
		     (stage-repl
			(lambda () (repljs %global %worker))))))
	   (repljs %global %worker))
       (error "hop-repl"
	  "not enough threads to start a REPL (see --threads-max option)"
	  (hop-max-threads))))

;*---------------------------------------------------------------------*/
;*    stage-repl ...                                                   */
;*---------------------------------------------------------------------*/
(define (stage-repl repl)
   (lambda (scd thread)
      (debug-thread-info-set! thread "stage-repl")
      (hop-verb 1 "Entering repl...\n")
      (begin (repl) (exit 0))))

;*---------------------------------------------------------------------*/
;*    hop-event-server ...                                             */
;*---------------------------------------------------------------------*/
(define (hop-event-server scd)
   (hop-event-init!)
   (when (and (hop-enable-fast-server-event)
	      (not (=fx (hop-fast-server-event-port) (hop-port)))
	      (>fx (with-access::scheduler scd (size) size) 1))
      ;; run an event server socket in a separate thread
      (let ((serv (make-server-socket (hop-fast-server-event-port)
		     :name (hop-server-listen-addr))))
	 (scheduler-accept-loop scd serv #f))))

;*---------------------------------------------------------------------*/
;*    hop-hopscript-worker ...                                         */
;*---------------------------------------------------------------------*/
(define (hop-hopscript-worker scd %global %worker)
   (if (>fx (hop-max-threads) 2)
       (with-access::WorkerHopThread %worker (mutex condv)
	  (synchronize mutex
	     (thread-start-joinable! %worker)
	     (condition-variable-wait! condv mutex)))
       (error "hop-repl"
	  "not enough threads to start the main worker (see --threads-max option)"
	  (hop-max-threads))))
