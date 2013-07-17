;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/src/main.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Wed Jul 17 10:23:00 2013 (serrano)                */
;*    Copyright   :  2004-13 Manuel Serrano                            */
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
      (hop-library (extern (export main "hop_main"))
		   (export (main x)))
      (boot-from-java
         ; java name is bigloo.hop.main.main (args)
         (export (main::int args::pair-nil)))
      (else (main main))))


;*---------------------------------------------------------------------*/
;*    signal-init! ...                                                 */
;*---------------------------------------------------------------------*/
(define (signal-init!)
   (cond-expand
      (enable-threads #unspecified)
      (else (signal sigpipe (lambda (n) #unspecified))))
   (signal sigsegv
	   (lambda (n)
	      (fprint (current-error-port) "Segmentation violation")
	      (display-trace-stack (get-trace-stack) (current-error-port))
	      (exit 2))))

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
      (lambda (f)
	 (hop-load-modified f :abase #t :afile #f)))
   ;; setup the hop module resolvers
   (bigloo-module-extension-handler-set! hop-module-extension-handler)
   (bigloo-module-resolver-set! (make-hop-module-resolver (bigloo-module-resolver)))
   ;; parse the command line
   (let ((files (parse-args args)))
      ;; install the builtin filters
      (hop-filter-add! service-filter)
      ;; prepare the regular http handling
      (init-http!)
      (when (hop-enable-webdav) (init-webdav!))
      (when (hop-enable-fast-server-event) (init-flash!))
      ;; start zeroconf
      (when (hop-enable-zeroconf) (init-zeroconf!))
      ;; close filters and users registration before starting
      (hop-filters-close!)
      (users-close!)
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
	    ;; start the job (background taks, a la cron) scheduler
	    (when (hop-enable-jobs)
	       (job-start-scheduler!))
	    ;; when needed, start the HOP repl
	    (when (hop-enable-repl)
	       (hop-repl (hop-scheduler)))
	    ;; when needed, start a loop for server events
	    (hop-event-server (hop-scheduler))
	    ;; execute the script file
	    (when (string? (hop-script-file))
	       (if (file-exists? (hop-script-file))
		   (hop-load (hop-script-file))
		   (hop-load-rc (hop-script-file))))
	    ;; preload the files of the command lines
	    (when (pair? files)
	       (let ((req (instantiate::http-server-request
			     (host "localhost")
			     (port (hop-port))
			     (user (anonymous-user)))))
		  ;; set a dummy request
		  (thread-request-set! #unspecified req)
		  ;; preload the user files
		  (for-each load-command-line-weblet files)
		  ;; unset the dummy request
		  (thread-request-set! #unspecified #unspecified)))
	    ;; preload all the forced services
	    (for-each (lambda (svc)
			 (let* ((path (string-append (hop-service-base) "/" svc))
				(req (instantiate::http-server-request
					(user (anonymous-user))
					(localclientp #t)
					(path path)
					(abspath path)
					(port (hop-port))
					(connection 'close))))
			    (with-handler
			       (lambda (err)
				  (exception-notify err)
				  (fprintf (current-error-port)
				     "*** WARNING: Service \"~a\" cannot be pre-loaded.\n" svc))
			       (current-request-set! #f req)
			       (service-filter req))))
	       (hop-preload-services))
	    (current-request-set! #f #f)
	    ;; start the main loop
	    (scheduler-accept-loop (hop-scheduler) serv #t)))))

;*---------------------------------------------------------------------*/
;*    set-scheduler! ...                                               */
;*---------------------------------------------------------------------*/
(define (set-scheduler!)
   (cond-expand
      (enable-threads
	 (case (hop-scheduling)
	    ((nothread)
	     (hop-scheduler-set! (instantiate::nothread-scheduler)))
	    ((queue)
	     (hop-scheduler-set! (instantiate::queue-scheduler
				    (size (hop-max-threads)))))
	    ((one-to-one)
	     (hop-scheduler-set! (instantiate::one-to-one-scheduler
				    (size (hop-max-threads)))))
	    ((pool)
	     (hop-scheduler-set! (instantiate::pool-scheduler
				    (size (hop-max-threads)))))
	    ((accept-many)
	     (hop-scheduler-set! (instantiate::accept-many-scheduler
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
(define (load-command-line-weblet f)
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
	  (let ((src (string-append (basename path) ".hop")))
	     (hop-load-weblet (make-file-name path src))))
	 (else
	  ;; this is a plain file
	  (hop-load-weblet path)))))

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
	      (spawn0 scd stage-repl)))
       (error "hop-repl" "not enought threads to start a REPL (see --threads-max option)" (hop-max-threads))))

;*---------------------------------------------------------------------*/
;*    stage-repl ...                                                   */
;*---------------------------------------------------------------------*/
(define (stage-repl scd thread)
   (debug-thread-info-set! thread "stage-repl")
   (hop-verb 1 "Entering repl...\n")
   (begin (repl) (exit 0)))

;*---------------------------------------------------------------------*/
;*    hop-event-server ...                                             */
;*---------------------------------------------------------------------*/
(define (hop-event-server scd)
   (hop-event-init!)
   (when (and (hop-enable-fast-server-event)
	      (not (=fx (hop-fast-server-event-port) (hop-port)))
	      (>fx (with-access::scheduler scd (size) size) 1))
      ;; run an event server socket in a separate thread
      (let ((serv (make-server-socket (hop-fast-server-event-port))))
	 (scheduler-accept-loop scd serv #f))))
