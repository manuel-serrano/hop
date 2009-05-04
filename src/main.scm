;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/src/main.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Mon May  4 18:05:21 2009 (serrano)                */
;*    Copyright   :  2004-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP entry point                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module main

   (cond-expand
      (enable-threads (library pthread)))

   (cond-expand
      (enable-ssl (library ssl)))

   (library multimedia web hop hopscheme scheme2js)

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

   (main    main))


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
	      (dump-trace-stack (current-error-port) 10)
	      (exit 2))))

;*---------------------------------------------------------------------*/
;*    hop-server-socket ...                                            */
;*    -------------------------------------------------------------    */
;*    Create the Hop server socket according to user options.          */
;*---------------------------------------------------------------------*/
(define (hop-server-socket)
   (if (hop-enable-https)
       (cond-expand
	  (enable-ssl
	   (let ((cert (read-certificate "/etc/ssl/certs/hop.pem"))
		 (pkey (read-private-key "/etc/ssl/private/hop.pem")))
	      (make-ssl-server-socket (hop-port)
				      :protocol (hop-https-protocol)
				      :cert cert :pkey pkey)))
	  (else
	   (error 'hop "SSL not supported by this version of Hop" #f)))
       (make-server-socket (hop-port) :backlog (hop-somaxconn))))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   ;; set the hop process owner
   (set-hop-owner! (hop-user))
   ;; catch critical signals
   (signal-init!)
   ;; set the Hop cond-expand identification
   (for-each register-eval-srfi! (hop-srfis))
   ;; set the library load path
   (bigloo-library-path-set! (hop-library-path))
   ;; preload the hop libraries
   (for-each (lambda (l)
		(eval `(library-load ',l)))
	     (hop-preload-libraries))
   ;; setup the hop readers
   (bigloo-load-reader-set! hop-read)
   (bigloo-load-module-set! (lambda (f) (hop-load-modified f :abase #f)))
   (bigloo-module-extension-handler-set! hop-module-extension-handler)
   (bigloo-module-resolver-set! (make-hop-module-resolver (bigloo-module-resolver)))
   ;; clear the module cache unless we preserve
   ;; caches from one session to another
   (unless (hop-restore-disk-cache)
      (let ((c (make-file-path (hop-cache-directory) (hop-api-cache))))
	 (when (directory? c)
	    (delete-path c)
	    (make-directory c))))
   ;; parse the command line
   (parse-args args)
   (hop-verb 1 "Starting hop (v" (hop-version)
	     ", " (hop-backend)
	     (cond-expand
		(enable-threads
		 (format ", ~a scheduler" (hop-scheduling)))
		(else
		 ", single-threaded"))
	     ") "
	     (if (hop-enable-https)
		 (format "https (~a):" (hop-https-protocol)) "http:")
	     (hop-port)
	     (if (hop-enable-fast-server-event)
		 (format ", server-events:~a" (hop-fast-server-event-port))
		 "")
	     "\n")
   ;; install the builtin filters
   (hop-filter-add! service-filter)
   ;; prepare the regular http handling
   (init-http!)
   (when (hop-enable-webdav) (init-webdav!))
   (when (hop-enable-fast-server-event) (init-flash!))
   ;; close filters and users registration before starting
   (hop-filters-close!)
   (users-close!)
   ;; create the scheduler
   (unless (scheduler? (hop-scheduler))
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
	      (error 'hop "Unknown scheduling policy" (hop-scheduling)))))
	 (else
	  (unless (eq? (hop-scheduling) 'nothread)
	     (warning 'hop "Threads disabled, forcing \"nothread\" scheduler."))
	  (hop-scheduler-set! (instantiate::nothread-scheduler)))))
   ;; start the hop scheduler loop (i.e. the hop main loop)
   (with-handler
      (lambda (e)
	 (exception-notify e)
	 (with-output-to-port (current-error-port)
	    (lambda ()
	       (print "An error has occurred in the Hop main loop, exiting...")
	       (newline)))
	 (exit 1))
      (let ((serv (hop-server-socket)))
	 ;; tune the server socket
	 (socket-option-set! serv :TCP_NODELAY #t)
	 ;; start the job (background taks, a la cron) scheduler
	 (when (>fx (hop-max-threads) 1)
	    (job-start-scheduler!))
	 ;; when needed, start the HOP repl
	 (when (and (hop-enable-repl) (>fx (hop-max-threads) 1))
	    (hop-repl (hop-scheduler)))
	 ;; when needed, start a loop for server events
	 (hop-event-server (hop-scheduler))
	 ;; execute the script file
	 (when (string? (hop-script-file))
	    (if (file-exists? (hop-script-file))
		(hop-load (hop-script-file))
		(hop-load-rc (hop-script-file))))
	 ;; preload all the forced services
	 (for-each (lambda (svc)
		      (let* ((path (string-append (hop-service-base) "/" svc))
			     (req (instantiate::http-request
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
			    (autoload-filter req))))
		   (hop-preload-services))
	 ;; start the main loop
	 (scheduler-accept-loop (hop-scheduler) serv #t))))

;*---------------------------------------------------------------------*/
;*    set-hop-owner! ...                                               */
;*---------------------------------------------------------------------*/
(define (set-hop-owner! user)
   
   (define (err)
      (error 'hop
	     "Hop is not allowed to be executed as `root'. Create a dedicated Hop user to run Hop on behalf of.\n"
	     "If you know what you are doing and want to run Hop with the
`root' permissions, edit the Hop configuration file and set the appropriate `hop-user' value."))

   (cond
      ((not (=fx (getuid) 0))
       #unspecified)
      ((not (pair? (getpwnam "root")))
       #unspecified)
      ((eq? user 'root)
       #unspecified)
      ((string? user)
       (let ((pw (getpwnam user)))
	  (if (pair? pw)
	      (setuid (caddr pw))
	      (error 'set-hop-owner! "Cannot find HOP system user" user))))
      (else
       (err))))

;*---------------------------------------------------------------------*/
;*    hop-repl ...                                                     */
;*---------------------------------------------------------------------*/
(define (hop-repl scd)
   (if (<=fx (scheduler-size scd) 1)
       (error 'hop-repl
	      "HOP REPL cannot be spawned without multi-threading"
	      scd)
       (spawn0 scd stage-repl)))

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
   (cond
      ((not (hop-enable-fast-server-event))
       ;; fast event are disabled
       (hop-event-init! #f))
      ((=fx (hop-fast-server-event-port) (hop-port))
       ;; will use the regular HOP port
       (hop-event-init! (hop-port)))
      ((<=fx (scheduler-size scd) 1)
       ;; disable fast event because no thread is available
       ;; and extra port is needed
       (hop-event-init! #f))
      (else
       ;; run in a separate thread
       (hop-event-init! (hop-fast-server-event-port))
       (let ((serv (make-server-socket (hop-fast-server-event-port))))
	  (scheduler-accept-loop scd serv #f)))))
