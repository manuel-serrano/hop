;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/src/main.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Thu Aug 28 07:01:27 2008 (serrano)                */
;*    Copyright   :  2004-08 Manuel Serrano                            */
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
	    hop_scheduler
	    hop_scheduler-nothread
	    hop_scheduler-queue
	    hop_scheduler-one-to-one
	    hop_scheduler-pool
	    hop_scheduler-cohort)

   (main    main))

;*---------------------------------------------------------------------*/
;*    hop-verb ...                                                     */
;*---------------------------------------------------------------------*/
(define-expander hop-verb
   (lambda (x e)
      (match-case x
	 ((?- (and (? integer?) ?level) . ?rest)
	  (let ((v (gensym)))
	     `(let ((,v ,(e level e)))
		 (if (>=fx (hop-verbose) ,v)
		     (with-lock *verb-mutex*
			(lambda ()
			   (hop-verb ,v ,@(map (lambda (x) (e x e)) rest))))))))
	 (else
	  `(with-lock *verb-mutex*
	      (lambda ()
		 (hop-verb ,@(map (lambda (x) (e x e)) (cdr x)))))))))

;*---------------------------------------------------------------------*/
;*    *verb-mutex* ...                                                 */
;*---------------------------------------------------------------------*/
(define *verb-mutex* (make-mutex 'hop-verb))

;*---------------------------------------------------------------------*/
;*    *socket-mutex* ...                                               */
;*---------------------------------------------------------------------*/
(define *socket-mutex* (make-mutex 'hop-sock))

;*---------------------------------------------------------------------*/
;*    *keep-alive* ...                                                 */
;*---------------------------------------------------------------------*/
(define *keep-alive* 0)

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
   ;; catch critical signals
   (signal-init!)
   ;; set the Hop cond-expand identification
   (register-eval-srfi! 'hop)
   (register-eval-srfi! (string->symbol (format "hop-~a" (hop-version))))
   ;; set the library load path
   (let ((hop-path (make-file-path (hop-lib-directory) "hop" (hop-version))))
      (bigloo-library-path-set! (cons hop-path (bigloo-library-path))))
   ;; preload the hop libraries
   (for-each (lambda (l)
		(eval `(library-load ',l)))
	     (hop-preload-libraries))
   ;; setup the hop readers
   (bigloo-load-reader-set! hop-read)
   (bigloo-load-module-set! hop-load-modified)
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
   ;; init hss, scm compilers, and services
   (init-hss-compiler!)
   (init-scm-compiler! compile-scheme-file)
   (init-hop-services!)
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
	     ((cohort)
	      (hop-scheduler-set! (instantiate::cohort-scheduler
				     (size (hop-max-threads))
				     (cohort 4))))
	     ((queue)
	      (hop-scheduler-set! (instantiate::queue-scheduler
				     (size (hop-max-threads)))))
	     ((one-to-one)
	      (hop-scheduler-set! (instantiate::one-to-one-scheduler
				     (size (hop-max-threads)))))
	     ((pool)
	      (hop-scheduler-set! (instantiate::pool-scheduler
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
	 ;; start the main loop
	 (hop-main-loop (hop-scheduler) serv))))

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
;*    debug-thread-info-set! ...                                       */
;*---------------------------------------------------------------------*/
(define-macro (debug-thread-info-set! thread info)
   `(when (>fx (bigloo-debug) 0)
       (thread-info-set! ,thread ,info)))

;*---------------------------------------------------------------------*/
;*    with-stage-handler ...                                           */
;*---------------------------------------------------------------------*/
(define-macro (with-stage-handler handler . body)
   `(begin
       (hopthread-onerror-set! thread ,handler)
       ,@body))

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
	  (hop-main-loop scd serv)))))

;*---------------------------------------------------------------------*/
;*    hop-main-loop ...                                                */
;*---------------------------------------------------------------------*/
(define (hop-main-loop scd serv)
   (let ((dummy-buffer (make-string 512)))
      (let loop ((id 1))
	 (let ((sock (socket-accept serv :buffer dummy-buffer)))
	    (hop-verb 2 (hop-color id id " ACCEPT")
		      ": " (socket-hostname sock) " [" (current-date) "]\n")
	    (spawn4 scd stage-request id sock 'connect (hop-read-timeout))
	    (loop (+fx id 1))))))

;*---------------------------------------------------------------------*/
;*    keep-alive ...                                                   */
;*---------------------------------------------------------------------*/
(define (keep-alive)
   (let ((v 0))
      (mutex-lock! *socket-mutex*)
      (set! v *keep-alive*)
      (mutex-unlock! *socket-mutex*)
      v))
   
;*---------------------------------------------------------------------*/
;*    keep-alive-- ...                                                 */
;*---------------------------------------------------------------------*/
(define (keep-alive--)
   (mutex-lock! *socket-mutex*)
   (set! *keep-alive* (-fx *keep-alive* 1))
   (mutex-unlock! *socket-mutex*))

;*---------------------------------------------------------------------*/
;*    keep-alive++ ...                                                 */
;*---------------------------------------------------------------------*/
(define (keep-alive++)
   (mutex-lock! *socket-mutex*)
   (set! *keep-alive* (+fx 1 *keep-alive*))
   (mutex-unlock! *socket-mutex*))
   
;*---------------------------------------------------------------------*/
;*    with-time ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (with-time expr id msg)
   (let ((v (gensym)))
      `(if (hop-report-execution-time)
	   (multiple-value-bind (value real sys user)
	      (time (lambda () ,expr))
	      (hop-verb 1
			(hop-color ,id ,id (format " ~a.time " ,msg))
			" real: " real
			" sys: " sys
			" user: " user
			"\n")
	      value)
	   ,expr)))

;*---------------------------------------------------------------------*/
;*    stage-request ...                                                */
;*    -------------------------------------------------------------    */
;*    This stage is in charge of parsing the request. It produces a    */
;*    request.                                                         */
;*---------------------------------------------------------------------*/
(define (stage-request scd thread id sock mode timeout)
   
   ;; is the error raised of a timeout in a keep-alive connection?
   (define (keep-alive-ellapsed-error? e)
      (and (eq? mode 'keep-alive)
	   (or (&io-timeout-error? e)
	       (and (&io-parse-error? e)
		    (eof-object? (&io-parse-error-obj e))))))

   ;; error handler specific to that the stage
   (define (connect-error-handler e)
      (if (keep-alive-ellapsed-error? e)
	  ;; this is not a true error, just log
	  (hop-verb 3 (hop-color id id " SHUTTING DOWN")
		    (cond
		       ((&io-timeout-error? e)
			" (keep-alive, timeout ellapsed)")
		       ((and (&io-parse-error? e)
			     (eof-object? (&io-parse-error-obj e)))
			" (keep-alive, connection reset by peer)")
		       (else
			" (keep-alive, parse error)"))
		    "\n")
	  ;; this one is a true error
	  (begin
	     (when (&exception? e)
		(hop-verb 1 (hop-color id id " ABORTING: ")
			  " " (trace-color 1 (find-runtime-type e))
			  "\n")
		(exception-notify e))
	     (when (and (&io-unknown-host-error? e) (not (socket-down? sock)))
		(with-stage-handler
		   (lambda (e)
		      ;; this error handler is invoked when the attempt to
		      ;; notify the previous error to the client fails
		      (when (&error? e) (error-notify e))
		      #unspecified)
		   ;; we will try to answer the error to the client
		   (unless (&io-sigpipe-error? e)
		      (let ((resp ((or (hop-http-request-error)
				       http-request-error)
				   e)))
			 (http-response resp sock)))))))
      ;; decrement the keep-alive number
      (when (eq? mode 'keep-alive) (keep-alive--))
      ;; abort this request
      (socket-close sock))

   ;; verbose function (only for log and debug)
   (define (http-connect-verb scd id sock req)
      (when (eq? mode 'keep-alive)
	 (hop-verb 3 (hop-color id id " KEEP-ALIVE"))
	 (hop-verb 3 (scheduler-stat scd))
	 (hop-verb 3 ": " (socket-hostname sock) " [" (current-date) "]\n"))
      (with-access::http-request req (method scheme host port path proxyp user
					     header)
	 (hop-verb 4 (hop-color id id " CONNECT.header") ": "
		   (with-output-to-string (lambda () (write header))) "\n")
	 (hop-verb 2 (if proxyp
			 (hop-color req req " EXEC.prox")
			 (hop-color req req " EXEC.serv"))
		   (scheduler-stat scd)
		   ": " method " " scheme "://"
		   (user-name user) "@" host ":" port (string-for-read path)
		   "\n")))

   ;; log
   (hop-verb 1 (hop-color id id " CONNECT")
	     (if (eq? mode 'keep-alive) "+" "")
	     (if (>=fx (hop-verbose) 2) (scheduler-stat scd) "")
	     (if (>=fx (hop-verbose) 3) (format " ~a" thread) "")
	     ": " (socket-hostname sock) " [" (current-date) "]\n")

   ;; debug trace
   (debug-thread-info-set! thread "connection established with ~a")

   ;; switch to the thread-specific buffer
   (input-port-buffer-set! (socket-input sock) (hopthread-inbuf thread))
   
   (with-stage-handler
      connect-error-handler
      (let ((req (with-time (http-parse-request sock id timeout) id "CONNECT")))
	 ;; debug info
	 (debug-thread-info-set! thread
				 (format "request parsed for ~a, ~a ~a"
					 (socket-hostname sock)
					 (http-request-method req)
					 (http-request-path req)))
	 (http-connect-verb scd id sock req)
	 ;; decrement the keep-alive number (we have a valid connection)
	 (when (eq? mode 'keep-alive) (keep-alive--))
	 ;; start compting the answer
	 (stage2 scd stage-response id req))))

;*---------------------------------------------------------------------*/
;*    response-error-handler ...                                       */
;*---------------------------------------------------------------------*/
(define (response-error-handler e scd req)
   (with-handler
      (lambda (e)
	 ;; there is nothing we can do but aborting the request
	 (socket-close (http-request-socket req))
	 (raise #f))
      (begin
	 ;; when the error is a response, we transmit it to the next stage
	 (cond
	    ((&io-sigpipe-error? e)
	     (response-sigpipe-error-handler e scd req))
	    ((&exception? e)
	     (response-exception-error-handler e scd req))
	    (else
	     (raise e))))))

;*---------------------------------------------------------------------*/
;*    response-sigpipe-error-handler ...                               */
;*---------------------------------------------------------------------*/
(define (response-sigpipe-error-handler e scd req)
   ;; signal the error
   (hop-verb 2 (hop-color req req " INTERRUPTED"))
   (hop-verb 2 ": " (&error-obj e) "\n")
   ;; there is nothing we can do but aborting the request
   (socket-close (http-request-socket req))
   ;; abort the request
   (raise #f))

;*---------------------------------------------------------------------*/
;*    response-exception-error-handler ...                             */
;*---------------------------------------------------------------------*/
(define (response-exception-error-handler e scd req)
   (begin
      (hop-verb 1 (hop-color req req " ERROR"))
      (hop-verb 2 (scheduler-stat scd))
      (hop-verb 1 ": " (trace-color 1 e) "\n")
      (if (%http-response? e)
	  e
	  (begin
	     (cond
		((&error? e)
		 (error-notify (evmeaning-annotate-exception! e)))
		((&warning? e)
		 (warning-notify (evmeaning-annotate-exception! e))))
	     ;; generate a legal response for the next stage (although
	     ;; this response denotes the error).
	     (let ((resp ((or (hop-http-response-error) http-error) e req))
		   (sock (http-request-socket req)))
		(http-response resp sock)
		;; abort this request
		(socket-close sock)
		'close)))))

;*---------------------------------------------------------------------*/
;*    http-response-static? ...                                        */
;*---------------------------------------------------------------------*/
(define (http-response-static? resp)
   (or (http-response-abort? resp)
       (http-response-string? resp)
       (http-response-file? resp)
       (http-response-error? resp)))

;*---------------------------------------------------------------------*/
;*    stage-response ...                                               */
;*    -------------------------------------------------------------    */
;*    This stage is in charge of building a response to the received   */
;*    request.                                                         */
;*---------------------------------------------------------------------*/
(define (stage-response scd thread id req)
   (current-request-set! req)
   (hop-verb 3 (hop-color id id " RESPONSE") "\n")
   (with-stage-handler
      (lambda (e) (response-error-handler e scd req))
      (let ((resp (with-time (request->response req) id "RESPONSE")))
	 (debug-thread-info-set! thread
				 (format "~a ~a://~a:~a~a... -> ~a"
					 (http-request-method req)
					 (http-request-scheme req)
					 (http-request-host req)
					 (http-request-port req)
					 (http-request-path req)
					 (find-runtime-type resp)))
	 (let ((proc (if (http-response-static? resp)
			 stage-static-answer
			 stage-dynamic-answer)))
	    (stage3 scd proc id req resp)))))

;*---------------------------------------------------------------------*/
;*    stage-static-answer ...                                          */
;*---------------------------------------------------------------------*/
(define (stage-static-answer scd thread id req resp)
   (stage-answer scd thread id req resp))

;*---------------------------------------------------------------------*/
;*    stage-dynamic-answer ...                                         */
;*---------------------------------------------------------------------*/
(define (stage-dynamic-answer scd thread id req resp)
   (stage-answer scd thread id req resp))
   
;*---------------------------------------------------------------------*/
;*    stage-answer ...                                                 */
;*---------------------------------------------------------------------*/
(define (stage-answer scd thread id req resp)
   (current-request-set! req)
   ;; log4
   (hop-verb 4 (hop-color req req " EXEC")
	     (scheduler-stat scd)
	     ": " (find-runtime-type resp)
	     " " (user-name (http-request-user req)) "\n")
   
   (with-stage-handler
      (lambda (e) (response-error-handler e scd req))
      (let* ((sock (http-request-socket req))
	     (connection (with-time (http-response resp sock) id "EXEC")))
	 ;; debug
	 (debug-thread-info-set! thread
				 (format "~a ~a://~a:~a~a... -> ~a ~a"
					 (http-request-method req)
					 (http-request-scheme req)
					 (http-request-host req)
					 (http-request-port req)
					 (http-request-path req)
					 (find-runtime-type resp)
					 connection))
	 ;; log2
	 (hop-verb 3 (hop-color req req " END")
		   (scheduler-stat scd)
		   ": " (find-runtime-type resp) " " connection
		   " [" (current-date) "] "
		   (if (and (eq? connection 'keep-alive) (>=fx (hop-verbose) 4))
		       (format " keep-alive [open=~a/~a]"
			       (keep-alive)
			       (hop-keep-alive-threshold))
		       connection)
		   "\n")
	 
	 (case connection
	    ((persistent)
	     #unspecified)
	    ((keep-alive)
	     (if (and (hop-enable-keep-alive)
		      (<fx (scheduler-load scd) 50)
		      (<fx (keep-alive) (hop-keep-alive-threshold)))
		 (begin
		    (keep-alive++)
		    (stage4 scd stage-request
			    id sock 'keep-alive (hop-keep-alive-timeout)))
		 (socket-close sock)))
	    (else
	     (socket-close sock))))))
      
	       
   
