;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/src/main.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Tue May  6 19:44:46 2008 (serrano)                */
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
		     (hop-verb ,v ,@(map (lambda (x) (e x e)) rest))))))
	 (else
	  `(hop-verb ,@(map (lambda (x) (e x e)) (cdr x)))))))
 
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
       (make-server-socket (hop-port))))

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
				     (names '(stage-start
					      stage-response
					      stage-answer)))))
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
       (schedule-start scd hop-repl-stage 'stage-start)))

;*---------------------------------------------------------------------*/
;*    hop-repl-stage ...                                               */
;*---------------------------------------------------------------------*/
(define (hop-repl-stage scd thread)
   (thread-info-set! thread "hop-repl")
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
   (let loop ((id 1))
      (let ((sock (socket-accept serv)))
	 (schedule-start scd
			 (lambda (s t)
			    (stage-request s t id
					   sock 'connect (hop-read-timeout)))
			 'stage-start)
	 (loop (+fx id 1)))))

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
		    (if (&io-timeout-error? e)
			" (keep-alive timeout ellapsed)"
			" (parse error)")
		    "\n")
	  ;; this one is a true error
	  (begin
	     (when (&exception? e)
		(hop-verb 1 (hop-color id id " ABORTING: ")
			  " " (trace-color 1 (find-runtime-type e))
			  "\n")
		(exception-notify e))
	     (when (and (&io-unknown-host-error? e) (not (socket-down? sock)))
		(with-handler
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
   (hop-verb 1 (hop-color id id " CONNECT") (if (eq? mode 'keep-alive) "+" ""))
   (hop-verb 2 (scheduler-stat scd))
   (hop-verb 1 ": " (socket-hostname sock) " [" (current-date) "]\n")
   
   ;; debug trace
   (thread-info-set! thread "connection establied with ~a")

   (with-handler
      connect-error-handler
      (let ((req (with-time (http-parse-request sock id timeout) id "CONNECT")))
	 ;; debug info
	 (thread-info-set! thread
			   (format "request parsed for ~a, ~a ~a"
				   (socket-hostname sock)
				   (http-request-method req)
				   (http-request-path req)))
	 (http-connect-verb scd id sock req)
	 (schedule scd
		   (lambda (s t) (stage-response s t id req))
		   'stage-response))))

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
;*    stage-response ...                                               */
;*    -------------------------------------------------------------    */
;*    This stage is in charge of building a response to the received   */
;*    request.                                                         */
;*---------------------------------------------------------------------*/
(define (stage-response scd thread id req)
   (current-request-set! req)
   (hop-verb 3 (hop-color id id " RESPONSE") "\n")
   (with-handler
      (lambda (e) (response-error-handler e scd req))
      (let ((resp (with-time (request->response req) id "RESPONSE")))
	 (thread-info-set! thread
			   (format "~a ~a://~a:~a~a... -> ~a"
				   (http-request-method req)
				   (http-request-scheme req)
				   (http-request-host req)
				   (http-request-port req)
				   (http-request-path req)
				   (find-runtime-type resp)))
	 (schedule scd
		   (lambda (s t) (stage-answer s t id req resp))
		   'stage-answer))))

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
   
   (with-handler
      (lambda (e) (response-error-handler e scd req))
      (let* ((sock (http-request-socket req))
	     (connection (with-time (http-response resp sock) id "EXEC")))
	 ;; debug
	 (thread-info-set! thread
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
		   " [" (current-date) "] " connection "\n")
	 (case connection
	    ((persistent)
	     #unspecified)
	    ((keep-alive)
	     (if (and (hop-enable-keep-alive) (<fx (scheduler-load scd) 50))
		 (schedule scd
			   (lambda (s t)
			      (stage-request s t id sock
					     'keep-alive
					     (hop-keep-alive-timeout)))
			   'stage-start)
		 (socket-close sock)))
	    (else
	     (socket-close sock))))))
      
	       
   
