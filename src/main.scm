;*=====================================================================*/
;*    serrano/prgm/project/hop/src/main.scm                            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Tue Nov 13 17:21:59 2007 (serrano)                */
;*    Copyright   :  2004-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP entry point                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module main

   (library multimedia web hop)

   (cond-expand
      (enable-threads (library pthread)))

   (cond-expand
      (enable-ssl (library ssl)))

   (import  hop_parseargs
	    hop_param)

   (with    hop_init)

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
		(eval `(library-load ',l))) (hop-preload-libraries))
   ;; setup the hop readers
   (bigloo-load-reader-set! hop-read)
   (bigloo-load-module-set! hop-load-modified)
   ;; install the hop expanders
   (hop-install-expanders!)
   ;; parse the command line
   (parse-args args)
   (hop-verb 1 "Starting hop (v" (hop-version)
	     ", " (hop-backend)
	     (cond-expand
		(enable-threads ", multi-threaded")
		(else ", single-threaded"))
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
   ;; start the job scheduler
   (job-start-scheduler!)
   ;; close filters and users registration before starting
   (hop-filters-close!)
   (users-close!)
   ;; start the hop main loop
   (with-handler
      (lambda (e)
	 (exception-notify e)
	 (exit 1))
      (let* ((ap (make-threads-pool 'accpt (hop-max-accept-thread) -1))
	     (rp (case (hop-scheduling)
		    ((cohort)
		     (make-threads-pool 'reply (hop-max-reply-thread) -1))
		    ((simple)
		     #f)
		    (else
		     (error 'hop
			    "Illegal scheduling policy"
			    (hop-scheduling)))))
	     (serv (hop-server-socket)))
	 ;; when needed, start the HOP repl
	 (hop-repl ap)
	 ;; when needed, start a loop for server events
	 (hop-server-event-loop ap rp)
	 ;; start the main loop
	 (hop-main-loop serv ap rp))))

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
;*    hop-server-event-loop ...                                        */
;*---------------------------------------------------------------------*/
(define (hop-server-event-loop ap rp)
   (when (hop-enable-fast-server-event)
      (cond
	 ((=fx (hop-fast-server-event-port) (hop-port))
	  ;; will use the regular HOP port
	  (hop-event-init! (hop-port)))
	 ((=fx (pool-thread-available ap) 1)
	  ;; disable fast event because no thread is available
	  ;; and extra port is needed
	  (hop-event-init! #f))
	 (else
	  ;; run in a separate thread
	  (hop-event-init! (hop-fast-server-event-port))
	  (let ((sv (make-server-socket (hop-fast-server-event-port))))
	     (pool-thread-execute ap
				  (lambda ()
				     (hop-main-loop sv ap rp))
				  (lambda (m)
				     'persistent)
				  0))))))

;*---------------------------------------------------------------------*/
;*    hop-main-loop ...                                                */
;*---------------------------------------------------------------------*/
(define (hop-main-loop s ap rp)
   (let loop ((n 1))
      (with-handler
	 (lambda (e)
	    (exception-notify e))
	 (let liip ()
	    (accept-connection s ap rp n)
	    (set! n (+fx n 1))
	    (liip)))
      (loop n)))

;*---------------------------------------------------------------------*/
;*    signal-init! ...                                                 */
;*---------------------------------------------------------------------*/
(define (signal-init!)
   (cond-expand
      (enable-thread #unspecified)
      (else (signal sigpipe (lambda (n) #unspecified))))
   (signal sigsegv
	   (lambda (n)
	      (fprint (current-error-port) "Segmentation violation")
	      (dump-trace-stack (current-error-port) 10)
	      (exit 2))))

;*---------------------------------------------------------------------*/
;*    accept-connection ...                                            */
;*---------------------------------------------------------------------*/
(define (accept-connection s::socket accept-pool::pool reply-pool::obj n::int)
   (let ((sock (socket-accept s)))
      (when (socket? sock)
	 (hop-verb 1 (hop-color n n " CONNECT"))
	 (hop-verb 2
		   " (" (pool-thread-available accept-pool)
		   "/" (hop-max-accept-thread)
		   "-"
		   (if (pool? reply-pool)
		       (pool-thread-available reply-pool)
		       "")
		   "/" (hop-max-reply-thread)
		   ")")
	 (hop-verb 1
		   ": " (socket-hostname sock) " [" (current-date) "]\n")
	 (handle-connection
	  sock accept-pool reply-pool (hop-read-timeout) n 'connect))))

;*---------------------------------------------------------------------*/
;*    handle-connection ...                                            */
;*---------------------------------------------------------------------*/
(define (handle-connection sock
			   accept-pool::pool
			   reply-pool
			   timeout::int
			   id::int
			   mode::symbol)
   (pool-thread-execute accept-pool
			(lambda ()
			   (http-connect sock
					 accept-pool reply-pool
					 timeout id mode))
			(lambda (m)
			   (http-response
			    (http-service-unavailable m) sock))
			id))

;*---------------------------------------------------------------------*/
;*    http-connect ...                                                 */
;*---------------------------------------------------------------------*/
(define (http-connect sock accept-pool reply-pool timeout id mode)
   (let ((req (with-handler
		 (lambda (e)
		    (if (and (eq? mode 'keep-alive)
			     (or (&io-timeout-error? e)
				 (and (&io-parse-error? e)
				      (eof-object? (&io-parse-error-obj e)))))
			(hop-verb 2 (hop-color id id " SHUTTING DOWN") "\n")
			(begin
			   (when (&error? e) (error-notify e))
			   (when (and (&io-unknown-host-error? e)
				      (not (socket-down? sock)))
			      (with-handler
				 (lambda (e)
				    (when (&error? e) (error-notify e))
				    #unspecified)
				 (unless (&io-sigpipe-error? e)
				    (let ((resp ((or (hop-http-request-error)
						     http-request-error)
						 e)))
				       (http-response resp sock)))))
			   (hop-verb 1 (hop-color id id " ABORTING")
				     " " (trace-color 1 (find-runtime-type e))
				     "\n")))
		    (socket-close sock)
		    #f)
		 (http-parse-request sock id timeout))))
      (when (http-request? req)
	 (when (eq? mode 'keep-alive)
	    (hop-verb 1 (hop-color id id " KEEP-ALIVE"))
	    (hop-verb 2
		      " (" (pool-thread-available accept-pool)
		      "/" (hop-max-accept-thread)
		      "-" 
		      (if (pool? reply-pool)
			  (pool-thread-available reply-pool)
			  "")
		      "/" (hop-max-reply-thread)
		      ")")
	    (hop-verb 1
		      ": "
		      (socket-hostname sock) " [" (current-date) "]\n"))
	 (with-access::http-request req (method scheme host port path proxyp
						user)
	    (hop-verb 2
		      (if proxyp
			  (hop-color req req " EXEC.prox")
			  (hop-color req req " EXEC.serv"))
		      " ("
		      (pool-thread-available accept-pool)
		      "/" (hop-max-accept-thread)
		      "-"
		      (if (pool? reply-pool)
			  (pool-thread-available reply-pool)
			  "")
		      "/" (hop-max-reply-thread) "): "
		      method " "
		      scheme "://"
		      (user-name user)
		      "@"
		      host ":" port (string-for-read path)
		      "\n"))
	 (if reply-pool
	     (pool-thread-execute
	      reply-pool
	      (lambda ()
		 (http-process req sock accept-pool reply-pool id))
	      (lambda (m)
		 (http-response
		  (http-service-unavailable m) sock))
	      req)
	     (http-process req sock accept-pool reply-pool id)))))

;*---------------------------------------------------------------------*/
;*    http-debug ...                                                   */
;*---------------------------------------------------------------------*/
(define (http-debug lvl . args)
   (when (>=fx (bigloo-debug) lvl)
      (apply fprint (current-error-port) args)))

;*---------------------------------------------------------------------*/
;*    http-process ...                                                 */
;*---------------------------------------------------------------------*/
(define (http-process req sock accept-pool reply-pool id)
   (with-handler
      (lambda (e)
	 (with-handler
	    (lambda (e) #f)
	    (hop-verb 1 (hop-color req req " ERROR"))
	    (hop-verb 2
		      " (" (pool-thread-available accept-pool)
		      "/" (hop-max-accept-thread)
		      "-" 
		      (pool-thread-available reply-pool)
		      "/" (hop-max-reply-thread)
		      ")")
	    (hop-verb 1 ": " (trace-color 1 e) "\n")
	    (if (http-response-string? e)
		(http-response e sock)
		(begin
		   (cond
		      ((&error? e)
		       (error-notify (evmeaning-annotate-exception! e)))
		      ((&warning? e)
		       (warning-notify (evmeaning-annotate-exception! e))))
		   (unless (&io-sigpipe-error? e)
		      (let ((resp ((or (hop-http-response-error) http-error)
				   e req)))
			 (http-response resp sock))))))
	 (socket-close sock)
	 #f)
      (let ((hp (request->response req)))
	 (hop-verb 4 (hop-color req req " EXEC")
		   " ("
		   (pool-thread-available accept-pool)
		   "/" (hop-max-accept-thread)
		   "-" 
		   (pool-thread-available reply-pool)
		   "/" (hop-max-reply-thread) "): "
		   (find-runtime-type hp)
		   " "
		   (user-name (http-request-user req))
		   "\n")
	 (let ((connection (http-response hp sock)))
	    (hop-verb 2 (hop-color req req " END")
		      " ("
		      (pool-thread-available accept-pool)
		      "/" (hop-max-accept-thread)
		      "-" 
		      (pool-thread-available reply-pool)
		      "/" (hop-max-reply-thread) "): "
		      (find-runtime-type hp) " " connection
		      " [" (current-date) "]"
		      (if (http-response-persistent? hp)
			  " persistent\n"
			  "\n"))
	    (case connection
	       ((persistent)
		#unspecified)
	       ((keep-alive)
		(if (and (hop-enable-keep-alive)
			 (>fx (pool-thread-available accept-pool) 0)
			 (>fx (pool-thread-available reply-pool) 0))
		    (handle-connection
		     sock accept-pool reply-pool
		     (hop-keep-alive-timeout) id connection)
		    (socket-close sock)))
	       (else
		(socket-close sock)))))))
   
;*---------------------------------------------------------------------*/
;*    hop-repl ...                                                     */
;*---------------------------------------------------------------------*/
(define (hop-repl pool)
   (when (hop-enable-repl)
      (cond-expand
	 (enable-threads
	  (cond
	     ((> (pool-thread-available pool) 2)
	      (pool-thread-execute pool
				   (lambda ()
				      (hop-verb 1 "Entering repl...\n")
				      (begin (repl) (exit 0)))
				   (lambda (m)
				      (error 'hop-repl "Illegal value" m))
				   0))
	     (else
	      (error 'hop-repl
		     "HOP REPL cannot be started because not enought accept threads avaiable"
		     (pool-thread-available pool)))))
	 (else
	  (error 'hop-repl
		 "HOP REPL cannot be spawned when threads support disabled"
		 #unspecified)))))
