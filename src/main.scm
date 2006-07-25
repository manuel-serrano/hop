;*=====================================================================*/
;*    serrano/prgm/project/hop/src/main.scm                            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Tue Jul 25 09:31:38 2006 (serrano)                */
;*    Copyright   :  2004-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP entry point                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module main

   (library pthread
	    web
	    hop)

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
	  `(hop-verb ,@(map (lambda (x) (e x e))) (cdr x))))))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   ;; catch critical signals
   (signal-init!)
   ;; set the library load path
   (let ((hop-path (make-file-path (hop-lib-directory) "hop" (hop-version))))
      (bigloo-library-path-set! (cons hop-path (bigloo-library-path))))
   ;; preload the hop libraries
   (for-each (lambda (l) (eval `(library-load ',l))) (hop-preload-libraries))
   ;; parse the command line
   (parse-args args)
   (hop-verb 1 "Starting hop (v" (hop-version) ", " (hop-backend)
	     ") on port " (hop-port) ":\n")
   ;; setup the hop readers
   (bigloo-load-reader-set! hop-read)
   (bigloo-load-module-set! load-once)
   ;; install the builtin filters
   (hop-filter-add-always-last! autoload-filter)
   (hop-filter-add! service-filter)
   ;; start the job scheduler
   (job-start-scheduler!)
   ;; start the hop main loop
   (with-handler
      (lambda (e)
	 (exception-notify e)
	 (exit 1))
      (let ((s (make-server-socket (hop-port)))
	    (ap (make-threads-pool 'accpt (hop-max-accept-thread) -1))
	    (rp (case (hop-scheduling)
		   ((cohort)
		    (make-threads-pool 'reply (hop-max-reply-thread) -1))
		   ((simple)
		    #f)
		   (else
		    (error 'hop
			   "Illegal scheduling policy"
			   (hop-scheduling))))))
	 (let loop ((n 1))
	    (with-handler
	       (lambda (e)
		  (exception-notify e))
	       (let liip ()
		  (accept-connection s ap rp n)
		  (set! n (+fx n 1))
		  (liip)))
	    (loop n)))))

;*---------------------------------------------------------------------*/
;*    signal-init! ...                                                 */
;*---------------------------------------------------------------------*/
(define (signal-init!)
   (signal sigsegv
	   (lambda (n)
	      (fprint (current-error-port) "Segmentation violation")
	      (dump-trace-stack (current-error-port) 10)
	      (exit 2))))

;*---------------------------------------------------------------------*/
;*    accept-connection ...                                            */
;*---------------------------------------------------------------------*/
(define (accept-connection s::socket accept-pool::pool reply-pool::pool n::int)
   (let ((sock (socket-accept s)))
      (when (socket? sock)
	 (hop-verb 1 (hop-color n n " CONNECT"))
	 (hop-verb 2
		   " (" (pool-thread-available accept-pool)
		   "/" (hop-max-accept-thread)
		   "-" 
		   (pool-thread-available reply-pool)
		   "/" (hop-max-reply-thread)
		   ")")
	 (hop-verb 1
		   ": "
		   (socket-hostname sock) " [" (current-date) "]\n")
	 (handle-connection
	  sock accept-pool reply-pool (*fx 1000000 (hop-read-timeout)) n 'connect))))

;*---------------------------------------------------------------------*/
;*    handle-connection ...                                            */
;*---------------------------------------------------------------------*/
(define (handle-connection sock
			   accept-pool::pool
			   reply-pool::pool
			   timeout::int
			   id::int
			   mode::symbol)
   (pool-thread-execute accept-pool
			(lambda ()
			   (http-connect sock accept-pool reply-pool timeout id mode))
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
		    (unless (and (eq? mode 'keep-alive)
				 (or (&io-timeout-error? e)
				     (and (&io-parse-error? e)
					  (eof-object? (&io-parse-error-obj e)))))
		       (when (&error? e) (error-notify e))
		       (when (and (&io-unknown-host-error? e)
				  (not (socket-down? sock)))
			  (with-handler
			     (lambda (e) #unspecified)
			     (unless (&io-sigpipe-error? e)
				(let ((resp ((or (hop-http-request-error)
						 http-request-error)
					     e)))
				   (http-response resp sock)))))
		       (hop-verb 1 (hop-color id id " ABORTING")
				 " " (trace-color 1 (find-runtime-type e))
				 "\n"))
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
		      (pool-thread-available reply-pool)
		      "/" (hop-max-reply-thread)
		      ")")
	    (hop-verb 1
		      ": "
		      (socket-hostname sock) " [" (current-date) "]\n"))
	 (with-access::http-request req (method scheme host port path proxyp)
	    (hop-verb 2
		      (if proxyp
			  (hop-color req req " EXEC (P)")
			  (hop-color req req " EXEC (S)"))
		      " ("
		      (pool-thread-available accept-pool)
		      "/" (hop-max-accept-thread)
		      "-" 
		      (pool-thread-available reply-pool)
		      "/" (hop-max-reply-thread) "): "
		      method " "
		      scheme "://" host ":" port (string-for-read path)
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
		       (error-notify e))
		      ((&warning? e)
		       (warning-notify e)))
		   (unless (&io-sigpipe-error? e)
		      (let ((resp ((or (hop-http-response-error)
 				       http-response-error)
				   e req)))
			 (http-response resp sock))))))
	 (socket-close sock)
	 #f)
      (let ((hp (hop req)))
	 (hop-verb 4 (hop-color req req " EXEC")
		   " ("
		   (pool-thread-available accept-pool)
		   "/" (hop-max-accept-thread)
		   "-" 
		   (pool-thread-available reply-pool)
		   "/" (hop-max-reply-thread) "): "
		   (find-runtime-type hp)
		   " "
		   (if (user? (http-request-user req))
		       (user-name (http-request-user req))
		       "anonymous") "\n")
	 (let ((connection (http-response hp sock)))
	    (hop-verb 2 (hop-color req req " RESPONSE")
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
		(if (hop-enable-keep-alive)
		    (handle-connection
		     sock accept-pool reply-pool
		     (*fx (hop-keep-alive-timeout) 1000) id connection)
		    (socket-close sock)))
	       (else
		(socket-close sock)))))))

;*---------------------------------------------------------------------*/
;*    can-connect? ...                                                 */
;*    -------------------------------------------------------------    */
;*    This predicate returns #t iff the current number of thread in    */
;*    POOL connected to SITE is lesser than                            */
;*    HOP-MAX-REPLY-PERSITE-THREAD.                                    */
;*---------------------------------------------------------------------*/
(define (can-connect? pool site)
   (with-access::pool pool (use)
      (let loop ((thread use)
		 (n 0))
	 (if (null? thread)
	     #t
	     (let ((t (car thread)))
		(if (http-request? (thread-data t))
		    (if (string=? (http-request-host (thread-data t)) site)
			(let ((nn (+fx 1 n)))
			   (if (>=fx nn (hop-max-reply-persite-thread))
			       #f
			       (loop (cdr thread) nn)))
			(loop (cdr thread) n))))))))
   
