;*=====================================================================*/
;*    serrano/prgm/project/hop/src/main.scm                            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Mon Mar 20 07:32:57 2006 (serrano)                */
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
	    hop_param
	    hop_http-request)

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
   ;; load the hop library
   (eval '(library-load 'hop))
   (eval '(library-load 'web))
   ;; parse the command line
   (parse-args args)
   (hop-verb 2 "Starting hop on port " (hop-port) ":\n")
   ;; setup the hop readers
   (bigloo-load-reader-set! hop-read)
   (bigloo-load-module-set! load-once)
   ;; install the builtin filters
   (hop-filter-add-always-first! autoload-filter)
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
	    (rp (make-threads-pool 'reply (hop-max-reply-thread) -1)))
	 (let loop ()
	    (with-handler
	       (lambda (e)
		  (exception-notify e))
	       (let liip ((n 1))
		  (handle-connection ap rp s n)
		  (liip (+fx n 1))))
	    (loop)))))

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
;*    handle-connection ...                                            */
;*---------------------------------------------------------------------*/
(define (handle-connection accept-pool::pool reply-pool::pool s::socket n::int)
   (let ((sock (socket-accept s)))
      (hop-verb 1 (hop-color n n " CONNECT")
		": " sock " [" (current-date) "]\n")
      (when (socket? sock)
	 (pool-thread-execute accept-pool
			      (http-connect sock reply-pool n)
			      (lambda (m)
				 (http-response
				  (http-service-unavailable m) sock))
			      n))))

;*---------------------------------------------------------------------*/
;*    http-connect ...                                                 */
;*---------------------------------------------------------------------*/
(define (http-connect sock reply-pool id)
   (lambda ()
      (let ((req (with-handler
		    (lambda (e)
		       (when (&error? e) (error-notify e))
		       (unless (socket-down? sock)
			  (with-handler
			     (lambda (e) #unspecified)
			     (unless (&io-sigpipe-error? e)
				(let ((resp ((or (hop-http-request-error)
						 http-request-error)
					     e)))
				   (http-response resp sock)))))
		       (socket-close sock)
		       #f)
		    (http-parse-request sock id))))
	 (when (http-request? req)
	    (pool-thread-execute reply-pool
				 (http-process req sock)
				 (lambda (m)
				    (http-response
				     (http-service-unavailable m) sock))
				 req
				 (let ((host (http-request-host req)))
				    (lambda (p)
				       (can-connect? reply-pool host))))))))

;*---------------------------------------------------------------------*/
;*    http-debug ...                                                   */
;*---------------------------------------------------------------------*/
(define (http-debug lvl . args)
   (when (>=fx (bigloo-debug) lvl)
      (apply fprint (current-error-port) args)))

;*---------------------------------------------------------------------*/
;*    http-process ...                                                 */
;*---------------------------------------------------------------------*/
(define (http-process req sock)
   (lambda ()
      (hop-verb 2 (hop-color req req " PROCESS") ": " req "\n")
      (with-handler
	 (lambda (e)
	    (with-handler
	       (lambda (e) #f)
	       (hop-verb 1 (hop-color req req " ERROR")
			 " " (trace-color 1 e) "\n")
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
		      ": " hp
		      " "
		      (if (user? (http-request-user req))
			  (user-name (http-request-user req))
			  "anonymous") "\n")
	    (let ((rep (http-response hp sock)))
	       (hop-verb 2 (hop-color req req " RESPONSE") ": "
			 (current-date)
			 " persistent: " (http-response-persistent? rep)
			 "\n")
	       (unless (http-response-persistent? rep)
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
   
