;*=====================================================================*/
;*    serrano/prgm/project/hop/src/main.scm                            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Tue Jun 13 10:57:47 2006 (eg)                     */
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
;*    log-response ...                                                 */
;*---------------------------------------------------------------------*/
(define (log-response port req resp)
  (define (two-digits n)
    (when (< n 10)
      (display #\0 port))
    (display n port))

  (with-access::http-request req (socket host user method encoded-path http header)
     ;; distant host address and user
     (fprintf port "~a - ~a "
	      (socket-host-address socket)
	      (or user "-"))
     ;; date
     (display "[" port)
     (let* ((d   (current-date))
	    (tz  (if (= 1 (date-is-dst d))
		     (- (date-timezone d) 3600)
		     (date-timezone d))))
       (two-digits (date-day d))    (display "/" port)
       (two-digits (date-month d))  (display "/" port)
       (display (date-year d) port) (display ":" port)
       (two-digits (date-hour d))   (display ":" port)
       (two-digits (date-minute d)) (display ":" port)
       (two-digits (date-second d)) (display " " port)
       (display (if (> tz 0) "+" "-") port)
       (two-digits (quotient  (abs tz) 3600))
       (two-digits (remainder (abs tz) 3600)))
     (display "] " port)
     ;; request
     (fprintf port "\"~a ~a ~a\" " method encoded-path http)
     ;; Return code
     (let* ((str (%http-response-local-start-line resp))
	    (len (string-length str)))
       (let Loop ((i  0)
		  (sp 0))
	 (cond
	   ((or (>= i len) (>= sp 2))
	    #f)
	   ((char=? (string-ref str i) #\space)
	    (Loop (+ i 1) (+ sp 1)))
	   ((= sp 1)
	    (display (string-ref str i) port)
	    (Loop (+ i 1) sp))
	   (else
	    (Loop (+ i 1) sp)))))
     (display " " port)
     ;; Content-length
     (let ((hdrs (%http-response-local-header resp)))
       (cond
	 ((http-response-file? resp)
	  (display (file-size (http-response-file-file resp)) port))
	 ((>elong (%http-response-content-length resp) #e0)
	  (display (%http-response-content-length resp) port))
	 (else
	  (display "-" port))))
     ;; Long version (add User-Agent and Referer)
     (let ((agent   (assoc :user-agent header))
	   (referer (assoc :referer header)))
       (when (and (pair? agent) (pair? referer))
	 (fprintf port " ~s ~s" (cdr referer) (cdr agent))))
     (newline port)
     (flush-output-port port)))


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
   (hop-verb 2 "Starting hop on port " (hop-port) ":\n")
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
	    (rp (make-threads-pool 'reply (hop-max-reply-thread) -1)))
	 (let loop ((n 1))
	    (with-handler
	       (lambda (e)
		  (exception-notify e))
	       (let liip ((n n))
		  (handle-connection ap rp s n)
		  (liip (+fx n 1))))
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
;*    handle-connection ...                                            */
;*---------------------------------------------------------------------*/
(define (handle-connection accept-pool::pool reply-pool::pool s::socket n::int)
   (let ((sock (socket-accept s)))
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
      (when (socket? sock)
	 (pool-thread-execute accept-pool
			      (lambda ()
				 (http-connect sock accept-pool reply-pool n))
			      (lambda (m)
				 (http-response
				  (http-service-unavailable m) sock))
			      n))))

;*---------------------------------------------------------------------*/
;*    http-connect ...                                                 */
;*---------------------------------------------------------------------*/
(define (http-connect sock accept-pool reply-pool id)
   (let ((req (with-handler
		 (lambda (e)
		    (when (&error? e) (error-notify e))
;*---------------------------------------------------------------------*/
;*    Why should be bother answering on error!?                        */
;*---------------------------------------------------------------------*/
;* 		    (unless (or (socket-down? sock)                    */
;* 				(hop-close-request-syntax-error))      */
;* 		       (with-handler                                   */
;* 			  (lambda (e) #unspecified)                    */
;* 			  (unless (&io-sigpipe-error? e)               */
;* 			     (let ((resp ((or (hop-http-request-error) */
;* 					      http-request-error)      */
;* 					  e)))                         */
;* 				(http-response resp sock)))))          */
		    (socket-close sock)
		    (hop-verb 1 (hop-color id id " CLOSING")
			      " " (trace-color 1 (find-runtime-type e))
			      "\n")
		    #f)
		 (http-parse-request sock id))))
      (when (http-request? req)
	 (with-access::http-request req (method scheme host port path)
	    (hop-verb 2 (hop-color req req " PROCESS")
		      " ("
		      (pool-thread-available accept-pool)
		      "/" (hop-max-accept-thread)
		      "-" 
		      (pool-thread-available reply-pool)
		      "/" (hop-max-reply-thread) "): "
		      method " "
		      scheme "://" host ":" port (string-for-read path)
		      "\n"))
	 (pool-thread-execute reply-pool
			      (lambda ()
				 (http-process req sock accept-pool reply-pool))
			      (lambda (m)
				 (http-response
				  (http-service-unavailable m) sock))
			      req))))
;*---------------------------------------------------------------------*/
;*    MS: 17may 06                                                     */
;*    -------------------------------------------------------------    */
;*    This is not the good time to execute this test. The              */
;*    HTTP-RESPONSE-REMOTE should be in charge.                        */
;*---------------------------------------------------------------------*/
;* 				 (let ((host (http-request-host req))) */
;* 				    (lambda (p)                        */
;* 				       (can-connect? reply-pool host)))))))) */

;*---------------------------------------------------------------------*/
;*    http-debug ...                                                   */
;*---------------------------------------------------------------------*/
(define (http-debug lvl . args)
   (when (>=fx (bigloo-debug) lvl)
      (apply fprint (current-error-port) args)))

;*---------------------------------------------------------------------*/
;*    http-process ...                                                 */
;*---------------------------------------------------------------------*/
(define (http-process req sock accept-pool reply-pool)
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
	 (let ((rep (http-response hp sock)))
	   ;; Eventually log this request
	   (when (%http-response-local? hp)
	     (let ((p (hop-log-file)))
	      (when (output-port? p)
		(log-response p req hp))))
	   
	    (hop-verb 2 (hop-color req req " RESPONSE")
		      " ("
		      (pool-thread-available accept-pool)
		      "/" (hop-max-accept-thread)
		      "-" 
		      (pool-thread-available reply-pool)
		      "/" (hop-max-reply-thread) "): "
		      rep
		      " [" (current-date) "]"
		      (if (http-response-persistent? rep)
			  " persistent\n"
			  "\n"))
	    (unless (http-response-persistent? rep)
	       (socket-close sock))))))

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
   
