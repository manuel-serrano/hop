;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/src/pipeline.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep  4 09:28:11 2008                          */
;*    Last change :  Thu Oct 29 07:46:01 2009 (serrano)                */
;*    Copyright   :  2008-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The pipeline into which requests transit.                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_pipeline

   (library hop) 

   (include "stage.sch")
   
   (cond-expand
      (enable-threads (library pthread)))

   (cond-expand
      (enable-ssl (library ssl)))

   (import hop_scheduler
	   hop_param
	   hop_accept)
   
   (export (stage-request ::scheduler ::thread ::int ::socket ::symbol ::obj)))

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
;*    *socket-mutex* ...                                               */
;*---------------------------------------------------------------------*/
(define *socket-mutex* (make-mutex 'hop-sock))

;*---------------------------------------------------------------------*/
;*    *keep-alive* ...                                                 */
;*---------------------------------------------------------------------*/
(define *keep-alive* 0)

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
;*    http-request.                                                    */
;*---------------------------------------------------------------------*/
(define (stage-request scd thread id sock mode timeout)
   
   ;; verbose function (only for log and debug)
   (define (http-connect-verb scd id sock req)
      (with-access::http-request req (method scheme host port path user header)
	 (hop-verb 4 (hop-color id id " CONNECT.header") ": "
		   (with-output-to-string (lambda () (write header))) "\n")
	 (hop-verb 2 (if (http-proxy-request? req)
			 (hop-color req req
				    (if (eq? mode 'keep-alive)
					" EXEC.prox+"
					" EXEC.prox"))
			 (hop-color req req
				    (if (eq? mode 'keep-alive)
					" EXEC.serv+"
					" EXEC.serv")))
		   (format " ~a" thread)
		   (scheduler-stat scd)
		   ": " method " " scheme "://"
		   (user-name user) "@" host ":" port (string-for-read path)
		   " " (http-request-http req)
		   "\n")))

   ;; log
   (unless (eq? mode 'keep-alive)
      (hop-verb 1 (hop-color id id " CONNECT")
		(if (>=fx (hop-verbose) 3) (format " ~a" thread) "")
		(if (>=fx (hop-verbose) 2) (scheduler-stat scd) "")
		": " (if (>=fx (hop-verbose) 2)
			 (socket-hostname sock)
			 (socket-host-address sock))
		" [" (current-date) "]\n"))

   ;; debug trace
   (debug-thread-info-set! thread "connection established with ~a")
   
   (with-stage-handler
      stage-request-error-handler (id sock mode)
      (let ((req (with-time (http-parse-request sock id timeout) id "CONNECT")))
	 ;; debug info
	 (debug-thread-info-set! thread
				 (format "request parsed for ~a, ~a ~a"
					 (if (>=fx (hop-verbose) 2)
					     (socket-hostname sock)
					     (socket-host-address sock))
					 (http-request-method req)
					 (http-request-path req)))
	 (http-connect-verb scd id sock req)
	 ;; decrement the keep-alive number (we have a valid connection)
	 (when (eq? mode 'keep-alive) (keep-alive--))
	 ;; start compting the answer
	 (stage scd thread stage-response id req))))

;*---------------------------------------------------------------------*/
;*    stage-request-error-handler ...                                  */
;*---------------------------------------------------------------------*/
(define (stage-request-error-handler e id sock mode)

   ;; is the error raised of a timeout in a keep-alive connection?
   (define (keep-alive-ellapsed-error? e)
      (and (eq? mode 'keep-alive)
	   (or (&io-timeout-error? e)
	       (and (&io-parse-error? e)
		    (eof-object? (&io-parse-error-obj e))))))

   (if (keep-alive-ellapsed-error? e)
       ;; this is not a true error, just log
       (hop-verb 3 (hop-color id id " SHUTDOWN")
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
	     (hop-verb 1 (hop-color id id " ABORT: ")
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
   
   ;; decrement the keep-alive number
   (when (eq? mode 'keep-alive) (keep-alive--))
   ;; abort this request
   (socket-close sock))

;*---------------------------------------------------------------------*/
;*    response-error-handler ...                                       */
;*---------------------------------------------------------------------*/
(define (response-error-handler e scd req)
   ;; when the error is a response, we transmit it to the next stage
   (cond
      ((&hop-autoload-error? e)
       (with-handler
	  (lambda (e)
	     ;; there is nothing we can do but aborting the request
	     (socket-close (http-request-socket req))
	     (raise (instantiate::&ignore-exception)))
	  (let ((e (&hop-autoload-error-obj e)))
	     (response-exception-error-handler e scd req))))
      ((&io-error? e)
       (response-io-error-handler e scd req)
       (raise e))
      (else
       (with-handler
	  (lambda (e)
	     ;; there is nothing we can do but aborting the request
	     (socket-close (http-request-socket req))
	     (raise (instantiate::&ignore-exception)))
	  (response-exception-error-handler e scd req)))))

;*---------------------------------------------------------------------*/
;*    response-io-error-handler ...                                    */
;*---------------------------------------------------------------------*/
(define (response-io-error-handler e scd req)
   ;; signal the error
   (hop-verb 2 (hop-color req req " INTERRUPTED"))
   (hop-verb 2 ": " (&error-obj e) "\n")
   ;; there is nothing we can do but aborting the request
   (socket-close (http-request-socket req))
   ;; abort the request
   (raise (instantiate::&ignore-exception)))

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
		 (warning-notify (evmeaning-annotate-exception! e)))
		(else
		 (exception-notify e)))
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
   (or (http-response-file? resp)
       (http-response-string? resp)
       (http-response-abort? resp)
       (http-response-error? resp)))

;*---------------------------------------------------------------------*/
;*    stage-response ...                                               */
;*    -------------------------------------------------------------    */
;*    This stage is in charge of building a response to the received   */
;*    request.                                                         */
;*---------------------------------------------------------------------*/
(define (stage-response scd thread id req)
   (current-request-set! thread req)
   (hop-verb 3 (hop-color id id " RESPONSE") (format " ~a" thread) "\n")
   (with-stage-handler
      response-error-handler (scd req)
      (let ((resp (with-time (request->response req thread) id "RESPONSE")))
	 (evmeaning-reset-error!)
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
	    (stage scd thread proc id req resp)))))

;*---------------------------------------------------------------------*/
;*    stage-static-answer ...                                          */
;*---------------------------------------------------------------------*/
(define (stage-static-answer scd thread id req resp)
   (stage-exec scd thread id req resp))

;*---------------------------------------------------------------------*/
;*    stage-dynamic-answer ...                                         */
;*---------------------------------------------------------------------*/
(define (stage-dynamic-answer scd thread id req resp)
   (stage-exec scd thread id req resp))

;*---------------------------------------------------------------------*/
;*    stage-exec-verb ...                                              */
;*---------------------------------------------------------------------*/
(define (stage-exec-verb scd thread req resp connection mode)
   (hop-verb 3 (hop-color req req mode)
	     (format " ~a" thread)
	     " load=" (scheduler-load scd)
	     (scheduler-stat scd)
	     ": " (find-runtime-type resp) " " connection
	     " [" (current-date) "] "
	     (if (and (eq? connection 'keep-alive) (>=fx (hop-verbose) 4))
		 (format " keep-alive [open=~a/~a]"
			 (keep-alive)
			 (hop-keep-alive-threshold))
		 connection)
	     "\n"))

;*---------------------------------------------------------------------*/
;*    stage-exec ...                                                   */
;*---------------------------------------------------------------------*/
(define (stage-exec scd thread id req resp)
   (current-request-set! thread req)
   ;; log
   (hop-verb 3 (hop-color req req " EXEC")
	     " load=" (scheduler-load scd)
	     (scheduler-stat scd)
	     (format " ~a" thread)
	     ": " (find-runtime-type resp)
	     " " (user-name (http-request-user req)) "\n")
   (with-stage-handler
      response-error-handler (scd req)
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
	 (case connection
	    ((persistent)
	     (when (>=fx (hop-verbose) 3)
		(stage-exec-verb scd thread req resp connection
				   " PERSISTENT"))
	     #unspecified)
	    ((keep-alive)
	     (let ((load (scheduler-load scd)))
		(cond
		   ((or (>=fx (keep-alive) (hop-keep-alive-threshold))
			(=fx load 100))
		    (when (>=fx (hop-verbose) 3)
		       (stage-exec-verb scd thread req resp connection
					  " END"))
		    (socket-close sock))
		   ((>=fx load 80)
		    (when (>=fx (hop-verbose) 3)
		       (stage-exec-verb scd thread req resp connection
					  " KEEP-ALIVE"))
		    (keep-alive++)
		    (stage scd thread stage-request id sock 'keep-alive 1))
		   (else
		    (when (>=fx (hop-verbose) 3)
		       (stage-exec-verb scd thread req resp connection
					  " KEEP-ALIVE"))
		    (keep-alive++)
		    (stage scd thread stage-request id sock 'keep-alive (hop-keep-alive-timeout))))))
	    (else
	     (when (>=fx (hop-verbose) 3)
		(stage-exec-verb scd thread req resp connection " END"))
	     (socket-close sock))))))
