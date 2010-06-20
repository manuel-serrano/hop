;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/src/pipeline.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep  4 09:28:11 2008                          */
;*    Last change :  Sat Jun 19 06:53:09 2010 (serrano)                */
;*    Copyright   :  2008-10 Manuel Serrano                            */
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
   
   (export (stage-request ::scheduler ::thread ::int ::socket ::obj)))

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
;*    *socket-mutex* ...                                               */
;*---------------------------------------------------------------------*/
(define *socket-mutex* (make-mutex "hop-sock"))

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
(define (stage-request scd thread id sock timeout)
   
   ;; verbose function (only for log and debug)
   (define (http-connect-verb scd id sock req mode num)
      (with-access::http-request req (method scheme host port path user header)
	 (hop-verb 1 (if (http-proxy-request? req)
			 (hop-color req req
				    (if (eq? mode 'keep-alive)
					(format " REQUEST.prox (+~a)" num)
					" REQUEST.prox"))
			 (hop-color req req
				    (if (eq? mode 'keep-alive)
					(format " REQUEST.serv (+~a)" num)
					" REQUEST.serv")))
		   (if (>=fx (hop-verbose) 2)
		       (format " ~a~a: " thread (scheduler-stat scd))
		       ": ")
		   method " " scheme "://"
		   (if (>=fx (hop-verbose) 2)
		       (string-append (user-name user) "@")
		       "")
		   host ":"
		   port (string-for-read path)
		   " "
		   (if (>=fx (hop-verbose) 2)
		       (http-request-http req)
		       "")
		   "\n")
	 (hop-verb 4 (hop-color id id " CONNECT.header") ": "
		   (with-output-to-string (lambda () (write header))) "\n")))

   (let loop ((mode 'connect)
	      (timeout timeout)
	      (num 1))
      (with-stage-handler
       stage-request-error-handler (id sock mode)
       ;; log
       (unless (eq? mode 'keep-alive)
	  (hop-verb 2 (hop-color id id " CONNECT")
		    (if (>=fx (hop-verbose) 3) (format " ~a" thread) "")
		    (if (>=fx (hop-verbose) 2) (scheduler-stat scd) "")
		    ": " (if (>=fx (hop-verbose) 2)
			     (socket-hostname sock)
			     (socket-host-address sock))
		    " [" (current-date) "]\n"))
       ;; debug trace
       (debug-thread-info-set! thread "connection established with ~a")
       (let ((req (with-time (http-parse-request sock id timeout) id "CONNECT")))
	  ;; debug info
	  (debug-thread-info-set! thread
				  (format "request parsed for ~a, ~a ~a"
					  (if (>=fx (hop-verbose) 2)
					      (socket-hostname sock)
					      (socket-host-address sock))
					  (http-request-method req)
					  (http-request-path req)))
	  (http-connect-verb scd id sock req mode num)
	  ;; decrement the keep-alive number (we have a valid connection)
	  (when (eq? mode 'keep-alive) (keep-alive--))
	  ;; start compting the answer
	  (let ((keep-alive-timeout (stage scd thread stage-response id req)))
	     (when (fixnum? keep-alive-timeout)
		(loop 'keep-alive keep-alive-timeout (+fx num 1))))))))

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
;*    response-error-handler ...                                       */
;*---------------------------------------------------------------------*/
(define (response-error-handler e scd req)
   ;; notify the erro
   (hop-verb 1 (hop-color req req " ERROR"))
   (hop-verb 2 (scheduler-stat scd))
   (hop-verb 1 ": " (trace-color 1 e) "\n")
   ;; when the error is a response, we transmit it to the next stage
   (with-handler
      (lambda (e)
	 ;; there is nothing we can do but aborting the request
	 (socket-close (http-request-socket req))
	 (raise (instantiate::&ignore-exception)))
      ;; try to send the error message
      (if (%http-response? e)
	  (http-response e (http-request-socket req))
	  (begin
	     (exception-notify e)
	     ;; generate a legal response for the next stage (although
	     ;; this response denotes the error).
	     (let ((resp ((or (hop-http-response-error) http-error) e req))
		   (sock (http-request-socket req)))
		(http-response resp sock)
		;; abort this request
		(socket-close sock)
		'close)))))

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
	     ": " (find-runtime-type resp) " "
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
   (if (http-response-abort? resp)
       (hop-verb 1 (hop-color req req " ABORT")
		 " user: " (user-name (http-request-user req)) "\n")
       (hop-verb 3 (hop-color req req " EXEC")
		 " load: " (scheduler-load scd)
		 (scheduler-stat scd)
		 (format " ~a" thread)
		 ": " (find-runtime-type resp)
		 " " (user-name (http-request-user req)) "\n"))
   (with-stage-handler
      exec-error-handler (scd req)
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
	     #f)
	    ((keep-alive)
	     (let ((load (scheduler-load scd)))
		(cond
		   ((or (>=fx (keep-alive) (hop-keep-alive-threshold))
			(=fx load 100))
		    (when (>=fx (hop-verbose) 3)
		       (stage-exec-verb scd thread req resp connection
					  " END"))
		    (socket-close sock)
		    #f)
		   ((>=fx load 80)
		    (when (>=fx (hop-verbose) 3)
		       (stage-exec-verb scd thread req resp connection
					  " KEEP-ALIVE"))
		    (keep-alive++)
		    ;(stage scd thread stage-request id sock 'keep-alive 1)
		    1)
		   (else
		    (when (>=fx (hop-verbose) 3)
		       (stage-exec-verb scd thread req resp connection
					  " KEEP-ALIVE"))
		    (keep-alive++)
		    ;(stage scd thread stage-request id sock 'keep-alive (hop-keep-alive-timeout))
		    (hop-keep-alive-timeout)))))
	    (else
	     (when (>=fx (hop-verbose) 3)
		(stage-exec-verb scd thread req resp connection " END"))
	     (socket-close sock)
	     #f)))))

;*---------------------------------------------------------------------*/
;*    exec-error-handler ...                                           */
;*---------------------------------------------------------------------*/
(define (exec-error-handler e scd req)
   ;; first, close the socket, anycase
   (socket-close (http-request-socket req))
   (unless (&io-sigpipe-error? e)
      ;; signal the error, when this is an error
      (hop-verb 2 (hop-color req req " INTERRUPTED"))
      (hop-verb 2 ": " (&error-obj e) "\n")
      ;; abort the request
      (raise e)))

