;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/src/pipeline.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep  4 09:28:11 2008                          */
;*    Last change :  Thu May 22 09:13:04 2014 (serrano)                */
;*    Copyright   :  2008-14 Manuel Serrano                            */
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
      (enable-ssl (library ssl)))

   (import hop_scheduler
	   hop_param
	   hop_accept)

   (export (stage-request ::scheduler ::thread ::int ::socket ::obj)))

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
   (synchronize *socket-mutex* *keep-alive*))

;*---------------------------------------------------------------------*/
;*    keep-alive-- ...                                                 */
;*---------------------------------------------------------------------*/
(define (keep-alive--)
   (synchronize *socket-mutex*
      (set! *keep-alive* (-fx *keep-alive* 1))))

;*---------------------------------------------------------------------*/
;*    keep-alive++ ...                                                 */
;*---------------------------------------------------------------------*/
(define (keep-alive++)
   (synchronize *socket-mutex*
      (set! *keep-alive* (+fx 1 *keep-alive*))))

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
         (when (>=fx (hop-verbose) 1)
	    (hop-verb 1 (if (isa? req http-proxy-request)
			    (hop-color req req
				       (if (eq? mode 'keep-alive)
					   (format " REQUEST.prox (+~a)" num)
					   " REQUEST.prox"))
			    (hop-color req req
                                       (if (eq? mode 'keep-alive)
					   (format " REQUEST.serv (+~a)" num)
					   " REQUEST.serv")))
		      (if (>=fx (hop-verbose) 3)
			  (format " ~a~a: " thread (scheduler-stat scd))
			  ": ")
		      method " " scheme "://"
		      (if (>=fx (hop-verbose) 2)
			  (with-access::user user (name)
			     (string-append name "@"))
			  "")
		      host ":"
		      port (string-for-read path)
		      " "
		      (if (>=fx (hop-verbose) 2)
			  (with-access::http-request req (http) http)
			  "")
		      "\n")
	    (hop-verb 4 (hop-color id id " CONNECT.header") ": "
		      (with-output-to-string (lambda () (write header))) "\n"))))

   (let loop ((mode 'connect)
	      (timeout timeout)
	      (num 1))
      (with-stage-handler stage-request-error-handler (id sock mode)
	 ;; debug trace
	 (debug-thread-info-set! thread "connection established with ~a")
	 (let ((req (with-time (http-parse-request sock id timeout) id "CONNECT")))
	    ;; debug info
	    (with-access::http-request req (method path)
	       (debug-thread-info-set! thread
		  (format "request parsed for ~a, ~a ~a"
		     (if (>=fx (hop-verbose) 2)
			 (socket-hostname sock)
			 (socket-host-address sock))
		     method
		     path)))
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
	   (or (isa? e &io-timeout-error)
	       (isa? e &io-connection-error)
	       (and (isa? e &io-parse-error)
		    (with-access::&io-parse-error e (obj)
		       (eof-object? obj))))))

   (if (keep-alive-ellapsed-error? e)
       ;; this is not a true error, just log
       (hop-verb 3 (hop-color id id " SHUTDOWN")
	  (cond
	     ((isa? e &io-timeout-error)
	      " (keep-alive, timeout ellapsed)")
	     ((and (isa? e &io-parse-error)
		   (with-access::&io-parse-error e (obj)
		      (eof-object? obj)))
	      " (keep-alive, connection reset by peer)")
	     (else
	      " (keep-alive, parse error)"))
	  "\n")
       ;; this one is a true error
       (begin
	  (when (isa? e &exception)
	     (hop-verb 1 (hop-color id id " ABORT: ")
		" " (trace-color 1 (typeof e))
		(if (>=fx (hop-verbose) 4)
		    (format "~a:~a"
		       (socket-hostname sock)
		       (socket-port-number sock))
		    "")
		"\n")
	     (when (>fx (bigloo-debug) 0)
		(hop-verb 1
		   (with-error-to-string
		      (lambda () (exception-notify e))))))
	  (when (and (isa? e &io-unknown-host-error) (not (socket-down? sock)))
	     (with-handler
		(lambda (e)
		   ;; this error handler is invoked when the attempt to
		   ;; notify the previous error to the client fails
		   (when (isa? e &error) (exception-notify e))
		   #unspecified)
		;; we will try to answer the error to the client
		(unless (isa? e &io-sigpipe-error)
		   (let ((resp ((or (hop-http-request-error) http-error) e)))
		      (http-response resp sock)))))))

   ;; decrement the keep-alive number
   (when (eq? mode 'keep-alive) (keep-alive--))
   ;; abort this request
   (socket-close sock))

;*---------------------------------------------------------------------*/
;*    http-response-static? ...                                        */
;*---------------------------------------------------------------------*/
(define (http-response-static? resp)
   (or (isa? resp http-response-file)
       (isa? resp http-response-string)
       (isa? resp http-response-abort)
       (isa? resp http-response-error)))

;*---------------------------------------------------------------------*/
;*    stage-response ...                                               */
;*    -------------------------------------------------------------    */
;*    This stage is in charge of building a response to the received   */
;*    request.                                                         */
;*---------------------------------------------------------------------*/
(define (stage-response scd thread id req)
   (current-request-set! thread req)
   (hop-verb 3 (hop-color id id " RESPONSE") (format " ~a" thread) "\n")
   (with-stage-handler response-error-handler (scd req)
      (let ((resp (with-time (request->response req thread) id "RESPONSE")))
	 (with-access::http-request req (method scheme host port path)
	    (debug-thread-info-set! thread
	       (format "~a ~a://~a:~a~a... -> ~a"
		  method scheme host
		  port path (typeof resp))))
	 (let ((proc (cond
			((http-response-static? resp) stage-static-answer)
			((isa? resp http-response-async) stage-async-answer)
			(else stage-dynamic-answer))))
	    (stage scd thread proc id req resp)))))

;*---------------------------------------------------------------------*/
;*    response-error-handler ...                                       */
;*---------------------------------------------------------------------*/
(define (response-error-handler e scd req)
   ;; notify the error
   (hop-verb 1 (hop-color req req " ERROR"))
   (hop-verb 2 (scheduler-stat scd))
   (hop-verb 1 ": " (trace-color 1 e) "\n")
   ;; when the error is a response, we transmit it to the next stage
   ;; when the error is an exception, we create a response that it
   ;; then transmitted to the next stage
   (with-handler
      (lambda (e)
	 (exception-notify e)
	 ;; there is nothing we can do but aborting the request
	 (socket-close (with-access::http-request req (socket) socket))
	 (raise (instantiate::&ignore-exception)))
      ;; try to send the error message
      (if (isa? e %http-response)
	  (with-access::http-request req (socket)
	     (http-response e socket))
	  (begin
	     (exception-notify e)
	     (when (>=fx (bigloo-debug) 1)
		(with-access::http-request req (header)
		   (let ((stk (http-header-field header hop-debug-stack:)))
		      (when (string? stk)
			 (with-handler
			    (lambda (e) #f)
			    (let ((stk (hop-debug-exception-stack
					  (string->obj (url-decode stk)))))
			       (when (pair? stk)
				  (display-trace-stack stk
				     (current-error-port)
				     (if (isa? e &exception)
					 (with-access::&exception e (stack)
					    (+fx 1 (length stack)))
					 1)))))))))
	     ;; generate a legal response for the next stage (although
	     ;; this response denotes the error).
	     (let ((resp ((or (hop-http-response-error) http-error) e)))
		(with-access::http-request req (socket)
		   (http-response resp socket)
		   ;; abort this request
		   (socket-close socket)
		   'close))))))

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
;*    stage-async-answer ...                                           */
;*---------------------------------------------------------------------*/
(define (stage-async-answer scd thread id req resp)
   (current-request-set! thread req)
   (with-stage-handler exec-error-handler (scd req)
      (with-access::http-request req (socket method scheme port host path)
	 (with-access::http-response-async resp (async)
	    (async
	       (lambda (resp)
		  (with-handler
		     (lambda (e)
			(exec-error-handler e scd req))
		     (let ((tmt (stage-exec scd thread id req resp)))
			(when (integer? tmt)
			   (spawn scd stage-request id socket tmt)))))))
	 #f)))
	   
;*---------------------------------------------------------------------*/
;*    stage-exec-verb ...                                              */
;*---------------------------------------------------------------------*/
(define (stage-exec-verb scd thread req resp connection mode)
   (hop-verb 3 (hop-color req req mode)
	     (format " ~a" thread)
	     " load=" (scheduler-load scd)
	     (scheduler-stat scd)
	     ": " (typeof resp) " "
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
   (with-access::http-request req (user)
      (if (isa? resp http-response-abort)
	  (hop-verb 1 (hop-color req req " ABORT")
	     " user: " (with-access::user user (name) name) "\n")
	  (hop-verb 3 (hop-color req req " EXEC")
	     " load: " (scheduler-load scd)
	     (scheduler-stat scd)
	     (format " ~a" thread)
	     ": " (typeof resp)
	     " " (with-access::user user (name) name) "\n")))
   (with-stage-handler exec-error-handler (scd req)
      (with-access::http-request req (socket method scheme port host path)
	 (let ((connection (with-time (http-response resp socket) id "EXEC")))
	    ;; debug
	    (debug-thread-info-set! thread
	       (format "~a ~a://~a:~a~a... -> ~a ~a"
		  method scheme host port path
		  (typeof resp) connection))
	    (case connection
	       ((keep-alive)
		(let ((load (scheduler-load scd)))
		   (cond
		      ((or (>=fx (keep-alive) (hop-keep-alive-threshold))
			   (=fx load 100))
		       (when (>=fx (hop-verbose) 3)
			  (stage-exec-verb scd thread req resp connection
			     " END"))
		       (socket-close socket)
		       #f)
		      ((>=fx load 80)
		       (when (>=fx (hop-verbose) 3)
			  (stage-exec-verb scd thread req resp connection
			     " KEEP-ALIVE"))
		       (keep-alive++)
		       1)
		      (else
		       (when (>=fx (hop-verbose) 3)
			  (stage-exec-verb scd thread req resp connection
			     " KEEP-ALIVE"))
		       (keep-alive++)
		       (hop-keep-alive-timeout)))))
	       ((persistent)
		(when (>=fx (hop-verbose) 3)
		   (stage-exec-verb scd thread req resp connection
		      " PERSISTENT"))
		#f)
	       (else
		(when (>=fx (hop-verbose) 3)
		   (stage-exec-verb scd thread req resp connection " END"))
		(socket-close socket)
		#f))))))

;*---------------------------------------------------------------------*/
;*    exec-error-handler ...                                           */
;*---------------------------------------------------------------------*/
(define (exec-error-handler e scd req)
   (with-access::http-request req (socket)
      ;; first, close the socket, anycase
      (socket-close socket)
      (unless (isa? e &io-sigpipe-error)
	 ;; signal the error, when this is an error
	 (hop-verb 2 (hop-color req req " INTERRUPTED"))
	 (hop-verb 2 ": " req "\n")
	 ;; abort the request
	 (if (and (>=fx (bigloo-debug) 1) (isa? e &exception))
	     (with-handler
		(lambda (e2)
		   (raise e))
		(begin
		   (exception-notify e)
		   (with-access::http-request req (header)
		      (let ((stk (http-header-field header hop-debug-stack:)))
			 (when (string? stk)
			    (with-handler
			       (lambda (e) #f)
			       (let ((stk (hop-debug-exception-stack
					     (string->obj (url-decode stk)))))
				  (when (pair? stk)
				     (display-trace-stack stk
					(current-error-port)
					(if (isa? e &exception)
					    (with-access::&exception e (stack)
					       (+fx 1 (length stack)))
					    1))))))))
		   #f))
	     (raise e)))))

