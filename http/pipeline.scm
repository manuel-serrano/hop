;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/http/pipeline.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep  4 09:28:11 2008                          */
;*    Last change :  Mon Sep  8 09:38:46 2025 (serrano)                */
;*    Copyright   :  2008-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The pipeline into which requests transit.                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __http_pipeline

   (include "stage.sch")

   (library pthread)

   (cond-expand
      (enable-ssl (library ssl)))

   (import __http_utils
	   __http_types
	   __http_scheduler
	   __http_accept
	   __http_parser)

   (export (stage-request ::scheduler ::thread ::int ::socket ::obj ::symbol)))

;*---------------------------------------------------------------------*/
;*    *socket-mutex* ...                                               */
;*---------------------------------------------------------------------*/
(define *socket-mutex* (make-spinlock "hop-sock"))

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
;*    hop-report-execution-time ...                                    */
;*---------------------------------------------------------------------*/
(define hop-report-execution-time
   (getenv "HOPPROFILE"))

;*---------------------------------------------------------------------*/
;*    with-time ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (with-time expr id msg)
   (let ((v (gensym)))
      `(if hop-report-execution-time
	   (multiple-value-bind (value real sys user)
	      (time (lambda () ,expr))
	      (trace 1 ,id (format " ~a.time " ,msg)
		 " real: " real
		 " sys: " sys
		 " user: " user)
	      value)
	   ,expr)))

;*---------------------------------------------------------------------*/
;*    stage-request ...                                                */
;*    -------------------------------------------------------------    */
;*    This stage is in charge of parsing the request. It produces a    */
;*    request object which is passed to the next stage.                */
;*---------------------------------------------------------------------*/
(define (stage-request scd thread id sock timeout mode)
   (let loop ((mode mode)
	      (timeout timeout)
	      (num 1))
      (with-stage-handler stage-request-error-handler (id sock mode)
	 (when (>fx timeout 0)
	    (input-port-timeout-set! (socket-input sock) (*fx 1000 timeout)))
	 (let ((req (with-time (http-parse-request sock id) id "CONNECT")))
	    (when-trace 1
	       (trace-http-connect scd id sock req mode num))
	    (when (>=fx (bigloo-debug) 2)
	       (debug-request req))
	    ;; decrement the keep-alive number (we have a valid connection)
	    (when (eq? mode 'keep-alive) (keep-alive--))
	    ;; start computing the answer
	    (let ((keep-alive-timeout (stage scd thread stage-response id req)))
	       (when (fixnum? keep-alive-timeout)
		  (loop 'keep-alive keep-alive-timeout (+fx num 1))))))))

;*---------------------------------------------------------------------*/
;*    trace-http-connect ...                                           */
;*---------------------------------------------------------------------*/
(define (trace-http-connect scd id sock req mode num)
   (with-access::http-request req ((rid id) method scheme host port path header)
      (trace 4 id "CONNECT.header"
	 ": " path " " 
	 (with-output-to-string (lambda () (write header))) "\n")
      (trace 1 rid (if (isa? req http-proxy-request)
		       (if (eq? mode 'keep-alive)
			   (format " REQUEST.prox (+~a)" num)
			   " REQUEST.prox")
		       (if (eq? mode 'keep-alive)
			   (format " REQUEST.serv (+~a)" num)
			   " REQUEST.serv"))
	 (if (>=fx (trace-level) 3)
	     (format " ~a~a: " thread (scheduler-stat scd))
	     ": ")
	 method " " scheme "://"
	 host ":"
	 port " "
	 (let ((s (string-for-read path)))
	    (if (and (=fx (trace-level) 1)
		     (>fx (string-length s) 80))
		(string-append (substring s 0 80) "...")
		s))
	 " "
	 (if (>=fx (trace-level) 2)
	     (with-access::http-request req (http) http)
	     ""))))

;*---------------------------------------------------------------------*/
;*    debug-request ...                                                */
;*---------------------------------------------------------------------*/
(define (debug-request req)
   (with-access::http-request req (method scheme host port path header)
      (with-output-to-port (current-error-port)
	 (lambda ()
	    (display* method " " scheme "://"
	       host ":"
	       port " "
	       (let ((s (string-for-read path)))
		  (if (>fx (string-length s) 80)
		      (string-append (substring s 0 80) "...")
		      s)))
	    (newline)))))

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
       (trace 3 id "SHUTDOWN"
	  (cond
	     ((isa? e &io-timeout-error)
	      " (keep-alive, timeout ellapsed)")
	     ((and (isa? e &io-parse-error)
		   (with-access::&io-parse-error e (obj)
		      (eof-object? obj)))
	      " (keep-alive, connection reset by peer)")
	     (else
	      " (keep-alive, parse error)")))
       ;; this one is a true error
       (begin
	  (when (isa? e &exception)
	     (trace 1 id "ABORT"
		" " (typeof e)
		(if (>=fx (trace-level) 4)
		    (format "~a:~a"
		       (socket-hostname sock)
		       (socket-port-number sock))
		    ""))
	     (trace 1 -1 "ERROR"
		(with-error-to-string
		   (lambda () (exception-notify e)))))
	  (when (and (isa? e &io-unknown-host-error) (not (socket-down? sock)))
	     (with-handler
		(lambda (e)
		   ;; this error handler is invoked when the attempt to
		   ;; notify the previous error to the client fails
		   (when (isa? e &error) (exception-notify e))
		   #unspecified)
		;; we will try to answer the error to the client
		(unless (isa? e &io-sigpipe-error)
		   (let* ((req (instantiate::http-request
				  (socket sock)))
			  (resp (instantiate::http-response-error
				   (err e))))
		      (http-response resp req sock)))))))

   ;; decrement the keep-alive number
   (when (eq? mode 'keep-alive) (keep-alive--))
   ;; abort this request
   (if (isa? e &io-connection-error)
       (socket-close sock)
       (socket-shutdown sock))
   ;; ignore the error
   #unspecified)

;*---------------------------------------------------------------------*/
;*    http-response-static? ...                                        */
;*---------------------------------------------------------------------*/
(define (http-response-static? resp)
   (or (isa? resp http-response-file)
       (isa? resp http-response-string)
       (isa? resp http-response-abort)))

;*---------------------------------------------------------------------*/
;*    stage-response ...                                               */
;*    -------------------------------------------------------------    */
;*    This stage is in charge of building a response to the received   */
;*    request.                                                         */
;*---------------------------------------------------------------------*/
(define (stage-response scd thread id req)
   (trace 3 id "RESPONSE" (format " ~a" thread))
   (with-stage-handler response-error-handler (scd req)
      (let ((resp (with-time (request->response req scd) id "RESPONSE")))
	 (cond
	    ((http-response-static? resp)
	     (stage scd thread stage-static-answer id req resp))
	    ((isa? resp http-response-async)
	     (stage scd thread stage-async-answer id req resp))
	    (else
	     (stage scd thread stage-dynamic-answer id req resp))))))

;*---------------------------------------------------------------------*/
;*    request->response ...                                            */
;*    -------------------------------------------------------------    */
;*    This function assumes that (HOP-FILTERS) returns a read-only     */
;*    immutable data structure. In other words, it assumes that no     */
;*    other thread can change the list (HOP-FILTERS) in the            */
;*    background. Because of this assumption, no lock is needed in     */
;*    this function.                                                   */
;*---------------------------------------------------------------------*/
(define (request->response::%http-response req::http-request scd::scheduler)
   (with-access::scheduler scd (filters)
      (let loop ((m req)
		 (fs filters))
	 (if (null? fs)
	     (instantiate::http-response-abort)
	     (let ((n ((cdar fs) m)))
		(cond
		   ((isa? n %http-response) n)
		   ((eq? n m) (loop m (cdr fs)))
		   ((isa? n http-request) (loop n (cdr fs)))
		   ((eq? n 'hop-resume) (loop m filters))
		   (else (loop m (cdr fs)))))))))

;*---------------------------------------------------------------------*/
;*    for ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (for range . body)
   (let ((for (gensym 'for))
	 (stop (gensym 'stop)))
      `(let ((,stop ,(caddr range)))
	  (let ,for ((,(car range) ,(cadr range)))
	       (when (<fx ,(car range) ,stop)
		  ,@body
		  (,for (+fx ,(car range) 1)))))))

;*---------------------------------------------------------------------*/
;*    response-error-handler ...                                       */
;*---------------------------------------------------------------------*/
(define (response-error-handler e scd req)
   ;; notify the error
   (with-access::http-request req (id)
      (trace 1 id " ERROR"
	 (call-with-output-string (lambda (p) (display e p)))))
   ;; when the error is a response, we transmit it to the next stage
   ;; when the error is an exception, we create a response that it
   ;; then transmitted to the next stage
   (with-handler
      (lambda (e)
	 (exception-notify e)
	 ;; there is nothing we can do but aborting the request
	 (socket-shutdown (with-access::http-request req (socket) socket))
	 #unspecified)
      ;; try to send the error message
      (if (isa? e %http-response)
	  (with-access::http-request req (socket)
	     (http-response e req socket))
	  (begin
	     (exception-notify e)
	     ;; generate a legal response for the next stage (although
	     ;; this response denotes the error).
	     (let ((resp (instantiate::http-response-error
			    (err e))))
		(with-access::http-request req (socket)
		   (http-response resp req socket)
		   ;; abort this request
		   (socket-shutdown socket)
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
(define (stage-async-answer scd thread id req respa)
   (with-stage-handler exec-error-handler (scd req)
      (with-access::http-request req (socket method scheme port host path connection)
	 (let* ((in (socket-input socket))
		(out (socket-output socket))
		(rtmt (input-port-timeout in))
		(wtmt (output-port-timeout out)))
	    (input-port-timeout-set! in 0)
	    (output-port-timeout-set! out 0)
	    ;; persistent and asynchronous responses have to duplicate
	    ;; the buffer they use as these buffers are reused by the 
	    ;; next requests
	    (input-port-buffer-set! in (string-copy (input-port-buffer in)))
	    (output-port-buffer-set! out (string-copy (output-port-buffer out)))
	    (with-access::http-response-async respa (async)
	       (let ((mutex (make-mutex))
		     (condv (make-condition-variable)))
		  (let ((resps #f))
		     (synchronize mutex
			(async
			   (lambda (r)
			      (synchronize mutex
				 (set! resps r)
				 (condition-variable-broadcast! condv))))
			(condition-variable-wait! condv mutex)
			(input-port-timeout-set! in rtmt)
			(output-port-timeout-set! out wtmt)
			(let ((tmt (with-handler
				      (lambda (e)
					 (exec-error-handler e scd req))
				      (stage-exec scd thread id req resps))))
			   (if (integer? tmt)
			       (stage-request scd thread id socket tmt 'keep-alive)
			       (begin
				  (socket-shutdown socket)
				  #f))))))))
	 #f)))

;*---------------------------------------------------------------------*/
;*    stage-exec-verb ...                                              */
;*---------------------------------------------------------------------*/
(define (stage-exec-verb scd thread req resp connection mode)
   (with-access::http-request req (id)
      (trace 3 id
	 (format " ~a" thread)
	 " load=" (scheduler-load scd)
	 (scheduler-stat scd)
	 ": " (typeof resp) " "
	 " [" (current-date) "] "
	 (if (and (eq? connection 'keep-alive) (>=fx (trace-level) 4))
	     (format " keep-alive [load=~a]" (scheduler-load scd))
	     connection))))

;*---------------------------------------------------------------------*/
;*    stage-exec ...                                                   */
;*---------------------------------------------------------------------*/
(define (stage-exec scd thread id req resp)
   ;; log
   (when (isa? resp http-response-abort)
      (trace 3 id "EXEC"
	 " load: " (scheduler-load scd)
	 (scheduler-stat scd)
	 (format " ~a" thread)
	 ": " (typeof resp)))
   (with-stage-handler exec-error-handler (scd req)
      (with-access::http-request req (socket method scheme port host path)
	 (let ((conn (with-time (http-response resp req socket) id "EXEC")))
	    ;; debug
	    (debug-thread-info-set! thread
	       (format "~a ~a://~a:~a~a... -> ~a ~a"
		  method scheme host port path
		  (typeof resp) conn))
	    (case conn
	       ((keep-alive)
		(let ((load (scheduler-load scd)))
		   (cond
		      ((>=fx load 90)
		       (when (>=fx (trace-level) 3)
			  (stage-exec-verb scd thread req resp conn
			     " END"))
		       (socket-shutdown socket)
		       #f)
		      ((>=fx load 70)
		       (when (>=fx (trace-level) 3)
			  (stage-exec-verb scd thread req resp conn
			     " KEEP-ALIVE"))
		       (keep-alive++)
		       1)
		      (else
		       (when (>=fx (trace-level) 3)
			  (stage-exec-verb scd thread req resp conn
			     " KEEP-ALIVE"))
		       (keep-alive++)
		       (with-access::scheduler scd (keep-alive-timeout)
			  keep-alive-timeout)))))
	       ((persistent)
		(when (>=fx (trace-level) 3)
		   (stage-exec-verb scd thread req resp conn
		      " PERSISTENT"))
		#f)
	       (else
		(when (>=fx (trace-level) 3)
		   (stage-exec-verb scd thread req resp conn " END"))
		(socket-shutdown socket)
		#f))))))

;*---------------------------------------------------------------------*/
;*    exec-error-handler ...                                           */
;*---------------------------------------------------------------------*/
(define (exec-error-handler e scd req)
   (with-access::http-request req (socket id)
      ;; first, close the socket, anycase
      (socket-shutdown socket)
      (unless (isa? e &io-sigpipe-error)
	 ;; signal the error, when this is an error
	 (trace 2 id "INTERRUPTED" (format " ~a" req))
	 ;; abort the request
	 (with-handler
	    (lambda (e2) #unspecified)
	    (begin
	       (exception-notify e)
	       #f))
	 #unspecified)))

