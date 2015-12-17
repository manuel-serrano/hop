;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/http_error.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:55:24 2004                          */
;*    Last change :  Thu Dec 17 05:38:32 2015 (serrano)                */
;*    Copyright   :  2004-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HTTP management                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_http-error
   
   (library web)
   
   (include "xml.sch")

   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_user
	    __hop_xml-types
	    __hop_xml
	    __hop_html-base
	    __hop_html-img
	    __hop_html-head
	    __hop_service
	    __hop_misc
	    __hop_hop
	    __hop_security)
   
   (export  (generic http-error::%http-response ::obj ::http-request)
	    (http-file-not-found file)
	    (http-service-not-found file ::http-request)
	    (http-permission-denied file)
	    (http-method-error obj)
	    (http-parse-error obj)
	    (http-bad-request obj)
	    (http-internal-error ::&exception ::obj ::obj)
	    (http-service-error ::http-request ::symbol ::bstring)
	    (http-invalidated-service-error ::http-request)
	    (http-corrupted-service-error ::http-request)
	    (http-warning msg #!optional dump)
	    (http-internal-warning e)
	    (http-service-unavailable obj)
	    (http-remote-error ::obj ::&exception ::http-request)
	    (http-gateway-timeout e)))

;*---------------------------------------------------------------------*/
;*    http-error-stack-header ...                                      */
;*---------------------------------------------------------------------*/
(define (http-error-stack-header err stack)
   
   (define (server-stack stack)
      (map (lambda (f)
	      (match-case f
		 ((?name ?loc . ?rest)
		  `(,name ,loc (type . server) (format . "$~a")))
		 (else
		  f)))
	 stack))
   
   (let ((defhd '((Cache-Control: . "no-cache") (Pragma: . "no-cache"))))
      (if (>fx (bigloo-debug) 0)
	  (let ((estack (url-path-encode
			   (obj->string (server-stack stack) 'hop-client)))
		(errobj (url-path-encode
			   (obj->string err 'hop-client))))
	     (cons* `(Hop-Error: . ,errobj)
		`(Hop-Debug-Stack: . ,estack)
		defhd))
	  defhd)))

;*---------------------------------------------------------------------*/
;*    http-error-header ...                                            */
;*---------------------------------------------------------------------*/
(define (http-error-header e)
   (if (isa? e &exception)
       (with-access::&exception e (stack)
	  (http-error-stack-header e stack))
       (http-error-stack-header e (get-trace-stack))))

;*---------------------------------------------------------------------*/
;*    http-start-line ...                                              */
;*---------------------------------------------------------------------*/
(define-macro (http-start-line req msg)
   (let ((tmp (gensym 'req)))
      `(let ((,tmp ,req))
	  (with-access::http-request ,tmp (http)
	     (if (eq? ,tmp 'HTTP/1.0)
		 ,(string-append "HTTP/1.0 " msg)
		 ,(string-append "HTTP/1.1 " msg))))))

;*---------------------------------------------------------------------*/
;*    hop-error-response ...                                           */
;*---------------------------------------------------------------------*/
(define (hop-error-response e)
   (instantiate::http-response-hop
      (start-line "HTTP/1.1 500 Internal Server Error")
      (backend (hop-xml-backend-secure))
      (charset (hop-charset))
      (header (http-error-header e))
      (bodyp #t)
      (content-type "application/x-hop")
      (value e)))

;*---------------------------------------------------------------------*/
;*    http-error ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (http-error e::obj req)
   (with-access::xml-backend (hop-xml-backend) (mime-type)
      (let* ((ths "text-align: right; color: #777; vertical-align: top;")
	     (msg (with-access::http-request req (host port path)
		     (<TABLE> :style "font-size: 12pt"
			(<COLGROUP> (<COL> :width "0*"))
			(<TR>
			   (<TD> :style ths "host:")
			   (<TD> 
			      (<TT> :style "font-size: 10pt; font-weight: bold"
				 host ":" port)))
			(<TR>
			   (<TD> :style ths "path:")
			   (<TD> (<TT> :style "font-size: 9pt"
				    (html-string-encode path)))))))
	     (s (cond
		   ((string? e)
		    e)
		   ((isa? e &exception)
		    (with-error-to-string
		       (lambda () (exception-notify e))))
		   (else
		    (with-output-to-string
		       (lambda () (display e)))))))
	 (instantiate::http-response-xml
	    (start-line "HTTP/1.1 500 Internal Server Error")
	    (header (http-error-header e))
	    (backend (hop-xml-backend))
	    (content-type mime-type)
	    (charset (hop-charset))
	    (xml (<HTML-ERROR> :class "error"
		    :title "Server Error"
		    :msg msg
		    :request req
		    (<DIV> :hssclass "hop-error-trace"
		       (<DIV> "Hop server stack:")
		       (<PRE> :hssclass "hop-error-notification"
			  (html-string-encode s)))))))))

;*---------------------------------------------------------------------*/
;*    http-error ::&exception ...                                      */
;*---------------------------------------------------------------------*/
(define-method (http-error e::&exception req)
   (let* ((ths "text-align: right; color: #777; vertical-align: top;")
	  (msg (with-access::http-request req (host port path)
		  (<TABLE> :style "font-size: 12pt"
		     (<COLGROUP> (<COL> :width "0*"))
		     (<TR>
			(<TD> :style ths "host:")
			(<TD> 
			   (<TT> :style "font-size: 10pt; font-weight: bold"
			      host ":" port)))
		     (<TR>
			(<TD> :style ths "path:")
			(<TD> (<TT> :style "font-size: 9pt"
				 (html-string-encode path))))))))
      (http-internal-error e msg req)))

;*---------------------------------------------------------------------*/
;*    http-error ::&io-unknown-host-error ...                          */
;*---------------------------------------------------------------------*/
(define-method (http-error e::&io-unknown-host-error req)
   (with-access::xml-backend (hop-xml-backend) (mime-type)
      (instantiate::http-response-xml
	 (start-line "HTTP/1.1 404 Not Found")
	 (header (http-error-header e))
	 (backend (hop-xml-backend))
	 (content-type mime-type)
	 (charset (hop-charset))
	 (xml (with-access::&error e (msg obj)
		 (<HTML-ERROR> :class "remote"
		    :title "Unknown Host!"
		    :msg  (list msg ": " (<TT> :class "notfound" obj))
		    :request req
		    (<PRE> :data-hss-class "hop-exception-notification"
		       (with-error-to-string
			  (lambda ()
			     (exception-notify e))))))))))

;*---------------------------------------------------------------------*/
;*    http-error ::&io-file-not-found-error ...                        */
;*---------------------------------------------------------------------*/
(define-method (http-error e::&io-file-not-found-error req)
   (with-access::&error e (obj)
      (http-file-not-found obj)))

;*---------------------------------------------------------------------*/
;*    http-error ::&io-error ...                                       */
;*---------------------------------------------------------------------*/
(define-method (http-error e::&io-error req)
   (http-io-error e req))

;*---------------------------------------------------------------------*/
;*    http-error ::&hop-method-error ...                               */
;*---------------------------------------------------------------------*/
(define-method (http-error e::&hop-method-error req)
   (with-access::&error e (obj)
      (http-method-error obj)))

;*---------------------------------------------------------------------*/
;*    http-error ::&hop-security-error ...                             */
;*---------------------------------------------------------------------*/
(define-method (http-error e::&hop-security-error req)
   (let ((s (with-error-to-string (lambda () (exception-notify e)))))
      (with-access::xml-backend (hop-xml-backend) (mime-type)
	 (instantiate::http-response-xml
	    (start-line "HTTP/1.1 200 ok")
	    (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
	    (backend (hop-xml-backend))
	    (content-type mime-type)
	    (charset (hop-charset))
	    (xml (with-access::&error e (msg)
		    (<HTML-ERROR> :class "security"
		       :title "Security Error"
		       :msg msg
		       :request req
		       (<PRE> (html-string-encode s)))))))))

;*---------------------------------------------------------------------*/
;*    http-error ::&hop-autoload-error ...                             */
;*---------------------------------------------------------------------*/
(define-method (http-error e::&hop-autoload-error req)
   (let ((s (with-error-to-string
	       (lambda ()
		  (with-access::&hop-autoload-error e (obj)
		     (if (isa? obj &exception)
			 (exception-notify obj)
			 (exception-notify e)))))))
      (http-internal-error e s req)))

;*---------------------------------------------------------------------*/
;*    <HTML-ERROR> ...                                                 */
;*---------------------------------------------------------------------*/
(define-tag <HTML-ERROR> ((class "error")
			  (title "error")
			  (request #f)
			  (msg "")
			  body)
   (<HTML>
      (if (isa? request http-proxy-request)
	  ;; this is a proxy request
	  (with-access::http-request request (socket)
	     (<HEAD> :base (format "http://~a:~a"
			      (socket-hostname socket)
			      (hop-port))))
	  ;; this is a local request
	  (<HEAD>))
      (<BODY> :data-hss-class "hop-exception"
	 (<DIV> :data-hss-class "hop-exception" :class class
	    (<SPAN> :data-hss-class "hop-exception-img")
	    (<DIV> :data-hss-class "hop-exception-body"
	       (<DIV> :data-hss-class "hop-exception-title" title)
	       (<DIV> :data-hss-class "hop-exception-msg"
		  (<TABLE> :style "font-weight: normal"
		     (<TR>
			(<TD>
			   (<SPAN> :style "color: #777; font-weight: bold"
			      msg))
			(<TR> (<TD> body))))))))))

;*---------------------------------------------------------------------*/
;*    http-file-not-found ...                                          */
;*---------------------------------------------------------------------*/
(define (http-file-not-found file)
   (instantiate::http-response-xml
      (start-line "HTTP/1.1 404 Not Found")
      (header (http-error-header (format "File not found ~s" file)))
      (backend (hop-xml-backend))
      (charset (hop-charset))
      (xml (<HTML-ERROR> :class "notfound"
	      :title "File Not Found!"
	      :msg (<TT> :class "notfound" file)))))

;*---------------------------------------------------------------------*/
;*    http-service-not-found ...                                       */
;*---------------------------------------------------------------------*/
(define (http-service-not-found file req)
   
   (define (illegal-service key msg)
      (with-access::xml-backend (hop-xml-backend) (mime-type)
	 (instantiate::http-response-xml
	    (start-line "HTTP/1.1 404 Not Found")
	    (header (http-error-header (format "Service not found ~s" file)))
	    (backend (hop-xml-backend))
	    (content-type mime-type)
	    (xml (<HTML-ERROR> :class "notfound"
		    :title (format "~a Service!" (string-capitalize key))
		    :msg (<TT> :class "notfound" file)
		    :request req
		    msg)))))

   (define (illegal-service-message msg)
      (format "You are trying to execute an ~a service!
<br><br>
This is generally due to a restart of the server.
On restart the server invalidates all anonymous services that hence
can no longer be executed.
Reloading the page is the only way to fix this problem." msg))
   
   (let ((svc (make-file-name (hop-service-base) (hop-service-weblet-name))))
      (cond
	 ((expired-service-path? file)
	  (illegal-service "expired"
			   "You are trying to execute an expired service!
<br><br>
This happens because at creation time, the service has been registered with
a timeout which has now expired. The service is then no longer available."))
	 ((substring-at? file svc 0)
	  (illegal-service "invalidated"
			   (illegal-service-message "invalidated")))
	 (else
	  (illegal-service "unbound"
			   "This service is unknown!")))))

;*---------------------------------------------------------------------*/
;*    http-permission-denied ...                                       */
;*---------------------------------------------------------------------*/
(define (http-permission-denied file)
   (instantiate::http-response-string
      (start-line "HTTP/1.0 403 Forbidden")
      (charset (hop-locale))
      (body (format "Permission denied: ~s" file))))

;*---------------------------------------------------------------------*/
;*    http-method-error ...                                            */
;*---------------------------------------------------------------------*/
(define (http-method-error obj)
   (instantiate::http-response-string
      (start-line "HTTP/1.0 501 Not Implemented")
      (charset (hop-locale))
      (body (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html><body>Method not implemented ~a</body></html>"
		    obj))))

;*---------------------------------------------------------------------*/
;*    http-parse-error ...                                             */
;*---------------------------------------------------------------------*/
(define (http-parse-error obj)
   (instantiate::http-response-string
      (start-line "HTTP/1.0 400 Bad Request")
      (charset (hop-locale))
      (body (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html><body>Parse error in HTTP request on token <tt>~a</tt>/body></html>"
		    obj))))

;*---------------------------------------------------------------------*/
;*    http-bad-request ...                                             */
;*---------------------------------------------------------------------*/
(define (http-bad-request obj)
   (instantiate::http-response-string
      (start-line "HTTP/1.0 400 Bad Request")
      (charset (hop-locale))
      (body (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html><body>Bad request <tt><pre>~a</pre></tt></body></html>" obj))))

;*---------------------------------------------------------------------*/
;*    http-internal-error ...                                          */
;*---------------------------------------------------------------------*/
(define (http-internal-error e msg req)
   (let ((s (with-error-to-string (lambda () (exception-notify e)))))
      (with-access::xml-backend (hop-xml-backend) (mime-type)
	 (instantiate::http-response-xml
	    (start-line "HTTP/1.1 500 Internal Server Error")
	    (header (http-error-header e))
	    (backend (hop-xml-backend))
	    (content-type mime-type)
	    (charset (hop-charset))
	    (xml (<HTML-ERROR>
		    :class (if (isa? e &io-timeout-error) "timeour" "error")
		    :title "Server Error"
		    :msg msg
		    (<DIV> :data-hss-class "hop-exception-trace"
		       (<DIV> "Hop server stack:")
		       (<PRE> :data-hss-class "hop-exception-notification"
			  (html-string-encode s)))))))))
   
;*---------------------------------------------------------------------*/
;*    http-service-error ...                                           */
;*---------------------------------------------------------------------*/
(define (http-service-error req service m)
   (with-access::xml-backend (hop-xml-backend) (mime-type)
      (instantiate::http-response-xml
	 (start-line "HTTP/1.1 400 Bad Request")
	 (backend (hop-xml-backend))
	 (content-type mime-type)
	 (charset (hop-charset))
	 (xml (<HTML-ERROR>
		 :title "Hop Service Error"
		 :msg (with-access::http-request req (host port)
			 (list "An error occurred while responding to "
			    (<TT> "http://" host
				  ":" port
				  (hop-service-base) service)
			    ", " (<TT> (hop-request-service-name req))))
		 (<PRE> :data-hss-class "hop-exception-notification"
		    (html-string-encode m)))))))

;*---------------------------------------------------------------------*/
;*    http-invalidated-service-error ...                               */
;*---------------------------------------------------------------------*/
(define (http-invalidated-service-error req)
   (with-access::xml-backend (hop-xml-backend) (mime-type)
      (instantiate::http-response-xml
	 (start-line "HTTP/1.0 404 Not Found")
	 (header (http-error-header "Invalidated service"))
	 (backend (hop-xml-backend))
	 (content-type mime-type)
	 (charset (hop-charset))
	 (xml (<HTML-ERROR> :class "stop"
		 :title "Invalidated service!"
		 :msg (<TT> (with-access::http-request req (path) path))
		 (<DIV> "You are trying to executed an unexisting or invalidated service!
<br><br>
This is generally due to a restart of the server.
On restart the server invalidates all anonymous services that hence
can no longer be executed.
Reloading the page is the only way to fix this problem."))))))

;*---------------------------------------------------------------------*/
;*    http-corrupted-service-error ...                                 */
;*---------------------------------------------------------------------*/
(define (http-corrupted-service-error req)
   (with-access::xml-backend (hop-xml-backend) (mime-type)
      (instantiate::http-response-xml
	 (start-line "HTTP/1.0 404 Not Found")
	 (header (http-error-header "Corrupted service"))
	 (backend (hop-xml-backend))
	 (content-type mime-type)
	 (charset (hop-charset))
	 (xml (<HTML-ERROR>
		 :title (list "Corrupted service!"
			   (<TT> (with-access::http-request req (path) path)))
		 (<DIV> "You are trying to executed an corrupted service!"))))))

;*---------------------------------------------------------------------*/
;*    http-internal-warning ...                                        */
;*---------------------------------------------------------------------*/
(define (http-internal-warning e)
   (let ((s (with-error-to-string (lambda () (warning-notify e)))))
      (instantiate::http-response-string
	 (start-line "HTTP/1.0 400 Bad Request")
	 (charset (hop-locale))
	 (body (format "<HTML><BODY><PRE> ~a </PRE></BODY></HTML>" s)))))
   
;*---------------------------------------------------------------------*/
;*    http-warning ...                                                 */
;*---------------------------------------------------------------------*/
(define (http-warning msg #!optional dump)
   (with-access::xml-backend (hop-xml-backend) (mime-type)
      (instantiate::http-response-xml
	 (start-line "HTTP/1.1 200 ok")
	 (header (http-error-header #f))
	 (backend (hop-xml-backend))
	 (content-type mime-type)
	 (charset (hop-charset))
	 (xml (<HTML-ERROR> :class "warning"
		 :title "Warning"
		 :msg (<TT> msg)
		 (<PRE> :data-hss-class "hop-exception-notification"
		    dump))))))

;*---------------------------------------------------------------------*/
;*    http-service-unavailable ...                                     */
;*---------------------------------------------------------------------*/
(define (http-service-unavailable e)
   (with-access::xml-backend (hop-xml-backend) (mime-type)
      (instantiate::http-response-xml
	 (start-line "HTTP/1.1 503 Service Unavailable")
	 (header (http-error-header e))
	 (backend (hop-xml-backend))
	 (content-type mime-type)
	 (charset (hop-charset))
	 (xml (<HTML-ERROR>
		 :title "Service Unavailable"
		 :msg (if (isa? e http-request)
			  (with-access::http-request e (path) path)
			  e)
		 "")))))

;*---------------------------------------------------------------------*/
;*    http-remote-error ...                                            */
;*---------------------------------------------------------------------*/
(define (http-remote-error host e req)
   (let ((s (with-error-to-string (lambda () (exception-notify e)))))
      (with-access::xml-backend (hop-xml-backend) (mime-type)
	 (instantiate::http-response-xml
	    (start-line "HTTP/1.1 503 Service Unavailable")
	    (header (http-error-header e))
	    (backend (hop-xml-backend))
	    (content-type mime-type)
	    (charset (hop-charset))
	    (xml (<HTML-ERROR>
		    :class (if (isa? e &io-timeout-error) "timeout" "remote")
		    :title "Remote Error"
		    :msg (list "An error occurred while talking to a remote host: "
			    (<TT> host))
		    :request req
		    (<PRE> :data-hss-class "hop-exception-notification"
		       (html-string-encode s))))))))

;*---------------------------------------------------------------------*/
;*    http-io-error ...                                                */
;*---------------------------------------------------------------------*/
(define (http-io-error e req)
   (let ((s (with-error-to-string (lambda () (exception-notify e)))))
      (with-access::xml-backend (hop-xml-backend) (mime-type)
	 (instantiate::http-response-xml
	    (start-line "HTTP/1.1 404 Not Found")
	    (header (http-error-header e))
	    (backend (hop-xml-backend))
	    (content-type mime-type)
	    (charset (hop-charset))
	    (xml (<HTML-ERROR>
		    :class (if (isa? e &io-timeout-error) "timeout" "error")
		    :title "IO Error"
		    :msg (list "Error type: " (<TT> (typeof e)))
		    :request req
		    (<PRE>  :data-hss-class "hop-exception-notification"
		       (html-string-encode s))))))))

;*---------------------------------------------------------------------*/
;*    http-gateway-timeout ...                                         */
;*---------------------------------------------------------------------*/
(define (http-gateway-timeout e)
   (instantiate::http-response-string
      (start-line "HTTP/1.0 502 Bad Gateway")
      (charset (hop-locale))
      (body (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html><body>Gateway Timeout ~a</body></html>" e))))
