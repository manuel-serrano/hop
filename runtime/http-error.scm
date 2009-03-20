;*=====================================================================*/
;*    serrano/prgm/project/hop/1.10.x/runtime/http-error.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:55:24 2004                          */
;*    Last change :  Tue Dec 16 08:27:28 2008 (serrano)                */
;*    Copyright   :  2004-08 Manuel Serrano                            */
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
	    __hop_xml
	    __hop_img
	    __hop_hop-extra
	    __hop_service
	    __hop_misc
	    __hop_hop)
   
   (export  (http-request-error::%http-response ::&error)
	    (http-error::%http-response ::&error ::http-request)
	    (http-unknown-host host)
	    (http-file-not-found file)
	    (http-service-not-found file)
	    (http-permission-denied file)
	    (http-method-error obj)
	    (http-parse-error obj)
	    (http-bad-request obj)
	    (http-internal-error ::obj ::obj)
	    (http-service-error ::http-request ::symbol ::bstring)
	    (http-invalidated-service-error ::http-request)
	    (http-corrupted-service-error ::http-request)
	    (http-warning msg #!optional dump)
	    (http-internal-warning e)
	    (http-service-unavailable obj)
	    (http-remote-error ::obj ::&exception)
	    (http-gateway-timeout e)))

;*---------------------------------------------------------------------*/
;*    http-request-error ...                                           */
;*---------------------------------------------------------------------*/
(define (http-request-error e::&error)
   (cond
      ((&io-unknown-host-error? e)
       (http-unknown-host (&error-obj e)))
      ((&hop-method-error? e)
       (http-method-error (&error-obj e)))
      ((&io-error? e)
       (http-io-error e))
      (else
       (http-bad-request
	(with-error-to-string
	   (lambda ()
	      (error-notify e)))))))

;*---------------------------------------------------------------------*/
;*    http-error ...                                                   */
;*---------------------------------------------------------------------*/
(define (http-error e::&error req::http-request)
   (let ((msg (<TABLE>
		 (<TR>
		    (<TD> "An error occured while responding to"))
		 (<TR>
		    (<TD>
		       :class "request-info"
		       (<TABLE>
			  (<COLGROUP> (<COL> :width "0*"))
			  (<TR>
			     (<TH> :align 'right "host:")
			     (<TD> (<TT> (http-request-host req))))
			  (<TR>
			     (<TH> :align 'right "port:")
			     (<TD> (<TT> (http-request-port req))))
			  (<TR>
			     (<TH> :align 'right "path:")
			     (<TD> (<TT> (http-request-path req))))))))))
      (cond
	 ((&io-unknown-host-error? e)
	  (http-unknown-host (&error-obj e)))
	 ((&io-file-not-found-error? e)
	  (http-file-not-found (&error-obj e)))
	 ((&io-error? e)
	  (http-io-error e))
	 (else
	  (http-internal-error e msg)))))

;*---------------------------------------------------------------------*/
;*    <EHEAD> ...                                                      */
;*---------------------------------------------------------------------*/
(define (<EHEAD> req)
   (if (http-proxy-request? req) 
       ;; this is a proxy request
       (<HEAD> :include "hop-error"
	  :base (format "http://~a:~a" (hostname) (hop-port)))
       ;; this is a local request
       (<HEAD> :include "hop-error")))
   
;*---------------------------------------------------------------------*/
;*    <EIMG> ...                                                       */
;*---------------------------------------------------------------------*/
(define (<EIMG> #!key src req)
   (<IMG> :style "padding: 20px;"
      :src (if (http-proxy-request? req)
	       (format "http://~a:~a~a"
		       (hostname)
		       (hop-port)
		       (make-file-name (hop-icons-directory) src))
	       (make-file-name (hop-icons-directory) src))))

;*---------------------------------------------------------------------*/
;*    <ETD> ...                                                        */
;*---------------------------------------------------------------------*/
(define-xml-compound <ETD> ((id #unspecified string)
			    (class #f)
			    (style "" string)
			    (valign 'middle)
			    body)
   (let* ((default (format "vertical-align: ~a; text-align: left;"
			   valign))
	  (add (cond
		  ((not class)
		   "")
		  ((string=? class "title")
		   "font-size: x-large;
  font-weight: bold;
  padding-bottom: 1px;
  color: red;")
		  ((string=? class "msg")
		   "width: 35em;
  margin-bottom: 20px;
  margin-top: 20px;
  padding-bottom: 20px;
  padding-top: 20px;
  border-bottom: 1px solid #bbb;
  border-top: 1px solid #bbb;
  font-family: sans-serif;")
		  ((string=? class "dump")
		   "padding-top: 20px;")
		  (else
		   ""))))
      (apply <TD> :id (xml-make-id id 'ETD) :style (string-append style default add) body)))

;*---------------------------------------------------------------------*/
;*    <ETABLE> ...                                                     */
;*---------------------------------------------------------------------*/
(define (<ETABLE> . args)
   (apply <TABLE>
	  :class "error"
	  :style "width: 50em; border: 1px solid #bbb; background: #fff; -moz-border-radius: 0.5em"
	  (<COLGROUP> (<COL> :width "0*"))
	  args))

;*---------------------------------------------------------------------*/
;*    title-style ...                                                  */
;*---------------------------------------------------------------------*/
(define title-style
   "font-size: x-large; font-weight: bold; padding-bottom: 1px;")

;*---------------------------------------------------------------------*/
;*    *anonymous-request* ...                                          */
;*---------------------------------------------------------------------*/
(define *anonymous-request* #f)

;*---------------------------------------------------------------------*/
;*    anonymous-request ...                                            */
;*---------------------------------------------------------------------*/
(define (anonymous-request)
   (unless (http-request? *anonymous-request*)
      (set! *anonymous-request*
	    (instantiate::http-server-request
	       (http 'HTTP/1.0)
	       (connection 'close)
	       (user (anonymous-user)))))
   *anonymous-request*)

;*---------------------------------------------------------------------*/
;*    http-start-line ...                                              */
;*---------------------------------------------------------------------*/
(define-macro (http-start-line req msg)
   (let ((tmp (gensym 'req)))
      `(let ((,tmp ,req))
	  (if (eq? (http-request-http ,tmp) 'HTTP/1.0)
	      ,(string-append "HTTP/1.0 " msg)
	      ,(string-append "HTTP/1.1 " msg)))))

;*---------------------------------------------------------------------*/
;*    http-unknown-host ...                                            */
;*---------------------------------------------------------------------*/
(define (http-unknown-host host)
   (let ((req (or (current-request) (anonymous-request))))
      (instantiate::http-response-hop
	 (start-line (http-start-line req "404 Not Found"))
	 (request req)
	 (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
	 (backend (hop-xml-backend))
	 (content-type (xml-backend-mime-type (hop-xml-backend)))
	 (charset (hop-charset))
	 (xml (<HTML>
		 (<EHEAD> (current-request))
		 (<BODY>
		    (<CENTER>
		       (<ETABLE>
			  (<TR>
			     (<ETD> :class "logo" :valign 'top
				(<EIMG> :src "error2.png" :req req))
			     (<ETD>
				(<TABLE> :width "100%"
				   (<TR> (<ETD> :class "title" "Unknown Host"))
				   (<TR> (<ETD> :class "msg"
					    (<SPAN> :class "filenotfound"
					       host))))))))))))))

;*---------------------------------------------------------------------*/
;*    http-file-not-found ...                                          */
;*---------------------------------------------------------------------*/
(define (http-file-not-found file)
   (let ((req (or (current-request) (anonymous-request))))
      (instantiate::http-response-hop
	 (request req)
	 (start-line (http-start-line req "404 Not Found"))
	 (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
	 (backend (hop-xml-backend))
	 (charset (hop-charset))
	 (xml (<HTML>
		 (<EHEAD> (current-request))
		 (<BODY>
		    (<CENTER>
		       (<ETABLE>
			  (<TR>
			     (<ETD> :class "logo" :valign 'top
				(<EIMG> :src "error2.png" :req req))
			     (<ETD>
				(<TABLE> :width "100%"
				   (<TR> (<ETD> :class "title"
					    "File not found!"))
				   (<TR> (<ETD> :class "msg"
					    (<SPAN> :class "filenotfound"
					       file))))))))))))))

;*---------------------------------------------------------------------*/
;*    http-service-not-found ...                                       */
;*---------------------------------------------------------------------*/
(define (http-service-not-found file)
   (define (illegal-service key msg)
      (let ((req (or (current-request) (anonymous-request))))
	 (instantiate::http-response-hop
	    (request req)
	    (start-line (http-start-line req "404 Not Found"))
	    (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
	    (backend (hop-xml-backend))
	    (content-type (xml-backend-mime-type (hop-xml-backend)))
	    (xml (<HTML>
		    (<EHEAD> (current-request))
		    (<BODY>
		       (<CENTER>
			  (<ETABLE>
			     (<TR>
				(<ETD> :class "logo" :valign 'top
				   (<EIMG> :src "warning.png" :req req))
				(<ETD>
				   (<TABLE> :width "100%"
				      (<TR> (<ETD> :class "title"
					       (format "~a service!"
						       (string-capitalize key))))
				      (<TR> (<ETD> :class "msg"
					       (<SPAN> :class "filenotfound"
						  file)))
				      (<TR> (<ETD> :class "dump"
					       (<SPAN> msg))))))))))))))
   (define (illegal-service-message msg)
      (format "You are trying to execute an ~a service!
<br><br>
This is generally due to a restart of the server.
On restart the server invalidates all anonymous services that hence
can no longer be executed.<br><br>
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
	  (illegal-service "unknown"
			   (illegal-service-message "unknown"))))))

;*---------------------------------------------------------------------*/
;*    http-permission-denied ...                                       */
;*---------------------------------------------------------------------*/
(define (http-permission-denied file)
   (instantiate::http-response-error
      (request (anonymous-request))
      (start-line "HTTP/1.0 403 Forbidden")
      (charset (hop-locale))
      (body (format "Permission denied: ~s" file))))

;*---------------------------------------------------------------------*/
;*    http-method-error ...                                            */
;*---------------------------------------------------------------------*/
(define (http-method-error obj)
   (instantiate::http-response-error
      (request (anonymous-request))
      (start-line "HTTP/1.0 501 Not Implemented")
      (charset (hop-locale))
      (body (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html><body>Method not implemented ~a</body></html>"
		    obj))))

;*---------------------------------------------------------------------*/
;*    http-parse-error ...                                             */
;*---------------------------------------------------------------------*/
(define (http-parse-error obj)
   (instantiate::http-response-error
      (request (anonymous-request))
      (start-line "HTTP/1.0 400 Bad Request")
      (charset (hop-locale))
      (body (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html><body>Parse error in HTTP request on token <tt>~a</tt>/body></html>"
		    obj))))

;*---------------------------------------------------------------------*/
;*    http-bad-request ...                                             */
;*---------------------------------------------------------------------*/
(define (http-bad-request obj)
   (instantiate::http-response-error
      (request (anonymous-request))
      (start-line "HTTP/1.0 400 Bad Request")
      (charset (hop-locale))
      (body (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html><body>Bad request <tt><pre>~a</pre></tt></body></html>" obj))))

;*---------------------------------------------------------------------*/
;*    http-internal-error ...                                          */
;*---------------------------------------------------------------------*/
(define (http-internal-error e msg)
   (let ((s (cond
	       ((string? e)
		e)
	       ((&exception? e)
		(with-error-to-string (lambda () (exception-notify e))))
	       (else
		(with-output-to-string (lambda () (display e))))))
	 (req (or (current-request) (anonymous-request))))
      (instantiate::http-response-hop
	 (request req)
	 (start-line (http-start-line req "500 Internal Server Error"))
	 (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
	 (backend (hop-xml-backend))
	 (content-type (xml-backend-mime-type (hop-xml-backend)))
	 (charset (hop-charset))
	 (xml (<HTML>
		 (<EHEAD> (current-request))
		 (<BODY>
		    (<CENTER>
		       (<ETABLE>
			  (<TR>
			     (<ETD> :class "logo" :valign 'top
				(<EIMG> :src (if (&io-timeout-error? e)
						 "timeout.png"
						 "error.png")
				   :req req))
			     (<ETD>
				(<TABLE> :width "100%"
				   (<TR>
				      (<ETD> :class "title" "Internal Error"))
				   (<TR>
				      (<ETD> :class "msg" msg))
				   (<TR>
				      (<ETD> :class "dump"
					     (<PRE>
						(html-string-encode s)))))))))))))))
   
;*---------------------------------------------------------------------*/
;*    http-service-error ...                                           */
;*---------------------------------------------------------------------*/
(define (http-service-error req service m)
   (let ((info (<TABLE>
		  (<TR>
		     (<ETD> "An error occured while responding to"))
		  (<TR>
		     (<ETD>
			:class "request-info"
			(<TABLE> :width "100%"
			   (<COLGROUP> (<COL> :width "0*"))
			   (<TR>
			      (<TH> :align 'right "host:")
			      (<ETD> (<TT> (http-request-host req))))
			   (<TR>
			      (<TH> :align 'right "port:")
			      (<ETD> (<TT> (http-request-port req))))
			   (<TR>
			      (<TH> :align 'right "service:")
			      (<ETD> (<TT> service)))
			   (<TR>
			      (<TH> :align 'right "filter:")
			      (<ETD> (<TT> (hop-request-service-name req))))))))))
      (instantiate::http-response-hop
	 (request req)
	 (start-line (http-start-line req "400 Bad Request"))
	 (backend (hop-xml-backend))
	 (content-type (xml-backend-mime-type (hop-xml-backend)))
	 (charset (hop-charset))
	 (xml (<HTML>
		 (<EHEAD> req)
		 (<BODY>
		    (<CENTER>
		       (<ETABLE>
			  (<TR>
			     (<ETD> :class "logo" :valign 'top
				(<EIMG> :src "error.png" :req req))
			     (<ETD>
				(<TABLE> :width "100%"
				   (<TR>
				      (<ETD> :class "title"
					     "Hop service error"))
				   (<TR>
				      (<ETD> :class "msg"
					     info))
				   (<TR>
				      (<ETD> :class "dump"
					     (<PRE>
						(html-string-encode m)))))))))))))))

;*---------------------------------------------------------------------*/
;*    http-invalidated-service-error ...                               */
;*---------------------------------------------------------------------*/
(define (http-invalidated-service-error req)
   (instantiate::http-response-hop
      (request req)
      (start-line "HTTP/1.0 404 Not Found")
      (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
      (backend (hop-xml-backend))
      (content-type (xml-backend-mime-type (hop-xml-backend)))
      (charset (hop-charset))
      (xml (<HTML>
	      (<EHEAD> req)
	      (<BODY>
		 (<CENTER>
		    (<ETABLE>
		       (<TR>
			  (<ETD> :class "logo" :valign 'top
			     (<EIMG> :src "stop.png" :req req))
			  (<ETD>
			     (<TABLE> :width "100%"
				(<TR> (<ETD> :class "title"
					     "Invalidated service!"
					     ))
				(<TR> (<ETD> :class "msg"
					     (<SPAN> :class "filenotfound"
						     (<TT> "(service ...)"))))
				(<TR> (<ETD> :class "dump"
					     (<SPAN> "You are trying to executed an unexisting or invalidated service!
<br><br>
This is generally due to a restart of the server.
On restart the server invalidates all anonymous services that hence
can no longer be executed.<br><br>
Reloading the page is the only way to fix this problem.")))))))))))))

;*---------------------------------------------------------------------*/
;*    http-corrupted-service-error ...                                 */
;*---------------------------------------------------------------------*/
(define (http-corrupted-service-error req)
   (instantiate::http-response-hop
      (request req)
      (start-line "HTTP/1.0 404 Not Found")
      (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
      (backend (hop-xml-backend))
      (content-type (xml-backend-mime-type (hop-xml-backend)))
      (charset (hop-charset))
      (xml (<HTML>
	      (<EHEAD> req)
	      (<BODY>
		 (<CENTER>
		    (<ETABLE>
		       (<TR>
			  (<ETD> :class "logo" :valign 'top
			     (<EIMG> :src "error.png" :req req))
			  (<ETD>
			     (<TABLE> :width "100%"
				(<TR> (<ETD> :class "title"
					     "Corrupted service!"
					     ))
				(<TR> (<ETD> :class "msg"
					     (<SPAN> :class "filenotfound"
						     (<TT> "(service ...)"))))
				(<TR> (<ETD> :class "dump"
					     (<SPAN> "You are trying to executed an corrupted service!")))))))))))))

;*---------------------------------------------------------------------*/
;*    http-internal-warning ...                                        */
;*---------------------------------------------------------------------*/
(define (http-internal-warning e)
   (let ((s (with-error-to-string (lambda () (warning-notify e)))))
      (instantiate::http-response-error
	 (request (anonymous-request))
	 (start-line "HTTP/1.0 400 Bad Request")
	 (charset (hop-locale))
	 (body (format "<HTML><BODY><PRE> ~a </PRE></BODY></HTML>" s)))))
   
;*---------------------------------------------------------------------*/
;*    http-warning ...                                                 */
;*---------------------------------------------------------------------*/
(define (http-warning msg #!optional dump)
   (let ((req (or (current-request) (anonymous-request))))
      (instantiate::http-response-hop
	 (request req)
	 (start-line (http-start-line req "200 ok"))
	 (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
	 (backend (hop-xml-backend))
	 (content-type (xml-backend-mime-type (hop-xml-backend)))
	 (charset (hop-charset))
	 (xml (<HTML>
		 (<EHEAD> (current-request))
		 (<BODY>
		    (<CENTER>
		       (<ETABLE>
			  (<TR>
			     (<ETD> :class "logo" :valign 'top
				(<EIMG> :src "warning.png" :req req))
			     (<ETD>
				(<TABLE> :width "100%"
				   (<TR> (<ETD> :class "title"
					    "Warning"))
				   (<TR> (<ETD> :class "msg"
					    msg))
				   (<TR> (<ETD> :class "dump"
					    (or dump ""))))))))))))))

;*---------------------------------------------------------------------*/
;*    http-service-unavailable ...                                     */
;*---------------------------------------------------------------------*/
(define (http-service-unavailable e)
   (let ((req (if (http-request? e)
		   e
		   (or (current-request) (anonymous-request)))))
      (instantiate::http-response-hop
	 (request req)
	 (start-line (http-start-line req "503 Service Unavailable"))
	 (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
	 (backend (hop-xml-backend))
	 (content-type (xml-backend-mime-type (hop-xml-backend)))
	 (charset (hop-charset))
	 (xml (<HTML>
		 (<EHEAD> (current-request))
		 (<BODY>
		    (<CENTER>
		       (<ETABLE>
			  (<TR>
			     (<ETD> :class "logo" :valign 'top
				(<EIMG> :src "error.png" :req req))
			     (<ETD>
				(<TABLE> :width "100%"
				   (<TR> (<ETD> :class "title" "Service Unavailable"))
				   (<TR> (<ETD> :class "msg" ""))
				   (<TR> (<ETD> :class "dump"
					    (<PRE> (if (http-request? e)
						       (http-request-path e)
						       e)))))))))))))))

;*---------------------------------------------------------------------*/
;*    http-remote-error ...                                            */
;*---------------------------------------------------------------------*/
(define (http-remote-error host e)
   (let ((s (with-error-to-string (lambda () (error-notify e))))
	 (req (current-request)))
      (instantiate::http-response-hop
	 (request req)
	 (start-line (http-start-line req "503 Service Unavailable"))
	 (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
	 (backend (hop-xml-backend))
	 (content-type (xml-backend-mime-type (hop-xml-backend)))
	 (charset (hop-charset))
	 (xml (<HTML>
		 (<EHEAD> (current-request))
		 (<BODY>
		    (<CENTER>
		       (<ETABLE>
			  (<TR>
			     (<ETD> :class "logo" :valign 'top
				(<EIMG> :src (if (&io-timeout-error? e)
						 "timeout.png"
						 "error.png")
				   :req req))
			     (<ETD>
				(<TABLE> :width "100%"
				   (<TR> (<ETD> :class "title" "An error occured while talking to a remote host"))
				   (<TR> (<ETD> :class "msg" host))
				   (<TR> (<ETD> :class "dump"
					    (<PRE> (html-string-encode s)))))))))))))))

;*---------------------------------------------------------------------*/
;*    http-io-error ...                                                */
;*---------------------------------------------------------------------*/
(define (http-io-error e)
   (let ((s (with-error-to-string (lambda () (error-notify e))))
	 (req (current-request)))
      (instantiate::http-response-hop
	 (request req)
	 (start-line (http-start-line req "404 Not Found"))
	 (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
	 (backend (hop-xml-backend))
	 (content-type (xml-backend-mime-type (hop-xml-backend)))
	 (charset (hop-charset))
	 (xml (<HTML>
		 (<EHEAD> (current-request))
		 (<BODY>
		    (<CENTER>
		       (<ETABLE>
			  (<TR>
			     (<ETD> :class "logo" :valign 'top
				(<EIMG> :src (if (&io-timeout-error? e)
						 "error2.png"
						 "error.png")
				   :req req))
			     (<ETD>
				(<TABLE> :width "100%"
				   (<TR> (<ETD> :class "title" "IO Error"))
				   (<TR> (<ETD> :class "msg"
						(list (find-runtime-type e)
						      ": "
						      (&error-msg e))))
				   (<TR> (<ETD> :class "dump"
						(<PRE> (html-string-encode s)))))))))))))))

;*---------------------------------------------------------------------*/
;*    http-gateway-timeout ...                                         */
;*---------------------------------------------------------------------*/
(define (http-gateway-timeout e)
   (instantiate::http-response-error
      (request (anonymous-request))
      (start-line "HTTP/1.0 502 Bad Gateway")
      (charset (hop-locale))
      (body (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html><body>Gateway Timeout ~a</body></html>" e))))

