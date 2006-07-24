;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/http-error.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:55:24 2004                          */
;*    Last change :  Fri Jul 21 18:51:49 2006 (serrano)                */
;*    Copyright   :  2004-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HTTP management                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_http-error
   
   (library web)
   
   (include "compiler-macro.sch"
	    "xml.sch")

   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_xml
	    __hop_hop-extra
	    __hop_service
	    __hop_misc)
   
   (export  (http-request-error::%http-response ::&error)
	    (http-response-error::%http-response ::&error ::http-request)
	    (http-unknown-host host)
	    (http-file-not-found file)
	    (http-service-not-found file)
	    (http-permission-denied file)
	    (http-method-error obj)
	    (http-parse-error obj)
	    (http-bad-request obj)
	    (http-internal-error ::&error ::obj)
	    (http-service-error ::http-request ::symbol ::bstring)
	    (http-invalidated-service-error ::http-request)
	    (http-corrupted-service-error ::http-request)
	    (http-warning msg #!optional dump)
	    (http-internal-warning e)
	    (http-service-unavailable obj)))

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
;*    http-response-error ...                                          */
;*---------------------------------------------------------------------*/
(define (http-response-error e::&error req::http-request)
   (let ((msg (<TABLE>
		 (<TR>
		    (<TD> "An error occured while responding to"))
		 (<TR>
		    (<TD>
		       :class "request-info"
		       (<TABLE>
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
;*    <EIMG> ...                                                       */
;*---------------------------------------------------------------------*/
(define (<EIMG> . args)
   (apply <IMG> :style "padding: 20px;" :inline #t args))

;*---------------------------------------------------------------------*/
;*    <ETD> ...                                                        */
;*---------------------------------------------------------------------*/
(define-xml-compound <ETD> ((id #unspecified string)
			    (class #f)
			    (style "" string)
			    body)
   (let* ((default "vertical-align: middle; text-align: left; font-weight: bold")
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
	  :style "width: 45em; border: 1px solid #bbb;  -moz-border-radius: 0.5em; background: #fff;"
	  args))

;*---------------------------------------------------------------------*/
;*    title-style ...                                                  */
;*---------------------------------------------------------------------*/
(define title-style
   "font-size: x-large; font-weight: bold; padding-bottom: 1px;")

;*---------------------------------------------------------------------*/
;*    http-unknown-host ...                                            */
;*---------------------------------------------------------------------*/
(define (http-unknown-host host)
   (instantiate::http-response-hop
      (start-line "HTTP/1.0 404 Not Found")
      (request (instantiate::http-request))
      (xml (<HTML>
	      (<HEAD> :css
		      (format "http://~a:~a/~a/hop-error.hss"
			      (hostname)
			      (hop-port)
			      (hop-share-directory)))
	      (<BODY>
		 (<CENTER>
		    (<ETABLE>
		       (<TR>
			  (<ETD>
			     (<EIMG> :src (format "http://~a:~a/~a/icons/notfound.png"
						  (hostname)
						  (hop-port)
						  (hop-share-directory))))
			  (<ETD>
			     (<TABLE>
				(<TR> (<ETD> :class "title" "Unknown Host"))
				(<TR> (<ETD> :class "msg"
					    (<SPAN> :class "filenotfound"
						    host)))))))))))))

;*---------------------------------------------------------------------*/
;*    http-file-not-found ...                                          */
;*---------------------------------------------------------------------*/
(define (http-file-not-found file)
   (instantiate::http-response-hop
      (request (instantiate::http-request))
      (start-line "HTTP/1.0 404 Not Found")
      (xml (<HTML>
	      (<HEAD> :css "hop-error.hss")
	      (<BODY>
		 (<CENTER>
		    (<ETABLE>
		       (<TR>
			  (<ETD>
			     (<EIMG> :src (format "~a/icons/notfound.png"
						  (hop-share-directory))))
			  (<ETD>
			     (<TABLE>
				:style "35em"
				(<TR> (<ETD> :class "title" "File not found"))
				(<TR> (<ETD> :class "msg"
					    (<SPAN> :class "filenotfound"
						    file)))))))))))))

;*---------------------------------------------------------------------*/
;*    http-service-not-found ...                                       */
;*---------------------------------------------------------------------*/
(define (http-service-not-found file)
   (instantiate::http-response-hop
      (request (instantiate::http-request))
      (start-line "HTTP/1.0 404 Not Found")
      (xml (<HTML>
	      (<HEAD> :css "hop-error.hss")
	      (<BODY>
		 (<CENTER>
		    (<ETABLE>
		       (<TR>
			  (<ETD>
			     (<EIMG> :src (format "~a/icons/notfound.png"
						  (hop-share-directory))))
			  (<ETD>
			     (<TABLE>
				:style "35em"
				(<TR> (<ETD> :class "title"
					     "Invalidated service!"
					     ))
				(<TR> (<ETD> :class "msg"
					     (<SPAN> :class "filenotfound"
						     file)))
				(<TR> (<ETD> :class "dump"
					     (<SPAN> "You are trying to executed an invalidated service!
<br><br>
This is generally due to a restart of the server.
On restart the server invalidates all anonymous services that hence
can no longer be executed.<br><br>
Reloading the page is the only workaround.")))))))))))))

;*---------------------------------------------------------------------*/
;*    http-permission-denied ...                                       */
;*---------------------------------------------------------------------*/
(define (http-permission-denied file)
   (instantiate::http-response-string
      (request (instantiate::http-request))
      (start-line "HTTP/1.0 403 Forbidden")
      (body (format "Permission denied: ~s" file))))

;*---------------------------------------------------------------------*/
;*    http-method-error ...                                            */
;*---------------------------------------------------------------------*/
(define (http-method-error obj)
   (instantiate::http-response-string
      (request (instantiate::http-request))
      (start-line "HTTP/1.0 501 Not Implemented")
      (body (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html><body>Method not implemented ~a</body></html>"
		    obj))))

;*---------------------------------------------------------------------*/
;*    http-parse-error ...                                             */
;*---------------------------------------------------------------------*/
(define (http-parse-error obj)
   (instantiate::http-response-string
      (request (instantiate::http-request))
      (start-line "HTTP/1.0 400 Bad Request")
      (body (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html><body>Parse error in HTTP request on token <tt>~a</tt>/body></html>"
		    obj))))

;*---------------------------------------------------------------------*/
;*    http-bad-request ...                                             */
;*---------------------------------------------------------------------*/
(define (http-bad-request obj)
   (instantiate::http-response-string
      (request (instantiate::http-request))
      (start-line "HTTP/1.0 400 Bad Request")
      (body (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html><body>Bad request <tt><pre>~a</pre></tt></body></html>" obj))))

;*---------------------------------------------------------------------*/
;*    http-internal-error ...                                          */
;*---------------------------------------------------------------------*/
(define (http-internal-error e msg)
   (let ((s (with-error-to-string (lambda () (error-notify e)))))
      (instantiate::http-response-hop
	 (request (instantiate::http-request))
	 (start-line "HTTP/1.0 501 Internal Server Error")
	 (xml (<HTML>
		 (<HEAD> :css "hop-error.hss")
		 (<BODY>
		    (<CENTER>
		       (<ETABLE>
			  (<TR>
			     (<ETD>
				(<EIMG> :src (format "~a/icons/error.png"
						     (hop-share-directory))))
			     (<ETD>
				(<TABLE>
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
			(<TABLE>
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
	 (start-line "HTTP/1.0 400 Bad Request")
	 (xml (<HTML>
		 (<HEAD> :css "hop-error.hss")
		 (<BODY>
		    (<CENTER>
		       (<ETABLE>
			  (<TR>
			     (<ETD>
				(<EIMG> :src (format "~a/icons/error.png"
						     (hop-share-directory))))
			     (<ETD>
				(<TABLE>
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
      (xml (<HTML>
	      (<HEAD> :css "hop-error.hss")
	      (<BODY>
		 (<CENTER>
		    (<ETABLE>
		       (<TR>
			  (<ETD>
			     (<EIMG> :src (format "~a/icons/notfound.png"
						  (hop-share-directory))))
			  (<ETD>
			     (<TABLE>
				:style "35em"
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
Reloading the page is the only workaround.")))))))))))))

;*---------------------------------------------------------------------*/
;*    http-corrupted-service-error ...                                 */
;*---------------------------------------------------------------------*/
(define (http-corrupted-service-error req)
   (instantiate::http-response-hop
      (request req)
      (start-line "HTTP/1.0 404 Not Found")
      (xml (<HTML>
	      (<HEAD> :css "hop-error.hss")
	      (<BODY>
		 (<CENTER>
		    (<ETABLE>
		       (<TR>
			  (<ETD>
			     (<EIMG> :src (format "~a/icons/notfound.png"
						  (hop-share-directory))))
			  (<ETD>
			     (<TABLE>
				:style "35em"
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
      (instantiate::http-response-string
	 (request (instantiate::http-request))
	 (start-line "HTTP/1.0 400 Bad Request")
	 (body (format "<HTML><BODY><PRE> ~a </PRE></BODY></HTML>" s)))))
   
;*---------------------------------------------------------------------*/
;*    http-warning ...                                                 */
;*---------------------------------------------------------------------*/
(define (http-warning msg #!optional dump)
   (instantiate::http-response-hop
      (request (instantiate::http-request))
      (start-line "HTTP/1.0 200 ok")
      (xml (<HTML>
	      (<HEAD> :css "hop-error.hss")
	      (<BODY>
		 (<CENTER>
		    (<ETABLE>
		       (<TR>
			  (<ETD>
			     (<EIMG> :src (format "~a/icons/warning.png"
						  (hop-share-directory))))
			  (<ETD>
			     (<TABLE>
				(<TR> (<ETD> :id "title" "warning"))
				(<TR> (<ETD> :id "msg" msg))
				(<TR> (<ETD> :id "dump" (or dump "")))))))))))))

;*---------------------------------------------------------------------*/
;*    http-service-unavailable ...                                     */
;*---------------------------------------------------------------------*/
(define (http-service-unavailable e)
   (instantiate::http-response-string
      (request (instantiate::http-request))
      (start-line "HTTP/1.0 503 Service Unavailable")
      (body (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html><body>Service unavailable ~a</body></html>" e))))

;*---------------------------------------------------------------------*/
;*    http-io-error ...                                                */
;*---------------------------------------------------------------------*/
(define (http-io-error e)
   (let ((s (with-error-to-string (lambda () (error-notify e)))))
      (instantiate::http-response-hop
	 (request (instantiate::http-request))
	 (start-line "HTTP/1.0 404 Not Found")
	 (xml (<HTML>
		 (<HEAD> :css
			 (format "http://~a:~a/~a/hop-error.hss"
				 (hostname)
				 (hop-port)
				 (hop-share-directory)))
		 (<BODY>
		    (<CENTER>
		       (<ETABLE>
			  (<TR>
			     (<ETD>
				(<EIMG> :src (format
					      "http://~a:~a/~a/icons/notfound.png"
					      (hostname)
					      (hop-port)
					      (hop-share-directory))))
			     (<ETD>
				(<TABLE>
				   (<TR> (<ETD> :id "title" "IO Error"))
				   (<TR> (<ETD> :id "msg"
						(list (find-runtime-type e)
						      ": "
						      (&error-msg e))))
				   (<TR> (<ETD> :id "dump"
						(<PRE> (html-string-encode s)))))))))))))))
