;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/http-error.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:55:24 2004                          */
;*    Last change :  Thu Jan 26 12:12:56 2006 (serrano)                */
;*    Copyright   :  2004-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HTTP management                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_http-error
   
   (library web)
   
   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_xml
	    __hop_html-extra
	    __hop_service)
   
   (export  (http-request-error::%http-response ::&error)
	    (http-response-error::%http-response ::&error ::http-request)
	    (http-unknown-host host)
	    (http-file-not-found file)
	    (http-permission-denied file)
	    (http-method-error obj)
	    (http-parse-error obj)
	    (http-bad-request obj)
	    (http-internal-error ::&error ::obj)
	    (http-service-error ::http-request ::symbol ::bstring)
	    (http-warning msg #!optional dump)
	    (http-internal-warning e)
	    (http-service-unavailable obj))

   (eval    (export-exports)))

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
   (tprint "http-response-error: " e)
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
;*    http-unknown-host ...                                            */
;*---------------------------------------------------------------------*/
(define (http-unknown-host host)
   (instantiate::http-response-hop
      (start-line "HTTP/1.0 404 Not Found")
      (xml (<HTML>
	      (<HEAD>
		 (<HOP-HEAD> :css
			     (format "http://~a:~a/~a/hop-error.hss"
				     (hostname)
				     (hop-port)
				     (hop-share-directory))))
	      (<BODY>
		 (<CENTER>
		    (<TABLE>
		       :class "error"
		       (<TR>
			  (<TD>
			     (<IMG> :src (format "http://~a:~a/~a/icons/notfound.png"
						 (hostname)
						 (hop-port)
						 (hop-share-directory))))
			  (<TD>
			     (<TABLE>
				(<TR> (<TD> :id "title" "Unknown Host"))
				(<TR> (<TD> :id "msg"
					    (<SPAN> :class "filenotfound"
						    host)))))))))))))

;*---------------------------------------------------------------------*/
;*    http-file-not-found ...                                          */
;*---------------------------------------------------------------------*/
(define (http-file-not-found file)
   (instantiate::http-response-hop
      (start-line "HTTP/1.0 404 Not Found")
      (xml (<HTML>
	      (<HEAD>
		 (<HOP-HEAD> :css "hop-error.hss"))
	      (<BODY>
		 (<CENTER>
		    (<TABLE>
		       :class "error"
		       (<TR>
			  (<TD>
			     (<IMG> :src (format "~a/icons/notfound.png"
						 (hop-share-directory))))
			  (<TD>
			     (<TABLE>
				(<TR> (<TD> :id "title" "File not found"))
				(<TR> (<TD> :id "msg"
					    (<SPAN> :class "filenotfound"
						    file)))))))))))))

;*---------------------------------------------------------------------*/
;*    http-permission-denied ...                                       */
;*---------------------------------------------------------------------*/
(define (http-permission-denied file)
   (instantiate::http-response-string
      (start-line "HTTP/1.0 403 Forbidden")
      (body (format "Permission denied: ~s" file))))

;*---------------------------------------------------------------------*/
;*    http-method-error ...                                            */
;*---------------------------------------------------------------------*/
(define (http-method-error obj)
    (instantiate::http-response-string
       (start-line "HTTP/1.0 501 Not Implemented")
       (body (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html><body>Method not implemented ~a</body></html>"
		     obj))))

;*---------------------------------------------------------------------*/
;*    http-parse-error ...                                             */
;*---------------------------------------------------------------------*/
(define (http-parse-error obj)
   (instantiate::http-response-string
      (start-line "HTTP/1.0 400 Bad Request")
      (body (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html><body>Parse error in HTTP request on token <tt>~a</tt>/body></html>"
		    obj))))

;*---------------------------------------------------------------------*/
;*    http-bad-request ...                                             */
;*---------------------------------------------------------------------*/
(define (http-bad-request obj)
   (instantiate::http-response-string
      (start-line "HTTP/1.0 400 Bad Request")
      (body (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html><body>Bad request <tt><pre>~a</pre></tt></body></html>" obj))))

;*---------------------------------------------------------------------*/
;*    http-internal-error ...                                          */
;*---------------------------------------------------------------------*/
(define (http-internal-error e msg)
   (let ((s (with-error-to-string (lambda () (error-notify e)))))
      (instantiate::http-response-hop
	 (start-line "HTTP/1.0 501 Internal Server Error")
	 (xml (<HTML>
		 (<HEAD>
		    (<HOP-HEAD> :css "hop-error.hss"))
		 (<BODY>
		    (<CENTER>
		       (<TABLE>
			  :class "error"
			  (<TR>
			     (<TD>
				(<IMG> :inline #t
				       :src (format "~a/icons/error.png"
						    (hop-share-directory))))
			     (<TD>
				(<TABLE>
				   (<TR> (<TD> :id "title" "Internal Error"))
				   (<TR> (<TD> :id "msg" msg))
				   (<TR> (<TD> :id "dump"
					       (<PRE> (html-string-encode s)))))))))))))))
   
;*---------------------------------------------------------------------*/
;*    http-service-error ...                                           */
;*---------------------------------------------------------------------*/
(define (http-service-error req service m)
   (let ((info (<TABLE>
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
			      (<TH> :align 'right "service:")
			      (<TD> (<TT> service)))
			   (<TR>
			      (<TH> :align 'right "filter:")
			      (<TD> (<TT> (hop-request-service-name req))))))))))
      (instantiate::http-response-hop
	 (start-line "HTTP/1.0 400 Bad Request")
	 (xml (<HTML>
		 (<HEAD>
		    (<HOP-HEAD> :css "hop-error.hss"))
		 (<BODY>
		    (<CENTER>
		       (<TABLE>
			  :class "error"
			  (<TR>
			     (<TD>
				(<IMG> :inline #t
				       :src (format "~a/icons/error.png"
						    (hop-share-directory))))
			     (<TD>
				(<TABLE>
				   (<TR>
				      (<TD>
					 :id "title" "Hop service error"))
				   (<TR>
				      (<TD>
					 :id "msg" info))
				   (<TR>
				      (<TD>
					 :id "dump"
					 (<PRE>
					    (html-string-encode m)))))))))))))))

;*---------------------------------------------------------------------*/
;*    http-internal-warning ...                                        */
;*---------------------------------------------------------------------*/
(define (http-internal-warning e)
   (let ((s (with-error-to-string (lambda () (warning-notify e)))))
      (instantiate::http-response-string
	 (start-line "HTTP/1.0 400 Bad Request")
	 (body (format "<HTML><BODY><PRE> ~a </PRE></BODY></HTML>")))))
   
;*---------------------------------------------------------------------*/
;*    http-warning ...                                                 */
;*---------------------------------------------------------------------*/
(define (http-warning msg #!optional dump)
      (instantiate::http-response-hop
	 (start-line "HTTP/1.0 200 ok")
	 (xml (<HTML>
		 (<HEAD>
		    (<HOP-HEAD> :css "hop-error.hss"))
		 (<BODY>
		    (<CENTER>
		       (<TABLE>
			  :class "error"
			  (<TR>
			     (<TD>
				(<IMG> :inline #t
				       :src (format "~a/icons/warning.png"
						    (hop-share-directory))))
			     (<TD>
				(<TABLE>
				   (<TR> (<TD> :id "title" "warning"))
				   (<TR> (<TD> :id "msg" msg))
				   (<TR> (<TD> :id "dump" (or dump "")))))))))))))

;*---------------------------------------------------------------------*/
;*    http-service-unavailable ...                                     */
;*---------------------------------------------------------------------*/
(define (http-service-unavailable e)
   (instantiate::http-response-string
      (start-line "HTTP/1.0 503 Service Unavailable")
      (body (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html><body>Service unavailable ~a</body></html>" e))))

;*---------------------------------------------------------------------*/
;*    http-io-error ...                                                */
;*---------------------------------------------------------------------*/
(define (http-io-error e)
   (let ((s (with-error-to-string (lambda () (error-notify e)))))
      (instantiate::http-response-hop
	 (start-line "HTTP/1.0 200 ok")
	 (xml (<HTML>
		 (<HEAD>
		    (<HOP-HEAD> :css
				(format "http://~a:~a/~a/hop-error.hss"
					(hostname)
					(hop-port)
					(hop-share-directory))))
		 (<BODY>
		    (<CENTER>
		       (<TABLE>
			  :class "error"
			  (<TR>
			     (<TD>
				(<IMG> :src (format
					     "http://~a:~a/~a/icons/notfound.png"
					     (hostname)
					     (hop-port)
					     (hop-share-directory))))
			     (<TD>
				(<TABLE>
				   (<TR> (<TD> :id "title" "IO Error"))
				   (<TR> (<TD> :id "msg"
					       (list (find-runtime-type e)
						     ": "
						     (&error-msg e))))
				   (<TR> (<TD> :id "dump"
					       (<PRE> (html-string-encode s)))))))))))))))
