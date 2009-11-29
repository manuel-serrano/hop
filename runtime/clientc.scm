;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/clientc.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 25 14:37:34 2009                          */
;*    Last change :  Sun Nov 29 09:00:17 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HOP client-side compiler                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_clientc

   (library web)
   
   (include "xml.sch"
	    "service.sch")
   
   (import  __hop_read
	    __hop_param
	    __hop_cache)
   
   (use	    __hop_user
	    __hop_hop
	    __hop_cgi
	    __hop_misc
	    __hop_service
	    __hop_mime
	    __hop_types
	    __hop_http-error
	    __hop_xml)
   
   (export  (class clientc
	       (filec::procedure read-only)
	       (expressionc::procedure read-only)
	       (modulec::procedure read-only)
	       (macroe::procedure read-only)
	       (JS-expression::procedure read-only)
	       (JS-statement::procedure read-only)
	       (JS-return::procedure read-only))

	    (init-clientc-compiler! #!key
				    filec expressionc modulec macroe
				    JS-expression JS-statement JS-return)
	    (clientc-url ::bstring)
	    (clientc-response::%http-response ::http-request ::bstring)
	    (get-clientc-compiled-file ::bstring)))

;*---------------------------------------------------------------------*/
;*    clientc-cache ...                                                */
;*---------------------------------------------------------------------*/
(define clientc-cache #f)

;*---------------------------------------------------------------------*/
;*    clientc-mutex ...                                                */
;*---------------------------------------------------------------------*/
(define clientc-mutex (make-mutex 'scm))

;*---------------------------------------------------------------------*/
;*    init-clientc-compiler! ...                                       */
;*---------------------------------------------------------------------*/
(define (init-clientc-compiler! #!key filec expressionc modulec macroe
				JS-expression JS-statement JS-return)
   ;; prepare the client-code compiler cache
   (set! clientc-cache
	 (instantiate::cache-disk
	    (path (make-file-path (hop-cache-directory)
				  (string-append "clientc-"
						 (integer->string (hop-port)))))
	    (out (lambda (o p) (with-output-to-port p (lambda () (print
								  o)))))))
   ;; hook the client-code compiler
   (hop-clientc-set!
    (instantiate::clientc
       (filec (lambda (file env)
		 (hop-load-afile (dirname file))
		 (filec file env)))
       (expressionc expressionc)
       (modulec modulec)
       (macroe macroe)
       (JS-expression JS-expression)
       (JS-statement JS-statement)
       (JS-return JS-return))))
   
;*---------------------------------------------------------------------*/
;*    clientc-url ...                                                  */
;*---------------------------------------------------------------------*/
(define (clientc-url path)
   (string-append path "?" (hop-scm-compile-suffix)))

;*---------------------------------------------------------------------*/
;*    clientc-response ...                                             */
;*---------------------------------------------------------------------*/
(define (clientc-response req path)
   (let ((cache (cache-get clientc-cache path))
	 (mime (mime-type path (hop-javascript-mime-type)))
	 (method (http-request-method req)))
      (if (string? cache)
	  ;; since we are serving a cached answer, we also have
	  ;; to check that the client is allowed to the requested
	  ;; file, i.e., the non-compiled file.
	  (instantiate::http-response-file
	     (request req)
	     (charset (hop-locale))
	     (content-type mime)
	     (bodyp (eq? method 'GET))
	     (header '((accept-ranges: . "bytes")))
	     (file cache))
	  (let ((m (eval-module)))
	     (unwind-protect
		(let* ((jscript ((clientc-filec (hop-clientc)) path '()))
		       (cache (cache-put! clientc-cache path jscript)))
		   (instantiate::http-response-file
		      (request req)
		      (charset (hop-locale))
		      (content-type mime)
		      (bodyp (eq? method 'GET))
		      (header '((accept-ranges: . "bytes")))
		      (file cache)))
		(eval-module-set! m))))))

;*---------------------------------------------------------------------*/
;*    get-clientc-compiled-file ...                                    */
;*---------------------------------------------------------------------*/
(define (get-clientc-compiled-file path)
   (let* ((req (current-request))
	  (rep (clientc-response req path)))
      (with-input-from-file (http-response-file-file rep) read-string)))
      
