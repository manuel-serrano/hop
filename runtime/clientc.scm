;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/runtime/clientc.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 25 14:37:34 2009                          */
;*    Last change :  Mon Apr 12 11:31:02 2010 (serrano)                */
;*    Copyright   :  2009-10 Manuel Serrano                            */
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
	       (valuec::procedure read-only)
	       (modulec::procedure read-only)
	       (macroe::procedure read-only)
	       (sexp->precompiled::procedure read-only)
	       (precompiled->JS-expression::procedure read-only)
	       (precompiled->JS-statement::procedure read-only)
	       (precompiled->JS-return::procedure read-only)
	       (precompiled->sexp::procedure read-only)
	       (precompiled-declared-variables::procedure read-only)
	       (precompiled-free-variables::procedure read-only))

	    (init-clientc-compiler! #!key
				    filec expressionc valuec
				    modulec macroe
				    sexp->precompiled
				    precompiled->JS-expression
				    precompiled->JS-statement
				    precompiled->JS-return
				    precompiled->sexp
				    precompiled-declared-variables
				    precompiled-free-variables)

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
(define (init-clientc-compiler! #!key filec expressionc valuec
				modulec macroe
				sexp->precompiled
				precompiled->JS-expression
				precompiled->JS-statement
				precompiled->JS-return
				precompiled->sexp
				precompiled-declared-variables
				precompiled-free-variables)

   (define (null e) '())
   
   ;; prepare the client-code compiler cache
   (set! clientc-cache
	 (instantiate::cache-disk
	    (path (make-file-path (hop-cache-directory)
				  (string-append "clientc-"
						 (integer->string (hop-port)))))
	    (out (lambda (o p) (with-output-to-port p (lambda () (print o)))))))

   ;; hook the client-code compiler
   (hop-clientc-set!
    (instantiate::clientc
       (filec (lambda (file env)
		 (hop-load-afile (dirname file))
		 (filec file env)))
       (expressionc expressionc)
       (valuec valuec)
       (modulec modulec)
       (macroe macroe)
       (sexp->precompiled sexp->precompiled)
       (precompiled->JS-expression precompiled->JS-expression)
       (precompiled->JS-statement precompiled->JS-statement)
       (precompiled->JS-return precompiled->JS-return)
       (precompiled->sexp precompiled->sexp)
       (precompiled-declared-variables (or precompiled-declared-variables null))
       (precompiled-free-variables (or precompiled-free-variables null)))))
   
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
	     (header '((Accept-Ranges: . "bytes")))
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
		      (header '((Accept-Ranges: . "bytes")))
		      (file cache)))
		(eval-module-set! m))))))

;*---------------------------------------------------------------------*/
;*    get-clientc-compiled-file ...                                    */
;*---------------------------------------------------------------------*/
(define (get-clientc-compiled-file path)
   (let* ((req (current-request))
	  (rep (clientc-response req path)))
      (with-input-from-file (http-response-file-file rep) read-string)))

