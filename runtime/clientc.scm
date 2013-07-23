;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/runtime/clientc.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 25 14:37:34 2009                          */
;*    Last change :  Fri Jul 19 16:02:37 2013 (serrano)                */
;*    Copyright   :  2009-13 Manuel Serrano                            */
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
	    __hop_cache
	    __hop_types
	    __hop_xml-types
	    __hop_xml
	    __hop_misc
	    __hop_mime
	    __hop_types
	    __hop_hop
	    __hop_http-lib
	    __hop_configure)
   
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

	    (current-module-clientc-import)
	    
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
(define clientc-mutex (make-mutex "scm"))

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
	    (clear (hop-clientc-clear-cache))
	    (path (make-cache-name "clientc"))
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
;*    current-module-clientc-import ...                                */
;*---------------------------------------------------------------------*/
(define (current-module-clientc-import)
   (let ((mod (eval-module)))
      (if (evmodule? mod)
	  (evmodule-extension mod)
	  '())))

;*---------------------------------------------------------------------*/
;*    clientc-url ...                                                  */
;*---------------------------------------------------------------------*/
(define (clientc-url path)
   (string-append path "?" (hop-scm-compile-suffix)))

;*---------------------------------------------------------------------*/
;*    clientc-response ...                                             */
;*---------------------------------------------------------------------*/
(define (clientc-response req path)
   (with-access::http-request req (method header)
      (let ((ce (cache-get clientc-cache path))
	    (mime (hop-mime-type)))
	 (if (isa? ce cache-entry)
	     ;; since we are serving a cached answer, we also have
	     ;; to check that the client is allowed to the requested
	     ;; file, i.e., the non-compiled file.
	     (with-access::cache-entry ce (value signature)
		(let ((etag (http-header-field header if-none-match:))
		      (hd `((ETag: . ,signature))))
		   (if (and (string? etag)
			    (=elong (string->elong etag) signature))
		       (instantiate::http-response-string
			  (request req)
			  (start-line "HTTP/1.1 304 Not Modified")
			  (content-type mime)
			  (header hd)
			  (charset (hop-locale)))
		       (instantiate::http-response-file
			  (request req)
			  (charset (hop-locale))
			  (content-type mime)
			  (bodyp (eq? method 'GET))
			  (header (cons '(Accept-Ranges: . "bytes") hd))
			  (file value)))))
	     (let ((m (eval-module)))
		(unwind-protect
		   (with-access::clientc (hop-clientc) (filec)
		      (let* ((jscript (filec path '()))
			     (ce (cache-put! clientc-cache path jscript))
			     (hd (if (isa? ce cache-entry)
				     (with-access::cache-entry ce (signature)
					(cons `(ETag: . ,signature)
					   '((Accept-Ranges: . "bytes"))))
				     '((Accept-Ranges: . "bytes")))))
			 (instantiate::http-response-string
			    (request req)
			    (charset (hop-locale))
			    (content-type mime)
			    (bodyp (eq? method 'GET))
			    (header hd)
			    (body jscript))))
		   (eval-module-set! m)))))))

;*---------------------------------------------------------------------*/
;*    dummy-request ...                                                */
;*---------------------------------------------------------------------*/
(define dummy-request
   (instantiate::http-request
      (user (class-nil user))))

;*---------------------------------------------------------------------*/
;*    get-clientc-compiled-file ...                                    */
;*---------------------------------------------------------------------*/
(define (get-clientc-compiled-file path)
   (let* ((req (or (current-request) dummy-request))
	  (rep (clientc-response req path)))
      (cond
	 ((isa? rep http-response-file)
	  (with-access::http-response-file rep (file)
	     (with-input-from-file file read-string)))
	 ((isa? rep http-response-string)
	  (with-access::http-response-string rep (body)
	     body))
	 (else
	  (error "get-clientc-compiled-file" "Illegal clientc response" rep)))))
   
