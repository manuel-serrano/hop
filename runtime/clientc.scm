;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/clientc.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 25 14:37:34 2009                          */
;*    Last change :  Wed Nov 19 07:11:45 2014 (serrano)                */
;*    Copyright   :  2009-14 Manuel Serrano                            */
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
	       (precompiled-free-variables::procedure read-only)
	       (jsc::procedure read-only)
	       (filename-resolver::procedure read-only (default find-file/path)))

	    (init-clientc-compiler! #!key
				    filec expressionc valuec
				    modulec macroe
				    sexp->precompiled
				    precompiled->JS-expression
				    precompiled->JS-statement
				    precompiled->JS-return
				    precompiled->sexp
				    precompiled-declared-variables
				    precompiled-free-variables
				    filename-resolver
				    jsc)

	    (current-module-clientc-import)
	    
	    (clientc-cached-response ::bstring)
	    (clientc-response::%http-response ::http-request ::bstring ::bstring)
	    (get-clientc-compiled-file ::bstring ::bstring)
	    (clientc-resolve-filename ::bstring ::obj)))

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
	   precompiled-free-variables
	   filename-resolver
	   jsc)
   
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
	 (filec (lambda (ifile ofile env)
		   (hop-load-afile (dirname ifile))
		   (filec ifile ofile env)))
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
	 (precompiled-free-variables (or precompiled-free-variables null))
	 (filename-resolver (or filename-resolver (lambda (n p) n)))
	 (jsc (or jsc error)))))
   
;*---------------------------------------------------------------------*/
;*    current-module-clientc-import ...                                */
;*---------------------------------------------------------------------*/
(define (current-module-clientc-import)
   (let ((mod (eval-module)))
      (if (evmodule? mod)
	  (evmodule-extension mod)
	  '())))

;*---------------------------------------------------------------------*/
;*    clientc-response ...                                             */
;*---------------------------------------------------------------------*/
(define (clientc-response req path name)
   (synchronize clientc-mutex
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
			     #;(request req)
			     (start-line "HTTP/1.1 304 Not Modified")
			     (content-type mime)
			     (header hd)
			     (charset (hop-locale)))
			  (instantiate::http-response-file
			     #;(request req)
			     (charset (hop-locale))
			     (content-type mime)
			     (bodyp (eq? method 'GET))
			     (header (cons '(Accept-Ranges: . "bytes") hd))
			     (file value)))))
		(let ((m (eval-module)))
		   (unwind-protect
		      ;; add a dummy entry value in the cache
		      (let ((ce (cache-put! clientc-cache path "")))
			 (if (isa? ce cache-entry)
			     (with-access::cache-entry ce (value signature)
				;; override the cache file
				(compile-client path name value '())
				;; sent the file response
				(instantiate::http-response-file
				   #;(request req)
				   (charset (hop-locale))
				   (content-type mime)
				   (bodyp (eq? method 'GET))
				   (header (cons `(ETag: . ,signature)
					      '((Accept-Ranges: . "bytes"))))
				   (file value)))
			     ;; no cache, use a string
			     (instantiate::http-response-string
				#;(request req)
				(charset (hop-locale))
				(content-type mime)
				(bodyp (eq? method 'GET))
				(body (with-output-to-string
					 (lambda ()
					    (compile-client path name "-" '())))))))
		      (eval-module-set! m))))))))

;*---------------------------------------------------------------------*/
;*    compile-client ...                                               */
;*---------------------------------------------------------------------*/
(define (compile-client path name output env)
   (with-access::clientc (hop-clientc) (filec jsc)
      (if (string-suffix? ".js" path)
	  (jsc path name output)
	  (filec path output env))))
   
;*---------------------------------------------------------------------*/
;*    clientc-cached-response ...                                      */
;*---------------------------------------------------------------------*/
(define (clientc-cached-response path)
   (synchronize clientc-mutex
      (let ((ce (cache-get clientc-cache path)))
	 (when (isa? ce cache-entry)
	    (with-access::cache-entry ce (value)
	       value)))))

;*---------------------------------------------------------------------*/
;*    dummy-request ...                                                */
;*---------------------------------------------------------------------*/
(define dummy-request
   (instantiate::http-request))

;*---------------------------------------------------------------------*/
;*    get-clientc-compiled-file ...                                    */
;*---------------------------------------------------------------------*/
(define (get-clientc-compiled-file path name)
   (let* ((req dummy-request)
	  (rep (clientc-response req path name)))
      (cond
	 ((isa? rep http-response-file)
	  (with-access::http-response-file rep (file)
	     (with-input-from-file file read-string)))
	 ((isa? rep http-response-string)
	  (with-access::http-response-string rep (body)
	     body))
	 (else
	  (error "get-clientc-compiled-file" "Illegal clientc response" rep)))))
   
;*---------------------------------------------------------------------*/
;*    clientc-resolve-filename ...                                     */
;*---------------------------------------------------------------------*/
(define (clientc-resolve-filename url path)
   (with-access::clientc (hop-clientc) (filename-resolver)
      (filename-resolver url path)))
