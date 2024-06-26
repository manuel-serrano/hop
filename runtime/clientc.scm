;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/runtime/clientc.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 25 14:37:34 2009                          */
;*    Last change :  Tue May 14 12:42:08 2024 (serrano)                */
;*    Copyright   :  2009-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP client-side compiler                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_clientc

   (library web http)
   
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
	    __hop_http-utils
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
	       (jsonc::procedure read-only)
	       (htmlc::procedure read-only)
	       (filename-resolver::procedure read-only
		  (default (lambda (file path mod) (find-file/path file path)))))

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
	       jsc
	       jsonc
	       htmlc)

	    (current-module-clientc-import)
	    
	    (clientc-cached-response ::bstring)
	    (clientc-response::%http-response ::http-request ::bstring ::bstring ::obj)
	    (get-clientc-compiled-file ::bstring ::bstring ::obj)
	    (clientc-resolve-filename ::bstring ::obj ::obj)))

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
	   jsc
	   jsonc
	   htmlc)
   
   (define (null e) '())
   
   ;; prepare the client-code compiler cache
   (set! clientc-cache
      (if (hop-cache-enable)
	  (instantiate::cache-disk
	     (clear (hop-clientc-clear-cache))
	     (path (make-cache-name "clientc"))
	     (out (lambda (o p) (with-output-to-port p (lambda () (print o))))))
	  (instantiate::cache-memory
	     (max-file-size #e1024))))
   
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
	 (filename-resolver (or filename-resolver (lambda (n p m) n)))
	 (jsc (or jsc error))
	 (jsonc (or jsonc error))
	 (htmlc (or htmlc error)))))
   
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
(define (clientc-response req path name query)
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
			     (server (hop-server-name))
			     (start-line "HTTP/1.1 304 Not Modified")
			     (content-type mime)
			     (header hd)
			     (charset (hop-locale)))
			  (instantiate::http-response-file
			     (server (hop-server-name))
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
				(compile-client path name value query)
				;; sent the file response
				(instantiate::http-response-file
				   (server (hop-server-name))
				   (charset (hop-locale))
				   (content-type mime)
				   (bodyp (eq? method 'GET))
				   (header (cons `(ETag: . ,signature)
					      '((Accept-Ranges: . "bytes"))))
				   (file value)))
			     ;; no cache, use a string
			     (instantiate::http-response-string
				(server (hop-server-name))
				(charset (hop-locale))
				(content-type mime)
				(bodyp (eq? method 'GET))
				(body (with-output-to-string
					 (lambda ()
					    (compile-client path name "-" query)))))))
		      (eval-module-set! m))))))))

;*---------------------------------------------------------------------*/
;*    compile-client ...                                               */
;*---------------------------------------------------------------------*/
(define (compile-client path name output query)
   (let ((f (the-loading-file)))
      (loading-file-set! path)
      (unwind-protect
	 (with-access::clientc (hop-clientc) (filec jsc jsonc htmlc)
	    (cond
	       ((string-suffix? ".js" path)
		(jsc path name output query))
	       ((string-suffix? ".ts" path)
		(jsc path name output query))
	       ((string-suffix? ".json" path)
		(jsonc path name output query))
	       ((string-suffix? ".html" path)
		(htmlc path name output query))
	       (else
		(filec path output '()))))
	 (loading-file-set! f))))
   
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
(define (get-clientc-compiled-file path name query)
   (let* ((req dummy-request)
	  (rep (clientc-response req path name query)))
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
(define (clientc-resolve-filename url path module)
   (with-access::clientc (hop-clientc) (filename-resolver)
      (filename-resolver url path module)))
