(module scm-compil
   
   (option (set! *dlopen-init* "hopscheme_s"))
   
   (library hop
	    scheme2js)
   
   (import hopscheme-config
	   hopscheme_aliases))
   
(define *sscript-mutex* (make-mutex 'sscript))

(define sscript-cache
   (instantiate::cache
      (path (make-file-path (hop-rc-directory)
			    "cache"
			    (string-append "sscript-"
					   (integer->string (hop-port)))))
      (out (lambda (o p) (with-output-to-port p (lambda () (print o)))))))

;*---------------------------------------------------------------------*/
(define (hop-file path file)
   (let ((p (find-file/path file path)))
      (if (string? p)
	  p
	  (make-file-name (hop-share-directory) file))))

;*---------------------------------------------------------------------*/
(define (compile-scheme-file file)
   (with-output-to-string
      (lambda ()
	 (scheme2js-compile-files! (list file)          ;; input-files
				"-"                  ;; output-file
				(hopscheme-aliases)   ;; js-interface
				(hopscheme-config)    ;; config
				:reader (lambda (p v)
					   (hop-read p))))))


;*---------------------------------------------------------------------*/
(define (sscript-response req path)
   (with-access::http-request req (method header)
      (with-lock *sscript-mutex*
	 (lambda ()
	    (let ((cache (cache-get sscript-cache path))
		  (mime (mime-type path (hop-javascript-mime-type))))
	       (if (string? cache)
		   ;; since we are serving a cached answer, we also have
		   ;; to check that the client is allowed to the requested
		   ;; file, i.e., the non-compiled file.
		   (if (authorized-path? req path)
		       (instantiate::http-response-file
			  (request req)
			  (charset (hop-locale))
			  (content-type mime)
			  (bodyp (eq? method 'GET))
			  (file cache))
		       (user-access-denied req))
		   (let* ((jscript (compile-scheme-file path))
			  (cache (cache-put! sscript-cache path jscript)))
		      (instantiate::http-response-file
			 (request req)
			 (charset (hop-locale))
			 (content-type mime)
			 (bodyp (eq? method 'GET))
			 (file cache)))))))))

(hop-filter-add-always-last!
 (lambda (req)
    (when (http-request-localhostp req)
       (with-access::http-request req (path method header)
	  (when (and (eq? method 'GET) (substring-at? path (hop-hopcc-base) 0))
	     (let ((p (substring path 6 (string-length path))))
		(if (file-exists? p)
		    (sscript-response req p)
		    (http-file-not-found p))))))))
