(module scm-compil
   
   (option (set! *dlopen-init* "bgl_dload_init_s_hopscheme"))
   
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
(define (sscript-response req)
   (with-access::http-request req (path method header)
      (with-lock *sscript-mutex*
	 (lambda ()
	    (let ((cache (cache-get sscript-cache path))
		  (mime (mime-type path (hop-javascript-mime-type))))
	       (if (string? cache)
		   (instantiate::http-response-file
		      (request req)
		      (charset (hop-locale))
		      (content-type mime)
		      (bodyp (eq? method 'GET))
		      (file cache))
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
	  (case method
	     ((GET HEAD)
	      (if (and (file-exists? path)
		       (is-suffix? (http-request-path req) "scm"))
		  (sscript-response req)
		  req)))))))
