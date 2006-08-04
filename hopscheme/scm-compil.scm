(module scm-compil
   (library hop
	    scheme2js)
   (import hopscheme-config
	   hopscheme_aliases)
   (export (<HOP-SCHEME-HEAD> . obj))
   (eval (export-all)))

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
				(hopscheme-config)))))  ;; config


;*---------------------------------------------------------------------*/
(define (hop-sscript file dir)
   (if (= (string-length file) 0)
       (error '<HOP-SCHEME-HEAD> "Illegal sscript" file)
       (with-lock *sscript-mutex*
	  (lambda ()
	     (let* ((path (if (char=? (string-ref file 0) (file-separator))
			      file
			      (hop-file dir file)))
		    (cached-name (string-append path ".compiled"))
		    (cached (cache-get sscript-cache cached-name)))
		(if cached
		    cached
		    (let* ((compiled (compile-scheme-file path))
			   (cached (cache-put! sscript-cache cached-name compiled)))
		       cached)))))))

;*---------------------------------------------------------------------*/
;; deprecated now.
(define (<HOP-SCHEME-HEAD> . obj)
   (let* ((req (the-current-request))
	  (dir (if (http-request? req)
		   (list (dirname (http-request-path req)))
		   '()))
	  (mode #f))
      (let loop ((a obj)
		 (filtered '()))
	 (cond
	    ((null? a)
	     (apply <HEAD> (reverse! filtered)))
	    ((pair? (car a))
	     (loop (append (car a) (cdr a))
		   filtered))
	    ((null? (car a))
	     (loop (cdr a)
		   filtered))
	    ((keyword? (car a))
	     (if (null? (cdr a))
		 (error '<HOP-SCHEME-HEAD> (format "Missing ~a value" (car a)) a)
		 (case (car a)
		    ((:sscript)
		     (set! mode :sscript)
		     (loop (cddr a)
			   (cons* (hop-sscript (cadr a) dir)
				 :jscript
				 filtered)))
		    (else
		     (set! mode #f)
		     (loop (cddr a)
			   (cons* (cadr a)
				  (car a)
				  filtered))))))
	    ((string? (car a))
	     (case mode
		((:sscript)
		 (loop (cdr a)
		       (cons* (hop-sscript (cadr a) dir)
			     :jscript
			     filtered)))
		(else
		 (loop (cdr a)
		       (cons (car a) filtered)))))
	    (else
	     (error '<HOP-SCHEME-HEAD> (format "Illegal ~a argument" (car a)) a))))))

(define (sscript-response req)
   (with-access::http-request req (path method header)
      (with-lock *sscript-mutex*
	 (lambda ()
	    (let ((cache (cache-get sscript-cache path))
		  (mime (mime-type path "text/javascript")))
	       (if (string? cache)
		   (instantiate::http-response-file
		      (request req)
		      (content-type mime)
		      (bodyp (eq? method 'GET))
		      (file cache))
		   (let* ((jscript (compile-scheme-file path))
			  (cache (cache-put! sscript-cache path jscript)))
		      (instantiate::http-response-file
			 (request req)
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
