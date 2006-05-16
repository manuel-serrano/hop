(module hop-scheme-head
   (library hop
	    scheme2js)
   (import hopscheme-config)
   (export (<HOP-SCHEME-HEAD> . obj))
   (eval (export-all)))

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
				'()                  ;; js-interface
				(hopscheme-config)))))  ;; config


;*---------------------------------------------------------------------*/
(define (hop-sscript file dir)
   (if (= (string-length file) 0)
       (error '<HOP-SCHEME-HEAD> "Illegal sscript" file)
       (let* ((path (if (char=? (string-ref file 0) (file-separator))
			file
			(hop-file dir file)))
	      (cached (cache-get sscript-cache path)))
	  (if cached
	      cached
	      (let* ((compiled (compile-scheme-file path))
		     (cached (cache-put! sscript-cache path compiled)))
		 cached)))))

;*---------------------------------------------------------------------*/
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
