(module hop-scheme-head
   (library hop
	    scheme2js)
   (import hopscheme-config)
   (export (<HOP-SCHEME-HEAD> . obj))
   (eval (export-all)))

(define (hop-file path file)
   (let ((p (find-file/path file path)))
      (if (string? p)
	  p
	  (make-file-name (hop-share-directory) file))))

;*---------------------------------------------------------------------*/
(define (compile-scheme-file file)
   (let* ((tmp-name (string-append (symbol->string (gensym 'tmpFile)) ".js"))
	  (tmp-file-name (make-file-name "/tmp" tmp-name)))
      (scheme2js-compile-files! (list file)          ;; input-files
				tmp-file-name        ;; output-file
				'()                  ;; js-interface
				(hopscheme-config))  ;; config
      tmp-file-name))


;*---------------------------------------------------------------------*/
(define (hop-sscript file dir)
   (if (= (string-length file) 0)
       (error '<HOP-SCHEME-HEAD> "Illegal sscript" file)
       (compile-scheme-file
	(if (char=? (string-ref file 0) (file-separator))
	    file
	    (hop-file dir file)))))

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
