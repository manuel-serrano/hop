(module export-runtime
   (main myMain))

(define *export-line* (pregexp "/// export[ ]*(.*)"))
(define *exported-fun* (pregexp "(?:var|function) ((?:sc_)?([^ (;]*))"))

(define (starts-with? s sub-s)
   (substring-at? s sub-s 0))

(define (ends-with? s sub-s)
   (let ((s-length (string-length s))
	 (sub-s-length (string-length sub-s)))
      (and (>= s-length sub-s-length)
	   (substring-at? s sub-s (- s-length sub-s-length)))))

(define (unmarshall s)
   (let ((s-without-string-mode
	  (cond
	     ((ends-with? s "_mutable")
	      (substring s 0 (- (string-length s) (string-length "_mutable"))))
	     ((ends-with? s "_immutable")
	      (substring s 0 (- (string-length s)
				(string-length "_immutable"))))
	     (else
	      s))))
      (let loop ((chars (string->list s-without-string-mode))
		 (rev-res '()))
	 (if (null? chars)
	     (let ((res (list->string (reverse! rev-res))))
		(if (starts-with? res "is-")
		    (string-append (substring res 3 (string-length res)) "?")
		    res))
	     (cond
		((char-upper-case? (car chars)) ;; starting upper-case
		 (loop (cdr chars)
		       (cons (char-downcase (car chars)) (cons #\- rev-res))))
		((eq? (car chars) #\2)
		 (loop (cdr chars)
		       (cons #\> (cons #\- rev-res))))
		(else
		 (loop (cdr chars)
		       (cons (car chars) rev-res))))))))

(define (string-mode s)
   (cond
      ((ends-with? s "_mutable")
       'mutable)
      ((ends-with? s "_immutable")
       'immutable)
      (else #f)))

(define (parse-line line)
   (let ((match (pregexp-match *export-line* line)))
      (and match
	   (let* ((fun-match (pregexp-match *exported-fun* line))
		  (exported-fun (cadr fun-match))
		  (interface-fun (string-append "sci_"
						(caddr fun-match)))
		  (mutable/immutable? (string-mode exported-fun))
		  (override-names (cadr match))
		  (scheme-funs (if (string=? override-names "")
				   (list (unmarshall (caddr fun-match)))
				   (string-split override-names
						 " "))))
	      (if (eq? *mode* 'map)
		  (list mutable/immutable? scheme-funs interface-fun)
		  (cons interface-fun exported-fun))))))

(define *mode* #f)

(define (extract-mapping! file)
   (with-input-from-file file
      (lambda ()
	 (let loop ((line (read-line))
		    (shared '())
		    (mutable '())
		    (immutable '()))
	    (if (eof-object? line)
		(begin
		   (pp `(define *shared-runtime-var-mapping* ',shared))
		   (pp `(define *mutable-runtime-var-mapping*
			   (append ',mutable
				   *shared-runtime-var-mapping*)))
		   (pp `(define *immutable-runtime-var-mapping*
			   (append ',immutable
				   *shared-runtime-var-mapping*))))
		(let ((parsed-line (parse-line line)))
		   (if parsed-line
		       (let* ((mutable/immutable? (car parsed-line))
			      (scheme-funs (cadr parsed-line))
			      (interface-fun (caddr parsed-line))
			      (sym-interface-fun (string->symbol interface-fun))
			      (new-mappings
			       (map! (lambda (scheme-fun)
					(list (string->symbol scheme-fun)
					      sym-interface-fun))
				     scheme-funs)))
			  (loop (read-line)
				(if mutable/immutable?
				    shared
				    (append! new-mappings shared))
				(if (eq? mutable/immutable? 'mutable)
				    (append! new-mappings mutable)
				    mutable)
				(if (eq? mutable/immutable? 'immutable)
				    (append! new-mappings immutable)
				    immutable)))
		       (loop (read-line) shared mutable immutable))))))))

(define (extract-interface! file)
   (with-input-from-file file
      (lambda ()
	 (let loop ((line (read-line)))
	    (unless (eof-object? line)
	       (let ((parsed-line (parse-line line)))
		  (if parsed-line
		      (print "var " (car parsed-line) " = "
			     (cdr parsed-line) ";")))
	       (loop (read-line)))))))
   
(define (myMain args)
   (cond
      ((not (and (pair? args)
		 (pair? (cdr args))
		 (pair? (cddr args))))
       (error #f (string-append (car args) " runtime.js mapping|interface")
	      (cdr args)))
      ((string=? (caddr args) "mapping")
       (set! *mode* 'map))
      ((string=? (caddr args) "interface")
       (set! *mode* 'interface))
      (else
       (error #f (string-append (car args) " runtime.js mapping|interface")
	      (cdr args))))
   (if (eq? *mode* 'map)
       (extract-mapping! (cadr args))
       (extract-interface! (cadr args))))
