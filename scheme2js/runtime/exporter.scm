(module export-runtime
   (main myMain))

(define *export-line-pattern* (pregexp "/// export[ ]*(.*)"))
(define *exported-fun-pattern* (pregexp "(?:var|function) ((?:sc_)?([^ (;]*))"))
(define *attributes-pattern* (pregexp "((?:sc_)?(?:_[A-Z]|[^_])*)(?:_(.*))?"))

(define (starts-with? s sub-s)
   (substring-at? s sub-s 0))

(define (ends-with? s sub-s)
   (let ((s-length (string-length s))
	 (sub-s-length (string-length sub-s)))
      (and (>= s-length sub-s-length)
	   (substring-at? s sub-s (- s-length sub-s-length)))))

(define (unmarshall s)
   (let ((s-without-attributes (cadr (pregexp-match *attributes-pattern* s))))
      (let loop ((chars (string->list s-without-attributes))
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

(define (extract-attributes s)
   (let* ((match (pregexp-match *attributes-pattern* s))
	  (attributes_ext (caddr match)))
      (and attributes_ext
	   (map string->symbol
		(string-split attributes_ext "_")))))

(define (parse-line line)
   (let ((match (pregexp-match *export-line-pattern* line)))
      (and match
	   (let* ((fun-match (pregexp-match *exported-fun-pattern* line))
		  (exported-fun (cadr fun-match))
		  (interface-fun (string-append "sci_"
						(caddr fun-match)))
		  (attributes (extract-attributes exported-fun))
		  (override-names (cadr match))
		  (scheme-funs (if (string=? override-names "")
				   (list (unmarshall (caddr fun-match)))
				   (string-split override-names
						 " "))))
	      (if (eq? *mode* 'map)
		  (list attributes scheme-funs interface-fun)
		  (cons interface-fun exported-fun))))))

(define *mode* #f)

(define (print-runtime-var-mapping ht)
   (let ((combinations '(("mutable" mutable default)
			 ("immutable" immutable default)
			 ("mutable-call/cc" mutable-call/cc call/cc mutable
					    default)
			 ("immutable-call/cc" immutable-call/cc call/cc
					      immutable default))))
      (for-each
       (lambda (combination)
	  (let ((name (string->symbol
		       (string-append "*" (car combination) "-runtime-var-mapping*")))
		(done-symbols (make-hashtable)))
	     ;(with-output-to-port (current-error-port) (lambda () (print name)))
	     (let loop ((res-mapping '())
			(remaining (cdr combination)))
		(if (null? remaining)
		    (pp `(define ,name ',res-mapping))
		    (let* ((part-mapping (hashtable-get ht (car remaining)))
;			   (dummy
;			    (with-output-to-port (current-error-port)
;			       (lambda () (print (car remaining)))))
			   (part-mapping-filtered
			    (filter (lambda (p)
				       (not (hashtable-get done-symbols
							   (car p))))
				    part-mapping)))
		       (for-each (lambda (p)
				    (hashtable-put! done-symbols (car p) #t))
				 part-mapping-filtered)
		       (loop (append! part-mapping-filtered res-mapping)
			     (cdr remaining)))))))
       combinations)))

(define (extract-mapping! file)
   (with-input-from-file file
      (lambda ()
	 (let loop ((line (read-line))
		    (ht (make-hashtable)))
	    (if (eof-object? line)
		(print-runtime-var-mapping ht)
		(let ((parsed-line (parse-line line)))
		   (if parsed-line
		       (let* ((attributes (car parsed-line))
			      (scheme-funs (cadr parsed-line))
			      (interface-fun (caddr parsed-line))
			      (sym-interface-fun (string->symbol interface-fun))
			      (new-mappings
			       (map! (lambda (scheme-fun)
					(list (string->symbol scheme-fun)
					      sym-interface-fun))
				     scheme-funs)))
			      (hashtable-update!
			       ht
			       (cond
				  ((not attributes)
				   'default)
				  ((and (memq 'mutable attributes)
					(memq 'callcc attributes))
				   'mutable-call/cc)
				  ((and (memq 'immutable attributes)
					(memq 'callcc attributes))
				   'immutable-call/cc)
				  ((memq 'mutable attributes)
				   'mutable)
				  ((memq 'immutable attributes)
				   'immutable)
				  ((memq 'callcc attributes)
				   'call/cc)
				  (else
				   (error #f "should not happen " attributes)))
			       (lambda (old-mappings)
				  (append! new-mappings old-mappings))
			       new-mappings)))
		   (loop (read-line) ht)))))))

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
       (extract-interface! (cadr args)))
   (exit 0))
