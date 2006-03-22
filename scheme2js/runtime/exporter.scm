(module export-runtime
   (main myMain))

(define *export-line* (pregexp "/// export[ ]*(.*)"))
(define *exported-fun* (pregexp "(?:var|function) ((?:sc_)?([^ (;]*))"))

(define (starts-with? s sub-s)
   (substring-at? s sub-s 0))

(define (unmarshall s)
   (let loop ((chars (string->list s))
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
		    (cons (car chars) rev-res)))))))

(define *mode* #f)

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
   (with-input-from-file (cadr args)
      (lambda ()
	 (if (eq? *mode* 'map) (print "(js"))
	 (let loop ((line (read-line)))
	    (if (eof-object? line)
		(if (eq? *mode* 'map) (print ")"))
		(let ((match (pregexp-match *export-line* line)))
		   (if match
		       (let* ((fun-match (pregexp-match *exported-fun* line))
			      (exported-fun (cadr fun-match))
			      (interface-fun (string-append "sci_"
							    (caddr fun-match)))
			      (override-names (cadr match))
			      (scheme-funs (if (string=? override-names "")
					       (list (unmarshall (caddr fun-match)))
					       (string-split override-names
							     " "))))
			  (if (eq? *mode* 'map)
			      (for-each (lambda (scheme-fun)
					   (print "  (" scheme-fun
						  " " interface-fun ")"))
					scheme-funs)
			      (print "var " interface-fun " = " exported-fun ";"))))
		   (loop (read-line))))))))
