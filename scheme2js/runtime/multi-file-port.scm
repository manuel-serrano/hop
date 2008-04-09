(define (open-multi-file files)
   (let ((current-port #f))
      (open-input-procedure
       (lambda ()
	  (define (read-multi)
	     (cond
		((and (null? files)
		      (not current-port))
		 #f)
		((not current-port)
		 (set! current-port (open-input-file (car files)))
		 (if (not current-port)
		     (error "multi-file-port"
			    "could not open file: "
			    (car files)))
		 (set! files (cdr files))
		 (read-multi))
		(else
		 (let ((chars (read-chars 100 current-port)))
		    (if (or (eof-object? chars)
			    (zero? (string-length chars)))
			(begin
			   (close-input-port current-port)
			   (set! current-port #f)
			   (read-multi))
			chars)))))
	  (read-multi)))))
