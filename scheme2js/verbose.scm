(module verbose
   (export (verbose . Lmessage)
	   *verbose*))

(define *verbose* #f)

(define (verbose . Lmessage)
   (if *verbose*
       (with-output-to-port (current-error-port)
	  (lambda ()
	     (apply print Lmessage)))))
