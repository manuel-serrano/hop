(module verbose
   (import config)
   (export (verbose . Lmessage)))

(define (verbose . Lmessage)
   (if (config 'verbose)
       (with-output-to-port (current-error-port)
	  (lambda ()
	     (apply print Lmessage)))))
