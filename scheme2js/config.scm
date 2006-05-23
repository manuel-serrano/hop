(module config
   (export (config-init! . Lconfig)
	   (config-set! conf value)
	   (config conf)))

(define (config-init! . Lconfig)
   (thread-parameter-set! '*config* (if (null? Lconfig)
					(make-hashtable)
					(car Lconfig))))

(define (config conf)
   (hashtable-get (thread-parameter '*config*) conf))

(define (config-set! conf value)
   (hashtable-put! (thread-parameter '*config*) conf value))
