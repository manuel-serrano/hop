(module config
   (export (config-init! . Lconfig)
	   (config-set! conf value)
	   (config conf)))

(define *config* #unspecified)

(define (config-init! . Lconfig)
   (set! *config* (if (null? Lconfig)
			(make-hashtable)
			(car Lconfig))))

(define (config conf)
   (hashtable-get *config* conf))

(define (config-set! conf value)
   (hashtable-put! *config* conf value))
