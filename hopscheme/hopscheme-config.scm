(module hopscheme-config
   (library scheme2js)
   (export (hopscheme-config)))

(define (hopscheme-config)
   (let ((config (default-scheme2js-config)))
      ;; unresolved symbols are considered to be JS variables
      (hashtable-put! config 'unresolved=JS #t)
      config))
