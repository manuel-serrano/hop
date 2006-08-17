(module hopscheme-config
   (library scheme2js)
   (export (hopscheme-config)))

(define (hopscheme-config)
   (let ((config (default-scheme2js-config)))
      ;; unresolved symbols are considered to be JS variables
      (hashtable-put! config 'unresolved=JS #t)

      ;; procedures may use the 'this' variable. (This was actually already
      ;; possible due to the 'unresolved=JS flag. But this is cleaner.)
      (hashtable-put! config 'procedures-provide-js-this #t)

      ;; one can use the 'return!' form now. eg: (return! #t)
      (hashtable-put! config 'return #t)

      ;; if one wants to test trampolines:
      ;(hashtable-put! config 'trampoline)
      config))
