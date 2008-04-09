(module hopscheme-config
   (library scheme2js)
   (library hop)
   (export (hopscheme-config compile-file?)))

(define *module-counter* 0)
(define *module-counter-lock* (make-mutex))

;; CARE !!!! (MS 19 Jan 2008): THIS MUST BE IMPROVED. IT IS FAR TOO EXPENSIVE
;; TO CALL HOPSCHEME-CONFIG WHEN COMPILING EACH CLIENT EXPRESSION. IT IS
;; NOT REALISTIC TO ALLOCATE A NEW HASH TABLE FOR EACH COMPILATION.

;; do not reuse hopscheme-configs!
(define (hopscheme-config compile-file?)
   (let ((config (default-scheme2js-config)))
      
      ;; indentation only when debugging
      (if (>fx (bigloo-debug) 0)
	  (hashtable-put! config 'indent 2)
	  (hashtable-put! config 'indent #f))
	 
      ;; unresolved symbols are considered to be JS variables
      (hashtable-put! config 'unresolved=JS 'module)

      ;; procedures may use the 'this' variable. (This was actually already
      ;; possible due to the 'unresolved=JS flag. But this is cleaner.)
      (hashtable-put! config 'procedures-provide-js-this #t)

      ;; one can use the 'return!' form now. eg: (return! #t)
      (hashtable-put! config 'return #t)

      ;; all global variables should be exported (a must in HOP)
      ;; not needed anymore. if it's a module usual rules apply. otherwise
      ;; globals are exported
      ; (hashtable-put! config 'export-globals #t)

      ;; make sure static variables (not exported, but global) do not clash
      (unless compile-file?
	 (with-lock *module-counter-lock*
	    (lambda ()
	       (hashtable-put! config 'statics-suffix
			       (string-append "_hopM"
					      (number->string *module-counter*)))
	       (set! *module-counter* (+ *module-counter* 1)))))

      (hashtable-put! config 'include-paths (list (hop-share-directory)))

      (if (>fx (hop-verbose) 10)
	  (hashtable-put! config 'verbose #t))
      
      ;; if one wants to test trampolines:
      ;(hashtable-put! config 'trampoline #t)

      ;; to enable call/cc uncomment the following line:
      ;(hashtable-put! config 'call/cc #t)

      config))
