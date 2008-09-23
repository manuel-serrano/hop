(module __hopscheme_config
   (library scheme2js)
   (library hop)
   (export (hopscheme-config compile-file?)))

(define *module-counter* -1)
(define *cached-config* #f)
(define *module-counter-lock* (make-mutex))

;; CARE !!!! (MS 19 Jan 2008): THIS MUST BE IMPROVED. IT IS FAR TOO EXPENSIVE
;; TO CALL HOPSCHEME-CONFIG WHEN COMPILING EACH CLIENT EXPRESSION. IT IS
;; NOT REALISTIC TO ALLOCATE A NEW HASH TABLE FOR EACH COMPILATION.

(define (get-cached-config)
   (or *cached-config*
       (begin
	  ;; no need to worry about multi-threading. in the worst case we
	  ;; create several lists...
	  (set! *cached-config*
		(cons*
		 ;; unresolved symbols are considered to be JS variables
		 '(unresolved=JS . module)
		 ;; procedures may use the 'this' variable. (This was actually
		 ;; already possible due to the 'unresolved=JS flag. But this
		 ;; is cleaner.)
		 '(procedures-provide-js-this . #t)
		 ;; one can use the 'return!' form now. eg: (return! #t)
		 '(return . #t)
		 ;; no indentation
		 '(indent . #f)
		 ;; include-path
		 `(include-paths . ,(list (hop-share-directory)))
		 ;; on top of scheme2js's default config
		 (default-scheme2js-config)))
	  *cached-config*)))

;; do not reuse hopscheme-configs!
(define (hopscheme-config compile-file?)

   (define (add-suffix-clause conf)
      ;; make sure static variables (not exported, but global) do not clash
      (if compile-file?
	  conf
	  (with-lock *module-counter-lock*
	     (lambda ()
		(set! *module-counter* (+ *module-counter* 1))
		(cons `(statics-suffix
			. ,(string-append "_hopM"
					  (number->string *module-counter*)))
		      conf)))))

   (let* ((config (get-cached-config))
	  (conf-indent (if (>fx (hop-verbose) 0)
			   (cons '(indent . 2) config)
			   config))
	  (conf-verbose (if (>fx (hop-verbose) 10)
			    (cons '(verbose . #t) conf-indent)
			    conf-indent))
	  (conf-module (add-suffix-clause conf-verbose)))
      conf-module))
